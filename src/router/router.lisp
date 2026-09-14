(in-package #:llm-protocol)

;;; CLOS routing policies. Compose by wrapping (budget wraps fallback wraps …).
;;; No config DSL. Fallback reuses WITH-AUTO-RETRY's retryable set
;;; (LLM-HTTP-ERROR 408/409/429/5xx) plus other LLM-ERROR subtypes except
;;; LLM-OUTPUT-ERROR — try the next backend instead of retrying the same one.

(defclass routing-policy () ()
  (:documentation "SELECT-BACKEND picks among router candidates."))

(defclass llm-route-request ()
  ((operation :initarg :operation :accessor route-request-operation
              :initform :generate)
   (turns :initarg :turns :accessor route-request-turns :initform nil)
   (model :initarg :model :accessor route-request-model :initform nil)
   (settings :initarg :settings :accessor route-request-settings :initform nil)
   (inputs :initarg :inputs :accessor route-request-inputs :initform nil)
   (scope :initarg :scope :accessor route-request-scope :initform nil)))

(defun make-llm-route-request (&key (operation :generate) turns model settings
                                 inputs scope)
  (make-instance 'llm-route-request
                 :operation operation :turns turns :model model
                 :settings settings :inputs inputs :scope scope))

(defun llm-route-request-p (x)
  (typep x 'llm-route-request))

(defun %as-route-request (request)
  (cond
    ((llm-route-request-p request) request)
    ((and (consp request) (keywordp (car request)))
     (apply #'make-llm-route-request request))
    (t (make-llm-route-request :operation :generate :turns request))))

(defgeneric select-backend (policy request candidates)
  (:documentation "→ backend for REQUEST from CANDIDATES.
REQUEST is an LLM-ROUTE-REQUEST or a plist (:operation :turns :model :settings)."))

(defmethod select-backend :around (policy request candidates)
  (call-next-method policy (%as-route-request request) candidates))

(defmethod select-backend ((policy routing-policy) request candidates)
  (declare (ignore policy request))
  (first candidates))

(defclass fallback-chain-policy (routing-policy)
  ((candidates :initarg :candidates :accessor fallback-candidates :initform nil)))

(defun make-fallback-chain-policy (&key candidates)
  (make-instance 'fallback-chain-policy :candidates (copy-list candidates)))

(defun fallback-chain-policy-p (x)
  (typep x 'fallback-chain-policy))

(defmethod select-backend ((policy fallback-chain-policy) request candidates)
  (declare (ignore request))
  (let ((order (or (fallback-candidates policy) candidates)))
    (find-if (lambda (b) (member b candidates :test #'eq)) order)))

(defclass llm-budget ()
  ((max-tokens :initarg :max-tokens :accessor llm-budget-max-tokens :initform nil)
   (max-cost :initarg :max-cost :accessor llm-budget-max-cost :initform nil))
  (:documentation "Ceilings for a budget-policy scope. NIL = unlimited.
Cost uses LLM-MODEL-INFO INPUT-PRICE / OUTPUT-PRICE as USD per 1M tokens."))

(defun make-llm-budget (&key max-tokens max-cost)
  (make-instance 'llm-budget :max-tokens max-tokens :max-cost max-cost))

(defun llm-budget-p (x)
  (typep x 'llm-budget))

(defclass %scope-usage ()
  ((tokens :initform 0 :accessor %scope-usage-tokens)
   (cost :initform 0.0d0 :accessor %scope-usage-cost)))

(defclass budget-policy (routing-policy)
  ((inner :initarg :inner :accessor budget-policy-inner :initform nil)
   (budget :initarg :budget :accessor budget-policy-budget)
   (ledgers :initform (make-hash-table :test 'equal)
            :accessor budget-policy-ledgers)))

(defun make-budget-policy (&key inner budget)
  (make-instance 'budget-policy :inner inner :budget budget))

(defun budget-policy-p (x)
  (typep x 'budget-policy))

(defmethod select-backend ((policy budget-policy) request candidates)
  (select-backend (or (budget-policy-inner policy)
                      (make-instance 'routing-policy))
                  request candidates))

(defclass least-latency-policy (routing-policy)
  ((inner :initarg :inner :accessor latency-policy-inner :initform nil)
   (alpha :initarg :alpha :accessor latency-policy-alpha :initform 0.3d0)
   (estimates :initform (make-hash-table :test 'eq)
              :accessor latency-policy-estimates)))

(defun make-least-latency-policy (&key inner (alpha 0.3d0))
  (make-instance 'least-latency-policy :inner inner :alpha alpha))

(defun least-latency-policy-p (x)
  (typep x 'least-latency-policy))

(defmethod select-backend ((policy least-latency-policy) request candidates)
  (declare (ignore request))
  (let ((pool (copy-list candidates)))
    (when (and (null pool) (latency-policy-inner policy))
      (setf pool (policy-candidate-list (latency-policy-inner policy))))
    (first (stable-sort pool #'<
                        :key (lambda (b)
                               (gethash b (latency-policy-estimates policy)
                                        0d0))))))

(defun policy-candidate-list (policy)
  (typecase policy
    (fallback-chain-policy (copy-list (fallback-candidates policy)))
    (budget-policy (policy-candidate-list (budget-policy-inner policy)))
    (least-latency-policy (policy-candidate-list (latency-policy-inner policy)))
    (t nil)))

(defun %has-fallback-chain (policy)
  (typecase policy
    (fallback-chain-policy t)
    (budget-policy (%has-fallback-chain (budget-policy-inner policy)))
    (least-latency-policy (%has-fallback-chain (latency-policy-inner policy)))
    (t nil)))

(defun %find-budget-policy (policy)
  (typecase policy
    (budget-policy policy)
    (least-latency-policy (%find-budget-policy (latency-policy-inner policy)))
    (fallback-chain-policy nil)
    (t nil)))

(defun %fallback-worthy-p (condition)
  "Only retryable HTTP errors advance the fallback chain.
   LLM-OUTPUT-ERROR keeps IGNORE-OUTPUT / RETRY on the signaling frame."
  (and (typep condition 'llm-http-error)
       (llm-http-error-retryable-p condition)))

(defun %scope-usage (policy scope)
  (let ((table (budget-policy-ledgers policy)))
    (or (gethash scope table)
        (setf (gethash scope table) (make-instance '%scope-usage)))))

(defun %usage-tokens (usage)
  (or (and usage (llm-usage-total-tokens usage))
      (and usage
           (+ (or (llm-usage-input-tokens usage) 0)
              (or (llm-usage-output-tokens usage) 0)))
      0))

(defun %model-prices (backend model)
  (let ((info (%lookup-model-info backend model)))
    (values (and (llm-model-info-p info) (llm-model-info-input-price info))
            (and (llm-model-info-p info) (llm-model-info-output-price info)))))

(defun %usage-cost (usage backend model)
  "USD from USAGE using per-1M INPUT-PRICE / OUTPUT-PRICE."
  (multiple-value-bind (in-price out-price)
      (%model-prices backend model)
    (let ((in (or (and usage (llm-usage-input-tokens usage)) 0))
          (out (or (and usage (llm-usage-output-tokens usage)) 0)))
      (+ (* in (/ (or in-price 0) 1000000.0d0))
         (* out (/ (or out-price 0) 1000000.0d0))))))

(defun %over-budget-p (budget used-tokens used-cost)
  (or (and (llm-budget-max-tokens budget)
           (>= used-tokens (llm-budget-max-tokens budget)))
      (and (llm-budget-max-cost budget)
           (>= used-cost (llm-budget-max-cost budget)))))

(defun %maybe-check-budget (policy scope)
  "Signals LLM-BUDGET-EXCEEDED when the scope is at/over ceiling.
   → (values replacement-backend replacement-model)."
  (let ((bp (%find-budget-policy policy)))
    (if (null bp)
        (values nil nil)
        (let* ((budget (budget-policy-budget bp))
               (usage (%scope-usage bp scope))
               (used-tokens (%scope-usage-tokens usage))
               (used-cost (%scope-usage-cost usage)))
          (if (or (null budget)
                  (not (%over-budget-p budget used-tokens used-cost)))
              (values nil nil)
              (restart-case
                  (error 'llm-budget-exceeded
                         :budget budget
                         :scope scope
                         :used-tokens used-tokens
                         :used-cost used-cost
                         :max-tokens (llm-budget-max-tokens budget)
                         :max-cost (llm-budget-max-cost budget))
                (continue-anyway ()
                  :report "Continue despite the budget ceiling"
                  (values nil nil))
                (use-cheaper-model (backend &optional model)
                  :report "Use a cheaper backend or model"
                  :interactive (lambda ()
                                 (format *query-io* "Cheaper backend: ")
                                 (force-output *query-io*)
                                 (list (read *query-io*)))
                  (values backend model))
                (abort ()
                  :report "Abort generation"
                  (error 'llm-budget-exceeded
                         :message (format nil "generation aborted (scope ~s)" scope)
                         :budget budget
                         :scope scope
                         :used-tokens used-tokens
                         :used-cost used-cost))))))))

(defgeneric record-usage (policy scope usage &key backend model)
  (:documentation "Add USAGE to POLICY's ledger for SCOPE."))

(defmethod record-usage ((policy t) scope usage &key backend model)
  (declare (ignore policy scope usage backend model))
  nil)

(defmethod record-usage ((policy budget-policy) scope usage &key backend model)
  (let ((u (%scope-usage policy (or scope "default"))))
    (incf (%scope-usage-tokens u) (%usage-tokens usage))
    (incf (%scope-usage-cost u) (%usage-cost usage backend model)))
  (record-usage (budget-policy-inner policy) scope usage
                :backend backend :model model))

(defmethod record-usage ((policy least-latency-policy) scope usage &key backend model)
  (record-usage (latency-policy-inner policy) scope usage
                :backend backend :model model))

(defgeneric record-latency (policy backend seconds)
  (:documentation "Record a sample for LEAST-LATENCY-POLICY's EWMA."))

(defmethod record-latency ((policy t) backend seconds)
  (declare (ignore policy backend seconds))
  nil)

(defmethod record-latency ((policy least-latency-policy) backend seconds)
  (let* ((est (latency-policy-estimates policy))
         (prev (gethash backend est))
         (a (float (latency-policy-alpha policy) 1.0d0))
         (s (float seconds 1.0d0)))
    (setf (gethash backend est)
          (if prev (+ (* a s) (* (- 1d0 a) prev)) s)))
  (record-latency (latency-policy-inner policy) backend seconds))

(defmethod record-latency ((policy budget-policy) backend seconds)
  (record-latency (budget-policy-inner policy) backend seconds))

(defun %result-usage (result)
  (cond
    ((llm-response-p result) (llm-response-usage result))
    ((llm-embed-result-p result) (llm-embed-result-usage result))
    (t nil)))

(defun %metered-operation-p (operation)
  (member operation '(:generate :stream-generate :respond :stream-respond :embed)))

(defclass llm-router-backend (llm-backend)
  ((policy :initarg :policy :accessor llm-router-policy)
   (candidates :initarg :candidates :accessor llm-router-candidates :initform nil)
   (scope :initarg :scope :accessor llm-router-scope :initform "default")))

(defun make-llm-router-backend (&key policy candidates (scope "default"))
  (let ((cands (copy-list candidates)))
    (make-instance 'llm-router-backend
                   :policy (or policy (make-fallback-chain-policy :candidates cands))
                   :candidates cands
                   :scope scope)))

(defun llm-router-backend-p (x)
  (typep x 'llm-router-backend))

(defun %router-candidates (router)
  (or (copy-list (llm-router-candidates router))
      (policy-candidate-list (llm-router-policy router))))

(defun %router-select (router &optional (operation :generate))
  (let ((cands (%router-candidates router)))
    (or (ignore-errors
          (select-backend (llm-router-policy router)
                          (make-llm-route-request :operation operation)
                          cands))
        (first cands))))

(defun %router-dispatch (router request fn)
  (let* ((policy (llm-router-policy router))
         (request (%as-route-request request))
         (scope (or (route-request-scope request)
                    (llm-router-scope router)
                    "default"))
         (remaining (%router-candidates router))
         (forced nil)
         (forced-model nil))
    (when (%metered-operation-p (route-request-operation request))
      (multiple-value-bind (replacement model)
          (%maybe-check-budget policy scope)
        (when replacement
          (setf forced replacement))
        (when model
          (setf forced-model model
                (route-request-model request) model))))
    (tagbody
     :next
       (let* ((backend (or forced (select-backend policy request remaining)))
              (start (get-internal-real-time)))
         (unless backend
           (error 'llm-missing-backend
                  :message "llm router has no backend for this request"))
         (handler-bind
             ((llm-http-error
               (lambda (c)
                 (let ((next (remove backend remaining :count 1 :test #'eq)))
                   (when (and (%has-fallback-chain policy)
                              (%fallback-worthy-p c)
                              next
                              (not (eq next remaining)))
                     (setf remaining next
                           forced nil)
                     (go :next))))))
           (let ((result (funcall fn backend
                                  (or forced-model
                                      (route-request-model request)))))
             (record-latency policy backend
                             (float (/ (- (get-internal-real-time) start)
                                       internal-time-units-per-second)
                                    1.0d0))
             (record-usage policy scope (%result-usage result)
                           :backend backend
                           :model (or forced-model
                                      (route-request-model request)
                                      (and (llm-response-p result)
                                           (llm-response-model result))
                                      (and (llm-embed-result-p result)
                                           (llm-embed-result-model result))))
             (return-from %router-dispatch result)))))))

(defmethod backend-model ((backend llm-router-backend))
  (let ((b (%router-select backend)))
    (and b (backend-model b))))

(defmethod backend-supports-p ((backend llm-router-backend) feature)
  (some (lambda (b) (backend-supports-p b feature))
        (%router-candidates backend)))

(defmethod generate ((backend llm-router-backend) turns &key model settings tools
                     tool-choice output)
  (%router-dispatch
   backend
   (make-llm-route-request :operation :generate :turns turns :model model
                           :settings settings)
   (lambda (selected chosen-model)
     (generate selected turns :model (or chosen-model model)
               :settings settings :tools tools :tool-choice tool-choice
               :output output))))

(defmethod stream-generate ((backend llm-router-backend) turns &key model settings
                            tools tool-choice on-part output)
  (%router-dispatch
   backend
   (make-llm-route-request :operation :stream-generate :turns turns :model model
                           :settings settings)
   (lambda (selected chosen-model)
     (stream-generate selected turns :model (or chosen-model model)
                      :settings settings :tools tools :tool-choice tool-choice
                      :on-part on-part :output output))))

(defmethod respond ((backend llm-router-backend) items &key model settings tools
                    tool-choice output)
  (%router-dispatch
   backend
   (make-llm-route-request :operation :respond :turns items :model model
                           :settings settings)
   (lambda (selected chosen-model)
     (respond selected items :model (or chosen-model model)
              :settings settings :tools tools :tool-choice tool-choice
              :output output))))

(defmethod stream-respond ((backend llm-router-backend) items &key model settings
                           tools tool-choice on-part output)
  (%router-dispatch
   backend
   (make-llm-route-request :operation :stream-respond :turns items :model model
                           :settings settings)
   (lambda (selected chosen-model)
     (stream-respond selected items :model (or chosen-model model)
                     :settings settings :tools tools :tool-choice tool-choice
                     :on-part on-part :output output))))

(defmethod embed ((backend llm-router-backend) inputs &key model dimensions
                  encoding-format)
  (%router-dispatch
   backend
   (make-llm-route-request :operation :embed :inputs inputs :model model)
   (lambda (selected chosen-model)
     (embed selected inputs :model (or chosen-model model)
            :dimensions dimensions :encoding-format encoding-format))))

(defmethod list-models ((backend llm-router-backend) &key)
  (mapcan (lambda (b)
            (copy-list (ignore-errors (list-models b))))
          (%router-candidates backend)))

(defmethod count-tokens :around ((backend llm-router-backend) thing)
  (let ((selected (%router-select backend :count-tokens)))
    (if (and selected (not (eq selected backend)))
        (count-tokens selected thing)
        (call-next-method))))

(defmethod context-window ((backend llm-router-backend) model)
  (let ((selected (%router-select backend :context-window)))
    (if selected
        (context-window selected model)
        (call-next-method))))
