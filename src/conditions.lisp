(in-package #:llm-protocol)

;;; Conditions + interactive/programmatic restarts (pathlib shape).
;;;
;;; Typical recoveries:
;;;   retry         — re-run GENERATE / RESPOND / LIST-MODELS (outer WITH-LLM-RESTARTS)
;;;   use-value     — supply an LLM-RESPONSE (or parsed output on LLM-OUTPUT-ERROR)
;;;   ignore-output — leave LLM-RESPONSE-OUTPUT NIL on a parse failure
;;;
;;; RETRY is for transient HTTP (429 / 408 / 409 / 5xx). Do not default-retry
;;; structured-output parse failures — that is *STRUCTURED-OUTPUT-REPAIR*
;;; (one extra GENERATE + relaxed parse + text fallback), not HTTP RETRY.

(define-condition llm-error (error)
  ((message :initarg :message :reader llm-error-message :initform nil))
  (:report (lambda (c s)
             (format s "llm error~@[: ~a~]" (llm-error-message c)))))

(define-condition llm-missing-backend (llm-error) ()
  (:report (lambda (c s)
             (format s "llm backend missing~@[: ~a~]" (llm-error-message c)))))

(define-condition llm-unsupported (llm-error) ()
  (:report (lambda (c s)
             (format s "llm unsupported~@[: ~a~]" (llm-error-message c)))))

(define-condition llm-unknown-provider (llm-error)
  ((name :initarg :name :reader llm-unknown-provider-name :initform nil))
  (:report (lambda (c s)
             (format s "llm unknown provider ~s~@[: ~a~]"
                     (llm-unknown-provider-name c)
                     (llm-error-message c)))))

(defun http-status-retryable-p (status)
  "T for 408 / 409 / 429 / 5xx."
  (and (integerp status)
       (or (= status 408) (= status 409) (= status 429) (>= status 500))))

(define-condition llm-http-error (llm-error)
  ((status :initarg :status :reader llm-http-error-status :initform nil)
   ;; 0.7.0: where the request went, for messages that name the provider,
   ;; the base URL and the model. Never the API key.
   (provider :initarg :provider :reader llm-error-provider :initform nil)
   (url :initarg :url :reader llm-error-url :initform nil)
   (model :initarg :model :reader llm-error-model :initform nil)
   (body :initarg :body :reader llm-http-error-body :initform nil)
   (retry-after :initarg :retry-after :reader llm-http-error-retry-after
                :initform nil
                :documentation "Seconds the provider asked to wait (Retry-After), or NIL.")
   (retryable-p :initarg :retryable-p))
  (:report (lambda (c s)
             (format s "llm HTTP error~@[ ~a~]~@[: ~a~]"
                     (llm-http-error-status c)
                     (or (llm-error-message c) (llm-http-error-body c))))))

(defun llm-http-error-retryable-p (condition)
  "Explicit :RETRYABLE-P if supplied, otherwise derived from STATUS."
  (if (slot-boundp condition 'retryable-p)
      (slot-value condition 'retryable-p)
      (http-status-retryable-p (llm-http-error-status condition))))

(define-condition llm-output-error (llm-error)
  ((response :initarg :response :accessor llm-output-error-response :initform nil)
   (cause :initarg :cause :reader llm-output-error-cause :initform nil))
  (:report (lambda (c s)
             (format s "llm structured output error~@[: ~a~]"
                     (llm-error-message c)))))

(define-condition llm-budget-exceeded (llm-error)
  ((budget :initarg :budget :reader llm-budget-exceeded-budget :initform nil)
   (scope :initarg :scope :reader llm-budget-exceeded-scope :initform nil)
   (used-tokens :initarg :used-tokens :reader llm-budget-exceeded-used-tokens
                :initform 0)
   (used-cost :initarg :used-cost :reader llm-budget-exceeded-used-cost
              :initform 0)
   (max-tokens :initarg :max-tokens :reader llm-budget-exceeded-max-tokens
               :initform nil)
   (max-cost :initarg :max-cost :reader llm-budget-exceeded-max-cost
             :initform nil))
  (:report (lambda (c s)
             (format s "llm budget exceeded~@[ for scope ~s~] (tokens ~a~@[/~a~]~@[, cost ~a~@[/~a~]~])~@[: ~a~]"
                     (llm-budget-exceeded-scope c)
                     (llm-budget-exceeded-used-tokens c)
                     (llm-budget-exceeded-max-tokens c)
                     (llm-budget-exceeded-used-cost c)
                     (llm-budget-exceeded-max-cost c)
                     (llm-error-message c)))))

;;; --- context overflow + retry classes -------------------------------------

(define-condition llm-context-overflow (llm-http-error) ()
  (:default-initargs :retryable-p nil)
  (:documentation "The prompt does not fit the model's context window. Never
retried as is: compact the turns (conversation-protocol COMPACT-TURNS) first.")
  (:report (lambda (c s)
             (format s "llm prompt exceeds the context window~@[ (HTTP ~a)~]~@[: ~a~]"
                     (llm-http-error-status c)
                     (llm-error-message c)))))

;;; --- incomplete (truncated) streamed response ----------------------------

(define-condition llm-incomplete-response (llm-http-error)
  ((partial :initarg :partial :reader llm-incomplete-response-partial
            :initform nil
            :documentation "The LLM-RESPONSE assembled from what arrived before
the stream closed (text, thinking, tool calls; finish reason :INCOMPLETE)."))
  (:default-initargs :retryable-p t)
  (:documentation "A streamed response ended without a terminal signal (no
finish reason / end sentinel / completed event): the connection closed or the
server aborted mid-answer. LLM-RETRY-CLASS :TRANSIENT (no status, retryable),
so a RETRY-POLICY retries it. Signalled
by SIGNAL-INCOMPLETE-RESPONSE, which offers USE-PARTIAL-RESPONSE.")
  (:report (lambda (c s)
             (format s "llm stream ended without a terminal signal~@[: ~a~]"
                     (llm-error-message c)))))

(defun signal-incomplete-response (partial &key message)
  "Signal LLM-INCOMPLETE-RESPONSE carrying PARTIAL (an LLM-RESPONSE whose
finish reason is :INCOMPLETE). Restarts:
  USE-PARTIAL-RESPONSE — return PARTIAL as the result;
  USE-VALUE (value)    — return VALUE instead.
An outer RETRY (WITH-LLM-RESTARTS, CALL-WITH-RETRY-POLICY) re-runs the call."
  (restart-case
      (error 'llm-incomplete-response :partial partial :message message)
    (use-partial-response ()
      :report "Use the partial response (finish reason :INCOMPLETE)"
      partial)
    (use-value (value)
      :report "Use a supplied response instead"
      :interactive (lambda ()
                     (format *query-io* "Value to use: ")
                     (force-output *query-io*)
                     (list (read *query-io*)))
      value)))

(defparameter +context-overflow-markers+
  '("prompt is too long" "context_length_exceeded" "maximum context length"
    "context length" "context window" "too many tokens" "input is too long"
    "exceeds the context" "request too large")
  "Lower-case phrases providers use for an oversized prompt.")

(defun context-overflow-text-p (text)
  "T when TEXT (an error message or body) says the prompt is too long.
Backends use it to signal LLM-CONTEXT-OVERFLOW instead of LLM-HTTP-ERROR."
  (and (stringp text)
       (let ((low (string-downcase text)))
         (some (lambda (m) (search m low)) +context-overflow-markers+))
       t))

(defgeneric llm-retry-class (condition)
  (:documentation "Failure class of CONDITION for a RETRY-POLICY:
:RATE-LIMIT (429), :OVERLOADED (503 / 529), :TRANSIENT (408 / 409 / other
5xx), :CONTEXT-OVERFLOW, or NIL (not retryable: auth, bad request, ...).")
  (:method ((condition condition)) nil)
  (:method ((condition llm-context-overflow)) :context-overflow)
  (:method ((condition llm-http-error))
    (when (llm-http-error-retryable-p condition)
      (case (llm-http-error-status condition)
        (429 :rate-limit)
        ((503 529) :overloaded)
        (t :transient)))))

(defclass retry-policy ()
  ((max-attempts :initarg :max-attempts :accessor retry-policy-max-attempts
                 :initform 5
                 :documentation "Attempts in all, the first one included.")
   (base-delay :initarg :base-delay :accessor retry-policy-base-delay
               :initform 2
               :documentation "Seconds before the second attempt; doubles after.")
   (max-delay :initarg :max-delay :accessor retry-policy-max-delay
              :initform 30
              :documentation "Upper bound on one wait, Retry-After included.")
   (classes :initarg :classes :accessor retry-policy-classes
            :initform '(:rate-limit :overloaded :transient)
            :documentation "LLM-RETRY-CLASS values this policy retries.")
   (jitter :initarg :jitter :accessor retry-policy-jitter :initform 0
           :documentation "0.7.0: fraction in [0, 1]. A backoff delay D becomes
D x (1 - JITTER x R), R uniform in [0, 1) from *RETRY-RANDOM-STATE*. A
Retry-After delay is never jittered. 0 (the default) keeps 0.6 delays."))
  (:documentation "Which failures to retry and how long to wait between tries."))

(defun make-retry-policy (&key (max-attempts 5) (base-delay 2) (max-delay 30)
                            (classes '(:rate-limit :overloaded :transient))
                            (jitter 0))
  (check-type max-attempts (integer 1 *))
  (check-type base-delay (real 0 *))
  (check-type max-delay (real 0 *))
  (check-type jitter (real 0 1))
  (make-instance 'retry-policy :max-attempts max-attempts :base-delay base-delay
                               :max-delay max-delay :classes classes
                               :jitter jitter))

(defvar *retry-random-state* (make-random-state t)
  "0.7.0: the random state RETRY-DELAY draws jitter from. Tests bind a fixed one.")

(defvar *retry-policy* (make-retry-policy)
  "Default policy for CALL-WITH-RETRY-POLICY and the ai-agent generate step.")

(defgeneric retry-delay (policy condition attempt)
  (:documentation "Seconds to wait before retrying after ATTEMPT (1-based)
failed with CONDITION, or NIL when POLICY does not retry it.")
  (:method ((policy null) condition attempt)
    (declare (ignore condition attempt))
    nil)
  (:method ((policy retry-policy) condition attempt)
    (let ((class (llm-retry-class condition)))
      (when (and class
                 (member class (retry-policy-classes policy))
                 (< attempt (retry-policy-max-attempts policy)))
        (let ((after (and (typep condition 'llm-http-error)
                          (llm-http-error-retry-after condition)))
              (cap (retry-policy-max-delay policy)))
          (if (realp after)
              (min cap (max 0 after))
              (let ((backoff (min cap
                                  (* (retry-policy-base-delay policy)
                                     (expt 2 (1- attempt))
                                     ;; Overloaded servers recover slower than a blip.
                                     (if (eq class :overloaded) 2 1)))))
                (if (plusp (retry-policy-jitter policy))
                    (jittered-delay backoff (retry-policy-jitter policy))
                    backoff))))))))

(defun call-with-retry-policy (thunk &key (policy *retry-policy*) (sleep #'sleep))
  "Call THUNK; when it signals an LLM-ERROR that POLICY retries, unwind, call
SLEEP with the delay and call THUNK again. Other conditions pass through.
Also establishes a RETRY restart (no wait) for outer handlers."
  (let ((attempt 1))
    (loop
      (let ((delay nil))
        (restart-case
            (let ((retry (find-restart 'retry)))
              (return-from call-with-retry-policy
                (handler-bind ((llm-error
                                 (lambda (c)
                                   (let ((d (retry-delay policy c attempt)))
                                     (when d
                                       (setf delay d)
                                       (invoke-restart retry))))))
                  (funcall thunk))))
          (retry ()
            :report "Retry the LLM operation"
            nil))
        (when (and delay (plusp delay))
          (funcall sleep delay))
        (incf attempt)))))

(defmacro with-retry-policy ((&key (policy '*retry-policy*) (sleep '#'sleep)) &body body)
  "Run BODY under CALL-WITH-RETRY-POLICY."
  `(call-with-retry-policy (lambda () ,@body) :policy ,policy :sleep ,sleep))

;;; --- restart helpers -------------------------------------------------------

(defun %report (stream format-control &rest args)
  (apply #'format stream format-control args))

(defun call-with-llm-restarts (thunk)
  "Establish RETRY / USE-VALUE around THUNK."
  (tagbody
   :retry
     (return-from call-with-llm-restarts
       (restart-case (funcall thunk)
         (retry ()
           :report "Retry the LLM operation"
           (go :retry))
         (use-value (value)
           :report "Use a supplied value instead"
           :interactive (lambda ()
                          (format *query-io* "Value to use: ")
                          (force-output *query-io*)
                          (list (read *query-io*)))
           value)))))

(defmacro with-llm-restarts (&body body)
  `(call-with-llm-restarts (lambda () ,@body)))

(defun %invoke-retry ()
  (let ((r (find-restart 'retry)))
    (if r
        (invoke-restart r)
        (error "RETRY restart not active; wrap the call in WITH-LLM-RESTARTS"))))

(defun invoke-retry (&optional condition)
  (let ((r (find-restart 'retry condition)))
    (when r (invoke-restart r))))

(defun invoke-use-value (value &optional condition)
  (let ((r (find-restart 'use-value condition)))
    (when r (invoke-restart r value))))

(defun invoke-ignore-output (&optional condition)
  (let ((r (find-restart 'ignore-output condition)))
    (when r (invoke-restart r))))

(defun invoke-use-partial-response (&optional condition)
  "Invoke USE-PARTIAL-RESPONSE when active (decline otherwise), e.g. from a
HANDLER-BIND on LLM-INCOMPLETE-RESPONSE."
  (let ((r (find-restart 'use-partial-response condition)))
    (when r (invoke-restart r))))

(defun invoke-continue-anyway (&optional condition)
  (let ((r (find-restart 'continue-anyway condition)))
    (when r (invoke-restart r))))

(defun invoke-use-cheaper-model (backend &optional model condition)
  (let ((r (find-restart 'use-cheaper-model condition)))
    (when r
      (if model
          (invoke-restart r backend model)
          (invoke-restart r backend)))))

(defun auto-retry (condition)
  "HANDLER-BIND: RETRY only when LLM-HTTP-ERROR-RETRYABLE-P.
   Decline (return) when the error is not retryable so outer handlers run."
  (when (and (typep condition 'llm-http-error)
             (llm-http-error-retryable-p condition)
             (find-restart 'retry condition))
    (invoke-retry condition)))

(defun auto-ignore-output (condition)
  "HANDLER-BIND: IGNORE-OUTPUT on LLM-OUTPUT-ERROR.
   Decline (return) when the restart is not active so outer handlers run."
  (when (find-restart 'ignore-output condition)
    (invoke-ignore-output condition)))

(defmacro with-auto-retry (&body body)
  `(handler-bind ((llm-http-error #'auto-retry))
     (with-llm-restarts ,@body)))

(defmacro with-auto-ignore-output (&body body)
  `(handler-bind ((llm-output-error #'auto-ignore-output))
     (with-llm-restarts ,@body)))
