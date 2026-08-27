(in-package #:llm-protocol)

;;; Conditions + interactive/programmatic restarts (pathlib shape).
;;;
;;; Typical recoveries:
;;;   retry         — re-run GENERATE / RESPOND / LIST-MODELS (outer WITH-LLM-RESTARTS)
;;;   use-value     — supply an LLM-RESPONSE (or parsed output on LLM-OUTPUT-ERROR)
;;;   ignore-output — leave LLM-RESPONSE-OUTPUT NIL on a parse failure
;;;
;;; RETRY is for transient HTTP (429 / 408 / 409 / 5xx). Do not default-retry
;;; structured-output parse failures.

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

(defun http-status-retryable-p (status)
  "T for 408 / 409 / 429 / 5xx."
  (and (integerp status)
       (or (= status 408) (= status 409) (= status 429) (>= status 500))))

(define-condition llm-http-error (llm-error)
  ((status :initarg :status :reader llm-http-error-status :initform nil)
   (body :initarg :body :reader llm-http-error-body :initform nil)
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
