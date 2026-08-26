(in-package #:llm-protocol)

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

(define-condition llm-http-error (llm-error)
  ((status :initarg :status :reader llm-http-error-status :initform nil)
   (body :initarg :body :reader llm-http-error-body :initform nil))
  (:report (lambda (c s)
             (format s "llm HTTP error~@[ ~a~]~@[: ~a~]"
                     (llm-http-error-status c)
                     (or (llm-error-message c) (llm-http-error-body c))))))
