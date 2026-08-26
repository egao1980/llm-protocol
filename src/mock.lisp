(in-package #:llm-protocol)

(defclass mock-llm-backend (llm-backend)
  ((prefix :initarg :prefix :accessor mock-llm-prefix :initform "echo: ")
   (handler :initarg :handler :accessor mock-llm-handler :initform nil)
   (models :initarg :models :accessor mock-llm-models
           :initform (list (make-llm-model :id "mock" :owned-by "llm-protocol")))
   (tool-calls :initarg :tool-calls :accessor mock-llm-tool-calls :initform nil)
   (default-model :initarg :default-model :accessor mock-llm-default-model
                  :initform "mock")))

(defun make-mock-llm-backend (&key (prefix "echo: ") handler models tool-calls
                                (default-model "mock"))
  (make-instance 'mock-llm-backend
                 :prefix prefix :handler handler
                 :models (or models (list (make-llm-model :id default-model
                                                          :owned-by "llm-protocol")))
                 :tool-calls tool-calls
                 :default-model default-model))

(defun use-mock-llm-backend (&rest args &key &allow-other-keys)
  (setf *llm-backend* (apply #'make-mock-llm-backend args)))

(defmethod generate ((backend mock-llm-backend) messages &key model tools stream
                     temperature max-tokens stop tool-choice)
  (when stream
    (error 'llm-unsupported :message "mock backend does not stream"))
  (let ((normalized (coerce-messages messages)))
    (if (mock-llm-handler backend)
        (funcall (mock-llm-handler backend) backend normalized
                 :model model :tools tools :temperature temperature
                 :max-tokens max-tokens :stop stop :tool-choice tool-choice)
        (let* ((text (last-user-text normalized))
               (tcs (mock-llm-tool-calls backend))
               (msg (make-llm-message
                     :role "assistant"
                     :content (if tcs "" (concatenate 'string (mock-llm-prefix backend) text))
                     :tool-calls tcs)))
          (make-llm-result
           :message msg
           :model (or model (mock-llm-default-model backend))
           :finish-reason (if tcs "tool_calls" "stop"))))))

(defmethod list-models ((backend mock-llm-backend) &key)
  (copy-list (mock-llm-models backend)))
