(in-package #:llm-protocol)

(defclass mock-llm-backend (llm-backend)
  ((prefix :initarg :prefix :accessor mock-llm-prefix :initform "echo: ")
   (handler :initarg :handler :accessor mock-llm-handler :initform nil)
   (models :initarg :models :accessor mock-llm-models
           :initform (list (make-llm-model-info :id "mock" :owned-by "llm-protocol")))
   (tool-calls :initarg :tool-calls :accessor mock-llm-tool-calls :initform nil)
   (default-model :initarg :default-model :accessor mock-llm-default-model
                  :initform "mock")))

(defun make-mock-llm-backend (&key (prefix "echo: ") handler models tool-calls
                                (default-model "mock"))
  (make-instance 'mock-llm-backend
                 :prefix prefix :handler handler
                 :models (or models
                             (list (make-llm-model-info :id default-model
                                                        :owned-by "llm-protocol")))
                 :tool-calls tool-calls
                 :default-model default-model))

(defun use-mock-llm-backend (&rest args &key &allow-other-keys)
  (setf *llm-backend* (apply #'make-mock-llm-backend args)))

(defmethod backend-model ((backend mock-llm-backend))
  (mock-llm-default-model backend))

(defmethod backend-supports-p ((backend mock-llm-backend) (feature (eql :tools)))
  t)

(defmethod backend-supports-p ((backend mock-llm-backend) (feature (eql :stream)))
  t)

(defmethod backend-supports-p ((backend mock-llm-backend) (feature (eql :responses)))
  t)

(defmethod generate ((backend mock-llm-backend) turns &key model settings tools
                     tool-choice)
  (let ((normalized (coerce-turns turns)))
    (if (mock-llm-handler backend)
        (funcall (mock-llm-handler backend) backend normalized
                 :model model :settings settings :tools tools :tool-choice tool-choice)
        (let* ((text (last-user-text normalized))
               (tcs (mapcar (lambda (tc)
                              (if (llm-tool-call-part-p tc)
                                  tc
                                  (%coerce-tool-call-part tc)))
                            (%as-list (mock-llm-tool-calls backend))))
               (parts (if tcs
                          tcs
                          (list (make-llm-text-part
                                 :text (concatenate 'string
                                                    (mock-llm-prefix backend)
                                                    text))))))
          (make-llm-response
           :parts parts
           :model (or model (mock-llm-default-model backend))
           :finish-reason (if tcs :tool-use :stop))))))

(defmethod stream-generate ((backend mock-llm-backend) turns &key model settings
                            tools tool-choice on-part)
  (let ((response (generate backend turns :model model :settings settings
                            :tools tools :tool-choice tool-choice)))
    (when on-part
      (dolist (part (llm-response-parts response))
        (funcall on-part part)))
    response))

(defmethod list-models ((backend mock-llm-backend) &key)
  (copy-list (mock-llm-models backend)))
