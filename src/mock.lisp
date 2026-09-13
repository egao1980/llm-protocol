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

(defmethod backend-supports-p ((backend mock-llm-backend) (feature (eql :structured-output)))
  t)

(defmethod backend-supports-p ((backend mock-llm-backend) (feature (eql :responses)))
  t)

(defmethod backend-supports-p ((backend mock-llm-backend) (feature (eql :embeddings)))
  t)

(defun %mock-embedding-vector (text dimensions)
  (let* ((n (or dimensions 8))
         (v (make-array n :element-type 'single-float :initial-element 0f0)))
    (loop for i from 0 below (min n (length text))
          do (setf (aref v i) (float (mod (char-code (char text i)) 256) 1f0)))
    v))

(defmethod embed ((backend mock-llm-backend) inputs &key model dimensions
                  encoding-format)
  (when (and encoding-format
             (not (member encoding-format '(:float "float") :test #'equal)))
    (error 'llm-unsupported
           :message (format nil "mock embed is float-only, got ~s" encoding-format)))
  (let ((texts (coerce-embed-inputs inputs)))
    (make-llm-embed-result
     :embeddings (loop for text in texts for i from 0
                       collect (make-llm-embedding
                                :vector (%mock-embedding-vector text dimensions)
                                :index i))
     :model (or model (mock-llm-default-model backend))
     :usage (make-llm-usage
             :input-tokens (reduce #'+ texts :key #'length :initial-value 0)
             :total-tokens (reduce #'+ texts :key #'length :initial-value 0)))))

(defmethod generate ((backend mock-llm-backend) turns &key model settings tools
                     tool-choice output)
  (declare (ignore output))
  (let ((normalized (coerce-turns turns)))
    (if (mock-llm-handler backend)
        (funcall (mock-llm-handler backend) backend normalized
                 :model model :settings settings :tools tools :tool-choice tool-choice)
        (let* ((user-text (last-user-text normalized))
               (tcs (mapcar (lambda (tc)
                              (if (llm-tool-call-part-p tc)
                                  tc
                                  (%coerce-tool-call-part tc)))
                            (%as-list (mock-llm-tool-calls backend))))
               (out-text (if tcs
                             ""
                             (concatenate 'string (mock-llm-prefix backend)
                                          user-text)))
               (parts (if tcs
                          tcs
                          (list (make-llm-text-part :text out-text))))
               (in (count-tokens backend normalized))
               (out (count-tokens backend out-text)))
          (make-llm-response
           :parts parts
           :model (or model (mock-llm-default-model backend))
           :finish-reason (if tcs :tool-use :stop)
           :usage (make-llm-usage :input-tokens in :output-tokens out
                                  :total-tokens (+ in out)))))))

(defmethod stream-generate ((backend mock-llm-backend) turns &key model settings
                            tools tool-choice on-part output)
  (let ((response (generate backend turns :model model :settings settings
                            :tools tools :tool-choice tool-choice :output output)))
    (when on-part
      (dolist (part (llm-response-parts response))
        (funcall on-part part)))
    response))

(defmethod list-models ((backend mock-llm-backend) &key)
  (copy-list (mock-llm-models backend)))
