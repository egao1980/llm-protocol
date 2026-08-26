(in-package #:llm-protocol)

(defclass llm-message ()
  ((role :initarg :role :accessor llm-message-role :initform "user")
   (content :initarg :content :accessor llm-message-content :initform "")
   (name :initarg :name :accessor llm-message-name :initform nil)
   (tool-call-id :initarg :tool-call-id :accessor llm-message-tool-call-id
                 :initform nil)
   (tool-calls :initarg :tool-calls :accessor llm-message-tool-calls
               :initform nil)))

(defun make-llm-message (&key (role "user") (content "") name tool-call-id tool-calls)
  (make-instance 'llm-message
                 :role role :content content :name name
                 :tool-call-id tool-call-id :tool-calls tool-calls))

(defun llm-message-p (x)
  (typep x 'llm-message))

(defclass llm-tool-call ()
  ((id :initarg :id :accessor llm-tool-call-id :initform nil)
   (name :initarg :name :accessor llm-tool-call-name)
   (arguments :initarg :arguments :accessor llm-tool-call-arguments
              :initform "{}")))

(defun make-llm-tool-call (&key id name (arguments "{}"))
  (make-instance 'llm-tool-call :id id :name name :arguments arguments))

(defun llm-tool-call-p (x)
  (typep x 'llm-tool-call))

(defclass llm-tool ()
  ((name :initarg :name :accessor llm-tool-name)
   (description :initarg :description :accessor llm-tool-description :initform nil)
   (parameters :initarg :parameters :accessor llm-tool-parameters :initform nil)))

(defun make-llm-tool (&key name description parameters)
  (make-instance 'llm-tool :name name :description description
                 :parameters parameters))

(defun llm-tool-p (x)
  (typep x 'llm-tool))

(defclass llm-model ()
  ((id :initarg :id :accessor llm-model-id)
   (owned-by :initarg :owned-by :accessor llm-model-owned-by :initform nil)))

(defun make-llm-model (&key id owned-by)
  (make-instance 'llm-model :id id :owned-by owned-by))

(defun llm-model-p (x)
  (typep x 'llm-model))

(defclass llm-result ()
  ((message :initarg :message :accessor llm-result-message)
   (model :initarg :model :accessor llm-result-model :initform nil)
   (finish-reason :initarg :finish-reason :accessor llm-result-finish-reason
                  :initform nil)
   (usage :initarg :usage :accessor llm-result-usage :initform nil)))

(defun make-llm-result (&key message model finish-reason usage)
  (make-instance 'llm-result :message message :model model
                 :finish-reason finish-reason :usage usage))

(defun llm-result-p (x)
  (typep x 'llm-result))

(defun llm-result-text (result)
  "Assistant text of RESULT, or NIL."
  (let ((msg (and result (llm-result-message result))))
    (and msg (llm-message-content msg))))
