(in-package #:llm-protocol)

(defclass llm-generation-adapter (capability-protocol:llm-generation-capability)
  ((backend :initarg :backend :accessor llm-generation-backend :initform nil))
  (:documentation "Implements :llm-generation COMPLETE / STREAM-COMPLETE via GENERATE."))

(defun make-llm-generation-adapter (&key backend)
  (make-instance 'llm-generation-adapter :backend backend))

(defun %cap-backend (cap)
  (or (llm-generation-backend cap) *llm-backend*))

(defmethod capability-protocol:complete ((cap llm-generation-adapter) prompt
                                         &key model settings)
  (generate (%cap-backend cap) prompt :model model
            :settings (coerce-settings settings)))

(defmethod capability-protocol:stream-complete ((cap llm-generation-adapter) prompt
                                                &key model settings on-part)
  (stream-generate (%cap-backend cap) prompt :model model
                   :settings (coerce-settings settings) :on-part on-part))

(defmethod capability-protocol:capability-operations ((cap llm-generation-adapter))
  (let ((ops (call-next-method))
        (b (%cap-backend cap)))
    (if (and b (backend-supports-p b :stream))
        ops
        (remove 'capability-protocol:stream-complete ops
                :key #'capability-protocol:capability-operation-name))))

(defparameter +llm-support-capabilities+
  '((:tools . capability-protocol:llm-tools-capability)
    (:vision . capability-protocol:llm-vision-capability)
    (:audio . capability-protocol:llm-audio-capability)
    (:video . capability-protocol:llm-video-capability)
    (:files . capability-protocol:llm-files-capability)
    (:speech . capability-protocol:llm-speech-capability)
    (:thinking . capability-protocol:llm-thinking-capability)
    (:structured-output . capability-protocol:llm-structured-output-capability)
    (:responses . capability-protocol:llm-responses-capability)))

(defun make-llm-catalogue (backend)
  "Live :llm catalogue with instances this BACKEND actually implements.
Query with CAPABILITY-SUPPORTED-P / GET-CAPABILITY / LIST-CAPABILITIES."
  (let ((cat (capability-protocol:make-catalogue :llm)))
    (capability-protocol:register-capability
     cat (make-llm-generation-adapter :backend backend))
    (dolist (pair +llm-support-capabilities+)
      (when (backend-supports-p backend (car pair))
        (capability-protocol:register-capability
         cat (make-instance (cdr pair)))))
    cat))

(defun register-llm-backend (bb backend)
  "Register BACKEND's :llm catalogue instances onto blackboard BB."
  (let ((cat (make-llm-catalogue backend)))
    (dolist (row (capability-protocol:list-capabilities cat))
      (capability-protocol:register-capability
       bb (capability-protocol:get-capability cat (getf row :name))))
    cat))
