(in-package #:llm-protocol)

(defclass llm-generation-adapter (capability-protocol:llm-generation-capability)
  ((backend :initarg :backend :accessor llm-generation-backend :initform nil))
  (:documentation "Implements :llm-generation COMPLETE via GENERATE."))

(defun make-llm-generation-adapter (&key backend)
  (make-instance 'llm-generation-adapter :backend backend))

(defmethod capability-protocol:complete ((cap llm-generation-adapter) prompt
                                         &key model settings)
  (generate (or (llm-generation-backend cap) *llm-backend*)
            prompt :model model :settings (coerce-settings settings)))
