(in-package #:llm-protocol)

(defclass llm-backend () ()
  (:documentation "Base class for llm-protocol backends."))

(defvar *llm-backend* nil
  "Current LLM backend. Set by loading llm-backend-* or USE-MOCK-LLM-BACKEND.")

(defun %ensure-backend (&optional (backend *llm-backend*))
  (or backend
      (error 'llm-missing-backend
             :message "*llm-backend* is nil — load a backend or bind generate's backend")))

(defun %gref (obj key &optional default)
  (cond
    ((null obj) default)
    ((hash-table-p obj)
     (or (gethash key obj)
         (let ((alt (cond
                      ((string= key "tool_call_id") (gethash "toolCallId" obj))
                      ((string= key "tool_calls") (gethash "toolCalls" obj))
                      ((string= key "max_tokens") (gethash "maxTokens" obj))
                      (t nil))))
           (or alt default))))
    ((and (listp obj) (keywordp (car obj)))
     (getf obj (intern (string-upcase (substitute #\- #\_ key)) :keyword) default))
    (t default)))

(defun %as-list (seq)
  (cond
    ((null seq) nil)
    ((stringp seq) (list seq))
    ((vectorp seq) (coerce seq 'list))
    ((listp seq) seq)
    (t (list seq))))

(defun %content-text (content)
  (cond
    ((null content) "")
    ((stringp content) content)
    ((hash-table-p content)
     (or (gethash "text" content)
         (gethash "content" content)
         ""))
    ((and (listp content) (keywordp (car content)))
     (or (getf content :text) (getf content :content) ""))
    ((or (vectorp content) (listp content))
     (with-output-to-string (s)
       (dolist (part (%as-list content))
         (let ((chunk (%content-text part)))
           (when (plusp (length chunk))
             (write-string chunk s))))))
    (t (princ-to-string content))))

(defun %coerce-tool-call (tc)
  (cond
    ((llm-tool-call-p tc) tc)
    ((hash-table-p tc)
     (let ((fn (or (gethash "function" tc) tc)))
       (make-llm-tool-call
        :id (%gref tc "id")
        :name (or (%gref fn "name") (%gref tc "name"))
        :arguments (or (%gref fn "arguments") (%gref tc "arguments") "{}"))))
    ((and (listp tc) (keywordp (car tc)))
     (make-llm-tool-call
      :id (getf tc :id)
      :name (getf tc :name)
      :arguments (or (getf tc :arguments) "{}")))
    (t (error 'llm-error :message (format nil "not a tool-call: ~s" tc)))))

(defun %coerce-tool-calls (tcs)
  (mapcar #'%coerce-tool-call (%as-list tcs)))

(defun coerce-message (msg)
  (cond
    ((llm-message-p msg) msg)
    ((stringp msg) (make-llm-message :role "user" :content msg))
    ((hash-table-p msg)
     (make-llm-message
      :role (or (%gref msg "role") "user")
      :content (%content-text (or (%gref msg "content") (%gref msg "text")))
      :name (%gref msg "name")
      :tool-call-id (%gref msg "tool_call_id")
      :tool-calls (%coerce-tool-calls (%gref msg "tool_calls"))))
    ((and (listp msg) (keywordp (car msg)))
     (make-llm-message
      :role (let ((r (getf msg :role)))
              (if r (string-downcase (string r)) "user"))
      :content (%content-text (getf msg :content))
      :name (getf msg :name)
      :tool-call-id (getf msg :tool-call-id)
      :tool-calls (%coerce-tool-calls (getf msg :tool-calls))))
    (t (error 'llm-error :message (format nil "not a message: ~s" msg)))))

(defun coerce-messages (messages)
  "Normalize MESSAGES to a list of LLM-MESSAGE.
Accepts a string, one message, or a sequence of strings / CLOS / hash-tables / plists."
  (cond
    ((null messages) nil)
    ((or (stringp messages) (llm-message-p messages) (hash-table-p messages)
         (and (listp messages) (keywordp (car messages))))
     (list (coerce-message messages)))
    (t (mapcar #'coerce-message (%as-list messages)))))

(defun last-user-text (messages)
  (let ((text ""))
    (dolist (m (coerce-messages messages) text)
      (when (string-equal (llm-message-role m) "user")
        (setf text (or (llm-message-content m) ""))))))

(defgeneric generate (backend messages &key model tools stream temperature
                      max-tokens stop tool-choice)
  (:documentation "One-shot completion. TOOLS optional. STREAM non-nil is wave-1 unsupported.
MESSAGES: string, llm-message, or a sequence of those / JSON objects / plists."))

(defgeneric list-models (backend &key)
  (:documentation "→ list of LLM-MODEL."))

(defmethod generate ((backend null) messages &key model tools stream temperature
                     max-tokens stop tool-choice)
  (generate (%ensure-backend) messages
            :model model :tools tools :stream stream
            :temperature temperature :max-tokens max-tokens
            :stop stop :tool-choice tool-choice))

(defmethod list-models ((backend null) &key)
  (list-models (%ensure-backend)))

(defmethod generate ((backend llm-backend) messages &key model tools stream
                     temperature max-tokens stop tool-choice)
  (declare (ignore messages model tools stream temperature max-tokens
                   stop tool-choice))
  (error 'llm-unsupported
         :message (format nil "~a does not implement generate" (class-of backend))))

(defmethod list-models ((backend llm-backend) &key)
  (error 'llm-unsupported
         :message (format nil "~a does not implement list-models" (class-of backend))))
