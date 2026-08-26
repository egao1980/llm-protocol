(in-package #:llm-protocol)

(defclass llm-backend () ()
  (:documentation "Provider. Concrete backends live in llm-backend-*."))

(defun llm-backend-p (x)
  (typep x 'llm-backend))

(defvar *llm-backend* nil
  "Current backend. Bind or call USE-MOCK-LLM-BACKEND / a backend's USE-* .")

(defun %ensure-backend (&optional (backend *llm-backend*))
  (or backend
      (error 'llm-missing-backend
             :message "*llm-backend* is nil — load a backend or pass one to GENERATE")))

(defgeneric backend-model (backend)
  (:documentation "Default model name for BACKEND, or NIL.")
  (:method ((backend llm-backend)) nil)
  (:method ((backend null))
    (backend-model (%ensure-backend))))

(defgeneric backend-supports-p (backend feature)
  (:documentation "Wire-level probe. Client lookup of what can be done is
capability-protocol: CAPABILITY-SUPPORTED-P on a catalogue (see llm-protocol/capability).")
  (:method ((backend llm-backend) feature)
    (declare (ignore feature))
    nil)
  (:method ((backend null) feature)
    (backend-supports-p (%ensure-backend) feature)))

(defun %as-list (seq)
  (cond
    ((null seq) nil)
    ((stringp seq) (list seq))
    ((vectorp seq) (coerce seq 'list))
    ((listp seq) seq)
    (t (list seq))))

(defun %gref (obj key &optional default)
  (cond
    ((null obj) default)
    ((hash-table-p obj)
     (or (gethash key obj)
         (gethash (substitute #\- #\_ key) obj)
         (let ((alt (cond
                      ((string= key "tool_call_id") (or (gethash "toolCallId" obj)
                                                        (gethash "id" obj)))
                      ((string= key "tool_calls") (gethash "toolCalls" obj))
                      ((string= key "max_tokens") (gethash "maxTokens" obj))
                      (t nil))))
           (or alt default))))
    ((and (consp obj) (keywordp (car obj)))
     (getf obj (intern (string-upcase (substitute #\- #\_ key)) :keyword) default))
    (t default)))

(defun %keywordize (x)
  (cond
    ((null x) nil)
    ((keywordp x) x)
    ((symbolp x) (intern (string-upcase (symbol-name x)) :keyword))
    ((stringp x)
     (intern (string-upcase (substitute #\- #\_ x)) :keyword))
    (t nil)))

(defun %role (x)
  (let ((r (%keywordize (if (stringp x) x x))))
    (cond
      ((member r '(:system :user :assistant :tool)) r)
      ((eq r :function) :tool)
      (t :user))))

(defun %content-text (content)
  (cond
    ((null content) "")
    ((stringp content) content)
    ((llm-text-part-p content) (llm-text-part-text content))
    ((hash-table-p content)
     (or (gethash "text" content) (gethash "content" content) ""))
    ((and (consp content) (keywordp (car content)))
     (or (getf content :text) (getf content :content) ""))
    ((or (vectorp content) (listp content))
     (with-output-to-string (s)
       (dolist (part (%as-list content))
         (let ((chunk (%content-text part)))
           (when (plusp (length chunk))
             (write-string chunk s))))))
    (t (princ-to-string content))))

(defgeneric part-text (part)
  (:method ((part llm-text-part)) (or (llm-text-part-text part) ""))
  (:method ((part llm-thinking-part)) nil)
  (:method ((part llm-part)) nil)
  (:method ((part t)) (%content-text part)))

(defun %coerce-tool-call-part (tc)
  (cond
    ((llm-tool-call-part-p tc) tc)
    ((hash-table-p tc)
     (let ((fn (or (gethash "function" tc) tc)))
       (make-llm-tool-call-part
        :id (%gref tc "id")
        :name (or (%gref fn "name") (%gref tc "name"))
        :arguments (or (%gref fn "arguments") (%gref tc "arguments")
                       (%gref fn "input") "{}"))))
    ((and (consp tc) (keywordp (car tc)))
     (make-llm-tool-call-part :id (getf tc :id) :name (getf tc :name)
                              :arguments (or (getf tc :arguments) "{}")))
    (t (error 'llm-error :message (format nil "not a tool-call part: ~s" tc)))))

(defun %coerce-part (part)
  (cond
    ((llm-part-p part) part)
    ((stringp part) (make-llm-text-part :text part))
    ((hash-table-p part)
     (let ((type (%keywordize (or (gethash "type" part) "text"))))
       (case type
         ((:text :output-text) (make-llm-text-part :text (%content-text part)))
         ((:image :image-url :input-image)
          (make-llm-image-part
           :url (or (gethash "url" part)
                    (let ((iu (gethash "image_url" part)))
                      (if (hash-table-p iu) (gethash "url" iu) iu)))
           :media-type (or (gethash "media_type" part) (gethash "mediaType" part))
           :data (gethash "data" part)))
         ((:tool-call :function-call :tool-use)
          (%coerce-tool-call-part part))
         ((:tool-result :function-call-output)
          (make-llm-tool-result-part
           :id (or (%gref part "tool_call_id") (%gref part "id"))
           :name (or (%gref part "name") (%gref part "tool_name"))
           :content (%content-text (or (gethash "content" part)
                                       (gethash "output" part)
                                       (gethash "result" part)))
           :error-p (or (gethash "is_error" part) (gethash "isError" part))))
         ((:thinking :reasoning :reasoning-content)
          (make-llm-thinking-part
           :text (%content-text (or (gethash "thinking" part) (gethash "text" part)))
           :signature (gethash "signature" part)))
         (t (make-llm-text-part :text (%content-text part))))))
    ((and (consp part) (keywordp (car part)))
     (%coerce-part (let ((h (make-hash-table :test 'equal)))
                     (loop for (k v) on part by #'cddr
                           do (setf (gethash (string-downcase (symbol-name k)) h) v))
                     h)))
    (t (make-llm-text-part :text (%content-text part)))))

(defgeneric coerce-turn (object)
  (:documentation "Normalize OBJECT to an LLM-TURN.")
  (:method ((object llm-turn))
    object)
  (:method ((object string))
    (user-turn object))
  (:method ((object hash-table))
    (let* ((role (%role (or (gethash "role" object) :user)))
           (content (or (gethash "content" object) (gethash "parts" object)))
           (tcs (or (gethash "tool_calls" object) (gethash "toolCalls" object)))
           (thinking (or (gethash "thinking" object)
                         (gethash "reasoning_content" object)
                         (gethash "thinking_blocks" object)))
           (parts (append
                   (mapcar #'%coerce-part
                           (if (and content (not (stringp content))
                                    (or (vectorp content) (listp content)))
                               (%as-list content)
                               (and content (list content))))
                   (and thinking
                        (mapcar (lambda (b)
                                  (if (llm-thinking-part-p b)
                                      b
                                      (make-llm-thinking-part
                                       :text (%content-text
                                              (if (hash-table-p b)
                                                  (or (gethash "thinking" b)
                                                      (gethash "text" b))
                                                  b))
                                       :signature (and (hash-table-p b)
                                                       (gethash "signature" b)))))
                                (%as-list thinking)))
                   (mapcar #'%coerce-tool-call-part (%as-list tcs)))))
      (when (and (eq role :tool) (not (find-if #'llm-tool-result-part-p parts)))
        (push (make-llm-tool-result-part
               :id (%gref object "tool_call_id")
               :name (%gref object "name")
               :content (%content-text content))
              parts))
      (make-llm-turn :role role :parts (or parts (list (make-llm-text-part))))))
  (:method ((object cons))
    (if (keywordp (car object))
        (coerce-turn (let ((h (make-hash-table :test 'equal)))
                       (loop for (k v) on object by #'cddr
                             do (setf (gethash (string-downcase (symbol-name k)) h) v))
                       h))
        (error 'llm-error :message (format nil "not a turn: ~s" object))))
  (:method ((object t))
    (error 'llm-error :message (format nil "not a turn: ~s" object))))

(defun coerce-turns (object)
  "Normalize OBJECT to a list of LLM-TURN.
A string is one user turn. A single turn / plist / hash-table is one-element."
  (cond
    ((null object) nil)
    ((or (stringp object) (llm-turn-p object) (hash-table-p object)
         (and (consp object) (keywordp (car object))))
     (list (coerce-turn object)))
    (t (mapcar #'coerce-turn (%as-list object)))))

(defun turn-text (turn)
  "Concatenate text parts of TURN (not thinking)."
  (with-output-to-string (s)
    (dolist (part (llm-turn-parts (if (llm-turn-p turn) turn (coerce-turn turn))))
      (let ((tx (part-text part)))
        (when (and tx (plusp (length tx)))
          (write-string tx s))))))

(defun last-user-text (turns)
  (let ((text ""))
    (dolist (turn (coerce-turns turns) text)
      (when (eq (llm-turn-role turn) :user)
        (setf text (turn-text turn))))))

(defun coerce-settings (object)
  (etypecase object
    (null nil)
    (llm-settings object)
    (list (apply #'make-llm-settings object))))

(defun llm-response-text (response)
  "Assistant text parts only (not thinking)."
  (when response
    (with-output-to-string (s)
      (dolist (part (llm-response-parts response))
        (let ((tx (part-text part)))
          (when (and tx (plusp (length tx)))
            (write-string tx s)))))))

(defun llm-response-thinking (response)
  (when response
    (with-output-to-string (s)
      (dolist (part (llm-response-parts response))
        (when (llm-thinking-part-p part)
          (let ((tx (llm-thinking-part-text part)))
            (when (and tx (plusp (length tx)))
              (write-string tx s))))))))

(defun llm-response-tool-calls (response)
  (when response
    (remove-if-not #'llm-tool-call-part-p (llm-response-parts response))))

(defgeneric generate (backend turns &key model settings tools tool-choice)
  (:documentation "One-shot generation. TURNS: string, LLM-TURN, or a sequence of those.
TOOLS are descriptors (LLM-TOOL), not executors. SETTINGS is LLM-SETTINGS or a plist.
→ LLM-RESPONSE."))

(defgeneric stream-generate (backend turns &key model settings tools tool-choice on-part)
  (:documentation "Streaming sibling of GENERATE. ON-PART is called with each LLM-PART.
Default method signals LLM-UNSUPPORTED. → LLM-RESPONSE when the stream ends."))

(defgeneric list-models (backend &key)
  (:documentation "→ list of LLM-MODEL-INFO."))

(defmethod generate ((backend null) turns &key model settings tools tool-choice)
  (generate (%ensure-backend) turns :model model :settings settings
            :tools tools :tool-choice tool-choice))

(defmethod stream-generate ((backend null) turns &key model settings tools
                            tool-choice on-part)
  (stream-generate (%ensure-backend) turns :model model :settings settings
                   :tools tools :tool-choice tool-choice :on-part on-part))

(defmethod list-models ((backend null) &key)
  (list-models (%ensure-backend)))

(defmethod generate ((backend llm-backend) turns &key model settings tools tool-choice)
  (declare (ignore turns model settings tools tool-choice))
  (error 'llm-unsupported
         :message (format nil "~a does not implement generate" (class-of backend))))

(defmethod stream-generate ((backend llm-backend) turns &key model settings tools
                            tool-choice on-part)
  (declare (ignore turns model settings tools tool-choice on-part))
  (error 'llm-unsupported
         :message (format nil "~a does not implement stream-generate" (class-of backend))))

(defmethod list-models ((backend llm-backend) &key)
  (error 'llm-unsupported
         :message (format nil "~a does not implement list-models" (class-of backend))))
