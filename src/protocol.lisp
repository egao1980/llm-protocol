(in-package #:llm-protocol)

(defclass llm-backend () ()
  (:documentation "Provider. Concrete backends live in llm-protocol-* repos."))

(defun llm-backend-p (x)
  (typep x 'llm-backend))

(defvar *llm-backend* nil
  "Current backend. Bind or call USE-MOCK-LLM-BACKEND / a backend's USE-* .")

(defun %ensure-backend (&optional (backend *llm-backend*))
  (or backend
      (restart-case
          (error 'llm-missing-backend
                 :message "*llm-backend* is nil — load a backend or pass one to GENERATE")
        (use-value (value)
          :report "Use a supplied LLM-BACKEND"
          value))))

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
         ((:thinking :reasoning :reasoning-content :reasoning-text)
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

(defun %seq-blocks (x)
  (let ((list (%as-list x)))
    (and list (plusp (length list)) list)))

(defun %block-text (b)
  (if (hash-table-p b)
      (or (gethash "text" b) (gethash "summary_text" b) "")
      b))

(defun %reasoning-text (obj)
  "OpenAI summary_text and LM Studio reasoning_text (empty summary is absent)."
  (unless (hash-table-p obj)
    (return-from %reasoning-text (%content-text obj)))
  (let ((summary (%seq-blocks (gethash "summary" obj)))
        (content (%seq-blocks (gethash "content" obj))))
    (cond
      (summary (%content-text (mapcar #'%block-text summary)))
      (content (%content-text (mapcar #'%block-text content)))
      (t (%content-text (or (gethash "text" obj) (gethash "thinking" obj) obj))))))

(declaim (ftype (function (t) list) turn->items))

(defgeneric coerce-item (object)
  (:documentation "Normalize OBJECT to one LLM-ITEM. A turn that splits into several
items is an error — use COERCE-ITEMS.")
  (:method ((object llm-item))
    object)
  (:method ((object string))
    (make-llm-message-item :role :user :parts (list (make-llm-text-part :text object))))
  (:method ((object hash-table))
    (let ((type (%keywordize (or (gethash "type" object) "message"))))
      (case type
        ((:message)
         (make-llm-message-item
          :id (gethash "id" object)
          :role (%role (or (gethash "role" object) :user))
          :parts (mapcar #'%coerce-part
                         (%as-list (or (gethash "content" object)
                                       (gethash "parts" object))))))
        ((:function-call :tool-call)
         (make-llm-function-call-item
          :id (gethash "id" object)
          :call-id (or (gethash "call_id" object) (gethash "callId" object)
                       (gethash "id" object))
          :name (gethash "name" object)
          :arguments (or (gethash "arguments" object) "{}")))
        ((:function-call-output :tool-result)
         (make-llm-function-call-output-item
          :id (gethash "id" object)
          :call-id (or (gethash "call_id" object) (gethash "callId" object)
                       (gethash "tool_call_id" object) (gethash "id" object))
          :output (%content-text (or (gethash "output" object)
                                     (gethash "content" object)))))
        ((:reasoning :thinking)
         (make-llm-reasoning-item
          :id (gethash "id" object)
          :text (%reasoning-text object)
          :signature (gethash "signature" object)))
        (t (let ((items (turn->items (coerce-turn object))))
             (if (and items (null (rest items)))
                 (first items)
                 (error 'llm-error
                        :message (format nil "not a single item: ~s" object))))))))
  (:method ((object cons))
    (if (keywordp (car object))
        (coerce-item (let ((h (make-hash-table :test 'equal)))
                       (loop for (k v) on object by #'cddr
                             do (setf (gethash (string-downcase (symbol-name k)) h) v))
                       h))
        (error 'llm-error :message (format nil "not an item: ~s" object))))
  (:method ((object llm-turn))
    (let ((items (turn->items object)))
      (if (and items (null (rest items)))
          (first items)
          (error 'llm-error
                 :message "turn expanded to multiple items — use COERCE-ITEMS"))))
  (:method ((object t))
    (error 'llm-error :message (format nil "not an item: ~s" object))))

(defun turn->items (turn)
  (let* ((turn (if (llm-turn-p turn) turn (coerce-turn turn)))
         (role (llm-turn-role turn)))
    (if (eq role :tool)
        (or (mapcar (lambda (p)
                      (make-llm-function-call-output-item
                       :call-id (llm-tool-result-part-id p)
                       :output (or (llm-tool-result-part-content p) "")))
                    (remove-if-not #'llm-tool-result-part-p (llm-turn-parts turn)))
            (list (make-llm-function-call-output-item
                   :call-id nil :output (turn-text turn))))
        (let ((msg-parts nil)
              (extra nil))
          (dolist (p (llm-turn-parts turn))
            (cond
              ((llm-thinking-part-p p)
               (push (make-llm-reasoning-item :text (or (llm-thinking-part-text p) "")
                                              :signature (llm-thinking-part-signature p))
                     extra))
              ((llm-tool-call-part-p p)
               (push (make-llm-function-call-item
                      :id (llm-tool-call-part-id p)
                      :call-id (llm-tool-call-part-id p)
                      :name (llm-tool-call-part-name p)
                      :arguments (llm-tool-call-part-arguments p))
                     extra))
              (t (push p msg-parts))))
          (append (and msg-parts
                       (list (make-llm-message-item :role role
                                                    :parts (nreverse msg-parts))))
                  (nreverse extra))))))

(defun coerce-items (object)
  "Normalize OBJECT to a list of LLM-ITEM. A string is one user message item."
  (cond
    ((null object) nil)
    ((or (stringp object) (llm-item-p object) (llm-turn-p object)
         (hash-table-p object)
         (and (consp object) (keywordp (car object))))
     (if (llm-turn-p object)
         (turn->items object)
         (list (coerce-item object))))
    (t (mapcan #'coerce-items (%as-list object)))))

(defun turns->items (turns)
  (mapcan #'turn->items (coerce-turns turns)))

(defun items->turns (items)
  "Regroup Responses items into chat turns (mock / generate fallback)."
  (let ((turns nil)
        (pending nil))
    (labels ((flush ()
               (when pending
                 (push (make-llm-turn :role :assistant :parts (nreverse pending)) turns)
                 (setf pending nil))))
      (dolist (it (coerce-items items) (progn (flush) (nreverse turns)))
        (etypecase it
          (llm-message-item
           (if (eq (llm-message-item-role it) :assistant)
               (dolist (p (reverse (copy-list (llm-message-item-parts it))))
                 (push p pending))
               (progn
                 (flush)
                 (push (make-llm-turn :role (llm-message-item-role it)
                                      :parts (copy-list (llm-message-item-parts it)))
                       turns))))
          (llm-function-call-item
           (push (make-llm-tool-call-part
                  :id (or (llm-function-call-item-call-id it) (llm-item-id it))
                  :name (llm-function-call-item-name it)
                  :arguments (or (llm-function-call-item-arguments it) "{}"))
                 pending))
          (llm-reasoning-item
           (push (make-llm-thinking-part :text (or (llm-reasoning-item-text it) "")
                                         :signature (llm-reasoning-item-signature it))
                 pending))
          (llm-function-call-output-item
           (flush)
           (push (tool-turn (llm-function-call-output-item-call-id it)
                            (or (llm-function-call-output-item-output it) ""))
                 turns)))))))

(defun %assistant-items-from-response (response)
  (turns->items (list (make-llm-turn :role :assistant
                                     :parts (copy-list (llm-response-parts response))))))


(defun coerce-settings (object)
  (etypecase object
    (null nil)
    (llm-settings object)
    (list (apply #'make-llm-settings object))))

(defun copy-llm-settings (settings &key (output nil outputp))
  (make-llm-settings
   :temperature (llm-settings-temperature settings)
   :max-tokens (llm-settings-max-tokens settings)
   :stop (llm-settings-stop settings)
   :top-p (llm-settings-top-p settings)
   :response-format (llm-settings-response-format settings)
   :output (if outputp output (llm-settings-output settings))
   :extra (llm-settings-extra settings)))

(defun %settings-with-output (settings output)
  (let ((s (coerce-settings settings)))
    (cond
      ((null output) s)
      ((null s) (make-llm-settings :output output))
      (t (copy-llm-settings s :output output)))))

(defun llm-response-text (response)
  "Assistant text parts only (not thinking)."
  (when response
    (with-output-to-string (s)
      (dolist (part (llm-response-parts response))
        (let ((tx (part-text part)))
          (when (and tx (plusp (length tx)))
            (write-string tx s)))))))

(defun llm-response-content (response)
  "Content blocks (same object as LLM-RESPONSE-PARTS)."
  (and response (llm-response-parts response)))

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

(defgeneric structured-output-json-schema (schema)
  (:documentation "JSON Schema hash-table for SCHEMA (OpenAI json_schema.schema).
Hash-tables pass through. CLOS schemas need llm-protocol/schema (schema-protocol-json).")
  (:method ((schema hash-table))
    schema)
  (:method (schema)
    (error 'llm-output-error
           :message (format nil "load llm-protocol/schema to emit JSON Schema for ~s" schema))))

(defgeneric parse-structured-output (schema source)
  (:documentation "Parse SOURCE (JSON string or table) into SCHEMA.
NIL schema = JSON if SOURCE looks like JSON, else NIL.
CLOS designators need llm-protocol/schema.")
  (:method ((schema null) source)
    (try-parse-json-output source))
  (:method ((schema hash-table) (source hash-table))
    (declare (ignore schema))
    source)
  (:method ((schema hash-table) (source string))
    (or (try-parse-json-output source) source))
  (:method (schema source)
    (declare (ignore source))
    (error 'llm-output-error
           :message (format nil "load llm-protocol/schema to parse output as ~s" schema))))

(defun %trimmed-text (source)
  (cond
    ((null source) "")
    ((stringp source) (string-trim '(#\Space #\Tab #\Newline #\Return) source))
    (t "")))

(defun %looks-like-json (source)
  (let ((s (%trimmed-text source)))
    (and (plusp (length s))
         (let ((c (char s 0)))
           (or (char= c #\{) (char= c #\[))))))

(defun try-parse-json-output (source)
  "Decode SOURCE as JSON when it looks like an object/array. Else NIL.
   Soft-uses json-protocol when a backend is bound."
  (cond
    ((hash-table-p source) source)
    ((and (vectorp source) (not (stringp source))) source)
    ((not (%looks-like-json source)) nil)
    (t
     (let* ((pkg (find-package '#:json-protocol))
            (decode (and pkg (find-symbol "DECODE" pkg)))
            (backend (and pkg (find-symbol "*JSON-BACKEND*" pkg))))
       (when (and decode (fboundp decode) backend (symbol-value backend))
         (ignore-errors (funcall decode (%trimmed-text source))))))))

(defun %signal-output-error (response err &optional cause)
  (when (typep err 'llm-output-error)
    (setf (llm-output-error-response err) response))
  (let ((condition (if (typep err 'llm-output-error)
                       err
                       (make-condition 'llm-output-error
                                       :message (princ-to-string err)
                                       :response response
                                       :cause (or cause err)))))
    (restart-case (error condition)
      (use-value (parsed)
        :report "Use a supplied parsed output"
        :interactive (lambda ()
                       (format *query-io* "Parsed output: ")
                       (force-output *query-io*)
                       (list (read *query-io*)))
        (setf (llm-response-output response) parsed)
        parsed)
      (ignore-output ()
        :report "Leave LLM-RESPONSE-OUTPUT NIL"
        nil))))

(defun %attach-structured-output (response settings)
  (when (and response (null (llm-response-output response)))
    (let ((schema (and settings (llm-settings-output settings)))
          (text (llm-response-text response)))
      (if schema
          (handler-case
              (let ((out (parse-structured-output schema text)))
                (when out
                  (setf (llm-response-output response) out)))
            (llm-output-error (e)
              (%signal-output-error response e))
            (error (e)
              (%signal-output-error response e e)))
          (let ((out (try-parse-json-output text)))
            (when out
              (setf (llm-response-output response) out))))))
  response)

(defgeneric generate (backend turns &key model settings tools tool-choice output)
  (:documentation "One-shot generation. TURNS: string, LLM-TURN, or a sequence of those.
TOOLS are descriptors (LLM-TOOL), not executors. SETTINGS is LLM-SETTINGS or a plist.
OUTPUT is a schema-protocol designator (or JSON Schema hash) — parsed into
LLM-RESPONSE-OUTPUT. → LLM-RESPONSE."))

(defgeneric stream-generate (backend turns &key model settings tools tool-choice
                             on-part output)
  (:documentation "Streaming sibling of GENERATE. ON-PART is called with each LLM-PART.
Default method signals LLM-UNSUPPORTED. → LLM-RESPONSE when the stream ends."))

(defgeneric respond (backend items &key model settings tools tool-choice output)
  (:documentation "Responses-style one-shot. ITEMS: string, LLM-ITEM, LLM-TURN, or a
sequence. Default method is ITEMS->TURNS then GENERATE. → LLM-RESPONSE (ITEMS + PARTS)."))

(defgeneric stream-respond (backend items &key model settings tools tool-choice
                            on-part output)
  (:documentation "Streaming sibling of RESPOND. Default: STREAM-GENERATE after ITEMS->TURNS."))

(defgeneric list-models (backend &key)
  (:documentation "→ list of LLM-MODEL-INFO."))

(defun %call-with-output (fn backend payload args)
  (with-llm-restarts
    (let* ((settings (getf args :settings))
           (output (getf args :output))
           (effective (%settings-with-output settings output))
           (pass (loop for (k v) on args by #'cddr
                       unless (member k '(:settings :output))
                         collect k and collect v))
           (r (apply fn backend payload :settings effective pass)))
      (%attach-structured-output r effective)
      r)))

(defmethod generate :around ((backend llm-backend) turns &rest args
                             &key &allow-other-keys)
  (%call-with-output #'call-next-method backend turns args))

(defmethod stream-generate :around ((backend llm-backend) turns &rest args
                                    &key &allow-other-keys)
  (%call-with-output #'call-next-method backend turns args))

(defmethod respond :around ((backend llm-backend) items &rest args
                            &key &allow-other-keys)
  (%call-with-output #'call-next-method backend items args))

(defmethod stream-respond :around ((backend llm-backend) items &rest args
                                   &key &allow-other-keys)
  (%call-with-output #'call-next-method backend items args))

(defmethod generate ((backend null) turns &rest args &key &allow-other-keys)
  (apply #'generate (%ensure-backend) turns args))

(defmethod stream-generate ((backend null) turns &rest args &key &allow-other-keys)
  (apply #'stream-generate (%ensure-backend) turns args))

(defmethod respond ((backend null) items &rest args &key &allow-other-keys)
  (apply #'respond (%ensure-backend) items args))

(defmethod stream-respond ((backend null) items &rest args &key &allow-other-keys)
  (apply #'stream-respond (%ensure-backend) items args))

(defmethod list-models :around ((backend llm-backend) &key)
  (with-llm-restarts
    (call-next-method)))

(defmethod list-models ((backend null) &key)
  (list-models (%ensure-backend)))

(defmethod generate ((backend llm-backend) turns &key model settings tools tool-choice
                     output)
  (declare (ignore turns model settings tools tool-choice output))
  (error 'llm-unsupported
         :message (format nil "~a does not implement generate" (class-of backend))))

(defmethod stream-generate ((backend llm-backend) turns &key model settings tools
                            tool-choice on-part output)
  (declare (ignore turns model settings tools tool-choice on-part output))
  (error 'llm-unsupported
         :message (format nil "~a does not implement stream-generate" (class-of backend))))

(defmethod respond ((backend llm-backend) items &key model settings tools tool-choice
                    output)
  (let ((r (generate backend (items->turns items)
                     :model model :settings settings
                     :tools tools :tool-choice tool-choice :output output)))
    (unless (llm-response-items r)
      (setf (llm-response-items r) (%assistant-items-from-response r)))
    r))

(defmethod stream-respond ((backend llm-backend) items &key model settings tools
                           tool-choice on-part output)
  (let ((r (stream-generate backend (items->turns items)
                            :model model :settings settings
                            :tools tools :tool-choice tool-choice
                            :on-part on-part :output output)))
    (unless (llm-response-items r)
      (setf (llm-response-items r) (%assistant-items-from-response r)))
    r))

(defmethod list-models ((backend llm-backend) &key)
  (error 'llm-unsupported
         :message (format nil "~a does not implement list-models" (class-of backend))))
