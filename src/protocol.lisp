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

(defun copy-llm-settings (settings &key (output nil outputp)
                                     (output-repair nil output-repair-p))
  (make-llm-settings
   :temperature (llm-settings-temperature settings)
   :max-tokens (llm-settings-max-tokens settings)
   :stop (llm-settings-stop settings)
   :top-p (llm-settings-top-p settings)
   :response-format (llm-settings-response-format settings)
   :output (if outputp output (llm-settings-output settings))
   :output-repair (if output-repair-p
                      output-repair
                      (llm-settings-output-repair settings))
   :extra (llm-settings-extra settings)))

(defun %settings-with-output (settings output)
  (let ((s (coerce-settings settings)))
    (cond
      ((null output) s)
      ((null s) (make-llm-settings :output output))
      (t (copy-llm-settings s :output output)))))

(defun %settings-with-repair (settings output-repair)
  (cond
    ((null settings) (make-llm-settings :output-repair output-repair))
    (t (copy-llm-settings settings :output-repair output-repair))))

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

(defun %try-decode-json-text (text)
  (when (%looks-like-json text)
    (let* ((pkg (find-package '#:json-protocol))
           (decode (and pkg (find-symbol "DECODE" pkg)))
           (backend (and pkg (find-symbol "*JSON-BACKEND*" pkg))))
      (when (and decode (fboundp decode) backend (symbol-value backend))
        (ignore-errors (funcall decode text))))))

(defun %fence-body (text)
  "Body of the first markdown fence (``` / ```json), or NIL."
  (let ((start (search "```" text)))
    (when start
      (let* ((after-open (+ start 3))
             (nl (position #\Newline text :start after-open))
             (body-start (if nl (1+ nl) after-open))
             (close (search "```" text :start2 body-start)))
        (when close
          (string-trim '(#\Space #\Tab #\Newline #\Return)
                       (subseq text body-start close)))))))

(defun %extract-balanced-json (text start)
  "Substring of TEXT from START that is one JSON object or array."
  (declare (type string text))
  (let ((n (length text)))
    (when (and (integerp start) (< -1 start n))
      (let ((open (char text start)))
        (when (or (char= open #\{) (char= open #\[))
          (let ((depth 0)
                (in-string nil)
                (escape nil))
            (loop for i from start below n
                  for c = (char text i)
                  do (cond
                       (escape
                        (setf escape nil))
                       ((char= c #\\)
                        (when in-string (setf escape t)))
                       ((char= c #\")
                        (setf in-string (not in-string)))
                       (in-string
                        nil)
                       ((or (char= c #\{) (char= c #\[))
                        (incf depth))
                       ((or (char= c #\}) (char= c #\]))
                        (decf depth)
                        (when (zerop depth)
                          (return (subseq text start (1+ i)))))))))))))

(defun %first-json-index (text)
  (loop for i from 0 below (length text)
        for c = (char text i)
        when (or (char= c #\{) (char= c #\[))
          return i))

(defun %extract-json-text (source)
  "Best-effort JSON object/array substring: fences, then first balanced value."
  (let ((s (%trimmed-text source)))
    (when (plusp (length s))
      (or (and (%looks-like-json s)
               (or (%extract-balanced-json s 0) s))
          (let ((fence (%fence-body s)))
            (when (and fence (plusp (length fence)))
              (or (%extract-json-text fence) fence)))
          (let ((i (%first-json-index s)))
            (and i (%extract-balanced-json s i)))))))

(defun try-parse-json-output (source &key relaxed)
  "Decode SOURCE as JSON when it looks like an object/array. Else NIL.
   Soft-uses json-protocol when a backend is bound.
   RELAXED T: extract the first JSON object/array from mixed text or
   markdown fences (``` / ```json) before decoding."
  (cond
    ((hash-table-p source) source)
    ((and (vectorp source) (not (stringp source))) source)
    (t
     (or (%try-decode-json-text (%trimmed-text source))
         (and relaxed
              (let ((extracted (%extract-json-text source)))
                (and extracted (%try-decode-json-text extracted))))))))

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

(defvar *structured-output-repair* :repair
  "Policy when GENERATE / RESPOND is given :OUTPUT and parse fails.

  :REPAIR   — one extra GENERATE (raw completion + schema hint), then
              relaxed parse, then text fallback (output stays NIL).
  :RELAXED  — skip the extra GENERATE; extract JSON from mixed text /
              markdown fences, then text fallback.
  :FALLBACK — leave LLM-RESPONSE-OUTPUT NIL; do not signal.
  :SIGNAL   — signal LLM-OUTPUT-ERROR (IGNORE-OUTPUT / USE-VALUE).
  NIL / :OFF / :ERROR are :SIGNAL. T is :REPAIR.

  Per-call override: GENERATE :OUTPUT-REPAIR or LLM-SETTINGS-OUTPUT-REPAIR.
  This is not HTTP RETRY.")

(defvar *%structured-output-repairing* nil
  "Bound T while the one extra repair GENERATE is in flight.")

(defun %normalize-repair-policy (policy)
  (cond
    ((null policy) :signal)
    ((eq policy t) :repair)
    ((eq policy :repair) :repair)
    ((eq policy :relaxed) :relaxed)
    ((member policy '(:fallback :ignore) :test #'eq) :fallback)
    ((member policy '(:signal :off :error) :test #'eq) :signal)
    ((eq policy :inherit)
     (let ((v *structured-output-repair*))
       (if (eq v :inherit) :repair (%normalize-repair-policy v))))
    (t :repair)))

(defun %effective-repair-policy (settings)
  (let ((from (if settings (llm-settings-output-repair settings) :inherit)))
    (if (eq from :inherit)
        (%normalize-repair-policy *structured-output-repair*)
        (%normalize-repair-policy from))))

(defun %as-turns (payload)
  (cond
    ((null payload) nil)
    ((or (llm-item-p payload)
         (and (consp payload) (not (keywordp (car payload)))
              (llm-item-p (first payload))))
     (items->turns (coerce-items payload)))
    (t (coerce-turns payload))))

(defun %schema-hint (schema)
  (or (ignore-errors
        (let ((js (structured-output-json-schema schema)))
          (when (hash-table-p js)
            (let* ((pkg (find-package '#:json-protocol))
                   (encode (and pkg (find-symbol "ENCODE" pkg)))
                   (backend (and pkg (find-symbol "*JSON-BACKEND*" pkg))))
              (when (and encode (fboundp encode) backend (symbol-value backend))
                (funcall encode js))))))
      (prin1-to-string schema)))

(defparameter +structured-output-repair-preamble+
  "The previous completion was not valid structured output.")

(defun %repair-turns (payload raw-text schema)
  (append (%as-turns payload)
          (list (assistant-turn (or raw-text ""))
                (user-turn
                 (format nil "~a~%Previous completion:~%~a~%~%~
Reply with JSON only (no markdown fences) matching this schema:~%~a"
                         +structured-output-repair-preamble+
                         (or raw-text "")
                         (%schema-hint schema))))))

(defun %repair-structured-output (backend payload args settings response policy)
  "Apply remaining repair steps. Returns RESPONSE or the repair GENERATE result."
  (let ((schema (llm-settings-output settings)))
    (when (eq policy :repair)
      (let* ((raw (or (llm-response-text response) ""))
             (*%structured-output-repairing* t)
             (repaired
              (generate backend (%repair-turns payload raw schema)
                        :model (getf args :model)
                        :settings settings
                        :tools (getf args :tools)
                        :tool-choice (getf args :tool-choice)
                        :output schema
                        :output-repair :fallback)))
        (when (and repaired (llm-response-output repaired))
          (return-from %repair-structured-output repaired))
        (%attach-structured-output repaired settings :relaxed t :on-error :decline)
        (when (and repaired (llm-response-output repaired))
          (return-from %repair-structured-output repaired))))
    (when (member policy '(:repair :relaxed) :test #'eq)
      (%attach-structured-output response settings :relaxed t :on-error :decline)
      (when (and response (llm-response-output response))
        (return-from %repair-structured-output response)))
    response))

(defun %attach-structured-output (response settings &key relaxed (on-error :signal))
  (when (and response (null (llm-response-output response)))
    (let ((schema (and settings (llm-settings-output settings)))
          (text (llm-response-text response)))
      (if schema
          (handler-case
              (let* ((source (if relaxed
                                 (or (%extract-json-text text) text)
                                 text))
                     (out (parse-structured-output schema source)))
                (when out
                  (setf (llm-response-output response) out)))
            (llm-output-error (e)
              (if (eq on-error :signal)
                  (%signal-output-error response e)
                  nil))
            (error (e)
              (if (eq on-error :signal)
                  (%signal-output-error response e e)
                  nil)))
          (let ((out (try-parse-json-output text :relaxed relaxed)))
            (when out
              (setf (llm-response-output response) out))))))
  response)

(defgeneric generate (backend turns &key model settings tools tool-choice output)
  (:documentation "One-shot generation. TURNS: string, LLM-TURN, or a sequence of those.
TOOLS are descriptors (LLM-TOOL), not executors. SETTINGS is LLM-SETTINGS or a plist.
OUTPUT is a schema-protocol designator (or JSON Schema hash) — parsed into
LLM-RESPONSE-OUTPUT. Parse failures follow *STRUCTURED-OUTPUT-REPAIR*
(override with :OUTPUT-REPAIR / LLM-SETTINGS-OUTPUT-REPAIR). → LLM-RESPONSE."))

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
           (repair-tail (member :output-repair args))
           (effective (%settings-with-output settings output))
           (effective (if repair-tail
                          (%settings-with-repair effective (second repair-tail))
                          effective))
           (pass (loop for (k v) on args by #'cddr
                       unless (member k '(:settings :output :output-repair))
                         collect k and collect v))
           (policy (%effective-repair-policy effective))
           (on-error (if (eq policy :signal) :signal :decline))
           (r (apply fn backend payload :settings effective pass)))
      (%attach-structured-output r effective :on-error on-error)
      (when (and (not *%structured-output-repairing*)
                 effective
                 (llm-settings-output effective)
                 (null (and r (llm-response-output r)))
                 (not (eq policy :signal)))
        (setf r (%repair-structured-output backend payload args effective r policy)))
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

(defun coerce-embed-inputs (inputs)
  "INPUTS: a string or a sequence of strings. → list of strings."
  (cond
    ((stringp inputs) (list inputs))
    ((or (listp inputs) (vectorp inputs))
     (let ((xs (%as-list inputs)))
       (unless (and xs (every #'stringp xs))
         (error 'llm-error
                :message (format nil "embed inputs must be strings: ~s" inputs)))
       xs))
    (t (error 'llm-error
              :message (format nil "embed inputs must be a string or sequence of strings: ~s"
                               inputs)))))

(defgeneric embed (backend inputs &key model dimensions encoding-format)
  (:documentation "Embed INPUTS (string or sequence of strings). → LLM-EMBED-RESULT.
Default method signals LLM-UNSUPPORTED. DIMENSIONS is an optional output width.
ENCODING-FORMAT is :float (wave-1); other values are backend-defined."))

(defun embed-query (backend text &rest args &key &allow-other-keys)
  "Sugar: first embedding vector from EMBED."
  (check-type text string)
  (let* ((r (apply #'embed backend text args))
         (emb (and r (first (llm-embed-result-embeddings r)))))
    (and emb (llm-embedding-vector emb))))

(defmethod embed :around ((backend llm-backend) inputs &key &allow-other-keys)
  (with-llm-restarts
    (call-next-method)))

(defmethod embed ((backend null) inputs &rest args &key &allow-other-keys)
  (apply #'embed (%ensure-backend) inputs args))

(defmethod embed ((backend llm-backend) inputs &key model dimensions encoding-format)
  (declare (ignore inputs model dimensions encoding-format))
  (error 'llm-unsupported
         :message (format nil "~a does not implement embed" (class-of backend))))

;;; --- tokens / context -------------------------------------------------------

(defclass token-fit-policy ()
  ((max-tokens :initarg :max-tokens :accessor token-fit-policy-max-tokens
               :initform nil)
   (reserve :initarg :reserve :accessor token-fit-policy-reserve :initform 0))
  (:documentation "Budget for FIT-TURNS. MAX-TOKENS overrides CONTEXT-WINDOW.
RESERVE tokens are subtracted from the window/budget (completion headroom)."))

(defun make-token-fit-policy (&key max-tokens (reserve 0))
  (make-instance 'token-fit-policy :max-tokens max-tokens :reserve reserve))

(defun token-fit-policy-p (x)
  (typep x 'token-fit-policy))

(defun %model-id (model)
  (cond
    ((null model) nil)
    ((llm-model-info-p model) (llm-model-info-id model))
    ((stringp model) model)
    ((symbolp model) (string-downcase (symbol-name model)))
    (t (princ-to-string model))))

(defun %provider-for-backend (catalog backend)
  (when catalog
    (find backend (ignore-errors (list-providers catalog))
          :key #'llm-provider-backend :test #'eq)))

(defun %lookup-model-info (backend model)
  "→ LLM-MODEL-INFO or NIL. MODEL may be a string, id, or LLM-MODEL-INFO."
  (when (llm-model-info-p model)
    (return-from %lookup-model-info model))
  (let ((id (%model-id (or model (ignore-errors (backend-model backend))))))
    (or (when id
          (find-if (lambda (m)
                     (and (llm-model-info-p m)
                          (equal (llm-model-info-id m) id)))
                   (ignore-errors (list-models backend))))
        (when (and id *llm-catalog*)
          (let ((prov (%provider-for-backend *llm-catalog* backend)))
            (when prov
              (find-if (lambda (m)
                         (and (llm-model-info-p m)
                              (equal (llm-model-info-id m) id)))
                       (llm-provider-models prov))))))))

(defgeneric count-tokens (backend thing)
  (:documentation "Estimate tokens for THING on BACKEND.
Default heuristic: CEILING of character length / 4. Exact tokenizers live
in backends (llama.cpp native; HTTP backends can calibrate from LLM-USAGE)."))

(defmethod count-tokens ((backend null) thing)
  (count-tokens (%ensure-backend) thing))

(defmethod count-tokens (backend (thing string))
  (declare (ignore backend))
  (let ((n (length thing)))
    (if (zerop n) 0 (ceiling n 4))))

(defmethod count-tokens (backend (thing llm-text-part))
  (count-tokens backend (or (llm-text-part-text thing) "")))

(defmethod count-tokens (backend (thing llm-thinking-part))
  (count-tokens backend (or (llm-thinking-part-text thing) "")))

(defmethod count-tokens (backend (thing llm-tool-call-part))
  (+ (count-tokens backend (or (llm-tool-call-part-name thing) ""))
     (count-tokens backend (or (llm-tool-call-part-arguments thing) ""))))

(defmethod count-tokens (backend (thing llm-tool-result-part))
  (count-tokens backend (or (llm-tool-result-part-content thing) "")))

(defmethod count-tokens (backend (thing llm-part))
  (let ((tx (part-text thing)))
    (if (and tx (plusp (length tx)))
        (count-tokens backend tx)
        0)))

(defmethod count-tokens (backend (thing llm-turn))
  (count-tokens backend (turn-text thing)))

(defmethod count-tokens (backend (thing llm-message-item))
  (reduce #'+ (llm-message-item-parts thing)
          :key (lambda (p) (count-tokens backend p))
          :initial-value 0))

(defmethod count-tokens (backend (thing cons))
  (if (keywordp (car thing))
      (count-tokens backend (coerce-turn thing))
      (reduce #'+ thing
              :key (lambda (x) (count-tokens backend x))
              :initial-value 0)))

(defmethod count-tokens (backend (thing vector))
  (loop for x across thing sum (count-tokens backend x)))

(defmethod count-tokens (backend thing)
  (if (null thing)
      0
      (count-tokens backend (princ-to-string thing))))

(defgeneric context-window (backend model)
  (:documentation "Token context window for MODEL on BACKEND, or NIL.
Looks up LLM-MODEL-INFO via LIST-MODELS / *LLM-CATALOG*, then
LLM-PROVIDER-CONTEXT-WINDOW."))

(defmethod context-window ((backend null) model)
  (context-window (%ensure-backend) model))

(defmethod context-window ((backend llm-backend) model)
  (let ((info (%lookup-model-info backend model)))
    (or (and (llm-model-info-p info) (llm-model-info-context-window info))
        (let ((prov (and *llm-catalog*
                         (%provider-for-backend *llm-catalog* backend))))
          (and prov (llm-provider-context-window prov))))))

(defun %token-budget (backend policy model)
  (let ((reserve 0)
        (explicit nil))
    (etypecase policy
      (null)
      (integer
       (setf explicit policy))
      (token-fit-policy
       (setf reserve (or (token-fit-policy-reserve policy) 0)
             explicit (token-fit-policy-max-tokens policy))))
    (let ((window (or explicit (context-window backend model))))
      (when window
        (max 0 (- window reserve))))))

(defun %drop-oldest-non-system (turns)
  (let ((dropped nil))
    (loop for turn in turns
          if (and (not dropped)
                  (not (eq (llm-turn-role turn) :system)))
            do (setf dropped t)
          else
            collect turn)))

(defgeneric fit-turns (turns backend &key policy model)
  (:documentation "Trim oldest non-system turns until COUNT-TOKENS fits the budget.
Keep all :system turns. POLICY is an integer token budget or TOKEN-FIT-POLICY
(:reserve subtracted from CONTEXT-WINDOW or :max-tokens). NIL policy uses
CONTEXT-WINDOW; NIL window leaves TURNS unchanged."))

(defmethod fit-turns (turns (backend null) &key policy model)
  (fit-turns turns (%ensure-backend) :policy policy :model model))

(defmethod fit-turns (turns backend &key policy model)
  (let ((kept (copy-list (coerce-turns turns)))
        (budget (%token-budget backend policy model)))
    (if (null budget)
        kept
        (loop while (and (> (count-tokens backend kept) budget)
                         (find-if (lambda (turn)
                                    (not (eq (llm-turn-role turn) :system)))
                                  kept))
              do (setf kept (%drop-oldest-non-system kept))
              finally (return kept)))))
