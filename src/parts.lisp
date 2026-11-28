(in-package #:llm-protocol)

;;; --- part state ------------------------------------------------------------
;;; The storable form of a part: a plist of strings, numbers, keywords and
;;; lists (no hash tables, no instances), with :TYPE first. Journals and stores
;;; keep parts this way and read them back exactly, so a replay sends the
;;; provider what it sent before: signed thinking, server tool calls, provider
;;; blocks such as an Anthropic compaction. A backend that has its own part
;;; classes adds PART-STATE methods for them and STATE-PART methods on its
;;; :TYPE keywords.

(defgeneric part-state (part)
  (:documentation "PART as a storable plist with :TYPE. STATE-PART reads it back.")
  (:method ((part llm-text-part))
    (list :type :text :text (llm-text-part-text part)))
  (:method ((part llm-image-part))
    (list :type :image :url (llm-image-part-url part)
          :media-type (llm-image-part-media-type part)
          :data (llm-image-part-data part)))
  (:method ((part llm-tool-call-part))
    (list :type :tool-call :id (llm-tool-call-part-id part)
          :name (llm-tool-call-part-name part)
          :arguments (or (llm-tool-call-part-arguments part) "{}")))
  (:method ((part llm-tool-result-part))
    (list :type :tool-result :id (llm-tool-result-part-id part)
          :name (llm-tool-result-part-name part)
          :content (llm-tool-result-part-content part)
          :error-p (and (llm-tool-result-part-error-p part) t)))
  (:method ((part llm-thinking-part))
    (list :type :thinking :text (llm-thinking-part-text part)
          :signature (llm-thinking-part-signature part)))
  (:method ((part llm-part))
    (list :type :unknown :class (prin1-to-string (type-of part)))))

(defgeneric state-part (type state)
  (:documentation "Read back a PART-STATE plist STATE whose :TYPE is TYPE.
→ an LLM-PART, or NIL when no loaded system knows TYPE (the part is dropped).")
  (:method ((type (eql :text)) state)
    (make-llm-text-part :text (or (getf state :text) "")))
  (:method ((type (eql :image)) state)
    (make-llm-image-part :url (getf state :url) :media-type (getf state :media-type)
                         :data (getf state :data)))
  (:method ((type (eql :tool-call)) state)
    (make-llm-tool-call-part :id (getf state :id) :name (getf state :name)
                             :arguments (or (getf state :arguments) "{}")))
  (:method ((type (eql :tool-result)) state)
    (make-llm-tool-result-part :id (getf state :id) :name (getf state :name)
                               :content (or (getf state :content) "")
                               :error-p (getf state :error-p)))
  (:method ((type (eql :thinking)) state)
    (make-llm-thinking-part :text (or (getf state :text) "")
                            :signature (getf state :signature)))
  (:method (type state)
    (declare (ignore type state))
    nil))

(defmethod part-state ((part llm-audio-part))
  (list :type :audio :url (llm-audio-part-url part)
        :media-type (llm-audio-part-media-type part)
        :data (llm-audio-part-data part)))

(defmethod state-part ((type (eql :audio)) state)
  (make-llm-audio-part :url (getf state :url) :media-type (getf state :media-type)
                       :data (getf state :data)))

(defun part-from-state (state)
  "STATE-PART on STATE's :TYPE. → LLM-PART or NIL."
  (state-part (getf state :type) state))

(defun turn-state (turn)
  "TURN as (:role R :parts (part-state…))."
  (let ((turn (coerce-turn turn)))
    (list :role (llm-turn-role turn)
          :parts (mapcar #'part-state (llm-turn-parts turn)))))

(defun state-turn (state)
  "Read back a TURN-STATE. Parts no loaded system knows are dropped."
  (make-llm-turn :role (getf state :role)
                 :parts (remove nil (mapcar #'part-from-state (getf state :parts)))))

;;; --- provider compaction ----------------------------------------------------
;;; A provider can summarize a conversation itself (Anthropic: on demand with
;;; SERVER-COMPACT, or at a token threshold inside an ordinary request). The
;;; summary comes back as a provider part: COMPACTION-PART-P is true for it and
;;; COMPACTION-PART-TEXT reads its summary, if the provider returns one in text.
;;; Client lookup is the :llm-compaction capability (llm-protocol/capability);
;;; BACKEND-SUPPORTS-P :COMPACTION is the wire probe.

(defgeneric compaction-part-p (part)
  (:documentation "T when PART is a provider-side compaction summary (an
Anthropic `compaction` block). Where it stands decides what it replaces: from
a threshold compaction it follows the turns it summarizes, which the provider
then ignores; from SERVER-COMPACT it opens the conversation, and the turns it
summarizes are gone.")
  (:method (part)
    (declare (ignore part))
    nil))

(defgeneric compaction-part-text (part)
  (:documentation "The summary text a compaction PART carries, or NIL.")
  (:method (part)
    (declare (ignore part))
    nil))

(defgeneric server-compact (backend turns &key model settings tools instructions)
  (:documentation "Ask BACKEND to summarize TURNS now. INSTRUCTIONS replaces the
provider's summarization prompt. TOOLS and the system turns should be those of
the conversation (the summarizer reads them; it calls no tool).
→ (values new-turns compacted-p response). NEW-TURNS are the system turns of
TURNS, then the summary (a compaction part); without a summary they are TURNS
and COMPACTED-P is NIL — continue and compact later. The default signals
LLM-UNSUPPORTED.")
  (:method ((backend null) turns &rest args &key &allow-other-keys)
    (apply #'server-compact (%ensure-backend) turns args))
  (:method (backend turns &key model settings tools instructions)
    (declare (ignore turns model settings tools instructions))
    (error 'llm-unsupported
           :message (format nil "~a has no provider compaction" (type-of backend)))))

(defparameter +portable-summary-prefix+
  "Summary of the earlier conversation:"
  "Text before a provider summary that PORTABLE-TURNS hands to another provider.")

(defun %compaction-turn-p (turn)
  (some #'compaction-part-p (llm-turn-parts turn)))

(defun portable-turns (turns)
  "TURNS for a backend without provider compaction. A compaction part that
opens the conversation (from SERVER-COMPACT: the turns it summarized are
gone) becomes a :system turn with its summary text. Any other compaction
part (threshold: the turns it summarized are still here) is dropped. A turn
left with no parts is dropped. TURNS without compaction parts come back as
they are."
  (let ((turns (coerce-turns turns)))
    (if (notany #'%compaction-turn-p turns)
        turns
        (loop with seen-conversation = nil
              for turn in turns
              for parts = (llm-turn-parts turn)
              for opens = (not seen-conversation)
              for kept = (remove-if #'compaction-part-p parts)
              for summary = (and opens
                                 (some (lambda (p)
                                         (and (compaction-part-p p)
                                              (compaction-part-text p)))
                                       parts))
              when summary
                collect (make-llm-turn
                         :role :system
                         :parts (list (make-llm-text-part
                                       :text (format nil "~a~%~a"
                                                     +portable-summary-prefix+
                                                     summary))))
              when kept
                collect (if (= (length kept) (length parts))
                            turn
                            (make-llm-turn :role (llm-turn-role turn) :parts kept))
              unless (eq (llm-turn-role turn) :system)
                do (setf seen-conversation t)))))

(defun %portable-payload (backend turns)
  "TURNS as BACKEND can take them: PORTABLE-TURNS unless it compacts natively.
Only a list of LLM-TURNs is rewritten (RESPOND items stay as they are)."
  (if (and (listp turns)
           (every #'llm-turn-p turns)
           (some #'%compaction-turn-p turns)
           (not (backend-supports-p backend :compaction)))
      (portable-turns turns)
      turns))
