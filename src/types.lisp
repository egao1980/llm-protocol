(in-package #:llm-protocol)

;;; Role lives on the turn; type lives on the part.
;;; That is the Anthropic / Gemini / OpenAI Responses / Vercel ModelMessage
;;; majority — not a chat-completions "one message, glued concerns" blob.

(deftype llm-role ()
  '(member :system :user :assistant :tool))

(deftype llm-finish-reason ()
  '(member :stop :length :tool-use :content-filter))

(defclass llm-part () ()
  (:documentation "One content block. Subclass, do not hang extra meaning on TEXT."))

(defun llm-part-p (x)
  (typep x 'llm-part))

(defclass llm-text-part (llm-part)
  ((text :initarg :text :accessor llm-text-part-text :initform "")))

(defun make-llm-text-part (&key (text ""))
  (make-instance 'llm-text-part :text (if text (string text) "")))

(defun llm-text-part-p (x)
  (typep x 'llm-text-part))

(defclass llm-image-part (llm-part)
  ((url :initarg :url :accessor llm-image-part-url :initform nil)
   (media-type :initarg :media-type :accessor llm-image-part-media-type :initform nil)
   (data :initarg :data :accessor llm-image-part-data :initform nil)))

(defun make-llm-image-part (&key url media-type data)
  (make-instance 'llm-image-part :url url :media-type media-type :data data))

(defun llm-image-part-p (x)
  (typep x 'llm-image-part))

(defclass llm-tool-call-part (llm-part)
  ((id :initarg :id :accessor llm-tool-call-part-id :initform nil)
   (name :initarg :name :accessor llm-tool-call-part-name)
   (arguments :initarg :arguments :accessor llm-tool-call-part-arguments
              :initform "{}")))

(defun make-llm-tool-call-part (&key id name (arguments "{}"))
  (make-instance 'llm-tool-call-part :id id :name name :arguments arguments))

(defun llm-tool-call-part-p (x)
  (typep x 'llm-tool-call-part))

(defclass llm-tool-result-part (llm-part)
  ((id :initarg :id :accessor llm-tool-result-part-id)
   (name :initarg :name :accessor llm-tool-result-part-name :initform nil)
   (content :initarg :content :accessor llm-tool-result-part-content :initform "")
   (error-p :initarg :error-p :accessor llm-tool-result-part-error-p :initform nil)))

(defun make-llm-tool-result-part (&key id name (content "") error-p)
  (make-instance 'llm-tool-result-part :id id :name name :content content :error-p error-p))

(defun llm-tool-result-part-p (x)
  (typep x 'llm-tool-result-part))

(defclass llm-thinking-part (llm-part)
  ((text :initarg :text :accessor llm-thinking-part-text :initform "")
   (signature :initarg :signature :accessor llm-thinking-part-signature :initform nil)))

(defun make-llm-thinking-part (&key (text "") signature)
  (make-instance 'llm-thinking-part :text text :signature signature))

(defun llm-thinking-part-p (x)
  (typep x 'llm-thinking-part))

(defclass llm-turn ()
  ((role :initarg :role :accessor llm-turn-role :initform :user)
   (parts :initarg :parts :accessor llm-turn-parts :initform nil)))

(defun make-llm-turn (&key (role :user) parts)
  (make-instance 'llm-turn :role role :parts (copy-list parts)))

(defun llm-turn-p (x)
  (typep x 'llm-turn))

(defun user-turn (text &rest more-parts)
  (make-llm-turn :role :user :parts (list* (make-llm-text-part :text text) more-parts)))

(defun system-turn (text)
  (make-llm-turn :role :system :parts (list (make-llm-text-part :text text))))

(defun assistant-turn (text &key tool-calls thinking)
  (make-llm-turn
   :role :assistant
   :parts (append (and thinking (list (if (llm-thinking-part-p thinking)
                                          thinking
                                          (make-llm-thinking-part :text thinking))))
                  (and text (plusp (length text))
                       (list (make-llm-text-part :text text)))
                  (copy-list tool-calls))))

(defun tool-turn (id content &key name error-p)
  (make-llm-turn :role :tool
                 :parts (list (make-llm-tool-result-part :id id :name name
                                                         :content content :error-p error-p))))

(defclass llm-settings ()
  ((temperature :initarg :temperature :accessor llm-settings-temperature :initform nil)
   (max-tokens :initarg :max-tokens :accessor llm-settings-max-tokens :initform nil)
   (stop :initarg :stop :accessor llm-settings-stop :initform nil)
   (top-p :initarg :top-p :accessor llm-settings-top-p :initform nil)
   (response-format :initarg :response-format :accessor llm-settings-response-format
                    :initform nil)
   (output :initarg :output :accessor llm-settings-output :initform nil
           :documentation "schema-protocol designator, JSON Schema hash, or NIL.")
   (extra :initarg :extra :accessor llm-settings-extra :initform nil)))

(defun make-llm-settings (&key temperature max-tokens stop top-p response-format
                            output extra)
  (make-instance 'llm-settings
                 :temperature temperature :max-tokens max-tokens :stop stop
                 :top-p top-p :response-format response-format
                 :output output :extra extra))

(defun llm-settings-p (x)
  (typep x 'llm-settings))

(defclass llm-tool ()
  ((name :initarg :name :accessor llm-tool-name)
   (description :initarg :description :accessor llm-tool-description :initform nil)
   (parameters :initarg :parameters :accessor llm-tool-parameters :initform nil)))

(defun make-llm-tool (&key name description parameters)
  (check-type name string)
  (make-instance 'llm-tool :name name :description description :parameters parameters))

(defun llm-tool-p (x)
  (typep x 'llm-tool))

(defclass llm-usage ()
  ((input-tokens :initarg :input-tokens :accessor llm-usage-input-tokens :initform nil)
   (output-tokens :initarg :output-tokens :accessor llm-usage-output-tokens :initform nil)
   (total-tokens :initarg :total-tokens :accessor llm-usage-total-tokens :initform nil)))

(defun make-llm-usage (&key input-tokens output-tokens total-tokens)
  (make-instance 'llm-usage :input-tokens input-tokens
                 :output-tokens output-tokens :total-tokens total-tokens))

(defun llm-usage-p (x)
  (typep x 'llm-usage))

(defclass llm-item ()
  ((id :initarg :id :accessor llm-item-id :initform nil))
  (:documentation "Responses-style grain. Role lives on LLM-MESSAGE-ITEM only."))

(defun llm-item-p (x)
  (typep x 'llm-item))

(defclass llm-message-item (llm-item)
  ((role :initarg :role :accessor llm-message-item-role :initform :user)
   (parts :initarg :parts :accessor llm-message-item-parts :initform nil)))

(defun make-llm-message-item (&key id (role :user) parts)
  (make-instance 'llm-message-item :id id :role role :parts (copy-list parts)))

(defun llm-message-item-p (x)
  (typep x 'llm-message-item))

(defclass llm-function-call-item (llm-item)
  ((call-id :initarg :call-id :accessor llm-function-call-item-call-id :initform nil)
   (name :initarg :name :accessor llm-function-call-item-name)
   (arguments :initarg :arguments :accessor llm-function-call-item-arguments
              :initform "{}")))

(defun make-llm-function-call-item (&key id call-id name (arguments "{}"))
  (make-instance 'llm-function-call-item
                 :id id :call-id (or call-id id) :name name :arguments arguments))

(defun llm-function-call-item-p (x)
  (typep x 'llm-function-call-item))

(defclass llm-function-call-output-item (llm-item)
  ((call-id :initarg :call-id :accessor llm-function-call-output-item-call-id)
   (output :initarg :output :accessor llm-function-call-output-item-output
           :initform "")))

(defun make-llm-function-call-output-item (&key id call-id (output ""))
  (make-instance 'llm-function-call-output-item
                 :id id :call-id call-id :output output))

(defun llm-function-call-output-item-p (x)
  (typep x 'llm-function-call-output-item))

(defclass llm-reasoning-item (llm-item)
  ((text :initarg :text :accessor llm-reasoning-item-text :initform "")
   (signature :initarg :signature :accessor llm-reasoning-item-signature :initform nil)))

(defun make-llm-reasoning-item (&key id (text "") signature)
  (make-instance 'llm-reasoning-item :id id :text text :signature signature))

(defun llm-reasoning-item-p (x)
  (typep x 'llm-reasoning-item))

(defclass llm-response ()
  ((parts :initarg :parts :accessor llm-response-parts :initform nil)
   (items :initarg :items :accessor llm-response-items :initform nil)
   (id :initarg :id :accessor llm-response-id :initform nil)
   (model :initarg :model :accessor llm-response-model :initform nil)
   (finish-reason :initarg :finish-reason :accessor llm-response-finish-reason
                  :initform :stop)
   (usage :initarg :usage :accessor llm-response-usage :initform nil)
   (output :initarg :output :accessor llm-response-output :initform nil
           :documentation "Parsed structured output (schema-protocol instance), or NIL.")))

(defun make-llm-response (&key parts items id model (finish-reason :stop) usage output)
  (make-instance 'llm-response :parts (copy-list parts) :items (copy-list items)
                 :id id :model model :finish-reason finish-reason :usage usage
                 :output output))

(defun llm-response-p (x)
  (typep x 'llm-response))

(defclass llm-model-info ()
  ((id :initarg :id :accessor llm-model-info-id)
   (owned-by :initarg :owned-by :accessor llm-model-info-owned-by :initform nil)))

(defun make-llm-model-info (&key id owned-by)
  (make-instance 'llm-model-info :id id :owned-by owned-by))

(defun llm-model-info-p (x)
  (typep x 'llm-model-info))
