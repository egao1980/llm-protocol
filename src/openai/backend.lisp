(in-package #:llm-backend-openai)

(defparameter +default-openai-base-url+ "http://127.0.0.1:1234/v1"
  "LM Studio OpenAI-compatible default.")

(defun %env (name)
  (let ((v (uiop:getenv name)))
    (and v (plusp (length v)) v)))

(defclass openai-compat-backend (llm-backend)
  ((base-url :initarg :base-url :accessor openai-base-url
             :initform +default-openai-base-url+)
   (api-key :initarg :api-key :accessor openai-api-key :initform nil)
   (default-model :initarg :default-model :accessor openai-default-model
                  :initform "gpt-4o-mini")
   (organization :initarg :organization :accessor openai-organization :initform nil)
   (request-fn :initarg :request-fn :accessor openai-request-fn :initform nil)))

(defun make-openai-compat-backend (&key base-url api-key default-model
                                     organization request-fn)
  (make-instance 'openai-compat-backend
                 :base-url (or base-url (%env "OPENAI_BASE_URL")
                               +default-openai-base-url+)
                 :api-key (or api-key (%env "OPENAI_API_KEY"))
                 :default-model (or default-model (%env "OPENAI_MODEL") "gpt-4o-mini")
                 :organization (or organization (%env "OPENAI_ORGANIZATION"))
                 :request-fn request-fn))

(defun use-openai-compat-backend (&rest args &key &allow-other-keys)
  (setf *llm-backend* (apply #'make-openai-compat-backend args)))

(defmethod backend-model ((backend openai-compat-backend))
  (openai-default-model backend))

(defmethod backend-supports-p ((backend openai-compat-backend) (feature (eql :tools)))
  t)

(defmethod backend-supports-p ((backend openai-compat-backend)
                               (feature (eql :structured-output)))
  t)

(defmethod backend-supports-p ((backend openai-compat-backend) (feature (eql :vision)))
  t)

(defun %ht (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          unless (or (null k) (eq v :omit) (null v))
            do (setf (gethash k h) v))
    h))

(defun %join (base path)
  (format nil "~a~a" (string-right-trim "/" (or base "")) path))

(defun %headers (backend)
  (let ((h `(("content-type" . "application/json")
             ("accept" . "application/json"))))
    (when (and (openai-api-key backend) (plusp (length (openai-api-key backend))))
      (push (cons "authorization"
                  (format nil "Bearer ~a" (openai-api-key backend)))
            h))
    (when (openai-organization backend)
      (push (cons "openai-organization" (openai-organization backend)) h))
    h))

(defun %body-string (response)
  (let ((b (http-protocol:response-body response)))
    (cond
      ((stringp b) b)
      ((and (vectorp b) (not (stringp b)))
       (babel:octets-to-string b :encoding :utf-8))
      (t ""))))

(defun %http-request (method url &key headers content)
  (unless http-protocol:*http-backend*
    (error 'llm-error
           :message "*http-backend* is nil — bind an http-protocol backend"))
  (let ((res (if content
                 (http:request method url :headers headers :content content)
                 (http:request method url :headers headers))))
    (values (http-protocol:response-status res) (%body-string res))))

(defun %request (backend method path &optional object)
  (let* ((fn (or (openai-request-fn backend) #'%http-request))
         (url (%join (openai-base-url backend) path))
         (content (and object (stack-json:encode object))))
    (multiple-value-bind (status body)
        (funcall fn method url :headers (%headers backend) :content content)
      (values status body))))

(defun %error-message (obj fallback)
  (cond
    ((and (hash-table-p obj) (hash-table-p (gethash "error" obj)))
     (or (gethash "message" (gethash "error" obj)) fallback))
    ((and (hash-table-p obj) (gethash "error" obj))
     (let ((err (gethash "error" obj)))
       (if (stringp err) err (princ-to-string err))))
    (t fallback)))

(defun %decode (status body)
  (let ((obj (ignore-errors (stack-json:decode body))))
    (cond
      ((<= 200 status 299) (or obj (error 'llm-error :message "empty JSON body")))
      (t (error 'llm-http-error
                :status status
                :body body
                :message (%error-message obj (format nil "HTTP ~a" status)))))))

(defun %str (x)
  (cond
    ((or (null x) (eq x :null)) "")
    ((stringp x) x)
    (t (princ-to-string x))))

(defgeneric %wire-part (part)
  (:method ((part llm-text-part))
    (%ht "type" "text" "text" (or (llm-text-part-text part) "")))
  (:method ((part llm-image-part))
    (%ht "type" "image_url"
         "image_url" (%ht "url" (or (llm-image-part-url part)
                                    (and (llm-image-part-data part)
                                         (format nil "data:~a;base64,~a"
                                                 (or (llm-image-part-media-type part)
                                                     "image/png")
                                                 (llm-image-part-data part)))))))
  (:method ((part llm-thinking-part))
    nil)
  (:method ((part llm-part))
    nil))

(defun %wire-tool-call (part)
  (%ht "id" (or (llm-tool-call-part-id part) "call_0")
       "type" "function"
       "function" (%ht "name" (llm-tool-call-part-name part)
                       "arguments"
                       (let ((a (llm-tool-call-part-arguments part)))
                         (if (stringp a) a (stack-json:encode a))))))

(defun %wire-turn (turn)
  (let* ((turn (coerce-turn turn))
         (role (string-downcase (symbol-name (llm-turn-role turn))))
         (texts (remove nil (mapcar #'%wire-part (llm-turn-parts turn))))
         (calls (remove-if-not #'llm-tool-call-part-p (llm-turn-parts turn)))
         (results (remove-if-not #'llm-tool-result-part-p (llm-turn-parts turn)))
         (thinking (find-if #'llm-thinking-part-p (llm-turn-parts turn))))
    (cond
      ((eq (llm-turn-role turn) :tool)
       (let ((r (or (first results)
                    (make-llm-tool-result-part :id nil :content (turn-text turn)))))
         (%ht "role" "tool"
              "tool_call_id" (llm-tool-result-part-id r)
              "name" (llm-tool-result-part-name r)
              "content" (or (llm-tool-result-part-content r) ""))))
      (t
       (let ((content (cond
                        ((and texts (null (rest texts))
                              (equal (gethash "type" (first texts)) "text")
                              (null calls))
                         (gethash "text" (first texts)))
                        (texts (map 'vector #'identity texts))
                        (t ""))))
         (let ((h (%ht "role" role "content" content)))
           (when calls
             (setf (gethash "tool_calls" h)
                   (map 'vector #'%wire-tool-call calls)))
           (when thinking
             (setf (gethash "reasoning_content" h) (llm-thinking-part-text thinking)))
           h))))))

(defun %wire-tool (tool)
  (cond
    ((llm-tool-p tool)
     (%ht "type" "function"
          "function" (%ht "name" (llm-tool-name tool)
                          "description" (llm-tool-description tool)
                          "parameters" (or (llm-tool-parameters tool)
                                           (%ht "type" "object"
                                                "properties" (%ht))))))
    ((hash-table-p tool) tool)
    ((and (consp tool) (keywordp (car tool)))
     (%wire-tool (make-llm-tool :name (getf tool :name)
                                :description (getf tool :description)
                                :parameters (getf tool :parameters))))
    (t (error 'llm-error :message (format nil "not a tool: ~s" tool)))))

(defun %wire-tool-choice (choice)
  (etypecase choice
    (null nil)
    ((eql :auto) "auto")
    ((eql :none) "none")
    ((eql :required) "required")
    (string (%ht "type" "function" "function" (%ht "name" choice)))
    (hash-table choice)))

(defun %finish-reason (raw)
  (cond
    ((or (null raw) (eq raw :null)) :stop)
    ((string-equal raw "stop") :stop)
    ((string-equal raw "length") :length)
    ((or (string-equal raw "tool_calls") (string-equal raw "tool_use")) :tool-use)
    ((string-equal raw "content_filter") :content-filter)
    (t :stop)))

(defun %usage (obj)
  (when (hash-table-p obj)
    (make-llm-usage
     :input-tokens (or (gethash "prompt_tokens" obj) (gethash "input_tokens" obj))
     :output-tokens (or (gethash "completion_tokens" obj) (gethash "output_tokens" obj))
     :total-tokens (gethash "total_tokens" obj))))

(defun %parse-response (obj requested-model)
  (let* ((choice (let ((cs (gethash "choices" obj)))
                   (and cs (plusp (length cs)) (elt cs 0))))
         (msg (and choice (gethash "message" choice)))
         (content (and msg (gethash "content" msg)))
         (tcs (and msg (gethash "tool_calls" msg)))
         (thinking (and msg (or (gethash "reasoning_content" msg)
                                (gethash "thinking" msg))))
         (parts (append
                 (and thinking (not (eq thinking :null))
                      (list (make-llm-thinking-part :text (%str thinking))))
                 (and content (not (eq content :null)) (plusp (length (%str content)))
                      (list (make-llm-text-part :text (%str content))))
                 (mapcar #'llm-protocol::%coerce-tool-call-part
                         (llm-protocol::%as-list tcs)))))
    (make-llm-response
     :parts parts
     :model (or (gethash "model" obj) requested-model)
     :finish-reason (%finish-reason (and choice (gethash "finish_reason" choice)))
     :usage (%usage (and obj (gethash "usage" obj))))))

(defmethod generate ((backend openai-compat-backend) turns &key model settings
                     tools tool-choice)
  (let* ((settings (coerce-settings settings))
         (model (or model (openai-default-model backend)))
         (body (%ht "model" model
                    "messages" (map 'vector #'%wire-turn (coerce-turns turns))
                    "temperature" (and settings (llm-settings-temperature settings))
                    "max_tokens" (and settings (llm-settings-max-tokens settings))
                    "stop" (and settings (llm-settings-stop settings))
                    "top_p" (and settings (llm-settings-top-p settings))
                    "response_format" (and settings
                                           (llm-settings-response-format settings))
                    "tools" (and tools (map 'vector #'%wire-tool
                                            (llm-protocol::%as-list tools)))
                    "tool_choice" (%wire-tool-choice tool-choice))))
    (multiple-value-bind (status text)
        (%request backend :post "/chat/completions" body)
      (%parse-response (%decode status text) model))))

(defmethod stream-generate ((backend openai-compat-backend) turns &key model
                            settings tools tool-choice on-part)
  (declare (ignore turns model settings tools tool-choice on-part))
  (error 'llm-unsupported :message "openai-compat wave-1 does not stream"))

(defmethod list-models ((backend openai-compat-backend) &key)
  (multiple-value-bind (status text)
      (%request backend :get "/models")
    (let* ((obj (%decode status text))
           (data (or (and (hash-table-p obj) (gethash "data" obj)) #())))
      (mapcar (lambda (m)
                (make-llm-model-info
                 :id (if (hash-table-p m) (gethash "id" m) (princ-to-string m))
                 :owned-by (and (hash-table-p m) (gethash "owned_by" m))))
              (llm-protocol::%as-list data)))))
