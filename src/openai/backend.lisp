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

(defun %wire-tool-call (tc)
  (let ((tc (if (llm-tool-call-p tc) tc (llm-protocol::%coerce-tool-call tc))))
    (%ht "id" (or (llm-tool-call-id tc) "call_0")
         "type" "function"
         "function" (%ht "name" (llm-tool-call-name tc)
                         "arguments"
                         (let ((a (llm-tool-call-arguments tc)))
                           (if (stringp a) a (stack-json:encode a)))))))

(defun %wire-message (msg)
  (let ((h (%ht "role" (llm-message-role msg)
                "content" (or (llm-message-content msg) "")
                "name" (llm-message-name msg)
                "tool_call_id" (llm-message-tool-call-id msg))))
    (when (llm-message-tool-calls msg)
      (setf (gethash "tool_calls" h)
            (map 'vector #'%wire-tool-call (llm-message-tool-calls msg))))
    h))

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
    ((and (listp tool) (keywordp (car tool)))
     (%wire-tool (make-llm-tool :name (getf tool :name)
                                :description (getf tool :description)
                                :parameters (getf tool :parameters))))
    (t (error 'llm-error :message (format nil "not a tool: ~s" tool)))))

(defun %parse-tool-calls (raw)
  (mapcar #'llm-protocol::%coerce-tool-call (llm-protocol::%as-list raw)))

(defun %str (x)
  (cond
    ((or (null x) (eq x :null)) "")
    ((stringp x) x)
    (t (princ-to-string x))))

(defun %parse-result (obj requested-model)
  (let* ((choice (let ((cs (gethash "choices" obj)))
                   (and cs (plusp (length cs)) (elt cs 0))))
         (msg (and choice (gethash "message" choice)))
         (content (and msg (gethash "content" msg)))
         (tcs (and msg (gethash "tool_calls" msg))))
    (make-llm-result
     :message (make-llm-message
               :role (or (and msg (gethash "role" msg)) "assistant")
               :content (%str content)
               :tool-calls (%parse-tool-calls tcs))
     :model (or (gethash "model" obj) requested-model)
     :finish-reason (and choice (gethash "finish_reason" choice))
     :usage (and obj (gethash "usage" obj)))))

(defmethod generate ((backend openai-compat-backend) messages &key model tools
                     stream temperature max-tokens stop tool-choice)
  (when stream
    (error 'llm-unsupported :message "openai-compat wave-1 does not stream"))
  (let* ((model (or model (openai-default-model backend)))
         (body (%ht "model" model
                    "messages" (map 'vector #'%wire-message (coerce-messages messages))
                    "temperature" temperature
                    "max_tokens" max-tokens
                    "stop" stop
                    "tools" (and tools (map 'vector #'%wire-tool
                                            (llm-protocol::%as-list tools)))
                    "tool_choice" tool-choice)))
    (multiple-value-bind (status text)
        (%request backend :post "/chat/completions" body)
      (%parse-result (%decode status text) model))))

(defmethod list-models ((backend openai-compat-backend) &key)
  (multiple-value-bind (status text)
      (%request backend :get "/models")
    (let* ((obj (%decode status text))
           (data (or (and (hash-table-p obj) (gethash "data" obj)) #())))
      (mapcar (lambda (m)
                (make-llm-model
                 :id (if (hash-table-p m) (gethash "id" m) (princ-to-string m))
                 :owned-by (and (hash-table-p m) (gethash "owned_by" m))))
              (llm-protocol::%as-list data)))))
