(in-package #:llm-protocol/tests)

(defun %ht (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          unless (null v)
            do (setf (gethash k h) v))
    h))

(defun %fake-openai (method url &key headers content)
  (declare (ignore headers))
  (cond
    ((and (eq method :post) (search "/chat/completions" url))
     (let* ((body (stack-json:decode content))
            (msgs (gethash "messages" body))
            (last (elt msgs (1- (length msgs))))
            (tools (gethash "tools" body))
            (temp (gethash "temperature" body)))
       (declare (ignore temp))
       (values 200
               (stack-json:encode
                (%ht "model" (or (gethash "model" body) "gpt-4o-mini")
                     "usage" (%ht "prompt_tokens" 3 "completion_tokens" 2
                                  "total_tokens" 5)
                     "choices"
                     (vector (%ht "finish_reason" (if tools "tool_calls" "stop")
                                  "message"
                                  (%ht "role" "assistant"
                                       "content" (if tools
                                                     :null
                                                     (format nil "ok:~a"
                                                             (gethash "content" last)))
                                       "tool_calls"
                                       (when tools
                                         (vector (%ht "id" "call_1"
                                                      "type" "function"
                                                      "function"
                                                      (%ht "name" "sum"
                                                           "arguments" "{\"a\":1}"))))))))))))
    ((search "/models" url)
     (values 200 (stack-json:encode
                  (%ht "data" (vector (%ht "id" "local" "owned_by" "lmstudio"))))))
    (t (values 404 "{}"))))

(defun %fake-openai-error (method url &key headers content)
  (declare (ignore method url headers content))
  (values 401 (stack-json:encode
               (%ht "error" (%ht "message" "invalid api key" "type" "auth")))))

(deftest openai-generate-mock-http
  (let* ((backend (llm-backend-openai:make-openai-compat-backend
                   :base-url "http://example.invalid/v1"
                   :api-key "sk-test"
                   :request-fn #'%fake-openai))
         (r (llm-protocol:generate backend "hi" :model "local")))
    (ok (equal "ok:hi" (llm-protocol:llm-response-text r)))
    (ok (equal "local" (llm-protocol:llm-response-model r)))
    (ok (eq :stop (llm-protocol:llm-response-finish-reason r)))
    (ok (= 5 (llm-protocol:llm-usage-total-tokens (llm-protocol:llm-response-usage r))))))

(deftest openai-settings-on-wire
  (let ((seen nil))
    (flet ((capture (method url &key headers content)
             (declare (ignore method url headers))
             (setf seen (stack-json:decode content))
             (%fake-openai :post "http://x/chat/completions" :content content)))
      (llm-protocol:generate
       (llm-backend-openai:make-openai-compat-backend :request-fn #'capture)
       "hi"
       :settings '(:temperature 0 :max-tokens 16))
      (ok (zerop (gethash "temperature" seen)))
      (ok (= 16 (gethash "max_tokens" seen))))))

(deftest openai-tools-mock-http
  (let* ((backend (llm-backend-openai:make-openai-compat-backend
                   :request-fn #'%fake-openai))
         (r (llm-protocol:generate backend "add"
                                   :tools (list (llm-protocol:make-llm-tool :name "sum")))))
    (ok (eq :tool-use (llm-protocol:llm-response-finish-reason r)))
    (ok (equal "sum" (llm-protocol:llm-tool-call-part-name
                      (first (llm-protocol:llm-response-tool-calls r)))))))

(deftest openai-list-models-mock-http
  (let ((models (llm-protocol:list-models
                 (llm-backend-openai:make-openai-compat-backend
                  :request-fn #'%fake-openai))))
    (ok (equal "local" (llm-protocol:llm-model-info-id (first models))))
    (ok (equal "lmstudio" (llm-protocol:llm-model-info-owned-by (first models))))))

(deftest openai-http-error
  (ok (signals (llm-protocol:generate
                (llm-backend-openai:make-openai-compat-backend
                 :request-fn #'%fake-openai-error)
                "hi")
               'llm-protocol:llm-http-error)))

(deftest openai-stream-unsupported
  (ok (signals (llm-protocol:stream-generate
                (llm-backend-openai:make-openai-compat-backend :request-fn #'%fake-openai)
                "hi")
               'llm-protocol:llm-unsupported)))

(deftest openai-live-optional
  (if (and (uiop:getenv "LLM_OPENAI_LIVE")
           (plusp (length (uiop:getenv "LLM_OPENAI_LIVE"))))
      (let* ((backend (llm-backend-openai:make-openai-compat-backend))
             (r (llm-protocol:generate
                 backend "Reply with the single word pong."
                 :settings '(:temperature 0 :max-tokens 16))))
        (ok (llm-protocol:llm-response-p r))
        (ok (plusp (length (or (llm-protocol:llm-response-text r) "")))))
      (skip "set LLM_OPENAI_LIVE=1 for a live OpenAI-compat call")))
