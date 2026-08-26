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
            (tools (gethash "tools" body)))
       (values 200
               (stack-json:encode
                (%ht "model" (or (gethash "model" body) "gpt-4o-mini")
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
    (ok (equal "ok:hi" (llm-protocol:llm-result-text r)))
    (ok (equal "local" (llm-protocol:llm-result-model r)))
    (ok (equal "stop" (llm-protocol:llm-result-finish-reason r)))))

(deftest openai-tools-mock-http
  (let* ((backend (llm-backend-openai:make-openai-compat-backend
                   :request-fn #'%fake-openai))
         (r (llm-protocol:generate backend "add"
                                   :tools (list (llm-protocol:make-llm-tool :name "sum")))))
    (ok (equal "tool_calls" (llm-protocol:llm-result-finish-reason r)))
    (ok (equal "sum" (llm-protocol:llm-tool-call-name
                      (first (llm-protocol:llm-message-tool-calls
                              (llm-protocol:llm-result-message r))))))))

(deftest openai-list-models-mock-http
  (let ((models (llm-protocol:list-models
                 (llm-backend-openai:make-openai-compat-backend
                  :request-fn #'%fake-openai))))
    (ok (equal "local" (llm-protocol:llm-model-id (first models))))
    (ok (equal "lmstudio" (llm-protocol:llm-model-owned-by (first models))))))

(deftest openai-http-error
  (ok (signals (llm-protocol:generate
                (llm-backend-openai:make-openai-compat-backend
                 :request-fn #'%fake-openai-error)
                "hi")
               'llm-protocol:llm-http-error)))

(deftest openai-stream-unsupported
  (ok (signals (llm-protocol:generate
                (llm-backend-openai:make-openai-compat-backend :request-fn #'%fake-openai)
                "hi" :stream t)
               'llm-protocol:llm-unsupported)))

(deftest openai-live-optional
  (if (and (uiop:getenv "LLM_OPENAI_LIVE")
           (plusp (length (uiop:getenv "LLM_OPENAI_LIVE"))))
      (let* ((backend (llm-backend-openai:make-openai-compat-backend))
             (r (llm-protocol:generate backend "Reply with the single word pong."
                                       :temperature 0 :max-tokens 16)))
        (ok (llm-protocol:llm-result-p r))
        (ok (plusp (length (or (llm-protocol:llm-result-text r) "")))))
      (skip "set LLM_OPENAI_LIVE=1 for a live OpenAI-compat call")))
