(in-package #:llm-protocol/tests)

(deftest mcp-sampling-json-params
  (let* ((backend (llm-protocol:make-mock-llm-backend))
         (handler (llm-protocol/mcp:make-mcp-sampling-handler :backend backend))
         (client (make-instance 'mcp-protocol:mcp-client :sampling-handler handler))
         (params (mcp-protocol:json-object
                  "messages" (vector (mcp-protocol:json-object
                                      "role" "user"
                                      "content" (mcp-protocol:make-text-content "ping")))
                  "maxTokens" 16
                  "modelPreferences" (mcp-protocol:json-object
                                      "hints" (vector (mcp-protocol:json-object
                                                       "name" "mock")))))
         (out (mcp-protocol:create-message client params)))
    (ok (equal "assistant" (gethash "role" out)))
    (ok (equal "echo: ping" (gethash "text" (gethash "content" out))))
    (ok (equal "endTurn" (gethash "stopReason" out)))
    (ok (equal "mock" (gethash "model" out)))))

(deftest mcp-sampling-request-object
  (let* ((backend (llm-protocol:make-mock-llm-backend :prefix "s:"))
         (handler (llm-protocol/mcp:make-mcp-sampling-handler :backend backend))
         (req (mcp-protocol:make-mcp-sampling-request
               (list (make-instance 'mcp-protocol:mcp-sampling-message
                                    :role "user"
                                    :content "pong"))
               :system-prompt "sys"
               :max-tokens 8))
         (out (funcall handler req)))
    (ok (equal "s:pong" (gethash "text" (gethash "content" out))))))
