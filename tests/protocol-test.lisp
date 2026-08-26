(in-package #:llm-protocol/tests)

(deftest no-backend-signals
  (let ((llm-protocol:*llm-backend* nil))
    (ok (signals (llm-protocol:generate nil "hi")
                 'llm-protocol:llm-missing-backend))
    (ok (signals (llm-protocol:list-models nil)
                 'llm-protocol:llm-missing-backend))))

(deftest coerce-messages-shapes
  (let ((from-string (llm-protocol:coerce-messages "hello")))
    (ok (= 1 (length from-string)))
    (ok (equal "user" (llm-protocol:llm-message-role (first from-string))))
    (ok (equal "hello" (llm-protocol:llm-message-content (first from-string)))))
  (let ((from-plist (llm-protocol:coerce-messages '(:role :assistant :content "a"))))
    (ok (equal "assistant" (llm-protocol:llm-message-role (first from-plist)))))
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "role" ht) "user")
    (setf (gethash "content" ht) (let ((c (make-hash-table :test 'equal)))
                                   (setf (gethash "type" c) "text")
                                   (setf (gethash "text" c) "ping")
                                   c))
    (ok (equal "ping" (llm-protocol:llm-message-content
                       (first (llm-protocol:coerce-messages ht)))))))

(deftest mock-echo
  (let* ((backend (llm-protocol:make-mock-llm-backend))
         (r (llm-protocol:generate backend "hi")))
    (ok (llm-protocol:llm-result-p r))
    (ok (equal "echo: hi" (llm-protocol:llm-result-text r)))
    (ok (equal "mock" (llm-protocol:llm-result-model r)))
    (ok (equal "stop" (llm-protocol:llm-result-finish-reason r)))))

(deftest mock-star-backend
  (let ((llm-protocol:*llm-backend* (llm-protocol:make-mock-llm-backend :prefix "x:")))
    (ok (equal "x:yo" (llm-protocol:llm-result-text (llm-protocol:generate nil "yo"))))))

(deftest mock-tool-calls
  (let* ((tc (llm-protocol:make-llm-tool-call :id "c1" :name "sum" :arguments "{\"a\":1}"))
         (backend (llm-protocol:make-mock-llm-backend :tool-calls (list tc)))
         (r (llm-protocol:generate backend "use tools")))
    (ok (equal "tool_calls" (llm-protocol:llm-result-finish-reason r)))
    (ok (equal "sum" (llm-protocol:llm-tool-call-name
                      (first (llm-protocol:llm-message-tool-calls
                              (llm-protocol:llm-result-message r))))))))

(deftest mock-list-models
  (let ((models (llm-protocol:list-models (llm-protocol:make-mock-llm-backend))))
    (ok (equal "mock" (llm-protocol:llm-model-id (first models))))))

(deftest mock-stream-unsupported
  (ok (signals (llm-protocol:generate (llm-protocol:make-mock-llm-backend) "hi"
                                      :stream t)
               'llm-protocol:llm-unsupported)))

(deftest mock-custom-handler
  (let* ((backend (llm-protocol:make-mock-llm-backend
                   :handler (lambda (b messages &key model &allow-other-keys)
                              (declare (ignore b messages))
                              (llm-protocol:make-llm-result
                               :message (llm-protocol:make-llm-message
                                         :role "assistant" :content "canned")
                               :model (or model "h")))))
         (r (llm-protocol:generate backend "ignored" :model "m1")))
    (ok (equal "canned" (llm-protocol:llm-result-text r)))
    (ok (equal "m1" (llm-protocol:llm-result-model r)))))
