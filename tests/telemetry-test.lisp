(in-package #:llm-protocol/tests)

(defun %tel-attr (attrs key)
  (loop for (k v) on attrs by #'cddr
        when (equal k key)
          return v))

(defun %tel-span (spans name)
  (find name spans :key #'telemetry-protocol:telemetry-span-name :test #'equal))

(defmacro with-recording-llm-telemetry (&body body)
  `(let ((telemetry-protocol:*telemetry-backend*
           (telemetry-protocol:make-recording-telemetry-backend))
         (telemetry-protocol:*tracer-provider* nil)
         (telemetry-protocol:*current-span* nil)
         (telemetry-protocol:*current-trace-id* nil))
     ,@body))

(deftest generate-span-records-model-and-usage
  (with-recording-llm-telemetry
    (let* ((b (llm-protocol:make-mock-llm-backend))
           (r (llm-protocol:generate b "hi"))
           (usage (llm-protocol:llm-response-usage r))
           (span (%tel-span (telemetry-protocol:recorded-spans
                             telemetry-protocol:*telemetry-backend*)
                            "generate"))
           (attrs (and span (telemetry-protocol:telemetry-span-attributes span))))
      (ok (llm-protocol:llm-response-p r))
      (ok span)
      (ok (equal "chat" (%tel-attr attrs telemetry-protocol:+gen-ai-operation-name+)))
      (ok (equal "mock" (%tel-attr attrs telemetry-protocol:+gen-ai-request-model+)))
      (ok (equal "mock" (%tel-attr attrs telemetry-protocol:+gen-ai-response-model+)))
      (ok (= (llm-protocol:llm-usage-input-tokens usage)
             (%tel-attr attrs telemetry-protocol:+gen-ai-usage-input-tokens+)))
      (ok (= (llm-protocol:llm-usage-output-tokens usage)
             (%tel-attr attrs telemetry-protocol:+gen-ai-usage-output-tokens+)))
      (ok (null (%tel-attr attrs llm-protocol::+gen-ai-usage-cost+))))))

(deftest generate-span-cost-from-catalog-prices
  (with-recording-llm-telemetry
    (let* ((b (llm-protocol:make-mock-llm-backend))
           (cat (llm-protocol:make-in-memory-provider-catalog))
           (llm-protocol:*llm-catalog* cat))
      (llm-protocol:register-provider
       cat "mock" b
       :models (list (llm-protocol:make-llm-model-info
                      :id "mock" :input-price 1.0d0 :output-price 2.0d0)))
      (let* ((r (llm-protocol:generate b "hi"))
             (usage (llm-protocol:llm-response-usage r))
             (in (llm-protocol:llm-usage-input-tokens usage))
             (out (llm-protocol:llm-usage-output-tokens usage))
             (expected (+ (* in (/ 1.0d0 1000000.0d0))
                          (* out (/ 2.0d0 1000000.0d0))))
             (span (%tel-span (telemetry-protocol:recorded-spans
                               telemetry-protocol:*telemetry-backend*)
                              "generate"))
             (cost (%tel-attr (telemetry-protocol:telemetry-span-attributes span)
                              llm-protocol::+gen-ai-usage-cost+)))
        (ok span)
        (ok (numberp cost))
        (ok (< (abs (- cost expected)) 1d-12))))))

(deftest generate-span-cost-from-list-models-prices
  (with-recording-llm-telemetry
    (let ((b (llm-protocol:make-mock-llm-backend
              :models (list (llm-protocol:make-llm-model-info
                             :id "mock" :input-price 3.0d0 :output-price 4.0d0)))))
      (let* ((r (llm-protocol:generate b "hi"))
             (usage (llm-protocol:llm-response-usage r))
             (expected (+ (* (llm-protocol:llm-usage-input-tokens usage)
                             (/ 3.0d0 1000000.0d0))
                          (* (llm-protocol:llm-usage-output-tokens usage)
                             (/ 4.0d0 1000000.0d0))))
             (span (%tel-span (telemetry-protocol:recorded-spans
                               telemetry-protocol:*telemetry-backend*)
                              "generate"))
             (cost (%tel-attr (telemetry-protocol:telemetry-span-attributes span)
                              llm-protocol::+gen-ai-usage-cost+)))
        (ok span)
        (ok (numberp cost))
        (ok (< (abs (- cost expected)) 1d-12))))))

(deftest respond-and-embed-spans
  (with-recording-llm-telemetry
    (let ((b (llm-protocol:make-mock-llm-backend)))
      (llm-protocol:respond b "hi")
      (llm-protocol:embed b "ab" :dimensions 4)
      (let* ((spans (telemetry-protocol:recorded-spans
                     telemetry-protocol:*telemetry-backend*))
             (respond (%tel-span spans "respond"))
             (embed (%tel-span spans "embeddings")))
        (ok respond)
        (ok embed)
        (ok (equal "chat"
                   (%tel-attr (telemetry-protocol:telemetry-span-attributes respond)
                              telemetry-protocol:+gen-ai-operation-name+)))
        (ok (equal "embeddings"
                   (%tel-attr (telemetry-protocol:telemetry-span-attributes embed)
                              telemetry-protocol:+gen-ai-operation-name+)))
        (ok (equal "mock"
                   (%tel-attr (telemetry-protocol:telemetry-span-attributes embed)
                              telemetry-protocol:+gen-ai-request-model+)))
        (ok (numberp
             (%tel-attr (telemetry-protocol:telemetry-span-attributes embed)
                        telemetry-protocol:+gen-ai-usage-input-tokens+)))))))

(deftest stream-generate-span
  (with-recording-llm-telemetry
    (let ((b (llm-protocol:make-mock-llm-backend)))
      (ok (equal "echo: hi"
                 (llm-protocol:llm-response-text
                  (llm-protocol:stream-generate b "hi"))))
      (ok (%tel-span (telemetry-protocol:recorded-spans
                      telemetry-protocol:*telemetry-backend*)
                     "stream-generate")))))

(deftest noop-telemetry-still-generates
  (telemetry-protocol:use-noop-telemetry)
  (ok (equal "echo: hi"
             (llm-protocol:llm-response-text
              (llm-protocol:generate (llm-protocol:make-mock-llm-backend) "hi")))))
