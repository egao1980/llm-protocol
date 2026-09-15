(in-package #:llm-protocol/tests)

(deftest capability-complete
  (let* ((backend (llm-protocol:make-mock-llm-backend))
         (cap (llm-protocol:make-llm-generation-adapter :backend backend))
         (bb (blackboard-protocol:make-blackboard))
         (r (capability-protocol:complete cap "hi")))
    (ok (equal "echo: hi" (llm-protocol:llm-response-text r)))
    (capability-protocol:register-capability bb cap)
    (ok (eq cap (capability-protocol:get-capability bb :llm-generation)))
    (ok (equal "echo: yo"
               (llm-protocol:llm-response-text
                (capability-protocol:invoke-operation
                 cap 'capability-protocol:complete "yo"))))
    (ok (capability-protocol:capability-supported-p bb :llm-generation))))

(deftest llm-catalogue-from-backend
  (let* ((backend (llm-protocol:make-mock-llm-backend))
         (cat (llm-protocol:make-llm-catalogue backend)))
    (ok (capability-protocol:capability-supported-p cat :llm-generation))
    (ok (capability-protocol:capability-supported-p cat :llm-tools))
    (ok (capability-protocol:capability-supported-p cat :llm-responses))
    (ok (capability-protocol:capability-supported-p cat :llm-embeddings))
    (ok (capability-protocol:capability-supported-p cat :llm-structured-output))
    (ng (capability-protocol:capability-supported-p cat :llm-vision))
    (ok (capability-protocol:catalogue-defines-p cat :llm-vision))
    (let ((gen (capability-protocol:get-capability cat :llm-generation)))
      (ok (find 'capability-protocol:stream-complete
                (capability-protocol:capability-operations gen)
                :key #'capability-protocol:capability-operation-name))
      (ok (equal "echo: hi"
                 (llm-protocol:llm-response-text
                  (capability-protocol:stream-complete gen "hi")))))
    (let ((emb (capability-protocol:get-capability cat :llm-embeddings)))
      (ok (llm-protocol:llm-embed-result-p
           (capability-protocol:embed emb "hi"))))
    (let ((bb (blackboard-protocol:make-blackboard)))
      (llm-protocol:register-llm-backend bb backend)
      (ok (capability-protocol:capability-supported-p bb :llm-tools))
      (ng (capability-protocol:capability-supported-p bb :llm-vision)))))
