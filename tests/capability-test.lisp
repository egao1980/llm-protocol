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
                 cap 'capability-protocol:complete "yo"))))))
