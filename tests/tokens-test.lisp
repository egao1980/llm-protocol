(in-package #:llm-protocol/tests)

(deftest count-tokens-heuristic
  (let ((b (make-instance 'llm-protocol:llm-backend)))
    (ok (= 3 (llm-protocol:count-tokens b "123456789012")))
    (ok (= 1 (llm-protocol:count-tokens b "ab")))
    (ok (zerop (llm-protocol:count-tokens b "")))
    (ok (= 1 (llm-protocol:count-tokens b (llm-protocol:make-llm-text-part :text "abcd"))))
    (let ((turns (list (llm-protocol:user-turn "abcd")
                       (llm-protocol:user-turn "abcdefgh"))))
      (ok (= 3 (llm-protocol:count-tokens b turns))))
    (ok (= 1 (llm-protocol:count-tokens b (llm-protocol:user-turn "wxyz"))))))

(deftest fit-turns-keeps-system-drops-oldest
  (let* ((b (make-instance 'llm-protocol:llm-backend))
         (sys (llm-protocol:system-turn "sys"))
         (u1 (llm-protocol:user-turn "AAAA"))
         (u2 (llm-protocol:user-turn "BBBB"))
         (u3 (llm-protocol:user-turn "CCCC"))
         (fitted (llm-protocol:fit-turns (list sys u1 u2 u3) b :policy 3)))
    (ok (= 3 (length fitted)))
    (ok (eq :system (llm-protocol:llm-turn-role (first fitted))))
    (ok (equal "BBBB" (llm-protocol:turn-text (second fitted))))
    (ok (equal "CCCC" (llm-protocol:turn-text (third fitted))))))

(deftest fit-turns-reserve-uses-context-window
  (let* ((b (llm-protocol:make-mock-llm-backend
             :models (list (llm-protocol:make-llm-model-info
                            :id "mock" :context-window 4))))
         (sys (llm-protocol:system-turn "sys"))
         (u1 (llm-protocol:user-turn "AAAA"))
         (u2 (llm-protocol:user-turn "BBBB"))
         (u3 (llm-protocol:user-turn "CCCC"))
         (fitted (llm-protocol:fit-turns
                  (list sys u1 u2 u3) b
                  :policy (llm-protocol:make-token-fit-policy :reserve 2)
                  :model "mock")))
    (ok (= 2 (length fitted)))
    (ok (eq :system (llm-protocol:llm-turn-role (first fitted))))
    (ok (equal "CCCC" (llm-protocol:turn-text (second fitted))))))

(deftest context-window-from-model-info
  (let ((b (llm-protocol:make-mock-llm-backend
            :models (list (llm-protocol:make-llm-model-info
                           :id "mock" :context-window 2048
                           :input-price 1.0 :output-price 2.0)))))
    (ok (= 2048 (llm-protocol:context-window b "mock")))
    (ok (= 1.0 (llm-protocol:llm-model-info-input-price
                (first (llm-protocol:list-models b)))))
    (ok (null (llm-protocol:context-window
               (llm-protocol:make-mock-llm-backend) "missing")))))

(deftest context-window-from-catalog-provider
  (let* ((b (llm-protocol:make-mock-llm-backend))
         (cat (llm-protocol:make-in-memory-provider-catalog))
         (llm-protocol:*llm-catalog* cat))
    (llm-protocol:register-provider cat "mock" b :context-window 8192)
    (ok (= 8192 (llm-protocol:context-window b "mock")))))

(deftest anchored-token-count-uses-measured-usage
  (let* ((b (make-instance 'llm-protocol:llm-backend))
         (turns (list (llm-protocol:user-turn "abcd")
                      (llm-protocol:assistant-turn "abcd")
                      (llm-protocol:user-turn "abcdefgh")))
         (usage (llm-protocol:make-llm-usage :input-tokens 10 :output-tokens 5
                                             :cache-read-tokens 100
                                             :cache-write-tokens 20)))
    (ok (= 135 (llm-protocol:llm-usage-prompt-tokens
                (llm-protocol:make-llm-usage :input-tokens 10 :cache-read-tokens 100
                                             :cache-write-tokens 25))))
    (ok (= 4 (llm-protocol:anchored-token-count b turns)))
    ;; 130 measured prompt + 5 output, then "abcdefgh" estimated as 2.
    (ok (= 137 (llm-protocol:anchored-token-count b turns :usage usage :anchor-count 2)))
    (ok (= 4 (llm-protocol:anchored-token-count
              b turns :usage (llm-protocol:make-llm-usage) :anchor-count 2)))))
