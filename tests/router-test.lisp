(in-package #:llm-protocol/tests)

(deftest router-generate-delegates
  (let* ((b (llm-protocol:make-mock-llm-backend :prefix "sel: "))
         (policy (llm-protocol:make-fallback-chain-policy :candidates (list b)))
         (router (llm-protocol:make-llm-router-backend
                  :policy policy :candidates (list b)))
         (r (llm-protocol:generate router "hi")))
    (ok (llm-protocol:llm-router-backend-p router))
    (ok (equal "sel: hi" (llm-protocol:llm-response-text r)))))

(deftest fallback-chain-advances-on-429
  (let* ((n 0)
         (fail (llm-protocol:make-mock-llm-backend
                :handler (lambda (backend turns &key &allow-other-keys)
                           (declare (ignore backend turns))
                           (incf n)
                           (error 'llm-protocol:llm-http-error
                                  :status 429 :message "rate"))))
         (ok-b (llm-protocol:make-mock-llm-backend :prefix "ok: "))
         (cands (list fail ok-b))
         (policy (llm-protocol:make-fallback-chain-policy :candidates cands))
         (router (llm-protocol:make-llm-router-backend
                  :policy policy :candidates cands))
         (r (llm-protocol:generate router "hi")))
    (ok (equal "ok: hi" (llm-protocol:llm-response-text r)))
    (ok (= 1 n))))

(deftest fallback-chain-does-not-advance-on-400
  (let* ((n 0)
         (fail (llm-protocol:make-mock-llm-backend
                :handler (lambda (backend turns &key &allow-other-keys)
                           (declare (ignore backend turns))
                           (incf n)
                           (error 'llm-protocol:llm-http-error
                                  :status 400 :message "bad" :retryable-p nil))))
         (ok-b (llm-protocol:make-mock-llm-backend :prefix "ok: "))
         (cands (list fail ok-b))
         (router (llm-protocol:make-llm-router-backend
                  :policy (llm-protocol:make-fallback-chain-policy
                           :candidates cands)
                  :candidates cands)))
    (ok (signals (llm-protocol:generate router "hi")
                 'llm-protocol:llm-http-error))
    (ok (= 1 n))))

(deftest budget-policy-exceeded-continue-anyway
  (let* ((b (llm-protocol:make-mock-llm-backend
             :handler (lambda (backend turns &key &allow-other-keys)
                        (declare (ignore backend turns))
                        (llm-protocol:make-llm-response
                         :parts (list (llm-protocol:make-llm-text-part :text "x"))
                         :usage (llm-protocol:make-llm-usage
                                 :input-tokens 10
                                 :output-tokens 5
                                 :total-tokens 15)))))
         (policy (llm-protocol:make-budget-policy
                  :inner (llm-protocol:make-fallback-chain-policy
                          :candidates (list b))
                  :budget (llm-protocol:make-llm-budget :max-tokens 10)))
         (router (llm-protocol:make-llm-router-backend
                  :policy policy :candidates (list b))))
    (ok (llm-protocol:llm-response-p (llm-protocol:generate router "first")))
    (ok (signals (llm-protocol:generate router "second")
                 'llm-protocol:llm-budget-exceeded))
    (let ((r (handler-bind ((llm-protocol:llm-budget-exceeded
                             (lambda (c)
                               (llm-protocol:invoke-continue-anyway c))))
               (llm-protocol:generate router "second"))))
      (ok (llm-protocol:llm-response-p r))
      (ok (equal "x" (llm-protocol:llm-response-text r))))))

(deftest router-embed-delegates
  (let* ((b (llm-protocol:make-mock-llm-backend))
         (router (llm-protocol:make-llm-router-backend :candidates (list b)))
         (r (llm-protocol:embed router "ab" :dimensions 4)))
    (ok (llm-protocol:llm-embed-result-p r))
    (ok (= 4 (length (llm-protocol:llm-embedding-vector
                      (first (llm-protocol:llm-embed-result-embeddings r))))))))
