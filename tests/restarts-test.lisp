(in-package #:llm-protocol/tests)

(deftest http-status-retryable
  (ok (llm-protocol:http-status-retryable-p 429))
  (ok (llm-protocol:http-status-retryable-p 408))
  (ok (llm-protocol:http-status-retryable-p 503))
  (ng (llm-protocol:http-status-retryable-p 400))
  (ng (llm-protocol:http-status-retryable-p 401)))

(deftest http-error-retryable-slot
  (ok (llm-protocol:llm-http-error-retryable-p
       (make-condition 'llm-protocol:llm-http-error :status 429)))
  (ng (llm-protocol:llm-http-error-retryable-p
       (make-condition 'llm-protocol:llm-http-error :status 401)))
  (ok (llm-protocol:llm-http-error-retryable-p
       (make-condition 'llm-protocol:llm-http-error :status 400 :retryable-p t))))

(deftest generate-retry-then-succeed
  (let* ((n 0)
         (b (llm-protocol:make-mock-llm-backend
             :handler (lambda (backend turns &key &allow-other-keys)
                        (declare (ignore backend turns))
                        (incf n)
                        (if (= n 1)
                            (error 'llm-protocol:llm-http-error
                                   :status 429 :message "rate" :retryable-p t)
                            (llm-protocol:make-llm-response
                             :parts (list (llm-protocol:make-llm-text-part :text "ok"))
                             :model "mock"))))))
    (let ((text (llm-protocol:llm-response-text
                 (llm-protocol:with-auto-retry
                   (llm-protocol:generate b "hi")))))
      (ok (equal "ok" text)))
    (ok (= 2 n))))

(deftest generate-use-value-substitutes
  (let ((b (llm-protocol:make-mock-llm-backend
            :handler (lambda (backend turns &key &allow-other-keys)
                       (declare (ignore backend turns))
                       (error 'llm-protocol:llm-http-error
                              :status 500 :message "boom" :retryable-p t)))))
    (let ((text (llm-protocol:llm-response-text
                 (handler-bind ((llm-protocol:llm-http-error
                                 (lambda (c)
                                   (use-value
                                    (llm-protocol:make-llm-response
                                     :parts (list (llm-protocol:make-llm-text-part
                                                   :text "supplied")))
                                    c))))
                   (llm-protocol:generate b "hi")))))
      (ok (equal "supplied" text)))))

(deftest generate-non-retryable-does-not-auto-retry
  (let* ((n 0)
         (b (llm-protocol:make-mock-llm-backend
             :handler (lambda (backend turns &key &allow-other-keys)
                        (declare (ignore backend turns))
                        (incf n)
                        (error 'llm-protocol:llm-http-error
                               :status 400 :message "bad" :retryable-p nil)))))
    (ok (signals (llm-protocol:with-auto-retry (llm-protocol:generate b "hi"))
                 'llm-protocol:llm-http-error))
    (ok (= 1 n))))

(deftest output-error-ignore-output
  (let ((llm-protocol:*structured-output-repair* :signal)
        (b (llm-protocol:make-mock-llm-backend
            :handler (lambda (backend turns &key &allow-other-keys)
                       (declare (ignore backend turns))
                       (llm-protocol:make-llm-response
                        :parts (list (llm-protocol:make-llm-text-part :text "{}"))
                        :model "mock")))))
    (let ((r (llm-protocol:with-auto-ignore-output
               (llm-protocol:generate b "city" :output '%llm-city))))
      (ok (llm-protocol:llm-response-p r))
      (ok (null (llm-protocol:llm-response-output r))))))

(deftest output-error-use-value
  (let ((llm-protocol:*structured-output-repair* :signal)
        (b (llm-protocol:make-mock-llm-backend
            :handler (lambda (backend turns &key &allow-other-keys)
                       (declare (ignore backend turns))
                       (llm-protocol:make-llm-response
                        :parts (list (llm-protocol:make-llm-text-part :text "{}"))
                        :model "mock")))))
    (let ((r (handler-bind ((llm-protocol:llm-output-error
                             (lambda (c)
                               (use-value :patched c))))
               (llm-protocol:generate b "city" :output '%llm-city))))
      (ok (eq :patched (llm-protocol:llm-response-output r))))))

(deftest retry-classes
  (flet ((cls (status &rest args)
           (llm-protocol:llm-retry-class
            (apply #'make-condition 'llm-protocol:llm-http-error :status status args))))
    (ok (eq :rate-limit (cls 429)))
    (ok (eq :overloaded (cls 529)))
    (ok (eq :overloaded (cls 503)))
    (ok (eq :transient (cls 500)))
    (ng (cls 401))
    (ng (cls 500 :retryable-p nil)))
  (let ((c (make-condition 'llm-protocol:llm-context-overflow :status 400)))
    (ok (eq :context-overflow (llm-protocol:llm-retry-class c)))
    (ng (llm-protocol:llm-http-error-retryable-p c)))
  (ng (llm-protocol:llm-retry-class (make-condition 'llm-protocol:llm-error)))
  (ok (llm-protocol:context-overflow-text-p "prompt is too long: 210000 tokens > 200000 maximum"))
  (ok (llm-protocol:context-overflow-text-p "{\"code\":\"context_length_exceeded\"}"))
  (ng (llm-protocol:context-overflow-text-p "invalid x-api-key")))

(deftest retry-delay-backoff-and-retry-after
  (let ((p (llm-protocol:make-retry-policy :max-attempts 4 :base-delay 1 :max-delay 10))
        (c (make-condition 'llm-protocol:llm-http-error :status 500)))
    (ok (= 1 (llm-protocol:retry-delay p c 1)))
    (ok (= 2 (llm-protocol:retry-delay p c 2)))
    (ok (= 4 (llm-protocol:retry-delay p c 3)))
    (ng (llm-protocol:retry-delay p c 4))
    (ok (= 4 (llm-protocol:retry-delay
              p (make-condition 'llm-protocol:llm-http-error :status 529) 2)))
    (ok (= 7 (llm-protocol:retry-delay
              p (make-condition 'llm-protocol:llm-http-error :status 429 :retry-after 7) 1)))
    (ok (= 10 (llm-protocol:retry-delay
               p (make-condition 'llm-protocol:llm-http-error :status 429 :retry-after 99) 1)))
    (ng (llm-protocol:retry-delay
         p (make-condition 'llm-protocol:llm-context-overflow :status 400) 1))
    (ng (llm-protocol:retry-delay
         (llm-protocol:make-retry-policy :classes '(:rate-limit)) c 1))))

(deftest call-with-retry-policy-sleeps-between-tries
  (let ((n 0) (slept '()))
    (ok (eq :done
            (llm-protocol:call-with-retry-policy
             (lambda ()
               (incf n)
               (if (< n 3)
                   (error 'llm-protocol:llm-http-error :status 503)
                   :done))
             :policy (llm-protocol:make-retry-policy :base-delay 1)
             :sleep (lambda (s) (push s slept)))))
    (ok (= 3 n))
    (ok (equal '(4 2) slept)))
  (let ((n 0))
    (ok (signals (llm-protocol:call-with-retry-policy
                  (lambda ()
                    (incf n)
                    (error 'llm-protocol:llm-http-error :status 401))
                  :sleep (lambda (s) (declare (ignore s))))
                 'llm-protocol:llm-http-error))
    (ok (= 1 n)))
  (let ((n 0))
    (ok (signals (llm-protocol:call-with-retry-policy
                  (lambda ()
                    (incf n)
                    (error 'llm-protocol:llm-http-error :status 500))
                  :policy (llm-protocol:make-retry-policy :max-attempts 2)
                  :sleep (lambda (s) (declare (ignore s))))
                 'llm-protocol:llm-http-error))
    (ok (= 2 n))))

(defun %partial-response ()
  (llm-protocol:make-llm-response
   :parts (list (llm-protocol:make-llm-text-part :text "half"))
   :finish-reason :incomplete))

(deftest incomplete-response-condition
  (let ((partial (%partial-response)))
    (handler-case (llm-protocol:signal-incomplete-response partial :message "cut")
      (llm-protocol:llm-incomplete-response (c)
        (ok (typep c 'llm-protocol:llm-http-error))
        (ok (eq partial (llm-protocol:llm-incomplete-response-partial c)))
        (ok (llm-protocol:llm-http-error-retryable-p c))
        (ok (eq :transient (llm-protocol:llm-retry-class c)))
        (ok (search "cut" (princ-to-string c)))))
    (ok (typep :incomplete 'llm-protocol::llm-finish-reason))))

(deftest incomplete-response-use-partial-restart
  (let* ((partial (%partial-response))
         (r (handler-bind ((llm-protocol:llm-incomplete-response
                             #'llm-protocol:invoke-use-partial-response))
              (llm-protocol:signal-incomplete-response partial))))
    (ok (eq partial r))
    (ok (eq :incomplete (llm-protocol:llm-response-finish-reason r)))
    (ok (equal "half" (llm-protocol:llm-response-text r))))
  (ok (eq :other
          (handler-bind ((llm-protocol:llm-incomplete-response
                           (lambda (c) (llm-protocol:invoke-use-value :other c))))
            (llm-protocol:signal-incomplete-response (%partial-response))))))

(deftest incomplete-response-retried-by-policy
  (let ((n 0))
    (ok (eq :done
            (llm-protocol:call-with-retry-policy
             (lambda ()
               (incf n)
               (if (< n 2)
                   (llm-protocol:signal-incomplete-response (%partial-response))
                   :done))
             :sleep (lambda (s) (declare (ignore s))))))
    (ok (= 2 n))))
