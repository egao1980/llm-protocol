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
  (let ((b (llm-protocol:make-mock-llm-backend
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
  (let ((b (llm-protocol:make-mock-llm-backend
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
