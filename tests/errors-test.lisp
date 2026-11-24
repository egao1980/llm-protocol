(in-package #:llm-protocol/tests)

;;; 0.7.0 error taxonomy (src/errors.lisp). Every case names its input so a
;;; failure says which row broke.

(defparameter +class-rows+
  '((429 nil "rate limit reached" llm-protocol:llm-rate-limited)
    (503 nil "service unavailable" llm-protocol:llm-overloaded)
    (529 nil "Overloaded" llm-protocol:llm-overloaded)
    (500 "{\"type\":\"error\",\"error\":{\"type\":\"overloaded_error\"}}" nil
     llm-protocol:llm-overloaded)
    (502 nil "The server is overloaded, try later" llm-protocol:llm-overloaded)
    (500 nil "internal error" llm-protocol:llm-http-error)
    (502 nil "bad gateway" llm-protocol:llm-http-error)
    (504 nil "gateway timeout" llm-protocol:llm-http-error)
    (408 nil "request timeout" llm-protocol:llm-http-error)
    (401 nil "invalid x-api-key" llm-protocol:llm-auth-error)
    (403 nil "forbidden" llm-protocol:llm-auth-error)
    (404 nil "model not found" llm-protocol:llm-not-found)
    (400 nil "bad request" llm-protocol:llm-http-error)
    (400 nil "prompt is too long: 210000 tokens > 200000 maximum"
     llm-protocol:llm-context-overflow)
    (400 "{\"error\":{\"code\":\"context_length_exceeded\"}}" "bad"
     llm-protocol:llm-context-overflow)
    (413 nil "request too large" llm-protocol:llm-context-overflow)
    (429 nil "context window exceeded" llm-protocol:llm-rate-limited))
  "(status body message expected-class). Overflow wording counts only on 400 / 413.")

(deftest http-error-class-rows
  (dolist (row +class-rows+)
    (destructuring-bind (status body message class) row
      (ok (eq class (llm-protocol:llm-http-error-class status :body body :message message))
          (format nil "~a ~s ~s → ~a" status body message class)))))

(deftest make-http-error-object
  (let ((c (llm-protocol:make-llm-http-error 429 :message "slow down" :retry-after "7"
                                                 :provider "openai"
                                                 :url "http://localhost:1234/v1"
                                                 :model "qwen")))
    (ok (typep c 'llm-protocol:llm-rate-limited))
    (ok (typep c 'llm-protocol:llm-http-error))
    (ok (eql 429 (llm-protocol:llm-http-error-status c)))
    (ok (eql 7 (llm-protocol:llm-http-error-retry-after c)))
    (ok (equal "openai" (llm-protocol:llm-error-provider c)))
    (ok (equal "http://localhost:1234/v1" (llm-protocol:llm-error-url c)))
    (ok (equal "qwen" (llm-protocol:llm-error-model c)))
    (ok (llm-protocol:llm-http-error-retryable-p c))
    (ok (search "slow down" (princ-to-string c)))))

(deftest make-http-error-retryable-flags
  (flet ((retryable (status &rest args)
           (llm-protocol:llm-http-error-retryable-p
            (apply #'llm-protocol:make-llm-http-error status args))))
    (ok (retryable 429))
    (ok (retryable 503))
    (ok (retryable 529))
    (ok (retryable 500))
    (ok (retryable 502))
    (ok (retryable 504))
    (ok (retryable 408))
    (ng (retryable 400))
    (ng (retryable 401))
    (ng (retryable 403))
    (ng (retryable 404))
    (ng (retryable 400 :message "maximum context length is 8192 tokens"))
    (ng (retryable 413 :message "request too large"))))

(deftest make-http-error-default-message
  (let ((c (llm-protocol:make-llm-http-error 500)))
    (ok (equal "HTTP 500" (llm-protocol:llm-error-message c)))))

(deftest make-http-error-is-not-signalled
  (ok (typep (llm-protocol:make-llm-http-error 401) 'condition))
  (ok (typep (llm-protocol:make-llm-http-error 401) 'llm-protocol:llm-auth-error)))

(deftest retry-class-of-new-types
  (ok (eq :rate-limit (llm-protocol:llm-retry-class
                       (llm-protocol:make-llm-http-error 429))))
  (ok (eq :overloaded (llm-protocol:llm-retry-class
                       (llm-protocol:make-llm-http-error 529))))
  (ok (eq :overloaded (llm-protocol:llm-retry-class
                       (llm-protocol:make-llm-http-error 500 :body "overloaded_error"))))
  (ok (eq :transient (llm-protocol:llm-retry-class
                      (llm-protocol:make-llm-http-error 502))))
  (ok (null (llm-protocol:llm-retry-class (llm-protocol:make-llm-http-error 401))))
  (ok (null (llm-protocol:llm-retry-class (llm-protocol:make-llm-http-error 404))))
  (ok (eq :context-overflow
          (llm-protocol:llm-retry-class
           (llm-protocol:make-llm-http-error 400 :message "prompt is too long"))))
  (ok (eq :transient (llm-protocol:llm-retry-class
                      (make-condition 'llm-protocol:llm-transport-error :kind :connect))))
  (ok (eq :transient (llm-protocol:llm-retry-class
                      (make-condition 'llm-protocol:llm-transport-error :kind :timeout))))
  (ok (null (llm-protocol:llm-retry-class
             (make-condition 'llm-protocol:llm-transport-error :kind :canceled)))))

(deftest parse-retry-after-rows
  (let ((now (encode-universal-time 0 0 12 1 1 2026 0)))
    (dolist (row `((7 7) (-3 0) (2.5 2.5)
                   ("120" 120) (" 5 " 5) ("1.5" 1.5) ("0" 0)
                   ("Thu, 01 Jan 2026 12:00:30 GMT" 30)
                   ("Thu, 01 Jan 2026 11:00:00 GMT" 0)
                   ("" nil) ("soon" nil) (nil nil)
                   ("Thursday, 01-Jan-26 12:00:30 GMT" nil)))
      (destructuring-bind (value expected) row
        (let ((got (llm-protocol:parse-retry-after value :now now)))
          (ok (if expected (and got (= expected got)) (null got))
              (format nil "~s → ~s (got ~s)" value expected got)))))))

(deftest retry-after-string-reaches-the-delay
  (let ((c (llm-protocol:make-llm-http-error 429 :retry-after "3"))
        (policy (llm-protocol:make-retry-policy :base-delay 1 :max-delay 30)))
    (ok (= 3 (llm-protocol:retry-delay policy c 1)))))

(deftest transport-error-object
  (let* ((cause (make-condition 'simple-error :format-control "connection refused"
                                              :format-arguments nil))
         (c (make-condition 'llm-protocol:llm-transport-error
                            :cause cause :kind :connect :provider "anthropic"
                            :url "https://api.anthropic.com")))
    (ok (typep c 'llm-protocol:llm-error))
    (ok (not (typep c 'llm-protocol:llm-http-error)))
    (ok (eq cause (llm-protocol:llm-transport-error-cause c)))
    (ok (search "transport error (connect)" (princ-to-string c)))
    (ok (search "connection refused" (princ-to-string c)))
    (ok (equal '(:connect :reset :tls :timeout :canceled)
               llm-protocol:+transport-error-kinds+))))

(deftest transport-error-kind-for-plain-conditions
  ;; Without http-protocol loaded (llm-protocol/tests does not load it) only
  ;; the message rule applies.
  (ok (eq :reset (llm-protocol:transport-error-kind-for
                  (make-condition 'simple-error
                                  :format-control "incomplete HTTP response"
                                  :format-arguments nil))))
  (ok (null (llm-protocol:transport-error-kind-for
             (make-condition 'simple-error :format-control "boom" :format-arguments nil)))))

(define-condition fake-timeout (error) ())
(define-condition fake-conn (error) ()
  (:report (lambda (c s) (declare (ignore c)) (write-string "connection reset by peer" s))))

(deftest transport-error-kind-for-custom-types
  ;; TYPES names are FIND-SYMBOLed in HTTP-PROTOCOL; with no such package the
  ;; rows are skipped, so these fall back to the message rule.
  (ok (null (llm-protocol:transport-error-kind-for
             (make-condition 'fake-timeout)
             :types '(("FAKE-TIMEOUT" . :timeout)))))
  (ok (equal '("HTTP-CANCELED" "HTTP-TIMEOUT-ERROR" "HTTP-TLS-ERROR" "HTTP-CONNECTION-ERROR")
             (mapcar #'car llm-protocol:+http-protocol-transport-types+))))

(deftest retry-policy-retries-transport-errors
  (let ((n 0) (sleeps '()))
    (ok (eq :ok
            (llm-protocol:call-with-retry-policy
             (lambda ()
               (if (< (incf n) 3)
                   (error 'llm-protocol:llm-transport-error :kind :reset)
                   :ok))
             :policy (llm-protocol:make-retry-policy :base-delay 1)
             :sleep (lambda (s) (push s sleeps)))))
    (ok (= 3 n))
    (ok (equal '(2 1) sleeps))))

(deftest retry-policy-never-retries-auth
  (let ((n 0))
    (ok (signals (llm-protocol:call-with-retry-policy
                  (lambda ()
                    (incf n)
                    (error (llm-protocol:make-llm-http-error 401 :message "bad key")))
                  :sleep (lambda (s) (declare (ignore s))))
                 'llm-protocol:llm-auth-error))
    (ok (= 1 n))))

(deftest jitter-rules
  (ok (= 4 (llm-protocol:jittered-delay 4 0)))
  (let ((llm-protocol:*retry-random-state* (make-random-state t)))
    (dotimes (i 50)
      (let ((d (llm-protocol:jittered-delay 4 0.5)))
        (ok (and (<= 2 d) (<= d 4)) (format nil "~a in [2, 4]" d))))))

(deftest jitter-in-retry-delay
  (let ((policy (llm-protocol:make-retry-policy :base-delay 2 :jitter 0.25))
        (after (llm-protocol:make-llm-http-error 429 :retry-after 10))
        (plain (llm-protocol:make-llm-http-error 502)))
    (ok (= 0.25 (llm-protocol:retry-policy-jitter policy)))
    (ok (= 10 (llm-protocol:retry-delay policy after 1)) "Retry-After is never jittered")
    (let ((llm-protocol:*retry-random-state* (make-random-state t)))
      (let ((d (llm-protocol:retry-delay policy plain 2)))
        (ok (and (<= 3 d) (<= d 4)) (format nil "backoff 4 jittered to ~a" d))))
    (ok (= 4 (llm-protocol:retry-delay (llm-protocol:make-retry-policy :base-delay 2)
                                       plain 2))
        "jitter 0 keeps the 0.6 delays")))

(deftest make-retry-policy-checks-jitter
  (ok (signals (llm-protocol:make-retry-policy :jitter 2) 'type-error))
  (ok (signals (llm-protocol:make-retry-policy :jitter -0.1) 'type-error)))
