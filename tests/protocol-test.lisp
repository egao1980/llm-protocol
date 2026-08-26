(in-package #:llm-protocol/tests)

(deftest no-backend-signals
  (let ((llm-protocol:*llm-backend* nil))
    (ok (signals (llm-protocol:generate nil "hi")
                 'llm-protocol:llm-missing-backend))
    (ok (signals (llm-protocol:list-models nil)
                 'llm-protocol:llm-missing-backend))))

(deftest coerce-turns-shapes
  (let ((from-string (llm-protocol:coerce-turns "hello")))
    (ok (= 1 (length from-string)))
    (ok (eq :user (llm-protocol:llm-turn-role (first from-string))))
    (ok (equal "hello" (llm-protocol:turn-text (first from-string)))))
  (let ((from-plist (llm-protocol:coerce-turns '(:role :assistant :content "a"))))
    (ok (eq :assistant (llm-protocol:llm-turn-role (first from-plist)))))
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "role" ht) "user")
    (setf (gethash "content" ht) (let ((c (make-hash-table :test 'equal)))
                                   (setf (gethash "type" c) "text")
                                   (setf (gethash "text" c) "ping")
                                   c))
    (ok (equal "ping" (llm-protocol:turn-text
                       (first (llm-protocol:coerce-turns ht)))))))

(deftest user-assistant-turns
  (let ((turns (list (llm-protocol:system-turn "sys")
                     (llm-protocol:user-turn "hi"))))
    (ok (eq :system (llm-protocol:llm-turn-role (first turns))))
    (ok (equal "hi" (llm-protocol:turn-text (second turns))))))

(deftest mock-echo
  (let* ((backend (llm-protocol:make-mock-llm-backend))
         (r (llm-protocol:generate backend "hi")))
    (ok (llm-protocol:llm-response-p r))
    (ok (equal "echo: hi" (llm-protocol:llm-response-text r)))
    (ok (equal "mock" (llm-protocol:llm-response-model r)))
    (ok (eq :stop (llm-protocol:llm-response-finish-reason r)))))

(deftest mock-star-backend
  (let ((llm-protocol:*llm-backend* (llm-protocol:make-mock-llm-backend :prefix "x:")))
    (ok (equal "x:yo" (llm-protocol:llm-response-text (llm-protocol:generate nil "yo"))))))

(deftest mock-tool-calls
  (let* ((tc (llm-protocol:make-llm-tool-call-part :id "c1" :name "sum"
                                                   :arguments "{\"a\":1}"))
         (backend (llm-protocol:make-mock-llm-backend :tool-calls (list tc)))
         (r (llm-protocol:generate backend "use tools")))
    (ok (eq :tool-use (llm-protocol:llm-response-finish-reason r)))
    (ok (equal "sum" (llm-protocol:llm-tool-call-part-name
                      (first (llm-protocol:llm-response-tool-calls r)))))))

(deftest mock-list-models
  (let ((models (llm-protocol:list-models (llm-protocol:make-mock-llm-backend))))
    (ok (equal "mock" (llm-protocol:llm-model-info-id (first models))))))

(deftest mock-supports
  (let ((b (llm-protocol:make-mock-llm-backend)))
    (ok (llm-protocol:backend-supports-p b :tools))
    (ok (llm-protocol:backend-supports-p b :stream))
    (ng (llm-protocol:backend-supports-p b :vision))))

(deftest mock-stream-generate
  (let* ((seen nil)
         (r (llm-protocol:stream-generate
             (llm-protocol:make-mock-llm-backend)
             "hi"
             :on-part (lambda (p) (push p seen)))))
    (ok (equal "echo: hi" (llm-protocol:llm-response-text r)))
    (ok (llm-protocol:llm-text-part-p (first seen)))))

(deftest mock-custom-handler
  (let* ((backend (llm-protocol:make-mock-llm-backend
                   :handler (lambda (b turns &key model &allow-other-keys)
                              (declare (ignore b turns))
                              (llm-protocol:make-llm-response
                               :parts (list (llm-protocol:make-llm-text-part
                                             :text "canned"))
                               :model (or model "h")))))
         (r (llm-protocol:generate backend "ignored" :model "m1")))
    (ok (equal "canned" (llm-protocol:llm-response-text r)))
    (ok (equal "m1" (llm-protocol:llm-response-model r)))))

(deftest settings-plist
  (ok (llm-protocol:llm-settings-p
       (llm-protocol:coerce-settings '(:temperature 0 :max-tokens 16))))
  (ok (zerop (llm-protocol:llm-settings-temperature
              (llm-protocol:coerce-settings '(:temperature 0))))))
