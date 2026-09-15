(in-package #:llm-protocol/tests)

(deftest no-backend-signals
  (let ((llm-protocol:*llm-backend* nil))
    (ok (signals (llm-protocol:generate nil "hi")
                 'llm-protocol:llm-missing-backend))
    (ok (signals (llm-protocol:list-models nil)
                 'llm-protocol:llm-missing-backend))
    (ok (signals (llm-protocol:embed nil "hi")
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
    (ok (llm-protocol:backend-supports-p b :responses))
    (ok (llm-protocol:backend-supports-p b :embeddings))
    (ok (llm-protocol:backend-supports-p b :structured-output))
    (ng (llm-protocol:backend-supports-p b :vision))
    (ng (llm-protocol:backend-supports-p (make-instance 'llm-protocol:llm-backend)
                                         :structured-output))))

(deftest mock-embed
  (let* ((b (llm-protocol:make-mock-llm-backend))
         (r (llm-protocol:embed b "ab" :dimensions 4)))
    (ok (llm-protocol:llm-embed-result-p r))
    (ok (equal "mock" (llm-protocol:llm-embed-result-model r)))
    (let ((v (llm-protocol:llm-embedding-vector
              (first (llm-protocol:llm-embed-result-embeddings r)))))
      (ok (= 4 (length v)))
      (ok (= (float (char-code #\a) 1f0) (aref v 0)))
      (ok (= (float (char-code #\b) 1f0) (aref v 1)))
      (ok (zerop (aref v 2)))))
  (let* ((r (llm-protocol:embed (llm-protocol:make-mock-llm-backend)
                                '("x" "y") :model "e"))
         (embs (llm-protocol:llm-embed-result-embeddings r)))
    (ok (= 2 (length embs)))
    (ok (zerop (llm-protocol:llm-embedding-index (first embs))))
    (ok (= 1 (llm-protocol:llm-embedding-index (second embs))))
    (ok (equal "e" (llm-protocol:llm-embed-result-model r))))
  (let ((v (llm-protocol:embed-query (llm-protocol:make-mock-llm-backend) "z"
                                     :dimensions 2)))
    (ok (= 2 (length v)))
    (ok (= (float (char-code #\z) 1f0) (aref v 0)))))

(deftest default-embed-unsupported
  (ok (signals (llm-protocol:embed (make-instance 'llm-protocol:llm-backend) "hi")
               'llm-protocol:llm-unsupported)))

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

(deftest reasoning-text-part
  (let* ((h (make-hash-table :test 'equal)))
    (setf (gethash "type" h) "reasoning_text"
          (gethash "text" h) "scratch")
    (let ((p (llm-protocol::%coerce-part h)))
      (ok (llm-protocol:llm-thinking-part-p p))
      (ok (equal "scratch" (llm-protocol:llm-thinking-part-text p))))))

(deftest items-roundtrip
  (let* ((turns (list (llm-protocol:system-turn "be brief")
                      (llm-protocol:user-turn "ping")
                      (llm-protocol:assistant-turn "pong"
                       :tool-calls (list (llm-protocol:make-llm-tool-call-part
                                          :id "c1" :name "sum" :arguments "{}"))
                       :thinking "hmm")
                      (llm-protocol:tool-turn "c1" "3" :name "sum")))
         (items (llm-protocol:turns->items turns))
         (back (llm-protocol:items->turns items)))
    (ok (llm-protocol:llm-message-item-p (first items)))
    (ok (eq :system (llm-protocol:llm-message-item-role (first items))))
    (ok (find-if #'llm-protocol:llm-reasoning-item-p items))
    (ok (find-if #'llm-protocol:llm-function-call-item-p items))
    (ok (find-if #'llm-protocol:llm-function-call-output-item-p items))
    (ok (equal "ping" (llm-protocol:turn-text (second back))))
    (ok (eq :assistant (llm-protocol:llm-turn-role (third back))))
    (ok (find-if #'llm-protocol:llm-thinking-part-p
                 (llm-protocol:llm-turn-parts (third back))))
    (ok (eq :tool (llm-protocol:llm-turn-role (fourth back))))))

(deftest reasoning-item-lmstudio-content
  (let* ((part (make-hash-table :test 'equal))
         (item (make-hash-table :test 'equal)))
    (setf (gethash "type" part) "reasoning_text"
          (gethash "text" part) "scratch")
    (setf (gethash "type" item) "reasoning"
          (gethash "summary" item) #()
          (gethash "content" item) (vector part))
    (let ((it (llm-protocol:coerce-item item)))
      (ok (llm-protocol:llm-reasoning-item-p it))
      (ok (equal "scratch" (llm-protocol:llm-reasoning-item-text it))))))

(deftest mock-respond
  (let* ((b (llm-protocol:make-mock-llm-backend))
         (r (llm-protocol:respond b "hi")))
    (ok (equal "echo: hi" (llm-protocol:llm-response-text r)))
    (ok (llm-protocol:llm-message-item-p (first (llm-protocol:llm-response-items r))))
    (ok (eq :assistant (llm-protocol:llm-message-item-role
                        (first (llm-protocol:llm-response-items r)))))))
