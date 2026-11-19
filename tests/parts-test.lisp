(in-package #:llm-protocol/tests)

;;; Part state round trip, provider compaction, portable turns.

(defun %roundtrip (part)
  (llm-protocol:part-from-state (llm-protocol:part-state part)))

(deftest part-state-round-trips-the-base-parts
  (let ((thinking (%roundtrip (llm-protocol:make-llm-thinking-part
                               :text "hmm" :signature "sig-1")))
        (call (%roundtrip (llm-protocol:make-llm-tool-call-part
                           :id "c1" :name "f" :arguments "{\"a\":1}")))
        (result (%roundtrip (llm-protocol:make-llm-tool-result-part
                             :id "c1" :name "f" :content "ok" :error-p t))))
    (ok (equal "sig-1" (llm-protocol:llm-thinking-part-signature thinking))
        "the thinking signature survives")
    (ok (equal "hmm" (llm-protocol:llm-thinking-part-text thinking)))
    (ok (equal "{\"a\":1}" (llm-protocol:llm-tool-call-part-arguments call)))
    (ok (llm-protocol:llm-tool-result-part-error-p result))
    (ok (equal "hi" (llm-protocol:llm-text-part-text
                     (%roundtrip (llm-protocol:make-llm-text-part :text "hi")))))
    (testing "an unknown :type reads back as NIL"
      (ng (llm-protocol:part-from-state '(:type :no-such-part))))
    (testing "turn state"
      (let ((turn (llm-protocol:state-turn
                   (llm-protocol:turn-state (llm-protocol:assistant-turn "yo")))))
        (ok (eq :assistant (llm-protocol:llm-turn-role turn)))
        (ok (equal "yo" (llm-protocol:turn-text turn)))))))

(defun %summary-backend (&optional (summary "SUM"))
  (llm-protocol:make-mock-llm-backend
   :compactor (lambda (turns &key instructions)
                (declare (ignore turns))
                (format nil "~a~@[ (~a)~]" summary instructions))))

(defun %chat ()
  (list (llm-protocol:system-turn "be brief")
        (llm-protocol:user-turn "q1")
        (llm-protocol:assistant-turn "a1")
        (llm-protocol:user-turn "q2")))

(deftest server-compact-on-a-native-backend
  (let ((b (%summary-backend)))
    (ok (llm-protocol:backend-supports-p b :compaction))
    (multiple-value-bind (turns did)
        (llm-protocol:server-compact b (%chat) :instructions "keep names")
      (ok did)
      (ok (= 2 (length turns)) "system turn, then the summary")
      (ok (eq :system (llm-protocol:llm-turn-role (first turns))))
      (let ((part (first (llm-protocol:llm-turn-parts (second turns)))))
        (ok (llm-protocol:compaction-part-p part))
        (ok (equal "SUM (keep names)" (llm-protocol:compaction-part-text part)))
        (ok (llm-protocol:compaction-part-p (%roundtrip part))
            "a provider part round-trips through its own state")))
    (testing "no summary: the turns come back, not compacted"
      (let ((none (llm-protocol:make-mock-llm-backend
                   :compactor (lambda (turns &key instructions)
                                (declare (ignore turns instructions))
                                nil))))
        (multiple-value-bind (turns did) (llm-protocol:server-compact none (%chat))
          (ng did)
          (ok (= 4 (length turns))))))
    (testing "a backend without compaction signals llm-unsupported"
      (ng (llm-protocol:backend-supports-p (llm-protocol:make-mock-llm-backend)
                                           :compaction))
      (ok (signals (llm-protocol:server-compact (llm-protocol:make-mock-llm-backend)
                                                (%chat))
                   'llm-protocol:llm-unsupported)))))

(defun %compaction-turn (text)
  (llm-protocol:make-llm-turn
   :role :assistant
   :parts (list (make-instance 'llm-protocol:mock-compaction-part :text text))))

(deftest portable-turns-for-other-providers
  (testing "an opening summary becomes a system turn"
    (let ((turns (llm-protocol:portable-turns
                  (list (llm-protocol:system-turn "be brief")
                        (%compaction-turn "SUM")
                        (llm-protocol:user-turn "q3")))))
      (ok (= 3 (length turns)))
      (ok (eq :system (llm-protocol:llm-turn-role (second turns))))
      (ok (search "SUM" (llm-protocol:turn-text (second turns))))
      (ok (notany (lambda (turn)
                    (some #'llm-protocol:compaction-part-p
                          (llm-protocol:llm-turn-parts turn)))
                  turns))))
  (testing "a threshold summary after the turns it covers is dropped"
    (let ((turns (llm-protocol:portable-turns
                  (list (llm-protocol:user-turn "q1")
                        (llm-protocol:make-llm-turn
                         :role :assistant
                         :parts (list (make-instance 'llm-protocol:mock-compaction-part
                                                     :text "SUM")
                                      (llm-protocol:make-llm-text-part :text "a1")))))))
      (ok (= 2 (length turns)))
      (ok (equal "a1" (llm-protocol:turn-text (second turns))))
      (ng (search "SUM" (format nil "~{~a~}" (mapcar #'llm-protocol:turn-text turns))))))
  (testing "turns without compaction parts are unchanged"
    (let ((chat (%chat)))
      (ok (equal chat (llm-protocol:portable-turns chat)))))
  (testing "generate on a non-native backend sends portable turns"
    (let* ((seen nil)
           (b (llm-protocol:make-mock-llm-backend
               :handler (lambda (b turns &key &allow-other-keys)
                          (declare (ignore b))
                          (setf seen turns)
                          (llm-protocol:make-llm-response
                           :parts (list (llm-protocol:make-llm-text-part :text "ok")))))))
      (llm-protocol:generate b (list (%compaction-turn "SUM") (llm-protocol:user-turn "q")))
      (ok (= 2 (length seen)))
      (ok (eq :system (llm-protocol:llm-turn-role (first seen))))))
  (testing "a native backend gets the compaction part as it is"
    (let* ((seen nil)
           (b (llm-protocol:make-mock-llm-backend
               :compactor (lambda (turns &key instructions)
                            (declare (ignore turns instructions))
                            "x")
               :handler (lambda (b turns &key &allow-other-keys)
                          (declare (ignore b))
                          (setf seen turns)
                          (llm-protocol:make-llm-response
                           :parts (list (llm-protocol:make-llm-text-part :text "ok")))))))
      (llm-protocol:generate b (list (%compaction-turn "SUM") (llm-protocol:user-turn "q")))
      (ok (llm-protocol:compaction-part-p
           (first (llm-protocol:llm-turn-parts (first seen))))))))

(deftest compaction-capability-in-the-catalogue
  (let ((cat (llm-protocol:make-llm-catalogue (%summary-backend))))
    (ok (capability-protocol:capability-supported-p cat :llm-compaction))
    (multiple-value-bind (turns did)
        (capability-protocol:compact (capability-protocol:get-capability cat :llm-compaction)
                                     (%chat))
      (ok did)
      (ok (= 2 (length turns)))))
  (ng (capability-protocol:capability-supported-p
       (llm-protocol:make-llm-catalogue (llm-protocol:make-mock-llm-backend))
       :llm-compaction)))

(deftest router-compacts-on-a-native-candidate
  (let* ((plain (llm-protocol:make-mock-llm-backend))
         (native (%summary-backend "ROUTED"))
         (router (llm-protocol:make-llm-router-backend
                  :policy (llm-protocol:make-fallback-chain-policy)
                  :candidates (list plain native))))
    (ok (llm-protocol:backend-supports-p router :compaction))
    (multiple-value-bind (turns did) (llm-protocol:server-compact router (%chat))
      (ok did)
      (ok (equal "ROUTED" (llm-protocol:compaction-part-text
                           (first (llm-protocol:llm-turn-parts (second turns)))))))
    (testing "a router with no native candidate is unsupported"
      (ok (signals (llm-protocol:server-compact
                    (llm-protocol:make-llm-router-backend
                     :policy (llm-protocol:make-fallback-chain-policy)
                     :candidates (list plain))
                    (%chat))
                   'llm-protocol:llm-unsupported)))))
