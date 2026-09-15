(in-package #:llm-protocol/tests)

(schema-protocol:defschema %llm-city ()
  (name string)
  (country string))

(deftest structured-output-emit-json-schema
  (let ((js (llm-protocol:structured-output-json-schema '%llm-city)))
    (ok (hash-table-p js))
    (ok (equal "object" (gethash "type" js)))
    (ng (gethash "$schema" js))
    (ok (gethash "properties" js))))

(deftest structured-output-parse-generate
  (let* ((b (llm-protocol:make-mock-llm-backend
             :handler (lambda (backend turns &key &allow-other-keys)
                        (declare (ignore backend turns))
                        (llm-protocol:make-llm-response
                         :parts (list (llm-protocol:make-llm-text-part
                                       :text "{\"name\":\"Oslo\",\"country\":\"NO\"}"))
                         :model "mock"))))
         (r (llm-protocol:generate b "city" :output '%llm-city))
         (out (llm-protocol:llm-response-output r)))
    (ok (equal "Oslo" (slot-value out 'name)))
    (ok (equal "NO" (slot-value out 'country)))
    (ok (equal "{\"name\":\"Oslo\",\"country\":\"NO\"}"
               (llm-protocol:llm-response-text r)))
    (ok (llm-protocol:llm-text-part-p
         (first (llm-protocol:llm-response-content r))))))

(deftest structured-output-json-without-schema
  (let* ((b (llm-protocol:make-mock-llm-backend
             :handler (lambda (backend turns &key &allow-other-keys)
                        (declare (ignore backend turns))
                        (llm-protocol:make-llm-response
                         :parts (list (llm-protocol:make-llm-text-part
                                       :text "{\"name\":\"Oslo\",\"country\":\"NO\"}"))
                         :model "mock"))))
         (r (llm-protocol:generate b "city"))
         (out (llm-protocol:llm-response-output r)))
    (ok (hash-table-p out))
    (ok (equal "Oslo" (gethash "name" out)))
    (ok (null (llm-protocol:llm-response-output
               (llm-protocol:generate
                (llm-protocol:make-mock-llm-backend) "hi"))))))

(deftest structured-output-invalid
  (let ((llm-protocol:*structured-output-repair* :signal)
        (b (llm-protocol:make-mock-llm-backend
            :handler (lambda (backend turns &key &allow-other-keys)
                       (declare (ignore backend turns))
                       (llm-protocol:make-llm-response
                        :parts (list (llm-protocol:make-llm-text-part :text "{}"))
                        :model "mock")))))
    (ok (signals (llm-protocol:generate b "city" :output '%llm-city)
                 'llm-protocol:llm-output-error))))

(deftest structured-output-repair-then-valid
  (let* ((n 0)
         (seen nil)
         (b (llm-protocol:make-mock-llm-backend
             :handler (lambda (backend turns &key &allow-other-keys)
                        (declare (ignore backend))
                        (incf n)
                        (when (= n 2)
                          (setf seen turns))
                        (llm-protocol:make-llm-response
                         :parts (list (llm-protocol:make-llm-text-part
                                       :text (if (= n 1)
                                                 "not-json"
                                                 "{\"name\":\"Oslo\",\"country\":\"NO\"}")))
                         :model "mock"))))
         (r (llm-protocol:generate b "city" :output '%llm-city))
         (repair-text (llm-protocol:turn-text (car (last seen)))))
    (ok (= 2 n))
    (ok (search "not valid structured output" repair-text))
    (ok (search "not-json" repair-text))
    (ok (equal "Oslo" (slot-value (llm-protocol:llm-response-output r) 'name)))
    (ok (equal "NO" (slot-value (llm-protocol:llm-response-output r) 'country)))))

(deftest structured-output-fenced-json
  (let* ((n 0)
         (fenced (format nil "Sure:~%```json~%{\"name\":\"Oslo\",\"country\":\"NO\"}~%```"))
         (b (llm-protocol:make-mock-llm-backend
             :handler (lambda (backend turns &key &allow-other-keys)
                        (declare (ignore backend turns))
                        (incf n)
                        (llm-protocol:make-llm-response
                         :parts (list (llm-protocol:make-llm-text-part :text fenced))
                         :model "mock"))))
         (r (llm-protocol:generate b "city" :output '%llm-city
                                   :output-repair :relaxed)))
    (ok (= 1 n))
    (ok (equal "Oslo" (slot-value (llm-protocol:llm-response-output r) 'name)))
    (ok (hash-table-p (llm-protocol:try-parse-json-output fenced :relaxed t)))))

(deftest structured-output-mixed-text-relaxed
  (let* ((b (llm-protocol:make-mock-llm-backend
             :handler (lambda (backend turns &key &allow-other-keys)
                        (declare (ignore backend turns))
                        (llm-protocol:make-llm-response
                         :parts (list (llm-protocol:make-llm-text-part
                                       :text "Here {\"name\":\"Bergen\",\"country\":\"NO\"} done"))
                         :model "mock"))))
         (r (llm-protocol:generate b "city" :output '%llm-city
                                   :output-repair :relaxed)))
    (ok (equal "Bergen" (slot-value (llm-protocol:llm-response-output r) 'name)))))

(deftest structured-output-fallback-does-not-signal
  (let* ((n 0)
         (b (llm-protocol:make-mock-llm-backend
             :handler (lambda (backend turns &key &allow-other-keys)
                        (declare (ignore backend turns))
                        (incf n)
                        (llm-protocol:make-llm-response
                         :parts (list (llm-protocol:make-llm-text-part :text "nope"))
                         :model "mock"))))
         (r (llm-protocol:generate b "city" :output '%llm-city)))
    (ok (llm-protocol:llm-response-p r))
    (ok (null (llm-protocol:llm-response-output r)))
    (ok (equal "nope" (llm-protocol:llm-response-text r)))
    (ok (= 2 n))))
