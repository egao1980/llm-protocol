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
  (let ((b (llm-protocol:make-mock-llm-backend
            :handler (lambda (backend turns &key &allow-other-keys)
                       (declare (ignore backend turns))
                       (llm-protocol:make-llm-response
                        :parts (list (llm-protocol:make-llm-text-part :text "{}"))
                        :model "mock")))))
    (ok (signals (llm-protocol:generate b "city" :output '%llm-city)
                 'llm-protocol:llm-output-error))))
