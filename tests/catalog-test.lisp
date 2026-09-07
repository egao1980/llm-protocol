(in-package #:llm-protocol/tests)

(deftest parse-provider-ref-shapes
  (multiple-value-bind (n m) (llm-protocol:parse-provider-ref "anthropic:claude")
    (ok (equal "anthropic" n))
    (ok (equal "claude" m)))
  (multiple-value-bind (n m) (llm-protocol:parse-provider-ref "anthropic")
    (ok (equal "anthropic" n))
    (ok (null m)))
  (multiple-value-bind (n m) (llm-protocol:parse-provider-ref :vllm)
    (ok (equal "vllm" n))
    (ok (null m)))
  (multiple-value-bind (n m) (llm-protocol:parse-provider-ref '("openai" "gpt-4o"))
    (ok (equal "openai" n))
    (ok (equal "gpt-4o" m))))

(deftest catalog-register-resolve
  (let* ((cat (llm-protocol:make-in-memory-provider-catalog))
         (a (llm-protocol:make-mock-llm-backend :prefix "a: "))
         (b (llm-protocol:make-mock-llm-backend :prefix "b: ")))
    (llm-protocol:register-provider cat "anthropic" a :models '("claude"))
    (llm-protocol:register-provider cat :vllm b)
    (ok (= 2 (length (llm-protocol:list-providers cat))))
    (multiple-value-bind (backend model)
        (llm-protocol:resolve-backend cat "anthropic:claude-sonnet")
      (ok (eq a backend))
      (ok (equal "claude-sonnet" model)))
    (multiple-value-bind (backend model)
        (llm-protocol:resolve-backend cat "vllm")
      (ok (eq b backend))
      (ok (null model)))
    (multiple-value-bind (backend model)
        (llm-protocol:resolve-backend cat a)
      (ok (eq a backend))
      (ok (null model)))))

(deftest catalog-list-models-registered
  (let ((cat (llm-protocol:make-in-memory-provider-catalog)))
    (llm-protocol:register-provider cat "anthropic"
                                    (llm-protocol:make-mock-llm-backend)
                                    :models '("claude" "haiku"))
    (let ((ids (mapcar #'llm-protocol:llm-model-info-id
                       (remove-if-not #'llm-protocol:llm-model-info-p
                                      (llm-protocol:catalog-list-models cat)))))
      (declare (ignore ids))
      (ok (equal '("claude" "haiku")
                 (llm-protocol:catalog-list-models cat :provider "anthropic"))))))

(deftest catalog-unknown-signals
  (let ((cat (llm-protocol:make-in-memory-provider-catalog)))
    (ok (signals (llm-protocol:resolve-backend cat "nope")
                 'llm-protocol:llm-unknown-provider))))

(deftest catalog-unknown-use-value
  (let* ((cat (llm-protocol:make-in-memory-provider-catalog))
         (mock (llm-protocol:make-mock-llm-backend)))
    (handler-bind ((llm-protocol:llm-unknown-provider
                    (lambda (c)
                      (use-value mock c))))
      (multiple-value-bind (backend model)
          (llm-protocol:resolve-backend cat "missing:m")
        (ok (eq mock backend))
        (ok (equal "m" model))))))

(deftest catalog-unregister-continue
  (let ((cat (llm-protocol:make-in-memory-provider-catalog)))
    (handler-bind ((llm-protocol:llm-unknown-provider
                    (lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'continue))))
      (llm-protocol:unregister-provider cat "ghost"))
    (ok (null (llm-protocol:list-providers cat)))))

(deftest catalog-nil-use-value
  (let* ((cat (llm-protocol:make-in-memory-provider-catalog))
         (mock (llm-protocol:make-mock-llm-backend))
         (llm-protocol:*llm-catalog* nil))
    (llm-protocol:register-provider cat "x" mock)
    (handler-bind ((llm-protocol:llm-missing-backend
                    (lambda (c)
                      (use-value cat c))))
      (multiple-value-bind (backend model)
          (llm-protocol:resolve-backend nil "x")
        (ok (eq mock backend))
        (ok (null model))))))
