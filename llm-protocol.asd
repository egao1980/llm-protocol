(defsystem "llm-protocol"
  :version "0.1.0"
  :description "CLOS LLM protocol (turns + typed parts) for cl-stack; not blackboard core"
  :author "egao1980"
  :license "MIT"
  :depends-on ()
  :properties (:cl-repo
               (:ci (:with ("llm-protocol/capability"
                            "llm-protocol/schema"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "types")
               (:file "protocol")
               (:file "mock"))
  :in-order-to ((test-op (test-op "llm-protocol/tests"))))

(defsystem "llm-protocol/capability"
  :version "0.1.0"
  :description "capability-protocol :llm-generation adapter over llm-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("llm-protocol" "capability-protocol")
  :serial t
  :pathname "src/capability"
  :components ((:file "adapter"))
  :in-order-to ((test-op (test-op "llm-protocol/tests"))))

(defsystem "llm-protocol/schema"
  :version "0.1.0"
  :description "schema-protocol + schema-protocol-json structured output for llm-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("llm-protocol" "schema-protocol" "schema-protocol-json"
               "json-protocol" "json-backend-jzon")
  :serial t
  :pathname "src/schema"
  :components ((:file "adapter"))
  :in-order-to ((test-op (test-op "llm-protocol/tests"))))

(defsystem "llm-protocol/tests"
  :depends-on ("llm-protocol"
               "llm-protocol/capability"
               "llm-protocol/schema"
               "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "protocol-test")
               (:file "capability-test")
               (:file "schema-test")
               (:file "restarts-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
