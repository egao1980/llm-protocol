(defsystem "llm-protocol"
  :version "0.3.0"
  :description "CLOS LLM protocol (turns + typed parts) for cl-stack; not blackboard core"
  :author "egao1980"
  :license "MIT"
  :depends-on ()
  :properties (:cl-repo
               (:ci (:with ("llm-protocol/capability"
                            "llm-protocol/schema"
                            "llm-protocol/router"
                            "llm-protocol/telemetry"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "types")
               (:file "protocol")
               (:file "catalog")
               (:file "mock"))
  :in-order-to ((test-op (test-op "llm-protocol/tests"))))

(defsystem "llm-protocol/capability"
  :version "0.2.0"
  :description "capability-protocol :llm-generation / :llm-embeddings adapters over llm-protocol"
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

(defsystem "llm-protocol/router"
  :version "0.3.0"
  :description "CLOS routing policies (fallback / budget / latency) over llm-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("llm-protocol")
  :pathname "src/router"
  :serial t
  :components ((:file "router"))
  :in-order-to ((test-op (test-op "llm-protocol/tests"))))

(defsystem "llm-protocol/telemetry"
  :version "0.3.0"
  :description "GenAI semconv spans for llm-protocol generate/respond/embed"
  :author "egao1980"
  :license "MIT"
  :depends-on ("llm-protocol" "telemetry-protocol")
  :pathname "src/telemetry"
  :serial t
  :components ((:file "instrument"))
  :in-order-to ((test-op (test-op "llm-protocol/tests"))))

(defsystem "llm-protocol/tests"
  :depends-on ("llm-protocol"
               "llm-protocol/capability"
               "llm-protocol/schema"
               "llm-protocol/router"
               "llm-protocol/telemetry"
               "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "protocol-test")
               (:file "catalog-test")
               (:file "capability-test")
               (:file "schema-test")
               (:file "restarts-test")
               (:file "tokens-test")
               (:file "router-test")
               (:file "telemetry-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
