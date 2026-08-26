(defsystem "llm-protocol"
  :version "0.1.0"
  :description "CLOS LLM generate protocol for cl-stack (adapter; not blackboard core)"
  :author "egao1980"
  :license "MIT"
  :depends-on ()
  :properties (:cl-repo
               (:ci (:with ("llm-backend-openai"
                            "llm-protocol/capability"
                            "llm-protocol/mcp")
                     :sources (("rove" :ql)))))
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

(defsystem "llm-protocol/mcp"
  :version "0.1.0"
  :description "Optional MCP sampling-handler helper over llm-protocol:generate"
  :author "egao1980"
  :license "MIT"
  :depends-on ("llm-protocol" "mcp-protocol")
  :serial t
  :pathname "src/mcp"
  :components ((:file "package")
               (:file "sampling"))
  :in-order-to ((test-op (test-op "llm-protocol/tests"))))

(defsystem "llm-protocol/tests"
  :depends-on ("llm-protocol"
               "llm-protocol/capability"
               "llm-protocol/mcp"
               "llm-backend-openai"
               "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "protocol-test")
               (:file "openai-test")
               (:file "capability-test")
               (:file "mcp-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
