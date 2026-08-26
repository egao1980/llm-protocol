(defsystem "llm-backend-openai"
  :version "0.1.0"
  :description "OpenAI-compatible chat/completions backend for llm-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("llm-protocol" "http-protocol" "json-protocol" "json-backend-jzon" "babel")
  :serial t
  :pathname "src/openai"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "llm-protocol/tests"))))
