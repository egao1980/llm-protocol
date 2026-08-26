(defpackage #:llm-protocol
  (:use #:cl)
  (:nicknames #:stack-llm)
  (:export #:llm-error
           #:llm-error-message
           #:llm-missing-backend
           #:llm-unsupported
           #:llm-http-error
           #:llm-http-error-status
           #:llm-http-error-body

           #:llm-backend
           #:*llm-backend*

           #:llm-message
           #:make-llm-message
           #:llm-message-p
           #:llm-message-role
           #:llm-message-content
           #:llm-message-name
           #:llm-message-tool-call-id
           #:llm-message-tool-calls

           #:llm-tool-call
           #:make-llm-tool-call
           #:llm-tool-call-p
           #:llm-tool-call-id
           #:llm-tool-call-name
           #:llm-tool-call-arguments

           #:llm-tool
           #:make-llm-tool
           #:llm-tool-p
           #:llm-tool-name
           #:llm-tool-description
           #:llm-tool-parameters

           #:llm-model
           #:make-llm-model
           #:llm-model-p
           #:llm-model-id
           #:llm-model-owned-by

           #:llm-result
           #:make-llm-result
           #:llm-result-p
           #:llm-result-message
           #:llm-result-model
           #:llm-result-finish-reason
           #:llm-result-usage
           #:llm-result-text

           #:coerce-messages
           #:generate
           #:list-models

           #:mock-llm-backend
           #:make-mock-llm-backend
           #:use-mock-llm-backend
           #:mock-llm-prefix
           #:mock-llm-handler
           #:mock-llm-models
           #:mock-llm-tool-calls

           #:llm-generation-adapter
           #:llm-generation-backend
           #:make-llm-generation-adapter))

(in-package #:llm-protocol)
