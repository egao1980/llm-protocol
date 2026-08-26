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
           #:llm-backend-p
           #:*llm-backend*
           #:backend-model
           #:backend-supports-p

           #:llm-part
           #:llm-part-p
           #:llm-text-part
           #:make-llm-text-part
           #:llm-text-part-p
           #:llm-text-part-text
           #:llm-image-part
           #:make-llm-image-part
           #:llm-image-part-p
           #:llm-image-part-url
           #:llm-image-part-media-type
           #:llm-image-part-data
           #:llm-tool-call-part
           #:make-llm-tool-call-part
           #:llm-tool-call-part-p
           #:llm-tool-call-part-id
           #:llm-tool-call-part-name
           #:llm-tool-call-part-arguments
           #:llm-tool-result-part
           #:make-llm-tool-result-part
           #:llm-tool-result-part-p
           #:llm-tool-result-part-id
           #:llm-tool-result-part-name
           #:llm-tool-result-part-content
           #:llm-tool-result-part-error-p
           #:llm-thinking-part
           #:make-llm-thinking-part
           #:llm-thinking-part-p
           #:llm-thinking-part-text
           #:llm-thinking-part-signature

           #:llm-turn
           #:make-llm-turn
           #:llm-turn-p
           #:llm-turn-role
           #:llm-turn-parts
           #:user-turn
           #:system-turn
           #:assistant-turn
           #:tool-turn
           #:coerce-turn
           #:coerce-turns
           #:turn-text

           #:llm-settings
           #:make-llm-settings
           #:llm-settings-p
           #:llm-settings-temperature
           #:llm-settings-max-tokens
           #:llm-settings-stop
           #:llm-settings-top-p
           #:llm-settings-response-format
           #:llm-settings-extra
           #:coerce-settings

           #:llm-tool
           #:make-llm-tool
           #:llm-tool-p
           #:llm-tool-name
           #:llm-tool-description
           #:llm-tool-parameters

           #:llm-usage
           #:make-llm-usage
           #:llm-usage-p
           #:llm-usage-input-tokens
           #:llm-usage-output-tokens
           #:llm-usage-total-tokens

           #:llm-response
           #:make-llm-response
           #:llm-response-p
           #:llm-response-parts
           #:llm-response-model
           #:llm-response-finish-reason
           #:llm-response-usage
           #:llm-response-text
           #:llm-response-thinking
           #:llm-response-tool-calls

           #:llm-model-info
           #:make-llm-model-info
           #:llm-model-info-p
           #:llm-model-info-id
           #:llm-model-info-owned-by

           #:generate
           #:stream-generate
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
