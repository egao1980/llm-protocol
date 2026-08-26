(defpackage #:llm-protocol/mcp
  (:use #:cl #:llm-protocol)
  (:export #:make-mcp-sampling-handler
           #:llm-response->mcp-create-message))

(in-package #:llm-protocol/mcp)
