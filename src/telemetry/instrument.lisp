(in-package #:llm-protocol)

;;; Optional GenAI spans. Load llm-protocol/telemetry — core stays dep-free.
;;; Soft-bind: *TELEMETRY-BACKEND* defaults to no-op, so these :around
;;; methods are safe to leave installed.

(defparameter +gen-ai-usage-cost+ "gen_ai.usage.cost"
  "USD cost attribute (not yet a fixed OTel GenAI key). Set when prices exist.")

(defparameter +gen-ai-token-type+ "gen_ai.token.type")

(defparameter +gen-ai-client-token-usage+ "gen_ai.client.token.usage")

(defun %telemetry-request-model (backend model)
  (or (%model-id model)
      (ignore-errors (backend-model backend))))

(defun %telemetry-model-prices (backend model)
  "→ (values input-price output-price). USD per 1M tokens.
   LIST-MODELS wins when it has prices; otherwise the provider-catalog
   price table (*LLM-CATALOG* / REGISTER-PROVIDER :models)."
  (let ((info (%lookup-model-info backend model)))
    (let ((in (and (llm-model-info-p info) (llm-model-info-input-price info)))
          (out (and (llm-model-info-p info) (llm-model-info-output-price info))))
      (if (or in out)
          (values in out)
          (let* ((id (or (and (llm-model-info-p info) (llm-model-info-id info))
                         (%telemetry-request-model backend model)))
                 (prov (and *llm-catalog*
                            (%provider-for-backend *llm-catalog* backend)))
                 (cinfo (and prov id
                             (find-if (lambda (m)
                                        (and (llm-model-info-p m)
                                             (equal (llm-model-info-id m) id)))
                                      (llm-provider-models prov)))))
            (values (and (llm-model-info-p cinfo)
                         (llm-model-info-input-price cinfo))
                    (and (llm-model-info-p cinfo)
                         (llm-model-info-output-price cinfo))))))))

(defun %telemetry-usage-cost (usage backend model)
  "USD from USAGE × per-1M INPUT-PRICE / OUTPUT-PRICE. NIL when no price."
  (multiple-value-bind (in-price out-price)
      (%telemetry-model-prices backend model)
    (when (or in-price out-price)
      (let ((in (or (and usage (llm-usage-input-tokens usage)) 0))
            (out (or (and usage (llm-usage-output-tokens usage)) 0)))
        (+ (* in (/ (or in-price 0) 1000000.0d0))
           (* out (/ (or out-price 0) 1000000.0d0)))))))

(defun %telemetry-usage (result)
  (cond
    ((llm-response-p result) (llm-response-usage result))
    ((llm-embed-result-p result) (llm-embed-result-usage result))
    (t nil)))

(defun %telemetry-response-model (result)
  (cond
    ((llm-response-p result) (llm-response-model result))
    ((llm-embed-result-p result) (llm-embed-result-model result))
    (t nil)))

(defun %annotate-llm-span (span backend request-model result)
  (let* ((usage (%telemetry-usage result))
         (response-model (%telemetry-response-model result))
         (response-id (and (llm-response-p result) (llm-response-id result)))
         (in (and usage (llm-usage-input-tokens usage)))
         (out (and usage (llm-usage-output-tokens usage)))
         (model (or response-model request-model))
         (cost (%telemetry-usage-cost usage backend model)))
    (telemetry-protocol:instrument-gen-ai-span
     span
     :response-model response-model
     :response-id response-id
     :input-tokens in
     :output-tokens out)
    (when cost
      (telemetry-protocol:set-span-attribute
       telemetry-protocol:*telemetry-backend* span +gen-ai-usage-cost+ cost))
    (when in
      (telemetry-protocol:record-metric
       telemetry-protocol:*telemetry-backend* +gen-ai-client-token-usage+ in
       :unit "token"
       :attributes (list telemetry-protocol:+gen-ai-request-model+ (or model "")
                         +gen-ai-token-type+ "input")))
    (when out
      (telemetry-protocol:record-metric
       telemetry-protocol:*telemetry-backend* +gen-ai-client-token-usage+ out
       :unit "token"
       :attributes (list telemetry-protocol:+gen-ai-request-model+ (or model "")
                         +gen-ai-token-type+ "output")))
    span))

(defun %call-with-llm-span (span-name operation-name backend model thunk)
  (let ((request-model (%telemetry-request-model backend model)))
    (telemetry-protocol:with-span (span-name :kind :client)
      (telemetry-protocol:instrument-gen-ai-span
       telemetry-protocol:*current-span*
       :operation-name operation-name
       :request-model request-model)
      (let ((result (funcall thunk)))
        (%annotate-llm-span telemetry-protocol:*current-span*
                            backend request-model result)
        result))))

;;; Specializer T (not LLM-BACKEND): an :around with the same
;;; specializers would replace the core %CALL-WITH-OUTPUT / WITH-LLM-RESTARTS
;;; arounds. T is less specific, so those stay outermost.

(defun %wrap-llm-span (backend model span-name operation-name next)
  (if (llm-backend-p backend)
      (%call-with-llm-span span-name operation-name backend model next)
      (funcall next)))

(defmethod generate :around ((backend t) turns &rest args
                             &key model &allow-other-keys)
  (declare (ignore turns args))
  (%wrap-llm-span backend model "generate" "chat"
                  (lambda () (call-next-method))))

(defmethod stream-generate :around ((backend t) turns &rest args
                                    &key model &allow-other-keys)
  (declare (ignore turns args))
  (%wrap-llm-span backend model "stream-generate" "chat"
                  (lambda () (call-next-method))))

(defmethod respond :around ((backend t) turns &rest args
                            &key model &allow-other-keys)
  (declare (ignore turns args))
  (%wrap-llm-span backend model "respond" "chat"
                  (lambda () (call-next-method))))

(defmethod stream-respond :around ((backend t) turns &rest args
                                   &key model &allow-other-keys)
  (declare (ignore turns args))
  (%wrap-llm-span backend model "stream-respond" "chat"
                  (lambda () (call-next-method))))

(defmethod embed :around ((backend t) inputs &rest args
                          &key model &allow-other-keys)
  (declare (ignore inputs args))
  (%wrap-llm-span backend model "embeddings" "embeddings"
                  (lambda () (call-next-method))))
