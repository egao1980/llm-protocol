(in-package #:llm-protocol/mcp)

(defun %hint-name (prefs)
  (cond
    ((null prefs) nil)
    ((typep prefs 'mcp-protocol:mcp-model-preferences)
     (%hint-name (mcp-protocol::mcp-model-preferences-hints prefs)))
    ((hash-table-p prefs)
     (%hint-name (or (gethash "hints" prefs) (gethash "name" prefs))))
    ((or (vectorp prefs) (listp prefs))
     (let ((first (car (llm-protocol::%as-list prefs))))
       (cond
         ((null first) nil)
         ((stringp first) first)
         ((hash-table-p first) (or (gethash "name" first) (gethash "model" first)))
         ((typep first 'mcp-protocol:mcp-model-preferences)
          (%hint-name first))
         (t nil))))
    ((stringp prefs) prefs)
    (t nil)))

(defun %sampling-model (params)
  (cond
    ((typep params 'mcp-protocol:mcp-sampling-request)
     (%hint-name (mcp-protocol::mcp-sampling-request-model-preferences params)))
    (t
     (or (mcp-protocol:param params "model")
         (%hint-name (mcp-protocol:param params "modelPreferences"))))))

(defun %sampling-slot (params json-key accessor)
  (if (typep params 'mcp-protocol:mcp-sampling-request)
      (funcall accessor params)
      (mcp-protocol:param params json-key)))

(defun %mcp-message->llm (msg)
  (cond
    ((typep msg 'mcp-protocol:mcp-sampling-message)
     (make-llm-message
      :role (or (mcp-protocol::mcp-sampling-message-role msg) "user")
      :content (llm-protocol::%content-text
                (mcp-protocol::mcp-sampling-message-content msg))))
    (t (coerce-message msg))))

(defun %sampling-messages (params)
  (let* ((raw (if (typep params 'mcp-protocol:mcp-sampling-request)
                  (mcp-protocol::mcp-sampling-request-messages params)
                  (mcp-protocol:param params "messages")))
         (msgs (mapcar #'%mcp-message->llm (llm-protocol::%as-list raw)))
         (sys (if (typep params 'mcp-protocol:mcp-sampling-request)
                  (mcp-protocol::mcp-sampling-request-system-prompt params)
                  (or (mcp-protocol:param params "systemPrompt")
                      (mcp-protocol:param params "system")))))
    (if (and sys (plusp (length (string sys))))
        (cons (make-llm-message :role "system" :content sys) msgs)
        msgs)))

(defun %stop-reason (finish)
  (cond
    ((null finish) "endTurn")
    ((string-equal finish "stop") "endTurn")
    ((string-equal finish "length") "maxTokens")
    ((string-equal finish "tool_calls") "endTurn")
    (t (string finish))))

(defun llm-result->mcp-create-message (result)
  "Map GENERATE's LLM-RESULT to a sampling/createMessage result object."
  (mcp-protocol:json-object
   "role" "assistant"
   "model" (or (llm-result-model result) :omit)
   "content" (mcp-protocol:make-text-content (or (llm-result-text result) ""))
   "stopReason" (%stop-reason (llm-result-finish-reason result))))

(defun make-mcp-sampling-handler (&key (backend *llm-backend*))
  "Return a function suitable as MCP-CLIENT-SAMPLING-HANDLER.
PARAMS may be a hash-table (JSON-RPC) or MCP-SAMPLING-REQUEST. Does not invent
a second create-message — the host still uses MCP-PROTOCOL:CREATE-MESSAGE."
  (lambda (params)
    (llm-result->mcp-create-message
     (generate (or backend *llm-backend*)
               (%sampling-messages params)
               :model (%sampling-model params)
               :temperature (%sampling-slot params "temperature"
                                            #'mcp-protocol::mcp-sampling-request-temperature)
               :max-tokens (%sampling-slot params "maxTokens"
                                           #'mcp-protocol::mcp-sampling-request-max-tokens)
               :stop (%sampling-slot params "stopSequences"
                                     #'mcp-protocol::mcp-sampling-request-stop-sequences)
               :tools (%sampling-slot params "tools"
                                      #'mcp-protocol::mcp-sampling-request-tools)
               :tool-choice (%sampling-slot params "toolChoice"
                                            #'mcp-protocol::mcp-sampling-request-tool-choice)))))
