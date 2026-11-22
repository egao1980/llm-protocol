(in-package #:llm-protocol)

;;; 0.7.0 — one error taxonomy for every backend (egao1980/cl-stack-code H3f).
;;;
;;; Backends used to signal a bare LLM-HTTP-ERROR with a status, so a caller
;;; that wanted "rate limited" or "bad key" had to bind on the type and then
;;; read an integer. CLHS 9 and the stack rule: handlers bind on TYPE. The
;;; classes below are the types; MAKE-LLM-HTTP-ERROR is the one place that
;;; maps a provider's answer onto them, so backends only add what is really
;;; provider-specific (their overflow wording, their error envelope).
;;;
;;;   llm-error
;;;   ├── llm-http-error (status body retry-after retryable-p provider url model)
;;;   │   ├── llm-rate-limited          429                     retryable, :rate-limit
;;;   │   ├── llm-overloaded            503, 529                retryable, :overloaded
;;;   │   ├── llm-auth-error            401, 403                not retryable, NIL class
;;;   │   ├── llm-not-found             404 (model or endpoint) not retryable, NIL class
;;;   │   ├── llm-context-overflow      (0.5) 400 / 413 + overflow text
;;;   │   └── llm-incomplete-response   (0.6) stream ended early
;;;   └── llm-transport-error (cause kind)                      retryable, :transient
;;;
;;; Rules:
;;;
;;;   LLM-HTTP-ERROR-CLASS status &key body message → class name.
;;;     400 or 413 whose MESSAGE or BODY satisfies CONTEXT-OVERFLOW-TEXT-P →
;;;     LLM-CONTEXT-OVERFLOW (whatever else the status says). 429 →
;;;     LLM-RATE-LIMITED. 503 and 529 → LLM-OVERLOADED; so is any 5xx whose
;;;     body or message contains "overloaded" (case-insensitive; Anthropic's
;;;     overloaded_error, vLLM / LM Studio "server is overloaded"). 401, 403 →
;;;     LLM-AUTH-ERROR. 404 → LLM-NOT-FOUND. Anything else → LLM-HTTP-ERROR.
;;;
;;;   MAKE-LLM-HTTP-ERROR status &key body message retry-after provider url
;;;   model → the condition object (MAKE-CONDITION; not signalled). Its class
;;;   is LLM-HTTP-ERROR-CLASS. RETRY-AFTER is seconds, or a header string
;;;   read with PARSE-RETRY-AFTER. RETRYABLE-P is HTTP-STATUS-RETRYABLE-P of
;;;   STATUS, except: overflow, auth and not-found are never retryable, and
;;;   rate-limited / overloaded always are. MESSAGE defaults to "HTTP <status>".
;;;
;;;   PARSE-RETRY-AFTER value &key (now (get-universal-time)) → seconds (a
;;;   non-negative real) or NIL. A real → itself (negative → 0). A string:
;;;   trimmed; delta-seconds ("120", also "1.5", which OpenAI-compatible
;;;   servers send); or an IMF-fixdate HTTP date ("Sun, 06 Nov 1994 08:49:37
;;;   GMT") → seconds from NOW, 0 when it is in the past. Anything else
;;;   (NIL, "", junk, RFC 850 dates) → NIL. Never signals.
;;;
;;;   LLM-RETRY-CLASS (extends 0.5's GF): LLM-RATE-LIMITED → :RATE-LIMIT,
;;;   LLM-OVERLOADED → :OVERLOADED, LLM-AUTH-ERROR and LLM-NOT-FOUND → NIL,
;;;   LLM-TRANSPORT-ERROR → :TRANSIENT (NIL for kind :CANCELED). The 0.5
;;;   status-based method stays for plain LLM-HTTP-ERRORs.
;;;
;;;   LLM-TRANSPORT-ERROR: the request never produced an HTTP answer, or the
;;;   connection died during one. KIND is one of +TRANSPORT-ERROR-KINDS+:
;;;   :CONNECT (refused, DNS, unreachable), :RESET (reset or closed mid-body,
;;;   incomplete response), :TLS, :TIMEOUT, :CANCELED. CAUSE is the original
;;;   condition (an http-protocol one, usually). Backends wrap their transport
;;;   conditions in it (llm-protocol has no http-protocol dependency, so the
;;;   mapping from http-protocol types lives in each backend; see
;;;   TRANSPORT-ERROR-KIND-FOR, which maps by class NAME so it needs none).
;;;   The report is "llm transport error (KIND): <cause>".
;;;
;;;   TRANSPORT-ERROR-KIND-FOR condition &key (TYPES
;;;   +HTTP-PROTOCOL-TRANSPORT-TYPES+) → a kind or NIL. TYPES is an alist
;;;   (type-name . kind) tried in order: each name is FIND-SYMBOLed in the
;;;   "HTTP-PROTOCOL" package (skipped when the package or the symbol is
;;;   missing) and tested with TYPEP. HTTP-CANCELED → :CANCELED,
;;;   HTTP-TIMEOUT-ERROR → :TIMEOUT, HTTP-TLS-ERROR → :TLS,
;;;   HTTP-CONNECTION-ERROR → :CONNECT, or :RESET when its text mentions
;;;   "reset", "closed", "incomplete" or "EOF" (case-insensitive). Any
;;;   other condition whose text contains "incomplete HTTP response"
;;;   (http-backend-async#64, a plain http-protocol-error today) → :RESET.
;;;   Anything else → NIL. llm-protocol keeps no http-protocol dependency.
;;;
;;;   JITTERED-DELAY delay jitter → DELAY x (1 - JITTER x R), R from
;;;   (RANDOM 1.0d0 *RETRY-RANDOM-STATE*). JITTER 0 → DELAY unchanged.
;;;   RETRY-DELAY calls it for backoff delays only (never for Retry-After).

(defun %h3f-pending (name)
  (error "llm-protocol 0.7.0: ~a is not implemented yet (H3f contract)" name))

(define-condition llm-rate-limited (llm-http-error) ()
  (:default-initargs :status 429 :retryable-p t)
  (:report (lambda (c s)
             (format s "llm rate limited~@[ (HTTP ~a)~]~@[, retry after ~as~]~@[: ~a~]"
                     (llm-http-error-status c) (llm-http-error-retry-after c)
                     (llm-error-message c)))))

(define-condition llm-overloaded (llm-http-error) ()
  (:default-initargs :status 529 :retryable-p t)
  (:report (lambda (c s)
             (format s "llm provider overloaded~@[ (HTTP ~a)~]~@[: ~a~]"
                     (llm-http-error-status c) (llm-error-message c)))))

(define-condition llm-auth-error (llm-http-error) ()
  (:default-initargs :status 401 :retryable-p nil)
  (:report (lambda (c s)
             (format s "llm authentication failed~@[ (HTTP ~a)~]~@[: ~a~]"
                     (llm-http-error-status c) (llm-error-message c)))))

(define-condition llm-not-found (llm-http-error) ()
  (:default-initargs :status 404 :retryable-p nil)
  (:report (lambda (c s)
             (format s "llm model or endpoint not found~@[ (HTTP ~a)~]~@[: ~a~]"
                     (llm-http-error-status c) (llm-error-message c)))))

(defparameter +transport-error-kinds+ '(:connect :reset :tls :timeout :canceled))

(define-condition llm-transport-error (llm-error)
  ((cause :initarg :cause :reader llm-transport-error-cause :initform nil)
   (kind :initarg :kind :reader llm-transport-error-kind :initform :connect)
   (provider :initarg :provider :reader llm-transport-error-provider :initform nil)
   (url :initarg :url :reader llm-transport-error-url :initform nil))
  (:report (lambda (c s)
             (format s "llm transport error (~(~a~))~@[: ~a~]"
                     (llm-transport-error-kind c)
                     (or (llm-error-message c)
                         (let ((cause (llm-transport-error-cause c)))
                           (and cause (princ-to-string cause))))))))

(defparameter +http-protocol-transport-types+
  '(("HTTP-CANCELED" . :canceled)
    ("HTTP-TIMEOUT-ERROR" . :timeout)
    ("HTTP-TLS-ERROR" . :tls)
    ("HTTP-CONNECTION-ERROR" . :connect))
  "http-protocol condition type names → transport kinds, tried in order.")

(defun llm-http-error-class (status &key body message)
  (declare (ignore status body message))
  (%h3f-pending 'llm-http-error-class))

(defun make-llm-http-error (status &key body message retry-after provider url model)
  (declare (ignore status body message retry-after provider url model))
  (%h3f-pending 'make-llm-http-error))

(defun parse-retry-after (value &key (now (get-universal-time)))
  (declare (ignore value now))
  (%h3f-pending 'parse-retry-after))

(defun transport-error-kind-for (condition &key (types +http-protocol-transport-types+))
  (declare (ignore condition types))
  (%h3f-pending 'transport-error-kind-for))

(defun jittered-delay (delay jitter)
  (declare (ignore delay jitter))
  (%h3f-pending 'jittered-delay))

(defmethod llm-retry-class ((condition llm-rate-limited))
  (%h3f-pending 'llm-retry-class))

(defmethod llm-retry-class ((condition llm-overloaded))
  (%h3f-pending 'llm-retry-class))

(defmethod llm-retry-class ((condition llm-auth-error))
  (%h3f-pending 'llm-retry-class))

(defmethod llm-retry-class ((condition llm-not-found))
  (%h3f-pending 'llm-retry-class))

(defmethod llm-retry-class ((condition llm-transport-error))
  (%h3f-pending 'llm-retry-class))
