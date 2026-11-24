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

(defun %text-mentions-p (text &rest needles)
  "T when TEXT is a string that contains one of NEEDLES, ignoring case."
  (and (stringp text)
       (some (lambda (needle) (search needle text :test #'char-equal)) needles)
       t))

(defun llm-http-error-class (status &key body message)
  "The LLM-HTTP-ERROR class name for an HTTP error answer (rules: this file's
header). STATUS is split into disjoint types, so no clause depends on the
order of the others, and a cell refines its class with one text test at most.
Any other object (NIL, \"429\", 429.0) falls into the last cell. Never signals."
  (typecase status
    ((member 400 413)
     (if (or (context-overflow-text-p message) (context-overflow-text-p body))
         'llm-context-overflow
         'llm-http-error))
    ((eql 429) 'llm-rate-limited)
    ((member 401 403) 'llm-auth-error)
    ((eql 404) 'llm-not-found)
    ((integer 500 599)
     (if (or (member status '(503 529))
             (%text-mentions-p message "overloaded")
             (%text-mentions-p body "overloaded"))
         'llm-overloaded
         'llm-http-error))
    (t 'llm-http-error)))

(defun make-llm-http-error (status &key body message retry-after provider url model)
  "The condition object for an HTTP error answer, made with MAKE-CONDITION and
not signalled. Its class is LLM-HTTP-ERROR-CLASS. RETRYABLE-P is not passed:
each subclass's :DEFAULT-INITARGS carry it (rate-limited and overloaded T;
auth, not-found and context overflow NIL), and a plain LLM-HTTP-ERROR leaves
the slot unbound, so LLM-HTTP-ERROR-RETRYABLE-P derives it from STATUS.
MESSAGE defaults to \"HTTP <status>\" for an integer STATUS. RETRY-AFTER is
read with PARSE-RETRY-AFTER. Never signals."
  (make-condition (llm-http-error-class status :body body :message message)
                  :status status
                  :body body
                  :message (or message
                               (and (integerp status) (format nil "HTTP ~d" status)))
                  :retry-after (parse-retry-after retry-after)
                  :provider provider
                  :url url
                  :model model))

(defun %ascii-integer (string start end)
  "The integer that STRING spells from START to END in ASCII digits (one or
more), else NIL. DIGIT-CHAR-P and PARSE-INTEGER also accept other scripts'
digits, so every character is checked against #\\0..#\\9 first."
  (and (< start end)
       (loop for i from start below end
             always (char<= #\0 (char string i) #\9))
       (parse-integer string :start start :end end)))

(defun %parse-delta-seconds (string)
  "Delta-seconds, 1*DIGIT [\".\" 1*DIGIT], as seconds, else NIL (a sign, an
exponent, \".5\" and \"5.\" are not). An integer stays an integer (\"7\" is 7).
A fraction is a single-float, which the rate-limited report prints as \"1.5\";
from 10^6 s up it is the ceiling integer, so FLOAT cannot overflow on a long
digit string."
  (let ((dot (position #\. string))
        (end (length string)))
    (if (null dot)
        (%ascii-integer string 0 end)
        (let ((whole (%ascii-integer string 0 dot))
              (fraction (%ascii-integer string (1+ dot) end)))
          (when (and whole fraction)
            (let ((seconds (+ whole (/ fraction (expt 10 (- end dot 1))))))
              (if (< whole 1000000)
                  (float seconds 1.0)
                  (ceiling seconds))))))))

(defun %name-index (names string start)
  "The index in NAMES of the three-letter name at START in STRING, or NIL."
  (position-if (lambda (name) (string= name string :start2 start :end2 (+ start 3)))
               names))

(defun %days-in-month (month year)
  "Days in MONTH (1-12) of YEAR, with Gregorian leap years."
  (case month
    (2 (if (and (zerop (mod year 4))
                (or (plusp (mod year 100)) (zerop (mod year 400))))
           29
           28))
    ((4 6 9 11) 30)
    (t 31)))

(defun %parse-imf-fixdate (string)
  "The universal time of an IMF-fixdate (RFC 9110 5.6.7: \"Sun, 06 Nov 1994
08:49:37 GMT\", 29 characters, case-sensitive), else NIL. Every field is
checked before ENCODE-UNIVERSAL-TIME, which signals on some bad fields (second
60, day 0, a year before 1900), normalises others (Feb 31 is Mar 3) and reads a
year below 100 as one near today. A leap second (60) is 59 plus one."
  (when (and (= (length string) 29)
             (%name-index '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") string 0)
             (string= ", " string :start2 3 :end2 5)
             (char= #\Space (char string 7) (char string 11) (char string 16))
             (char= #\: (char string 19) (char string 22))
             (string= " GMT" string :start2 25))
    (let ((day (%ascii-integer string 5 7))
          (month (%name-index '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                              string 8))
          (year (%ascii-integer string 12 16))
          (hour (%ascii-integer string 17 19))
          (minute (%ascii-integer string 20 22))
          (second (%ascii-integer string 23 25)))
      (when (and day month year hour minute second
                 (<= 1900 year)
                 (<= 1 day (%days-in-month (1+ month) year))
                 (<= hour 23) (<= minute 59) (<= second 60))
        (+ (encode-universal-time (min second 59) minute hour day (1+ month) year 0)
           (- second (min second 59)))))))

(defun parse-retry-after (value &key (now (get-universal-time)))
  "Seconds to wait (a non-negative real) from a Retry-After VALUE, or NIL
(rules: this file's header). Never signals: every input is checked before the
call that could signal, and nothing is caught."
  (typecase value
    (real (if (minusp value) 0 value))
    (string
     (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) value)))
       (or (%parse-delta-seconds trimmed)
           (let ((at (%parse-imf-fixdate trimmed)))
             (and at (realp now) (max 0 (- at now)))))))
    (t nil)))

(defun %http-protocol-class (name)
  "The class that NAME (a string) names in the HTTP-PROTOCOL package, or NIL
when the package, the symbol or the class is missing. TYPEP signals on a
symbol that names no type, so only a class object ever reaches it."
  (let ((package (find-package "HTTP-PROTOCOL")))
    (and package
         (stringp name)
         (let ((symbol (find-symbol name package)))
           (and symbol (find-class symbol nil))))))

(defun %condition-text (condition)
  "CONDITION's report on one line: no pretty-printer breaks, no print limits."
  (let ((*print-pretty* nil) (*print-length* nil) (*print-level* nil))
    (princ-to-string condition)))

(defun transport-error-kind-for (condition &key (types +http-protocol-transport-types+))
  "The +TRANSPORT-ERROR-KINDS+ kind for CONDITION, or NIL (rules: this file's
header). TYPES rows (name . kind) are tried in order, and a name is only
looked up in the HTTP-PROTOCOL package, so llm-protocol needs no http-protocol
dependency. A :CONNECT match whose text mentions reset, closed, incomplete or
EOF is :RESET. With no row matching, text that contains \"incomplete HTTP
response\" is :RESET. The report is rendered only when a rule reads it."
  (let ((row (find-if (lambda (row)
                        (let ((class (and (consp row) (%http-protocol-class (car row)))))
                          (and class (typep condition class))))
                      types)))
    (cond ((null row)
           (and (%text-mentions-p (%condition-text condition) "incomplete HTTP response")
                :reset))
          ((and (eq (cdr row) :connect)
                (%text-mentions-p (%condition-text condition)
                                  "reset" "closed" "incomplete" "eof"))
           :reset)
          (t (cdr row)))))

(defun jittered-delay (delay jitter)
  "DELAY x (1 - JITTER x R), R from (RANDOM 1.0d0 *RETRY-RANDOM-STATE*), so the
result is in (DELAY x (1 - JITTER), DELAY]. JITTER 0 returns DELAY itself and
draws nothing. RETRY-DELAY calls it for backoff delays only."
  (check-type delay (real 0 *))
  (check-type jitter (real 0 1))
  (if (zerop jitter)
      delay
      (* delay (- 1 (* jitter (random 1.0d0 *retry-random-state*))))))

(defmethod llm-retry-class ((condition llm-rate-limited))
  "Retryable unless an explicit :RETRYABLE-P NIL says otherwise (the 0.5
status method's rule), so RETRY-POLICY and AUTO-RETRY agree."
  (and (llm-http-error-retryable-p condition) :rate-limit))

(defmethod llm-retry-class ((condition llm-overloaded))
  "Retryable unless an explicit :RETRYABLE-P NIL says otherwise."
  (and (llm-http-error-retryable-p condition) :overloaded))

(defmethod llm-retry-class ((condition llm-auth-error))
  "Never retried, whatever RETRYABLE-P says."
  nil)

(defmethod llm-retry-class ((condition llm-not-found))
  "Never retried, whatever RETRYABLE-P says."
  nil)

(defmethod llm-retry-class ((condition llm-transport-error))
  ":TRANSIENT, except NIL for kind :CANCELED (the caller gave up)."
  (if (eq (llm-transport-error-kind condition) :canceled) nil :transient))
