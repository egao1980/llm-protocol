(in-package #:llm-protocol)

;;; Provider catalog: name → llm-backend. Not LiteLLM.
;;; Model metadata (context-window, per-1M prices) lives on LLM-MODEL-INFO.
;;; Routing / budget is llm-protocol/router.
;;; Refs: "anthropic:claude-sonnet" | "anthropic" | (:anthropic "claude-sonnet")

(defclass llm-provider-catalog () ())

(defclass llm-provider ()
  ((name :initarg :name :accessor llm-provider-name)
   (backend :initarg :backend :accessor llm-provider-backend)
   (models :initarg :models :accessor llm-provider-models :initform nil)
   (context-window :initarg :context-window :accessor llm-provider-context-window
                   :initform nil)))

(defun make-llm-provider (&key name backend models context-window)
  (check-type name string)
  (make-instance 'llm-provider :name name :backend backend
                 :models (copy-list models)
                 :context-window context-window))

(defun llm-provider-p (x)
  (typep x 'llm-provider))

(defclass in-memory-provider-catalog (llm-provider-catalog)
  ((table :initform (make-hash-table :test 'equal)
          :accessor in-memory-catalog-table)))

(defun make-in-memory-provider-catalog ()
  (make-instance 'in-memory-provider-catalog))

(defvar *llm-catalog* nil)

(defun %ensure-catalog (&optional (catalog *llm-catalog*))
  (or catalog
      (restart-case
          (error 'llm-missing-backend
                 :message "*llm-catalog* is nil — call MAKE-IN-MEMORY-PROVIDER-CATALOG")
        (use-value (value)
          :report "Use a supplied LLM-PROVIDER-CATALOG"
          value))))

(defun %provider-key (name)
  (string-downcase
   (cond
     ((stringp name) name)
     ((symbolp name) (symbol-name name))
     (t (princ-to-string name)))))

(defun parse-provider-ref (designator)
  "→ (values provider-name model-or-nil).
   \"anthropic:claude\" → \"anthropic\", \"claude\"
   \"anthropic\" / :anthropic → \"anthropic\", NIL
   (\"anthropic\" \"claude\") / (:anthropic \"claude\") same."
  (cond
    ((null designator)
     (values nil nil))
    ((llm-backend-p designator)
     (values nil nil))
    ((and (consp designator) (null (cdr (last designator))))
     (values (%provider-key (first designator))
             (and (second designator)
                  (if (stringp (second designator))
                      (second designator)
                      (princ-to-string (second designator))))))
    ((symbolp designator)
     (values (%provider-key designator) nil))
    ((stringp designator)
     (let ((colon (position #\: designator)))
       (if colon
           (values (%provider-key (subseq designator 0 colon))
                   (let ((rest (subseq designator (1+ colon))))
                     (and (plusp (length rest)) rest)))
           (values (%provider-key designator) nil))))
    (t (error 'llm-error
              :message (format nil "not a provider ref: ~s" designator)))))

(defgeneric register-provider (catalog name backend &key models context-window)
  (:documentation "Register BACKEND under NAME (string or symbol).
MODELS may be strings or LLM-MODEL-INFO (context-window / prices).
CONTEXT-WINDOW is an optional provider-wide default."))

(defgeneric unregister-provider (catalog name)
  (:documentation "Drop NAME. Missing → LLM-UNKNOWN-PROVIDER (CONTINUE skips)."))

(defgeneric find-provider (catalog name)
  (:documentation "→ LLM-PROVIDER or NIL."))

(defgeneric list-providers (catalog)
  (:documentation "→ list of LLM-PROVIDER."))

(defgeneric resolve-backend (catalog designator)
  (:documentation "→ (values backend model-or-nil).
   DESIGNATOR is an LLM-BACKEND, \"name\", \"name:model\", or (name model)."))

(defgeneric catalog-list-models (catalog &key provider)
  (:documentation "Union of registered models. PROVIDER limits to one name."))

(defmethod register-provider ((catalog in-memory-provider-catalog) name backend
                              &key models context-window)
  (let ((key (%provider-key name)))
    (setf (gethash key (in-memory-catalog-table catalog))
          (make-llm-provider :name key :backend backend :models models
                             :context-window context-window))
    catalog))

(defmethod register-provider ((catalog null) name backend &key models context-window)
  (register-provider (%ensure-catalog) name backend :models models
                     :context-window context-window))

(defmethod unregister-provider ((catalog in-memory-provider-catalog) name)
  (let ((key (%provider-key name))
        (table (in-memory-catalog-table catalog)))
    (if (nth-value 1 (gethash key table))
        (remhash key table)
        (restart-case
            (error 'llm-unknown-provider
                   :name key
                   :message (format nil "unknown provider ~s" key))
          (continue ()
            :report "Skip missing provider"
            nil)
          (use-value (value)
            :report "Return a supplied value"
            (return-from unregister-provider value))))
    catalog))

(defmethod unregister-provider ((catalog null) name)
  (unregister-provider (%ensure-catalog) name))

(defmethod find-provider ((catalog in-memory-provider-catalog) name)
  (gethash (%provider-key name) (in-memory-catalog-table catalog)))

(defmethod find-provider ((catalog null) name)
  (find-provider (%ensure-catalog) name))

(defmethod list-providers ((catalog in-memory-provider-catalog))
  (let ((out '()))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v out))
             (in-memory-catalog-table catalog))
    (sort out #'string< :key #'llm-provider-name)))

(defmethod list-providers ((catalog null))
  (list-providers (%ensure-catalog)))

(defun %missing-provider (name)
  (restart-case
      (error 'llm-unknown-provider
             :name name
             :message (format nil "unknown provider ~s" name))
    (use-value (value)
      :report "Use a supplied LLM-BACKEND"
      value)))

(defmethod resolve-backend ((catalog llm-provider-catalog) designator)
  (cond
    ((llm-backend-p designator)
     (values designator nil))
    (t
     (multiple-value-bind (name model)
         (parse-provider-ref designator)
       (let ((prov (and name (find-provider catalog name))))
         (unless prov
           (let ((supplied (%missing-provider name)))
             (return-from resolve-backend (values supplied model))))
         (values (llm-provider-backend prov) model))))))

(defmethod resolve-backend ((catalog null) designator)
  (if (llm-backend-p designator)
      (values designator nil)
      (resolve-backend (%ensure-catalog) designator)))

(defmethod catalog-list-models ((catalog in-memory-provider-catalog) &key provider)
  (let ((provs (if provider
                   (let ((p (find-provider catalog provider)))
                     (if p (list p)
                         (let ((supplied (%missing-provider (%provider-key provider))))
                           (return-from catalog-list-models
                             (if (listp supplied) supplied (list supplied))))))
                   (list-providers catalog))))
    (mapcan (lambda (p)
              (or (copy-list (llm-provider-models p))
                  (ignore-errors (list-models (llm-provider-backend p)))))
            provs)))

(defmethod catalog-list-models ((catalog null) &key provider)
  (catalog-list-models (%ensure-catalog) :provider provider))

(defun use-in-memory-provider-catalog ()
  (setf *llm-catalog* (make-in-memory-provider-catalog)))
