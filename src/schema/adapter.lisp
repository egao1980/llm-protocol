(in-package #:llm-protocol)

;;; CLOS output via schema-protocol; JSON Schema emit via schema-protocol-json.
;;; Not a PydanticAI Agent — no output tools. Parse-failure repair lives in
;;; GENERATE (*STRUCTURED-OUTPUT-REPAIR*), not here.

(defun %json-object (source)
  (cond
    ((hash-table-p source) source)
    ((stringp source)
     (stack-json:decode (string-trim '(#\Space #\Tab #\Newline #\Return) source)))
    (t source)))

(defun %strip-schema-uri (table)
  (when (hash-table-p table)
    (remhash "$schema" table))
  table)

(defmethod structured-output-json-schema ((schema symbol))
  (%strip-schema-uri (schema-protocol:json-schema schema)))

(defmethod structured-output-json-schema ((schema schema-protocol:schema-class))
  (structured-output-json-schema (class-name schema)))

(defmethod parse-structured-output ((schema symbol) source)
  (schema-protocol:parse schema (%json-object source) :coerce t))

(defmethod parse-structured-output ((schema schema-protocol:schema-class) source)
  (schema-protocol:parse schema (%json-object source) :coerce t))

(defmethod parse-structured-output ((schema hash-table) (source string))
  (declare (ignore schema))
  (%json-object source))
