(in-package #:llm-protocol/tests)

;;; 0.8.0: llm-protocol#21 (RETRY exported), #16 (CONTEXT-WINDOW: catalog
;;; first, cached BACKEND-MODEL-INFO), #18 (:overloaded-factor), #20
;;; (MCP image mimeType, audio parts).

;;; --- #21 -------------------------------------------------------------------

(deftest retry-restart-is-exported
  (ok (eq :external (nth-value 1 (find-symbol "RETRY" :llm-protocol))))
  (let ((tries 0))
    (ok (eq :done
            (handler-bind ((llm-protocol:llm-error
                             (lambda (c)
                               (declare (ignore c))
                               (when (find-restart 'llm-protocol:retry)
                                 (invoke-restart 'llm-protocol:retry)))))
              (llm-protocol:with-llm-restarts
                (if (< (incf tries) 3)
                    (error 'llm-protocol:llm-error :message "again")
                    :done))))
        "a handler names the restart as llm-protocol:retry")
    (ok (= 3 tries))))

;;; --- #16 -------------------------------------------------------------------

(defclass counting-backend (llm-protocol:mock-llm-backend)
  ((calls :initform 0 :accessor counting-backend-calls)))

(defmethod llm-protocol:list-models :before ((backend counting-backend) &key)
  (incf (counting-backend-calls backend)))

(defun %counting-backend (&rest models)
  (make-instance 'counting-backend :models models :default-model "m1"))

(deftest context-window-caches-the-model-listing
  (let ((backend (%counting-backend (llm-protocol:make-llm-model-info :id "m1" :context-window 4096)))
        (llm-protocol:*llm-catalog* nil))
    (dotimes (i 3)
      (ok (= 4096 (llm-protocol:context-window backend "m1"))))
    (ok (= 1 (counting-backend-calls backend)) "one LIST-MODELS for three lookups")
    (llm-protocol:clear-model-info-cache backend)
    (ok (= 4096 (llm-protocol:context-window backend "m1")))
    (ok (= 2 (counting-backend-calls backend)) "CLEAR-MODEL-INFO-CACHE drops the listing")
    (let ((llm-protocol:*model-info-cache-ttl* 0))
      (llm-protocol:context-window backend "m1")
      (llm-protocol:context-window backend "m1")
      (ok (= 4 (counting-backend-calls backend)) "TTL 0 caches nothing"))))

(deftest context-window-prefers-the-catalog
  (let* ((backend (%counting-backend (llm-protocol:make-llm-model-info :id "m1")))
         (llm-protocol:*llm-catalog* (llm-protocol:make-in-memory-provider-catalog)))
    (llm-protocol:register-provider llm-protocol:*llm-catalog* "p" backend
                                    :models (list (llm-protocol:make-llm-model-info
                                                   :id "m1" :context-window 8000)))
    (ok (= 8000 (llm-protocol:context-window backend "m1"))
        "a listing without a window does not shadow the catalog's")
    (ok (= 0 (counting-backend-calls backend)) "the catalog answers without a listing")))

(defclass info-backend (counting-backend) ())

(defmethod llm-protocol:backend-model-info ((backend info-backend) id)
  (and (equal id "m1") (llm-protocol:make-llm-model-info :id "m1" :context-window 128000)))

(deftest backend-model-info-is-the-hook
  (let ((backend (make-instance 'info-backend :default-model "m1"))
        (llm-protocol:*llm-catalog* nil))
    (ok (= 128000 (llm-protocol:context-window backend "m1")))
    (ok (= 0 (counting-backend-calls backend)) "a backend method needs no listing")
    (ok (null (llm-protocol:context-window backend "m2")))))

;;; --- #18 -------------------------------------------------------------------

(deftest overloaded-factor-is-configurable
  (let ((overloaded (make-condition 'llm-protocol:llm-http-error :status 503)))
    (ok (= 20 (llm-protocol:retry-delay (llm-protocol:make-retry-policy :base-delay 10)
                                        overloaded 1))
        "the default doubles an overloaded delay, as before")
    (ok (= 10 (llm-protocol:retry-delay (llm-protocol:make-retry-policy :base-delay 10
                                                                         :overloaded-factor 1)
                                        overloaded 1)))
    (ok (= 1 (llm-protocol:retry-policy-overloaded-factor
              (llm-protocol:make-retry-policy :overloaded-factor 1))))
    (ok (signals (llm-protocol:make-retry-policy :overloaded-factor -1) 'type-error))))

;;; --- #20 -------------------------------------------------------------------

(defun %hash (&rest plist)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on plist by #'cddr do (setf (gethash k h) v))
    h))

(deftest mcp-image-media-type
  (let ((part (llm-protocol::%coerce-part
               (%hash "type" "image" "data" "iVBORw0K" "mimeType" "image/png"))))
    (ok (llm-protocol:llm-image-part-p part))
    (ok (equal "image/png" (llm-protocol:llm-image-part-media-type part)))
    (ok (equal "iVBORw0K" (llm-protocol:llm-image-part-data part)))))

(deftest audio-blocks-become-audio-parts
  (let ((mcp (llm-protocol::%coerce-part
              (%hash "type" "audio" "data" "UklGRg==" "mimeType" "audio/wav")))
        (openai (llm-protocol::%coerce-part
                 (%hash "type" "input_audio"
                        "input_audio" (%hash "data" "SUQz" "format" "mp3")))))
    (ok (llm-protocol:llm-audio-part-p mcp))
    (ok (equal "audio/wav" (llm-protocol:llm-audio-part-media-type mcp)))
    (ok (equal "UklGRg==" (llm-protocol:llm-audio-part-data mcp)))
    (ok (llm-protocol:llm-audio-part-p openai))
    (ok (equal "audio/mpeg" (llm-protocol:llm-audio-part-media-type openai)))
    (ok (equal "SUQz" (llm-protocol:llm-audio-part-data openai)))
    (let ((back (llm-protocol:part-from-state (llm-protocol:part-state mcp))))
      (ok (llm-protocol:llm-audio-part-p back) "part-state round trip")
      (ok (equal "audio/wav" (llm-protocol:llm-audio-part-media-type back)))
      (ok (equal "UklGRg==" (llm-protocol:llm-audio-part-data back))))))
