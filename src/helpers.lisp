(in-package #:lisp-pay)

(defgeneric raw-body (request)
  (:documentation "Returns the raw body of request."))

(defmethod raw-body ((request lack.request:request))
  (let* ((len (lack.request:request-content-length request))
         (raw (lack.request:request-raw-body request))
         (seq (make-array len :element-type '(unsigned-byte 8))))
    (read-sequence seq raw)
    seq))

(defmethod raw-body ((request tbnl:request))
  (tbnl:raw-post-data :force-binary t))


(defgeneric to-array (ele)
  (:documentation "Convert ELE to an array"))

(defmethod to-array ((str string))
  (babel:string-to-octets))

(defmethod to-array ((str array))
  str)


(defgeneric request-headers (request)
  (:documentation "Returns the headers for REQUEST."))

(defmethod request-headers ((request lack.request:request))
  (lack.request:request-headers request))

(defmethod request-headers ((request tbnl:request))
  (tbnl:headers-in request))

(defun crc-raw (raw-body)
  (ironclad:octets-to-integer (ironclad:digest-sequence :crc32 raw-body)))

(defun in-list (obj)
  (if (listp obj)
      (first obj)
      obj))

(c2mop:define-method-combination string-gen (&optional (order ':most-specific-last))
  ((around (:around))
   (primary (string-gen)))
  (case order
    (:most-specific-first)
    (:most-specific-last (setq primary (reverse primary))))
  (let ((form (if (rest primary)
                  `(concatenate 'string ,@(mapcar #'(lambda (method)
                                                      `(call-method ,method))
                                                  primary))
                  `(call-method ,(first primary)))))

    (if around
        `(call-method ,(first around)
                      (,@(rest around))
                      (make-method ,form))
        form)))

(defgeneric write-json (obj &optional stream)
  (:documentation "Write JSON"))

(defmethod %write-json (processor obj &optional stream)
  (shasht:write-json obj stream))

(defmethod write-json (obj &optional stream)
  (%write-json (symbol-value (find-symbol "*PROCESSOR*")) obj stream))

(defgeneric read-json (stream-or-string)
  (:documentation "Read JSON"))

(defmethod %read-json (processor stream-or-string)
  (shasht:read-json stream-or-string))

(defmethod read-json (stream-or-string)
  (%read-json (symbol-value (find-symbol "*PROCESSOR*")) stream-or-string))


(defgeneric print-all-slots (obj stream))

(defmethod print-all-slots (obj stream)
  (let ((slots (c2mop:class-slots (class-of obj))))
    (format stream "~%")
    (mapc (lambda (slot)
            (let ((name (c2mop:slot-definition-name slot)))
              (when (slot-boundp obj name)
                (format stream "~A: ~A~%" name (slot-value obj name)))))
          slots)))

(defmacro with-hash-keys (keys hash &body body)
  "Creates a let binding for each of the keys listed in KEYS in HASH using gethash, 
each of these KEYS has to have a non nil value otherwise signals 'malformed-json."
  (alexandria:once-only (hash)
    `(let ,(mapcar (lambda (key)
                     `(,key (gethash ,(string key) ,hash)))
            keys)
       (locally ,@body))))

