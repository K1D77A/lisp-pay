(in-package #:lisp-pay)

#||
Many helpers for defining MOP protocols for API wrappers.
||#

(defun replace-vars-for-slot-names (split slots)
  (mapcar (lambda (str)
            (let ((found?
                    (find (subseq str 1) slots :test #'string-equal)))
              (if found?
                  found?
                  str)))
          split))

(defun gen-url-generator (class)
  (with-accessors ((endpoint endpoint)
                   (genned-slot-names genned-slot-names))
      class 
    (let* ((split (str:split #\/ (in-list endpoint) :omit-nulls t))
           (slots (in-list (genned-slot-names class)))
           (compared (replace-vars-for-slot-names split slots)))
      (if slots 
          (compile nil
                   `(lambda (request)
                      (format nil "/~{~A~^/~}"
                              (loop :for slot? :in ',compared
                                    :collect
                                    (if (stringp slot?)
                                        slot? 
                                        (quri:url-encode 
                                         (slot-value request slot?)))))))
          (lambda (request)
            (declare (ignore request))
            (in-list endpoint))))))

(defun gen-query-generator (query-slots query-slot-names)
  (if query-slots 
      (compile nil
               `(lambda (request)
                  (let ((str
                          (format nil "?~{~A~^&~}"
                                  (loop :for slot :in ',query-slots
                                        :for slot-name :in ',query-slot-names
                                        :if (slot-boundp request slot-name)
                                          :collect
                                          (encode-query-slot request slot slot-name)))))
                    (if (string= str "?")
                        ""
                        str))))
      (lambda (req)
        (declare (ignore req))
        "")))

(defgeneric encode-query-slot (request slot slot-name))

(defmethod encode-query-slot (request slot slot-name)
  (format nil "~A=~A"
          (if (slot-boundp slot 'as-string)
              (slot-value slot 'as-string)
              (string-downcase (symbol-name slot-name)))
          (encode-query-value slot (slot-value request slot-name))))

(defgeneric encode-query-value (slot value))

(defmethod encode-query-value (slot (value string))
  (quri:url-encode value))

(defmethod encode-query-value (slot (value number))
  (quri:url-encode (format nil "~D" value)))

(defmethod encode-query-value (slot (value (eql nil)))
  "false")

(defmethod encode-query-value (slot (value (eql t)))
  "true")

(defmethod encode-query-value (slot (value sequence))
  (let ((name (string-downcase (c2mop:slot-definition-name slot))))
    (format nil "~{~A~^&~}" 
            (map 'list
                 (lambda (val)
                   (format nil "~A[]=~A" name (quri:url-encode val)))
                 value))))

(defun slots-from-url (url)
  (let* ((split (str:split #\/ url :omit-nulls t))
         (slots (remove-if-not (lambda (ele) (char= #\: (aref ele 0))) split)))
    (mapcar (lambda (slot)
              (let* ((name (subseq slot 1))
                     (upcase (string-upcase name))
                     (intern (intern upcase))
                     (key (intern upcase :keyword)))
                (list intern :accessor intern :initarg key)))
            slots)))

(defgeneric generate-url (processor request))

(defmethod generate-url (processor req)
  (with-accessors ((string-constructor string-constructor)
                   (query-constructor query-constructor))
      (class-of req)
    (concatenate 'string
                 (base-url processor)
                 (funcall string-constructor req)
                 (when query-constructor
                   (funcall query-constructor req)))))

(defgeneric generate-dex-list (processor request)
  (:method-combination append :most-specific-last))



(defgeneric call-api (request)
  (:documentation "Generic means of making per processor requests."))

(defmethod call-api (request)
  (%call-api (symbol-value (find-symbol "*PROCESSOR*")) request))

(defmethod %call-api (processor request)
  "Call the API using PROCESSOR. Use an :around with your processor to establish restarts."
  (let ((url (generate-url processor request))
        (args (generate-dex-list processor request))
        (fun (request-fun request)))
    (construct-response-from-api processor
                                 (wrap-dex-call 
                                   (apply fun url args)))))
