(in-package #:lisp-pay)

#||
Many helpers for defining MOP protocols for API wrappers.
||#
(defclass processor ()
  ((base-url
    :accessor base-url
    :initarg :base-url)
   (api-metaclass
    :accessor api-metaclass)))

(defclass testing-processor (processor)
  ())

(defun replace-vars-for-slot-names (split slots)
  "Takes in a split list split by #\/ (url) produces a list of slot names"
  (mapcar (lambda (str)
            (let ((found? (find (subseq str 1) slots :test #'string-equal)))
              (if found?
                  found?
                  str)))
          split))

(defun gen-url-generator (class)
  "Compiles a function for CLASS that when called with the request object. When evaluated
the function returns a string with specific slots values written into a string."
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
  "Given QUERY-SLOTS and QUERY-SLOT-NAMES compiles a function that when evaluated 
with a request will step through the query slots and concatenate the query slot names and
values into a string. Uses #'encode-query-slot to encode the query params."
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

(defgeneric encode-query-slot (request slot slot-name)
  (:documentation "Based on REQUEST SLOT and SLOT-NAME encode the slot in a HTTP Query 
param format Uses #'encode-query-value for value encoding."))

(defmethod encode-query-slot (request slot slot-name)
  (format nil "~A=~A"
          (if (slot-boundp slot 'as-string)
              (slot-value slot 'as-string)
              (string-downcase (symbol-name slot-name)))
          (encode-query-value slot (slot-value request slot-name))))

(defgeneric encode-query-value (slot value)
  (:documentation "Given a SLOT and a VALUE encodes the value correctly for a HTTP 
Query param. Encode differently based on the type of the Value."))

(defmethod encode-query-value (slot (value string))
  (quri:url-encode value))

(defmethod encode-query-value (slot (value number))
  (quri:url-encode (format nil "~D" value)))

(defmethod encode-query-value (slot (value (eql nil)))
  "false")

(defmethod encode-query-value (slot (value (eql t)))
  "true")

(defmethod encode-query-value (slot (value sequence))
  "When a VALUE is sequence, then we will encode it like a query param array."
  (let ((name (string-downcase (c2mop:slot-definition-name slot))))
    (format nil "~{~A~^&~}" 
            (map 'list
                 (lambda (val)
                   (format nil "~A[]=~A" name (quri:url-encode val)))
                 value))))

(defun slots-from-url (url)
  "Given a URL splits the strings out by #\: and then generates a list of plists 
used for creating slots in a class."
  (let* ((split (str:split #\/ url :omit-nulls t))
         (slots (remove-if-not (lambda (ele) (char= #\: (aref ele 0))) split)))
    (mapcar (lambda (slot)
              (let* ((name (subseq slot 1))
                     (upcase (string-upcase name))
                     (intern (intern upcase))
                     (key (intern upcase :keyword)))
                (list intern :accessor intern :initarg key)))
            slots)))

(defgeneric generate-url (processor request)
  (:documentation "Generate a request URL that is passed to Dex."))

(defmethod generate-url (processor req)
  "Default URL generator."
  (with-accessors ((string-constructor string-constructor)
                   (query-constructor query-constructor))
      (class-of req)
    (concatenate 'string
                 (base-url processor)
                 (funcall string-constructor req)
                 (when query-constructor
                   (funcall query-constructor req)))))

(defgeneric generate-dex-list (processor request)
  (:method-combination append :most-specific-last)
  (:documentation "Generate a list passed to dex using #'apply. Specialized by each 
payment processor."))

(defgeneric call-api (request)
  (:documentation "Generic means of making per processor requests."))

(defmethod call-api (request)
  (%call-api lisp-pay:*processor* request))

(defmacro wrap-dex-call (&body body)
  `(new-dex-response
    (handler-case
        (multiple-value-list (locally ,@body))
      (dexador:http-request-failed (c)
        c))))

(defmethod %call-api (processor request)
  "Call the API using PROCESSOR. Use an :around with your processor to establish restarts."
  (let ((url (generate-url processor request))
        (args (generate-dex-list processor request))
        (fun (request-fun request)))
    (construct-response-from-api processor
                                 (wrap-dex-call 
                                   (apply fun url args)))))
