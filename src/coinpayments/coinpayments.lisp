(in-package #:lisp-pay/coinpayments)

(defparameter *strings->symbols* (make-hash-table :test #'equal)
  "A hash-table of key values representing the strings and their converted counterparts")

(defparameter *ipn-dispatchers* ()
  "A list of plists used to dispatch on various ipn-types.")

(defun new-symbol (string)
  (setf (gethash string *strings->symbols*)
        (intern (string-upcase (str:param-case string)) :keyword)))

(defun string->symbol (string)
  (let ((sym (gethash string *strings->symbols*)))
    (if sym
        sym
        (new-symbol string))))

(defun new-ipn-dispatcher (name ipn-type status-class args-count fun)
  (setf (getf (getf (getf (getf *ipn-dispatchers* name) args-count) status-class) ipn-type)
        fun))

(defmacro def-ipn-dispatcher (name ((ipn-var ipn-type) (status-var status-class)
                                    &rest args)
                              &body body)
  "Defines a new dispatch function under the name NAME. This is very similar to 
a defmethod form however dispatching from most to least specific subclasses is only done
with the status-class. IPN-VAR names the IPN passed into the fun (a plist). STATUS-VAR 
names the status object passed into the fun, args is a list of "
  (check-type ipn-type keyword)
  `(new-ipn-dispatcher ',name ,ipn-type ',status-class
                       ,(length args)
                       (lambda (,ipn-var ,status-var ,@args)
                         (declare (ignorable ,ipn-var ,status-var ,@args))
                         (locally ,@body))))

(defun find-dispatcher (name ipn-type status arg-count)
  "Attempts to find the most specific function for NAME IPN-TYPE and STATUS (all symbols)
within *ipn-dispatchers*, dispatching is done both on the ipn-type, arg-count and status, 
if no dispatcher is found under name then returns nil, if one is then tries to find
dispatcher with ARG-COUNT args, if non exists returns nil, otherwise checks under
 the ipn-status, if none are found by that name under that specific status
then recalls FIND-DISPATCHER with the superclasses of STATUS. If some dispatchers are found
under that name and that status, then attempts to find the dispatcher by IPN-TYPE, if one 
is found returns it, otherwise returns nil. This will find most specific dispatchers first."
  (let ((by-name (getf *ipn-dispatchers* name)))    
    (when by-name
      (let ((by-arg (getf by-name arg-count)))
        (when by-arg 
          (let ((by-status (getf by-arg status)))        
            (if by-status
                (getf by-status ipn-type)
                (loop :for superclass :in (c2mop:class-direct-superclasses
                                           (find-class status))
                        :thereis (find-dispatcher name ipn-type
                                                  (class-name superclass)
                                                  arg-count)))))))))
            
(defun ipn-dispatch (name ipn status &rest args)
  "Uses FIND-DISPATCH to execute the dispatcher found with IPN, STATUS and (length  ARGS).
 If
no dispatcher is found signals 'no-dispatcher-found. Its important to note that no 
default dispatchers are provided so your fall-back behaviour should involve catching
that condition and doing something with the condition object."
  (let* ((type (intern (string-upcase (getf ipn :ipn-type)) :KEYWORD))
         (status-class (class-name (class-of status)))
         (dispatcher (find-dispatcher name type status-class (length args))))
    (if dispatcher
        (apply dispatcher ipn status args)
        (error 'no-dispatcher-found :status-type status-class :ipn-type type
                                    :name name :ipn ipn :status status
                                    :arg-count (length args)))))

(defmacro dispatch-ipn-by-name (name ipn status &rest args)
  "Macro version of IPN-DISPATCH. It just looks nicer."
  `(ipn-dispatch ',name ,ipn ,status ,@args))

(defgeneric parse-data (data)
  (:documentation "Attempts to parse 'data' into a plist with keys that match the 
IPN class definitions in classes.lisp."))

(defmethod parse-data ((data list))
  (loop :for list :in data
        :appending (destructuring-bind (key . val)
                       list
                     (list (string->symbol key) val))))

(defmethod parse-data ((data string))
  "Parse a www-url-form-encoded string into something that can be used to 
   make a coinpayment ipn object."
  (let ((data (str:split #\& data)))
    (loop :for key-val :in data
          :appending (destructuring-bind (key val)
                         (str:split #\= key-val)
                       (list (string->symbol key) val)))))

(defmethod parse-data ((data array))
  "Convert an array which contains the raw post data into a plist representing the IPN.
This method assumes the array provided is not unsigned-byte 8 so it converts it."
  (let ((arr (make-array (length data) :element-type '(unsigned-byte 8)
                                       :initial-contents data)))
    (parse-data (babel:octets-to-string arr))))

(defgeneric verify-data (hmac private-key raw-post)
  (:documentation "A generic function to verify the integrity of a request while
PRIVATE-KEY and RAW-POST are in different formats. Depending on the request, 
I think PRIVATE-KEY is either meant to be your private-api key, or your private IPN key.
IPN key for validating IPN's and private api key for sending requests to the API."))

(defmethod verify-data (hmac private-key raw-post)
  (string= hmac 
           (ironclad:byte-array-to-hex-string
            (ironclad::hkdf-extract 'ironclad:sha512 private-key raw-post))))

(defmethod verify-data (hmac (private-key string) (raw-post array))
  (let ((pk-b (babel:string-to-octets private-key)))
    (verify-data hmac pk-b raw-post)))

(defmethod verify-data (hmac private-key (raw-post string))
  (let ((rp-b (babel:string-to-octets raw-post)))
    (verify-data hmac private-key rp-b)))

(defun determine-unknown-status (n)
  "After calling CONSTRUCT-STATUS this is called in an attempt to determine whether the 
status is part of the API not implemented yet."
  (cond ((< n 0)
         (make-instance 'ipn-failure))
        ((<= 0 n 99)
         (make-instance 'ipn-payment-pending))
        ((>= n 100)
         (make-instance 'ipn-payment-sucess))
        (t (error 'unknown-status :status n))))

(defun n->status (n)
  "Given a number N attempts to determine which IPN-STATUS object to create,
you can see all of the available status classes within classes.lisp. If it cannot match
the numbers specifically then more general classes are used based on the coinpayments
'loose' recommendations for future proofing, this is done with DETERMINE-UNKNOWN-STATUS."
  (case n
    (-2 (make-instance 'negative-2))
    (-1 (make-instance 'negative-1))
    (0 (make-instance 'zero))
    (1 (make-instance 'one))
    (2 (make-instance 'two))
    (3 (make-instance 'three))
    (5 (make-instance 'five))
    (100 (make-instance 'one-hundred))
    (otherwise (determine-unknown-status n))))

(defun construct-status (plist)
  "Given a plist of post parameters that have been parsed by parse-data beforehand,
attempts to first construct and IPN object, in the event this fails it will signal 
'unsupported-ipn, then attempts to construct an IPN-STATUS object using CONSTRUCT-STATUS.
These are then returned as multiple values ipn,status."
  (let ((ipn (getf plist :ipn-type))
        (status (parse-integer (getf plist :status) :junk-allowed t)))
    (when (and ipn status)      
      (values (n->status status) plist))))

;; ;;;these funs were just used to parse the ipn description html.
;; (defun field->slot (field)
;;   (destructuring-bind (&key field desc &allow-other-keys)
;;       field
;;     (format t "(~A~%:documentation \"~A\"~%:initarg ~S)~%"
;;             field desc (intern (symbol-name field) :keyword))))

;; (defun fields-to-slots (string)
;;   (let ((split-newline (str:split #\Newline string :omit-nulls t)))
;;     (let ((slots
;;             (loop :for list :in 
;;                             (mapcar (lambda (string)
;;                                       (str:split #\Tab string))
;;                                     split-newline)
;;                   :collect (destructuring-bind (field description required)
;;                                list
;;                              (list :field (intern (string-upcase (str:param-case field)))
;;                                    :desc description
;;                                    :required (if (string= required "Yes")
;;                                                  t
;;                                                  nil))))))
;;       (mapc #'field->slot slots))))

