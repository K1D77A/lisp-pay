(in-package #:lisp-pay/coinpayments)

;;;;this file contains some helpers for working with the coinpayments.net api.

(defparameter *sym->string* (make-hash-table :test #'equal)
  "A list of symbols and their string counterparts")

(defun new-string (symbol)
  (setf (gethash (symbol-name symbol) *sym->string*)
        (str:snake-case (symbol-name symbol))))

(defun symbol->string (symbol)
  (let ((sym (gethash (symbol-name symbol) *sym->string*)))
    (if sym
        sym
        (new-string symbol))))

(defmacro new-request (name cmd required &rest params)
  "Creates a new class by NAME which is a subclass of 'request, 
CMD is a string which is set to the initform of the slot cmd. REQUIRED is a list
of slots that are required for this request. PARAMS is all of the slots, yes
you will have to repeat whats in required in params. Also if you were to add some
new API that I didn't see then where they use a case like abc_def you can just
use symbols like abc-def because they are automatically translated to the 
correct case, dont worry this is memoized to make it faster."
  `(progn (defclass ,name (coinpayment-request)
            ((cmd :initform ,cmd)
             (required :initform ',(append required '(merchant-secret-key key)))
             ,@(loop :for param :in params
                     :collect `(,param
                                :accessor ,param
                                :initarg ,(intern (string-upcase param) :keyword)
                                :type string)))
            )
          (export '(,name ,@params) 'lisp-pay/coinpayments)))

(defmethod initialize-instance :after ((request coinpayment-request)
                                       &rest initargs &key &allow-other-keys)
  "After initialization this confirms that all required slots have been filled, 
computes the alist for the post request which is passed to dex:post, computes a 
list of those same post parameters which is used for generating a hmac header, 
and then finally computes that hmac header."
  (declare (ignore initargs))
  (validate-slots request)
  ;; (convert-merchant-secret-key request)
  (compute-dex-alist request)
  (compute-post-params request)
  (compute-final-hmac request))

(defmethod convert-merchant-secret-key ((request coinpayment-request))
  (with-accessors ((key merchant-secret-key))
      request
    (when (stringp key)
      (make-array (length key) :element-type '(unsigned-byte 8)
                               :initial-contents (babel:string-to-octets key)))))

(defmethod validate-slots ((request coinpayment-request))
  "When an instance of REQUEST is initialized, afterwards this will guarantee that 
all of the slots that are required for that api call have been set. If any haven't
then signals a condition of type 'required-slots-not-bound."
  (with-slots (required)
      request
    (when required 
      (unless (every (lambda (slot)
                       (slot-boundp request slot))
                     required)
        (let ((unbound
                (loop :for slot :in required
                      :unless (slot-boundp request slot)
                        :collect slot)))
          (error 'required-slots-not-bound :not-set unbound
                                           :required required))))))

(defgeneric compute-hmac (merchant-secret-key string)
  (:documentation "Computes a hmac from the merchant-secret-key and a string."))

(defmethod compute-hmac ((merchant-secret-key array) (string array))
  (ironclad:byte-array-to-hex-string
   (ironclad::hkdf-extract 'ironclad:sha512 merchant-secret-key string)))

(defmethod compute-hmac ((merchant-secret-key string) (string string))
  (let ((pk (babel:string-to-octets merchant-secret-key))
        (str (babel:string-to-octets string)))
    (compute-hmac
     (make-array (length pk) :element-type '(unsigned-byte 8)
                             :initial-contents pk)
     (make-array (length str) :element-type '(unsigned-byte 8)
                              :initial-contents str))))

(defmethod compute-dex-alist ((request coinpayment-request))
  "This function must be called before 'compute-dex-alist. This computes the 
alist used as the :content key to dex:post, it loops through all the slots in the class
and creates an alist from their slotname and values, it ignores slots that are 
any of '(dex-alist required merchant-secret-key) or are unbound."
  (let* ((slots (c2mop:class-slots (class-of request))))
    (setf (dex-alist request)
          (loop :for slot :in slots
                :for name := (c2mop:slot-definition-name slot)
                  :then (c2mop:slot-definition-name slot)
                :unless (or (find name '(dex-alist required merchant-secret-key))
                            (not  (slot-boundp request name)))
                  :collect (cons (symbol->string name)
                                 (slot-value request name))))
    request))

(defmethod compute-post-params ((request coinpayment-request))
  "This function must be called before 'compute-final-hmac. This computes a 'post string'
that can be used for computing the hmac. It uses the dex-alist values as its arguments."
  (let* ((stream (make-string-output-stream)))
    (loop :for (key . val) :in (dex-alist request)
          :do (format stream "~A=~A&" key val))
    (let ((post (get-output-stream-string stream)))
      (setf (post-string request) (subseq post 0 (1- (length post))))))
  request)

(defmethod compute-final-hmac ((request coinpayment-request))
  "This function is the last step in initializing a request object, it computes the 
final HMAC from the merchant-secret-key and the post-string which was previously generated."
  (setf (hmac request)
        (compute-hmac (merchant-secret-key request) (post-string request)))
  request)

(defmethod request ((request coinpayment-request))
  "When given a constructed REQUEST object, attempts to use the object to make 
a request to the coinpayments API. If successfully returns a 'good-response object,
if there was a failure with your values then returns a 'bad-response object. 
See the definition of 'response for information on the slots."
  (with-accessors ((hmac hmac)
                   (dex-alist dex-alist))
      request
    (multiple-value-bind (result code headers url stream)
        (dex:post "https://www.coinpayments.net/api.php"
                  :content dex-alist
                  :headers `(("HMAC" . ,hmac)))
      (destructuring-bind (&key |result| |error| &allow-other-keys)
          (locally (declare (optimize (speed 3) (safety 1)))
            (jojo:parse result))
        (apply #'make-instance 
               (if (string= |error| "ok")
                   'good-response
                   'bad-response)
               (list :result-slot |result|
                     :error-slot |error|
                     :dex-extra (list :code code :headers headers
                                      :url url :stream stream)
                     :request request))))))


