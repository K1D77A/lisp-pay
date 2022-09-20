(in-package #:lisp-pay/stripe)

(defclass api-call (c2mop:funcallable-standard-class)
  ((string-constructor
    :accessor string-constructor
    :initform nil)
   (endpoint
    :accessor endpoint
    :initarg :endpoint
    :initform nil)
   (genned-slot-names
    :accessor genned-slot-names
    :initarg :genned-slot-names)))

(defclass api-slot (c2mop:slot-definition)
  ((translator 
    :accessor translator 
    :initarg :translator 
    :initform nil)))

(defclass api-slot-direct (api-slot c2mop:standard-direct-slot-definition)
  ())

(defclass api-slot-effective (api-slot c2mop:standard-effective-slot-definition)
  ())

(defmethod c2mop:validate-superclass ((class api-call)
                                      (metaclass c2mop:funcallable-standard-class))
  t)

(defmethod c2mop:validate-superclass ((class api-slot)
                                      (metaclass standard-class))
  t)


(defmethod c2mop:effective-slot-definition-class ((class api-call) &rest initargs)
  (find-class 'api-slot-effective))

(defmethod c2mop:direct-slot-definition-class ((class api-call) &rest initargs)
  (find-class 'api-slot-direct))

(defmethod c2mop:compute-effective-slot-definition ((class api-call) name dslots)
  (call-next-method))

(defclass stripe-request ()
  ((request-fun
    :reader request-fun
    :initarg :request-fun
    :initform 'dex:get))
  (:metaclass api-call))

(defclass request-without-content (stripe-request)
  ()
  (:metaclass api-call))

(defclass get-request (request-without-content)
  ((request-fun :initform 'dex:get))
  (:metaclass api-call))

(defclass delete-request (request-without-content)
  ((request-fun :initform 'dex:delete))
  (:metaclass api-call))

(defclass request-with-content (stripe-request)
  ((content
    :accessor content
    :initarg :content
    :type list))
  (:metaclass api-call))

(defclass post-request (request-with-content)
  ((request-fun :initform 'dex:post))
  (:metaclass api-call))

(defclass post-files-request (post-request)
  ()
  (:metaclass api-call))

(defclass put-request (request-with-content)
  ((request-fun :initform 'dex:put))
  (:metaclass api-call))

(defun in-list (obj)
  (if (listp obj)
      (first obj)
      obj))

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

(defmacro defapi (name (endpoint super))
  (let* ((slots (slots-from-url endpoint))
         (names (mapcar #'first slots)))
    `(let ((class 
             (defclass ,name (,super)
               ,slots 
               ,@(append `((:metaclass api-call)
                           (:genned-slot-names ,names)
                           (:endpoint ,endpoint))))))
       (c2mop:ensure-finalized class)
       (with-slots (string-constructor)
           class
         (setf (string-constructor class) (gen-url-generator class))))))

(defmacro defapi%get (name (endpoint))
  `(defapi ,name (,endpoint get-request)))

(defmacro defapi%delete (name (endpoint))
  `(defapi ,name (,endpoint delete-request)))

(defgeneric form-dex-args (request)
  (:method-combination append :most-specific-last))

(defmethod form-dex-args append ((request stripe-request))
  `(:basic-auth ,(list *api-key*) :headers ,(list `("Stripe-Version" . ,*api-version*))))

(defmethod form-dex-args append ((request request-without-content))
  nil)

(defmethod form-dex-args append ((request request-with-content))
  (with-slots (content)
      request 
    `(:content ,content :headers (("Content-Type" . "application/x-www-form-urlencoded")))))

(defmethod call-api (req)
  (with-slots (url request-fun)
      req 
    (let ((complete-url (generate-url req))
          (args (form-dex-args req)))
      (with-captured-api-failure
       (jojo:parse (apply request-fun complete-url args) :as *parse-as*)))))

(defmethod determine-base-url (req)
  *url*)

(defmethod determine-base-url ((req post-files-request))
  "https://files.stripe.com")

(defmethod generate-url ((req stripe-request))
  (with-accessors ((string-constructor string-constructor))
      (class-of req)
    (format nil "~A~A" (determine-base-url req)
            (funcall (in-list string-constructor) req))))
