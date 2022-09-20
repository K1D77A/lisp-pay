(in-package #:lisp-pay/stripe)

(defclass stripe-api-call (lisp-pay-api-call)
  ())

(defclass stripe-api-slot (lisp-pay-api-slot)
  ((translator 
    :accessor translator 
    :initarg :translator 
    :initform nil)))

(defclass stripe-api-slot-direct (stripe-api-slot
                                  c2mop:standard-direct-slot-definition)
  ())

(defclass stripe-api-slot-effective (stripe-api-slot
                                     c2mop:standard-effective-slot-definition)
  ())

(defmethod c2mop:validate-superclass ((class stripe-api-call)
                                      (metaclass c2mop:funcallable-standard-class))
  t)

(defmethod c2mop:validate-superclass ((class stripe-api-slot)
                                      (metaclass standard-class))
  t)

(defmethod c2mop:effective-slot-definition-class ((class stripe-api-call) &rest initargs)
  (find-class 'stripe-api-slot-effective))

(defmethod c2mop:direct-slot-definition-class ((class stripe-api-call) &rest initargs)
  (find-class 'stripe-api-slot-direct))

(defmethod c2mop:compute-effective-slot-definition ((class stripe-api-call) name dslots)
  (call-next-method))

(defclass stripe-request (request)
  ((request-fun
    :reader request-fun
    :initarg :request-fun
    :initform 'dex:get))
  (:metaclass stripe-api-call))

(defclass request-without-content (stripe-request)
  ()
  (:metaclass stripe-api-call))

(defclass get-request (request-without-content)
  ((request-fun :initform 'dex:get))
  (:metaclass stripe-api-call))

(defclass delete-request (request-without-content)
  ((request-fun :initform 'dex:delete))
  (:metaclass stripe-api-call))

(defclass request-with-content (stripe-request)
  ((content
    :accessor content
    :initarg :content
    :type list))
  (:metaclass stripe-api-call))

(defclass post-request (request-with-content)
  ((request-fun :initform 'dex:post))
  (:metaclass stripe-api-call))

(defclass post-files-request (post-request)
  ()
  (:metaclass stripe-api-call))

(defclass put-request (request-with-content)
  ((request-fun :initform 'dex:put))
  (:metaclass stripe-api-call))

(defmacro defapi (name (endpoint super))
  (let* ((slots (slots-from-url endpoint))
         (names (mapcar #'first slots)))
    `(let ((class 
             (defclass ,name (,super)
               ,slots 
               ,@(append `((:metaclass stripe-api-call)
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
