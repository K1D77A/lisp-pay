(in-package #:lisp-pay)

(defclass lisp-pay-api-call (c2mop:funcallable-standard-class)
  ((string-constructor
    :accessor string-constructor
    :initform nil)
   (endpoint
    :accessor endpoint
    :initarg :endpoint
    :initform nil)
   (genned-slot-names
    :accessor genned-slot-names
    :initarg :genned-slot-names)
   (query-slot-names
    :accessor query-slot-names
    :initarg :query-slot-names)
   (query-constructor
    :accessor query-constructor
    :initarg :query-constructor)))

(defmethod string-constructor ((class lisp-pay-api-call))
  (in-list (slot-value class 'string-constructor)))

(defmethod query-constructor ((class lisp-pay-api-call))
  (in-list (slot-value class 'query-constructor)))

(defmethod endpoint ((class lisp-pay-api-call))
  (in-list (slot-value class 'endpoint)))


(defclass lisp-pay-api-slot (c2mop:slot-definition)
  ())

(defmethod c2mop:validate-superclass ((class lisp-pay-api-call)
                                      (metaclass c2mop:funcallable-standard-class))
  t)

(defmethod c2mop:validate-superclass ((class lisp-pay-api-slot)
                                      (metaclass standard-class))
  t)

(defclass request ()
  ((request-fun
    :reader request-fun
    :initarg :request-fun
    :initform 'dex:get))
  (:documentation "Top level request class")
  (:metaclass lisp-pay-api-call))

(defclass request-without-content (request)
  ()
  (:metaclass lisp-pay-api-call))

(defclass get-request (request-without-content)
  ((request-fun :initform 'dex:get))
  (:metaclass lisp-pay-api-call))

(defclass delete-request (request-without-content)
  ((request-fun :initform 'dex:delete))
  (:metaclass lisp-pay-api-call))

(defclass request-with-content (request)
  ((content
    :accessor content
    :initarg :content
    :type list))
  (:metaclass lisp-pay-api-call))

(defclass post-request (request-with-content)
  ((request-fun :initform 'dex:post))
  (:metaclass lisp-pay-api-call))

(defclass post-files-request (post-request)
  ((content-type
    :initform "multipart/related"))
  (:metaclass lisp-pay-api-call))

(defclass put-request (request-with-content)
  ((request-fun :initform 'dex:put))
  (:metaclass lisp-pay-api-call))

(defclass patch-request (request)
  ((request-fun :initform 'dex:patch)
   (patch-request
    :accessor patch-request
    :initarg :patch-request))
  (:metaclass lisp-pay-api-call))



(defclass response ()
  ()
  (:documentation "Top level response class"))
