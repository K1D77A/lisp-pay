(in-package #:lisp-pay)


#||
This file contains the code for a Metaclass used to create the request objects for each
payment processor.
||#

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
    :initarg :query-constructor
    :initform nil)))

(defmethod string-constructor ((class lisp-pay-api-call))
  (in-list (slot-value class 'string-constructor)))

(defmethod query-constructor ((class lisp-pay-api-call))
  (in-list (slot-value class 'query-constructor)))

(defmethod endpoint ((class lisp-pay-api-call))
  (in-list (slot-value class 'endpoint)))


(defclass lisp-pay-api-slot (c2mop:slot-definition)
  ((as-string
    :accessor as-string
    :initarg :as-string
    :type string
    :documentation "String version of the slot-name. When set this is used in place 
of the slot name when encoding in the Query string.")))


(defclass lisp-pay-api-slot-direct (lisp-pay-api-slot c2mop:standard-direct-slot-definition)
  ())

(defclass lisp-pay-api-slot-effective (lisp-pay-api-slot
                                       c2mop:standard-effective-slot-definition)
  ())

(defmethod c2mop:validate-superclass ((class lisp-pay-api-call)
                                      (metaclass c2mop:funcallable-standard-class))
  t)

(defmethod c2mop:validate-superclass ((class lisp-pay-api-slot)
                                      (metaclass standard-class))
  t)

(defmethod c2mop:effective-slot-definition-class ((class lisp-pay-api-call) &rest initargs)
  (declare (ignore initargs))
  (find-class 'lisp-pay-api-slot-effective))

(defmethod c2mop:direct-slot-definition-class ((class lisp-pay-api-call) &rest initargs)
  (declare (ignore initargs))
  (find-class 'lisp-pay-api-slot-direct))

(defclass request ()
  ((request-fun
    :reader request-fun
    :initarg :request-fun
    :initform 'dex:get)
   (content-type
    :accessor content-type
    :initarg :content-type
    :initform "application/json"))
  (:documentation "Top level request class")
  (:metaclass lisp-pay-api-call))

(defmethod print-object ((obj request) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (print-all-slots obj stream)))

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

(defclass patch-request (request-with-content)
  ((request-fun :initform 'dex:patch))
  (:metaclass lisp-pay-api-call))

(defclass response ()
  ()
  (:documentation "Top level response class"))

(defclass api-failure ()
  ()
  (:documentation "API Failure superclass."))

(defmethod print-object ((obj api-failure) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~%")
    (print-all-slots obj stream)))
