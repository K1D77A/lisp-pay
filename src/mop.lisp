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
    :initarg :genned-slot-names)))

(defclass lisp-pay-api-slot (c2mop:slot-definition)
  ())

(defmethod c2mop:validate-superclass ((class lisp-pay-api-call)
                                      (metaclass c2mop:funcallable-standard-class))
  t)

(defmethod c2mop:validate-superclass ((class lisp-pay-api-slot)
                                      (metaclass standard-class))
  t)

(defclass request ()
  ()
  (:documentation "Top level request class")
  (:metaclass lisp-pay-api-call))

(defclass response ()
  ()
  (:documentation "Top level response class"))
