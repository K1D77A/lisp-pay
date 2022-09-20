(in-package #:lisp-pay/coinpayments)

(define-condition coinpayment-condition (lisp-pay-condition)
  ())

(define-condition unknown-status (coinpayment-condition)
  ((status
    :accessor status
    :initarg :status
    :documentation "The unknown status code."))
  (:documentation "signalled when an unknown status code is received.")
  (:report
   (lambda (obj stream)
     (format stream "Unknown status received from IPN. Status: ~A."
             (status obj)))))

(define-condition unsupported-ipn (coinpayment-condition)
  ((ipn
    :accessor ipn
    :initarg :ipn
    :documentation "The IPN that was received but isn't supported.")
   (plist
    :accessor plist
    :initarg :plist
    :documentation "The plist used to try and construct an IPN."))
  (:documentation "Signalled when an attempt was made to construct an unsupported IPN.")
  (:report
   (lambda (obj stream)
     (format stream "An IPN was received that is unknown. IPN: ~A. See (plist <condition>) ~
                    for the original post data."
             (ipn obj)))))

(define-condition no-dispatcher-found (coinpayment-condition)
  ((name
    :accessor name
    :initarg :name
    :documentation "The name of the dispatcher.")
   (ipn
    :accessor ipn
    :initarg :ipn
    :documentation "The IPN in question.")
   (status
    :accessor status
    :initarg :status
    :documentation "The STATUS in question.")
   (arg-count
    :accessor arg-count
    :initarg :arg-count
    :documentation "The number of ARGS for the dispatcher.")
   (ipn-type
    :accessor ipn-type
    :initarg :ipn-type
    :documentation "The ipn-type of the dispatcher.")
   (status-type
    :accessor status-type
    :initarg :status-type
    :documentation "The status-type of the dispatcher."))
  (:documentation "Signalled when an IPN dispatcher cant be found.")
  (:report
   (lambda (obj stream)
     (format stream "Cannot find dispatcher for NAME: ~A. IPN-TYPE: ~A. STATUS-TYPE: ~A ~
                     with ~D ARGS.~%"
             (name obj)
             (ipn-type obj)
             (status-type obj)
             (arg-count obj)))))

(define-condition coinpayment-api-condition (coinpayment-condition)
  ()
  (:documentation "Top level api condition"))

(define-condition required-slots-not-bound (coinpayment-api-condition)
  ((required
    :accessor required
    :initarg :required
    :type list
    :documentation "A list of required slots.")
   (not-set
    :accessor not-set
    :initarg :not-set
    :type list
    :documentation "A list of the required that weren't set."))
  (:documentation "Signalled when you try to instantiate a class but dont set 
required slots")
  (:report
   (lambda (obj stream)
     (format stream "One or more required slots hasn't been set. Required: ~A~%Not set: ~A"
             (required obj)
             (not-set obj)))))

