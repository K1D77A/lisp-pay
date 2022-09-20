(in-package #:lisp-pay)

(defgeneric validate-webhook (request &rest args)
  (:documentation "Given a REQUEST attempts to validate a webhook REQUEST. ARGS is used to
pass in arguments to the individual validation methods. If it fails then signals 
'webhook-validation-failed"))

(defmethod validate-webhook :around (request &rest args)
  (or (call-next-method)
      (error 'webhook-validation-failed
             :request request
             :args args)))

