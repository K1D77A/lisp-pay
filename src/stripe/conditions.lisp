(in-package #:lisp-pay/stripe)

(define-condition stripe-condition (lisp-pay-condition)
  ())

(class*:defclass* stripe-api-class (api-failure)
  ((error-type
    :accessor error-type
    :initarg :error-type)
   (parent-condition
    :accessor parent-condition 
    :initarg :parent-condition)
   (code
    :accessor code
    :initarg :code)
   (decline-code
    :accessor decline-code
    :initarg :decline-code)
   (message
    :accessor message
    :initarg :message)
   (param
    :accessor param
    :initarg :param)
   (payment-intent
    :accessor payment-intent
    :initarg :payment-intent)
   (doc-url
    :accessor doc-url
    :initarg :doc-url)
   (payment-method
    :accessor payment-method
    :Initarg :payment-method)
   (payment-method-type
    :accessor payment-method-type
    :initarg :payment-method-type)
   (setup-intent
    :accessor setup-intent
    :initarg :setup-intent)
   (source
    :accessor source
    :initarg :source))
  (:export-class-name-p t)
  (:export-accessor-names-p t))

(class*:defclass* api-error (stripe-api-class)
  ((error-type :initform "api_error"))
  (:documentation "API errors cover any other type of problem (e.g., a temporary problem with Stripe's servers), and are extremely uncommon."))

(class*:defclass* card-error (stripe-api-class)
  ((error-type :initform "card_error")
   (charge
    :accessor charge
    :initarg :charge))
  (:documentation "Card errors are the most common type of error you should expect to handle. They result when the user enters a card that can't be charged for some reason."))

(class*:defclass* indempotency-error (stripe-api-class)
  ((error-type :initform "indempotency_error"))
  (:documentation "Idempotency errors occur when an Idempotency-Key is re-used on a request that does not match the first request's API endpoint and parameters."))

(class*:defclass* invalid-request-error (stripe-api-class)
  ((error-type :initform "invalid_request_error"))
  (:documentation "Invalid request errors arise when your request has invalid parameters."))

(defun %determine-error-class (type)
  (cond ((string= type "invalid_request_error")
         'invalid-request-error)
        ((string= type "card_error")
         'card-error)
        ((string= type "indempotency_error")
         'indempotency-error)
        ((string= type "api_error")
         'api-error)
        (t (error "received an error code that is unexpected."))))

(defmethod %error-key->slot-name (key)
  (let ((alist 
          '(("type" . nil)
            ("message" . nil)
            ("code" . status-code)
            ("decline_code" . decline-code)
            ("param" . param)
            ("payment_intent" . payment-intent)
            ("charge" . charge)
            ("doc_url" . doc-url)
            ("payment_method" . payment-method)
            ("payment_method_type" . payment-method-type)
            ("setup_intent" . setup-intent)
            ("source" . source))))
    (cdr (assoc key alist :test #'string=))))

(defmethod construct-api-failure-object ((processor stripe)
                                         response)
  (with-hash-keys (|error| |type|)
      (gethash "type" (body response))
    (let ((obj (make-instance (%determine-error-class type)
                              :message message
                              :parent-condition condition)))
      (maphash (lambda (key val)
                 (let ((slot-name (%error-key->slot-name parse-as key)))
                   (when slot-name
                     (setf (slot-value obj slot-name) val))))
               parsed)
      obj)))






