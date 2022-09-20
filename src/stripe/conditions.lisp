(in-package #:lisp-pay/stripe)

(defparameter *parse-as* :plist
  "Used to parse data with call-api. Defaults to jojo's :plist, can be any valid parser key, best use :hash-table")

(define-condition stripe-condition (lisp-pay-condition)
  ())

(define-condition stripe-api-condition (stripe-condition)
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
    :initarg :source)))

(define-condition api-error (stripe-api-condition)
  ((error-type :initform "api_error"))
  (:documentation "API errors cover any other type of problem (e.g., a temporary problem with Stripe's servers), and are extremely uncommon."))

(define-condition card-error (stripe-api-condition)
  ((error-type :initform "card_error")
   (charge
    :accessor charge
    :initarg :charge))
  (:documentation "Card errors are the most common type of error you should expect to handle. They result when the user enters a card that can't be charged for some reason."))

(define-condition indempotency-error (stripe-api-condition)
  ((error-type :initform "indempotency_error"))
  (:documentation "Idempotency errors occur when an Idempotency-Key is re-used on a request that does not match the first request's API endpoint and parameters."))

(define-condition invalid-request-error (stripe-api-condition)
  ((error-type :initform "invalid_request_error"))
  (:documentation "Invalid request errors arise when your request has invalid parameters."))

(defun %determine-condition-class (type)
  (cond ((string= type "invalid_request_error")
         'invalid-request-error)
        ((string= type "card_error")
         'card-error)
        ((string= type "indempotency_error")
         'indempotency-error)
        ((string= type "api_error")
         'api-error)
        (t (error "received an error code that is unexpected."))))

(defmethod %error-key->slot-name ((parse-as (eql :plist)) key)
  (case key
    (:|type| nil)
    (:|message| nil)
    (:|code| 'code)
    (:|decline_code| 'decline_code)
    (:|param| 'param)
    (:|payment_intent| 'payment-intent)
    (:|charge| 'charge)
    (:|doc_url| 'doc-url)
    (:|payment_method| 'payment-method)
    (:|payment_method_type| 'payment-method-type)
    (:|setup_intent| 'setup-intent)
    (:|source| 'source)))

(defmethod %error-key->slot-name ((parse-as (eql :hash-table)) key)
  (let ((alist 
          '(("type" . nil)
            ("message" . nil)
            ("code" . code)
            ("decline_code" . decline_code)
            ("param" . param)
            ("payment_intent" . payment-intent)
            ("charge" . charge)
            ("doc_url" . doc-url)
            ("payment_method" . payment-method)
            ("payment_method_type" . payment-method-type)
            ("setup_intent" . setup-intent)
            ("source" . source))))
    (cdr (assoc key alist :test #'string=))))


(defgeneric %failed-request-to-condition (parse-as request)
  (:documentation "Convert a failed request to a condition using *parse-as*"))

(defmethod %failed-request-to-condition ((parse-as (eql :plist)) condition)
  "Converts the error returned by Dex into a nice stripe condition."
  (let* ((http-body (dexador.error:response-body condition))
         (parsed (jojo:parse http-body)))
    (destructuring-bind (&key |type| |message| &allow-other-keys)
        (getf parsed :|error|)
      (let ((obj (make-instance (%determine-condition-class |type|)
                                :message |message|
                                :parent-condition condition)))
        (alexandria:doplist (key val parsed)
          (let ((slot-name (%error-key->slot-name parse-as key)))
            (when slot-name
              (setf (slot-value obj slot-name) val))))
        (error obj)))))

(defmethod %failed-request-to-condition ((parse-as (eql :hash-table)) condition)
  "Converts the error returned by Dex into a nice stripe condition."
  (let* ((http-body (dexador.error:response-body condition))
         (parsed (jojo:parse http-body :as :hash-table))
         (err (gethash "error" parsed))
         (type (gethash "type" err))
         (message (gethash "message" err)))
    (let ((obj (make-instance (%determine-condition-class type)
                              :message message
                              :parent-condition condition)))
      (maphash (lambda (key val)
                 (let ((slot-name (%error-key->slot-name parse-as key)))
                   (when slot-name
                     (setf (slot-value obj slot-name) val))))
               parsed)
      (error obj))))

(defmethod print-object ((obj stripe-api-condition) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (let ((slots (c2mop:class-slots (class-of obj))))
      (format stream "~%")
      (mapc (lambda (slot)
              (let ((name (c2mop:slot-definition-name slot)))
                (when (slot-boundp obj name)
                  (format stream "~A: ~A~%" name (slot-value obj name)))))
            slots))))

(defmacro with-captured-api-failure (&body body)
  `(handler-case (locally ,@body)
     (dexador.error:http-request-failed (c)
       (%failed-request-to-condition *parse-as* c))))






