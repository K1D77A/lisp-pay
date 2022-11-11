(in-package #:lisp-pay/paypal)

(defprocessor paypal lisp-pay-api-call 
  ((base-url 
    :initform "https://api-m.paypal.com")
   (token
    :accessor token 
    :initarg :token
    :initform nil
    :type (or token null)
    :documentation "Paypal token, this is used for every api call except receiving the token itself.")
   (secret-id 
    :accessor secret-id 
    :initarg :secret-id 
    :initform
    "EMBuo5-J3kWfSEJYY5mtQd8Hm9JezbxjkUUJ2D9JwKwwas1E05Ejp4A1wlpNuuFd3YyIoKZrSxjs9OUb"
    :type string)
   (client-id 
    :accessor client-id 
    :initarg :client-id
    :initform
    "ATiiZbWBH3_qd_y3P3AZQiQlBIh9mVTDSTtr4ALOPqfTd5eBZooqeJlLT0o6-HLF95_Vj2GADaIhp5Ee")))

(class*:defclass* paypal-testing (paypal)
  ((base-url
    :initform "https://api-m.sandbox.paypal.com"))
  (:export-class-name-p t))

(defvar *processor*
  (make-instance 'paypal-testing))

(defmethod generate-dex-list append ((processor paypal) req)
  (declare (special *request-headers*))
  (check-token-bound processor)
  (check-token-non-nil processor)
  (check-expired-token (token processor))
  `(:headers ,(append `(("Content-Type" . ,(content-type req))
                        ("Authorization" . ,(format nil "Bearer ~A" (access-token
                                                                     (token processor)))))
                      (when (boundp '*request-headers*)
                        *request-headers*))))

(defmethod generate-dex-list append ((processor paypal) (req request-with-content))
  `(:content ,(write-json (content req) nil)))

(defmethod %call-api :around ((processor paypal) request)
  (restart-case
      (call-next-method)
    (missing-or-expired-token ()
      :report "Token could be broken, refresh and try again?"
      (get-token processor)
      (call-next-method))))

(defmethod construct-api-failure-object ((processor paypal)
                                         response)
  (with-accessors ((body body)
                   (status-code status-code))
      response
    (let ((class (determine-failure-class status-code)))
      (with-hash-keys (|name| |message|)
          body 
        (make-instance class 
                       :name |name|
                       :message |message|)))))
