(in-package #:lisp-pay/btcpay)

(defparameter *test-store*
  "FD33ULa49mBcCzdgpsgVU82V6WZD7TV5uqLYPgYq6Ra4")

(defprocessor btcpay lisp-pay-api-call 
  ((api-key
    :accessor api-key
    :initform "3e88ac02bba3af5c053af8260008c6cc890e3b74"
    :type string)
   (base-url 
    :initform "https://btcpay.test.btcbank.li")))


(defparameter *processor*
  (make-instance 'btcpay))

(defgeneric generate-headers (a b)
  (:method-combination append :most-specific-last))

(defmethod generate-url (processor req)
  (with-accessors ((string-constructor string-constructor)
                   (query-constructor query-constructor))
      (class-of req)
    (concatenate 'string
                 (base-url processor)
                 (funcall string-constructor req)
                 (when query-constructor
                   (funcall query-constructor req)))))


(defmethod generate-headers append ((processor btcpay) req)
  `(("Authorization" . ,(format nil "token ~A" (api-key processor)))))

(defapi webhooks%all ("/api/v1/stores/:store-id/webhooks" get-request)
        ())
