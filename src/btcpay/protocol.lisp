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

(defmethod generate-url ((processor btcpay) req)
  (with-accessors ((string-constructor string-constructor)
                   (query-constructor query-constructor))
      (class-of req)
    (concatenate 'string
                 (base-url processor)
                 (funcall string-constructor req)
                 (when query-constructor
                   (funcall query-constructor req)))))


(defmethod generate-dex-list ((processor btcpay) req)
  `(:headers (("Authorization" . ,(format nil "token ~A" (api-key processor))))))

(defclass btcpay-api-failure-obj ()
  ((code
    :accessor code
    :initarg :code)
   (message
    :accessor message
    :initarg :message)))

(defmethod construct-api-failure-object ((processor btcpay)
                                         response)
  (with-accessors ((body body))
      response
    (let ((code (gethash "code" body))
          (message (gethash "message" body)))
      (make-instance 'btcpay-api-failure-obj
                     :code code
                     :message message))))

(defapi webhooks%all ("/api/v1/stores/:store-id/webhooks" get-request)
        ())



