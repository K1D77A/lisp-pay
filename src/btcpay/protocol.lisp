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

(defmethod generate-dex-list append ((processor btcpay) req)
  `(:headers (("Authorization" . ,(format nil "token ~A" (api-key processor))))))

(defmethod generate-dex-list append ((processor btcpay) (req request-with-content))
  `(:content ,(write-json (content req) nil)))

(defclass btcpay-api-failure-obj (api-failure)
  ((code
    :accessor code
    :initarg :code)
   (message
    :accessor message
    :initarg :message)))

(defmethod print-object ((obj btcpay-api-failure-obj) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~%")
    (print-all-slots obj stream)))

(defmethod construct-api-failure-object ((processor btcpay)
                                         response)
  (with-accessors ((body body))
      response
    (let ((code (gethash "code" body))
          (message (gethash "message" body)))
      (make-instance 'btcpay-api-failure-obj
                     :code code
                     :message message))))





