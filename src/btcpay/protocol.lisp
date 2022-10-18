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
  `(:headers (("Authorization" . ,(format nil "token ~A" (api-key processor)))
              ("Content-Type" . ,(content-type req)))))

(defmethod generate-dex-list append ((processor btcpay) (req request-with-content))
  `(:content ,(write-json (content req) nil)))

(defclass btcpay-api-failure-obj (api-failure)
  ())

(defclass btcpay-api-failure-obj-single (btcpay-api-failure-obj)
  ((code
    :accessor code
    :initarg :code)
   (message
    :accessor message
    :initarg :message)))

(defclass btcpay-api-failure-obj-list-entry (btcpay-api-failure-obj)
  ((path
    :accessor path
    :initarg :path)
   (message
    :accessor message
    :initarg :message)))

(defclass btcpay-api-failure-obj-list (btcpay-api-failure-obj)
  ((issues
    :accessor issues
    :initarg :issues
    :initform () 
    :type list)))

(defmethod construct-api-failure-object ((processor btcpay)
                                         response)
  (with-accessors ((status-code status-code)
                   (body body))
      response
    (case status-code
      (422 (map 'list (lambda (hashes)
                        (with-hash-keys (|path| |message|)
                            hashes
                          (make-instance 'btcpay-api-failure-obj-list-entry
                                         :message |message|
                                         :path |path|)))
                body))
      (otherwise 
       (with-hash-keys (|code| |message|)
           (body response)
         (make-instance 'btcpay-api-failure-obj
                        :code |code|
                        :message |message|))))))
