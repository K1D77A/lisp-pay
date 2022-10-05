(in-package #:lisp-pay/stripe)

(defprocessor stripe lisp-pay-api-call
  ((api-key
    :accessor api-key
    :initarg :api-key
    :initform #.(format nil "sk_test_51Jn99bKazowTfVdknr4RjY5oqPKBrUV6B613Wj3afh~
                             pM76nl9QauaUPsWZo9nzxCalG8S1BUwSPewl9tDd2u28bN00D2DefuQi"))
   (api-version
    :accessor api-version
    :initarg :api-version
    :initform "2020-08-27")
   (base-url
    :accessor base-url
    :initarg :base-url
    :initform "https://api.stripe.com")))

(defmethod generate-dex-list append ((processor stripe)
                                     (request request))
  `(:basic-auth ,(list (api-key processor))))

(defmethod generate-dex-list append ((processor stripe) (request request-with-content))
  `(:content ,(content request)
    :headers (("Stripe-Version" . ,(api-version processor))
              ("Content-Type" . "application/x-www-form-urlencoded"))))

(defmethod determine-base-url ((processor stripe) req)
  (base-url processor))

(defmethod determine-base-url ((processor stripe) (req post-files-request))
  "https://files.stripe.com")

(defmethod generate-url ((processor stripe) req)
  (with-accessors ((string-constructor string-constructor)
                   (query-constructor query-constructor))
      (class-of req)
    (concatenate 'string
                 (determine-base-url processor req)
                 (funcall string-constructor req))))
