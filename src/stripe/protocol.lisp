(in-package #:lisp-pay/stripe)

(defprocessor stripe lisp-pay-api-call
  ((api-key
    :accessor api-key
    :initarg :api-key
    :initform #.(format nil "sk_test_51Jn99bKazowTfVdknr4RjY5oqPKBrUV6B613Wj3afh~
                             pM76nl9QauaUPsWZo9nzxCalG8S1BUwSPewl9tDd2u28bN00D2DefuQi"))
   (public-key
    :accessor public-key
    :initarg :public-key
    :type (or keyword string))
   (api-version
    :accessor api-version
    :initarg :api-version
    :initform "2020-08-27")
   (base-url
    :accessor base-url
    :initarg :base-url
    :initform "https://api.stripe.com")))

(defparameter *processor*
  (make-instance 'stripe))

(defmethod generate-dex-list append ((processor stripe)
                                     (request request))
  `(:basic-auth ,(list (api-key processor))))

(defmethod generate-dex-list append ((processor stripe) (request request-with-content))
  ;;sometimes content is actually optional 
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

(defmethod construct-api-failure-object ((processor stripe)
                                         response)
  (let ((error-hash (gethash "error" (body response))))
    (with-hash-keys (|type|)
        error-hash
    (let ((obj (make-instance (%determine-error-class |type|))))
      (maphash (lambda (key val)
                 (let ((slot-name (%error-key->slot-name key)))
                   (when slot-name
                     (setf (slot-value obj slot-name) val))))
               error-hash)
      obj))))
