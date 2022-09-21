(in-package #:lisp-pay/stripe)

(defmacro defapi (name (endpoint super) &optional query-slots)
  (let* ((slots (slots-from-url endpoint))
         (names (mapcar #'first slots))
         (query-slot-names (mapcar #'first query-slots)))
    `(let ((class 
             (defclass ,name (,super)
               ,(append slots query-slots)
               ,@(append `((:metaclass lisp-pay-api-call)
                           (:genned-slot-names ,names)
                           (:query-slot-names ,query-slot-names)
                           (:endpoint ,endpoint))))))
       (c2mop:ensure-finalized class)
       (with-slots (string-constructor headers
                    query-constructor query-slot-names)
           class
         (setf (string-constructor class) (gen-url-generator class))
         (when ',query-slots
           (setf (query-constructor class)
                 (gen-query-generator class query-slot-names)))))))

(defmacro defapi%get (name (endpoint))
  `(defapi ,name (,endpoint get-request)))

(defmacro defapi%delete (name (endpoint))
  `(defapi ,name (,endpoint delete-request)))

(defgeneric form-dex-args (request)
  (:method-combination append :most-specific-last))

(defmethod form-dex-args append ((request request))
  `(:basic-auth ,(list *api-key*) :headers ,(list `("Stripe-Version" . ,*api-version*))))

(defmethod form-dex-args append ((request request-without-content))
  nil)

(defmethod form-dex-args append ((request request-with-content))
  (with-slots (content)
      request 
    `(:content ,content :headers (("Content-Type" . "application/x-www-form-urlencoded")))))

(defmethod call-api (req)
  (with-slots (url request-fun)
      req 
    (let ((complete-url (generate-url req))
          (args (form-dex-args req)))
      (with-captured-api-failure
        (jojo:parse (apply request-fun complete-url args) :as *parse-as*)))))

(defmethod determine-base-url (req)
  *url*)

(defmethod determine-base-url ((req post-files-request))
  "https://files.stripe.com")

(defmethod generate-url ((req request))
  (with-accessors ((string-constructor string-constructor))
      (class-of req)
    (format nil "~A~A" (determine-base-url req)
            (funcall (in-list string-constructor) req))))
