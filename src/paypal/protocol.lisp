(in-package #:lisp-pay/paypal)

(defclass paypal-api-slot (lisp-pay-api-slot)
  ((name
    :accessor name
    :initarg :name)
   (name->json
    :accessor name->json 
    :initarg :name->json
    :initform nil)))

(defclass paypal-api-call (lisp-pay-api-call)
  ())

(defclass paypal-api-slot-direct (paypal-api-slot c2mop:standard-direct-slot-definition)
  ())

(defclass paypal-api-slot-effective (paypal-api-slot
                                     c2mop:standard-effective-slot-definition)
  ())

(defmethod c2mop:validate-superclass ((class paypal-api-call)
                                      (metaclass c2mop:funcallable-standard-class))
  t)

(defmethod c2mop:validate-superclass ((class paypal-api-slot)
                                      (metaclass standard-class))
  t)

(defmethod c2mop:effective-slot-definition-class ((class paypal-api-call) &rest initargs)
  (find-class 'paypal-api-slot-effective))

(defmethod c2mop:direct-slot-definition-class ((class paypal-api-call) &rest initargs)
  (find-class 'paypal-api-slot-direct))

;; (defclass paypal-request (request)
;;   ((content-type
;;     :reader content-type
;;     :initarg :content-type
;;     :initform "application/json"))
;;   (:metaclass paypal-api-call))

;; (defclass request-without-content (paypal-request)
;;   ()
;;   (:metaclass paypal-api-call))

;; (defclass get-r (request-without-content)
;;   ((request-fun :initform 'dex:get))
;;   (:metaclass paypal-api-call))

;; (defclass query-req (request-without-content)
;;   ()
;;   (:metaclass paypal-api-call))

;; (defclass get-r-query (query-req)
;;   ()
;;   (:metaclass paypal-api-call))

;; (defclass delete-r  (request-without-content)
;;   ((request-fun :initform 'dex:delete))
;;   (:metaclass paypal-api-call))

;; (defclass request-with-content (paypal-request)
;;   ((content
;;     :accessor content
;;     :initarg :content
;;     :type (or hash-table list)))
;;   (:metaclass paypal-api-call))

;; (defclass patch-r (paypal-request)
;;   ((request-fun :initform 'dex:patch)
;;    (patch-request
;;     :accessor patch-request
;;     :initarg :patch-request))
;;   (:metaclass paypal-api-call))

;; (defclass post-r (request-with-content)
;;   ((request-fun :initform 'dex:post))
;;   (:metaclass paypal-api-call))


;; (defclass query-req-content (request-with-content)
;;   ()
;;   (:metaclass paypal-api-call))

;; (defclass post-query-r (query-req-content post-r)
;;   ()
;;   (:metaclass paypal-api-call))

;; (defclass post-files-request (post-r)
;;   ((content-type
;;     :initform "multipart/related"))
;;   (:metaclass ))

;; (defclass put-r (request-with-content)
;;   ((request-fun :initform 'dex:put))
;;   (:metaclass paypal-api-call))

;; (defclass put-query-r (query-req-content put-r)
;;   ()
;;   (:metaclass paypal-api-call))

(defmethod generate-url (req)
  (with-accessors ((string-constructor string-constructor)
                   (query-constructor query-constructor))
      (class-of req)
    (concatenate 'string
                 (if *testing*
                     "https://api-m.sandbox.paypal.com"
                     "https://api-m.paypal.com")
                 (funcall string-constructor req)
                 (when query-constructor 
                   (funcall query-constructor req)))))

(defgeneric generate-headers (req)
  (:method-combination append :most-specific-last))

(defmethod generate-headers :around (req)
  (is-token-non-nil)
  (is-token-bound)
  (is-expired-token *token*)
  `(:headers ,(call-next-method)))

(defmethod content-type (req)
  "application/json")

(defmethod content-type ((req request))
  (slot-value req 'content-type))

(defmethod generate-headers append (req)
  `(("Content-Type" . ,(content-type req))
    ("Authorization" . ,(format nil "Bearer ~A" (access-token *token*)))))

(defmethod generate-headers append ((req request))
  (declare (special *request-headers*))
  (when (boundp '*request-headers*)
    *request-headers*))

(defmethod generate-content (req)
  nil)

(defmethod generate-content ((req request-with-content))
  (list :content (funcall *json-encoder* (slot-value req 'content))))

(defmethod generate-content ((req patch-request))
  (list :content (funcall *json-encoder* (slot-value req 'patch-request))))

(defmethod generate-content ((req post-files-request))
  `(:content ,(slot-value req 'content)))

(defmethod generate-dex-list (req)
  (append (generate-headers req) (generate-content req)))

(defmacro defapi (name (endpoint super) &optional query-slots)
  (let* ((slots (slots-from-url endpoint))
         (names (mapcar #'first slots))
         (query-slot-names (mapcar #'first query-slots)))
    `(let ((class 
             (defclass ,name (,super)
               ,(append slots query-slots)
               ,@(append `((:metaclass paypal-api-call)
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

(defgeneric call-api (req))

(defmethod call-api (req)
  (flet ((body (req)
           (let ((url (generate-url req))
                 (args (generate-dex-list req))
                 (fun (request-fun req)))
             (wrapped-dex-call (resp status)
               (apply fun url args)
               (make-instance (determine-good-class status)
                              :body (typecase resp
                                      (simple-vector nil)
                                      (simple-string (jojo:parse resp :as *parse-as*))))))))
    (restart-case 
        (body req)
      (missing-token ()
        :report "Token could be broken, refresh and try again?"
        (get-token)
        (body req)))))

(defmethod print-object ((obj request) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A ~A~A"
            (request-fun obj)
            (generate-url t)
            (in-list (endpoint (class-of obj))))))
