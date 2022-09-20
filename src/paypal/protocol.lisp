(in-package #:lisp-pay/paypal)

(defclass api-slot (c2mop:slot-definition)
  ((name
    :accessor name
    :initarg :name)
   (name->json
    :accessor name->json 
    :initarg :name->json
    :initform nil)))

(defclass api-call (c2mop:funcallable-standard-class)
  ((endpoint
    :accessor endpoint
    :initarg :endpoint
    :documentation "A URL with :<slot-name> within where the slot value is encoded.")
   (string-constructor
    :accessor string-constructor
    :initarg :string-constructor
    :documentation "A function that returns a generated URL.")
   (genned-slot-names
    :accessor genned-slot-names
    :initarg :genned-slot-names)
   (query-slot-names
    :accessor query-slot-names
    :initarg :query-slot-names)
   (query-constructor
    :accessor query-constructor
    :initarg :query-constructor)))


(defclass api-slot-direct (api-slot c2mop:standard-direct-slot-definition)
  ())

(defclass api-slot-effective (api-slot c2mop:standard-effective-slot-definition)
  ())

(defmethod c2mop:validate-superclass ((class api-call)
                                      (metaclass c2mop:funcallable-standard-class))
  t)

(defmethod c2mop:validate-superclass ((class api-slot)
                                      (metaclass standard-class))
  t)

(defmethod c2mop:effective-slot-definition-class ((class api-call) &rest initargs)
  (find-class 'api-slot-effective))

(defmethod c2mop:direct-slot-definition-class ((class api-call) &rest initargs)
  (find-class 'api-slot-direct))

(defclass request ()
  ((content-type
    :reader content-type
    :initarg :content-type
    :initform "application/json")
   (request-fun
    :reader request-fun
    :initarg :request-fun
    :initform 'dex:get))
  (:metaclass api-call))

(defclass request-without-content (request)
  ()
  (:metaclass api-call))

(defclass get-r (request-without-content)
  ((request-fun :initform 'dex:get))
  (:metaclass api-call))

(defclass query-req (request-without-content)
  ()
  (:metaclass api-call))

(defclass get-r-query (query-req)
  ()
  (:metaclass api-call))

(defclass delete-r  (request-without-content)
  ((request-fun :initform 'dex:delete))
  (:metaclass api-call))

(defclass request-with-content (request)
  ((content
    :accessor content
    :initarg :content
    :type (or hash-table list)))
  (:metaclass api-call))

(defclass patch-r (request)
  ((request-fun :initform 'dex:patch)
   (patch-request
    :accessor patch-request
    :initarg :patch-request))
  (:metaclass api-call))

(defclass post-r (request-with-content)
  ((request-fun :initform 'dex:post))
  (:metaclass api-call))



(defclass query-req-content (request-with-content)
  ()
  (:metaclass api-call))

(defclass post-query-r (query-req-content post-r)
  ()
  (:metaclass api-call))

(defclass post-files-r (post-r)
  ((content-type
    :initform "multipart/related"))
  (:metaclass api-call))

(defclass put-r (request-with-content)
  ((request-fun :initform 'dex:put))
  (:metaclass api-call))

(defclass put-query-r (query-req-content put-r)
  ()
  (:metaclass api-call))

(defun in-list (obj)
  (if (listp obj)
      (first obj)
      obj))

(defun replace-vars-for-slot-names (split slots)
  (mapcar (lambda (str)
            (let ((found?
                    (find (subseq str 1) slots :test #'string-equal)))
              (if found?
                  found?
                  str)))
          split))

(defun gen-url-generator (class)
  (with-accessors ((endpoint endpoint)
                   (genned-slot-names genned-slot-names))
      class 
    (let* ((split (str:split #\/ (in-list endpoint) :omit-nulls t))
           (slots (in-list (genned-slot-names class)))
           (compared (replace-vars-for-slot-names split slots)))
      (if slots 
          (compile nil
                   `(lambda (request)
                      (format nil "/~{~A~^/~}"
                              (loop :for slot? :in ',compared
                                    :collect
                                    (if (stringp slot?)
                                        slot? 
                                        (quri:url-encode 
                                         (slot-value request slot?)))))))
          (lambda (request)
            (declare (ignore request))
            (in-list endpoint))))))

(defun gen-query-generator (class)
  (with-accessors ((query-slot-names query-slot-names))
      class 
    (let* ((slots (in-list query-slot-names)))
      (if slots 
          (compile nil
                   `(lambda (request)
                      (let ((str
                              (format nil "?~{~A~^&~}"
                                      (loop :for slot :in ',slots
                                            :if (slot-boundp request slot)
                                              :collect
                                              (format nil "~A=~A"
                                                      (string-downcase (symbol-name slot))
                                                      (quri:url-encode
                                                       (slot-value request slot)))))))
                        (if (string= str "?")
                            ""
                            str))))
          (lambda (req)
            (declare (ignore req))
            "")))))

(defun slots-from-url (url)
  (let* ((split (str:split #\/ url :omit-nulls t))
         (slots (remove-if-not (lambda (ele) (char= #\: (aref ele 0))) split)))
    (mapcar (lambda (slot)
              (let* ((name (subseq slot 1))
                     (upcase (string-upcase name))
                     (intern (intern upcase))
                     (key (intern upcase :keyword)))
                (list intern :accessor intern :initarg key)))
            slots)))

(defmacro defapi (name (endpoint super) query-slots)
  (let* ((slots (slots-from-url endpoint))
         (names (mapcar #'first slots))
         (query-slot-names (mapcar #'first query-slots)))
    `(let ((class 
             (defclass ,name (,super)
               ,(append slots query-slots)
               ,@(append `((:metaclass api-call)
                           (:genned-slot-names ,names)
                           (:query-slot-names ,query-slot-names)
                           (:endpoint ,endpoint))))))
       (c2mop:ensure-finalized class)
       (with-slots (string-constructor headers query-constructor)
           class
         (setf (string-constructor class) (gen-url-generator class))
         (when ',query-slots
           (setf (query-constructor class) (gen-query-generator class)))))))

(c2mop:define-method-combination string-gen (&optional (order ':most-specific-last))
  ((around (:around))
   (primary (string-gen)))
  (case order
    (:most-specific-first)
    (:most-specific-last (setq primary (reverse primary))))
  (let ((form (if (rest primary)
                  `(concatenate 'string ,@(mapcar #'(lambda (method)
                                                      `(call-method ,method))
                                                  primary))
                  `(call-method ,(first primary)))))

    (if around
        `(call-method ,(first around)
                      (,@(rest around))
                      (make-method ,form))
        form)))

(defgeneric generate-url (req)
  (:method-combination string-gen :most-specific-last))

(defmethod generate-url string-gen (req)
  (if *testing*
      "https://api-m.sandbox.paypal.com"
      "https://api-m.paypal.com"))

(defmethod generate-url string-gen ((req request))
  (funcall (in-list (string-constructor (class-of req))) req))

(defmethod generate-url string-gen ((req query-req))
  (funcall (in-list (query-constructor (class-of req))) req))

(defmethod generate-url string-gen ((req query-req-content))
  (funcall (in-list (query-constructor (class-of req))) req))

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


(defmethod generate-content ((req patch-r))
  (list :content (funcall *json-encoder* (slot-value req 'patch-request))))

(defmethod generate-content ((req post-files-r))
  `(:content ,(slot-value req 'content)))

(defmethod generate-dex-list (req)
  (append (generate-headers req) (generate-content req)))

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
