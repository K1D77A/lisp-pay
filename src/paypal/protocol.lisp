(in-package #:lisp-pay/paypal)

(defprocessor paypal lisp-pay-api-call 
  ((api-key
    :accessor api-key
    :initform 
    :type string)
   (base-url 
    :initform "https://api-m.paypal.com")
   (token
    :accessor token 
    :initarg :token
    :type token)
   (secret
    :accessor secret
    :initarg :secret
    :initform
    "EMBuo5-J3kWfSEJYY5mtQd8Hm9JezbxjkUUJ2D9JwKwwas1E05Ejp4A1wlpNuuFd3YyIoKZrSxjs9OUb"
    :type string)
   (client
    :accessor client
    :initarg :client
    :initform
    "ATiiZbWBH3_qd_y3P3AZQiQlBIh9mVTDSTtr4ALOPqfTd5eBZooqeJlLT0o6-HLF95_Vj2GADaIhp5Ee")))

(defclass paypal-testing (paypal)
  ((base-url
    :initform "https://api-m.sandbox.paypal.com")))

(defvar *processor*
  (make-instance 'paypal-testing))

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

;; (defmethod call-api (req)
;;   (flet ((body (req)
;;            (let ((url (generate-url req))
;;                  (args (generate-dex-list req))
;;                  (fun (request-fun req)))
;;              (wrapped-dex-call (resp status)
;;                (apply fun url args)
;;                (make-instance (determine-good-class status)
;;                               :body (typecase resp
;;                                       (simple-vector nil)
;;                                       (simple-string (jojo:parse resp :as *parse-as*))))))))
;;     (restart-case 
;;         (body req)
;;       (missing-token ()
;;         :report "Token could be broken, refresh and try again?"
;;         (get-token)
;;         (body req)))))

;; (defmethod %call-api :around ((processor paypal) req)
;;   (restart-case
;;       (call-next-method)
;;     (missing-token ()
;;       :report "Token could be broken, refresh and try again?"
;;       (get-token)
;;       (call-next-method))))

(defmethod print-object ((obj request) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A ~A~A"
            (request-fun obj)
            (generate-url t)
            (in-list (endpoint (class-of obj))))))
