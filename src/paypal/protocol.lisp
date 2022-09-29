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

(defmethod generate-dex-list append ((processor paypal) req)
  (declare (special *request-headers*))
  (is-token-non-nil processor)
  (is-token-bound processor)
  (is-expired-token (token processor))
  `(:headers (append (("Content-Type" . (content-type req))
                      ("Authorization" . (format nil "Bearer ~A" (token processor))))
                     (when (boundp '*special-headers*)
                       *special-headers*))))

(defmethod generate-dex-list append ((processor paypal) (req request-with-content))
  `(:content (write-json ,(content req) nil)))

(defmethod %call-api :around ((processor paypal) request)
  (restart-case
      (call-next-method)
    (missing-token ()
      :report "Token could be broken, refresh and try again?"
      (get-token)
      (call-next-method))))

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
