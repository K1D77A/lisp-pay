(in-package #:lisp-pay/paypal)


(defclass token ()
  ((nonce
    :reader nonce
    :initarg :nonce
    :type string)
   (expires-in
    :reader expires-in
    :initarg :expires-in
    :type local-time:timestamp)
   (app-id
    :reader app-id
    :initarg :app-id
    :type (or null string))
   (token-type
    :reader token-type
    :initarg :token-type
    :type string)
   (access-token
    :reader access-token
    :initarg :access-token
    :type string)
   (scope
    :reader scope
    :initarg :scope
    :type string)))

(defun parse-token (token-hash)
  (with-hash-keys (|nonce| |expires_in| |app_id| |token_type| |access_token| |scope|)
    token-hash 
    (make-instance 'token :scope |scope| :access-token |access_token|
                          :nonce |nonce| :expires-in
                          (local-time:timestamp+ (local-time:now) |expires_in| :sec)
                          :app-id |app_id| :token-type |token_type|)))

(defun get-token (&optional (ignore-checks nil))
  (if (or ignore-checks 
          (or (not (boundp '*token*))
              (null *token*)
              (expiredp *token*)))
      (wrapped-dex-call
       (resp status)
       (dex:post (format nil "~A/v1/oauth2/token"
                         (generate-url t))
                 :basic-auth `(,*client* . ,*secret*)
                 :headers '(("Accept" . "application/json")
                            ("Accept-Language" . "en_US"))
                 :content '(("grant_type" . "client_credentials")))
       (let ((token (parse-token (read-json resp))))
         (values
          (setf *token* token)
          (make-instance (determine-good-class status) :body (list token)))))
      *token*))

(defmethod expiredp (token)
  (error 'unbound-token))

(defmethod expiredp ((token token))
  "Checks to see if the token has expired."
  (with-accessors ((expires-in expires-in))
      token 
    (let* ((now (local-time:now)))
      (local-time:timestamp<= expires-in now))))

(defun is-token-non-nil ()
  (unless *token*
    (error 'unbound-token)))

(defmethod is-expired-token (token)
  (error 'unbound-token))

(defmethod is-expired-token ((token token))
  (when (expiredp token)
    (error 'expired-token :token token)))

(defmethod is-token-bound ()
  (unless (boundp '*token*)
    (error 'unbound-token)))
