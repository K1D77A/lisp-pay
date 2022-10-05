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

(defun get-token (processor &optional (ignore-checks nil))
  (with-accessors ((token token)
                   (client client)
                   (secret secret))
      processor 
    (if (or ignore-checks 
            (or (not (slot-boundp processor 'token))
                (null token)
                (expiredp token)))
        (let ((res (construct-response-from-api
                    processor
                    (wrap-dex-call 
                      (dex:post (format nil "~A/v1/oauth2/token"
                                        (base-url processor))
                                :basic-auth `(,client . ,secret)
                                :headers '(("Accept" . "application/json")
                                           ("Accept-Language" . "en_US"))
                                :content '(("grant_type" . "client_credentials")))))))
          (setf token (parse-token (body res)))))))

(defmethod expiredp (token)
  (error 'unbound-token))

(defmethod expiredp ((token token))
  "Checks to see if the token has expired."
  (with-accessors ((expires-in expires-in))
      token 
    (let* ((now (local-time:now)))
      (local-time:timestamp<= expires-in now))))

(defun is-token-non-nil (processor)
  (unless (token processor)
    (error 'unbound-token)))

(defmethod is-expired-token (token)
  (error 'unbound-token))

(defmethod is-expired-token ((token token))
  (when (expiredp token)
    (error 'expired-token :token token)))

(defmethod is-token-bound (processor)
  (unless (slot-boundp processor 'token)
    (error 'unbound-token)))
