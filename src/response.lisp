(in-package #:lisp-pay)


#||
This file contains code contains a generic wrapper over responses from the various
APIs
||#

(define-condition api-response-condition (lisp-pay-condition)
  ((status-code
    :accessor status-code
    :initarg :status-code)
   (body
    :accessor body
    :initarg :body)
   (dex-response
    :accessor dex-response
    :initarg :dex-response)
   (api-failure
    :accessor api-failure
    :initarg :api-failure
    :initform nil
    :documentation "A per API failure object")))

(define-condition client-error-response (api-response-condition)
  ())

(define-condition server-error-response (api-response-condition)
  ())

(define-condition unknown-server-response (api-response-condition)
  ())

(defclass api-response-class ()
  ((status-code
    :accessor status-code
    :initarg :status-code)
   (body
    :accessor body
    :initarg :body)
   (dex-response
    :accessor dex-response
    :initarg :dex-response)))

(defclass information-response (api-response-class)
  ())

(defclass successful-response (api-response-class)
  ())

(defclass redirection-response (api-response-class)
  ())


(defmethod determine-response (processor n)
  (cond ((<= 100 n 199)
         (values #'make-instance  'information-response))
        ((<= 200 n 299)
         (values #'make-instance  'successful-response))
        ((<= 300 n 399)
         (values #'make-instance  'redirection-response))
        ((<= 400 n 499)
         (values #'make-condition 'client-error-response))
        ((<= 500 n 599)
         (values #'make-condition 'server-error-response))
        (t
         (values #'make-condition 'unknown-server-response))))

(defmethod response-status ((response list))
  (second response))

(defmethod response-status ((response dex:http-request-failed))
  (dexador:response-status response))

(defmethod response-body ((response list))
  (first response))

(defmethod response-body ((response dex:http-request-failed))
  (dexador:response-body response))

(defmethod construct-response-from-api (processor response)
  (let ((status (response-status response))
        (body (response-body response)))
    (multiple-value-bind (fun class)
        (determine-response processor status)
      (signal-when-condition
       processor 
       (apply fun class (list :status-code status
                              :body (read-json body)
                              :dex-response response))))))


(defgeneric signal-when-condition (processor c))

(defmethod signal-when-condition (processor (c condition))
  (setf (api-failure c) (construct-api-failure-object processor c))
  (error c))

(defmethod signal-when-condition (processor c)
  c)

(defmacro wrap-dex-call (&body body)
  `(handler-case
       (multiple-value-list (locally ,@body))
     (dexador:http-request-failed (c)
       c)))

(defgeneric construct-api-failure-object (processor api-response)
  (:documentation "Take the API-response condition and post process it into a 
nicer to read object."))

(defmethod construct-api-failure-object (processor api-response)
  nil)


