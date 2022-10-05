(in-package #:lisp-pay)


#||
This file contains code contains a generic wrapper over responses from the various
APIs
||#
(defmethod response-status ((response list))
  (second response))

(defmethod response-status ((response dex:http-request-failed))
  (dexador:response-status response))

(defmethod response-body ((response list))
  (first response))

(defmethod response-body ((response dex:http-request-failed))
  (dexador:response-body response))

(defmethod response-headers ((response dex:http-request-failed))
  (dexador:response-headers response))

(defmethod response-headers ((response list))
  (third response))

(defmethod response-quri ((response dex:http-request-failed))
  (dexador:request-uri response))

(defmethod response-quri ((response list))
  (fourth response))

(defstruct dex-response
  (body)
  (status-code)
  (headers)
  (quri))

(defun new-dex-response (response)
  (make-dex-response :body (response-body response)
                     :status-code (response-status response)
                     :headers (response-headers response)
                     :quri (response-quri response)))

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

(defmethod print-object ((obj api-response-condition) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (print-all-slots obj stream)))


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

(defmethod print-object ((obj api-response-class) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (print-all-slots obj stream)))

(defclass information-response (api-response-class)
  ())

(defclass successful-response (api-response-class)
  ())

(defclass redirection-response (api-response-class)
  ())

(defmethod determine-response (processor response)
  (let ((n (dex-response-status-code response)))
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
           (values #'make-condition 'unknown-server-response)))))

(defgeneric construct-initargs-for-response (processor status body response)
  (:method-combination append :most-specific-last)
  (:documentation "Construct the default initargs for a response object. 
This can be specialized for a more advanced response object like those provided by Stripe"))

(defmethod construct-initargs-for-response append ((processor processor)
                                                   status body response)
  (list :status-code status :body (read-json body) :dex-response response))

(defmethod construct-response-from-api (processor response)
  (let ((status (dex-response-status-code response))
        (body (dex-response-body response)))
    (multiple-value-bind (fun class)
        (determine-response processor response)
      (signal-when-condition
       processor 
       (apply fun class
              (construct-initargs-for-response processor status body response))))))

(defgeneric signal-when-condition (processor c))

(defmethod signal-when-condition :before (processor (c condition))
  (setf (api-failure c) (construct-api-failure-object processor c)))

(defmethod signal-when-condition (processor (c condition))
  (error c))

(defmethod signal-when-condition (processor c)
  c)

(defgeneric construct-api-failure-object (processor api-response)
  (:documentation "Take the API-response condition and post process it into a 
nicer to read object."))

(defmethod construct-api-failure-object (processor api-response)
  nil)


