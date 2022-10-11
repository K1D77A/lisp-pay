(in-package #:lisp-pay)


#||
This file contains code contains a generic wrapper over responses from the various
APIs
||#

(defgeneric response-status (response)
  (:documentation "Generic means of getting the response-status.")
  (:method ((response list))
    (second response))
  (:method ((response dex:http-request-failed))
    (dex:response-status response)))

(defgeneric response-body (response)
  (:documentation "Generic means of getting the response body.")
  (:method ((response list))
    (first response))
  (:method ((response dex:http-request-failed))
    (dex:response-body response)))

(defgeneric response-headers (response)
  (:documentation "Generic means of getting the response headers.")
  (:method ((response list))
    (third response))
  (:method ((response dex:http-request-failed))
    (dex:response-headers response)))

(defgeneric response-quri (response)
  (:documentation "Generic means of getting the response quri.")
  (:method ((response list))
    (fourth response))
  (:method ((response dex:http-request-failed))
    (dex:request-uri response)))

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
    :initarg :status-code
    :documentation "HTTP Status code")
   (body
    :accessor body
    :initarg :body
    :documentation "The body of the HTTP request.")
   (dex-response
    :accessor dex-response
    :initarg :dex-response
    :documentation "The dex response object")
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
    :initarg :status-code
    :documentation "HTTP status code.")
   (body
    :accessor body
    :initarg :body
    :documentation "HTTP request body.")
   (dex-response
    :accessor dex-response
    :initarg :dex-response
    :documentation "The Dex response.")))

(defmethod print-object ((obj api-response-class) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (print-all-slots obj stream)))

(defclass information-response (api-response-class)
  ())

(defclass successful-response (api-response-class)
  ())

(defclass redirection-response (api-response-class)
  ())

(defgeneric determine-response (processor response)
  (:documentation "Generic means of determining what class we want our RESPONSE to
be. RESPONSE is an instance of dex-response. Must evaluate to the constructor
function ie #'make-instance/#'make-condition and a class."))

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
  (list :status-code status
        :body (read-json body)
        :dex-response response
        :processor processor))

(defgeneric construct-response-from-api (processor response)
  (:documentation "Means of taking a response from a dex call and converting it into 
something generic."))

(defmethod construct-response-from-api (processor response)
  (let ((status (dex-response-status-code response))
        (body (dex-response-body response)))
    (multiple-value-bind (fun class)
        (determine-response processor response)
      (signal-when-condition
       processor 
       (apply fun class
              (construct-initargs-for-response processor status body response))))))

(defgeneric signal-when-condition (processor c)
  (:documentation "Signals a condition when C is a condition.")
  (:method :before (processor (c condition))
    (setf (api-failure c) (construct-api-failure-object processor c)))
  (:method (processor (c condition))
    (error c))
  (:method (processor c)
    c))

(defgeneric construct-api-failure-object (processor api-response)
  (:documentation "Take the API-response condition and post process it into a 
nicer to read object. Specialize in order to construct processor specific failure objects")
  (:method (processor api-response)
    nil))

