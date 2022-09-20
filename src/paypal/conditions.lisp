(in-package #:lisp-pay/paypal)

(defparameter *parse-as* :plist)
(defparameter *json-encoder* #'jojo:to-json)

(define-condition paypal-condition (lisp-pay-condition)
  ())

(define-condition token-issue (paypal-condition)
  ())

(define-condition unbound-token (token-issue)
  ()
  (:report
   (lambda (obj stream)
     (declare (ignore obj))
     (format stream "You have not evaluated 'get-token'."))))

(define-condition expired-token (token-issue)
  ((token
    :accessor token
    :initarg :token))
  (:report
   (lambda (obj stream)
     (format stream "Your token has expired recall 'get-token'. ~A"
             (token obj)))))

(define-condition paypal-api-condition (paypal-condition)
  ((body
    :accessor body
    :initarg :body)
   (status-code
    :accessor status-code
    :initarg :status-code)
   (status-text
    :accessor status-text
    :initarg :status-text)))

(define-condition failed-request (paypal-api-condition)
  ())

(define-condition authorization-error (failed-request)
  ())

(define-condition server-error (failed-request)
  ())


(defmacro quick-api-condition (name (super status-code status-text))
  `(progn (define-condition ,name (,super)
            ((status-code :initform ,status-code)
             (status-text :initform ,status-text)))))

(defmacro qc (name (super status-code status-text))
  `(quick-api-condition ,name (,super ,status-code ,status-text)))

(qc four-hundred (authorization-error 400 "Bad Request"))

(qc four-hundred-one (authorization-error 401 "Bad Request"))

(qc four-hundred-three (authorization-error 403 "Forbidden"))

(qc four-hundred-four (failed-request 404 "Not Found"))

(qc four-hundred-five (failed-request 405 "Method Not Allowed"))

(qc four-hundred-six (failed-request 406 "Not Acceptable"))

(qc four-hundred-fifteen (failed-request 415 "Unsupported Media Type"))

(qc four-hundred-twenty-two (failed-request 422 "Unprocessable Entity"))

(qc four-hundred-twenty-nine (failed-request 429 "Unprocessable Entity"))

(qc five-hundred (server-error 500 "Internal Server Error"))

(qc five-hundred-three (server-error 503 "Service Unavailable"))

(defun determine-condition (n)
  (case n
    (400 'four-hundred)
    (401 'four-hundred-one)
    (403 'four-hundred-three)
    (404 'four-hundred-four)
    (405 'four-hundred-five)
    (406 'four-hundred-six)
    (415 'four-hundred-fifteen)
    (422 'four-hundred-twenty-two)
    (429 'four-hundred-twenty-nine)
    (500 'five-hundred)
    (503 'five-hundred-three)))

(defmacro wrapped-dex-call ((resp status) &body body)
  `(wrap-dex-condition 
     (multiple-value-bind (,resp ,status)
         ,@body)))

(defmacro wrap-dex-condition (&body body)
  `(handler-case
       (locally ,@body)
     (dexador:http-request-failed (c)
       (error (determine-condition (dexador.error:response-status c))
              :body (jojo:parse (dexador.error:response-body c) :as *parse-as*)))))

(defmethod print-object ((obj failed-request) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-accessors ((status-code status-code)
                     (body body)
                     (status-text status-text))
        obj 
      (format stream "Status: ~A.~%Status Text: ~A.~%Body: ~S"
              status-code status-text body))))








