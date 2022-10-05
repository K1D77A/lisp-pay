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

(defclass paypal-api-condition (api-failure)
  ((status-text
    :accessor status-text
    :initarg :status-text)
   (message
    :accessor message
    :initarg :message
    :documentation "Message key from the response")
   (name
    :accessor name
    :initarg :name)))

(defclass failed-request (paypal-api-condition)
  ())

(defclass authorization-error (failed-request)
  ())

(defclass server-error (failed-request)
  ())


(defmacro quick-api-condition (name (super status-text))
  `(progn (defclass ,name (,super)
            ((status-text :initform ,status-text)))))

(defmacro qc (name (super status-text))
  `(quick-api-condition ,name (,super ,status-text)))

(qc four-hundred (authorization-error "Bad Request"))

(qc four-hundred-one (authorization-error "Bad Request"))

(qc four-hundred-three (authorization-error "Forbidden"))

(qc four-hundred-four (failed-request "Not Found"))

(qc four-hundred-five (failed-request "Method Not Allowed"))

(qc four-hundred-six (failed-request  "Not Acceptable"))

(qc four-hundred-fifteen (failed-request "Unsupported Media Type"))

(qc four-hundred-twenty-two (failed-request  "Unprocessable Entity"))

(qc four-hundred-twenty-nine (failed-request "Unprocessable Entity"))

(qc five-hundred (server-error "Internal Server Error"))

(qc five-hundred-three (server-error "Service Unavailable"))

(defun determine-failure-class (n)
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

(defmethod print-object ((obj failed-request) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (print-all-slots obj stream)))
    





