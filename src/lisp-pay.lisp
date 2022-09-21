;;;; lisp-pay.lisp

(in-package #:lisp-pay)

(defclass processor ()
  ((base-url
    :accessor base-url
    :initarg :base-url)
   (api-metaclass
    :accessor api-metaclass)))

(defclass testing-processor (processor)
  ())

