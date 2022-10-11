(in-package #:lisp-pay)

(define-condition lisp-pay-condition (error)
  ((processor
    :accessor processor
    :initarg :processor
    :documentation "The current value of *processor*")))
