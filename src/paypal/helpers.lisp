(in-package #:lisp-pay/paypal)

(defun %quick-hash (alist &rest rest &key &allow-other-keys)
  "Takes in an alist and quickly generators a hash"
  (let ((hashtable (apply #'make-hash-table rest)))
    (mapc (lambda (alist)
            (destructuring-bind (a . b)
                                alist
                                (unless (eq b :ne)
                                  (setf (gethash a hashtable) b))))
          alist)
    hashtable))

(defmacro with-hash-keys (keys hash &body body)
  "Creates a let binding for each of the keys listed in KEYS in HASH using gethash, 
each of these KEYS has to have a non nil value otherwise signals 'malformed-json."
  (alexandria:once-only (hash)
    `(let ,(mapcar (lambda (key)
                     `(,key (gethash ,(string key) ,hash)))
            keys)
       (locally ,@body))))
