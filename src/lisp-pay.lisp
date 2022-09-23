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

(defmacro defprocessor (name metaclass direct-slots &rest options)
  `(progn
     (defclass ,name (processor)
       ,(append direct-slots
         `((api-metaclass
            :initform ',metaclass)))
       ,@options)
     (defmacro defapi (name (endpoint super &key (metaclass ',metaclass))
                       &optional query-slots)
       (let* ((slots (slots-from-url endpoint))
              (names (mapcar #'first slots))
              (query-slot-names (mapcar #'first query-slots)))
         `(let ((class 
                  (defclass ,name (,super)
                    ,(append slots query-slots)
                    ,@(append `((:metaclass ,metaclass)
                                (:genned-slot-names ,names)
                                (:query-slot-names ,query-slot-names)
                                (:endpoint ,endpoint))))))
            (c2mop:ensure-finalized class)
            (with-slots (string-constructor headers
                         query-constructor query-slot-names)
                class
              (setf (string-constructor class) (gen-url-generator class))
              (when ',query-slots
                (setf (query-constructor class)
                      (gen-query-generator class query-slot-names)))))))))
