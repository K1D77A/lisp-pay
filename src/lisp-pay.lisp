;;;; lisp-pay.lisp

(in-package #:lisp-pay)


(defparameter *processor* (make-instance 'processor))

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
                  (defclass* ,name (,super)
                    ,(append slots query-slots)
                    ,@(append `((:metaclass ,metaclass)
                                (:genned-slot-names ,names)
                                (:query-slot-names ,query-slot-names)
                                (:endpoint ,endpoint)))
                    (:export-class-name-p t)
                    (:export-accessor-names-p t)
                    (:export-predicate-name nil))))
            (c2mop:ensure-finalized class)
            (let* ((direct-slots (c2mop:class-direct-slots class))
                   (direct-query-slots
                     (mapcar (lambda (slot-name)
                               (find slot-name direct-slots
                                     :key #'c2mop:slot-definition-name
                                     :test #'string-equal))
                             ',query-slot-names)))
              (with-slots (string-constructor query-constructor)
                  class
                (setf (string-constructor class) (gen-url-generator class))
                (when ',query-slots
                  (setf (query-constructor class)
                        (gen-query-generator direct-query-slots
                                             ',query-slot-names))))))))))
