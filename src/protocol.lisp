(in-package #:lisp-pay)

#||
Many helpers for defining MOP protocols for API wrappers.
||#

(defun replace-vars-for-slot-names (split slots)
  (mapcar (lambda (str)
            (let ((found?
                    (find (subseq str 1) slots :test #'string-equal)))
              (if found?
                  found?
                  str)))
          split))

(defun gen-url-generator (class)
  (with-accessors ((endpoint endpoint)
                   (genned-slot-names genned-slot-names))
      class 
    (let* ((split (str:split #\/ (in-list endpoint) :omit-nulls t))
           (slots (in-list (genned-slot-names class)))
           (compared (replace-vars-for-slot-names split slots)))
      (if slots 
          (compile nil
                   `(lambda (request)
                      (format nil "/~{~A~^/~}"
                              (loop :for slot? :in ',compared
                                    :collect
                                    (if (stringp slot?)
                                        slot? 
                                        (quri:url-encode 
                                         (slot-value request slot?)))))))
          (lambda (request)
            (declare (ignore request))
            (in-list endpoint))))))

(defun gen-query-generator (class query-slot-names)
  (let* ((slots (in-list query-slot-names)))
    (if slots 
        (compile nil
                 `(lambda (request)
                    (let ((str
                            (format nil "?~{~A~^&~}"
                                    (loop :for slot :in ',slots
                                          :if (slot-boundp request slot)
                                            :collect
                                            (format nil "~A=~A"
                                                    (string-downcase (symbol-name slot))
                                                    (quri:url-encode
                                                     (slot-value request slot)))))))
                      (if (string= str "?")
                          ""
                          str))))
        (lambda (req)
          (declare (ignore req))
          ""))))

(defun slots-from-url (url)
  (let* ((split (str:split #\/ url :omit-nulls t))
         (slots (remove-if-not (lambda (ele) (char= #\: (aref ele 0))) split)))
    (mapcar (lambda (slot)
              (let* ((name (subseq slot 1))
                     (upcase (string-upcase name))
                     (intern (intern upcase))
                     (key (intern upcase :keyword)))
                (list intern :accessor intern :initarg key)))
            slots)))

