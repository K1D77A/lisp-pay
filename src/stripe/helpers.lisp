(in-package #:lisp-pay/stripe)

#||
Below is the definition of a primitive DSL which will convert a list into an 
alist that can be passed as the :content key to dex:post. Unfortunately although
it has some of the code required to work with many nested arrays it will 
only work for a single depth array right now.

||#

(defparameter *test* 
  '(("fur" . "fluffy")
    ("cat" . "dog")
    (:array "woofers"
            ("dog" "wolf")
            (("smol" . "shih-tzu")
             ("big" . "labrador")))
    (:array "animals"
            (("oof" . "doof")
             ("kaboof" . "foo"))
            ("dog"
             "cat"
             "bird"))
    (:array "images"
            (("fur" . "fluffy")
             ("colour" . "brown")))
    ("fur" . "fluffy")
    ("colour" . "brown")))

(defparameter *test2* 
  '(("fur" . "fluffy")
    ("cat" . "dog")
    (:array "animals"
     (("oof" . "doof")
      ("kaboof" . "foo"))
     ("dog"
      "cat"
      "bird"))
    (:array "images"
     (("fur" . "fluffy")
      ("colour" . "brown"))
     (:array "nested-images"
      (("fluff" . "fluffy"))
      ("pos" "foo" "bar")))
    (:array "cats"
     ("brown" "white" "black"))
    ("fur" . "fluffy")
    ("colour" . "brown")))

(defparameter *test3*
  (jojo:parse "{
  \"id\": \"prod_L5RRGNG2CUWAS4\",
  \"object\": \"product\",
  \"active\": true,
  \"created\": 1643937936,
  \"images\": [1,2,3],
  \"livemode\": false,
  \"metadata\": [{
    \"order_id\": \"6735\"
  },{
    \"order_id\": \"6736\"
     \"crack\" : [1, 2, 3]
  }],
  \"hoes\": [1,2,3],
  \"name\": \"Pro (beta testing, monthly subscription)\",
  \"updated\": 1643937936
}" :as :hash-table))

#||
(ec *test2*)
=> 
(("fur" . "fluffy") ("cat" . "dog") ("animals[0][oof]" . "doof")
("animals[0][kaboof]" . "foo") ("animals[1]" . "dog") ("animals[2]" . "cat")
("animals[3]" . "bird") ("images[0][fur]" . "fluffy")
("images[0][colour]" . "brown") ("images[1][0][fluff]" . "fluffy")
("images[1][1]" . "pos") ("images[1][2]" . "foo") ("images[1][3]" . "bar")
("cats[0]" . "brown") ("cats[1]" . "white") ("cats[2]" . "black")
("fur" . "fluffy") ("colour" . "brown"))
||#

#||
(ec *test* )
=>
(("fur" . "fluffy") ("cat" . "dog") ("woofers[0]" . "dog")
("woofers[1]" . "wolf") ("woofers[2][smol]" . "shih-tzu")
("woofers[2][big]" . "labrador") ("animals[0][oof]" . "doof")
("animals[0][kaboof]" . "foo") ("animals[1]" . "dog") ("animals[2]" . "cat")
("animals[3]" . "bird") ("images[0][fur]" . "fluffy")
("images[0][colour]" . "brown") ("fur" . "fluffy") ("colour" . "brown"))
||#

#||
(ec *test3*)
=>
(("id" . "prod_L5RRGNG2CUWAS4") ("object" . "product") ("images[0]" . 1)
("images[1]" . 2) ("images[2]" . 3) ("metadata[0][order_id]" . "6735")
("metadata[1][order_id]" . "6736") ("metadata[1][crack][0]" . 1)
("metadata[1][crack][1]" . 2) ("metadata[1][crack][2]" . 3) ("hoes[0]" . 1)
("hoes[1]" . 2) ("hoes[2]" . 3)
("name" . "Pro (beta testing, monthly subscription)"))
||#

#||
(ec *test3* *test3*)
=>
(("id" . "prod_L5RRGNG2CUWAS4") ("object" . "product") ("images[0]" . 1)
 ("images[1]" . 2) ("images[2]" . 3) ("metadata[0][order_id]" . "6735")
 ("metadata[1][order_id]" . "6736") ("metadata[1][crack][0]" . 1)
 ("metadata[1][crack][1]" . 2) ("metadata[1][crack][2]" . 3) ("hoes[0]" . 1)
 ("hoes[1]" . 2) ("hoes[2]" . 3)
 ("name" . "Pro (beta testing, monthly subscription)") ("fur" . "fluffy")
 ("cat" . "dog") ("animals[0][oof]" . "doof") ("animals[0][kaboof]" . "foo")
 ("animals[1]" . "dog") ("animals[2]" . "cat") ("animals[3]" . "bird")
 ("images[0][fur]" . "fluffy") ("images[0][colour]" . "brown")
 ("images[1][0][fluff]" . "fluffy") ("images[1][1]" . "pos")
 ("images[1][2]" . "foo") ("images[1][3]" . "bar") ("cats[0]" . "brown")
 ("cats[1]" . "white") ("cats[2]" . "black") ("fur" . "fluffy")
 ("colour" . "brown"))
||#

(defun format-object-in-array (array-name positions slot-name val)
  (cons (format nil "~A~{[~D]~}[~A]" array-name positions slot-name) val))

(defun format-basic-array (array-name positions val)
  (cons (format nil "~A~{[~D]~}" array-name positions) val))

(defun determine-list-type (list)
  (let ((first (first list)))
    (if (keywordp first)
        first
        :object)))

(defmacro destructure-environment ((env) &body body)
  `(let ((array-name (getf ,env :array-name))
         (positions (getf ,env :positions)))
     (declare (ignorable array-name positions))
     (locally ,@body)))

(defmacro with-changed-array ((array-name) env &body body)
  (alexandria:with-gensyms (previous)
    `(symbol-macrolet ((arr (getf ,env :array-name)))
       (let ((,previous arr))
         (unless arr
           (setf arr ,array-name))
         (unwind-protect
              (locally ,@body)
           (setf arr ,previous))))))

(defmacro with-resetting-current-pos ((env) &body body)
  (alexandria:with-gensyms (pos prev)
    `(let* ((,pos (getf ,env :positions))
            (,prev (first ,pos)))
       (unwind-protect
            (locally ,@body)
         (setf (first (getf ,env :positions)) ,prev)))))

(defmacro with-current-pos ((env) &body body)
  `(symbol-macrolet ((pos (first (getf ,env :positions))))
     (locally ,@body)))

(defmacro with-new-pos ((env) &body body)
  `(progn (push (the fixnum 0) (getf ,env :positions))
          (prog1 (locally ,@body)
            (setf (getf ,env :positions)
                  (rest (getf ,env :positions))))))

(defmethod process-obj ((type (eql :object)) list env acc)
  (declare (optimize (speed 3) (safety 1)))
  (destructure-environment (env)
    (cond ((and positions array-name (listp (first list)));obj inside array
           (with-current-pos (env)
             (loop :for ele :in list
                   :do (push (format-object-in-array array-name (reverse positions)
                                                     (car ele) (cdr ele))
                             (res-list acc))
                   :finally (incf (the fixnum pos)))))
          ((and positions array-name (stringp (first list)))
           (with-current-pos (env)
             (loop :for ele :in list
                   :do (push (format-basic-array array-name (reverse positions) ele)
                             (res-list acc))
                       (incf (the fixnum pos)))))
          ((stringp (cdr list));basic object
           (push list (res-list acc))))))

(defmethod process-obj ((type (eql :array)) list env acc)
  (declare (optimize (speed 3) (safety 1)))
  (with-changed-array ((second list))
                      env
    (with-new-pos (env)
      (let ((cddr (cddr list)))
        (with-resetting-current-pos (env)
          (loop :for lst :in cddr
                :do (rec lst env acc))
          acc)))))

(defmethod process-obj :around (type list env acc)
  (call-next-method))

(defun entry (list env acc)
  (loop :for list :in list
        :do (rec list env acc)))

(defun rec (list env acc)
  (let ((type (determine-list-type list)))
    (process-obj type list env acc)))

(defstruct res
  list)

(defun construct-alist (list)
  (let ((acc (make-res :list ())))
    (entry list () acc)
    (res-list acc)))

(defun ec (&rest entries)
  (let ((res ()))
    (dolist (entry entries (nreverse (reduce #'nconc res)))
      (typecase entry
        (list (push (construct-alist entry) res))
        (hash-table (push (enc-h entry) res))))))
#||
Here lies some code to convert hash tables into the horrible url encoding stripe uses.
||#

(defclass env ()
  ((parents
    :accessor parents
    :initform ())
   (res
    :accessor res
    :initform ())))

(defmacro with-resetting-parents ((env) &body body)
  (alexandria:with-gensyms (before)
    `(let ((,before (parents ,env)))
       (unwind-protect 
            (locally ,@body)
         (setf (parents ,env) ,before)))))

(defun encode-key-array (parents key val)
  (cons (format nil "~A~{[~A]~}[~A]" (first parents) (rest parents) key) val))

(defun encode-array-array (parents val)
  (cons (format nil "~A~{[~A]~}" (first parents) (rest parents)) val))

(defun encode-key-val (key val)
  (cons key val))

(defun encode-hash-table (env hash-table)
  (with-accessors ((res res)
                   (parents parents))
      env
    (maphash (lambda (key val)
               (cond ((and parents (stringp val));;nested key array
                      (push (encode-key-array (reverse parents) key val) res))
                     ((and parents (listp val))
                      (with-resetting-parents (env)
                        (push key parents)
                        (loop :for n :from 0 :to (length val)
                              :for ele :in val
                              :do (with-resetting-parents (env)
                                    (push n parents)
                                    (if (hash-table-p ele)
                                        (encode-hash-table env ele)
                                        (push (encode-array-array (reverse parents) ele)
                                              res))))))
                     ((and (stringp key)
                           (stringp val))
                      ;;basic
                      (push (encode-key-val key val) res))
                     ((listp val)
                      ;;array
                      (with-resetting-parents (env)
                        (push key parents)
                        (loop :for n :from 0 :to (length val)
                              :for ele :in val
                              :do (with-resetting-parents (env)
                                    (push n parents)
                                    (if (hash-table-p ele)
                                        (encode-hash-table env ele)
                                        (push (encode-array-array (reverse parents)
                                                                  ele)
                                              res))))))))
             hash-table)
    (res env)))

(defun enc-h (hash)
  (encode-hash-table (make-instance 'env) hash))









