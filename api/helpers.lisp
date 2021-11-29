(in-package #:stripe-against-the-modern-world)

#||
Below is the definition of a primitive DSL which will convert a list into an 
alist that can be passed as the :content key to dex:post. Unfortunately although
it has some of the code required to work with many nested arrays it will 
only work for a single depth array right now.

||#

(defparameter *test* 
  '(("fur" . "fluffy")
    ("cat" . "dog")
    (:array "animals"
     ("oof" . "doof")
     ("kaboof" . "foo")
     ("dog"
      "cat"
      "bird"))
    (:array "images"
     ("fur" . "fluffy")
     ("colour" . "brown"))
    ("fur" . "fluffy")
    ("colour" . "brown")))

(defparameter *test2* 
  '(("fur" . "fluffy")
    ("cat" . "dog")
    (:array "animals"
     ("oof" . "doof")
     ("kaboof" . "foo")
     ("dog"
      "cat"
      "bird"))
    (:array "images"
     ("fur" . "fluffy")
     ("colour" . "brown")
     (:array "nested-images"
      ("fluff" . "fluffy")
      ("pos" "foo" "bar")))
    ("fur" . "fluffy")
    ("colour" . "brown")))

#||
(ec *test2*)
=> 
broken.
Dont nest arrays in arrays.
||#

#||
(ec *test* )
=>
(("fur" . "fluffy") ("cat" . "dog") ("animals[0][oof]" . "doof")
("animals[0][kaboof]" . "foo") ("animals[1]" . "dog") ("animals[2]" . "cat")
("animals[3]" . "bird") ("images[0][fur]" . "fluffy")
("images[0][colour]" . "brown") ("fur" . "fluffy") ("colour" . "brown"))
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
         (setf arr ,array-name)
         (unwind-protect
              (locally ,@body)
           (setf arr ,previous))))))

(defmethod process-obj ((type (eql :object)) list env)
  (destructure-environment (env)
    (cond ((and positions array-name (stringp (cdr list)));obj inside array 
           (format-object-in-array array-name positions (car list) (cdr list)))
          ((and positions (listp (cdr list)))
           (with-increment-current-pos (env)
             (let ((current (first positions)))
               (loop :for ele :in list
                     :for pos :from current :upto (+ current (length list))
                     :collect (format-basic-array array-name (list pos) ele)))))          
          ((stringp (cdr list));basic object
           (list (list list))))))

(defmacro with-increment-current-pos ((env) &body body)
  (alexandria:with-gensyms (pos prev)
    `(let* ((,pos (getf ,env :positions))
            (,prev (first ,pos)))
       (incf (first ,pos))
       (unwind-protect
            (locally ,@body)
         (setf (first (getf ,env :positions)) ,prev)))))

(defmacro with-new-pos ((env) &body body)
  `(progn (push 0 (getf ,env :positions))
          (prog1 (locally ,@body)
            (setf (getf ,env :positions)
                  (rest (getf ,env :positions))))))

(defmethod process-obj ((type (eql :array)) list env)
  (with-changed-array ((second list))
                      env
    (with-new-pos (env)
      (prog1 (let ((cddr (cddr list)))
               (loop :for list :in cddr 
                     :for x :from 0 :upto (length cddr)
                     :collect (rec list env)))))))

(defmethod process-obj :around (type list env)
  (let ((res (call-next-method)))
    (if (stringp (first res))
        (list res)
        res)))

(defun entry (list env)
  (reduce #'nconc
          (destructure-environment (env)
            (loop :for list :in list
                  :nconcing (rec list env)))))

(defun rec (list env)
  (let* ((type (determine-list-type list))
         (res  (process-obj type list env)))
    (if (stringp (first res))
        res res)))

(defun construct-alist (&rest lists)
  (loop :for list :in lists
        :appending (entry list nil)))

(defun ec (&rest lists)
  (apply #'construct-alist lists))


