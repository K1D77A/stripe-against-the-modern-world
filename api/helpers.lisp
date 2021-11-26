(in-package #:stripe-against-the-modern-world)


;;;we use dexador to send the application-form data, this requires an alist, however
;;;they provide no default means of sending arrays, if you put a list in the alist
;;;it is not automatically modified.
(defun %encode-seq (name seq)
  (loop :for item :across seq
        :for x :from 0 :to (length seq)
        :collect (cons (format nil "~A[~D]" name x) item)))

(defgeneric encode-alist (key obj)
  (:documentation "Given a key and an obj, decides which way to encode the obj."))

(defmethod encode-alist (key (seq array))
  "In the case SEQ is an array uses #'%encode-seq to generate a new alist."
  (%encode-seq key seq))

(defmethod encode-alist (key (val string))
  "In the case val is a string then return a 1 length alist."
  (list (cons key val)))

(defun encode-content (&rest lists)
  "Encode alist by automatically converting an array in the cdr position into 
a new alist that represents would represent an form-encoded array."
  (loop :for (key . val) :in (reduce #'append lists)
        :nconcing (encode-alist key val)))

(defun ec (&rest lists)
  (apply #'encode-content lists))
