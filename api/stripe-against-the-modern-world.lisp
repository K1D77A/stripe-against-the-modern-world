;;;; stripe-against-the-modern-world.lisp

(in-package #:stripe-against-the-modern-world)

(defparameter *api-version* "2020-08-27")

(defparameter *api-key*
  "sk_test_51Jn99bKazowTfVdknr4RjY5oqPKBrUV6B613Wj3afhpM76nl9QauaUPsWZo9nzxCalG8S1BUwSPewl9tDd2u28bN00D2DefuQi")

(defparameter *url* "https://api.stripe.con")

(defclass stripe-request ()
  ((url
    :reader url
    :initarg :url)
   (request-fun
    :reader request-fun
    :initarg :request-fun
    :initform 'dex:get)
   (url-generator
    :accessor url-generator
    :initarg :url-generator)))

(defclass request-without-content (stripe-request)
  ())

(defclass get-request (request-without-content)
  ((request-fun :initform 'dex:get)))

(defclass delete-request (request-without-content)
  ((request-fun :initform 'dex:delete)))

(defclass request-with-content (stripe-request)
  ((content
    :accessor content
    :initarg :content
    :type list)))

(defclass post-request (request-with-content)
  ((request-fun :initform 'dex:post)))

(defclass put-request (request-with-content)
  ((request-fun :initform 'dex:put)))

(defgeneric form-dex-args (request)
  (:method-combination append :most-specific-last))

(defmethod form-dex-args append ((request stripe-request))
  `(:basic-auth ,(list *api-key*) :headers ,(list `("Stripe-Version" . ,*api-version*))))

(defmethod form-dex-args append ((request request-without-content))
  nil)

(defmethod form-dex-args append ((request request-with-content))
  (with-slots (content)
      request 
    `(:content ,content :headers (("Content-Type" . "application/x-www-form-urlencoded")))))

(defmethod api-call (req)
  (with-slots (url request-fun)
      req 
    (let ((complete-url (format nil "~A~A" *url* url))
          (args (form-dex-args req)))
      (with-captured-api-failure
        (jojo:parse (apply request-fun complete-url args))))))

(defun gen-url-generator (url slots)
  (compile nil 
           `(lambda (request)
              (format nil "/~{~A~^/}"
                      (loop :for slot :in ',slots
                            :collect
                            (do-urlencode:urlencode
                             (slot-value request slot)))))))


(defmacro defapi ((name super url))
  (let* ((slots (slots-from-url url))
         (url-generator (gen-url-generator url slots)))
    `(progn (defclass ,name (,super)
              ((url :initform ,url)
               (url-generator :initform ,url-generator)
               ,@slots)))))

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



(defapi (balance%get-balance get-request "/v1/balance/:id"))
