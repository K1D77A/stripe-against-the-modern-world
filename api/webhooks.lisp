(in-package #:stripe-against-the-modern-world)

#||
This file contains the code for verifying a webhook from stripe.
https://stripe.com/docs/webhooks/signatures#verify-manually
||#

(defmethod ->array ((str string))
  (babel:string-to-octets str))

(defmethod ->array ((str array))
  str)

(defun compute-signature (signature timestamp raw-body)
  (let ((bytes (concatenate '(vector (unsigned-byte 8))
                            (->array timestamp)
                            (->array ".")
                            (->array raw-body))))
    (ironclad::hkdf-extract 'ironclad:sha256 (->array signature) bytes)))

(defun verify-signature (signature timestamp raw-body)
  "Verifies the received SIGNATURE using TIMESTAMP and RAW-BODY. Returns whether it is 
valid (bool) and the difference between TIMESTAMP and #'local-time:now (unix epoch time)"
  (let* ((signature (->array signature))
         (genned (compute-signature signature timestamp raw-body))
         (ts (parse-integer timestamp)))
    (values (equalp signature genned)
            (- (local-time:timestamp-to-unix (local-time:now)) ts))))



