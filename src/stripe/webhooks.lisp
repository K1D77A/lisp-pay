(in-package #:lisp-pay/stripe)

#||
This file contains the code for verifying a webhook from stripe.
https://stripe.com/docs/webhooks/signatures#verify-manually
||#
(defun compute-signature (signature timestamp raw-body)
  "Convert everything that is not an array to an array and then compute a hmac. Return 
a hex string as the final result."
  (let ((bytes (concatenate '(vector (unsigned-byte 8))
                            (to-array timestamp)
                            (to-array ".")
                            (to-array raw-body)))
        (hmac (ironclad:make-hmac (to-array signature)
                                  :sha256)))
    (ironclad:update-hmac hmac bytes)
    (ironclad:byte-array-to-hex-string (crypto:hmac-digest hmac))))

(defun verify-signature (signing-secret v1 timestamp raw-body)
  "Verifies the received V1 using TIMESTAMP and RAW-BODY. Returns whether it is 
valid (bool) and the difference between TIMESTAMP and #'local-time:now (unix epoch time)"
  (let* ((ss (to-array signing-secret))
         (genned (compute-signature ss timestamp raw-body))
         (ts (parse-integer timestamp)))
    (values (string= v1 genned)
            (- (local-time:timestamp-to-unix (local-time:now)) ts))))

(defun %validate-stripe-webhook (signing-secret signatures request)
  (let ((hash (make-hash-table :test #'equal))
        (split (str:split #\, signatures :omit-nulls t)))
    (mapc (lambda (split)
            (destructuring-bind (key val)
                (str:split #\= split :omit-nulls t)
              (setf (gethash key hash) val)))
          split)
    (let ((raw (raw-body request)))
      (multiple-value-bind (validp time-dif)
          (verify-signature signing-secret (gethash "v1" hash) (gethash "t" hash) raw)
        (values validp time-dif raw)))))

(defmethod verify-webhook (signing-secret (request LACK.REQUEST:REQUEST))
  ;;this could fail on multiple v1's idk... they are a bit vague about it in the spec
  (let* ((headers (request-headers request))
         (signatures (gethash "stripe-signature" headers)))
    (%validate-stripe-webhook signing-secret signatures request)))

(defmethod verify-webhook (signing-secret (request tbnl:request))
  (let* ((headers (request-headers request))
         (signatures (cdr (assoc :stripe-signature headers))))
    (%validate-stripe-webhook signing-secret signatures request)))

(defmethod no-applicable-method ((fun (eql #'verify-webhook)) &rest args)
  (declare (ignore args))
  (error "No default implementation for anything other than 
lack.request:"))
