(in-package #:lisp-pay/btcpay)

(defun %extract-sha256 (signature)
  (second (str:split #\= signature :omit-nulls t)))

(defun %validate-btcpay-webhook (signing-secret signature request)
  "Extract the original hmac and compare it with a newly computed HMAC. Returns
two values, validp and the raw body."
  (let ((original-sha256 (%extract-sha256 signature))
        (raw (raw-body request))
        (hmac (ironclad:make-hmac (to-array signing-secret)
                                  :sha256)))
    (ironclad:update-hmac hmac raw)
    (values (string= (ironclad:byte-array-to-hex-string (crypto:hmac-digest hmac))
                     original-sha256)
            raw)))

(defmethod verify-webhook (signing-secret (request LACK.REQUEST:REQUEST))
  (let* ((headers (request-headers request))
         (signature (gethash "btcpay-sig" headers)))
    (%validate-btcpay-webhook signing-secret signature request)))

(defmethod verify-webhook (signing-secret (request tbnl:request))
  (let* ((headers (request-headers request))
         (signature (cdr (assoc :btcpay-sig headers))))
    (%validate-btcpay-webhook signing-secret signature request)))

