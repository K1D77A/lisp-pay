(in-package #:lisp-pay/paypal)

(defparameter *sig-cache* (make-hash-table :test #'equal))

(defun %get-rsa-public-key (cert-url)
  (cl-tls:x509-decode
   (cdar (cl-tls::parse-pem
          (babel:octets-to-string (dex:get cert-url))))))

(defmethod %rsa-details ((key CL-TLS::X509V3-CERTIFICATE))
  (with-slots (cl-tls::tbs-certificate)
      key
    (with-slots (cl-tls::subject-pki)
        cl-tls::tbs-certificate
      (getf cl-tls::subject-pki :subject-public-key))))

(defun %rsa-details->ironclad-rsa (list)
  (destructuring-bind (&key public-exponent modulus)
      list
    (make-instance 'ironclad:rsa-public-key :e public-exponent :n modulus)))

(defgeneric cert->public-key (algo cert-url)
  (:documentation "Download the CERT-URL and convert it to the relevant objects for ALGO."))

(defmethod cert->public-key ((algo (eql :SHA256WITHRSA)) cert-url)
  (let ((sig (gethash cert-url *sig-cache*)))
    (or sig 
        (setf (gethash cert-url *sig-cache*)
              (%rsa-details->ironclad-rsa (%rsa-details (%get-rsa-public-key cert-url)))))))

(defun %hash-message (message)
  (ironclad:digest-sequence :sha256 message))

(defun %generate-signature-bytes (transmission-id timestamp webhook-id crc)
  "Generate the bytes used to verify the signature."
  (let ((to-sign (format nil "~A|~A|~A|~A" transmission-id timestamp webhook-id crc)))
    (babel:string-to-octets to-sign)))

(defgeneric %verify-message (algo public-key signature message)
  (:documentation "Verifies the message signature using ALGO."))

(defmethod %verify-message ((algo (eql :SHA256WITHRSA))
                            public-key signature message)
  (cl-tls:rsassa-pkcs1.5-verify public-key 
                                message
                                (cl-base64:base64-string-to-usb8-array signature)
                                :sha256))

(defun %verify-paypal-webhook (algo cert-url transmission-signature transmission-id
                              timestamp webhook-id raw-body)
  (let* ((crc (crc-raw raw-body))
         (key (cert->public-key algo cert-url))
         (message (%generate-signature-bytes
                   transmission-id timestamp webhook-id crc)))
    (values (%verify-message algo key transmission-signature message)
            raw-body)))

(defun %algo->key (algo)
  "Convert the algo string into a keyword."
  (cond ((string= algo "SHA256withRSA")
         :SHA256WITHRSA)
        (t (error "Unknown encryption algorithm. Please implement or inform maintainer."))))

(defgeneric verify-webhook (webhook-id request)
  (:documentation "Generic means of verifying a webhook from Paypal. Just a simple wrapper
around #'%verify-paypal-webhook which extracts the extracts the required
 information from the headers."))

(defmethod verify-webhook (webhook-id (request hunchentoot:request))
  (let* ((headers (request-headers request))
         (auth-algo (%algo->key (cdr (assoc :paypal-auth-algo headers))))
         (transmission-sig (cdr (assoc :paypal-transmission-sig headers)))
         (cert-url (cdr (assoc :paypal-cert-url headers)))
         (transmission-id (cdr (assoc :paypal-transmission-id headers)))
         (transmission-time (cdr (assoc :paypal-transmission-time headers))))
    (%verify-paypal-webhook auth-algo cert-url transmission-sig transmission-id
                    transmission-time webhook-id (raw-body request))))

(defmethod verify-webhook (webhook-id (request lack.request:request))
  (let* ((headers (request-headers request))
         (auth-algo (%algo->key (gethash "paypal-auth-algo" headers)))
         (transmission-sig (gethash "paypal-transmission-sig" headers))
         (cert-url (gethash "paypal-cert-url" headers))
         (transmission-id (gethash "paypal-transmission-id" headers))
         (transmission-time (gethash "paypal-transmission-time" headers)))
    (%verify-paypal-webhook auth-algo cert-url transmission-sig transmission-id
                    transmission-time webhook-id (raw-body request))))
