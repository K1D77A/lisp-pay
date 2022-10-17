;;;; lisp-pay.asd

(asdf:defsystem #:lisp-pay
  :description "Wrappers over multiple Payment Processor APIs"
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.3"
  :depends-on (#:str
               #:ironclad
               #:babel
               #:alexandria
               #:jonathan
               #:dexador
               #:lack
               #:ningle
               #:cl-base64
               #:cl-tls
               #:hunchentoot
               #:closer-mop
               #:hu.dwim.defclass-star
               #:shasht)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "helpers")
               (:file "protocol")
               (:file "response")
               (:file "mop")
               (:file "lisp-pay")
               (:module "btcpay"
                :components ((:file "package")
                             (:file "protocol")
                             (:file "btcpay")))
               (:module "stripe"
                :components ((:file "package")
                             (:file "conditions")
                             (:file "helpers")
                             (:file "protocol")
                             (:file "stripe")
                             (:file "webhooks")))
               (:module "paypal"
                :components ((:file "package")
                             (:file "conditions")
                             (:file "protocol")
                             (:file "token")
                             (:file "paypal")
                             (:file "webhook-verify")))
               (:module "coinpayments"
                :components ((:file "package")
                             (:file "classes")
                             (:file "conditions")
                             (:file "api-helpers")
                             (:file "coinpayments")
                             (:file "api-forms")))))



