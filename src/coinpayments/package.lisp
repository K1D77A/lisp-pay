(defpackage #:lisp-pay/coinpayments
  (:use #:cl #:lisp-pay)
  (:nicknames #:coin #:coinpayments #:cl-coinpayments)
  (:export #:ipn-status
           #:ipn-failure
           #:ipn-payment-pending
           #:ipn-payment-success
           #:negative-2
           #:negative-1
           #:zero
           #:one
           #:two
           #:three
           #:five
           #:one-hundred
           #:request
           #:response
           #:good-response
           #:bad-response
           ;;classes ^
           ;;class accessors
           #:error-slot
           #:result-slot
           #:dex-extra
           #:request
           #:version
           #:dex-alist
           #:post-string
           #:merchant-secret-key
           #:key
           #:cmd
           #:format
           #:hmac
           #:required
           ;;conditions
           #:coinpayment-condition
           #:unknown-status
           #:unsupported-ipn
           #:no-dispatcher-found
           #:coinpayment-api-condition
           #:required-slots-not-bound
           ;;condition accessors
           #:ipn
           #:plist
           #:name
           #:status
           #:arg-count
           #:ipn-type
           #:status-type
           #:required
           #:not-set
           ;;main variables
           #:*ipn-dispatchers*
           ;;funs and macros
           #:new-ipn-dispatcher
           #:def-ipn-dispatcher
           #:find-dispatcher
           #:ipn-dispatch
           #:dispatch-ipn-by-name
           #:parse-data
           #:verify-data
           #:construct-status
           ;;api helpers
           #:new-request
           #:convert-merchant-secret-key
           #:validate-slots
           #:compute-hmac
           #:compute-dex-alist
           #:compute-post-params
           #:compute-final-hmac
           #:request
           ))
