;;;; package.lisp

(defpackage #:lisp-pay
  (:use #:cl)
  (:nicknames #:pay)
  (:export #:lisp-pay-condition
           
           ;;helpers
           #:request
           #:raw-body
           #:to-array
           #:request-headers
           #:crc-raw
           #:string-gen
           #:in-list

           ;;protocol
           #:replace-vars-for-slot-names
           #:gen-url-generator
           #:gen-query-generator
           #:slots-from-url

           ;;mop
           #:lisp-pay-api-call
           #:lisp-pay-api-slot
           #:request
           #:string-constructor
           #:endpoint
           #:genned-slot-names))
