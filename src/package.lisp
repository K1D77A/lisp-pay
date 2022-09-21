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
           #:defapi

           ;;mop
           #:lisp-pay-api-call
           #:lisp-pay-api-slot
           #:lisp-pay-api-call-with-query
           #:string-constructor
           #:string-constructor
           #:endpoint           
           #:genned-slot-names
           #:query-slot-names
           #:query-constructor
           #:request
           #:request-fun
           #:content 
           #:request-without-content
           #:get-request
           #:delete-request
           #:request-with-content
           #:post-request
           #:post-files-request
           #:put-request
           #:patch-request
           #:response))

           
