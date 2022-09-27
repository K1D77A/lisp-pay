;;;; package.lisp

(defpackage #:lisp-pay
  (:use #:cl)
  (:nicknames #:pay)
  (:export #:lisp-pay-condition
           
           ;;lisp-pay
           #:processor
           #:api-metaclass
           #:base-url
           #:defprocessor

           ;;helpers
           #:request
           #:raw-body
           #:to-array
           #:request-headers
           #:crc-raw
           #:string-gen
           #:in-list
           #:write-json
           #:read-json
           #:%write-json
           #:%read-json
           #:print-all-slots

           ;;protocol
           #:replace-vars-for-slot-names
           #:gen-url-generator
           #:gen-query-generator
           #:slots-from-url
           #:call-api
           #:%call-api
           #:generate-url
           #:generate-dex-list
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
           #:response

           ;;response
           #:construct-api-failure-object
           #:construct-response-from-api
           #:determine-response
           #:api-response-condition
           #:client-error-response
           #:server-error-response
           #:unknown-server-response

           #:api-response-class
           #:information-response
           #:successful-response
           #:redirection-response

           #:status-code
           #:body
           #:dex-response
           #:api-failure
           ))


