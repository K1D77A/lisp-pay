;;;; package.lisp

(defpackage #:lisp-pay
  (:use #:cl)
  (:nicknames #:pay)
  (:import-from #:hu.dwim.defclass-star
                #:defclass*)
  (:export #:lisp-pay-condition
           
           ;;lisp-pay
           #:processor
           #:api-metaclass
           #:base-url
           #:defprocessor

           ;;helpers
           #:*processor*
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
           #:with-hash-keys

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
           #:wrap-dex-call

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
           #:content-type
           #:request-without-content
           #:get-request
           #:delete-request
           #:request-with-content
           #:post-request
           #:post-files-request
           #:put-request
           #:patch-request
           #:response
           #:api-failure

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


