# lisp-pay

## Wrapper over multiple Payment Processor API's

Fundamentally this is a library for writing API wrappers. 

With it I have wrapped 3 API's (BTCPay, Paypal, Stripe) and have included 
Coinpayments separately because I have no plans to integrate it into Lisp-pays core system.

## Differences from their original libraries
I have removed all \*parse-as\* all global variables except \*processor\*, everything is
now configured using a specific object for each library which is bound to \*processor\* for
each package. 
Everything is automatically parsed as a hash-table using Shasht.

This default behaviour can be changed by specializing the Generic function 
```lisp
(defmethod %read-json (processor stream-or-string)
  (shasht:read-json stream-or-string))
```
But if you do that something will probably break so you will have to fix that yourself.

Writing is done using Shasht: 
```lisp
(defmethod %write-json (processor obj &optional stream)
  (shasht:write-json obj stream))
```
These are in `src/helpers.lisp`

## How it works

Every processor has the exported symbol `*processor*` which is bound to an instance 
of a subclass of `processor`. Paypal, Stripe, and BTCPay all have their own versions,
so you can set 
```lisp
(setf stripe:*processor* (make-instance 'stripe:stripe \<initargs\>))
```
And then your version with your keys will be used when evaluating `call-api`

## TODO

Easy macro for lexically binding the value of `<package>:*processor*` on a per use
basis. 

### Stripe

- api-key 
- api-version (This should remain as default)
- base-url 

### Paypal 
- base-url (This should remain as default unless using sandbox)
- secret-id
- token (bound with #'get-token \<processor\>)
- client-id 

### BTCPay
- api-key
- base-url


## Conditions

All conditions are a subclass of `api-response-condition` 
Each payment processor has a specific `api-failure` object which is a subclass of 
`api-failure` these are just a nicely version of the response body. You can use this 
to dispatch.

## General notes

### Query parameters

Query parameters are slots within the object, just set them and the ones that are bound will be encoded and added onto the end of the URL.

### Path parameters

Path parameters are slots within the request object, just set the slots and they will be automatically encoded into the URL.


## Paypal 

To get started create your own version of the paypal processor object and then bind 
paypal:\*processor\* to this. 

Then call `(get-token <my-processor>)`.

Now you have your token you can make requests. 
```lisp
PAYPAL> (make-instance 'products%list)
#<PRODUCTS%LIST 
REQUEST-FUN: GET
CONTENT-TYPE: application/json
 {100C63FE0B}>
PAYPAL> (call-api *)
; Debugger entered on #<UNBOUND-SLOT TOKEN {100CEAF943}>
[1] PAYPAL> 
#<SUCCESSFUL-RESPONSE 
STATUS-CODE: 200
BODY: #<HASH-TABLE :TEST EQUAL :COUNT 2 {100F4B1423}>
DEX-RESPONSE: #S(DEX-RESPONSE
                 :BODY {"products":[{"id":"PROD-301675791L907343Y","name":"name","create_time":"2022-02-24T08:10:22Z","links":[{"href":"https://api.sandbox.paypal.com/v1/catalogs/products/PROD-301675791L907343Y","rel":"self","method":"GET"}]},{"id":"PROD-55L8435121622000F","name":"name","create_time":"2022-02-24T08:11:21Z","links":[{"href":"https://api.sandbox.paypal.com/v1/catalogs/products/PROD-55L8435121622000F","rel":"self","method":"GET"}]}],"links":[{"href":"https://api.sandbox.paypal.com/v1/catalogs/products?page_size=10&page=1","rel":"self","method":"GET"}]}
                 :STATUS-CODE 200
                 :HEADERS #<HASH-TABLE :TEST EQUAL :COUNT 16 {100F4A7CF3}>
                 :QURI https://api-m.sandbox.paypal.com/v1/catalogs/products)
 {100F4B4DB3}>
PAYPAL> 
```


### Token issues
If you have failed to set token or it has expired
```lisp
PAYPAL> (call-api *)
; Debugger entered on #<UNBOUND-TOKEN {100BD3E133}>

You have not evaluated 'get-token'.
   [Condition of type UNBOUND-TOKEN]

Restarts:
 0: [MISSING-TOKEN] Token could be broken, refresh and try again?
```
All requests made with `call-api` have the restart `missing-token` just in case your token expires. 

```lisp
#<PRODUCTS%LIST {100BD37F5B}>
PAYPAL> (handler-bind ((token-issue (lambda (c)
                                   (declare (ignore c))
                                   (invoke-restart 'missing-token))))
       (call-api *))
#<TWO-HUNDRED {1003A50663}>
PAYPAL> (body *)
<hash table>
PAYPAL> 
```
### Adding headers

Some calls accept other headers like Paypal-Request-Id to add these headers to a request lexically bind the variable `*request-headers*`

```lisp
#<PRODUCTS%LIST {101130BC0B}>
PAYPAL> (let ((*request-headers* '(("Paypal-Auth-Assertion" . "imauthassertion"))))
       (declare (special *request-headers*))
       <request> 
       <call-api>)
```
Headers are sent using Dex so they have to be a properly formed alist like above.
You can see the additional headers in the paypal dev docs.



## Webhook verification
To verify the signature of a paypal request you can use 
`(paypal:verify-paypal-webhook)` 
which takes `webhook-id request raw-body`
The webhook-id is returned when you create a webhook.

Here is an example of how I use it in practice: 
```lisp

(defmethod handle-webhook (client processor request)
  "Handles a webhook REQUEST properly by validating it for each processor, 
then constructing an internal representation of the hook, preprocessing it and then 
responding to it."
  (destructuring-bind (&key validp raw &allow-other-keys)
      (validate-received-webhook client processor request)
    (if (and validp raw)
        ....)))

(defmethod validate-received-webhook (client (processor paypal) req)
  (let ((raw (de:ningle-raw-body req)))
    (list :validp (paypal:verify-paypal-webhook (webhook-id processor) req raw)
          :raw raw)))
          
```
(please note that PROCESSOR above is not the same as the definition within this library)

There are methods for `lack.request` and a `tbnl:request`


# Stripe

This is a an implementation of the Stripe API. 

Currently have wrapped the section Core Resources and under Products the products, prices and shipping then, under Checkout the sessions and finally under Webhooks the webhooks. 

## How to 
To change the default parser from jojo's plist to a hash-table change `*parse-as*` to 
a valid (jojo:parse <content> :as <key>), I suggest :hash-table

First set the value of `stripe:*processor* to your instance of the stripe processor.

Then you simply do the following:

```lisp
STRIPE> (make-instance 'events%all)
#<EVENTS%ALL 
REQUEST-FUN: GET
CONTENT-TYPE: application/json
 {100849AFDB}>

STRIPE> (call-api *)
; Evaluation aborted on #<CLIENT-ERROR-RESPONSE 
; PROCESSOR: #<STRIPE {1009884D93}>
; STATUS-CODE: 401
; BODY: #<HASH-TABLE :TEST EQUAL :COUNT 1 {10084B3813}>
; DEX-RESPONSE: #S(DEX-RESPONSE
;                  :BODY {
;   "error": {
;     "code": "api_key_expired",
;     "doc_url": "https://stripe.com/docs/error-codes/api-key-expired",
;     "message": "Expired API Key provided: sk_test_*********************************************************************************************DefuQi",
;     "type": "invalid_request_error"
;   }
; }
; 
;                  :STATUS-CODE 401
;                  :HEADERS #<HASH-TABLE :TEST EQUAL :COUNT 14 {10084B0BC3}>
;                  :QURI https://api.stripe.com/v1/events)
; API-FAILURE: #<INVALID-REQUEST-ERROR 
; 
; ERROR-TYPE: invalid_request_error
; STATUS-CODE: api_key_expired
; MESSAGE: Expired API Key provided: sk_test_*********************************************************************************************DefuQi
; DOC-URL: https://stripe.com/docs/error-codes/api-key-expired
;  {10084B54C3}>
;  {10084B4D63}>
```

```lisp
STRIPE> (make-instance 'events%id :id "abc")
#<EVENTS%ID {100F182A7B}>
STRIPE> (call-api *)
<invalid-request-error because no known id>
```
If you have a post request that requires values then these requests have a slot called `content` that you fill with an ALIST.
```lisp
STRIPE> (make-instance 'charges%create :content '(("amount" . 100)("currency" . "gbp")("source" . "abc")))
#<CHARGES%CREATE {100F4CF67B}>
```
Dexador is used to send the requests so it must be a properly formed ALIST.

### Alist construct
In `src/stripe/helpers.lisp` I have built a very simple DSL which will parse into an alist, you can pass the result of evaluating this as the :content key to dex:post. 
```lisp
(defparameter *test* 
  '(("fur" . "fluffy")
    ("cat" . "dog")
    (:array "woofers"
     ("dog" "wolf")
     (("smol" . "shih-tzu")
      ("big" . "labrador")))
    (:array "animals"
     (("oof" . "doof")
      ("kaboof" . "foo"))
     ("dog"
      "cat"
      "bird"))
    (:array "images"
     (("fur" . "fluffy")
      ("colour" . "brown")))
    ("fur" . "fluffy")
    ("colour" . "brown")))

STRIPE> (ec *test*)
(("fur" . "fluffy") ("cat" . "dog") ("woofers[0]" . "dog")
 ("woofers[1]" . "wolf") ("woofers[2][smol]" . "shih-tzu")
 ("woofers[2][big]" . "labrador") ("animals[0][oof]" . "doof")
 ("animals[0][kaboof]" . "foo") ("animals[1]" . "dog") ("animals[2]" . "cat")
 ("animals[3]" . "bird") ("images[0][fur]" . "fluffy")
 ("images[0][colour]" . "brown") ("fur" . "fluffy") ("colour" . "brown"))
 ```
 It accepts an arbitrary number of lists and appends them together. 
 The DSL means you can create an alist that will correctly format as a form-url encoded string, this is annoying but its how Stripe handles requests...
 
Supports nested arrays although I've never tested it.
```lisp
(defparameter *test2* 
  '(("fur" . "fluffy")
    ("cat" . "dog")
    (:array "animals"
     (("oof" . "doof")
      ("kaboof" . "foo"))
     ("dog"
      "cat"
      "bird"))
    (:array "images"
     (("fur" . "fluffy")
      ("colour" . "brown"))
     (:array "nested-images"
      (("fluff" . "fluffy"))
      ("pos" "foo" "bar")))
    (:array "cats"
     ("brown" "white" "black"))
    ("fur" . "fluffy")
    ("colour" . "brown")))

STRIPE> (ec *test2*)
(("fur" . "fluffy") ("cat" . "dog") ("animals[0][oof]" . "doof")
 ("animals[0][kaboof]" . "foo") ("animals[1]" . "dog") ("animals[2]" . "cat")
 ("animals[3]" . "bird") ("images[0][fur]" . "fluffy")
 ("images[0][colour]" . "brown") ("images[1][0][fluff]" . "fluffy")
 ("images[1][1]" . "pos") ("images[1][2]" . "foo") ("images[1][3]" . "bar")
 ("cats[0]" . "brown") ("cats[1]" . "white") ("cats[2]" . "black")
 ("fur" . "fluffy") ("colour" . "brown"))
```
`ec` now also accepts hash-tables and will attempt to convert them into the correctly encoded format for Stripe. You can even combine lists written in the basic DSL I wrote with hash-tables to produce one large alist to pass to Stripe.

## Webhooks

To verify the webhooks from Stripe you need to follow the instructions here:
https://stripe.com/docs/webhooks/signatures

Extract the raw-body, the signature (v1), and the timestamp then 
pass them as arguments to `verify-signature`. This returns a boolean (t or nil) 
to tell you if it validated and the time difference between the timestamp received 
and `local-time:now`

`verify-webhook` works with a `lack.request` or a `tbnl:request`
in my example I use it in much the same way as in Paypal.

```lisp
(defmethod handle-webhook (client processor request)
  "Handles a webhook REQUEST properly by validating it for each processor, 
then constructing an internal representation of the hook, preprocessing it and then 
responding to it."
  (destructuring-bind (&key validp raw &allow-other-keys)
      (validate-received-webhook client processor request)
    (if (and validp raw)
        ....)))

(defmethod validate-received-webhook (client (processor stripe) req)
  (multiple-value-bind (valid-hook-p time-dif raw)
      (satmw:verify-webhook (webhook-secret processor) req)
    (declare (ignore time-dif))
    (list :validp valid-hook-p :raw raw))
```
(please note that PROCESSOR above is not the same as the definition within this library)

# BTCPay

Docs are coming soon. I have wrapped the endpoints but I have not integrated it yet
so I do not know if it works properly.






# Coinpayments

This has not been integrated into lisp-pay and I have no plans to do it. I suggest
you use your own version of BTCPayserver instead.

## cl-coinpayments
This is simply a helper library for using working with the original version of the 
coinpayment.net API, there is a new version of the API in the works.


## Intro
The coinpayment IPN (Instant Payment Notification) system sends messages to a listening 
server to inform the server of activity within their coinpayment account, whether this is 
to inform the user of a processed payment, whether a payment has failed etc. 
You can see the current docs on IPN's here: https://www.coinpayments.net/merchant-tools-ipn
this library just helps with managing the IPN's you receive.

### Quick notes

The post data sent from the coinpayment server is formatted in snake case ie 
`i_am_a_variable` but when you convert the data into a plist representing an IPN, 
all the snake_case keys are converted to kebab case ie i-am-a-variable, so 
`ipn_type` would be the keyword `:IPN-TYPE`. Parsing is memoized so it should be
pretty fast.

## Working with IPN's

This section is talking about the IPN's, the next is about API calls.

### Parsing the IPN's

There is a generic function called `(parse-data data)` which accepts a list, a string or an 
array, with this it attempts to convert it to a PLIST.

```lisp
#(97 109 111 117 110 116 61 49 48 46 48 48 48 48 48 48 48 48 38 97 109 111 117
  110 116 105 61 49 48 48 48 48 48 48 48 48 48 38 99 117 114 114 101 110 99 121
  61 76 84 67 84 38 102 101 101 61 48 46 48 48 48 48 48 48 48 48 38 102 101 101
  105 61 48 38 102 105 97 116 95 97 109 111 117 110 116 61 49 49 48 56 46 48 51
  48 50 57 49 53 48 38 102 105 97 116 95 97 109 111 117 110 116 105 61 49 49 48
  56 48 51 48 50 57 49 53 48 38 102 105 97 116 95 99 111 105 110 61 71 66 80 38
  102 114 111 109 61 56 97 99 57 53 50 56 100 49 57 49 102 56 102 98 51 102 53
  98 57 100 97 97 50 53 53 51 56 99 52 97 101 38 105 112 110 95 105 100 61 102
  54 48 102 55 51 57 49 101 99 54 48 54 100 50 54 51 49 101 54 100 53 51 49 53
  55 57 48 54 100 98 102 38 105 112 110 95 109 111 100 101 61 104 109 97 99 38
  105 112 110 95 116 121 112 101 61 116 114 97 110 115 102 101 114 38 105 112
  110 95 118 101 114 115 105 111 110 61 49 46 48 38 109 101 114 99 104 97 110
  116 61 54 57 56 57 48 54 51 98 99 53 101 48 102 52 51 99 49 51 57 102 56 100
  100 102 101 48 101 55 49 100 98 57 38 115 116 97 116 117 115 61 50 38 115 116
  97 116 117 115 95 116 101 120 116 61 67 111 109 112 108 101 116 101 38 116
  120 110 95 105 100 61 67 84 70 70 51 84 77 74 73 57 79 74 65 73 74 56 83 83
  77 74 86 75 69 66 72 74)
CL-COINPAYMENTS> (parse-data *)
(:AMOUNT "10.00000000" :AMOUNTI "1000000000" :CURRENCY "LTCT" :FEE "0.00000000"
 :FEEI "0" :FIAT-AMOUNT "1108.03029150" :FIAT-AMOUNTI "110803029150" :FIAT-COIN
 "GBP" :FROM "8ac9528d191f8fb3f5b9daa25538c4ae" :IPN-ID
 "f60f7391ec606d2631e6d53157906dbf" :IPN-MODE "hmac" :IPN-TYPE "transfer" ... )
 ```

### Verifying the source

coinpayment signs all its messages from the API with a HMAC header, with Hunchentoot you 
can extract that header like so: 

```lisp
 (let* ((headers (tbnl:headers-in*))
        (hmac (cdr (assoc :hmac headers))))
     hmac)
 ```
 
Now using the method `(verify-data hmac private-key raw-post)`
you can parse this header your IPN Secret, this is 
not your API secret key but the key *you* provided as a 'secret' 
"Your IPN Secret is a string of your choosing that is used to verify that an IPN was really sent from our servers "

and either the parsed plist, the raw-data or a string consisting of post parameters like so:

```lisp
(hunchentoot:define-easy-handler (ipn :uri "/ipnreceiver" )
    ()
  (let* ((headers (tbnl:headers-in*))
         (hmac (cdr (assoc :hmac headers)))
         (raw-data (tbnl:raw-post-data))
         (plist (cl-coinpayments::parse-data (tbnl:post-parameters*))))
    (when (and (string= (getf plist :merchant) *coinpayment-merchant-id*)
               (cl-coinpayments::verify-data hmac *coinpayment-ipn-secret*
                                             raw-data))
      (let ((status (cl-coinpayments:construct-status plist)))
        (handler-case 
            (cl-coinpayments:dispatch-ipn-by-name process plist status)
          (cl-coinpayments:no-dispatcher-found (c)
            (log:warn "Received an IPN that wasn't handled explicitly. Status: ~A~%IPN: ~A"
                      (cl-coinpayments:status c)
                      (cl-coinpayments:ipn c)))))))
  "done")
```

In the above example I also confirm that my merchants ID and the merchants ID sent are
the same.

## Working with the IPN

Now you have verified the legitimacy of your IPN you can construct a status object.

```lisp
CL-COINPAYMENTS> (construct-status *)
#<TWO {10196A08D3}>
(:AMOUNT "10.00000000" :AMOUNTI "1000000000" :CURRENCY "LTCT" :FEE "0.00000000"
 :FEEI "0" :FIAT-AMOUNT "1108.03029150" :FIAT-AMOUNTI "110803029150" :FIAT-COIN
 "GBP" :FROM "8ac9528d191f8fb3f5b9daa25538c4ae" :IPN-ID
 "f60f7391ec606d2631e6d53157906dbf" :IPN-MODE "hmac" :IPN-TYPE "transfer"
 :IPN-VERSION "1.0" :MERCHANT "oof" :STATUS "2" :STATUS-TEXT "Complete" :TXN-ID
 "CTFF3TMJI9OJAIJ8SSMJVKEBHJ")
 ```
 
This returns two values, with the most easily referenced being the new status object.
The status objects rules are described here: https://www.coinpayments.net/merchant-tools-ipn 
Under the heading 'Payment Statuses'.
If the status cannot be determined (which probably means the request is bogus) 
the condition 'unknown-status is signalled.

 If you look in classes.lisp you can see the 
relationship between the status objects, CLOS is used to represent the relationship 
described in those docs, so the class one-hundred is a subclass of ipn-payment-success
and ipn-payment-success is a subclass of ipn-status etc, this is important for the next
part.
 
### Dispatching on statuses

This library has a means of creating functions that are executed based on the 
name of the dispatcher, the type of IPN sent from the server and the two main args the
class of the STATUS object and the number of arguments.
This is best demonstrated with an example:
```lisp

(def-ipn-dispatcher print-info ((foo :transfer) (ipn ipn-status) arg1)
  (print foo)
  (print ipn)
  (print "less specific")
  (print arg1))

(def-ipn-dispatcher print-info ((foo :transfer) (ipn ipn-status) arg1 arg2)
  (print foo)
  (print ipn)
  (print "less specific")
  (print arg1)
  (print arg2))

(def-ipn-dispatcher print-info ((foo :transfer) (ipn two) arg1)
  (print foo)
  (print ipn)
  (print "more specific"))

(def-ipn-dispatcher print-info ((foo :transfer) (ipn two) arg1 arg2)
  (print arg1)
  (print arg2))

  ```
Some example inputs using the function `(ipn-dispatch name ipn status args)`
```
CL-COINPAYMENTS> (ipn-dispatch 'print-info '(:IPN-TYPE "transfer") (make-instance 'zero) "abc")

(:IPN-TYPE "transfer") 
#<ZERO {1025E64EB3}> 
"less specific" 
"abc" 
"abc"


CL-COINPAYMENTS> (ipn-dispatch 'print-info '(:IPN-TYPE "transfer") (make-instance 'zero) "abc" "deeef")

(:IPN-TYPE "transfer") 
#<ZERO {1025E852C3}> 
"less specific" 
"abc" 
"deeef" 
"deeef"


CL-COINPAYMENTS> (ipn-dispatch 'print-info '(:IPN-TYPE "transfer") (make-instance 'two) "abc" "deeef")

"abc" 
"deeef" 
"deeef"

```
I assume you get the jist. Anyway you can use this to perform actions based the same types 
of IPN's but when they are in varying states.
There is the macro `(dispatch-ipn-by-name name ipn status args)` which does the same 
```lisp
CL-COINPAYMENTS> (dispatch-ipn-by-name print-info '(:IPN-TYPE "transfer") (make-instance 'ipn-status) "abc" "def")

(:IPN-TYPE "transfer") 
#<IPN-STATUS {1025E8F173}> 
"less specific" 
"abc" 
"def" 
"def"
```

If a dispatcher cannot be found for the args provided then the condition 
'no-dispatcher-found is signalled, you should wrap all calls to dispatch-ipn and 
dispatch-ipn-by-name within a handler-case to make sure you have some fallback 
functionality in the case the server sends something unexpected, which it has done for me...

Just a note the list '(:IPN-TYPE "transfer") is the most basic of IPN's represented as a
plist that is required for this to function, this is why it is used in the examples, in 
a real world example the IPN would be what has been received and parsed by (parse-data ..)

## Working with the API.

In cl-coinpayments API requests are all represented by objects, so if you wish to make 
an API call you instantiate an object of the request type, pass in the correct arguments
as initargs and then call the method `(request <instance>)` on it. Its important to note
that the :merchant-secret-key initarg is the special key *YOU* gave to coinpayments as your
secret, not the API secret automatically generated and :key is your API public key.

```lisp

CL-COINPAYMENTS> (make-instance 'get-basic-info :merchant-secret-key *coinpayment-private* :key *coinpayment-public*)
#<GET-BASIC-INFO {100230D643}>

```
This request has the following slots:
```lisp
[ ]  CMD                 = "get_basic_info"
[ ]  DEX-ALIST           = (("version" . "1") ("key" . <removed for privacy>) ("cmd" . "get_basic_info") ("format" . "json"))
[ ]  FORMAT              = "json"
[ ]  HMAC                = "c776c7821d4d7c785c5971652e6139ab5499ca3f7af195e8bd4dc5b56aeae91be8185e42a98d2af843bfa8a88062ac76362a5759f2cbdc612b9a6ece2a37a478"
[ ]  KEY                 = <removed for privacy>
[ ]  MERCHANT-SECRET-KEY = <removed for privacy>
[ ]  NONCE               = #<unbound>
[ ]  POST-STRING         = "version=1&key=<removed for privacy>&cmd=get_basic_info&format=json"
[ ]  REQUIRED            = (MERCHANT-SECRET-KEY KEY)
[ ]  VERSION             = "1"
```

When a response is received it is parsed into either a good-response or bad-response object:
```lisp
CL-COINPAYMENTS> (make-instance 'get-basic-info :merchant-secret-key *coinpayment-private* :key *coinpayment-public*)
#<GET-BASIC-INFO {101A377823}>
CL-COINPAYMENTS> (request *)
#<BAD-RESPONSE {101B67EC63}>
CL-COINPAYMENTS> (make-instance 'currency-prices :merchant-secret-key *coinpayment-private* :key *coinpayment-public*)
#<CURRENCY-PRICES {101B8E9323}>
CL-COINPAYMENTS> (request *)
#<GOOD-RESPONSE {101879EC63}>
CL-COINPAYMENTS> 
```
The slots look like this:
```lisp
[ ]  DEX-EXTRA   = (:CODE 200 :HEADERS #<HASH-TABLE :TEST EQUAL :COUNT 11 {1001CF6FB3}> :URL #<QURI.URI.HTTP:URI-HTTPS https://www.coinpayments.net/api.php> :STREAM #<CL+SSL::SSL-STREAM for #<FD-STREAM for "socket 192.168.200.16:49534, peer: 205.220.231.4:443" {1001B3AF73}>>)
[ ]  ERROR-SLOT  = "This API Key does not have permission to use that command!"
[ ]  REQUEST     = #<GET-BASIC-INFO {10018804D3}>
[ ]  RESULT-SLOT = NIL
```
All of the values returned by dex:post that are not directly used are stored in 
DEX-EXTRA, RESULT-SLOT contains any parsed results, ERROR-SLOT will either be 'ok' or 
a descriptive string, and REQUEST is the object used to send the request.

All requests inherit their slots from the toplevel class `'request`, currently 
the nonce is not used but if you wanted to use it you can, if you set the value to an 
integer represented as a string then it will also be added into the post params and the HMAC.
The same goes for if you modify any of the slots that are used for storing values 
which are later used in the post request. You could change FORMAT to XML, however 
that would probably break `(request ..)` so maybe dont do that.

Each request within the docs https://www.coinpayments.net/apidoc-intro has its own class
and they are all listed in api-forms.lisp. 
If you try to make a request object without providing 'required' arguments then
you will get an error of type `'required-slots-not-bound' 
```lisp
CL-COINPAYMENTS> (make-instance 'create-transfer :merchant-secret-key *coinpayment-private* :key *coinpayment-public* :amount "1" :currency "btc")
; Debugger entered on #<REQUIRED-SLOTS-NOT-BOUND {1002506873}>
[1] CL-COINPAYMENTS> 
; Evaluation aborted on #<REQUIRED-SLOTS-NOT-BOUND {1002506873}>
```

I think that all of the API calls will work except maybe `create-mass-withdrawal` which 
I have not tested but it has some odd characteristics, so dont rely on it to work. 
If you do test it and find it doesn't work just open an issue and I'll fix it. 

