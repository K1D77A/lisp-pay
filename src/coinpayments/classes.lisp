(in-package #:lisp-pay/coinpayments)

(defclass coinpayment-response (response)
  ((error-slot
    :accessor error-slot
    :initarg :error-slot
    :documentation "The parsed 'error' slot.")
   (result-slot
    :accessor result-slot
    :initarg :result-slot
    :documentation "The parsed result.")
   (dex-extra
    :accessor dex-extra
    :initarg :dex-extra
    :documentation "The other values returned by dex:post")
   (request
    :accessor request
    :initarg :request
    :documentation "the request object."))
  (:documentation "A class for putting the parsed results."))

(defclass good-response (response)
  ())

(defclass bad-response (response)
  ())

(defmethod print-object ((obj bad-response) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "ERROR: ~A"
            (when (slot-boundp obj 'error-slot)
              (error-slot obj)))))

(defclass coinpayment-request ()
  ((version
    :accessor version
    :initarg :version
    :initform "1"
    :type string
    :documentation "The API version.")
   (dex-alist
    :accessor dex-alist
    :type list
    :documentation "A computed a list of all post vars for sending as form-urlencoded.")
   (post-string
    :accessor post-string
    :type string
    :documentation "The computed post string.")
   (merchant-secret-key
    :accessor merchant-secret-key
    :initarg :merchant-secret-key
    :type string
    :documentation "The private key used to sign the requests.")
   (key
    :accessor key
    :initarg :key
    :type string
    :documentation "The users public key")
   (cmd
    :accessor cmd
    :initarg :cmd
    :type string
    :documentation "The API being called.")
   (nonce
    :accessor nonce
    :initarg :nonce
    :type string
    :documentation "An optional nonce that should be one higher than the previous")
   (format
    :accessor response-format
    :initarg :format
    :initform "json"
    :type string
    :documentation "The response format. Default JSON.")
   (hmac
    :accessor hmac
    :initarg :hmac
    :type string
    :documentation "The computed hmac of the request.")
   (required
    :accessor required
    :initarg :required
    :initform nil
    :type list
    :documentation "A list of required slots."))
  (:documentation "The base class for all API requests."))


(defclass ipn-status ()
  ())

(defclass ipn-failure (ipn-status)
  ())

(defclass ipn-payment-pending (ipn-status)
  ())

(defclass ipn-payment-success (ipn-status)
  ())

(defclass negative-2 (ipn-failure)
  ()
  (:documentation "Paypal Refund or Reversal"))

(defclass negative-1 (ipn-failure)
  ()
  (:documentation "Cancelled/Timed out"))

(defclass zero (ipn-payment-pending)
  ()
  (:documentation "Waiting for buyer funds."))

(defclass one (ipn-payment-pending)
  ()
  (:documentation "We have confirmed coin reception from the buyer."))

(defclass two (ipn-payment-pending)
  ()
  (:documentation "queued for nightly payout. (if you have Payout Mode for this coin 
set to Nightly)"))

(defclass three (ipn-payment-pending)
  ()
  (:documentation "PayPal Pending (eChecks of other types of holds)"))

(defclass five (ipn-payment-pending)
  ()
  (:documentation "In Escrow (if you are using SetEscrow)"))

(defclass one-hundred (ipn-payment-success)
  ()
  (:documentation "Payment complete"))
