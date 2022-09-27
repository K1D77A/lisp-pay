(in-package #:btcpay)

;;;webhooks https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Webhooks

(defapi webhooks%all ("/api/v1/stores/:store-id/webhooks" get-request)
        ())

(defapi webhooks%create ("/api/v1/stores/:store-id/webhooks" post-request)
        ())

(defapi webhooks%get ("/api/v1/stores/:store-id/webhooks/:webhook-id" get-request)
        ())

(defapi webhooks%update ("/api/v1/stores/:store-id/webhooks/:webhook-id" put-request)
        ())

(defapi webhooks%delete ("/api/v1/stores/:store-id/webhooks/:webhook-id" delete-request)
        ())

(defapi webhooks%latest-deliveries
    ("/api/v1/stores/:store-id/webhooks/:webhook-id/deliveries" get-request)
    ((count
      :initarg :count
      :type string)))

(defapi webhooks%get-delivery-info
    ("/api/v1/stores/:store-id/webhooks/:webhook-id/deliveries/:delivery-id" get-request)
    ())

(defapi webhooks%get-delivery-object
    ("/api/v1/stores/:store-id/webhooks/:webhook-id/deliveries/:delivery-id/request"
     get-request)
    ())

(defapi webhooks%redeliver-object
    ("/api/v1/stores/:store-id/webhooks/:webhook-id/deliveries/:delivery-id/redeliver"
     post-request)
    ())


;;;api keys https://docs.btcpayserver.org/API/Greenfield/v1/#tag/API-Keys

(defapi api-keys%delete ("/api/v1/api-keys/:api-key" delete-request)
        ())

(defapi api-keys%get ("/api/v1/api-keys/current" get-request)
        ())

(defapi api-keys%revoke ("/api/v1/api-keys/current" delete-request)
        ())

(defapi api-keys%new ("/api/v1/api-keys" post-request)
        ())

;;;Apps https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Apps

(defapi apps%new ("/api/v1/stores/:store-id/apps/pos" post-request)
        ())

(defapi apps%get ("/api/v1/apps/:app-id" get-request)
        ())

(defapi apps%delete ("/api/v1/apps/:app-id" delete-request)
        ())

;;;authorization https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Authorization

(defapi authorize%authorize ("/api-keys/authorize" get-request)
        ((permissions
          :initarg :permissions
          :type array)
         (application-name
          :initarg :application-name
          :as-string "applicationName")
         (strict
          :initarg :strict
          :initform t
          :type bool)
         (selective-stores
          :initarg :selective-stores
          :initform nil
          :as-string "selectiveStores"
          :type bool)
         (redirect
          :initarg :redirect
          :type string)
         (application-identifier
          :initarg :application-identifier
          :as-string "applicationIdentifier"
          :type string)))


;;;custodians https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Custodians

(defapi custodians%all ("/api/v1/custodians" get-request)
        ())

(defapi custodians%get ("/api/v1/stores/:store-id/custodian-accounts" get-request)
        ((asset-balances
          :initarg :asset-balances
          :as-string "assetBalances"
          :initform nil 
          :type bool)))

(defapi custodians%new ("/api/v1/stores/:store-id/custodian-accounts" post-request)
        ())


(defapi custodians%get-account ("/api/v1/stores/:store-id/custodian-accounts/:account-id"
                                get-request)
        ((asset-balances
          :initarg :asset-balances
          :as-string "assetBalances"
          :initform nil
          :type bool)))

(defapi custodians%update-account
    ("/api/v1/stores/:store-id/custodian-accounts/:account-id" put-request)
    ())

(defapi custodians%delete-account
    ("/api/v1/stores/:store-id/custodian-accounts/:account-id" delete-request)
    ())

(defapi custodians%get-trading-quote
    ("/api/v1/stores/:store-id/custodian-accounts/:account-id/trades/quote" get-request)
    ((from-asset
      :initarg :from-asset
      :as-string "fromAsset"
      :type string)
     (to-asset
      :initarg :to-asset
      :as-string "toAsset"
      :type string)))

(defapi custodians%trade-assets
    ("/api/v1/stores/:store-id/custodian-accounts/:account-id/trades/market"
     post-request))

(defapi custodians%get-depost-address
    ("/api/v1/stores/:store-id/custodian-accounts/:account-id/addresses/:payment-method"
     get-request))

(defapi custodians%withdraw-to-store-wallet
    ("/api/v1/stores/:store-id/custodian-accounts/:account-id/withdrawals"
     post-request))

(defapi custodians%get-withdrawal-info
    ("/api/v1/stores/:store-id/custodian-accounts/:account-id/withdrawals/:withdrawal-id"
     get-request))


;;;health https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Health

(defapi health%get ("/api/v1/health" get-request))

;;;invoices https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Invoices

(defapi invoices%all ("/api/v1/stores/:store-id/invoices" get-request))

(defapi invoices%create ("/api/v1/stores/:store-id/invoices" post-request))

(defapi invoices%get ("/api/v1/stores/:store-id/invoices/:invoice-id" get-request))

(defapi invoices%archive ("/api/v1/stores/:store-id/invoices/:invoice-id" delete-request))

(defapi invoices%update ("/api/v1/stores/:store-id/invoices/:invoice-id" put-request))

(defapi invoices%payment-methods
    ("/api/v1/stores/:store-id/invoices/:invoice-id/payment-methods" get-request)
    ((only-accounted-payments
      :initarg :only-accounted-payments
      :as-string "onlyAccountedPayments"
      :initform t
      :type bool)))

(defapi invoices%mark-status ("/api/v1/stores/:store-id/invoices/:invoice-id/status"
                              post-request)
        ())

(defapi invoices%unarchive ("/api/v1/stores/:store-id/invoices/:invoice-id/unarchive"
                            post-request)
        ())

(defapi invoices%activate-payment-method
    ("/api/v1/stores/:store-id/invoices/:invoice-id/payment-methods/:payment-method/activate"
     post-request)
    ())

;;;lightning-internal
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Lightning-(Internal-Node)

(defapi lightning-internal%info ("/api/v1/server/lightning/:crypto-code/info" get-request)
        ())

(defapi lightning-internal%balance
    ("/api/v1/server/lightning/:crypto-code/balance" get-request)
    ())

(defapi lightning-internal%connect
    ("/api/v1/server/lightning/:crypto-code/connect" post-request)
    ())

(defapi lightning-internal%get-channels
    ("/api/v1/server/lightning/:crypto-code/channels" get-request)
    ())

(defapi lightning-internal%open-channel
    ("/api/v1/server/lightning/:crypto-code/channels" post-request)
    ())

(defapi lightning-internal%deposit-address
    ("/api/v1/server/lightning/:crypto-code/address" post-request)
    ())

(defapi lightning-internal%get-payment
    ("/api/v1/server/lightning/:crypto-code/payments/:payment-hash" get-request)
    ())

(defapi lightning-internal%get-invoice
    ("/api/v1/server/lightning/:crypto-code/invoices/:invoice-id" get-request)
    ())

(defapi lightning-internal%pay-invoice
    ("/api/v1/server/lightning/:crypto-code/invoices/pay" post-request)
    ())


(defapi lightning-internal%create-invoice
    ("/api/v1/server/lightning/:crypto-code/invoices" post-request)
    ())

;;;lightning (store)
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Lightning-(Store)

(defapi lightning-store%info ("/api/v1/stores/:store-id/lightning/:crypto-code/info"
                              get-request)
        ())

(defapi lightning-store%balance
    ("/api/v1/stores/:store-id/lightning/:crypto-code/balance" get-request)
    ())

(defapi lightning-store%connect
    ("/api/v1/stores/:store-id/lightning/:crypto-code/connect" post-request)
    ())

(defapi lightning-store%get-channels
    ("/api/v1/stores/:store-id/lightning/:crypto-code/channels" get-request)
    ())

(defapi lightning-store%open-channel
    ("/api/v1/stores/:store-id/lightning/:crypto-code/channels" post-request)
    ())

(defapi lightning-store%deposit-address
    ("/api/v1/stores/:store-id/lightning/:crypto-code/address" post-request)
    ())

(defapi lightning-store%get-payment
    ("/api/v1/stores/:store-id/lightning/:crypto-code/payments/:payment-hash" get-request)
    ())

(defapi lightning-store%get-invoice
    ("/api/v1/stores/:store-id/lightning/:crypto-code/invoices/:invoice-id" get-request)
    ())

(defapi lightning-store%pay-invoice
    ("/api/v1/stores/:store-id/lightning/:crypto-code/invoices/pay" post-request)
    ())


(defapi lightning-store%create-invoice
    ("/api/v1/stores/:store-id/lightning/:crypto-code/invoices" post-request)
    ())

;;;misc https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Miscelleneous

(defapi misc%permission-metadata
    ("/misc/permissions" get-request)
    ())

(defapi misc%language-codes
    ("/misc/lang" get-request)
    ())

(defapi misc%invoice-checkout-page
    ("/i/:invoice-id" get-request)
    ((language
      :initarg :language
      :as-string "lang"
      :type string)))

;;;notifications
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#operation/Notifications_GetNotifications

(defapi notifications%all ("/api/v1/users/me/notifications" get-request)
        ((seen
          :initarg :seen
          :type string)
         (skip
          :initarg :skip
          :type fixnum)
         (take
          :initarg :take
          :type fixnum)))

(defapi notifications%get ("/api/v1/users/me/notifications/:notification-id" get-request)
        ())

(defapi notifications%update ("/api/v1/users/me/notifications/:notification-id" put-request)
        ())

(defapi notifications%remove ("/api/v1/users/me/notifications/:notification-id"
                              delete-request)
        ())


;;;payment requests
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Payment-Requests

(defapi payment-requests%all ("/api/v1/stores/:store-id/payment-requests" get-request)
        ())

(defapi payment-requests%create ("/api/v1/stores/:store-id/payment-requests" post-request)
        ())

(defapi payment-requests%get
    ("/api/v1/stores/:store-id/payment-requests/:payment-request-id" get-request)
    ())

(defapi payment-requests%archive
    ("/api/v1/stores/:store-id/payment-requests/:payment-request-id" delete-request)
    ())

(defapi payment-requests%update
    ("/api/v1/stores/:store-id/payment-requests/:payment-request-id" put-request)
    ())

;;;stores payout processors
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#operation/StorePayoutProcessors_GetStorePayoutProcessors

(defapi stores-payout-processors%all
    ("/api/v1/stores/:store-id/payout-processors" get-request)
    ())

(defapi store-payout-processors%remove
    ("/api/v1/stores/:store-id/payout-processors/:processor-id/:payment-method"
     delete-request)
    ())

(defapi stores-payout-processors%get-automated
    (#.(format nil "/api/v1/stores/:store-id/payout-processors/~
                    OnChainAutomatedTransferSenderFactory/:payment-method")
       get-request)
    ())

(defapi stores-payout-processors%update-automated
    (#.(format nil "/api/v1/stores/:store-id/payout-processors/~
                    OnChainAutomatedTransferSenderFactory/:payment-method")
       put-request)
    ())

(defapi stores-payout-processors%get-lightning-automated
    (#.(format nil "/api/v1/stores/:store-id/payout-processors/~
                    LightningAutomatedTransferSenderFactory/:payment-method")
       get-request)
    ())

(defapi stores-payout-processors%update-lightning-automated
    (#.(format nil "/api/v1/stores/:store-id/payout-processors/~
                    LightningAutomatedTransferSenderFactory/:payment-method")
       put-request)
    ())

(defapi stores-payout-processors%all-automated
    (#.(format nil "/api/v1/stores/:store-id/payout-processors/~
                    OnChainAutomatedTransferSenderFactory")
       get-request)
    ())

(defapi stores-payout-processors%update-all-automated
    (#.(format nil "/api/v1/stores/:store-id/payout-processors/~
                    OnChainAutomatedTransferSenderFactory")
       put-request)
    ())

(defapi stores-payout-processors%all-lightning-automated
    (#.(format nil "/api/v1/stores/:store-id/payout-processors/~
                    LightningAutomatedTransferSenderFactory")
       get-request)
    ())

;;;payout processors 
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#operation/PayoutProcessors_GetPayoutProcessors

(defapi payout-processors%all 
    ("/api/v1/payout-processors" get-request)
    ())

;;;pull payments management
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Pull-payments-(Management)

(defapi stores-pull-payments%all
    ("/api/v1/stores/:store-id/pull-payments"
     get-request)
    ((include-archived
      :initarg :include-archived
      :as-string "includeArchived"
      :initform nil
      :type bool)))

(defapi stores-pull-payments%create
    ("/api/v1/stores/:store-id/pull-payments"
     post-request))

(defapi stores-pull-payments%archive
    ("/api/v1/stores/:store-id/pull-payments/:pull-payment-id"
     delete-request))

;;;pull payments public
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Pull-payments-(Public)

(defapi public-pull-payments%get
    ("/api/v1/pull-payments/:pull-payment-id"
     get-request))

(defapi public-pull-payments%get-payouts
    ("/api/v1/pull-payments/:pull-payment-id/payouts"
     get-request)
    ((include-cancelled
      :initarg :include-cancelled
      :as-string "includeCancelled"
      :initform nil
      :type bool)))

(defapi public-pull-payments%create-payout
    ("/api/v1/pull-payments/:pull-payment-id/payouts"
     post-request))

(defapi public-pull-payments%get-payout 
    ("/api/v1/pull-payments/:pull-payment-id/payouts/:payout-id"
     get-request))

;;;there is another API here but it is exactly the same as the one above...
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#operation/PullPayments_GetPayout


;;;stores payouts
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Stores-(Payouts)

(defapi store-payouts%create
    ("/api/v1/stores/:store-id/payouts"
     post-request))

(defapi store-payouts%all
    ("/api/v1/stores/:store-id/payouts"
     get-request))

(defapi store-payouts%approve
    ("/api/v1/stores/:store-id/payouts/:payout-id"
     post-request))

(defapi store-payouts%cancel
    ("/api/v1/stores/:store-id/payouts/:payout-id"
     delete-request))

(defapi store-payouts%mark-paid
    ("/api/v1/stores/:store-id/payouts/:payout-id"
     post-request))

;;;server info
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/ServerInfo

(defapi server-info%all
    ("/api/v1/server/info"
     get-request))

;;;stores email
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Stores-(Email)

(defapi server-email%all
    ("/api/v1/stores/:store-id/email"
     get-request))

(defapi server-email%update
    ("/api/v1/stores/:store-id/email"
     put-request))

(defapi server-email%send
    ("/api/v1/stores/:store-id/email/send"
     post-request))

;;;store payment methods
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Store-Payment-Methods

(defapi store-payment-methods%all
    ("/api/v1/stores/:store-id/payment-methods"
     get-request))

;;;store payment methods lightning
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Store-Payment-Methods-(Lightning-Network)

(defapi store-payment-methods-lightning%all
    ("/api/v1/stores/:store-id/payment-methods/LightningNetwork"
     get-request))

(defapi store-payment-methods-lightning%get
    ("/api/v1/stores/:store-id/payment-methods/LightningNetwork/:crypto-code"
     get-request))

(defapi store-payment-methods-lightning%update
    ("/api/v1/stores/:store-id/payment-methods/LightningNetwork/:crypto-code"
     put-request))

(defapi store-payment-methods-lightning%remove
    ("/api/v1/stores/:store-id/payment-methods/LightningNetwork/:crypto-code"
     delete-request))

;;;store payment methods lnurl
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Store-Payment-Methods-(LNURL-Pay)
(defapi store-payment-methods-lnurl%all
    ("/api/v1/stores/:store-id/payment-methods/LNURL"
     get-request))

(defapi store-payment-methods-lnurl%get
    ("/api/v1/stores/:store-id/payment-methods/LNURL/:crypto-code"
     get-request))

(defapi store-payment-methods-lnurl%update
    ("/api/v1/stores/:store-id/payment-methods/LNURL/:crypto-code"
     put-request))

(defapi store-payment-methods-lnurl%remove
    ("/api/v1/stores/:store-id/payment-methods/LNURL/:crypto-code"
     delete-request))





;;;store payment methods on chain
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Store-Payment-Methods-(On-Chain)

(defapi store-payment-methods-on-chain%all
    ("/api/v1/stores/:store-id/payment-methods/OnChain"
     get-request))

(defapi store-payment-methods-on-chain%get
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code"
     get-request))

(defapi store-payment-methods-on-chain%update
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code"
     put-request))

(defapi store-payment-methods-on-chain%remove
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code"
     delete-request))

(defapi store-payment-methods-on-chain%generate-wallet
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code/generate"
     post-request))

(defapi store-payment-methods-on-chain%preview
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code/preview"
     get-request)
    ((offset
      :initarg :offset
      :type fixnum)
     (amount
      :initarg :amount
      :type fixnum)))

(defapi store-payment-methods-on-chain%preview-proposed
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code/preview"
     post-request)
    ((offset
      :initarg :offset
      :type fixnum)
     (amount
      :initarg :amount
      :type fixnum)))

;;;stores users
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Stores-(Users)

(defapi stores-users%all
    ("/api/v1/stores/:store-id/users"
     get-request))

(defapi stores-users%add
    ("/api/v1/stores/:store-id/users"
     post-request))

(defapi stores-users%remove
    ("/api/v1/stores/:store-id/users/:user-id"
     delete-request))








