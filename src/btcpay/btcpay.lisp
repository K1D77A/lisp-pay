(in-package #:btcpay)

;;;webhooks https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Webhooks

(defapi stores-webhooks%all
    ("/api/v1/stores/:store-id/webhooks" get-request)
    ())

(defapi stores-webhooks%create
    ("/api/v1/stores/:store-id/webhooks" post-request)
    ())

(defapi stores-webhooks%get
    ("/api/v1/stores/:store-id/webhooks/:webhook-id" get-request)
    ())

(defapi stores-webhooks%update
    ("/api/v1/stores/:store-id/webhooks/:webhook-id" put-request)
    ())

(defapi stores-webhooks%delete
    ("/api/v1/stores/:store-id/webhooks/:webhook-id"
     delete-request)
    ())

(defapi stores-webhooks%latest-deliveries
    ("/api/v1/stores/:store-id/webhooks/:webhook-id/deliveries" get-request)
    ((count
      :initarg :count
      :type string)))

(defapi stores-webhooks%get-delivery-info
    ("/api/v1/stores/:store-id/webhooks/:webhook-id/deliveries/:delivery-id" get-request)
    ())

(defapi stores-webhooks%get-delivery-object
    ("/api/v1/stores/:store-id/webhooks/:webhook-id/deliveries/:delivery-id/request"
     get-request)
    ())

(defapi stores-webhooks%redeliver-object
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

(defapi stores-apps%new ("/api/v1/stores/:store-id/apps/pos" post-request)
        ())

(defapi stores-apps%get ("/api/v1/apps/:app-id" get-request)
        ())

(defapi stores-apps%delete ("/api/v1/apps/:app-id" delete-request)
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

(defapi stores-custodians%all ("/api/v1/custodians" get-request)
        ())

(defapi stores-custodians%get ("/api/v1/stores/:store-id/custodian-accounts" get-request)
        ((asset-balances
          :initarg :asset-balances
          :as-string "assetBalances"
          :initform nil 
          :type bool)))

(defapi stores-custodians%new ("/api/v1/stores/:store-id/custodian-accounts" post-request)
        ())

(defapi stores-custodians%get-account
    ("/api/v1/stores/:store-id/custodian-accounts/:account-id"
     get-request)
    ((asset-balances
      :initarg :asset-balances
      :as-string "assetBalances"
      :initform nil
      :type bool)))

(defapi stores-custodians%update-account
    ("/api/v1/stores/:store-id/custodian-accounts/:account-id" put-request)
    ())

(defapi stores-custodians%delete-account
    ("/api/v1/stores/:store-id/custodian-accounts/:account-id" delete-request)
    ())

(defapi stores-custodians%get-trading-quote
    ("/api/v1/stores/:store-id/custodian-accounts/:account-id/trades/quote" get-request)
    ((from-asset
      :initarg :from-asset
      :as-string "fromAsset"
      :type string)
     (to-asset
      :initarg :to-asset
      :as-string "toAsset"
      :type string)))

(defapi stores-custodians%trade-assets
    ("/api/v1/stores/:store-id/custodian-accounts/:account-id/trades/market"
     post-request))

(defapi stores-custodians%get-depost-address
    ("/api/v1/stores/:store-id/custodian-accounts/:account-id/addresses/:payment-method"
     get-request))

(defapi stores-custodians%withdraw-to-store-wallet
    ("/api/v1/stores/:store-id/custodian-accounts/:account-id/withdrawals"
     post-request))

(defapi stores-custodians%get-withdrawal-info
    ("/api/v1/stores/:store-id/custodian-accounts/:account-id/withdrawals/:withdrawal-id"
     get-request))


;;;health https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Health

(defapi health%get ("/api/v1/health" get-request))

;;;invoices https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Invoices

(defapi stores-invoices%all ("/api/v1/stores/:store-id/invoices" get-request))

(defapi stores-invoices%create ("/api/v1/stores/:store-id/invoices" post-request))

(defapi stores-invoices%get ("/api/v1/stores/:store-id/invoices/:invoice-id" get-request))

(defapi stores-invoices%archive
    ("/api/v1/stores/:store-id/invoices/:invoice-id" delete-request))

(defapi stores-invoices%update ("/api/v1/stores/:store-id/invoices/:invoice-id" put-request))

(defapi stores-invoices%payment-methods
    ("/api/v1/stores/:store-id/invoices/:invoice-id/payment-methods" get-request)
    ((only-accounted-payments
      :initarg :only-accounted-payments
      :as-string "onlyAccountedPayments"
      :initform t
      :type bool)))

(defapi stores-invoices%mark-status ("/api/v1/stores/:store-id/invoices/:invoice-id/status"
                                     post-request)
        ())

(defapi stores-invoices%unarchive ("/api/v1/stores/:store-id/invoices/:invoice-id/unarchive"
                                   post-request)
        ())

(defapi stores-invoices%activate-payment-method
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

(defapi stores-lightning%info ("/api/v1/stores/:store-id/lightning/:crypto-code/info"
                               get-request)
        ())

(defapi stores-lightning%balance
    ("/api/v1/stores/:store-id/lightning/:crypto-code/balance" get-request)
    ())

(defapi stores-lightning%connect
    ("/api/v1/stores/:store-id/lightning/:crypto-code/connect" post-request)
    ())

(defapi stores-lightning%get-channels
    ("/api/v1/stores/:store-id/lightning/:crypto-code/channels" get-request)
    ())

(defapi stores-lightning%open-channel
    ("/api/v1/stores/:store-id/lightning/:crypto-code/channels" post-request)
    ())

(defapi stores-lightning%deposit-address
    ("/api/v1/stores/:store-id/lightning/:crypto-code/address" post-request)
    ())

(defapi stores-lightning%get-payment
    ("/api/v1/stores/:store-id/lightning/:crypto-code/payments/:payment-hash" get-request)
    ())

(defapi stores-lightning%get-invoice
    ("/api/v1/stores/:store-id/lightning/:crypto-code/invoices/:invoice-id" get-request)
    ())

(defapi stores-lightning%pay-invoice
    ("/api/v1/stores/:store-id/lightning/:crypto-code/invoices/pay" post-request)
    ())


(defapi stores-lightning%create-invoice
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

(defapi stores-payment-methods-lightning%all
    ("/api/v1/stores/:store-id/payment-methods/LightningNetwork"
     get-request))

(defapi stores-payment-methods-lightning%get
    ("/api/v1/stores/:store-id/payment-methods/LightningNetwork/:crypto-code"
     get-request))

(defapi stores-payment-methods-lightning%update
    ("/api/v1/stores/:store-id/payment-methods/LightningNetwork/:crypto-code"
     put-request))

(defapi stores-payment-methods-lightning%remove
    ("/api/v1/stores/:store-id/payment-methods/LightningNetwork/:crypto-code"
     delete-request))

;;;store payment methods lnurl
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Store-Payment-Methods-(LNURL-Pay)
(defapi stores-payment-methods-lnurl%all
    ("/api/v1/stores/:store-id/payment-methods/LNURL"
     get-request))

(defapi stores-payment-methods-lnurl%get
    ("/api/v1/stores/:store-id/payment-methods/LNURL/:crypto-code"
     get-request))

(defapi stores-payment-methods-lnurl%update
    ("/api/v1/stores/:store-id/payment-methods/LNURL/:crypto-code"
     put-request))

(defapi stores-payment-methods-lnurl%remove
    ("/api/v1/stores/:store-id/payment-methods/LNURL/:crypto-code"
     delete-request))





;;;store payment methods on chain
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Store-Payment-Methods-(On-Chain)

(defapi stores-payment-methods-on-chain%all
    ("/api/v1/stores/:store-id/payment-methods/OnChain"
     get-request))

(defapi stores-payment-methods-on-chain%get
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code"
     get-request))

(defapi stores-payment-methods-on-chain%update
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code"
     put-request))

(defapi stores-payment-methods-on-chain%remove
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code"
     delete-request))

(defapi stores-payment-methods-on-chain%generate-wallet
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code/generate"
     post-request))

(defapi stores-payment-methods-on-chain%preview
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code/preview"
     get-request)
    ((offset
      :initarg :offset
      :type fixnum)
     (amount
      :initarg :amount
      :type fixnum)))

(defapi stores-payment-methods-on-chain%preview-proposed
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


;;;store wallet on chain
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Store-Wallet-(On-Chain)

(defapi stores-wallet-on-chain%all
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code/wallet"
     get-request))

(defapi stores-wallet-on-chain%fee-rate
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code/wallet/fee-rate"
     get-request)
    ((block-target
      :initarg :block-target
      :as-string "blockTarget"
      :type number)))

(defapi stores-wallet-on-chain%address
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code/wallet/address"
     get-request)
    ((force-generate
      :initarg :force-generate
      :as-string "forceGenerate"
      :initform nil
      :type bool)))

(defapi stores-wallet-on-chain%unreserve
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code/wallet/address"
     delete-request))

(defapi stores-wallet-on-chain%transactions
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code/wallet/transactions"
     get-request)
    ((status-filter
      :initarg :status-filter
      :as-string "statusFilter"
      :type array)
     (label-filter
      :initarg :label-filter
      :as-string "labelFilter"
      :type string)
     (skip
      :initarg :skip
      :type integer)
     (limit
      :initarg :limit 
      :type integer)))

(defapi stores-wallet-on-chain%create-wallet-transaction
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code/wallet/transactions"
     post-request))

(defapi stores-wallet-on-chain%get-transaction
    (#.(format nil "/api/v1/stores/:store-id/payment-methods/OnChain/~
                    :crypto-code/wallet/transactions/:transaction-id")
       get-request))

(defapi stores-wallet-on-chain%patch-transaction-info
    (#.(format nil "/api/v1/stores/:store-id/payment-methods/OnChain/~
                    :crypto-code/wallet/transactions/:transaction-id")
       patch-request)
    ((force
      :initarg :force
      :type string)))

(defapi stores-wallet-on-chain%get-utxos
    ("/api/v1/stores/:store-id/payment-methods/OnChain/:crypto-code/wallet/utxos"
     get-request))

;;;stores
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Stores

(defapi stores%all
    ("/api/v1/stores"
     get-request))

(defapi stores%create
    ("/api/v1/stores"
     post-request))

(defapi stores%get
    ("/api/v1/stores/:store-id"
     get-request))

(defapi stores%update
    ("/api/v1/stores/:store-id"
     put-request))

(defapi stores%remove
    ("/api/v1/stores/:store-id"
     delete-request))

;;;users
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Users

(defapi users%info
    ("/api/v1/users/me"
     get-request))

(defapi users%delete-me
    ("/api/v1/users/me"
     delete-request))

(defapi users%all
    ("/api/v1/users"
     get-request))

(defapi users%create
    ("/api/v1/users"
     post-request))

(defapi users%get
    ("/api/v1/users/:user-id-or-email"
     get-request))

(defapi users%delete
    ("/api/v1/users/:user-id-or-email"
     delete-request))

(defapi users%lock
    ("/api/v1/users/:user-id-or-email/lock"
     delete-request))
