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

;;;lighting-internal
;;;https://docs.btcpayserver.org/API/Greenfield/v1/#tag/Lightning-(Internal-Node)















