(in-package #:lisp-pay/paypal)

(defparameter *testing* t)


;;;tracking
(defapi tracking%update-or-cancel ("/v1/shipping/trackers/:id" put-request)
        ())

(defapi tracking%information ("/v1/shipping/trackers/:id" get-request)
        ())

(defapi tracking%batch ("/v1/shipping/trackers-batch" post-request)
        ())

;;;billing

(defapi billing%create ("/v1/payments/billing-agreements" post-request)
        ())

(defapi billing%update ("/v1/payments/billing-agreements/:agreement-id" patch-request)
        ())

(defapi billing%information ("/v1/payments/billing-agreements/:agreement-id" get-request)
        ())

(defapi billing%bill-balance
    ("/v1/payments/billing-agreements/:agreement-id/balance" post-request)
    ())

(defapi billing%cancel
    ("/v1/payments/billing-agreements/:agreement-id/cancel" post-request)
    ())

(defapi billing%re-activate
    ("/v1/payments/billing-agreements/:agreement-id/re-activate" post-request)
    ())

(defapi billing%set-balance
    ("/v1/payments/billing-agreements/:agreement-id/set-balance" post-request)
    ())

(defapi billing%suspend
    ("/v1/payments/billing-agreements/:agreement-id/suspend" post-request)
    ())

(defapi billing%list-transactions
    ("/v1/payments/billing-agreements/:agreement-id/transactions" get-request)
    ((start-date
      :accessor start-date
      :initarg start-date
      :as-string "start_date")
     (end-date
      :accessor end-date
      :initarg :end-date
      :as-string "end-date")))

(defapi billing%execute
    ("/v1/payments/billing-agreements/:agreement-id/agreement-execute" post-request)
    ())

;;;catalog products
(defapi products%list ("/v1/catalogs/products" get-request)
        ((page-size
          :accessor page-size
          :initarg :page-size
          :as-string "page_size")
         (page
          :accessor page
          :initarg :page)
         (total-required
          :accessor total-requried
          :initarg :total-required
          :as-string "total_required")))

(defapi products%create ("/v1/catalogs/products" post-request)
        ())
;;has the extra header Prefer and Paypal-Request-Id

(defapi products%update ("/v1/catalogs/products/:product-id" patch-request)
        ())

(defapi products%details ("/v1/catalogs/products/:product-id" get-request)
        ())


;;;disputes
(defapi disputes%get ("/v1/customer/disputes" get-request)
        ((start-time
          :accessor start-time
          :initarg :start-time
          :as-string "start_time")
         (disputed-transaction-id
          :accessor disputed-transaction_id
          :initarg :disputed-transaction_id
          :as-string "disputed_transaction_id")
         (page-size
          :accessor page-size 
          :initarg :page-size
          :as-string "page_size")
         (next-page-token
          :accessor next-page-token
          :initarg :next-page-token
          :as-string "next_page_token")
         (dispute-state
          :accessor dispute-state
          :initarg :dispute-state
          :as-string "dispute_state")
         (update-time-before
          :accessor update-time-before
          :initarg :update-time-before
          :as-string "update_time_before")
         (update-time-after
          :accessor update-time-after
          :initarg :update-time-after
          :as-string "update_time_after")))

(defapi disputes%update ("/v1/customer/disputes/:dispute-id" patch-request)
        ())

(defapi disputes%details ("/v1/customer/disputes/:dispute-id" get-request)
        ())

;;;dispute-actions

(defapi disputes-actions%accept-claim ("/v1/customer/disputes/:dispute-id/accept-claim"
                                       post-request)
        ())

(defapi disputes-actions%accept-resolve ("/v1/customer/disputes/:dispute-id/accept-offer"
                                         post-request)
        ())

(defapi disputes-actions%acknowledge-return
    ("/v1/customer/disputes/:dispute-id/acknowledge-return-item"
     post-request)
    ())

(defapi disputes-actions%adjudicate 
    ("/v1/customer/disputes/:dispute-id/adjudicate"
     post-request)
    ());;for sandbox use only

(defapi disputes-actions%appeal
    ("/v1/customer/disputes/:dispute-id/appeal"
     post-files-request)
    ())

(defapi disputes-actions%deny-resolve
    ("/v1/customer/disputes/:dispute-id/deny-offer"
     post-request)
    ())

(defapi disputes-actions%escalate
    ("/v1/customer/disputes/:dispute-id/escalate"
     post-request)
    ())

(defapi disputes-actions%offer-resolve
    ("/v1/customer/disputes/:dispute-id/make-offer"
     post-request)
    ())

(defapi disputes-actions%provide-evidence
    ("/v1/customer/disputes/:dispute-id/provide-evidence"
     post-files-request)
    ())

(defapi disputes-actions%provide-supporting-info
    ("/v1/customer/disputes/:dispute-id/provide-supporting-info"
     post-files-request)
    ());;this wont work if you only want to upload notes.

(defapi disputes-actions%require-evidence
    ("/v1/customer/disputes/:dispute-id/require-evidence"
     post-request)
    ());;sandbox only

(defapi disputes-actions%send-message
    ("/v1/customer/disputes/:dispute-id/send-message"
     post-files-request)
    ())
;;;the way to make these api calls that accept either files or json would be to
;;;set the content-type to your desired then change-class into either post-files-r
;;;which will send the data as multipart-form or post-request which will send it as
;;;json

;;;identity

(defapi identity-userinfo&profile-info ("/v1/identity/oauth2/userinfo" get-request)
        ((schema
          :accessor schema
          :initarg :schema)))

(defapi identity-applications%create ("/v1/identity/applications" post-request)
        ())

(defapi identity-account%set-properties ("/v1/identity/account-settings" post-request)
        ())

(defapi identity-account%disable-properties
    ("/v1/identity/account-settings/deactivate" post-request)
    ())

;;;invoices

(defapi invoices%generate-invoice-number ("/v2/invoicing/generate-next-invoice-number"
                                          post-request)
        ())

(defapi invoices%list ("/v2/invoicing/invoices" get-request)
        ((page-size
          :accessor page-size
          :initarg :page-size
          :as-string "page_size")
         (page
          :accessor page
          :initarg :page)
         (total-required
          :accessor total-requried
          :initarg :total-required
          :as-string "total_required")
         (fields
          :accessor fields
          :initarg :fields)))

(defapi invoices%create-draft ("/v2/invoicing/invoices"
                               post-request)
        ())

(defapi invoices%delete ("/v2/invoicing/invoices/:invoice-id"
                         delete-request)
        ())

(defapi invoices%update-invoice ("/v2/invoicing/invoices/:invoice-id"
                                 put-request)
        ((send-to-recipient
          :accessor send-to-recipient
          :initarg :send-to-recipient
          :as-string "send_to_recipient")
         (send_to_invoicer
          :accessor send-to-invoicer
          :initarg :send-to-invoicer
          :as-string "send_to_invoicer")))

(defapi invoices%details ("/v2/invoicing/invoices/:invoice-id"
                          get-request)
        ())

(defapi invoices%cancel ("/v2/invoicing/invoices/:invoice-id/cancel"
                         post-request)
        ())

(defapi invoices%generate-qr-code ("/v2/invoicing/invoices/:invoice-id/generate-qr-code"
                                   post-request)
        ())

(defapi invoices%record-payment ("/v2/invoicing/invoices/:invoice-id/payments"
                                 post-request)
        ())

(defapi invoices%delete-external-payment
    ("/v2/invoicing/invoices/:invoice-id/payments/:transaction-id"
     delete-request)
    ())

(defapi invoices%record-refund
    ("/v2/invoicing/invoices/:invoice-id/refunds"
     post-request)
    ())

(defapi invoices%delete-external-refund
    ("/v2/invoicing/invoices/:invoice-id/refunds/:transaction-id"
     delete-request)
    ())

(defapi invoices%remind
    ("/v2/invoicing/invoices/:invoice-id/remind"
     post-request)
    ())

(defapi invoices%send
    ("/v2/invoicing/invoices/:invoice-id/send"
     post-request)
    ());;has the Paypal-Request-Id header

(defapi invoices%search ("/v2/invoicing/search-invoices" post-request)
        ((page-size
          :accessor page-size
          :initarg :page-size
          :as-string "page_size")
         (page
          :accessor page
          :initarg :page)
         (total_required
          :accessor total-requried
          :initarg :total-required
          :as-string "total_required")))

(defapi invoices-templates%list ("/v2/invoicing/templates" get-request)
        ((page-size
          :accessor page-size
          :initarg :page-size
          :as-string "page_size")
         (page
          :accessor page
          :initarg :page)
         (fields
          :accessor fields
          :initarg :fields)))

(defapi invoices-templates%create ("/v2/invoicing/templates" post-request)
        ())

(defapi invoices-templates%delete ("/v2/invoicing/templates/:template-id" delete-request)
        ())

(defapi invoices-templates%update ("/v2/invoicing/templates/:template-id" put-request)
        ())

(defapi invoices-templates%details ("/v2/invoicing/templates/:template-id" get-request)
        ())


;;;orders

(defapi orders%create ("/v2/checkout/orders" post-request)
        ());;has the request-id partner-att client-metadata and prefer headers if wanted

(defapi orders%update ("/v2/checkout/orders/:order-id" patch-request)
        ())

(defapi orders%details ("/v2/checkout/orders/:order-id" get-request)
        ((fields
          :accessor fields
          :initarg :fields)))

(defapi orders%authorize ("/v2/checkout/orders/:order-id/authorize" post-request)
        ());;has the request-id metadata prefer auth-assertion


(defapi orders%capture ("/v2/checkout/orders/:order-id/capture" post-request)
        ());;has the request-id prefer metadata auth-assertion


;;;partner referrals

(defapi partner%create ("/v2/customer/partner-referrals" post-request)
        ())

(defapi partner%get-data ("/v2/customer/partner-referrals/:referral-id" get-request)
        ())


;;;payment-experience

(defapi web-profiles%list ("/v1/payment-experience/web-profiles" get-request)
        ())

(defapi web-profiles%create ("/v1/payment-experience/web-profiles" post-request)
        ());;has request-id

(defapi web-profiles%delete ("/v1/payment-experience/web-profiles/:profile-id" delete-request)
        ())

(defapi web-profiles%update ("/v1/payment-experience/web-profiles/:profile-id" patch-request)
        ())

(defapi web-profiles%partial-update
    ("/v1/payment-experience/web-profiles/:profile-id" patch-request)
    ())

(defapi web-profiles%details
    ("/v1/payment-experience/web-profiles/:profile-id" get-request)
    ())

;;;payments

(defapi payments-authorization%details
    ("/v2/payments/authorizations/:authorization-id" get-request)
    ())

(defapi payments-authorization%capture
    ("/v2/payments/authorizations/:authorization-id/capture" post-request)
    ());;has request id and prefer

(defapi payments-authorization%reauthorize
    ("/v2/payments/authorizations/:authorization-id/reauthorize" post-request)
    ());;has request id and prefer

(defapi payments-authorization%void
    ("/v2/payments/authorizations/:authorization-id/void" post-request)
    ());;has auth assertion

(defapi payments-captures%details
    ("/v2/payments/captures/:capture-id" get-request)
    ());;has auth assertion

(defapi payments-captures%refund
    ("/v2/payments/captures/:capture-id/refund" post-request)
    ());;has request-id prefer auth-assertion

(defapi payments-refunds%details
    ("/v2/payments/refunds/:refund-id" get-request)
    ())

;;;payouts

(defapi payouts-batch%create
    ("/v1/payments/payouts" post-request)
    ());has request-id

(defapi payouts-batch%details
    ("/v1/payments/payouts/:batch-id" get-request)
    ((page-size
      :accessor page-size
      :initarg :page-size
      :as-string "page_size")
     (page
      :accessor page
      :initarg :page)
     (fields
      :accessor fields
      :initarg :fields)
     (total-required
      :accessor total-required
      :initarg :total-required
      :as-string "total_required")))

(defapi payouts-item%details
    ("/v1/payments/payouts-item/:payout-id" get-request)
    ())

(defapi payouts-item%cancel-unclaimed
    ("/v1/payments/payouts-item/:payout-id/cancel" post-request)
    ())

;;;reference payouts

(defapi referenced-payouts-batch%create
    ("/v1/payments/referenced-payouts" post-request)
    ());has partner-attribution request-id prefer 

(defapi referenced-payouts-batch%details
    ("/v1/payments/referenced-payouts/:batch-id" get-request)
    ((page-size
      :accessor page-size
      :initarg :page-size
      :as-string "page_size")
     (page
      :accessor page
      :initarg :page)
     (fields
      :accessor fields
      :initarg :fields)
     (total-required
      :accessor total-required
      :initarg :total-required
      :as-string "total_required")))

(defapi referenced-payouts-item%create
    ("/v1/payments/referenced-payouts-items" post-request)
    ());;partner-attribution request-id prefer

(defapi referenced-payouts-item%cancel-unclaimed
    ("/v1/payments/referenced-payouts-items/:payout-id" get-request)
    ());;has partner-attribution

;;;subscriptions
(defapi subscriptions-plans%list ("/v1/billing/plans" get-request)
        ((page-size
          :accessor page-size
          :initarg :page-size
          :as-string "page_size")
         (page
          :accessor page
          :initarg :page)
         (product-id
          :accessor product-id
          :initarg :product-id
          :as-string "product_id")
         (plan-ids 
          :accessor plan-ids 
          :initarg :plan-ids
          :as-string "plan_ids")
         (total-required
          :accessor total-required
          :initarg :total-required
          :as-string "total_required")))

(defapi subscribtions-plans%create ("/v1/billing/plans" post-request)
        ());;has prefer request-id

(defapi subscribtions-plans%update ("/v1/billing/plans/:plan-id" patch-request)
        ())

(defapi subscribtions-plans%details ("/v1/billing/plans/:plan-id" get-request)
        ())

(defapi subscribtions-plans%activate ("/v1/billing/plans/:plan-id/activate" post-request)
        ())

(defapi subscribtions-plans%deactivate ("/v1/billing/plans/:plan-id/deactivate" post-request)
        ())

(defapi subscribtions-plans%update-pricing-schemas
    ("/v1/billing/plans/:plan-id/update-pricing-schemas" post-request)
    ())

(defapi subscriptions%create ("/v1/billing/subscriptions" post-request)
        ());has prefer

(defapi subscriptions%update ("/v1/billing/subscriptions/:sub-id" patch-request)
        ())

(defapi subscriptions%details ("/v1/billing/subscriptions/:sub-id" get-request)
        ())

(defapi subscriptions%activate ("/v1/billing/subscriptions/:sub-id/activate" post-request)
        ())

(defapi subscriptions%cancel ("/v1/billing/subscriptions/:sub-id/activate" post-request)
        ())

(defapi subscriptions%capture ("/v1/billing/subscriptions/:sub-id/capture" post-request)
        ())

(defapi subscriptions%revise ("/v1/billing/subscriptions/:sub-id/revise" post-request)
        ())

(defapi subscriptions%suspend ("/v1/billing/subscriptions/:sub-id/suspend" post-request)
        ())

(defapi subscriptions%transactions ("/v1/billing/subscriptions/:sub-id/transactions"
                                    post-request)
        ((start-time
          :accessor start-time 
          :initarg :start-time
          :as-string "start_time")
         (end-time 
          :accessor end-time 
          :initarg :end-time
          :as-string "end_time")))

;;;search
(defapi search-transactions%list ("/v1/reporting/transactions" get-request)
        ((transaction-id
          :accessor transaction-id
          :initarg :transaction-id
          :as-string "transaction_id")
         (transaction-type
          :accessor transaction-type
          :initarg :transaction-type
          :as-string "transaction_type")
         (transaction-status
          :accessor transaction-status
          :initarg :transaction-status
          :as-string "transaction_status")
         (transaction-amount
          :accessor transaction-amount
          :initarg :transaction-amount
          :as-string "transaction_amount")
         (transaction-currency
          :accessor transaction-currency
          :initarg :transaction-currency
          :as-string "transaction_currency")
         (start-date
          :accessor start-date 
          :initarg :start-date
          :as-string "start_date")
         (end-date
          :accessor end-date 
          :initarg :end-date
          :as-string "end_date")
         (payment-instrument-type
          :accessor payment-instrument-type
          :initarg :payment-instrument-type
          :as-string "payment_instrument_type")
         (store-id 
          :accessor store-id
          :initarg :store-id
          :as-string "store_id")
         (terminal-id
          :accessor terminal-id 
          :initarg :terminal-id
          :as-string "terminal_id")
         (fields
          :accessor fields
          :initarg :fields)
         (balance-affecting-records-only
          :accessor balance-affecting-records-only
          :initarg :balance-affecting-records-only
          :as-string "balance_affecting_records_only")
         (page-size
          :accessor page-size 
          :initarg :page-size
          :as-string "page_size")
         (page
          :accessor page
          :initarg :page)))

(defapi search-balances%list ("/v1/reporting/balances" get-request)
        ((as-of-time
          :accessor as-of-time
          :initarg :as-of-time
          :as-string "as_of_time")
         (currency-code
          :accessor currency-code
          :initarg :currency-code
          :as-string "currency_code")))


;;;webhooks
(defapi webhooks%list ("/v1/notifications/webhooks" get-request)
        ((anchor-time
          :accessor anchor-time
          :initarg :anchor-time
          :as-string "anchor_time")))

(defapi webhooks%create ("/v1/notifications/webhooks" post-request)
        ())

(defapi webhooks%delete ("/v1/notifications/webhooks/:webhook-id" delete-request)
        ())

(defapi webhooks%update ("/v1/notifications/webhooks/:webhook-id" patch-request)
        ())

(defapi webhooks%details ("/v1/notifications/webhooks/:webhook-id" get-request)
        ())

(defapi webhooks%list-event-subscriptions
    ("/v1/notifications/webhooks/:webhook-id/event-types" get-request)
    ())

;; (defapi webhooks%verify-signature ("/v1/notifications/verify-webhook-signature" post-request%jojo)
;;   ());;this will not work.

(defapi webhooks%list-event-types ("/v1/notifications/webhooks-event-types" get-request)
        ())

(defapi webhooks%list-event-notifications ("/v1/notifications/webhooks-events" get-request)
        ((page_size
          :accessor page-size
          :initarg :page-size)
         (transaction_id
          :accessor transaction-id
          :initarg :transaction-id)
         (event_type
          :accessor event-type
          :initarg :event-type)
         
         (start-time
          :accessor start-time 
          :initarg :start-time
          :as-string "start_time")
         (end-time
          :accessor end-time 
          :initarg :end-time
          :as-string "end_time")))

(defapi webhooks%notification-details ("/v1/notifications/webhooks-events/:event-id"
                                       get-request)
        ())

(defapi webhooks%resend-event ("/v1/notifications/webhooks-events/:event-id/resend"
                               post-request)
        ())

(defapi webhooks%simulate-event ("/v1/notifications/simulate-event"
                                 post-request)
        ())
