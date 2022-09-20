(in-package #:lisp-pay/paypal)

(defparameter *testing* t)


;;;tracking
(defapi tracking%update-or-cancel ("/v1/shipping/trackers/:id" put-r)
        ())

(defapi tracking%information ("/v1/shipping/trackers/:id" get-r)
        ())

(defapi tracking%batch ("/v1/shipping/trackers-batch" post-r)
        ())

;;;billing

(defapi billing%create ("/v1/payments/billing-agreements" post-r)
        ())

(defapi billing%update ("/v1/payments/billing-agreements/:agreement-id" patch-r)
        ())

(defapi billing%information ("/v1/payments/billing-agreements/:agreement-id" get-r)
        ())

(defapi billing%bill-balance
    ("/v1/payments/billing-agreements/:agreement-id/balance" post-r)
    ())

(defapi billing%cancel
    ("/v1/payments/billing-agreements/:agreement-id/cancel" post-r)
    ())

(defapi billing%re-activate
    ("/v1/payments/billing-agreements/:agreement-id/re-activate" post-r)
    ())

(defapi billing%set-balance
    ("/v1/payments/billing-agreements/:agreement-id/set-balance" post-r)
    ())

(defapi billing%suspend
    ("/v1/payments/billing-agreements/:agreement-id/suspend" post-r)
    ())

(defapi billing%list-transactions
    ("/v1/payments/billing-agreements/:agreement-id/transactions" get-r-query)
    ((start_date
      :accessor start-date
      :initarg start-date)
     (end_date
      :accessor end-date
      :initarg :end-date)))

(defapi billing%execute
    ("/v1/payments/billing-agreements/:agreement-id/agreement-execute" post-r)
    ())

;;;catalog products
(defapi products%list ("/v1/catalogs/products" get-r-query)
        ((page_size
          :accessor page-size
          :initarg :page-size)
         (page
          :accessor page
          :initarg :page)
         (total_required
          :accessor total-requried
          :initarg :total-required)))

(defapi products%create ("/v1/catalogs/products" post-r)
        ())
;;has the extra header Prefer and Paypal-Request-Id

(defapi products%update ("/v1/catalogs/products/:product-id" patch-r)
        ())

(defapi products%details ("/v1/catalogs/products/:product-id" get-r)
        ())


;;;disputes
(defapi disputes%get ("/v1/customer/disputes" get-r-query)
        ((start_time
          :accessor start-time
          :initarg :start-time)
         (disputed_transaction_id
          :accessor disputed-transaction_id
          :initarg :disputed-transaction_id)
         (page_size
          :accessor page-size 
          :initarg :page-size)
         (next_page_token
          :accessor next-page-token
          :initarg :next-page-token)
         (dispute_state
          :accessor dispute-state
          :initarg :dispute-state)
         (update_time_before
          :accessor update-time-before
          :initarg :update-time-before)
         (update_time_after
          :accessor update-time-after
          :initarg :update-time-after)))

(defapi disputes%update ("/v1/customer/disputes/:dispute-id" patch-r)
        ())

(defapi disputes%details ("/v1/customer/disputes/:dispute-id" get-r)
        ())

;;;dispute-actions

(defapi disputes-actions%accept-claim ("/v1/customer/disputes/:dispute-id/accept-claim"
                                       post-r)
        ())

(defapi disputes-actions%accept-resolve ("/v1/customer/disputes/:dispute-id/accept-offer"
                                         post-r)
        ())

(defapi disputes-actions%acknowledge-return
    ("/v1/customer/disputes/:dispute-id/acknowledge-return-item"
     post-r)
    ())

(defapi disputes-actions%adjudicate 
    ("/v1/customer/disputes/:dispute-id/adjudicate"
     post-r)
    ());;for sandbox use only

(defapi disputes-actions%appeal
    ("/v1/customer/disputes/:dispute-id/appeal"
     post-files-r)
    ())

(defapi disputes-actions%deny-resolve
    ("/v1/customer/disputes/:dispute-id/deny-offer"
     post-r)
    ())

(defapi disputes-actions%escalate
    ("/v1/customer/disputes/:dispute-id/escalate"
     post-r)
    ())

(defapi disputes-actions%offer-resolve
    ("/v1/customer/disputes/:dispute-id/make-offer"
     post-r)
    ())

(defapi disputes-actions%provide-evidence
    ("/v1/customer/disputes/:dispute-id/provide-evidence"
     post-files-r)
    ())

(defapi disputes-actions%provide-supporting-info
    ("/v1/customer/disputes/:dispute-id/provide-supporting-info"
     post-files-r)
    ());;this wont work if you only want to upload notes.

(defapi disputes-actions%require-evidence
    ("/v1/customer/disputes/:dispute-id/require-evidence"
     post-r)
    ());;sandbox only

(defapi disputes-actions%send-message
    ("/v1/customer/disputes/:dispute-id/send-message"
     post-files-r)
    ())
;;;the way to make these api calls that accept either files or json would be to
;;;set the content-type to your desired then change-class into either post-files-r
;;;which will send the data as multipart-form or post-r which will send it as
;;;json

;;;identity

(defapi identity-userinfo&profile-info ("/v1/identity/oauth2/userinfo" get-r-query)
        ((schema
          :accessor schema
          :initarg :schema)))

(defapi identity-applications%create ("/v1/identity/applications" post-r)
        ())

(defapi identity-account%set-properties ("/v1/identity/account-settings" post-r)
        ())

(defapi identity-account%disable-properties
    ("/v1/identity/account-settings/deactivate" post-r)
    ())

;;;invoices

(defapi invoices%generate-invoice-number ("/v2/invoicing/generate-next-invoice-number"
                                          post-r)
        ())

(defapi invoices%list ("/v2/invoicing/invoices" get-r-query)
        ((page_size
          :accessor page-size
          :initarg :page-size)
         (page
          :accessor page
          :initarg :page)
         (total_required
          :accessor total-requried
          :initarg :total-required)
         (fields
          :accessor fields
          :initarg :fields)))

(defapi invoices%create-draft ("/v2/invoicing/invoices"
                               post-r)
        ())

(defapi invoices%delete ("/v2/invoicing/invoices/:invoice-id"
                         delete-r)
        ())

(defapi invoices%update-invoice ("/v2/invoicing/invoices/:invoice-id"
                                 put-query-r)
        ((send_to_recipient
          :accessor send-to-recipient
          :initarg :send-to-recipient)
         (send_to_invoicer
          :accessor send-to-invoicer
          :initarg :send-to-invoicer)))

(defapi invoices%details ("/v2/invoicing/invoices/:invoice-id"
                          get-r)
        ())

(defapi invoices%cancel ("/v2/invoicing/invoices/:invoice-id/cancel"
                         post-r)
        ())

(defapi invoices%generate-qr-code ("/v2/invoicing/invoices/:invoice-id/generate-qr-code"
                                   post-r)
        ())

(defapi invoices%record-payment ("/v2/invoicing/invoices/:invoice-id/payments"
                                 post-r)
        ())

(defapi invoices%delete-external-payment
    ("/v2/invoicing/invoices/:invoice-id/payments/:transaction-id"
     delete-r)
    ())

(defapi invoices%record-refund
    ("/v2/invoicing/invoices/:invoice-id/refunds"
     post-r)
    ())

(defapi invoices%delete-external-refund
    ("/v2/invoicing/invoices/:invoice-id/refunds/:transaction-id"
     delete-r)
    ())

(defapi invoices%remind
    ("/v2/invoicing/invoices/:invoice-id/remind"
     post-r)
    ())

(defapi invoices%send
    ("/v2/invoicing/invoices/:invoice-id/send"
     post-r)
    ());;has the Paypal-Request-Id header

(defapi invoices%search ("/v2/invoicing/search-invoices" post-query-r)
        ((page_size
          :accessor page-size
          :initarg :page-size)
         (page
          :accessor page
          :initarg :page)
         (total_required
          :accessor total-requried
          :initarg :total-required)))

(defapi invoices-templates%list ("/v2/invoicing/templates" get-r-query)
        ((page_size
          :accessor page-size
          :initarg :page-size)
         (page
          :accessor page
          :initarg :page)
         (fields
          :accessor fields
          :initarg :fields)))

(defapi invoices-templates%create ("/v2/invoicing/templates" post-r)
        ())

(defapi invoices-templates%delete ("/v2/invoicing/templates/:template-id" delete-r)
        ())

(defapi invoices-templates%update ("/v2/invoicing/templates/:template-id" put-r)
        ())

(defapi invoices-templates%details ("/v2/invoicing/templates/:template-id" get-r)
        ())


;;;orders

(defapi orders%create ("/v2/checkout/orders" post-r)
        ());;has the request-id partner-att client-metadata and prefer headers if wanted

(defapi orders%update ("/v2/checkout/orders/:order-id" patch-r)
        ())

(defapi orders%details ("/v2/checkout/orders/:order-id" get-r)
        ((fields
          :accessor fields
          :initarg :fields)))

(defapi orders%authorize ("/v2/checkout/orders/:order-id/authorize" post-r)
        ());;has the request-id metadata prefer auth-assertion


(defapi orders%capture ("/v2/checkout/orders/:order-id/capture" post-r)
        ());;has the request-id prefer metadata auth-assertion


;;;partner referrals

(defapi partner%create ("/v2/customer/partner-referrals" post-r)
        ())

(defapi partner%get-data ("/v2/customer/partner-referrals/:referral-id" get-r)
        ())


;;;payment-experience

(defapi web-profiles%list ("/v1/payment-experience/web-profiles" get-r)
        ())

(defapi web-profiles%create ("/v1/payment-experience/web-profiles" post-r)
        ());;has request-id

(defapi web-profiles%delete ("/v1/payment-experience/web-profiles/:profile-id" delete-r)
        ())

(defapi web-profiles%update ("/v1/payment-experience/web-profiles/:profile-id" patch-r)
        ())

(defapi web-profiles%partial-update
    ("/v1/payment-experience/web-profiles/:profile-id" patch-r)
    ())

(defapi web-profiles%details
    ("/v1/payment-experience/web-profiles/:profile-id" get-r)
    ())

;;;payments

(defapi payments-authorization%details
    ("/v2/payments/authorizations/:authorization-id" get-r)
    ())

(defapi payments-authorization%capture
    ("/v2/payments/authorizations/:authorization-id/capture" post-r)
    ());;has request id and prefer

(defapi payments-authorization%reauthorize
    ("/v2/payments/authorizations/:authorization-id/reauthorize" post-r)
    ());;has request id and prefer

(defapi payments-authorization%void
    ("/v2/payments/authorizations/:authorization-id/void" post-r)
    ());;has auth assertion

(defapi payments-captures%details
    ("/v2/payments/captures/:capture-id" get-r)
    ());;has auth assertion

(defapi payments-captures%refund
    ("/v2/payments/captures/:capture-id/refund" post-r)
    ());;has request-id prefer auth-assertion

(defapi payments-refunds%details
    ("/v2/payments/refunds/:refund-id" get-r)
    ())

;;;payouts

(defapi payouts-batch%create
    ("/v1/payments/payouts" post-r)
    ());has request-id

(defapi payouts-batch%details
    ("/v1/payments/payouts/:batch-id" get-r)
    ((page_size
      :accessor page-size
      :initarg :page-size)
     (page
      :accessor page
      :initarg :page)
     (fields
      :accessor fields
      :initarg :fields)
     (total_required
      :accessor total-required
      :initarg :total-required)))

(defapi payouts-item%details
    ("/v1/payments/payouts-item/:payout-id" get-r)
    ())

(defapi payouts-item%cancel-unclaimed
    ("/v1/payments/payouts-item/:payout-id/cancel" post-r)
    ())

;;;reference payouts

(defapi referenced-payouts-batch%create
    ("/v1/payments/referenced-payouts" post-r)
    ());has partner-attribution request-id prefer 

(defapi referenced-payouts-batch%details
    ("/v1/payments/referenced-payouts/:batch-id" get-r)
    ((page_size
      :accessor page-size
      :initarg :page-size)
     (page
      :accessor page
      :initarg :page)
     (fields
      :accessor fields
      :initarg :fields)
     (total_required
      :accessor total-required
      :initarg :total-required)))

(defapi referenced-payouts-item%create
    ("/v1/payments/referenced-payouts-items" post-r)
    ());;partner-attribution request-id prefer

(defapi referenced-payouts-item%cancel-unclaimed
    ("/v1/payments/referenced-payouts-items/:payout-id" get-r)
    ());;has partner-attribution

;;;subscriptions
(defapi subscriptions-plans%list ("/v1/billing/plans" get-r)
        ((page_size
          :accessor page-size
          :initarg :page-size)
         (page
          :accessor page
          :initarg :page)
         (product_id
          :accessor product-id
          :initarg :product-id)
         (plan_ids 
          :accessor plan-ids 
          :initarg :plan-ids)
         (total_required
          :accessor total-required
          :initarg :total-required)))

(defapi subscribtions-plans%create ("/v1/billing/plans" post-r)
        ());;has prefer request-id

(defapi subscribtions-plans%update ("/v1/billing/plans/:plan-id" patch-r)
        ())

(defapi subscribtions-plans%details ("/v1/billing/plans/:plan-id" get-r)
        ())

(defapi subscribtions-plans%activate ("/v1/billing/plans/:plan-id/activate" post-r)
        ())

(defapi subscribtions-plans%deactivate ("/v1/billing/plans/:plan-id/deactivate" post-r)
        ())

(defapi subscribtions-plans%update-pricing-schemas
    ("/v1/billing/plans/:plan-id/update-pricing-schemas" post-r)
    ())

(defapi subscriptions%create ("/v1/billing/subscriptions" post-r)
        ());has prefer

(defapi subscriptions%update ("/v1/billing/subscriptions/:sub-id" patch-r)
        ())

(defapi subscriptions%details ("/v1/billing/subscriptions/:sub-id" get-r)
        ())

(defapi subscriptions%activate ("/v1/billing/subscriptions/:sub-id/activate" post-r)
        ())

(defapi subscriptions%cancel ("/v1/billing/subscriptions/:sub-id/activate" post-r)
        ())

(defapi subscriptions%capture ("/v1/billing/subscriptions/:sub-id/capture" post-r)
        ())

(defapi subscriptions%revise ("/v1/billing/subscriptions/:sub-id/revise" post-r)
        ())

(defapi subscriptions%suspend ("/v1/billing/subscriptions/:sub-id/suspend" post-r)
        ())

(defapi subscriptions%transactions ("/v1/billing/subscriptions/:sub-id/transactions"
                                    post-r)
        ((start_time
          :accessor start-time 
          :initarg :start-time)
         (end_time 
          :accessor end-time 
          :initarg :end-time)))

;;;search
(defapi search-transactions%list ("/v1/reporting/transactions" get-r)
        ((transaction_id
          :accessor transaction-id
          :initarg :transaction-id)
         (transaction_type
          :accessor transaction-type
          :initarg :transaction-type)
         (transaction_status
          :accessor transaction-status
          :initarg :transaction-status)
         (transaction_amount
          :accessor transaction-amount
          :initarg :transaction-amount)
         (transaction_currency
          :accessor transaction-currency
          :initarg :transaction-currency)
         (start_date
          :accessor start-date 
          :initarg :start-date)
         (end_date
          :accessor end-date 
          :initarg :end-date)
         (payment_instrument_type
          :accessor payment-instrument-type
          :initarg :payment-instrument-type)
         (store_id 
          :accessor store-id
          :initarg :store-id)
         (terminal_id
          :accessor terminal-id 
          :initarg :terminal-id)
         (fields
          :accessor fields
          :initarg :fields)
         (balance_affecting_records_only
          :accessor balance-affecting-records-only
          :initarg :balance-affecting-records-only)
         (page_size 
          :accessor page-size 
          :initarg :page-size)
         (page
          :accessor page
          :initarg :page)))

(defapi search-balances%list ("/v1/reporting/balances" get-r)
        ((as_of_time
          :accessor as-of-time
          :initarg :as-of-time)
         (currency_code
          :accessor currency-code
          :initarg :currency-code)))


;;;webhooks
(defapi webhooks%list ("/v1/notifications/webhooks" get-r)
        ((anchor_time
          :accessor anchor-time
          :initarg :anchor-time)))

(defapi webhooks%create ("/v1/notifications/webhooks" post-r)
        ())

(defapi webhooks%delete ("/v1/notifications/webhooks/:webhook-id" delete-r)
        ())

(defapi webhooks%update ("/v1/notifications/webhooks/:webhook-id" patch-r)
        ())

(defapi webhooks%details ("/v1/notifications/webhooks/:webhook-id" get-r)
        ())

(defapi webhooks%list-event-subscriptions
    ("/v1/notifications/webhooks/:webhook-id/event-types" get-r)
    ())

;; (defapi webhooks%verify-signature ("/v1/notifications/verify-webhook-signature" post-r%jojo)
;;   ());;this will not work.

(defapi webhooks%list-event-types ("/v1/notifications/webhooks-event-types" get-r)
        ())

(defapi webhooks%list-event-notifications ("/v1/notifications/webhooks-events" get-r)
        ((page_size
          :accessor page-size
          :initarg :page-size)
         (transaction_id
          :accessor transaction-id
          :initarg :transaction-id)
         (event_type
          :accessor event-type
          :initarg :event-type)
         (start_time
          :accessor start-time 
          :initarg :start-time)
         (end_time 
          :accessor end-time 
          :initarg :end-time)))

(defapi webhooks%notification-details ("/v1/notifications/webhooks-events/:event-id"
                                       get-r)
        ())

(defapi webhooks%resend-event ("/v1/notifications/webhooks-events/:event-id/resend"
                               post-r)
        ())

(defapi webhooks%simulate-event ("/v1/notifications/simulate-event"
                                 post-r)
        ())
