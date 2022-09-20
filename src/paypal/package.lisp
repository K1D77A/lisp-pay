;;;; package.lisp

(defpackage #:lisp-pay/paypal
  (:use #:cl #:lisp-pay)
  (:nicknames #:ldp #:paypal)
  (:export #:paypal-condition
           #:*parse-as*
           #:*json-encoder*
           #:token-issue
           #:missing-token
           
           #:unbound-token
           
           #:expired-token
           #:token

           #:paypal-api-condition
           #:body
           #:status-code
           #:status-text

           #:failed-request

           #:authorization-error

           #:server-error

           #:four-hundred

           #:four-hundred-one

           #:four-hundred-three

           #:four-hundred-four

           #:four-hundred-five

           #:four-hundred-six

           #:four-hundred-fifteen

           #:four-hundred-twenty-two

           #:four-hundred-twenty-nine

           #:five-hundred

           #:five-hundred-three

           ;;protocol
           #:call-api
           
           #:request
           #:content-type
           #:request-fun
           
           #:request-without-content

           #:get-r

           #:query-req

           #:get-r-query

           #:delete-r

           #:request-with-content

           #:patch-r

           #:post-r

           #:query-req-content

           #:post-query-r

           #:post-files-r

           #:put-r

           #:put-query-r

           #:defapi

           #:*request-headers*

           ;;response
           #:response
           #:body
           #:status-code
           #:status-string

           #:good-response

           #:two-hundred

           #:two-hundred-one

           #:two-hundred-two

           #:two-hundred-four

           ;;token
           #:*token*

           #:*client*

           #:*secret*

           #:token
           #:nonce
           #:expires-in
           #:app-id
           #:token-type
           #:access-token
           #:scope

           #:get-token
           
           ;;latter-day-paypal
           #:*testing*

           #:tracking%update-or-cancel

           #:tracking%information

           #:tracking%batch

           #:billing%create

           #:billing%update

           #:billing%information

           #:billing%bill-balance

           #:billing%cancel

           #:billing%re-activate

           #:billing%set-balance

           #:billing%suspend

           #:billing%list-transactions

           #:billing%execute

           #:products%list
           #:page_size
           #:page-size
           #:page
           #:total_required
           #:total-required

           #:products%create

           #:products%update

           #:products%details

           #:disputes%get
           #:start_time
           #:start-time
           #:disputed_transaction_id
           #:disputed-transaction-id
           #:next_page_token
           #:next-page-token
           #:dispute_state
           #:dispute-state
           #:update_time_before
           #:update-time-before
           #:update_time_after
           #:update-time-after

           #:disputes%update

           #:disputes%details

           #:disputes-actions%accept-claim

           #:disputes-actions%accept-resolve

           #:disputes-actions%acknowledge-return

           #:disputes-actions%adjudicate

           #:disputes-actions%appeal

           #:disputes-actions%deny-resolve

           #:disputes-actions%escalate

           #:disputes-actions%offer-resolve

           #:disputes-actions%provide-evidence

           #:disputes-actions%provide-supporting-info

           #:disputes-actions%require-evidence

           #:disputes-actions%send-message

           #:identity-userinfo%profile-info
           #:schema

           #:identity-applications%create

           #:identity-account%set-properties

           #:identity-account%disable-properties

           #:invoices%generate-invoice-number

           #:invoices%list
           #:fields
           #:total_required
           #:total-required

           #:invoices%create-draft

           #:invoices%delete

           #:invoices%update-invoice

           #:invoices%details

           #:invoices%cancel

           #:invoices%generate-qr-code

           #:invoices%record-payment

           #:invoices%delete-external-payment

           #:invoices%record-refund

           #:invoices%remind

           #:invoices%send

           #:invoices%search

           #:invoices-templates%list

           #:invoices-templates%create

           #:invoices-templates%delete

           #:invoices-templates%update

           #:invoices-templates%details

           #:orders%create

           #:orders%update

           #:orders%details

           #:orders%authorize

           #:orders%capture

           #:partner%create

           #:partner%get-data

           #:web-profiles%list

           #:web-profiles%create

           #:web-profiles%delete

           #:web-profiles%update

           #:web-profiles%partial-update

           #:web-profiles%details

           #:payments-authorization%details

           #:payments-authorization%capture

           #:payments-authorization%reauthorize

           #:payments-authorization%void

           #:payments-captures%details

           #:payments-captures%refund

           #:payments-refund%details

           #:payouts-batch%create

           #:payouts-batch%details

           #:payouts-item%details

           #:payouts-item%cancel-unclaimed

           #:referenced-payouts-batch%create

           #:referenced-payouts-batch%details

           #:referenced-payouts-item%create

           #:referenced-payouts-item%cancel-unclaimed

           #:subscriptions-plans%list
           #:product_id
           #:product-id
           #:plan-ids
           #:plan_ids

           #:subscriptions-plans%create

           #:subscriptions-plans%update

           #:subscriptions-plans%details

           #:subscriptions-plans%activate

           #:subscriptions-plans%deactivate

           #:subscriptions-plans%update-pricing-schemas

           #:subscriptions%create

           #:subscriptions%update

           #:subscriptions%details

           #:subscriptions%activate

           #:subscriptions%cancel

           #:subscriptions%capture

           #:subscriptions%revise

           #:subscriptions%suspend

           #:subscriptions%transactions

           #:search-transactions%list
           #:transaction_id
           #:transaction-id
           #:transaction_type
           #:transaction-type
           #:transaction_status
           #:transaction-status
           #:transaction_amount
           #:transaction-amount
           #:transaction_currency
           #:transaction-currency
           #:start_date
           #:start-date
           #:end_date
           #:end-date
           #:payment_instrument_type
           #:payment-instrument-type
           #:store_id
           #:store-id
           #:terminal_id
           #:terminal-id
           #:balance_affecting_records_only
           #:balance-affecting-records-only

           #:search-balances%list
           #:as_of_time
           #:currency_code

           #:webhooks%list
           #:anchor_time
           #:anchor-time

           #:webhooks%create

           #:webhooks%delete

           #:webhooks%update

           #:webhooks%details

           #:webhooks%list-event-subscriptions

           #:webhooks%verify-signature

           #:webhooks%list-event-types

           #:webhooks%list-event-notifications

           #:webhooks%notification-details

           #:webhooks%resent-event

           #:webhooks%simulate-event

           ;;helpers
           #:%quick-hash

           ;;webhook-verify
           #:verify-paypal-webhook

           #:verify-webhook

           #:%algo->key
           ))
