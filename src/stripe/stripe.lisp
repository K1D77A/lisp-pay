(in-package #:lisp-pay/stripe)

;;;core resources
(defapi balance%get-balance ("/v1/balance" get-request))

(defapi balance-transactions%all ("/v1/balance_transactions" get-request))

(defapi balance-transactions%id ("/v1/balance_transactions/:id" get-request))



(defapi charges%create ("/v1/charges" post-request))

(defapi charges%all ("/v1/charges" get-request))

(defapi charges%id ("/v1/charges/:id" get-request))

(defapi charges%update ("/v1/charges/:id" post-request))

(defapi charges%capture ("/v1/charges/:id/capture" post-request))



(defapi customers%create ("/v1/customers" post-request))

(defapi customers%update ("/v1/customers/:id" post-request))

(defapi customers%all ("/v1/customers" get-request))

(defapi customers%id ("/v1/customers/:id" get-request))

(defapi customers%id ("/v1/customers/:id" delete-request))



(defapi disputes%all ("/v1/disputes" get-request))

(defapi disputes%id ("/v1/disputes/:id" get-request))

(defapi disputes%update ("/v1/disputes/:id" post-request))

(defapi disputes%close ("/v1/disputes/:id/close" post-request))



(defapi events%all ("/v1/events" get-request))

(defapi events%id ("/v1/events/:id" get-request))



(defapi files%all ("/v1/files" get-request))

(defapi files%id ("/v1/files/:id" get-request))

(defapi files%create ("/v1/files" post-files-request))



(defapi file-links%id ("/v1/files_links/:id" get-request))

(defapi file-links%all ("/v1/files_links" get-request))

(defapi file-links%update ("/v1/files_links/:id" post-request))

(defapi file-links%create ("/v1/files_links" post-request))



(defapi mandates%id ("/v1/mandates/:id" get-request))



(defapi payment-intents%id ("/v1/payment_intents/:id" get-request))

(defapi payment-intents%all ("/v1/payment_intents" get-request))

(defapi payment-intents%create ("/v1/payment_intents" post-request))

(defapi payment-intents%update ("/v1/payment_intents/:id" post-request))

(defapi payment-intents%confirm ("/v1/payment_intents/:id/confirm" post-request))

(defapi payment-intents%capture ("/v1/payment_intents/:id/capture" post-request))

(defapi payment-intents%cancel ("/v1/payment_intents/:id/cancel" post-request))



(defapi setup-intents%id ("/v1/setup_intents/:id" get-request))

(defapi setup-intents%all ("/v1/setup_intents" get-request))

(defapi setup-intents%create ("/v1/setup_intents" post-request))

(defapi setup-intents%update ("/v1/setup_intents/:id" post-request))

(defapi setup-intents%confirm ("/v1/setup_intents/:id/confirm" post-request))

(defapi setup-intents%cancel ("/v1/setup_intents/:id/cancel" post-request))



(defapi setup-attempts%all ("/v1/setup_attempts" get-request))



(defapi payouts%id ("/v1/payouts/:id" get-request))

(defapi payouts%all ("/v1/payouts" get-request))

(defapi payouts%create ("/v1/payouts" post-request))

(defapi payouts%update ("/v1/payouts/:id" post-request))

(defapi payouts%confirm ("/v1/payouts/:id/reverse" post-request))

(defapi payouts%cancel ("/v1/payouts/:id/cancel" post-request))



(defapi refunds%id ("/v1/refunds/:id" get-request))

(defapi refunds%all ("/v1/refunds" get-request))

(defapi refunds%create ("/v1/refunds" post-request))

(defapi refunds%update ("/v1/refunds/:id" post-request))



(defapi tokens%id ("/v1/tokens/:id" get-request))

(defapi tokens%create ("/v1/tokens" post-request))

;;;payment methods



;;;products

(defapi products%create ("/v1/products" post-request))

(defapi products%id ("/v1/products/:id" get-request))

(defapi products%update ("/v1/products/:id" post-request))

(defapi products%all ("/v1/products" get-request))

(defapi products%delete ("/v1/products/:id" delete-request))

;;;prices

(defapi prices%create ("/v1/prices" post-request))

(defapi prices%id ("/v1/prices/:id" get-request))

(defapi prices%update ("/v1/prices/:id" post-request))

(defapi prices%all ("/v1/prices" get-request))



;;;checkouts


(defapi sessions%create ("/v1/checkout/sessions" post-request))

(defapi sessions%expire ("/v1/checkout/sessions/:id/expire" post-request))

(defapi sessions%get ("/v1/checkout/sessions/:id" get-request))

(defapi sessions%all ("/v1/checkout/sessions" get-request))

(defapi sessions%line-items ("/v1/checkout/sessions/:id/line_items" get-request))


;;;shipping


(defapi shipping%create ("/v1/shipping_rates" post-request))

(defapi shipping%get ("/v1/shipping_rates/:id" get-request))

(defapi shipping%all ("/v1/shipping_rates" get-request))

(defapi shipping%update ("/v1/shipping_rates/:id" post-request))


;;;webhooks


(defapi webhooks%create ("/v1/webhook_endpoints" post-request))

(defapi webhooks%get ("/v1/webhook_endpoints/:id" get-request))

(defapi webhooks%all ("/v1/webhook_endpoints" get-request))

(defapi webhooks%update ("/v1/webhook_endpoints/:id" post-request))

(defapi webhooks%delete ("/v1/webhook_endpoints/:id" delete-request))
