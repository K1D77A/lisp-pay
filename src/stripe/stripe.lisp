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

(defapi%get customers%all ("/v1/customers"))

(defapi%get customers%id ("/v1/customers/:id"))

(defapi%delete customers%id ("/v1/customers/:id"))



(defapi%get disputes%all ("/v1/disputes"))

(defapi%get disputes%id ("/v1/disputes/:id"))

(defapi disputes%update ("/v1/disputes/:id" post-request))

(defapi disputes%close ("/v1/disputes/:id/close" post-request))



(defapi%get events%all ("/v1/events"))

(defapi%get events%id ("/v1/events/:id"))



(defapi%get files%all ("/v1/files"))

(defapi%get files%id ("/v1/files/:id"))

(defapi files%create ("/v1/files" post-files-request))



(defapi%get file_links%id ("/v1/files_links/:id"))

(defapi%get file_links%all ("/v1/files_links"))

(defapi file_links%update ("/v1/files_links/:id" post-request))

(defapi file_links%create ("/v1/files_links" post-request))



(defapi%get mandates%id ("/v1/mandates/:id"))



(defapi%get payment_intents%id ("/v1/payment_intents/:id"))

(defapi%get payment_intents%all ("/v1/payment_intents"))

(defapi payment_intents%create ("/v1/payment_intents" post-request))

(defapi payment_intents%update ("/v1/payment_intents/:id" post-request))

(defapi payment_intents%confirm ("/v1/payment_intents/:id/confirm" post-request))

(defapi payment_intents%capture ("/v1/payment_intents/:id/capture" post-request))

(defapi payment_intents%cancel ("/v1/payment_intents/:id/cancel" post-request))



(defapi%get setup_intents%id ("/v1/setup_intents/:id"))

(defapi%get setup_intents%all ("/v1/setup_intents"))

(defapi setup_intents%create ("/v1/setup_intents" post-request))

(defapi setup_intents%update ("/v1/setup_intents/:id" post-request))

(defapi setup_intents%confirm ("/v1/setup_intents/:id/confirm" post-request))

(defapi setup_intents%cancel ("/v1/setup_intents/:id/cancel" post-request))



(defapi%get setup_attempts%all ("/v1/setup_attempts"))



(defapi%get payouts%id ("/v1/payouts/:id"))

(defapi%get payouts%all ("/v1/payouts"))

(defapi payouts%create ("/v1/payouts" post-request))

(defapi payouts%update ("/v1/payouts/:id" post-request))

(defapi payouts%confirm ("/v1/payouts/:id/reverse" post-request))

(defapi payouts%cancel ("/v1/payouts/:id/cancel" post-request))



(defapi%get refunds%id ("/v1/refunds/:id"))

(defapi%get refunds%all ("/v1/refunds"))

(defapi refunds%create ("/v1/refunds" post-request))

(defapi refunds%update ("/v1/refunds/:id" post-request))



(defapi%get tokens%id ("/v1/tokens/:id"))

(defapi tokens%create ("/v1/tokens" post-request))

;;;payment methods



;;;products

(defapi products%create ("/v1/products" post-request))

(defapi%get products%id ("/v1/products/:id"))

(defapi products%update ("/v1/products/:id" post-request))

(defapi%get products%all ("/v1/products"))

(defapi%delete products%delete ("/v1/products/:id"))

;;;prices

(defapi prices%create ("/v1/prices" post-request))

(defapi%get prices%id ("/v1/prices/:id"))

(defapi prices%update ("/v1/prices/:id" post-request))

(defapi%get prices%all ("/v1/prices"))



;;;checkouts


(defapi sessions%create ("/v1/checkout/sessions" post-request))

(defapi sessions%expire ("/v1/checkout/sessions/:id/expire" post-request))

(defapi%get sessions%get ("/v1/checkout/sessions/:id"))

(defapi%get sessions%all ("/v1/checkout/sessions"))

(defapi%get sessions%line-items ("/v1/checkout/sessions/:id/line_items"))


;;;shipping


(defapi shipping%create ("/v1/shipping_rates" post-request))

(defapi%get shipping%get ("/v1/shipping_rates/:id"))

(defapi%get shipping%all ("/v1/shipping_rates"))

(defapi shipping%update ("/v1/shipping_rates/:id" post-request))


;;;webhooks


(defapi webhooks%create ("/v1/webhook_endpoints" post-request))

(defapi%get webhooks%get ("/v1/webhook_endpoints/:id"))

(defapi%get webhooks%all ("/v1/webhook_endpoints"))

(defapi webhooks%update ("/v1/webhook_endpoints/:id" post-request))

(defapi%delete webhooks%delete ("/v1/webhook_endpoints/:id"))
