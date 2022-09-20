(in-package #:lisp-pay/coinpayments)

;;https://www.coinpayments.net/apidoc-rates
(new-request currency-prices "rates" () short accepted)

;;https://www.coinpayments.net/apidoc-get-basic-info
(new-request get-basic-info "get_basic_info" ())

;;https://www.coinpayments.net/apidoc-create-transaction
(new-request create-fixed-price-transaction "create_transaction"
             (amount currency1 currency2 buyer-email)
             amount currency1 currency2 buyer-email address buyer-name
             item-name item-number invoice custom ipn-url success-url
             cancel-url)

;;https://www.coinpayments.net/apidoc-get-callback-address
(new-request get-callback-address "get_callback_address" (currency)
             currency ipn-url label eip55)

;;https://www.coinpayments.net/apidoc-get-tx-info
(new-request get-multiple-transaction-information "get_tx_info_multi" (txid)
             txid)
(new-request get-transaction-information "get_tx_info" (txid)
             txid full)

;;https://www.coinpayments.net/apidoc-get-tx-ids
(new-request get-transaction-ids "get_tx_ids" ()
             limit start newer all)

;;https://www.coinpayments.net/apidoc-balances
(new-request get-coin-balances "balances" ()
             all)

;;https://www.coinpayments.net/apidoc-get-deposit-address
(new-request get-deposit-address "get_deposit_address"
             (currency) currency)

;;https://www.coinpayments.net/apidoc-create-transfer
(new-request create-transfer "create_transfer";;either merchant or pbntag must be set
             (amount currency) amount currency merchant pbntag
             auto-confirm note)

(defmethod validate-slots ((request create-transfer))
  "Special method for create-transfer which has two slots one of either must be set."
  (when (or (slot-boundp request 'merchant)
            (slot-boundp request 'pbntag)
            (error 'required-slots-not-bound :not-set '(merchant pbntag)
                                             :required '(:OR merchant pbntag)))
    (call-next-method)))

(new-request create-withdrawal "create_withdrawal"
             (currency);;either address pbntag or domain must be set
             amount add_tx_fee currency currency2 address pbntag domain
             dest-tag ipn-url auto-confirm note)

(defmethod validate-slots ((request create-withdrawal))
  "Special method for create-transfer which has two slots one of either must be set."
  (when (or (slot-boundp request 'domain)
            (slot-boundp request 'address)
            (slot-boundp request 'pbntag)
            (error 'required-slots-not-bound :not-set '(address pbntag domain)
                                             :required '(:OR domain pbntag address)))
    (call-next-method)))

(new-request create-mass-withdrawal "create_mass_withdrawal"
             (wd) wd)

;;https://www.coinpayments.net/apidoc-cancel-withdrawal
(new-request cancel-withdrawal "cancel_withdrawal"
             (id) id)

;;https://www.coinpayments.net/apidoc-convert
(new-request convert-coins "convert" (amount from to)
             amount from to address dest-tag)

;;https://www.coinpayments.net/apidoc-convert-limits
(new-request conversion-limits "convert_limits" (from to)
             from to)

;;https://www.coinpayments.net/apidoc-get-withdrawal-history
(new-request get-withdrawal-history "get_withdrawal_history"
             () limit start newer)

;;https://www.coinpayments.net/apidoc-get-withdrawal-info
(new-request get-withdrawal-information "get_withdrawal_information"
             (id) id)

;;https://www.coinpayments.net/apidoc-get-conversion-info
(new-request get-conversion-information "get_conversion_information"
             (id) id)

;;https://www.coinpayments.net/apidoc-get-pbn-info
(new-request get-pbn-profile-information "get_pnb_info"
             (pbntag) pbntag)

;;https://www.coinpayments.net/apidoc-get-pbn-list
(new-request get-pbn-tag-list "get_pbn_list"
             ())

;;https://www.coinpayments.net/apidoc-buy-pbn-tags
(new-request buy-pbn-tags "buy_pbn_tags"
             (coin) coin num)

;;https://www.coinpayments.net/apidoc-claim-pbn-tag
(new-request claim-pbn-tag "claim_pbn_tag"
             (tagid name) tagid name)

;;https://www.coinpayments.net/apidoc-update-pbn-tag
(new-request update-pbn-profile "update_pbn_tag"
             (tagid) name email url image)

;;https://www.coinpayments.net/apidoc-renew-pbn-tag
(new-request renew-pbn-tag "renew_pbn_tag"
             (tagid coin) tagid coin years)

;;https://www.coinpayments.net/apidoc-delete-pbn-tag
(new-request delete-pbn-tag "delete_pbn_tag"
             (tagid) tagid)

;;https://www.coinpayments.net/apidoc-claim-pbn-coupon
(new-request claim-pbn-coupon "claim_pbn_coupon"
             (coupon) coupon)
