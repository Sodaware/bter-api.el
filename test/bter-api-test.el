;; Main method tests

(ert-deftest bter-api-test/can-get-pairs ()
  (with-mock
   (mock-request "pairs" nil "pairs.json")
   (should (= 154 (length (bter-api-get-pairs))))))

(ert-deftest bter-api-test/get-pairs-returns-list ()
  (with-mock
   (mock-request "pairs" nil "pairs.json")
   (should (listp (bter-api-get-pairs)))))

(ert-deftest bter-api-test/can-validate-pair ()
  (with-mock
   (mock-request "pairs" nil "pairs.json")
   (should (bter-api-valid-pair-p "btc_usd"))
   (should-not (bter-api-valid-pair-p "bbb_bbb"))))

(ert-deftest bter-api-test/get-all-market-info-returns-list ()
  (with-mock
   (mock-request "marketinfo" nil "marketinfo.json")
   (should (listp (bter-api-get-all-market-info)))))

(ert-deftest bter-api-test/get-all-market-info-contains-market-info ()
  (with-mock
   (mock-request "marketinfo" nil "marketinfo.json")
   (let* ((market-info (bter-api-get-all-market-info))
          (first-market (elt market-info 0)))
     (should (string= "btc_cny" (assoc-default :pair first-market))))))

(ert-deftest bter-api-test/can-get-single-market-info ()
  (with-mock
   (mock-request "marketinfo" nil "marketinfo.json")
   (let ((market (bter-api-get-market-info "btc_usd")))
     (should (string= "btc_usd" (assoc-default :pair market)))
     (should (= 3 (assoc-default :decimal-places market)))
     (should (= 0.0001 (assoc-default :min-amount market)))
     (should (= 0.2 (assoc-default :fee market))))))

(ert-deftest bter-api-test/can-get-all-market-details ()
  (with-mock
   (mock-request "marketlist" nil "marketlist.json")
   (let* ((markets (bter-api-get-all-market-details))
          (first-market (elt markets 0)))
     (should (= 1 (assoc-default :number first-market)))
     (should (string= "BTC" (assoc-default :symbol first-market))))))

(ert-deftest bter-api-test/get-all-market-details-returns-fields-as-correct-types ()
  (with-mock
   (mock-request "marketlist" nil "marketlist.json")
   (let* ((markets (bter-api-get-all-market-details))
          (first-market (elt markets 0)))
     (should (numberp (assoc-default :number first-market)))
     (should (numberp (assoc-default :rate first-market)))
     (should (numberp (assoc-default :volume-a first-market)))
     (should (numberp (assoc-default :volume-b first-market)))
     (should (numberp (assoc-default :rate-percent first-market)))
     (should (numberp (assoc-default :market-cap first-market))))))

(ert-deftest bter-api-test/can-get-single-market-details ()
  (with-mock
   (mock-request "marketlist" nil "marketlist.json")
   (let ((market (bter-api-get-market-details "btc_usd")))
     (should (string= "btc_usd" (assoc-default :pair market)))
     (should (= 263.1 (assoc-default :volume-a market)))
     (should (string= "USD" (assoc-default :symbol market))))))

(ert-deftest bter-api-test/get-tickers-returns-list ()
  (with-mock
   (mock-request "tickers" nil "tickers.json")
   (should (listp (bter-api-get-tickers)))))

(ert-deftest bter-api-test/can-get-tickers ()
  (with-mock
   (mock-request "tickers" nil "tickers.json")
   (let* ((tickers (bter-api-get-tickers))
          (first-ticker (elt tickers 0)))
     (should (= 2083 (assoc-default :last first-ticker)))
     (should (string= "btc_cny" (assoc-default :pair first-ticker)))
     (should (= 252.8188 (assoc-default :volume-from first-ticker)))
     (should (= 533668.57 (assoc-default :volume-to first-ticker))))))

(ert-deftest bter-api-test/can-get-ticker-by-name ()
  (with-mock
   (mock-request "ticker/btc_usd" nil "ticker-btc_usd.json")
   (let ((ticker (bter-api-get-ticker "btc" "usd")))
     (should (= 386 (assoc-default :last ticker)))
     (should (= 414.001 (assoc-default :high ticker)))
     (should (= 360 (assoc-default :low ticker)))
     (should (= 392.842 (assoc-default :average ticker)))
     (should (= 393.88 (assoc-default :sell ticker)))
     (should (= 368.011 (assoc-default :buy ticker)))
     (should (= 1.1309 (assoc-default :volume-from ticker)))
     (should (= 444.259 (assoc-default :volume-to ticker))))))

(ert-deftest bter-api-test/get-ticker-throws-error-with-invalid-pair ()
  (with-mock
   (mock-request "ticker/inv_inv" nil "invalid_ticker.json")
   (should-error (bter-api-get-ticker "inv" "inv"))))

(ert-deftest bter-api-test/get-depth-contains-ask-and-bid-lists ()
  (with-mock
   (mock-request "depth/btc_usd" nil "depth-btc_usd.json")
   (let ((response (bter-api-get-depth "btc" "usd")))
     (should (listp (assoc-default :asks response)))
     (should (listp (assoc-default :bids response))))))

(ert-deftest bter-api-test/get-depth-contains-ask-data ()
  (with-mock
   (mock-request "depth/btc_usd" nil "depth-btc_usd.json")
   (let ((response (bter-api-get-depth "btc" "usd")))
     (should (= 64 (length (assoc-default :asks response))))
     (mapcar (lambda (depth)
               (should-not (null (assoc-default :price depth)))
               (should (numberp (assoc-default :price depth)))
               (should-not (null (assoc-default :amount depth)))
               (should (numberp (assoc-default :amount depth))))
             (assoc-default :asks response)))))

(ert-deftest bter-api-test/get-depth-contains-bid-data ()
  (with-mock
   (mock-request "depth/btc_usd" nil "depth-btc_usd.json")
   (let ((response (bter-api-get-depth "btc" "usd")))
     (should (= 31 (length (assoc-default :bids response))))
     (mapcar (lambda (depth)
               (should-not (null (assoc-default :price depth)))
               (should (numberp (assoc-default :price depth)))
               (should-not (null (assoc-default :amount depth)))
               (should (numberp (assoc-default :amount depth))))
             (assoc-default :bids response)))))

(ert-deftest bter-api-test/get-trades-contains-elapsed-time ()
  (with-mock
   (mock-request "trade/btc_usd" nil "trade-btc_usd.json")
   (let ((response (bter-api-get-trades "btc" "usd")))
     (should (= 0.907 (assoc-default :elapsed response))))))

(ert-deftest bter-api-test/get-trades-contains-trade-data ()
  (with-mock
   (mock-request "trade/btc_usd" nil "trade-btc_usd.json")
   (let* ((response (bter-api-get-trades "btc" "usd"))
          (first-trade (car (assoc-default :trades response))))
     (should (= 17 (length (assoc-default :trades response))))
     (should (= 1415900654 (assoc-default :date first-trade)))
     (should (= 420 (assoc-default :price first-trade)))
     (should (= 0.0175 (assoc-default :amount first-trade)))
     (should (= 10127208 (assoc-default :tid first-trade)))
     (should (equal :sell (assoc-default :type first-trade))))))

(ert-deftest bter-api-test/get-depth-throws-error-with-invalid-pair ()
  (with-mock
   (mock-request "depth/inv_inv" nil "invalid_ticker.json")
   (should-error (bter-api-get-depth "inv" "inv"))))


;; Internal Tests

(ert-deftest bter-api-test/can-create-endpoint-without-query-vars ()
  (should (string=
           "http://data.bter.com/api/1/test-path/"
           (bter-api--create-endpoint "test-path"))))

(ert-deftest bter-api-test/can-create-endpoint-with-query-vars ()
  (should (string=
           "http://data.bter.com/api/1/test-path/?arg=value"
           (bter-api--create-endpoint "test-path" (list :arg "value")))))

(ert-deftest bter-api-test/string-to-number-returns-number-with-string-value ()
  (should (= 123.456 (bter-api--string-to-number "123.456"))))

(ert-deftest bter-api-test/string-to-number-returns-number-with-number-value ()
  (should (= 123.456 (bter-api--string-to-number 123.456))))

(ert-deftest bter-api-test/can-create-pair-name-from-strings ()
  (should (string= "btc_usd" (bter-api--get-pair-name "btc" "usd")))
  (should (string= "btc_usd" (bter-api--get-pair-name "BTC" "USD"))))

(ert-deftest bter-api-test/can-create-pair-name-from-quoted-values ()
  (should (string= "btc_usd" (bter-api--get-pair-name 'btc 'usd)))
  (should (string= "btc_usd" (bter-api--get-pair-name 'BTC 'USD))))

(ert-deftest bter-api-test/can-create-pair-name-from-keywords ()
  (should (string= "btc_usd" (bter-api--get-pair-name :btc :usd)))
  (should (string= "btc_usd" (bter-api--get-pair-name :BTC :USD))))

(ert-deftest bter-api-test/can-convert-trade-type-quoted-value-to-symbol ()
  (should (eq :buy (bter-api--symbolify-trade-type 'buy)))
  (should (eq :sell (bter-api--symbolify-trade-type 'sell))))

(ert-deftest bter-api-test/can-convert-trade-type-string-to-symbol ()
  (should (eq :buy (bter-api--symbolify-trade-type "buy")))
  (should (eq :sell (bter-api--symbolify-trade-type "sell"))))
