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
