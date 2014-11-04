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


;; Internal Tests

(ert-deftest bter-api-test/can-create-endpoint-without-query-vars ()
  (should (string=
           "http://data.bter.com/api/1/test-path/"
           (bter-api--create-endpoint "test-path"))))

(ert-deftest bter-api-test/can-create-endpoint-with-query-vars ()
  (should (string=
           "http://data.bter.com/api/1/test-path/?arg=value"
           (bter-api--create-endpoint "test-path" (list :arg "value")))))
