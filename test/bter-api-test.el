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

;; Internal Tests

(ert-deftest bter-api-test/can-create-endpoint-without-query-vars ()
  (should (string=
           "http://data.bter.com/api/1/test-path/"
           (bter-api--create-endpoint "test-path"))))

(ert-deftest bter-api-test/can-create-endpoint-with-query-vars ()
  (should (string=
           "http://data.bter.com/api/1/test-path/?arg=value"
           (bter-api--create-endpoint "test-path" (list :arg "value")))))
