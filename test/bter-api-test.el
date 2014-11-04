;; Main method tests

(ert-deftest bter-api-test/can-get-pairs ()
  (with-mock
   (mock-request "pairs" nil "pairs.json")
   (should (= 154 (length (bter-api-get-pairs))))))


;; Internal Tests

(ert-deftest bter-api-test/can-create-endpoint-without-query-vars ()
  (should (string=
           "http://data.bter.com/api/1/test-path/"
           (bter-api--create-endpoint "test-path"))))

(ert-deftest bter-api-test/can-create-endpoint-with-query-vars ()
  (should (string=
           "http://data.bter.com/api/1/test-path/?arg=value"
           (bter-api--create-endpoint "test-path" (list :arg "value")))))
