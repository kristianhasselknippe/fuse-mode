(ert-deftest fuse--test-create-request-1 ()
  (should (equal
		   (fuse--create-request "foo" "bar" "foobar") "{\"Name\":\"foo\",\"Id\":\"bar\",\"Arguments\":\"foobar\"}")))

(ert-deftest fuse--test-create-request-2 ()
  (should (equal
		   (fuse--create-request "f" "b" "fo") "{\"Name\":\"f\",\"Id\":\"b\",\"Arguments\":\"fo\"}")))

(ert-deftest fuse--test-create-subscription-request-1 ()
  (setq fuse--message-id-index 0)
  (setq fuse--subscription-id-index 0)
  (should (equal
		   (fuse--create-subscription-request "a" "true")
		   "{\"Name\":\"Subscribe\",\"Id\":0,\"Arguments\":{\"Filter\":\"a\",\"Replay\":\"true\",\"SubscriptionId\":0}}")))

(ert-deftest fuse--test-create-subscription-request-1 ()
  (setq fuse--message-id-index 0)
  (setq fuse--subscription-id-index 0)
  (should (equal
		   (fuse--create-subscription-request "a" "true")
		   "{\"Name\":\"Subscribe\",\"Id\":0,\"Arguments\":{\"Filter\":\"a\",\"Replay\":\"true\",\"SubscriptionId\":0}}")))

(ert-deftest fuse--test-create-subscription-request-2 ()
  (setq fuse--message-id-index 1)
  (setq fuse--subscription-id-index 2)
  (should (equal
		   (fuse--create-subscription-request "foo" "true")
		   "{\"Name\":\"Subscribe\",\"Id\":1,\"Arguments\":{\"Filter\":\"foo\",\"Replay\":\"true\",\"SubscriptionId\":2}}")))

(provide 'fuse-messages-tests)
