(defun fuse--testing-create-test-message ()
  ("Event" 301 "Event\n301\n{\"Name\":\"Fuse.BuildStarted\",\"Data\":{\"BuildType\":\"LoadMarkup\",\"BuildId\":\"f77139bb-d329-4d76-aece-144781e7fc22\",\"BuildTag\":\"Host\",\"PreviewId\":\"af8865ad-7d83-458a-b261-03e03c4be3bf\",\"ProjectPath\":\"/Users/Hassel/FuseProjects/EmacsPlugin/EmacsTest/EmacsTest.unoproj\",\"Target\":\"Unknown\"},\"SubscriptionId\":0}"))


(ert-deftest fuse--test-parse-all-messages-1 ()
  (fuse--reset-for-testing "Foo\n5\nabcdeBar\n4\nabcd" 0 0)
  (fuse--parse-all-messages)
  (should (equal "" fuse--buffer-string)))

(ert-deftest fuse--test-parse-all-messages-2 ()
  (fuse--reset-for-testing "Foo\n5\nabcdeBar\n4\nab" -1 -1)
  (fuse--parse-all-messages)
  (should (equal "Bar\n4\nab" fuse--buffer-string)))




(provide 'fuse-message-decoder-tests)
