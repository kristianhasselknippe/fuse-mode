(defun fuse--message-box-testing-reset (msg-i msg-list)
  (setq fuse--message-id-index msg-i)
  (setq fuse--messages-list msg-list))


(ert-deftest fuse--test-get-message-type-for-id ()
  (fuse--message-box-testing-reset 5 '(ev req res eve req))
  (should (equal (fuse--get-message-type-for-id 2) 'res))
  (should (equal (fuse--get-message-type-for-id 7) nil))
  (should (equal (fuse--get-message-type-for-id 0) 'ev))
  (should (equal (fuse--get-message-type-for-id -1) nil)))

(provide 'fuse-message-box-tests)
