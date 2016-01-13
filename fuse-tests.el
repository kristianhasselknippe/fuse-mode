(defun fuse--reset-for-testing (buffer-str sym-ptr buffer-ptr)
  (setq fuse--buffer-string buffer-str)
  (setq fuse--symbol-pointer sym-ptr)
  (setq fuse--buffer-pointer buffer-ptr))


(ert-deftest fuse--test-get-next-character-1 ()
  (fuse--reset-for-testing "foobar" 0 -1)
  (should (equal (fuse--get-next-character) ?f))
  (should (equal fuse--buffer-pointer 0))
  (should (equal (fuse--get-next-character) ?o))
  (should (equal fuse--buffer-pointer 1))
  (should (equal (fuse--get-next-character) ?o))
  (should (equal (fuse--get-next-character) ?b))
  (should (equal fuse--buffer-pointer 3))
  (should (equal (fuse--get-next-character) ?a))
  (should (equal (fuse--get-next-character) ?r)))

(ert-deftest fuse--test-get-next-character-2 ()
  (fuse--reset-for-testing "" 0 -1)
  (should (equal (fuse--get-next-character) nil)))

(ert-deftest fuse--test-get-next-character-3 ()
  (fuse--reset-for-testing "f" 0 0)
  (should (equal (fuse--get-next-character) nil)))

(ert-deftest fuse--test-get-next-character-4 ()
  (fuse--reset-for-testing "fa" 342 1)
  (should (equal (fuse--get-next-character) nil)))



(ert-deftest fuse--test-get-current-character-1 ()
  (fuse--reset-for-testing "foobar" 0 2)
  (should (equal (fuse--get-current-character) ?o)))

(ert-deftest fuse--test-get-current-character-2 ()
  (fuse--reset-for-testing "foobar" 0 4)
  (should (equal (fuse--get-current-character) ?a)))

(ert-deftest fuse--test-get-current-character-3 ()
  (fuse--reset-for-testing "foobar" 0 6)
  (should (equal (fuse--get-current-character) nil)))

(ert-deftest fuse--test-get-current-character-4 ()
  (fuse--reset-for-testing "foobar" 0 -1)
  (should (equal (fuse--get-current-character) nil)))






(ert-deftest fuse--test-get-current-symbol-1 ()
  (fuse--reset-for-testing "galskapogvin" 7 9)
  (should (equal (fuse--get-current-symbol) "og"))
  (should (equal fuse--symbol-pointer 9)))

(ert-deftest fuse--test-get-current-symbol-2 ()
  (fuse--reset-for-testing "galskapogvin" 9 12)
  (should (equal (fuse--get-current-symbol) "vin"))
  (should (equal fuse--symbol-pointer 12)))

(ert-deftest fuse--test-get-current-symbol-3 ()
  (fuse--reset-for-testing "galskapogvin" 0 7)
  (should (equal (fuse--get-current-symbol) "galskap"))
  (should (equal fuse--symbol-pointer 7)))

(ert-deftest fuse--test-get-current-symbol-4 ()
  (fuse--reset-for-testing "galskapogvin" 10 7)
  (should (equal (fuse--get-current-symbol) nil)))

(ert-deftest fuse--test-get-current-symbol-5 ()
  (fuse--reset-for-testing "galskapogvin" 9 13)
  (should (equal (fuse--get-current-symbol) nil)))





(ert-deftest fuse--test-parse-line-1 ()
  (fuse--reset-for-testing "foobar\n" 0 -1)
  (should (equal (fuse--parse-line) "foobar")))

(ert-deftest fuse--test-parse-line-2 ()
  (fuse--reset-for-testing "foobar" 0 -1)
  (should (equal (fuse--parse-line) nil)))

(ert-deftest fuse--test-parse-line-3 ()
  (fuse--reset-for-testing "foobar\nbarbar\n" 0 -1)
  (should (equal (fuse--parse-line) "foobar"))
  (should (equal (fuse--parse-line) "barbar")))

(ert-deftest fuse--test-parse-line-4 ()
  (fuse--reset-for-testing "foobar\nbarbar" 0 -1)
  (should (equal (fuse--parse-line) "foobar"))
  (should (equal (fuse--parse-line) nil)))

(ert-deftest fuse--test-parse-line-5 ()
  (fuse--reset-for-testing "a\nba\ncar\nfart\n" 0 -1)
  (should (equal (fuse--parse-line) "a"))
  (should (equal (fuse--parse-line) "ba"))
  (should (equal (fuse--parse-line) "car"))
  (should (equal (fuse--parse-line) "fart")))

(ert-deftest fuse--test-parse-line-6 ()
  (fuse--reset-for-testing "bar\nbar\nbar\n" 4 4)
  (should (equal (fuse--parse-line) "bar"))
  (should (equal (fuse--parse-line) "bar"))
  (should (equal (fuse--parse-line) nil)))





(ert-deftest fuse--test-parse-bytes-1 ()
  (fuse--reset-for-testing "0123456789abcdefghijklmnopqrst" 0 -1)
  (should (equal (fuse--parse-bytes 10) "0123456789"))
  (should (equal fuse--symbol-pointer 10)))

(ert-deftest fuse--test-parse-bytes-2 ()
  (fuse--reset-for-testing "0123456789abcdefghijklmnopqrst" 0 -1)
  (should (equal (fuse--parse-bytes 4) "0123"))
  (should (equal fuse--symbol-pointer 4)))

(ert-deftest fuse--test-parse-bytes-3 ()
  (fuse--reset-for-testing "0123456789abcdefghijklmnopqrst" 0 -1)
  (should (equal (fuse--parse-bytes 15) "0123456789abcde"))
  (should (equal fuse--symbol-pointer 15)))

(ert-deftest fuse--test-parse-bytes-4 ()
  (fuse--reset-for-testing "0123456789abcdefghijklmnopqrst" 0 -1)
  (should (equal (fuse--parse-bytes 5) "01234"))
  (should (equal (fuse--parse-bytes 3) "567"))
  (should (equal fuse--symbol-pointer 8)))

(ert-deftest fuse--test-parse-bytes-5 ()
  (fuse--reset-for-testing "0123456789" 5 5)
  (should (equal (fuse--parse-bytes 5) "56789")))






(ert-deftest fuse--test-parse-to-character ()
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?g) ?g))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?a) ?a))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?c) ?c))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?f) ?f))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?o) ?o))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?p) ?p))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?r) nil))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?q) nil))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?t) nil))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?w) nil)))


(ert-deftest fuse--test-parse-message ()
  (fuse--reset-for-testing "Event\n301\n{\"Name\":\"Fuse.BuildStarted\",\"Data\":{\"BuildType\":\"LoadMarkup\",\"BuildId\":\"f77139bb-d329-4d76-aece-144781e7fc22\",\"BuildTag\":\"Host\",\"PreviewId\":\"af8865ad-7d83-458a-b261-03e03c4be3bf\",\"ProjectPath\":\"/Users/Hassel/FuseProjects/EmacsPlugin/EmacsTest/EmacsTest.unoproj\",\"Target\":\"Unknown\"},\"SubscriptionId\":0}" 0 0)
  (let ((res (fuse--parse-message)))
	(should (not (equal res nil)))
	(should (equal 301 (nth 1 res)))))

(ert-deftest fuse--test-parse-message-1 ()
  (fuse--reset-for-testing "Event\n10\nthisismypa" 0 -1)
  (let ((res (fuse--parse-message)))
	(should (not (equal res nil)))
	(should (equal "thisismypa" (nth 2 res)))))

(ert-deftest fuse--test-parse-message-2 ()
  (fuse--reset-for-testing "Event\n301\n{\"Name\":\"Fuse.BuildStarted\",\"Data\":{\"BuildType\":\"LoadMarkup\",\"BuildId\":\"f77139bb-d329-4d76-aece-144781e7fc22\",\"BuildTag\":\"Host\",\"PreviewId\":\"af8865ad-7d83-458a-b261-03e03c4be3bf\",\"ProjectPath\":\"/Users/Hassel/FuseProjects/EmacsPlugin/EmacsTest/EmacsTest.unoproj\",\"Target\":\"Unknown\"},\"SubscriptionId\":0}" 0 0)
  (let (message)
	(setq message (fuse--parse-message))
	(should (equal message '("Event" 301 "{\"Name\":\"Fuse.BuildStarted\",\"Data\":{\"BuildType\":\"LoadMarkup\",\"BuildId\":\"f77139bb-d329-4d76-aece-144781e7fc22\",\"BuildTag\":\"Host\",\"PreviewId\":\"af8865ad-7d83-458a-b261-03e03c4be3bf\",\"ProjectPath\":\"/Users/Hassel/FuseProjects/EmacsPlugin/EmacsTest/EmacsTest.unoproj\",\"Target\":\"Unknown\"},\"SubscriptionId\":0}")))))



;(ert-deftest fuse--test-parse-message-3 ()
;  (fuse--reset-for-testing "Event\n301\n{\"Name\":\"Fuse.BuildStarted\",\"Data\":{\"BuildType\":\"LoadMarkup\",\"BuildId\":\"f77139bb-d329-4d76-aece-144781e7fc22\",\"BuildTag\":\"Host\",\"PreviewId\":\"af8865ad-7d83-458a-b261-03e03c4be3bf\",\"ProjectPath\":\"/Users/Hassel/FuseProjects/EmacsPlugin/EmacsTest/EmacsTest.unoproj\",\"Target\":\"Unknown\"},\"SubscriptionId\":0}Event\n301\n{\"Name\":\"Fuse.BuildStarted\",\"Data\":{\"BuildType\":\"LoadMarkup\",\"BuildId\":\"f77139bb-d329-4d76-aece-144781e7fc22\",\"BuildTag\":\"Host\",\"PreviewId\":\"af8865ad-7d83-458a-b261-03e03c4be3bf\",\"ProjectPath\":\"/Users/Hassel/FuseProjects/EmacsPlugin/EmacsTest/EmacsTest.unoproj\",\"Target\":\"Unknown\"},\"SubscriptionId\":0}" 0 0)
;  (let (message)
;	(setq message (fuse--parse-message))
;	(princ message)
;	(should (equal message '("Event" 301 "{\"Name\":\"Fuse.BuildStarted\",\"Data\":{\"BuildType\":\"LoadMarkup\",\"BuildId\":\"f77139bb-d329-4d76-aece-144781e7fc22\",\"BuildTag\":\"Host\",\"PreviewId\":\"af8865ad-7d83-458a-b261-03e03c4be3bf\",\"ProjectPath\":\"/Users/Hassel/FuseProjects/EmacsPlugin/EmacsTest/EmacsTest.unoproj\",\"Target\":\"Unknown\"},\"SubscriptionId\":0}")))
;	(setq message (fuse--parse-message))
;	(should (equal message '("Event" 301 "{\"Name\":\"Fuse.BuildStarted\",\"Data\":{\"BuildType\":\"LoadMarkup\",\"BuildId\":\"f77139bb-d329-4d76-aece-144781e7fc22\",\"BuildTag\":\"Host\",\"PreviewId\":\"af8865ad-7d83-458a-b261-03e03c4be3bf\",\"ProjectPath\":\"/Users/Hassel/FuseProjects/EmacsPlugin/EmacsTest/EmacsTest.unoproj\",\"Target\":\"Unknown\"},\"SubscriptionId\":0}")))
;	(should (equal fuse--buffer-string ""))))


(ert-deftest fuse--test-consume-buffer-to-ptr ()
  (fuse--reset-for-testing "abcdefghijklmnop" 0 7)
  (fuse--get-current-symbol)
  (fuse--consume-buffer-to-ptr)
  (should (equal fuse--buffer-string "hijklmnop")))


(ert-deftest fuse--test-consume-buffer-to-ptr-1 ()
  (fuse--reset-for-testing "abcdefghijklmnop" 0 9)
  (fuse--get-current-symbol)
  (fuse--consume-buffer-to-ptr)
  (should (equal fuse--buffer-string "jklmnop")))

(ert-deftest fuse--test-consume-buffer-to-ptr-2 ()
  (fuse--reset-for-testing "abcdefghijklmnop" 6 7)
  (fuse--get-current-symbol)
  (fuse--consume-buffer-to-ptr)
  (should (equal fuse--buffer-string "hijklmnop")))


(ert-deftest fuse--test-add-string-to-buffer ()
  (fuse--reset-for-testing "" 0 0)
  (fuse--add-string-to-buffer "foobar")
  (should (equal fuse--buffer-string "foobar")))

(ert-deftest fuse--test-add-string-to-buffer-1 ()
  (fuse--reset-for-testing "foo" 0 0)
  (fuse--add-string-to-buffer "bar")
  (should (equal fuse--buffer-string "foobar")))

(ert-deftest fuse--test-add-string-to-buffer-2 ()
  (fuse--reset-for-testing "foobar" 0 0)
  (fuse--add-string-to-buffer "foobar")
  (should (equal fuse--buffer-string "foobarfoobar")))

;(ert-deftest fuse--test-parse-all-messages ()
;  (fuse--reset-for-testing "Foo\n5\nabcdeBar\n4\nabcd" 0 0)
;  (fuse--client-parse-all-messages)
;  (should (equal "" fuse--buffer-string)))
;
;(ert-deftest fuse--test-parse-all-messages-1 ()
;  (fuse--reset-for-testing "Foo\n5\nabcdeBar\n4\nab" -1 -1)
;  (fuse--client-parse-all-messages)
;  (should (equal "Bar\n4\nab" fuse--buffer-string)))
