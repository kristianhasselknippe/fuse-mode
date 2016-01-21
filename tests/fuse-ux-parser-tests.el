(defun fuse--ux-reset-for-testing (bfr sym-ptr bfr-ptr)
  (setq fuse--ux-bfr bfr)
  (setq fuse--ux-sym-ptr sym-ptr)
  (setq fuse--ux-bfr-ptr bfr-ptr))

(ert-deftest fuse--test-ux-read-current-char-1 ()
  (fuse--ux-reset-for-testing "<App></App>" 1 1)
  (should (equal (fuse--ux-read-current-char) ?A))
  (should (equal fuse--ux-bfr-ptr 1)))

(ert-deftest fuse--test-ux-read-current-char-2 ()
  (fuse--ux-reset-for-testing "<App></App>" 1 4)
  (should (equal (fuse--ux-read-current-char) ?>))
  (should (equal fuse--ux-bfr-ptr 4)))

(ert-deftest fuse--test-ux-read-current-char-3 ()
  (fuse--ux-reset-for-testing "<App></App>" 1 -1)
  (should (equal (fuse--ux-read-current-char) nil)))

(ert-deftest fuse--test-ux-read-current-char-4 ()
  (fuse--ux-reset-for-testing "<App></App>" 1 50)
  (should (equal (fuse--ux-read-current-char) nil)))




(ert-deftest fuse--test-ux-read-next-char-1 ()
  (fuse--ux-reset-for-testing "<App></App>" -1 0)
  (should (equal (fuse--ux-read-next-char) ?A))
  (should (equal fuse--ux-bfr-ptr 1)))

(ert-deftest fuse--test-ux-read-next-char-2 ()
  (fuse--ux-reset-for-testing "<App></App>" -1 -1)
  (should (equal (fuse--ux-read-next-char) ?<))
  (should (equal fuse--ux-bfr-ptr 0)))

(ert-deftest fuse--test-ux-read-next-char-3 ()
  (fuse--ux-reset-for-testing "123" -1 2)
  (should (equal (fuse--ux-read-next-char) nil)))

(ert-deftest fuse--test-ux-read-next-char-4 ()
  (fuse--ux-reset-for-testing "<Foobar" -1 -10)
  (should (equal (fuse--ux-read-next-char) ?<))
  (should (equal fuse--ux-bfr-ptr 0)))



(ert-deftest fuse--test-ux-peek-next-char-1 ()
  (fuse--ux-reset-for-testing "<Foobar" -1 -1)
  (should (equal (fuse--ux-peek-next-char) ?<))
  (should (equal fuse--ux-bfr-ptr -1)))

(ert-deftest fuse--test-ux-peek-next-char-2 ()
  (fuse--ux-reset-for-testing "<Foobar" -1 -10)
  (should (equal (fuse--ux-peek-next-char) ?<))
  (should (equal fuse--ux-bfr-ptr -1)))

(ert-deftest fuse--test-ux-peek-next-char-3 ()
  (fuse--ux-reset-for-testing "<Foobar" -1 0)
  (should (equal (fuse--ux-peek-next-char) ?F))
  (should (equal fuse--ux-bfr-ptr 0)))



(ert-deftest fuse--test-ux-read-current-symbol-1 ()
  (fuse--ux-reset-for-testing "<App></App>" 1 4)
  (should (equal (fuse--ux-read-current-symbol) "App")))

(ert-deftest fuse--test-ux-read-current-symbol-2 ()
  (fuse--ux-reset-for-testing "<AppBar></App>" 1 7)
  (should (equal (fuse--ux-read-current-symbol) "AppBar")))

(ert-deftest fuse--test-ux-read-current-symbol-3 ()
  (fuse--ux-reset-for-testing "<AppFooBar>" 1 20)
  (should (equal (fuse--ux-read-current-symbol) nil)))

(ert-deftest fuse--test-ux-read-current-symbol-4 ()
  (fuse--ux-reset-for-testing "<AppFooBar>" -1 -1)
  (should (equal (fuse--ux-read-current-symbol) nil)))

(ert-deftest fuse--test-ux-read-current-symbol-5 ()
  (fuse--ux-reset-for-testing "<AppFooBar>" -1 5)
  (should (equal (fuse--ux-read-current-symbol) "<AppF")))






(ert-deftest fuse--test-ux-name-chars ()
  )






(ert-deftest fuse--test-ux-parse-tag ()
  (fuse--ux-reset-for-testing "<App>" -1 -1)
  (should (equal (fuse--ux-parse-tag) "App")))
