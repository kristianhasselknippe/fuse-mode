(load-file "fuse-logging.el")

(defun test-file-path (path)
  (if (not (equal (contains-fuse-file-path path) nil))
	  (princ "passed\n")
	(princ "did not pass\n")))

"[\\/].*"

(defun all-logging-tests ()
  (progn
	(test-file-path "C:/foobar/tes23ting.ux")
	(test-file-path "C:/fo\"Fobs\"ar.ux")
	(test-file-path "~/foobar.uno")
	(test-file-path "//foobar.ux")
	(test-file-path "/foo/asdj/aisjd/_bar.ux")
	(test-file-path "/fo\.obar.uno")
	(test-file-path "aosijaobar.ux")
	(test-file-path "asojasd ar.ux")
	(test-file-path "/foobar.uno")))
	(test-file-path "/fo/231obar.ux")
