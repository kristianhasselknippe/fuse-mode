(load-file "fuse-logging.el")

(defun test-file-path (path)
  (if (not (equal (contains-fuse-file-path path) nil))
	  (princ "passed\n")
	(princ "did not pass\n")))
