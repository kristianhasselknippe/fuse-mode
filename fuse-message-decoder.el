(require 'json)
(load-file "~/fuse-mode/fuse-error-log.el")


(defun fuse--dispatch-build-issue-detected (data)
  (fuse-write-error-log data))

(defun fuse--dispatch-build-started ()
  (fuse-clear-error-log))



(defun fuse--decode-message (message)
  (princ message))


(fuse--reset-for-testing "Foo\n5\nabcdeBar\n4\nabcd" 0 0)
(fuse--client-parse-all-messages)
