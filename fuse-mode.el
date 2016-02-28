(load-file "~/fuse-mode/fuse-message-parser.el")

(load-file "~/fuse-mode/fuse-daemon-connection.el")

(load-file "~/fuse-mode/fuse-error-log.el")
(load-file "~/fuse-mode/fuse-message-decoder.el")

(load-file "~/fuse-mode/fuse-message-box.el")
(load-file "~/fuse-mode/fuse-messages.el")
(load-file "~/fuse-mode/fuse-code-completion.el")
(load-file "~/fuse-mode/fuse-common.el")


(load-file "~/fuse-mode/fuse-preview.el")
(load-file "~/fuse-mode/fuse-selection-changed.el")

;(load-file "~/fuse-mode/fuse-ux-parser.el")


(load-file "~/fuse-mode/tests/fuse-tests.el")





(defun fuse--request-build-issue-detected ()
  (fuse--client-send-string
   (fuse--create-message 'Request (fuse--create-subscription-request "Fuse.BuildIssueDetected" t))))

(defun fuse--request-build-started ()
  (fuse--client-send-string
   (fuse--create-message 'Request (fuse--create-subscription-request "Fuse.BuildStarted" t))))

(defun fuse--request-build-ended ()
  (fuse--client-send-string
   (fuse--create-message 'Request (fuse--create-subscription-request "Fuse.BuildEnded" t))))

(defun fuse--request-build-logged ()
  )



(defun fuse-jack-in ()
  (interactive)
  (fuse--request-build-started)
  (fuse--request-build-issue-detected))



(provide 'fuse-mode)
