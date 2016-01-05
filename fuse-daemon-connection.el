(load-file "~/fuse-mode/fuse-message-parser.el")
(load-file "~/fuse-mode/fuse-message-decoder.el")
(load-file "~/fuse-mode/fuse-messages.el")

(defun fuse-client-filter (proc string)
  (fuse-message-parser-add string)
  (let (ret)
	(while (not (equal ret -1))
	  (setq ret (fuse-client-parse))
	  (if (not (equal ret -1))
		  (progn
			(fuse-decode-message (nth 1 ret)))))))


(defun fuse-create-client ()
  (start-process "fuse-emacs" "fuse-emacs"
				 "/usr/local/bin/fuse"
				 "daemon-client"
				 "fuse-mode")
  (set-process-filter (get-process "fuse-emacs") 'fuse-client-filter))

(defun fuse-client-send-command (command-string)
  (process-send-string (get-process "fuse-emacs") command-string))



(defun fuse-mode ()
  (interactive)
  (fuse-create-client)
  (process-send-string
   (get-process "fuse-emacs")
   (create-request-build-issue-detected))
  (process-send-string
   (get-process "fuse-emacs")
   (create-request-build-started)))


(defun fuse-mode-test ()
  (fuse-mode)
  (process-send-string (get-process "fuse-emacs") "Request\n104\n{\"Name\":\"Subscribe\",\"Id\":0,\"Arguments\":{\"Filter\":\"Fuse.BuildStarted\",\"Replay\":false,\"SubscriptionId\":0}}"))


(provide 'fuse.mode)
