
(defun fuse-client-filter (proc string)
  (let ((buf (get-buffer-create "fuse-client-messages")))
	(insert string)))

(defun fuse-create-client ()
  (start-process "fuse-emacs" "fuse-emacs"
				 "/usr/local/bin/fuse"
				 "daemon-client"
				 "fuse-mode")
  (set-process-filter (get-process "fuse-emacs") 'fuse-client-filter))
