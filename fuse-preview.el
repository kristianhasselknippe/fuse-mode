(defun fuse-create-client ()
  (start-process "fuse-emacs" "fuse-emacs"
				 "/usr/local/bin/fuse"
				 "daemon-client"
				 "fuse-mode")
  (set-process-filter (get-process "fuse-emacs") 'fuse-client-filter))
