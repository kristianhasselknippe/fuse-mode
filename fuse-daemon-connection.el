(require 'fuse-message-parser)
(require 'fuse-message-decoder)
(require 'fuse-messages)

(defun fuse--client-filter (proc string)
  (fuse--receive-string string))

(defun fuse--get-client-process ()
  (get-process "fuse-emacs"))

(defun fuse--create-client ()
  (start-process "fuse-emacs" "fuse-emacs"
				 "/usr/local/bin/fuse"
				 "daemon-client"
				 "fuse-mode")
  (set-process-filter (fuse-get-client-process) 'fuse-client-filter))

(defun fuse--client-send-string (command-string)
  (process-send-string (fuse-get-client-process) command-string))

(provide 'fuse-daemon-connection)
