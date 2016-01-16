(load-file "~/fuse-mode/fuse-message-parser.el")
(load-file "~/fuse-mode/fuse-message-decoder.el")
(load-file "~/fuse-mode/fuse-messages.el")

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
