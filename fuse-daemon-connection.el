;(require 'fuse-message-parser)
;(require 'fuse-message-decoder)
;(require 'fuse-messages)


(defun fuse--client-filter (proc string)
  (fuse--receive-string string))

(defun fuse--create-client ()
  (start-process "fuse-emacs" "fuse-emacs"
				 "/usr/local/bin/fuse"
				 "daemon-client"
				 "fuse-mode")
  (set-process-filter (get-process "fuse-emacs") 'fuse--client-filter))

(defun fuse--get-client-process ()
  (let ((proc (get-process "fuse-emacs")))
	(when (equal proc nil)
	  (fuse--create-client))
	(get-process "fuse-emacs")))

(defun fuse--print-to-fuse-buffer (string)
  (with-current-buffer (get-buffer-create "fuse-emacs")
	(save-excursion
	  (goto-char (point-max))
	  (insert string))))

(defun fuse--clear-fuse-buffer ()
  (with-current-buffer (get-buffer-create "fuse-emacs")
	(save-excursion
	  (erase-buffer))))


(defun fuse--println-to-fuse-buffer (string)
  (fuse--print-to-fuse-buffer (concat string "\n")))


(defun fuse--client-send-string (command-string)
  (process-send-string (fuse--get-client-process) command-string))

(provide 'fuse-daemon-connection)
