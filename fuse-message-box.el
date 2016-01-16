(require 'dash)

(load-file "~/fuse-mode/fuse-daemon-connection.el")

(defvar fuse--message-id-index 0)

(defvar fuse--messages-list '())

(defun fuse--increment-message-id-index ()
  (setq fuse--message-id-index (1+ fuse--message-id-index)))

(defun fuse--get-message-type-for-id (id)
  (if (or (>= id fuse--message-id-index)
		  (< id 0))
	  nil
	(nth id fuse--messages-list)))


(defun fuse--send-message (message)
  )
