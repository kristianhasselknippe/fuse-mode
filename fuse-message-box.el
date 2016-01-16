(require 'dash)

(load-file "~/fuse-mode/fuse-daemon-connection.el")

(defvar fuse--message-id-index 0)
(defvar fuse--messages-list '())


(defun fuse--increment-message-id-index ()
  (let ((ret fuse--message-id-index))
	(setq fuse--message-id-index (1+ fuse--message-id-index))
	ret))

(defun fuse--get-message-type-for-id (id)
  (if (or (>= id fuse--message-id-index)
		  (< id 0))
	  nil
	(nth id fuse--messages-list)))

(defvar fuse--subscription-id-index 0)
(defvar fuse--subscription-list '())

(defun fuse--increment-subscription-id-index ()
  (let ((ret fuse--subscription-id-index))
	(setq fuse--subscription-id-index (1+ fuse--subscription-id-index))
	ret))

(defun fuse--get-subscription-for-id (id)
  (if (or (>= id fuse--subscription-id-index)
		  (< id 0))
	  nil
	(nth id fuse--subscription-list)))



(defun fuse--send-message (message)
  )
