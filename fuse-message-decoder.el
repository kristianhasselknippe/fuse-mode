(require 'json)
(load-file "~/fuse-mode/fuse-error-log.el")




(defun fuse--dispatch-response (message)
  )

(defun fuse--dispatch-event (message)
  )

(defun fuse--dispatch-request (message)
  )

(defun fuse--decode-message (message)
  (let ((type (nth 0 message))
		(data (nth 2 message)))
	(cond ((equal type "Response") (fuse--dispatch-response data))
		  ((equal type "Event") (fuse--dispatch-event data))
		  ((equal type "Request") (fuse--dispatch-request data)))))
