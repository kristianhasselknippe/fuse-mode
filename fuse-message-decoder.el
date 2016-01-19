(require 'json)
(require 'fuse-error-log)


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


(defun fuse--parse-all-messages ()
  (let ((message (fuse--parse-message)))
	(while (not (equal message nil))
	  (fuse--decode-message message)
	  (setq message (fuse--parse-message)))))

(defun fuse--receive-string (string)
  (fuse--add-string-to-buffer string)
  (fuse--parse-all-messages))

(provide 'fuse-message-decoder)
