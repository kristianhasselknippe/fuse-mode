(require 'json)

(defun fuse--dispatch-response (message)
  (fuse--print-to-fuse-buffer ":::::Got Response:::::\n\n")
  (fuse--print-to-fuse-buffer (concat message "\n")))

(defun fuse--dispatch-event (message)
  (fuse--decode-event message))

(defun fuse--dispatch-request (message)
  (fuse--print-to-fuse-buffer message))

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
