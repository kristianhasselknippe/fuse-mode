(require 'cl)
(require 'dash)
(require 's)
(require 'json)
(require 'edebug)

(defun cdra (key alist)
  (cdr (assoc key alist)))

(setq fuse--buffer "")
(setq fuse--daemon-proc nil)

(cl-defstruct event name subscription-id data)

(cl-defstruct issue-detected-data build-id issue-type path start-pos end-pos error-code message)

(cl-defstruct subscribe-request-args filter replay id)
(defun subscribe-request-args-to-obj (sub-req-args)
  `(:Filter ,(subscribe-request-args-filter sub-req-args)
			:Replay ,(subscribe-request-args-replay sub-req-args)
			:SubscriptionId ,(subscribe-request-args-id sub-req-args)))

(cl-defstruct request name id arguments)
(defun request-to-obj (request)
  `(:Name ,(request-name request)
		  :Id ,(request-id request)
		  :Arguments ,(subscribe-request-args-to-obj (request-arguments request))))

(cl-defstruct response id status result errors)

(cl-defstruct message type length payload)
(defun message-to-string (message)
  (unless (message-p message) (error "message-to-string only handles message types"))
  (format "%s\n%d\n%s\n" (message-type message) (message-length message) (message-payload message)))


(cl-defstruct subscription-response id status error results)

(defun fuse--log (msg)
  (with-current-buffer (get-buffer-create "fuse-log")
	(insert msg)))


(defun request-services ()
	(let* ((request (make-request
					 :name "Subscribe"
					 :id 0
					 :arguments
					 (make-subscribe-request-args
					  :filter "Fuse.BuildIssueDetected"
					  :replay t
					  :id 1)))
		   (payload (json-encode (request-to-obj request)))
		   (message (make-message :type "Request"
								  :length (length payload)
								  :payload payload)))
	  (process-send-string fuse--daemon-proc (message-to-string message))))


(defun fuse--log-issue-detected (data)
  (fuse--log (concat
			  "ErrorCode: " (issue-detected-data-error-code data)
			  "\nPath: " (issue-detected-data-path data)
			  "\nLine: " (number-to-string (cdra 'Line (issue-detected-data-start-pos data))))))


(defun fuse--filter (proc msg)
  (setf fuse--buffer (concat fuse--buffer msg))
  (let ((message-split (s-split-up-to "\n" fuse--buffer 3)))
	(when (>= (length message-split) 3)
	  (let ((message (make-message :type (nth 0 message-split)
								   :length (nth 1 message-split)
								   :payload (nth 2 message-split))))

		(if (= (length message-split) 4)
			(setf fuse--buffer (nth 3 message-split))
		  (setf fuse--buffer ""))
		(cond ((string= (message-type message) "Response") (let ((decoded-payload (json-read (message-payload message))))
															 ))
			  ((string= (message-type message) "Event")(let* ((decoded-payload (json-read-from-string (message-payload message)))
															  (event (make-event :name (cdra 'Name decoded-payload)
																				 :subscription-id (cdra 'SubscriptionId decoded-payload)
																				 :data (cdra 'Data decoded-payload)))
															  (decoded-data (cdr (assoc 'Data decoded-payload)))
															  (data (make-issue-detected-data :build-id (cdra 'BuildId decoded-data)
																							  :issue-type (cdra 'IssueType decoded-data)
																							  :path (cdra 'Path decoded-data)
																							  :start-pos (cdra 'StartPosition decoded-data)
																							  :end-pos (cdra 'EndPosition decoded-data)
																							  :error-code (cdra 'ErrorCode decoded-data)
																							  :message (cdra 'Message decoded-data))))
														 (fuse--log-issue-detected data))))))))

(defun fuse--mode-init ()
  (setf fuse--daemon-proc (start-process "fuse-mode"
										 "fuse-mode"
										 "/usr/local/bin/fuse" "daemon-client" "fuse-mode"))

  (set-process-filter fuse--daemon-proc 'fuse--filter)
  (set-process-sentinel fuse--daemon-proc (lambda (proc msg) )))


;;;###autoload
(define-minor-mode fuse-mode
  "The Fuse minor mode."
  :lighter "fuse"
  :keymap (make-sparse-keymap))

;;;###autoload
(add-hook 'fuse-mode-hook 'fuse--mode-init)

(provide 'fuse-mode)
