(require 'json)
(require 'fuse-error-log)

(load-file "~/fuse-mode/fuse-message-box.el")

(defun fuse--create-message (type content)
  (concat (symbol-name type) "\n" (number-to-string (length content)) "\n" content ))


(defun fuse--create-request (name id arguments)
  (json-encode `(:Name ,name
				 :Id ,id
				 :Arguments ,arguments)))

(defun fuse--create-subscription-request (filters replay)
  (fuse--create-request "Subscribe"
						(fuse--increment-message-id-index)
						`(:Filter ,filters
						 :Replay ,replay
						 :SubscriptionId ,(fuse--increment-subscription-id-index))))


;Request types:
;Fuse.GetCodeSuggestions



(defun fuse--create-request-build-started ()
  (create-message "Request"
				  (create-command 'Subscribe 'Fuse.BuildStarted json-false 0)))

(defun fuse--create-request-build-issue-detected ()
  (create-message "Request"
				  (create-command 'Subscribe 0 'Fuse.BuildIssueDetected json-false 0)))


(defun fuse--build-issue-detected-handler (data)
  (fuse-write-error-log data))

(defun fuse--build-started-handler ()
  (fuse--clear-error-log))

(defun fuse--build-ended-handler ()
  (fuse--println-to-fuse-buffer "Build did finish"))

(defun fuse--decode-event (event-json-string)
  (let ((data (json-read-from-string event-json-string)))
	(let ((name (cdr (assoc 'Name data))))
	  (cond ((equal name "Fuse.BuildStarted") (fuse--build-started-handler))
			((equal name "Fuse.BuildEnded")   (fuse--build-ended-handler))
			((equal name "Fuse.BuildIssueDetected") (fuse--build-issue-detected-handler (cdr (assoc 'Data data))))))))

(provide 'fuse-messages)
