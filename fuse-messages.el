(require 'json)

(load-file "~/fuse-mode/fuse-message-box.el")

(defun create-message (type content)
  (concat type "\n" (number-to-string (length content)) "\n" content ))


(defun create-command (name id filters replay subscriptionId)
  (json-encode `(:Name ,name
							  :Id ,id
							  :Arguments (:Filter ,filters
												   :Replay ,replay
												   :SubscriptionId ,subscriptionId))))

;Request types:
;Fuse.GetCodeSuggestions
;



(defun create-request-build-started ()
  (create-message "Request"
				  (create-command 'Subscribe 'Fuse.BuildStarted json-false 0)))

(defun create-request-build-issue-detected ()
  (create-message "Request"
				  (create-command 'Subscribe 0 'Fuse.BuildIssueDetected json-false 0)))






;{
;    "Name": "Subscribe",
;    "Id": 1,
;    "Arguments": {
;        "Filter": "Fuse.BuildStarted",
;        "Replay": false,
;        "SubscriptionId": 0
;    }
;}
