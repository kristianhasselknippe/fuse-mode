(require 'json)

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






;{
;    "Name": "Subscribe",
;    "Id": 1,
;    "Arguments": {
;        "Filter": "Fuse.BuildStarted",
;        "Replay": false,
;        "SubscriptionId": 0
;    }
;}
