(require 'json)
(load-file "fuse-error-log.el")

(setq test-build-issue-message "{\"Name\":\"Fuse.BuildIssueDetected\",\"Data\":{\"BuildId\":\"4c81d3ab-77fc-4244-b1ce-6844b8754351\",\"IssueType\":\"Error\",\"Path\":\"C:\\Users\\Emil\\Documents\\Fuse\\asdd\\MainView.ux\",\"StartPosition\":{\"Line\":6,\"Character\":1},\"EndPosition\":null,\"ErrorCode\":\"E0000\",\"Message\":\"Name cannot begin with character, hexadecimal value 0x3C. Line 6, position 5.\"},\"SubscriptionId\":2}")


(defun fuse-build-issue-detected (data)
  (fuse-write-error-log data))

(defun fuse-build-started ()
  (fuse-clear-error-log))

(defun fuse-decode-message (message)
  (let (name data)
	(setq data (json-read-from-string message))
	(setq name (cdr (assq 'Name data)))
	(cond
	 ((equal name "Fuse.BuildIssueDetected")
	  (fuse-build-issue-detected (assq 'Data data)))
	 ((equal name "Fuse.BuildStarted")
	  (fuse-build-started)))))



(defun test-message-decoder ()
  (fuse-decode-message test-build-issue-message))



;{
;   "Name":"Fuse.BuildIssueDetected",
;   "Data":{
;      "BuildId":"4c81d3ab-77fc-4244-b1ce-6844b8754351",
;      "IssueType":"Error",
;      "Path":"C:\\Users\\Emil\\Documents\\Fuse\\asdd\\MainView.ux",
;      "StartPosition":{
;         "Line":6,
;         "Character":1
;      },
;      "EndPosition":null,
;      "ErrorCode":"E0000",
;      "Message":"Name cannot begin with the '<' character, hexadecimal value 0x3C. Line 6, position 5."
;   },
;   "SubscriptionId":2
;}
