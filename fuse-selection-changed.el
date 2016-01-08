(load-file "~/fuse-mode/fuse-common.el")
(load-file "~/fuse-mode/fuse-daemon-connection.el")

(defun create-selection-changed-message ()
  (create-message "Event"
				  (json-encode `(:Name Fuse.Preview.SelectionChanged
									   :Data (:Path ,(fuse--get-current-file-name)
													:Text ,(fuse--get-current-file-contents)
													:CaretPosition (:Line ,(fuse--get-current-caret-line)
																		  :Character ,(fuse--get-current-caret-character)))))))

(defun fuse-selection-changed ()
  (interactive)
  ; have to do it two times for some reason :S
  (progn
	(fuse-client-send-command (create-selection-changed-message))
	(fuse-client-send-command (create-selection-changed-message))))

(defun fuse-test-fuse-selection-changed ()
  (interactive)
  (princ (create-selection-changed-message)))

(buffer-file-name)
