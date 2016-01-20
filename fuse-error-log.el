(require 's)
(require 'dash)
(require 'fuse-daemon-connection)

(defun fuse--error-log-escape-newline (str)
  (s-replace-all '(("\n" . "/n")) str))

(defun fuse--create-goto-file-button (file-name)
  ;TODO: Let the user click the file name to go to the file and line
  )

(defun fuse-write-error-log (data)
  (let ((message (cdr (assq 'Message data)))
		(error-code	(assq 'ErrorCode data))
		(end-position (assq 'EndPosition data))
		(line (cdr (assq 'Line (assq 'StartPosition data))))
		(path (cdr (assq 'Path data)))
		(issue-type	(assq 'IssueType data))
		(build-id (assq 'BuildId data)))
	(fuse--println-to-fuse-buffer
	 (concat (-last-item (s-split "/" path))
			 ": "
			 (if (numberp line)
				 (number-to-string line)
			   line)))
    (fuse--println-to-fuse-buffer message)))

(defun fuse--clear-error-log ()
  (fuse--clear-fuse-buffer))

(provide 'fuse-error-log)
