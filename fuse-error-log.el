(defvar fuse-error-log-buffer nil)
(defvar fuse-error-log-name "fuse-error-log")

(defun fuse-create-error-log-buffer ()
  (setq fuse-error-log-buffer (get-buffer-create fuse-error-log-name)))


(defun fuse-error-log-write-message (message)
  (save-excursion
	  (set-buffer fuse-error-log-buffer)
	  (insert (concat (cdr message) "\n"))))

(defun fuse-clear-error-log ()
  (save-excursion
	(set-buffer fuse-error-log-buffer)
	(erase-buffer)))

(defun fuse-write-error-log (data)
  (if (equal nil fuse-error-log-buffer)
	  (fuse-create-error-log-buffer))
  (let ((message (assq 'Message data))
		(error-code	(assq 'ErrorCode data))
		(end-position (assq 'EndPosition data))
		(start-position (assq 'StartPosition data))
		(path (assq 'Path data))
		(issue-type	(assq 'IssueType data))
		(build-id (assq 'BuildId data)))
    (fuse-error-log-write-message message)))

(defun fuse-clear-error-log ()
  )

(provide 'fuse-error-log)
