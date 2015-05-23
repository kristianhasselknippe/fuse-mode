(load-file "fuse-communication.el")

(defvar api-version)

(defun fuse-current-file-name ()
  (buffer-file-name (current-buffer)))


(defun create-set-features (features)
	(json-encode
	 `(:Command SetFeatures
				:Arguments
				(:Features
				 ,(mapcar (lambda (feature) `((:Name . ,feature))) features)))))

(defun create-default-features ()
  (create-set-features '(CodeCompletion ShortcutFeature BuildEvent)))



(defun write-to-fuse-buffer (str)
	(save-excursion
	  (set-buffer (process-buffer fuse-client))
	  (insert (format "[GOT] %s" (prin1-to-string str)))))

(defun write-line-to-fuse-buffer (str)
  (write-to-fuse-buffer (concat (prin1-to-string str) "\n")))


(defun set-api-version (command-args)
  (setq api-version
		(string-to-number (cdr
						   (assoc 'Version (json-read-from-string command-args))))))

(defun write-to-console (command-args)
  (message command-args)
  (write-line-to-fuse-buffer (cdr (assoc 'Text (json-read-from-string command-args)))))

  

(defun delegate-command (command-string)
  ;(princ command-string)
  (let ((command (json-read-from-string command-string)))
	(let ((command-type (cdr (assoc 'Command command))))
	  ;(print command-type)
	  (cond ((string= command-type "SetAPIVersion") (set-api-version (cdr (assoc 'Arguments command))))
			((string= command-type "WriteToConsole") (write-to-console (cdr (assoc 'Arguments command))))
			((string= command-type "SetCodeSuggestions") (write-to-console (cdr (assoc 'Arguments command))))
			))))


(defun set-features ()
  (interactive)
  (send-command (create-default-features-string)))


(defun send-request-code-completion ()
  (interactive)
  (send-command (get-code-completion-info)))


(defun send-new-build ()
  (interactive)
  (send-command (json-encode '((Command . NewBuild)))))

(defun send-recompile ()
  (interactive)
  (send-command (json-encode '((Command . Recompile)))))

;C-h m
