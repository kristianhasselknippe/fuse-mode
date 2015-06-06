(load-file "./fuse-communication.el")
(load-file "./fuse-file-info.el")
(load-file "./fuse-send-commands.el")
(load-file "./fuse-logging.el")

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




(defun set-api-version (command-args)
  (setq api-version
		(string-to-number (cdr
						   (assoc 'Version (json-read-from-string command-args))))))


(defun display-code-suggestions (command-args)
  (assoc 'CodeSuggestions (json-read-from-string (cdr command-args))))

(defun write-to-console (command)
  (message "sodowidjqowdijqwdoiqjwdoqijwdoqwidj")
  (let ((command-args (json-read-from-string command)))
	(let ((log-type (cdr (assoc 'Type command-args)))
		  (text (cdr (assoc 'Text command-args))))
	  (message "aspokaspdok")
	  (cond
	   ((string= log-type "DebugLog")(fuse-write-to-debug-log text))
	   ((string= log-type "BuildLog")(fuse-write-to-build-log text))))))
										;(write-line-to-fuse-buffer (cdr (assoc 'Text (json-read-from-string command-args)))))

(defun open-definition-in-new-buffer (path line)
  (progn
	;(message (concat "path: " path))
	;(message (number-to-string line))
	(set-buffer (find-file path))
	(goto-char 0)
	(forward-line line)))

(defun goto-definition (command-args)
  (let ((args (json-read-from-string command-args)))
	(if (cdr (assoc 'FoundDefinition args))
		(let ((path (cdr (assoc 'Path args)))
			  (line (cdr (assoc 'Line (assoc 'CaretPosition args)))))
		  (open-definition-in-new-buffer path line)))))

(defun delegate-command (command-string)
  (message command-string)
  (let ((command (json-read-from-string command-string)))
	(let ((command-type (cdr (assoc 'Command command))))
	  (cond
	   ((string= command-type "WriteToConsole")
		(write-to-console (cdr (assoc 'Arguments command))))
	   ((string= command-type "GoToDefinitionResponse")
		(goto-definition (cdr (assoc 'Arguments command))))
	   ((string= command-type "SetCodeSuggestions")
		  (display-code-suggestions (cdr (assoc 'Arguments command))))
	   ))))


(defun set-features ()
  (interactive)
  (send-command (create-default-features-string)))


(defun send-request-code-completion ()
  (interactive)
  (send-command (get-code-completion-info 'RequestCodeCompletion)))


(defun send-new-build ()
  (interactive)
  (send-command (json-encode '((Command . NewBuild)))))

(defun send-recompile ()
  (interactive)
  (send-command (json-encode '((Command . Recompile)))))

(defun send-goto-definition()
  (interactive)
  (let ((cmd (get-code-completion-info 'GotoDefinition)))
	;(message cmd)
	(send-command cmd)))


;C-h m


;not sure about this one
;(noop (defun fuse-completion-at-point ())
;  (interactive)
;  (let ((pt (point)) ;; collect point
;        start end)
;          (goto-char pt)
;          (re-search-backward "\\S *")
;          (setq start (point))
;          (re-search-forward "\\S *")
;          (setq end (point))
;          (list start end (cdr ())))
;
;(push 'fuse-completion-at-point completion-at-point-functions))
