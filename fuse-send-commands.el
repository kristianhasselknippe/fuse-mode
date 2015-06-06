(load-file "./fuse-communication.el")

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
