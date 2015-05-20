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
  (create-set-features '(CodeCompletion ShortcutFeature)))



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

(defun delegate-command (command-string)
  (let ((command (json-read-from-string command-string)))
	(let ((command-type (cdr (assoc 'Command command))))
		(cond ((string= command-type "SetAPIVersion") (set-api-version (cdr (assoc 'Arguments command))))))))


(defun set-features ()
  (send-command (create-default-features-string)))


(defun request-code-completion (id path text type c-line c-character)
  (send-command
	(json-encode `((Command . RequestCodeCompletion)
				  (Arguments .
							  ((QueryId . ,id)
							   (Path . ,path)
							   (Text . ,text)
							   (Type . ,type)
							   (CaretPosition . ((Line . ,c-line) (Character . ,c-character)))))))))


(defun get-suggestions ()
  (interactive)
  (request-code-completion 2 "path" "text" "type" 4 9))





;C-h m
