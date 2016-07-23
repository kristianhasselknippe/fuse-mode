(require 'cl-lib)
(require 'popup)
(require 'dash)
(require 's)
(require 'json)
(require 'edebug)
(require 'ov)

(defun cdra (key alist)
  (cdr (assoc key alist)))

(defun line-offset ()
  (interactive)
  (let* ((p2 (line-beginning-position))
		 (p1 (point))
		 (p-res (- p1 p2)))
	p-res))

(defvar fuse--structs '())

(defun fuse--serializable (struct)
  (let ((didFindMatch 'nil)
		(ret 'nil))
	(-each fuse--structs
	  (lambda (pred)
		(when (funcall pred struct)
		  (progn
			(setq ret (funcall (intern (format "%s-to-obj"
											   (s-join "-" (-remove-last (lambda (x) t) (s-split "-" (symbol-name pred))))
											   )) struct))
			(setq didFindMatch t)))))
	(if didFindMatch
		ret
	  struct)))

(defmacro defstruct-and-to-obj (name &rest args)
  (append `(progn) `((cl-defstruct ,name ,@args)
					 ,(append `(defun ,(intern (format "%s-to-obj" name)) (arg))
							  `(,(let ((body `()))
								   (-each args (lambda (a)
												 (setq body (append body
																	`(,(intern (format ":%s" a)))
																	`((fuse--serializable (,(intern (format "%s-%s" name a)) arg)))))))
								   `(list ,@body)))))
		  `((setq fuse--structs (append fuse--structs (list (quote ,(intern (format "%s-p" name)))))))))


(defvar fuse--buffer)
(defvar fuse--daemon-proc)
(setq fuse--buffer "")
(setq fuse--daemon-proc nil)

(defstruct-and-to-obj event Name Data)
(defstruct-and-to-obj selection-changed-data Path Text CaretPosition)

(defstruct-and-to-obj issue-detected-data BuildId IssueType Path StartPosition EndPosition ErrorCode Message)
(defstruct-and-to-obj subscribe-request-args Filter Replay Id)
(defstruct-and-to-obj caret-position Line Character)
(defstruct-and-to-obj code-completion-request-args SyntaxType Path Text CaretPosition)
(defstruct-and-to-obj request Name Id Arguments)
(defstruct-and-to-obj response Id Status Result Errors)
(defstruct-and-to-obj message Type Length Payload)
(defstruct-and-to-obj subscription-response Id Status Error Results)

(defstruct-and-to-obj method-argument name ArgType IsOut)
(defstruct-and-to-obj code-suggestions Suggestion PreText PostText Type ReturnType AccessModifiers FieldModifiers MethodArguments)
(defstruct-and-to-obj code-completion-response IsUpdatingCache CodeSuggestions)


(defun message-to-string (message)
  (unless (message-p message) (error "message-to-string only handles message types"))
  (format "%s\n%d\n%s\n0\n" (message-Type message) (message-Length message) (message-Payload message)))


(defun fuse--log (msg)
  (with-current-buffer (get-buffer-create "fuse-log")
	(insert msg)))

(defun fuse--debug-log (msg)
  (with-current-buffer (get-buffer-create "fuse-debug-log")
						 (insert msg)))

(defun pos-at-line-col (pos)
  (save-excursion
	(let ((l (1- (cdra 'Line pos)))
		  (c (cdra 'Character pos)))
	  (goto-char (point-min))
	  (forward-line l)
	  (move-to-column c)
	  (point))))

(defun pos-at-end-of-line (line)
  (save-excursion
	(goto-char (point-min))
	(forward-line (- line 1))
	(line-end-position)))

(defun pos-at-start-of-line (line)
  (save-excursion
	(goto-char (point-min))
	(forward-line (- line 1))
	(message (format "LAP: %d" (line-number-at-pos (point))))
	(back-to-indentation)
	(point)))

;BuildId IssueType Path StartPosition EndPosition ErrorCode Message)
(defun fuse--log-issue-detected (data)
  (fuse--debug-log "issue detected")
  (let ((error-code (issue-detected-data-ErrorCode data))
		(path (issue-detected-data-Path data))
		(line (number-to-string (cdra 'Line (issue-detected-data-StartPosition data))))
		(start-pos (issue-detected-data-StartPosition data))
		(end-pos (issue-detected-data-EndPosition data)))
	(fuse--log (concat
				"ErrorCode: " error-code
				"\nPath: " path
				"\nLine: " line))
	(when (equal (downcase (file-name-base (buffer-file-name)))
				 (downcase (file-name-base path)))
	  (unless (equal start-pos 'nil)
		(if (equal end-pos 'nil)
			(progn
			  (let ((sp (pos-at-start-of-line (cdra 'Line start-pos)))
					(ep (pos-at-end-of-line (cdra 'Line start-pos))))
				(message "going to start and end")
				(message (format ":::::: %d %d" (cdra 'Line start-pos) (cdra 'Line start-pos)))
				(let ((overlay (ov-make sp ep)))
				  (ov-set overlay 'face '(:foreground "red")))))
										;do something for this case
		  )))))


(defun fuse--filter (proc msg)
  (when (s-contains? "Failed to parse message length." msg)
	(fuse--debug-log "Server: failed to parse mssage length.")
	(edebug))
  (fuse--debug-log msg)
  (setf fuse--buffer (concat fuse--buffer msg))
  (let ((message-split (s-split-up-to "\n" fuse--buffer 2)))
	(when (>= (length message-split) 2)
	  (let ((msg-len (string-to-number (nth 1 message-split))))
		(when (>= (length (nth 2 message-split)) msg-len)
		  (let* ((message (make-message
						   :Length msg-len
						   :Type (nth 0 message-split)
						   :Payload (substring-no-properties (nth 2 message-split) 0 msg-len))))

			(setf fuse--buffer (substring (nth 2 message-split) msg-len (length (nth 2 message-split))))
			(cond ((string= (message-Type message) "Response")
				   (let* ((decoded-payload (json-read-from-string (message-Payload message)))
						  (response
						   (make-response
							:Id (cdra 'Id decoded-payload)
							:Status (cdra 'Status decoded-payload)
							:Errors (cdra 'Errors decoded-payload)
							:Result (cdra 'Result decoded-payload))))
					 (cond ((= (response-Id response) 2)

							(let ((code-com-resp
								   (make-code-completion-response
									:IsUpdatingCache (cdra 'IsUpdatingCache (response-Result response))
									:CodeSuggestions (cdra 'CodeSuggestions (response-Result response))))
								  (completions-cache '()))
							  (-each (append (code-completion-response-CodeSuggestions code-com-resp) nil)
								(lambda (sugg)
								  (let ((code-suggestion
										 (make-code-suggestions
										  :Suggestion (cdra 'Suggestion sugg)
										  :PreText (cdra 'PreText sugg)
										  :PostText (cdra 'PostText sugg)
										  :Type (cdra 'Type sugg)
										  :ReturnType (cdra 'ReturnType
															sugg)
										  :AccessModifiers (cdra 'AccessModifiers sugg)
										  :FieldModifiers (cdra 'FieldModifiers sugg)
										  :MethodArguments (cdra 'MethodArguments sugg))))
									(setq completions-cache
										  (append completions-cache
												  (list code-suggestion))))))
							  (fuse--completion-callback completions-cache))))))
				               ;; CODE COMPLETION ^^^^^^

				  ((string= (message-Type message) "Event")
				   (let* ((decoded-payload (json-read-from-string (message-Payload message)))
						  (event (make-event :Name (cdra 'Name decoded-payload)
											 ;:SubscriptionId (cdra 'SubscriptionId decoded-payload)
											 :Data (cdra 'Data decoded-payload)))
						  (decoded-data (cdr (assoc 'Data decoded-payload)))
						  (data (make-issue-detected-data :BuildId (cdra 'BuildId decoded-data)
														  :IssueType (cdra 'IssueType decoded-data)
														  :Path (cdra 'Path decoded-data)
														  :StartPosition (cdra 'StartPosition decoded-data)
														  :EndPosition (cdra 'EndPosition decoded-data)
														  :ErrorCode (cdra 'ErrorCode decoded-data)
														  :Message (cdra 'Message decoded-data))))
					 (fuse--log-issue-detected data))))))))))




(defvar fuse--was-initiated 'nil)
(defun fuse--mode-init ()
  (when (equal fuse--daemon-proc nil)
	(if (equal system-type 'windows-nt)
		(setf fuse--daemon-proc (start-process "fuse-mode"
											   "fuse-mode"
											   "C:/Program Files (x86)/Fuse/Fuse.exe" "daemon-client" "fuse-mode"))
	  (setf fuse--daemon-proc (start-process "fuse-mode"
											 "fuse-mode"
											 "/usr/local/bin/fuse" "daemon-client" "fuse-mode")))

	(set-process-filter fuse--daemon-proc 'fuse--filter)
	(set-process-sentinel fuse--daemon-proc (lambda (proc msg) (fuse--debug-log msg) ))
	(setq fuse--buffer "")
	(fuse--request-services)))

(defun fuse--process-send-string (msg)
  (when (equal fuse--was-initiated 'nil)
	(fuse--mode-init)
	(setq fuse--was-initiated 't))
  (fuse--debug-log msg)
  (process-send-string fuse--daemon-proc msg))

(defun fuse--request-services ()
  (fuse--debug-log "requesting services")
  (let* ((request (make-request :Name "Subscribe" :Id 0
								:Arguments (make-subscribe-request-args
											:Filter "Fuse.BuildIssueDetected" :Replay t :Id 1)))
		 (payload (json-encode (request-to-obj request)))
		 (message (make-message :Type "Request"
								:Length (string-bytes payload)
								:Payload payload)))
	(let ((msg (message-to-string message)))
	  (fuse--debug-log msg)
	  (fuse--process-send-string msg))))

(defun fuse--get-buffer-path ()
  (if (equal system-type 'windows-nt)
	  (s-replace "/" "\\" (buffer-file-name))
	(buffer-file-name)))

(defun fuse--get-buffer-text ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun fuse--request-code-completion ()
  (let* ((request (make-request :Name "Fuse.GetCodeSuggestions"
								:Id 2
								:Arguments (make-code-completion-request-args
											:SyntaxType (car (last (s-split "\\." (buffer-file-name))))
											:Path (fuse--get-buffer-path)
											:Text (fuse--get-buffer-text)
											:CaretPosition (make-caret-position
															:Line (count-lines 1 (point))
															:Character (+ (line-offset) 1))))))
	(let* ((req-obj (fuse--serializable request))
		   (req-obj-json (json-encode req-obj))
		   (message-to-send (make-message :Type "Request"
										  :Length  (string-bytes req-obj-json)
										  :Payload req-obj-json))
		   (the-message (message-to-string message-to-send)))
	  (fuse--process-send-string the-message))))


(defun fuse--selection-changed ()
  (interactive)
  (let ((event (make-event :Name "Fuse.Preview.SelectionChanged"
						   :Data (make-selection-changed-data :Path (fuse--get-buffer-path)
															  :Text (fuse--get-buffer-text)
															  :CaretPosition (make-caret-position
																			  :Line  (count-lines 1 (point))
																			  :Character (+ (line-offset) 1))))))
	(let* ((req-obj (fuse--serializable event))
		   (req-obj-json (json-encode req-obj))
		   (message-to-send (make-message :Type "Event"
										  :Length (string-bytes req-obj-json)
										  :Payload req-obj-json))
		   (the-message (message-to-string message-to-send)))
	  ;(fuse--debug-log the-message)
	  (fuse--process-send-string the-message))))


(defun fuse--get-necessary-completion (selected-value)
  (save-excursion
	(let ((m (point)))
	  (backward-word)
	  (let ((substr (buffer-substring-no-properties (point) m)))
		(s-chop-prefix substr selected-value)))))

(defun fuse--char-looking-back (&optional count)
  (save-excursion
	(let ((m (point)))
	  (backward-char (if (not (equal count nil)) count 1))
	  (buffer-substring-no-properties (point) m))))


(defun fuse--word-looking-back ()
  (if (equal " " (fuse--char-looking-back))
	  ""
	(save-excursion
	  (let ((m (point)))
		(if
			(backward-word)
			(buffer-substring-no-properties (point) m))))))

(defun fuse--get-symbol-looking-back ()
  (cond
   ((equal "=\"" (fuse--char-looking-back 2)) "")
   ((equal "<" (fuse--char-looking-back)) "")
   (t (fuse--word-looking-back))))


(defun fuse--completion-callback (list)
  (if (equal list 'nil)
	  (message "Found no completions")
	(let ((initial-search-value (fuse--get-symbol-looking-back)))
	  (message initial-search-value)
	  (let ((selected-value (popup-menu* (->> list (-map (lambda (item)
														   (code-suggestions-Suggestion item)))
											  (-filter (lambda (i)
														 (if (= (length initial-search-value) 0)
															 i
														   (s-starts-with? initial-search-value i)))))
										 :isearch 't
										 :isearch-filter (lambda (pattern list)
														   (-filter (lambda (i)
																	  (s-starts-with? (concat initial-search-value pattern) i)) list)))))
		(insert (fuse--get-necessary-completion selected-value))))))


(defun fuse-auto-complete ()
  (interactive)
  (fuse--request-code-completion))


;;Here comes some utilities for creating new Fuse related files
(defun fuse--file-created ()
  (when (stringp buffer-file-name)
	(when (string-empty-p (buffer-string))
	  (cond
	   ((string-match "\\.js\\'" buffer-file-name) (insert "var Observable = require('FuseJS/Observable');


module.exports = {

};")
		(js2-mode))
	   ((string-match "\\.ux\\'" buffer-file-name)
		(let* ((class-name (file-name-base buffer-file-name))
			   (content (concat "<Panel ux:Class=\"" class-name "\">\n</Panel>")))
		  (insert content)
		  (nxml-mode)))
	   ((string-match "\\.unoproj\\'" buffer-file-name)
		(json-mode))))))

(add-hook 'find-file-hook 'fuse--file-created)




;;;###autoload
(define-minor-mode fuse-mode
  "The Fuse minor mode."
  :lighter "fuse"
  :keymap (make-sparse-keymap))

(defun fuse--next-line-and-selection-change ()
  (interactive)
  (next-line)
  (fuse--selection-changed))

(defun fuse--prev-line-and-selection-changed ()
  (interactive)
  (previous-line)
  (fuse--selection-changed))

;;;###autoload
(add-hook 'fuse-mode-hook 'fuse--mode-init)

(define-key fuse-mode-map (kbd "C-c c") 'fuse-auto-complete)

(define-key fuse-mode-map (kbd "C-n") 'fuse--next-line-and-selection-change)

(define-key fuse-mode-map (kbd "C-p") 'fuse--prev-line-and-selection-changed)

(provide 'fuse-mode)

 ;;; fuse-mode.el ends here
