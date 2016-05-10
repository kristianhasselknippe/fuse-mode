(require 'cl)
(require 'dash)
(require 's)
(require 'json)
(require 'edebug)

(defun cdra (key alist)
  (cdr (assoc key alist)))

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


(setq fuse--buffer "")
(setq fuse--daemon-proc nil)

(defstruct-and-to-obj event name subscription-id data)
(defstruct-and-to-obj issue-detected-data build-id issue-type path start-pos end-pos error-code message)
(defstruct-and-to-obj subscribe-request-args filter replay id)
(defstruct-and-to-obj caret-position line character)
(defstruct-and-to-obj code-completion-request-args syntax-type path text caret-position)
(defstruct-and-to-obj request name id arguments)
(defstruct-and-to-obj response id status result errors)
(defstruct-and-to-obj message type length payload)
(defstruct-and-to-obj subscription-response id status error results)

(defstruct-and-to-obj method-argument name arg-type is-out)
(defstruct-and-to-obj code-suggestions suggestion pre-text post-text type return-type access-modifiers field-modifiers method-arguments)
(defstruct-and-to-obj code-completion-response is-updating-cache code-suggestions)



(defun message-to-string (message)
  (unless (message-p message) (error "message-to-string only handles message types"))
  (format "%s\n%d\n%s\n" (message-type message) (message-length message) (message-payload message)))


(defun fuse--log (msg)
  (with-current-buffer (get-buffer-create "fuse-log")
	(insert msg)))


(defun fuse--request-services ()
  (let* ((request (make-request :name "Subscribe" :id 0
								:arguments (make-subscribe-request-args
											:filter "Fuse.BuildIssueDetected" :replay t :id 1)))
		   (payload (json-encode (request-to-obj request)))
		   (message (make-message :type "Request"
								  :length (length payload)
								  :payload payload)))
	  (process-send-string fuse--daemon-proc (message-to-string message))))


(defun fuse--log-issue-detected (data)
  (fuse--log (concat
			  "ErrorCode: " (issue-detected-data-error-code data)
			  "\nPath: " (issue-detected-data-path data)
			  "\nLine: " (number-to-string (cdra 'Line (issue-detected-data-start-pos data))))))


(defun fuse--request-code-completion ()
  (interactive)
  (let* ((request (make-request :name "Fuse.GetCodeSuggestions"
								:id 2
								:arguments (make-code-completion-request-args
											:syntax-type (s-upcase (car (last (s-split "\\." (buffer-file-name)))))
											:path (s-replace "/" "\\" (buffer-file-name))
											:text (buffer-substring-no-properties (point-min) (point-max))
											:caret-position (make-caret-position
															 :line (count-lines 1 (point))
															 :character (current-column))))))
	(let* ((req-obj (fuse--serializable request))
		   (req-obj-json (json-encode req-obj))
		   (msg (make-message :type "Request"
								  :length (length req-obj-json)
								  :payload req-obj-json)))
	  (princ (message-to-string msg))
	  (process-send-string fuse--daemon-proc (message-to-string msg)))))

(defvar ac-source-fuse-mode
  '((candidates . (list "Foo" "Bar" "Baz"))))

(defun ac-complete-fuse-mode ()
  (interactive)
  (auto-complete '(ac-source-fuse-mode)))

(defun fuse--filter (proc msg)
  (message msg)
  (setf fuse--buffer (concat fuse--buffer msg))
  (let ((message-split (s-split-up-to "\n" fuse--buffer 3)))
	(when (>= (length message-split) 3)
	  (let ((message (make-message :type (nth 0 message-split)
								   :length (nth 1 message-split)
								   :payload (nth 2 message-split))))

		(if (= (length message-split) 4)
			(setf fuse--buffer (nth 3 message-split))
		  (setf fuse--buffer ""))
		(cond ((string= (message-type message) "Response") (let ((decoded-payload (json-read-from-string (message-payload message))))
															 (princ (fuse--serializable message))

															 ;(cond ((string=
																  (response (make-response
																			 :id (cdra 'Id decoded-payload)
																			 :status (cdra 'Status decoded-payload)
																			 :errors (cdra 'Errors decoded-payload)))
															 ))
			  ((string= (message-type message) "Event")(let* ((decoded-payload (json-read-from-string (message-payload message)))
															  (event (make-event :name (cdra 'Name decoded-payload)
																				 :subscription-id (cdra 'SubscriptionId decoded-payload)
																				 :data (cdra 'Data decoded-payload)))
															  (decoded-data (cdr (assoc 'Data decoded-payload)))
															  (data (make-issue-detected-data :build-id (cdra 'BuildId decoded-data)
																							  :issue-type (cdra 'IssueType decoded-data)
																							  :path (cdra 'Path decoded-data)
																							  :start-pos (cdra 'StartPosition decoded-data)
																							  :end-pos (cdra 'EndPosition decoded-data)
																							  :error-code (cdra 'ErrorCode decoded-data)
																							  :message (cdra 'Message decoded-data))))
														 (fuse--log-issue-detected data))))))))

;{
;    "Id": 42, // Id of request
;    "Status": "Success",
;    "Result":
;    {
;        "IsUpdatingCache": false, // If true you should consider trying again later
;        "CodeSuggestions":
;        [
;            {
;                "Suggestion": "<suggestion>",
;                "PreText": "<pretext>",
;                "PostText": "<posttext>",
;                "Type": "<datatype>",
;                "ReturnType": "<datatype>",
;                "AccessModifiers": [ "<accessmodifier>", ... ],
;                "FieldModifiers": [ "<fieldmodifier>", ... ],
;                "MethodArguments":
;                [
;                    { "Name": "<name>", "ArgType": "<datatype>", "IsOut": "<false|true>" },
;                    ...
;                ],
;            },
;            ...
;            ],
;    }
;}

(defun fuse--mode-init ()
  (if (equal system-type 'windows-nt)
	  (setf fuse--daemon-proc (start-process "fuse-mode"
											 "fuse-mode"
											 "C:/Program Files (x86)/Fuse/Fuse.exe" "daemon-client" "fuse-mode"))
	(setf fuse--daemon-proc (start-process "fuse-mode"
										   "fuse-mode"
										   "/usr/local/bin/fuse" "daemon-client" "fuse-mode")))

  (set-process-filter fuse--daemon-proc 'fuse--filter)
  (set-process-sentinel fuse--daemon-proc (lambda (proc msg) ))
  (fuse--request-services))




;;;###autoload
(define-minor-mode fuse-mode
  "The Fuse minor mode."
  :lighter "fuse"
  :keymap (make-sparse-keymap))

;;;###autoload
(add-hook 'fuse-mode-hook 'fuse--mode-init)

(provide 'fuse-mode)

 ;;; fuse-mode.el ends here
