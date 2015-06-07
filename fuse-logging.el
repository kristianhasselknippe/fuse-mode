(load-file "fuse-helpers.el")

(defvar fuse-debug-log (generate-new-buffer "*fuse-debug-log*"))
(defvar fuse-build-log (generate-new-buffer "*fuse-build-log*"))

(defun write-human-readable-to-buffer (str buffer)
  (save-excursion
	(set-buffer buffer)
	(goto-char (point-max))
	(princ str (lambda (c) (insert c)))))

(defun write-to-buffer (str buffer)
  (save-excursion
	(set-buffer buffer)
	(goto-char (point-max))
	(insert str)))

(defun insert-button-to-buffer (label act buffer)
  (save-excursion
	(set-buffer buffer)
	(insert-button label 'action act)))

(defun fuse-write-to-build-log (str)
  (write-human-readable-to-buffer str fuse-build-log)
  (write-to-buffer "\n" fuse-build-log))


(defun fuse-write-to-debug-log (str)
  (write-human-readable-to-buffer str fuse-debug-log)
  (write-to-buffer "\n" fuse-debug-log))

(defun contains-fuse-file-path (str)
  (string-match "[\\\/].*" str))


;{
;    "Command": "BuildEventRaised",
;    "Arguments":
;    {
;        "Path": "C:\\Test.uno",
;        "Type": "Error",
;        "StartPosition": { "Line": 1, "Character": 4 },
;        "EndPosition": { "Line": 1, "Character": 8 },
;        "ErrorCode": 0,
;        "Message": "Unexpected semicolon after ..."
;    }

										;}

(defvar fuse-errors (generate-new-buffer "*fuse-errors*"))

(defun insert-open-file-button (path line char)
  (lexical-let ((path path)
				(line line)
				(char char))
  (insert-button path
				 'action (lambda (button)
						   (progn
							 (set-buffer (find-file path))
							 (goto-char 0)
							 (forward-line line)
							 (goto-char (+ (point) char)))))))


(defun write-build-event (build-event-arguments)
  (let ((path (cdr-assoc 'Path build-event-arguments))
		(type (cdr-assoc 'Type build-event-arguments))
		(start-position (cdr-assoc 'StartPosition build-event-arguments))
		(end-position (cdr-assoc 'EndPosition build-event-arguments))
		(error-code (cdr-assoc 'ErrorCode build-event-arguments))
		(message (cdr-assoc 'Message build-event-arguments)))
    (save-excursion
	  (set-buffer fuse-errors)
	  (goto-char (point-max))
	  (insert (concat type ": "))
	  (insert-open-file-button path
							   (cdr-assoc 'Line end-position)
							   (cdr-assoc 'Character end-position))
	  (insert "\n\n" message "\n\n"))))
