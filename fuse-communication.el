(require 'json)
(require 'cl)

(defvar listen-port 12122
    "port of the server")

(defvar listen-host "127.0.0.1"
  "host of the server")

(defvar fuse-client)


(defvar buffer-string "")

(defun string-integer-p (string)
   (if (string-match "\\`[-+]?[0-9]+\\'" string)
       t
     nil))

(defun trim-leading-chars (str)
  (dotimes (i (length str))
	(when (> (string-to-number (string (aref str i))) 0)
	  (return (substring str i)))))

(defun eat-buffer (message)
  (let ((received (setq buffer-string (trim-leading-chars (concat buffer-string message)))))
	(let ((command (split-string received "\n")))
	  (let ((command-length (string-to-number (car command))))
		(princ (length command))
		(princ (numberp (car command)))
		(print (car command))
		(when (and (> (length command) 1)
				   (string-integer-p (car command))
				   (>= (length (substring received (length (car command))))
					   command-length))
		  (progn
			(princ "foobar\n")
			(let ((command-string (substring
								   received
								   command-length
								   (length (cadr command)))))
			  (progn
				(write-line-to-fuse-buffer "We have command :D")
				(print received)
				(setq buffer-string
					   (substring received
								  (+ (length (car command))
									 command-length
									 1)))
				))))))))


(defun plugin-filter (process message)
  (eat-buffer message))
										;	(delegate-command message)


(defun create-connection ()
  (setq fuse-client
		(make-network-process
		 :name "fuse-client"
		 :family 'ipv4
		 :host listen-host
		 :service listen-port
		 :buffer "*fuse-plugin*"
		 :sentinel 'plugin-sentinel
		 :filter 'plugin-filter)))

(defun stop-connection ()
  (stop-process fuse-client))

(defun plugin-sentinel (process msg)
  
  )

(defun get-length-part (str)
  (concat
   (number-to-string
	(length str)) "\n"))

(defun create-default-features-string ()
  (create-set-features
   '(CodeCompletion
	 ShortcutFeature
	 Console
	 BuildEvent)))



(defun send-command (command)
  (let ((str (concat
			 (concat
			  (number-to-string (length command)) "\n")
			 command)))
	(print str)
	(process-send-string fuse-client str)))

(defun fuse-status ()
  (process-status fuse-client))
