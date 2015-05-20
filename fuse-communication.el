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

;todo: this should not have side effects!
(defun pop-command (str)
  "Pops a command from str and modifies buffer-string"
  (let ((strings (split-string str "\n")))
	(if (> (length (trim-leading-chars (car strings))) 0)
	(let ((command-length (string-to-number (trim-leading-chars (car strings))))
		  (tail-string (reduce (lambda (s1 s2) (concat s1 "\n" s2)) (cdr strings))))
	  (if (>= (length tail-string) command-length)
		  (progn
			(setq buffer-string (substring tail-string command-length))
			(substring tail-string 0 command-length))
		""))
	"")))


(defun plugin-filter (process message)
  (delegate-command
   (pop-command (setq buffe-string (concat message buffer-string)))))


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
