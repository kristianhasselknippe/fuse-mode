(require 'json)

(defvar listen-port 12122
    "port of the server")

(defvar listen-host "127.0.0.1"
  "host of the server")

(defvar fuse-client)

(defun send-command (command)
  (let ((str (concat
			 (concat
			  (number-to-string (length command)) "\n")
			 command)))
	(print str)
	(process-send-string fuse-client str)))


(defun plugin-filter (process message)
	(delegate-command message))

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


(defun fuse-status ()
  (process-status fuse-client))
