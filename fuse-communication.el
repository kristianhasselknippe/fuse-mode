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
  (let ((ret "")
		(loop? 't)
		(i 0))
	(while loop?
	  (progn
		(if (> (string-to-number (substring str i (+ i 1))) 0)
			(progn
			  (setq ret (substring str i))
			  (setq loop? nil)))
		(setq i (+ i 1))
		(if (>= i (length str))
			(setq loop? nil))))
	ret))

(defun string-list-to-string (string-list)
  (let ((ret ""))
	(dotimes (i (length string-list))
	  (setq ret (concat ret (nth i string-list))))
	ret))

(defun pop-command-from-string (str)
  (if (> (length str) 0)
	  (let ((strings (split-string (trim-leading-chars str) "\n")))
		(if (> (length strings) 1)
			(if (string-integer-p (car strings))
				(let ((command-length (string-to-number (car strings)))
					  (tail (string-list-to-string (cdr strings))))
				  (if (>= (length tail) command-length)
					  (substring tail 0 command-length)
					""))
			  "")
		  ""))
	""))

(defun command-string-length-including-size (command-string)
  (length 
   (concat (number-to-string (length command-string)) "\n" command-string)))



(defun process-commands-in-buffer (buffer-str)
  (let ((command (pop-command-from-string (setq buffer-string (concat message buffer-string)))))
	(message (concat "Command foooooooooooooooo: \n" command))
	(if (> (length command) 0)
		(progn
		  (message "We got a command yo :D")
		  (setq buffer-string (substring buffer-string (+ (command-string-length-including-size command) 1)))
		  (delegate-command command)))))

(defun plugin-filter (process message)
  (progn
	(setq buffer-string (concat buffer-string message))
	(process-commands-in-buffer buffer-string)))


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
  (if (equal (fuse-status) 'closed)
	  (progn
		(create-connection)
		(set-features)))
  (let ((str (concat
			  (concat
			   (number-to-string (length command)) "\n")
			  command)))
	;(print str)
	(process-send-string fuse-client str)))

(defun fuse-status ()  
  (process-status fuse-client))
