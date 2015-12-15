(defvar buffer-string "")
(defvar buffer-pointer -1)

(defun testing-reset ()
  (setq buffer-string "foobar\n4\nfarmasdok")
  (setq buffer-pointer -1))

(defun char-to-number (char)
  (string-to-number (char-to-string char)))

(defun get-next-char ()
  (if (= buffer-pointer (- (length buffer-string) 1))
	  -1
	(aref buffer-string (setq buffer-pointer (+ buffer-pointer 1)))))

(defun get-current-symbol ()
  (substring buffer-string 0 buffer-pointer))

(defun accept-message (length)
  (print (number-to-string length))
  (print (number-to-string (length buffer-string)))
  (print (number-to-string buffer-pointer))
  (let (ret)
	(setq ret (substring buffer-string buffer-pointer length))
	(setq buffer-pointer (+ buffer-pointer length))
	(setq buffer-string buffer-pointer (length buffer-string))))


(defun parse-event-type ()
  (let (c)
	(setq c (get-next-char))
	(while (or
			(not (equal c ?\n))
			(not (equal c -1)))
	  (setq c (get-next-char)))
	(if (equal c ?\n)
		(let (event-type)
		  (setq event-type (get-current-symbol))
		  (parse-message-length event-type))
	  -1)))

(defun parse-message-length (event-type)
  (let (c)
	(setq c (get-next-char))
	(while (or
			(not (equal c ?\n))
			(not (equal c -1)))
	  (setq c (get-next-char))
	  (if (equal c ?\n)
		  (let (m-len current-sym)
			(setq current-sym (get-current-symbol))
			(setq m-len (string-to-number current-sym))
			(print (format "We got length: %s" current-sym))
			(parse-message m-len))
		-1))))


(defun parse-message (message-length)
  (if (>= (- (length buffer-string) buffer-pointer) message-length)
	  (accept-message message-length)
	-1))







(defun plugin-filter (process message)
  (setq buffer-string (concat buffer-string message))
  (process-commands-in-buffer buffer-string))
