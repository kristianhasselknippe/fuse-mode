(defvar buffer-string "")
(defvar buffer-pointer -1)
(defvar current-sym-pointer 0)

(defun testing-reset ()
  (setq buffer-string "foobar\n5\nfarmaother\n10\nonetwothreefourfivesix")
  (setq buffer-pointer -1)
  (setq current-sym-pointer 0))

(defun print-fuse-mode ()
  (print (format "BS: %s, BP: %d, CSP: %d" buffer-string buffer-pointer current-sym-pointer)))

(defun char-to-number (char)
  (string-to-number (char-to-string char)))

(defun get-next-char ()
  (if (= buffer-pointer (1- (length buffer-string)))
	  -1
	(aref buffer-string (setq buffer-pointer (1+ buffer-pointer)))))

(defun peek-next-char ()
  (if (< buffer-pointer (1- (length buffer-string)))
	  (aref buffer-string (+ buffer-pointer))
	-1 ))

(defun backtrack-1 ()
  (setq buffer-pointer (1- buffer-pointer)))

(defun get-current-symbol ()
  (let (ret)
	(setq ret (substring buffer-string current-sym-pointer buffer-pointer))
	(setq current-sym-pointer buffer-pointer)
	ret))


(defun parse-event-type ()
  (let (c)
	(while (and (not (eq c ?\n)) (not (eq c -1)))
	  (setq c (get-next-char)))
	(if (equal c ?\n)
		(let (event-type)
		  (setq event-type (get-current-symbol))
		  (get-next-char)
		  (get-current-symbol)
		  event-type)
	  -1)))

(defun parse-message-length ()
  (let (c)
	(while (and (not (eq c ?\n)) (not (eq c -1)))
	  (setq c (get-next-char)))
	(if (equal c ?\n)
		(let (m-len current-sym)
		  (setq current-sym (get-current-symbol))
		  (setq m-len (string-to-number current-sym))
		  (get-next-char)
		  (get-current-symbol)
		  m-len)
	  -1)))


(defun parse-message (message-length)
  (if (>= (- (length buffer-string) buffer-pointer) message-length)
	  (let (ret)
		(setq buffer-pointer (+ buffer-pointer message-length))
		(setq ret (get-current-symbol))
		ret)
	-1))




(defun plugin-filter (process message)
  (setq buffer-string (concat buffer-string message))
  (process-commands-in-buffer buffer-string))

(defun fuse-client-parse ()
  (let (event-type length message)
	(setq event-type (parse-event-type))
	(setq length (parse-message-length))
	(setq message (parse-message length))

	(format "Result: %s, %d, %s" event-type length message)))


(defun test-fuse-mode ()
  (testing-reset)
  (fuse-client-parse))
