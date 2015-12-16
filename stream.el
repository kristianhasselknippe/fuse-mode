(defvar buffer-string "")
(defvar buffer-pointer -1)
(defvar current-sym-pointer 0)

(defun testing-reset ()
  (setq buffer-string "foobar\n4\nfarmasdok")
  (setq buffer-pointer -1)
  (setq current-sym-pointer 0))

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
	(print (format "Symbol %s" ret))
	ret))


(defun parse-event-type ()
  (let (c)
	(while (and (not (eq c ?\n)) (not (eq c -1)))
	  (setq c (get-next-char)))
	(if (equal c ?\n)
		(let (event-type)
		  (backtrack-1)
		  (setq event-type (get-current-symbol))
		  (get-next-char)
		  (parse-message-length event-type))
	  -1)))

(defun parse-message-length (event-type)
  (let (c)
	(while (and (not (eq c ?\n)) (not (eq c -1)))
	  (setq c (get-next-char))
	  (if (equal c ?\n)
		  (let (m-len current-sym)
			(backtrack-1)
			(setq current-sym (get-current-symbol))
			(setq m-len (string-to-number current-sym))
			(get-next-char)
			(parse-message m-len))
		-1))))


(defun parse-message (message-length)
  (if (>= (- (length buffer-string) buffer-pointer) message-length)
	  (progn
		(setq buffer-pointer (+ buffer-pointer message-length))
		(get-current-symbol))
	-1))




(defun plugin-filter (process message)
  (setq buffer-string (concat buffer-string message))
  (process-commands-in-buffer buffer-string))
