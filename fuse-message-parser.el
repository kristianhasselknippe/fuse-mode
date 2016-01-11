(defvar fuse--buffer-string "")
(defvar fuse--buffer-pointer -1)
(defvar fuse--symbol-pointer -1)

(defun fuse--get-next-character ()
  "Increments the buffer pointer and returns the character pointed to"
  (if (equal fuse--buffer-pointer (1- (length fuse--buffer-string)))
	  nil
	(progn
	  (setq fuse--buffer-pointer (1+ fuse--buffer-pointer))
	  (aref fuse--buffer-string fuse--buffer-pointer))))

(defun fuse--get-current-character ()
  "Returns the current character pointed to by buffer pointer"
  (if (equal fuse--buffer-pointer (length fuse--buffer-string))
	  nil
	(aref fuse--buffer-string fuse--buffer-pointer)))

(defun fuse--get-current-symbol ()
  "Gets the current symbol and sets the symbol pointer equal to buffer pointer"
  (let (ret)
	(setq ret (substring fuse--buffer-string fuse--symbol-pointer fuse--buffer-pointer))
	(setq fuse--symbol-pointer fuse--buffer-pointer)
	ret))

(defun fuse--parse-to-character (char)
  "Parses until the specified character is found"
  (let ((current-char (fuse--get-next-character)))
	(while (and (not (equal current-char char))
				(not (equal current-char nil)))
	  (setq current-char (fuse--get-next-character)))
	current-char))

(defun fuse--parse-line ()
  "Parses a linew (separated by \n)"
  (fuse--parse-to-character ?\n)
  (if (equal (fuse--get-current-character) ?\n)
	  (let (ret)
		(setq ret (fuse--get-current-symbol))
		(setq fuse--symbol-pointer (1+ fuse--symbol-pointer))
		(setq fuse--buffer-pointer (1+ fuse--buffer-pointer))
		ret)
	nil))

(defun fuse--parse-bytes (nBytes)

  )

(defun fuse--parse-message ()
  )

(defun fuse-client-parse ()
  )



;(list event-type message)
