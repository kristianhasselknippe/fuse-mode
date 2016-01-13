(defvar fuse--buffer-string "")
(defvar fuse--buffer-pointer -1)
(defvar fuse--symbol-pointer 0)

(defun fuse--get-current-character ()
  "Returns the current character pointed to by buffer pointer"
  (if (or (< fuse--buffer-pointer 0)
		  (>= fuse--buffer-pointer (length fuse--buffer-string)))
	  nil
	(aref fuse--buffer-string fuse--buffer-pointer)))

(defun fuse--get-next-character ()
  "Increments the buffer pointer and returns the character pointed to"
  (if (or (equal (length fuse--buffer-string) 0)
		  (>= fuse--buffer-pointer (1- (length fuse--buffer-string)))
		  (< fuse--buffer-pointer -1))
	  nil
	(progn
		  (setq fuse--buffer-pointer (1+ fuse--buffer-pointer))
		  (aref fuse--buffer-string fuse--buffer-pointer))))

(defun fuse--get-current-symbol ()
  "Gets the current symbol and sets the symbol pointer equal to buffer pointer"
  (if (or (>= fuse--symbol-pointer fuse--buffer-pointer)
		  (< 0 fuse--symbol-pointer)
		  (>= fuse--buffer-pointer (1- (length fuse--buffer-string))))
	  nil
	(let (ret)
	  (setq ret (substring fuse--buffer-string fuse--symbol-pointer fuse--buffer-pointer))
	  (setq fuse--symbol-pointer fuse--buffer-pointer)
	  ret)))

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
  "Parses nBytes number of characters"
  (setq fuse--buffer-pointer (+ fuse--buffer-pointer nBytes))
  (fuse--get-current-symbol))

(defun fuse--consume-buffer-to-ptr ()
  (setq fuse--buffer-string (substring fuse--buffer-string fuse--buffer-pointer))
  (setq fuse--buffer-pointer -1)
  (setq fuse--symbol-pointer -1))

(defun fuse--parse-message ()
  (let* ((type (fuse--parse-line))
		 (nBytes-tmp (fuse--parse-line))
		 (nBytes (if (not (equal nil nBytes-tmp))
					 (string-to-number nBytes-tmp)
				   nil))
		 (payload (if (not (equal nil nBytes))
					  (fuse--parse-bytes nBytes)
					nil)))
	(if (not (or (equal nil type)
				 (equal nil nBytes)
				 (equal nil payload)))
		(progn
		  (fuse--consume-buffer-to-ptr)
		  (list type nBytes payload))
	  nil)))

(defun fuse--add-string-to-buffer (string)
  (setq fuse--buffer-string (concat fuse--buffer-string string)))



(defun fuse--client-parse-all-messages ()
  (let ((message (fuse--parse-message)))
	(while (not (equal message nil))
	  ;(fuse--decode-message message)
	  (setq message (fuse--parse-message)))))
