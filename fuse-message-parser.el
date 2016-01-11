(defvar fuse--buffer-string "")
(defvar fuse--buffer-pointer 0)
(defvar fuse--symbol-pointer 0)

(defun fuse--get-next-character ()
  (if (equal fuse--buffer-pointer (1- (length fuse--buffer-string)))
	  nil
	(progn
	  (setq fuse--buffer-pointer (1+ fuse--buffer-pointer))
	  (aref fuse--buffer-string fuse--buffer-pointer))))

(defun fuse--get-current-character ()
  (if (equal fuse--buffer-pointer (length fuse--buffer-string))
	  nil
	(aref fuse--buffer-string fuse--buffer-pointer)))

(defun fuse--parse-line ()
  (while (not (equal ?\n (fuse--get-next-character))

(defun fuse--parse-payload ()
  )

(defun fuse-client-parse ()
  )



;(list event-type message)
