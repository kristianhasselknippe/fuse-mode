(require 'dash)
(require 's)

(defvar fuse--ux-bfr-ptr 0)
(defvar fuse--ux-sym-ptr 0)
(defvar fuse--ux-bfr "<App><Panel></Panel></App>")

(defun fuse--ux-nth (n)
  (if (and (< n (length fuse--ux-bfr))
		   (>= n 0))
	  (aref fuse--ux-bfr n)
	nil))

(defun fuse--ux-read-current-char ()
  (when (< fuse--ux-bfr-ptr 0)
	  (setq fuse--ux-bfr-ptr 0))
  (fuse--ux-nth fuse--ux-bfr-ptr))

(defun fuse--ux-read-next-char ()
  (when (< fuse--ux-bfr-ptr -1)
	(setq fuse--ux-bfr-ptr -1))
  (setq fuse--ux-bfr-ptr (1+ fuse--ux-bfr-ptr))
  (fuse--ux-nth fuse--ux-bfr-ptr))

(defun fuse--ux-peek-next-char ()
  (when (< fuse--ux-bfr-ptr -1)
	(setq fuse--ux-bfr-ptr -1))
  (fuse--ux-nth (1+ fuse--ux-bfr-ptr)))

(defun fuse--ux-read-current-symbol ()
  "Gets the current symbol and sets the symbol pointer equal to buffer pointer"
  (when (< fuse--ux-sym-ptr 0)
	(setq fuse--ux-sym-ptr 0))
  (if (or (>= fuse--ux-sym-ptr fuse--ux-bfr-ptr)
		  (> fuse--ux-bfr-ptr (length fuse--ux-bfr)))
	  nil
	(let (ret)
	  (setq ret (substring fuse--ux-bfr fuse--ux-sym-ptr (1+ fuse--ux-bfr-ptr)))
	  (setq fuse--ux-sym-ptr fuse--ux-bfr-ptr)
	  ret)))

(defun fuse--ux-parse-char (char)
  (if (equal char (fuse--ux-peek-next-char))
	  (fuse--ux-read-next-char)
	nil))

(defvar fuse--ux-valid-name-chars nil)
(defun fuse--ux-valid-name-chars ()
  (if (equal fuse--ux-valid-name-chars nil)
	  (setq fuse--ux-valid-name-chars (let (tmp)
			  (setq tmp (split-string "abcdefghijklmnopqrstuvwxyzABCEDFGHIJKLMNOPQRSTUVWXYZ0123456789_01234256789" ""))
			  (-slice tmp 1 (1- (length tmp)))))
	fuse--ux-valid-name-chars))


(defun fuse--ux-parse-name-until (char)
  (while (and
		  (-contains? (fuse--ux-valid-name-chars) (char-to-string (fuse--ux-read-current-char)))
		  (not (equal (fuse--ux-read-current-char) char))
		  (not (equal (fuse--ux-read-current-char) nil)))
	(fuse--ux-read-next-char))
  (if (equal char (fuse--ux-read-current-char))
	  (fuse--ux-read-current-symbol)
	nil))


(defun fuse--ux-parse-tag ()
  (if (fuse--ux-parse-char ?<)
	  (progn
		(fuse--ux-read-current-symbol)
		(let ((tag-name (fuse--ux-parse-name-until ?>)))
		  (princ tag-name)
		  (when (not (equal tag-name nil))
			(princ (s-concat "We got a tag name: " tag-name)))))
	nil))



(provide 'fuse-ux-parser)
