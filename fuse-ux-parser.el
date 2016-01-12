(require 'dash)

(defvar fuse--ux-bfr-ptr 0)
(defvar fuse--ux-sym-ptr 0)
(defvar fuse--ux-bfr "<App><Panel></Panel></App>")

(defun fuse--ux-nth (n)
  (if (< n (length fuse--ux-bfr))
	  (aref fuse--ux-bfr n)
	nil))

(defun fuse--ux-read-current-char ()
  (fuse--ux-nth fuse--ux-bfr fuse--ux-bfr))

(defun fuse--ux-read-next-char ()
  (setq fuse--ux-bfr (1+ fuse--ux-bfr))
  (fuse--ux-nth fuse--ux-bfr fuse--ux-bfr-ptr))

(defun fuse--ux-peek-next-char ()
  (fuse--ux-nth (1+ fuse--ux-bfr-ptr)))

(defun fuse--ux-parse-char (char)
  (if (equal char (fuse--ux-peek-next-char) fuse--ux-bfr)
	  (fuse--ux-read-next-char)
	nil))


(defvar fuse--ux-valid-names nil)
(defun fuse--ux-valid-names ()
  (if (equal fuse--ux-valid-names nil)
	  (setq fuse--ux-valid-names (let (tmp)
			  (setq tmp (split-string "abcdefghijklmnopqrstuvwxyzABCEDFGHIJKLMNOPQRSTUVWXYZ0123456789" ""))
			  (-slice tmp 1 (1- (length tmp)))))
	fuse--ux-valid-names))


(defun fuse--ux-parse-name-until (char)
  (while (and
		  (-contains? fuse--ux-valid-names (fuse--ux-read-current-char))
		  (not (equal (fuse--ux-read-current-char) char))
		  (not (equal (fuse--ux-read-current-char) nil)))
	(fuse--ux-read-next-char))
  (if (equal char fuse--ux-read-current-char)
	  ;; foo
