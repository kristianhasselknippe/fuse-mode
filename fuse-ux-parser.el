(require 's)
(defvar ux-buffer "")
(defvar ux-pos 0)

(defvar ux-identifier-chars)
(defvar ux-identifier-chars-list)
(setq ux-identifier-chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
(setq ux-identifier-chars-list (mapcar (lambda (x) x) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"))

(defun fuse-aref (buf pos)
  (if (<= (length buf) pos)
	  'nil
	(aref buf pos)))

(defun fuse-peek (&optional c)
  (fuse-aref ux-buffer (if c (+ ux-pos c) ux-pos)))

(defun fuse-consume (&optional c)
  (let ((ret (fuse-peek)))
	(setf ux-pos (+ ux-pos (if c c 1)))
	ret))

(defun fuse-parse-char (c &optional offset)
  (if (eq c (fuse-aref ux-buffer (+ ux-pos (if offset offset 0))))
	  (fuse-consume)
	'nil))

(defun fuse-parse-any-char (char-list)
  (if (member (fuse-peek) char-list)
	  (fuse-consume)
	'nil))

(defun fuse-parse-string (s)
  (let ((offset 0))
	(while (and (< offset (length s))
			(fuse-parse-char (aref s offset) 0))
	  (setq offset (1+ offset)))
	(if (equal offset (length s))
		s
	  'nil)))

(defun fuse-parse-whitespace ()
  (fuse-parse-any-char '(?\s ?\t)))

(defun fuse-parse-many-whitespace (ast)
  (while (fuse-parse-whitespace))
  't)

(defun fuse-parse-identifier (ast)
  (let ((beginning-pos ux-pos))
	(while (fuse-parse-any-char ux-identifier-chars-list))
	(if (> ux-pos beginning-pos)
		(substring ux-buffer beginning-pos ux-pos)
	  'nil)))

(defun fuse-parse-start-tag (ast)
  (if (fuse-parse-string "<")
	  (let ((element (fuse-parse-identifier ast)))
		(if (and (fuse-parse-many-whitespace ast)
				 (fuse-parse-string ">"))
			element
		  'nil))
	'nil))


(defun fuse-parse-root (ast)
  )

(defun fuse-parse-ux (ux)
  (setf ux-buffer ux)
  (fuse-parse-root))

(defun fuse-prepare-test-ux (ux)
  (setf ux-buffer ux)
  (setf ux-pos 0))

(defun fuse-print-ast (ast)
  (princ ast))

(defun fuse-reset-test (ux)
  (setq ux-pos 0)
  (setq ux-buffer ux))
