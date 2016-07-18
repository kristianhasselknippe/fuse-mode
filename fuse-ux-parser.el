(require 's)
(defvar ux-buffer "")
(defvar ux-pos 0)

(defvar ux-identifier-chars "abcdefghijklmnopqrstuvwxyz0123456789_")

(defun fuse-aref (buf pos)
  (if (<= (length buf) pos)
	  'nil
	(aref buf pos)))

(defun fuse-consume (&optional c)
  (setf ux-pos (+ ux-pos (if c c 1))))

(defun fuse-peek (&optional c)
  (fuse-aref ux-buffer (if c (+ ux-pos c) ux-pos)))

(defun fuse-parse-char (c &optional offset)
  (if (eq c (fuse-aref ux-buffer (+ ux-pos (if offset offset 0))))
	  (fuse-consume)
	'nil))

(defun fuse-parse-any-char (char-list)
  (if (member (fuse-peek) char-lsit)
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
  (if (or (fuse-parse-char ?\s)
		  (fuse-parse-char ?\t))
	  't
	'nil))

(defun fuse-parse-identifier (ast)
  )

(defun fuse-parse-element (ast)
  (fuse-parse-string "<")
  (fuse-parse-string )

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

(defun fuse-test-string ()
  (fuse-prepare-test-ux "<App>")
  (fuse-parse-string "<App>"))

(defun fuse-test-element ()
  (fuse-prepare-test-ux "<App>")
  (let ((ret (fuse-parse-element '())))
	(fuse-print-ast ret)))

(defun fuse-test-ux-1 ()
  (fuse-prepare-test-ux "<App><Panel></Panel></App>")
  (fuse-parse-root '()))
