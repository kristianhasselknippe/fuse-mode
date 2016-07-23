(require 's)
(require 'cl-lib)

(cl-defstruct (attrib
			   (:constructor new-attrib (name value)))
  name value)

(cl-defstruct (element
			   (:constructor new-element (name attribs content)))
  name attribs content)


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

(defun fuse-parse-while (predicate)
  (let ((beginning-pos ux-pos))
	(while (funcall predicate))
	(substring ux-buffer beginning-pos ux-pos)))

(defun fuse-parse-any-char (char-list)
  (if (member (fuse-peek) char-list)
	  (fuse-consume)
	'nil))

(defun fuse-parse-char-except (exception-list &optional offset)
  (if (not (member (fuse-peek) exception-list))
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

(defun fuse-parse-many-whitespace ()
  (while (fuse-parse-whitespace))
  't)

(defun fuse-parse-identifier (ast)
  (let ((beginning-pos ux-pos))
	(while (fuse-parse-any-char ux-identifier-chars-list))
	(if (> ux-pos beginning-pos)
		(substring ux-buffer beginning-pos ux-pos)
	  'nil)))

(defun fuse-parse-string-literal (ast)
  (if (fuse-parse-string "\"")
	  (let ((ret (fuse-parse-while (lambda () (fuse-parse-char-except '(?\"))))))
		(if (fuse-parse-string "\"")
			ret
		  'nil))
	'nil))

(defun fuse-parse-attribute (ast)
  (let ((identifier (fuse-parse-identifier ast)))
	(if (and identifier
			 (fuse-parse-string "="))
		(let ((value (fuse-parse-string-literal ast)))
		  (if value
			  `(,identifier . ,value)
			'nil))
	  'nil)))

(defun fuse-parse-0-or-more (ast parser)
  (let ((ret '())
		(call-res))
	(while (setq call-res (funcall parser ast))
	  (setq ret (cons call-res ret)))
	(print (reverse ret))
	(reverse ret)))

(defun fuse-parse-start-tag (ast)
  (let (element)
	(if (and (fuse-parse-string "<")
			 (setq element (fuse-parse-identifier ast))
			 (fuse-parse-many-whitespace)
			 (fuse-parse-0-or-more ast (lambda (a)
									 (fuse-parse-many-whitespace)
									 (fuse-parse-attribute a)))
			 (fuse-parse-string ">"))
		element
	  'nil)))

(defun fuse-parse-end-tag (ast start-tag-name)
  (if (and (fuse-parse-string "</")
		   (fuse-parse-string start-tag-name)
		   (fuse-parse-string ">"))
	  start-tag-name
	'nil))

(defun fuse-pare-element (ast)
  (let (tag-name
		attributes-list
		content)
	(if (and (fuse-parse-start-tag)
			 ()))))


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
