(require 's)
(require 'cl-lib)
(require 'dash)

(cl-defstruct (attrib
			   (:constructor new-attrib (name value)))
  name value)

(cl-defstruct (element
			   (:constructor new-element (name attribs content)))
  name attribs content)

(defmacro def-parser (name args &rest body)
  `(defun ,name ,args
	 (let ((last-pos ux-pos)
		   (ret (progn ,@body)))
	   (if ret
		   ret
		 (progn
		   (setq ux-pos last-pos)
		   ret)))))


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

(def-parser fuse-parse-char (c &optional offset)
  (if (eq c (fuse-aref ux-buffer (+ ux-pos (if offset offset 0))))
	  (fuse-consume)
	'nil))

(def-parser fuse-parse-while (predicate)
  (let ((beginning-pos ux-pos))
	(while (funcall predicate))
	(substring ux-buffer beginning-pos ux-pos)))

(def-parser fuse-parse-any-char (char-list)
  (if (member (fuse-peek) char-list)
	  (fuse-consume)
	'nil))

(def-parser fuse-parse-char-except (exception-list &optional offset)
  (if (not (member (fuse-peek) exception-list))
	  (fuse-consume)
	'nil))

(def-parser fuse-parse-string (s)
  (let ((offset 0))
	(while (and (< offset (length s))
			(fuse-parse-char (aref s offset) 0))
	  (setq offset (1+ offset)))
	(if (equal offset (length s))
		s
	  'nil)))

(def-parser fuse-parse-whitespace ()
  (fuse-parse-any-char '(?\s ?\t)))

(def-parser fuse-parse-many-whitespace ()
  (while (fuse-parse-whitespace))
  't)

(def-parser fuse-parse-identifier ()
  (let ((beginning-pos ux-pos))
	(while (fuse-parse-any-char ux-identifier-chars-list))
	(if (> ux-pos beginning-pos)
		(substring ux-buffer beginning-pos ux-pos)
	  'nil)))

(def-parser fuse-parse-string-literal ()
  (if (fuse-parse-string "\"")
	  (let ((ret (fuse-parse-while (lambda () (fuse-parse-char-except '(?\"))))))
		(if (fuse-parse-string "\"")
			ret
		  'nil))
	'nil))

(def-parser fuse-parse-attribute ()
  (let ((identifier (fuse-parse-identifier)))
	(if (and identifier
			 (fuse-parse-string "="))
		(let ((value (fuse-parse-string-literal)))
		  (if value
			  (new-attrib identifier value)
			'nil))
	  'nil)))

(def-parser fuse-parse-0-or-more (parser)
  (let ((ret '())
		(call-res))
	(while (setq call-res (funcall parser))
	  (setq ret (cons call-res ret)))
    (reverse ret)
	))

(def-parser fuse-parse-or (p1 p2)
  (unless (apply p1)
	(apply p2)))

(def-parser fuse-parse-start-tag ()
  (let (element-name
		attributes)
	(if (and (fuse-parse-string "<")
			 (setq element-name (fuse-parse-identifier))
			 (fuse-parse-many-whitespace)
			 (progn (setq attributes (fuse-parse-0-or-more
								 (lambda ()
								   (fuse-parse-many-whitespace)
								   (fuse-parse-attribute))))
					't)
			 (fuse-parse-string ">"))
		`(,element-name . ,attributes)
	  'nil)))

(def-parser fuse-parse-end-tag ()
  (let ((ret)))
  (if (and (fuse-parse-string "</")
		   (setq ret (fuse-parse-identifier))
		   (fuse-parse-string ">"))
	  ret
	'nil))


(def-parser fuse-parse-element ()
  (let ((start-tag (fuse-parse-start-tag)))
	(let ((element-name (car start-tag))
		  (attributes-list (cdr start-tag))
		  (end-tag (fuse-parse-end-tag))
		  (content))
	  (if (not end-tag)
		  (progn
			(setq content (fuse-parse-element))
			(setq end-tag (fuse-parse-end-tag))
			(new-element element-name (cdr start-tag) content))
		(progn
		  (if (equal end-tag element-name)
			  (new-element element-name (cdr start-tag) content)
			'nil))))))


(defun fuse-parse-root ()
  )

(defun fuse-parse-ux (ux)
  (setf ux-buffer ux)
  (fuse-parse-root))

(defun fuse-prepare-test-ux (ux)
  (setf ux-buffer ux)
  (setf ux-pos 0))

(defun fuse-print-ast ()
  (princ ast))

(defun fuse-reset-test (ux)
  (setq ux-pos 0)
  (setq ux-buffer ux))
