(require 's)
(require 'cl-lib)
(require 'dash)

(cl-defstruct (attrib
			   (:constructor new-attrib (name value)))
  name value)

(cl-defstruct (element
			   (:constructor new-element (name attribs content)))
  name attribs content)

(cl-defstruct (start-tag
			   (:constructor new-start-tag (name attribs type)))
  name attribs type)

(defun new-self-closing-tag (name attribs)
  (new-start-tag name attribs 'self-closing-tag))
(defun self-closing-tag-p (start-tag)
  (equal (aref start-tag 3) 'self-closing-tag))

(defun new-opening-tag (name attribs)
  (new-start-tag name attribs 'opening-tag))
(defun opening-tag-p (opening-tag)
  (equal (aref opening-tag 3) 'opening-tag))


(defmacro def-parser (name args &rest body)
  `(defun ,name ,args
	 (let ((last-pos ux-pos)
		   (to-be-returned (progn ,@body)))
	   (if to-be-returned
		   to-be-returned
		 (progn
		   (setq ux-pos last-pos)
		   to-be-returned)))))

(defvar ux-buffer "")
(defvar ux-pos 0)

(defvar ux-identifier-chars)
(defvar ux-identifier-chars-list)
(defvar ux-attribute-identifier-chars)
(setq ux-identifier-chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
(setq ux-identifier-chars-list (mapcar (lambda (x) x) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"))

(setq ux-attribute-identifier-chars (cons ?. (cons ?: ux-identifier-chars-list)))

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
  (fuse-parse-any-char '(?\s ?\t ?\n)))

(def-parser fuse-parse-many-whitespace ()
  (while (fuse-parse-whitespace))
  't)

(def-parser fuse-parse-identifier ()
  (let ((beginning-pos ux-pos))
	(while (fuse-parse-any-char ux-identifier-chars-list))
	(if (> ux-pos beginning-pos)
		(substring ux-buffer beginning-pos ux-pos)
	  'nil)))

(def-parser fuse-parse-attribute-identifier ()
  (let ((beginning-pos ux-pos))
	(while (fuse-parse-any-char ux-attribute-identifier-chars))
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
  (let ((identifier (fuse-parse-attribute-identifier)))
	(if (and identifier
			 (fuse-parse-string "="))
		(let ((value (fuse-parse-string-literal)))
		  (if value
			  (new-attrib identifier value)
			'nil))
	  'nil)))

(def-parser fuse-parse-0-or-more (parser)
  (let ((r)
		(call-res 'nil))
	(while (setq call-res (funcall parser))
	  (setq r (cons call-res r)))
	  (if r
		  (reverse r)
		'empty)))

(def-parser fuse-parse-or (p1 p2)
  (unless (apply p1)
	(apply p2)))

(def-parser fuse-parse-start-tag ()
  (let (element-name
		attributes)
	(let (r)
	  (if (and (fuse-parse-string "<")
			   (setq element-name (fuse-parse-identifier))
			   (fuse-parse-many-whitespace)
			   (progn
				 (setq attributes (fuse-parse-0-or-more
								   (lambda ()
									 (fuse-parse-many-whitespace)
									 (fuse-parse-attribute))))
				 't)
			   (setq r (cond
						((fuse-parse-string "/>") (new-self-closing-tag element-name attributes))
						((fuse-parse-string ">") (new-opening-tag element-name attributes))
						('t 'nil))))
		  r
		'nil))))

(def-parser fuse-parse-end-tag ()
  (let ((ret)))
  (if (and (fuse-parse-string "</")
		   (setq ret (fuse-parse-identifier))
		   (fuse-parse-string ">"))
	  ret
	'nil))


(def-parser fuse-parse-element ()
  (fuse-parse-many-whitespace)
  (let ((start-tag (fuse-parse-start-tag)))
	(cond
	 ((equal start-tag 'nil) 'nil)

	 ((self-closing-tag-p start-tag)
	  (princ "self closing tag") (princ (start-tag-name start-tag))
	  (new-element (start-tag-name start-tag) (start-tag-attribs start-tag) 'nil))

	 ((opening-tag-p start-tag)
	  (fuse-parse-many-whitespace)
	  (let ((content (fuse-parse-0-or-more 'fuse-parse-element)))
		(fuse-parse-many-whitespace)
		(if (equal (start-tag-name start-tag) (fuse-parse-end-tag))
			(progn
			  (princ "normal tag") (princ (start-tag-name start-tag))
			  (new-element (start-tag-name start-tag)
						   (start-tag-attribs start-tag)
						   content))
		  'nil))))))

(defun testittest ()
  (setq ux-pos 0)
  (fuse-parse-element))


(defun fuse--get-buffer-contents ()
  (buffer-substring-no-properties (point-min) (point-max)))


(defun fuse-attrib-to-string (a &optional depth)
  (if a
	  (let ((ret (make-string depth ?\t)))
		(-each a (lambda (attr)
				   (princ attr)
				   (setq ret (concat ret "AName: " (attrib-name attr) " AVal: " (attrib-value attr) ", "))))
		ret)
	""))

(defun fuse-element-to-string (e &optional depth)
  (let ((str ""))
	(unless depth (setq depth 0))
	(unless str (setq str ""))
	(setq str (concat str (make-string depth ?\t) "EName: " (element-name e) " \n"))
	(when (not (equal  (element-attribs e) 'empty))
	  (setq str (concat str (fuse-attrib-to-string (element-attribs e) depth) "\n")))
	(let ((elem-cont (element-content e)))
	  (when (and elem-cont
				 (not (equal elem-cont 'empty)))
		(-each elem-cont
		  (lambda (elem)
			(setq str (concat str (fuse-element-to-string elem (1+ depth))))))))
	str))


(defun fuse-parse-current-ux-buffer ()
  (interactive)
  (setq ux-pos 0)
  (setq ux-buffer (fuse--get-buffer-contents))
  (let ((ret (fuse-parse-element)))
	(message (fuse-element-to-string ret))))
