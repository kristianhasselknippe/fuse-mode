(defvar ux-buffer "")
(defvar ux-pos 0)

(defun fuse/consume (c)
  (setf ux-pos (+ ux-pos c)))

(defun fuse/consume ()
  (fuse/consume 1))

(defun fuse/parse-char (c)
  (if (eq c (aref ux-buffer ux-pos))
	  (fuse/consume)
	'nil))

(defun fuse/parse-string (s)
  (contains ux-buffer))

(defun fuse/parse-element (ast)
  )

(defun fuse/parse-root (ast)
  )

(defun fuse/parse-ux (ux)
  (setf ux-buffer ux)
  (fuse/parse-root))

(defun fuse/test-ux (ux procedure &rest args)
  (setf ux-buffer ux)
  (setf ux-pos 0)
  (apply procedure args))


(defun fuse/test-ux-1 ()
  (fuse/test-ux "<App><Panel></Panel></App>" 'fuse/parse-root))
