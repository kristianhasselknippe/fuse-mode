(defvar ux-buffer "")
(defvar ux-pos 0)

(defun fuse-consume (c)
  (setf ux-pos (+ ux-pos c)))

(defun fuse-consume ()
  (fuse-consume 1))

(defun fuse-parse-char (c)
  (if (eq c (aref ux-buffer ux-pos))
	  (fuse-consume)
	'nil))

(defun fuse-parse-string (s)
  (while (fuse-parse-char ()

(defun fuse-parse-element (ast)
  (fuse-parse-string "<"))

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
