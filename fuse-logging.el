(defvar fuse-debug-log (generate-new-buffer "*fuse-debug-log*"))
(defvar fuse-build-log (generate-new-buffer "*fuse-build-log*"))



(defun write-human-readable-to-buffer (str buffer)
  (save-excursion
	(set-buffer buffer)
	(princ str (lambda (c) (insert c)))))

(defun insert-button-to-buffer (label act buffer)
  (save-excursion
	(set-buffer buffer)
	(insert-button label 'action act)))

(defun fuse-write-to-build-log (str)
  (write-human-readable-to-buffer str fuse-build-log))


(defun fuse-write-to-debug-log (str)
  (write-human-readable-to-buffer str fuse-debug-log))


(defun contains-fuse-file-path (str)
  (string-match "\([^\"]+\)" str))
