(defun fuse--get-current-file-name ()
  (buffer-file-name))

(defun fuse--get-current-file-contents ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun fuse--get-current-caret-line ()
  (line-number-at-pos))

(defun fuse--get-current-caret-character ()
  (current-column))

(provide 'fuse-common)
