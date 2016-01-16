(defun fuse--reset-for-testing (buffer-str sym-ptr buffer-ptr)
  (setq fuse--buffer-string buffer-str)
  (setq fuse--symbol-pointer sym-ptr)
  (setq fuse--buffer-pointer buffer-ptr))
