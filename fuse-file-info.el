request-code-completion (id path text type c-line c-character)



{
    "Command": "RequestCodeCompletion",
    "Arguments": 
    {
        "QueryId": 0,
        "Path": "C:\\Test.uno",
        "Text": "using Uno;",
        "Type": "Uno",
        "CaretPosition": { "Line": 1, "Character": 4 }
    }
}


(defun get-current-path ()
  (interactive)
  (buffer-file-name (current-buffer)))


(defun get-current-text (file)
  )

(defun get-current-type ()
  (interactive)
  (let ((ret (last (split-string (get-current-path) "\\."))))
	(print ret)
	ret))

(defun get-current-line ()
  (interactive)
  (let ((ret (line-number-at-pos)))
	(princ ret)
	ret))

(defun get-current-character ()
  (interactive)
  (current-column))

