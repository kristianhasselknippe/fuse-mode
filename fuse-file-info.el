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


(defun get-current-text ()
  (interactive)
   (let ((buffer (current-buffer)))
	 (with-current-buffer buffer
	   (save-excursion
		 (save-restriction
		   (widen)
		   (buffer-substring-no-properties (point-min) (point-max)))))))

(defun get-current-type ()
  (interactive)
  (car (last (split-string (get-current-path) "\\."))))

(defun get-current-line ()
  (interactive)
  (line-number-at-pos))

(defun get-current-character ()
  (interactive)
  (let ((current-point (point)))
	(let ((text (get-current-text)))
	  (let ((rec (lambda (offset text)
					 (if (not (or (<= (- current-point offset) 0)
								  (char-equal (aref text (- current-point offset)) ?\n)))
						 (funcall rec (+ offset 1) text)
					   offset))))
		(let ((ret (funcall rec 0 text)))
		  (print ret)
		  ret)))))

(defun get-code-completion-info ()
  (interactive)
  (let ((ret
		 (let ((id 1)
			   (path (get-current-path))
			   (text (get-current-text))
			   (type (get-current-type))
			   (c-line (get-current-line))
			   (c-character (get-current-character)))
		   (json-encode `((Command . RequestCodeCompletion)
						  (Arguments .
									 ((QueryId . ,id)
									  (Path . ,path)
									  (Text . ,text)
									  (Type . ,type)
									  (CaretPosition . ((Line . ,c-line) (Character . ,c-character))))))))))
	
	ret))
