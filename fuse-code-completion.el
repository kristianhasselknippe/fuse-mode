(require 'auto-complete)


(defun fuse--create-get-completion-message ()
  (create-message "Request"
				  (json-encode `(:Id 0
								 :Name Fuse.GetCodeSuggestions
								 :Arguments (:SyntaxType "UX"
								 			 :Path ,(fuse--get-current-file-name)
											 :Text ,(fuse--get-current-file-contents)
											 :CaretPosition (:Line ,(fuse--get-current-caret-line)
											 				 :Character ,(fuse--get-current-caret-character)))))))

(defun fuse--get-code-completion ()
  (interactive)
  (let (m)
	(setq  m (fuse--create-get-completion-message))
	(message m)
	(fuse-client-send-command m)))

;{
;    "Id": 42, // Unique request id
;    "Name": "Fuse.GetCodeSuggestions",
;    "Arguments":
;    {
;        "SyntaxType": "UX", // Typically "UX" or "Uno"
;        "Path": "C:\\FuseProjects\\MainView.ux", // Path to document where suggestion is requested
;        "Text": "<App>\n\t<Button />\n</App>", // Full source of document where suggestion is requested
;        "CaretPosition": { "Line": 2, "Character": 9 } // 1-indexed text position within Text where suggestion is requested
;    }
;}


(ac-define-source fuse-source
  '((candidates . (list "Foo" "Bar" "Baz"))))

(append ac-sources '(ac-source-fuse-source))

(provide 'fuse-code-completion)
