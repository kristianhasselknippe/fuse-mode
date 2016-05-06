(require 'cl)
(require 'json)

;Event
;{
;    "Name": "ExampleEvent",
;    "SubscriptionId": 32, // The id of the event subscription (set automatically by the daemon),
;    "Data": { ... }, // An event-specific JSON-object with the event data
;}

;Request
;{
;    "Name": "MyRequest",
;    "Id": 242, // Make this a unique number for each request, so you can recognize the matching response message
;    "Arguments": { ... }, // A request-specific JSON-object with the request
;}

;Response
;{
;    "Id": 242, // The id of the request to which this is a response
;    "Status": "Success", // Can be "Success", "Error" or "Unhandled"
;    "Result": { ... }, // If status is "Succsess", a request-specific JSON-object with the response
;    "Errors": [ ... ], // If status is "Error", an array of objects containing more error information
;}


(cl-defstruct event name subscription-id data)
(cl-defstruct request name id arguments)
(cl-defstruct response id status result errors)



(make-variable-buffer-local
 (defvar fuse--buffer ""))

;;###autoload
(define-minor-mode fuse-mode
  "The Fuse minor mode."
  :lighter "fuse"
  :keymap (make-sparse-keymap))

(provide 'fuse-mode)
