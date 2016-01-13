(load-file "~/fuse-mode/fuse-daemon-connection.el")
(load-file "~/fuse-mode/fuse-code-completion.el")
(load-file "~/fuse-mode/fuse-selection-changed.el")



(fuse--reset-for-testing "Foo\n5\nabcdeBar\n4\nab" 0 0)
(fuse--client-parse-all-messages)
