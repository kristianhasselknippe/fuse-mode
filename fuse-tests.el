(defun fuse--reset-for-testing (buffer-str sym-ptr buffer-ptr)
  (setq fuse--buffer-string buffer-str)
  (setq fuse--buffer-pointer buffer-ptr)
  (setq fuse--symbol-pointer sym-ptr))



(ert-deftest fuse--test-get-current-symbol ()
  (fuse--reset-for-testing "galskapogvin" 0 7)
  (should (equal (fuse--get-current-symbol) "galskap"))
  (should (equal fuse--symbol-pointer 7)))

(ert-deftest fuse--test-get-current-symbol-1 ()
  (fuse--reset-for-testing "galskapogvin" 7 9)
  (should (equal (fuse--get-current-symbol) "og"))
  (should (equal fuse--symbol-pointer 9)))

(ert-deftest fuse--test-get-current-symbol-2 ()
  (fuse--reset-for-testing "galskapogvin" 9 12)
  (should (equal (fuse--get-current-symbol) "vin"))
  (should (equal fuse--symbol-pointer 12)))

(ert-deftest fuse--test-parse-line ()
  (fuse--reset-for-testing "foobar\n" 0 -1)
  (should (equal (fuse--parse-line) "foobar")))

(ert-deftest fuse--test-parse-line-1 ()
  (fuse--reset-for-testing "foobar" 0 -1)
  (should (equal (fuse--parse-line) nil)))

(ert-deftest fuse--test-parse-line-2 ()
  (fuse--reset-for-testing "foobar\nbarbar\n" 0 -1)
  (should (equal (fuse--parse-line) "foobar"))
  (should (equal (fuse--parse-line) "barbar")))

(ert-deftest fuse--test-parse-line-3 ()
  (fuse--reset-for-testing "foobar\nbarbar" 0 -1)
  (should (equal (fuse--parse-line) "foobar"))
  (should (equal (fuse--parse-line) nil)))

(ert-deftest fuse--test-parse-line-4 ()
  (fuse--reset-for-testing "a\nba\ncar\nfart\n" 0 -1)
  (should (equal (fuse--parse-line) "a"))
  (should (equal (fuse--parse-line) "ba"))
  (should (equal (fuse--parse-line) "car"))
  (should (equal (fuse--parse-line) "fart")))

(ert-deftest fuse--test-parse-bytes ()
  (fuse--reset-for-testing "0123456789abcdefghijklmnopqrst" 0 0)
  (should (equal (fuse--parse-bytes 10) "0123456789"))
  (should (equal fuse--symbol-pointer 10)))

(ert-deftest fuse--test-parse-bytes-1 ()
  (fuse--reset-for-testing "0123456789abcdefghijklmnopqrst" 0 0)
  (should (equal (fuse--parse-bytes 4) "0123"))
  (should (equal fuse--symbol-pointer 4)))

(ert-deftest fuse--test-parse-bytes-2 ()
  (fuse--reset-for-testing "0123456789abcdefghijklmnopqrst" 0 0)
  (should (equal (fuse--parse-bytes 15) "0123456789abcde"))
  (should (equal fuse--symbol-pointer 15)))

(ert-deftest fuse--test-parse-bytes-3 ()
  (fuse--reset-for-testing "0123456789abcdefghijklmnopqrst" 0 0)
  (should (equal (fuse--parse-bytes 5) "01234"))
  (should (equal (fuse--parse-bytes 3) "567"))
  (should (equal fuse--symbol-pointer 8)))

(ert-deftest fuse--test-parse-bytes-4 ()
  (fuse--reset-for-testing "0123456789" 5 5)
  (should (equal (fuse--parse-bytes 5) "56789")))



(ert-deftest fuse--test-parse-to-character ()
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?g) ?g))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?a) ?a))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?c) ?c))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?f) ?f))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?o) ?o))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?p) ?p))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?r) nil))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?q) nil))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?t) nil))
  (fuse--reset-for-testing "abcdefghijklmnop" 0 -1)
  (should (equal (fuse--parse-to-character ?w) nil)))


(ert-deftest fuse--test-parse-message ()
  (fuse--reset-for-testing "Event\n301\n{\"Name\":\"Fuse.BuildStarted\",\"Data\":{\"BuildType\":\"LoadMarkup\",\"BuildId\":\"f77139bb-d329-4d76-aece-144781e7fc22\",\"BuildTag\":\"Host\",\"PreviewId\":\"af8865ad-7d83-458a-b261-03e03c4be3bf\",\"ProjectPath\":\"/Users/Hassel/FuseProjects/EmacsPlugin/EmacsTest/EmacsTest.unoproj\",\"Target\":\"Unknown\"},\"SubscriptionId\":0}" 0 0)
  (let ((res (fuse--parse-message)))
	(should (not (equal res nil)))
	(should (equal 301 (nth 1 res)))))

(ert-deftest fuse--test-parse-message-1 ()
  (fuse--reset-for-testing "Event\n10\nthisismypa" 0 -1)
  (let ((res (fuse--parse-message)))
	(should (not (equal res nil)))
	(should (equal "thisismypa" (nth 2 res)))))

(ert-deftest fuse--test-parse-message-2 ())
  ;(fuse--reset-for-testing "Event\n301\n{\"Name\":\"Fuse.BuildStarted\",\"Data\":{\"BuildType\":\"LoadMarkup\",\"BuildId\":\"f77139bb-d329-4d76-aece-144781e7fc22\",\"BuildTag\":\"Host\",\"PreviewId\":\"af8865ad-7d83-458a-b261-03e03c4be3bf\",\"ProjectPath\":\"/Users/Hassel/FuseProjects/EmacsPlugin/EmacsTest/EmacsTest.unoproj\",\"Target\":\"Unknown\"},\"SubscriptionId\":0}Response\n9559\n{\"Id\":0,\"Status\":\"Success\",\"Errors\":null,\"Result\":{\"IsUpdatingCache\":false,\"CodeSuggestions\":[{\"Suggestion\":\"xmlns\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"ux:Name\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"ux:Ref\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"ux:Path\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"ux:ClassName\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"ux:Class\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"ux:AutoCtor\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"ux:AutoBind\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"ux:Binding\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"ux:ClearColor\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"ux:Resource\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"ux:Generate\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"ux:Cascade\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Behaviors\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"DataContext\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Name\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"IsEnabled\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"SnapToPixels\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Resources\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Styles\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"IgnoreStyle\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Transforms\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"CachingMode\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Effects\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"HitTestMode\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"HitTestOpacityThreshold\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Width\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"MinWidth\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"MinHeight\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"MaxWidth\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"MaxHeight\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Alignment\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Visibility\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Padding\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Offset\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"X\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Y\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Anchor\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"ClipToBounds\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"BoxSizing\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Opacity\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"TransformOrigin\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Children\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Appearance\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Layout\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Property\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"DataContextChanged\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"IsEnabledChanged\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"IsVisibleChanged\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"RequestBringIntoView\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"TransformsChanged\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"BeginRemove\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Added\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Removed\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Update\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Rooted\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Unrooted\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"PropertyChanged\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Placed\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"Preplacement\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"IsInteractingChanged\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"ControlRenderPropertyChanged\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"ChildAdded\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]},{\"Suggestion\":\"ChildRemoved\",\"PreText\":\"\",\"PostText\":\"\",\"Type\":\"Event\",\"ReturnType\":\"\",\"AccessModifiers\":[],\"FieldModifiers\":[],\"MethodArguments\":[]}]}}"))
