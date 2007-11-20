;;; bug 417: toplevel nil confusing source-path logic
nil
(defmethod frob ((package package) stream)
  (if (string= (package-name package) "FOO")
      (pprint-logical-block (stream nil))
      (print-unreadable-object (package stream))))
