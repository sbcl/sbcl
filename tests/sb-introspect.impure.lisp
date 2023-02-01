(require :sb-introspect)
(unless (sb-c:source-location) (push :no-source-locs *features*))
(test-util:with-scratch-file (f "fasl")
  (load (compile-file "../contrib/sb-introspect/xref-test-data.lisp" :output-file f))
  (load "../contrib/sb-introspect/xref-test.lisp")
 ;; This test is quite bogus. because it depended on WITH-COMPILATION-UNIT
 ;; being wrapped around both the COMPILE-FILE and LOAD which is very weird.
 ;; If we take the W-C-U out from the LOAD, then the assertions on CL-USER::FOUR
 ;; fails because we don't append property lists. And if we don't COMPILE-FILE,
 ;; then we lose information about COMPILE-TIME-TOO-FUN. As written, it's hard to
 ;; assert about behaviors that actually occur in each step.
 ;; If someone thinks this is an important distinction, that someone can
 ;; write better tests.
 (with-compilation-unit (:source-plist '(:test-outer "OUT"))
   (load (compile-file "../contrib/sb-introspect/test.lisp" :output-file f)))
 (load "../contrib/sb-introspect/test-driver.lisp"))
