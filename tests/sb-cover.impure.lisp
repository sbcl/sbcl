(defparameter run-tests::*allowed-inputs* :any)
(require :sb-cover)
(defparameter *source-directory* (truename #P"../contrib/sb-cover/"))

(with-test (:name :sb-cover)
 (test-util:with-test-directory (coveragedir)
   (defvar cl-user::*coverage-report-directory* coveragedir)
   ;; Weak pointers to toplevel forms need to survive the entire test run
   (sb-sys:without-gcing
     (load (merge-pathnames "tests.lisp" *source-directory*)))))
