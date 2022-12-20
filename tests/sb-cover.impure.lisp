(defparameter run-tests::*allowed-inputs* :any)
(require :sb-cover)
(defparameter *source-directory* (truename #P"../contrib/sb-cover/"))

;;; sb-cover uses weak pointers which disappear randomly and make test
;;; results unreliable.
(with-test (:name :sb-cover :broken-on :sbcl)
 (test-util:with-test-directory (coveragedir)
   (defvar cl-user::*coverage-report-directory* coveragedir)
   (load (merge-pathnames "tests.lisp" *source-directory*))))
