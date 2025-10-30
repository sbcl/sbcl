(defparameter run-tests::*allowed-inputs* :any)
(require :sb-cover)

(defpackage "SB-COVER-TEST"
  (:export
   "*OUTPUT-DIRECTORY*" "*SOURCE-DIRECTORY*"
   "COMPILE-LOAD" "GET-STATES" "REPORT" "REPORT-EXPECT-FAILURE" "SOURCE-PATHNAME")
  (:use "CL"))

(defvar sb-cover-test:*output-directory*)
(defparameter sb-cover-test:*source-directory* (truename #p"../contrib/sb-cover/"))

(defun sb-cover-test:source-pathname (x)
  (merge-pathnames (merge-pathnames x (make-pathname :type "lisp")) sb-cover-test:*source-directory*))

(defun sb-cover-test:compile-load (x)
  (let ((p (sb-cover-test:source-pathname x)))
    (load (compile-file p :output-file sb-cover-test:*output-directory*))))

(defun sb-cover-test:report ()
  (handler-case
      (sb-cover:report sb-cover-test:*output-directory*)
    (warning (condition)
      (error "Unexpected warning: ~A" condition))))

(defun sb-cover-test:report-expect-failure ()
  (handler-case
      (progn
        (sb-cover:report sb-cover-test:*output-directory*)
        (error "Should've signaled a warning"))
    (warning ())))

(defun sb-cover-test:get-states (x)
  (handler-case
      (progn
        (sb-cover::refresh-coverage-info)
        (nth-value 1 (sb-cover::compute-file-info (namestring (sb-cover-test:source-pathname x)) :default)))
    (warning (condition)
      (error "Unexpected warning: ~A" condition))))

(with-test (:name :sb-cover)
  (test-util:with-test-directory (sb-cover-test:*output-directory*)
    ;; Weak pointers to toplevel forms need to survive the entire test run
    (sb-sys:without-gcing
      (load (merge-pathnames "tests.lisp" sb-cover-test:*source-directory*)))
    (sb-sys:without-gcing
      (load (merge-pathnames "file-info-tests.lisp" sb-cover-test:*source-directory*)))
    (sb-sys:without-gcing
      (load (merge-pathnames "save-restore-tests.lisp" sb-cover-test:*source-directory*)))))
