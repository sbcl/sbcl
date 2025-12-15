(defparameter run-tests::*allowed-inputs* :any)
(require :sb-cover)

(defpackage "SB-COVER-TEST"
  (:export
   "*OUTPUT-DIRECTORY*" "*SOURCE-DIRECTORY*"
   "COMPILE-LOAD" "GET-STATES" "REPORT" "REPORT-EXPECT-FAILURE" "SOURCE-PATHNAME"
   "SOURCE-RECORDING-READTABLE")
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
        (sb-cover::refresh-coverage-bits)
        (nth-value 1 (sb-cover::compute-file-info (namestring (sb-cover-test:source-pathname x)) :default)))
    (warning (condition)
      (error "Unexpected warning: ~A" condition))))

(defun sb-cover-test:source-recording-readtable ()
  (sb-cover::make-source-recording-readtable (copy-readtable nil)
                                             (list (make-hash-table :test 'eq) 0)))

(defglobal *new-code* nil)
(defun preserve-code (underlying-fun object reason)
  (declare (ignore underlying-fun reason))
  (push object *new-code*)
  object)
(compile 'preserve-code)
;; Weak pointers to toplevel forms need to survive the entire test run
(sb-int:encapsulate 'sb-fasl::possibly-log-new-code 'override #'preserve-code)

(with-test (:name :sb-cover)
  (test-util:with-test-directory (sb-cover-test:*output-directory*)
    (load (merge-pathnames "tests.lisp" sb-cover-test:*source-directory*))
    (load (merge-pathnames "file-info-tests.lisp" sb-cover-test:*source-directory*))
    (load (merge-pathnames "save-restore-tests.lisp" sb-cover-test:*source-directory*)))
  (assert (> (length *new-code*) 40)))

;;; the return values tested here don't actually need to be as
;;; specific as COMPLEX, ARRAY (etc.): probably ATOM would do (though
;;; we might as well test for the slightly stronger consistency that
;;; we happen to have implemented).  In case of future conflict,
;;; probably weaken these tests rather than contort the
;;; implementation.
(with-test (:name (:sb-cover read :sharp-c))
  (let ((*readtable* (sb-cover-test:source-recording-readtable)))
    (assert (typep (read-from-string "#c(0 0)") 'complex))
    (assert (typep (read-from-string "#C(0d0 1/2)") 'complex))))

(with-test (:name (:sb-cover read :sharp-a))
  (let ((*readtable* (sb-cover-test:source-recording-readtable)))
    (assert (typep (read-from-string "#2A((1 2) (3 4))") 'array))
    (assert (typep (read-from-string "#a(4 base-char . \"abcd\")") 'array))))

(with-test (:name :no-redundant-form-paths)
  (dolist (c *new-code*)
    (let ((map (sb-cover::%find-coverage-map c)))
      (when map
        (sb-int:dovector (paths map)
          (let ((x (remove-duplicates paths :test 'equal)))
            (assert (= (length x) (length paths)))))))))
