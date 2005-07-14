(require :sb-introspect)

(defpackage :sb-introspect-test
  (:use "SB-INTROSPECT" "CL"))
(in-package :sb-introspect-test)

(with-compilation-unit (:source-plist (list :test-outer "OUT"))
  (load (compile-file (merge-pathnames "test.lisp" *load-pathname*))))

(assert (equal (function-arglist 'cl-user::one)
               '(cl-user::a cl-user::b cl-user::c)))
(assert (equal (function-arglist 'the)
               '(type sb-c::value)))

(let ((source (find-definition-source 'cl-user::one)))
  (assert (= (definition-source-file-write-date source)
             (file-write-date (merge-pathnames "test.lisp" *load-pathname*))))
  (assert (equal (getf (definition-source-plist source) :test-outer)
                 "OUT")))

(let ((plist (definition-source-plist (find-definition-source 'cl-user::four))))
  (assert (equal (getf plist :test-outer) "OUT"))
  (assert (equal (getf plist :test-inner) "IN")))

(defun matchp (object form-number)
  (let ((ds (sb-introspect:find-definition-source object)))
    (and (pathnamep (sb-introspect:definition-source-pathname ds))
         (= form-number
            (first (sb-introspect:definition-source-form-path ds))))))

(assert (matchp 'cl-user::one 2))
(assert (matchp #'cl-user::one 2))
; (assert (matchp 'two 2)) ; defgenerics don't work yet
(assert (matchp (car (sb-pcl:generic-function-methods #'cl-user::two)) 4))

;;; Unix success convention for exit codes
(sb-ext:quit :unix-status 0)
