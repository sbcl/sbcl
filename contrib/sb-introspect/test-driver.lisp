(defpackage :sb-introspect-test
  (:use "SB-INTROSPECT" "CL"))
(load (compile-file (merge-pathnames "test.lisp" *load-pathname*)))

(assert (equal (function-arglist 'cl-user::one)
	       '(cl-user::a cl-user::b cl-user::c)))
(assert (equal (function-arglist 'the)
	       '(type sb-c::value)))

(defun matchp (object form-number)
  (let ((ds (sb-introspect:find-definition-source object)))
    (and (pathnamep (sb-introspect:definition-source-pathname ds))
	 (= form-number (sb-introspect:definition-source-form-number ds)))))

(assert (matchp 'cl-user::one 2))
(assert (matchp #'cl-user::one 2))
; (assert (matchp 'two 2)) ; defgenerics don't work yet
(assert (matchp (car (sb-pcl:generic-function-methods #'cl-user::two)) 4))
