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

(assert (equal (function-arglist #'(sb-pcl::slow-method cl-user::j (t)))
               '(sb-pcl::method-args sb-pcl::next-methods)))

(let ((source (find-definition-source #'cl-user::one)))
  (assert (= (definition-source-file-write-date source)
             (file-write-date (merge-pathnames "test.lisp" *load-pathname*))))
  (assert (equal (getf (definition-source-plist source) :test-outer)
                 "OUT")))

(let ((plist (definition-source-plist
                 (find-definition-source #'cl-user::four))))
  (assert (equal (getf plist :test-outer) "OUT"))
  (assert (equal (getf plist :test-inner) "IN")))

(defun matchp (object form-number)
  (let ((ds (sb-introspect:find-definition-source object)))
    (and (pathnamep (sb-introspect:definition-source-pathname ds))
         (= form-number
            (first (sb-introspect:definition-source-form-path ds))))))

(defun matchp-name (type object form-number)
  (let ((ds (car (sb-introspect:find-definition-sources-by-name object type))))
    (and (pathnamep (sb-introspect:definition-source-pathname ds))
         (= form-number
            (first (sb-introspect:definition-source-form-path ds))))))

(defun matchp-length (type object form-numbers)
  (let ((ds (sb-introspect:find-definition-sources-by-name object type)))
    (= (length ds) form-numbers)))

(assert (matchp-name :function 'cl-user::one 2))
(assert (matchp #'cl-user::one 2))
(assert (matchp-name :generic-function 'cl-user::two 3))
(assert (matchp (car (sb-pcl:generic-function-methods #'cl-user::two)) 4))

(assert (matchp-name :variable 'cl-user::*a* 8))
(assert (matchp-name :variable 'cl-user::*b* 9))
(assert (matchp-name :class 'cl-user::a 10))
(assert (matchp-name :condition 'cl-user::b 11))
(assert (matchp-name :structure 'cl-user::c 12))
(assert (matchp-name :function 'cl-user::make-c 12))
(assert (matchp-name :function 'cl-user::c-e 12))
(assert (matchp-name :structure 'cl-user::d 13))
(assert (matchp-name :function 'cl-user::make-d 13))
(assert (matchp-name :function 'cl-user::d-e 13))
(assert (matchp-name :package 'cl-user::e 14))
(assert (matchp-name :symbol-macro 'cl-user::f 15))
(assert (matchp-name :type 'cl-user::g 16))
(assert (matchp-name :constant 'cl-user::+h+ 17))
(assert (matchp-length :method 'cl-user::j 2))
(assert (matchp-name :macro 'cl-user::l 20))
(assert (matchp-name :compiler-macro 'cl-user::m 21))
(assert (matchp-name :setf-expander 'cl-user::n 22))
(assert (matchp-name :function  '(setf cl-user::o) 23))
(assert (matchp-name :method  '(setf cl-user::p) 24))
(assert (matchp-name :macro  'cl-user::q 25))
(assert (matchp-name :method-combination 'cl-user::r 26))
(assert (matchp-name :setf-expander 'cl-user::s 27))

(sb-profile:profile cl-user::one)
(assert (matchp-name :function 'cl-user::one 2))
(sb-profile:unprofile cl-user::one)

;;; Test the xref facility

(load (merge-pathnames "xref-test.lisp" *load-pathname*))

;;; Unix success convention for exit codes
(sb-ext:quit :unix-status 0)
