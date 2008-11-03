(require :sb-introspect)

(defpackage :sb-introspect-test
  (:use "SB-INTROSPECT" "CL"))
(in-package :sb-introspect-test)

(with-compilation-unit (:source-plist (list :test-outer "OUT"))
  (load (compile-file (merge-pathnames "test.lisp" *load-pathname*))))

(assert (equal (function-arglist 'cl-user::one)
               '(cl-user::a cl-user::b cl-user::c)))
(assert (equal (function-arglist 'the)
               '(sb-c::value-type sb-c::form)))

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


;;;; Check correctness of FUNCTION-ARGLIST.

(assert (equal (function-arglist 'cl-user::one)
               '(cl-user::a cl-user::b cl-user::c)))
(assert (equal (function-arglist 'the)
               '(sb-c::value-type sb-c::form)))

;;; Check wrt. interplay of generic functions and their methods.

(defgeneric xuuq (gf.a gf.b          &rest gf.rest &key gf.k-X))
(defmethod  xuuq ((m1.a number) m1.b &rest m1.rest &key gf.k-X m1.k-Y m1.k-Z)
  (declare (ignore m1.a m1.b m1.rest gf.k-X m1.k-Y m1.k-Z))
  'm1)
(defmethod  xuuq ((m2.a string) m2.b &rest m2.rest &key gf.k-X m1.k-Y m2.k-Q)
  (declare (ignore m2.a m2.b m2.rest gf.k-X m1.k-Y m2.k-Q))
  'm2)

;; XUUQ's lambda list should look similiar to
;;
;;    (GF.A GF.B &REST GF.REST &KEY GF.K-X M1.K-Z M1.K-Y M2.K-Q)
;;
(multiple-value-bind (required optional restp rest keyp keys allowp
                      auxp aux morep more-context more-count)
    (sb-int:parse-lambda-list (function-arglist #'xuuq))
  (assert (equal required '(gf.a gf.b)))
  (assert (null optional))
  (assert (and restp (eql rest 'gf.rest)))
  (assert (and keyp
               (member 'gf.k-X keys)
               (member 'm1.k-Y keys)
               (member 'm1.k-Z keys)
               (member 'm2.k-Q keys)))
  (assert (not allowp))
  (assert (and (not auxp) (null aux)))
  (assert (and (not morep) (null more-context) (not more-count))))

;;; Check what happens when there's no explicit DEFGENERIC.

(defmethod kroolz (r1 r2 &optional opt &aux aux)
  (declare (ignore r1 r2 opt aux))
  'kroolz)
(assert (equal (function-arglist #'kroolz) '(r1 r2 &optional opt)))

;;;; Test finding a type that isn't one
(assert (not (find-definition-sources-by-name 'fboundp :type)))

;;;; Test the xref facility

(load (merge-pathnames "xref-test.lisp" *load-pathname*))

;;;; Unix success convention for exit codes
(sb-ext:quit :unix-status 0)
