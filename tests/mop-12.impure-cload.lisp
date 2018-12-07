;;;; miscellaneous side-effectful tests of the MOP

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

;;; this file attempts to test possible metacircularity issues arising
;;; from adding slots to methods in odd places.

(defclass super-method ()
  ((abc :accessor abc :initarg :abc)))

;;; Test case reported by Jean Bresson sbcl-devel 2006-02-09
(defclass sub-generic-function1 (standard-generic-function) ()
  (:metaclass sb-mop:funcallable-standard-class))

(defclass sub-method1 (standard-method super-method) ())

(defgeneric myfun1 (a b)
  (:generic-function-class sub-generic-function1)
  (:method-class sub-method1))

(defvar *count1* 0)

(defmethod myfun1 (a b)
  (incf *count1*))

(with-test (:name (:mop-12 1))
  (myfun1 2 3)
  (assert (= *count1* 1))
  (myfun1 t nil)
  (assert (= *count1* 2)))

(defmethod myfun1 ((a integer) (b integer))
  (incf *count1* 2))

(with-test (:name (:mop-12 2))
  (myfun1 2 3)
  (assert (= *count1* 4))
  (myfun1 t nil)
  (assert (= *count1* 5)))

;;; Friendlier superclass order test case from Pascal Costanza
;;; sbcl-devel 2006-02-09
(defclass sub-generic-function2 (standard-generic-function) ()
  (:metaclass sb-mop:funcallable-standard-class))

(defclass sub-method2 (super-method standard-method) ())

(defgeneric myfun2 (a b)
  (:generic-function-class sub-generic-function2)
  (:method-class sub-method2))

(defvar *count2* 0)

(defmethod myfun2 (a b)
  (incf *count2*))

(with-test (:name (:mop-12 3))
  (myfun2 2 3)
  (assert (= *count2* 1))
  (myfun2 t nil)
  (assert (= *count2* 2)))

(defmethod myfun2 ((a integer) (b integer))
  (incf *count2* 2))

(with-test (:name (:mop-12 4))
  (myfun2 2 3)
  (assert (= *count2* 4))
  (myfun2 t nil)
  (assert (= *count2* 5)))
