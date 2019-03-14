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
;;; from changing discriminating functions.

(defclass gf1-class (standard-generic-function) ()
  (:metaclass sb-mop:funcallable-standard-class))
(defgeneric gf1 (x)
  (:method ((x t)) x)
  (:generic-function-class gf1-class))
(with-test (:name (:mop-11 1))
  (assert (= (gf1 3) 3)))

(defclass gf2-class (standard-generic-function) ()
  (:metaclass sb-mop:funcallable-standard-class))
(defgeneric gf2 (y)
  (:method ((x number)) x)
  (:generic-function-class gf2-class))
(with-test (:name (:mop-11 2))
  (assert (= (gf2 4) 4)))

(defgeneric gf1a (x)
  (:method ((x symbol)) (symbol-name x))
  (:generic-function-class gf1-class))
(with-test (:name (:mop-11 3))
  (assert (string= (gf1a t) "T")))

(defclass gf3-class (standard-generic-function) ()
  (:metaclass sb-mop:funcallable-standard-class))
(defgeneric gf3 (x y)
  (:method ((x number) (y number)) (+ x y))
  (:generic-function-class gf3-class))
(with-test (:name (:mop-11 4))
  (assert (= (gf3 1 2) 3)))
