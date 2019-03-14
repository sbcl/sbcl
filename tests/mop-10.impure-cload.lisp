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

;;; this file contains tests of REINITIALIZE-INSTANCE on generic
;;; functions.

(defclass my-generic-function (standard-generic-function)
  ()
  (:metaclass sb-mop:funcallable-standard-class))

(defgeneric foo (x)
  (:method-combination list)
  (:method list ((x float)) (* x x))
  (:method list ((x integer)) (1+ x))
  (:method list ((x number)) (expt x 2))
  (:generic-function-class my-generic-function))

(with-test (:name (:mop-10 1))
  (assert (equal (foo 3) '(4 9))))

(defmethod sb-mop:compute-discriminating-function ((gf my-generic-function))
  (let ((orig (call-next-method)))
    (lambda (&rest args)
      (let ((orig-result (apply orig args)))
        (cons gf (reverse orig-result))))))

(with-test (:name (:mop-10 2))
  (assert (equal (foo 3) '(4 9)))
  (reinitialize-instance #'foo)
  (assert (equal (foo 3) (cons #'foo '(9 4)))))
