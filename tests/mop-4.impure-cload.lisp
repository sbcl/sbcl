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

;;; This file contains tests for COMPUTE-DISCRIMINATING-FUNCTION on
;;; subclasses of generic functions.

;;; bug 343
(defclass my-generic-function1 (standard-generic-function) ()
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod sb-mop:compute-discriminating-function ((gf my-generic-function1))
  (let ((dfun (call-next-method)))
    (lambda (&rest args)
      (1+ (apply dfun args)))))

(defgeneric foo (x)
  (:generic-function-class my-generic-function1))

(defmethod foo (x) (+ x x))

(with-test (:name (:mop-4 1))
  (assert (= (foo 5) 11)))

;;; from PCL sources

(defclass my-generic-function-pcl1 (standard-generic-function) ()
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod sb-mop:compute-discriminating-function ((gf my-generic-function-pcl1))
  (let ((std (call-next-method)))
    (lambda (arg)
      (print (list 'call-to-gf gf arg))
      (funcall std arg))))

(defgeneric pcl1 (x)
  (:generic-function-class my-generic-function-pcl1))

(defmethod pcl1 ((x integer)) (1+ x))

(with-test (:name (:mop-4 2))
 (let ((output (with-output-to-string (*standard-output*)
                 (pcl1 3))))
   (assert (search "(CALL-TO-GF #<MY-GENERIC-FUNCTION-PCL1 COMMON-LISP-USER::PCL1 (1)> 3)" output))))

#|
(defclass my-generic-function-pcl2 (standard-generic-function) ()
  (:metaclass sb-mop:funcallable-standard-class))
(defmethod sb-mop:compute-discriminating-function ((gf my-generic-function-pcl2))
  (lambda (arg)
   (cond (<some condition>
          <store some info in the generic function>
          (sb-mop:set-funcallable-instance-function
            gf
            (sb-mop:compute-discriminating-function gf))
          (funcall gf arg))
         (t
          <call-a-method-of-gf>))))
|#

;;; from clisp's test suite

(progn
  (defclass traced-generic-function (standard-generic-function)
    ()
    (:metaclass sb-mop:funcallable-standard-class))
  (defvar *last-traced-arguments* nil)
  (defvar *last-traced-values* nil)
  (defmethod sb-mop:compute-discriminating-function ((gf traced-generic-function))
    (let ((orig-df (call-next-method)))
      (lambda (&rest arguments)
        (setq *last-traced-arguments* arguments)
        (let ((values (multiple-value-list (apply orig-df arguments))))
          (setq *last-traced-values* values)
          (values-list values)))))
  (defgeneric testgf15 (x)
    (:generic-function-class traced-generic-function)
    (:method ((x number))
      (values x (- x) (* x x) (/ x)))))

(with-test (:name (:mop-4 3))
  (testgf15 5)
  (assert (equal (list *last-traced-arguments* *last-traced-values*)
                 '((5) (5 -5 25 1/5)))))

;;; also we might be in a position to run the "application example"
;;; from mop.tst in clisp's test suite
