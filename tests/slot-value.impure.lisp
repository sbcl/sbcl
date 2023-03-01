;;;; Tests of SLOT-VALUE

(load "compiler-test-util.lisp")
(defpackage "SLOT-VALUE-TEST"
  (:use "CL" "SB-MOP" "ASSERTOID" "TEST-UTIL"))

(in-package "SLOT-VALUE-TEST")

(defclass a-class ()
  ((x :initform 123)))

(defun a-class-x-0 (a)
  (slot-value a 'x))

(defmethod a-class-x-1 ((a a-class))
  (slot-value a 'x))

(defmethod a-class-x-2 ((a a-class))
  (let ((%a a))
    (slot-value %a 'x)))

(defmethod a-class-x-3 ((a t))
  (slot-value a 'x))

(with-test (:name (slot-value defun))
  (assert (= (a-class-x-0 (make-instance 'a-class)) 123)))

(with-test (:name (slot-value defmethod))
  (assert (= (a-class-x-1 (make-instance 'a-class)) 123)))

(with-test (:name (slot-value defmethod let))
  (assert (= (a-class-x-2 (make-instance 'a-class)) 123)))

(with-test (:name (slot-value defmethod t))
  (assert (= (a-class-x-3 (make-instance 'a-class)) 123)))
