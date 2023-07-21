;;;; tests of SLOT-UNBOUND

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;
;;;; This software is in the public domain and is provided with
;;;; absoluely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(load "compiler-test-util.lisp")
(defpackage "SLOT-UNBOUND-TEST"
  (:use "CL" "SB-MOP" "ASSERTOID" "TEST-UTIL"))

(in-package "SLOT-UNBOUND-TEST")

(defstruct (struct-a (:constructor make-struct-a (&aux unboundable)))
  (boxed 0 :type integer)
  (raw 0.0d0 :type double-float)
  (unboundable))

(defvar *slot-unbounds*)

(defmethod slot-unbound (class (s struct-a) slot-name)
  (push (cons 'struct-a slot-name) *slot-unbounds*)
  42)

(define-condition condition-a ()
  ((condition-bound :initform 0)
   (condition-unbound)))

(defmethod slot-unbound (class (c condition-a) slot-name)
  (push (cons 'condition-a slot-name) *slot-unbounds*)
  43)

(defclass class-a ()
  ((class-bound :initform 0)
   (class-unbound)))

(defmethod slot-unbound (class (c class-a) slot-name)
  (push (cons 'class-a slot-name) *slot-unbounds*)
  44)

(with-test (:name (slot-unbound :struct-a))
  (setf *slot-unbounds* nil)
  (let ((struct-a (make-struct-a)))
    (declare (optimize safety))
    (assert (eql (slot-value struct-a 'boxed) 0))
    (assert (eql (slot-value struct-a 'raw) 0.0d0))
    (assert (eql (slot-value struct-a 'unboundable) 42))
    (assert (equal *slot-unbounds* '((struct-a . unboundable))))))

(with-test (:name (slot-unbound :condition-a))
  (setf *slot-unbounds* nil)
  (let ((condition-a (make-condition 'condition-a)))
    (assert (eql (slot-value condition-a 'condition-bound) 0))
    (assert (eql (slot-value condition-a 'condition-unbound) 43))
    (assert (equal *slot-unbounds* '((condition-a . condition-unbound))))))

(with-test (:name (slot-unbound :class-a))
  (setf *slot-unbounds* nil)
  (let ((class-a (make-instance 'class-a)))
    (assert (eql (slot-value class-a 'class-bound) 0))
    (assert (eql (slot-value class-a 'class-unbound) 44))
    (assert (equal *slot-unbounds* '((class-a . class-unbound))))))
