;;;; tests of SLOT-BOUNDP and SLOT-MAKUNBOUND

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
(defpackage "SLOT-BOUNDP-TEST"
  (:use "CL" "SB-MOP" "ASSERTOID" "TEST-UTIL"))

(in-package "SLOT-BOUNDP-TEST")

(defstruct struct-a
  (boxed 0 :type integer)
  (raw 0.0d0 :type double-float))

(defstruct (struct-b
            (:include struct-a)
            (:constructor make-struct-b
                (&aux unboundable unboundable-boxed unboundable-raw)))
  (unboundable)
  (unboundable-boxed 0 :type integer)
  (unboundable-raw 0.0d0 :type double-float))

(define-condition condition-a ()
  ((condition-unbound)
   (condition-bound :initform 0)
   (condition-allocation-class :allocation :class)))

(defclass class-a ()
  ((class-unbound)
   (class-bound :initform 0)
   (class-allocation-class :allocation :class)))

(defun find-slotd (class name)
  (find name (class-slots class) :key 'slot-definition-name))

(with-test (:name (:always-boundp-struct-a :notinline))
  (let ((fun (lambda (x slot-name)
               (declare (notinline slot-boundp))
               (slot-boundp x slot-name)))
        (struct-a (make-struct-a)))
    (assert (funcall fun struct-a 'boxed))
    (assert (funcall fun struct-a 'raw))))

(with-test (:name (:always-boundp-struct-a :inline))
  (let ((struct-a (make-struct-a)))
    (assert (slot-boundp struct-a 'boxed))
    (assert (slot-boundp struct-a 'raw))))

(with-test (:name (:always-boundp-struct-a :mop))
  (let ((struct-a (make-struct-a))
        (class (find-class 'struct-a)))
    (assert (slot-boundp-using-class class struct-a (find-slotd class 'boxed)))
    (assert (slot-boundp-using-class class struct-a (find-slotd class 'raw)))))

(with-test (:name (:always-boundp-struct-a :struct-accessors))
  (let ((struct-a (make-struct-a)))
    (assert (eql 0 (struct-a-boxed struct-a)))
    (assert (eql 0.0d0 (struct-a-raw struct-a)))))

(with-test (:name (:not-always-boundp-struct-b :notinline))
  (let ((fun (lambda (x slot-name)
               (declare (notinline slot-boundp))
               (slot-boundp x slot-name)))
        (struct-b (make-struct-b)))
    (assert (funcall fun struct-b 'boxed))
    (assert (funcall fun struct-b 'raw))

    (assert (not (funcall fun struct-b 'unboundable)))
    (setf (struct-b-unboundable struct-b) 42)
    (assert (= (slot-value struct-b 'unboundable) 42))
    (assert (funcall fun struct-b 'unboundable))

    (assert (not (funcall fun struct-b 'unboundable-boxed)))
    (setf (struct-b-unboundable-boxed struct-b) 43)
    (assert (= (slot-value struct-b 'unboundable-boxed) 43))
    (assert (funcall fun struct-b 'unboundable-boxed))

    ;; Raw slots are always-bound even if there is an &AUX constructor
    ;; (there is no reasonable way to represent an unbound marker in a
    ;; raw slot).
    (assert (funcall fun struct-b 'unboundable-raw))
    (setf (struct-b-unboundable-raw struct-b) 44.0d0)
    (assert (= (slot-value struct-b 'unboundable-raw) 44.0d0))
    (assert (funcall fun struct-b 'unboundable-raw))))

(with-test (:name (:not-always-boundp-struct-b :inline))
  (let ((struct-b (make-struct-b)))
    (assert (slot-boundp struct-b 'boxed))
    (assert (slot-boundp struct-b 'raw))

    (assert (not (slot-boundp struct-b 'unboundable)))
    (setf (struct-b-unboundable struct-b) 42)
    (assert (= (slot-value struct-b 'unboundable) 42))
    (assert (slot-boundp struct-b 'unboundable))

    (assert (not (slot-boundp struct-b 'unboundable-boxed)))
    (setf (struct-b-unboundable-boxed struct-b) 43)
    (assert (= (slot-value struct-b 'unboundable-boxed) 43))
    (assert (slot-boundp struct-b 'unboundable-boxed))

    (assert (slot-boundp struct-b 'unboundable-raw))
    (setf (struct-b-unboundable-raw struct-b) 44.0d0)
    (assert (= (slot-value struct-b 'unboundable-raw) 44.0d0))
    (assert (slot-boundp struct-b 'unboundable-raw))))

(with-test (:name (:not-always-boundp-struct-b :mop))
  (let ((struct-b (make-struct-b))
        (class (find-class 'struct-b)))
    (assert (slot-boundp-using-class class struct-b (find-slotd class 'boxed)))
    (assert (slot-boundp-using-class class struct-b (find-slotd class 'raw)))

    (assert (not (slot-boundp-using-class class struct-b (find-slotd class 'unboundable))))
    (setf (struct-b-unboundable struct-b) 42)
    (assert (= (slot-value struct-b 'unboundable) 42))
    (assert (slot-boundp-using-class class struct-b (find-slotd class 'unboundable)))

    (assert (not (slot-boundp-using-class class struct-b (find-slotd class 'unboundable-boxed))))
    (setf (struct-b-unboundable-boxed struct-b) 43)
    (assert (= (slot-value struct-b 'unboundable-boxed) 43))
    (assert (slot-boundp-using-class class struct-b (find-slotd class 'unboundable-boxed)))

    (assert (slot-boundp-using-class class struct-b (find-slotd class 'unboundable-raw)))
    (setf (struct-b-unboundable-raw struct-b) 44.0d0)
    (assert (= (slot-value struct-b 'unboundable-raw) 44.0d0))
    (assert (slot-boundp-using-class class struct-b (find-slotd class 'unboundable-raw)))))

(with-test (:name (:not-always-boundp-struct-b :struct-accessors))
  (let ((struct-b (make-struct-b)))
    (assert (eql 0 (struct-b-boxed struct-b)))
    (assert (eql 0.0d0 (struct-b-raw struct-b)))

    (assert-error (struct-b-unboundable struct-b))
    (setf (struct-b-unboundable struct-b) 42)
    (assert (= (struct-b-unboundable struct-b) 42))

    (assert-error (struct-b-unboundable-boxed struct-b))
    (setf (struct-b-unboundable-boxed struct-b) 43)
    (assert (= (struct-b-unboundable-boxed struct-b) 43))

    (assert (eql 0.0d0 (struct-b-unboundable-raw struct-b)))
    (setf (struct-b-unboundable-raw struct-b) 44.0d0)
    (assert (= (slot-value struct-b 'unboundable-raw) 44.0d0))))

(with-test (:name (:cannot-makunbound-struct-a :notinline))
  (let ((fun (lambda (x slot-name)
               (declare (notinline slot-makunbound))
               (slot-makunbound x slot-name)))
        (struct-a (make-struct-a)))
    (assert-error (funcall fun struct-a 'boxed))
    (assert-error (funcall fun struct-a 'raw))))

(with-test (:name (:cannot-makunbound-struct-a :inline))
  (let ((struct-a (make-struct-a)))
    (assert-error (slot-makunbound struct-a 'boxed))
    (assert-error (slot-makunbound struct-a 'raw))))

(with-test (:name (:cannot-makunbound-struct-a :mop))
  (let ((struct-a (make-struct-a))
        (class (find-class 'struct-a)))
    (assert-error (slot-makunbound-using-class class struct-a (find-slotd class 'boxed)))
    (assert-error (slot-makunbound-using-class class struct-a (find-slotd class 'raw)))))

(with-test (:name (:can-makunbound-struct-b :notinline))
  (let ((fun (lambda (x slot-name)
               (declare (notinline slot-makunbound))
               (slot-makunbound x slot-name)))
        (struct-b (make-struct-b)))
    (assert (not (slot-boundp struct-b 'unboundable)))
    (assert-error (funcall fun struct-b 'boxed))
    (assert-error (funcall fun struct-b 'raw))
    (assert (eql (funcall fun struct-b 'unboundable) struct-b))

    (setf (struct-b-unboundable struct-b) 42)
    (assert (slot-boundp struct-b 'unboundable))
    (assert (= (slot-value struct-b 'unboundable) 42))
    (assert (eql (funcall fun struct-b 'unboundable) struct-b))
    (assert (not (slot-boundp struct-b 'unboundable)))

    (setf (struct-b-unboundable-boxed struct-b) 43)
    (assert (slot-boundp struct-b 'unboundable-boxed))
    (assert (= (slot-value struct-b 'unboundable-boxed) 43))
    (assert (eql (funcall fun struct-b 'unboundable-boxed) struct-b))
    (assert (not (slot-boundp struct-b 'unboundable-boxed)))

    (setf (struct-b-unboundable-raw struct-b) 44.0d0)
    (assert (slot-boundp struct-b 'unboundable-raw))
    (assert (= (slot-value struct-b 'unboundable-raw) 44.0d0))
    (assert-error (funcall fun struct-b 'unboundable-raw))
    (assert (slot-boundp struct-b 'unboundable-raw))))

(with-test (:name (:can-makunbound-struct-b :inline))
  (let ((struct-b (make-struct-b)))
    (assert (not (slot-boundp struct-b 'unboundable)))
    (assert-error (slot-makunbound struct-b 'boxed))
    (assert-error (slot-makunbound struct-b 'raw))
    (assert (eql (slot-makunbound struct-b 'unboundable) struct-b))

    (setf (struct-b-unboundable struct-b) 42)
    (assert (slot-boundp struct-b 'unboundable))
    (assert (= (slot-value struct-b 'unboundable) 42))
    (assert (eql (slot-makunbound struct-b 'unboundable) struct-b))
    (assert (not (slot-boundp struct-b 'unboundable)))

    (setf (struct-b-unboundable-boxed struct-b) 43)
    (assert (slot-boundp struct-b 'unboundable-boxed))
    (assert (= (slot-value struct-b 'unboundable-boxed) 43))
    (assert (eql (slot-makunbound struct-b 'unboundable-boxed) struct-b))
    (assert (not (slot-boundp struct-b 'unboundable-boxed)))

    (setf (struct-b-unboundable-raw struct-b) 44.0d0)
    (assert (slot-boundp struct-b 'unboundable-raw))
    (assert (= (slot-value struct-b 'unboundable-raw) 44.0d0))
    (assert-error (slot-makunbound struct-b 'unboundable-raw))
    (assert (slot-boundp struct-b 'unboundable-raw))))

(with-test (:name (:can-makunbound-struct-b :mop))
  (let ((struct-b (make-struct-b))
        (class (find-class 'struct-b)))
    (assert (not (slot-boundp struct-b 'unboundable)))
    (assert-error (slot-makunbound-using-class class struct-b (find-slotd class 'boxed)))
    (assert-error (slot-makunbound-using-class class struct-b (find-slotd class 'raw)))
    (assert (eql (slot-makunbound-using-class class struct-b (find-slotd class 'unboundable))
                 struct-b))

    (setf (struct-b-unboundable struct-b) 42)
    (assert (slot-boundp struct-b 'unboundable))
    (assert (= (slot-value struct-b 'unboundable) 42))
    (assert (eql (slot-makunbound-using-class class struct-b (find-slotd class 'unboundable))
                 struct-b))
    (assert (not (slot-boundp struct-b 'unboundable)))

    (setf (struct-b-unboundable-boxed struct-b) 43)
    (assert (slot-boundp struct-b 'unboundable-boxed))
    (assert (= (slot-value struct-b 'unboundable-boxed) 43))
    (assert
     (eql (slot-makunbound-using-class class struct-b (find-slotd class 'unboundable-boxed))
          struct-b))
    (assert (not (slot-boundp struct-b 'unboundable-boxed)))

    (setf (struct-b-unboundable-raw struct-b) 44.0d0)
    (assert (slot-boundp struct-b 'unboundable-raw))
    (assert (= (slot-value struct-b 'unboundable-raw) 44.0d0))
    (assert-error
     (slot-makunbound-using-class class struct-b (find-slotd class 'unboundable-raw)))
    (assert (slot-boundp struct-b 'unboundable-raw))))

(with-test (:name (:not-always-boundp-condition-a :notinline))
  (let ((fun (lambda (x slot-name)
               (declare (notinline slot-boundp))
               (slot-boundp x slot-name)))
        (condition-a (make-condition 'condition-a)))
    (assert (funcall fun condition-a 'condition-bound))

    (assert (not (funcall fun condition-a 'condition-unbound)))
    (setf (slot-value condition-a 'condition-unbound) 42)
    (assert (funcall fun condition-a 'condition-unbound))

    (assert (not (funcall fun condition-a 'condition-allocation-class)))
    (setf (slot-value condition-a 'condition-allocation-class) 43)
    (assert (funcall fun condition-a 'condition-allocation-class))))

(with-test (:name (:can-makunbound-condition-a :notinline))
  (let ((fun (lambda (x slot-name)
               (declare (notinline slot-makunbound))
               (slot-makunbound x slot-name)))
        (condition-a (make-condition 'condition-a)))
    (assert (eql (funcall fun condition-a 'condition-bound) condition-a))
    (assert (not (slot-boundp condition-a 'condition-bound)))

    (assert (eql (funcall fun condition-a 'condition-unbound) condition-a))
    (setf (slot-value condition-a 'condition-unbound) 42)
    (assert (slot-boundp condition-a 'condition-unbound))
    (assert (funcall fun condition-a 'condition-unbound))
    (assert (not (slot-boundp condition-a 'condition-unbound)))

    (assert (eql (funcall fun condition-a 'condition-allocation-class) condition-a))
    (setf (slot-value condition-a 'condition-allocation-class) 43)
    (assert (slot-boundp condition-a 'condition-allocation-class))
    (assert (funcall fun condition-a 'condition-allocation-class))
    (assert (not (slot-boundp condition-a 'condition-allocation-class)))))

(with-test (:name (:not-always-boundp-class-a :notinline))
  (slot-makunbound (make-instance 'class-a) 'class-allocation-class)
  (let ((fun (lambda (x slot-name)
               (declare (notinline slot-boundp))
               (slot-boundp x slot-name)))
        (class-a (make-instance 'class-a)))
    (assert (funcall fun class-a 'class-bound))

    (assert (not (funcall fun class-a 'class-unbound)))
    (setf (slot-value class-a 'class-unbound) 42)
    (assert (funcall fun class-a 'class-unbound))

    (assert (not (funcall fun class-a 'class-allocation-class)))
    (setf (slot-value class-a 'class-allocation-class) 43)
    (assert (funcall fun class-a 'class-allocation-class))))

(defmethod not-always-boundp-class-a ((x class-a))
  (assert (slot-boundp x 'class-bound))

  (assert (not (slot-boundp x 'class-unbound)))
  (setf (slot-value x 'class-unbound) 42)
  (assert (slot-boundp x 'class-unbound))

  (assert (not (slot-boundp x 'class-allocation-class)))
  (setf (slot-value x 'class-allocation-class) 43)
  (assert (slot-boundp x 'class-allocation-class))

  (let ((y (make-instance 'class-a)))
    (assert (slot-boundp y 'class-bound))
    (assert (not (slot-boundp y 'class-unbound)))
    (assert (slot-boundp y 'class-allocation-class))))

(with-test (:name (:not-always-boundp-class-a :method))
  (slot-makunbound (make-instance 'class-a) 'class-allocation-class)
  (not-always-boundp-class-a (make-instance 'class-a)))

(with-test (:name (:can-makunbound-class-a :notinline))
  (let ((fun (lambda (x slot-name)
               (declare (notinline slot-makunbound))
               (slot-makunbound x slot-name)))
        (class-a (make-instance 'class-a)))
    (assert (eql (funcall fun class-a 'class-bound) class-a))
    (assert (not (slot-boundp class-a 'class-bound)))

    (assert (eql (funcall fun class-a 'class-unbound) class-a))
    (setf (slot-value class-a 'class-unbound) 42)
    (assert (slot-boundp class-a 'class-unbound))
    (assert (funcall fun class-a 'class-unbound))
    (assert (not (slot-boundp class-a 'class-unbound)))

    (assert (eql (funcall fun class-a 'class-allocation-class) class-a))
    (setf (slot-value class-a 'class-allocation-class) 43)
    (assert (slot-boundp class-a 'class-allocation-class))
    (assert (funcall fun class-a 'class-allocation-class))
    (assert (not (slot-boundp class-a 'class-allocation-class)))))

(defmethod can-makunbound-class-a ((x class-a))
  (assert (eql (slot-makunbound x 'class-bound) x))
  (assert (not (slot-boundp x 'class-bound)))

  (assert (eql (slot-makunbound x 'class-unbound) x))
  (setf (slot-value x 'class-unbound) 42)
  (assert (slot-boundp x 'class-unbound))
  (assert (slot-makunbound x 'class-unbound))
  (assert (not (slot-boundp x 'class-unbound)))

  (assert (eql (slot-makunbound x 'class-allocation-class) x))
  (setf (slot-value x 'class-allocation-class) 43)
  (assert (slot-boundp x 'class-allocation-class))
  (assert (slot-makunbound x 'class-allocation-class))
  (assert (not (slot-boundp x 'class-allocation-class)))

  (let ((y (make-instance 'class-a)))
    (assert (slot-boundp y 'class-bound))
    (assert (not (slot-boundp y 'class-unbound)))
    (assert (not (slot-boundp y 'class-allocation-class)))))

(with-test (:name (:can-makunbound-class-a :method))
  (slot-makunbound (make-instance 'class-a) 'class-allocation-class)
  (can-makunbound-class-a (make-instance 'class-a)))
