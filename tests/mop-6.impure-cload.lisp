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

;;; This file contains simple tests for COMPUTE-SLOTS :AROUND
;;; respecting the order requested by the primary method.

;;; COMPUTE-SLOTS :AROUND respecting requested order
(defclass slot-rearrangement-class (standard-class)
  ())
(defmethod sb-mop:compute-slots ((c slot-rearrangement-class))
  (reverse (call-next-method)))
(defmethod sb-mop:validate-superclass ((c slot-rearrangement-class)
                                       (s standard-class))
  t)
(defclass rearranged-class ()
  ((a :initarg :a :initform 1)
   (b :initarg :b :initform 2))
  (:metaclass slot-rearrangement-class))

(with-test (:name (:compute-slots :standard-class :order))
  (let ((class (find-class 'rearranged-class)))
    (sb-mop:finalize-inheritance class)
    (assert (equal (mapcar #'sb-mop:slot-definition-name
                           (sb-mop:class-slots class))
                   '(b a)))))
(with-test (:name (:compute-slots :standard-class :slots))
  (let ((r (make-instance 'rearranged-class))
        (r2 (make-instance 'rearranged-class :a 3 :b 4)))
    (assert (eql (slot-value r 'a) 1))
    (assert (eql (slot-value r 'b) 2))
    (assert (eql (slot-value r2 'a) 3))
    (assert (eql (slot-value r2 'b) 4))))

(defclass funcallable-slot-rearrangement-class (sb-mop:funcallable-standard-class)
  ())
(defmethod sb-mop:compute-slots ((c funcallable-slot-rearrangement-class))
  (reverse (call-next-method)))
(defmethod sb-mop:validate-superclass ((c funcallable-slot-rearrangement-class)
                                       (s sb-mop:funcallable-standard-class))
  t)
(defclass funcallable-rearranged-class ()
  ((a :initarg :a :initform 1)
   (b :initarg :b :initform 2))
  (:metaclass funcallable-slot-rearrangement-class))

(with-test (:name (:compute-slots :funcallable-standard-class :order))
  (let ((class (find-class 'funcallable-rearranged-class)))
    (sb-mop:finalize-inheritance class)
    (assert (equal (mapcar #'sb-mop:slot-definition-name
                           (sb-mop:class-slots class))
                   '(b a)))))

(with-test (:name (:compute-slots :funcallable-standard-class :slots))
  (let ((r (make-instance 'funcallable-rearranged-class))
        (r2 (make-instance 'funcallable-rearranged-class :a 3 :b 4)))
    (assert (eql (slot-value r 'a) 1))
    (assert (eql (slot-value r 'b) 2))
    (assert (eql (slot-value r2 'a) 3))
    (assert (eql (slot-value r2 'b) 4))))

(with-test (:name (:compute-slots :funcallable-standard-clas :function))
  (let ((r (make-instance 'funcallable-rearranged-class)))
    (sb-mop:set-funcallable-instance-function r (lambda (x) (list "Hello, World!" x)))
    (assert (equal (funcall r 3) '("Hello, World!" 3)))))
