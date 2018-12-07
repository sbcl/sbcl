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

;;; this file tests the programmatic class example from pp.67-69 of
;;; AMOP.

(defun make-programmatic-instance (superclass-names &rest initargs)
  (apply #'make-instance
         (find-programmatic-class
          (mapcar #'find-class superclass-names))
         initargs))

(defun find-programmatic-class (superclasses)
  (let ((class (find-if
                 (lambda (class)
                   (equal superclasses
                          (sb-mop:class-direct-superclasses class)))
                 (sb-mop:class-direct-subclasses (car superclasses)))))
    (or class
        (make-programmatic-class superclasses))))

(defun make-programmatic-class (superclasses)
  (make-instance 'standard-class
                 :name (mapcar #'class-name superclasses)
                 :direct-superclasses superclasses
                 :direct-slots '()))

(defclass shape () ())
(defclass circle (shape) ())
(defclass color () ())
(defclass orange (color) ())
(defclass magenta (color) ())
(defclass label-type () ())
(defclass top-labeled (label-type) ())
(defclass bottom-labeled (label-type) ())

(with-test (:name (:mop-17 1))
  (assert (null (sb-mop:class-direct-subclasses (find-class 'circle)))))

(defvar *i1* (make-programmatic-instance '(circle orange top-labeled)))
(defvar *i2* (make-programmatic-instance '(circle magenta bottom-labeled)))
(defvar *i3* (make-programmatic-instance '(circle orange top-labeled)))

(with-test (:name (:mop-17 2))
  (assert (not (eq *i1* *i3*)))

  (assert (= (length (sb-mop:class-direct-subclasses (find-class 'circle))) 2)))
