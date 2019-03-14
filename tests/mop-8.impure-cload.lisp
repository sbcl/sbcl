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

;;; This file contains tests of UPDATE-DEPENDENT.

(defclass dependent-history ()
  ((history :initarg :history :accessor history)))

(defmethod sb-mop:update-dependent ((generic-function generic-function)
                                    (history dependent-history)
                                    &rest args)
  (push args (history history)))
(defmethod sb-mop:update-dependent ((class class)
                                    (history dependent-history)
                                    &rest args)
  (push (cons class args) (history history)))

(defvar *history* (make-instance 'dependent-history :history nil))

(defgeneric upd1 (x))
(sb-mop:add-dependent #'upd1 *history*)
(defmethod upd1 ((x integer)) x)

(with-test (:name (:mop-8 1))
  (let ((last (car (history *history*))))
    (assert (eq (car last) 'add-method))
    (assert (typep (cadr last) 'standard-method))))

(defclass foo ()
  ())
(sb-mop:add-dependent (find-class 'foo) *history*)
(defclass foo ()
  ((a :initarg :a)))

(with-test (:name (:mop-8 2))
  (let ((last (car (history *history*))))
    (assert (eq (car last) (find-class 'foo)))))
