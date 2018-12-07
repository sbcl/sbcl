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

;;; This file contains simple tests for
;;; SET-FUNCALLABLE-INSTANCE-FUNCTION on FUNCALLABLE-INSTANCEs

;;; from Justin Dubs on comp.lang.lisp
(defclass fn ()
  ()
  (:metaclass sb-mop:funcallable-standard-class))

(defvar *fn*)

(defmethod initialize-instance :after ((fn fn) &rest initargs &key
                                       &allow-other-keys)
  (declare (ignore initargs))
  (sb-mop:set-funcallable-instance-function fn
                                            (lambda (x)
                                              (setf *fn* fn)
                                              (1+ x))))

(with-test (:name (:mop-5 1))
  (let ((fun (make-instance 'fn)))
    (assert (= (funcall fun 42) 43))
    (assert (eq *fn* fun))))

;;; from Tony Martinez sbcl-devel
(defclass counter ()
  ((number :initarg :start :accessor counter))
  (:metaclass sb-pcl::funcallable-standard-class))

(defun make-counter (&key (start 0))
  (let ((instance (make-instance 'counter :start start)))
    (sb-mop:set-funcallable-instance-function
     instance
     ;; When run, this function doesn't print the instance, but (what
     ;; I think is) itself.
     (lambda () (print instance (make-broadcast-stream))))
    instance))

(defparameter *counter* (make-counter :start 666))

(with-test (:name (:mop-5 2))
  (assert (eq (funcall *counter*) *counter*)))
