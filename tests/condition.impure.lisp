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

(cl:in-package :cl-user)

;;; Bug from CLOCC.
(defpackage :p1
  (:use :cl)
  (:export #:code #:code-msg #:%code-msg))
(in-package :p1)
(define-condition code ()
  ((msg :reader code-msg :reader %code-msg :initarg :msg)))

(defpackage :p2
  (:use :cl :p1))
(in-package :p2)
(define-condition code1 (code)
  ((msg :accessor code-msg :initarg :msg)))

(let ((code (make-condition 'code :msg 1)))
  (assert (typep code 'code))
  (assert (eql (code-msg code) 1))
  (assert (eql (%code-msg code) 1)))
(let ((code (make-condition 'code1 :msg 1)))
  (assert (typep code 'code))
  (assert (eql (code-msg code) 1))
  (assert (eql (%code-msg code) 1))
  (setf (code-msg code) 2)
  (assert (eql (code-msg code) 2))
  (assert (eql (%code-msg code) 1)))

;;; Check that initializing the condition class metaobject doesn't create
;;; any instances. Reported by Marco Baringer on sbcl-devel Mon, 05 Jul 2004.
(defvar *condition-count* 0)
(define-condition counted-condition () ((slot :initform (incf *condition-count*))))
(defmethod frob-counted-condition ((x counted-condition)) x)
(assert (= 0 *condition-count*))
(assert (typep (sb-mop:class-prototype (find-class 'counted-condition))
               '(and condition counted-condition)))

;;; success
