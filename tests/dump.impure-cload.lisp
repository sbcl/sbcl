;;;; tests related to the way objects are dumped into fasl files

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

(declaim (optimize (debug 3) (speed 2) (space 1)))

;;; Don Geddis reported this test case 25 December 1999 on a CMU CL
;;; mailing list: dumping circular lists caused the compiler to enter
;;; an infinite loop. Douglas Crosher reported a patch 27 Dec 1999.
;;; The patch was tested on SBCL by Martin Atzmueller 2 Nov 2000, and
;;; merged in sbcl-0.6.8.11.
(defun q-dg1999-1 () (dolist (x '#1=("A" "B" . #1#)) x))
(defun q-dg1999-2 () (dolist (x '#1=("C" "D" . #1#)) x))
(defun q-dg1999-3 () (dolist (x '#1=("E" "F" . #1#)) x))
(defun q-dg1999-4 () (dolist (x '#1=("C" "D" . #1#)) x))
(defun useful-dg1999 (keys)
  (declare (type list keys))
  (loop
      for c in '#1=("Red" "Blue" . #1#)
      for key in keys))

;;; sbcl-0.6.11.25 or so had DEF!STRUCT/MAKE-LOAD-FORM/HOST screwed up
;;; so that the compiler couldn't dump pathnames.
(format t "Now the compiler can dump pathnames again: ~S ~S~%" #p"" #p"/x/y/z")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct foo x y)
  (defmethod make-load-form ((foo foo) &optional env)
    (declare (ignore env))
    ;; an extremely meaningless MAKE-LOAD-FORM method whose only point
    ;; is to exercise the mechanism a little bit
    (values `(make-foo :x (list ',(foo-x foo)))
	    `(setf (foo-y ,foo) ',foo))))

(defparameter *foo*
  #.(make-foo :x "X" :y "Y"))

(assert (equalp (foo-x *foo*) '("X")))
(assert (eql (foo-y *foo*) *foo*))

;;; Logical pathnames should be dumpable, too, but what does it mean?
;;; As of sbcl-0.7.7.16, we've taken dumping the host part to mean
;;; dumping a reference to the name of the host (much as dumping a
;;; symbol involves dumping a reference to the name of its package).
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (logical-pathname-translations "MY-LOGICAL-HOST")
	(list '("**;*.*.*" "/tmp/*.*"))))

(defparameter *path* #p"MY-LOGICAL-HOST:FOO;BAR.LISP")

;;; Non-SIMPLE-ARRAY VECTORs should be dumpable, though they can lose
;;; their complex attributes.

(defparameter *string* #.(make-array 3 :initial-element #\a
				       :fill-pointer 2
				       :element-type 'character))

;;; SBCL 0.7.8 incorrectly read high bits of (COMPLEX DOUBLE-FLOAT)
;;; components as unsigned bytes.
(defparameter *numbers*
  '(-1s0 -1f0 -1d0 -1l0
    #c(-1s0 -1s0) #c(-1f0 -1f0) #c(-1d0 -1d0) #c(-1l0 -1l0)))

(sb-ext:quit :unix-status 104) ; success
