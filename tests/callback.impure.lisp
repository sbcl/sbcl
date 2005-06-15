;;;; package lock tests with side effects

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

(in-package :cl-user)

(defun alien-callback (type fun)
  (sb-alien-internals:alien-callback type fun))

(defun thunk ()
  (write-string "hi"))

(defvar *thunk* (alien-callback '(function c-string) #'thunk))

(assert (equal (with-output-to-string (*standard-output*)
		 (alien-funcall *thunk*))
	       "hi"))

(defun add-two-ints (arg1 arg2)
  (+ arg1 arg2))

(defvar *add-two-ints* (alien-callback '(function int int int) 'add-two-ints))

(assert (= (alien-funcall *add-two-ints* 555 444444) 444999))

(define-alien-routine qsort void
  (base (* t))
  (nmemb int)
  (size int)
  (compar (function int (* double) (* double))))

(sb-alien::define-alien-callback double*-cmp int ((arg1 (* double)) (arg2 (* double)))
  (let ((a1 (deref arg1))
	(a2 (deref arg2)))
    (cond ((= a1 a2) 0)
	  ((< a1 a2) -1)
	  (t 1))))

(let* ((vector (coerce '(0.1d0 0.5d0 0.2d0 1.2d0 1.5d0 2.5d0 0.0d0 0.1d0 0.2d0 0.3d0)
		       '(vector double-float)))
       (sorted (sort (copy-seq vector) #'<)))
  (gc :full t)
  (sb-sys:with-pinned-objects (vector)
    (qsort (sb-sys:vector-sap vector)
	   (length vector)
	   (alien-size double :bytes)
	   double*-cmp))
  (assert (equalp vector sorted)))


(sb-alien::define-alien-callback redefined-fun int ()
    0)

(eval
 '(sb-alien::define-alien-callback redefined-fun int ()
   42))

(assert (= 42 (alien-funcall redefined-fun)))

(sb-alien::define-alien-callback return-single float ((x float))
  x)

(sb-alien::define-alien-callback return-double double ((x double))
  x)

(defconstant spi (coerce pi 'single-float))

(assert (= spi (alien-funcall return-single spi)))
(assert (= pi (alien-funcall return-double pi)))

(quit :unix-status 104)
