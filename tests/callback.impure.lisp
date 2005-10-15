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

;;; callbacks only on a few platforms
#-(or (and ppc darwin) x86 x86-64)
(quit :unix-status 104)

;;; simple callback for a function

(defun thunk ()
  (write-string "hi"))

(defvar *thunk*
  (sb-alien::alien-callback (function c-string) #'thunk))

(assert (equal (with-output-to-string (*standard-output*)
                 (alien-funcall *thunk*))
               "hi"))

;;; simple callback for a symbol

(defun add-two-ints (arg1 arg2)
  (+ arg1 arg2))

(defvar *add-two-ints*
  (sb-alien::alien-callback (function int int int) 'add-two-ints))

(assert (= (alien-funcall *add-two-ints* 555 444444) 444999))

;;; actually using a callback with foreign code

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

;;; returning floats

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

;;; invalidation

(sb-alien::define-alien-callback to-be-invalidated int ()
  5)

(assert (= 5 (alien-funcall to-be-invalidated)))

(multiple-value-bind (p valid) (sb-alien::alien-callback-p to-be-invalidated)
  (assert p)
  (assert valid))

(sb-alien::invalidate-alien-callback to-be-invalidated)

(multiple-value-bind (p valid) (sb-alien::alien-callback-p to-be-invalidated)
  (assert p)
  (assert (not valid)))

(multiple-value-bind (res err)
    (ignore-errors (alien-funcall to-be-invalidated))
  (assert (and (not res) (typep err 'error))))

;;; getting and setting the underlying function

(sb-alien::define-alien-callback foo int ()
  13)

(defvar *foo* #'foo)

(assert (eq #'foo (sb-alien::alien-callback-function foo)))

(defun bar ()
  26)

(setf (sb-alien::alien-callback-function foo) #'bar)

(assert (eq #'bar (sb-alien::alien-callback-function foo)))

(assert (= 26 (alien-funcall foo)))

;;; callbacks with void return values

(with-test (:name void-return)
  (sb-alien::alien-lambda void ()
    (values)))

;;; tests for a sign extension problem in callback argument handling on x86-64

(with-test (:name sign-extension :fails-on :x86-64)
  (let ((*add-two-ints*
         (sb-alien::alien-callback (function int int int) #'+)))
    (assert (= (alien-funcall *add-two-ints* #x-80000000 1)
               -2147483647))
    (assert (= (alien-funcall *add-two-ints* #x-80000000 -1)
               #x7fffffff))))

