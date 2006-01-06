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
#-alien-callbacks
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

(defvar *add-two-ints* (sb-alien::alien-callback (function int int int) #'+))

(with-test (:name :sign-extension)
  (assert (= (alien-funcall *add-two-ints* #x-80000000 1) -2147483647)))

;;; On x86 This'll signal a TYPE-ERROR "The value -2147483649 is not of type
;;; (SIGNED-BYTE 32)". On x86-64 it'll wrap around to 2147483647, probably
;;; due to the sign-extension done by the (INTEGER :NATURALIZE-GEN)
;;; alien-type-method. I believe the former behaviour is the one we want.
;;; -- JES, 2005-10-16

(with-test (:name :underflow-detection :fails-on :x86-64)
  (assert (raises-error? (alien-funcall *add-two-ints* #x-80000000 -1))))


;;; test for callbacks of various arities
;;; CLH 2005-12-21

(defparameter *type-abbreviations*
  '((sb-alien:int . "i")
    (sb-alien:float . "f")
    (sb-alien:double . "d")
    (sb-alien:short . "h")
    (sb-alien:char . "c")))

(defun parse-callback-arg-spec (spec)
  (let ((l (coerce spec 'list)))
    (loop for g in l by #'cddr
       collect (car (rassoc (string-downcase g) *type-abbreviations* :test #'equal)))))

(macrolet ((define-callback-adder2 (return-type spec)
             (let ((fname (format nil "*add-~A*" spec))
                   (l (parse-callback-arg-spec spec)))
               `(progn
                  (defparameter ,(intern (string-upcase fname))
                    (sb-alien::alien-callback
                     (function ,return-type
                               ,@l) '+))))))
  (define-callback-adder2 int "i-i"))

(macrolet ((define-callback-adder (&rest types)
             (let ((fname (format nil "*add-~{~A~^-~}*"
                                  (mapcar
                                   #'(lambda (x)
                                       (cdr (assoc x *type-abbreviations*)))
                                   (mapcar
                                    #'(lambda (y) (find-symbol (string-upcase y) 'sb-alien))
                                    (cdr types))))))
               `(progn
                  (print ,fname)
                  (defparameter ,(intern
                                  (string-upcase fname))
                    (sb-alien::alien-callback (function ,@types) '+))))))

  (define-callback-adder int int int)
  (define-callback-adder int int int int)
  (define-callback-adder int int int int int)
  (define-callback-adder int int int int int int)
  (define-callback-adder int int int int int int int)
  (define-callback-adder int int int int int int int int)
  (define-callback-adder int int int int int int int int int)
  (define-callback-adder int int int int int int int int int int)
  (define-callback-adder int int int int int int int int int int int)
  (define-callback-adder int int int int int int int int int int int int)
  (define-callback-adder int int int int int int int int int int int int int)

  (define-callback-adder float float float)
  (define-callback-adder float float float float)
  (define-callback-adder float float float float float)
  (define-callback-adder float float float float float float)
  (define-callback-adder float float float float float float float)
  (define-callback-adder float float float float float float float float)
  (define-callback-adder float float float float float float float float float)
  (define-callback-adder float float float float float float float float float float)
  (define-callback-adder float float float float float float float float float float float)
  (define-callback-adder float float float float float float float float float float float float)
  (define-callback-adder float float float float float float float float float float float float float)

  (define-callback-adder double double double)
  (define-callback-adder double double double double double)
  (define-callback-adder double double double double double double)
  (define-callback-adder double double double double double double double)
  (define-callback-adder double double double double double double double double)
  (define-callback-adder double double double double double double double double double)
  (define-callback-adder double double double double double double double double double double)
  (define-callback-adder double double double double double double double double double double double)
  (define-callback-adder double double double double double double double double double double double double)
  (define-callback-adder double double double double double double double double double double double double double)

  (define-callback-adder float int float)
  (define-callback-adder float float int)
  (define-callback-adder float float int int int)

  (define-callback-adder double double int)
  (define-callback-adder double int double)

  (define-callback-adder double double float)
  (define-callback-adder double float double)

  (define-callback-adder double double float int)
  (define-callback-adder double int float double)
  (define-callback-adder double int float double double)

  (define-callback-adder double double int int int)
  (define-callback-adder double double int int int double int int int)

  (define-callback-adder double double double int int int int int int)

  (define-callback-adder double double double int int)

  (define-callback-adder double int double int double int double int double int double)

  (define-callback-adder double short double)

  (define-callback-adder double char double))


(defmacro alien-apply-form (f args)
  `(let ((a ,args))
     `(alien-funcall ,,f ,@a)))

(defmacro alien-apply (f &rest args)
  `(eval (alien-apply-form ,f ,@args)))

(defun iota (x) (if (equalp x 1) (list x) (cons x (iota (1- x)))))

(alien-funcall *add-i-i* 1 2)
(alien-funcall *add-f-f* 1.0s0 2.0s0)
(alien-funcall *add-d-d* 2.0d0 4.0d0)

(assert (= (alien-apply *add-i-i-i-i-i-i-i-i* (iota 8)) 36))
(assert (= (alien-apply *add-i-i-i-i-i-i-i-i-i-i* (iota 10)) 55))
(assert (= (alien-apply *add-i-i-i-i-i-i-i-i-i-i-i-i* (iota 12)) 78))

(assert (= (alien-apply *add-f-f-f-f-f-f-f-f* (iota 8s0)) 36s0))
(assert (= (alien-apply *add-f-f-f-f-f-f-f-f-f-f* (iota 10.0s0)) 55s0))

(assert (= (alien-apply *add-d-d-d-d-d-d-d-d* (iota 8d0)) 36d0))
(assert (= (alien-apply *add-d-d-d-d-d-d-d-d-d-d* (iota 10d0)) 55d0))

(assert (= (alien-funcall *add-i-i* 2 3) 5))
(assert (= (alien-funcall *add-d-d* 2d0 3d0) 5d0))
(assert (= (alien-funcall *add-i-d* 2 3d0) 5d0))
(assert (= (alien-funcall *add-d-i* 2d0 3) 5d0))
(assert (= (alien-funcall *add-d-f* 2d0 3s0) 5d0))
(assert (= (alien-funcall *add-f-d* 2s0 3d0) 5d0))

(assert (= (alien-funcall *add-d-i-i-i-d-i-i-i* 1d0 2 3 4 5d0 6 7 8) 36d0))

(assert (= (alien-apply *add-i-d-i-d-i-d-i-d-i-d*
             (mapcan #'(lambda (x y) (list x y)) (iota 5) (iota 5.0d0)))
           30d0))

