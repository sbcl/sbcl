
;;;
;;; The order of the forms must not change, as the order is checked in
;;; `test-driver.lisp'. Thus do not alter this file unless you edit
;;; test-driver.lisp to match.
;;;

(declaim (optimize (debug 3)))
(in-package :cl-user)

(defun one (a b c) (+ a b c))

(defgeneric two (a b))
(defmethod two ((a number) b)
  (* 2 a))

(defstruct three four five)

(with-compilation-unit (:source-plist (list :test-inner "IN"))
  (eval '(defun four () 4)))

"oops-off-by-one"

(defparameter *a* 1)

(defvar *b* 2)

(defclass a ()
  (a))

(define-condition b (warning) (a))

(defstruct c e f)

(defstruct (d (:type list)) e f)

(defpackage e (:use :cl))

(define-symbol-macro f 'e)

(deftype g () 'fixnum)

(defconstant +h+ 1)

(defmethod j ((a t))
  2)

(defmethod j ((b null))
  2)

(defmacro l (a)
  a)

(define-compiler-macro m (a)
  (declare (ignore a))
  'b)

(defsetf n (a) (store)
  (format t "~a ~a~%" a store))

(defun (setf o) (x)
  (print x))

(defmethod (setf p) (x y)
  (format t "~a ~a~%" x y))

(define-modify-macro q (x) logand)

(define-method-combination r)

(define-setf-expander s (a b)
  (format t "~a ~a~%" a b))

(eval-when (:compile-toplevel)
  (defun compile-time-too-fun ()
    :foo))

(sb-ext:defglobal **global** 'value)

(sb-alien:define-alien-type test-alien-type sb-alien:long)

(sb-alien:define-alien-type nil
  (struct test-alien-struct
          (x sb-alien:system-area-pointer)))

(sb-alien:define-alien-variable ("errno" test-alien-var) sb-alien:int)

(define-condition test-condition (error)
  ((a :reader condition-slot-reader
      :writer condition-slot-writer)))

(defun with-a-local-function ()
  (flet ((x ()))
    (declare (notinline x))
    (x)))

(defun 0-debug (a b c) (declare (optimize (debug 0))) (+ a b c))
