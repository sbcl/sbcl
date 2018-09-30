;;;; -*- lisp -*-

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

;;;; recognize self-calls
(declaim (optimize speed))

;;; This is a fopcompilable form that caused FOP stack underflow
;;; because the PROGN and SETQ each failed to push a NIL onto the stack.
;;; >>> DO NOT ADD A (WITH-TEST) TO THIS. <<< It must stay fopcompilable.
(let ((a (progn)) ; lp# 1427050
      (b (setq))
      (c (+ 1 2)))
  (print c)
  (defvar *aaa* a)
  (defvar *bbb* b))


;;;; These three forms should be equivalent.

;;; This used to be a bug in the handling of null-lexenv vs toplevel
;;; policy: LOCALLY and MACROLET hid the toplevel policy from view.

(locally
    (defun foo (n)
      (frob 'foo)
      (if (<= n 0)
          n
          (foo (1- n)))))

(progn
  (defun bar (n)
    (frob 'bar)
    (if (<= n 0)
        n
        (bar (1- n)))))

(macrolet ()
  (defun quux (n)
    (frob 'quux)
    (if (<= n 0)
        n
        (quux (1- n)))))

(defun frob (x)
  (setf (fdefinition x) (constantly 13)))

(defun test ()
  (list (foo 1) (bar 1) (quux 1)))

(assert (equal (test) '(0 0 0)))
(assert (equal (test) '(13 13 13))) ; sanity check

;;; Bug in 1.0.2 and 1.0.3, where the XEP was compiled with the wrong
;;; policy. (Test-case derived from code posted by alexander.ekart in
;;; comp.lang.lisp).

(locally
    (declare (optimize (safety 0)))
  (defun bubblesort (x y)
    (declare (type (simple-array fixnum (*)) x)
             (type fixnum y)
             (optimize (speed 3) (safety 3) (space 0) (debug 0)))
    (aref x y)))

(assert-error (bubblesort (make-array 10) 9))

(define-symbol-macro %trash% what)
(locally
  ;; just in case we get so smart that INFO becomes foldable
  (declare (notinline sb-int:info))
  (assert (eq (sb-int:info :variable :kind '%trash%) :macro))
  (assert (eq (sb-int:info :variable :macro-expansion '%trash%) 'what))
  (assert (sb-int:info :source-location :symbol-macro '%trash%)))
(let ()
  (declare (notinline sb-int:info))
  (defconstant %trash% 9) ; this is non-toplevel
  (multiple-value-bind (val foundp)
      (sb-int:info :variable :macro-expansion '%trash%)
    (assert (and (not val) (not foundp)))))

;; This must be one toplevel form.
;; In practice you'd never do anything like this I suspect.
(progn
  (defvar *foofoo1* 1)
  (eval-when (:compile-toplevel)
    (setq sb-c::*source-plist* '(strange "Yes")))
  (defvar *foofoo2* 2))

(with-test (:name :source-location-plist-invalid-memoization)
  (assert (null (sb-c:definition-source-location-plist
                    (sb-int:info :source-location :variable '*foofoo1*))))
  (assert (equal (sb-c:definition-source-location-plist
                     (sb-int:info :source-location :variable '*foofoo2*))
                 '(strange "Yes"))))

(defstruct zombie-cast-struct
  (value nil :type zombie-cast-struct-v))

(defstruct zombie-cast-struct-v
  (uses nil :type (or list zombie-cast-struct)))

(with-test (:name :flush-zombie-casts)
  (checked-compile `(lambda (x)
                      (declare (optimize (debug 2) (speed 1)))
                      (let ((value (zombie-cast-struct-value x)))
                        (labels ((l (x)
                                   (declare (ignore x))
                                   (let ((uses (zombie-cast-struct-v-uses value)))
                                     (when (zombie-cast-struct-p uses)
                                       (list* uses (l uses)))))))))))

(let ()
  (define-condition non-top-level-condition (error) ()))

(with-test (:name :non-top-level-condition)
  (assert
   (handler-case (signal 'non-top-level-condition)
     (non-top-level-condition () t))))

(defun somefun (x)
  (declare (optimize (sb-c:store-source-form 3)))
  (+ x 101))

(locally
    (declare (optimize (sb-c:store-source-form 3)))
  (defun otherfun (y)
    (/ y 3)))

(locally
    (declare (optimize (sb-c:store-source-form 2)))
  (defun nosourcefun (y)
    (/ y 3)))

(with-test (:name :store-source-form)
  (assert (function-lambda-expression #'somefun))
  (assert (function-lambda-expression #'otherfun))
  (assert (not (function-lambda-expression #'nosourcefun))))
