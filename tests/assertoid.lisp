;;;; the ASSERTOID macro, asserting something with added generality
;;;; to help in regression tests

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

(cl:defpackage "ASSERTOID"
  (:use "CL")
  (:export "GRAB-CONDITION" "RAISES-ERROR?" "IS" "ASSERTOID"))

(cl:in-package "ASSERTOID")

(defmacro grab-condition (&body body)
  `(nth-value 1
     (ignore-errors ,@body)))

(defmacro raises-error? (form &optional (error-subtype-spec 'error))
  `(typep (nth-value 1 (ignore-errors ,form)) ',error-subtype-spec))

;;; EXPR is an expression to evaluate (both with EVAL and with
;;; COMPILE/FUNCALL). EXTRA-OPTIMIZATIONS is a list of lists of
;;; optimizations to pass to (DECLARE (OPTIMIZE ..)), to cause the
;;; expression to be tested in other than the default optimization
;;; level(s).
;;;
;;; The messiness with the various flavors of EXPECTED stuff is
;;; to handle various issues:
;;;   * Some things are expected to signal errors instead of returning
;;;     ordinary values.
;;;   * Some things are expected to return multiple values.
;;;   * Some things can return any of several values (e.g. generalized
;;;     booleans).
;;; The default is to expect a generalized boolean true.
;;;
;;; Use EXPECTED-LAMBDA to require an answer which satisfies the given
;;; LAMBDA. EXPECTED-EQL, EXPECTED-EQUAL, and EXPECTED-EQUALP are
;;; shorthand for special cases of EXPECTED-LAMBDA.
;;;
;;; Use EXPECTED-ERROR to require an error to be signalled. Use
;;; EXPECTED-ERROR-LAMBDA to require that an error be signalled and
;;; that further it satisfies the given lambda.
(defmacro assertoid (expr
                     &key
                     extra-optimizations
                     (expected-eql nil expected-eql-p)
                     (expected-equal nil expected-equal-p)
                     (expected-equalp nil expected-equalp-p)
                     (expected-lambda (cond
                                       (expected-eql-p
                                        (lambda (x)
                                          (eql x (eval expected-eql))))
                                       (expected-equal-p
                                        (lambda (x)
                                          (equal x (eval expected-equal))))
                                       (expected-equalp-p
                                        (lambda (x)
                                          (equalp x (eval expected-equalp))))
                                       (t
                                        (lambda (x)
                                          x)))
                                      expected-lambda-p)
                     (expected-error-type nil expected-error-type-p)
                     (expected-error-lambda (if expected-error-type
                                                (lambda (condition)
                                                  (typep condition
                                                         expected-error-type))
                                                nil)
                                            expected-error-lambda-p))
  (when (> (count-if #'identity
                     (vector expected-eql-p
                             expected-equal-p
                             expected-equalp-p
                             expected-lambda-p
                             expected-error-type-p
                             expected-error-lambda-p))
           1)
    (error "multiple EXPECTED-FOO arguments"))
  (when expected-error-lambda
    (error "stub: expected-error functionality not supported yet"))
  (let ((eval-expected-lambda (eval expected-lambda)))
    (flet ((frob (evaloid)
                 (let ((result (funcall evaloid expr)))
                   (unless (funcall eval-expected-lambda result)
                     (error "failed assertoid ~S" expr))))
           (compile-as-evaloid (optimizations)
             (lambda (expr)
               (funcall (compile nil
                                 `(lambda ()
                                    (declare (optimize ,@optimizations))
                                    ,expr))))))
      (frob #'eval)
      (frob (compile-as-evaloid ()))
      (dolist (i extra-optimizations)
        (frob (compile-as-evaloid i))))))

;;; examples
(assertoid (= 2 (length (list 1 2))))
(assertoid (= 2 (length (list 1 2)))
           :extra-optimizations (((speed 2) (space 3))
                                 ((speed 1) (space 3))))
(assertoid (cons 1 2)
           :expected-lambda (lambda (x) (equal x '(1 . 2))))
(assertoid (cons (list 1 2) (list 1 2))
           :expected-equal '((1 2) 1 2))
;;; not implemented yet:
#+nil (assertoid (length (eval (find-package :cl)))
                 :expected-error-type 'type-error)

(defmacro is (form)
  (if (consp form)
      (destructuring-bind (op expected real) form
        `(let ((expected-value ,expected)
               (real-value ,real))
           (unless (,op expected-value real-value)
             (error "Wanted ~S, got ~S:~% ~S"
                    expected-value real-value ',form))))
      `(unless ,form
         (error "~S evaluated to NIL" ',form))))
