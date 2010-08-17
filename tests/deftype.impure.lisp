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

(load "assertoid.lisp")
(use-package "ASSERTOID")

;;; Check for correct defaulting of unsupplied parameters to *
(deftype opt (&optional arg)
  `(integer 0 ,arg))
(deftype opt-singleton (&optional (arg))
  `(integer 0 ,arg))
(deftype key (&key arg)
  `(integer 0 ,arg))
(deftype key-singleton (&key (arg))
  `(integer 0 ,arg))

(assert (typep 1 'opt))
(assert (typep 1 'opt-singleton))
(assert (typep 1 'key))
(assert (typep 1 'key-singleton))

;;; empty body
(deftype deftype-with-empty-body ())
(assert (subtypep 'deftype-with-empty-body nil))
(assert (subtypep nil 'deftype-with-empty-body))

;; Ensure that DEFTYPE can successfully replace a DEFSTRUCT type
;; definition.
(defstruct foo)
(assert (progn (deftype foo () 'integer)
               (null (find-class 'foo nil))
               t))

;; Ensure that DEFCLASS after DEFTYPE nukes the lambda-list.
(deftype bar (x) `(integer ,x))
(assert (equal '(x) (sb-int:info :type :lambda-list 'bar)))
(defclass bar () ())
(assert (not (sb-int:info :type :lambda-list 'bar)))

;; Need to work with plain symbols as the body.
(defconstant whatever 't)
(deftype anything () whatever)
(assert (typep 42 'anything))

(with-test (:name :deftype-not-list-lambda-list)
  (assert (raises-error? (eval `(deftype ,(gensym) non-list-argument)))))
