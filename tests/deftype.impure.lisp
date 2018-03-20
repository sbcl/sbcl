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

;;; atom body
(deftype deftype.atom-body () t)
(with-test (:name (deftype atom :body))
  (assert (subtypep 'deftype.atom-body t))
  (assert (subtypep t 'deftype.atom-body)))

;; Ensure that DEFTYPE can successfully replace a DEFSTRUCT type
;; definition.
(defstruct foo)
(assert (progn (deftype foo () 'integer)
               (null (find-class 'foo nil))
               t))

;; Ensure that DEFCLASS after DEFTYPE nukes the lambda-list.
(defun get-deftype-lambda-list (symbol)
  (let ((expander (sb-int:info :type :expander symbol)))
    (and (functionp expander)
         (sb-kernel:%fun-lambda-list expander))))
(deftype bar (x) `(integer ,x))
(assert (equal '(x) (get-deftype-lambda-list 'bar)))
(defclass bar () ())
(assert (not (get-deftype-lambda-list 'bar)))

;; Need to work with plain symbols as the body.
(defconstant whatever 't)
(deftype anything () whatever)
(assert (typep 42 'anything))

(with-test (:name :deftype-not-list-lambda-list)
  (assert-error (eval `(deftype ,(gensym) non-list-argument))))
