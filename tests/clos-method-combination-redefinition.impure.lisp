;;;; testing method combination redefinition

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

(defpackage "CLOS-METHOD-COMBINATION-REDEFINITION"
  (:use "COMMON-LISP" "TEST-UTIL"))

(in-package "CLOS-METHOD-COMBINATION-REDEFINITION")

;;;; long-form method combination redefinition

;;; first, define a method combination
(define-method-combination long-or ()
  ((primary () :required t))
  `(or ,@(reverse (mapcar (lambda (m) `(call-method ,m)) primary))))

;;; GF has standard method combination
(defgeneric long-or-test (x)
  (:method ((x fixnum)) 'fixnum)
  (:method ((x integer)) 'integer)
  (:method ((x number)) 'number))

(with-test (:name (:method-combination standard))
  (assert (eql (long-or-test 3) 'fixnum))
  (assert (eql (long-or-test (1+ most-positive-fixnum)) 'integer))
  (assert (eql (long-or-test 3.2) 'number)))

;;; add the method combination
(defgeneric long-or-test (x)
  (:method ((x fixnum)) 'fixnum)
  (:method ((x integer)) 'integer)
  (:method ((x number)) 'number)
  (:method-combination long-or))

(with-test (:name (:method-combination :long-or-reverse))
  (assert (eql (long-or-test 3) 'number))
  (assert (eql (long-or-test (1+ most-positive-fixnum)) 'number))
  (assert (eql (long-or-test 3.2) 'number)))

;;; redefine the method combination
(define-method-combination long-or ()
  ((primary () :required t))
  `(or ,@(mapcar (lambda (m) `(call-method ,m)) primary)))

(with-test (:name (:method-combination :long-or))
  (assert (eql (long-or-test 3) 'fixnum))
  (assert (eql (long-or-test (1+ most-positive-fixnum)) 'integer))
  (assert (eql (long-or-test 3.2) 'number)))

;;;; short-form method-combination redefiniton

;;; define a method-combination
(define-method-combination div :operator /)

(defgeneric short-div (x)
  (:method-combination div :most-specific-first)
  (:method div ((x number)) 4)
  (:method div ((x fixnum)) 8))

(with-test (:name (:method-combination :short-div))
  (assert (= (short-div 3) 2))
  (assert (= (short-div 3.0) 1/4)))

;;; check that changing method-combination options works
(defgeneric short-div (x)
  (:method-combination div :most-specific-last)
  (:method div ((x number)) 4)
  (:method div ((x fixnum)) 8))

(with-test (:name (:method-combination :short-div :most-specific-last))
  (assert (= (short-div 3) 1/2))
  (assert (= (short-div 3.0) 1/4)))

;;; check that choosing new short-form options works
(define-method-combination div :operator / :identity-with-one-argument t)

(with-test (:name (:method-combination :short-div :identity-with-one-argument))
  (assert (= (short-div 3) 1/2))
  (assert (= (short-div 3.0) 4)))

;;; check that changing method-combination options works (deletion of :most-specific-last)
(defgeneric short-div (x)
  (:method-combination div)
  (:method div ((x number)) 4)
  (:method div ((x fixnum)) 8))

(with-test (:name (:method-combination :short-div :identity-with-one-argument :most-specific-first))
  (assert (= (short-div 3) 2))
  (assert (= (short-div 3.0) 4)))

;;; check that changing operator works
(define-method-combination div :operator -)

(with-test (:name (:method-combination :short-div :operator -))
  (assert (= (short-div 3) 4))
  (assert (= (short-div 3.0) -4)))

;;; TODO
;;;
;;; 1. redefinition of long-form including change to args-lambda-list
;;;
;;; 3. redefinition to update documentation and source location
;;;
;;; 4. (possibly) move long-method-combination-function into method
;;;    combination objects, in which case need to check that existing
;;;    method combination objects are updated (currently the method
;;;    combination function lives in a global hash table and that is
;;;    updated at redefinition time).
;;;
;;; 5. redefinition between short- and long-form method combinations

