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

(define-method-combination long-or ()
  ((primary () :required t))
  `(or ,@(reverse (mapcar (lambda (m) `(call-method ,m)) primary))))

(defgeneric long-or-test (x)
  (:method ((x fixnum)) 'fixnum)
  (:method ((x integer)) 'integer)
  (:method ((x number)) 'number))

(with-test (:name (:method-combination standard))
  (assert (eql (long-or-test 3) 'fixnum))
  (assert (eql (long-or-test (1+ most-positive-fixnum)) 'integer))
  (assert (eql (long-or-test 3.2) 'number)))

(defgeneric long-or-test (x)
  (:method ((x fixnum)) 'fixnum)
  (:method ((x integer)) 'integer)
  (:method ((x number)) 'number)
  (:method-combination long-or))

(with-test (:name (:method-combination :long-or-reverse))
  (assert (eql (long-or-test 3) 'number))
  (assert (eql (long-or-test (1+ most-positive-fixnum)) 'number))
  (assert (eql (long-or-test 3.2) 'number)))

(define-method-combination long-or ()
  ((primary () :required t))
  `(or ,@(mapcar (lambda (m) `(call-method ,m)) primary)))

(with-test (:name (:method-combination :long-or))
  (assert (eql (long-or-test 3) 'fixnum))
  (assert (eql (long-or-test (1+ most-positive-fixnum)) 'integer))
  (assert (eql (long-or-test 3.2) 'number)))
