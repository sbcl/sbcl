;;;; tests for problems in the interface presented to the user/programmer

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

;;;; properties of symbols, e.g. presence of doc strings for public symbols

;;; FIXME: It would probably be good to require here that every
;;; external symbol either has a doc string or has some good excuse
;;; (like being an accessor for a structure which has a doc string).

;;;; tests of interface machinery

;;; APROPOS should accept a package designator, not just a package, and
;;; furthermore do the right thing when it gets a package designator.
;;; (bug reported and fixed by Alexey Dejneka sbcl-devel 2001-10-17)
(assert (< 0
	   (length (apropos-list "PRINT" :cl))
	   (length (apropos-list "PRINT"))))

;;; DESCRIBE shouldn't fail on rank-0 arrays (bug reported and fixed
;;; by Lutz Euler sbcl-devel 2002-12-03)
(describe #0a0)
(describe #(1 2 3))
(describe #2a((1 2) (3 4)))

;;; TYPEP, SUBTYPEP, UPGRADED-ARRAY-ELEMENT-TYPE and
;;; UPGRADED-COMPLEX-PART-TYPE should be able to deal with NIL as an
;;; environment argument
(typep 1 'fixnum nil)
(subtypep 'fixnum 'integer nil)
(upgraded-array-element-type '(mod 5) nil)
(upgraded-complex-part-type '(single-float 0.0 1.0) nil)

;;; We should have documentation for our extension package:
(assert (documentation (find-package "SB-EXT") t))
