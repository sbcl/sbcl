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

(quit :unix-status 104)
