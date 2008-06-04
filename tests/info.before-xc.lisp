;;;; tests of the INFO compiler database, initially with particular
;;;; reference to knowledge of constants, intended to be executed as
;;;; soon as the cross-compiler is built.

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

(in-package "SB!KERNEL")

(/show "beginning tests/info.before-xc.lisp")

(assert (eq (sb!int:info :variable :kind 'sb!vm:vector-data-offset)
            :constant))
;;; It's possible in general for a constant to have the value NIL, but
;;; not for vector-data-offset, which must be a number:
(assert (boundp 'sb!vm:vector-data-offset))
(assert (integerp (symbol-value 'sb!vm:vector-data-offset)))

(/show "done with tests/info.before-xc.lisp")
