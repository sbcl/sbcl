;;;; cross-compiler-only versions of conditions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(define-condition simple-style-warning (simple-condition style-warning) ())
(define-condition format-too-few-args-warning (simple-warning) ())
;;; in the cross-compiler, this is a full warning.  In the target
;;; compiler, it will only be a style-warning.
(define-condition format-too-many-args-warning (simple-warning) ())
