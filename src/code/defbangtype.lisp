;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;;;; the DEF!TYPE macro

;;; DEF!TYPE = cold DEFTYPE, a version of DEFTYPE which at
;;; build-the-cross-compiler time defines its macro both in the
;;; cross-compilation host Lisp and in the target Lisp. Basically,
;;; DEF!TYPE does something like
;;;   (DEFTYPE SB-XC:FOO ..)
;;;   #+SB-XC-HOST (SB-XC:DEFTYPE FOO ..)
;;; except that it also automatically delays the SB-XC:DEFTYPE call,
;;; if necessary, until the cross-compiler's DEFTYPE machinery has been
;;; set up.

(defvar *delayed-def!types* nil)

(defmacro def!type (name &rest rest)
  ;; Attempting to define a type named by a CL symbol is an error.
  ;; Therefore NAME is wrong if it uncrosses to something other than itself.
  (assert (eq (uncross name) name))
  `(progn
     (deftype ,name ,@rest)
     #+sb-xc-host
     ,(let ((form `(sb-xc:deftype ,name ,@rest)))
        (if (boundp '*delayed-def!types*)
            `(push ',form *delayed-def!types*)
            form))))
