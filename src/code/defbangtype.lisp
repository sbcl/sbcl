;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;;; the DEF!TYPE macro

;;; DEF!TYPE = cold DEFTYPE, a version of DEFTYPE which at
;;; build-the-cross-compiler time defines its macro both in the
;;; cross-compilation host Lisp and in the target Lisp. Basically,
;;; DEF!TYPE does something like
;;;   (DEFTYPE SB!XC:FOO ..)
;;;   #+SB-XC-HOST (SB!XC:DEFTYPE FOO ..)
;;; except that it also automatically delays the SB!XC:DEFTYPE call,
;;; if necessary, until the cross-compiler's DEFTYPE machinery has been
;;; set up.

;;; FIXME: This code was created by cut-and-paste from the
;;; corresponding code for DEF!MACRO. DEF!TYPE and DEF!MACRO are
;;; currently very parallel, and if we ever manage to rationalize the
;;; use of UNCROSS in the cross-compiler, they should become
;;; completely parallel, at which time they should be merged to
;;; eliminate the duplicate code.

(defmacro def!type (name &rest rest)
  `(progn
     (deftype ,name ,@rest)
     #+sb-xc-host
     ,(let ((form `(sb!xc:deftype ,(uncross name) ,@rest)))
        (if (boundp '*delayed-def!types*)
            `(push ',form *delayed-def!types*)
            form))))

;;; machinery to implement DEF!TYPE delays
#+sb-xc-host
(progn
  (/show "binding *DELAYED-DEF!TYPES*")
  (defvar *delayed-def!types* nil)
  (/show "done binding *DELAYED-DEF!TYPES*")
  (defun force-delayed-def!types ()
    (if (boundp '*delayed-def!types*)
        (progn
          (mapc #'eval *delayed-def!types*)
          (makunbound '*delayed-def!types*))
        ;; This condition is probably harmless if it comes up when
        ;; interactively experimenting with the system by loading a
        ;; source file into it more than once. But it's worth warning
        ;; about it because it definitely shouldn't come up in an
        ;; ordinary build process.
        (warn "*DELAYED-DEF!TYPES* is already unbound."))))
