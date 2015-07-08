;;;; DEF!MACRO = cold DEFMACRO, a version of DEFMACRO which at
;;;; build-the-cross-compiler time defines its macro both in the
;;;; cross-compilation host Lisp and in the target Lisp. Basically,
;;;; DEF!MACRO does something like
;;;;   (DEFMACRO SB!XC:FOO (,@ARGS) (FOO-EXPANDER ,@ARGS))
;;;;   #+SB-XC-HOST (SB!XC:DEFMACRO FOO (,@ARGS) (FOO-EXPANDER ,@ARGS))
;;;; an idiom which would otherwise be handwritten repeatedly.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

#+sb-xc-host
(progn
  ;; a description of the DEF!MACRO call to be stored until we get enough
  ;; of the system running to finish processing it
  (defstruct (delayed-def!macro (:constructor make-delayed-def!macro (args)))
    (args nil :type cons)
    (package (sane-package) :type package))
  ;; a list of DELAYED-DEF!MACROs stored until we get DEF!MACRO working fully
  ;; so that we can apply it to them. After DEF!MACRO is made to work, this
  ;; list is processed, and then should no longer be used; it's made unbound in
  ;; hopes of discouraging any attempt to pushing anything more onto it.
  ;; (DEF!MACRO knows about this behavior, and uses the unboundness of
  ;; *DELAYED-DEF!MACROS* as a way to decide to just call SB!XC:DEFMACRO
  ;; instead of pushing onto *DELAYED-DEF!MACROS*.)
  (defvar *delayed-def!macros* nil))

;;; KLUDGE: This is unfortunately somewhat tricky. (A lot of the
;;; cross-compilation-unfriendliness of Common Lisp comes home to roost here.)
(defmacro def!macro (name &rest rest)
  #-(or sb-xc-host sb-xc) `(defmacro ,name ,@rest)
  #+sb-xc-host `(progn
                  (defmacro ,name ,@rest)
                  ,(let ((uncrossed-args `(,(uncross name) ,@rest)))
                     (if (boundp '*delayed-def!macros*)
                         `(push (make-delayed-def!macro ',uncrossed-args)
                                *delayed-def!macros*)
                         `(sb!xc:defmacro ,@uncrossed-args))))
  ;; When cross-compiling, we don't want the DEF!MACRO to have any
  ;; effect at compile time, because (1) we already defined the macro
  ;; when building the cross-compiler, so at best it would be redundant
  ;; and inefficient to replace the current compiled macro body with
  ;; an interpreted macro body, and (2) because of the various games
  ;; with SB!XC vs. CL which are played when cross-compiling, we'd
  ;; be at risk of making an incorrect definition, with something which
  ;; should be e.g. calling SB!XC:TYPEP instead calling CL:TYPEP
  ;; and getting all confused. Using an ordinary assignment (and not
  ;; any special forms like DEFMACRO) guarantees that there are no
  ;; effects at compile time.
  #+sb-xc `(defmacro-mundanely ,name ,@rest))

#+sb-xc-host
(defun force-delayed-def!macros ()
  (if (boundp '*delayed-def!macros*)
    (progn
      (mapcar (lambda (x)
                (let ((*package* (delayed-def!macro-package x)))
                  (eval `(sb!xc:defmacro ,@(delayed-def!macro-args x)))))
              (reverse *delayed-def!macros*))
      ;; We shouldn't need this list any more. Making it unbound serves as a
      ;; signal to DEF!MACRO that it needn't delayed DEF!MACROs any more.
      ;; It is also generally a good thing for other reasons: it frees
      ;; garbage, and it discourages anyone else from pushing anything else
      ;; onto the list later.
      (makunbound '*delayed-def!macros*))
    ;; This condition is probably harmless if it comes up when
    ;; interactively experimenting with the system by loading a source
    ;; file into it more than once. But it's worth warning about it
    ;; because it definitely shouldn't come up in an ordinary build
    ;; process.
    (warn "*DELAYED-DEF!MACROS* is already unbound.")))
