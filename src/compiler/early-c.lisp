;;;; This file contains compiler code and compiler-related stuff which
;;;; can be built early on. Some of the stuff may be here because it's
;;;; needed early on, some other stuff (e.g. constants) just because
;;;; it might as well be done early so we don't have to think about
;;;; whether it's done early enough.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; ANSI limits on compilation
(defconstant sb!xc:call-arguments-limit most-positive-fixnum
  #!+sb-doc
  "The exclusive upper bound on the number of arguments which may be passed
  to a function, including &REST args.")
(defconstant sb!xc:lambda-parameters-limit most-positive-fixnum
  #!+sb-doc
  "The exclusive upper bound on the number of parameters which may be specifed
  in a given lambda list. This is actually the limit on required and &OPTIONAL
  parameters. With &KEY and &AUX you can get more.")
(defconstant sb!xc:multiple-values-limit most-positive-fixnum
  #!+sb-doc
  "The exclusive upper bound on the number of multiple VALUES that you can
  return.")

;;; FIXME: Shouldn't SB!C::&MORE be in this list?
(defconstant-eqx sb!xc:lambda-list-keywords
  '(&optional &rest &key &aux &body &whole &allow-other-keys &environment)
  #!+sb-doc
  #'equal
  "symbols which are magical in a lambda list")

;;;; cross-compiler-only versions of CL special variables, so that we
;;;; don't have weird interactions with the host compiler

(defvar sb!xc:*compile-file-pathname*)
(defvar sb!xc:*compile-file-truename*)
(defvar sb!xc:*compile-print*)
(defvar sb!xc:*compile-verbose*)

;;;; miscellaneous types used both in the cross-compiler and on the target

;;;; FIXME: The INDEX and LAYOUT-DEPTHOID definitions probably belong
;;;; somewhere else, not "early-c", since they're after all not part
;;;; of the compiler.

;;; the type of LAYOUT-DEPTHOID slot values
(def!type sb!kernel::layout-depthoid () '(or index (integer -1 -1)))

;;; possible values for the INLINE-ness of a function.
(deftype inlinep ()
  '(member :inline :maybe-inline :notinline nil))
(defparameter *inlinep-translations*
  '((inline . :inline)
    (notinline . :notinline)
    (maybe-inline . :maybe-inline)))

;;; the lexical environment we are currently converting in
(defvar *lexenv*)
(declaim (type lexenv *lexenv*))

;;; *FREE-VARIABLES* translates from the names of variables referenced
;;; globally to the LEAF structures for them. *FREE-FUNCTIONS* is like
;;; *FREE-VARIABLES*, only it deals with function names.
(defvar *free-variables*)
(defvar *free-functions*)
(declaim (type hash-table *free-variables* *free-functions*))

;;; We use the same CONSTANT structure to represent all equal anonymous
;;; constants. This hashtable translates from constants to the LEAFs that
;;; represent them.
(defvar *constants*)
(declaim (type hash-table *constants*))

;;; miscellaneous forward declarations
(defvar *code-segment*)
#!+sb-dyncount (defvar *collect-dynamic-statistics*)
(defvar *component-being-compiled*)
(defvar *compiler-error-context*)
(defvar *compiler-error-count*)
(defvar *compiler-warning-count*)
(defvar *compiler-style-warning-count*)
(defvar *compiler-note-count*)
(defvar *compiler-trace-output*)
(defvar *constraint-number*)
(defvar *count-vop-usages*)
(defvar *current-path*)
(defvar *current-component*)
(defvar *delayed-ir1-transforms*)
(defvar *policy*)
(defvar *dynamic-counts-tn*)
(defvar *elsewhere*)
(defvar *event-info*)
(defvar *event-note-threshold*)
(defvar *failure-p*)
(defvar *fixups*)
(defvar *in-pack*)
(defvar *info-environment*)
(defvar *lexenv*)
(defvar *source-info*)
(defvar *trace-table*)
(defvar *undefined-warnings*)
(defvar *warnings-p*)

;;;; miscellaneous utilities

;;; Delete any undefined warnings for NAME and KIND. This is for the
;;; benefit of the compiler, but it's sometimes called from stuff like
;;; type-defining code which isn't logically part of the compiler.
(declaim (ftype (function ((or symbol cons) keyword) (values))
		note-name-defined))
(defun note-name-defined (name kind)
  ;; We do this BOUNDP check because this function can be called when
  ;; not in a compilation unit (as when loading top-level forms).
  (when (boundp '*undefined-warnings*)
    (setq *undefined-warnings*
	  (delete-if (lambda (x)
		       (and (equal (undefined-warning-name x) name)
			    (eq (undefined-warning-kind x) kind)))
		     *undefined-warnings*)))
  (values))

;;; to be called when a variable is lexically bound
(declaim (ftype (function (symbol) (values)) note-lexical-binding))
(defun note-lexical-binding (symbol)
    ;; This check is intended to protect us from getting silently
    ;; burned when we define
    ;;   foo.lisp:
    ;;     (DEFVAR *FOO* -3)
    ;;     (DEFUN FOO (X) (+ X *FOO*))
    ;;   bar.lisp:
    ;;     (DEFUN BAR (X)
    ;;       (LET ((*FOO* X))
    ;;         (FOO 14)))
    ;; and then we happen to compile bar.lisp before foo.lisp.
  (when (looks-like-name-of-special-var-p symbol)
      ;; FIXME: should be COMPILER-STYLE-WARNING?
      (style-warn "using the lexical binding of the symbol ~S, not the~@
dynamic binding, even though the symbol name follows the usual naming~@
convention (names like *FOO*) for special variables" symbol))
  (values))
