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

;;; FIXME: shouldn't SB-C::&MORE be in this list?
(defconstant sb!xc:lambda-list-keywords
  '(&optional &rest &key &aux &body &whole &allow-other-keys &environment)
  #!+sb-doc
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

;;; a value for an optimization declaration
(def!type sb!c::cookie-quality () '(or (rational 0 3) null))

;;; A COOKIE holds information about the compilation environment for a
;;; node. See the LEXENV definition for a description of how it is
;;; used.
(def!struct (cookie (:copier nil))
  (speed   nil :type cookie-quality)
  (space   nil :type cookie-quality)
  (safety  nil :type cookie-quality)
  (cspeed  nil :type cookie-quality)
  (brevity nil :type cookie-quality)
  (debug   nil :type cookie-quality))

;;; KLUDGE: This needs to be executable in cold init toplevel forms, earlier
;;; than the default copier closure created by DEFSTRUCT toplevel forms would
;;; be available, and earlier than LAYOUT-INFO is initialized (which is a
;;; prerequisite for COPY-STRUCTURE to work), so we define it explicitly using
;;; DEFUN, so that it can be installed by the cold loader, and using
;;; hand-written, hand-maintained slot-by-slot copy it doesn't need to call
;;; COPY-STRUCTURE. -- WHN 19991019
(defun copy-cookie (cookie)
  (make-cookie :speed   (cookie-speed   cookie)
	       :space   (cookie-space   cookie)
	       :safety  (cookie-safety  cookie)
	       :cspeed  (cookie-cspeed  cookie)
	       :brevity (cookie-brevity cookie)
	       :debug   (cookie-debug   cookie)))

;;; *DEFAULT-COOKIE* holds the current global compiler policy information.
;;; Whenever the policy is changed, we copy the structure so that old uses will
;;; still get the old values. *DEFAULT-INTERFACE-COOKIE* holds any values
;;; specified by an OPTIMIZE-INTERFACE declaration.
;;;
;;; FIXME: Why isn't COOKIE called POLICY?
(declaim (type cookie *default-cookie* *default-interface-cookie*))
(defvar *default-cookie*)	   ; initialized in cold init
(defvar *default-interface-cookie*) ; initialized in cold init

;;; possible values for the INLINE-ness of a function.
(deftype inlinep ()
  '(member :inline :maybe-inline :notinline nil))
(defconstant inlinep-translations
  '((inline . :inline)
    (notinline . :notinline)
    (maybe-inline . :maybe-inline)))

;;; The lexical environment we are currently converting in.
(defvar *lexenv*)
(declaim (type lexenv *lexenv*))

;;; *FREE-VARIABLES* translates from the names of variables referenced
;;; globally to the LEAF structures for them. *FREE-FUNCTIONS* is like
;;; *FREE-VARIABLES*, only it deals with function names.
(defvar *free-variables*)
(defvar *free-functions*)
(declaim (hash-table *free-variables* *free-functions*))

;;; We use the same Constant structure to represent all equal anonymous
;;; constants. This hashtable translates from constants to the Leafs that
;;; represent them.
(defvar *constants*)
(declaim (hash-table *constants*))

;;; miscellaneous forward declarations
(defvar *code-segment*)
#!+sb-dyncount (defvar *collect-dynamic-statistics*)
(defvar *component-being-compiled*)
(defvar *compiler-error-context*)
(defvar *compiler-error-count*)
(defvar *compiler-warning-count*)
(defvar *compiler-style-warning-count*)
(defvar *compiler-note-count*)
(defvar *converting-for-interpreter*)
(defvar *count-vop-usages*)
(defvar *current-path*)
(defvar *current-component*)
(defvar *default-cookie*)
(defvar *default-interface-cookie*)
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
  (let ((name (symbol-name symbol)))
    ;; This check is intended to protect us from getting silently burned when
    ;; we define
    ;;   foo.lisp:
    ;;     (DEFVAR *FOO*)
    ;;     (DEFUN FOO (X) (1+ X *FOO*))
    ;;   bar.lisp:
    ;;     (DEFUN BAR (X)
    ;;       (LET ((*FOO* X))
    ;;         (FOO 14)))
    ;; and then we happen to compile bar.lisp before foo.lisp.
    (when (and (char= #\* (aref name 0))
	       (char= #\* (aref name (1- (length name)))))
      (style-warn "using the lexical binding of the symbol ~S, not the~@
dynamic binding, even though the symbol name follows the usual naming~@
convention (names like *FOO*) for special variables" symbol)))
  (values))
