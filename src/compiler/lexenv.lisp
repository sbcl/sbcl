;;;; the representation of a lexical environment

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

#!-sb-fluid (declaim (inline internal-make-lexenv)) ; only called in one place

;;; The LEXENV represents the lexical environment used for IR1 conversion.
;;; (This is also what shows up as an ENVIRONMENT value in macroexpansion.)
#!-sb-fluid (declaim (inline internal-make-lexenv)) ; only called in one place
(def!struct (lexenv
	     (:constructor make-null-lexenv ())
	     (:constructor make-null-interactive-lexenv
			   (&aux (policy (list '(safety . 3)
					       '(compilation-speed . 2)
					       '(speed . 1)
					       '(space . 1)
					       '(debug . 1)
					       '(inhibit-warnings . 1)))))
	     (:constructor internal-make-lexenv
			   (funs vars blocks tags type-restrictions
				 lambda cleanup policy)))
  ;; an alist of (NAME . WHAT), where WHAT is either a FUNCTIONAL (a
  ;; local function), a DEFINED-FUN, representing an
  ;; INLINE/NOTINLINE declaration, or a list (MACRO . <function>) (a
  ;; local macro, with the specifier expander). Note that NAME may be
  ;; a (SETF <name>) list, not necessarily a single symbol.
  (funs nil :type list)
  ;; an alist translating variable names to LEAF structures. A special
  ;; binding is indicated by a :SPECIAL GLOBAL-VAR leaf. Each special
  ;; binding within the code gets a distinct leaf structure, as does
  ;; the current "global" value on entry to the code compiled.
  ;; (locally (special ...)) is handled by adding the most recent
  ;; special binding to the front of the list.
  ;;
  ;; If the CDR is (MACRO . <exp>), then <exp> is the expansion of a
  ;; symbol macro.
  (vars nil :type list)
  ;; BLOCKS and TAGS are alists from block and go-tag names to 2-lists
  ;; of the form (<entry> <continuation>), where <continuation> is the
  ;; continuation to exit to, and <entry> is the corresponding ENTRY node.
  (blocks nil :type list)
  (tags nil :type list)
  ;; an alist (THING . CTYPE) which is used to keep track of
  ;; "pervasive" type declarations. When THING is a leaf, this is for
  ;; type declarations that pertain to the type in a syntactic extent
  ;; which does not correspond to a binding of the affected name. When
  ;; THING is a continuation, this is used to track the innermost THE
  ;; type declaration.
  (type-restrictions nil :type list)
  ;; the lexically enclosing lambda, if any
  ;;
  ;; FIXME: This should be :TYPE (OR CLAMBDA NULL), but it was too hard
  ;; to get CLAMBDA defined in time for the cross-compiler.
  (lambda nil)
  ;; the lexically enclosing cleanup, or NIL if none enclosing within Lambda
  (cleanup nil)
  ;; the current OPTIMIZE policy
  (policy *policy* :type policy))

;;; support for the idiom (in MACROEXPAND and elsewhere) that NIL is
;;; to be taken as a null lexical environment
(defun coerce-to-lexenv (x)
  (etypecase x
    (null (make-null-lexenv))
    (lexenv x)))

;;; Is it safe to just grab the lambda expression LAMBDA in isolation,
;;; ignoring the LEXENV?
;;;
;;; Note: The corresponding CMU CL code did something hairier so that
;;; it could save inline definitions of DEFUNs in nontrivial lexical
;;; environments. If it's ever important to try to do that, take a
;;; look at the old CMU CL #'INLINE-SYNTACTIC-CLOSURE.
(defun lambda-independent-of-lexenv-p (lambda lexenv)
  (declare (type list lambda) (type lexenv lexenv))
  (aver (eql (first lambda) 'lambda)) ; basic sanity check
  ;; This is a trivial implementation that just makes sure that LEXENV
  ;; doesn't have anything interesting in it. A more sophisticated
  ;; implementation could skip things in LEXENV which aren't captured
  ;; by LAMBDA, but this implementation doesn't try.
  (and (null (lexenv-blocks lexenv))
       (null (lexenv-tags lexenv))
       (null (lexenv-vars lexenv))
       (null (lexenv-funs lexenv))))
