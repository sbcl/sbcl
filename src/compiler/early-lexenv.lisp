;;;; This file contains early compiler-related structure definitions.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(defvar *handled-conditions* nil)
(defvar *disabled-package-locks* nil)

;;; The LEXENV represents the lexical environment used for IR1 conversion.
;;; (This is also what shows up as an ENVIRONMENT value in macroexpansion.)
#!-sb-fluid (declaim (inline internal-make-lexenv)) ; only called in one place
(defstruct (lexenv
             #-no-ansi-print-object
             (:print-function
              (lambda (lexenv stream depth)
                (if (null-lexenv-p lexenv)
                    (print-unreadable-object (lexenv stream)
                      (write-string "NULL-LEXENV" stream))
                    (default-structure-print lexenv stream depth))))
             (:constructor make-null-lexenv ())
             (:constructor internal-make-lexenv
                           (funs vars blocks tags
                            type-restrictions
                            lambda cleanup handled-conditions
                            disabled-package-locks %policy user-data
                            parent)))
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
  ;; continuation to exit to, and <entry> is the corresponding ENTRY
  ;; node.
  (blocks nil :type list)
  (tags nil :type list)
  ;; an alist (THING . CTYPE) which is used to keep track of
  ;; "pervasive" type declarations. When THING is a leaf, this is for
  ;; type declarations that pertain to the type in a syntactic extent
  ;; which does not correspond to a binding of the affected name.
  (type-restrictions nil :type list)
  ;; the lexically enclosing lambda, if any
  ;;
  ;; FIXME: This should be :TYPE (OR CLAMBDA NULL), but it was too hard
  ;; to get CLAMBDA defined in time for the cross-compiler.
  (lambda nil)
  ;; the lexically enclosing cleanup, or NIL if none enclosing within LAMBDA
  (cleanup nil)
  ;; condition types we handle with a handler around the compiler
  (handled-conditions *handled-conditions*)
  ;; lexically disabled package locks (list of symbols)
  (disabled-package-locks *disabled-package-locks*)
  ;; the current OPTIMIZE policy. this is null in the null environment,
  ;; and the global policy is stored in *POLICY*. (Because we want to
  ;; be able to affect it from :WITH-COMPILATION-UNIT.) NIL here also
  ;; works as a convenient null-lexenv identifier.
  (%policy nil :type (or null policy))
  ;; A list associating extra user info to symbols.  The entries
  ;; are of the form (:declare name . value),
  ;; (:variable name key . value), or (:function name key . value)
  (user-data nil :type list)
  parent)

;;; the lexical environment we are currently converting in
(defvar *lexenv*)
(declaim (type lexenv *lexenv*))

;;; an object suitable for input to standard functions that accept
;;; "environment objects" (of the ANSI glossary)
(deftype lexenv-designator () '(or lexenv null))

(defun lexenv-policy (lexenv)
  (or (lexenv-%policy lexenv) *policy*))

(defun null-lexenv-p (lexenv)
  (not (lexenv-%policy lexenv)))
