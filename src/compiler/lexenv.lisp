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
	     ;; FIXME: should probably be called MAKE-EMPTY-LEXENV or
	     ;; MAKE-NULL-LEXENV
	     (:constructor make-null-lexenv ())
	     (:constructor internal-make-lexenv
			   (functions variables blocks tags type-restrictions
				      lambda cleanup cookie
				      interface-cookie options)))
  ;; Alist (name . what), where What is either a Functional (a local function),
  ;; a DEFINED-FUNCTION, representing an INLINE/NOTINLINE declaration, or
  ;; a list (MACRO . <function>) (a local macro, with the specifier
  ;; expander.)    Note that Name may be a (SETF <name>) function.
  (functions nil :type list)
  ;; An alist translating variable names to Leaf structures. A special binding
  ;; is indicated by a :Special Global-Var leaf. Each special binding within
  ;; the code gets a distinct leaf structure, as does the current "global"
  ;; value on entry to the code compiled. (locally (special ...)) is handled
  ;; by adding the most recent special binding to the front of the list.
  ;;
  ;; If the CDR is (MACRO . <exp>), then <exp> is the expansion of a symbol
  ;; macro.
  (variables nil :type list)
  ;; Blocks and Tags are alists from block and go-tag names to 2-lists of the
  ;; form (<entry> <continuation>), where <continuation> is the continuation to
  ;; exit to, and <entry> is the corresponding Entry node.
  (blocks nil :type list)
  (tags nil :type list)
  ;; An alist (Thing . CType) which is used to keep track of "pervasive" type
  ;; declarations. When Thing is a leaf, this is for type declarations that
  ;; pertain to the type in a syntactic extent which does not correspond to a
  ;; binding of the affected name. When Thing is a continuation, this is used
  ;; to track the innermost THE type declaration.
  (type-restrictions nil :type list)
  ;; The lexically enclosing lambda, if any.
  ;; 
  ;; FIXME: This should be :TYPE (OR CLAMBDA NULL), but it was too hard
  ;; to get CLAMBDA defined in time for the cross-compiler.
  (lambda nil) 
  ;; The lexically enclosing cleanup, or NIL if none enclosing within Lambda.
  ;;
  ;; FIXME: This should be :TYPE (OR CLEANUP NULL), but it was too hard
  ;; to get CLEANUP defined in time for the cross-compiler.
  (cleanup nil)
  ;; The representation of the current OPTIMIZE policy.
  (cookie *default-cookie* :type cookie)
  ;; The policy that takes effect in XEPs and related syntax parsing functions.
  ;; Slots in this cookie may be null to indicate that the normal value in
  ;; effect.
  (interface-cookie *default-interface-cookie* :type cookie)
  ;; an alist of miscellaneous options that are associated with the lexical
  ;; environment
  (options nil :type list))
