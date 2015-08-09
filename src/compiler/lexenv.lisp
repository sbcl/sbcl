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

(defvar *handled-conditions* nil)
(defvar *disabled-package-locks* nil)

;;; The LEXENV represents the lexical environment used for IR1 conversion.
;;; (This is also what shows up as an ENVIRONMENT value in macroexpansion.)
#!-sb-fluid (declaim (inline internal-make-lexenv)) ; only called in one place
(def!struct (lexenv
             (:print-function print-lexenv)
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
(def!type lexenv-designator () '(or lexenv null))

(defun lexenv-policy (lexenv)
  (or (lexenv-%policy lexenv) *policy*))

(defun null-lexenv-p (lexenv)
  (not (lexenv-%policy lexenv)))

;;; support for the idiom (in MACROEXPAND and elsewhere) that NIL is
;;; to be taken as a null lexical environment
(defun coerce-to-lexenv (x)
  (etypecase x
    (null (make-null-lexenv))
    (lexenv x)))

(defun print-lexenv (lexenv stream level)
  (if (null-lexenv-p lexenv)
      (print-unreadable-object (lexenv stream)
        (write-string "NULL-LEXENV" stream))
      (default-structure-print lexenv stream level)))

;;; Take the lexenv surrounding an inlined function and extract things
;;; needed for the inline expansion suitable for dumping into fasls.
;;; Right now it's MACROLET, SYMBOL-MACROLET, SPECIAL and
;;; INLINE/NOTINLINE declarations. Upon encountering something else return NIL.
;;; This is later used by PROCESS-INLINE-LEXENV to reproduce the lexenv.
;;;
;;; Previously it just used the functions and vars of the innermost
;;; lexenv, but the body of macrolet can refer to other macrolets
;;; defined earlier, so it needs to process all the parent lexenvs to
;;; recover the proper order.
(defun reconstruct-lexenv (lexenv)
  (let (shadowed-funs
        shadowed-vars
        result)
    (loop for env = lexenv then parent
          for parent = (lexenv-parent env)
          for vars = (lexenv-vars env)
          for funs = (lexenv-funs env)
          for declarations = nil
          for symbol-macros = nil
          for macros = nil
          do
          (loop for binding in vars
                for (name . what) = binding
                unless (and parent
                            (find binding (lexenv-vars parent)))
                do (typecase what
                     (cons
                      (aver (eq (car what) 'macro))
                      (push name shadowed-vars)
                      (push (list name (cdr what)) symbol-macros))
                     (global-var
                      (aver (eq (global-var-kind what) :special))
                      (push `(special ,name) declarations))
                     (t
                      (unless (memq name shadowed-vars)
                        (return-from reconstruct-lexenv)))))
          (loop for binding in funs
                for (name . what) = binding
                unless (and parent
                            (find binding (lexenv-funs parent)))
                do
                (typecase what
                  (cons
                   (push name shadowed-funs)
                   (push (cons name (function-lambda-expression (cdr what))) macros))
                  ;; FIXME: Is there a good reason for this not to be
                  ;; DEFINED-FUN (which :INCLUDEs GLOBAL-VAR, in case
                  ;; you're wondering how this ever worked :-)? Maybe
                  ;; in conjunction with an AVERrance that it's not an
                  ;; (AND GLOBAL-VAR (NOT GLOBAL-FUN))? -- CSR,
                  ;; 2002-07-08
                  (global-var
                   (unless (defined-fun-p what)
                     (return-from reconstruct-lexenv))
                   (push `(,(car (rassoc (defined-fun-inlinep what)
                                         *inlinep-translations*))
                           ,name)
                         declarations))
                  (t
                   (unless (memq name shadowed-funs)
                     (return-from reconstruct-lexenv)))))
          (when declarations
            (setf result (list* :declare declarations (and result (list result)))))
          (when symbol-macros
            (setf result (list* :symbol-macro symbol-macros (and result (list result)))))
          (when macros
            (setf result (list* :macro macros (and result (list result)))))
          while (and parent
                     (not (null-lexenv-p parent))))
    result))

(defun maybe-inline-syntactic-closure (lambda lexenv)
  (declare (type list lambda) (type lexenv lexenv))
  (aver (eql (first lambda) 'lambda))
  ;; We used to have a trivial implementation, verifying that lexenv
  ;; was effectively null. However, this fails to take account of the
  ;; idiom
  ;;
  ;; (declaim (inline foo))
  ;; (macrolet ((def (x) `(defun ,x () ...)))
  ;;   (def foo))
  ;;
  ;; which, while too complicated for the cross-compiler to handle in
  ;; unfriendly foreign lisp environments, would be good to support in
  ;; the target compiler. -- CSR, 2002-05-13 and 2002-11-02
  (let ((vars (lexenv-vars lexenv))
        (funs (lexenv-funs lexenv)))
    (cond
      ((or (lexenv-blocks lexenv) (lexenv-tags lexenv)) nil)
      ((and (null vars) (null funs)) lambda)
      ;; too complicated for cross-compilation
      #-sb-xc-host
      (t
       (let ((env (reconstruct-lexenv lexenv)))
         (and env
          `(lambda-with-lexenv ,env ,@(cdr lambda))))))))

