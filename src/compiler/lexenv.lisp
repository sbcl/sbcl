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

;;; support for the idiom (in MACROEXPAND and elsewhere) that NIL is
;;; to be taken as a null lexical environment.
;;; Of course this is a mostly pointless "idiom" because NIL *is*
;;; an environment, as far as most environment inquiry functions care.
(defun coerce-to-lexenv (x)
  (etypecase x
    (null (make-null-lexenv))
    (lexenv x)
    #!+(and sb-fasteval (host-feature sb-xc))
    (sb!interpreter:basic-env (sb!interpreter:lexenv-from-env x))))

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

;;; Return a sexpr for LAMBDA in LEXENV such that loading it from fasl
;;; preserves the original lexical environment for inlining.
;;; Return NIL if the lexical environment is too complicated.
(defun maybe-inline-syntactic-closure (lambda lexenv)
  (declare (type list lambda) (type lexenv-designator lexenv))
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
  (typecase lexenv
   (lexenv
    (let ((vars (lexenv-vars lexenv))
          (funs (lexenv-funs lexenv)))
      (acond ((or (lexenv-blocks lexenv) (lexenv-tags lexenv)) nil)
             ((and (null vars) (null funs)) lambda)
             ;; If the lexenv is too hairy for cross-compilation,
             ;; you'll find out later, when trying to perform inlining.
             ;; This is fine, because if the inline expansion is only
             ;; for the target, it's totally OK to cross-compile this
             ;; defining form. The syntactic env is correctly captured.
             ((reconstruct-lexenv lexenv)
              `(lambda-with-lexenv ,it ,@(cdr lambda))))))
   #!+(and sb-fasteval (host-feature sb-xc))
   (sb!interpreter:basic-env
    (awhen (sb!interpreter::reconstruct-syntactic-closure-env lexenv)
      `(lambda-with-lexenv ,it ,@(cdr lambda))))
   #!+sb-fasteval
   (null lambda))) ; trivial case. Never occurs in the compiler.
