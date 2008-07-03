;;;; functions from classic CMU CL src/compiler/main.lisp which are
;;;; needed only (and which may make sense only) on the
;;;; cross-compilation target, not the cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; CL:COMPILE

(defun get-lambda-to-compile (definition-designator)
  (if (consp definition-designator)
      definition-designator
      (multiple-value-bind (definition env-p)
                           (function-lambda-expression definition-designator)
        (when env-p
          (error "~S was defined in a non-null environment."
                 definition-designator))
        (unless definition
          (error "can't find a definition for ~S" definition-designator))
        definition)))

;;; Handle the nontrivial case of CL:COMPILE.
(defun actually-compile (name definition *lexenv*)
  (with-compilation-values
    (sb!xc:with-compilation-unit ()
      ;; FIXME: These bindings were copied from SUB-COMPILE-FILE with
      ;; few changes. Once things are stable, the shared bindings
      ;; probably be merged back together into some shared utility
      ;; macro, or perhaps both merged into one of the existing utility
      ;; macros SB-C::WITH-COMPILATION-VALUES or
      ;; CL:WITH-COMPILATION-UNIT.
      (let* (;; FIXME: Do we need the *INFO-ENVIRONMENT* rebinding
             ;; here? It's a literal translation of the old CMU CL
             ;; rebinding to (OR *BACKEND-INFO-ENVIRONMENT*
             ;; *INFO-ENVIRONMENT*), and it's not obvious whether the
             ;; rebinding to itself is needed now that SBCL doesn't
             ;; need *BACKEND-INFO-ENVIRONMENT*.
             (*info-environment* *info-environment*)
             (form (get-lambda-to-compile definition))
             (*source-info* (make-lisp-source-info form :parent *source-info*))
             (*toplevel-lambdas* ())
             (*block-compile* nil)
             (*allow-instrumenting* nil)
             (*code-coverage-records* nil)
             (*code-coverage-blocks* nil)
             (*compiler-error-bailout*
              (lambda (&optional error)
                (declare (ignore error))
                (compiler-mumble
                 "~2&fatal error, aborting compilation~%")
                (return-from actually-compile (values nil t nil))))
             (*current-path* nil)
             (*last-source-context* nil)
             (*last-original-source* nil)
             (*last-source-form* nil)
             (*last-format-string* nil)
             (*last-format-args* nil)
             (*last-message-count* 0)
             (*last-error-context* nil)
             (*gensym-counter* 0)
             ;; KLUDGE: This rebinding of policy is necessary so that
             ;; forms such as LOCALLY at the REPL actually extend the
             ;; compilation policy correctly.  However, there is an
             ;; invariant that is potentially violated: future
             ;; refactoring must not allow this to be done in the file
             ;; compiler.  At the moment we're clearly alright, as we
             ;; call %COMPILE with a core-object, not a fasl-stream,
             ;; but caveat future maintainers. -- CSR, 2002-10-27
             (*policy* (lexenv-policy *lexenv*))
             ;; see above
             (*handled-conditions* (lexenv-handled-conditions *lexenv*))
             ;; ditto
             (*disabled-package-locks* (lexenv-disabled-package-locks *lexenv*))
             ;; FIXME: ANSI doesn't say anything about CL:COMPILE
             ;; interacting with these variables, so we shouldn't. As
             ;; of SBCL 0.6.7, COMPILE-FILE controls its verbosity by
             ;; binding these variables, so as a quick hack we do so
             ;; too. But a proper implementation would have verbosity
             ;; controlled by function arguments and lexical variables.
             (*compile-verbose* nil)
             (*compile-print* nil))
        (handler-bind (((satisfies handle-condition-p) #'handle-condition-handler))
          (clear-stuff)
          (find-source-paths form 0)
          (%compile form (make-core-object)
                    :name name
                    :path '(original-source-start 0 0)))))))

(defun compile-in-lexenv (name definition lexenv)
  (multiple-value-bind (compiled-definition warnings-p failure-p)
      (cond
        #!+sb-eval
        ((sb!eval:interpreted-function-p definition)
         (multiple-value-bind (definition lexenv)
             (sb!eval:prepare-for-compile definition)
           (actually-compile name definition lexenv)))
        ((compiled-function-p definition)
         (values definition nil nil))
        (t (actually-compile name definition lexenv)))
    (cond (name
           (if (and (symbolp name)
                    (macro-function name))
               (setf (macro-function name) compiled-definition)
               (setf (fdefinition name) compiled-definition))
           (values name warnings-p failure-p))
          (t
           (values compiled-definition warnings-p failure-p)))))

(defun compile (name &optional (definition (or (macro-function name)
                                               (fdefinition name))))
  #!+sb-doc
  "Coerce DEFINITION (by default, the function whose name is NAME)
  to a compiled function, returning (VALUES THING WARNINGS-P FAILURE-P),
  where if NAME is NIL, THING is the result of compilation, and
  otherwise THING is NAME. When NAME is not NIL, the compiled function
  is also set into (MACRO-FUNCTION NAME) if NAME names a macro, or into
  (FDEFINITION NAME) otherwise."
  (multiple-value-bind (function warnings-p failure-p)
      (compile-in-lexenv name definition (make-null-lexenv))
    (values (or function
                name
                (lambda (&rest arguments)
                  (error 'simple-program-error
                         :format-control
                         "Called function compiled with errors. Original ~
                          definition:~%  ~S~@[~%Arguments:~% ~{ ~S~}~]"
                         :format-arguments (list definition arguments))))
            warnings-p
            failure-p)))
