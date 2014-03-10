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
;;;
;;; If ERRORP is true signals an error immediately -- otherwise returns
;;; a function that will signal the error.
(defun actually-compile (name definition *lexenv* source-info tlf errorp)
  (let ((source-paths (when source-info *source-paths*)))
    (with-compilation-values
      (sb!xc:with-compilation-unit ()
        ;; FIXME: These bindings were copied from SUB-COMPILE-FILE with
        ;; few changes. Once things are stable, the shared bindings
        ;; probably be merged back together into some shared utility
        ;; macro, or perhaps both merged into one of the existing utility
        ;; macros SB-C::WITH-COMPILATION-VALUES or
        ;; CL:WITH-COMPILATION-UNIT.
        (with-source-paths
          (prog* ((tlf (or tlf 0))
                  ;; If we have a source-info from LOAD, we will
                  ;; also have a source-paths already set up -- so drop
                  ;; the ones from WITH-COMPILATION-VALUES.
                  (*source-paths* (or source-paths *source-paths*))
                  (form (get-lambda-to-compile definition))
                  (*source-info* (or source-info
                                  (make-lisp-source-info
                                   form :parent *source-info*)))
                  (*toplevel-lambdas* ())
                  (*block-compile* nil)
                  (*allow-instrumenting* nil)
                  (*code-coverage-records* nil)
                  (*code-coverage-blocks* nil)
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
                  (*compile-print* nil)
                  (oops nil))
             (handler-bind (((satisfies handle-condition-p) #'handle-condition-handler))
               (unless source-paths
                 (find-source-paths form tlf))
               (let ((*compiler-error-bailout*
                       (lambda (e)
                         (setf oops e)
                         ;; Unwind the compiler frames: users want the know where
                         ;; the error came from, not how the compiler got there.
                         (go :error))))
                 (return
                   (with-world-lock ()
                     (%compile form (make-core-object)
                               :name name
                               :path `(original-source-start 0 ,tlf))))))
           :error
             ;; Either signal the error right away, or return a function that
             ;; will signal the corresponding COMPILED-PROGRAM-ERROR. This is so
             ;; that we retain our earlier behaviour when called with erronous
             ;; lambdas via %SIMPLE-EVAL. We could legally do just either one
             ;; always, but right now keeping the old behaviour seems like less
             ;; painful option: compiler.pure.lisp is full of tests that make all
             ;; sort of assumptions about when which things are signalled. FIXME,
             ;; probably.
             (if errorp
                 (error oops)
                 (let ((message (princ-to-string oops))
                       (source (source-to-string form)))
                   (return
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (error 'compiled-program-error
                              :message message
                              :source source)))))))))))

(defun compile-in-lexenv (name definition lexenv
                          &optional source-info tlf errorp)
  (multiple-value-bind (compiled-definition warnings-p failure-p)
      (cond
        #!+sb-eval
        ((sb!eval:interpreted-function-p definition)
         (multiple-value-bind (definition lexenv)
             (sb!eval:prepare-for-compile definition)
           (actually-compile name definition lexenv source-info tlf errorp)))
        ((compiled-function-p definition)
         (values definition nil nil))
        (t
         (actually-compile name definition lexenv source-info tlf errorp)))
    (check-type compiled-definition compiled-function)
    (cond (name
           (if (and (symbolp name)
                    (macro-function name))
               (setf (macro-function name) compiled-definition)
               (setf (fdefinition name) compiled-definition))
           (values name warnings-p failure-p))
          (t
           (values compiled-definition warnings-p failure-p)))))

(defun compile (name &optional (definition (or (and (symbolp name)
                                                    (macro-function name))
                                               (fdefinition name))))
  #!+sb-doc
  "Produce a compiled function from DEFINITION. If DEFINITION is a
lambda-expression, it is coerced to a function. If DEFINITION is an
interpreted function, it is compiled. If DEFINITION is already a compiled
function, it is used as-is. (Future versions of SBCL might try to
recompile the existing definition, but this is not currently supported.)

If NAME is NIL, the compiled function is returned as the primary value.
Otherwise the resulting compiled function replaces existing function
definition of NAME, and NAME is returned as primary value; if NAME is a symbol
tha names a macro, its macro function is replaced and NAME is returned as
primary value.

Also returns secondary value which is true if any conditions of type WARNING
occur during the compilation, and NIL otherwise.

Tertiary value is true if any conditions of type ERROR, or WARNING that are
not STYLE-WARNINGs occur during compilation, and NIL otherwise.
"
  (compile-in-lexenv name definition (make-null-lexenv)))
