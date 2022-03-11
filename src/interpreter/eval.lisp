;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-INTERPRETER")

;;; *APPLYHOOK* works more-or-less as described in CLtL2, which is
;;; not at all like the *SELF-APPLYHOOK* that hooks every call
;;; into a function as part of the function itself.
;;; Don't bind it to an interpreted-function; probably don't bind to a
;;; symbol, and definitely not a lambda expression- just a compiled function.
;;; Also note: it's never rebound to NIL around each application,
;;; because that would make EVAL non-tail-recursive.  It is assumed that
;;; anyone messing with it knows what (s)he is doing.
(defvar *applyhook* nil)

;;; Retrieve the value of the binding (either lexical or special) of
;;; the variable named by SYMBOL in the environment ENV. For symbol
;;; macros the expansion is returned instead.
;;; Second values is T if the primary value is a macroexpansion.
;;; Tertiary value is the type to assert, or NIL if no type should be asserted.
;;; That is, policy-related decisions need to be made here, not in the caller.
(defun expand-or-eval-symbol (env symbol)
  (declare (symbol symbol))
  (binding* (((binding kind nil value) (find-lexical-var env symbol))
             (type (var-type-assertion env symbol binding :read))
             ((macro-p value)
              (if kind
                  (case kind
                    (:normal (values nil value))
                    (:macro  (values t (symexpand env symbol value)))
                    (t       (values nil (symbol-value symbol))))
                  (case (info :variable :kind symbol)
                    (:macro  (values t (symexpand env symbol)))
                    (:alien  (values nil (alien-value symbol)))
                    (t       (values nil (symbol-value symbol)))))))
    (values value macro-p type)))

;; Macros must go through the hook, but we can avoid it if the hook is FUNCALL.
(defun symexpand (env sym
                  &optional (expansion (info :variable :macro-expansion sym)))
  (let ((hook (valid-macroexpand-hook)))
    (if (eq hook #'funcall)
        expansion
        (funcall hook
                 (lambda (form env) (declare (ignore form env)) expansion)
                 sym env))))

;;; Implementation note: APPLY is the main reason the interpreter conses.
;;; It would be nice if you could preallocate a DX arglist and pass that
;;; on the stack. Applying directly from a DX arglist would make the
;;; interpreter non-tail-recursive, so we don't want to do that.
;;; There is sort of a way - assuming MAKE-LIST is made to be DXable -
;;; but it is probably much worse for performance:
;;; (multiple-value-call thing
;;;    (let ((foo (make-list n)))
;;;       (setf (nth 0 foo) (eval-nth-arg ...))
;;;       (values-list foo)
;;;

(defparameter *eval-level* -1)
(defparameter *eval-verbose* nil)

(defun %eval (exp env)
  (labels
      ((%%eval (&aux fname)
         (cond
          ((symbolp exp)
           ;; CLHS 3.1.2.1.1 Symbols as Forms
           (binding* (((val expanded-p type) (expand-or-eval-symbol env exp))
                      (eval-val (if expanded-p (%eval val env) val)))
             (when (and type (not (itypep eval-val type)))
               (typecheck-fail/ref exp eval-val type))
             eval-val))
          ;; CLHS 3.1.2.1.3 Self-Evaluating Objects
          ;; We can save a few instructions vs. testing ATOM
          ;; because SYMBOLP was already picked off.
          ((not (listp exp)) exp)
          ;; CLHS 3.1.2.1.2 Conses as Forms
          ((eq (setq fname (car exp)) 'setq)
           (eval-setq (cdr exp) env nil)) ; SEXPR = nil
          ;; CLHS 3.1.2.1.2.4 Lambda Forms
          ((typep fname '(cons (eql lambda)))
           (if (eq sb-ext:*evaluator-mode* :interpret)
               ;; It should be possible to avoid consing a function,
               ;; but this syntax isn't common enough to matter.
               (apply-it (funcall (if (must-freeze-p env) #'enclose-freeze #'enclose)
                                  (make-proto-fn fname) env nil))
               (compile-it)))
          ((not (symbolp fname))
           (%program-error "Invalid function name: ~S" fname))
          ;; CLHS 3.1.2.1.2.1 Special Forms
          ;; Pick off special forms first for speed. Special operators
          ;; can't be shadowed by local defs.
          ((logtest (get-header-data fname) +special-op-symbol+)
           (cond ((or (logtest (get-header-data fname) +simple-special-op+)
                      (eq sb-ext:*evaluator-mode* :interpret))
                  (cond ((and (symbol-extra-slot-p fname)
                              (functionp (symbol-extra fname)))
                         (funcall (truly-the function (symbol-extra fname))
                                  (cdr exp) env))
                        (t
                         (dispatch (%sexpr exp) env))))
                 (t
                  (compile-it))))
          (t ; Everything else: macros and functions.
           (multiple-value-bind (fn macro-p) (get-function (car exp) env)
             (if macro-p
                 (%eval (funcall (valid-macroexpand-hook) fn exp env) env)
                 (apply-it fn))))))
       (compile-it () ; the escape hatch for evaluator-mode = :COMPILE.
         (sb-impl::%simple-eval
          exp (if env (lexenv-from-env env) (make-null-lexenv))))
       (apply-it (f)
         (let ((args (mapcar (lambda (arg) (%eval arg env)) (cdr exp)))
               (h *applyhook*))
           (if (or (null h)
                   (eq h (load-time-value #'funcall t))
                   (eq h 'funcall))
               (apply f args)
               (funcall h f args)))))
    ;; Binding *EVAL-LEVEL* inhibits tail-call, so try to avoid it
    (if *eval-verbose*
        (let ((*eval-level* (1+ *eval-level*)))
          (let ((*print-circle* t))
            (format t "~&~vA~S~%" *eval-level* "" `(%eval ,exp)))
          (%%eval))
        (%%eval))))

;; DIGEST-FORM both "digests" and EVALs a form.
;; It should stash an optimized handler into the SEXPR so that DIGEST-FORM
;; will (ideally) not be called again on this SEXPR.
;; The new handler is invoked right away.
;;
;; A few special-form-processors exist for standard macros. I test INFO on
;; special-forms before considering FIND-LEXICAL-FUN. After I apply my
;; globaldb speedups, it will actually be faster to use the two-part test
;; than just check the lexical environment. Usually existence of a processor
;; implies a special form, which is illegal to rebind lexically; whereas
;; technically it's legal to rebind standard macros, though weird and
;; scoring no readability points.
;;
(defun digest-form (form env sexpr)
  (declare (sexpr sexpr))
  (cond ((symbolp form) ; CLHS 3.1.2.1.1 Symbols as Forms
         (return-from digest-form (symeval form env sexpr)))
        ((not (listp form)) ; CLHS 3.1.2.1.3 Self-Evaluating Objects
         (setf (sexpr-handler sexpr) (return-constant form))
         (return-from digest-form form)))
  ;; CLHS 3.1.2.1.2 Conses as Forms
  (let ((fname (car form)))
    (cond ((eq fname 'setq)
           ;; SETQ mandates a different protocol which slightly
           ;; simplifies the treatment of symbol macros.
           (return-from digest-form
             (eval-setq (cdr form) env sexpr)))
          ((typep fname '(cons (eql lambda)))
           ;; CLHS 3.1.2.1.2.4 "A lambda form is equivalent to using funcall of
           ;; a lexical closure of the lambda expression on the given arguments."
           (return-from digest-form
             (digest-form `(funcall #',fname ,@(cdr form)) env sexpr)))
          ((not (symbolp fname))
           (%program-error "Invalid function name: ~S" fname)))
    ;; CLHS 3.1.2.1.2.1 Special Forms.
    (when (logtest (get-header-data fname) +special-op-symbol+)
      (let ((processor (info :function :interpreter fname)))
        (when (functionp processor)
          (return-from digest-form
            (let ((digested-form (funcall processor (cdr form) env)))
              (setf (sexpr-handler sexpr) digested-form)
              (%dispatch sexpr env)))))
      (when (eq (info :function :kind fname) :special-form)
        ;; Special operators that reimplement macros can decline,
        ;; falling back upon the macro. This allows faster
        ;; implementations of things like AND,OR,COND,INCF
        ;; without having to deal with their full generality.
        (error "Operator ~S mustn't decline to handle ~S" fname form)))
    (let ((frame-ptr (local-fn-frame-ptr fname env)))
      (if (eq frame-ptr :macro)
          ;; CLHS 3.1.2.1.2.2 Macro Forms
          (multiple-value-bind (expansion keys)
              (tracing-macroexpand-1 form env)
            (cond (keys
                   (setf expansion (%sexpr expansion)
                           (sexpr-handler sexpr)
                           (digest-macro-form expansion fname keys))
                   (dispatch expansion env))
                  (t
                   (digest-form expansion env sexpr))))
        (progn
          (setf (sexpr-handler sexpr)
                (if frame-ptr ; a lexical function
                    (digest-local-call frame-ptr (cdr form))
                    (digest-global-call fname (cdr form) env)))
          (%dispatch sexpr env))))))

(fmakunbound 'eval-in-environment)
(defun eval-in-environment (form env)
  (incf *eval-calls*)
  (let ((interpreter-env
         (typecase env
          (sb-kernel:lexenv (if (sb-c::null-lexenv-p env) nil (env-from-lexenv env)))
          (t env))))
    (if (eq interpreter-env :compile)
        (funcall (handler-case
                     ;; Final arg of T means signal errors immediately rather
                     ;; than returning a function that signals when called.
                     (sb-c:compile-in-lexenv `(lambda () ,form) env nil nil nil nil t)
                  (error ()
                     ;; Whatever went wrong, just say "too complex"
                   (error 'compiler-environment-too-complex-error
                          :format-control
                          "~@<Lexical environment is too complex to evaluate in: ~S~:@>"
                          :format-arguments (list env)))))
        ;; FIXME: should this be (OR INTERPRETER-ENV (CAPTURE-TOPLEVEL-ENV)) ?
        ;; Whether we decide to capture the policy here or not, there will always
        ;; be some use-case that comes out wrong. Capturing it is necessary for
        ;; the following to work in the interpreter:
        #|
        (defmacro some-macro (x &environment e)
          (if (policy e (= safety 3)) (expand-to-safe-code) (expand-normally)))
        (with-compilation-unit (:policy '(optimize (safety 3)))
           (some-macro (whatever)))
        |#
        ;; because WITH-COMPILATION-UNIT rebinds *POLICY* and so we need
        ;; to look at that policy regardless of whether interpreting or compiling.
        ;; But %COERCE-TO-POLICY as used in the (POLICY) macro would return
        ;; **BASELINE-POLICY** instead of *POLICY* when given NIL as the env,
        ;; because the compiler wants that.
        ;; But if we do capture the policy up front, then we _fail_ to see
        ;; any changes that are made by PROCLAIM because those _don't_
        ;; affect the policy in an interpreter environment.
        (%eval form interpreter-env))))

(push
  (let ((this-pkg (find-package "SB-INTERPRETER")))
    `("SB-INTERPRETER"
      %%eval ; got inlined
      ,@(let (macros)
          (do-symbols (s "SB-INTERPRETER" macros)
            (when (and (eq (symbol-package s) this-pkg)
                       (macro-function s)
                       (not (member s '(defspecial with-subforms do-decl-spec)))) ; for SB-CLTL2
              (push s macros)))
          macros)))
  *!removable-symbols*)
