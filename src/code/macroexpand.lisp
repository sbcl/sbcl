;;;; MACROEXPAND and friends

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; syntactic environment access

(defun sb!xc:special-operator-p (symbol)
  #!+sb-doc
  "If the symbol globally names a special form, return T, otherwise NIL."
  (declare (symbol symbol))
  (eq (info :function :kind symbol) :special-form))

(defvar sb!xc:*macroexpand-hook* 'funcall
  #!+sb-doc
  "The value of this variable must be a designator for a function that can
  take three arguments, a macro expander function, the macro form to be
  expanded, and the lexical environment to expand in. The function should
  return the expanded form. This function is called by MACROEXPAND-1
  whenever a runtime expansion is needed. Initially this is set to
  FUNCALL.")

;;; Return *MACROEXPAND-HOOK* as a compiled function, or signal an error
;;; if that's not possible. Having an interpreted function as the expander
;;; hook can easily lead to an infinite loop.
;;; Something insane like a generic function with an interpreted method
;;; on CONS would appear to be a compiled-function. Nothing can prevent that,
;;; but hopefully this wrapper protects against reasonable mistakes.
(defun valid-macroexpand-hook (&optional (hook sb!xc:*macroexpand-hook*))
  (when (eq hook 'funcall)
    (return-from valid-macroexpand-hook #'funcall))
  ;; If you mistakenly bind the hook to a un-fboundp symbol (esp. NIL),
  ;; it is nicer to say that the hook is invalid rather than randomly
  ;; getting "unbound function" at indeterminate places in your code.
  (let ((fun (if (functionp hook)
                 hook
                 ;; We need to get the function named by the designator.
                 ;; The type proclamation in 'cl-specials' seems to think
                 ;; that SETF functions are permitted here, though that
                 ;; really seems like a bug. If it is permitted,
                 ;; we can't use SYMBOL-FUNCTION. But using FDEFINITION
                 ;; would strip encapsulations, so use %COERCE-NAME-TO-FUN.
                 ;; (This allows tracing the macroexpand-hook, e.g.)
                 (and (fboundp hook)
                      #+sb-xc-host (fdefinition hook)
                      #-sb-xc-host (%coerce-name-to-fun hook)))))
    ;; We could do one of several things instead of failing:
    ;; - preprocess the body to ensure that there are no macros,
    ;;   and install that body, letting it run interpreted.
    ;; - call COMPILE and install it as the FIN-FUNCTION, and use that.
    ;; - call COMPILE and just return the result, which is a horrible
    ;;   technique, as it would call COMPILE once per macro usage.
    (if (compiled-function-p fun)
        fun
        (error 'sb!kernel::macroexpand-hook-type-error
               :datum hook
               :expected-type 'compiled-function))))

(defun sb!xc:macroexpand-1 (form &optional env)
  #!+sb-doc
  "If form is a macro (or symbol macro), expand it once. Return two values,
   the expanded form and a T-or-NIL flag indicating whether the form was, in
   fact, a macro. ENV is the lexical environment to expand in, which defaults
   to the null environment."
  (flet ((perform-expansion (expander &optional (expansion nil expansion-p))
           ;; There is no compelling reason to coerce NIL to a LEXENV when
           ;; supplying it to a user-defined macro which receives &ENVIRONMENT,
           ;; and it is expressly the wrong thing to do. An environment is
           ;; opaque, and the only thing you can legally do with one is pass
           ;; it to a standard functions defined to receive it.
           ;; The validity of NIL as an "environment object" is undeniably
           ;; legal in *any* usage demanding one, based on CLHS 3.1.1.3.1.
           ;; Importantly, macros can sense when they are producing code for the
           ;; compiler or interpreter based on the type of environment.
           (let ((hook (truly-the function (valid-macroexpand-hook))))
             (values (if (eq hook #'funcall)
                         (if expansion-p expansion (funcall expander form env))
                         (funcall hook expander form env))
                     t)))
         (symbol-expansion (sym env)
           (flet ((global-expansion () (info :variable :macro-expansion sym)))
             (typecase env
               (null (global-expansion))
               #!+(and sb-fasteval (host-feature sb-xc))
               (sb!interpreter:basic-env
                (multiple-value-bind (cell kind frame-ptr def)
                    (sb!interpreter:find-lexical-var env sym)
                  (declare (ignore cell frame-ptr))
                  (cond ((eq kind :macro) (values def t))
                        ((null kind) (global-expansion))
                        (t (values nil nil)))))
               (lexenv
                (let ((def (cdr (assoc sym (sb!c::lexenv-vars env)))))
                  (cond ((null def) (global-expansion))
                        ((listp def) (values (cdr def) t))
                        (t (values nil nil)))))))))
    (acond ((symbolp form)
            (multiple-value-bind (exp expanded-p) (symbol-expansion form env)
              ;; CLHS 3.1.2.1.1 specifies that symbol-macros are expanded
              ;; via the macroexpand hook.
              (if expanded-p
                  (perform-expansion #'symbol-expansion exp)
                  (values form nil))))
           ((and (listp form)
                 (let ((fn (car form)))
                   (and (symbolp fn) (sb!xc:macro-function fn env))))
            (perform-expansion it))
           (t
            (values form nil)))))

(defun sb!xc:macroexpand (form &optional env)
  #!+sb-doc
  "Repetitively call MACROEXPAND-1 until the form can no longer be expanded.
   Returns the final resultant form, and T if it was expanded. ENV is the
   lexical environment to expand in, or NIL (the default) for the null
   environment."
  (labels ((frob (form expanded)
             (multiple-value-bind (new-form newly-expanded-p)
                 (sb!xc:macroexpand-1 form env)
               (if newly-expanded-p
                   (frob new-form t)
                   (values new-form expanded)))))
    (frob form nil)))

;;; Like MACROEXPAND-1, but takes care not to expand special forms.
(defun %macroexpand-1 (form &optional env)
  (if (or (atom form)
          (let ((op (car form)))
            (not (and (symbolp op) (sb!xc:special-operator-p op)))))
      (sb!xc:macroexpand-1 form env)
      (values form nil)))

;;; Like MACROEXPAND, but takes care not to expand special forms.
(defun %macroexpand (form &optional env)
  (labels ((frob (form expanded)
             (multiple-value-bind (new-form newly-expanded-p)
                 (%macroexpand-1 form env)
               (if newly-expanded-p
                   (frob new-form t)
                   (values new-form expanded)))))
    (frob form nil)))
