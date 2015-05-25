;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(/show0 "parse-lambda-list.lisp 12")

;;; Break something like a lambda list (but not necessarily actually a
;;; lambda list, e.g. the representation of argument types which is
;;; used within an FTYPE specification) into its component parts. We
;;; return eight values:
;;;  1. a cons of booleans indicating presence of &KEY/&ALLOW-OTHER-KEYS
;;;     or NIL if there were no lambda list keywords at all.
;;;  2. a list of the required args;
;;;  3. a list of the &OPTIONAL arg specs;
;;;  4. a singleton list of the &REST arg if present;
;;;  5. a list of the &KEY arg specs;
;;;  6. a list of the &AUX specifiers;
;;;  7. the &MORE context and count vars if present;
;;;  8. a singleton list of the &ENVIRONMENT arg if present
;;;
;;; The top level lambda list syntax is checked for validity, but the
;;; arg specifiers are just passed through untouched. If something is
;;; wrong, we use COMPILER-ERROR, aborting compilation to the last
;;; recovery point.
(declaim (ftype (sfunction
                 (list &key (:context t) (:disallow list) (:silent boolean))
                 (values list list list list list list list list))
                parse-lambda-list))

;;; Note: CLHS 3.4.4 [macro lambda list] allows &ENVIRONMENT anywhere,
;;; but 3.4.7 [defsetf lambda list] has it only at the end.
;;; This is possibly surprising to people since there seems to be some
;;; expectation that a DEFSETF lambda list is a macro lambda list,
;;; which it isn't. We'll relax and accept &ENVIRONMENT in the middle.
;;; But we won't accept the ugly syntax that parse-defmacro accidentally
;;; allows of (A B &ENVIRONMENT E X Y) which has 4 positional parameters.
;;; Nor can it appear between &KEY and &ALLOW-OTHER-KEYS.
;;;
(defun parse-lambda-list (list &key (disallow '(&environment)) context silent)
  (collect ((required) (optional) (more) (keys) (aux))
    (let ((rest nil)
          (keyp nil)
          (allowp nil)
          (env nil)
          (tail list)
          (arg nil)
          (saved-state nil)
          (state :required))
      (labels ((croak (string &rest err-args)
                 (compiler-error 'simple-program-error
                                 :format-control string
                                 :format-arguments err-args))
               (need-symbol (x why)
                 (unless (symbolp x)
                   (croak "~A is not a symbol: ~S" why x))))
        (flet ((transition (from-states to-state &optional visited-p)
                 (when (and (eq state :post-env) (neq saved-state :key))
                   (shiftf state saved-state nil)) ; pop the state
                 (cond ((not (member state from-states))
                        (croak "misplaced ~S in lambda list: ~S" arg list))
                       (visited-p
                        (croak "repeated ~S in lambda list: ~S" arg list))
                       (t
                        (setq state to-state))))
               (handle-parameter ()
                 (setq state
                  (case state
                    (:required (required arg) state)
                    (:optional (optional arg) state)
                    (:rest     (setq rest (list arg)) :post-rest)
                    (:more     (more arg) (if (cdr (more)) :post-more :more))
                    (:key      (keys arg) state)
                    (:aux      (aux arg) state)
                    (:env      (setq env (list arg))
                               (if (and (eq saved-state :required)
                                        (not (required))) :required :post-env))
                    (t (croak "expecting lambda list keyword at ~S in: ~S"
                              arg list)))))
               (explain-context ()
                 (case context
                   (type "a FUNCTION or VALUES type specifier")
                   (t context))))
          (loop
           (when (endp tail) (return))
           (setq arg (pop tail))
           (cond
            ((or (not (symbolp arg))
                 (let ((name (symbol-name arg)))
                   (or (zerop (length name)) (char/= (char name 0) #\&))))
             (handle-parameter))
            ((member arg disallow)
             (if context
                 (croak "~A is not allowed in ~A: ~S" arg (explain-context) list)
                 (croak "Bad lambda list keyword ~S in: ~S" arg list)))
            (t
             (case arg
              (&optional (transition '(:required) :optional))
              (&rest     (transition '(:required :optional) :rest))
              (&more     (transition '(:required :optional) :more))
              (&key
               (transition '(:required :optional :post-rest :post-more)
                           :key)
               (setq keyp t)
               #-sb-xc-host
               (when (and (optional) (not silent))
                 (compiler-style-warn
                  "&OPTIONAL and &KEY found in the same lambda list: ~S" list)))
              (&allow-other-keys (transition '(:key) :allow-other-keys)
                                 (setq allowp t))
              (&aux (transition '(:post-more :required :optional :post-rest
                                  :key :allow-other-keys) :aux))
              (&environment
               (setq saved-state state)
               ;; Valid "from" states are almost like &AUX
               (transition '(:required :optional :post-rest
                             :key :allow-other-keys) :env env))
              ((&body &whole)
               ;; It could be argued that &WHOLE and friends would be
               ;; just ordinary variables in an ordinary lambda-list,
               ;; but since (1) that seem exceedingly to have been the
               ;; programmers intent and (2) the spec can be
               ;; interpreted as giving as licence to signal an
               ;; error[*] that is what we do.
               ;;
               ;; [* All lambda list keywords used in the
               ;; implementation appear in LAMBDA-LIST-KEYWORDS. Each
               ;; member of a lambda list is either a parameter
               ;; specifier ot a lambda list keyword. Ergo, symbols
               ;; appearing in LAMBDA-LIST-KEYWORDS cannot be
               ;; parameter specifiers.]
               (croak "Bad lambda list keyword ~S in: ~S" arg list))
              (t
               (unless silent
                    ;; Should this be COMPILER-STYLE-WARN?
                 (style-warn "suspicious variable in lambda list: ~S." arg))
               (handle-parameter))))))

          (when (member state '(:rest :more :env))
            (croak "~A expects a variable following it" arg)))

      ;; For CONTEXT other than 'TYPE we reject illegal list elements.
      ;; TYPEs have arbitrary shapes,
      ;; such as (VALUES (ARRAY (MUMBLE) (1 2)) &OPTIONAL (MEMBER X Y Z)).
      ;; But why don't we reject constant symbols here?
        (unless (eq context 'type)
          (dolist (i (required))
            (need-symbol i "Required argument"))
          (dolist (i (optional))
            (typecase i
              (symbol)
              (cons
               (destructuring-bind (var &optional init-form supplied-p) i
                 (declare (ignore init-form supplied-p))
                 (need-symbol var "&OPTIONAL parameter name")))
              (t
               (croak "&OPTIONAL parameter is not a symbol or cons: ~S" i))))
          (when rest
            (need-symbol (car rest) "&REST argument"))
          (dolist (i (keys))
            (typecase i
              (symbol)
              (cons
               (destructuring-bind (var-or-kv &optional init-form supplied-p) i
                 (declare (ignore init-form supplied-p))
                 (if (consp var-or-kv)
                     (destructuring-bind (keyword-name var) var-or-kv
                       (declare (ignore keyword-name))
                       (need-symbol var "&KEY parameter name"))
                     (need-symbol var-or-kv "&KEY parameter name"))))
              (t
               (croak "&KEY parameter is not a symbol or cons: ~S" i))))))

    ;; Voila.
      (values (if (or (neq state :required) env) (cons keyp allowp))
              (required) (optional) rest (keys) (aux) (more) env))))

(/show0 "parse-lambda-list.lisp end of file")
