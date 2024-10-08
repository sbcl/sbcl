;;;; lots of basic macros for the target SBCL

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")


;;;; DEFMACRO

;;; Inform the cross-compiler how to expand SB-XC:DEFMACRO (= DEFMACRO)
;;; and supporting macros using the already defined host macros until
;;; this file is itself cross-compiled.
#+sb-xc-host
(flet ((defmacro-using-host-expander (name)
         (setf (macro-function name)
               (lambda (form env)
                 (declare (ignore env))
                 ;; Since SB-KERNEL:LEXENV isn't compatible with the host,
                 ;; just pass NIL. The expansion correctly captures a non-null
                 ;; environment, but the expander doesn't need it.
                 (funcall (cl:macro-function name) form nil)))))
  (defmacro-using-host-expander 'sb-xc:defmacro)
  (defmacro-using-host-expander 'named-ds-bind)
  (defmacro-using-host-expander 'binding*)
  (defmacro-using-host-expander 'sb-xc:deftype)
  ;; FIXME: POLICY doesn't support DEFMACRO, but we need it ASAP.
  (defmacro-using-host-expander 'sb-c:policy))


;;;; Destructuring-bind

(sb-xc:defmacro destructuring-bind (lambda-list expression &body body
                                                           &environment env)
  (declare (ignore env)) ; could be policy-sensitive (but isn't)
  "Bind the variables in LAMBDA-LIST to the corresponding values in the
tree structure resulting from the evaluation of EXPRESSION."
  `(binding* ,(sb-c::expand-ds-bind lambda-list expression t nil)
     ,@body))


;;;; DEFUN

;;; Should we save the inline expansion of the function named NAME?
(defun save-inline-expansion-p (name)
  (or
   ;; the normal reason for saving the inline expansion
   (let ((inlinep (info :function :inlinep name)))
     (member inlinep '(inline maybe-inline)))
   ;; another reason for saving the inline expansion: If the
   ;; ANSI-recommended idiom
   ;;   (DECLAIM (INLINE FOO))
   ;;   (DEFUN FOO ..)
   ;;   (DECLAIM (NOTINLINE FOO))
   ;; has been used, and then we later do another
   ;;   (DEFUN FOO ..)
   ;; without a preceding
   ;;   (DECLAIM (INLINE FOO))
   ;; what should we do with the old inline expansion when we see the
   ;; new DEFUN? Overwriting it with the new definition seems like
   ;; the only unsurprising choice.
   (nth-value 1 (fun-name-inline-expansion name))))

(defun extract-dx-args (lambda-list decl-forms)
  (let (dx-decls)
    (dolist (form decl-forms)
      (dolist (expr (cdr form))
        (when (typep expr '(cons (eql dynamic-extent)))
          (setf dx-decls (union dx-decls (cdr expr))))))
    (unless dx-decls
      (return-from extract-dx-args nil))
    ;; TODO: in addition to ":SILENT T" supressing warnings, PARSE-LAMBDA-LIST
    ;; needs to allow :CONDITION-CLASS = NIL to ask that no errors be signaled.
    ;; An indicator can be returned so that at worst the code below does nothing.
    (multiple-value-bind (llks required optional rest key aux)
        (parse-lambda-list lambda-list :silent t)
      (declare (ignore llks rest))
      ;; We enforce uniqueness of the symbols in the union of REQUIRED,
      ;; OPTIONAL, REST, KEY (including any supplied-p variables),
      ;; but there may be an AUX binding shadowing a lambda binding.
      ;; This affects something like:
      ;;  (LAMBDA (X &AUX (X (MAKE-FOO X))) (DECLARE (DYNAMIC-EXTENT X))
      ;; in which the decl does not pertain to argument X.
      (let ((arg-index 0) caller-dxable)
        (labels ((examine (sym dx-note)
                   (when (and (member sym dx-decls) (not (shadowed-p sym)))
                     (push dx-note caller-dxable))
                   (incf arg-index))
                 (shadowed-p (sym)
                   (dolist (binding aux)
                     (when (eq (if (listp binding) (car binding) binding) sym)
                       (return t)))))
          (dolist (spec required)
            (examine spec arg-index))
          (dolist (spec optional)
            (examine (if (listp spec) (car spec) spec) arg-index))
          (dolist (spec key)
            (multiple-value-bind (keyword var) (parse-key-arg-spec spec)
              (examine var keyword))))
        (nreverse caller-dxable)))))

(defun block-compilation-non-entry-point (name)
  (and (boundp 'sb-c:*compilation*)
       (let* ((compilation sb-c:*compilation*)
              (entry-points (sb-c::entry-points compilation)))
         (and (sb-c::block-compile compilation)
              entry-points
              (not (member name entry-points :test #'equal))))))

(defun specialized-xep-for-type-p (lambda-list type)
  (and #-(or arm64 x86-64) nil
       (fun-type-p type)
       (not (or (fun-type-optional type)
                (fun-type-keyp type)
                (fun-type-rest type)))
       (multiple-value-bind (llks required) (parse-lambda-list lambda-list)
         (and (zerop llks)
              (= (length required)
                 (length (fun-type-required type)))
              ;; FIXME: The number of float regs.
              (<= (length required) 16)))
       (loop for arg in (fun-type-required type)
             thereis (csubtypep arg (specifier-type 'double-float)))
       (cdr (type-specifier type))))

(defun make-specialized-xep-stub (name specialized
                                  &optional (xep-name
                                             `(specialized-xep ,name ,@specialized)))
  (let ((vars (loop for arg in (car specialized)
                    for i from 0
                    collect (make-symbol (format nil "A~a" i)))))
    (values
     `(named-lambda ,xep-name ,vars
        (declare (notinline ,name)
                 (muffle-conditions warning))
        (funcall ',name ,@vars))
     xep-name)))

(flet ((defun-expander (env name lambda-list body snippet &optional source-form)
  (multiple-value-bind (forms decls doc) (parse-body body t)
    ;; Maybe kill docstring, but only under the cross-compiler.
    #+(and (not sb-doc) sb-xc-host) (setq doc nil)
    (let* (;; stuff shared between LAMBDA and INLINE-LAMBDA and NAMED-LAMBDA
           (lambda-guts `(,@decls (block ,(fun-name-block-name name) ,@forms)))
           (lambda `(lambda ,lambda-list ,@lambda-guts))
           (named-lambda `(named-lambda ,name ,lambda-list
                           ,@(when *top-level-form-p* '((declare (sb-c::top-level-form))))
                           ,@(when doc (list doc)) ,@lambda-guts))
           ;; DXABLE-ARGS and SNIPPET are mutually exclusive, so we can sleazily pass
           ;; whichever exists (if either does) as one parameter to %DEFUN.
           (extra-info (or snippet (extract-dx-args lambda-list decls)))
           (inline-thing
            (cond ((member snippet '(:predicate :copier :accessor)) nil)
                  ;; If the defstruct snippet is :CONSTRUCTOR, we might have to store
                  ;; a full inline expansion depending on the lexical environment.
                  ((save-inline-expansion-p name)
                  ;; we want to attempt to inline, so complain if we can't
                   (cond ((sb-c:inline-syntactic-closure-lambda lambda env))
                         (t
                          (#+sb-xc-host warn
                           #-sb-xc-host sb-c:maybe-compiler-notify
                           "lexical environment too hairy, can't inline DEFUN ~S"
                           name)
                          nil)))))
           (specialized-xep (and (not (or inline-thing
                                          (info :function :info name)))
                                 (specialized-xep-for-type-p lambda-list (info :function :type name)))))
      (when (and (eq snippet :constructor)
                 (not (typep inline-thing '(cons (eql sb-c:lambda-with-lexenv)))))
        ;; constructor in null lexenv need not save the expansion
        (setq inline-thing nil))
      (when inline-thing
        (setq inline-thing (list 'quote inline-thing)))
      (when (and extra-info (not (keywordp extra-info)))
        (setq extra-info (list 'quote extra-info)))
      `(progn
         ,@(let ((existing-specialized-xep (info :function :specialized-xep name)))
             (if (and existing-specialized-xep
                      (not (equal existing-specialized-xep specialized-xep)))
                 (multiple-value-bind (xep xep-name)
                     (make-specialized-xep-stub name existing-specialized-xep)
                   `((progn
                       (eval-when (:compile-toplevel)
                         (clear-info :function :specialized-xep ',name))
                       (when (fdefinition ',xep-name)
                         (setf (fdefinition ',xep-name) ,xep)))))))
         ,@(if specialized-xep
               (let ((xep-name `(specialized-xep ,name ,@specialized-xep)))
                 `((eval-when (:compile-toplevel)
                     (sb-c:%compiler-defun ',name t nil nil ',specialized-xep))
                    (let ((xep (named-lambda ,xep-name ,@(cddr named-lambda))))
                      (sb-impl::%defun-specialized-xep
                       ',name
                       (named-lambda ,name ,lambda-list
                         ,@(when *top-level-form-p* '((declare (sb-c::top-level-form))))
                         ,@(when doc (list doc))
                         (multiple-value-prog1
                             (funcall xep ,@lambda-list)
                           ;; Avoid tail calls for unboxed returns.
                           (values)))
                       xep
                       ',specialized-xep))))
               (let ((definition
                       (if (block-compilation-non-entry-point name)
                           `(progn
                              (sb-c::%refless-defun ,named-lambda)
                              ',name)
                           `(%defun ',name ,named-lambda
                                    ,@(when (or inline-thing extra-info) `(,inline-thing))
                                    ,@(when extra-info `(,extra-info))))))
                 `((eval-when (:compile-toplevel)
                     (sb-c:%compiler-defun ',name t ,inline-thing ,extra-info))
                   ,(if source-form
                        `(sb-c::with-source-form ,source-form ,definition)
                        definition)
                   ;; This warning, if produced, comes after the DEFUN happens.
                   ;; When compiling, there's no real difference, but when interpreting,
                   ;; if there is a handler for style-warning that nonlocally exits,
                   ;; it's wrong to have skipped the DEFUN itself, since if there is no
                   ;; function, then the warning ought not to have been issued at all.
                   ,@(when (typep name '(cons (eql setf)))
                       `((eval-when (:compile-toplevel :execute)
                           (sb-c::warn-if-setf-macro ',name))
                         ',name))))))))))

;;; This is one of the major places where the semantics of block
;;; compilation is handled. Substitution for global names is totally
;;; inhibited if (block-compile *compilation*) is NIL. And if
;;; (block-compile *compilation*) is true and entry points are
;;; specified, then we don't install global definitions for non-entry
;;; functions (effectively turning them into local lexical functions.)
  (sb-xc:defmacro defun (&environment env name lambda-list &body body)
    "Define a function at top level."
    (check-designator name 'defun #'legal-fun-name-p "function name")
    #+sb-xc-host
    (unless (cl:symbol-package (fun-name-block-name name))
      (warn "DEFUN of uninterned function name ~S (tricky for GENESIS)" name))
    (defun-expander env name lambda-list body nil))

  ;; extended defun as used by defstruct
  (sb-xc:defmacro sb-c:xdefun (&environment env name snippet source-form lambda-list &body body)
    (defun-expander env name lambda-list body snippet source-form)))

;;;; DEFCONSTANT, DEFVAR and DEFPARAMETER

(sb-xc:defmacro defconstant (name value &optional (doc nil docp))
  "Define a global constant, saying that the value is constant and may be
  compiled into code. If the variable already has a value, and this is not
  EQL to the new value, the code is not portable (undefined behavior). The
  third argument is an optional documentation string for the variable."
  (check-designator name 'defconstant)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%defconstant ',name ,value (sb-c:source-location)
                   ,@(and docp `(',doc)))))


(declaim (ftype (sfunction (symbol t &optional t t) null)
                about-to-modify-symbol-value))
;;; the guts of DEFCONSTANT
(defun %defconstant (name value source-location &optional (doc nil docp))
  #+sb-xc-host (declare (ignore doc docp))
  (unless (symbolp name)
    (error "The constant name is not a symbol: ~S" name))
  (with-single-package-locked-error (:symbol name
                                             "defining ~s as a constant")
   (when (looks-like-name-of-special-var-p name)
     (style-warn 'asterisks-around-constant-variable-name
                 :format-control "Defining ~S as a constant"
                 :format-arguments (list name)))
   (when source-location
     (setf (info :source-location :constant name) source-location))
   (let ((kind (info :variable :kind name)))
     (case kind
       (:constant
        ;; Note: This behavior (discouraging any non-EQL modification)
        ;; is unpopular, but it is specified by ANSI (i.e. ANSI says a
        ;; non-EQL change has undefined consequences). If people really
        ;; want bindings which are constant in some sense other than
        ;; EQL, I suggest either just using DEFVAR (which is usually
        ;; appropriate, despite the un-mnemonic name), or defining
        ;; something like the DEFCONSTANT-EQX macro used in SBCL (which
        ;; is occasionally more appropriate). -- WHN 2001-12-21
        (if (boundp name)
            (if (typep name '(or boolean keyword))
                ;; Non-continuable error.
                (about-to-modify-symbol-value name 'defconstant)
                (let ((old (symbol-value name)))
                  (unless (or (eql value old)
                              ;; SAPs behave like numbers but yet EQL doesn't work on them,
                              ;; special case it.
                              ;; Nobody will notices that the constant
                              ;; is not EQ, since it can be copied at
                              ;; any time anyway.
                              #-sb-xc-host
                              (and (system-area-pointer-p old)
                                   (system-area-pointer-p value)
                                   (sap= old value)))
                    (multiple-value-bind (ignore aborted)
                        (with-simple-restart (abort "Keep the old value.")
                          (cerror "Go ahead and change the value."
                                  'defconstant-uneql
                                  :name name
                                  :old-value old
                                  :new-value value))
                      (declare (ignore ignore))
                      (when aborted
                        (return-from %defconstant name))))))
            (warn "redefining a MAKUNBOUND constant: ~S" name)))
       (:unknown
        ;; (This is OK -- undefined variables are of this kind. So we
        ;; don't warn or error or anything, just fall through.)
        )
       (t (warn "redefining ~(~A~) ~S to be a constant" kind name)))))
  (dolist (backpatch (info :variable :forward-references name))
    (funcall backpatch value))
  (clear-info :variable :forward-references name)
  ;; We ought to be consistent in treating any change of :VARIABLE :KIND
  ;; as a continuable error. The above CASE expression pre-dates the
  ;; existence of symbol-macros (I believe), but at a bare minimum,
  ;; INFO should return NIL for its second value if requesting the
  ;; :macro-expansion of something that is getting defined as constant.
  (clear-info :variable :macro-expansion name)
  (clear-info :source-location :symbol-macro name)
  #-sb-xc-host
  (progn
    (when docp
      (setf (documentation name 'variable) doc))
    (%set-symbol-value name value))
  ;; Define the constant in the cross-compilation host, since the
  ;; value is used when cross-compiling for :COMPILE-TOPLEVEL contexts
  ;; which reference the constant.
  #+sb-xc-host
  (eval `(unless (boundp ',name) (defconstant ,name ',value)))
  (setf (info :variable :kind name) :constant)
  ;; Deoptimize after changing it to :CONSTANT, and not before, though tbh
  ;; if your code cares about the timing of PROGV relative to DEFCONSTANT,
  ;; well, I can't even.
  #-sb-xc-host (unset-symbol-progv-optimize name)
  name)

(sb-xc:defmacro defvar (var &optional (val nil valp) (doc nil docp))
  "Define a special variable at top level. Declare the variable
  SPECIAL and, optionally, initialize it. If the variable already has a
  value, the old value is not clobbered. The third argument is an optional
  documentation string for the variable."
  (check-designator var 'defvar)
  ;; Maybe kill docstring, but only under the cross-compiler.
  #+(and (not sb-doc) sb-xc-host) (setq doc nil)
  `(progn
     (eval-when (:compile-toplevel)
       (%compiler-defvar ',var))
     (%defvar ',var
              (sb-c:source-location)
              ,@(cond ((not valp)
                       nil)
                      ((constantp val)
                       ;; No need to avoid evaluation if it's a constant.
                       `(',(constant-form-value val)))
                      (val
                       `((unless (%boundp ',var) ,val))))
              ,@(and docp
                     `(',doc)))))

(sb-xc:defmacro defparameter (var val &optional (doc nil docp))
  "Define a parameter that is not normally changed by the program,
  but that may be changed without causing an error. Declare the
  variable special and sets its value to VAL, overwriting any
  previous value. The third argument is an optional documentation
  string for the parameter."
  (check-designator var 'defparameter)
  ;; Maybe kill docstring, but only under the cross-compiler.
  #+(and (not sb-doc) sb-xc-host) (setq doc nil)
  `(progn
     (eval-when (:compile-toplevel)
       (%compiler-defvar ',var))
     (%defparameter ',var ,val (sb-c:source-location)
                    ,@(and docp
                           `(',doc)))))

(defun %compiler-defvar (var)
  (proclaim `(special ,var)))


;;;; DEFGLOBAL and DEFINE-LOAD-TIME-GLOBAL

(sb-xc:defmacro defglobal (name value &optional (doc nil docp))
  "Defines NAME as a global variable that is always bound. VALUE is evaluated
and assigned to NAME both at compile- and load-time, but only if NAME is not
already bound.

Global variables share their values between all threads, and cannot be
locally bound, declared special, defined as constants, and neither bound
nor defined as symbol macros.

See also the declarations SB-EXT:GLOBAL and SB-EXT:ALWAYS-BOUND."
  (check-designator name 'defglobal)
  (let ((boundp (make-symbol "BOUNDP")))
    `(progn
       (eval-when (:compile-toplevel)
         (let ((,boundp (boundp ',name)))
           (%compiler-defglobal ',name :always-bound
                                (not ,boundp) (unless ,boundp ,value))))
       (%defglobal ',name
                   (if (%boundp ',name) (make-unbound-marker) ,value)
                   (sb-c:source-location)
                   ,@(and docp `(',doc))))))

(sb-xc:defmacro define-load-time-global (name value &optional (doc nil docp))
  "Defines NAME as a global variable that is always bound. VALUE is evaluated
and assigned to NAME at load-time, but only if NAME is not already bound.

Attempts to read NAME at compile-time will signal an UNBOUND-VARIABLE error
unless it has otherwise been assigned a value.

See also DEFGLOBAL which assigns the VALUE at compile-time too."
  (check-designator name 'define-load-time-global)
  `(progn
     (eval-when (:compile-toplevel)
       (%compiler-defglobal ',name :eventually nil nil))
     (%defglobal ',name
                 (if (%boundp ',name) (make-unbound-marker) ,value)
                 (sb-c:source-location)
                 ,@(and docp `(',doc)))))

(defun %compiler-defglobal (name always-boundp assign-it-p value)
  (proclaim `(global ,name))
  (when assign-it-p
    (set-symbol-global-value name value))
  (sb-c::process-variable-declaration
   name 'always-bound
   ;; don't "weaken" the proclamation if it's in fact always bound now
   (if (eq (info :variable :always-bound name) :always-bound)
       :always-bound
       always-boundp)))


;;;; various conditional constructs
(flet ((prognify (forms env)
         (cond ((not forms) nil)
               ((and (singleton-p forms)
                     (sb-c:policy env (= sb-c:store-coverage-data 0)))
                (car forms))
               (t `(progn ,@forms)))))
  ;; COND defined in terms of IF
  (sb-xc:defmacro cond (&rest clauses &environment env)
    (named-let make-clauses ((clauses clauses))
      (if (endp clauses)
          nil
          (let ((clause (first clauses))
                (more (rest clauses)))
            (with-current-source-form (clauses)
              (if (atom clause)
                  (error 'simple-type-error
                         :format-control "COND clause is not a ~S: ~S"
                         :format-arguments (list 'cons clause)
                         :expected-type 'cons
                         :datum clause)
                  (let ((test (first clause))
                        (forms (rest clause)))
                    (if (endp forms)
                        `(or ,test ,(make-clauses more))
                        (if (and (eq test t)
                                 (not more))
                            ;; THE to preserve non-toplevelness for FOO in
                            ;;   (COND (T (FOO)))
                            `(the t ,(prognify forms env))
                            `(if ,test
                                 ,(prognify forms env)
                                 ,(when more (make-clauses more))))))))))))

  (sb-xc:defmacro when (test &body forms &environment env)
  "If the first argument is true, the rest of the forms are
evaluated as a PROGN."
    `(if ,test ,(prognify forms env)))

  (sb-xc:defmacro unless (test &body forms &environment env)
  "If the first argument is not true, the rest of the forms are
evaluated as a PROGN."
    `(if ,test nil ,(prognify forms env))))


(sb-xc:defmacro return (&optional (value nil))
  `(return-from nil ,value))

;;;; various sequencing constructs
(flet ((prog-expansion-from-let (varlist body-decls let)
         (multiple-value-bind (body decls) (parse-body body-decls nil)
           `(block nil
              (,let ,varlist
                ,@decls
                (tagbody ,@body))))))
  (sb-xc:defmacro prog (varlist &body body-decls)
    (prog-expansion-from-let varlist body-decls 'let))
  (sb-xc:defmacro prog* (varlist &body body-decls)
    (prog-expansion-from-let varlist body-decls 'let*)))

(sb-xc:defmacro prog1 (result &body body)
  (let ((n-result (gensym)))
    `(let ((,n-result ,result))
       (progn
         ,@body
         ,n-result))))

(sb-xc:defmacro prog2 (form1 result &body body)
  `(prog1 (progn ,form1 ,result) ,@body))

;; AND and OR are defined in terms of IF.
(sb-xc:defmacro and (&rest forms)
  (named-let expand-forms ((nested nil) (forms forms) (ignore-last nil))
    (cond ((endp forms) t)
          ((endp (rest forms))
           (let ((car (car forms)))
             (cond (nested
                    car)
                   (t
                    ;; Preserve non-toplevelness of the form!
                    `(the t ,car)))))
          ((and ignore-last
                (endp (cddr forms)))
           (car forms))
          ;; Better code that way, since the result will only have two
          ;; values, NIL or the last form, and the precedeing tests
          ;; will only be used for jumps
          ((and (not nested) (cddr forms))
           `(if ,(expand-forms t forms t)
                ,@(last forms)))
          (t
           `(if ,(first forms)
                ,(expand-forms t (rest forms) ignore-last))))))

(sb-xc:defmacro or (&rest forms)
  (named-let expand-forms ((nested nil) (forms forms))
    (cond ((endp forms) nil)
          ((endp (rest forms))
           ;; Preserve non-toplevelness of the form!
           (let ((car (car forms))) (if nested car `(the t ,car))))
          (t
           (let ((n-result (gensym)))
             `(let ((,n-result ,(first forms)))
                (if ,n-result
                    ,n-result
                    ,(expand-forms t (rest forms)))))))))


;;;; Multiple value macros:

;;; All the multiple-value receiving forms are defined in terms of
;;; MULTIPLE-VALUE-CALL.
(flet ((validate-vars (vars)
         (with-current-source-form (vars)
           (unless (and (listp vars) (every #'symbolp vars))
             (error "Vars is not a list of symbols: ~S" vars)))))

(sb-xc:defmacro multiple-value-bind (vars value-form &body body)
  (validate-vars vars)
  (if (= (length vars) 1)
      ;; Not only does it look nicer to reduce to LET in this special case,
      ;; if might produce better code or at least compile quicker.
      ;; Certainly for the evaluator it's preferable.
      `(let ((,(car vars) ,value-form))
         ,@body)
      (flet ((maybe-list (x) (if (member x lambda-list-keywords) (list x) x)))
        (let ((ignore '#:ignore))
          `(multiple-value-call #'(lambda (&optional ,@(mapcar #'maybe-list vars)
                                         &rest ,ignore)
                                  (declare (ignore ,ignore))
                                  ,@body)
                              ,value-form)))))

(sb-xc:defmacro multiple-value-setq (vars value-form)
  (validate-vars vars)
  ;; MULTIPLE-VALUE-SETQ is required to always return just the primary
  ;; value of the value-from, even if there are no vars. (SETF VALUES)
  ;; in turn is required to return as many values as there are
  ;; value-places, hence this:
  (if vars
      `(values (setf (values ,@vars) ,value-form))
      `(values ,value-form))))

(sb-xc:defmacro multiple-value-list (value-form)
  `(multiple-value-call #'list ,value-form))

(sb-xc:defmacro nth-value (n form &environment env)
  "Evaluate FORM and return the Nth value (zero based)
 without consing a temporary list of values."
  ;; FIXME: The above is true, if slightly misleading.  The
  ;; MULTIPLE-VALUE-BIND idiom [ as opposed to MULTIPLE-VALUE-CALL
  ;; (LAMBDA (&REST VALUES) (NTH N VALUES)) ] does indeed not cons at
  ;; runtime.  However, for large N (say N = 200), COMPILE on such a
  ;; form will take longer than can be described as adequate, as the
  ;; optional dispatch mechanism for the M-V-B gets increasingly
  ;; hairy.
  (let ((val (and (constantp n env) (constant-form-value n env))))
    (if (and (integerp val) (<= 0 val (or #+(or x86-64 arm64 riscv) ;; better DEFAULT-UNKNOWN-VALUES
                                          1000
                                          10))) ; Arbitrary limit.
        (let ((dummy-list (make-gensym-list val))
              (keeper (gensym "KEEPER")))
          `(multiple-value-bind (,@dummy-list ,keeper) ,form
             (declare (ignore ,@dummy-list))
             ,keeper))
      ;; &MORE conversion handily deals with non-constant N,
      ;; avoiding the unstylish practice of inserting FORM into the
      ;; expansion more than once to pick off a few small values.
      ;; This is not as good as above, because it uses TAIL-CALL-VARIABLE.
        `(multiple-value-call
             (lambda (n &rest list) (nth (truly-the index n) list))
           (the index ,n) ,form))))


;;;; ASSERT and CHECK-TYPE

;;; ASSERT is written this way, to call ASSERT-ERROR, because of how
;;; closures are compiled. RESTART-CASE has forms with closures that
;;; the compiler causes to be generated at the top of any function
;;; using RESTART-CASE, regardless of whether they are needed. Thus if
;;; we just wrapped a RESTART-CASE around the call to ERROR, we'd have
;;; to do a significant amount of work at runtime allocating and
;;; deallocating the closures regardless of whether they were ever
;;; needed.
(sb-xc:defmacro assert (test-form &optional places datum &rest arguments
                            &environment env)
  "Signals an error if the value of TEST-FORM is NIL. Returns NIL.

   Optional DATUM and ARGUMENTS can be used to change the signaled
   error condition and are interpreted as in (APPLY #'ERROR DATUM
   ARGUMENTS).

   Continuing from the signaled error using the CONTINUE restart will
   allow the user to alter the values of the SETFable locations
   specified in PLACES and then start over with TEST-FORM.

   If TEST-FORM is of the form

     (FUNCTION ARG*)

   where FUNCTION is a function (but not a special operator like
   CL:OR, CL:AND, etc.) the results of evaluating the ARGs will be
   included in the error report if the assertion fails."
  (collect ((bindings) (infos))
    (let* ((func (if (listp test-form) (car test-form)))
           (new-test
            (if (and (typep func '(and symbol (not null)))
                     (not (macro-function func env))
                     (not (special-operator-p func))
                     (proper-list-p (cdr test-form)))
                ;; TEST-FORM is a function call. We do not attempt this
                ;; if TEST-FORM is a macro invocation or special form.
                `(,func ,@(mapcar (lambda (place)
                                    (if (constantp place env)
                                        place
                                        (with-unique-names (temp)
                                          (bindings `(,temp ,place))
                                          (infos `',place)
                                          (infos temp)
                                          temp)))
                                  (rest test-form)))
                ;; For all other cases, just evaluate TEST-FORM
                ;; and don't report any details if the assertion fails.
                test-form))
           (try '#:try)
           (done  '#:done))
      ;; If TEST-FORM, potentially using values from BINDINGS, does not
      ;; hold, enter a loop which reports the assertion error,
      ;; potentially changes PLACES, and retries TEST-FORM.
      `(tagbody
        ,try
          (let ,(bindings)
            (when ,new-test
              (go ,done))

            (assert-error ',test-form
                          ,@(and (infos)
                                 `(,(/ (length (infos)) 2)))
                          ,@(infos)
                          ,@(and (or places datum
                                     arguments)
                                 `(',places))
                          ,@(and (or places datum
                                     arguments)
                                 `(,datum))
                          ,@arguments))
          ,@(mapcar (lambda (place)
                      `(setf ,place (assert-prompt ',place ,place)))
                    places)
          (go ,try)
        ,done))))

(defun assert-prompt (name value)
  (cond ((y-or-n-p "The old value of ~S is ~S.~
                    ~%Do you want to supply a new value? "
                   name value)
         (format *query-io* "~&Type a form to be evaluated:~%")
         (eval (read *query-io*)))
        (t value)))

;;; CHECK-TYPE is written this way, to call CHECK-TYPE-ERROR, because
;;; of how closures are compiled. RESTART-CASE has forms with closures
;;; that the compiler causes to be generated at the top of any
;;; function using RESTART-CASE, regardless of whether they are
;;; needed. Because it would be nice if CHECK-TYPE were cheap to use,
;;; and some things (e.g., READ-CHAR) can't afford this excessive
;;; consing, we bend backwards a little.
(sb-xc:defmacro check-type (place type &optional type-string
                                &environment env)
  "Signal a restartable error of type TYPE-ERROR if the value of PLACE
is not of the specified type. If an error is signalled and the restart
is used to return, this can only return if the STORE-VALUE restart is
invoked. In that case it will store into PLACE and start over."
  ;; Detect a common user-error.
  (when (and (consp type) (eq 'quote (car type)))
    (error 'simple-reference-error
           :format-control "Quoted type specifier in ~S: ~S"
           :format-arguments (list 'check-type type)
           :references '((:ansi-cl :macro check-type))))
  ;; KLUDGE: We use a simpler form of expansion if PLACE is just a
  ;; variable to work around Python's blind spot in type derivation.
  ;; For more complex places getting the type derived should not
  ;; matter so much anyhow.
  (let ((expanded (%macroexpand place env))
        (type (let ((ctype (sb-c::careful-specifier-type type)))
                (if ctype
                    (type-specifier ctype)
                    type))))
    (if (symbolp expanded)
        `(do ()
             ((typep ,place ',type))
           (setf ,place (check-type-error ',place ,place ',type
                                          ,@(and type-string
                                                 `(,type-string)))))
        (let ((value (gensym)))
          `(do ((,value ,place ,place))
               ((typep ,value ',type))
             (setf ,place
                   (check-type-error ',place ,value ',type
                                     ,@(and type-string
                                            `(,type-string)))))))))

;;;; DEFINE-SYMBOL-MACRO

(sb-xc:defmacro define-symbol-macro (name expansion)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (sb-c::%define-symbol-macro ',name ',expansion (sb-c:source-location))))

(defun sb-c::%define-symbol-macro (name expansion source-location)
  (unless (symbolp name)
    (error 'simple-type-error :datum name :expected-type 'symbol
           :format-control "Symbol macro name is not a symbol: ~S."
           :format-arguments (list name)))
  (with-single-package-locked-error
      (:symbol name "defining ~A as a symbol-macro"))
  (let ((kind (info :variable :kind name)))
    (case kind
     ((:macro :unknown)
      (when source-location
        (setf (info :source-location :symbol-macro name) source-location))
      (setf (info :variable :kind name) :macro)
      (setf (info :variable :macro-expansion name) expansion))
     (t
      (%program-error "Symbol ~S is already defined as ~A."
                      name (case kind
                             (:alien "an alien variable")
                             (:constant "a constant")
                             (:special "a special variable")
                             (:global "a global variable")
                             (t kind))))))
  name)

;;;; DEFINE-COMPILER-MACRO

(sb-xc:defmacro define-compiler-macro (name lambda-list &body body)
  "Define a compiler-macro for NAME."
  (check-designator name 'define-compiler-macro
      #'legal-fun-name-p "function name")
  (when (and (symbolp name) (special-operator-p name))
    (%program-error "cannot define a compiler-macro for a special operator: ~S"
                    name))
  ;; DEBUG-NAME is called primarily for its side-effect of asserting
  ;; that (COMPILER-MACRO-FUNCTION x) is not a legal function name.
  (let ((def (make-macro-lambda (sb-c::debug-name 'compiler-macro name)
                                lambda-list body 'define-compiler-macro name
                                :accessor 'sb-c::compiler-macro-args)))
    ;; FIXME: Shouldn't compiler macros also get source locations?
    ;; Plain DEFMACRO supplies source location information.
    `(progn
       (eval-when (:compile-toplevel)
         (sb-c::%compiler-defmacro :compiler-macro-function ',name))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (sb-c::%define-compiler-macro ',name ,def)))))

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun sb-c::%define-compiler-macro (name definition)
    (sb-c::warn-if-compiler-macro-dependency-problem name)
    ;; FIXME: warn about incompatible lambda list with
    ;; respect to parent function?
    (setf (compiler-macro-function name) definition)
    name))

;;;; CASE, TYPECASE, and friends

;;; Make this a full warning during SBCL build.
#+sb-xc ; Don't redefine if recompiling in a warm REPL
(define-condition duplicate-case-key-warning (#-sb-xc-host style-warning #+sb-xc-host warning)
  ((key :initarg :key
        :reader case-warning-key)
   (case-kind :initarg :case-kind
              :reader case-warning-case-kind)
   (occurrences :initarg :occurrences
                :type list
                :reader duplicate-case-key-warning-occurrences))
  (:report
    (lambda (condition stream)
      (format stream
        "Duplicate key ~S in ~S form, ~
         occurring in~{~#[~; and~]~{ the ~:R clause:~%~<  ~S~:>~}~^,~}."
        (case-warning-key condition)
        (case-warning-case-kind condition)
        (duplicate-case-key-warning-occurrences condition)))))

;;; Return three values:
;;; 1. an array of LAYOUT
;;; 2. an array of (unsigned-byte 16) for the clause index to select
;;; 3. an expression mapping each layout in LAYOUT-LISTS to an integer 0..N-1
(defun build-sealed-struct-typecase-map (layout-lists hashes)
  ;; The hash-generator emulator wants a cookie identifying the set of objects
  ;; that were hashed.
  (let ((lambda (sb-c:make-perfect-hash-lambda
                 hashes
                 #+sb-xc-host
                 (map 'vector
                      (lambda (list)
                        (mapcar (lambda (layout)
                                  (list :type (classoid-name (layout-classoid layout))))
                                list))
                      layout-lists))))
    (unless lambda
      (return-from build-sealed-struct-typecase-map (values nil nil nil)))
    (let* ((phashfun (sb-c::compile-perfect-hash lambda hashes))
           (n (length hashes))
           (domain (make-array n :initial-element nil))
           (range (sb-xc:make-array n :element-type '(unsigned-byte 16))))
      (loop for clause-index from 1 for list across layout-lists
          do (dolist (layout list)
               (let* ((hash (ldb (byte 32 0) (layout-clos-hash layout)))
                      (index (funcall phashfun hash)))
                 (aver (null (aref domain index)))
                 (setf (aref domain index) layout
                       (aref range index) clause-index))))
      (values domain range lambda))))

(declaim (ftype function sb-pcl::emit-cache-lookup))
(defun optimize-%typecase-index (layout-lists object sealed)
  ;; If no new subtypes can be defined, then there is a compiled-time-computable
  ;; mapping from CLOS-hash to jump table index.
  ;; Try the hash-based expansion if applicable. It's allowed to fail, as it will
  ;; when 32-bit hashes are nonunique.
  (when sealed
    ;; TODO: this could eliminate an array lookup when there is only a single layout
    ;; per clause. So instead of the perfect hash identifying a clause index via lookup,
    ;; the hash _is_ the clause index; the clauses in the CASE need to be permuted
    ;; to match the hashes, which doesn't work in the way TYPECASE currently expands.
    (let ((seen-layouts)
          (expanded-lists (make-array (length layout-lists) :initial-element nil))
          (index 0))
      (flet ((add-to-clause (layout)
               (unless (member layout seen-layouts)
                 (push layout seen-layouts)
                 (push layout (aref expanded-lists index)))))
        (dovector (layouts layout-lists)
          (dolist (layout layouts)
            ;; unless this layout was in a prior clause
            (when (add-to-clause layout)
              (sb-kernel::do-subclassoids ((classoid layout) (layout-classoid layout))
               (declare (ignore classoid))
               (add-to-clause layout))))
          (incf index)))
      (let ((hashes (map '(array (unsigned-byte 32) (*))
                         (lambda (x) #+64-bit (ldb (byte 32 0) (layout-clos-hash x))
                                     #-64-bit (layout-clos-hash x))
                         seen-layouts)))
        (multiple-value-bind (layouts indices expr)
            (build-sealed-struct-typecase-map expanded-lists hashes)
          (when expr
            (return-from optimize-%typecase-index
              `(truly-the
                (integer 0 ,(length layout-lists))
                (if (not (%instancep ,object))
                    0
                    (let* ((l (%instance-layout ,object)) ; layout
                           (h (,expr (ldb (byte 32 0) (layout-clos-hash l))))) ; perfect hash
                      (if (and (< h ,(length layouts)) (eq l (svref ,layouts h)))
                          (aref ,indices h)
                          0))))))))))
  ;; The generated s-expression is too sensitive to the order LOAD-TIME-VALUE fixups are
  ;; patched in by cold-init. You'll have a bad time if CACHE-CELL is an unbound-marker.
  #+sb-xc-host (error "PCL cache won't work for cross-compiled TYPECASE")
  ;; Use a PCL cache when the sealed logic was inapplicable (or failed due to hash collisions).
  ;; A cache is usually an improvement over sequential tests but it's impossible to know
  ;; (the first clause could get taken 99% of the time)
  (let ((n (length layout-lists)))
    `(truly-the
      (integer 0 ,n)
      (if (not (%instancep ,object))
          0
          ;; TODO: if we access the cache by layout ID instead of the object,
          ;; one word can store the layout ID and the clause index
          ;; (certainly true for 64-bit, maybe not 32).
          ;; The benefit would be never having to observe a key lacking a value.
          ;; Also: we don't really need the test for the object layout's hash is 0
          ;; because it can't be. On the other hand, we might want to utilize
          ;; this macro on STANDARD-OBJECT.
          (prog* ((clause 0)
                  (cache-cell
                   (load-time-value
                    ;; Assume the cache will want at least N lines
                    (cons (sb-pcl::make-cache :key-count 1 :size ,n :value t)
                          ,layout-lists)))
                  (cache (car (truly-the cons cache-cell)))
                  (layout (%instance-layout ,object)))
             (declare (optimize (safety 0)))
             ,(sb-pcl::emit-cache-lookup 'cache '(layout) 'miss 'clause)
             (return clause)
           MISS
             (return (sb-pcl::%struct-typecase-miss ,object cache-cell)))))))

;;; Decide whether to bind EXPR to a random gensym or a COPY-SYMBOL, or not at all,
;;; for purposes of CASE/TYPECASE. Lexical vars don't require rebinding because
;;; no SET can occur in dispatching to a clause, and multiple refs are devoid
;;; of side-effects (such as UNBOUND-SYMBOL or undefined-alien trap)
(defun choose-tempvar (bind expr env)
  (let ((bind
         (cond ((or bind (consp expr)) t)
               ((not (symbolp expr)) nil)
               (t
                (let ((found (and (sb-c::lexenv-p env)
                                  (sb-c:lexenv-find expr vars :lexenv env))))
                  (cond ((or (sb-c::global-var-p found)
                             (listp found) ; special, macro, or not found
                             (eq found :bogus)) ; PCL walker shenanigans
                         t)
                        ((sb-c::lambda-var-specvar found)
                         (bug "can't happen"))
                        (t
                         nil)))))))
    (cond ((not bind) expr)
          ((and (symbolp expr)
                ;; Some broken 3rd-party code walker is confused by #:_
                ;; and this hack of forcing a random gensym seems
                ;; to partially cure whatever the problem is.
                (string/= expr "_"))
           (copy-symbol expr))
          (t
           (gensym)))))

;;; Given an arbitrary TYPECASE, see if it is a discriminator over
;;; an assortment of structure-object subtypes. If it is, potentially turn it
;;; into a dispatch based on layout-clos-hash.
;;; The decision to use a hash-based lookup should depend on the number of types
;;; matched, but if there are a lot of types matched all rooted at a common
;;; ancestor, it may not be as beneficial.
;;;
;;; The expansion currently works only with sealed classoids.
;;; Making it work with unsealed classoids isn't too tough.
;;; The dispatch will look something like a PCL cache but simpler.
;;; First of all, there's no reason we can't use stable hashes
;;; for structure layouts, because an incompatibly redefined structure
;;; (which is unportable to begin with), doesn't require a new hash.
;;; In fact as far as I can tell, redefining a standard class doesn't require a new hash
;;; because the obsolete layout always gets clobbered to 0, and cache lookups always check
;;; for a match on both the hash and the layout.
(defun expand-struct-typecase (keyform normal-clauses type-specs default errorp)
  (let* ((n (length type-specs))
         (n-base-types 0)
         (layout-lists (make-array n))
         (exhaustive-list) ; of classoids
         (temp (choose-tempvar t keyform nil))
         (all-sealed t))
    (labels
        ((ok-classoid (classoid)
           ;; Return T if this is a classoid this expander can work with.
           ;; Also figure if all classoids accepted by this test were sealed.
           (when (or (structure-classoid-p classoid)
                     (and (sb-kernel::built-in-classoid-p classoid)
                          (not (memq (classoid-name classoid)
                                     sb-kernel::**non-instance-classoid-types**))))
             ;; If this classoid is sealed, then its children are sealed too,
             ;; and we don't need to verify that.
             (unless (eq (classoid-state classoid) :sealed)
               (setq all-sealed nil))
             (sb-kernel::do-subclassoids ((subclassoid layout) classoid)
               (declare (ignore layout))
               (pushnew subclassoid exhaustive-list))
             t))
         (get-layouts (ctype)
           (let ((list
                  (cond ((ok-classoid ctype) (list (classoid-layout ctype)))
                        ((and (union-type-p ctype)
                              (every #'ok-classoid (union-type-types ctype)))
                         (mapcar 'classoid-layout (union-type-types ctype))))))
             (incf n-base-types (length list))
             list)))
      ;; For each clause, if it effectively an OR over acceptable instance types,
      ;; collect the layouts of those types.
      (loop for i from 0 for spec in type-specs
            do (let ((parse (specifier-type spec)))
                 (setf (aref layout-lists i) (or (get-layouts parse)
                                                 (return-from expand-struct-typecase nil)))))
      ;; The number of base types is an upper bound on the number of different TYPEP
      ;; executions that could occur.
      ;; Let's say 1 to 4 TYPEP tests isn't to bad. Just do them sequentially.
      ;; But given something like:
      ;; (typecase x
      ;;   ((or parent1 parent2 parent3) ...)
      ;;   ((or parent4 parent5 parent6) ...)
      ;; where eaach parent has dozens of children (directly or indirectly),
      ;; it may be worse to use a hash-based lookup.
      (when (or (< n-base-types 5) ; too few cases
                (> (length exhaustive-list) (* 3 n-base-types))) ; too much "bloat"
        (return-from expand-struct-typecase))
      ;; I don't know if these criteria are sane: Use hashing only if either all sealed,
      ;; or very large? Why is this an additional restriction beyond the above heuristics?
      (when (or all-sealed (>= n-base-types 8))
        `(let ((,temp ,keyform))
           (case (sb-kernel::%typecase-index ,layout-lists ,temp ,all-sealed)
             ,@(loop for i from 1 for clause in normal-clauses
                     collect `(,i
                                   ;; CLAUSE is ((TYPEP #:G 'a-type) . forms)
                                   (sb-c::%type-constraint ,temp ,(third (car clause)))
                                   ,@(cdr clause)))
             (0 ,@(if errorp
                          `((etypecase-failure ,temp ',type-specs))
                          (cdr default)))))))))

(defun should-attempt-hash-based-case-dispatch (keys)
  ;; Guess a good minimum table size, with a slight bias against using the xperfecthash files.
  ;; If there are a mixture of key types, penalize slightly be requiring a larger minimum
  ;; number of keys. If we don't do that, then the expression can have a ridiculous amount
  ;; of math in it that would surely outweigh any savings over an IF/ELSE chain.
  ;; Technically I should generate the perfect hash function and then decide how costly it is
  ;; using PHASH-CONVERT-TO-2-OPERAND-CODE as the cost model (number of instructions).
  (let ((minimum #+sb-xc-host 5 #-sb-xc-host 4))
    (when (and (some #'symbolp keys) (or (some #'integerp keys) (some #'characterp keys)))
      (incf minimum 2))
    (>= (length keys) minimum)))

(defun wrap-if (condition with form)
  (if condition
      (append with (list form))
      form))

;;; CASE-BODY returns code for all the standard "case" macros. NAME is
;;; the macro name, and KEYFORM is the thing to case on.
;;; When ERRORP, no OTHERWISE-CLAUSEs are recognized,
;;; and an ERROR or CERROR form is generated where control falls off the end
;;; of the ordinary clauses.

;;; Note the absence of EVAL-WHEN here. The cross-compiler calls this function
;;; and gets the compiled code that the host produced in make-host-1.
;;; If recompiled, you do not want an interpreted definition that might come
;;; from EVALing a toplevel form - the stack blows due to infinite recursion.
(defun case-body (whole lexenv test errorp
                  &aux (clauses ())
                       (case-clauses (if (eq test 'typep) '(0))) ; generalized boolean
                       (keys))
  (destructuring-bind (name keyform &rest specified-clauses
                       &aux (keyform-value (choose-tempvar (eq errorp 'cerror)
                                                           keyform lexenv)))
      whole
    (unless (or (cdr whole) (not errorp))
      (warn "no clauses in ~S" name))
    (do* ((cases specified-clauses (cdr cases))
          (clause (car cases) (car cases))
          (keys-seen (make-hash-table :test #'eql))
          (case-position 1 (1+ case-position)))
         ((null cases) nil)
      (flet ((check-clause (case-keys)
               (loop for k in case-keys
                  for existing = (gethash k keys-seen)
                  do (when existing
                       (warn 'duplicate-case-key-warning
                             :key k
                             :case-kind name
                             :occurrences `(,existing (,case-position (,clause))))))
               (let ((record (list case-position (list clause))))
                 (dolist (k case-keys)
                   (setf (gethash k keys-seen) record))))
             (testify (k)
               `(,test ,keyform-value
                       ,(if (and (eq test 'eql) (self-evaluating-p k)) k `',k))))
        (unless (list-of-length-at-least-p clause 1)
          (with-current-source-form (cases)
            (error "~S -- bad clause in ~S" clause name)))
        (with-current-source-form (clause)
          ;; https://sourceforge.net/p/sbcl/mailman/message/11863996/ contains discussion
          ;; of whether to warn when seeing OTHERWISE in a normal-clause position, but
          ;; it is in fact an error: "In the case of case, the symbols t and otherwise
          ;; MAY NOT be used as the keys designator."
          ;; T in CASE heads an otherwise-clause, but in TYPECASE it's either a plain
          ;; type specifier OR it is the otherwise-clause. They're equivalent, but
          ;; EXPAND-STRUCT-TYPECASE has a fixed expectation re. normal and otherwise clause
          (destructuring-bind (keyoid &rest forms) clause
            (when (null forms)
              (setq forms '(nil)))
            (cond (;; an OTHERWISE-CLAUSE
                   (and (not errorp) ; possible only in CASE or TYPECASE,
                                        ; not in [EC]CASE or [EC]TYPECASE
                        (or (eq keyoid 'otherwise)
                            (and (eq keyoid 't) (or (eq test 'eql) (not (cdr cases))))))
                   (cond ((null (cdr cases))
                          (push `(t ,@forms) clauses))
                         ((eq name 'case)
                          (error 'simple-reference-error
                                 :format-control
                            "~@<~IBad ~S clause:~:@_  ~S~:@_~S allowed as the key ~
                           designator only in the final otherwise-clause, not in a ~
                           normal-clause. Use (~S) instead, or move the clause to the ~
                           correct position.~:@>"
                            :format-arguments (list 'case clause keyoid keyoid)
                            :references `((:ansi-cl :macro case))))
                         (t
                          ;; OTHERWISE is a redundant bit of the behavior of TYPECASE
                          ;; since T is the universal type. OTHERWISE could not legally
                          ;; be DEFTYPEed so this _must_ be a misplaced clause.
                          (error 'simple-reference-error
                                     :format-control
                            "~@<~IBad ~S clause:~:@_  ~S~:@_~S is allowed only in the final clause. ~
                           Use T instead, or move the clause to the correct position.~:@>"
                            :format-arguments (list 'typecase clause keyoid)
                            :references `((:ansi-cl :macro typecase))))))
                  ((and (listp keyoid) (eq test 'eql))
                   (unless (proper-list-p keyoid) ; REVERSE would err with unclear message
                     (error "~S is not a proper list" keyoid))
                   (check-clause keyoid)
                   (setf keys (nconc (reverse keyoid) keys))
                   ;; This inserts an unreachable clause if KEYOID is NIL, but
                   ;; FORMS could contain a side-effectful LOAD-TIME-VALUE.
                   (push `(,(cond ((cdr keyoid) `(or ,@(mapcar #'testify keyoid)))
                                  (keyoid (testify (car keyoid))))
                           ,@forms)
                         clauses))
                  (t
                   (when (and (eq test 'typep) (eq keyoid t))
                     ;; - if ERRORP is nil and there are more clauses, this shadows them,
                     ;;   which seems suspicious and worth style-warning for.
                     ;; - if ERRORP is non-nil, though this isn't technically an "otherwise"
                     ;;   clause, in acts just like one.
                     (if errorp
                         (setq errorp :none)))
                   (when case-clauses ; try the TYPECASE into CASE reduction
                     (let ((typespec (ignore-errors (typexpand keyoid))))
                       (cond ((typep typespec '(cons (eql member) (satisfies proper-list-p)))
                              (push (cons (cdr typespec) forms) case-clauses))
                             ((and (eq typespec t) (not (cdr cases))) ; one more KLUDGE
                              (push clause case-clauses))
                             (t
                              (setq case-clauses nil)))))
                   (push keyoid keys)
                   (check-clause (list keyoid))
                   (push `(,(testify keyoid) ,@forms) clauses)))))))
    (when (eq errorp :none)
      (setq errorp nil))

    ;; [EC]CASE has an advantage over [EC]TYPECASE in that we readily notice when
    ;; the expansion can use symbol-hash to pick the clause.
    (when case-clauses
      (return-from case-body
        `(,(cond ((not errorp) 'case) ((eq name 'ctypecase) 'ccase) (t 'ecase))
           ,keyform
          ,@(cdr (reverse case-clauses)) ; 1st elt was a boolean flag
          ,@(when (eq (caar clauses) t) (list (car clauses))))))

    (setq keys
          (nreverse (mapcon (lambda (tail)
                              (unless (member (car tail) (cdr tail))
                                (list (car tail))))
                            keys)))
    (when (eq test 'typep)
      (let (types)
        (loop for key in keys
              for clause in specified-clauses
              do
              (with-current-source-form (clause)
                (let ((type (specifier-type key)))
                  (when (and type
                             (neq type *empty-type*))
                    (let ((existing (loop for (prev . spec) in types
                                          when (and (csubtypep type prev)
                                                    (not (or (and (eq prev (specifier-type 'single-float))
                                                                  (eq key 'short-float))
                                                             #-long-float
                                                             (and (eq prev (specifier-type 'double-float))
                                                                  (eq key 'long-float))
                                                             (and (csubtypep type (specifier-type 'array))
                                                                  ;; Ignore due to upgrading
                                                                  (sb-kernel::ctype-array-any-specialization-p prev)))))
                                          return spec)))
                      (if existing
                          (style-warn "Clause ~s is shadowed by ~s"
                                      key existing)
                          (push (cons type key) types)))))))))
    ;; Try hash-based dispatch only if expanding for the compiler
    (when (and (neq errorp 'cerror)
               (sb-c::compiling-p lexenv)
               ;; See slow-findhash-allowed
               (sb-c:policy lexenv (and (>= speed compilation-speed)
                                        (> sb-c:jump-table 0)))
               (sb-c::vop-existsp :named sb-c:jump-table))
      (let* ((default (if (eq (caar clauses) 't) (car clauses)))
             (normal-clauses (reverse (if default (cdr clauses) clauses))))
        ;; Try expanding a using perfect hash and either a jump table or k/v vectors
        ;; depending on constant-ness of results.
        (cond ((eq test 'typep)
               (awhen (expand-struct-typecase keyform normal-clauses keys
                                              default errorp)
                 (return-from case-body it))))))

    (setq clauses (nreverse clauses))

    (let ((expected-type `(,(if (eq test 'eql) 'member 'or) ,@keys)))
      (when (eq errorp 'cerror) ; CCASE or CTYPECASE
        (return-from case-body
      ;; It is not a requirement to evaluate subforms of KEYFORM once only, but it often
      ;; reduces code size to do so, as the update form will take advantage of typechecks
      ;; already performed. (Nor is it _required_ to re-evaluate subforms)
          (binding* ((switch (make-symbol "SWITCH"))
                     (retry
                      ;; TODO: consider using the union type simplifier algorithm here
                      `(case-body-error ',name ',keyform ,keyform-value ',expected-type ',keys))
                     ((vars vals stores writer reader) (get-setf-expansion keyform)))
          `(let* ,(mapcar #'list vars vals)
             (named-let ,switch ((,keyform-value ,reader))
               (cond ,@clauses
                     (t (multiple-value-bind ,stores ,retry (,switch ,writer)))))))))

      (when (and (eq keyform-value keyform) (not keys))
        (setq keyform-value '#:dummy)) ; force a rebinding to "use" the value
      (let ((switch
             `(cond
                ,@clauses
                ,@(when errorp
                    `((t
                       ,(wrap-if
                         (sb-c::compiling-p lexenv)
                         '(locally (declare (muffle-conditions code-deletion-note)))
                         (ecase name
                           (etypecase
                             `(etypecase-failure
                                 ,keyform-value ,(etypecase-error-spec keys)))
                           (ecase
                             `(ecase-failure ,keyform-value ',keys))))))))))
        (if (eq keyform-value keyform)
            switch
            `(let ((,keyform-value ,keyform))
               ;; binding must be IGNORABLE in either of these expressions:
               ;;   (CASE KEY (() 'res))
               ;;   (CASE KEY (T 'res))
               (declare (ignorable ,keyform-value))
               ,switch))))))

;;; ETYPECASE over clauses that form a "simpler" type specifier should use that,
;;; e.g. partitions of INTEGER:
;;;  (etypecase ((integer * -1) ...) ((eql 0) ...) ((integer 1 *) ...))
;;;  (etypecase (fixnum ...) (bignum ...))
(defun etypecase-error-spec (types)
  (when (cdr types) ; no sense in doing this for a single type
    (let ((parsed (mapcar #'sb-c::careful-specifier-type types)))
      (when (every (lambda (x) (and x (not (contains-unknown-type-p x))))
                   parsed)
        (let* ((union (apply #'type-union parsed))
               (unparsed (type-specifier union)))
          ;; If the type-union of the types is a simpler expression than (OR ...),
          ;; then return the simpler one. CTYPECASE could do this also, but doesn't.
          ;; http://www.lispworks.com/documentation/HyperSpec/Body/m_tpcase.htm#etypecase
          ;;   "If no normal-clause matches, a non-correctable error of type type-error
          ;;    is signaled. The offending datum is the test-key and the expected type
          ;;    is /type equivalent/ to (or type1 type2 ...)"
          (when (symbolp unparsed)
            (return-from etypecase-error-spec `',unparsed))))))
  ;; This constant can make its way into generic function dispatch.
  ;; The compiled code must not to point to an arena if one is active.
  `',(ensure-heap-list types))

(sb-xc:defmacro case (&whole form &environment env &rest r)
  (declare (sb-c::lambda-list (keyform &body cases)) (ignore r))
  "CASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If a singleton key is T then the clause is a default clause."
  (case-body form env 'eql nil))

(sb-xc:defmacro ccase (&whole form &environment env &rest r)
  (declare (sb-c::lambda-list (keyform &body cases)) (ignore r))
  "CCASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If none of the keys matches then a correctable error is
  signalled."
  (case-body form env 'eql 'cerror))

(sb-xc:defmacro ecase (&whole form &environment env &rest r)
  (declare (sb-c::lambda-list (keyform &body cases)) (ignore r))
  "ECASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If none of the keys matches then an error is signalled."
  (case-body form env 'eql 'error))

(sb-xc:defmacro typecase (&whole form &environment env &rest r)
  (declare (sb-c::lambda-list (keyform &body cases)) (ignore r))
  "TYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true."
  (case-body form env 'typep nil))

(sb-xc:defmacro ctypecase (&whole form &environment env &rest r)
  (declare (sb-c::lambda-list (keyform &body cases)) (ignore r))
  "CTYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true. If no form is satisfied then a correctable error is signalled."
  (case-body form env 'typep 'cerror))

(sb-xc:defmacro etypecase (&whole form &environment env &rest r)
  (declare (sb-c::lambda-list (keyform &body cases)) (ignore r))
  "ETYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true. If no form is satisfied then an error is signalled."
  (case-body form env 'typep 'error))

;;; Compile a version of BODY for all TYPES, and dispatch to the
;;; correct one based on the value of VAR. This was originally used
;;; only for strings, hence the name. Renaming it to something more
;;; generic might not be a bad idea.
(sb-xc:defmacro string-dispatch ((&rest types) var &body body)
  (let ((fun (gensym "STRING-DISPATCH-FUN")))
    `(flet ((,fun (,var)
              ,@body))
       (declare (inline ,fun))
       (etypecase ,var
         ,@(loop for type in types
                 ;; TRULY-THE allows transforms to take advantage of the type
                 ;; information without need for constraint propagation.
                 collect `(,type (,fun (truly-the ,type ,var))))))))

;;;; WITH-FOO i/o-related macros

(sb-xc:defmacro with-open-stream ((var stream) &body body)
  (multiple-value-bind (forms decls) (parse-body body nil)
    `(let ((,var ,stream))
       ,@decls
       (unwind-protect
            (progn ,@forms)
         (close ,var)))))

(sb-xc:defmacro with-open-file ((stream filespec &rest options)
                                &body body)
  (multiple-value-bind (forms decls) (parse-body body nil)
    (let ((abortp (gensym)))
      `(let ((,stream (open ,filespec ,@options))
             (,abortp t))
         ,@decls
         (unwind-protect
              (multiple-value-prog1
                  (progn ,@forms)
                (setq ,abortp nil))
           (when ,stream
             (close ,stream :abort ,abortp)))))))

;;;; Iteration macros:

(flet
    ((frob-do-body (varlist endlist decls-and-code bind step name block)
       ;; Check for illegal old-style DO.
       (when (not (listp varlist))
         (with-current-source-form (varlist)
           (error "~@<Ill-formed ~S variable list -- possibly illegal ~
                   old style DO?~@:>"
                  name)))
       (when (atom endlist)
         (with-current-source-form (endlist)
           (error "~@<Ill-formed ~S end test list -- possibly illegal ~
                  old style DO?~@:>"
                  name)))
       (collect ((steps))
         (let ((inits
                (with-current-source-form (varlist)
                  (mapcar (lambda (var)
                            (with-current-source-form (var)
                              (or (cond ((symbolp var) (list var))
                                        ((listp var)
                                         (unless (symbolp (first var))
                                           (error "~S step variable is not a symbol: ~S"
                                                  name (first var)))
                                         (case (length var)
                                           ((1 2) var)
                                           (3 (steps (first var) (third var))
                                              (list (first var) (second var))))))
                                  (error "~S is an illegal form for a ~S varlist."
                                         var name))))
                          varlist))))
           (multiple-value-bind (code decls) (parse-body decls-and-code nil)
             (let ((label-1 (gensym)) (label-2 (gensym)))
               `(block ,block
                  (,bind ,inits
                    ,@decls
                    (declare (ignorable ,@(mapcar #'car inits)))
                    (tagbody
                     (go ,label-2)
                     ,label-1
                     (tagbody ,@code)
                     (,step ,@(steps))
                     ,label-2
                     (unless ,(first endlist) (go ,label-1))
                     (return-from ,block (progn ,@(rest endlist))))))))))))

  ;; This is like DO, except it has no implicit NIL block.
  (sb-xc:defmacro do-anonymous (varlist endlist &rest body)
    (frob-do-body varlist endlist body 'let 'psetq 'do-anonymous (gensym)))

  (sb-xc:defmacro do (varlist endlist &body body)
  "DO ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
  Iteration construct. Each Var is initialized in parallel to the value of the
  specified Init form. On subsequent iterations, the Vars are assigned the
  value of the Step form (if any) in parallel. The Test is evaluated before
  each evaluation of the body Forms. When the Test is true, the Exit-Forms
  are evaluated as a PROGN, with the result being the value of the DO. A block
  named NIL is established around the entire expansion, allowing RETURN to be
  used as an alternate exit mechanism."
  (frob-do-body varlist endlist body 'let 'psetq 'do nil))

  (sb-xc:defmacro do* (varlist endlist &body body)
  "DO* ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
  Iteration construct. Each Var is initialized sequentially (like LET*) to the
  value of the specified Init form. On subsequent iterations, the Vars are
  sequentially assigned the value of the Step form (if any). The Test is
  evaluated before each evaluation of the body Forms. When the Test is true,
  the Exit-Forms are evaluated as a PROGN, with the result being the value
  of the DO. A block named NIL is established around the entire expansion,
  allowing RETURN to be used as an alternate exit mechanism."
  (frob-do-body varlist endlist body 'let* 'setq 'do* nil)))

(sb-xc:defmacro dotimes ((var count &optional (result nil)) &body body)
  ;; A nice optimization would be that if VAR is never referenced,
  ;; it's slightly more efficient to count backwards, but that's tricky.
  (let ((c (if (integerp count) count (gensym))))
    `(do ((,var 0 (1+ ,var))
          ,@(if (symbolp c) `((,c (the integer ,count)))))
         ((>= ,var ,c) ,result)
       (declare (type unsigned-byte ,var))
       ,@body)))

(defun segregate-dolist-decls (var decls)
  (collect ((bound-type-decls)
            (bound-nontype-decls)
            (free-decls))
    (dolist (decl decls)
      (aver (eq (car decl) 'declare))
      (dolist (expr (cdr decl))
        (let ((head (car expr))
              (tail (cdr expr)))
          (cond ((consp head) ; compound type specifier
                 (when (member var tail) (bound-type-decls head))
                 (awhen (remove var tail) (free-decls `(,head ,@it))))
                ((not (symbolp head)) (free-decls expr)) ; bogus
                (t
                 (case head
                   ((special dynamic-extent)
                    ;; dynamic-extent makes no sense but this logic has to correctly
                    ;; recognize all the standard atoms that DECLARE accepts.
                    (when (member var tail) (bound-nontype-decls `(,head ,var)))
                    (awhen (remove var tail) (free-decls `(,head ,@it))))
                   (type
                    (when (member var (cdr tail)) (bound-type-decls (cadr expr)))
                    (awhen (remove var (cdr tail)) (free-decls `(type ,(cadr expr) ,@it))))
                   ((ignore ignorable)
                    (awhen (remove var tail) (free-decls `(,head ,@it))))
                   ((optimize ftype inline notinline maybe-inline
                     muffle-conditions unmuffle-conditions)
                    (free-decls expr))
                   (t
                    ;; Assume that any decl pertaining to bindings must have the symbol appear
                    ;; in TAIL. Is this true of custom decls? I would certainly think so.
                    (cond ((not (member var tail)) (free-decls expr))
                          ((info :declaration :known head)
                           ;; Declaimed declaration can't be a type decl.
                           (bound-nontype-decls expr))
                          ((not (sb-c::careful-specifier-type head))
                           ;; If can't be parsed, then what is it? A free decl is as good as anything
                           (free-decls expr))
                          ((contains-unknown-type-p (sb-c::careful-specifier-type head))
                           ;; Stuff it into bound-nontype decls which is no worse
                           ;; than what FILTER-DOLIST-DECLARATIONS could do.
                           (bound-nontype-decls expr))
                          (t
                           ;; A valid type declaration can pertain to some non-bound vars and/or
                           ;; the bound var, nicely handling (STRING x y iterationvar).
                           (when (member var tail) (bound-type-decls head))
                           (awhen (remove var tail) (free-decls `(,head ,@it))))))))))))
    (values (mapcar (lambda (x) `(type ,x ,var)) (bound-type-decls))
            (mapcar (lambda (x) `(type (or null ,x) ,var)) (bound-type-decls))
            (bound-nontype-decls)
            (free-decls))))

(sb-xc:defmacro dolist ((var list &optional (result nil)) &body body &environment env)
  ;; We repeatedly bind the var instead of setting it so that we never
  ;; have to give the var an arbitrary value such as NIL (which might
  ;; conflict with a declaration). If there is a result form, we
  ;; introduce a gratuitous binding of the variable to NIL without the
  ;; declarations, then evaluate the result form in that
  ;; environment. We spuriously reference the gratuitous variable,
  ;; since we don't want to use IGNORABLE on what might be a special
  ;; var.
  (binding* (((forms decls) (parse-body body nil))
             ((iter-type-decl res-type-decl other-decl free-decl)
              (segregate-dolist-decls var decls))
             (n-list (gensym "LIST"))
             (start (gensym "START"))
             ((clist members clist-ok)
              (with-current-source-form (list)
                (cond
                  ((constantp list env)
                   (binding* ((value (constant-form-value list env))
                              ((all dot) (list-members value :max-length 20)))
                     (when (eql dot t)
                       ;; Full warning is too much: the user may terminate the loop
                       ;; early enough. Contents are still right, though.
                       (style-warn "Dotted list ~S in DOLIST." value))
                     (if (eql dot :maybe)
                         (values value nil nil)
                         (values value all t))))
                  ((and (consp list) (eq 'list (car list))
                        (every (lambda (arg) (constantp arg env)) (cdr list)))
                   (let ((values (mapcar (lambda (arg) (constant-form-value arg env)) (cdr list))))
                     (values values values t)))
                  (t
                   (values nil nil nil))))))
    `(block nil
       (let ((,n-list ,(if clist-ok
                           (list 'quote clist)
                           ;; Don't want to use a cast because
                           ;; the type will actually be checked by ENDP first.
                           ;; But it doesn't detect the mismatch because the SETF
                           ;; mixes in T with the initial type.
                           `(the* (list :use-annotations t :source-form ,list) ,list))))
         ,@(when free-decl `((declare ,@free-decl)))
         (tagbody
            ,start
            (unless (endp ,n-list)
              (let ((,var ,(if clist-ok
                               `(truly-the (member ,@members) (car ,n-list))
                               `(car ,n-list))))
                (declare ,@iter-type-decl ,@other-decl (ignorable ,var))
                (setq ,n-list (cdr ,n-list))
                (tagbody ,@forms))
              (go ,start)))
         ;; still within the scope of decls pertinent to other than the VAR binding
         ,@(when result
            `((let ((,var nil))
               ,@(if (or res-type-decl other-decl) `((declare ,@res-type-decl ,@other-decl)))
               ,var
               ,result)))))))


;;;; Miscellaneous macros:

(sb-xc:defmacro lambda (&whole whole args &body body)
  (declare (ignore args body))
  `#',whole)

(sb-xc:defmacro named-lambda (&whole whole name args &body body)
  (declare (ignore name args body))
  `#',whole)

(sb-xc:defmacro declaim (&rest specs)
  "DECLAIM Declaration*
  Do a declaration or declarations for the global environment."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar (lambda (spec)
                 `(sb-c::%proclaim ',spec (sb-c:source-location)))
               specs)))

(sb-xc:defmacro print-unreadable-object ((object stream &key type identity)
                                             &body body)
  "Output OBJECT to STREAM with \"#<\" prefix, \">\" suffix, optionally
  with object-type prefix and object-identity suffix, and executing the
  code in BODY to provide possible further output."
  ;; Note: possibly out-of-order keyword argument evaluation.
  ;; But almost always the :TYPE and :IDENTITY are each literal T or NIL,
  ;; and so the LOGIOR expression reduces to a fixed value from 0 to 3.
  (let ((call `(%print-unreadable-object
                ,object ,stream (logior (if ,type 1 0) (if ,identity 2 0)))))
    (if body
        (let ((fun (make-symbol "THUNK")))
          `(dx-flet ((,fun () (progn ,@body))) (,@call #',fun)))
        call)))

;; A macroexpander helper. Not sure where else to put this.
(defun funarg-bind/call-forms (funarg arg-forms)
  (if (typep funarg
             '(or (cons (eql function) (cons (satisfies legal-fun-name-p) null))
                  (cons (eql quote) (cons symbol null))
                  (cons (eql lambda))))
      (values nil `(funcall ,funarg . ,arg-forms))
    (let ((fn-sym (gensym))) ; for ONCE-ONLY-ish purposes
      (values `((,fn-sym (%coerce-callable-to-fun ,funarg)))
              `(sb-c::%funcall ,fn-sym . ,arg-forms)))))

;;; Ordinarily during self-build, nothing would need this macro except the
;;; calls in src/code/list, and src/code/seq.
;;; However, if cons profiling is enbled, then all calls to COPY-LIST
;;; transform into the macro, so it must be available early.
(sb-xc:defmacro copy-list-macro (input &key check-proper-list)
  ;; Unless CHECK-PROPER-LIST is true, the list is copied correctly
  ;; even if the list is not terminated by NIL. The new list is built
  ;; by CDR'ing SPLICE which is always at the tail of the new list.
  (with-unique-names (orig copy splice cell)
    ;; source transform gave input the ONCE-ONLY treatment already.
    `(when ,input
       (let ((,copy (list (car ,input))))
         (do ((,orig (cdr ,input) (cdr ,orig))
              (,splice ,copy
                       (let ((,cell (list (car ,orig))))
                         (rplacd (truly-the cons ,splice) ,cell)
                         ,cell)))
           (,@(if check-proper-list
                  `((endp ,orig))
                  `((atom ,orig)
                    ;; always store the CDR even if NIL. A blind write
                    ;; is cheaper than a branch around a write.
                    (rplacd (truly-the cons ,splice) ,orig)))
            ,copy))))))

(defun expand-with-output-to-string (var element-type body wild-result-type)
  ;; This is simpler than trying to arrange transforms that cause
  ;; MAKE-STRING-OUTPUT-STREAM to be DXable. While that might be awesome,
  ;; this macro exists for a reason.
  (let* ((initial-buffer '#:buf)
         (dummy '#:stream)
         (string-ctor
           (if (and (sb-xc:constantp element-type)
                    (let ((ctype (sb-c::careful-specifier-type
                                  (constant-form-value element-type))))
                      (and ctype
                           (csubtypep ctype (specifier-type 'character))
                           (neq ctype *empty-type*))))
               ;; Using MAKE-ARRAY avoids a style-warning if et is 'STANDARD-CHAR:
               ;; "The default initial element #\Nul is not a STANDARD-CHAR."
               'make-array ; hooray! it's known be a valid string type
               ;; Force a runtime STRINGP check unless futher transforms
               ;; deduce a known type.
               'make-string)))
    ;; A full call to MAKE-STRING-OUTPUT-STREAM uses a larger initial buffer
    ;; if BASE-CHAR but I really don't care to think about that here.
    `(let ((,initial-buffer (,string-ctor 31 :element-type ,element-type)))
       (declare (dynamic-extent ,initial-buffer))
       (dx-let ((,dummy (%allocate-string-ostream)))
         (let ((,var (%init-string-output-stream ,dummy ,initial-buffer
                                                 ,wild-result-type)))
           (declare (ignorable ,var))
           ,@body)
         (get-output-stream-string ,dummy)))))


;;;; COMPARE-AND-SWAP
;;;;
;;;; SB-EXT:COMPARE-AND-SWAP is the public API for now.
;;;;
;;;; Internally our interface has CAS, GET-CAS-EXPANSION,
;;;; DEFCAS, and #'(CAS ...) functions.

(defun expand-structure-slot-cas (info name place)
  (let* ((dd (car info))
         (structure (dd-name dd))
         (slotd (cdr info))
         (index (dsd-index slotd))
         (type (dsd-type slotd))
         (casser
           (case (dsd-raw-type slotd)
             ((t) '%instance-cas)
             #+(or arm64 ppc ppc64 riscv x86 x86-64)
             ((word) '%raw-instance-cas/word)
             #+(or arm64 riscv x86 x86-64)
             ((sb-vm:signed-word) '%raw-instance-cas/signed-word))))
    (unless casser
      (error "Cannot use COMPARE-AND-SWAP with structure accessor ~
                for a typed slot: ~S"
             place))
    (when (dsd-read-only slotd)
      (error "Cannot use COMPARE-AND-SWAP with structure accessor ~
                for a read-only slot: ~S"
             place))
    (destructuring-bind (op arg) place
      (aver (eq op name))
      (with-unique-names (instance old new)
        (values (list instance)
                (list `(the ,structure ,arg))
                old
                new
                `(truly-the (values ,type &optional)
                            (,casser ,instance ,index
                                     (the ,type ,old)
                                     (the ,type ,new)))
                `(,op ,instance))))))

;;; FIXME: remove (it's EXPERIMENTAL, so doesn't need to go through deprecation)
(defun get-cas-expansion (place &optional environment)
  "Analogous to GET-SETF-EXPANSION. Returns the following six values:

 * list of temporary variables

 * list of value-forms whose results those variable must be bound

 * temporary variable for the old value of PLACE

 * temporary variable for the new value of PLACE

 * form using the aforementioned temporaries which performs the
   compare-and-swap operation on PLACE

 * form using the aforementioned temporaries with which to perform a volatile
   read of PLACE

Example:

  (get-cas-expansion '(car x))
  ; => (#:CONS871), (X), #:OLD872, #:NEW873,
  ;    (SB-KERNEL:%COMPARE-AND-SWAP-CAR #:CONS871 #:OLD872 :NEW873).
  ;    (CAR #:CONS871)

  (defmacro my-atomic-incf (place &optional (delta 1) &environment env)
    (multiple-value-bind (vars vals old new cas-form read-form)
        (get-cas-expansion place env)
     (let ((delta-value (gensym \"DELTA\")))
       `(let* (,@(mapcar 'list vars vals)
               (,old ,read-form)
               (,delta-value ,delta)
               (,new (+ ,old ,delta-value)))
          (loop until (eq ,old (setf ,old ,cas-form))
                do (setf ,new (+ ,old ,delta-value)))
          ,new))))

EXPERIMENTAL: Interface subject to change."
  ;; FIXME: this seems wrong on two points:
  ;; 1. if TRULY-THE had a CAS expander (which it doesn't) we'd want
  ;;    to use %MACROEXPAND[-1] so as not to lose the "truly-the"-ness
  ;; 2. if both a CAS expander and a macro exist, the CAS expander
  ;;    should be preferred before macroexpanding (just like SETF does)
  (let ((expanded (macroexpand place environment)))
    (flet ((invalid-place ()
             (error "Invalid place to CAS: ~S -> ~S" place expanded)))
      (unless (consp expanded)
        (cond ((and (symbolp expanded)
                    (member (info :variable :kind expanded)
                            '(:global :special)))
               (setq expanded `(symbol-value ',expanded)))
              (t
               (invalid-place))))
      (let ((name (car expanded)))
        (unless (symbolp name)
          (invalid-place))
        (acond
         ((info :cas :expander name)
          ;; CAS expander.
          (funcall it expanded environment))

         ;; Structure accessor
         ((structure-instance-accessor-p name)
          (expand-structure-slot-cas it name expanded))

         ;; CAS function
         (t
          (with-unique-names (old new)
            (let ((vars nil)
                  (vals nil)
                  (args nil))
              (dolist (x (reverse (cdr expanded)))
                (cond ((constantp x environment)
                       (push x args))
                      (t
                       (let ((tmp (gensymify x)))
                         (push tmp args)
                         (push tmp vars)
                         (push x vals)))))
              (values vars vals old new
                      `(funcall #'(cas ,name) ,old ,new ,@args)
                      `(,name ,@args))))))))))


;;; This is what it all comes down to.
;;; Possible todo: implement CAS-WEAK like in C and C++ standards
;;; so that we don't loop-in-a-loop where failure has to re-test
;;; whether some item is in a list, and retry the CAS anyway.
(sb-xc:defmacro cas (place old new &environment env)
  "Synonym for COMPARE-AND-SWAP.

Additionally DEFUN, DEFGENERIC, DEFMETHOD, FLET, and LABELS can be also used to
define CAS-functions analogously to SETF-functions:

  (defvar *foo* nil)

  (defun (cas foo) (old new)
    (cas (symbol-value '*foo*) old new))

First argument of a CAS function is the expected old value, and the second
argument of is the new value. Note that the system provides no automatic
atomicity for CAS functions, nor can it verify that they are atomic: it is up
to the implementor of a CAS function to ensure its atomicity.

EXPERIMENTAL: Interface subject to change."
  ;; It's not necessary that GET-CAS-EXPANSION work on defined alien vars.
  ;; They're not generalized places in the sense that they could hold any object,
  ;; so there's very little point to being more general.
  ;; In particular, allowing ATOMIC-PUSH or ATOMIC-POP on them is wrong.
  (awhen (and (symbolp place)
              (eq (info :variable :kind place) :alien)
              (sb-alien::cas-alien place old new))
    (return-from cas it))
  (multiple-value-bind (temps place-args old-temp new-temp cas-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar #'list temps place-args)
            (,old-temp ,old)
            (,new-temp ,new))
       ,cas-form)))

(sb-xc:defmacro compare-and-swap (place old new)
  "Atomically stores NEW in PLACE if OLD matches the current value of PLACE.
Two values are considered to match if they are EQ. Returns the previous value
of PLACE: if the returned value is EQ to OLD, the swap was carried out.

PLACE must be an CAS-able place. Built-in CAS-able places are accessor forms
whose CAR is one of the following:

 CAR, CDR, FIRST, REST, SVREF, SYMBOL-PLIST, SYMBOL-VALUE, SVREF, SLOT-VALUE
 SB-MOP:STANDARD-INSTANCE-ACCESS, SB-MOP:FUNCALLABLE-STANDARD-INSTANCE-ACCESS,

or the name of a DEFSTRUCT created accessor for a slot whose storage type
is not raw. (Refer to the the \"Efficiency\" chapter of the manual
for the list of raw slot types.  Future extensions to this macro may allow
it to work on some raw slot types.)

In case of SLOT-VALUE, if the slot is unbound, SLOT-UNBOUND is called unless
OLD is EQ to SB-PCL:+SLOT-UNBOUND+ in which case SB-PCL:+SLOT-UNBOUND+ is
returned and NEW is assigned to the slot. Additionally, the results are
unspecified if there is an applicable method on either
SB-MOP:SLOT-VALUE-USING-CLASS, (SETF SB-MOP:SLOT-VALUE-USING-CLASS), or
SB-MOP:SLOT-BOUNDP-USING-CLASS.

Additionally, the PLACE can be a anything for which a CAS-function has
been defined. (See SB-EXT:CAS for more information.)
"
  `(cas ,place ,old ,new))


;;;; ATOMIC-INCF and ATOMIC-DECF

(defun expand-atomic-frob
    (name specified-place diff env
     &aux (place (macroexpand specified-place env)))
  (declare (type (member atomic-incf atomic-decf) name))
  (flet ((invalid-place ()
           (error "Invalid first argument to ~S: ~S" name specified-place))
         (compute-newval (old) ; used only if no atomic inc vop
           `(logand (,(case name (atomic-incf '+) (atomic-decf '-)) ,old
                     (the sb-vm:signed-word ,diff)) sb-ext:most-positive-word))
         (compute-delta () ; used only with atomic inc vop
           `(logand ,(case name
                       (atomic-incf `(the sb-vm:signed-word ,diff))
                       (atomic-decf `(- (the sb-vm:signed-word ,diff))))
                    sb-ext:most-positive-word)))
    (declare (ignorable #'compute-newval #'compute-delta))
    (when (and (symbolp place)
               (eq (info :variable :kind place) :global)
               (type= (info :variable :type place) (specifier-type 'fixnum)))
      ;; Global can't be lexically rebound.
      (return-from expand-atomic-frob
        `(truly-the fixnum (,(case name
                               (atomic-incf '%atomic-inc-symbol-global-value)
                               (atomic-decf '%atomic-dec-symbol-global-value))
                            ',place (the fixnum ,diff)))))
    (unless (consp place) (invalid-place))
    (destructuring-bind (op . args) place
      ;; FIXME: The lexical environment should not be disregarded.
      ;; CL builtins can't be lexically rebound, but structure accessors can.
      (case op
        (aref
         (unless (singleton-p (cdr args))
           (invalid-place))
         (with-unique-names (array)
           `(let ((,array (the (simple-array word (*)) ,(car args))))
              #+compare-and-swap-vops
              (%array-atomic-incf/word
               ,array
               (check-bound ,array (array-dimension ,array 0) ,(cadr args))
               ,(compute-delta))
              #-compare-and-swap-vops
              ,(with-unique-names (index old-value)
                 `(without-interrupts
                    (let* ((,index ,(cadr args))
                           (,old-value (aref ,array ,index)))
                      (setf (aref ,array ,index) ,(compute-newval old-value))
                      ,old-value))))))
        ((car cdr first rest)
         (when (cdr args)
           (invalid-place))
         `(truly-the
           fixnum
           (,(case op
               ((first car) (case name
                              (atomic-incf '%atomic-inc-car)
                              (atomic-decf '%atomic-dec-car)))
               ((rest cdr)  (case name
                              (atomic-incf '%atomic-inc-cdr)
                              (atomic-decf '%atomic-dec-cdr))))
            ,(car args) (the fixnum ,diff))))
        (t
         (when (or (cdr args)
                   ;; Because accessor info is identical for the writer and reader
                   ;; functions, without a SYMBOLP check this would erroneously allow
                   ;;   (ATOMIC-INCF ((SETF STRUCT-SLOT) x))
                   (not (symbolp op))
                   (not (structure-instance-accessor-p op)))
           (invalid-place))
         (let* ((accessor-info (structure-instance-accessor-p op))
                (slotd (cdr accessor-info))
                (type (dsd-type slotd)))
           (unless (and (eq 'sb-vm:word (dsd-raw-type slotd))
                        (type= (specifier-type type) (specifier-type 'sb-vm:word)))
             (error "~S requires a slot of type (UNSIGNED-BYTE ~S), not ~S: ~S"
                    name sb-vm:n-word-bits type place))
           (when (dsd-read-only slotd)
             (error "Cannot use ~S with structure accessor for a read-only slot: ~S"
                    name place))
           #+compare-and-swap-vops
           `(truly-the sb-vm:word
                       (%raw-instance-atomic-incf/word
                        (the ,(dd-name (car accessor-info)) ,@args)
                        ,(dsd-index slotd)
                        ,(compute-delta)))
           #-compare-and-swap-vops
           (with-unique-names (structure old-value)
             `(without-interrupts
                (let* ((,structure ,@args)
                       (,old-value (,op ,structure)))
                  (setf (,op ,structure) ,(compute-newval old-value))
                  ,old-value)))))))))

(sb-xc:defmacro atomic-incf (&environment env place &optional (diff 1))
  #.(format nil
  "Atomically increments PLACE by DIFF, and returns the value of PLACE before
the increment.

PLACE must access one of the following:
 - a DEFSTRUCT slot with declared type (UNSIGNED-BYTE ~D~:*)
   or AREF of a (SIMPLE-ARRAY (UNSIGNED-BYTE ~D~:*) (*))
   The type SB-EXT:WORD can be used for these purposes.
 - CAR or CDR (respectively FIRST or REST) of a CONS.
 - a variable defined using DEFGLOBAL with a proclaimed type of FIXNUM.
Macroexpansion is performed on PLACE before expanding ATOMIC-INCF.

Incrementing is done using modular arithmetic,
which is well-defined over two different domains:
 - For structures and arrays, the operation accepts and produces
   an (UNSIGNED-BYTE ~D~:*), and DIFF must be of type (SIGNED-BYTE ~D).
   ATOMIC-INCF of #x~x by one results in #x0 being stored in PLACE.
 - For other places, the domain is FIXNUM, and DIFF must be a FIXNUM.
   ATOMIC-INCF of #x~x by one results in #x~x
   being stored in PLACE.

DIFF defaults to 1.

EXPERIMENTAL: Interface subject to change."
  sb-vm:n-word-bits most-positive-word
  most-positive-fixnum most-negative-fixnum)
  (expand-atomic-frob 'atomic-incf place diff env))

(sb-xc:defmacro atomic-decf (&environment env place &optional (diff 1))
  #.(format nil
  "Atomically decrements PLACE by DIFF, and returns the value of PLACE before
the decrement.

PLACE must access one of the following:
 - a DEFSTRUCT slot with declared type (UNSIGNED-BYTE ~D~:*)
   or AREF of a (SIMPLE-ARRAY (UNSIGNED-BYTE ~D~:*) (*))
   The type SB-EXT:WORD can be used for these purposes.
 - CAR or CDR (respectively FIRST or REST) of a CONS.
 - a variable defined using DEFGLOBAL with a proclaimed type of FIXNUM.
Macroexpansion is performed on PLACE before expanding ATOMIC-DECF.

Decrementing is done using modular arithmetic,
which is well-defined over two different domains:
 - For structures and arrays, the operation accepts and produces
   an (UNSIGNED-BYTE ~D~:*), and DIFF must be of type (SIGNED-BYTE ~D).
   ATOMIC-DECF of #x0 by one results in #x~x being stored in PLACE.
 - For other places, the domain is FIXNUM, and DIFF must be a FIXNUM.
   ATOMIC-DECF of #x~x by one results in #x~x
   being stored in PLACE.

DIFF defaults to 1.

EXPERIMENTAL: Interface subject to change."
  sb-vm:n-word-bits most-positive-word
  most-negative-fixnum most-positive-fixnum)
  (expand-atomic-frob 'atomic-decf place diff env))

(sb-xc:defmacro atomic-update (place update-fn &rest arguments &environment env)
  "Updates PLACE atomically to the value returned by calling function
designated by UPDATE-FN with ARGUMENTS and the previous value of PLACE.

PLACE may be read and UPDATE-FN evaluated and called multiple times before the
update succeeds: atomicity in this context means that the value of PLACE did
not change between the time it was read, and the time it was replaced with the
computed value.

PLACE can be any place supported by SB-EXT:COMPARE-AND-SWAP.

Examples:

  ;;; Conses T to the head of FOO-LIST.
  (defstruct foo list)
  (defvar *foo* (make-foo))
  (atomic-update (foo-list *foo*) #'cons t)

  (let ((x (cons :count 0)))
     (mapc #'sb-thread:join-thread
           (loop repeat 1000
                 collect (sb-thread:make-thread
                          (lambda ()
                            (loop repeat 1000
                                  do (atomic-update (cdr x) #'1+)
                                     (sleep 0.00001))))))
     ;; Guaranteed to be (:COUNT . 1000000) -- if you replace
     ;; atomic update with (INCF (CDR X)) above, the result becomes
     ;; unpredictable.
     x)
"
  (multiple-value-bind (vars vals old new cas-form read-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar 'list vars vals)
            (,old ,read-form))
       (loop for ,new = (funcall ,update-fn ,@arguments ,old)
             until (eq ,old (setf ,old ,cas-form))
             finally (return ,new)))))

(sb-xc:defmacro atomic-push (obj place &environment env)
  "Like PUSH, but atomic. PLACE may be read multiple times before
the operation completes -- the write does not occur until such time
that no other thread modified PLACE between the read and the write.

Works on all CASable places."
  (multiple-value-bind (vars vals old new cas-form read-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar 'list vars vals)
            (,old ,read-form)
            (,new (cons ,obj ,old)))
       (loop until (eq ,old (setf ,old ,cas-form))
             do (setf (cdr ,new) ,old)
             finally (return ,new)))))

(sb-xc:defmacro atomic-pop (place &environment env)
  "Like POP, but atomic. PLACE may be read multiple times before
the operation completes -- the write does not occur until such time
that no other thread modified PLACE between the read and the write.

Works on all CASable places."
  (multiple-value-bind (vars vals old new cas-form read-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar 'list vars vals)
            (,old ,read-form))
       (loop (let ((,new (cdr ,old)))
               (when (eq ,old (setf ,old ,cas-form))
                 (return (car (truly-the list ,old)))))))))
