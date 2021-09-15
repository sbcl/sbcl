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
        (when (eq (car expr) 'dynamic-extent)
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

(flet ((defun-expander (env name lambda-list body snippet)
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
                   (cond ((sb-c:maybe-inline-syntactic-closure lambda env))
                         (t
                          (#+sb-xc-host warn
                           #-sb-xc-host sb-c:maybe-compiler-notify
                           "lexical environment too hairy, can't inline DEFUN ~S"
                           name)
                          nil))))))
      (when (and (eq snippet :constructor)
                 (not (typep inline-thing '(cons (eql sb-c:lambda-with-lexenv)))))
        ;; constructor in null lexenv need not save the expansion
        (setq inline-thing nil))
      (when inline-thing
        (setq inline-thing (list 'quote inline-thing)))
      (when (and extra-info (not (keywordp extra-info)))
        (setq extra-info (list 'quote extra-info)))
      `(progn
         (eval-when (:compile-toplevel)
           (sb-c:%compiler-defun ',name t ,inline-thing ,extra-info))
         ,(if (block-compilation-non-entry-point name)
              `(progn
                 (sb-c::%refless-defun ,named-lambda)
                 ',name)
              `(%defun ',name ,named-lambda
                       ,@(when (or inline-thing extra-info) `(,inline-thing))
                       ,@(when extra-info `(,extra-info))))
         ;; This warning, if produced, comes after the DEFUN happens.
         ;; When compiling, there's no real difference, but when interpreting,
         ;; if there is a handler for style-warning that nonlocally exits,
         ;; it's wrong to have skipped the DEFUN itself, since if there is no
         ;; function, then the warning ought not to have been issued at all.
         ,@(when (typep name '(cons (eql setf)))
             `((eval-when (:compile-toplevel :execute)
                 (sb-c::warn-if-setf-macro ',name))
               ',name)))))))

;;; This is one of the major places where the semantics of block
;;; compilation is handled.  Substitution for global names is totally
;;; inhibited if (block-compile *compilation*) is NIL.  And if
;;; (block-compile *compilation*) is true and entry points are
;;; specified, then we don't install global definitions for non-entry
;;; functions (effectively turning them into local lexical functions.)
  (sb-xc:defmacro defun (&environment env name lambda-list &body body)
    "Define a function at top level."
    (check-designator name defun)
    #+sb-xc-host
    (unless (cl:symbol-package (fun-name-block-name name))
      (warn "DEFUN of uninterned function name ~S (tricky for GENESIS)" name))
    (defun-expander env name lambda-list body nil))

  ;; extended defun as used by defstruct
  (sb-xc:defmacro sb-c:xdefun (&environment env name snippet lambda-list &body body)
    (defun-expander env name lambda-list body snippet)))

;;;; DEFCONSTANT, DEFVAR and DEFPARAMETER

(sb-xc:defmacro defconstant (name value &optional (doc nil docp))
  "Define a global constant, saying that the value is constant and may be
  compiled into code. If the variable already has a value, and this is not
  EQL to the new value, the code is not portable (undefined behavior). The
  third argument is an optional documentation string for the variable."
  (check-designator name defconstant)
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
                  (unless (eql value old)
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
  ;; Record the names of hairy defconstants when block compiling.
  (when (and sb-c::*compile-time-eval*
             (eq (sb-c::block-compile sb-c::*compilation*) t))
    (unless (sb-xc:typep value '(or fixnum character symbol))
      (push name sb-c::*hairy-defconstants*)))
  ;; Define the constant in the cross-compilation host, since the
  ;; value is used when cross-compiling for :COMPILE-TOPLEVEL contexts
  ;; which reference the constant.
  #+sb-xc-host
  (eval `(defconstant ,name ',value))
  (setf (info :variable :kind name) :constant)
  name)

(sb-xc:defmacro defvar (var &optional (val nil valp) (doc nil docp))
  "Define a special variable at top level. Declare the variable
  SPECIAL and, optionally, initialize it. If the variable already has a
  value, the old value is not clobbered. The third argument is an optional
  documentation string for the variable."
  (check-designator var defvar)
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
  (check-designator var defparameter)
  ;; Maybe kill docstring, but only under the cross-compiler.
  #+(and (not sb-doc) sb-xc-host) (setq doc nil)
  `(progn
     (eval-when (:compile-toplevel)
       (%compiler-defvar ',var))
     (%defparameter ',var ,val (sb-c:source-location)
                    ,@(and docp
                           `(',doc)))))

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

(defun %compiler-defvar (var)
  (proclaim `(special ,var)))


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
  (let ((n-result (sb-xc:gensym)))
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
           (let ((n-result (sb-xc:gensym)))
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
      (let ((ignore (sb-xc:gensym)))
        `(multiple-value-call #'(lambda (&optional ,@(mapcar #'list vars)
                                         &rest ,ignore)
                                  (declare (ignore ,ignore))
                                  ,@body)
                              ,value-form))))

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
              (keeper (sb-xc:gensym "KEEPER")))
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
  (let ((expanded (%macroexpand place env)))
    (if (symbolp expanded)
        `(do ()
             ((typep ,place ',type))
           (setf ,place (check-type-error ',place ,place ',type
                                          ,@(and type-string
                                                 `(,type-string)))))
        (let ((value (sb-xc:gensym)))
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
  (check-designator name define-compiler-macro)
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

#|
;;; ---------------------------------------------------------------------------
;;; And now some theory about how to turn CASE expressions which dispatch on
;;; symbols into expressions which dispatch on numbers instead. The idea is
;;; to use SYMBOL-HASH. It is easily enforceable that symbols referenced from
;;; compiled code have a hash, though generally SYMBOL-HASH is lazily computed.
;;; The masked hash maps to an integer from 0 to (ASH 1 nbits) which will be
;;; dispatched via a jump table on compiler backends which support jump tables.

;;; There are complications that make any naive attempt to dispatch on hash
;;; inadmissible. First off, any symbol could hash to the hash of a symbol
;;; in the dispatched set, so there always has to be a check for a match on
;;; the symbol. That's the easy part. The difficulty is figuring out what to
;;; do with hash collisions; but worse, symbols within a given clause of the
;;; CASE (having the same consequent) may hash differently.
;;; Consider one example.

(CASE sym
 ((u v) (cc1)) ; "CC" designates the "canonical clause" in that ordinal position
 ((w x) (cc2))
 ((y z) (cc3)))

;;; In reality, we would probably find a subset of bits of SYMBOL-HASH producing
;;; unique bins, but suppose for argument's sake that there are collisions:

hash bin 0: (u . cc1) (w . cc2)
hash bin 1: (v . cc1) (y . cc3)
hash bin 2: (x . cc2) (z . cc3)
hash bin 3: - empty -

;;; Something has to be done to resolve the selection of the consequent.
;;; Described below are 4 possible techniques.

;;; Option (I)
;;; Syntactically repeat clause consequents giving a literal rendering
;;; of the hash-table.  This is a poor choice unless the consequent
;;; is a self-evaluating object. But it has the nice property of invoking
;;; at most one IF after the jump-table-based dispatch.

(case hash
  (0 (if (eq sym 'u) (cc1) (cc2)))  ; each canonical consequent appears twice
  (1 (if (eq sym 'v) (cc1) (cc3)))
  (2 (if (eq sym 'x) (cc2) (cc3))))

;;; Option (II)
;;; Merge bins to create equivalence classes by canonical clause.

(case hash
  ((0 1) (case sym (y (cc3)) (w (cc2)) (otherwise (cc1))))
  (2 (if (eq sym 'x) (cc2) (cc3)))

;;; So this expansion has avoided inserting the s-expression CC1 more than once,
;;; but it inserts CC2 and CC3 each twice.  If those are not self-evaluating
;;; literals, then we should further merge (0 1) with (2). Doing the second
;;; merge lumps everything into 1 bin which is as bad as, if not worse than,
;;; a chain if IF expressions testing the original symbol.
;;; i.e. bin merging could make the hashing operation pointless.

;;; Option (III)
;;; Use a "two-stage" decode.
;;; This makes the consequent of the inner case side-effect free,
;;; but is probably not great for performance, though is an elegant
;;; rendition of the equivalence class technique.

(let ((original-clause-index
       (case hash
        (0 (if (eq sym 'u) 1 2))
        (1 (if (eq sym 'v) 1 3))
        (2 (if (eq sym 'x) 2 3)))))
  (case original-clause-index (1 (cc1)) (2 (cc2)) (3 (cc3))))

;;; Option (IV)
;;; Expand involving a tagbody, which though slightly ugly, presumably produces
;;; decent assembly code.

(block b
 (tagbody

   (case hash
     (0 (if (eq sym 'u) (go clause1) (go clause2)))
     (1 (if (eq sym 'v) (go clause1) (go clause3)))
     (2 (if (eq sym 'x) (go clause2) (go clause3))))
   clause1 (return-from b (progn (cc1)))
   clause2 (return-from b (progn (cc2)))
   ...))

;;; Additionally, if some bin has only 1 possibility, the GO is elided
;;; and we can just do (return-from b (consequent)).

;;; In practice, the expander does something not entirely unlike any of the above.
;;; It will insert a consequent more than one time if it is a self-evaluating object,
;;; and it will merge bins provided that the merging is simple and does not
;;; introduce any new IF expressions.

;;; A minimal example that won't work - after forcing the expander to try to
;;; operate on just 4 symbols which normally it would not try to process:
;;; (CASE (H) ((U V) (F)) ((:U :Z) Y))
clause 0 -> bins (2 3)
clause 1 -> bins (1 2)
bin 0 -> NIL
bin 1 -> ((:Z . 1))
bin 2 -> ((:U . 1) (U . 0))
bin 3 -> ((V . 0))
symbol-case giving up: case=((V U) (F))

;;; ---------------------------------------------------------------------------
|#

;;; For the specified symbols, choose bits from the hash producing as near a
;;; perfect hash function as can be achived without extraction of one
;;; byte or logxor of two bytes.
;;; This will fail to find a unique hash if there are symbols whose names are
;;; spelled the same, since their SXHASHes are the same.
;;;
;;; HASH-FUN is taken as an argument mainly for testing purposes so that any
;;; behavior can be simulated without having to bother with finding symbols
;;; whose SXHASH hashes collide.
;;;
;;; FIXME: If not a perfect hash, then the best choice should be informed by a
;;; cost function that takes into account which symbols share a clause of the
;;; CASE form. Prefer collisions on symbols whose consequent in the CASE
;;; is the same, otherwise the expansion becomes convoluted.

(defun pick-best-sxhash-bits (keys &optional (hash-fun 'sxhash)
                                             (maxbytes 2)
                                             (maxbits sb-vm:n-positive-fixnum-bits))
  (declare (type (member 1 2) maxbytes))
  (unless keys
    (bug "Give me some objects")) ; it beats getting division by zero error
  (let* ((nkeys (length keys))
         (ideal-table-size (power-of-two-ceiling nkeys))
         (required-nbits (integer-length (1- ideal-table-size)))
         ;; If each bin has 2 items in it (which isn't ideal), then the table could
         ;; be half the "required" minimum size. This occurs with symbol sets such
         ;; as {AND, NOT, OR, :AND, :NOT, :OR} on account of the fact that hashing
         ;; by string produces 2 of each hash.
         (smallest-nbits (max 2 (1- required-nbits)))
         (largest-nbits (1+ required-nbits)) ; allow double the required minimum
         (hashes (mapcar hash-fun keys)))
    (macrolet ((stats (mixer answer)
                 ;; Compute the average number of items per bin,
                 ;; looking only at bins that contain something.
                 ;; It does not improve things to increase the total bins
                 ;; without distributing items into at least 1 more bin.
                 `(progn
                    (fill bin-counts 0)
                    (dolist (hash hashes)
                      (incf (aref bin-counts ,mixer)))
                    (let ((max-bin-count 0) (n-used 0))
                      (dovector (count bin-counts)
                        (when (plusp count) (incf n-used))
                        (setf max-bin-count (max count max-bin-count)))
                      (when (= max-bin-count 1) ; can't beat a perfect hash
                        (return-from try (values 1 ,answer)))
                      (let ((average (/ nkeys n-used)))
                        (when (or (< max-bin-count best-max-bin-count)
                                  (and (= max-bin-count best-max-bin-count)
                                       (< average best-average)))
                          (setq best-answer ,answer
                                best-max-bin-count max-bin-count
                                best-average average)))))))
      (flet ((try-one-byte ()
               (let ((best-answer nil)
                     ;; "best" means smallest
                     (best-max-bin-count most-positive-fixnum) ; sentinel value
                     (best-average most-positive-fixnum))
                 (loop named try
                       for nbits from smallest-nbits to largest-nbits
                       do (let ((bin-counts (make-array (ash 1 nbits) :initial-element 0)))
                            (dotimes (pos (- (1+ maxbits) nbits))
                              (stats (ldb (byte nbits pos) hash) (byte nbits pos))))
                       finally (return-from try (values best-max-bin-count best-answer)))))
             (try-two-bytes ()
               (let ((best-answer nil)
                     (best-max-bin-count most-positive-fixnum)
                     (best-average most-positive-fixnum))
                 (loop named try
                       for nbits from smallest-nbits to largest-nbits
                       do (let ((bin-counts (make-array (ash 1 nbits) :initial-element 0)))
                            (dotimes (pos1 (- (1+ maxbits) nbits))
                              (dotimes (pos2 pos1)
                                (stats (logxor (ldb (byte nbits pos1) hash)
                                               (ldb (byte nbits pos2) hash))
                                       (vector (byte nbits pos1) (byte nbits pos2))))))
                        finally (return-from try (values best-max-bin-count best-answer))))))
        (multiple-value-bind (score1 answer1) (try-one-byte)
          ;; Return if perfect score, or if caller doesn't want to try using two bytes
          (if (or (= score1 1) (= maxbytes 1))
              (values score1 answer1)
              (multiple-value-bind (score2 answer2) (try-two-bytes)
                (if (<= score1 score2)
                    (values score1 answer1) ; not improved
                    (values score2 answer2))))))))) ; 2 bytes = better

(defun build-sealed-struct-typecase-map (type-unions)
  (let* ((layout-lists
          (mapcar (lambda (x) (mapcar #'find-layout x)) type-unions))
         (byte (nth-value 1
                (pick-best-sxhash-bits (apply #'append layout-lists)
                                       #'wrapper-clos-hash 1)))
         (array (make-array (ash 1 (byte-size byte)) :initial-element nil))
         (perfectp t)
         (seen))
    (loop for layout-list in layout-lists
          for selector from 1
          do (dolist (layout layout-list)
               (unless (member layout seen) ; in case of dups / overlaps
                 (push layout seen)
                 (let ((bin (ldb byte (wrapper-clos-hash layout))))
                   (when (aref array bin)
                     (setq perfectp nil))
                   (setf (aref array bin)
                         (nconc (aref array bin) (list (cons layout selector))))))))
    (values `(byte ,(byte-size byte) ,(byte-position byte))
            perfectp
            (if perfectp
                ;; at most one element is in each bin
                (let ((result (make-array (* (length array) 2) :initial-element 0)))
                  (dotimes (index (length array) result)
                    (awhen (aref array index)
                      (setf (aref result index) (caar it)
                            (aref result (+ index (length array))) (cdar it)))))
                array))))

(sb-xc:defmacro %sealed-struct-typecase-index (cases object)
  (binding* (((byte perfectp cells) (build-sealed-struct-typecase-map cases))
             (n-pairs (/ (length cells) 2)))
    `(if (not (%instancep ,object))
         0
         (let* ((layout (%instance-layout ,object))
                (index (ldb ,byte (layout-clos-hash layout)))
                ;; Compare by WRAPPER, not LAYOUT. LAYOUTs can't go in a simple-vector.
                (wrapper (layout-friend layout))
                (cells ,cells))
           (declare (optimize (safety 0)))
           (the (mod ,(1+ (length cases)))
                ,(if perfectp
                     `(if (eq (aref cells index) wrapper) (aref cells (+ index ,n-pairs)) 0)
                     ;; DOLIST performs a leading test, but we're OK with a cell
                     ;; that is NIL. It'll miss and then exit at the end of the loop.
                     ;; This potentially avoids one comparison vs NIL if we hit immediately.
                     `(let ((list (svref cells index)))
                        (loop (let ((cell (car list)))
                                (cond ((eq wrapper (car cell)) (return (cdr cell)))
                                      ((null (setq list (cdr list))) (return 0))))))))))))

;;; Given an arbitrary TYPECASE, see if it is a discriminator over
;;; an assortment of structure-object subtypes. If it is, potentially turn it
;;; into a dispatch based on layout-clos-hash.
;;; The decision to use a hash-based lookup should depend on the number of types
;;; matched, but if there are a lot of types matched all rooted at a common
;;; ancestor, it is not beneficial.
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
(defun expand-struct-typecase (keyform temp normal-clauses type-specs default errorp
                               &aux (n-root-types 0) (exhaustive-list))
  (labels
      ((ok-classoid (classoid)
         (or (structure-classoid-p classoid)
             (and (sb-kernel::built-in-classoid-p classoid)
                  (not (memq (classoid-name classoid)
                             sb-kernel::**non-instance-classoid-types**)))))
       (discover-subtypes (specifier)
           (let* ((parse (specifier-type specifier))
                  (worklist
                   (cond ((ok-classoid parse) (list parse))
                         ((and (union-type-p parse)
                               (every #'ok-classoid (union-type-types parse)))
                          (copy-list (union-type-types parse)))
                         (t
                          (return-from discover-subtypes nil)))))
             ;; Assume that each "root" type would require its own test based
             ;; on %instance-layout of self and ancestor and that all root types
             ;; are disjoint. This may not be true if a later one includes
             ;; an earlier one.
             (incf n-root-types (length worklist))
             (collect ((visited))
               (loop (unless worklist (return))
                     (let ((classoid (pop worklist)))
                       (visited classoid)
                       (sb-kernel::do-subclassoids ((subclassoid wrapper) classoid)
                         (declare (ignore wrapper))
                         (unless (or (member subclassoid (visited))
                                     (member subclassoid worklist))
                           (setf worklist (nconc worklist (list subclassoid)))))))
               (setf exhaustive-list (union (visited) exhaustive-list))
               (visited)))))
    (let ((classoid-lists (mapcar #'discover-subtypes type-specs)))
      (when (or (some #'null classoid-lists)
                (< n-root-types 6)
                ;; Given something like:
                ;; (typecase x
                ;;   ((or parent1 parent2 parent3) ...)
                ;;   ((or parent4 parent5 parent6) ...)
                ;; where eaach parent has dozens of children (directly or indirectly),
                ;; it may be worse to use a hash-based lookup.
                (> (length exhaustive-list) (* 3 n-root-types)))
        (return-from expand-struct-typecase))
      (if (every (lambda (classoids)
                   (every (lambda (classoid)
                            (eq (classoid-state classoid) :sealed))
                          classoids))
                 classoid-lists)
          `(let ((,temp ,keyform))
             (case (%sealed-struct-typecase-index
                    ,(mapcar (lambda (list) (mapcar #'classoid-name list))
                             classoid-lists)
                    ,temp)
               ,@(loop for i from 1 for clause in normal-clauses
                       collect `(,i
                                 ;; CLAUSE is ((TYPEP #:G 'a-type) NIL . forms)
                                 (sb-c::%type-constraint ,temp ,(third (car clause)))
                                 ,@(cddr clause)))
               (0 ,@(if errorp
                        `((etypecase-failure ,temp ',type-specs))
                        (cddr default)))))
          ;; not done
          nil))))

;;; Turn a case over symbols into a case over their hashes.
;;; Regarding the apparently redundant UNREACHABLE branches:
;;; The first one causes the total number of ways to be (ASH 1 NBITS) exactly.
;;; The second allows proper type derivation, because otherwise it is not obvious
;;; (to the compiler) that all prior COND clauses were exhaustive.
;;; This actually matters to to encoding of alien ENUM types. In the conventional
;;; expansion of ECASE, all branches cover the enum, and the failure branch
;;; ensures non-return of anything but an integer.
;;; In the fancy expansion, we would get a compile-time error:
;;;   debugger invoked on a SIMPLE-ERROR in thread
;;;   #<THREAD "main thread" RUNNING {10005304C3}>:
;;;     #<SB-C:TN t1 :NORMAL> is not valid as the first argument to VOP:
;;;     SB-VM::MOVE-WORD-ARG
;;; without the UNREACHABLE branch, because the expansion otherwise
;;; looked as if the COND might return NIL.
(defun expand-symbol-case (keyform clauses keys errorp hash-fun &optional (maxbytes 2))
  (declare (ignorable keyform clauses keys errorp))
  ;; for few keys, the clever algorithm  probably gives no better performance
  ;; - and potentially worse - than the CPU's branch predictor.
  (unless (>= (length keys) 6)
    (return-from expand-symbol-case nil))
  (multiple-value-bind (maxprobes byte) (pick-best-sxhash-bits keys hash-fun maxbytes)
    (when (and (vectorp byte) (< (length keys) 9))
      ;; It took two bytes to hash well enough. That's more fixed overhead,
      ;; so require more symbols. Otherwise just go back to linear scan.
      (return-from expand-symbol-case nil))
    ;; (format t "maxprobes ~d byte ~s~%" maxprobes byte)
    (when (> maxprobes 2) ; Give up (for now) if more than 2 keys hash the same.
      #+sb-devel (format t "~&symbol-case giving up: probes=~d byte=~d~%"
                         maxprobes byte)
      (return-from expand-symbol-case nil))
    (let* ((default (when (eql (caar clauses) 't) (pop clauses)))
           (unique-symbols)
           (clauses
            ;; This is crummy, but we first have to undo our pre-expansion
            ;; and remove dups. Otherwise the (BUG "Messup") below could occur.
            (mapcar
             (lambda (clause)
               (destructuring-bind (antecedent . consequent) clause
                 (aver (typep consequent '(cons (eql nil))))
                 (pop consequent) ; strip the NIL that CASE-BODY inserts
                 (when (typep antecedent '(cons (eql eql)))
                   (setq antecedent `(or ,antecedent)))
                 (flet ((extract-key (form) ; (EQL #:gN (QUOTE foo)) -> FOO
                          (let ((third (third form)))
                            (aver (typep third '(cons (eql quote))))
                            (the symbol (second third)))))
                   (let (clause-symbols)
                     ;; De-duplicate across all clauses and within each clause
                     ;; due to possible extreme stupidity in source code.
                     (dolist (term (cdr antecedent))
                       (aver (typep term '(cons (eql eql))))
                       (let ((symbol (extract-key term)))
                         (unless (member symbol unique-symbols)
                           (push symbol unique-symbols)
                           (push symbol clause-symbols))))
                     (if clause-symbols ; all symbols could have dropped out
                         (cons clause-symbols consequent)
                         ;; give up. There are reasons the compiler should see
                         ;; all subforms. Maybe not good reasons.
                         (return-from expand-symbol-case nil))))))
             (reverse clauses)))
           (clause->bins (make-array (length clauses) :initial-element nil))
           (table-nbits (byte-size (if (vectorp byte) (elt byte 0) byte)))
           (bins (make-array (ash 1 table-nbits) :initial-element 0))
           (symbol (sb-xc:gensym "S"))
           (hash (sb-xc:gensym "H"))
           (vector (sb-xc:gensym "V"))
           (is-hashable
            ;; For x86-64, any non-immediate object is considered hashable,
            ;; so we only do a lowtag test on the object, though the correct hash
            ;; is obtained only if the object is a symbol.
            #+x86-64 `(pointerp ,symbol)
            ;; For others backends, the set of keys in a particular CASE form
            ;; makes a difference. NIL as a possible key mandates choosing SYMBOLP
            ;; but NON-NULL-SYMBOL-P is the quicker test.
            #-x86-64 `(,(if (member nil keys) 'symbolp 'non-null-symbol-p) ,symbol))
           (sxhash
             ;; Always access the pre-memoized value in the hash slot.
             ;; SYMBOL-HASH* reads the word following the header word
             ;; in any pointer object regardless of lowtag.
             #+x86-64
             (if (eq hash-fun 'sxhash) `(symbol-hash* ,symbol nil) `(,hash-fun ,symbol))
             ;; For others, use SYMBOL-HASH.
             #-x86-64 `(,(if (eq hash-fun 'sxhash) 'symbol-hash hash-fun) ,symbol))
           (calc-hash
             (if (vectorp byte) ; mix 2 bytes
                 ;; FIXME: this could be performed as ((h >> c1) ^ (h >> c2)) & mask
                 ;; instead of having two AND operations as it does now.
                 (let ((b1 (elt byte 0)) (b2 (elt byte 1)))
                   `(let ((,hash ,sxhash))
                      (logxor (ldb (byte ,(byte-size b1) ,(byte-position b1)) ,hash)
                              (ldb (byte ,(byte-size b2) ,(byte-position b2)) ,hash))))
                 `(ldb (byte ,(byte-size byte) ,(byte-position byte)) ,sxhash))))

      (flet ((trivial-result-p (clause)
               ;; Return 2 values: T/NIL if the clause's consequent
               ;; is trivial, and its value when trivial.
               (let ((consequent (cdr clause)))
                 (if (singleton-p consequent)
                     (let ((form (car consequent)))
                       (cond ((typep form '(cons (eql quote) (cons t null)))
                              (values t (cadr form)))
                             ((self-evaluating-p form) (values t form))
                             (t (values nil nil))))
                     ;; NIL -> T, otherwise -> NIL
                     (values (not consequent) nil))))
             (hash-bits (symbol)
               (let ((sxhash (funcall hash-fun symbol)))
                 (if (vectorp byte)
                     (logxor (ldb (elt byte 0) sxhash)
                             (ldb (elt byte 1) sxhash))
                     (ldb byte sxhash)))))

        ;; Try doing an array lookup if the hashing has no collisions and
        ;; every consequent is trivial.
        (when (= maxprobes 1)
          (block try-table-lookup
            (let ((values (make-array (length bins) :initial-element 0))
                  (single-value) ; only if exactly one clause
                  (types nil))
              (dolist (clause clauses)
                (multiple-value-bind (trivialp value) (trivial-result-p clause)
                  (unless trivialp
                    (return-from try-table-lookup))
                  (setq single-value value)
                  (push (ctype-of value) types)
                  (dolist (symbol (car clause))
                    (let ((index (hash-bits symbol)))
                      (aver (eql (aref bins index) 0)) ; no collisions
                      (setf (aref bins index) symbol
                            (aref values index) value)))))
              (return-from expand-symbol-case
                ;; FIXME: figure out how to eliminate the dead store.
                ;; If hash gets used, then we store into it later, killing the initial store.
                `(let ((,symbol ,keyform) (,hash 0))
                   (if (and ,is-hashable (eq ,symbol (svref ,bins (setq ,hash ,calc-hash))))
                       ,(if (singleton-p clauses)
                            `',single-value
                            `(truly-the ,(type-specifier (apply #'type-union types))
                                        (aref ,(sb-c::coerce-to-smallest-eltype values)
                                              ,hash)))
                       ,(if errorp
                            ;; coalescing of simple-vectors in a saved core
                            ;; could eliminate repeated data from the same
                            ;; ECASE that gets inlined many times over.
                            `(ecase-failure
                              ,symbol ,(coerce keys 'simple-vector))
                            `(progn ,@(cddr default)))))))))

        ;; Reset the bins, try it the long way
        (fill bins nil)
        ;; Place each symbol into its hash bin. Also, for each clause compute the
        ;; set of hash bins that it has placed a symbol into.
        (loop for clause in clauses for clause-index from 0
              do (dolist (symbol (car clause))
                   (let ((masked-hash (hash-bits symbol)))
                     (pushnew masked-hash (aref clause->bins clause-index))
                     (push (cons symbol clause-index) (aref bins masked-hash)))))
        ;; Canonically order each element of clause->bins
        (dotimes (i (length clause->bins))
          (setf (aref clause->bins i) (sort (aref clause->bins i) #'<)))

        ;; Perform a very limited bin merging step as follows: if a clause
        ;; placed its symbols in more than one bin, allow it provided that either:
        ;; - the clause's consequent is trivial, OR
        ;; - none of its bins contains a symbol from a different clause.
        ;; In the former case, we don't need to merge bins.
        ;; In the latter case, the bins define an equivalence class
        ;; that does not intersect any other equivalence class.
        ;;
        ;; To make this work, if a bin requires an IF to disambiguate which consequent
        ;; to take, then it is removed from the clase->bins entry for each clause
        ;; for which it contains a consequent so that the code below which computes
        ;; the COND does not see the bin as a member of any equivalence class.
        ;; Pass 1: decide whether to give up or go on.
        (loop for clause in clauses for clause-index from 0
              do (let ((equivalence-class (aref clause->bins clause-index)))
                   ;; if a nontrivial consequent is distributed into
                   ;; more than one hash bin ...
                   (when (and (cdr equivalence-class)
                              (not (trivial-result-p clause)))
                     (dolist (bin-index (aref clause->bins clause-index))
                       (dolist (item (aref bins bin-index))
                         (unless (eql (cdr item) clause-index)
                           #+sb-devel (format t "~&symbol-case gives up: case=~s~%" clause)
                           (return-from expand-symbol-case)))))))
        ;; Pass 2: remove bins with more than one consequent from their
        ;; clause equivalence class.
        ;; Maybe these passes could be combined, but I'd rather conservatively
        ;; preserve accumulated data thus far before wrecking it.
        (dotimes (bin-index (length bins))
          (let ((unique-clause-indices
                 (remove-duplicates (mapcar #'cdr (aref bins bin-index)))))
            (when (cdr unique-clause-indices)
              (dolist (clause-index unique-clause-indices)
                (setf (aref clause->bins clause-index)
                      (delete bin-index (aref clause->bins clause-index))))))))

      ;; Compute the COND clauses over the range of hash values.
      (let ((symbol-vector (make-array (* maxprobes (length bins)) :initial-element 0))
            (cond-clauses))
        ;; For each nonempty bin:
        ;; - If it contains >1 consequent, then arbitrarily pick one symbol to test
        ;;   to see which consequent pertains.
        ;; - Otherwise, it contains exactly one consequent. See if the bin's equivalence
        ;;   class has >1 item. If it does, insert for only a representative element.
        ;;   By the above construction, no other bin in the class can have >1 consequent.
        (flet ((action (clause-index)
                 (let ((clause (nth clause-index clauses)))
                   (or (cdr clause) '(nil)))))
          (dotimes (bin-index (length bins))
            (let* ((bin-contents (aref bins bin-index))
                   (unique-clause-indices
                    (remove-duplicates (mapcar #'cdr bin-contents)))
                   (cond-clause
                    (cond ((cdr unique-clause-indices)
                           ;; Exactly 2 actions in the bin due to hardcoded limitation
                           ;; on the implementation of collision resolution.
                           `((eql ,hash ,bin-index)
                             (cond ((eq ,symbol ',(caar bin-contents))
                                    ,@(action (cdar bin-contents)))
                                   (t
                                    ,@(action (cdadr bin-contents))))))
                          (unique-clause-indices
                           (let* ((clause-index (car unique-clause-indices))
                                  (equivalence-class (aref clause->bins clause-index)))
                             (when (eql bin-index (car equivalence-class))
                               ;; This is the representative bin of a class
                               `(,(if (cdr equivalence-class)
                                      `(or ,@(mapcar (lambda (x) `(eql ,hash ,x))
                                                     equivalence-class))
                                      `(eql ,hash ,bin-index))
                                 ,@(action clause-index))))))))
              (when cond-clause (push cond-clause cond-clauses)))))

        ;; Fill in the symbol vector. Symbols in a bin are adjacent in the vector.
        ;; 2 items per bin looks like so: | elt0 | elt1 | elt2 | elt3 | ...
        ;;                                | <- bin 0 -> | <- bin 1 -> | ...
        (dotimes (hash (length bins))
          (let ((items (aref bins hash))
                (index (* hash maxprobes)))
            (dotimes (i maxprobes)
              (unless items (return))
              (setf (aref symbol-vector index) (car (pop items)))
              (incf index))
            (when items (bug "Messup in CASE vectors for ~S" keys))))

        ;; If there is only one clause (plus possibly a default), then hash
        ;; collisions do not need to be disambiguated. Hence it can be implemented
        ;; as one or two array lookups.
        (when (singleton-p clauses)
          (return-from expand-symbol-case
            `(let ((,symbol ,keyform) (,hash 0))
               (if (and ,is-hashable
                        ,(ecase maxprobes
                           (1 `(eq (svref ,symbol-vector (setq ,hash ,calc-hash)) ,symbol))
                           ;; FIXME: see why SVREF on constant vector of symbols would get
                           ;; >1 header constant unless lexically bound.
                           (2 `(let ((,vector ,symbol-vector))
                                 (or (eq (svref ,vector (setq ,hash (ash ,calc-hash 1))) ,symbol)
                                     (eq (svref ,vector (1+ ,hash)) ,symbol))))))
                   (progn ,@(cdar clauses))
                   ,(if errorp
                        `(ecase-failure ,symbol ,(coerce keys 'simple-vector))
                        `(progn ,@(cddr default)))))))

        ;; Produce a COND only if the backend supports the multiway branch vop.
        #+(or x86 x86-64)
        (let ((block (sb-xc:gensym "B"))
              (unused-bins))
          ;; Take note of the unused bins
          (dotimes (i (length bins))
            (unless (aref bins i) (push i unused-bins)))
          `(let ((,symbol ,keyform))
             (block ,block
               (when ,is-hashable
                 (let ((,hash ,calc-hash))
                   (declare (sb-c::no-constraints ,hash))
                   ;; At most two probes are required, and usually just 1
                   (when ,(ecase maxprobes
                            (1 `(eq (svref ,symbol-vector ,hash) ,symbol))
                            (2 `(let ((,vector ,symbol-vector)
                                      (,hash (ash ,hash 1)))
                                  (declare (sb-c::no-constraints ,hash))
                                  (or (eq (svref ,vector ,hash) ,symbol)
                                      (eq (svref ,vector (1+ ,hash)) ,symbol)))))
                     (return-from ,block
                       (cond ,@(nreverse cond-clauses)
                             ,@(when unused-bins
                                 ;; to make it clear that the number of "ways"
                                 ;; is the exact right number, which avoids a range
                                 ;; test in multiway-branch.
                                 `(((or ,@(mapcar (lambda (x) `(eql ,hash ,x))
                                                  (nreverse unused-bins)))
                                    (unreachable))))
                             (t (unreachable)))))))
               ,@(if errorp
                     `((ecase-failure ,symbol ,(coerce keys 'simple-vector)))
                     (cddr default)))))))))

;;; CASE-BODY returns code for all the standard "case" macros. NAME is
;;; the macro name, and KEYFORM is the thing to case on. MULTI-P
;;; indicates whether a branch may fire off a list of keys; otherwise,
;;; a key that is a list is interpreted in some way as a single key.
;;; When MULTI-P, TEST is applied to the value of KEYFORM and each key
;;; for a given branch; otherwise, TEST is applied to the value of
;;; KEYFORM and the entire first element, instead of each part, of the
;;; case branch. When ERRORP, no OTHERWISE-CLAUSEs are recognized,
;;; and an ERROR form is generated where control falls off the end
;;; of the ordinary clauses. When PROCEEDP, it is an error to
;;; omit ERRORP, and the ERROR form generated is executed within a
;;; RESTART-CASE allowing KEYFORM to be set and retested.

;;; Note the absence of EVAL-WHEN here. The cross-compiler calls this function
;;; and gets the compiled code that the host produced in make-host-1.
;;; If recompiled, you do not want an interpreted definition that might come
;;; from EVALing a toplevel form - the stack blows due to infinite recursion.
(defun case-body (name keyform cases multi-p test errorp proceedp needcasesp)
  (unless (or cases (not needcasesp))
    (warn "no clauses in ~S" name))
  (let ((keyform-value (gensym))
        (clauses ())
        (keys ())
        (keys-seen (make-hash-table :test #'eql)))
    (do* ((cases cases (cdr cases))
          (case (car cases) (car cases))
          (case-position 1 (1+ case-position)))
         ((null cases) nil)
      (flet ((check-clause (case-keys)
               (loop for k in case-keys
                  for existing = (gethash k keys-seen)
                  do (when existing
                       (warn 'duplicate-case-key-warning
                             :key k
                             :case-kind name
                             :occurrences `(,existing (,case-position (,case))))))
               (let ((record (list case-position (list case))))
                 (dolist (k case-keys)
                   (setf (gethash k keys-seen) record)))))
        (unless (list-of-length-at-least-p case 1)
          (with-current-source-form (cases)
            (error "~S -- bad clause in ~S" case name)))
        (with-current-source-form (case)
          (destructuring-bind (keyoid &rest forms) case
            (cond (;; an OTHERWISE-CLAUSE
                   ;;
                   ;; By the way... The old code here tried gave
                   ;; STYLE-WARNINGs for normal-clauses which looked as
                   ;; though they might've been intended to be
                   ;; otherwise-clauses. As Tony Martinez reported on
                   ;; sbcl-devel 2004-11-09 there are sometimes good
                   ;; reasons to write clauses like that; and as I noticed
                   ;; when trying to understand the old code so I could
                   ;; understand his patch, trying to guess which clauses
                   ;; don't have good reasons is fundamentally kind of a
                   ;; mess. SBCL does issue style warnings rather
                   ;; enthusiastically, and I have often justified that by
                   ;; arguing that we're doing that to detect issues which
                   ;; are tedious for programmers to detect for by
                   ;; proofreading (like small typoes in long symbol
                   ;; names, or duplicate function definitions in large
                   ;; files). This doesn't seem to be an issue like that,
                   ;; and I can't think of a comparably good justification
                   ;; for giving STYLE-WARNINGs for legal code here, so
                   ;; now we just hope the programmer knows what he's
                   ;; doing. -- WHN 2004-11-20
                   (and (not errorp) ; possible only in CASE or TYPECASE,
                                        ; not in [EC]CASE or [EC]TYPECASE
                        (memq keyoid '(t otherwise))
                        (null (cdr cases)))
                   ;; The NIL has a reason for being here: Without it, COND
                   ;; will return the value of the test form if FORMS is NIL.
                   (push `(t nil ,@forms) clauses))
                  ((and multi-p (listp keyoid))
                   (setf keys (nconc (reverse keyoid) keys))
                   (check-clause keyoid)
                   (push `((or ,@(mapcar (lambda (key)
                                           `(,test ,keyform-value ',key))
                                         keyoid))
                           nil
                           ,@forms)
                         clauses))
                  (t
                   (when (and (eq name 'case)
                              (cdr cases)
                              (memq keyoid '(t otherwise)))
                     (error 'simple-reference-error
                            :format-control
                            "~@<~IBad ~S clause:~:@_  ~S~:@_~S allowed as the key ~
                           designator only in the final otherwise-clause, not in a ~
                           normal-clause. Use (~S) instead, or move the clause to the ~
                           correct position.~:@>"
                            :format-arguments (list 'case case keyoid keyoid)
                            :references `((:ansi-cl :macro case))))
                   (push keyoid keys)
                   (check-clause (list keyoid))
                   (push `((,test ,keyform-value ',keyoid)
                           nil
                           ,@forms)
                         clauses)))))))
    (setq keys
          (nreverse (mapcon (lambda (tail)
                              (unless (member (car tail) (cdr tail))
                                (list (car tail))))
                            keys)))
    (case-body-aux name keyform keyform-value clauses keys errorp proceedp
                   `(,(if multi-p 'member 'or) ,@keys))))

;;; CASE-BODY-AUX provides the expansion once CASE-BODY has groveled
;;; all the cases. Note: it is not necessary that the resulting code
;;; signal case-failure conditions, but that's what KMP's prototype
;;; code did. We call CASE-BODY-ERROR, because of how closures are
;;; compiled. RESTART-CASE has forms with closures that the compiler
;;; causes to be generated at the top of any function using the case
;;; macros, regardless of whether they are needed.
(defun case-body-aux (name keyform keyform-value clauses keys
                      errorp proceedp expected-type)
  (when proceedp ; CCASE or CTYPECASE
    (return-from case-body-aux
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
             (cond ,@(nreverse clauses)
                   (t (multiple-value-bind ,stores ,retry (,switch ,writer)))))))))
  (let ((implement-as name)
        (original-keys keys))
    (when (member name '(typecase etypecase))
      ;; Bypass all TYPEP handling if every clause of [E]TYPECASE is a MEMBER type.
      ;; More importantly, try to use the fancy expansion for symbols as keys.
      (let* ((default (if (eq (caar clauses) 't) (car clauses)))
             (normal-clauses (reverse (if default (cdr clauses) clauses)))
             (new-clauses))
        (collect ((new-keys))
          (dolist (clause normal-clauses)
            ;; clause is ((TYPEP #:x (QUOTE <sometype>)) NIL forms*)
            (destructuring-bind ((typep thing type-expr) . consequent) clause
              (declare (ignore thing))
              (unless (and (typep type-expr '(cons (eql quote) (cons t null)))
                           (eq typep 'typep))
                (bug "TYPECASE expander glitch"))
              (let* ((spec (second type-expr))
                     (clause-keys
                      (case (if (listp spec) (car spec))
                        (eql (when (singleton-p (cdr spec)) (list (cadr spec))))
                        (member (when (proper-list-p (cdr spec)) (cdr spec)))
                        (t :fail))))
                (cond ((eq clause-keys :fail)
                       (setq new-clauses :fail))
                      ((neq new-clauses :fail)
                       (dolist (key clause-keys)
                         (unless (member key (new-keys))
                           (new-keys key)))
                       (push (cons `(or ,@(mapcar (lambda (x) `(eql ,keyform-value ',x))
                                                  clause-keys))
                                   consequent)
                             new-clauses))))))
          (acond ((neq new-clauses :fail)
                  ;; all TYPECASE clauses were convertible to CASE clauses
                  (setq clauses (if default (cons default new-clauses) new-clauses)
                        keys (new-keys)
                        implement-as 'case))
                 ((and (sb-c::vop-existsp :named sb-c:multiway-branch-if-eq)
                       (expand-struct-typecase keyform keyform-value normal-clauses keys
                                          default errorp))
                  (return-from case-body-aux it))))))

    ;; Efficiently expanding CASE over symbols depends on CASE over integers being
    ;; translated as a jump table. If it's not - as on most backends - then we use
    ;; the customary expansion as a series of IF tests.
    ;; Production code rarely uses CCASE, and the expansion differs such that
    ;; it doesn't seem worth the effort to handle it as a jump table.
    (when (and (member implement-as '(case ecase)) (every #'symbolp keys))
      (awhen (expand-symbol-case keyform clauses keys errorp 'sxhash)
        (return-from case-body-aux it)))

    `(let ((,keyform-value ,keyform))
       (declare (ignorable ,keyform-value)) ; e.g. (CASE KEY (T))
       (cond ,@(nreverse clauses)
             ,@(when errorp
                 `((t ,(ecase name
                         (etypecase
                          `(etypecase-failure
                            ,keyform-value ,(etypecase-error-spec original-keys)))
                         (ecase
                          `(ecase-failure ,keyform-value ',original-keys))))))))))

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
  `',types)

(sb-xc:defmacro case (keyform &body cases)
  "CASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If a singleton key is T then the clause is a default clause."
  (case-body 'case keyform cases t 'eql nil nil nil))

(sb-xc:defmacro ccase (keyform &body cases)
  "CCASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If none of the keys matches then a correctable error is
  signalled."
  (case-body 'ccase keyform cases t 'eql t t t))

(sb-xc:defmacro ecase (keyform &body cases)
  "ECASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If none of the keys matches then an error is signalled."
  (case-body 'ecase keyform cases t 'eql t nil t))

(sb-xc:defmacro typecase (keyform &body cases)
  "TYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true."
  (case-body 'typecase keyform cases nil 'typep nil nil nil))

(sb-xc:defmacro ctypecase (keyform &body cases)
  "CTYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true. If no form is satisfied then a correctable error is signalled."
  (case-body 'ctypecase keyform cases nil 'typep t t t))

(sb-xc:defmacro etypecase (keyform &body cases)
  "ETYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true. If no form is satisfied then an error is signalled."
  (case-body 'etypecase keyform cases nil 'typep t nil t))

;;; Compile a version of BODY for all TYPES, and dispatch to the
;;; correct one based on the value of VAR. This was originally used
;;; only for strings, hence the name. Renaming it to something more
;;; generic might not be a bad idea.
(sb-xc:defmacro string-dispatch ((&rest types) var &body body)
  (let ((fun (sb-xc:gensym "STRING-DISPATCH-FUN")))
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
    (let ((abortp (sb-xc:gensym)))
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
             (let ((label-1 (sb-xc:gensym)) (label-2 (sb-xc:gensym)))
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
    (frob-do-body varlist endlist body 'let 'psetq 'do-anonymous (sb-xc:gensym)))

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
  (let ((c (if (integerp count) count (sb-xc:gensym))))
    `(do ((,var 0 (1+ ,var))
          ,@(if (symbolp c) `((,c (the integer ,count)))))
         ((>= ,var ,c) ,result)
       (declare (type unsigned-byte ,var))
       ,@body)))

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
             (n-list (sb-xc:gensym "LIST"))
             (start (sb-xc:gensym "START"))
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
         (tagbody
            ,start
            (unless (endp ,n-list)
              (let ((,var ,(if clist-ok
                               `(truly-the (member ,@members) (car ,n-list))
                               `(car ,n-list))))
                (declare (ignorable ,var))
                ,@decls
                (setq ,n-list (cdr ,n-list))
                (tagbody ,@forms))
              (go ,start))))
       ,(if result
            `(let ((,var nil))
               ;; Filter out TYPE declarations (VAR gets bound to NIL,
               ;; and might have a conflicting type declaration) and
               ;; IGNORE (VAR might be ignored in the loop body, but
               ;; it's used in the result form).
               ,@(filter-dolist-declarations decls)
               ,var
               ,result)
            nil))))


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
    (let ((fn-sym (sb-xc:gensym))) ; for ONCE-ONLY-ish purposes
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
  (let ((initial-buffer '#:buf)
        (dummy '#:stream)
        (string-let (or #+c-stack-is-control-stack 'dx-let 'let))
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
              ;; deduce a known type. You'll get "could not stack allocate"
              ;; perhaps, but that's acceptable.
              'make-string)))
    ;; A full call to MAKE-STRING-OUTPUT-STREAM uses a larger initial buffer
    ;; if BASE-CHAR but I really don't care to think about that here.
    `(,string-let ((,initial-buffer (,string-ctor 31 :element-type ,element-type)))
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
             #+riscv
             ((signed-word) '%raw-instance-cas/signed-word))))
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

#-metaspace
(progn
(sb-xc:defmacro wrapper-friend (x) x)
(sb-xc:defmacro layout-friend (x) x)
(sb-xc:defmacro layout-clos-hash (x) `(wrapper-clos-hash ,x))
#-64-bit (sb-xc:defmacro layout-depthoid (x) `(wrapper-depthoid ,x))
(sb-xc:defmacro layout-flags (x) `(wrapper-flags ,x))
)
