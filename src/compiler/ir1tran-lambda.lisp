;;;; This file contains code which does the translation of lambda
;;;; forms from Lisp code to the first intermediate representation
;;;; (IR1).

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; LAMBDA hackery

;;;; FIXME: where is that file?
;;;; Answer: As usual, with CMUCL.
;;;; Note: Take a look at the compiler-overview.tex section on "Hairy
;;;; function representation" before you seriously mess with this
;;;; stuff.
(declaim (start-block ir1-convert-lambda ir1-convert-lambda-body
                      ir1-convert-aux-bindings varify-lambda-arg
                      ir1-convert-lambdalike))

;;; Return a VAR structure for NAME, filling in info if it is globally
;;; special. If it is losing, we punt with a COMPILER-ERROR.
(declaim (ftype (sfunction (symbol) lambda-var) varify-lambda-arg))
(defun varify-lambda-arg (name)
  (case (info :variable :kind name)
    (:special
     (let ((variable (find-free-var name)))
       (make-lambda-var :%source-name name
                        :type (leaf-type variable)
                        :where-from (leaf-where-from variable)
                        :specvar variable)))
    (t
     (make-lambda-var :%source-name name))))

;;; Parse a lambda list into a list of VAR structures, stripping off
;;; any &AUX bindings. Each arg name is checked for legality, and
;;; duplicate names are checked for. If an arg is globally special,
;;; the var is marked as :SPECIAL instead of :LEXICAL. &KEY,
;;; &OPTIONAL and &REST args are annotated with an ARG-INFO structure
;;; which contains the extra information. If we hit something losing,
;;; we bug out with COMPILER-ERROR. These values are returned:
;;;  1. a list of the var structures for each top level argument;
;;;  2. a flag indicating whether &KEY was specified;
;;;  3. a flag indicating whether other &KEY args are allowed;
;;;  4. a list of the &AUX variables; and
;;;  5. a list of the &AUX values.
(declaim (ftype (sfunction (list) (values list boolean boolean list list))
                make-lambda-vars))
(defun make-lambda-vars (list)
  (multiple-value-bind (llks required optional rest/more keys aux)
      (multiple-value-call #'check-lambda-list-names (parse-lambda-list list))
    (collect ((vars)
              (aux-vars)
              (aux-vals))
      (flet ((add-var (name &optional source-form)
               (let ((var (varify-lambda-arg name)))
                 (setf (lambda-var-source-form var) source-form)
                 (vars var)
                 var))
             (add-info (var kind &key (default nil defaultp) suppliedp-var key)
               (let ((info (make-arg-info :kind kind)))
                 (when defaultp
                   (setf (arg-info-default info) default
                         (arg-info-default-p info) t))
                 (when suppliedp-var
                   (setf (arg-info-supplied-p info)
                         (varify-lambda-arg suppliedp-var)))
                 (when key
                   (setf (arg-info-key info) key))
                 (setf (lambda-var-arg-info var) info))))
        ;; Required
        (mapc #'add-var required)
        ;; Optional
        (dolist (spec optional)
          (multiple-value-bind (name default suppliedp-var defaultp)
              (parse-optional-arg-spec spec)
            (apply #'add-info (add-var name) :optional
                   :suppliedp-var (first suppliedp-var)
                   (when defaultp (list :default default)))))
        ;; Rest/more
        (when rest/more
          (mapc (lambda (name kind)
                  (add-info (add-var name) kind))
                rest/more (let ((morep (eq (ll-kwds-restp llks) '&more)))
                            (if morep '(:more-context :more-count) '(:rest)))))
        ;; Keys
        (dolist (spec keys)
          (multiple-value-bind (keyword name default suppliedp-var defaultp)
              (parse-key-arg-spec spec)
            (apply #'add-info (add-var name spec) :keyword
                   :suppliedp-var (first suppliedp-var)
                   :key keyword
                   (when defaultp (list :default default)))))
        ;; Aux
        (dolist (spec aux)
          (multiple-value-bind (name val)
              (if (atom spec) spec (values (car spec) (cadr spec)))
            (let ((var (varify-lambda-arg name)))
              (aux-vars var)
              (aux-vals val))))

        (values (vars) (ll-kwds-keyp llks) (ll-kwds-allowp llks)
                (aux-vars) (aux-vals))))))

;;; This is similar to IR1-CONVERT-PROGN-BODY except that we
;;; sequentially bind each AUX-VAR to the corresponding AUX-VAL before
;;; converting the body. If there are no bindings, just convert the
;;; body, otherwise do one binding and recurse on the rest.
;;;
;;; FIXME: This could and probably should be converted to use
;;; SOURCE-NAME and DEBUG-NAME. But I (WHN) don't use &AUX bindings,
;;; so I'm not motivated. Patches will be accepted...
(defun ir1-convert-aux-bindings (start next result body aux-vars aux-vals
                                 post-binding-lexenv)
  (declare (type ctran start next) (type (or lvar null) result)
           (list body aux-vars aux-vals))
  (if (null aux-vars)
      (let ((*lexenv* (make-lexenv :vars post-binding-lexenv)))
        (ir1-convert-progn-body start next result body))
      (let ((ctran (make-ctran))
            (fun-lvar (make-lvar))
            (fun (ir1-convert-lambda-body body
                                          (list (first aux-vars))
                                          :aux-vars (rest aux-vars)
                                          :aux-vals (rest aux-vals)
                                          :post-binding-lexenv post-binding-lexenv
                                          :debug-name (debug-name
                                                       '&aux-bindings
                                                       aux-vars))))
        (reference-leaf start ctran fun-lvar fun)
        (ir1-convert-combination-args fun-lvar ctran next result
                                      (list (first aux-vals)))))
  (values))

;;; This is similar to IR1-CONVERT-PROGN-BODY except that code to bind
;;; the SPECVAR for each SVAR to the value of the variable is wrapped
;;; around the body. If there are no special bindings, we just convert
;;; the body, otherwise we do one special binding and recurse on the
;;; rest.
;;;
;;; We make a cleanup and introduce it into the lexical
;;; environment. If there are multiple special bindings, the cleanup
;;; for the blocks will end up being the innermost one. We force NEXT
;;; to start a block outside of this cleanup, causing cleanup code to
;;; be emitted when the scope is exited.
(defun ir1-convert-special-bindings
    (start next result body aux-vars aux-vals svars post-binding-lexenv)
  (declare (type ctran start next) (type (or lvar null) result)
           (list body aux-vars aux-vals svars))
  (cond
   ((null svars)
    (ir1-convert-aux-bindings start next result body aux-vars aux-vals
                              post-binding-lexenv))
   (t
    (ctran-starts-block next)
    (let ((cleanup (make-cleanup :kind :special-bind))
          (var (first svars))
          (bind-ctran (make-ctran))
          (cleanup-ctran (make-ctran)))
      (ir1-convert start bind-ctran nil
                   `(%special-bind ',(leaf-source-name (lambda-var-specvar var)) ,var))
      (setf (cleanup-mess-up cleanup) (ctran-use bind-ctran))
      (let ((*lexenv* (make-lexenv :cleanup cleanup)))
        (ir1-convert bind-ctran cleanup-ctran nil '(%cleanup-point))
        (ir1-convert-special-bindings cleanup-ctran next result
                                      body aux-vars aux-vals
                                      (rest svars)
                                      post-binding-lexenv)))))
  (values))

;;; Create a lambda node out of some code, returning the result. The
;;; bindings are specified by the list of VAR structures VARS. We deal
;;; with adding the names to the LEXENV-VARS for the conversion. The
;;; result is added to the NEW-FUNCTIONALS in the *CURRENT-COMPONENT*
;;; and linked to the component head and tail.
;;;
;;; We detect special bindings here, replacing the original VAR in the
;;; lambda list with a temporary variable. We then pass a list of the
;;; special vars to IR1-CONVERT-SPECIAL-BINDINGS, which actually emits
;;; the special binding code.
;;;
;;; We ignore any ARG-INFO in the VARS, trusting that someone else is
;;; dealing with &NONSENSE, except for &REST vars with DYNAMIC-EXTENT.
;;;
;;; AUX-VARS is a list of VAR structures for variables that are to be
;;; sequentially bound. Each AUX-VAL is a form that is to be evaluated
;;; to get the initial value for the corresponding AUX-VAR.
(defun ir1-convert-lambda-body (body
                                vars
                                &key
                                aux-vars
                                aux-vals
                                (source-name '.anonymous.)
                                debug-name
                                (note-lexical-bindings t)
                                post-binding-lexenv
                                system-lambda
                                local-policy)
  (declare (list body vars aux-vars aux-vals))

  ;; We're about to try to put new blocks into *CURRENT-COMPONENT*.
  (aver-live-component *current-component*)

  (let* ((bind (make-bind))
         (lambda (make-lambda :vars vars
                              :bind bind
                              :%source-name source-name
                              :%debug-name debug-name
                              :system-lambda-p system-lambda
                              :lexenv (if local-policy
                                          (make-lexenv :policy local-policy)
                                          *lexenv*)))
         (result-ctran (make-ctran))
         (result-lvar (make-lvar)))
    ;; just to check: This function should fail internal assertions if
    ;; we didn't set up a valid debug name above.
    ;;
    ;; (In SBCL we try to make everything have a debug name, since we
    ;; lack the omniscient perspective the original implementors used
    ;; to decide which things didn't need one.)
    (functional-debug-name lambda)

    (setf (lambda-home lambda) lambda)
    (collect ((svars)
              (new-venv nil cons))

      (dolist (var vars)
        ;; As far as I can see, LAMBDA-VAR-HOME should never have
        ;; been set before. Let's make sure. -- WHN 2001-09-29
        (aver (not (lambda-var-home var)))
        (setf (lambda-var-home var) lambda)
        (let ((specvar (lambda-var-specvar var)))
          (cond (specvar
                 (svars var)
                 (new-venv (cons (leaf-source-name specvar) specvar)))
                (t
                 (when note-lexical-bindings
                   (note-lexical-binding (leaf-source-name var)))
                 (new-venv (cons (leaf-source-name var) var))))))

      (let ((*lexenv* (make-lexenv :vars (new-venv)
                                   :lambda lambda
                                   :cleanup nil)))
        (setf (bind-lambda bind) lambda)
        (setf (node-lexenv bind) *lexenv*)

        (let ((block (ctran-starts-block result-ctran)))
          (let ((return (make-return :result result-lvar :lambda lambda))
                (tail-set (make-tail-set :funs (list lambda))))
            (setf (lambda-tail-set lambda) tail-set)
            (setf (lambda-return lambda) return)
            (setf (lvar-dest result-lvar) return)
            (link-node-to-previous-ctran return result-ctran)
            (setf (block-last block) return))
          (link-blocks block (component-tail *current-component*)))

        (with-component-last-block (*current-component*
                                    (ctran-block result-ctran))
          (let ((prebind-ctran (make-ctran))
                (postbind-ctran (make-ctran)))
            (ctran-starts-block prebind-ctran)
            (link-node-to-previous-ctran bind prebind-ctran)
            (use-ctran bind postbind-ctran)
            (ir1-convert-special-bindings postbind-ctran result-ctran
                                          result-lvar body
                                          aux-vars aux-vals (svars)
                                          post-binding-lexenv)))))

    (link-blocks (component-head *current-component*) (node-block bind))
    (push lambda (component-new-functionals *current-component*))

    lambda))

;;; Entry point CLAMBDAs have a special kind
(defun register-entry-point (entry dispatcher)
  (declare (type clambda entry)
           (type optional-dispatch dispatcher))
  (setf (functional-kind entry) :optional)
  (setf (leaf-ever-used entry) t)
  (setf (lambda-optional-dispatch entry) dispatcher)
  entry)

;;; Create the actual entry-point function for an optional entry
;;; point. The lambda binds copies of each of the VARS, then calls FUN
;;; with the argument VALS and the DEFAULTS. Presumably the VALS refer
;;; to the VARS by name. The VALS are passed in the reverse order.
;;;
;;; If any of the copies of the vars are referenced more than once,
;;; then we mark the corresponding var as EVER-USED to inhibit
;;; "defined but not read" warnings for arguments that are only used
;;; by default forms.
(defun convert-optional-entry (fun vars vals defaults name)
  (declare (type clambda fun) (list vars vals defaults))
  (let* ((fvars (reverse vars))
         (arg-vars (mapcar (lambda (var)
                             (make-lambda-var
                              :%source-name (leaf-source-name var)
                              :type (leaf-type var)
                              :where-from (leaf-where-from var)
                              :specvar (lambda-var-specvar var)))
                           fvars))
         (fun (collect ((default-bindings)
                        (default-vals))
                (dolist (default defaults)
                  (if (constantp default)
                      (default-vals default)
                      (let ((var (sb-xc:gensym)))
                        (default-bindings `(,var ,default))
                        (default-vals var))))
                (let ((bindings (default-bindings))
                      (call
                       `(locally
                         ;; See lengthy comment at top of 'seqtran'
                         ;; as to why muffling is not done during xc.
                            #-sb-xc-host
                            (declare (muffle-conditions code-deletion-note))
                          (%funcall ,fun ,@(reverse vals) ,@(default-vals)))))
                  (ir1-convert-lambda-body (if bindings
                                               `((let (,@bindings) ,call))
                                               `(,call))
                                          arg-vars
                                          ;; FIXME: Would be nice to
                                          ;; share these names instead
                                          ;; of consing up several
                                          ;; identical ones. Oh well.
                                          :debug-name (debug-name
                                                       '&optional-processor
                                                       name)
                                          :note-lexical-bindings nil
                                          :system-lambda t)))))
    (mapc (lambda (var arg-var)
            (when (cdr (leaf-refs arg-var))
              (setf (leaf-ever-used var) t)))
          fvars arg-vars)
    fun))

;;; This function deals with supplied-p vars in optional arguments. If
;;; there is no supplied-p arg, then we just call
;;; IR1-CONVERT-HAIRY-ARGS on the remaining arguments, and generate a
;;; optional entry that calls the result. If there is a supplied-p
;;; var, then we add it into the default vars and throw a T into the
;;; entry values. The resulting entry point function is returned.
(defun generate-optional-default-entry (res default-vars default-vals
                                        entry-vars entry-vals
                                        vars supplied-p-p body
                                        aux-vars aux-vals
                                        source-name debug-name
                                        force post-binding-lexenv
                                        system-lambda)
  (declare (type optional-dispatch res)
           (list default-vars default-vals entry-vars entry-vals vars body
                 aux-vars aux-vals))
  (let* ((arg (first vars))
         (arg-name (leaf-source-name arg))
         (info (lambda-var-arg-info arg))
         (default (arg-info-default info))
         (supplied-p (arg-info-supplied-p info))
         (force (or force
                    (not (constantp (arg-info-default info)))))
         (ep (if supplied-p
                 (ir1-convert-hairy-args
                  res
                  (list* supplied-p arg default-vars)
                  (list* (leaf-source-name supplied-p) arg-name default-vals)
                  (cons arg entry-vars)
                  (list* t arg-name entry-vals)
                  (rest vars) t body aux-vars aux-vals
                  source-name debug-name
                  force post-binding-lexenv system-lambda)
                 (ir1-convert-hairy-args
                  res
                  (cons arg default-vars)
                  (cons arg-name default-vals)
                  (cons arg entry-vars)
                  (cons arg-name entry-vals)
                  (rest vars) supplied-p-p body aux-vars aux-vals
                  source-name debug-name
                  force post-binding-lexenv system-lambda))))

    ;; We want to delay converting the entry, but there exist
    ;; problems: hidden references should not be established to
    ;; lambdas of kind NIL should not have (otherwise the compiler
    ;; might let-convert or delete them) and to variables.
    (let ((name (or debug-name source-name)))
      (if (or force
              supplied-p-p ; this entry will be of kind NIL
              (and (lambda-p ep) (eq (lambda-kind ep) nil)))
          (convert-optional-entry ep
                                  default-vars default-vals
                                  (if supplied-p (list default nil) (list default))
                                  name)
          (let* ((value (constant-form-value default))
                 ;; One-and-only-once-more: MAYBE-EMIT-MAKE-LOAD-FORMS has a similar test.
                 (namedp-not-eql-comparable
                  (and (symbolp default)
                       (not (typep value '(or symbol character number)))))
                 (default (if namedp-not-eql-comparable default `',value))
                 (defaults (if supplied-p (list default nil) (list default))))
            ;; DEFAULT can contain a reference to a
            ;; to-be-optimized-away function/block/tag, so better to
            ;; reduce code now (but we possibly lose syntax checking
            ;; in an unreachable code).
            (delay
             (register-entry-point
              (convert-optional-entry (force ep)
                                      default-vars default-vals
                                      defaults
                                      name)
              res)))))))

;;; Create the MORE-ENTRY function for the OPTIONAL-DISPATCH RES.
;;; ENTRY-VARS and ENTRY-VALS describe the fixed arguments. REST is
;;; the var for any &REST arg. KEYS is a list of the &KEY arg vars.
;;;
;;; The most interesting thing that we do is parse keywords. We create
;;; a bunch of temporary variables to hold the result of the parse,
;;; and then loop over the supplied arguments, setting the appropriate
;;; temps for the supplied keyword. Note that it is significant that
;;; we iterate over the keywords in reverse order --- this implements
;;; the CL requirement that (when a keyword appears more than once)
;;; the first value is used.
;;;
;;; If there is no supplied-p var, then we initialize the temp to the
;;; default and just pass the temp into the main entry. Since
;;; non-constant &KEY args are forcibly given a supplied-p var, we
;;; know that the default is constant, and thus safe to evaluate out
;;; of order.
;;;
;;; If there is a supplied-p var, then we create temps for both the
;;; value and the supplied-p, and pass them into the main entry,
;;; letting it worry about defaulting.
;;;
;;; We deal with :ALLOW-OTHER-KEYS by delaying unknown keyword errors
;;; until we have scanned all the keywords.
(defun convert-more-entry (res entry-vars entry-vals rest morep keys name)
  (declare (type optional-dispatch res) (list entry-vars entry-vals keys))
  (collect ((arg-vars)
            (arg-vals (reverse entry-vals))
            (temps)
            (body))

    (dolist (var (reverse entry-vars))
      (arg-vars (make-lambda-var :%source-name (leaf-source-name var)
                                 :type (leaf-type var)
                                 :where-from (leaf-where-from var))))

    (let* ((n-context (sb-xc:gensym "N-CONTEXT-"))
           (context-temp (make-lambda-var :%source-name n-context
                                          :arg-info (make-arg-info :kind :more-context)))
           (n-count (sb-xc:gensym "N-COUNT-"))
           (count-temp (make-lambda-var :%source-name n-count
                                        :type (specifier-type 'index)
                                        :arg-info (make-arg-info :kind :more-count))))

      (arg-vars context-temp count-temp)

      (when rest
        (arg-vals `(%listify-rest-args ,n-context ,n-count)))
      (when morep
        (arg-vals n-context)
        (arg-vals n-count))

      ;; The reason for all the noise with
      ;; STACK-GROWS-DOWNWARD-NOT-UPWARD is to enable generation of
      ;; slightly more efficient code on x86oid processors.  (We can
      ;; hoist the negation of the index outside the main parsing loop
      ;; and take advantage of the base+index+displacement addressing
      ;; mode on x86oids.)
      (when (optional-dispatch-keyp res)
        (let ((n-index (sb-xc:gensym "N-INDEX-"))
              (n-key (sb-xc:gensym "N-KEY-"))
              (n-value-temp (sb-xc:gensym "N-VALUE-TEMP-"))
              (n-allowp (sb-xc:gensym "N-ALLOWP-"))
              (n-lose (sb-xc:gensym "N-LOSE-"))
              (allowp (or (optional-dispatch-allowp res)
                          (policy *lexenv* (zerop safety))))
              (found-allow-p nil))

          (temps #-stack-grows-downward-not-upward
                 `(,n-index (+ ,n-count ,(if (vop-existsp :translate %more-keyword-pair)
                                             0
                                             -1)))
                 #+stack-grows-downward-not-upward
                 `(,n-index (- (1- ,n-count)))
                 #-stack-grows-downward-not-upward n-value-temp
                 #-stack-grows-downward-not-upward n-key)
          (body `(declare (fixnum ,n-index)
                          #-stack-grows-downward-not-upward
                          (ignorable ,n-value-temp ,n-key)))

          (collect ((tests))
            (dolist (key keys)
              (let* ((info (lambda-var-arg-info key))
                     (default (arg-info-default info))
                     (keyword (arg-info-key info))
                     (supplied-p (arg-info-supplied-p info))
                     (supplied-used-p (arg-info-supplied-used-p info))
                     (n-value (sb-xc:gensym "N-VALUE-"))
                     (clause (cond (supplied-p
                                    (let ((n-supplied (sb-xc:gensym "N-SUPPLIED-")))
                                      (temps (list n-supplied
                                                   (if supplied-used-p
                                                       nil
                                                       0)))
                                      (arg-vals n-value n-supplied)
                                      `((,keyword)
                                        (setq ,n-supplied ,(if supplied-used-p
                                                               t
                                                               1))
                                        (setq ,n-value ,n-value-temp))))
                                   (t
                                    (arg-vals n-value)
                                    `((,keyword)
                                      (setq ,n-value ,n-value-temp))))))
                (when (and (not allowp) (eq keyword :allow-other-keys))
                  (setq found-allow-p t)
                  (setq clause
                        (append clause `((setq ,n-allowp ,n-value-temp)))))
                (temps `(,n-value ,(if (and default
                                            (neq (lambda-var-type key) *universal-type*))
                                       `(the* (,(lambda-var-type key)
                                               :use-annotations t
                                               :source-form ,(lambda-var-source-form key))
                                              ,default)
                                       default)))
                (tests clause)))

            (unless allowp
              (temps n-allowp
                     (list n-lose '(make-unbound-marker)))
              (unless found-allow-p
                (tests `(:allow-other-keys
                         (setq ,n-allowp ,n-value-temp))))
              (tests `(t
                       (setq ,n-lose ,n-key))))

            (body
             `(when (oddp ,(if (vop-existsp :translate %more-keyword-pair)
                               n-index
                               n-count))
                (%odd-key-args-error)))

            (body
             #-stack-grows-downward-not-upward
             `(locally
                (declare (optimize (safety 0)))
                (loop
                 ,@(cond ((vop-existsp :translate %more-keyword-pair)
                          `((when (zerop ,n-index) (return))
                            (decf ,n-index 2)
                            (multiple-value-bind (key value)
                                (%more-keyword-pair ,n-context ,n-index)
                              (setf ,n-value-temp value ,n-key key))))
                         (t
                          `((when (minusp ,n-index) (return))
                            (setf ,n-value-temp (%more-arg ,n-context ,n-index))
                            (decf ,n-index)
                            (setq ,n-key (%more-arg ,n-context ,n-index))
                            (decf ,n-index))))
                 (case ,n-key ,@(tests))))
             #+stack-grows-downward-not-upward
             `(locally (declare (optimize (safety 0)))
                (loop
                  (when (plusp ,n-index) (return))
                  (multiple-value-bind (,n-value-temp ,n-key)
                      (%more-kw-arg ,n-context ,n-index)
                    (declare (ignorable ,n-value-temp ,n-key))
                    (incf ,n-index 2)
                    (case ,n-key ,@(tests))))))

            (unless allowp
              (let ((location (opaquely-quote (make-restart-location))))
                (body `(if (and (not (unbound-marker-p ,n-lose))
                                (not ,n-allowp))
                           (%unknown-key-arg-error ,n-lose ,location)
                           (restart-point ,location))))))))

      (let ((ep (ir1-convert-lambda-body
                 `((let ,(temps)
                     ,@(body)
                     (%funcall ,(optional-dispatch-main-entry res)
                               ,@(arg-vals))))
                 (arg-vars)
                 :debug-name (debug-name '&more-processor name)
                 :note-lexical-bindings nil
                 :system-lambda t)))
        (setf (optional-dispatch-more-entry res)
              (register-entry-point ep res)))))

  (values))

;;; This is called by IR1-CONVERT-HAIRY-ARGS when we run into a &REST
;;; or &KEY arg. The arguments are similar to that function, but we
;;; split off any &REST arg and pass it in separately. REST is the
;;; &REST arg var, or NIL if there is no &REST arg. KEYS is a list of
;;; the &KEY argument vars.
;;;
;;; When there are &KEY arguments, we introduce temporary gensym
;;; variables to hold the values while keyword defaulting is in
;;; progress to get the required sequential binding semantics.
;;;
;;; This gets interesting mainly when there are &KEY arguments with
;;; supplied-p vars or non-constant defaults. In either case, pass in
;;; a supplied-p var. If the default is non-constant, we introduce an
;;; IF in the main entry that tests the supplied-p var and decides
;;; whether to evaluate the default or not. In this case, the real
;;; incoming value is NIL, so we must union NULL with the declared
;;; type when computing the type for the main entry's argument.
(defun ir1-convert-more (res default-vars default-vals entry-vars entry-vals
                         rest more-context more-count keys supplied-p-p
                         body aux-vars aux-vals source-name debug-name
                         post-binding-lexenv system-lambda)
  (declare (type optional-dispatch res)
           (list default-vars default-vals entry-vars entry-vals keys body
                 aux-vars aux-vals))
  (collect ((main-vars (reverse default-vars))
            (main-vals default-vals cons)
            (bind-vars)
            (bind-vals))
    (when rest
      (main-vars rest)
      (main-vals '())
      (unless (lambda-var-ignorep rest)
        ;; Make up two extra variables, and squirrel them away in
        ;; ARG-INFO-DEFAULT for transforming (VALUES-LIST REST) into
        ;; (%MORE-ARG-VALUES CONTEXT 0 COUNT) when possible.
        (let* ((context-name (sb-xc:gensym "REST-CONTEXT-"))
               (context (make-lambda-var :%source-name context-name
                                         :arg-info (make-arg-info :kind :more-context)))
               (count-name (sb-xc:gensym "REST-COUNT-"))
               (count (make-lambda-var :%source-name count-name
                                       :arg-info (make-arg-info :kind :more-count)
                                       :type (specifier-type 'index))))
          (setf (arg-info-default (lambda-var-arg-info rest)) (list context count)
                (lambda-var-ever-used context) t
                (lambda-var-ever-used count) t)
          (setf more-context context
                more-count count))))
    (when more-context
      (main-vars more-context)
      (main-vals nil)
      (main-vars more-count)
      (main-vals 0))

    (dolist (key keys)
      (let* ((info (lambda-var-arg-info key))
             (default (arg-info-default info))
             (hairy-default (not (constantp default)))
             (supplied-p (arg-info-supplied-p info))
             ;; was: (format nil "~A-DEFAULTING-TEMP" (leaf-source-name key))
             (n-val (make-symbol ".DEFAULTING-TEMP."))
             (val-temp (make-lambda-var :%source-name n-val))
             (default `(with-source-form ,(lambda-var-source-form key)
                         ,default)))
        (main-vars val-temp)
        (bind-vars key)
        (cond ((or hairy-default supplied-p)
               (let* ((n-supplied (sb-xc:gensym "N-SUPPLIED-"))
                      (supplied-temp (make-lambda-var
                                      :%source-name n-supplied)))
                 (unless supplied-p
                   (setf (arg-info-supplied-p info) supplied-temp))
                 (when hairy-default
                   (setf (arg-info-default info) nil)
                   (unless supplied-p
                     (setf (arg-info-supplied-used-p info) nil)))
                 (main-vars supplied-temp)
                 (cond (hairy-default
                        (main-vals nil
                                   (if supplied-p
                                       nil
                                       0))
                        (bind-vals
                         (if supplied-p
                             `(if ,n-supplied ,n-val ,default)
                             `(if (eq ,n-supplied 0) ,default ,n-val))))
                       (t
                        (main-vals default nil)
                        (bind-vals n-val)))
                 (when supplied-p
                   (bind-vars supplied-p)
                   (bind-vals n-supplied))))
              (t
               (main-vals default)
               (bind-vals n-val)))))

    (let* ((main-entry (ir1-convert-lambda-body
                        body (main-vars)
                        :aux-vars (append (bind-vars) aux-vars)
                        :aux-vals (append (bind-vals) aux-vals)
                        :post-binding-lexenv post-binding-lexenv
                        :source-name source-name
                        :debug-name debug-name
                        :system-lambda system-lambda))
           (name (or debug-name source-name))
           (last-entry (convert-optional-entry main-entry default-vars
                                               (main-vals) () name)))
      (setf (optional-dispatch-main-entry res)
            (register-entry-point main-entry res))
      (convert-more-entry res entry-vars entry-vals rest more-context keys
                          name)

      (push (register-entry-point
             (if supplied-p-p
                (convert-optional-entry last-entry entry-vars entry-vals
                                        () name)
                last-entry)
             res)
            (optional-dispatch-entry-points res))
      last-entry)))

;;; This function generates the entry point functions for the
;;; OPTIONAL-DISPATCH RES. We accomplish this by recursion on the list
;;; of arguments, analyzing the arglist on the way down and generating
;;; entry points on the way up.
;;;
;;; DEFAULT-VARS is a reversed list of all the argument vars processed
;;; so far, including supplied-p vars. DEFAULT-VALS is a list of the
;;; names of the DEFAULT-VARS.
;;;
;;; ENTRY-VARS is a reversed list of processed argument vars,
;;; excluding supplied-p vars. ENTRY-VALS is a list things that can be
;;; evaluated to get the values for all the vars from the ENTRY-VARS.
;;; It has the var name for each required or optional arg, and has T
;;; for each supplied-p arg.
;;;
;;; VARS is a list of the LAMBDA-VAR structures for arguments that
;;; haven't been processed yet. SUPPLIED-P-P is true if a supplied-p
;;; argument has already been processed; only in this case are the
;;; DEFAULT-XXX and ENTRY-XXX different.
;;;
;;; The result at each point is a lambda which should be called by the
;;; above level to default the remaining arguments and evaluate the
;;; body. We cause the body to be evaluated by converting it and
;;; returning it as the result when the recursion bottoms out.
;;;
;;; Each level in the recursion also adds its entry point function to
;;; the result OPTIONAL-DISPATCH. For most arguments, the defaulting
;;; function and the entry point function will be the same, but when
;;; SUPPLIED-P args are present they may be different.
;;;
;;; When we run into a &REST or &KEY arg, we punt out to
;;; IR1-CONVERT-MORE, which finishes for us in this case.
(defun ir1-convert-hairy-args (res default-vars default-vals
                               entry-vars entry-vals
                               vars supplied-p-p body aux-vars
                               aux-vals
                               source-name debug-name
                               force post-binding-lexenv
                               system-lambda)
  (declare (type optional-dispatch res)
           (list default-vars default-vals entry-vars entry-vals vars body
                 aux-vars aux-vals))
  (aver (or debug-name (neq '.anonymous. source-name)))
  (cond ((not vars)
         (if (optional-dispatch-keyp res)
             ;; Handle &KEY with no keys...
             (ir1-convert-more res default-vars default-vals
                               entry-vars entry-vals
                               nil nil nil vars supplied-p-p body aux-vars
                               aux-vals source-name debug-name
                               post-binding-lexenv system-lambda)
             (let* ((name (or debug-name source-name))
                    (fun (ir1-convert-lambda-body
                         body (reverse default-vars)
                         :aux-vars aux-vars
                         :aux-vals aux-vals
                         :post-binding-lexenv post-binding-lexenv
                         :source-name source-name
                         :debug-name debug-name
                         :system-lambda system-lambda)))

               (setf (optional-dispatch-main-entry res) fun)
               (register-entry-point fun res)
               (push (if supplied-p-p
                         (register-entry-point
                          (convert-optional-entry fun entry-vars entry-vals ()
                                                  name)
                          res)
                          fun)
                     (optional-dispatch-entry-points res))
               fun)))
        ((not (lambda-var-arg-info (first vars)))
         (let* ((arg (first vars))
                (nvars (cons arg default-vars))
                (nvals (cons (leaf-source-name arg) default-vals)))
           (ir1-convert-hairy-args res nvars nvals nvars nvals
                                   (rest vars) nil body aux-vars aux-vals
                                   source-name debug-name
                                   nil post-binding-lexenv system-lambda)))
        (t
         (let* ((arg (first vars))
                (info (lambda-var-arg-info arg))
                (kind (arg-info-kind info)))
           (ecase kind
             (:optional
              (let ((ep (generate-optional-default-entry
                         res default-vars default-vals
                         entry-vars entry-vals vars supplied-p-p body
                         aux-vars aux-vals
                         source-name debug-name
                         force post-binding-lexenv
                         system-lambda)))
                ;; See GENERATE-OPTIONAL-DEFAULT-ENTRY.
                (push (if (lambda-p ep)
                          (register-entry-point
                           (if supplied-p-p
                               (convert-optional-entry
                                ep entry-vars entry-vals nil
                                (or debug-name source-name))
                               ep)
                           res)
                          (progn (aver (not supplied-p-p))
                                 ep))
                      (optional-dispatch-entry-points res))
                ep))
             (:rest
              (ir1-convert-more res default-vars default-vals
                                entry-vars entry-vals
                                arg nil nil (rest vars) supplied-p-p body
                                aux-vars aux-vals
                                source-name debug-name
                                post-binding-lexenv system-lambda))
             (:more-context
              (ir1-convert-more res default-vars default-vals
                                entry-vars entry-vals
                                nil arg (second vars) (cddr vars) supplied-p-p
                                body aux-vars aux-vals
                                source-name debug-name
                                post-binding-lexenv system-lambda))
             (:keyword
              (ir1-convert-more res default-vars default-vals
                                entry-vars entry-vals
                                nil nil nil vars supplied-p-p body aux-vars
                                aux-vals source-name debug-name
                                post-binding-lexenv system-lambda)))))))

;;; This function deals with the case where we have to make an
;;; OPTIONAL-DISPATCH to represent a LAMBDA. We cons up the result and
;;; call IR1-CONVERT-HAIRY-ARGS to do the work. When it is done, we
;;; figure out the MIN-ARGS and MAX-ARGS.
(defun ir1-convert-hairy-lambda (body vars keyp allowp aux-vars aux-vals
                                 &key post-binding-lexenv
                                 (source-name '.anonymous.)
                                 debug-name system-lambda)
  (declare (list body vars aux-vars aux-vals))
  (aver (or debug-name (neq '.anonymous. source-name)))
  (let ((res (make-optional-dispatch :arglist vars
                                     :allowp allowp
                                     :keyp keyp
                                     :%source-name source-name
                                     :%debug-name debug-name
                                     :source-path *current-path*))
        (min (or (position-if #'lambda-var-arg-info vars) (length vars))))
    (aver-live-component *current-component*)
    (ir1-convert-hairy-args res () () () () vars nil body aux-vars aux-vals
                            source-name debug-name nil post-binding-lexenv
                            system-lambda)
    ;; ir1-convert-hairy-args can throw 'locall-already-let-converted
    ;; push optional-dispatch into the current component only after it
    ;; normally returned
    (push res (component-new-functionals *current-component*))
    (setf (optional-dispatch-min-args res) min)
    (setf (optional-dispatch-max-args res)
          (+ (1- (length (optional-dispatch-entry-points res))) min))

    res))

;;; Convert a LAMBDA form into a LAMBDA leaf or an OPTIONAL-DISPATCH leaf.
(defun ir1-convert-lambda (form &key (source-name '.anonymous.)
                           debug-name maybe-add-debug-catch
                           system-lambda)
  (unless (consp form)
    (compiler-error "A ~S was found when expecting a lambda expression:~%  ~S"
                    (type-of form)
                    form))
  (unless (eq (car form) 'lambda)
    (compiler-error "~S was expected but ~S was found:~%  ~S"
                    'lambda
                    (car form)
                    form))
  (unless (and (consp (cdr form)) (listp (cadr form)))
    (compiler-error
     "The lambda expression has a missing or non-list lambda list:~%  ~S"
     form))
  (when (and system-lambda maybe-add-debug-catch)
    (bug "Both SYSTEM-LAMBDA and MAYBE-ADD-DEBUG-CATCH specified"))
  (unless (or debug-name (neq '.anonymous. source-name))
    (setf debug-name (name-lambdalike form)))
  (multiple-value-bind (forms decls doc) (parse-body (cddr form) t)
    (let ((*lexenv* (process-muffle-decls decls *lexenv*)))
      (multiple-value-bind (vars keyp allow-other-keys aux-vars aux-vals)
          (make-lambda-vars (cadr form))
        (binding* (((*lexenv* result-type post-binding-lexenv
                              lambda-list explicit-check source-form
                              local-policy)
                    (process-decls decls (append aux-vars vars) nil
                                   :binding-form-p t :allow-lambda-list t))
                   (debug-catch-p (and maybe-add-debug-catch
                                       *allow-instrumenting*
                                       (policy *lexenv*
                                           (>= insert-debug-catch 2))))
                   (forms (if debug-catch-p
                              (wrap-forms-in-debug-catch forms)
                              forms))
                   (forms (if (eq result-type *wild-type*)
                              forms
                              `((the ,(type-specifier result-type) (progn ,@forms)))))
                   (*allow-instrumenting* (and (not system-lambda) *allow-instrumenting*))
                   (res (cond ((or (find-if #'lambda-var-arg-info vars) keyp)
                               (ir1-convert-hairy-lambda forms vars keyp
                                                         allow-other-keys
                                                         aux-vars aux-vals
                                                         :post-binding-lexenv post-binding-lexenv
                                                         :source-name source-name
                                                         :debug-name debug-name
                                                         :system-lambda system-lambda))
                              (t
                               (ir1-convert-lambda-body forms vars
                                                        :aux-vars aux-vars
                                                        :aux-vals aux-vals
                                                        :post-binding-lexenv post-binding-lexenv
                                                        :source-name source-name
                                                        :debug-name debug-name
                                                        :system-lambda system-lambda
                                                        :local-policy local-policy)))))
          (when explicit-check
            (setf (getf (functional-plist res) 'explicit-check) explicit-check))
          (setf (functional-inline-expansion res) (or source-form form))
          (setf (functional-arg-documentation res)
                (if (eq lambda-list :unspecified)
                    (strip-lambda-list (cadr form) :arglist)
                    lambda-list))
          (setf (functional-documentation res) doc)
          (when (boundp '*lambda-conversions*)
            ;; KLUDGE: Not counting TL-XEPs is a lie, of course, but
            ;; keeps things less confusing to users of TIME, where this
            ;; count gets used.
            (unless (and (consp debug-name) (eq 'tl-xep (car debug-name)))
              (incf *lambda-conversions*)))
          res)))))

(defun wrap-forms-in-debug-catch (forms)
  #+unwind-to-frame-and-call-vop
  `((multiple-value-prog1
      (progn
        ,@forms)
      ;; Just ensure that there won't be any tail-calls, IR2 magic will
      ;; handle the rest.
      (values)))
  #-unwind-to-frame-and-call-vop
  `( ;; Normally, we'll return from this block with the below RETURN-FROM.
    (block
        return-value-tag
      ;; If DEBUG-CATCH-TAG is thrown (with a thunk as the value) the
      ;; RETURN-FROM is elided and we funcall the thunk instead. That
      ;; thunk might either return a value (for a RETURN-FROM-FRAME)
      ;; or call this same function again (for a RESTART-FRAME).
      ;; -- JES, 2007-01-09
      (funcall
       (the function
         ;; Use a constant catch tag instead of consing a new one for every
         ;; entry to this block. The uniquencess of the catch tags is
         ;; ensured when the tag is throw by the debugger. It'll allocate a
         ;; new tag, and modify the reference this tag in the proper
         ;; catch-block structure to refer to that new tag. This
         ;; significantly decreases the runtime cost of high debug levels.
         ;;  -- JES, 2007-01-09
         (catch 'debug-catch-tag
           (return-from return-value-tag
             (progn
               ,@forms))))))))

;; FIXME: really should be an aspect of the lexical environment,
;; but LEXENVs don't know whether they are toplevel or not.
(defun has-toplevelness-decl (lambda-expr)
  (dolist (expr (cddr lambda-expr)) ; Skip over (LAMBDA (ARGS))
    (cond ((equal expr '(declare (top-level-form))) (return t))
          ((typep expr '(or (cons (eql declare)) string))) ; DECL | DOCSTRING
          (t (return nil)))))

;;; helper for LAMBDA-like things, to massage them into a form
;;; suitable for IR1-CONVERT-LAMBDA.
(defun ir1-convert-lambdalike (thing
                               &key
                               (source-name '.anonymous.)
                               debug-name)
  (when (and (not debug-name) (eq '.anonymous. source-name))
    (setf debug-name (name-lambdalike thing)))
  (ecase (car thing)
    ((lambda)
     (ir1-convert-lambda thing
                         :maybe-add-debug-catch t
                         :source-name source-name
                         :debug-name debug-name))
    ((named-lambda)
     (let* ((name (cadr thing))
            (lambda-expression `(lambda ,@(cddr thing)))
            (*inline-expansions* (list name 1 *inline-expansions*)))
       (if (and name (legal-fun-name-p name))
           (let ((defined-fun-res (get-defined-fun name (second lambda-expression)))
                 (res (ir1-convert-lambda lambda-expression
                                          :maybe-add-debug-catch t
                                          :source-name name))
                 (info (info :function :info name)))
             (setf (functional-inlinep res) (info :function :inlinep name)
                   (defined-fun-named-lambda-p defined-fun-res) t)
             (when (has-toplevelness-decl lambda-expression)
               (setf (functional-top-level-defun-p res) t))
             ;; FIXME: Should non-entry block compiled defuns have
             ;; this propagate?
             (assert-global-function-definition-type name res)
             (push res (defined-fun-functionals defined-fun-res))
             (unless (or
                      (eq (defined-fun-inlinep defined-fun-res) 'notinline)
                      ;; Don't treat recursive stubs like CAR as self-calls
                      ;; Maybe just use the fact that it is a known function?
                      ;; Though a known function may be used
                      ;; because of some other attributues but
                      ;; still wants to get optimized self calls
                      (and info
                           (or (fun-info-templates info)
                               (fun-info-transforms info)
                               (fun-info-ltn-annotate info)
                               (fun-info-ir2-convert info)
                               (fun-info-optimizer info))))
               (if (block-compile *compilation*)
                   (substitute-leaf res defined-fun-res)
                   (substitute-leaf-if
                    (lambda (ref)
                      (policy ref (> recognize-self-calls 0)))
                    res defined-fun-res)))
             res)
           (ir1-convert-lambda lambda-expression
                               :maybe-add-debug-catch t
                               :debug-name
                               (or name (name-lambdalike thing))))))
    ((lambda-with-lexenv)
     (ir1-convert-inline-lambda thing
                                :source-name source-name
                                :debug-name debug-name))))

(declaim (end-block))


;;;; defining global functions
;;; Given a lambda-list, return a FUN-TYPE object representing the signature:
;;; return type is *, and each individual arguments type is T -- but we get
;;; the argument counts and keywords.
;;; TODO: enhance this to optionally accept an alist of (var . type)
;;; and use that lieu of SB-INTERPRETER:APPROXIMATE-PROTO-FN-TYPE.
(defun ftype-from-lambda-list (lambda-list)
  (multiple-value-bind (llks req opt rest key-list)
      (parse-lambda-list lambda-list :silent t)
    (flet ((list-of-t (list) (mapcar (constantly t) list)))
      (let ((reqs (list-of-t req))
            (opts (when opt (cons '&optional (list-of-t opt))))
            ;; When it comes to building a type, &REST means pretty much the
            ;; same thing as &MORE.
            (rest (when rest '(&rest t)))
            (keys (when (ll-kwds-keyp llks)
                    (cons '&key (mapcar (lambda (spec)
                                          (list (parse-key-arg-spec spec) t))
                                        key-list))))
            (allow (when (ll-kwds-allowp llks) '(&allow-other-keys))))
        (careful-specifier-type `(function (,@reqs ,@opts ,@rest ,@keys ,@allow) *))))))

(declaim (start-block maybe-inline-syntactic-closure))

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
                   (let ((expression (function-lambda-expression (cdr what))))
                     (aver expression)
                     (push (cons name expression) macros)))
                  ;; FIXME: Is there a good reason for this not to be
                  ;; DEFINED-FUN (which :INCLUDEs GLOBAL-VAR, in case
                  ;; you're wondering how this ever worked :-)? Maybe
                  ;; in conjunction with an AVERrance that it's not an
                  ;; (AND GLOBAL-VAR (NOT GLOBAL-FUN))? -- CSR,
                  ;; 2002-07-08
                  (global-var
                   (unless (defined-fun-p what)
                     (return-from reconstruct-lexenv))
                   (push `(,(car (defined-fun-inlinep what))
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
   #+(and sb-fasteval (not sb-xc-host))
   (sb-interpreter:basic-env
    (awhen (sb-interpreter::reconstruct-syntactic-closure-env lexenv)
      `(lambda-with-lexenv ,it ,@(cdr lambda))))
   #+sb-fasteval
   (null lambda))) ; trivial case. Never occurs in the compiler.

(declaim (end-block))

(declaim (start-block ir1-convert-inline-lambda))

;;; Convert the forms produced by RECONSTRUCT-LEXENV to LEXENV
(defun process-inline-lexenv (inline-lexenv)
  (labels ((recurse (inline-lexenv lexenv)
             (let ((*lexenv* lexenv))
               (if (null inline-lexenv)
                   lexenv
                   (destructuring-bind (type bindings &optional body) inline-lexenv
                     (case type
                       (:declare
                        (recurse body
                                 (process-decls `((declare ,@bindings)) nil nil)))
                       (:macro
                        (let ((macros
                               (mapcar (lambda (binding)
                                         ;; XC compile-in-lexenv ignores its second arg
                                         #+sb-xc-host (aver (null-lexenv-p lexenv))
                                         (list* (car binding) 'macro
                                                (compile-in-lexenv (cdr binding) lexenv
                                                                   nil nil nil t nil)))
                                       bindings)))
                          (recurse body
                                   (make-lexenv :default lexenv
                                                :funs macros))))
                       (:symbol-macro
                        (funcall-in-symbol-macrolet-lexenv bindings
                                                           (lambda (&optional vars)
                                                             (declare (ignore vars))
                                                             (recurse body *lexenv*))
                                                           :compile))))))))
    (recurse inline-lexenv (make-null-lexenv))))

;;; Convert FUN as a lambda in the null environment, but use the
;;; current compilation policy. Note that FUN may be a
;;; LAMBDA-WITH-LEXENV, so we may have to augment the environment to
;;; reflect the state at the definition site.
(defun ir1-convert-inline-lambda (fun
                                  &key
                                  (source-name '.anonymous.)
                                  debug-name
                                  system-lambda
                                  (policy (lexenv-policy *lexenv*)))
  (when (and (not debug-name) (eq '.anonymous. source-name))
    (setf debug-name (name-lambdalike fun)))
  (let* ((lambda-with-lexenv-p (eq (car fun) 'lambda-with-lexenv))
         (body (if lambda-with-lexenv-p
                   `(lambda ,@(cddr fun))
                   fun))
         (lexenv-lambda (lexenv-lambda *lexenv*))
         (*lexenv*
           (if lambda-with-lexenv-p
               (make-lexenv
                :default (process-inline-lexenv (second fun))
                :handled-conditions (lexenv-handled-conditions *lexenv*)
                :policy policy
                :flushable (lexenv-flushable *lexenv*)
                :lambda lexenv-lambda
                :parent *lexenv*)
               (make-almost-null-lexenv
                policy
                ;; Inherit MUFFLE-CONDITIONS from the call-site lexenv
                ;; rather than the definition-site lexenv, since it seems
                ;; like a much more common case.
                (lexenv-handled-conditions *lexenv*)
                (lexenv-flushable *lexenv*)
                lexenv-lambda
                *lexenv*)))
         (*inlining* (1+ *inlining*))
         (clambda (ir1-convert-lambda body
                                      :source-name source-name
                                      :debug-name debug-name
                                      :system-lambda system-lambda)))
    (setf (functional-inline-expanded clambda) t)
    clambda))

(declaim (end-block))

;;; Get a DEFINED-FUN object for a function we are about to define. If
;;; the function has been forward referenced, then substitute for the
;;; previous references.
(defun get-defined-fun (name &optional (lambda-list nil lp))
  (proclaim-as-fun-name name)
  (let ((found (find-free-fun name "shouldn't happen! (defined-fun)"))
        (free-funs (free-funs *ir1-namespace*)))
    (note-name-defined name :function)
    (cond ((not (defined-fun-p found))
           ;; This assertion is wrong in block compilation mode, for
           ;; instance
           ;;
           ;; (defun foo (x) (bar x))
           ;; (declaim (inline bar))
           ;; (defun bar (x) x)
           (aver (or (block-compile *compilation*)
                     (not (info :function :inlinep name))))
           (let* ((where-from (leaf-where-from found))
                  (res (make-defined-fun
                        :%source-name name
                        :where-from (if (eq where-from :declared)
                                        :declared
                                        :defined-here)
                        :type (if (eq :declared where-from)
                                  (leaf-type found)
                                  (or (and lp
                                           (ignore-errors
                                            (ftype-from-lambda-list lambda-list)))
                                      (specifier-type 'function))))))
             (substitute-leaf res found)
             (setf (gethash name free-funs) res)))
          ;; If FREE-FUNS has a previously converted definition
          ;; for this name, then blow it away and try again.
          ((defined-fun-functionals found)
           (remhash name free-funs)
           (get-defined-fun name lambda-list))
          (t found))))

;;; Check a new global function definition for consistency with
;;; previous declaration or definition, and assert argument/result
;;; types if appropriate. This assertion is suppressed by the
;;; EXPLICIT-CHECK attribute, which is specified on functions that
;;; check their argument types as a consequence of type dispatching.
;;; This avoids redundant checks such as NUMBERP on the args to +, etc.
;;; FIXME: this seems to have nothing at all to do with adding "new"
;;; definitions, as it is only called from IR1-CONVERT-INLINE-EXPANSION.
(defun assert-new-definition (var fun)
  (let* ((type (leaf-type var))
         (for-real (eq (leaf-where-from var) :declared))
         (name (leaf-source-name var))
         (info (info :function :info name))
         (explicit-check (getf (functional-plist fun) 'explicit-check)))
    (assert-definition-type
     fun type
     ;; KLUDGE: Common Lisp is such a dynamic language that in general
     ;; all we can do here is issue a STYLE-WARNING. It would be nice
     ;; to issue a full WARNING in the special case of type mismatches
     ;; within a compilation unit (as in section 3.2.2.3 of the spec)
     ;; but at least as of sbcl-0.6.11, we don't keep track of whether
     ;; the mismatched data came from the same compilation unit, so we
     ;; can't do that. -- WHN 2001-02-11
     :lossage-fun #'compiler-style-warn
     :unwinnage-fun (cond (info #'compiler-style-warn)
                          (for-real #'compiler-notify)
                          (t nil))
     :really-assert (if for-real
                        (explicit-check->really-assert explicit-check))
     :where (if for-real
                "previous declaration"
                "previous definition"))))

;;; Used for global inline expansion. Earlier something like this was
;;; used by %DEFUN too. FIXME: And now it's probably worth rethinking
;;; whether this function is a good idea at all.
(defun ir1-convert-inline-expansion (name expansion var inlinep info)
  ;; Unless a INLINE function, we temporarily clobber the inline
  ;; expansion. This prevents recursive inline expansion of
  ;; opportunistic pseudo-inlines.
  (unless (eq inlinep 'inline)
    (setf (defined-fun-inline-expansion var) nil))
  (let ((fun (ir1-convert-inline-lambda expansion
                                        :source-name name
                                        ;; prevent instrumentation of
                                        ;; known function expansions
                                        :system-lambda (and info t))))
    (setf (functional-inlinep fun) inlinep)
    (assert-new-definition var fun)
    (setf (defined-fun-inline-expansion var) expansion)
    ;; Associate VAR with the FUN -- and in case of an optional dispatch
    ;; with the various entry-points. This allows XREF to know where the
    ;; inline CLAMBDA comes from.
    (flet ((note-inlining (f)
             (typecase f
               (functional
                (setf (functional-inline-expanded f) var))
               (cons
                ;; Delayed entry-point.
                (if (car f)
                    (setf (functional-inline-expanded (cdr f)) var)
                    (let ((old-thunk (cdr f)))
                      (setf (cdr f) (lambda ()
                                      (let ((g (funcall old-thunk)))
                                        (setf (functional-inline-expanded g) var)
                                        g)))))))))
      (note-inlining fun)
      (when (optional-dispatch-p fun)
        (note-inlining (optional-dispatch-main-entry fun))
        (note-inlining (optional-dispatch-more-entry fun))
        (mapc #'note-inlining (optional-dispatch-entry-points fun))))
    ;; substitute for any old references
    (unless (or (eq (defined-fun-inlinep var) :notinline)
                (not (block-compile *compilation*))
                (and info
                     (or (fun-info-transforms info)
                         (fun-info-templates info)
                         (fun-info-ir2-convert info))))
      (substitute-leaf fun var)
      ;; If in a simple environment, then we can allow backward references
      ;; to this function from following top-level forms.
      (when expansion
        (push fun (defined-fun-functionals var))))
    fun))


;;; Entry point utilities

;;; Return a function for the Nth entry point.
(defun optional-dispatch-entry-point-fun (dispatcher n)
  (declare (type optional-dispatch dispatcher)
           (type unsigned-byte n))
  (let ((*lexenv* (functional-lexenv dispatcher))
        (*current-path* (optional-dispatch-source-path dispatcher)))
    (force (nth n (optional-dispatch-entry-points dispatcher)))))

;;; Store INLINE-LAMBDA as the inline expansion of NAME.
;;; EXTRA-INFO is either a keyword denoting that NAME pertains to
;;; an auto-generated defstruct function, or else it is the list of
;;; funargs that could be auto-dxified.
(defun %set-inline-expansion (name defined-fun inline-lambda extra-info
                                   &aux (defstruct-snippet
                                          (when (keywordp extra-info)
                                            extra-info))
                                        (dxable-args
                                          (unless (keywordp extra-info)
                                            extra-info)))
  (cond (defstruct-snippet
         ;; In this case, NAME is a system-generated function. Warn if blowing away
         ;; a previously existing inline expansion coming from an ordinary DEFUN.
         ;; FIXME: It's tricky to correctly warn about stomping on a constructor
         ;; because it might actually be the right inline lambda.
         ;; Probably should compare with EQUALP.
         ;; FIXME: what does the below KLUDGE mean ?
         (unless (eq defstruct-snippet :constructor)
           (let ((old (info :function :inlining-data name)))
           ;; KLUDGE: This is like (NTH-VALUE 1 (FUN-NAME-INLINE-EXPANSION))
           ;; but expressed in a way that doesn't crash in cold-init.
             (when (or (typep old 'inlining-data) (consp old))
             ;; Any inline expansion that existed can't be useful.
               (warn "structure ~(~A~) ~S clobbers inline function"
                     defstruct-snippet name)))))
        (t
         (let ((info (info :function :source-transform name)))
           ;; If NAME was a defstruct snippet, and now it isn't, then warn
           ;; and remove the transform.
           (when (consp info)
             (clear-info :function :source-transform name)
             ;; This is serious enough that you can get two warnings:
             ;; - one because you redefined a function at all,
             ;; - and one because the source-transform is erased.
             (warn "redefinition of ~S clobbers structure ~:[accessor~;~(~a~)~]"
                   name (symbolp (cdr info)) (cdr info))))))
  ;; says CLHS: "Only an implementation that was willing to be responsible
  ;; for recompiling f if the definition of g changed incompatibly could
  ;; legitimately stack allocate the list argument to g in f."
  ;; Yeah, well, we're not going be responsible for bupkis.
  ;; If you want to do something dangerous, then do it.
  ;; Of course it would be nice NOT to warn when we haven't actually baked-in
  ;; any assumptions about callees, but I don't feel like adding more metadata
  ;; to track when we assumed something.
  (let ((old (fun-name-dx-args name)))
    (when (and old (not (subsetp old dxable-args)))
      (warn "redefinition of ~S with differing DYNAMIC-EXTENT declarations ~
is potentially harmful to any already-compiled callers using (SAFETY 0)."
            name)))
  (if (or inline-lambda dxable-args)
      (setf (info :function :inlining-data name)
            (if dxable-args
                (if inline-lambda
                    (make-inlining-data inline-lambda dxable-args)
                    (make-dxable-args dxable-args))
                inline-lambda))
      (clear-info :function :inlining-data name))
  (when (and inline-lambda defined-fun)
    (setf (defined-fun-inline-expansion defined-fun)
          inline-lambda)))

;;; the even-at-compile-time part of DEFUN
;;;
;;; INLINE-LAMBDA is either (LAMBDA (...) ...) or (LAMBDA-WITH-LEXENV ...)
;;; EXTRA-INFO is one of:
;;; * a symbol in {:ACCESSOR, :PREDICATE, :COPIER, :CONSTRUCTOR} if the function
;;;   came from defstruct; or
;;; * a possibly empty list of dynamic extent arguments.
;;; The inline lambda will be NIL for a structure accessor, predicate, or copier
;;; since those can always be reconstructed from a defstruct description.
(defun %compiler-defun (name compile-toplevel inline-lambda extra-info)
  (let ((defined-fun nil)) ; will be set below if we're in the compiler
    (when compile-toplevel
      (with-single-package-locked-error
          (:symbol name "defining ~S as a function")
        (setf defined-fun
              ;; Try to pass the lambda-list to GET-DEFINED-FUN if we can.
              (if (atom inline-lambda)
                  (get-defined-fun name)
                  (get-defined-fun
                   name (ecase (car inline-lambda)
                         (lambda-with-lexenv (third inline-lambda))
                         (lambda (second inline-lambda)))))))
      (when (boundp '*lexenv*)
        (aver (producing-fasl-file))
        (if (member name (fun-names-in-this-file *compilation*) :test #'equal)
            (warn 'duplicate-definition :name name)
            (push name (fun-names-in-this-file *compilation*))))
      ;; I don't know why this is guarded by (WHEN compile-toplevel),
      ;; because regular old %DEFUN is going to call this anyway.
      (%set-inline-expansion name defined-fun inline-lambda extra-info))

    (become-defined-fun-name name)

    ;;
    ;; If there is a type from a previous definition, blast it, since it is
    ;; obsolete.
    (when (and defined-fun (neq :declared (leaf-where-from defined-fun)))
      (setf (leaf-type defined-fun)
            ;; FIXME: If this is a block compilation thing, shouldn't
            ;; we be setting the type to the full derived type for the
            ;; definition, instead of this most general function type?
            (specifier-type 'function))))

  (values))
