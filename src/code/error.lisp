;;;; SBCL-specific parts of the condition system, i.e. parts which
;;;; don't duplicate/clobber functionality already provided by the
;;;; cross-compilation host Common Lisp

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;;; a utility for SIGNAL, ERROR, CERROR, WARN, COMPILER-NOTIFY and
;;; INVOKE-DEBUGGER: Parse the hairy argument conventions into a
;;; single argument that's directly usable by all the other routines.
(defun coerce-to-condition (datum default-type fun-name &rest arguments)
  (declare (explicit-check)
           (dynamic-extent arguments))
  (cond ((and (%instancep datum)
              (let ((wrapper (%instance-wrapper datum)))
                (and (logtest (wrapper-flags wrapper) +condition-layout-flag+)
                     ;; An invalid layout will drop into the (MAKE-CONDITION) branch
                     ;; which rightly fails because ALLOCATE-CONDITION asserts that
                     ;; the first argument is a condition-designator, which it won't be.
                     (not (wrapper-invalid wrapper)))))
         (when (and arguments (not (eq fun-name 'cerror)))
           (cerror "Ignore the additional arguments."
                   'simple-type-error
                   :datum (copy-list arguments)
                   :expected-type 'null
                   :format-control "You may not supply additional arguments ~
                                    when giving ~S to ~S."
                   :format-arguments (list datum fun-name)))
         datum)
        ((or (stringp datum) (functionp datum))
         (make-condition default-type
                         :format-control datum
                         :format-arguments (copy-list arguments)))
        (t
         (apply #'make-condition datum arguments))))

;;; This condition inherits from the hosts's classes when compiling
;;; the cross-compiler and the target's when cross-compiling.
(define-condition simple-program-error (simple-condition program-error) ())
(defun %program-error (&optional datum &rest arguments)
  (error (apply #'coerce-to-condition datum
                'simple-program-error '%program-error arguments)))


;;;; HANDLER-BIND

(sb-xc:defmacro %handler-bind (bindings form &environment env)
  (unless bindings
    (return-from %handler-bind form))
  ;; As an optimization, this looks at the handler parts of BINDINGS
  ;; and turns handlers of the forms (lambda ...) and (function
  ;; (lambda ...)) into local, dynamic-extent functions.
  ;;
  ;; Type specifiers in BINDINGS which name classoids are parsed
  ;; into the classoid, otherwise are translated local TYPEP wrappers.
  ;;
  ;; As a further optimization, it is possible to eliminate some runtime
  ;; consing (which is a speed win if not a space win, since it's dx already)
  ;; in special cases such as (HANDLER-BIND ((WARNING #'MUFFLE-WARNING)) ...).
  ;; If all bindings are optimizable, then the runtime cost of making them
  ;; is one dx cons cell for the whole cluster.
  ;; Otherwise it takes 1+2N cons cells where N is the number of bindings.
  ;;
  (collect ((local-functions) (cluster-entries) (dummy-forms) (complex-initforms))
    (flet ((const-cons (test handler)
             ;; If possible, render HANDLER as a load-time constant so that
             ;; consing the test and handler is also load-time constant.
             (let ((quote (car handler))
                   (name (cadr handler)))
               (cond ((and (eq quote 'function)
                           (or (assq name (local-functions))
                               (sb-c::fun-locally-defined-p name env)))
                      `(cons ,(case (car test)
                                ((named-lambda function) test)
                                (t `(load-time-value ,test t)))
                             (the (function-designator (condition)) ,handler)))
                     ((info :function :info name) ; known
                      ;; This takes care of CONTINUE,ABORT,MUFFLE-WARNING.
                      ;; #' will be evaluated in the null environment.
                      `(load-time-value
                        (cons ,test (the (function-designator (condition)) #',name))
                        t))
                     (t
                      ;; For each handler specified as #'F we must verify
                      ;; that F is fboundp upon entering the binding scope.
                      ;; Referencing #'F is enough to ensure a warning if the
                      ;; function isn't defined at compile-time, but the
                      ;; compiler considers it elidable unless something forces
                      ;; an apparent use of the form at runtime,
                      ;; so instead use SAFE-FDEFN-FUN on the fdefn.
                      (when (eq (car handler) 'function)
                        (dummy-forms `(sb-c:safe-fdefn-fun
                                       (load-time-value
                                        (find-or-create-fdefn ',name) t))))
                      ;; Resolve to an fdefn at load-time.
                      `(load-time-value
                        (cons ,test (find-or-create-fdefn
                                     (the (function-designator (condition)) ',name)))
                        t)))))

           (const-list (items)
             ;; If the resultant list is (LIST (L-T-V ...) (L-T-V ...) ...)
             ;; then pull the L-T-V outside.
             (if (every (lambda (x) (typep x '(cons (eql load-time-value))))
                        items)
                 `(load-time-value (list ,@(mapcar #'second items)) t)
                 `(list ,@items))))

      (with-current-source-form (bindings)
        (dolist (binding bindings)
          (with-current-source-form (binding)
            (unless (proper-list-of-length-p binding 2)
              (error "ill-formed handler binding: ~S" binding))
            (destructuring-bind (type handler) binding
              (setq type (typexpand type env))
              ;; Simplify a singleton AND or OR.
              (when (typep type '(cons (member and or) (cons t null)))
                (setf type (second type)))
              (cluster-entries
               (const-cons
                ;; Compute the test expression
                (cond ((member type '(t condition))
                       ;; Every signal is necesarily a CONDITION, so
                       ;; whether you wrote T or CONDITION, this is
                       ;; always an eligible handler.
                       '#'constantly-t)
                      ((typep type '(cons (eql satisfies) (cons t null)))
                       ;; (SATISFIES F) => #'F but never a local
                       ;; definition of F.  The predicate is used only
                       ;; if needed - it's not an error if not fboundp
                       ;; (though dangerously stupid) - so just
                       ;; reference #'F for the compiler to see the
                       ;; use of the name.  But (KLUDGE): since the
                       ;; ref is to force a compile-time effect, the
                       ;; interpreter should not see that form,
                       ;; because there is no way for it to perform an
                       ;; unsafe ref, (and it wouldn't signal a
                       ;; style-warning anyway), and so it would
                       ;; actually fail immediately if predicate were
                       ;; not defined.
                       (let ((name (second type)))
                         (when (typep env 'lexenv)
                           (dummy-forms `#',name))
                         `(find-or-create-fdefn ',name)))
                      ((and (symbolp type)
                            (condition-classoid-p (find-classoid type nil)))
                       ;; It's debatable whether we need to go through
                       ;; a classoid-cell instead of just using
                       ;; load-time-value on FIND-CLASS, but the extra
                       ;; indirection is safer, and no slower than
                       ;; what TYPEP does.
                       `(find-classoid-cell ',type :create t))
                      (t ; No runtime consing here- not a closure.
                       `(named-lambda (%handler-bind ,type) (c)
                          (declare (optimize (sb-c:verify-arg-count 0)))
                          (typep c ',type))))
                ;; Compute the handler expression.
                ;; Unless the expression is ({FUNCTION|QUOTE} <sym>), then create a
                ;; new local function. If the supplied handler is spelled
                ;; (LAMBDA ...) or #'(LAMBDA ...), then the local function is the
                ;; lambda but named.  If not spelled as such, the function funcalls
                ;; the user's sexpr through a variable binding so that the compiler
                ;; enforces callable-ness but evaluates the supplied form only once.
                (if (typep handler '(cons (member function quote) (cons symbol null)))
                    handler
                    (let* ((name (let ((*gensym-counter*
                                        (length (cluster-entries))))
                                   (sb-xc:gensym "H")))
                           (lexpr
                            (typecase handler
                              ;; These two are merely expansion prettifiers,
                              ;; and not strictly necessary.
                              ((cons (eql function) (cons (cons (eql lambda)) null))
                               (cadr handler))
                              ((cons (eql lambda))
                               handler)
                              (t
                               (complex-initforms `(,name ,handler))
                               ;; Should be (THE (FUNCTION-DESIGNATOR (CONDITION)))
                               ;; but the cast kills DX allocation.
                               `(lambda (c) (funcall ,name c))))))
                      (local-functions
                       `(,name ,(cadr lexpr)
                               ,@(when (typep (cadr lexpr) '(cons t null))
                                   '((declare (sb-c::local-optimize (sb-c::verify-arg-count 0)))))
                               ,@(cddr lexpr)))
                      `#',name))))))))

      `(let ,(complex-initforms)
         (dx-flet ,(local-functions)
           ,@(dummy-forms)
           (dx-let ((*handler-clusters*
                     (cons ,(const-list (cluster-entries))
                           *handler-clusters*)))
             ,form))))))

(sb-xc:defmacro handler-bind (bindings &body forms)
  "(HANDLER-BIND ( {(type handler)}* ) body)

Executes body in a dynamic context where the given handler bindings are in
effect. Each handler must take the condition being signalled as an argument.
The bindings are searched first to last in the event of a signalled
condition."
  ;; Bindings which meet specific criteria can be established with
  ;; slightly less runtime overhead than in general.
  ;; To allow the optimization, TYPE must be either be (SATISFIES P)
  ;; or a symbol naming a condition class at compile time,
  ;; and HANDLER must be a global function specified as either 'F or #'F.
  `(%handler-bind ,bindings
                  #-x86 (progn ,@forms)
                  ;; Need to catch FP errors here!
                  #+x86 (multiple-value-prog1 (progn ,@forms) (float-wait))))

;;;; HANDLER-CASE and IGNORE-ERRORS.
(sb-xc:defmacro handler-case (form &rest cases)
  "(HANDLER-CASE form { (type ([var]) body) }* )

Execute FORM in a context with handlers established for the condition types. A
peculiar property allows type to be :NO-ERROR. If such a clause occurs, and
form returns normally, all its values are passed to this clause as if by
MULTIPLE-VALUE-CALL. The :NO-ERROR clause accepts more than one var
specification."
  (let ((no-error-clause (assoc ':no-error cases)))
    (if no-error-clause
        (let ((normal-return (make-symbol "normal-return"))
              (error-return  (make-symbol "error-return")))
          `(block ,error-return
             (multiple-value-call (lambda ,@(cdr no-error-clause))
               (block ,normal-return
                 (return-from ,error-return
                   (handler-case (return-from ,normal-return ,form)
                     ,@(remove no-error-clause cases)))))))
        (let* ((local-funs nil)
               (annotated-cases
                 (mapcar (lambda (case)
                           (with-current-source-form (case)
                             (with-unique-names (block fun)
                               (destructuring-bind (type ll &body body) case
                                 (unless (and (listp ll)
                                              (symbolp (car ll))
                                              (null (cdr ll)))
                                   (error "Malformed HANDLER-CASE lambda-list. Should be either () or (symbol), not ~s."
                                          ll))
                                 (multiple-value-bind (body declarations)
                                     (parse-body body nil)
                                   (push `(,fun ,ll ,@declarations (progn ,@body)) local-funs))
                                 (list block type ll fun)))))
                         cases)))
          (with-unique-names (block form-fun)
            (let ((body `(%handler-bind
                          ,(mapcar (lambda (annotated-case)
                                     (destructuring-bind (block type ll fun-name) annotated-case
                                       (declare (ignore fun-name))
                                       (list type
                                             `(lambda (temp)
                                                ,@(unless ll
                                                    `((declare (ignore temp))))
                                                (return-from ,block
                                                  ,@(and ll '(temp)))))))
                                   annotated-cases)
                          (return-from ,block (,form-fun)))))
              (labels ((wrap (cases)
                         (if cases
                             (destructuring-bind (fun-block type ll fun-name) (car cases)
                               (declare (ignore type))
                               `(return-from ,block
                                  ,(if ll
                                       `(,fun-name (block ,fun-block
                                                     ,(wrap (cdr cases))))
                                       `(progn (block ,fun-block
                                                 ,(wrap (cdr cases)))
                                               (,fun-name)))))
                             body)))
                `(flet ((,form-fun ()
                          #-x86 (progn ,form) ;; no declarations are accepted
                          ;; Need to catch FP errors here!
                          #+x86 (multiple-value-prog1 ,form (float-wait)))
                        ,@(reverse local-funs))
                   (declare (optimize (sb-c::check-tag-existence 0))
                            (inline ,form-fun
                                    ,@(mapcar #'car local-funs)))
                   (block ,block
                     ,(wrap annotated-cases))))))))))

(sb-xc:defmacro ignore-errors (&rest forms)
  "Execute FORMS handling ERROR conditions, returning the result of the last
  form, or (VALUES NIL the-ERROR-that-was-caught) if an ERROR was handled."
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))
