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
              (let ((layout (%instance-layout datum)))
                (and (logtest (layout-flags layout) +condition-layout-flag+)
                     ;; An invalid layout will drop into the (MAKE-CONDITION) branch
                     ;; which rightly fails because ALLOCATE-CONDITION asserts that
                     ;; the first argument is a condition-designator, which it won't be.
                     (not (layout-invalid layout)))))
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
(define-error-wrapper %program-error (&optional datum &rest arguments)
  (error (apply #'coerce-to-condition datum
                'simple-program-error '%program-error arguments)))


;;;; HANDLER-BIND

(sb-xc:defmacro %handler-bind (bindings form &environment env)
  (unless bindings
    (return-from %handler-bind form))
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
  (collect ((cluster-entries) (dummy-forms))
    (flet ((const-cons (test handler)
             ;; If possible, render HANDLER as a load-time constant so that
             ;; consing the test and handler is also load-time constant.
             (if (and (listp handler)
                      (memq (car handler) '(quote function))
                      (not (sb-c::fun-locally-defined-p (cadr handler) env))
                      (legal-fun-name-p (cadr handler)))
                 ;; The CLHS writeup of HANDLER-BIND says "Exceptional Situations: None."
                 ;; which might suggest that it's not an error if #'HANDLER is un-fboundp
                 ;; on entering the body, but we should check in safe code.
                 (let ((name (cadr handler)))
                   (cond ((info :function :info name) ; known
                          ;; This takes care of CONTINUE,ABORT,MUFFLE-WARNING.
                          ;; #' will be evaluated in the null environment.
                          `(load-time-value (cons ,test (the (function (condition)) #',name))
                                            t))
                         (t
                          (when (eq (car handler) 'function)
                            ;; Referencing #'F is enough to get a compile-time warning about unknown
                            ;; functions, but the use itself is flushable, so employ SAFE-FDEFN-FUN.
                            (dummy-forms `#',name)
                            (when (sb-c:policy env (= safety 3))
                              (let ((f (if (or #+linkage-space (symbolp name))
                                           `',name
                                           `(load-time-value (find-or-create-fdefn ',name) t))))
                              (dummy-forms `(sb-c:safe-fdefn-fun ,f)))))
                          `(load-time-value
                            (cons ,test (the (function-designator (condition)) ',name))
                            t))))
                 `(cons ,(case (car test)
                           ((named-lambda function) test)
                           (t `(load-time-value ,test t)))
                        (the (function-designator (condition)) ,handler))))
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
                      ((typep type '(cons (eql satisfies) (cons symbol null)))
                       ;; (SATISFIES F) => #'F but never a local definition of F.
                       ;; The predicate is used only if needed - it's not an error if not
                       ;; fboundp (though dangerously stupid) - so reference #'F for the
                       ;; compiler to see the use of the name.  But (KLUDGE): since the
                       ;; ref is to force a compile-time effect, the interpreter should not
                       ;; see that form, because there is no way for it to perform an
                       ;; unsafe ref, and it wouldn't signal a style-warning anyway.
                       (let ((name (second type)))
                         ;; FIXME: if you've locally flet NAME (why would you do that?)
                         ;; then this does not notice the use of the global function.
                         (when (typep env 'lexenv) (dummy-forms `#',name))
                         `',name))
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
                ;; If the supplied handler is spelled (LAMBDA ...) or
                ;; #'(LAMBDA ...), then insert a declaration to elide
                ;; arg checking.
                ;;
                ;; KLUDGE: This should really be done in a cleaner way.
                (let ((lambda-expression
                        (typecase handler
                          ((cons (eql function) (cons (cons (eql lambda)) null))
                           (cadr handler))
                          ((cons (eql lambda))
                           handler))))
                  (if lambda-expression
                      `(lambda ,(cadr lambda-expression)
                         (declare (sb-c::source-form ,binding))
                         ,@(when (typep (cadr lambda-expression) '(cons t null))
                             '((declare (sb-c::local-optimize (sb-c::verify-arg-count 0)))))
                         ,@(cddr lambda-expression))
                      handler))))))))
      `(let ((*handler-clusters*
               (cons ,(const-list (cluster-entries))
                     *handler-clusters*)))
         (declare (dynamic-extent *handler-clusters*))
         ,@(dummy-forms)
         ,form))))

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
             (sb-c::with-source-form ,no-error-clause
               (multiple-value-call (named-lambda (handler-case :no-error) ,@(cdr no-error-clause))
                 (block ,normal-return
                   (return-from ,error-return
                     (handler-case (return-from ,normal-return ,form)
                       ,@(remove no-error-clause cases))))))))
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
                                   (push `(,fun ,ll
                                                (declare (sb-c::source-form ,case))
                                                ,@declarations
                                                (progn ,@body))
                                         local-funs))
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
                `(dx-flet ((,form-fun ()
                          #-x86 (progn ,form) ;; no declarations are accepted
                          ;; Need to catch FP errors here!
                          #+x86 (multiple-value-prog1 ,form (float-wait)))
                        ,@(reverse local-funs))
                   (declare (inline ,form-fun
                                    ,@(mapcar #'car local-funs)))
                   (block ,block
                     ,(wrap annotated-cases))))))))))

(sb-xc:defmacro ignore-errors (&rest forms)
  "Execute FORMS handling ERROR conditions, returning the result of the last
  form, or (VALUES NIL the-ERROR-that-was-caught) if an ERROR was handled."
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))

;;; Condition slot access - needs DYNAMIC-SPACE-OBJ-P which needs misc-aliens
;;; which isn't available in target-error.
#-sb-xc-host
(labels
    ((atomic-acons (condition key val alist)
       ;; Force new conses to the heap if instance is arena-allocated
       (cas (condition-assigned-slots condition)
            alist
            (if (dynamic-space-obj-p condition)
                (locally (declare (sb-c::tlab :system)) (acons key val alist))
                (acons key val alist))))
     (initval (instance slot classoid operation)
       (let ((instance-length (%instance-length instance)))
         (do ((i (+ sb-vm:instance-data-start 1) (+ i 2)))
             ((>= i instance-length)
              (find-slot-default instance classoid slot
                                 (eq operation 'slot-boundp)))
           (when (memq (%instance-ref instance i) (condition-slot-initargs slot))
             (return (%instance-ref instance (1+ i)))))))
     (%get (condition name operation)
       ;; Shared code for CONDITION-SLOT-VALUE and CONDITION-SLOT-BOUNDP.
       ;; First look for a slot with :CLASS allocation
       (let ((classoid (layout-classoid (%instance-layout condition))))
         (dolist (cslot (condition-classoid-class-slots classoid))
           (when (eq (condition-slot-name cslot) name)
             (return-from %get (car (condition-slot-cell cslot)))))
         (let* ((alist (condition-assigned-slots condition))
                (cell (assq name alist)))
           (when cell (return-from %get (cdr cell)))
           ;; find the slot definition or else signal an error
           (let* ((slot (or (find-condition-class-slot classoid name)
                            (return-from %get
                              (values (slot-missing (classoid-pcl-class classoid)
                                                    condition name 'slot-value)))))
                  (val (initval condition slot classoid operation)))
             (loop
               (let ((old (atomic-acons condition name val alist)))
                 (when (eq old alist) (return val))
                 (setq alist old cell (assq name alist))
                 (when cell (return (cdr cell))))))))))

  ;; This is a stupid argument order. Shouldn't NEW-VALUE be first ?
  (defun set-condition-slot-value (condition new-value name)
    (dolist (cslot (condition-classoid-class-slots
                    (layout-classoid (%instance-layout condition))))
      (when (eq (condition-slot-name cslot) name)
        (return-from set-condition-slot-value
          (setf (car (condition-slot-cell cslot)) new-value))))
    ;; Apparently this does not care that there might not exist a slot named NAME
    ;; in the class, at least in this function. It seems to be handled
    ;; at a higher level of the slot access protocol.
    (let ((alist (condition-assigned-slots condition)))
      (loop
       (let ((cell (assq name alist)))
         (when cell
           (return (setf (cdr cell) new-value))))
        (let ((old (atomic-acons condition name new-value alist)))
          (if (eq old alist) (return new-value) (setq alist old))))))

  (defun condition-slot-value (condition name)
    (let ((value (%get condition name 'slot-value)))
      (if (unbound-marker-p value)
          (let ((class (classoid-pcl-class (layout-classoid (%instance-layout condition)))))
            (values (slot-unbound class condition name)))
          value)))

  (defun condition-slot-boundp (condition name)
    (not (unbound-marker-p (%get condition name 'slot-boundp))))

  (defun condition-slot-makunbound (condition name)
    (set-condition-slot-value condition sb-pcl:+slot-unbound+ name)))
