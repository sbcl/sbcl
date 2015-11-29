;;;; This file contains code which does the translation from Lisp code
;;;; to the first intermediate representation (IR1).

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(declaim (special *compiler-error-bailout*))

;;; *CURRENT-FORM-NUMBER* is used in FIND-SOURCE-PATHS to compute the
;;; form number to associate with a source path. This should be bound
;;; to an initial value of 0 before the processing of each truly
;;; top level form.
(declaim (type index *current-form-number*))
(defvar *current-form-number*)

;;; *SOURCE-PATHS* is a hashtable from source code forms to the path
;;; taken through the source to reach the form. This provides a way to
;;; keep track of the location of original source forms, even when
;;; macroexpansions and other arbitary permutations of the code
;;; happen. This table is initialized by calling FIND-SOURCE-PATHS on
;;; the original source.
;;;
;;; It is fairly useless to store symbols, characters, or fixnums in
;;; this table, as 42 is EQ to 42 no matter where in the source it
;;; appears. GET-SOURCE-PATH and NOTE-SOURCE-PATH functions should be
;;; always used to access this table.
(declaim (hash-table *source-paths*))
(defvar *source-paths*)

(declaim (inline source-form-has-path-p))
(defun source-form-has-path-p (form)
  (not (typep form '(or symbol fixnum character))))

(defun get-source-path (form)
  (when (source-form-has-path-p form)
    (gethash form *source-paths*)))

(defun ensure-source-path (form)
  (or (get-source-path form)
      (cons (simplify-source-path-form form)
            *current-path*)))

(defun simplify-source-path-form (form)
  (if (consp form)
      (let ((op (car form)))
        ;; In the compiler functions can be directly represented
        ;; by leaves. Having leaves in the source path is pretty
        ;; hard on the poor user, however, so replace with the
        ;; source-name when possible.
        (if (and (leaf-p op) (leaf-has-source-name-p op))
            (cons (leaf-source-name op) (cdr form))
            form))
      form))

(defun note-source-path (form &rest arguments)
  (when (source-form-has-path-p form)
    (setf (gethash form *source-paths*)
          (apply #'list* 'original-source-start *current-form-number* arguments))))

;;; *CURRENT-COMPONENT* is the COMPONENT structure which we link
;;; blocks into as we generate them. This just serves to glue the
;;; emitted blocks together until local call analysis and flow graph
;;; canonicalization figure out what is really going on. We need to
;;; keep track of all the blocks generated so that we can delete them
;;; if they turn out to be unreachable.
;;;
;;; FIXME: It's confusing having one variable named *CURRENT-COMPONENT*
;;; and another named *COMPONENT-BEING-COMPILED*. (In CMU CL they
;;; were called *CURRENT-COMPONENT* and *COMPILE-COMPONENT* respectively,
;;; which was also confusing.)
(declaim (type (or component null) *current-component*))
(defvar *current-component*)

;;; *CURRENT-PATH* is the source path of the form we are currently
;;; translating. See NODE-SOURCE-PATH in the NODE structure.
(declaim (list *current-path*))
(defvar *current-path*)

(defvar *derive-function-types* nil
  #!+sb-doc
  "Should the compiler assume that function types will never change,
  so that it can use type information inferred from current definitions
  to optimize code which uses those definitions? Setting this true
  gives non-ANSI, early-CMU-CL behavior. It can be useful for improving
  the efficiency of stable code.")

(defvar *fun-names-in-this-file* nil)

(defvar *post-binding-variable-lexenv* nil)

;;;; namespace management utilities

;; As with LEXENV-FIND, we assume use of *LEXENV*, but macroexpanders
;; receive an explicit environment and should pass it.
;; A declaration will trump a proclamation.
(defun fun-lexically-notinline-p (name &optional (env *lexenv*))
  (let ((answer
         (typecase env
           (null nil)
           #!+(and sb-fasteval (host-feature sb-xc))
           (sb!interpreter:basic-env
            (sb!interpreter::fun-lexically-notinline-p name env))
           (t
            (let ((fun (cdr (assoc name (lexenv-funs env) :test #'equal))))
              ;; FIXME: this seems to omit FUNCTIONAL
              (when (defined-fun-p fun)
                (return-from fun-lexically-notinline-p
                  (eq (defined-fun-inlinep fun) :notinline))))))))
    ;; If ANSWER is NIL, go for the global value
    (eq (or answer (info :function :inlinep name))
        :notinline)))

(defun maybe-defined-here (name where)
  (if (and (eq :defined where)
           (member name *fun-names-in-this-file* :test #'equal))
      :defined-here
      where))

;;; Return a GLOBAL-VAR structure usable for referencing the global
;;; function NAME.
(defun find-global-fun (name latep)
  (unless (info :function :kind name)
    (setf (info :function :kind name) :function)
    (setf (info :function :where-from name) :assumed))
  (let ((where (info :function :where-from name)))
    (when (and (eq where :assumed)
               ;; In the ordinary target Lisp, it's silly to report
               ;; undefinedness when the function is defined in the
               ;; running Lisp. But at cross-compile time, the current
               ;; definedness of a function is irrelevant to the
               ;; definedness at runtime, which is what matters.
               #-sb-xc-host (not (fboundp name))
               ;; LATEP is true when the user has indicated that
               ;; late-late binding is desired by using eg. a quoted
               ;; symbol -- in which case it makes little sense to
               ;; complain about undefined functions.
               (not latep))
      (note-undefined-reference name :function))
    (let ((ftype (proclaimed-ftype name))
          (notinline (fun-lexically-notinline-p name)))
      (make-global-var
       :kind :global-function
       :%source-name name
       :type (if (or (eq where :declared)
                     (and (not latep)
                          (not notinline)
                          *derive-function-types*))
                 ftype
                 (specifier-type 'function))
       :defined-type (if (and (not latep) (not notinline))
                         ftype
                         (specifier-type 'function))
       :where-from (if notinline
                       where
                       (maybe-defined-here name where))))))

;;; Have some DEFINED-FUN-FUNCTIONALS of a *FREE-FUNS* entry become invalid?
;;; Drop 'em.
;;;
;;; This was added to fix bug 138 in SBCL. It is possible for a *FREE-FUNS*
;;; entry to contain a DEFINED-FUN whose DEFINED-FUN-FUNCTIONAL object
;;; contained IR1 stuff (NODEs, BLOCKs...) referring to an already compiled
;;; (aka "dead") component. When this IR1 stuff was reused in a new component,
;;; under further obscure circumstances it could be used by
;;; WITH-IR1-ENVIRONMENT-FROM-NODE to generate a binding for
;;; *CURRENT-COMPONENT*. At that point things got all confused, since IR1
;;; conversion was sending code to a component which had already been compiled
;;; and would never be compiled again.
;;;
;;; Note: as of 1.0.24.41 this seems to happen only in XC, and the original
;;; BUGS entry also makes it seem like this might not be an issue at all on
;;; target.
(defun clear-invalid-functionals (free-fun)
  ;; There might be other reasons that *FREE-FUN* entries could
  ;; become invalid, but the only one we've been bitten by so far
  ;; (sbcl-0.pre7.118) is this one:
  (when (defined-fun-p free-fun)
    (setf (defined-fun-functionals free-fun)
          (delete-if (lambda (functional)
                       (or (eq (functional-kind functional) :deleted)
                           (when (lambda-p functional)
                             (or
                              ;; (The main reason for this first test is to bail
                              ;; out early in cases where the LAMBDA-COMPONENT
                              ;; call in the second test would fail because links
                              ;; it needs are uninitialized or invalid.)
                              ;;
                              ;; If the BIND node for this LAMBDA is null, then
                              ;; according to the slot comments, the LAMBDA has
                              ;; been deleted or its call has been deleted. In
                              ;; that case, it seems rather questionable to reuse
                              ;; it, and certainly it shouldn't be necessary to
                              ;; reuse it, so we cheerfully declare it invalid.
                              (not (lambda-bind functional))
                              ;; If this IR1 stuff belongs to a dead component,
                              ;; then we can't reuse it without getting into
                              ;; bizarre confusion.
                              (eq (component-info (lambda-component functional))
                                  :dead)))))
                     (defined-fun-functionals free-fun)))
    nil))

;;; If NAME already has a valid entry in *FREE-FUNS*, then return
;;; the value. Otherwise, make a new GLOBAL-VAR using information from
;;; the global environment and enter it in *FREE-FUNS*. If NAME
;;; names a macro or special form, then we error out using the
;;; supplied context which indicates what we were trying to do that
;;; demanded a function.
(declaim (ftype (sfunction (t string) global-var) find-free-fun))
(defun find-free-fun (name context)
  (or (let ((old-free-fun (gethash name *free-funs*)))
        (when old-free-fun
          (clear-invalid-functionals old-free-fun)
          old-free-fun))
      (ecase (info :function :kind name)
        ;; FIXME: The :MACRO and :SPECIAL-FORM cases could be merged.
        (:macro
         (compiler-error "The macro name ~S was found ~A." name context))
        (:special-form
         (compiler-error "The special form name ~S was found ~A."
                         name
                         context))
        ((:function nil)
         (check-fun-name name)
         (let ((expansion (fun-name-inline-expansion name))
               (inlinep (info :function :inlinep name)))
           (setf (gethash name *free-funs*)
                 (if (or expansion inlinep)
                     (let ((where (info :function :where-from name)))
                       (make-defined-fun
                        :%source-name name
                        :inline-expansion expansion
                        :inlinep inlinep
                        :where-from (if (eq inlinep :notinline)
                                        where
                                        (maybe-defined-here name where))
                        :type (if (and (eq inlinep :notinline)
                                       (neq where :declared))
                                  (specifier-type 'function)
                                  (proclaimed-ftype name))))
                     (find-global-fun name nil))))))))

;;; Return the LEAF structure for the lexically apparent function
;;; definition of NAME.
(declaim (ftype (sfunction (t string) leaf) find-lexically-apparent-fun))
(defun find-lexically-apparent-fun (name context)
  (let ((var (lexenv-find name funs :test #'equal)))
    (cond (var
           (unless (leaf-p var)
             (aver (and (consp var) (eq (car var) 'macro)))
             (compiler-error "found macro name ~S ~A" name context))
           var)
          (t
           (find-free-fun name context)))))

(defun maybe-find-free-var (name)
  (gethash name *free-vars*))

;;; Return the LEAF node for a global variable reference to NAME. If
;;; NAME is already entered in *FREE-VARS*, then we just return the
;;; corresponding value. Otherwise, we make a new leaf using
;;; information from the global environment and enter it in
;;; *FREE-VARS*. If the variable is unknown, then we emit a warning.
(declaim (ftype (sfunction (t) (or leaf cons heap-alien-info)) find-free-var))
(defun find-free-var (name)
  (unless (symbolp name)
    (compiler-error "Variable name is not a symbol: ~S." name))
  (or (gethash name *free-vars*)
      (let ((kind (info :variable :kind name))
            (type (info :variable :type name))
            (where-from (info :variable :where-from name))
            (deprecation-state (deprecated-thing-p 'variable name)))
        (when (and (eq kind :unknown) (not deprecation-state))
          (note-undefined-reference name :variable))
        (setf (gethash name *free-vars*)
              (case kind
                (:alien
                 (info :variable :alien-info name))
                ;; FIXME: The return value in this case should really be
                ;; of type SB!C::LEAF.  I don't feel too badly about it,
                ;; because the MACRO idiom is scattered throughout this
                ;; file, but it should be cleaned up so we're not
                ;; throwing random conses around.  --njf 2002-03-23
                (:macro
                 (let ((expansion (info :variable :macro-expansion name))
                       (type (type-specifier (info :variable :type name))))
                   `(macro . (the ,type ,expansion))))
                (:constant
                 (let ((value (symbol-value name)))
                   ;; Override the values of standard symbols in XC,
                   ;; since we can't redefine them.
                   #+sb-xc-host
                   (when (eql (find-symbol (symbol-name name) :cl) name)
                     (multiple-value-bind (xc-value foundp)
                         (xc-constant-value name)
                       (cond (foundp
                              (setf value xc-value))
                             ((not (eq value name))
                              (compiler-warn
                               "Using cross-compilation host's definition of ~S: ~A~%"
                               name (symbol-value name))))))
                   (find-constant value name)))
                (t
                 (make-global-var :kind kind
                                  :%source-name name
                                  :type type
                                  :where-from where-from)))))))

;;; Grovel over CONSTANT checking for any sub-parts that need to be
;;; processed with MAKE-LOAD-FORM. We have to be careful, because
;;; CONSTANT might be circular. We also check that the constant (and
;;; any subparts) are dumpable at all.
(defun maybe-emit-make-load-forms (constant &optional (name nil namep))
  (let ((xset (alloc-xset)))
    (labels ((trivialp (value)
               (typep value
                      '(or
                        #-sb-xc-host
                        (or unboxed-array #!+sb-simd-pack simd-pack)
                        #+sb-xc-host
                        (and array (not (array t)))
                        symbol
                        number
                        character
                        string))) ; subsumed by UNBOXED-ARRAY
             (grovel (value)
               ;; Unless VALUE is an object which which obviously
               ;; can't contain other objects
               (unless (trivialp value)
                 (if (xset-member-p value xset)
                     (return-from grovel nil)
                     (add-to-xset value xset))
                 (typecase value
                   (cons
                    (grovel (car value))
                    (grovel (cdr value)))
                   (simple-vector
                    (dotimes (i (length value))
                      (grovel (svref value i))))
                   ((vector t)
                    (dotimes (i (length value))
                      (grovel (aref value i))))
                   ((simple-array t)
                    ;; Even though the (ARRAY T) branch does the exact
                    ;; same thing as this branch we do this separately
                    ;; so that the compiler can use faster versions of
                    ;; array-total-size and row-major-aref.
                    (dotimes (i (array-total-size value))
                      (grovel (row-major-aref value i))))
                   ((array t)
                    (dotimes (i (array-total-size value))
                      (grovel (row-major-aref value i))))
                   (#+sb-xc-host structure!object
                    #-sb-xc-host instance
                    ;; In the target SBCL, we can dump any instance, but
                    ;; in the cross-compilation host, %INSTANCE-FOO
                    ;; functions don't work on general instances, only on
                    ;; STRUCTURE!OBJECTs.
                    ;;
                    ;; Behold the wonderfully clear sense of this-
                    ;;  WHEN (EMIT-MAKE-LOAD-FORM VALUE)
                    ;; meaning "when you're _NOT_ using a custom load-form"
                    ;;
                    ;; FIXME: What about funcallable instances with
                    ;; user-defined MAKE-LOAD-FORM methods?
                    (when (emit-make-load-form value)
                      #+sb-xc-host
                      (aver (zerop (layout-raw-slot-metadata
                                    (%instance-layout value))))
                      (do-instance-tagged-slot (i value)
                        (grovel (%instance-ref value i)))))
                   ;; The cross-compiler can dump certain instances that are not
                   ;; subtypes of STRUCTURE!OBJECT, as long as it has processed
                   ;; the defstruct.
                   #+sb-xc-host
                   ((satisfies sb!kernel::xc-dumpable-structure-instance-p)
                    (do-instance-tagged-slot (i value)
                      (grovel (%instance-ref value i))))
                   (t
                    (compiler-error
                     "Objects of type ~S can't be dumped into fasl files."
                     (type-of value)))))))
      ;; Dump all non-trivial named constants using the name.
      (if (and namep (not (typep constant '(or symbol character
                                            ;; FIXME: Cold init breaks if we
                                            ;; try to reference FP constants
                                            ;; thru their names.
                                            #+sb-xc-host number
                                            #-sb-xc-host fixnum))))
          (emit-make-load-form constant name)
          (grovel constant))))
  (values))

;;;; some flow-graph hacking utilities

;;; This function sets up the back link between the node and the
;;; ctran which continues at it.
(defun link-node-to-previous-ctran (node ctran)
  (declare (type node node) (type ctran ctran))
  (aver (not (ctran-next ctran)))
  (setf (ctran-next ctran) node)
  (setf (node-prev node) ctran))

;;; This function is used to set the ctran for a node, and thus
;;; determine what is evaluated next. If the ctran has no block, then
;;; we make it be in the block that the node is in. If the ctran heads
;;; its block, we end our block and link it to that block.
#!-sb-fluid (declaim (inline use-ctran))
(defun use-ctran (node ctran)
  (declare (type node node) (type ctran ctran))
  (if (eq (ctran-kind ctran) :unused)
      (let ((node-block (ctran-block (node-prev node))))
        (setf (ctran-block ctran) node-block)
        (setf (ctran-kind ctran) :inside-block)
        (setf (ctran-use ctran) node)
        (setf (node-next node) ctran))
      (%use-ctran node ctran)))
(defun %use-ctran (node ctran)
  (declare (type node node) (type ctran ctran) (inline member))
  (let ((block (ctran-block ctran))
        (node-block (ctran-block (node-prev node))))
    (aver (eq (ctran-kind ctran) :block-start))
    (when (block-last node-block)
      (error "~S has already ended." node-block))
    (setf (block-last node-block) node)
    (when (block-succ node-block)
      (error "~S already has successors." node-block))
    (setf (block-succ node-block) (list block))
    (when (memq node-block (block-pred block))
      (error "~S is already a predecessor of ~S." node-block block))
    (push node-block (block-pred block))))

;;; Insert NEW before OLD in the flow-graph.
(defun insert-node-before (old new)
  (let ((prev (node-prev old))
        (temp (make-ctran)))
    (ensure-block-start prev)
    (setf (ctran-next prev) nil)
    (link-node-to-previous-ctran new prev)
    (use-ctran new temp)
    (link-node-to-previous-ctran old temp))
  (values))

;;; This function is used to set the ctran for a node, and thus
;;; determine what receives the value.
(defun use-lvar (node lvar)
  (declare (type valued-node node) (type (or lvar null) lvar))
  (aver (not (node-lvar node)))
  (when lvar
    (setf (node-lvar node) lvar)
    (cond ((null (lvar-uses lvar))
           (setf (lvar-uses lvar) node))
          ((listp (lvar-uses lvar))
           (aver (not (memq node (lvar-uses lvar))))
           (push node (lvar-uses lvar)))
          (t
           (aver (neq node (lvar-uses lvar)))
           (setf (lvar-uses lvar) (list node (lvar-uses lvar)))))
    (reoptimize-lvar lvar)))

#!-sb-fluid(declaim (inline use-continuation))
(defun use-continuation (node ctran lvar)
  (use-ctran node ctran)
  (use-lvar node lvar))

;;;; exported functions

;;; This function takes a form and the top level form number for that
;;; form, and returns a lambda representing the translation of that
;;; form in the current global environment. The returned lambda is a
;;; top level lambda that can be called to cause evaluation of the
;;; forms. This lambda is in the initial component. If FOR-VALUE is T,
;;; then the value of the form is returned from the function,
;;; otherwise NIL is returned.
;;;
;;; This function may have arbitrary effects on the global environment
;;; due to processing of EVAL-WHENs. All syntax error checking is
;;; done, with erroneous forms being replaced by a proxy which signals
;;; an error if it is evaluated. Warnings about possibly inconsistent
;;; or illegal changes to the global environment will also be given.
;;;
;;; We make the initial component and convert the form in a PROGN (and
;;; an optional NIL tacked on the end.) We then return the lambda. We
;;; bind all of our state variables here, rather than relying on the
;;; global value (if any) so that IR1 conversion will be reentrant.
;;; This is necessary for EVAL-WHEN processing, etc.
;;;
;;; The hashtables used to hold global namespace info must be
;;; reallocated elsewhere. Note also that *LEXENV* is not bound, so
;;; that local macro definitions can be introduced by enclosing code.
(defun ir1-toplevel (form path for-value &optional (allow-instrumenting t))
  (declare (list path))
  (let* ((*current-path* path)
         (component (make-empty-component))
         (*current-component* component)
         (*allow-instrumenting* allow-instrumenting))
    (setf (component-name component) 'initial-component)
    (setf (component-kind component) :initial)
    (let* ((forms (if for-value `(,form) `(,form nil)))
           (res (ir1-convert-lambda-body
                 forms ()
                 :debug-name (debug-name 'top-level-form #+sb-xc-host nil #-sb-xc-host form))))
      (setf (functional-entry-fun res) res
            (functional-arg-documentation res) ()
            (functional-kind res) :toplevel)
      res)))

;;; This function is called on freshly read forms to record the
;;; initial location of each form (and subform.) Form is the form to
;;; find the paths in, and TLF-NUM is the top level form number of the
;;; truly top level form.
;;;
;;; This gets a bit interesting when the source code is circular. This
;;; can (reasonably?) happen in the case of circular list constants.
(defun find-source-paths (form tlf-num)
  (declare (type index tlf-num))
  (let ((*current-form-number* 0))
    (sub-find-source-paths form (list tlf-num)))
  (values))
(defun sub-find-source-paths (form path)
  (unless (get-source-path form)
    (note-source-path form path)
    (incf *current-form-number*)
    (let ((pos 0)
          (subform form)
          (trail form))
      (declare (fixnum pos))
      (macrolet ((frob ()
                   `(progn
                      (when (atom subform) (return))
                      (let ((fm (car subform)))
                        (when (sb!int:comma-p fm)
                          (setf fm (sb!int:comma-expr fm)))
                        (cond ((consp fm)
                               ;; If it's a cons, recurse.
                               (sub-find-source-paths fm (cons pos path)))
                              ((eq 'quote fm)
                               ;; Don't look into quoted constants.
                               ;; KLUDGE: this can't actually know about constants.
                               ;; e.g. (let ((quote (error "foo")))) or
                               ;; (list quote (error "foo")) are not
                               ;; constants and yet are ignored.
                               (return))
                              ((not (zerop pos))
                               ;; Otherwise store the containing form. It's not
                               ;; perfect, but better than nothing.
                               (note-source-path subform pos path)))
                        (incf pos))
                      (setq subform (cdr subform))
                      (when (eq subform trail) (return)))))
        (loop
         (frob)
         (frob)
         (setq trail (cdr trail)))))))

;;;; IR1-CONVERT, macroexpansion and special form dispatching

(declaim (ftype (sfunction (ctran ctran (or lvar null) t)
                           (values))
                ir1-convert))
(macrolet (;; Bind *COMPILER-ERROR-BAILOUT* to a function that throws
           ;; out of the body and converts a condition signalling form
           ;; instead. The source form is converted to a string since it
           ;; may contain arbitrary non-externalizable objects.
           (ir1-error-bailout ((start next result form) &body body)
             (with-unique-names (skip condition)
               `(block ,skip
                  (let ((,condition (catch 'ir1-error-abort
                                      (let ((*compiler-error-bailout*
                                              (lambda (&optional e)
                                                (throw 'ir1-error-abort e))))
                                        ,@body
                                        (return-from ,skip nil)))))
                    (ir1-convert ,start ,next ,result
                                 (make-compiler-error-form ,condition
                                                           ,form)))))))

  ;; Translate FORM into IR1. The code is inserted as the NEXT of the
  ;; CTRAN START. RESULT is the LVAR which receives the value of the
  ;; FORM to be translated. The translators call this function
  ;; recursively to translate their subnodes.
  ;;
  ;; As a special hack to make life easier in the compiler, a LEAF
  ;; IR1-converts into a reference to that LEAF structure. This allows
  ;; the creation using backquote of forms that contain leaf
  ;; references, without having to introduce dummy names into the
  ;; namespace.
  (defun ir1-convert (start next result form)
    (let* ((*current-path* (ensure-source-path form))
           (start (instrument-coverage start nil form)))
      (ir1-error-bailout (start next result form)
        (cond ((atom form)
               (cond ((and (symbolp form) (not (keywordp form)))
                      (ir1-convert-var start next result form))
                     ((leaf-p form)
                      (reference-leaf start next result form))
                     (t
                      (reference-constant start next result form))))
              (t
               (ir1-convert-functoid start next result form)))))
    (values))

  ;; Generate a reference to a manifest constant, creating a new leaf
  ;; if necessary.
  (defun reference-constant (start next result value)
    (declare (type ctran start next)
             (type (or lvar null) result))
    (ir1-error-bailout (start next result value)
      (let* ((leaf (find-constant value))
             (res (make-ref leaf)))
        (push res (leaf-refs leaf))
        (link-node-to-previous-ctran res start)
        (use-continuation res next result)))
    (values)))

;;; Add FUNCTIONAL to the COMPONENT-REANALYZE-FUNCTIONALS, unless it's
;;; some trivial type for which reanalysis is a trivial no-op, or
;;; unless it doesn't belong in this component at all.
;;;
;;; FUNCTIONAL is returned.
(defun maybe-reanalyze-functional (functional)

  (aver (not (eql (functional-kind functional) :deleted))) ; bug 148
  (aver-live-component *current-component*)

  ;; When FUNCTIONAL is of a type for which reanalysis isn't a trivial
  ;; no-op
  (when (typep functional '(or optional-dispatch clambda))

    ;; When FUNCTIONAL knows its component
    (when (lambda-p functional)
      (aver (eql (lambda-component functional) *current-component*)))

    (pushnew functional
             (component-reanalyze-functionals *current-component*)))

  functional)

;;; Generate a REF node for LEAF, frobbing the LEAF structure as
;;; needed. If LEAF represents a defined function which has already
;;; been converted, and is not :NOTINLINE, then reference the
;;; functional instead.
(defun reference-leaf (start next result leaf &optional (name '.anonymous.))
  (declare (type ctran start next) (type (or lvar null) result) (type leaf leaf))
  (assure-leaf-live-p leaf)
  (let* ((type (lexenv-find leaf type-restrictions))
         (leaf (or (and (defined-fun-p leaf)
                        (not (eq (defined-fun-inlinep leaf)
                                 :notinline))
                        (let ((functional (defined-fun-functional leaf)))
                          (when (and functional (not (functional-kind functional)))
                            (maybe-reanalyze-functional functional))))
                   (when (and (lambda-p leaf)
                              (memq (functional-kind leaf)
                                    '(nil :optional)))
                     (maybe-reanalyze-functional leaf))
                   leaf))
         (ref (make-ref leaf name)))
    (push ref (leaf-refs leaf))
    (setf (leaf-ever-used leaf) t)
    (link-node-to-previous-ctran ref start)
    (cond (type (let* ((ref-ctran (make-ctran))
                       (ref-lvar (make-lvar))
                       (cast (make-cast ref-lvar
                                        (make-single-value-type type)
                                        (lexenv-policy *lexenv*))))
                  (setf (lvar-dest ref-lvar) cast)
                  (use-continuation ref ref-ctran ref-lvar)
                  (link-node-to-previous-ctran cast ref-ctran)
                  (use-continuation cast next result)))
          (t (use-continuation ref next result)))))

(defun always-boundp (name)
  (case (info :variable :always-bound name)
    (:always-bound t)
    ;; Compiling to fasl considers a symbol always-bound if its
    ;; :always-bound info value is now T or will eventually be T.
    (:eventually (fasl-output-p *compile-object*))))

;;; Convert a reference to a symbolic constant or variable. If the
;;; symbol is entered in the LEXENV-VARS we use that definition,
;;; otherwise we find the current global definition. This is also
;;; where we pick off symbol macro and alien variable references.
(defun ir1-convert-var (start next result name)
  (declare (type ctran start next) (type (or lvar null) result) (symbol name))
  (let ((var (or (lexenv-find name vars) (find-free-var name))))
    (if (and (global-var-p var) (not (always-boundp name)))
        ;; KLUDGE: If the variable may be unbound, convert using SYMBOL-VALUE
        ;; which is not flushable, so that unbound dead variables signal an
        ;; error (bug 412, lp#722734): checking for null RESULT is not enough,
        ;; since variables can become dead due to later optimizations.
        (ir1-convert start next result
                     (if (eq (global-var-kind var) :global)
                         `(symbol-global-value ',name)
                         `(symbol-value ',name)))
        (etypecase var
          (leaf
           (cond
             ((lambda-var-p var)
              (let ((home (ctran-home-lambda-or-null start)))
                (when home
                  (sset-adjoin var (lambda-calls-or-closes home))))
              (when (lambda-var-ignorep var)
                ;; (ANSI's specification for the IGNORE declaration requires
                ;; that this be a STYLE-WARNING, not a full WARNING.)
                #-sb-xc-host
                (compiler-style-warn "reading an ignored variable: ~S" name)
                ;; there's no need for us to accept ANSI's lameness when
                ;; processing our own code, though.
                #+sb-xc-host
                (warn "reading an ignored variable: ~S" name)))
             (t
              ;; This case signals {EARLY,LATE}-DEPRECATION-WARNING
              ;; for CONSTANT nodes in :EARLY and :LATE deprecation
              ;; (constants in :FINAL deprecation are represented as
              ;; symbol-macros).
              (aver (memq (check-deprecated-thing 'variable name)
                          '(nil :early :late)))))
           (reference-leaf start next result var name))
          ((cons (eql macro)) ; symbol-macro
           ;; This case signals {EARLY,LATE,FINAL}-DEPRECATION-WARNING
           ;; for symbol-macros. Includes variables, constants,
           ;; etc. in :FINAL deprecation.
           (check-deprecated-thing 'variable name)
           ;; FIXME: [Free] type declarations. -- APD, 2002-01-26
           (ir1-convert start next result (cdr var)))
          (heap-alien-info
           (ir1-convert start next result `(%heap-alien ',var))))))
  (values))

;;; Find a compiler-macro for a form, taking FUNCALL into account.
(defun find-compiler-macro (opname form)
  (flet ((legal-cm-name-p (name)
           (and (legal-fun-name-p name)
                (or (not (symbolp name))
                    (not (sb!xc:macro-function name *lexenv*))))))
    (if (eq opname 'funcall)
        (let ((fun-form (cadr form)))
          (cond ((and (consp fun-form) (eq 'function (car fun-form))
                      (not (cddr fun-form)))
                 (let ((real-fun (cadr fun-form)))
                   (if (legal-cm-name-p real-fun)
                       (values (sb!xc:compiler-macro-function real-fun *lexenv*)
                               real-fun)
                       (values nil nil))))
                ((sb!xc:constantp fun-form *lexenv*)
                 (let ((fun (constant-form-value fun-form *lexenv*)))
                   (if (legal-cm-name-p fun)
                       ;; CLHS tells us that local functions must shadow
                       ;; compiler-macro-functions, but since the call is
                       ;; through a name, we are obviously interested
                       ;; in the global function.
                       ;; KLUDGE: CLHS 3.2.2.1.1 also says that it can be
                       ;; "a list whose car is funcall and whose cadr is
                       ;; a list (function name)", that means that
                       ;; (funcall 'name) that gets here doesn't fit the
                       ;; definition.
                       (values (sb!xc:compiler-macro-function fun nil) fun)
                       (values nil nil))))
                (t
                 (values nil nil))))
        (if (legal-fun-name-p opname)
            (values (sb!xc:compiler-macro-function opname *lexenv*) opname)
            (values nil nil)))))

;;; If FORM has a usable compiler macro, use it; otherwise return FORM itself.
;;; Return the name of the compiler-macro as a secondary value, if applicable.
(defun expand-compiler-macro (form)
  (binding* ((name (car form))
             ((cmacro-fun cmacro-fun-name)
              (find-compiler-macro name form)))
    (cond
      ((and cmacro-fun
            ;; CLHS 3.2.2.1.3 specifies that NOTINLINE
            ;; suppresses compiler-macros.
            (not (fun-lexically-notinline-p cmacro-fun-name)))
       (check-deprecated-thing 'function name)
       (values (handler-case (careful-expand-macro cmacro-fun form t)
                 (compiler-macro-keyword-problem (condition)
                   (print-compiler-message
                    *error-output* "note: ~A" (list condition))
                   form))
               cmacro-fun-name))
      (t
       (values form nil)))))

;;; Picks off special forms and compiler-macro expansions, and hands
;;; the rest to IR1-CONVERT-COMMON-FUNCTOID
(defun ir1-convert-functoid (start next result form)
  (let* ((op (car form))
         (translator (and (symbolp op) (info :function :ir1-convert op))))
    (if translator
        (funcall translator start next result form)
        (multiple-value-bind (res cmacro-fun-name)
               (expand-compiler-macro form)
             (cond ((eq res form)
                    (ir1-convert-common-functoid start next result form op))
                   (t
                    (unless (policy *lexenv* (zerop store-xref-data))
                      (record-call cmacro-fun-name (ctran-block start)
                                   *current-path*))
                    (ir1-convert start next result res)))))))

;;; Handles the "common" cases: any other forms except special forms
;;; and compiler-macros.
(defun ir1-convert-common-functoid (start next result form op)
  (cond ((or (symbolp op) (leaf-p op))
         (let ((lexical-def (if (leaf-p op) op (lexenv-find op funs))))
           (typecase lexical-def
             (null
              (check-deprecated-thing 'function op)
              (ir1-convert-global-functoid start next result form op))
             (functional
              (ir1-convert-local-combination start next result form
                                             lexical-def))
             (global-var
              (check-deprecated-thing 'function (leaf-source-name lexical-def))
              (ir1-convert-srctran start next result lexical-def form))
             (t
              (aver (and (consp lexical-def) (eq (car lexical-def) 'macro)))
              (ir1-convert start next result
                           (careful-expand-macro (cdr lexical-def) form))))))
        ((or (atom op) (not (eq (car op) 'lambda)))
         (compiler-error "illegal function call"))
        (t
         ;; implicitly (LAMBDA ..) because the LAMBDA expression is
         ;; the CAR of an executed form.
         (ir1-convert start next result `(%funcall ,@form)))))

;;; Convert anything that looks like a global function call.
(defun ir1-convert-global-functoid (start next result form fun)
  (declare (type ctran start next) (type (or lvar null) result)
           (list form))
  (when (eql fun 'declare)
    (compiler-error
     "~@<There is no function named ~S.  ~
      References to ~S in some contexts (like starts of blocks) are unevaluated ~
      expressions, but here the expression is being evaluated, which invokes ~
      undefined behaviour.~@:>" fun fun))
  ;; FIXME: Couldn't all the INFO calls here be converted into
  ;; standard CL functions, like MACRO-FUNCTION or something? And what
  ;; happens with lexically-defined (MACROLET) macros here, anyway?
  (ecase (info :function :kind fun)
    (:macro
     (ir1-convert start next result
                  (careful-expand-macro (info :function :macro-function fun)
                                        form))
     (unless (policy *lexenv* (zerop store-xref-data))
       (record-macroexpansion fun (ctran-block start) *current-path*)))
    ((nil :function)
     (ir1-convert-srctran start next result
                          (find-free-fun fun "shouldn't happen! (no-cmacro)")
                          form))))

;;; Expand FORM using the macro whose MACRO-FUNCTION is FUN, trapping
;;; errors which occur during the macroexpansion.
(defun careful-expand-macro (fun form &optional cmacro)
  (flet (;; Return a string to use as a prefix in error reporting,
         ;; telling something about which form caused the problem.
         (wherestring ()
           (let (;; We rely on the printer to abbreviate FORM.
                 (*print-length* 3)
                 (*print-level* 3))
             (format nil
                     "~@<~A of ~S. Use ~S to intercept.~%~:@>"
                     (cond (cmacro
                            #-sb-xc-host "Error during compiler-macroexpansion"
                            #+sb-xc-host "Error during XC compiler-macroexpansion")
                           (t
                            #-sb-xc-host "during macroexpansion"
                            #+sb-xc-host "during XC macroexpansion"))
                     form
                     '*break-on-signals*))))
    (handler-bind (;; KLUDGE: CMU CL in its wisdom (version 2.4.6 for Debian
                   ;; Linux, anyway) raises a CL:WARNING condition (not a
                   ;; CL:STYLE-WARNING) for undefined symbols when converting
                   ;; interpreted functions, causing COMPILE-FILE to think the
                   ;; file has a real problem, causing COMPILE-FILE to return
                   ;; FAILURE-P set (not just WARNINGS-P set). Since undefined
                   ;; symbol warnings are often harmless forward references,
                   ;; and since it'd be inordinately painful to try to
                   ;; eliminate all such forward references, these warnings
                   ;; are basically unavoidable. Thus, we need to coerce the
                   ;; system to work through them, and this code does so, by
                   ;; crudely suppressing all warnings in cross-compilation
                   ;; macroexpansion. -- WHN 19990412
                   #+(and cmu sb-xc-host)
                   (warning (lambda (c)
                              (compiler-notify
                               "~@<~A~:@_~
                                ~A~:@_~
                                ~@<(KLUDGE: That was a non-STYLE WARNING. ~
                                   Ordinarily that would cause compilation to ~
                                   fail. However, since we're running under ~
                                   CMU CL, and since CMU CL emits non-STYLE ~
                                   warnings for safe, hard-to-fix things (e.g. ~
                                   references to not-yet-defined functions) ~
                                   we're going to have to ignore it and ~
                                   proceed anyway. Hopefully we're not ~
                                   ignoring anything  horrible here..)~:@>~:>"
                               (wherestring)
                               c)
                              (muffle-warning)
                              (bug "no MUFFLE-WARNING restart")))
                   (error
                     (lambda (c)
                       (cond
                         (cmacro
                          ;; The spec is silent on what we should do. Signaling
                          ;; a full warning but declining to expand seems like
                          ;; a conservative and sane thing to do.
                          (compiler-warn "~@<~A~@:_ ~A~:>" (wherestring) c)
                          (return-from careful-expand-macro form))
                         (t
                          (compiler-error "~@<~A~@:_ ~A~:>"
                                          (wherestring) c))))))
      (funcall (valid-macroexpand-hook) fun form *lexenv*))))

;;;; conversion utilities

;;; Convert a bunch of forms, discarding all the values except the
;;; last. If there aren't any forms, then translate a NIL.
(declaim (ftype (sfunction (ctran ctran (or lvar null) list) (values))
                ir1-convert-progn-body))
(defun ir1-convert-progn-body (start next result body)
  (if (endp body)
      (reference-constant start next result nil)
      (let ((this-start start)
            (forms body))
        (loop
          (let ((form (car forms)))
            (setf this-start
                  (maybe-instrument-progn-like this-start forms form))
            (when (endp (cdr forms))
              (ir1-convert this-start next result form)
              (return))
            (let ((this-ctran (make-ctran)))
              (ir1-convert this-start this-ctran nil form)
              (setq this-start this-ctran
                    forms (cdr forms)))))))
  (values))


;;;; code coverage

;;; Check the policy for whether we should generate code coverage
;;; instrumentation. If not, just return the original START
;;; ctran. Otherwise insert code coverage instrumentation after
;;; START, and return the new ctran.
(defun instrument-coverage (start mode form)
  ;; We don't actually use FORM for anything, it's just convenient to
  ;; have around when debugging the instrumentation.
  (declare (ignore form))
  (if (and (policy *lexenv* (> store-coverage-data 0))
           *code-coverage-records*
           *allow-instrumenting*)
      (let ((path (source-path-original-source *current-path*)))
        (when mode
          (push mode path))
        (if (member (ctran-block start)
                    (gethash path *code-coverage-blocks*))
            ;; If this source path has already been instrumented in
            ;; this block, don't instrument it again.
            start
            (let ((store
                   ;; Get an interned record cons for the path. A cons
                   ;; with the same object identity must be used for
                   ;; each instrument for the same block.
                   (or (gethash path *code-coverage-records*)
                       (setf (gethash path *code-coverage-records*)
                             (cons path +code-coverage-unmarked+))))
                  (next (make-ctran))
                  (*allow-instrumenting* nil))
              (push (ctran-block start)
                    (gethash path *code-coverage-blocks*))
              (let ((*allow-instrumenting* nil))
                (ir1-convert start next nil
                             `(locally
                                  (declare (optimize speed
                                                     (safety 0)
                                                     (debug 0)
                                                     (check-constant-modification 0)))
                                ;; We're being naughty here, and
                                ;; modifying constant data. That's ok,
                                ;; we know what we're doing.
                                (%rplacd ',store t))))
              next)))
      start))

;;; In contexts where we don't have a source location for FORM
;;; e.g. due to it not being a cons, but where we have a source
;;; location for the enclosing cons, use the latter source location if
;;; available. This works pretty well in practice, since many PROGNish
;;; macroexpansions will just directly splice a block of forms into
;;; some enclosing form with `(progn ,@body), thus retaining the
;;; EQness of the conses.
(defun maybe-instrument-progn-like (start forms form)
  (or (when (and *allow-instrumenting*
                 (not (get-source-path form)))
        (let ((*current-path* (get-source-path forms)))
          (when *current-path*
            (instrument-coverage start nil form))))
      start))

(defun record-code-coverage (info cc)
  (setf (gethash info *code-coverage-info*) cc))

(defun clear-code-coverage ()
  (clrhash *code-coverage-info*))

(defun reset-code-coverage ()
  (maphash (lambda (info cc)
             (declare (ignore info))
             (dolist (cc-entry cc)
               (setf (cdr cc-entry) +code-coverage-unmarked+)))
           *code-coverage-info*))

(defun code-coverage-record-marked (record)
  (aver (consp record))
  (ecase (cdr record)
    ((#.+code-coverage-unmarked+) nil)
    ((t) t)))


;;;; converting combinations

;;; Does this form look like something that we should add single-stepping
;;; instrumentation for?
(defun step-form-p (form)
  (flet ((step-symbol-p (symbol)
           (and (not (member (symbol-package symbol)
                             (load-time-value
                              ;; KLUDGE: packages we're not interested in
                              ;; stepping.
                              (mapcar #'find-package '(sb!c sb!int sb!impl
                                                       sb!kernel sb!pcl)) t)))
                ;; Consistent treatment of *FOO* vs (SYMBOL-VALUE '*FOO*):
                ;; we insert calls to SYMBOL-VALUE for most non-lexical
                ;; variable references in order to avoid them being elided
                ;; if the value is unused.
                (or (not (member symbol '(symbol-value symbol-global-value)))
                    (not (constantp (second form)))))))
    (and *allow-instrumenting*
         (policy *lexenv* (= insert-step-conditions 3))
         (listp form)
         (symbolp (car form))
         (step-symbol-p (car form)))))

;;; Convert a function call where the function FUN is a LEAF. FORM is
;;; the source for the call. We return the COMBINATION node so that
;;; the caller can poke at it if it wants to.
(declaim (ftype (sfunction (ctran ctran (or lvar null) list leaf) combination)
                ir1-convert-combination))
(defun ir1-convert-combination (start next result form fun)
  (let ((ctran (make-ctran))
        (fun-lvar (make-lvar)))
    (ir1-convert start ctran fun-lvar `(the (or function symbol) ,fun))
    (let ((combination
           (ir1-convert-combination-args fun-lvar ctran next result
                                         (cdr form))))
      (when (step-form-p form)
        ;; Store a string representation of the form in the
        ;; combination node. This will let the IR2 translator know
        ;; that we want stepper instrumentation for this node. The
        ;; string will be stored in the debug-info by DUMP-1-LOCATION.
        (setf (combination-step-info combination)
              (let ((*print-pretty* t)
                    (*print-circle* t)
                    (*print-readably* nil))
                (prin1-to-string form))))
      combination)))

;;; Convert the arguments to a call and make the COMBINATION
;;; node. FUN-LVAR yields the function to call. ARGS is the list of
;;; arguments for the call, which defaults to the cdr of source. We
;;; return the COMBINATION node.
(defun ir1-convert-combination-args (fun-lvar start next result args)
  (declare (type ctran start next)
           (type lvar fun-lvar)
           (type (or lvar null) result)
           (list args))
  (let ((node (make-combination fun-lvar)))
    (setf (lvar-dest fun-lvar) node)
    (collect ((arg-lvars))
      (let ((this-start start)
            (forms args))
        (dolist (arg args)
          (setf this-start
                (maybe-instrument-progn-like this-start forms arg))
          (setf forms (cdr forms))
          (let ((this-ctran (make-ctran))
                (this-lvar (make-lvar node)))
            (ir1-convert this-start this-ctran this-lvar arg)
            (setq this-start this-ctran)
            (arg-lvars this-lvar)))
        (link-node-to-previous-ctran node this-start)
        (use-continuation node next result)
        (setf (combination-args node) (arg-lvars))))
    node))

;;; Convert a call to a global function. If not :NOTINLINE, then we do
;;; source transforms and try out any inline expansion. If there is no
;;; expansion, but is :INLINE, then give an efficiency note (unless a
;;; known function which will quite possibly be open-coded.) Next, we
;;; go to ok-combination conversion.
(defun ir1-convert-srctran (start next result var form)
  (declare (type ctran start next) (type (or lvar null) result)
           (type global-var var))
  (let ((name (leaf-source-name var))
        (inlinep (when (defined-fun-p var)
                   (defined-fun-inlinep var))))
    (if (eq inlinep :notinline)
        (ir1-convert-combination start next result form var)
        (let* ((transform (info :function :source-transform name)))
          (if transform
              (multiple-value-bind (transformed pass)
                  (if (functionp transform)
                      (funcall transform form *lexenv*)
                      (let ((result
                             (if (eq (cdr transform) :predicate)
                                 (and (singleton-p (cdr form))
                                      `(%instance-typep
                                        ,(cadr form)
                                        ',(dd-name (car transform))))
                                 (slot-access-transform
                                  (if (consp name) :write :read)
                                  (cdr form) transform))))
                        (values result (null result))))
                (cond (pass
                       (ir1-convert-maybe-predicate start next result form var))
                      (t
                       (unless (policy *lexenv* (zerop store-xref-data))
                         (record-call name (ctran-block start) *current-path*))
                       (ir1-convert start next result transformed))))
              (ir1-convert-maybe-predicate start next result form var))))))

;;; KLUDGE: If we insert a synthetic IF for a function with the PREDICATE
;;; attribute, don't generate any branch coverage instrumentation for it.
(defvar *instrument-if-for-code-coverage* t)

;;; If the function has the PREDICATE attribute, and the RESULT's DEST
;;; isn't an IF, then we convert (IF <form> T NIL), ensuring that a
;;; predicate always appears in a conditional context.
;;;
;;; If the function isn't a predicate, then we call
;;; IR1-CONVERT-COMBINATION-CHECKING-TYPE.
(defun ir1-convert-maybe-predicate (start next result form var)
  (declare (type ctran start next)
           (type (or lvar null) result)
           (list form)
           (type global-var var))
  (let ((info (info :function :info (leaf-source-name var))))
    (if (and info
             (ir1-attributep (fun-info-attributes info) predicate)
             (not (if-p (and result (lvar-dest result)))))
        (let ((*instrument-if-for-code-coverage* nil))
          (ir1-convert start next result `(if ,form t nil)))
        (ir1-convert-combination-checking-type start next result form var))))

;;; Actually really convert a global function call that we are allowed
;;; to early-bind.
;;;
;;; If we know the function type of the function, then we check the
;;; call for syntactic legality with respect to the declared function
;;; type. If it is impossible to determine whether the call is correct
;;; due to non-constant keywords, then we give up, marking the call as
;;; :FULL to inhibit further error messages. We return true when the
;;; call is legal.
;;;
;;; If the call is legal, we also propagate type assertions from the
;;; function type to the arg and result lvars. We do this now so that
;;; IR1 optimize doesn't have to redundantly do the check later so
;;; that it can do the type propagation.
(defun ir1-convert-combination-checking-type (start next result form var)
  (declare (type ctran start next) (type (or lvar null) result)
           (list form)
           (type leaf var))
  (let* ((node (ir1-convert-combination start next result form var))
         (fun-lvar (basic-combination-fun node))
         (type (leaf-type var)))
    (when (validate-call-type node type var t)
      (setf (lvar-%derived-type fun-lvar)
            (make-single-value-type type))
      (setf (lvar-reoptimize fun-lvar) nil)))
  (values))

;;; Convert a call to a local function, or if the function has already
;;; been LET converted, then throw FUNCTIONAL to
;;; LOCALL-ALREADY-LET-CONVERTED. The THROW should only happen when we
;;; are converting inline expansions for local functions during
;;; optimization.
(defun ir1-convert-local-combination (start next result form functional)
  (assure-functional-live-p functional)
  (ir1-convert-combination start next result
                           form
                           (maybe-reanalyze-functional functional)))

;;;; PROCESS-DECLS

;;; Given a list of LAMBDA-VARs and a variable name, return the
;;; LAMBDA-VAR for that name, or NIL if it isn't found. We return the
;;; *last* variable with that name, since LET* bindings may be
;;; duplicated, and declarations always apply to the last.
(declaim (ftype (sfunction (list symbol) (or lambda-var list))
                find-in-bindings))
(defun find-in-bindings (vars name)
  (let ((found nil))
    (dolist (var vars)
      (cond ((leaf-p var)
             (when (eq (leaf-source-name var) name)
               (setq found var))
             (let ((info (lambda-var-arg-info var)))
               (when info
                 (let ((supplied-p (arg-info-supplied-p info)))
                   (when (and supplied-p
                              (eq (leaf-source-name supplied-p) name))
                     (setq found supplied-p))))))
            ((and (consp var) (eq (car var) name))
             (setf found (cdr var)))))
    found))

;;; Called by PROCESS-DECLS to deal with a variable type declaration.
;;; If a LAMBDA-VAR being bound, we intersect the type with the var's
;;; type, otherwise we add a type restriction on the var. If a symbol
;;; macro, we just wrap a THE around the expansion.
(defun process-type-decl (decl res vars context)
  (declare (type list decl vars) (type lexenv res))
  (let* ((type-specifier (first decl))
         (type (progn
                 (when (typep type-specifier 'type-specifier)
                   (check-deprecated-type type-specifier))
                 (compiler-specifier-type type-specifier))))
    (collect ((restr nil cons)
             (new-vars nil cons))
      (dolist (var-name (rest decl))
        (unless (symbolp var-name)
          (compiler-error "Variable name is not a symbol: ~S." var-name))
        (when (boundp var-name)
          (program-assert-symbol-home-package-unlocked
           context var-name "declaring the type of ~A"))
        (let* ((bound-var (find-in-bindings vars var-name))
               (var (or bound-var
                        (lexenv-find var-name vars)
                        (find-free-var var-name))))
          (etypecase var
            (leaf
             (flet
                 ((process-var (var bound-var)
                    (let* ((old-type (or (lexenv-find var type-restrictions)
                                         (leaf-type var)))
                           (int (if (or (fun-type-p type)
                                        (fun-type-p old-type))
                                    type
                                    (type-approx-intersection2
                                     old-type type))))
                      (cond ((eq int *empty-type*)
                             (unless (policy *lexenv* (= inhibit-warnings 3))
                               (warn
                                'type-warning
                                :format-control
                                "The type declarations ~S and ~S for ~S conflict."
                                :format-arguments
                                (list
                                 (type-specifier old-type)
                                 (type-specifier type)
                                 var-name))))
                            (bound-var
                             (setf (leaf-type bound-var) int
                                   (leaf-where-from bound-var) :declared))
                            (t
                             (restr (cons var int)))))))
               (process-var var bound-var)
               (awhen (and (lambda-var-p var)
                           (lambda-var-specvar var))
                      (process-var it nil))))
            (cons
             ;; FIXME: non-ANSI weirdness. [See lp#309122]
             (aver (eq (car var) 'macro))
             (new-vars `(,var-name . (macro . (the ,(first decl)
                                                ,(cdr var))))))
            (heap-alien-info
             (compiler-error
              "~S is an alien variable, so its type can't be declared."
              var-name)))))

      (if (or (restr) (new-vars))
          (make-lexenv :default res
                       :type-restrictions (restr)
                       :vars (new-vars))
          res))))

;;; This is somewhat similar to PROCESS-TYPE-DECL, but handles
;;; declarations for function variables. In addition to allowing
;;; declarations for functions being bound, we must also deal with
;;; declarations that constrain the type of lexically apparent
;;; functions.
(defun process-ftype-decl (type-specifier res names fvars context)
  (declare (type list names fvars)
           (type lexenv res))
  (let ((type (compiler-specifier-type type-specifier)))
    (check-deprecated-type type-specifier)
    (unless (csubtypep type (specifier-type 'function))
      (compiler-style-warn "ignoring declared FTYPE: ~S (not a function type)"
                           type-specifier)
      (return-from process-ftype-decl res))
    (collect ((res nil cons))
      (dolist (name names)
        (when (fboundp name)
          (program-assert-symbol-home-package-unlocked
           context name "declaring the ftype of ~A"))
        (let ((found (find name fvars :key #'leaf-source-name :test #'equal)))
          (cond
           (found
            (setf (leaf-type found) type)
            (assert-definition-type found type
                                    :unwinnage-fun #'compiler-notify
                                    :where "FTYPE declaration"))
           (t
            (res (cons (find-lexically-apparent-fun
                        name "in a function type declaration")
                       type))))))
      (if (res)
          (make-lexenv :default res :type-restrictions (res))
          res))))

;;; Process a special declaration, returning a new LEXENV. A non-bound
;;; special declaration is instantiated by throwing a special variable
;;; into the variables if BINDING-FORM-P is NIL, or otherwise into
;;; *POST-BINDING-VARIABLE-LEXENV*.
(defun process-special-decl (spec res vars binding-form-p context)
  (declare (list spec vars) (type lexenv res))
  (collect ((new-venv nil cons))
    (dolist (name (cdr spec))
      ;; While CLHS seems to allow local SPECIAL declarations for constants,
      ;; whatever the semantics are supposed to be is not at all clear to me
      ;; -- since constants aren't allowed to be bound it should be a no-op as
      ;; no-one can observe the difference portably, but specials are allowed
      ;; to be bound... yet nowhere does it say that the special declaration
      ;; removes the constantness. Call it a spec bug and prohibit it. Same
      ;; for GLOBAL variables.
      (let ((kind (info :variable :kind name)))
        (unless (member kind '(:special :unknown))
          (error "Can't declare ~(~A~) variable locally special: ~S" kind name)))
      (program-assert-symbol-home-package-unlocked
       context name "declaring ~A special")
      (let ((var (find-in-bindings vars name)))
        (etypecase var
          (cons
           (aver (eq (car var) 'macro))
           (compiler-error
            "~S is a symbol-macro and thus can't be declared special."
            name))
          (lambda-var
           (when (lambda-var-ignorep var)
             ;; ANSI's definition for "Declaration IGNORE, IGNORABLE"
             ;; requires that this be a STYLE-WARNING, not a full WARNING.
             (compiler-style-warn
              "The ignored variable ~S is being declared special."
              name))
           (setf (lambda-var-specvar var)
                 (specvar-for-binding name)))
          (null
           (unless (or (assoc name (new-venv) :test #'eq))
             (new-venv (cons name (specvar-for-binding name))))))))
    (cond (binding-form-p
           (setf *post-binding-variable-lexenv*
                 (append (new-venv) *post-binding-variable-lexenv*))
           res)
          ((new-venv)
           (make-lexenv :default res :vars (new-venv)))
          (t
           res))))

;;; Return a DEFINED-FUN which copies a GLOBAL-VAR but for its INLINEP
;;; (and TYPE if notinline), plus type-restrictions from the lexenv.
(defun make-new-inlinep (var inlinep local-type)
  (declare (type global-var var) (type inlinep inlinep))
  (let* ((type (if (and (eq inlinep :notinline)
                        (not (eq (leaf-where-from var) :declared)))
                   (specifier-type 'function)
                   (leaf-type var)))
         (res (make-defined-fun
               :%source-name (leaf-source-name var)
               :where-from (leaf-where-from var)
               :type (if local-type
                         (type-intersection local-type type)
                         type)
               :inlinep inlinep)))
    (when (defined-fun-p var)
      (setf (defined-fun-inline-expansion res)
            (defined-fun-inline-expansion var))
      (setf (defined-fun-functionals res)
            (defined-fun-functionals var)))
    ;; FIXME: Is this really right? Needs we not set the FUNCTIONAL
    ;; to the original global-var?
    res))

;;; Parse an inline/notinline declaration. If it's a local function we're
;;; defining, set its INLINEP. If a global function, add a new FENV entry.
(defun process-inline-decl (spec res fvars)
  (let ((sense (cdr (assoc (first spec) *inlinep-translations* :test #'eq)))
        (new-fenv ()))
    (dolist (name (rest spec))
      (let ((fvar (find name fvars :key #'leaf-source-name :test #'equal)))
        (if fvar
            (setf (functional-inlinep fvar) sense)
            (let ((found (find-lexically-apparent-fun
                          name "in an inline or notinline declaration")))
              (etypecase found
                (functional
                 (when (policy *lexenv* (>= speed inhibit-warnings))
                   (compiler-notify "ignoring ~A declaration not at ~
                                     definition of local function:~%  ~S"
                                    sense name)))
                (global-var
                 (let ((type
                        (cdr (assoc found (lexenv-type-restrictions res)))))
                   (push (cons name (make-new-inlinep found sense type))
                         new-fenv))))))))
    (if new-fenv
        (make-lexenv :default res :funs new-fenv)
        res)))

;;; like FIND-IN-BINDINGS, but looks for #'FOO in the FVARS
(defun find-in-bindings-or-fbindings (name vars fvars)
  (declare (list vars fvars))
  (typecase name
    (atom
     (find-in-bindings vars name))
    ((cons (eql function) (cons * null))
     (find (cadr name) fvars :key #'leaf-source-name :test #'equal))
    (t
     (compiler-error "Malformed function or variable name ~S." name))))

;;; Process an ignore/ignorable declaration, checking for various losing
;;; conditions.
(defun process-ignore-decl (spec vars fvars lexenv)
  (declare (list spec vars fvars))
  (dolist (name (rest spec))
    (let ((var (find-in-bindings-or-fbindings name vars fvars)))
      (cond
        ((not var)
         ;; ANSI's definition for "Declaration IGNORE, IGNORABLE"
         ;; requires that this be a STYLE-WARNING, not a full WARNING.
         ;; But, other Lisp hosts signal a full warning, so when building
         ;; the cross-compiler, compile it as #'WARN so that in a self-hosted
         ;; build we can at least crash in the same way,
         ;; until we resolve this question about how severe the warning is.
         (multiple-value-call #+sb-xc-host #'warn
                              #-sb-xc-host #'compiler-style-warn
           "~A declaration for ~A: ~A"
           (first spec)
           (if (symbolp name)
               (values
                (case (info :variable :kind name)
                  (:special "a special variable")
                  (:global "a global lexical variable")
                  (:alien "a global alien variable")
                  (t (if (assoc name (lexenv-vars lexenv))
                         "a variable from outer scope"
                         "an unknown variable")))
                name)
               (values
                (cond ((assoc (second name) (lexenv-funs lexenv)
                              :test #'equal)
                       "a function from outer scope")
                      ((info :function :kind (second name))
                       "a global function")
                      (t
                       "an unknown function"))
                (second name)))))
        ((and (consp var) (eq (car var) 'macro))
         ;; Just ignore the IGNORE decl: we don't currently signal style-warnings
         ;; for unused symbol-macros, so there's no need to do anything.
         )
        ((functional-p var)
         (setf (leaf-ever-used var) t))
        ((and (lambda-var-specvar var) (eq (first spec) 'ignore))
         ;; ANSI's definition for "Declaration IGNORE, IGNORABLE"
         ;; requires that this be a STYLE-WARNING, not a full WARNING.
         (compiler-style-warn "Declaring special variable ~S to be ~A"
                              name
                              (first spec)))
        ((eq (first spec) 'ignorable)
         (setf (leaf-ever-used var) t))
        (t
         (setf (lambda-var-ignorep var) t)))))
  (values))

(defun process-extent-decl (names vars fvars kind)
  (let ((extent
          (ecase kind
            (truly-dynamic-extent
             :always-dynamic)
            (dynamic-extent
             (when *stack-allocate-dynamic-extent*
               :maybe-dynamic))
            (indefinite-extent
             :indefinite))))
    (if extent
        (dolist (name names)
          (cond
            ((symbolp name)
             (let* ((bound-var (find-in-bindings vars name))
                    (var (or bound-var
                             (lexenv-find name vars)
                             (maybe-find-free-var name))))
               (etypecase var
                 (leaf
                  (if bound-var
                      (if (and (leaf-extent var) (neq extent (leaf-extent var)))
                          (warn "Multiple incompatible extent declarations for ~S?" name)
                          (setf (leaf-extent var) extent))
                      (compiler-notify
                       "Ignoring free ~S declaration: ~S" kind name)))
                 (cons
                  (compiler-error "~S on symbol-macro: ~S" kind name))
                 (heap-alien-info
                  (compiler-error "~S on alien-variable: ~S" kind name))
                 (null
                  (compiler-style-warn
                   "Unbound variable declared ~S: ~S" kind name)))))
            ((and (consp name)
                  (eq (car name) 'function)
                  (null (cddr name))
                  (valid-function-name-p (cadr name))
                  (neq :indefinite extent))
             (let* ((fname (cadr name))
                    (bound-fun (find fname fvars
                                     :key #'leaf-source-name
                                     :test #'equal))
                    (fun (or bound-fun (lexenv-find fname funs))))
               (etypecase fun
                 (leaf
                  (if bound-fun
                      #!+stack-allocatable-closures
                      (setf (leaf-extent bound-fun) extent)
                      #!-stack-allocatable-closures
                      (compiler-notify
                       "Ignoring DYNAMIC-EXTENT declaration on function ~S ~
                        (not supported on this platform)." fname)
                      (compiler-notify
                       "Ignoring free DYNAMIC-EXTENT declaration: ~S" name)))
                 (cons
                  (compiler-error "DYNAMIC-EXTENT on macro: ~S" name))
                 (null
                  (compiler-style-warn
                   "Unbound function declared DYNAMIC-EXTENT: ~S" name)))))
            (t
             (compiler-error "~S on a weird thing: ~S" kind name))))
        (when (policy *lexenv* (= speed 3))
          (compiler-notify "Ignoring DYNAMIC-EXTENT declarations: ~S" names)))))

;;; FIXME: This is non-ANSI, so the default should be T, or it should
;;; go away, I think.
;;; Or just rename the declaration to SB-C:RESULT-TYPE so that it's not
;;; a symbol in the CL package, and then eliminate this switch.
;;; It's permissible to have implementation-specific declarations.
(defvar *suppress-values-declaration* nil
  #!+sb-doc
  "If true, processing of the VALUES declaration is inhibited.")

;;; Process a single declaration spec, augmenting the specified LEXENV
;;; RES. Return RES and result type. VARS and FVARS are as described
;;; PROCESS-DECLS.
(defun process-1-decl (raw-spec res vars fvars binding-form-p context)
  (declare (type list raw-spec vars fvars))
  (declare (type lexenv res))
  (let ((spec (canonized-decl-spec raw-spec))
        (optimize-qualities))
    ;; FIXME: we can end up with a chain of spurious parent lexenvs,
    ;; when logically the processing of decls should yield at most
    ;; two new lexenvs: one for the bindings and one for post-binding.
    ;; It's possible that there's a simple fix of re-linking the resulting
    ;; lexenv directly to *lexenv* as its parent.
    (values
     (case (first spec)
       (type
        (process-type-decl (cdr spec) res vars context))
       ((ignore ignorable)
        (process-ignore-decl spec vars fvars res)
        res)
       (special (process-special-decl spec res vars binding-form-p context))
       (ftype
        (unless (cdr spec)
          (compiler-error "no type specified in FTYPE declaration: ~S" spec))
        (process-ftype-decl (second spec) res (cddr spec) fvars context))
       ((inline notinline maybe-inline)
        (process-inline-decl spec res fvars))
       (optimize
        (multiple-value-bind (new-policy specified-qualities)
            (process-optimize-decl spec (lexenv-policy res))
          (setq optimize-qualities specified-qualities)
          (make-lexenv :default res :policy new-policy)))
       (muffle-conditions
        (make-lexenv
         :default res
         :handled-conditions (process-muffle-conditions-decl
                              spec (lexenv-handled-conditions res))))
       (unmuffle-conditions
        (make-lexenv
         :default res
         :handled-conditions (process-unmuffle-conditions-decl
                              spec (lexenv-handled-conditions res))))
       ((dynamic-extent truly-dynamic-extent indefinite-extent)
        (process-extent-decl (cdr spec) vars fvars (first spec))
        res)
       ((disable-package-locks enable-package-locks)
        (make-lexenv
         :default res
         :disabled-package-locks (process-package-lock-decl
                                  spec (lexenv-disabled-package-locks res))))
       ;; We may want to detect LAMBDA-LIST and VALUES decls here,
       ;; and report them as "Misplaced" rather than "Unrecognized".
       (t
        (unless (info :declaration :recognized (first spec))
          (compiler-warn "unrecognized declaration ~S" raw-spec))
        (let ((fn (info :declaration :handler (first spec))))
          (if fn
              (funcall fn res spec vars fvars)
              res))))
     optimize-qualities)))

;;; Use a list of DECLARE forms to annotate the lists of LAMBDA-VAR
;;; and FUNCTIONAL structures which are being bound. In addition to
;;; filling in slots in the leaf structures, we return a new LEXENV,
;;; which reflects pervasive special and function type declarations,
;;; (NOT)INLINE declarations and OPTIMIZE declarations, and type of
;;; VALUES declarations. If BINDING-FORM-P is true, the third return
;;; value is a list of VARs that should not apply to the lexenv of the
;;; initialization forms for the bindings, but should apply to the body.
;;;
;;; This is also called in main.lisp when PROCESS-FORM handles a use
;;; of LOCALLY.
(defun process-decls (decls vars fvars &key
                      (lexenv *lexenv*) (binding-form-p nil) (context :compile)
                      (allow-lambda-list nil))
  (declare (list decls vars fvars))
  (let ((result-type *wild-type*)
        (allow-values-decl allow-lambda-list)
        (explicit-check)
        (allow-explicit-check allow-lambda-list)
        (lambda-list (if allow-lambda-list :unspecified nil))
        (optimize-qualities)
        (*post-binding-variable-lexenv* nil))
    (flet ((process-it (spec decl)
             (cond ((atom spec)
                    (compiler-error "malformed declaration specifier ~S in ~S"
                                    spec decl))
                   ((and (eq allow-lambda-list t)
                         (typep spec '(cons (eql lambda-list) (cons t null))))
                    (setq lambda-list (cadr spec) allow-lambda-list nil))
                   ((and allow-values-decl
                         (typep spec '(cons (eql values)))
                         (not *suppress-values-declaration*))
                    ;; Why do we allow more than one VALUES decl? I don't know.
                    (setq result-type
                          (values-type-intersection
                           result-type
                           (compiler-values-specifier-type
                            (let ((types (cdr spec)))
                              (if (singleton-p types)
                                  (car types)
                                  `(values ,@types)))))))
                   ((and allow-explicit-check
                         (typep spec '(cons (eql explicit-check))))
                    ;; EXPLICIT-CHECK can specify that all arguments will be
                    ;; checked by the function body, and/or all results.
                    ;; Alternatively, a subset of arguments can be listed,
                    ;; so that sequence operations can dispatch on the sequence
                    ;; arguments, but benefit from automatic checks of others.
                    ;; You can't actually specify anything yet, as the goal
                    ;; is to be 100% compatible with the globaldb attribute.
                    (aver (not (cdr spec)))
                    (setq explicit-check t
                          allow-explicit-check nil)) ; at most one of this decl
                   (t
                    (multiple-value-bind (new-env new-qualities)
                        (process-1-decl spec lexenv vars fvars
                                        binding-form-p context)
                      (setq lexenv new-env
                            optimize-qualities
                            (nconc new-qualities optimize-qualities)))))))
      (dolist (decl decls)
        (dolist (spec (rest decl))
          (if (eq context :compile)
              (let ((*current-path* (or (get-source-path spec)
                                        (get-source-path decl)
                                        *current-path*)))
                (process-it spec decl))
            ;; Kludge: EVAL calls this function to deal with LOCALLY.
              (process-it spec decl)))))
    (warn-repeated-optimize-qualities (lexenv-policy lexenv) optimize-qualities)
    (values lexenv result-type *post-binding-variable-lexenv*
            lambda-list explicit-check)))

(defun %processing-decls (decls vars fvars ctran lvar binding-form-p fun)
  (multiple-value-bind (*lexenv* result-type post-binding-lexenv)
      (process-decls decls vars fvars :binding-form-p binding-form-p)
    (cond ((eq result-type *wild-type*)
           (funcall fun ctran lvar post-binding-lexenv))
          (t
           (let ((value-ctran (make-ctran))
                 (value-lvar (make-lvar)))
             (multiple-value-prog1
                 (funcall fun value-ctran value-lvar post-binding-lexenv)
               (let ((cast (make-cast value-lvar result-type
                                      (lexenv-policy *lexenv*))))
                 (link-node-to-previous-ctran cast value-ctran)
                 (setf (lvar-dest value-lvar) cast)
                 (use-continuation cast ctran lvar))))))))

(defmacro processing-decls ((decls vars fvars ctran lvar
                                   &optional post-binding-lexenv)
                            &body forms)
  (check-type ctran symbol)
  (check-type lvar symbol)
  (let ((post-binding-lexenv-p (not (null post-binding-lexenv)))
        (post-binding-lexenv (or post-binding-lexenv (sb!xc:gensym "LEXENV"))))
    `(%processing-decls ,decls ,vars ,fvars ,ctran ,lvar
                        ,post-binding-lexenv-p
                        (lambda (,ctran ,lvar ,post-binding-lexenv)
                          (declare (ignorable ,post-binding-lexenv))
                          ,@forms))))

;;; Return the SPECVAR for NAME to use when we see a local SPECIAL
;;; declaration. If there is a global variable of that name, then
;;; check that it isn't a constant and return it. Otherwise, create an
;;; anonymous GLOBAL-VAR.
(defun specvar-for-binding (name)
  (cond ((not (eq (info :variable :where-from name) :assumed))
         (let ((found (find-free-var name)))
           (when (heap-alien-info-p found)
             (compiler-error
              "~S is an alien variable and so can't be declared special."
              name))
           (unless (global-var-p found)
             (compiler-error
              "~S is a constant and so can't be declared special."
              name))
           found))
        (t
         (make-global-var :kind :special
                          :%source-name name
                          :where-from :declared))))
