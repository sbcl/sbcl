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

(in-package "SB-C")

(declaim (special *compiler-error-bailout*))

;;; the lexical environment we are currently converting in
(defvar *lexenv*)
(declaim (type lexenv *lexenv*))

;;; *CURRENT-FORM-NUMBER* is used in FIND-SOURCE-PATHS to compute the
;;; form number to associate with a source path. This should be bound
;;; to an initial value of 0 before the processing of each truly
;;; top level form.
(declaim (type index *current-form-number*))
(defvar *current-form-number*)

;;; Report as we try each transform? (either source or IR)
;;; Bind to T to see the ones that didn't abort,
;;; :ALL if you want to see each attempted transform.
;;; :DERIVE-TYPE for newly derived types of combinations.
(defvar *show-transforms-p* nil)

(declaim (inline source-form-has-path-p))
(defun source-form-has-path-p (form)
  (not (typep form '(or symbol fixnum character))))

(defun get-source-path (form)
  (when (source-form-has-path-p form)
    (gethash form *source-paths*)))

(defvar *transforming* 0)
(defvar *inlining* 0)
(declaim (type fixnum *transforming* *inlining*)
         #-sb-xc-host (always-bound *transforming* *inlining*))

(defun ensure-source-path (form)
  (flet ((level> (value kind)
           (let ((x (memq kind *current-path*)))
             (> value
                (if x
                    (cadr x)
                    0)))))
    (or (get-source-path form)
        (cond ((level> *transforming* 'transformed)
               ;; Don't hide all the transformed paths, since we might want
               ;; to look at the final form for error reporting.
               (list* form 'transformed *transforming* *current-path*))
              ;; Avoids notes about inlined code leaking out
              ((level> *inlining* 'inlined)
               (list* form 'inlined *inlining* *current-path*))
              (t
               (cons form *current-path*))))))

(defun note-source-path (form &rest arguments)
  (when (source-form-has-path-p form)
    (setf (gethash form *source-paths*)
          (apply #'list* 'original-source-start *current-form-number* arguments))))

(defun proper-list (form)
  (if (proper-list-p form)
      form
      (compiler-error "~@<~S is not a proper list.~@:>" form)))

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

(defun call-with-current-source-form (thunk &rest forms)
  (let ((*current-path* (or (and (some #'identity forms)
                                 (boundp '*source-paths*)
                                 (some #'get-source-path forms))
                            (and (boundp '*current-path*)
                                 *current-path*))))
    (funcall thunk)))

(defvar *derive-function-types* nil
  "Should the compiler assume that function types will never change,
  so that it can use type information inferred from current definitions
  to optimize code which uses those definitions? Setting this true
  gives non-ANSI, early-CMU-CL behavior. It can be useful for improving
  the efficiency of stable code.")

;;;; namespace management utilities

;;; Unpack (INFO :FUNCTION :INLINING-DATA FUN-NAME). If an explicit expansion
;;; is not stored but FUN-NAME is a structure constructor, reconstitute the
;;; expansion from the defstruct description. Secondary value is T if the expansion
;;; was explicit. See SAVE-INLINE-EXPANSION-P for why we care.
;;; (Essentially, once stored then always stored, lest inconsistency result)
;;; If we have just a DXABLE-ARGS, or nothing at all, return NIL and NIL.
;;; If called on a string or anything that is not a function designator,
;;; return NIL and NIL.
(declaim (ftype (sfunction (t) (values list boolean))
                fun-name-inline-expansion))
(defun fun-name-inline-expansion (fun-name)
  (multiple-value-bind (answer winp) (info :function :inlining-data fun-name)
    (typecase answer
      ;; an INLINING-DATA is a DXABLE-ARGS, so test it first
      (inlining-data (setq answer (inlining-data-expansion answer)))
      (dxable-args   (setq answer nil winp nil)))
    (when (and (not winp) (symbolp fun-name))
      (let ((info (info :function :source-transform fun-name)))
        (when (typep info '(cons defstruct-description (eql :constructor)))
          (let* ((dd (car info)) (spec (assq fun-name (dd-constructors dd))))
            (aver spec)
            (setq answer `(lambda ,@(structure-ctor-lambda-parts dd (cdr spec))))))))
    (values answer winp)))
(defun fun-name-dx-args (fun-name)
  (let ((answer (info :function :inlining-data fun-name)))
    (when (typep answer 'dxable-args)
      (dxable-args-list answer))))

;; As with LEXENV-FIND, we assume use of *LEXENV*, but macroexpanders
;; receive an explicit environment and should pass it.
;; A declaration will trump a proclamation.
(defun fun-lexically-notinline-p (name &optional (env *lexenv*))
  (let ((answer
         (typecase env
           (null nil)
           #+(and sb-fasteval (not sb-xc-host))
           (sb-interpreter:basic-env
            (sb-interpreter::fun-lexically-notinline-p name env))
           (t
            (let ((fun (cdr (assoc name (lexenv-funs env) :test #'equal))))
              ;; FIXME: this seems to omit FUNCTIONAL
              (when (defined-fun-p fun)
                (return-from fun-lexically-notinline-p
                  (eq (defined-fun-inlinep fun) 'notinline)))
              (loop for data in (lexenv-user-data env)
                    when (and (eq (car data) 'no-compiler-macro)
                              (eq (cdr data) name))
                    do
                    (return-from fun-lexically-notinline-p
                      t)))))))
    ;; If ANSWER is NIL, go for the global value
    (eq (or answer (info :function :inlinep name)) 'notinline)))


(declaim (start-block find-free-fun find-lexically-apparent-fun
                      ;; needed by ir1-translators
                      find-global-fun))

(defun maybe-defined-here (name where)
  (if (and (eq :defined where)
           (boundp '*compilation*)
           (hashset-find (fun-names-in-this-file *compilation*) name))
      :defined-here
      where))

;;; Return a GLOBAL-VAR structure usable for referencing the global
;;; function NAME.
(defun find-global-fun (name latep)
  (let ((kind (info :function :kind name)))
    (unless kind
      (setf (info :function :kind name) :function)
      (setf (info :function :where-from name) :assumed))
    (let ((where (info :function :where-from name)))
      (when (and (eq where :assumed)
                 ;; Slot accessors are defined just-in-time, if not already.
                 (not (typep name '(cons (eql sb-pcl::slot-accessor))))
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
      (case kind
        ((:macro :special-form)
         (compiler-warn "~(~a~) ~s where a function is expected" kind name)))
      (let ((ftype (global-ftype name))
            (notinline (fun-lexically-notinline-p name)))
        #-sb-xc-host
        (when (and (eq where :declared)
                   (policy *lexenv* (and (>= safety 1)
                                         (= debug 3)))
                   (not (or
                         (info :function :info name)
                         (let ((name (if (consp name)
                                         (second name)
                                         name)))
                           (system-package-p (symbol-package name))))))
          (setf where :declared-verify))
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
                         (maybe-defined-here name where)))))))

;;; If NAME already has a valid entry in (FREE-FUNS *IR1-NAMESPACE*), then return
;;; the value. Otherwise, make a new GLOBAL-VAR using information from
;;; the global environment and enter it in FREE-FUNS. If NAME
;;; names a macro or special form, then we error out using the
;;; supplied context which indicates what we were trying to do that
;;; demanded a function.
(declaim (ftype (sfunction (t string) global-var) find-free-fun))
(defun find-free-fun (name context &aux (free-funs (free-funs *ir1-namespace*)))
  (or (gethash name free-funs)
      (let ((kind (info :function :kind name)))
        (ecase kind
          ((:macro :special-form)
           (compiler-error "The ~(~S~) name ~S was found ~A."
                           kind name context))
          ((:function nil)
           (check-fun-name name)
           (let ((expansion (fun-name-inline-expansion name))
                 (inlinep (info :function :inlinep name)))
             (setf (gethash name free-funs)
                   (if (or expansion inlinep)
                       (let ((where (info :function :where-from name)))
                         (make-defined-fun
                          :%source-name name
                          :inline-expansion expansion
                          :inlinep inlinep
                          :where-from (if (eq inlinep 'notinline)
                                          where
                                          (maybe-defined-here name where))
                          :type (if (and (eq inlinep 'notinline)
                                         (neq where :declared))
                                    (specifier-type 'function)
                                    (global-ftype name))))
                       (find-global-fun name nil)))))))))

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

(declaim (end-block))

;;; Return the LEAF node for a global variable reference to NAME. If
;;; NAME is already entered in (FREE-VARS *IR1-NAMESPACE*), then we just return the
;;; corresponding value. Otherwise, we make a new leaf using
;;; information from the global environment and enter it in
;;; FREE-VARS.
(declaim (ftype (sfunction (t) (or leaf cons heap-alien-info)) find-free-var))
(defun find-free-var (name &aux (free-vars (free-vars *ir1-namespace*))
                                (existing (gethash name free-vars)))
  (unless (symbolp name)
    (compiler-error "Variable name is not a symbol: ~S." name))
  (or (when (and existing (neq existing :deprecated))
        existing)
      (let ((kind (info :variable :kind name))
            (type (info :variable :type name))
            (where-from (info :variable :where-from name))
            (deprecation-state (deprecated-thing-p 'variable name)))
        ;; For deprecated vars, warn about LET and LAMBDA bindings, SETQ, and ref.
        ;; Don't warn again if the name was already seen by the transform
        ;; of SYMBOL[-GLOBAL]-VALUE.
        (unless (eq existing :deprecated)
          (case deprecation-state
            ((:early :late)
             (check-deprecated-thing 'variable name))))
        (setf (gethash name free-vars)
              (case kind
                (:alien
                 (info :variable :alien-info name))
                ;; FIXME: The return value in this case should really be
                ;; of type SB-C::LEAF.  I don't feel too badly about it,
                ;; because the MACRO idiom is scattered throughout this
                ;; file, but it should be cleaned up so we're not
                ;; throwing random conses around.  --njf 2002-03-23
                (:macro
                 (let ((expansion (info :variable :macro-expansion name))
                       (type (type-specifier (info :variable :type name))))
                   `(macro . (the ,type ,expansion))))
                (:constant
                 (let ((value (symbol-value name)))
                   (make-constant value (ctype-of value) name)))
                (t
                 (make-global-var :kind kind
                                  :%source-name name
                                  :type type
                                  :where-from where-from)))))))

;;; Return T if and only if OBJ's nature as an externalizable thing renders
;;; it a leaf for dumping purposes. Symbols are leaflike despite havings slots
;;; containing pointers; similarly (COMPLEX RATIONAL) and RATIO.
(defun dumpable-leaflike-p (obj)
  (or (sb-xc:typep obj '(or symbol number character
                            ;; (ARRAY NIL) is not included in UNBOXED-ARRAY
                            (or unboxed-array (array nil))
                            system-area-pointer
                            #+sb-simd-pack simd-pack
                            #+sb-simd-pack-256 simd-pack-256))
      ;; STANDARD-OBJECT layouts use MAKE-LOAD-FORM, but all other layouts
      ;; have the same status as symbols - composite objects but leaflike.
      (and (typep obj 'layout) (not (layout-for-pcl-obj-p obj)))
      ;; PACKAGEs are also leaflike.
      (cl:typep obj 'package)
      ;; The cross-compiler wants to dump CTYPE instances as leaves,
      ;; but CLASSOIDs are excluded since they have a MAKE-LOAD-FORM method.
      #+sb-xc-host (cl:typep obj '(and ctype (not classoid)))
      (eq obj sb-lockless:+tail+)))

;;; Grovel over CONSTANT checking for any sub-parts that need to be
;;; processed with MAKE-LOAD-FORM. We have to be careful, because
;;; CONSTANT might be circular. We also check that the constant (and
;;; any subparts) are dumpable at all.
(defun maybe-emit-make-load-forms (constant)
  (declare #-sb-xc-host (inline alloc-xset))
  (dx-let ((things-processed (alloc-xset)))
    (named-let grovel ((value constant))
      (unless (or (dumpable-leaflike-p value)
                  (xset-member-p value things-processed)
                  #-sb-xc-host
                  (unbound-marker-p value))
        (add-to-xset value things-processed)
        ;; FIXME: shouldn't this be something like SB-XC:TYPECASE ?
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
          (instance
           ;; Behold the wonderfully clear sense of this-
           ;;  WHEN (EMIT-MAKE-LOAD-FORM VALUE)
           ;; meaning "when you're _NOT_ using a custom load-form"
           ;;
           ;; FIXME: What about funcallable instances with
           ;; user-defined MAKE-LOAD-FORM methods?
           (when (emit-make-load-form value)
             #+sb-xc-host
             (aver (eql (layout-bitmap (%instance-layout value))
                        sb-kernel:+layout-all-tagged+))
             (do-instance-tagged-slot (i value)
               (grovel (%instance-ref value i)))))
          (t
           (compiler-error
            "Objects of type ~/sb-impl:print-type-specifier/ can't be dumped into fasl files."
            (type-of value)))))))
  (values))

;;;; some flow-graph hacking utilities

;; Bind *COMPILER-ERROR-BAILOUT* to a function that throws out of the
;; body and converts a condition signalling form instead. The source
;; form is converted to a string since it may contain arbitrary
;; non-externalizable objects.
(defmacro ir1-error-bailout ((start next result form) &body body)
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
                                                ,form))))))

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
(declaim (inline use-ctran))
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
    (setf (ctran-next prev) nil)
    (link-node-to-previous-ctran new prev)
    (use-ctran new temp)
    (setf (ctran-source-path temp) (ctran-source-path prev))
    (link-node-to-previous-ctran old temp))
  (values))

(defun insert-node-after (old new)
  (let ((next (node-next old)))
    (cond (next
           (insert-node-before (ctran-next next) new))
          (t
           (let ((ctran (make-ctran)))
             (link-node-to-previous-ctran new ctran)
             (setf (block-last (node-block old)) new)
             (use-ctran old ctran))))))

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

(declaim (inline use-continuation))
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
                 forms () :debug-name "top level form")))
      (setf (functional-entry-fun res) res
            (functional-arg-documentation res) ()
            (functional-kind res) (functional-kind-attributes toplevel))
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
                      (let ((fm (cond ((comma-p subform)
                                       (comma-expr subform))
                                      ((atom subform)
                                       (return))
                                      (t
                                       (car subform)))))
                        (when (comma-p fm)
                          (setf fm (comma-expr fm)))
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
                      (when (comma-p subform)
                        (return))
                      (setq subform (cdr subform))
                      (when (eq subform trail) (return)))))
        (loop
         (frob)
         (frob)
         (setq trail (cdr trail)))))))


;;;; IR1-CONVERT, macroexpansion and special form dispatching

(declaim (start-block ir1-convert ir1-convert-progn-body
                      ir1-convert-combination-args reference-leaf
                      reference-constant
                      expand-compiler-macro
                      maybe-reanalyze-functional
                      ir1-convert-common-functoid))

;;; Translate FORM into IR1. The code is inserted as the NEXT of the
;;; CTRAN START. RESULT is the LVAR which receives the value of the
;;; FORM to be translated. The translators call this function
;;; recursively to translate their subnodes.
;;;
;;; As a special hack to make life easier in the compiler, a LEAF
;;; IR1-converts into a reference to that LEAF structure. This allows
;;; the creation using backquote of forms that contain leaf
;;; references, without having to introduce dummy names into the
;;; namespace.
(defun ir1-convert (start next result form)
  (declare (type ctran start next)
           (type (or lvar null) result))
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

;;; Generate a reference to a manifest constant, creating a new leaf
;;; if necessary. If we are producing a fasl file, make sure that
;;; MAKE-LOAD-FORM gets used on any parts of the constant that it
;;; needs to be.
(defun reference-constant (start next result value &optional (emit-load-forms (producing-fasl-file)))
  (declare (type ctran start next)
           (type (or lvar null) result))
  (when emit-load-forms
    (maybe-emit-make-load-forms value))
  (let* ((leaf (find-constant value))
         (res (make-ref leaf)))
    (push res (leaf-refs leaf))
    (link-node-to-previous-ctran res start)
    (use-continuation res next result))
  (values))

;;; Add FUNCTIONAL to the COMPONENT-REANALYZE-FUNCTIONALS, unless it's
;;; some trivial type for which reanalysis is a trivial no-op.
;;;
;;; FUNCTIONAL is returned.
(defun maybe-reanalyze-functional (functional)
  (aver (not (functional-kind-eq functional deleted))) ; bug 148
  (aver-live-component *current-component*)
  ;; When FUNCTIONAL is of a type for which reanalysis isn't a trivial
  ;; no-op
  (when (typep functional '(or optional-dispatch clambda))
    (pushnew functional
             (component-reanalyze-functionals *current-component*)))
  functional)

;;; Generate a REF node for LEAF, frobbing the LEAF structure as
;;; needed. If LEAF represents a defined function which has already
;;; been converted in the same compilation block, and is not
;;; NOTINLINE, then reference the functional instead.
(defun reference-leaf (start next result leaf &optional (name '.anonymous.))
  (declare (type ctran start next) (type (or lvar null) result) (type leaf leaf))
  (assure-leaf-live-p leaf)
  (let* ((type (lexenv-find leaf type-restrictions))
         (leaf (or (and (defined-fun-p leaf)
                        (neq (defined-fun-inlinep leaf) 'notinline)
                        (defined-fun-same-block-p leaf)
                        (let ((functional (defined-fun-functional leaf)))
                          (when (and functional (functional-kind-eq functional nil))
                            (maybe-reanalyze-functional functional))))
                   (when (and (functional-p leaf)
                              (functional-kind-eq leaf nil optional))
                     (maybe-reanalyze-functional leaf))
                   leaf))
         (ref (make-ref leaf name)))
    (when (and result
               (lambda-var-p leaf)
               (lambda-var-constant leaf))
      (push (make-lvar-lambda-var-annotation :lambda-var leaf)
            (lvar-annotations result)))
    (push ref (leaf-refs leaf))
    (when (and (functional-p leaf)
               (functional-ignore leaf))
      (#-sb-xc-host compiler-style-warn
       #+sb-xc-host warn
       "Calling an ignored function: ~S" (leaf-debug-name leaf)))
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

;;; Convert a reference to a symbolic constant or variable. If the
;;; symbol is entered in the LEXENV-VARS we use that definition,
;;; otherwise we find the current global definition. This is also
;;; where we pick off symbol macro and alien variable references.
(defun ir1-convert-var (start next result name)
  (declare (type ctran start next) (type (or lvar null) result) (symbol name))
  (let ((var (or (lexenv-find name vars) (find-free-var name))))
    (etypecase var
      (leaf
       (when (lambda-var-p var)
         (let ((home (ctran-home-lambda-or-null start)))
           (when (and home (neq (lambda-var-home var) home))
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
       (maybe-note-undefined-variable-reference var name)
       (reference-leaf start next result var name))
      ((cons (eql macro))               ; symbol-macro
       ;; FIXME: the following comment is probably wrong now.
       ;; If we warn here on :early and :late deprecation
       ;; then we get an extra warning somehow.
       ;; This case signals {EARLY,LATE,FINAL}-DEPRECATION-WARNING
       ;; for symbol-macros. Includes variables, constants,
       ;; etc. in :FINAL deprecation.
       (when (eq (deprecated-thing-p 'variable name) :final)
         (check-deprecated-thing 'variable name))
       ;; FIXME: [Free] type declarations. -- APD, 2002-01-26
       (ir1-convert start next result (cdr var)))
      (heap-alien-info
       (ir1-convert start next result `(%heap-alien ',var)))))
  (values))

;;; Find a compiler-macro for a form, taking FUNCALL into account.
(defun find-compiler-macro (opname form)
  (flet ((legal-cm-name-p (name)
           (and (legal-fun-name-p name)
                (or (not (symbolp name))
                    (not (macro-function name *lexenv*))))))
    (if (eq opname 'funcall)
        (let ((fun-form (cadr form)))
          (cond ((and (consp fun-form) (eq 'function (car fun-form))
                      (not (cddr fun-form)))
                 (let ((real-fun (cadr fun-form)))
                   (if (legal-cm-name-p real-fun)
                       (values (compiler-macro-function real-fun *lexenv*)
                               real-fun)
                       (values nil nil))))
                ((constantp fun-form *lexenv*)
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
                       (values (compiler-macro-function fun nil) fun)
                       (values nil nil))))
                (t
                 (values nil nil))))
        (if (legal-fun-name-p opname)
            (values (compiler-macro-function opname *lexenv*) opname)
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
        (funcall translator start next result (proper-list form))
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
              ;; Unlike inline expansion, macroexpansion is not optional,
              ;; but we clearly can't recurse forever.
              (let ((expansions (memq lexical-def *inline-expansions*)))
                (if (<= (or (cadr expansions) 0) *inline-expansion-limit*)
                    (let ((*inline-expansions*
                            (if expansions
                                (progn (incf (cadr expansions))
                                       *inline-expansions*)
                                (list* lexical-def 1 *inline-expansions*))))
                      (ir1-convert start next result
                                   (careful-expand-macro (cdr lexical-def) form)))
                    (progn
                      (compiler-warn "Recursion limit reached while expanding local macro ~
~/sb-ext:print-symbol-with-prefix/" op)
                      ;; Treat it as an global function, which is probably
                      ;; what was intended. The expansion thus far could be wrong,
                      ;; but that's not our problem.
                      (ir1-convert-global-functoid start next result form op))))))))
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

;;; This may produce false matches when there are multiple copies and
;;; they are reordered, duplicated or omitted. Still better than
;;; nothing.
(defun recover-source-paths (original-form expanded)
  (when (policy *lexenv* (> debug 1))
    (let ((equal-table (make-hash-table :test #'equal))
          some)
      (let ((seen (alloc-xset)))
        (labels ((rec (form)
                   (when (and (consp form)
                              (not (xset-member-p form seen)))
                     (let ((path (gethash form *source-paths*)))
                       (push path (gethash form equal-table))
                       (when path
                         (setf some t)))
                     (loop while (and (consp form)
                                      (not (xset-member-p form seen)))
                           do
                           (add-to-xset form seen)
                           (rec (pop form))))))
          (rec original-form)))
      (when some
        (let ((seen (alloc-xset)))
          (labels ((rec (form)
                     (unless (xset-member-p form seen)
                       (let ((original (gethash form equal-table)))
                         (when original
                           (let ((location
                                   (if (cdr original)
                                       (prog1 (car (last original))
                                         (setf (gethash form equal-table)
                                               (nbutlast original)))
                                       (car original))))
                             (when (and location
                                        (not (gethash form *source-paths*)))
                               (setf (gethash form *source-paths*) location))))
                         (loop while (and (consp form)
                                          (not (xset-member-p form seen)))
                               do
                               (add-to-xset form seen)
                               (rec (pop form)))))))
            (rec expanded)))))))

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
    (handler-bind ((error
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
      (let ((result (funcall (valid-macroexpand-hook) fun form *lexenv*)))
        #-sb-xc-host
        (recover-source-paths form result)
        result))))

;;;; conversion utilities

;;; Convert a bunch of forms, discarding all the values except the
;;; last. If there aren't any forms, then translate a NIL.
(defun ir1-convert-progn-body (start next result body)
  (declare (type ctran start next)
           (type (or lvar null) result)
           (type list body))
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


;;;; converting combinations

;;; Does this form look like something that we should add single-stepping
;;; instrumentation for?
(defun step-form-p (form)
  (flet ((step-symbol-p (symbol)
           (not (member (sb-xc:symbol-package symbol)
                        (load-time-value
                         ;; KLUDGE: packages we're not interested in
                         ;; stepping.
                         (mapcar #'find-package '(sb-c sb-int sb-impl
                                                  sb-kernel sb-pcl))
                         t)))))
    (and *allow-instrumenting*
         (policy *lexenv* (= insert-step-conditions 3))
         (listp form)
         (symbolp (car form))
         (step-symbol-p (car form)))))

;;; Convert a function call where the function FUN is a LEAF. FORM is
;;; the source for the call. We return the COMBINATION node so that
;;; the caller can poke at it if it wants to.
(defun ir1-convert-combination (start next result form fun)
  (declare (type ctran start next)
           (type (or lvar null) result)
           (type list form)
           (type leaf fun)
           #-sb-xc-host (values combination))
  (let ((ctran (make-ctran))
        (fun-lvar (make-lvar)))
    (reference-leaf start ctran fun-lvar fun)
    (let ((combination
           (ir1-convert-combination-args fun-lvar ctran next result
                                         (cdr (proper-list form)))))
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
(defun ir1-convert-combination-args (fun-lvar start next result args
                                     &key (pass-nargs t)
                                          arg-source-forms)
  (declare (type ctran start next)
           (type lvar fun-lvar)
           (type (or lvar null) result)
           (type list args))
  (let ((node (make-combination fun-lvar)))
    (unless pass-nargs
      (setf (combination-pass-nargs node) nil))
    (setf (lvar-dest fun-lvar) node)
    (collect ((arg-lvars))
      (let ((this-start start)
            (forms args))
        (dolist (arg args)
          (cond ((lvar-p arg)
                 (aver (not (lvar-dest arg)))
                 (arg-lvars arg)
                 (setf (lvar-dest arg) node))
                (t
                 (setf this-start
                       (maybe-instrument-progn-like this-start forms arg))
                 (let ((this-ctran (make-ctran))
                       (this-lvar (make-lvar node))
                       (*current-path* (or (get-source-path (pop arg-source-forms))
                                           *current-path*)))
                   (ir1-convert this-start this-ctran this-lvar arg)
                   (setq this-start this-ctran)
                   (arg-lvars this-lvar))))
          (pop forms))
        (link-node-to-previous-ctran node this-start)
        (use-continuation node next result)
        (setf (combination-args node) (arg-lvars))))
    node))

;;; Convert a call to a global function. If not NOTINLINE, then we do
;;; source transforms and try out any inline expansion. If there is no
;;; expansion, but is INLINE, then give an efficiency note (unless a
;;; known function which will quite possibly be open-coded.) Next, we
;;; go to ok-combination conversion.
(defun ir1-convert-srctran (start next result var form)
  (declare (type ctran start next) (type (or lvar null) result)
           (type global-var var))
  (let ((name (leaf-source-name var))
        (inlinep (when (defined-fun-p var)
                   (defined-fun-inlinep var))))
    (if (eq inlinep 'notinline)
        (ir1-convert-combination start next result form var)
        (let ((transform (info :function :source-transform name)))
          (if transform
              (multiple-value-bind (transformed pass)
                  (if (functionp transform)
                      (funcall transform form *lexenv*)
                      (struct-fun-transform transform form name))
                ;; Note that "pass" means fail. Gotta love it.
                (cond (pass
                       (ir1-convert-maybe-predicate start next result
                                                    (proper-list form)
                                                    var))
                      (t
                       (unless (policy *lexenv* (zerop store-xref-data))
                         (record-call name (ctran-block start) *current-path*))
                       (when (show-transform-p *show-transforms-p* name)
                         (show-transform "src" name transformed))
                       (let ((*transforming* (1+ *transforming*)))
                         (when (eq (car *current-path*) 'original-source-start)
                           (setf (ctran-source-path start) *current-path*))
                         (ir1-convert start next result transformed)))))
              (ir1-convert-maybe-predicate start next result
                                           (proper-list form)
                                           var))))))

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
        (wrap-predicate start next result form var)
        (ir1-convert-combination-checking-type start next result form var))))

;;; Like (if form t nil) but without coverage and without inserting
;;; leafs coming from %funcall into *current-path*
(defun wrap-predicate (start next result form var)
  (let* ((pred-ctran (make-ctran))
         (pred-lvar (make-lvar))
         (then-ctran (make-ctran))
         (then-block (ctran-starts-block then-ctran))
         (else-ctran (make-ctran))
         (else-block (ctran-starts-block else-ctran))
         (node (make-if :test pred-lvar
                        :consequent then-block
                        :alternative else-block)))
    (setf (lvar-dest pred-lvar) node)
    (ir1-convert-combination-checking-type start pred-ctran pred-lvar form var)
    (link-node-to-previous-ctran node pred-ctran)

    (let ((start-block (ctran-block pred-ctran)))
      (setf (block-last start-block) node)
      (ctran-starts-block next)

      (link-blocks start-block then-block)
      (link-blocks start-block else-block))
    (let ((*current-path* (cons t *current-path*)))
      (reference-constant then-ctran next result t))
    (let ((*current-path* (cons nil *current-path*)))
      (reference-constant else-ctran next result nil))))

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

(declaim (start-block process-decls make-new-inlinep
                      find-in-bindings
                      process-muffle-decls))

;;; Given a list of LAMBDA-VARs and a variable name, return the
;;; LAMBDA-VAR for that name, or NIL if it isn't found. We return the
;;; *last* variable with that name, since LET* bindings may be
;;; duplicated, and declarations always apply to the last.
(defun find-in-bindings (vars name)
  (declare (list vars) (symbol name)
           #-sb-xc-host (values (or lambda-var list)))
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
    (if type
        (collect ((restr nil cons)
                  (new-vars nil cons))
          (dolist (var-name (rest decl))
            (unless (symbolp var-name)
              (compiler-error "Variable name is not a symbol: ~S." var-name))
            (unless (eq (info :variable :kind var-name) :unknown)
              (program-assert-symbol-home-package-unlocked
               context var-name "declaring the type of ~A"))
            (let* ((bound-var (find-in-bindings vars var-name))
                   (var (or bound-var
                            (lexenv-find var-name vars)
                            (find-free-var var-name))))
              (maybe-note-undefined-variable-reference var var-name)
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
                                    "The type declarations ~
                                  ~/sb-impl:print-type/ and ~
                                  ~/sb-impl:print-type/ for ~
                                  ~S conflict."
                                    :format-arguments
                                    (list old-type type var-name))))
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
              res))
        res)))

;;; This is somewhat similar to PROCESS-TYPE-DECL, but handles
;;; declarations for function variables. In addition to allowing
;;; declarations for functions being bound, we must also deal with
;;; declarations that constrain the type of lexically apparent
;;; functions.
(defun process-ftype-decl (type-specifier res names fvars context)
  (declare (type list names fvars)
           (type lexenv res))
  (let ((type (or (compiler-specifier-type type-specifier)
                  (return-from process-ftype-decl res))))
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
        (let ((found (find name fvars :key (lambda (x)
                                             (unless (consp x) ;; macrolet
                                               (leaf-source-name x)))
                                      :test #'equal)))
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
;;; into the variables if POST-BINDING-LEXENV is NIL,
;;; or by mutating POST-BINDING-LEXENV if non-nil.
;;; Note that POST-BINDING-LEXENV is not actually a lexenv - it is only
;;; the contents of the 'vars' slot of a LEXENV, and moreover, as supplied
;;; to this function, it has a dummy cons in front which is later removed.
;;; The dummy cons serves a double purpose - as an indicator that this decl
;;; occurs in a LET/LET* form, and facilitating pass by reference of the list.
(defun process-special-decl (spec res vars post-binding-lexenv context)
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
      (unless (symbolp name)
        (compiler-error
         "~S is not a symbol and thus can't be declared special."
         name))
      (let ((kind (info :variable :kind name)))
        (unless (member kind '(:special :unknown))
          (compiler-error
           "Can't declare ~(~A~) variable locally special: ~S"
           kind name)))
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
    (cond (post-binding-lexenv
           (setf (cdr post-binding-lexenv)
                 (append (new-venv) (cdr post-binding-lexenv)))
           res)
          ((new-venv)
           (make-lexenv :default res :vars (new-venv)))
          (t
           res))))

(defun process-no-constraints-decl (spec vars)
  (dolist (name (rest spec))
    (let ((var (find-in-bindings vars name)))
      (cond
        ((not var)
         (warn "No ~s variable" name))
        (t
         (setf (lambda-var-no-constraints var) t))))))

(defun process-constant-decl (spec vars)
  (dolist (name (rest spec))
    (let ((var (find-in-bindings vars name)))
      (cond
        ((not var)
         (warn "No ~s variable" name))
        (t
         (setf (lambda-var-constant var) t))))))

;;; Return a DEFINED-FUN which copies a GLOBAL-VAR but for its INLINEP
;;; (and TYPE if notinline), plus type-restrictions from the lexenv.
(defun make-new-inlinep (var inlinep local-type)
  (declare (type global-var var) (type inlinep inlinep))
  (let* ((type (if (and (eq inlinep 'notinline)
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
      (setf (defined-fun-functional res)
            (defined-fun-functional var))
      (setf (defined-fun-same-block-p res)
            (defined-fun-same-block-p var)))
    ;; FIXME: Is this really right? Needs we not set the FUNCTIONAL
    ;; to the original global-var?
    res))

;;; Parse an inline/notinline declaration. If it's a local function we're
;;; defining, set its INLINEP. If a global function, add a new FENV entry.
(defun process-inline-decl (spec res fvars)
  (let ((sense (first spec))
        (new-fenv ()))
    (dolist (name (rest spec))
      (let ((fvar (find name fvars
                        :key (lambda (x)
                               (unless (consp x) ;; macrolet
                                 (leaf-source-name x)))
                        :test #'equal)))
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
     (find (cadr name) fvars :key (lambda (x)
                                    (if (consp x) ;; MACROLET
                                        (car x)
                                        (leaf-source-name x))) :test #'equal))
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
        ((and (consp var)
              (or (eq (car var) 'macro)
                  (and (consp (cdr var))
                       (eq (cadr var) 'macro))))
         ;; Just ignore the IGNORE decl: we don't currently signal style-warnings
         ;; for unused macrolet or symbol-macros, so there's no need to do anything.
         )
        ((functional-p var)
         (setf (leaf-ever-used var) t)
         (when (eq (first spec) 'ignore)
           (setf (functional-ignore var) t)))
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

(defvar *stack-allocate-dynamic-extent* t
  "If true (the default), the compiler believes DYNAMIC-EXTENT declarations
and stack allocates otherwise inaccessible parts of the object whenever
possible.")

(defun process-dynamic-extent-decl (names vars fvars)
  (if *stack-allocate-dynamic-extent*
      (dolist (name names)
        (cond
          ((symbolp name)
           (let* ((bound-var (find-in-bindings vars name))
                  (var (or bound-var
                           (lexenv-find name vars)
                           (find-free-var name))))
             (maybe-note-undefined-variable-reference var name)
             (etypecase var
               (leaf
                (cond
                  ((and (typep var 'global-var) (eq (global-var-kind var) :unknown)))
                  (bound-var
                   (setf (leaf-dynamic-extent var) t))
                  (t (compiler-notify "Ignoring free DYNAMIC-EXTENT declaration: ~S" name))))
               (cons
                (compiler-error "DYNAMIC-EXTENT on symbol-macro: ~S" name))
               (heap-alien-info
                (compiler-error "DYNAMIC-EXTENT on alien-variable: ~S" name)))))
          ((and (consp name)
                (eq (car name) 'function)
                (null (cddr name))
                (valid-function-name-p (cadr name)))
           (let* ((fname (cadr name))
                  (bound-fun (find fname fvars
                                   :key (lambda (x)
                                          (unless (consp x) ;; macrolet
                                            (leaf-source-name x)))
                                   :test #'equal))
                  (fun (or bound-fun (lexenv-find fname funs))))
             (etypecase fun
               (leaf
                (if bound-fun
                    (setf (leaf-dynamic-extent bound-fun) t)
                    (compiler-notify
                     "Ignoring free DYNAMIC-EXTENT declaration: ~S" name)))
               (cons
                (compiler-error "DYNAMIC-EXTENT on macro: ~S" name))
               (null
                (compiler-style-warn
                 "Unbound function declared DYNAMIC-EXTENT: ~S" name)))))
          (t
           (compiler-error "DYNAMIC-EXTENT on a weird thing: ~S" name))))
      (when (policy *lexenv* (= speed 3))
        (compiler-notify "Ignoring DYNAMIC-EXTENT declarations: ~S" names))))

;;; FIXME: This is non-ANSI, so the default should be T, or it should
;;; go away, I think.
;;; Or just rename the declaration to SB-C:RESULT-TYPE so that it's not
;;; a symbol in the CL package, and then eliminate this switch.
;;; It's permissible to have implementation-specific declarations.
(defvar *suppress-values-declaration* nil
  "If true, processing of the VALUES declaration is inhibited.")

;;; Process a single declaration spec, augmenting the specified LEXENV
;;; RES. Return RES and result type. VARS and FVARS are as described
;;; PROCESS-DECLS.
(defun process-1-decl (raw-spec res vars fvars post-binding-lexenv context)
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
       (special
        (process-special-decl spec res vars post-binding-lexenv context))
       (ftype
        (unless (cdr spec)
          (compiler-error "no type specified in FTYPE declaration: ~S" spec))
        (process-ftype-decl (second spec) res (cddr spec) fvars context))
       ((inline notinline maybe-inline)
        (process-inline-decl spec res fvars))
       (no-compiler-macro
        (make-lexenv :default res
                     :user-data (list*
                                 (cons 'no-compiler-macro (second spec))
                                 (lexenv-user-data res))))
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
       ((dynamic-extent)
        (process-dynamic-extent-decl (cdr spec) vars fvars)
        res)
       ((disable-package-locks enable-package-locks)
        (make-lexenv
         :default res
         :disabled-package-locks (process-package-lock-decl
                                  spec (lexenv-disabled-package-locks res))))
       (flushable
        (make-lexenv
         :default res
         :flushable (cdr spec)))
       (current-defmethod
        (destructuring-bind (name qualifiers specializers lambda-list)
            (cdr spec)
          (let* ((gfs (or (cu-methods *compilation-unit*)
                          (setf (cu-methods *compilation-unit*)
                                (make-hash-table :test #'equal))))
                 (methods (or (gethash name gfs)
                              (setf (gethash name gfs)
                                    (make-hash-table :test #'equal)))))
            (setf (gethash (cons qualifiers specializers) methods)
                  lambda-list)))
        res)
       (no-constraints
        (process-no-constraints-decl spec vars)
        res)
       (constant-value
        (process-constant-decl spec vars)
        res)
       ;; We may want to detect LAMBDA-LIST and VALUES decls here,
       ;; and report them as "Misplaced" rather than "Unrecognized".
       (t
        (let ((info (info :declaration :known (first spec))))
          (unless info
            (compiler-warn "unrecognized declaration ~S" raw-spec))
          (if (functionp info)
              (funcall info res spec vars fvars)
              res))))
     optimize-qualities)))

(defun process-muffle-decls (decls lexenv)
  (let (source-form)
   (flet ((process-it (spec)
            (cond ((atom spec))
                  ((member (car spec) '(muffle-conditions unmuffle-conditions))
                   (setq lexenv
                         (process-1-decl spec lexenv nil nil nil nil)))
                  ((eq (car spec) 'source-form)
                   (setf source-form (cadr spec))))))
     (dolist (decl decls)
       (dolist (spec (rest decl))
         (process-it spec))))
    (values lexenv source-form)))

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
        (local-optimize)
        source-form
        (post-binding-lexenv (if binding-form-p (list nil)))) ; dummy cell
    (flet ((process-it (spec decl)
             (cond ((atom spec)
                    (compiler-warn "malformed declaration specifier ~S in ~S"
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
                           (or (compiler-values-specifier-type
                                (let ((types (cdr spec)))
                                  (if (singleton-p types)
                                      (car types)
                                      `(values ,@types))))
                               (return-from process-it)))))
                   ((and allow-explicit-check
                         (typep spec '(cons (eql explicit-check))))
                    ;; EXPLICIT-CHECK by itself specifies that all argument and
                    ;; result types are checked by the function body.
                    ;; Alternatively, a subset of arguments, and/or :RESULT,
                    ;; can be specified to indicate that only a subset are
                    ;; checked; in that case, the compiler asserts all others.
                    (awhen (remove-if (lambda (x)
                                        (or (member x vars
                                                    :test #'eq
                                                    :key #'lambda-var-%source-name)
                                            (eq x :result)))
                                      (cdr spec))
                      (compiler-error "explicit-check list ~S must refer to lambda vars"
                                      it))
                    (setq explicit-check (or (cdr spec) t)
                          allow-explicit-check nil)) ; at most one of this decl
                   ((equal spec '(top-level-form))) ; ignore
                   ((typep spec '(cons (eql source-form)))
                    (setf source-form (cadr spec)))
                   ;; Used only for the current function.
                   ;; E.g. suppressing argument checking without doing
                   ;; so in all the subforms.
                   ((typep spec '(cons (eql local-optimize)))
                    (setf local-optimize spec))
                   (t
                    (multiple-value-bind (new-env new-qualities)
                        (process-1-decl spec lexenv vars fvars
                                        post-binding-lexenv context)
                      (setq lexenv new-env
                            optimize-qualities
                            (nconc new-qualities optimize-qualities)))))))
      (dolist (decl decls)
        (dolist (spec (rest decl))
          (if (eq context :compile)
              (with-current-source-form (spec decl) ; TODO this is a slight change to the previous code. make sure the behavior is identical
                (process-it spec decl))
              ;; Kludge: EVAL calls this function to deal with LOCALLY.
              (process-it spec decl)))))
    (warn-repeated-optimize-qualities (lexenv-policy lexenv) optimize-qualities)

    (values lexenv result-type (cdr post-binding-lexenv)
            lambda-list explicit-check source-form
            (when local-optimize
              (process-optimize-decl local-optimize (lexenv-policy lexenv))))))

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

(declaim (end-block))


;;;; Proclamations:
;;;
;;; PROCLAIM changes the global environment, so we must handle its
;;; compile time side effects if we are to keep the information in the
;;; (FREE-xxx *IR1-NAMESPACE*) tables up to date. When there is a var
;;; structure we disown it by replacing it with an updated copy. Uses
;;; of the variable which were translated before the PROCLAIM will get
;;; the old version, while subsequent references will get the updated
;;; information.

;;; If a special block compilation delimiter, then start or end the
;;; block as appropriate. If :BLOCK-COMPILE is T or NIL, then we
;;; ignore any start/end block declarations.
(defun process-block-compile-proclamation (kind entry-points)
  (if (eq *block-compile-argument* :specified)
      (ecase kind
        (start-block
         (finish-block-compilation)
         (let ((compilation *compilation*))
           (setf (block-compile compilation) t)
           (setf (entry-points compilation) entry-points)))
        (end-block
         (finish-block-compilation)))
      (compiler-notify "ignoring ~S declaration since ~
                        :BLOCK-COMPILE is not :SPECIFIED"
                       kind)))

;;; Update function type info cached in (FREE-FUNS *IR1-NAMESPACE*).
;;; If:
;;; -- there is a GLOBAL-VAR, then just update the type and remove the
;;;    name from the list of undefined functions. Someday we should
;;;    check for incompatible redeclaration.
;;; -- there is a FUNCTIONAL, then apply the type assertion to that
;;;    function.  This will only happen during block compilation.
(defun process-1-ftype-proclamation (name type)
  (proclaim-as-fun-name name)
  (let* ((free-funs (free-funs *ir1-namespace*))
         (var (gethash name free-funs)))
    (etypecase var
      (null)
      (global-var
       (setf (gethash name free-funs)
             (let ((kind (global-var-kind var)))
               (if (defined-fun-p var)
                   (make-defined-fun
                    :%source-name name :type type :where-from :declared :kind kind
                    :inlinep (defined-fun-inlinep var)
                    :inline-expansion (defined-fun-inline-expansion var)
                    :same-block-p (defined-fun-same-block-p var)
                    :functional (defined-fun-functional var))
                   (make-global-var :%source-name name :type type
                                    :where-from :declared :kind kind))))
       (when (defined-fun-p var)
         (let ((fun (defined-fun-functional var)))
           (when fun
             (assert-definition-type fun type
                                     :unwinnage-fun #'compiler-notify
                                     :where "this declaration"))))))))

(defun process-ftype-proclamation (spec names)
  (declare (list names))
  (let ((type (specifier-type spec)))
    (unless (csubtypep type (specifier-type 'function))
      (error "Not a function type: ~/sb-impl:print-type/" spec))
    (dolist (name names)
      (process-1-ftype-proclamation name type))))

;;; Replace each old var entry with one having the new type.
(defun process-type-proclamation (spec names)
  (declare (list names))
  (let ((free-vars (free-vars *ir1-namespace*))
        (type (specifier-type spec)))
    (dolist (name names)
      (let ((var (gethash name free-vars)))
        (etypecase var
          (null)
          ;; Constants cannot be redefined, and we already give the
          ;; constant object the tightest type possible. We will error
          ;; if the type is incompatible.
          (constant)
          (global-var
           (setf (gethash name free-vars)
                 (make-global-var :%source-name name
                                  :type type :where-from :declared
                                  :kind (global-var-kind var)))))))))

;;; Similar in effect to FTYPE, but change the :INLINEP. Copying the
;;; global-var ensures that when we substitute a functional for a
;;; global var (i.e. for DEFUN) that we won't clobber any uses
;;; declared :NOTINLINE.
(defun process-inline-proclamation (kind funs)
  (declare (type (and inlinep (not null)) kind))
  (dolist (name funs)
    (proclaim-as-fun-name name)
    (let* ((free-funs (free-funs *ir1-namespace*))
           (var (gethash name free-funs)))
      (etypecase var
        (null)
        (global-var
         (setf (gethash name free-funs)
               ;; Use the universal type as the local type restriction,
               ;; since we are processing the proclamation at the top
               ;; level and hence in a null lexical environment.
               (make-new-inlinep var kind *universal-type*)))))))

;;; Handle the compile-time side effects of PROCLAIM that don't happen
;;; at load time. These mostly side-effect the global state of the
;;; compiler, rather than the global environment.
(defun %compiler-proclaim (kind args)
  (case kind
    ((special global)
     (dolist (name args)
       (let* ((free-vars (free-vars *ir1-namespace*))
              (old (gethash name free-vars)))
         (when old
           (ecase (global-var-kind old)
             ((:special :global))
             (:unknown
              (setf (gethash name free-vars)
                    (make-global-var :%source-name name :type (leaf-type old)
                                     :where-from (leaf-where-from old)
                                     :kind (ecase kind
                                             (special :special)
                                             (global :global))))))))))
    ((start-block end-block)
     #-(and sb-devel sb-xc-host)
     (process-block-compile-proclamation kind args))
    (ftype
     (destructuring-bind (spec &rest args) args
       (process-ftype-proclamation spec args)))
    (type
     (destructuring-bind (spec &rest args) args
       (process-type-proclamation spec args)))
    ((inline notinline maybe-inline)
     (process-inline-proclamation kind args))))
