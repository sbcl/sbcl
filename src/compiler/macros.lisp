;;;; miscellaneous types and macros used in writing the compiler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; DEFTYPEs

;;; An INLINEP value describes how a function is called. The values
;;; have these meanings:
;;;     NIL     No declaration seen: do whatever you feel like, but don't
;;;             dump an inline expansion.
;;; NOTINLINE  NOTINLINE declaration seen: always do full function call.
;;;    INLINE  INLINE declaration seen: save expansion, expanding to it
;;;             if policy favors.
;;; MAYBE-INLINE
;;;             Retain expansion, but only use it opportunistically.
;;;             MAYBE-INLINE is quite different from INLINE. As explained
;;;             by APD on #lisp 2005-11-26: "MAYBE-INLINE lambda is
;;;             instantiated once per component, INLINE - for all
;;;             references (even under #'without FUNCALL)."
(deftype inlinep () '(member inline maybe-inline notinline nil))


;;;; source-hacking defining forms

;;; Parse a DEFMACRO-style lambda-list, setting things up so that a
;;; compiler error happens if the syntax is invalid.
;;;
;;; Define a function that converts a special form or other magical
;;; thing into IR1. LAMBDA-LIST is a defmacro style lambda
;;; list. START-VAR, NEXT-VAR and RESULT-VAR are bound to the start and
;;; result continuations for the resulting IR1. KIND is the function
;;; kind to associate with NAME.
;;; FIXME - Translators which accept implicit PROGNs fail on improper
;;; input, e.g. (LAMBDA (X) (CATCH X (BAZ) . 3))
;;; This is because &REST is defined to allow a dotted tail in macros,
;;; and we have no implementation-specific workaround to disallow it.
(defmacro def-ir1-translator (name (lambda-list start-var next-var result-var)
                              &body body)
  (binding* ((fn-name (symbolicate "IR1-CONVERT-" name))
             (whole-var (make-symbol "FORM"))
             ((lambda-expr doc)
              (make-macro-lambda nil lambda-list body :special-form name
                                 :doc-string-allowed :external
                                 :wrap-block nil)))
    (declare (ignorable doc)) ; unused on host
    ;; Maybe kill docstring, but only under the cross-compiler.
    #+(and (not sb-doc) sb-xc-host) (setq doc nil)
    `(progn
      (declaim (ftype (function (ctran ctran (or lvar null) t) (values))
                      ,fn-name))
      (defun ,fn-name (,start-var ,next-var ,result-var ,whole-var)
        (declare (ignorable ,start-var ,next-var ,result-var))
           ;; The guard function is a closure, which can't have its lambda-list
           ;; changed independently of other closures based on the same
           ;; simple-fun. So change it in the ir1-translator since the actual
           ;; lambda-list is not terribly useful.
        (declare (lambda-list ,lambda-list))
        (,lambda-expr ,whole-var *lexenv*)
        (values))
      #-sb-xc-host
      (progn (install-guard-function ',name '(:special ,name))
             (setf (documentation (symbol-function ',name) t) ',doc))
           ;; FIXME: Evidently "there can only be one!" -- we overwrite any
           ;; other :IR1-CONVERT value. This deserves a warning, I think.
      (setf (info :function :ir1-convert ',name) #',fn-name)
           ;; FIXME: rename this to SPECIAL-OPERATOR, to update it to
           ;; the 1990s?
      (setf (info :function :kind ',name) :special-form)
      ',name)))

;;; (This is similar to DEF-IR1-TRANSLATOR, except that we pass if the
;;; syntax is invalid.)
;;;
;;; Define a macro-like source-to-source transformation for the
;;; function NAME. A source transform may "pass" by returning a
;;; non-nil second value. If the transform passes, then the form is
;;; converted as a normal function call. If the supplied arguments are
;;; not compatible with the specified LAMBDA-LIST, then the transform
;;; automatically passes.
;;;
;;; Source transforms may only be defined for functions. Source
;;; transformation is not attempted if the function is declared
;;; NOTINLINE. Source transforms should not examine their arguments.
;;; A macro lambda list which destructures more than one level would
;;; be legal but suspicious, as inner list parsing "examines" arguments.
;;; If it matters how the function is used, then DEFTRANSFORM should
;;; be used to define an IR1 transformation.
;;;
;;; If the desirability of the transformation depends on the current
;;; OPTIMIZE parameters, then the POLICY macro should be used to
;;; determine when to pass.
;;;
;;; Note that while a compiler-macro must deal with being invoked when its
;;; whole form matches (FUNCALL <f> ...) so that it must skip over <f> to find
;;; the arguments, a source-transform need not worry about that situation.
;;; Any FUNCALL which is eligible for a source-transform calls the expander
;;; with the form's head replaced by a compiler representation of the function.
;;; Internalizing the name avoids semantic problems resulting from syntactic
;;; substitution. One problem would be that a source-transform can exist for
;;; a function named (SETF X), but ((SETF X) arg ...) would be bad syntax.
;;; Even if we decided that it is ok within the compiler - just not in code
;;; given to the compiler - the problem remains that changing (FUNCALL 'F x)
;;; to (F x) is wrong when F also names a local functionoid.
;;; Hence, either a #<GLOBAL-VAR> or #<DEFINED-FUN> appears in the form head.
;;;
(defmacro define-source-transform (fun-name lambda-list &body body)
  ;; FIXME: this is redundant with what will become MAKE-MACRO-LAMBDA
  ;; except that it needs a "silently do nothing" mode, which may or may not
  ;; be a generally exposed feature.
  (binding*
      (((forms decls) (parse-body body nil))
       ((llks req opt rest keys aux env whole)
        (parse-lambda-list
         lambda-list
         :accept
         (logandc2 (logior (lambda-list-keyword-mask '&environment)
                           (lambda-list-keyword-mask 'destructuring-bind))
                   ;; Function lambda lists shouldn't take &BODY.
                   (lambda-list-keyword-mask '&body))
         :context "a source transform"))
       ((outer-decl inner-decls) (extract-var-decls decls (append env whole)))
       ;; With general macros, the user can declare (&WHOLE W &ENVIRONMENT E ..)
       ;; and then (DECLARE (IGNORE W E)) which is a problem if system code
       ;; touches user variables. But this is all system code, so it's fine.
       (lambda-whole (if whole (car whole) (make-symbol "FORM")))
       (lambda-env (if env (car env) (make-symbol "ENV")))
       (new-ll (if (or whole env) ; strip them out if present
                   (make-lambda-list llks nil req opt rest keys aux)
                   lambda-list)) ; otherwise use the original list
       (args (make-symbol "ARGS")))
    `(%define-source-transform ',fun-name
           (named-lambda (:source-transform ,fun-name)
               (,lambda-whole ,lambda-env &aux (,args (cdr ,lambda-whole)))
             ,@(if (not env) `((declare (ignore ,lambda-env))))
             ,@outer-decl ; SPECIALs or something? I hope not.
             (if (not ,(emit-ds-lambda-list-match args new-ll))
                 (values nil t)
                 ;; The body can return 1 or 2 values, but consistently
                 ;; returning 2 values from the XEP is stylistically
                 ;; preferable, and produces shorter assembly code too.
                 (multiple-value-bind (call pass)
                     (binding* ,(expand-ds-bind new-ll args nil 'truly-the)
                       ,@inner-decls
                       (block ,(fun-name-block-name fun-name) ,@forms))
                   (values call pass)))))))
(defun %define-source-transform (fun-name lambda)
  (when (info :function :source-transform fun-name)
    (warn "Redefining source-transform for ~S" fun-name))
  (setf (info :function :source-transform fun-name) lambda))

;;;; lambda-list parsing utilities
;;;;
;;;; IR1 transforms, optimizers and type inferencers need to be able
;;;; to parse the IR1 representation of a function call using a
;;;; standard function lambda-list.

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)

;;; Given a DEFTRANSFORM-style lambda-list, generate code that parses
;;; the arguments of a combination with respect to that lambda-list.
;;; NODE-VAR the variable that holds the combination node.
;;; ERROR-FORM is a form which is evaluated when the syntax of the
;;; supplied arguments is incorrect or a non-constant argument keyword
;;; is supplied. Defaults and other gunk are ignored. The second value
;;; is a list of all the arguments bound, which the caller should make
;;; IGNORABLE if their only purpose is to make the syntax work.
(defun parse-deftransform (lambda-list node-var error-form)
  (multiple-value-bind (llks req opt rest keys) (parse-lambda-list lambda-list)
    (let* ((tail (make-symbol "ARGS"))
           (dummies (make-gensym-list 2))
           (all-dummies (cons tail dummies))
           (keyp (ll-kwds-keyp llks))
           ;; FIXME: the logic for keywordicating a keyword arg specifier
           ;; is repeated in a as many places as there are calls to
           ;; parse-lambda-just, and more.
           (keys (mapcar (lambda (spec)
                           (multiple-value-bind (key var)
                               (parse-key-arg-spec spec)
                             (cons var key)))
                         keys))
           final-mandatory-arg)
      (collect ((binds))
        (binds `(,tail (basic-combination-args ,node-var)))
        ;; The way this code checks for mandatory args is to verify that
        ;; the last positional arg is not null (it should be an LVAR).
        ;; But somebody could pedantically declare IGNORE on the last arg
        ;; so bind a dummy for it and then bind from the dummy.
        (mapl (lambda (args)
                (cond ((cdr args)
                       (binds `(,(car args) (pop ,tail))))
                      (t
                       (setq final-mandatory-arg (pop dummies))
                       (binds `(,final-mandatory-arg (pop ,tail))
                              `(,(car args) ,final-mandatory-arg)))))
              req)
        ;; Optionals are pretty easy.
        (dolist (arg opt)
          (binds `(,(if (atom arg) arg (car arg)) (pop ,tail))))
        ;; Now if min or max # of args is incorrect,
        ;; or there are unacceptable keywords, bail out
        (when (or req keyp (not rest))
          (binds `(,(pop dummies) ; binding is for effect, not value
                   (unless (and ,@(if req `(,final-mandatory-arg))
                                ,@(if (and (not rest) (not keyp))
                                      `((endp ,tail)))
                                ,@(if keyp
                                      (if (ll-kwds-allowp llks)
                                          `((check-key-args-constant ,tail))
                                          `((check-transform-keys
                                             ,tail ',(mapcar #'cdr keys))))))
                      ,error-form))))
        (when rest
          (binds `(,(car rest) ,tail)))
        ;; Return list of bindings, the list of user-specified symbols,
        ;; and the list of gensyms to be declared ignorable.
        (values (append (binds)
                        (mapcar (lambda (k)
                                  `(,(car k)
                                     (find-keyword-lvar ,tail ',(cdr k))))
                                keys))
                (sort (append (nset-difference (mapcar #'car (binds)) all-dummies)
                               (mapcar #'car keys))
                      #'string<)
                (sort (intersection (mapcar #'car (binds)) (cdr all-dummies))
                      #'string<))))))
) ; EVAL-WHEN

;;;; DEFTRANSFORM

;;; Define an IR1 transformation for NAME. An IR1 transformation
;;; computes a lambda that replaces the function variable reference
;;; for the call. A transform may pass (decide not to transform the
;;; call) by calling the GIVE-UP-IR1-TRANSFORM function. LAMBDA-LIST
;;; both determines how the current call is parsed and specifies the
;;; LAMBDA-LIST for the resulting lambda.
;;;
;;; We parse the call and bind each of the lambda-list variables to
;;; the lvar which represents the value of the argument. When parsing
;;; the call, we ignore the defaults, and always bind the variables
;;; for unsupplied arguments to NIL. If a required argument is
;;; missing, an unknown keyword is supplied, or an argument keyword is
;;; not a constant, then the transform automatically passes. The
;;; DECLARATIONS apply to the bindings made by DEFTRANSFORM at
;;; transformation time, rather than to the variables of the resulting
;;; lambda. Bound-but-not-referenced warnings are suppressed for the
;;; lambda-list variables. The DOC-STRING is used when printing
;;; efficiency notes about the defined transform.
;;;
;;; Normally, the body evaluates to a form which becomes the body of
;;; an automatically constructed lambda. We make LAMBDA-LIST the
;;; lambda-list for the lambda, and automatically insert declarations
;;; of the argument and result types. If the second value of the body
;;; is non-null, then it is a list of declarations which are to be
;;; inserted at the head of the lambda. Automatic lambda generation
;;; may be inhibited by explicitly returning a lambda from the body.
;;;
;;; The ARG-TYPES and RESULT-TYPE are used to create a function type
;;; which the call must satisfy before transformation is attempted.
;;; The function type specifier is constructed by wrapping (FUNCTION
;;; ...) around these values, so the lack of a restriction may be
;;; specified by omitting the argument or supplying *. The argument
;;; syntax specified in the ARG-TYPES need not be the same as that in
;;; the LAMBDA-LIST, but the transform will never happen if the
;;; syntaxes can't be satisfied simultaneously. If there is an
;;; existing transform for the same function that has the same type,
;;; then it is replaced with the new definition.
;;;
;;; These are the legal keyword options:
;;;   :RESULT - A variable which is bound to the result lvar.
;;;   :NODE   - A variable which is bound to the combination node for the call.
;;;   :POLICY - A form which is supplied to the POLICY macro to determine
;;;             whether this transformation is appropriate. If the result
;;;             is false, then the transform automatically gives up.
;;;   :DEFUN-ONLY
;;;           - Don't actually instantiate a transform, instead just DEFUN
;;;             Name with the specified transform definition function. This
;;;             may be later instantiated with %DEFTRANSFORM.
;;;   :INFO   - an extra piece of information the transform receives,
;;;             typically for use with :DEFUN-ONLY
;;;   :IMPORTANT
;;;           - If the transform fails and :IMPORTANT is
;;;               NIL,       then never print an efficiency note.
;;;               :SLIGHTLY, then print a note if SPEED>INHIBIT-WARNINGS.
;;;               T,         then print a note if SPEED>=INHIBIT-WARNINGS.
;;;             :SLIGHTLY is the default.
(defmacro deftransform (name (lambda-list &optional (arg-types '*)
                                          (result-type '*)
                                          &key result policy node defun-only
                                          (info nil info-p)
                                          (important :slightly))
                             &body body-decls-doc)
  (declare (type (member nil :slightly t) important))
  (cond (defun-only
         (aver (eq important :slightly)) ; can't be specified
         (aver (not policy))) ; has no effect on the defun
        (t
         (aver (not info))))
  (multiple-value-bind (body decls doc) (parse-body body-decls-doc t)
    (let ((n-node (or node '#:node))
          (n-decls '#:decls)
          (n-lambda '#:lambda))
      (multiple-value-bind (bindings vars)
          (parse-deftransform lambda-list n-node
                              '(give-up-ir1-transform))
        (let ((stuff
                `((,n-node ,@(if info-p (list info)) &aux ,@bindings
                           ,@(when result
                               `((,result (node-lvar ,n-node)))))
                  (declare (ignorable ,@(mapcar #'car bindings)))
                  (declare (lambda-list (node)))
                  ,@decls
                  ,@(if doc `(,doc))
                  ;; What purpose does it serve to allow the transform's body
                  ;; to return decls as a second value? They would go in the
                  ;; right place if simply returned as part of the expression.
                  (block ,(fun-name-block-name name)
                   (multiple-value-bind (,n-lambda ,n-decls)
                      (progn ,@body)
                    (if (and (consp ,n-lambda) (eq (car ,n-lambda) 'lambda))
                        ,n-lambda
                        `(lambda ,',lambda-list
                           (declare (ignorable ,@',vars))
                           ,@,n-decls
                           ,,n-lambda)))))))
          (if defun-only
              `(defun ,name ,@stuff)
              `(%deftransform ',name
                              ,(and policy `(lambda (,n-node) (policy ,n-node ,policy)))
                              '(function ,arg-types ,result-type)
                              (named-lambda (deftransform ,name) ,@stuff)
                              ,important)))))))

(defmacro deftransforms (names (lambda-list &optional (arg-types '*)
                                                      (result-type '*)
                                &key result policy node (important :slightly))
                         &body body-decls-doc)

  (let ((transform-name (symbolicate (car names) '-transform))
        (type (list 'function arg-types result-type)))
    `(progn
       (deftransform ,transform-name
           (,lambda-list ,arg-types ,result-type
            :defun-only t
            :result ,result :policy ,policy :node ,node)
         ,@body-decls-doc)
       (flet ,(if policy `((policy-test (node) (policy node ,policy))))
         ,@(mapcar (lambda (name)
                     `(%deftransform ',name ,(if policy '#'policy-test) ',type
                                     #',transform-name ,important))
                   names)))))

;;; Create a function which parses combination args according to WHAT
;;; and LAMBDA-LIST, where WHAT is either a function name or a list
;;; (FUN-NAME KIND) and does some KIND of optimization.
;;;
;;; The FUN-NAME must name a known function. LAMBDA-LIST is used
;;; to parse the arguments to the combination as in DEFTRANSFORM. If
;;; the argument syntax is invalid or there are non-constant keys,
;;; then we simply return NIL.
;;;
;;; The function is DEFUN'ed as FUNCTION-KIND-OPTIMIZER. Possible
;;; kinds are DERIVE-TYPE, OPTIMIZER, LTN-ANNOTATE and IR2-CONVERT. If
;;; a symbol is specified instead of a (FUNCTION KIND) list, then we
;;; just do a DEFUN with the symbol as its name, and don't do anything
;;; with the definition. This is useful for creating optimizers to be
;;; passed by name to DEFKNOWN.
;;;
;;; If supplied, NODE-VAR is bound to the combination node being
;;; optimized. If additional VARS are supplied, then they are used as
;;; the rest of the optimizer function's lambda-list. LTN-ANNOTATE
;;; methods are passed an additional POLICY argument, and IR2-CONVERT
;;; methods are passed an additional IR2-BLOCK argument.
(defmacro defoptimizer (what (lambda-list
                              &optional (node (sb-xc:gensym) node-p)
                              &rest vars)
                        &body body)
  (let ((name (flet ((function-name (name)
                       (etypecase name
                         (symbol name)
                         ((cons (eql setf) (cons symbol null))
                          (symbolicate (car name) "-" (cadr name))))))
                (if (symbolp what)
                    what
                    (symbolicate (function-name (first what))
                                 "-"
                                 (if (consp (second what))
                                     (caadr what)
                                     (second what))
                                 "-OPTIMIZER")))))
    (if (typep what '(cons (eql vop-optimize)))
        `(progn
           (defun ,name (,lambda-list)
             ,@body)
           ,@(loop for vop-name in (ensure-list (second what))
                   collect
                   `(set-vop-optimizer (template-or-lose ',vop-name) #',name)))
        (binding* (((forms decls) (parse-body body nil))
                   ((var-decls more-decls) (extract-var-decls decls vars))
                   ;; In case the BODY declares IGNORE of the formal NODE var,
                   ;; we rebind it from N-NODE and never reference it from BINDS.
                   (n-node (make-symbol "NODE"))
                   ((binds lambda-vars gensyms)
                    (parse-deftransform lambda-list n-node
                                        `(return-from ,name
                                           ,(if (and (consp what)
                                                     (eq (second what)
                                                         'equality-constraint))
                                                :give-up
                                                nil)))))
          (declare (ignore lambda-vars))
          `(progn
             ;; We can't stuff the BINDS as &AUX vars into the lambda list
             ;; because there can be a RETURN-FROM in there.
             (defun ,name (,n-node ,@vars)
               ,@(if var-decls (list var-decls))
               (let* (,@binds ,@(if node-p `((,node ,n-node))))
                 ;; Syntax requires naming NODE even if undesired if VARS
                 ;; are present, so in that case make NODE ignorable.
                 (declare (ignorable ,@(if (and vars node-p) `(,node))
                                     ,@gensyms))
                 ,@more-decls ,@forms))
             ,@(when (consp what)
                 `((setf (,(let ((*package* (sb-xc:symbol-package 'fun-info)))
                             (symbolicate "FUN-INFO-" (second what)))
                          (fun-info-or-lose ',(first what)))
                         #',name))))))))

;;;; IR groveling macros

;;; Iterate over the blocks in a component, binding BLOCK-VAR to each
;;; block in turn. The value of ENDS determines whether to iterate
;;; over dummy head and tail blocks:
;;;    NIL  -- Skip Head and Tail (the default)
;;;   :HEAD -- Do head but skip tail
;;;   :TAIL -- Do tail but skip head
;;;   :BOTH -- Do both head and tail
;;;
;;; If supplied, RESULT-FORM is the value to return.
(defmacro do-blocks ((block-var component &optional ends result) &body body)
  (unless (member ends '(nil :head :tail :both))
    (error "losing ENDS value: ~S" ends))
  (let ((n-component (sb-xc:gensym))
        (n-tail (gensym)))
    `(let* ((,n-component ,component)
            (,n-tail ,(if (member ends '(:both :tail))
                          nil
                          `(component-tail ,n-component))))
       (do ((,block-var ,(if (member ends '(:both :head))
                             `(component-head ,n-component)
                             `(block-next (component-head ,n-component)))
                        (block-next ,block-var)))
           ((eq ,block-var ,n-tail) ,result)
         ,@body))))
;;; like DO-BLOCKS, only iterating over the blocks in reverse order
(defmacro do-blocks-backwards ((block-var component &optional ends result) &body body)
  (unless (member ends '(nil :head :tail :both))
    (error "losing ENDS value: ~S" ends))
  (let ((n-component (sb-xc:gensym))
        (n-head (gensym)))
    `(let* ((,n-component ,component)
            (,n-head ,(if (member ends '(:both :head))
                          nil
                          `(component-head ,n-component))))
       (do ((,block-var ,(if (member ends '(:both :tail))
                             `(component-tail ,n-component)
                             `(block-prev (component-tail ,n-component)))
                        (block-prev ,block-var)))
           ((eq ,block-var ,n-head) ,result)
         ,@body))))

;;; Iterate over the uses of LVAR, binding NODE to each one
;;; successively.
(defmacro do-uses ((node-var lvar &optional result) &body body)
  (with-unique-names (uses)
    `(let ((,uses (lvar-uses ,lvar)))
       (block nil
         (flet ((do-1-use (,node-var)
                  ,@body))
           (if (listp ,uses)
               (dolist (node ,uses)
                 (do-1-use node))
               (do-1-use ,uses)))
         ,result))))

;;; Iterate over the nodes in BLOCK, binding NODE-VAR to the each node
;;; and LVAR-VAR to the node's LVAR. The only keyword option is
;;; RESTART-P, which causes iteration to be restarted when a node is
;;; deleted out from under us. (If not supplied, this is an error.)
;;;
;;; In the forward case, we terminate when NODE does not have NEXT, so
;;; that we do not have to worry about our termination condition being
;;; changed when new code is added during the iteration. In the
;;; backward case, we do NODE-PREV before evaluating the body so that
;;; we can keep going when the current node is deleted.
;;;
;;; When RESTART-P is supplied to DO-NODES, we start iterating over
;;; again at the beginning of the block when we run into a ctran whose
;;; block differs from the one we are trying to iterate over, either
;;; because the block was split, or because a node was deleted out
;;; from under us (hence its block is NIL.) If the block start is
;;; deleted, we just punt. With RESTART-P, we are also more careful
;;; about termination, re-indirecting the BLOCK-LAST each time.
(defmacro do-nodes ((node-var lvar-var block &key restart-p)
                    &body body)
  (with-unique-names (n-block n-start)
    `(do* ((,n-block ,block)
           (,n-start (block-start ,n-block))

           (,node-var (ctran-next ,n-start)
                      ,(if restart-p
                           `(let ((next (node-next ,node-var)))
                              (cond
                                ((not next)
                                 (return))
                                ((eq (ctran-block next) ,n-block)
                                 (ctran-next next))
                                (t
                                 (let ((start (block-start ,n-block)))
                                   (unless (eq (ctran-kind start)
                                               :block-start)
                                     (return nil))
                                   (ctran-next start)))))
                           `(acond ((node-next ,node-var)
                                    (ctran-next it))
                                   (t (return)))))
           ,@(when lvar-var
                   `((,lvar-var (when (valued-node-p ,node-var)
                                  (node-lvar ,node-var))
                                (when (valued-node-p ,node-var)
                                  (node-lvar ,node-var))))))
          (nil)
       ,@body
       ,@(when restart-p
           `((when (block-delete-p ,n-block)
               (return)))))))

;;; Like DO-NODES, only iterating in reverse order. Should be careful
;;; with block being split under us.
(defmacro do-nodes-backwards ((node-var lvar block &key restart-p) &body body)
  (let ((n-block (gensym))
        (n-prev (gensym)))
    `(loop with ,n-block = ,block
           for ,node-var = (block-last ,n-block) then
                           ,(if restart-p
                                `(if (eq ,n-block (ctran-block ,n-prev))
                                     (ctran-use ,n-prev)
                                     (block-last ,n-block))
                                `(ctran-use ,n-prev))
           for ,n-prev = (when ,node-var (node-prev ,node-var))
           and ,lvar = (when (and ,node-var (valued-node-p ,node-var))
                         (node-lvar ,node-var))
           while ,(if restart-p
                      `(and ,node-var (not (block-to-be-deleted-p ,n-block)))
                      node-var)
           do (progn
                ,@body))))

(defmacro do-nodes-carefully ((node-var block) &body body)
  (with-unique-names (n-block n-ctran)
    `(loop with ,n-block = ,block
           for ,n-ctran = (block-start ,n-block) then (node-next ,node-var)
           for ,node-var = (and ,n-ctran (ctran-next ,n-ctran))
           while ,node-var
           do (progn ,@body))))

(defmacro do-nested-cleanups ((cleanup-var lexenv &optional return-value)
                              &body body)
  `(block nil
     (map-nested-cleanups
      (lambda (,cleanup-var) ,@body) ,lexenv ,return-value)))

;;; Bind the IR1 context variables to the values associated with NODE,
;;; so that new, extra IR1 conversion related to NODE can be done
;;; after the original conversion pass has finished.
(defmacro with-ir1-environment-from-node (node &rest forms)
  `(flet ((closure-needing-ir1-environment-from-node ()
            ,@forms))
     (%with-ir1-environment-from-node
      ,node
      #'closure-needing-ir1-environment-from-node)))

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

(defmacro with-source-paths (&body forms)
  (with-unique-names (source-paths)
    `(let* ((,source-paths (make-hash-table :test 'eq))
            (*source-paths* ,source-paths))
      (unwind-protect
           (progn ,@forms)
        (clrhash ,source-paths)))))

;;; Bind the hashtables used for keeping track of global variables,
;;; functions, etc.
(defmacro with-ir1-namespace (&body forms)
  `(let ((*ir1-namespace* (make-ir1-namespace))) ,@forms))

;;; Look up NAME in the lexical environment namespace designated by
;;; SLOT, returning the <value, T>, or <NIL, NIL> if no entry. The
;;; :TEST keyword may be used to determine the name equality
;;; predicate.
(defmacro lexenv-find (name slot &key (lexenv '*lexenv*) test
                                 &aux (accessor (package-symbolicate
                                                 (cl:symbol-package 'lexenv-funs)
                                                 "LEXENV-" slot)))
  (once-only ((n-res `(assoc ,name (,accessor ,lexenv) :test ,(or test '#'eq))))
    `(if ,n-res
         (values (cdr ,n-res) t)
         (values nil nil))))

(defmacro with-component-last-block ((component block) &body body)
  (with-unique-names (old-last-block)
    (once-only ((component component)
                (block block))
      `(let ((,old-last-block (component-last-block ,component)))
         (unwind-protect
              (progn (setf (component-last-block ,component)
                           ,block)
                     ,@body)
           (setf (component-last-block ,component)
                 ,old-last-block))))))


;;;; the EVENT statistics/trace utility

;;; FIXME: This seems to be useful for troubleshooting and
;;; experimentation, not for ordinary use, so it should probably
;;; become conditional on SB-SHOW.

(defstruct (event-info (:copier nil))
  ;; The name of this event.
  (name (missing-arg) :type symbol)
  ;; The string rescribing this event.
  (description (missing-arg) :type string)
  ;; The name of the variable we stash this in.
  (var (missing-arg) :type symbol)
  ;; The number of times this event has happened.
  (count 0 :type fixnum)
  ;; The level of significance of this event.
  (level (missing-arg) :type unsigned-byte)
  ;; If true, a function that gets called with the node that the event
  ;; happened to.
  (action nil :type (or function null)))
(declaim (freeze-type event-info))

;;; A hashtable from event names to event-info structures.
(define-load-time-global *event-info* (make-hash-table :test 'eq))

;;; Return the event info for Name or die trying.
(declaim (ftype (function (t) event-info) event-info-or-lose))
(defun event-info-or-lose (name)
  (let ((res (gethash name *event-info*)))
    (unless res
      (error "~S is not the name of an event." name))
    res))

;;; Return the number of times that EVENT has happened.
(declaim (ftype (function (symbol) fixnum) event-count))
(defun event-count (name)
  (event-info-count (event-info-or-lose name)))

;;; Return the function that is called when Event happens. If this is
;;; null, there is no action. The function is passed the node to which
;;; the event happened, or NIL if there is no relevant node. This may
;;; be set with SETF.
(declaim (ftype (function (symbol) (or function null)) event-action))
(defun event-action (name)
  (event-info-action (event-info-or-lose name)))
(declaim (ftype (function (symbol (or function null)) (or function null))
                %set-event-action))
(defun %set-event-action (name new-value)
  (setf (event-info-action (event-info-or-lose name))
        new-value))
(defsetf event-action %set-event-action)

;;; Return the non-negative integer which represents the level of
;;; significance of the event Name. This is used to determine whether
;;; to print a message when the event happens. This may be set with
;;; SETF.
(declaim (ftype (function (symbol) unsigned-byte) event-level))
(defun event-level (name)
  (event-info-level (event-info-or-lose name)))
(declaim (ftype (function (symbol unsigned-byte) unsigned-byte) %set-event-level))
(defun %set-event-level (name new-value)
  (setf (event-info-level (event-info-or-lose name))
        new-value))
(defsetf event-level %set-event-level)

;;; Define a new kind of event. NAME is a symbol which names the event
;;; and DESCRIPTION is a string which describes the event. Level
;;; (default 0) is the level of significance associated with this
;;; event; it is used to determine whether to print a Note when the
;;; event happens.
(defmacro defevent (name description &optional (level 0))
  (let ((var-name (symbolicate "*" name "-EVENT-INFO*")))
    `(progn
       (defglobal ,var-name
         (make-event-info :name ',name
                          :description ',description
                          :var ',var-name
                          :level ,level))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',name *event-info*) ,var-name))
       ',name)))

;;; the lowest level of event that will print a note when it occurs
(declaim (type unsigned-byte *event-note-threshold*))
(defvar *event-note-threshold* 1)

;;; Note that the event with the specified NAME has happened. NODE is
;;; evaluated to determine the node to which the event happened.
(defmacro event (name &optional node)
  ;; Increment the counter and do any action. Mumble about the event if
  ;; policy indicates.
  `(%event ,(event-info-var (event-info-or-lose name)) ,node))

;;; Print a listing of events and their counts, sorted by the count.
;;; Events that happened fewer than Min-Count times will not be
;;; printed. Stream is the stream to write to.
(declaim (ftype (function (&optional unsigned-byte stream) (values)) event-statistics))
(defun event-statistics (&optional (min-count 1) (stream *standard-output*))
  (collect ((info))
    (maphash (lambda (k v)
               (declare (ignore k))
               (when (>= (event-info-count v) min-count)
                 (info v)))
             *event-info*)
    (dolist (event (sort (info) #'> :key #'event-info-count))
      (format stream "~6D: ~A~%" (event-info-count event)
              (event-info-description event)))
    (values))
  (values))

(declaim (ftype (function nil (values)) clear-event-statistics))
(defun clear-event-statistics ()
  (maphash (lambda (k v)
             (declare (ignore k))
             (setf (event-info-count v) 0))
           *event-info*)
  (values))

;;;; functions on directly-linked lists (linked through specialized
;;;; NEXT operations)

(declaim (inline find-in position-in))

;;; Find ELEMENT in a null-terminated LIST linked by the accessor
;;; function NEXT. KEY and TEST are the same as for generic sequence functions.
(defun find-in (next
                element
                list
                &key
                (key #'identity)
                (test #'eql))
  (declare (type function next key test))
  ;; #-sb-xc-host (declare (dynamic-extent next key test)) ; emits "unable" note
  (do ((current list (funcall next current)))
      ((null current) nil)
    (when (funcall test (funcall key current) element)
      (return current))))

;;; Return the position of ELEMENT (or NIL if absent) in a
;;; null-terminated LIST linked by the accessor function NEXT.
;;; KEY and TEST are the same as for generic sequence functions.
(defun position-in (next
                    element
                    list
                    &key
                    (key #'identity)
                    (test #'eql))
  (declare (type function next key test))
  ;; #-sb-xc-host (declare (dynamic-extent next key test)) ; emits "unable" note
  (do ((current list (funcall next current))
       (i 0 (1+ i)))
      ((null current) nil)
    (when (funcall test (funcall key current) element)
      (return i))))

(defmacro deletef-in (next place item &environment env)
  (multiple-value-bind (temps vals stores store access)
      (#-sb-xc-host get-setf-expansion #+sb-xc-host cl:get-setf-expansion place env)
    (when (cdr stores)
      (error "multiple store variables for ~S" place))
    (let ((n-item (gensym))
          (n-place (gensym))
          (n-current (gensym))
          (n-prev (gensym)))
      `(let* (,@(mapcar #'list temps vals)
              (,n-place ,access)
              (,n-item ,item))
         (if (eq ,n-place ,n-item)
             (let ((,(first stores) (,next ,n-place)))
               ,store)
             (do ((,n-prev ,n-place ,n-current)
                  (,n-current (,next ,n-place)
                              (,next ,n-current)))
                 ((eq ,n-current ,n-item)
                  (setf (,next ,n-prev)
                        (,next ,n-current)))))
         (values)))))

;;; Push ITEM onto a list linked by the accessor function NEXT that is
;;; stored in PLACE.
;;;
(defmacro push-in (next item place &environment env)
  (multiple-value-bind (temps vals stores store access)
      (#-sb-xc-host get-setf-expansion #+sb-xc-host cl:get-setf-expansion place env)
    (when (cdr stores)
      (error "multiple store variables for ~S" place))
    `(let (,@(mapcar #'list temps vals)
           (,(first stores) ,item))
       (setf (,next ,(first stores)) ,access)
       ,store
       (values))))

(defmacro position-or-lose (&rest args)
  `(or (position ,@args)
       (error "shouldn't happen?")))

;;; user-definable compiler io syntax

;;; We use WITH-SANE-IO-SYNTAX to provide safe defaults, and provide
;;; *COMPILER-PRINT-VARIABLE-ALIST* for user customization.
(defvar *compiler-print-variable-alist* nil
  "an association list describing new bindings for special variables
to be used by the compiler for error-reporting, etc. Eg.

 ((*PRINT-LENGTH* . 10) (*PRINT-LEVEL* . 6) (*PRINT-PRETTY* . NIL))

The variables in the CAR positions are bound to the values in the CDR
during the execution of some debug commands. When evaluating arbitrary
expressions in the debugger, the normal values of the printer control
variables are in effect.

Initially empty, *COMPILER-PRINT-VARIABLE-ALIST* is Typically used to
specify bindings for printer control variables.")

(defmacro with-compiler-io-syntax (&body forms)
  `(with-sane-io-syntax
    (progv
        (nreverse (mapcar #'car *compiler-print-variable-alist*))
        (nreverse (mapcar #'cdr *compiler-print-variable-alist*))
      ,@forms)))

#|
In trying to figure out whether we can rectify the totally unobvious behavior
that FUN-INFO-TRANSFORMS are tried in the reverse order of their definitions,
it is helpful to understand when multiple transforms exist where more than
one could be selected.
In the absence of type overlap, it wouldn't matter what order they were attempted.
Unfortunately there are plenty of cases where the _least_ _specific_ fun-type
should be attempted first, which at least superfically makes no sense at all.

(in-package sb-c)
;; As a special case, delete any functions where there are 2 transforms
;; and they can't possibly both apply. Just check the first arg.
(defun domains-possibly-overlap (xforms)
  (when (/= (length xforms) 2) ; lazy, just say yes
    (return-from domains-possibly-overlap t))
  (let ((t1 (transform-type (car xforms)))
        (t2 (transform-type (cadr xforms))))
    (unless (and (fun-type-p t1) (fun-type-p t2))
      (return-from domains-possibly-overlap t))
    (let ((req1 (car (fun-type-required t1)))
          (req2 (car (fun-type-required t2))))
      (unless (and req1 req2)
        (return-from domains-possibly-overlap t))
      (when (or (and (type= req1 (specifier-type 'single-float))
                     (type= req2 (specifier-type 'double-float)))
                (and (type= req1 (specifier-type 'double-float))
                     (type= req2 (specifier-type 'single-float))))
        (return-from domains-possibly-overlap nil))))
  ;; The conservative answer is always T.
  t)
(let (list)
  (do-all-symbols (s)
    (dolist (name (list s `(setf ,s) `(cas ,s)))
      (let* ((info (sb-int:info :function :info name))
             (xforms (and info (sb-c::fun-info-transforms info))))
        (when (and info
                   (> (length xforms) 1)
                   (not (assoc name list)))
          (if (domains-possibly-overlap xforms)
              (push (cons name xforms) list)
              (format t "~&Nonoverlapping xforms on ~s~%" name))))))
  (let ((*print-pretty* nil))
    (dolist (x (sort (copy-list list) #'> :key (lambda (x) (length (cdr x))))
               (format t "~s functions~%" (length list)))
      (format t "~a~%" (car x))
      (let ((i 0))
        (dolist (xform (cdr x))
          (incf i)
          (format t " ~2d. ~s~%" i (sb-kernel:type-specifier
                                  (sb-c::transform-type xform)))
          (let* ((fun (sb-c::transform-function xform))
                 (code (sb-kernel:fun-code-header fun))
                 (info (sb-kernel:%code-debug-info code))
                 (source (sb-c::compiled-debug-info-source info))
                 (tlf (sb-c::compiled-debug-info-tlf-number info))
                 (offs (sb-c::compiled-debug-info-char-offset info))
                 (ns (sb-c::debug-source-namestring source))
                 (doc (documentation (sb-c::transform-function xform) 'function)))
            (format t "     ; ~a ~a ~d ~d~%" doc ns tlf offs)))))))
|#
