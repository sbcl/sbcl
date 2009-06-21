;;;; miscellaneous types and macros used in writing the compiler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(declaim (special *wild-type* *universal-type* *compiler-error-context*))

;;; An INLINEP value describes how a function is called. The values
;;; have these meanings:
;;;     NIL     No declaration seen: do whatever you feel like, but don't
;;;             dump an inline expansion.
;;; :NOTINLINE  NOTINLINE declaration seen: always do full function call.
;;;    :INLINE  INLINE declaration seen: save expansion, expanding to it
;;;             if policy favors.
;;; :MAYBE-INLINE
;;;             Retain expansion, but only use it opportunistically.
;;;             :MAYBE-INLINE is quite different from :INLINE. As explained
;;;             by APD on #lisp 2005-11-26: "MAYBE-INLINE lambda is
;;;             instantiated once per component, INLINE - for all
;;;             references (even under #'without FUNCALL)."
(deftype inlinep () '(member :inline :maybe-inline :notinline nil))

;;;; source-hacking defining forms

;;; Parse a DEFMACRO-style lambda-list, setting things up so that a
;;; compiler error happens if the syntax is invalid.
;;;
;;; Define a function that converts a special form or other magical
;;; thing into IR1. LAMBDA-LIST is a defmacro style lambda
;;; list. START-VAR, NEXT-VAR and RESULT-VAR are bound to the start and
;;; result continuations for the resulting IR1. KIND is the function
;;; kind to associate with NAME.
(defmacro def-ir1-translator (name (lambda-list start-var next-var result-var)
                              &body body)
  (let ((fn-name (symbolicate "IR1-CONVERT-" name))
        (guard-name (symbolicate name "-GUARD")))
    (with-unique-names (whole-var n-env)
      (multiple-value-bind (body decls doc)
          (parse-defmacro lambda-list whole-var body name "special form"
                          :environment n-env
                          :error-fun 'compiler-error
                          :wrap-block nil)
        `(progn
           (declaim (ftype (function (ctran ctran (or lvar null) t) (values))
                           ,fn-name))
           (defun ,fn-name (,start-var ,next-var ,result-var ,whole-var
                            &aux (,n-env *lexenv*))
             (declare (ignorable ,start-var ,next-var ,result-var))
             ,@decls
             ,body
             (values))
           #-sb-xc-host
           ;; It's nice to do this for error checking in the target
           ;; SBCL, but it's not nice to do this when we're running in
           ;; the cross-compilation host Lisp, which owns the
           ;; SYMBOL-FUNCTION of its COMMON-LISP symbols. These guard
           ;; functions also provide the documentation for special forms.
           (progn
             (defun ,guard-name (&rest args)
               ,@(when doc (list doc))
               (declare (ignore args))
               (error 'special-form-function :name ',name))
             (let ((fun #',guard-name))
               (setf (%simple-fun-arglist fun) ',lambda-list
                     (%simple-fun-name fun) ',name
                     (symbol-function ',name) fun)
               (fmakunbound ',guard-name)))
           ;; FIXME: Evidently "there can only be one!" -- we overwrite any
           ;; other :IR1-CONVERT value. This deserves a warning, I think.
           (setf (info :function :ir1-convert ',name) #',fn-name)
           ;; FIXME: rename this to SPECIAL-OPERATOR, to update it to
           ;; the 1990s?
           (setf (info :function :kind ',name) :special-form)
           ',name)))))

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
;;; If it matters how the function is used, then DEFTRANSFORM should
;;; be used to define an IR1 transformation.
;;;
;;; If the desirability of the transformation depends on the current
;;; OPTIMIZE parameters, then the POLICY macro should be used to
;;; determine when to pass.
(defmacro source-transform-lambda (lambda-list &body body)
  (with-unique-names (whole-var n-env name)
    (multiple-value-bind (body decls)
        (parse-defmacro lambda-list whole-var body "source transform" "form"
                        :environment n-env
                        :error-fun `(lambda (&rest stuff)
                                      (declare (ignore stuff))
                                      (return-from ,name
                                        (values nil t)))
                        :wrap-block nil)
      `(lambda (,whole-var &aux (,n-env *lexenv*))
         ,@decls
         (block ,name
           ,body)))))
(defmacro define-source-transform (name lambda-list &body body)
  `(setf (info :function :source-transform ',name)
         (source-transform-lambda ,lambda-list ,@body)))

;;;; boolean attribute utilities
;;;;
;;;; We need to maintain various sets of boolean attributes for known
;;;; functions and VOPs. To save space and allow for quick set
;;;; operations, we represent the attributes as bits in a fixnum.

(deftype attributes () 'fixnum)

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)

;;; Given a list of attribute names and an alist that translates them
;;; to masks, return the OR of the masks.
(defun compute-attribute-mask (names alist)
  (collect ((res 0 logior))
    (dolist (name names)
      (let ((mask (cdr (assoc name alist))))
        (unless mask
          (error "unknown attribute name: ~S" name))
        (res mask)))
    (res)))

) ; EVAL-WHEN

;;; Define a new class of boolean attributes, with the attributes
;;; having the specified ATTRIBUTE-NAMES. NAME is the name of the
;;; class, which is used to generate some macros to manipulate sets of
;;; the attributes:
;;;
;;;    NAME-attributep attributes attribute-name*
;;;      Return true if one of the named attributes is present, false
;;;      otherwise. When set with SETF, updates the place Attributes
;;;      setting or clearing the specified attributes.
;;;
;;;    NAME-attributes attribute-name*
;;;      Return a set of the named attributes.
#-sb-xc
(progn
  (def!macro !def-boolean-attribute (name &rest attribute-names)

    (let ((translations-name (symbolicate "*" name "-ATTRIBUTE-TRANSLATIONS*"))
          (test-name (symbolicate name "-ATTRIBUTEP"))
          (decoder-name (symbolicate "DECODE-" name "-ATTRIBUTES")))
      (collect ((alist))
        (do ((mask 1 (ash mask 1))
             (names attribute-names (cdr names)))
            ((null names))
          (alist (cons (car names) mask)))
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (defparameter ,translations-name ',(alist)))
           (defmacro ,(symbolicate name "-ATTRIBUTES") (&rest attribute-names)
             "Automagically generated boolean attribute creation function.
  See !DEF-BOOLEAN-ATTRIBUTE."
             (compute-attribute-mask attribute-names ,translations-name))
           (defmacro ,test-name (attributes &rest attribute-names)
             "Automagically generated boolean attribute test function.
  See !DEF-BOOLEAN-ATTRIBUTE."
             `(logtest ,(compute-attribute-mask attribute-names
                                                ,translations-name)
                       (the attributes ,attributes)))
           ;; This definition transforms strangely under UNCROSS, in a
           ;; way that DEF!MACRO doesn't understand, so we delegate it
           ;; to a submacro then define the submacro differently when
           ;; building the xc and when building the target compiler.
           (!def-boolean-attribute-setter ,test-name
                                          ,translations-name
                                          ,@attribute-names)
           (defun ,decoder-name (attributes)
             (loop for (name . mask) in ,translations-name
                   when (logtest mask attributes)
                     collect name))))))

  ;; It seems to be difficult to express in DEF!MACRO machinery what
  ;; to do with target-vs-host GET-SETF-EXPANSION in here, so we just
  ;; hack it by hand, passing a different GET-SETF-EXPANSION-FUN-NAME
  ;; in the host DEFMACRO and target DEFMACRO-MUNDANELY cases.
  (defun guts-of-!def-boolean-attribute-setter (test-name
                                                translations-name
                                                attribute-names
                                                get-setf-expansion-fun-name)
    (declare (ignore attribute-names))
    `(define-setf-expander ,test-name (place &rest attributes
                                             &environment env)
       "Automagically generated boolean attribute setter. See
 !DEF-BOOLEAN-ATTRIBUTE."
       #-sb-xc-host (declare (type sb!c::lexenv env))
       ;; FIXME: It would be better if &ENVIRONMENT arguments were
       ;; automatically declared to have type LEXENV by the
       ;; hairy-argument-handling code.
       (multiple-value-bind (temps values stores set get)
           (,get-setf-expansion-fun-name place env)
         (when (cdr stores)
           (error "multiple store variables for ~S" place))
         (let ((newval (sb!xc:gensym))
               (n-place (sb!xc:gensym))
               (mask (compute-attribute-mask attributes ,translations-name)))
           (values `(,@temps ,n-place)
                   `(,@values ,get)
                   `(,newval)
                   `(let ((,(first stores)
                           (if ,newval
                               (logior ,n-place ,mask)
                               (logand ,n-place ,(lognot mask)))))
                      ,set
                      ,newval)
                   `(,',test-name ,n-place ,@attributes))))))
  ;; We define the host version here, and the just-like-it-but-different
  ;; target version later, after DEFMACRO-MUNDANELY has been defined.
  (defmacro !def-boolean-attribute-setter (test-name
                                           translations-name
                                           &rest attribute-names)
    (guts-of-!def-boolean-attribute-setter test-name
                                           translations-name
                                           attribute-names
                                           'get-setf-expansion)))

;;; Otherwise the source locations for DEFTRANSFORM, DEFKNOWN, &c
;;; would be off by one toplevel form as their source locations are
;;; determined before cross-compiling where the above PROGN is not
;;; seen.
#+sb-xc (progn)

;;; And now for some gratuitous pseudo-abstraction...
;;;
;;; ATTRIBUTES-UNION
;;;   Return the union of all the sets of boolean attributes which are its
;;;   arguments.
;;; ATTRIBUTES-INTERSECTION
;;;   Return the intersection of all the sets of boolean attributes which
;;;   are its arguments.
;;; ATTRIBUTES
;;;   True if the attributes present in ATTR1 are identical to
;;;   those in ATTR2.
(defmacro attributes-union (&rest attributes)
  `(the attributes
        (logior ,@(mapcar (lambda (x) `(the attributes ,x)) attributes))))
(defmacro attributes-intersection (&rest attributes)
  `(the attributes
        (logand ,@(mapcar (lambda (x) `(the attributes ,x)) attributes))))
(declaim (ftype (function (attributes attributes) boolean) attributes=))
#!-sb-fluid (declaim (inline attributes=))
(defun attributes= (attr1 attr2)
  (eql attr1 attr2))

;;;; lambda-list parsing utilities
;;;;
;;;; IR1 transforms, optimizers and type inferencers need to be able
;;;; to parse the IR1 representation of a function call using a
;;;; standard function lambda-list.

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)

;;; Given a DEFTRANSFORM-style lambda-list, generate code that parses
;;; the arguments of a combination with respect to that
;;; lambda-list. BODY is the list of forms which are to be
;;; evaluated within the bindings. ARGS is the variable that holds
;;; list of argument lvars. ERROR-FORM is a form which is evaluated
;;; when the syntax of the supplied arguments is incorrect or a
;;; non-constant argument keyword is supplied. Defaults and other gunk
;;; are ignored. The second value is a list of all the arguments
;;; bound. We make the variables IGNORABLE so that we don't have to
;;; manually declare them IGNORE if their only purpose is to make the
;;; syntax work.
(defun parse-deftransform (lambda-list body args error-form)
  (multiple-value-bind (req opt restp rest keyp keys allowp)
      (parse-lambda-list lambda-list)
    (let* ((min-args (length req))
           (max-args (+ min-args (length opt)))
           (n-keys (gensym)))
      (collect ((binds)
                (vars)
                (pos 0 +)
                (keywords))
        (dolist (arg req)
          (vars arg)
          (binds `(,arg (nth ,(pos) ,args)))
          (pos 1))

        (dolist (arg opt)
          (let ((var (if (atom arg) arg (first  arg))))
            (vars var)
            (binds `(,var (nth ,(pos) ,args)))
            (pos 1)))

        (when restp
          (vars rest)
          (binds `(,rest (nthcdr ,(pos) ,args))))

        (dolist (spec keys)
          (if (or (atom spec) (atom (first spec)))
              (let* ((var (if (atom spec) spec (first spec)))
                     (key (keywordicate var)))
                (vars var)
                (binds `(,var (find-keyword-lvar ,n-keys ,key)))
                (keywords key))
              (let* ((head (first spec))
                     (var (second head))
                     (key (first head)))
                (vars var)
                (binds `(,var (find-keyword-lvar ,n-keys ,key)))
                (keywords key))))

        (let ((n-length (gensym))
              (limited-legal (not (or restp keyp))))
          (values
           `(let ((,n-length (length ,args))
                  ,@(when keyp `((,n-keys (nthcdr ,(pos) ,args)))))
              (unless (and
                       ;; FIXME: should be PROPER-LIST-OF-LENGTH-P
                       ,(if limited-legal
                            `(<= ,min-args ,n-length ,max-args)
                            `(<= ,min-args ,n-length))
                       ,@(when keyp
                           (if allowp
                               `((check-key-args-constant ,n-keys))
                               `((check-transform-keys ,n-keys ',(keywords))))))
                ,error-form)
              (let ,(binds)
                (declare (ignorable ,@(vars)))
                ,@body))
           (vars)))))))

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
;;;   :EVAL-NAME
;;;           - The name and argument/result types are actually forms to be
;;;             evaluated. Useful for getting closures that transform similar
;;;             functions.
;;;   :DEFUN-ONLY
;;;           - Don't actually instantiate a transform, instead just DEFUN
;;;             Name with the specified transform definition function. This
;;;             may be later instantiated with %DEFTRANSFORM.
;;;   :IMPORTANT
;;;           - If supplied and non-NIL, note this transform as ``important,''
;;;             which means efficiency notes will be generated when this
;;;             transform fails even if INHIBIT-WARNINGS=SPEED (but not if
;;;             INHIBIT-WARNINGS>SPEED).
(defmacro deftransform (name (lambda-list &optional (arg-types '*)
                                          (result-type '*)
                                          &key result policy node defun-only
                                          eval-name important)
                             &body body-decls-doc)
  (when (and eval-name defun-only)
    (error "can't specify both DEFUN-ONLY and EVAL-NAME"))
  (multiple-value-bind (body decls doc) (parse-body body-decls-doc)
    (let ((n-args (sb!xc:gensym))
          (n-node (or node (sb!xc:gensym)))
          (n-decls (sb!xc:gensym))
          (n-lambda (sb!xc:gensym))
          (decls-body `(,@decls ,@body)))
      (multiple-value-bind (parsed-form vars)
          (parse-deftransform lambda-list
                              (if policy
                                  `((unless (policy ,n-node ,policy)
                                      (give-up-ir1-transform))
                                    ,@decls-body)
                                  body)
                              n-args
                              '(give-up-ir1-transform))
        (let ((stuff
               `((,n-node)
                 (let* ((,n-args (basic-combination-args ,n-node))
                        ,@(when result
                            `((,result (node-lvar ,n-node)))))
                   (multiple-value-bind (,n-lambda ,n-decls)
                       ,parsed-form
                     (if (and (consp ,n-lambda) (eq (car ,n-lambda) 'lambda))
                         ,n-lambda
                       `(lambda ,',lambda-list
                          (declare (ignorable ,@',vars))
                          ,@,n-decls
                          ,,n-lambda)))))))
          (if defun-only
              `(defun ,name ,@(when doc `(,doc)) ,@stuff)
              `(%deftransform
                ,(if eval-name name `',name)
                ,(if eval-name
                     ``(function ,,arg-types ,,result-type)
                     `'(function ,arg-types ,result-type))
                (lambda ,@stuff)
                ,doc
                ,(if important t nil))))))))

;;;; DEFKNOWN and DEFOPTIMIZER

;;; This macro should be the way that all implementation independent
;;; information about functions is made known to the compiler.
;;;
;;; FIXME: The comment above suggests that perhaps some of my added
;;; FTYPE declarations are in poor taste. Should I change my
;;; declarations, or change the comment, or what?
;;;
;;; FIXME: DEFKNOWN is needed only at build-the-system time. Figure
;;; out some way to keep it from appearing in the target system.
;;;
;;; Declare the function NAME to be a known function. We construct a
;;; type specifier for the function by wrapping (FUNCTION ...) around
;;; the ARG-TYPES and RESULT-TYPE. ATTRIBUTES is an unevaluated list
;;; of boolean attributes of the function. See their description in
;;; (!DEF-BOOLEAN-ATTRIBUTE IR1). NAME may also be a list of names, in
;;; which case the same information is given to all the names. The
;;; keywords specify the initial values for various optimizers that
;;; the function might have.
(defmacro defknown (name arg-types result-type &optional (attributes '(any))
                    &body keys)
  (when (and (intersection attributes '(any call unwind))
             (intersection attributes '(movable)))
    (error "function cannot have both good and bad attributes: ~S" attributes))

  (when (member 'any attributes)
    (setq attributes (union '(call unsafe unwind) attributes)))
  (when (member 'flushable attributes)
    (pushnew 'unsafely-flushable attributes))

  `(%defknown ',(if (and (consp name)
                         (not (legal-fun-name-p name)))
                    name
                    (list name))
              '(sfunction ,arg-types ,result-type)
              (ir1-attributes ,@attributes)
              ,@keys))

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
(defmacro defoptimizer (what (lambda-list &optional (n-node (sb!xc:gensym))
                                          &rest vars)
                             &body body)
  (let ((name (if (symbolp what) what
                  (symbolicate (first what) "-" (second what) "-OPTIMIZER"))))

    (let ((n-args (gensym)))
      `(progn
        (defun ,name (,n-node ,@vars)
          (declare (ignorable ,@vars))
          (let ((,n-args (basic-combination-args ,n-node)))
            ,(parse-deftransform lambda-list body n-args
                                 `(return-from ,name nil))))
        ,@(when (consp what)
            `((setf (,(let ((*package* (symbol-package 'sb!c::fun-info)))
                        (symbolicate "FUN-INFO-" (second what)))
                     (fun-info-or-lose ',(first what)))
                    #',name)))))))

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
  (let ((n-component (gensym))
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
  (let ((n-component (gensym))
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

;;; Bind the IR1 context variables to the values associated with NODE,
;;; so that new, extra IR1 conversion related to NODE can be done
;;; after the original conversion pass has finished.
(defmacro with-ir1-environment-from-node (node &rest forms)
  `(flet ((closure-needing-ir1-environment-from-node ()
            ,@forms))
     (%with-ir1-environment-from-node
      ,node
      #'closure-needing-ir1-environment-from-node)))
(defun %with-ir1-environment-from-node (node fun)
  (declare (type node node) (type function fun))
  (let ((*current-component* (node-component node))
        (*lexenv* (node-lexenv node))
        (*current-path* (node-source-path node)))
    (aver-live-component *current-component*)
    (funcall fun)))

;;; Bind the hashtables used for keeping track of global variables,
;;; functions, etc. Also establish condition handlers.
(defmacro with-ir1-namespace (&body forms)
  `(let ((*free-vars* (make-hash-table :test 'eq))
         (*free-funs* (make-hash-table :test 'equal))
         (*constants* (make-hash-table :test 'equal))
         (*source-paths* (make-hash-table :test 'eq)))
     (handler-bind ((compiler-error #'compiler-error-handler)
                    (style-warning #'compiler-style-warning-handler)
                    (warning #'compiler-warning-handler))
       ,@forms)))

;;; Look up NAME in the lexical environment namespace designated by
;;; SLOT, returning the <value, T>, or <NIL, NIL> if no entry. The
;;; :TEST keyword may be used to determine the name equality
;;; predicate.
(defmacro lexenv-find (name slot &key test)
  (once-only ((n-res `(assoc ,name (,(let ((*package* (symbol-package 'lexenv-funs)))
                                          (symbolicate "LEXENV-" slot))
                                     *lexenv*)
                             :test ,(or test '#'eq))))
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

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)

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

;;; A hashtable from event names to event-info structures.
(defvar *event-info* (make-hash-table :test 'eq))

;;; Return the event info for Name or die trying.
(declaim (ftype (function (t) event-info) event-info-or-lose))
(defun event-info-or-lose (name)
  (let ((res (gethash name *event-info*)))
    (unless res
      (error "~S is not the name of an event." name))
    res))

) ; EVAL-WHEN

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
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defvar ,var-name
         (make-event-info :name ',name
                          :description ',description
                          :var ',var-name
                          :level ,level))
       (setf (gethash ',name *event-info*) ,var-name)
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

#!-sb-fluid (declaim (inline find-in position-in))

;;; Find ELEMENT in a null-terminated LIST linked by the accessor
;;; function NEXT. KEY, TEST and TEST-NOT are the same as for generic
;;; sequence functions.
(defun find-in (next
                element
                list
                &key
                (key #'identity)
                (test #'eql test-p)
                (test-not #'eql not-p))
  (declare (type function next key test test-not))
  (when (and test-p not-p)
    (error "It's silly to supply both :TEST and :TEST-NOT arguments."))
  (if not-p
      (do ((current list (funcall next current)))
          ((null current) nil)
        (unless (funcall test-not (funcall key current) element)
          (return current)))
      (do ((current list (funcall next current)))
          ((null current) nil)
        (when (funcall test (funcall key current) element)
          (return current)))))

;;; Return the position of ELEMENT (or NIL if absent) in a
;;; null-terminated LIST linked by the accessor function NEXT. KEY,
;;; TEST and TEST-NOT are the same as for generic sequence functions.
(defun position-in (next
                    element
                    list
                    &key
                    (key #'identity)
                    (test #'eql test-p)
                    (test-not #'eql not-p))
  (declare (type function next key test test-not))
  (when (and test-p not-p)
    (error "It's silly to supply both :TEST and :TEST-NOT arguments."))
  (if not-p
      (do ((current list (funcall next current))
           (i 0 (1+ i)))
          ((null current) nil)
        (unless (funcall test-not (funcall key current) element)
          (return i)))
      (do ((current list (funcall next current))
           (i 0 (1+ i)))
          ((null current) nil)
        (when (funcall test (funcall key current) element)
          (return i)))))


;;; KLUDGE: This is expanded out twice, by cut-and-paste, in a
;;;   (DEF!MACRO FOO (..) .. CL:GET-SETF-EXPANSION ..)
;;;   #+SB-XC-HOST
;;;   (SB!XC:DEFMACRO FOO (..) .. SB!XC:GET-SETF-EXPANSION ..)
;;; arrangement, in order to get it to work in cross-compilation. This
;;; duplication should be removed, perhaps by rewriting the macro in a more
;;; cross-compiler-friendly way, or perhaps just by using some (MACROLET ((FROB
;;; ..)) .. FROB .. FROB) form, or perhaps by completely eliminating this macro
;;; and its partner PUSH-IN, but I don't want to do it now, because the system
;;; isn't running yet, so it'd be too hard to check that my changes were
;;; correct -- WHN 19990806
(def!macro deletef-in (next place item &environment env)
  (multiple-value-bind (temps vals stores store access)
      (get-setf-expansion place env)
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
;;; #+SB-XC-HOST SB!XC:DEFMACRO version is in late-macros.lisp. -- WHN 19990806

;;; Push ITEM onto a list linked by the accessor function NEXT that is
;;; stored in PLACE.
;;;
;;; KLUDGE: This is expanded out twice, by cut-and-paste, in a
;;;   (DEF!MACRO FOO (..) .. CL:GET-SETF-EXPANSION ..)
;;;   #+SB-XC-HOST
;;;   (SB!XC:DEFMACRO FOO (..) .. SB!XC:GET-SETF-EXPANSION ..)
;;; arrangement, in order to get it to work in cross-compilation. This
;;; duplication should be removed, perhaps by rewriting the macro in a more
;;; cross-compiler-friendly way, or perhaps just by using some (MACROLET ((FROB
;;; ..)) .. FROB .. FROB) form, or perhaps by completely eliminating this macro
;;; and its partner DELETEF-IN, but I don't want to do it now, because the
;;; system isn't running yet, so it'd be too hard to check that my changes were
;;; correct -- WHN 19990806
(def!macro push-in (next item place &environment env)
  (multiple-value-bind (temps vals stores store access)
      (get-setf-expansion place env)
    (when (cdr stores)
      (error "multiple store variables for ~S" place))
    `(let (,@(mapcar #'list temps vals)
           (,(first stores) ,item))
       (setf (,next ,(first stores)) ,access)
       ,store
       (values))))
;;; #+SB-XC-HOST SB!XC:DEFMACRO version is in late-macros.lisp. -- WHN 19990806

(defmacro position-or-lose (&rest args)
  `(or (position ,@args)
       (error "shouldn't happen?")))

;;; user-definable compiler io syntax

;;; We use WITH-SANE-IO-SYNTAX to provide safe defaults, and provide
;;; *COMPILER-PRINT-VARIABLE-ALIST* for user customization.
(defvar *compiler-print-variable-alist* nil
  #!+sb-doc
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
