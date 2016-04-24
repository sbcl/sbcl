;;;; bootstrapping fundamental machinery (e.g. DEFUN, DEFCONSTANT,
;;;; DEFVAR) from special forms and primitive functions
;;;;
;;;; KLUDGE: The bootstrapping aspect of this is now obsolete. It was
;;;; originally intended that this file file would be loaded into a
;;;; Lisp image which had Common Lisp primitives defined, and DEFMACRO
;;;; defined, and little else. Since then that approach has been
;;;; dropped and this file has been modified somewhat to make it work
;;;; more cleanly when used to predefine macros at
;;;; build-the-cross-compiler time.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")


;;;; IN-PACKAGE

(sb!xc:proclaim '(special *package*))
(sb!xc:defmacro in-package (string-designator)
  (let ((string (string string-designator)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setq *package* (find-undeleted-package-or-lose ,string)))))

;;;; MULTIPLE-VALUE-FOO

(flet ((validate-vars (vars)
         (unless (and (listp vars) (every #'symbolp vars))
           (error "Vars is not a list of symbols: ~S" vars))))

(sb!xc:defmacro multiple-value-bind (vars value-form &body body)
  (validate-vars vars)
  (if (= (length vars) 1)
      ;; Not only does it look nicer to reduce to LET in this special case,
      ;; if might produce better code or at least compile quicker.
      ;; Certainly for the evaluator it's preferable.
      `(let ((,(car vars) ,value-form))
         ,@body)
      (let ((ignore (sb!xc:gensym)))
        `(multiple-value-call #'(lambda (&optional ,@(mapcar #'list vars)
                                         &rest ,ignore)
                                  (declare (ignore ,ignore))
                                  ,@body)
                              ,value-form))))

(sb!xc:defmacro multiple-value-setq (vars value-form)
  (validate-vars vars)
  ;; MULTIPLE-VALUE-SETQ is required to always return just the primary
  ;; value of the value-from, even if there are no vars. (SETF VALUES)
  ;; in turn is required to return as many values as there are
  ;; value-places, hence this:
  (if vars
      `(values (setf (values ,@vars) ,value-form))
      `(values ,value-form))))

(sb!xc:defmacro multiple-value-list (value-form)
  `(multiple-value-call #'list ,value-form))

;;;; various conditional constructs

;;; COND defined in terms of IF
(sb!xc:defmacro cond (&rest clauses)
  (if (endp clauses)
      nil
      (let ((clause (first clauses))
            (more (rest clauses)))
        (if (atom clause)
            (error 'simple-type-error
                   :format-control "COND clause is not a ~S: ~S"
                   :format-arguments (list 'cons clause)
                   :expected-type 'cons
                   :datum clause)
            (let ((test (first clause))
                  (forms (rest clause)))
              (if (endp forms)
                  (let ((n-result (gensym)))
                    `(let ((,n-result ,test))
                       (if ,n-result
                           ,n-result
                           (cond ,@more))))
                  (if (and (eq test t)
                           (not more))
                      ;; THE to preserve non-toplevelness for FOO in
                      ;;   (COND (T (FOO)))
                      `(the t (progn ,@forms))
                      `(if ,test
                           (progn ,@forms)
                           ,(when more `(cond ,@more))))))))))

(flet ((prognify (forms)
         (cond ((singleton-p forms) (car forms))
               ((not forms) nil)
               (t `(progn ,@forms)))))
  (sb!xc:defmacro when (test &body forms)
  #!+sb-doc
  "If the first argument is true, the rest of the forms are
evaluated as a PROGN."
  `(if ,test ,(prognify forms)))

  (sb!xc:defmacro unless (test &body forms)
  #!+sb-doc
  "If the first argument is not true, the rest of the forms are
evaluated as a PROGN."
  `(if ,test nil ,(prognify forms))))

(sb!xc:defmacro and (&rest forms)
  (cond ((endp forms) t)
        ((endp (rest forms))
         ;; Preserve non-toplevelness of the form!
         `(the t ,(first forms)))
        (t
         `(if ,(first forms)
              (and ,@(rest forms))
              nil))))

(sb!xc:defmacro or (&rest forms)
  (cond ((endp forms) nil)
        ((endp (rest forms))
         ;; Preserve non-toplevelness of the form!
         `(the t ,(first forms)))
        (t
         (let ((n-result (gensym)))
           `(let ((,n-result ,(first forms)))
              (if ,n-result
                  ,n-result
                  (or ,@(rest forms))))))))

;;;; various sequencing constructs

(flet ((prog-expansion-from-let (varlist body-decls let)
         (multiple-value-bind (body decls) (parse-body body-decls nil)
           `(block nil
              (,let ,varlist
                ,@decls
                (tagbody ,@body))))))
  (sb!xc:defmacro prog (varlist &body body-decls)
    (prog-expansion-from-let varlist body-decls 'let))
  (sb!xc:defmacro prog* (varlist &body body-decls)
    (prog-expansion-from-let varlist body-decls 'let*)))

(sb!xc:defmacro prog1 (result &body body)
  (let ((n-result (gensym)))
    `(let ((,n-result ,result))
       ,@body
       ,n-result)))

(sb!xc:defmacro prog2 (form1 result &body body)
  `(prog1 (progn ,form1 ,result) ,@body))

;;;; DEFUN

;;; Should we save the inline expansion of the function named NAME?
(defun inline-fun-name-p (name)
  (or
   ;; the normal reason for saving the inline expansion
   (let ((inlinep (info :function :inlinep name)))
     (member inlinep '(:inline :maybe-inline)))
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
   (info :function :inline-expansion-designator name)))

(sb!xc:defmacro defun (&environment env name lambda-list &body body)
  #!+sb-doc
  "Define a function at top level."
  #+sb-xc-host
  (unless (symbol-package (fun-name-block-name name))
    (warn "DEFUN of uninterned function name ~S (tricky for GENESIS)" name))
  (multiple-value-bind (forms decls doc) (parse-body body t)
    (let* (;; stuff shared between LAMBDA and INLINE-LAMBDA and NAMED-LAMBDA
           (lambda-guts `(,@decls (block ,(fun-name-block-name name) ,@forms)))
           (lambda `(lambda ,lambda-list ,@lambda-guts))
           (named-lambda `(named-lambda ,name ,lambda-list
                            ,@(when doc (list doc)) ,@lambda-guts))
           (inline-thing
            (or (sb!kernel::defstruct-generated-defn-p name lambda-list body)
                (when (inline-fun-name-p name)
                  ;; we want to attempt to inline, so complain if we can't
                  (acond ((sb!c:maybe-inline-syntactic-closure lambda env)
                          (list 'quote it))
                         (t
                          (#+sb-xc-host warn
                           #-sb-xc-host sb!c:maybe-compiler-notify
                           "lexical environment too hairy, can't inline DEFUN ~S"
                           name)
                          nil))))))
      `(progn
         (eval-when (:compile-toplevel)
           (sb!c:%compiler-defun ',name ,inline-thing t))
         (%defun ',name ,named-lambda (sb!c:source-location)
                 ,@(and inline-thing (list inline-thing)))
         ;; This warning, if produced, comes after the DEFUN happens.
         ;; When compiling, there's no real difference, but when interpreting,
         ;; if there is a handler for style-warning that nonlocally exits,
         ;; it's wrong to have skipped the DEFUN itself, since if there is no
         ;; function, then the warning ought not to have been issued at all.
         ,@(when (typep name '(cons (eql setf)))
             `((eval-when (:compile-toplevel :execute)
                 (sb!c::warn-if-setf-macro ',name))
               ',name))))))

#-sb-xc-host
(defun %defun (name def source-location &optional inline-lambda)
  (declare (type function def))
  ;; should've been checked by DEFMACRO DEFUN
  (aver (legal-fun-name-p name))
  (sb!c:%compiler-defun name inline-lambda nil)
  (when (fboundp name)
    (warn 'redefinition-with-defun
          :name name :new-function def :new-location source-location))
  (setf (sb!xc:fdefinition name) def)
  ;; %COMPILER-DEFUN doesn't do this except at compile-time, when it
  ;; also checks package locks. By doing this here we let (SETF
  ;; FDEFINITION) do the load-time package lock checking before
  ;; we frob any existing inline expansions.
  (sb!c::%set-inline-expansion name nil inline-lambda)
  (sb!c::note-name-defined name :function)
  name)

;;;; DEFVAR and DEFPARAMETER

(sb!xc:defmacro defvar (var &optional (val nil valp) (doc nil docp))
  #!+sb-doc
  "Define a special variable at top level. Declare the variable
  SPECIAL and, optionally, initialize it. If the variable already has a
  value, the old value is not clobbered. The third argument is an optional
  documentation string for the variable."
  `(progn
     (eval-when (:compile-toplevel)
       (%compiler-defvar ',var))
     (%defvar ',var
              (sb!c:source-location)
              ,@(and valp
                     `((unless (boundp ',var) ,val)))
              ,@(and docp
                     `(',doc)))))

(sb!xc:defmacro defparameter (var val &optional (doc nil docp))
  #!+sb-doc
  "Define a parameter that is not normally changed by the program,
  but that may be changed without causing an error. Declare the
  variable special and sets its value to VAL, overwriting any
  previous value. The third argument is an optional documentation
  string for the parameter."
  `(progn
     (eval-when (:compile-toplevel)
       (%compiler-defvar ',var))
     (%defparameter ',var ,val (sb!c:source-location)
                    ,@(and docp
                           `(',doc)))))

(defun %compiler-defvar (var)
  (sb!xc:proclaim `(special ,var)))

#-sb-xc-host
(defun %defvar (var source-location &optional (val nil valp) (doc nil docp))
  (%compiler-defvar var)
  (when (and valp
             (not (boundp var)))
    (set var val))
  (when docp
    (setf (fdocumentation var 'variable) doc))
  (when source-location
    (setf (info :source-location :variable var) source-location))
  var)

#-sb-xc-host
(defun %defparameter (var val source-location &optional (doc nil docp))
  (%compiler-defvar var)
  (set var val)
  (when docp
    (setf (fdocumentation var 'variable) doc))
  (when source-location
    (setf (info :source-location :variable var) source-location))
  var)

;;;; iteration constructs

(flet
    ((frob-do-body (varlist endlist decls-and-code bind step name block)
       ;; Check for illegal old-style DO.
       (when (or (not (listp varlist)) (atom endlist))
         (error "ill-formed ~S -- possibly illegal old style DO?" name))
       (collect ((steps))
         (let ((inits
                (mapcar (lambda (var)
                          (or (cond ((symbolp var) var)
                                    ((listp var)
                                     (unless (symbolp (first var))
                                       (error "~S step variable is not a symbol: ~S"
                                              name (first var)))
                                     (case (length var)
                                       ((1 2) var)
                                       (3 (steps (first var) (third var))
                                          (list (first var) (second var))))))
                              (error "~S is an illegal form for a ~S varlist."
                                     var name)))
                        varlist)))
           (multiple-value-bind (code decls) (parse-body decls-and-code nil)
             (let ((label-1 (sb!xc:gensym)) (label-2 (sb!xc:gensym)))
               `(block ,block
                  (,bind ,inits
                    ,@decls
                    (tagbody
                     (go ,label-2)
                     ,label-1
                     (tagbody ,@code)
                     (,step ,@(steps))
                     ,label-2
                     (unless ,(first endlist) (go ,label-1))
                     (return-from ,block (progn ,@(rest endlist))))))))))))

  ;; This is like DO, except it has no implicit NIL block.
  (sb!xc:defmacro do-anonymous (varlist endlist &rest body)
    (frob-do-body varlist endlist body 'let 'psetq 'do-anonymous (sb!xc:gensym)))

  (sb!xc:defmacro do (varlist endlist &body body)
  #!+sb-doc
  "DO ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
  Iteration construct. Each Var is initialized in parallel to the value of the
  specified Init form. On subsequent iterations, the Vars are assigned the
  value of the Step form (if any) in parallel. The Test is evaluated before
  each evaluation of the body Forms. When the Test is true, the Exit-Forms
  are evaluated as a PROGN, with the result being the value of the DO. A block
  named NIL is established around the entire expansion, allowing RETURN to be
  used as an alternate exit mechanism."
  (frob-do-body varlist endlist body 'let 'psetq 'do nil))

  (sb!xc:defmacro do* (varlist endlist &body body)
  #!+sb-doc
  "DO* ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
  Iteration construct. Each Var is initialized sequentially (like LET*) to the
  value of the specified Init form. On subsequent iterations, the Vars are
  sequentially assigned the value of the Step form (if any). The Test is
  evaluated before each evaluation of the body Forms. When the Test is true,
  the Exit-Forms are evaluated as a PROGN, with the result being the value
  of the DO. A block named NIL is established around the entire expansion,
  allowing RETURN to be used as an alternate exit mechanism."
  (frob-do-body varlist endlist body 'let* 'setq 'do* nil)))

(sb!xc:defmacro dotimes ((var count &optional (result nil)) &body body)
  ;; A nice optimization would be that if VAR is never referenced,
  ;; it's slightly more efficient to count backwards, but that's tricky.
  (let ((c (if (integerp count) count (sb!xc:gensym))))
    `(do ((,var 0 (1+ ,var))
          ,@(if (symbolp c) `((,c (the integer ,count)))))
         ((>= ,var ,c) ,result)
       (declare (type unsigned-byte ,var))
       ,@body)))

(sb!xc:defmacro dolist ((var list &optional (result nil)) &body body &environment env)
  ;; We repeatedly bind the var instead of setting it so that we never
  ;; have to give the var an arbitrary value such as NIL (which might
  ;; conflict with a declaration). If there is a result form, we
  ;; introduce a gratuitous binding of the variable to NIL without the
  ;; declarations, then evaluate the result form in that
  ;; environment. We spuriously reference the gratuitous variable,
  ;; since we don't want to use IGNORABLE on what might be a special
  ;; var.
  (multiple-value-bind (forms decls) (parse-body body nil)
    (let* ((n-list (gensym "N-LIST"))
           (start (gensym "START")))
      (multiple-value-bind (clist members clist-ok)
          (cond ((sb!xc:constantp list env)
                 (let ((value (constant-form-value list env)))
                   (multiple-value-bind (all dot) (list-members value :max-length 20)
                     (when (eql dot t)
                       ;; Full warning is too much: the user may terminate the loop
                       ;; early enough. Contents are still right, though.
                       (style-warn "Dotted list ~S in DOLIST." value))
                     (if (eql dot :maybe)
                         (values value nil nil)
                         (values value all t)))))
                ((and (consp list) (eq 'list (car list))
                      (every (lambda (arg) (sb!xc:constantp arg env)) (cdr list)))
                 (let ((values (mapcar (lambda (arg) (constant-form-value arg env)) (cdr list))))
                   (values values values t)))
                (t
                 (values nil nil nil)))
        `(block nil
           (let ((,n-list ,(if clist-ok (list 'quote clist) list)))
             (tagbody
                ,start
                (unless (endp ,n-list)
                  (let ((,var ,(if clist-ok
                                   `(truly-the (member ,@members) (car ,n-list))
                                   `(car ,n-list))))
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
                nil))))))

;;;; conditions, handlers, restarts

;;; KLUDGE: we PROCLAIM these special here so that we can use restart
;;; macros in the compiler before the DEFVARs are compiled.
;;;
;;; For an explanation of these data structures, see DEFVARs in
;;; target-error.lisp.
(sb!xc:proclaim '(special *handler-clusters* *restart-clusters*))

;;; Generated code need not check for unbound-marker in *HANDLER-CLUSTERS*
;;; (resp *RESTART-). To elicit this we must poke at the info db.
;;; SB!XC:PROCLAIM SPECIAL doesn't advise the host Lisp that *HANDLER-CLUSTERS*
;;; is special and so it rightfully complains about a SETQ of the variable.
;;; But I must SETQ if proclaming ALWAYS-BOUND because the xc asks the host
;;; whether it's currently bound.
;;; But the DEFVARs are in target-error. So it's one hack or another.
(setf (info :variable :always-bound '*handler-clusters*)
      #+sb-xc :always-bound #-sb-xc :eventually)
(setf (info :variable :always-bound '*restart-clusters*)
      #+sb-xc :always-bound #-sb-xc :eventually)

(sb!xc:defmacro with-condition-restarts
    (condition-form restarts-form &body body)
  #!+sb-doc
  "Evaluates the BODY in a dynamic environment where the restarts in the list
   RESTARTS-FORM are associated with the condition returned by CONDITION-FORM.
   This allows FIND-RESTART, etc., to recognize restarts that are not related
   to the error currently being debugged. See also RESTART-CASE."
  (once-only ((condition-form condition-form)
              (restarts restarts-form))
    (with-unique-names (restart)
      ;; FIXME: check the need for interrupt-safety.
      `(unwind-protect
           (progn
             (dolist (,restart ,restarts)
               (push ,condition-form
                     (restart-associated-conditions ,restart)))
             ,@body)
         (dolist (,restart ,restarts)
           (pop (restart-associated-conditions ,restart)))))))

(sb!xc:defmacro restart-bind (bindings &body forms)
  #!+sb-doc
  "(RESTART-BIND ({(case-name function {keyword value}*)}*) forms)
   Executes forms in a dynamic context where the given bindings are in
   effect. Users probably want to use RESTART-CASE. A case-name of NIL
   indicates an anonymous restart. When bindings contain the same
   restart name, FIND-RESTART will find the first such binding."
  (flet ((parse-binding (binding)
           (unless (>= (length binding) 2)
             (error "ill-formed restart binding: ~S" binding))
           (destructuring-bind (name function
                                &key interactive-function
                                     test-function
                                     report-function)
               binding
             (unless (or name report-function)
               (warn "Unnamed restart does not have a report function: ~
                      ~S" binding))
             `(make-restart ',name ,function
                            ,@(and (or report-function
                                       interactive-function
                                       test-function)
                                   `(,report-function))
                            ,@(and (or interactive-function
                                       test-function)
                                   `(,interactive-function))
                            ,@(and test-function
                                   `(,test-function))))))
    `(let ((*restart-clusters*
             (cons (list ,@(mapcar #'parse-binding bindings))
                   *restart-clusters*)))
       (declare (truly-dynamic-extent *restart-clusters*))
       ,@forms)))

;;; Wrap the RESTART-CASE expression in a WITH-CONDITION-RESTARTS if
;;; appropriate. Gross, but it's what the book seems to say...
(defun munge-restart-case-expression (expression env)
  (let ((exp (%macroexpand expression env)))
    (if (consp exp)
        (let* ((name (car exp))
               (args (if (eq name 'cerror) (cddr exp) (cdr exp))))
          (if (member name '(signal error cerror warn))
              (once-only ((n-cond `(coerce-to-condition
                                    ,(first args)
                                    (list ,@(rest args))
                                    ',(case name
                                        (warn 'simple-warning)
                                        (signal 'simple-condition)
                                        (t 'simple-error))
                                    ',name)))
                `(with-condition-restarts
                     ,n-cond
                     (car *restart-clusters*)
                   ,(if (eq name 'cerror)
                        `(cerror ,(second exp) ,n-cond)
                        `(,name ,n-cond))))
              expression))
        expression)))

(sb!xc:defmacro restart-case (expression &body clauses &environment env)
  #!+sb-doc
  "(RESTART-CASE form {(case-name arg-list {keyword value}* body)}*)
   The form is evaluated in a dynamic context where the clauses have
   special meanings as points to which control may be transferred (see
   INVOKE-RESTART).  When clauses contain the same case-name,
   FIND-RESTART will find the first such clause. If form is a call to
   SIGNAL, ERROR, CERROR or WARN (or macroexpands into such) then the
   signalled condition will be associated with the new restarts."
  ;; PARSE-CLAUSE (which uses PARSE-KEYWORDS-AND-BODY) is used to
  ;; parse all clauses into lists of the form
  ;;
  ;;  (NAME TAG KEYWORDS LAMBDA-LIST BODY)
  ;;
  ;; where KEYWORDS are suitable keywords for use in HANDLER-BIND
  ;; bindings. These lists are then passed to
  ;; * MAKE-BINDING which generates bindings for the respective NAME
  ;;   for HANDLER-BIND
  ;; * MAKE-APPLY-AND-RETURN which generates TAGBODY entries executing
  ;;   the respective BODY.
  (let ((block-tag (sb!xc:gensym "BLOCK"))
        (temp-var (gensym)))
    (labels ((parse-keywords-and-body (keywords-and-body)
               (do ((form keywords-and-body (cddr form))
                    (result '())) (nil)
                 (destructuring-bind (&optional key (arg nil argp) &rest rest)
                     form
                   (declare (ignore rest))
                   (setq result
                         (append
                          (cond
                            ((and (eq key :report) argp)
                             (list :report-function
                                   (if (stringp arg)
                                       `#'(lambda (stream)
                                            (write-string ,arg stream))
                                       `#',arg)))
                            ((and (eq key :interactive) argp)
                             (list :interactive-function `#',arg))
                            ((and (eq key :test) argp)
                             (list :test-function `#',arg))
                            (t
                             (return (values result form))))
                          result)))))
             (parse-clause (clause)
               (unless (and (listp clause) (>= (length clause) 2)
                            (listp (second clause)))
                 (error "ill-formed ~S clause, no lambda-list:~%  ~S"
                        'restart-case clause))
               (destructuring-bind (name lambda-list &body body) clause
                 (multiple-value-bind (keywords body)
                     (parse-keywords-and-body body)
                   (list name (sb!xc:gensym "TAG") keywords lambda-list body))))
             (make-binding (clause-data)
               (destructuring-bind (name tag keywords lambda-list body) clause-data
                 (declare (ignore body))
                 `(,name
                   (lambda ,(cond ((null lambda-list)
                                   ())
                                  ((and (null (cdr lambda-list))
                                        (not (member (car lambda-list)
                                                     '(&optional &key &aux))))
                                   '(temp))
                                  (t
                                   '(&rest temp)))
                     ,@(when lambda-list `((setq ,temp-var temp)))
                     (locally (declare (optimize (safety 0)))
                       (go ,tag)))
                   ,@keywords)))
             (make-apply-and-return (clause-data)
               (destructuring-bind (name tag keywords lambda-list body) clause-data
                 (declare (ignore name keywords))
                 `(,tag (return-from ,block-tag
                          ,(cond ((null lambda-list)
                                  `(progn ,@body))
                                 ((and (null (cdr lambda-list))
                                       (not (member (car lambda-list)
                                                    '(&optional &key &aux))))
                                  `(funcall (lambda ,lambda-list ,@body) ,temp-var))
                                 (t
                                  `(apply (lambda ,lambda-list ,@body) ,temp-var))))))))
      (let ((clauses-data (mapcar #'parse-clause clauses)))
        `(block ,block-tag
           (let ((,temp-var nil))
             (declare (ignorable ,temp-var))
             (tagbody
                (restart-bind
                    ,(mapcar #'make-binding clauses-data)
                  (return-from ,block-tag
                    ,(munge-restart-case-expression expression env)))
                ,@(mapcan #'make-apply-and-return clauses-data))))))))

(sb!xc:defmacro with-simple-restart ((restart-name format-string
                                                       &rest format-arguments)
                                         &body forms)
  #!+sb-doc
  "(WITH-SIMPLE-RESTART (restart-name format-string format-arguments)
   body)
   If restart-name is not invoked, then all values returned by forms are
   returned. If control is transferred to this restart, it immediately
   returns the values NIL and T."
  (let ((stream (sb!xc:gensym "STREAM")))
   `(restart-case
        ;; If there's just one body form, then don't use PROGN. This allows
        ;; RESTART-CASE to "see" calls to ERROR, etc.
        ,(if (= (length forms) 1) (car forms) `(progn ,@forms))
      (,restart-name ()
        :report (lambda (,stream)
                  (declare (type stream ,stream))
                  (format ,stream ,format-string ,@format-arguments))
        (values nil t)))))

(sb!xc:defmacro %handler-bind (bindings form &environment env)
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
  (collect ((local-functions) (cluster-entries) (dummy-forms))
    (flet ((const-cons (test handler)
             ;; If possible, render HANDLER as a load-time constant so that
             ;; consing the test and handler is also load-time constant.
             (let ((name (when (typep handler
                                      '(cons (member function quote)
                                             (cons symbol null)))
                           (cadr handler))))
               (cond ((or (not name)
                          (assq name (local-functions))
                          (and (eq (car handler) 'function)
                               (sb!c::fun-locally-defined-p name env)))
                      `(cons ,(case (car test)
                               ((named-lambda function) test)
                               (t `(load-time-value ,test t)))
                             ,(if (typep handler '(cons (eql function)))
                                  handler
                                  ;; Regardless of lexical policy, never allow
                                  ;; a non-callable into handler-clusters.
                                  `(let ((x ,handler))
                                     (declare (optimize (safety 3)))
                                     (the callable x)))))
                     ((info :function :info name) ; known
                      ;; This takes care of CONTINUE,ABORT,MUFFLE-WARNING.
                      ;; #' will be evaluated in the null environment.
                      `(load-time-value (cons ,test #',name) t))
                     (t
                      ;; For each handler specified as #'F we must verify
                      ;; that F is fboundp upon entering the binding scope.
                      ;; Referencing #'F is enough to ensure a warning if the
                      ;; function isn't defined at compile-time, but the
                      ;; compiler considers it elidable unless something forces
                      ;; an apparent use of the form at runtime,
                      ;; so instead use SAFE-FDEFN-FUN on the fdefn.
                      (when (eq (car handler) 'function)
                        (dummy-forms `(sb!c:safe-fdefn-fun
                                       (load-time-value
                                        (find-or-create-fdefn ',name) t))))
                      ;; Resolve to an fdefn at load-time.
                      `(load-time-value
                        (cons ,test (find-or-create-fdefn ',name))
                        t)))))

           (const-list (items)
             ;; If the resultant list is (LIST (L-T-V ...) (L-T-V ...) ...)
             ;; then pull the L-T-V outside.
             (if (every (lambda (x) (typep x '(cons (eql load-time-value))))
                        items)
                 `(load-time-value (list ,@(mapcar #'second items)) t)
                 `(list ,@items))))

      (dolist (binding bindings)
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
                   ;; Every signal is necesarily a CONDITION, so whether you
                   ;; wrote T or CONDITION, this is always an eligible handler.
                   '#'constantly-t)
                  ((typep type '(cons (eql satisfies) (cons t null)))
                   ;; (SATISFIES F) => #'F but never a local definition of F.
                   ;; The predicate is used only if needed - it's not an error
                   ;; if not fboundp (though dangerously stupid) - so just
                   ;; reference #'F for the compiler to see the use of the name.
                   ;; But (KLUDGE): since the ref is to force a compile-time
                   ;; effect, the interpreter should not see that form,
                   ;; because there is no way for it to perform an unsafe ref,
                   ;; (and it wouldn't signal a style-warning anyway),
                   ;; and so it would actually fail immediately if predicate
                   ;; were not defined.
                   (let ((name (second type)))
                     (when (typep env 'lexenv)
                       (dummy-forms `#',name))
                     `(find-or-create-fdefn ',name)))
                  ((and (symbolp type)
                        (condition-classoid-p (find-classoid type nil)))
                   ;; It's debatable whether we need to go through a
                   ;; classoid-cell instead of just using load-time-value
                   ;; on FIND-CLASS, but the extra indirection is
                   ;; safer, and no slower than what TYPEP does.
                   `(find-classoid-cell ',type :create t))
                  (t ; No runtime consing here- this is not a closure.
                   `(named-lambda (%handler-bind ,type) (c)
                      (declare (optimize (sb!c::verify-arg-count 0)))
                      (typep c ',type))))
            ;; Compute the handler expression
            (let ((lexpr (typecase handler
                          ((cons (eql lambda)) handler)
                          ((cons (eql function)
                                 (cons (cons (eql lambda)) null))
                           (cadr handler)))))
              (if lexpr
                  (let ((name (let ((sb!xc:*gensym-counter*
                                     (length (cluster-entries))))
                                (sb!xc:gensym "H"))))
                    (local-functions `(,name ,@(rest lexpr)))
                    `#',name)
                  handler))))))

      `(dx-flet ,(local-functions)
         ,@(dummy-forms)
         (dx-let ((*handler-clusters*
                   (cons ,(const-list (cluster-entries))
                         *handler-clusters*)))
           ,form)))))

(sb!xc:defmacro handler-bind (bindings &body forms)
  #!+sb-doc
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
                  #!-x86 (progn ,@forms)
                  ;; Need to catch FP errors here!
                  #!+x86 (multiple-value-prog1 (progn ,@forms) (float-wait))))

(sb!xc:defmacro handler-case (form &rest cases)
  #!+sb-doc
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
                          (with-unique-names (tag fun)
                            (destructuring-bind (type ll &body body) case
                              (push `(,fun ,ll ,@body) local-funs)
                              (list tag type ll fun))))
                        cases)))
          (with-unique-names (block cell form-fun)
            `(dx-flet ((,form-fun ()
                         #!-x86 ,form
                         ;; Need to catch FP errors here!
                         #!+x86 (multiple-value-prog1 ,form (float-wait)))
                       ,@(reverse local-funs))
               (declare (optimize (sb!c::check-tag-existence 0)))
               (block ,block
                 ;; KLUDGE: We use a dx CONS cell instead of just assigning to
                 ;; the variable directly, so that we can stack allocate
                 ;; robustly: dx value cells don't work quite right, and it is
                 ;; possible to construct user code that should loop
                 ;; indefinitely, but instead eats up some stack each time
                 ;; around.
                 (dx-let ((,cell (cons :condition nil)))
                   (declare (ignorable ,cell))
                   (tagbody
                      (%handler-bind
                       ,(mapcar (lambda (annotated-case)
                                  (destructuring-bind (tag type ll fun-name) annotated-case
                                    (declare (ignore fun-name))
                                    (list type
                                          `(lambda (temp)
                                             ,(if ll
                                                  `(setf (cdr ,cell) temp)
                                                  '(declare (ignore temp)))
                                             (go ,tag)))))
                                annotated-cases)
                       (return-from ,block (,form-fun)))
                      ,@(mapcan
                         (lambda (annotated-case)
                           (destructuring-bind (tag type ll fun-name) annotated-case
                             (declare (ignore type))
                             (list tag
                                   `(return-from ,block
                                      ,(if ll
                                           `(,fun-name (cdr ,cell))
                                           `(,fun-name))))))
                         annotated-cases))))))))))

;;;; miscellaneous

(sb!xc:defmacro return (&optional (value nil))
  `(return-from nil ,value))

(sb!xc:defmacro lambda (&whole whole args &body body)
  (declare (ignore args body))
  `#',whole)

(sb!xc:defmacro named-lambda (&whole whole name args &body body)
  (declare (ignore name args body))
  `#',whole)

(sb!xc:defmacro lambda-with-lexenv (&whole whole
                                        declarations macros symbol-macros
                                        &body body)
  (declare (ignore declarations macros symbol-macros body))
  `#',whole)
