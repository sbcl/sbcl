;;;; SETF and friends
;;;;
;;;; Note: The expansions for SETF and friends sometimes create
;;;; needless LET-bindings of argument values. The compiler will
;;;; remove most of these spurious bindings, so SETF doesn't worry too
;;;; much about creating them.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; Return T if FUN names a DEFSTRUCT slot accessor that we should
;;; transform from SETF into %INSTANCE-SET directly - bypassing
;;; #'(SETF MYSLOT) - which requires that the slot be read/writable.
;;; A local function named (SETF MYSLOT) inhibits the transform,
;;; though technically need not, as it is unspecified how SETF
;;; of a structure slot expands. It is likewise unportable to
;;; expect that a NOTINLINE does anything, but we'll check anyway.
(defun transformable-struct-setf-p (form env)
  (when (singleton-p (cdr form))
    (let* ((fun (car form))
           (slot-info (structure-instance-accessor-p fun)))
      (when (and slot-info (not (dsd-read-only (cdr slot-info))))
        (dx-let ((setter `(setf ,fun)))
          (when (and (not (sb-c::fun-locally-defined-p setter env))
                     (not (sb-c::fun-lexically-notinline-p setter env)))
            slot-info)))))) ; caller needs the (DD . DSD) pair

;;; The inverse for a generalized-variable reference function is stored in
;;; one of two ways:
;;;
;;; A SETF inverse property corresponds to the short form of DEFSETF. It is
;;; the name of a function takes the same args as the reference form, plus a
;;; new-value arg at the end.
;;;
;;; A SETF method expander is created by the long form of DEFSETF or
;;; by DEFINE-SETF-EXPANDER. It is a function that is called on the reference
;;; form and that produces five values: a list of temporary variables, a list
;;; of value forms, a list of the single store-value form, a storing function,
;;; and an accessing function.
(declaim (ftype (function (t &optional lexenv-designator))
                get-setf-expansion))
(defun get-setf-expansion (form &optional environment)
  "Return five values needed by the SETF machinery: a list of temporary
   variables, a list of values with which to fill them, a list of temporaries
   for the new values, the setting function, and the accessing function."
  (named-let retry ((form form))
    (labels ((newvals (count)
               (let ((*gensym-counter* 1))
                 (make-gensym-list count "NEW")))
             ;; Produce the expansion of a SETF form that calls either
             ;; #'(SETF name) or an inverse given by short form DEFSETF.
             (call (call arg-maker &aux (vals (newvals 1)))
               (multiple-value-bind (temp-vars temp-vals args)
                   (collect-setf-temps (cdr form) environment nil)
                 (values temp-vars temp-vals vals
                         `(,.call ,@(funcall arg-maker (car vals) args))
                         `(,(car form) ,@args)))))
      (declare (ftype (sfunction (t) list) newvals))
      (if (atom form)
          (multiple-value-bind (expansion expanded)
              ;; Previously this called %MACROEXPAND, but the two operations
              ;; are equivalent on atoms, so do the one that is "less".
              (macroexpand-1 form environment)
            (if expanded
                (retry expansion)
                (let ((vals (newvals 1)))
                  (values nil nil vals `(setq ,form ,(car vals)) form))))
          (let ((fname (car form)))
            ;; Local functions inhibit global SETF methods.
            (unless (sb-c::fun-locally-defined-p fname environment)
              ;; There are 3 possibilities for the expander:
              ;;  #<fun>                    - define-setf-expander
              ;;  (symbol doc . source-loc) - defsetf short form
              ;;  (integer . #<fun>)        - defsetf long form
              (awhen (info :setf :expander fname)
                (return-from retry
                  (typecase it
                    (function ; DEFINE-SETF-EXPANDER
                     (funcall it form environment))
                    ((cons symbol) ; short form DEFSETF
                     (call `(,(car it)) (lambda (new args) `(,@args ,new))))
                    (t ; long form DEFSETF
                     (binding* ((newvals (newvals (car it)))
                                (expander (the function (cdr it)))
                                ((tempvars tempvals call-args)
                                 (collect-setf-temps
                                  (cdr form) environment
                                  ;; NAME-HINTS affect aesthetics only
                                  (or #+sb-xc (%fun-lambda-list expander)))))
                       (values tempvars tempvals newvals
                               (apply expander call-args environment newvals)
                               `(,fname ,@call-args)))))))
              (awhen (transformable-struct-setf-p form environment)
                (let ((instance (make-symbol "OBJ"))
                      (vals (newvals 1)))
                  (return-from retry
                    (values (list instance) (list (cadr form)) vals
                            (slot-access-transform :setf (list instance (car vals)) it)
                            (slot-access-transform :read (list instance) it))))))
            (multiple-value-bind (expansion expanded)
                (%macroexpand-1 form environment)
              (if expanded
                  (retry expansion) ; if a macro, we start over
                  (call `(funcall #'(setf ,fname)) #'cons))))))))

;; Expand PLACE until it is a form that SETF might know something about.
;; Macros are expanded only when no SETF expander (or inverse) exists.
;; Symbol-macros are always expanded because there are no SETF expanders
;; for them. This is useful mainly when a symbol-macro or ordinary macro
;; expands to a "mundane" lexical or special variable.
(defun macroexpand-for-setf (place environment)
  (loop
     (when (and (listp place) (info :setf :expander (car place)))
       (return place))
     (multiple-value-bind (expansion macro-p) (%macroexpand-1 place environment)
       (if macro-p
           (setq place expansion) ; iterate
           (return place)))))

;;;; SETF itself

;; Code shared by SETF, PSETF, SHIFTF attempting to minimize the expansion.
;; This has significant speed+space benefit to a non-preprocessing interpreter,
;; and to some degree a preprocessing interpreter.
(labels ((gen-let* (bindings body-forms)
           (cond ((not bindings) body-forms)
                 (t
                  (when (and (singleton-p body-forms)
                             (listp (car body-forms))
                             (eq (caar body-forms) 'let*))
                    (let ((nested (cdar body-forms))) ; extract the nested LET*
                      (setq bindings (append bindings (car nested))
                            body-forms (cdr nested))))
                  `((let* ,bindings ,@body-forms)))))
         (gen-mv-bind (stores values body-forms)
           (if (singleton-p stores)
               (gen-let* `((,(car stores) ,values)) body-forms)
               `((multiple-value-bind ,stores ,values ,@body-forms))))
         (forms-list (form)
           (if (and (consp form) (eq (car form) 'progn))
               (cdr form)
               (list form)))
         ;; Instead of emitting (PROGN (VALUES (SETQ ...) (SETQ ...)) NIL)
         ;; the SETQs can be lifted into the PROGN. This is unimportant
         ;; for compiled code, but it helps the interpreter not needlessly
         ;; collect arguments to call VALUES; and it's more human-readable.
         (de-values-ify (forms)
           (mapcan (lambda (form)
                     (if (and (listp form) (eq (car form) 'values))
                         (copy-list (cdr form))
                         (list form)))
                   forms)))

  (sb-xc:defmacro setf (&whole form &rest args &environment env)
  "Takes pairs of arguments like SETQ. The first is a place and the second
  is the value that is supposed to go into that place. Returns the last
  value. The place argument may be any of the access forms for which SETF
  knows a corresponding setting form."
    (unless args
      (return-from setf nil))
    (destructuring-bind (place value-form . more) args
      (when more
        (return-from setf `(progn ,@(sb-c::explode-setq form 'error))))
      (when (atom (setq place (macroexpand-for-setf place env)))
        (return-from setf `(setq ,place ,value-form)))

      (let ((fun (car place)))
        (when (and (symbolp fun)
                   ;; Local definition of FUN precludes global knowledge.
                   (not (sb-c::fun-locally-defined-p fun env)))
          (let ((inverse (info :setf :expander fun)))
            ;; NIL is not a valid setf inverse name, for two reasons:
            ;;  1. you can't define a function named NIL,
            ;;  2. (DEFSETF THING () ...) is the long form DEFSETF syntax.
            (when (typep inverse '(cons symbol))
              (return-from setf `(,(car inverse) ,@(cdr place) ,value-form))))
          (awhen (transformable-struct-setf-p place env)
            (return-from setf
              (slot-access-transform :setf (list (cadr place) value-form) it)))))

      (multiple-value-bind (temps vals newval setter)
          (get-setf-expansion place env)
        (car (gen-let* (mapcar #'list temps vals)
                       (gen-mv-bind newval value-form (forms-list setter)))))))

  ;; various SETF-related macros

  (sb-xc:defmacro shiftf (&whole form &rest args &environment env)
  "One or more SETF-style place expressions, followed by a single
   value expression. Evaluates all of the expressions in turn, then
   assigns the value of each expression to the place on its left,
   returning the value of the leftmost."
  (when (< (length args) 2)
    (error "~S called with too few arguments: ~S" 'shiftf form))
  (collect ((let*-bindings) (mv-bindings) (setters) (getters))
    (dolist (arg (butlast args))
      (multiple-value-bind (temps subforms store-vars setter getter)
          (get-setf-expansion arg env)
        (let*-bindings (mapcar #'list  temps subforms))
        (mv-bindings store-vars)
        (setters setter)
        (getters getter)))
    ;; Handle the last arg specially here. The getter is just the last
    ;; arg itself.
    (getters (car (last args)))
    (labels ((thunk (mv-bindings getters setters)
               (if mv-bindings
                   (gen-mv-bind (car mv-bindings) (car getters)
                                (thunk (cdr mv-bindings) (cdr getters) setters))
                   setters)))
      (let ((outputs (loop for i below (length (car (mv-bindings)))
                           collect (sb-xc:gensym "OUT"))))
        (car (gen-let* (reduce #'nconc (let*-bindings))
                       (gen-mv-bind outputs (car (getters))
                                    (thunk (mv-bindings) (cdr (getters))
                                           `(,@(de-values-ify (setters))
                                             (values ,@outputs))))))))))

  (labels
      ((expand (args env operator single-op)
         (cond ((singleton-p (cdr args)) ; commonest case probably
                (return-from expand `(progn (,single-op ,@args) nil)))
               ((not args)
                (return-from expand nil)))
         (collect ((let*-bindings) (mv-bindings) (setters))
           (do ((a args (cddr a)))
               ((endp a))
             (when (endp (cdr a))
               (error "Odd number of args to ~S." operator))
             (let ((place (car a))
                   (value-form (cadr a)))
               (when (and (not (symbolp place)) (eq operator 'psetq))
                 (%program-error "Place ~S in PSETQ is not a SYMBOL" place))
               (multiple-value-bind (temps vals stores setter)
                   (get-setf-expansion place env)
                 (let*-bindings (mapcar #'list temps vals))
                 (mv-bindings (cons stores value-form))
                 (setters setter))))
           (car (build (let*-bindings) (mv-bindings)
                       (de-values-ify (setters))))))
       (build (let*-bindings mv-bindings setters)
         (if let*-bindings
             (gen-let* (car let*-bindings)
                       (gen-mv-bind (caar mv-bindings) (cdar mv-bindings)
                                    (build (cdr let*-bindings) (cdr mv-bindings)
                                           setters)))
             `(,@setters nil))))

  (sb-xc:defmacro psetf (&rest pairs &environment env)
  "This is to SETF as PSETQ is to SETQ. Args are alternating place
  expressions and values to go into those places. All of the subforms and
  values are determined, left to right, and only then are the locations
  updated. Returns NIL."
    (expand pairs env 'psetf 'setf))

  (sb-xc:defmacro psetq (&rest pairs &environment env)
  "PSETQ {var value}*
   Set the variables to the values, like SETQ, except that assignments
   happen in parallel, i.e. no assignments take place until all the
   forms have been evaluated."
    (expand pairs env 'psetq 'setq))))

(sb-xc:defmacro rotatef (&rest args &environment env)
  "Takes any number of SETF-style place expressions. Evaluates all of the
   expressions in turn, then assigns to each place the value of the form to
   its right. The rightmost form gets the value of the leftmost.
   Returns NIL."
  (when args
    (collect ((let*-bindings) (mv-bindings) (setters) (getters))
      (dolist (arg args)
        (multiple-value-bind (temps subforms store-vars setter getter)
            (get-setf-expansion arg env)
          (let*-bindings (mapcar #'list temps subforms))
          (mv-bindings store-vars)
          (setters setter)
          (getters getter)))
      (setters nil)
      (getters (car (getters)))
      (labels ((thunk (mv-bindings getters)
                 (if mv-bindings
                     `((multiple-value-bind ,(car mv-bindings) ,(car getters)
                         ,@(thunk (cdr mv-bindings) (cdr getters))))
                     (setters))))
        `(let* ,(reduce #'append (let*-bindings))
           ,@(thunk (mv-bindings) (cdr (getters))))))))

(sb-xc:defmacro push (obj place &environment env)
  "Takes an object and a location holding a list. Conses the object onto
  the list, returning the modified list. OBJ is evaluated before PLACE."
  ;; If PLACE has multiple store locations, what should we do?
  ;; In other Lisp implementations:
  ;; - One errs, says "Multiple store variables not expected"
  ;; - One pushes multiple values produced by OBJ form into multiple places.
  ;; - At least two produce an incorrect expansion that doesn't even work.
  (expand-rmw-macro 'cons (list obj) place '() nil env '(item)))

(sb-xc:defmacro pushnew (obj place &rest keys &environment env)
  "Takes an object and a location holding a list. If the object is
  already in the list, does nothing; otherwise, conses the object onto
  the list. Keyword arguments are accepted as per the ADJOIN function."
  ;; Can't specify the actual keywords above since, apparently,
  ;; non-constant keywords should be accepted.
  (declare (sb-c::lambda-list (obj place &key key test test-not)))
  ;; Passing AFTER-ARGS-BINDP = NIL causes the forms subsequent to PLACE
  ;; to be inserted literally as-is, giving the (apparently) desired behavior
  ;; of *not* evaluating them before the Read/Modify/Write of PLACE, which
  ;; seems to be an exception to the 5.1.3 exception on L-to-R evaluation.
  ;; The spec only mentions that ITEM is eval'd before PLACE.
  (expand-rmw-macro 'adjoin (list obj) place keys nil env '(item)))

(sb-xc:defmacro pop (place &environment env)
  "The argument is a location holding a list. Pops one item off the front
  of the list and returns it."
  (if (symbolp (setq place (macroexpand-for-setf place env)))
      `(prog1 (car ,place) (setq ,place (cdr ,place)))
      (multiple-value-bind (temps vals stores setter getter)
          (get-setf-expansion place env)
        (let ((list (copy-symbol 'list))
              (ret (copy-symbol 'car)))
          `(let* (,@(mapcar #'list temps vals)
                  (,list ,getter)
                  (,ret (car ,list))
                  (,(car stores) (cdr ,list))
                  ,@(cdr stores))
             ,setter
             ,ret)))))

(sb-xc:defmacro remf (place indicator &environment env)
  "Place may be any place expression acceptable to SETF, and is expected
  to hold a property list or (). This list is destructively altered to
  remove the property specified by the indicator. Returns T if such a
  property was present, NIL if not."
  (multiple-value-bind (temps vals newval setter getter)
      (get-setf-expansion place env)
    (let* ((flag (make-symbol "FLAG"))
           (body `(multiple-value-bind (,(car newval) ,flag)
              ;; See ANSI 5.1.3 for why we do out-of-order evaluation
                      (truly-the (values list boolean)
                                 (%remf ,indicator ,getter))
                    ,(if (cdr newval) `(let ,(cdr newval) ,setter) setter)
                    ,flag)))
      (if temps `(let* ,(mapcar #'list temps vals) ,body) body))))

;; Perform the work of REMF.
(defun %remf (indicator plist)
  (let ((tail plist) (predecessor))
    (loop
     (when (endp tail) (return (values plist nil)))
     (let ((key (pop tail)))
       (when (atom tail)
         (error (if tail
                    "Improper list in REMF."
                    "Odd-length list in REMF.")))
       (let ((next (cdr tail)))
         (when (eq key indicator)
           ;; This function is strict in its return type!
           (the list next) ; for effect
           (return (values (cond (predecessor
                                  (setf (cdr predecessor) next)
                                  plist)
                                 (t
                                  next))
                           t)))
         (setq predecessor tail tail next))))))

;;; INCF and DECF have a straightforward expansion, avoiding temp vars,
;;; when the PLACE is a non-macro symbol. Otherwise we do the generalized
;;; SETF-like thing. The compiler doesn't care either way, but this
;;; reduces the incentive to treat some macros as special-forms when
;;; squeezing more performance from a Lisp interpreter.
;;; DEFINE-MODIFY-MACRO could be used, but this expands more compactly.
(flet ((expand (place delta env operator)
         (if (symbolp (setq place (macroexpand-for-setf place env)))
             `(setq ,place (,operator ,delta ,place))
             (multiple-value-bind (dummies vals newval setter getter)
                 (get-setf-expansion place env)
               `(let* (,@(mapcar #'list dummies vals)
                       (,(car newval) (,operator ,delta ,getter))
                       ,@(cdr newval))
                  ,setter)))))
  (sb-xc:defmacro incf (place &optional (delta 1) &environment env)
  "The first argument is some location holding a number. This number is
  incremented by the second argument, DELTA, which defaults to 1."
    (expand place delta env '+))

  (sb-xc:defmacro decf (place &optional (delta 1) &environment env)
  "The first argument is some location holding a number. This number is
  decremented by the second argument, DELTA, which defaults to 1."
    (expand place delta env 'xsubtract)))

;;;; DEFINE-MODIFY-MACRO stuff

(sb-xc:defmacro define-modify-macro (name lambda-list function &optional doc-string)
  "Creates a new read-modify-write macro like PUSH or INCF."
  (check-designator name define-modify-macro)
  (binding* (((nil required optional rest)
              (parse-lambda-list
               lambda-list
               :accept (lambda-list-keyword-mask '(&optional &rest))
               :context "a DEFINE-MODIFY-MACRO lambda list"))
             (args (append required
                           (mapcar (lambda (x) (if (listp x) (car x) x))
                                   optional)))
             (place (make-symbol "PLACE"))
             (env (make-symbol "ENV")))
    `(sb-xc:defmacro ,name (,place ,@lambda-list &environment ,env)
       ,@(when doc-string (list (the string doc-string)))
       (expand-rmw-macro ',function '() ,place
                         (list* ,@args ,(car rest)) t ,env ',args))))

;;;; DEFSETF

;;; Assign SETF macro information for NAME, making all appropriate checks.
(defun %defsetf (name expander)
  (with-single-package-locked-error
      (:symbol name "defining a setf-expander for ~A"))
  (let ((setf-fn-name `(setf ,name)))
    (multiple-value-bind (where-from present-p)
        (info :function :where-from setf-fn-name)
      ;; One might think that :DECLARED merits a style warning, but SBCL
      ;; provides ~58 standard accessors as both (SETF F) and a macro.
      ;; So allow the user to declaim an FTYPE and we'll hush up.
      ;; What's good for the the goose is good for the gander.
      (case where-from
        (:assumed
         ;; This indicates probable user error. Compilation assumed something
         ;; to be functional; a macro says otherwise. Because :where-from's
         ;; default can be :assumed, PRESENT-P disambiguates "defaulted" from
         ;; "known" to have made an existence assumption.
         (when present-p
           (warn "defining setf macro for ~S when ~S was previously ~
             treated as a function" name setf-fn-name)))
        ;; This is a useless and unavoidable warning during self-build.
        ;; cf. similar disabling of warning in WARN-IF-SETF-MACRO.
        #-sb-xc-host
        (:defined
         ;; Somebody defined (SETF F) but then also said F has a macro.
         ;; A soft warning seems appropriate because in this case it's
         ;; at least in theory not wrong to call the function.
         ;; The user can declare an FTYPE if both things are intentional.
         (style-warn "defining setf macro for ~S when ~S is also defined"
                     name setf-fn-name)))))
  (setf (info :setf :expander name) expander)
  name)

;;; This is pretty broken if there are keyword arguments (lp#1452947)
;;; but the bug seems to be due to irreconcilable problems in the spec.
;;; Everybody seems to interpret the spec the way we do though.
(sb-xc:defmacro defsetf (access-fn &rest rest)
  "Associates a SETF update function or macro with the specified access
  function or macro. The format is complex. See the manual for details."
  (check-designator access-fn defsetf "access-function")
  (typecase rest
    ((cons (and symbol (not null)) (or null (cons string null)))
     `(eval-when (:load-toplevel :compile-toplevel :execute)
        (%defsetf ',access-fn
                  (list* ',(car rest) ,(cadr rest) (sb-c:source-location)))))
    ((cons list (cons list))
     (destructuring-bind (lambda-list (&rest stores) &body body) rest
       (binding* (((llks req opt rest key aux env)
                   (parse-lambda-list
                    lambda-list
                    :accept (lambda-list-keyword-mask
                               '(&optional &rest &key &allow-other-keys
                                 &environment))
                    :context "a DEFSETF lambda list"))
                  ((forms decls doc) (parse-body body t))
                  ((outer-decls inner-decls)
                   (extract-var-decls decls (append env stores)))
                  (subforms (copy-symbol 'subforms))
                  (env-var (if env (car env) (copy-symbol 'env)))
                  (lambda-list (make-lambda-list llks nil req opt rest key)))
         (declare (ignore aux))
         `(eval-when (:compile-toplevel :load-toplevel :execute)
            (%defsetf ',access-fn
                      (cons ,(length stores)
                            (named-lambda (%defsetf ,access-fn) (,subforms ,env-var ,@stores)
                              (declare (sb-c::lambda-list ,lambda-list))
                              ,@(if doc (list doc))
                              ,@(if outer-decls (list outer-decls))
                              ,@(unless env `((declare (ignore ,env-var))))
                              (apply (lambda ,lambda-list
                                       ,@inner-decls (block ,access-fn ,@forms))
                                     ,subforms))))))))
    (t
     (error "Ill-formed DEFSETF for ~S" access-fn))))

;; Given SEXPRS which is a list of things to evaluate, return four values:
;;  - a list of uninterned symbols to bind to any non-constant sexpr
;;  - a list of things to bind those symbols to
;;  - a list parallel to SEXPRS with each non-constant element
;;    replaced by its temporary variable from the first list.
;;  - a bitmask over the sexprs containing a 1 for each non-constant.
;; Uninterned symbols are named according to the NAME-HINTS so that
;; expansions use variables resembling the DEFSETF whence they came.
;;
(defun collect-setf-temps (sexprs environment name-hints)
  (labels ((next-name-hint ()
             ;; OK if list was nil or :UNKNOWN
             (let ((sym (and (listp name-hints) (pop name-hints))))
               (case sym
                 (&optional (next-name-hint))
                 ((&key &rest) (setq name-hints nil))
                 (t (if (listp sym) (car sym) sym)))))
           (nice-tempname (form)
             (acond ((next-name-hint) (copy-symbol it))
                    (t (gensymify form)))))
    (collect ((temp-vars) (temp-vals) (call-arguments))
      (let ((mask 0) (bit 1))
        (dolist (form sexprs (values (temp-vars) (temp-vals) (call-arguments)
                                     mask))
          (call-arguments (if (constantp form environment)
                              (progn (next-name-hint) form) ; Skip one hint.
                              (let ((temp (nice-tempname form)))
                                (setq mask (logior mask bit))
                                (temp-vars temp)
                                (temp-vals form)
                                temp)))
          (setq bit (ash bit 1)))))))

;; Expand a macro defined by DEFINE-MODIFY-MACRO.
;; The generated call resembles (FUNCTION <before-args> PLACE <after-args>)
;; but the read/write of PLACE is done after all {BEFORE,AFTER}-ARG-FORMS are
;; evaluated. Subforms of PLACE are evaluated in the usual order.
;;
;; Exception: See comment at PUSHNEW for the effect of AFTER-ARGS-BINDP = NIL.
(defun expand-rmw-macro (function before-arg-forms place after-arg-forms
                         after-args-bindp environment name-hints)
     ;; Note that NAME-HINTS do the wrong thing if you have both "before" and
     ;; "after" args. In that case it is probably best to specify them as ().
     (binding* (((before-temps before-vals before-args)
                 (collect-setf-temps before-arg-forms environment name-hints))
                ((place-temps place-subforms stores setter getter)
                 (get-setf-expansion place environment))
                ((after-temps after-vals after-args)
                 (if after-args-bindp
                     (collect-setf-temps after-arg-forms environment name-hints)
                     (values nil nil after-arg-forms)))
                (compute `(,function ,@before-args ,getter ,@after-args))
                (set-fn (and (listp setter) (car setter)))
                (newval-temp (car stores))
                (newval-binding `((,newval-temp ,compute))))
       ;; Elide the binding of NEWVAL-TEMP if it is ref'd exactly once
       ;; and all the call arguments are temporaries and/or constants.
       (when (and (= (count newval-temp setter) 1)
                  (or (eq set-fn 'setq)
                      (and (eq (info :function :kind set-fn) :function)
                           (every (lambda (x)
                                    (or (member x place-temps)
                                        (eq x newval-temp)
                                        (constantp x environment)))
                                  (cdr setter)))))
         (setq newval-binding nil
               setter (substitute compute newval-temp setter)))
       (let ((bindings
              (flet ((zip (list1 list2) (mapcar #'list list1 list2)))
                (append (zip before-temps before-vals)
                        (zip place-temps place-subforms)
                        (zip after-temps after-vals)
                        newval-binding
                        (cdr stores)))))
         (if bindings `(let* ,bindings ,setter) setter))))

;;;; DEFMACRO DEFINE-SETF-EXPANDER and various DEFINE-SETF-EXPANDERs

;;; DEFINE-SETF-EXPANDER is a lot like DEFMACRO.
(sb-xc:defmacro define-setf-expander (access-fn lambda-list &body body)
  "Syntax like DEFMACRO, but creates a setf expander function. The body
  of the definition must be a form that returns five appropriate values."
  (check-designator access-fn define-setf-expander "access-function")
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%defsetf ',access-fn
               ,(make-macro-lambda `(setf-expander ,access-fn) lambda-list body
                                   'define-setf-expander access-fn
                                   :doc-string-allowed :internal))))
