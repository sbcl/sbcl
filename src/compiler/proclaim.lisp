;;;; This file contains load-time support for declaration processing.
;;;; In CMU CL it was split off from the compiler so that the compiler
;;;; doesn't have to be in the cold load, but in SBCL the compiler is
;;;; in the cold load again, so this might not be valuable.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; A list of UNDEFINED-WARNING structures representing references to unknown
;;; stuff which came up in a compilation unit.
(defvar *undefined-warnings*)
(defvar *argument-mismatch-warnings*)
(declaim (list *undefined-warnings* *argument-mismatch-warnings*))

;;; Delete any undefined warnings for NAME and KIND. This is for the
;;; benefit of the compiler, but it's sometimes called from stuff like
;;; type-defining code which isn't logically part of the compiler.
(declaim (ftype (function ((or symbol cons) keyword) (values))
                note-name-defined))
(defun note-name-defined (name kind)
  #-sb-xc-host (atomic-incf sb-kernel::*type-cache-nonce*)
  ;; We do this BOUNDP check because this function can be called when
  ;; not in a compilation unit (as when loading top level forms).
  (when (boundp '*undefined-warnings*)
    (let ((name (uncross name)))
      (setq *undefined-warnings*
            (delete-if (lambda (x)
                         (and (equal (undefined-warning-name x) name)
                              (eq (undefined-warning-kind x) kind)))
                       *undefined-warnings*))))
  (values))

(defun check-variable-name (name &key
                                   (context "local variable")
                                   (signal-via #'compiler-error))
  (unless (legal-variable-name-p name)
    (funcall signal-via "~@<~S~[~; is a keyword and~; is not a symbol and~
                         ~] cannot be used as a ~A.~@:>"
             name
             (typecase name
               (null    0)
               (keyword 1)
               (t       2))
             context))
  name)

;;; Check that NAME is a valid function name, returning the name if
;;; OK, and signalling an error if not. In addition to checking for
;;; basic well-formedness, we also check that symbol names are not NIL
;;; or the name of a special form.
(defun check-fun-name (name)
  (typecase name
    (list
     (unless (legal-fun-name-p name)
       (compiler-error "~@<Illegal function name: ~S.~@:>" name)))
    (symbol
     (when (eq (info :function :kind name) :special-form)
       (compiler-error "~@<Special form is an illegal function name: ~S.~@:>"
                       name)))
    (t
     (compiler-error "~@<Illegal function name: ~S.~@:>" name)))
  name)

;;; Check that NAME is a valid class name, returning the name if OK,
;;; and signalling an error if not.
(declaim (inline sb-pcl::check-class-name))
(defun sb-pcl::check-class-name (name &optional (allow-nil t))
  ;; Apparently, FIND-CLASS and (SETF FIND-CLASS) accept any symbol,
  ;; but DEFCLASS only accepts non-NIL symbols.
  (if (or (not (legal-class-name-p name))
          (and (null name) (not allow-nil)))
      (error 'illegal-class-name-error :name name)
      name))

;;; This is called to do something about SETF functions that overlap
;;; with SETF macros. Perhaps we should interact with the user to see
;;; whether the macro should be blown away, but for now just give a
;;; warning. Due to the weak semantics of the (SETF FUNCTION) name, we
;;; can't assume that they aren't just naming a function (SETF FOO)
;;; for the heck of it. NAME is already known to be well-formed.
(defun warn-if-setf-macro (name)
  ;; Never warn about this situation when running the cross-compiler.
  ;; SBCL provides expanders/inverses *and* functions for most SETFable things
  ;; even when CLHS does not specifically state that #'(SETF x) exists.
  #+sb-xc-host (declare (ignore name))
  #-sb-xc-host
  (let ((stem (second name)))
    (when (info :setf :expander stem)
      (compiler-style-warn
         "defining function ~S when ~S already has a SETF macro"
         name stem)))
  (values))

;;; Record a new function definition, and check its legality.
(defun proclaim-as-fun-name (name)

  ;; legal name?
  (check-fun-name name)

  ;; KLUDGE: This can happen when eg. compiling a NAMED-LAMBDA, and isn't
  ;; guarded against elsewhere -- so we want to assert package locks here. The
  ;; reason we do it only when stomping on existing stuff is because we want
  ;; to keep
  ;;   (WITHOUT-PACKAGE-LOCKS (DEFUN LOCKED:FOO ...))
  ;; viable, which requires no compile-time violations in the harmless cases.
  (with-single-package-locked-error ()
    (flet ((assert-it ()
             (assert-symbol-home-package-unlocked name "proclaiming ~S as a function")))

      (let ((kind (info :function :kind name)))
        ;; scrubbing old data I: possible collision with a macro
        ;; There's a silly little problem with fun names that are not ANSI-legal names,
        ;; e.g. (CAS mumble). We can't ask the host whether that is FBOUNDP,
        ;; because it would rightly complain. So, just assume that it is not FBOUNDP.
        (when (and #+sb-xc-host (symbolp name)
                   (fboundp name)
                   (eq :macro kind))
          (assert-it)
          (compiler-style-warn "~S was previously defined as a macro." name)
          (setf (info :function :where-from name) :assumed)
          (clear-info :function :macro-function name))

        (unless (eq :function kind)
          (assert-it)
          ;; There's no reason to store (:FUNCTION :KIND) for names which
          ;; could only be of kind :FUNCTION if anything.
          (unless (pcl-methodfn-name-p name)
            (setf (info :function :kind name) :function))))))

  ;; scrubbing old data II: dangling forward references
  ;;
  ;; (This could happen if someone executes PROCLAIM FTYPE at
  ;; macroexpansion time, which is bad style, or at compile time, e.g.
  ;; in EVAL-WHEN (:COMPILE) inside something like DEFSTRUCT, in which
  ;; case it's reasonable style. Either way, NAME is no longer a free
  ;; function.)
  (when (boundp '*ir1-namespace*)       ; when compiling
    (unless (block-compile *compilation*)
      (remhash name (free-funs *ir1-namespace*))))

  (values))

;;; Make NAME no longer be a function name: clear everything back to
;;; the default.
(defun undefine-fun-name (name)
  (when name
    (macrolet ((frob (&rest types)
                 `(clear-info-values
                   name ',(mapcar (lambda (x)
                                    (meta-info-number (meta-info :function x)))
                                  types))))
      ;; Note that this does not clear the :DEFINITION.
      ;; That's correct, because if we lose the association between a
      ;; symbol and its #<fdefn> object, it could lead to creation of
      ;; a non-unique #<fdefn> for a name.
      (frob :info
            :type ; Hmm. What if it was proclaimed- shouldn't it stay?
            :where-from ; Ditto.
            :inlinep
            :kind
            :macro-function
            :inlining-data
            :source-transform
            :assumed-type)))
  (values))

;;; part of what happens with DEFUN, also with some PCL stuff: Make
;;; NAME known to be a function definition.
(defun become-defined-fun-name (name)
  (proclaim-as-fun-name name)
  (when (eq (info :function :where-from name) :assumed)
    (setf (info :function :where-from name) :defined)
    (if (info :function :assumed-type name)
        (clear-info :function :assumed-type name))))

;;; to be called when a variable is lexically bound
(declaim (ftype (function (symbol) (values)) note-lexical-binding))
(defun note-lexical-binding (symbol)
    ;; This check is intended to protect us from getting silently
    ;; burned when we define
    ;;   foo.lisp:
    ;;     (DEFVAR *FOO* -3)
    ;;     (DEFUN FOO (X) (+ X *FOO*))
    ;;   bar.lisp:
    ;;     (DEFUN BAR (X)
    ;;       (LET ((*FOO* X))
    ;;         (FOO 14)))
    ;; and then we happen to compile bar.lisp before foo.lisp.
  (when (looks-like-name-of-special-var-p symbol)
    ;; FIXME: should be COMPILER-STYLE-WARNING?
    (style-warn 'asterisks-around-lexical-variable-name
                :format-control
                "using the lexical binding of the symbol ~
                 ~/sb-ext:print-symbol-with-prefix/, not the~@
                 dynamic binding"
                :format-arguments (list symbol)))
  (values))

;;; In the target compiler, a lexenv can hold an alist of condition
;;; types (CTYPE . ACTION) such that when signaling condition CTYPE,
;;; we perform ACTION which is usually MUFFLE-CONDITION.
;;; Each CTYPE is a (parsed) CONDITION subtype, which is slightly
;;; more efficient than holding the mapping keys as s-expressions
;;; (type specifier).  However, the parsed representation is worse
;;; for in the cross-compiler, actually downright disastrous. Why?
;;; Because to process an entry in the list, we invert the parsed type
;;; back to a sexpr, and then inquire of the host via its CL:TYPEP
;;; whether a condition instance is of that type. (We use host
;;; condition objects). So why parse and unparse? Not only is that
;;; dumb, it's broken. For example, to invert #<classoid CODE-DELETION-NOTE>,
;;; you must already have seen a target definition of that type.
;;; But you haven't necessarily! If you haven't, then there is no
;;; CONDITION-CLASSOID for that, there is only an UNKNOWN-TYPE.
;;; And then you have to signal a PARSE-UNKNOWN-TYPE, and then you must
;;; ask how to handle _that_ condition (the PARSE-UNKNOWN-TYPE)
;;; signaled while trying to signal some other condition. What a mess.
(declaim (ftype (function (list list) list)
                process-handle-conditions-decl))
(defun process-handle-conditions-decl (spec list)
  (let ((new (copy-alist list)))
    (dolist (clause (cdr spec) new)
      (destructuring-bind (typespec restart-name) clause
        (let ((ospec (rassoc restart-name new :test #'eq)))
          #+sb-xc-host
          (if ospec
              (setf (car ospec) `(or ,typespec ,(car ospec)))
              (push (cons typespec restart-name) new))
          #-sb-xc-host
          (let ((type (compiler-specifier-type typespec)))
            (cond ((not type))
                  (ospec
                   (setf (car ospec) (type-union (car ospec) type)))
                  (t
                   (push (cons type restart-name) new)))))))))

(declaim (ftype (function (list list) list)
                process-muffle-conditions-decl))
(defun process-muffle-conditions-decl (expr list)
  (let ((spec (cond ((not expr) nil)
                    ((singleton-p (cdr expr)) (cadr expr))
                    (t `(or ,@(cdr expr))))))
    (process-handle-conditions-decl `(handle-conditions (,spec muffle-warning))
                                    list)))

(declaim (ftype (function (list list) list)
                process-unhandle-conditions-decl))
(defun process-unhandle-conditions-decl (spec list)
  (let ((new (copy-alist list)))
    (dolist (clause (cdr spec) new)
      (block nil
       (destructuring-bind (typespec restart-name) clause
         (let ((ospec (rassoc restart-name new :test #'eq)))
           (when ospec
             (let ((type (type-intersection
                          (car ospec)
                          (or (compiler-specifier-type `(not ,typespec))
                              (return)))))
               (if (type= type *empty-type*)
                   (setq new (delete restart-name new :test #'eq :key #'cdr))
                   (setf (car ospec) type))))))))))

(declaim (ftype (function (list list) list)
                process-unmuffle-conditions-decl))
(defun process-unmuffle-conditions-decl (spec list)
  (process-unhandle-conditions-decl
   `(unhandle-conditions ((or ,@(cdr spec)) muffle-warning))
   list))

(declaim (ftype (function (list list) list)
                process-package-lock-decl))
(defun process-package-lock-decl (spec old)
  (destructuring-bind (decl &rest names) spec
    (ecase decl
      (disable-package-locks
       ;; Why are we using EQUAL here if the only way to disable the
       ;; lock on (SETF CAR) is to list the name CAR and not (SETF CAR)?
       (union old names :test #'equal))
      (enable-package-locks
       (set-difference old names :test #'equal)))))

(defvar *queued-proclaims* nil)

(defun process-variable-declaration (name kind info-value)
  (unless (symbolp name)
    (error "Cannot proclaim a non-symbol as ~A: ~S" kind name))

  (when (and (eq kind 'always-bound) (eq info-value :always-bound)
             (not (boundp name))
             ;; Allow it to be unbound at compile-time.
             (not *compile-time-eval*)
             ;; Check if we are still bootstrapping.
             (not (boundp '*queued-proclaims*)))
    (error "Cannot proclaim an unbound symbol as ~A: ~S" kind name))

  (multiple-value-bind (allowed test)
      (ecase kind
        (special
         ;; KLUDGE: There is probably a better place to do this.
         (when (boundp '*ir1-namespace*)
           (remhash name (free-vars *ir1-namespace*)))
         (values '(:special :unknown) #'eq))
        (global
         ;; KLUDGE: Ditto.
         (when (boundp '*ir1-namespace*)
           (remhash name (free-vars *ir1-namespace*)))
         (values '(:global :unknown) #'eq))
        (always-bound (values '(:constant) #'neq)))
    (let ((old (info :variable :kind name)))
      (unless (member old allowed :test test)
        (error "Cannot proclaim a ~A variable ~A: ~S" old kind name))))

  (with-single-package-locked-error
      (:symbol name "globally declaring ~A ~A" kind)
    (if (eq kind 'always-bound)
        (setf (info :variable :always-bound name) info-value)
        (setf (info :variable :kind name) info-value))))

(defun type-proclamation-mismatch-warn (name old new &optional description)
  (warn 'type-proclamation-mismatch-warning
        :name name :old old :new new :description description))

(defun proclaim-type (name type type-specifier where-from)
  (unless (symbolp name)
    (error "Cannot proclaim TYPE of a non-symbol: ~S" name))

  (with-single-package-locked-error
      (:symbol name "globally declaring the TYPE of ~A")
    (when (eq (info :variable :where-from name) :declared)
      (let ((old-type (info :variable :type name)))
        (when (type/= type old-type)
          (type-proclamation-mismatch-warn
           name (type-specifier old-type) type-specifier))))
    (setf (info :variable :type name) type
          (info :variable :where-from name) where-from)))

(defun ftype-proclamation-mismatch-warn (name old new &optional description)
  (warn 'ftype-proclamation-mismatch-warning
        :name name :old old :new new :description description))

(defun proclaim-ftype (name type-oid type-specifier where-from)
  (declare (type (or ctype defstruct-description) type-oid))
  (unless (legal-fun-name-p name)
    (error "Cannot declare FTYPE of illegal function name ~S" name))
  (when (and (ctype-p type-oid)
             (not (csubtypep type-oid (specifier-type 'function))))
    (error "Not a function type: ~/sb-impl:print-type/" type-oid))
  (with-single-package-locked-error
      (:symbol name "globally declaring the FTYPE of ~A")
    (when (eq (info :function :where-from name) :declared)
      (let ((old-type (global-ftype name))
            (type (if (ctype-p type-oid)
                      type-oid
                      (specifier-type type-specifier))))
        (cond
          ((not (type/= type old-type))) ; not changed
          ((not (info :function :info name)) ; not a known function
           (ftype-proclamation-mismatch-warn
            name (type-specifier old-type) type-specifier))
          ((csubtypep type old-type)) ; tighten known function type
          (t
           (cerror "Continue"
                   'ftype-proclamation-mismatch-error
                   :name name
                   :old (type-specifier old-type)
                   :new type-specifier)))))
    ;; Now references to this function shouldn't be warned about as
    ;; undefined, since even if we haven't seen a definition yet, we
    ;; know one is planned.
    ;;
    ;; Other consequences of we-know-you're-a-function-now are
    ;; appropriate too, e.g. any MACRO-FUNCTION goes away.
    (proclaim-as-fun-name name)
    (note-name-defined name :function)

    ;; The actual type declaration.
    (setf (info :function :type name) type-oid
          (info :function :where-from name) where-from)))

(defun seal-class (classoid)
  (declare (type classoid classoid))
  (setf (classoid-state classoid) :sealed)
  (sb-kernel::do-subclassoids ((subclassoid wrapper) classoid)
    (declare (ignore wrapper))
    (setf (classoid-state subclassoid) :sealed)))

(defun process-freeze-type-declaration (type-specifier)
  (let ((class (specifier-type type-specifier)))
    (when (typep class 'classoid)
      (seal-class class))))

(defun process-inline-declaration (name kind)
  (declare (type (and inlinep (not null)) kind))
  ;; since implicitly it is a function, also scrubs (FREE-FUNS *IR1-NAMESPACE*)
  (proclaim-as-fun-name name)
  (warn-if-inline-failed/proclaim name kind)
  (setf (info :function :inlinep name) kind))

(defun process-block-compile-declaration (entries kind)
  (ecase kind
    (start-block
     (finish-block-compilation)
     (let ((compilation *compilation*))
       (setf (block-compile compilation) t)
       (setf (entry-points compilation) entries)))
    (end-block
     (finish-block-compilation))))

(defun check-deprecation-declaration (state since form)
  (unless (typep state 'deprecation-state)
    (error 'simple-type-error
           :datum            state
           :expected-type    'deprecation-state
           :format-control   "~@<In declaration ~S, ~S state is not a ~
                              valid deprecation state. Expected one ~
                              of ~{~S~^, ~}.~@:>"
           :format-arguments (list form state
                                   (rest (typexpand 'deprecation-state)))))
  (multiple-value-call #'values
    state (sb-impl::normalize-deprecation-since since)))

(defun process-deprecation-declaration (thing state software version)
  (destructuring-bind (namespace name &key replacement) thing
    (let ((info (make-deprecation-info state software version replacement)))
      (ecase namespace
        (function
         (when (eq state :final)
           (sb-impl::setup-function-in-final-deprecation
            software version name replacement))
         (setf (info :function :deprecated name) info))
        (variable
         (check-variable-name
          name :context "deprecated variable declaration" :signal-via #'error)
         (when (eq state :final)
           (sb-impl::setup-variable-in-final-deprecation
            software version name replacement))
         (setf (info :variable :deprecated name) info))
        (type
         (when (eq state :final)
           (sb-impl::setup-type-in-final-deprecation
            software version name replacement))
         (setf (info :type :deprecated name) info))))))

(defun process-declaration-declaration (name form)
  (unless (symbolp name)
    (error "In~%  ~S~%the declaration to be recognized is not a ~
            symbol:~%  ~S"
           form name))
  (with-single-package-locked-error
      (:symbol name "globally declaring ~A as a declaration proclamation"))
  (setf (info :declaration :known name) t))

;;; ANSI defines the declaration (FOO X Y) to be equivalent to
;;; (TYPE FOO X Y) when FOO is a type specifier. This function
;;; implements that by converting (FOO X Y) to (TYPE FOO X Y).
(defun canonized-decl-spec (decl-spec)
  (let ((id (first decl-spec)))
    (if (cond  ((symbolp id) (info :type :kind id))
               ((listp id)
                (let ((id (car id)))
                  (and (symbolp id)
                       (or (info :type :expander id)
                           (info :type :kind id)))))
               (t
                ;; FIXME: should be (TYPEP id '(OR CLASS CLASSOID))
                ;; but that references CLASS too soon.
                ;; See related hack in DEF!TYPE TYPE-SPECIFIER.
                (typep id 'instance)))
        (cons 'type decl-spec)
        decl-spec)))

;; These return values are intended for EQ-comparison in
;; STORE-LOCATION in %PROCLAIM.
(defun deprecation-location-key (namespace)
  (case namespace
    (function '(deprecated function))
    (variable '(deprecated variable))
    (type     '(deprecated type))))

(defun %proclaim (raw-form location)
  (destructuring-bind (&whole form &optional kind &rest args)
      (canonized-decl-spec raw-form)
    (labels ((store-location (name &key (key kind))
               (if location
                   (setf (getf (info :source-location :declaration name) key)
                         location)
                   ;; Without this WHEN, globaldb would accumulate
                   ;; a bunch of explicitly stored empty lists because
                   ;; it does not know that there's no need to store NIL.
                   (when (info :source-location :declaration name)
                     (remf (info :source-location :declaration name) key))))
             (map-names (names function &rest extra-args)
               (mapc (lambda (name)
                       (store-location name)
                       (apply function name extra-args))
                     names))
             (map-args (function &rest extra-args)
               (apply #'map-names args function extra-args)))
      (case kind
        ((special global always-bound)
         (map-args #'process-variable-declaration kind
                   (case kind
                     (special :special)
                     (global :global)
                     (always-bound :always-bound))))
        ((type ftype)
         (if *type-system-initialized*
             (destructuring-bind (type &rest names) args
               (check-deprecated-type type)
               (let ((ctype (specifier-type type)))
                 (map-names names (ecase kind
                                    (type #'proclaim-type)
                                    (ftype #'proclaim-ftype))
                            ctype type :declared)))
             #-sb-xc-host
             (push raw-form *queued-proclaims*)
             #+sb-xc-host
             (error "Type system not yet initialized.")))
        (freeze-type
         #-sb-fluid
         (map-args #'process-freeze-type-declaration))
        ((start-block end-block)
         #-(and sb-devel sb-xc-host)
         (when (and *compile-time-eval* (boundp '*compilation*))
           (if (eq *block-compile-argument* :specified)
               (process-block-compile-declaration args kind)
               (compiler-notify "ignoring ~S declaration since ~
                                :BLOCK-COMPILE is not :SPECIFIED"
                                kind))))
        (optimize
         (multiple-value-bind (new-policy specified-qualities)
             (process-optimize-decl form *policy*)
           (when (and (boundp '*compilation*)
                      ;; Should I also examine *COMPILE-TIME-EVAL* here? I don't think so.
                      (listp (saved-optimize-decls *compilation*)))
             (push form (saved-optimize-decls *compilation*)))
           (setq *policy* new-policy)
           (warn-repeated-optimize-qualities new-policy specified-qualities)))
        (muffle-conditions
         (setq *handled-conditions*
               (process-muffle-conditions-decl form *handled-conditions*)))
        (unmuffle-conditions
         ;; When cross-compiling, we're won't perform type algebra on the sexpr
         ;; representation. There is no need for this kind of ridiculous spec:
         ;;   (and (or this that) (not that)).
         #+sb-xc-host (bug "UNMUFFLE: not implemented")
         #-sb-xc-host
         (setq *handled-conditions*
               (process-unmuffle-conditions-decl form *handled-conditions*)))
        ((disable-package-locks enable-package-locks)
         (setq *disabled-package-locks*
               (process-package-lock-decl form *disabled-package-locks*)))
        ((#-sb-fluid inline notinline #-sb-fluid maybe-inline)
         (map-args #'process-inline-declaration kind))
        (deprecated
         (destructuring-bind (state since &rest things) args
           (multiple-value-bind (state software version)
               (check-deprecation-declaration state since form)
             (mapc (lambda (thing)
                     (process-deprecation-declaration thing state software version)
                     (destructuring-bind (namespace name &rest rest) thing
                       (declare (ignore rest))
                       (store-location
                        name :key (deprecation-location-key namespace))))
                   things))))
        (declaration
         (map-args #'process-declaration-declaration form))
        (t
         (unless (info :declaration :known kind)
           (compiler-warn "unrecognized declaration ~S" raw-form)))))))

(defun proclaim (raw-form)
  (/noshow "PROCLAIM" raw-form)
  (%proclaim raw-form nil)
  (values))

;;; Note that the type NAME has been (re)defined, updating the
;;; undefined warnings and VALUES-SPECIFIER-TYPE cache.
(defun %note-type-defined (name)
  (declare (symbol name))
  (note-name-defined name :type)
  (values-specifier-type-cache-clear)
  (values))

;; Issue a style warning if there are any repeated OPTIMIZE declarations
;; given the SPECIFIED-QUALITIES, unless there is no ambiguity.
(defun warn-repeated-optimize-qualities (new-policy specified-qualities)
  (let (dups)
    (dolist (quality-and-value specified-qualities)
      (let* ((quality (car quality-and-value))
             (current ; Read the raw quality value, not the adjusted value.
              (%%policy-quality new-policy (policy-quality-name-p quality))))
        (when (and (not (eql (cdr quality-and-value) current))
                   (not (assq quality dups)))
          (push `(,quality ,current) dups))))
    (when dups
      ;; If a restriction is in force, this message can be misleading,
      ;; as the "effective" value isn't always what the message claims.
      (compiler-style-warn "Repeated OPTIMIZE qualit~@P. Using ~{~S~^ and ~}"
                           (length dups) dups))))
