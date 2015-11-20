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

(in-package "SB!C")

;;; A list of UNDEFINED-WARNING structures representing references to unknown
;;; stuff which came up in a compilation unit.
(defvar *undefined-warnings*)
(declaim (list *undefined-warnings*))

;;; Look up some symbols in *FREE-VARS*, returning the var
;;; structures for any which exist. If any of the names aren't
;;; symbols, we complain.
(declaim (ftype (function (list) list) get-old-vars))
(defun get-old-vars (names)
  (collect ((vars))
    (dolist (name names (vars))
      (unless (symbolp name)
        (compiler-error "The name ~S is not a symbol." name))
      (let ((old (gethash name *free-vars*)))
        (when old (vars old))))))

(declaim (ftype (function (list list) list)
                process-handle-conditions-decl))
(defun process-handle-conditions-decl (spec list)
  (let ((new (copy-alist list)))
    (dolist (clause (cdr spec) new)
      (destructuring-bind (typespec restart-name) clause
        (let ((type (compiler-specifier-type typespec))
              (ospec (rassoc restart-name new :test #'eq)))
          (if ospec
              (setf (car ospec) (type-union (car ospec) type))
              (push (cons type restart-name) new)))))))

(declaim (ftype (function (list list) list)
                process-muffle-conditions-decl))
(defun process-muffle-conditions-decl (spec list)
  (process-handle-conditions-decl
   `(handle-conditions ((or ,@(cdr spec)) muffle-warning))
   list))

(declaim (ftype (function (list list) list)
                process-unhandle-conditions-decl))
(defun process-unhandle-conditions-decl (spec list)
  (let ((new (copy-alist list)))
    (dolist (clause (cdr spec) new)
      (destructuring-bind (typespec restart-name) clause
        (let ((ospec (rassoc restart-name new :test #'eq)))
          (if ospec
              (let ((type (type-intersection
                           (car ospec)
                           (compiler-specifier-type `(not ,typespec)))))
                (if (type= type *empty-type*)
                    (setq new (delete restart-name new :test #'eq :key #'cdr))
                    (setf (car ospec) type)))
              ;; do nothing?
              nil))))))

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
       (union old names :test #'equal))
      (enable-package-locks
       (set-difference old names :test #'equal)))))

(!defvar *queued-proclaims* nil) ; should this be !*QUEUED-PROCLAIMS* ?

(defun process-variable-declaration (name kind info-value)
  (unless (symbolp name)
    (error "Cannot proclaim a non-symbol as ~A: ~S" kind name))

  (when (and (eq kind 'always-bound) (eq info-value :always-bound)
             (not (boundp name)))
    (error "Cannot proclaim an unbound symbol as ~A: ~S" kind name))

  (multiple-value-bind (allowed test)
      (ecase kind
        (special (values '(:special :unknown) #'eq))
        (global (values '(:global :unknown) #'eq))
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
    (error "Not a function type: ~S" (type-specifier type-oid)))
  (with-single-package-locked-error
      (:symbol name "globally declaring the FTYPE of ~A")
    (when (eq (info :function :where-from name) :declared)
      (let ((old-type (proclaimed-ftype name))
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

(defun seal-class (class)
  (declare (type classoid class))
  (setf (classoid-state class) :sealed)
  (let ((subclasses (classoid-subclasses class)))
    (when subclasses
      (dohash ((subclass layout) subclasses :locked t)
        (declare (ignore layout))
        (setf (classoid-state subclass) :sealed)))))

(defun process-freeze-type-declaration (type-specifier)
  (let ((class (specifier-type type-specifier)))
    (when (typep class 'classoid)
      (seal-class class))))

(defun process-inline-declaration (name kind)
  ;; since implicitly it is a function, also scrubs *FREE-FUNS*
  (proclaim-as-fun-name name)
  ;; Check for problems before touching globaldb,
  ;; so that the report function can see the old value.
  (let ((newval
         (ecase kind
          (inline :inline)
          (notinline :notinline)
          (maybe-inline :maybe-inline))))
    (warn-if-inline-failed/proclaim name newval)
    (setf (info :function :inlinep name) newval)))

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
    state (sb!impl::normalize-deprecation-since since)))

(defun process-deprecation-declaration (thing state software version)
  (destructuring-bind (namespace name &key replacement) thing
    (let ((info (make-deprecation-info state software version replacement)))
      (ecase namespace
        (function
         (when (eq state :final)
           (sb!impl::setup-function-in-final-deprecation
            software version name replacement))
         (setf (info :function :deprecated name) info))
        (variable
         ;; TODO (check-variable-name name "deprecated variable declaration")
         (when (eq state :final)
           (sb!impl::setup-variable-in-final-deprecation
            software version name replacement))
         (setf (info :variable :deprecated name) info))
        (type
         (when (eq state :final)
           (sb!impl::setup-type-in-final-deprecation
            software version name replacement))
         (setf (info :type :deprecated name) info))))))

(defun process-declaration-declaration (name form)
  (unless (symbolp name)
    (error "In~%  ~S~%the declaration to be recognized is not a ~
            symbol:~%  ~S"
           form name))
  (with-single-package-locked-error
      (:symbol name "globally declaring ~A as a declaration proclamation"))
  (setf (info :declaration :recognized name) t))

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
                   (remf (info :source-location :declaration name) key)))
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
             (push raw-form *queued-proclaims*)))
        (freeze-type
         (map-args #'process-freeze-type-declaration))
        (optimize
         (multiple-value-bind (new-policy specified-qualities)
             (process-optimize-decl form *policy*)
           (setq *policy* new-policy)
           (warn-repeated-optimize-qualities new-policy specified-qualities)))
        (muffle-conditions
         (setq *handled-conditions*
               (process-muffle-conditions-decl form *handled-conditions*)))
        (unmuffle-conditions
         (setq *handled-conditions*
               (process-unmuffle-conditions-decl form *handled-conditions*)))
        ((disable-package-locks enable-package-locks)
         (setq *disabled-package-locks*
               (process-package-lock-decl form *disabled-package-locks*)))
        ((inline notinline maybe-inline)
         (map-args #'process-inline-declaration kind))
        (deprecated
         (destructuring-bind (state since &rest things) args
           (multiple-value-bind (state software version)
               (check-deprecation-declaration state since form)
             (mapc (lambda (thing)
                     (destructuring-bind (namespace name &rest rest) thing
                       (declare (ignore rest))
                       (store-location
                        name :key (deprecation-location-key namespace)))
                     (process-deprecation-declaration thing state software version))
                   things))))
        (declaration
         (map-args #'process-declaration-declaration form))
        (t
         (unless (info :declaration :recognized kind)
           (compiler-warn "unrecognized declaration ~S" raw-form)))))))

(defun sb!xc:proclaim (raw-form)
  #+sb-xc (/show0 "entering PROCLAIM, RAW-FORM=..")
  #+sb-xc (/hexstr raw-form)
  (%proclaim raw-form nil)
  #+sb-xc (/show0 "returning from PROCLAIM")
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
