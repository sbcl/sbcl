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

;;; Return a new POLICY containing the policy information represented
;;; by the optimize declaration SPEC. Any parameters not specified are
;;; defaulted from the POLICY argument.
(declaim (ftype (function (list policy) policy) process-optimize-decl))
(defun process-optimize-decl (spec policy)
  (let ((result nil))
    ;; Add new entries from SPEC.
    (dolist (q-and-v-or-just-q (cdr spec))
      (multiple-value-bind (quality raw-value)
          (if (atom q-and-v-or-just-q)
              (values q-and-v-or-just-q 3)
              (destructuring-bind (quality raw-value) q-and-v-or-just-q
                (values quality raw-value)))
        (cond ((not (policy-quality-name-p quality))
               (or (policy-quality-deprecation-warning quality)
                   (compiler-warn
                    "~@<Ignoring unknown optimization quality ~S in:~_ ~S~:>"
                    quality spec)))
              ((not (typep raw-value 'policy-quality))
               (compiler-warn "~@<Ignoring bad optimization value ~S in:~_ ~S~:>"
                              raw-value spec))
              (t
               ;; we can't do this yet, because CLOS macros expand
               ;; into code containing INHIBIT-WARNINGS.
               #+nil
               (when (eql quality 'sb!ext:inhibit-warnings)
                 (compiler-style-warn "~S is deprecated: use ~S instead"
                                      quality 'sb!ext:muffle-conditions))
               (push (cons quality raw-value)
                     result)))))
    ;; Add any nonredundant entries from old POLICY.
    (dolist (old-entry policy)
      (unless (assq (car old-entry) result)
        (push old-entry result)))
    ;; Voila.
    (sort-policy result)))

(declaim (ftype (function (list list) list)
                process-handle-conditions-decl))
(defun process-handle-conditions-decl (spec list)
  (let ((new (copy-alist list)))
    (dolist (clause (cdr spec))
      (destructuring-bind (typespec restart-name) clause
        (let ((ospec (rassoc restart-name new :test #'eq)))
          (if ospec
              (setf (car ospec)
                    (type-specifier
                     (type-union (specifier-type (car ospec))
                                 (specifier-type typespec))))
              (push (cons (type-specifier (specifier-type typespec))
                          restart-name)
                    new)))))
    new))
(declaim (ftype (function (list list) list)
                process-muffle-conditions-decl))
(defun process-muffle-conditions-decl (spec list)
  (process-handle-conditions-decl
   (cons 'handle-conditions
         (mapcar (lambda (x) (list x 'muffle-warning)) (cdr spec)))
   list))

(declaim (ftype (function (list list) list)
                process-unhandle-conditions-decl))
(defun process-unhandle-conditions-decl (spec list)
  (let ((new (copy-alist list)))
    (dolist (clause (cdr spec))
      (destructuring-bind (typespec restart-name) clause
        (let ((ospec (rassoc restart-name new :test #'eq)))
          (if ospec
              (let ((type-specifier
                     (type-specifier
                      (type-intersection
                       (specifier-type (car ospec))
                       (specifier-type `(not ,typespec))))))
                (if type-specifier
                    (setf (car ospec) type-specifier)
                    (setq new
                          (delete restart-name new :test #'eq :key #'cdr))))
              ;; do nothing?
              nil))))
    new))
(declaim (ftype (function (list list) list)
                process-unmuffle-conditions-decl))
(defun process-unmuffle-conditions-decl (spec list)
  (process-unhandle-conditions-decl
   (cons 'unhandle-conditions
         (mapcar (lambda (x) (list x 'muffle-warning)) (cdr spec)))
   list))

(declaim (ftype (function (list list) list)
                process-package-lock-decl))
(defun process-package-lock-decl (spec old)
  (let ((decl (car spec))
        (list (cdr spec)))
    (ecase decl
      (disable-package-locks
       (union old list :test #'equal))
      (enable-package-locks
       (set-difference old list :test #'equal)))))

;;; ANSI defines the declaration (FOO X Y) to be equivalent to
;;; (TYPE FOO X Y) when FOO is a type specifier. This function
;;; implements that by converting (FOO X Y) to (TYPE FOO X Y).
(defun canonized-decl-spec (decl-spec)
  (let ((id (first decl-spec)))
    (let ((id-is-type (if (symbolp id)
                          (info :type :kind id)
                          ;; A cons might not be a valid type specifier,
                          ;; but it can't be a declaration either.
                          (or (consp id)
                              (typep id 'class))))
          (id-is-declared-decl (info :declaration :recognized id)))
      ;; FIXME: Checking ID-IS-DECLARED is probably useless these days,
      ;; since we refuse to use the same symbol as both a type name and
      ;; recognized declaration name.
      (cond ((and id-is-type id-is-declared-decl)
             (compiler-error
              "ambiguous declaration ~S:~%  ~
              ~S was declared as a DECLARATION, but is also a type name."
              decl-spec id))
            (id-is-type
             (cons 'type decl-spec))
            (t
             decl-spec)))))

(defvar *queued-proclaims*) ; initialized in !COLD-INIT-FORMS

(!begin-collecting-cold-init-forms)
(!cold-init-forms (setf *queued-proclaims* nil))
(!defun-from-collected-cold-init-forms !early-proclaim-cold-init)

(defun proclaim-type (name type where-from)
  (unless (symbolp name)
    (error "Cannot proclaim TYPE of a non-symbol: ~S" name))

  (with-single-package-locked-error
      (:symbol name "globally declaring the TYPE of ~A")
    (when (eq (info :variable :where-from name) :declared)
      (let ((old-type (info :variable :type name)))
        (when (type/= type old-type)
          (type-proclamation-mismatch-warn
           name (type-specifier old-type) (type-specifier type)))))
    (setf (info :variable :type name) type
          (info :variable :where-from name) where-from)))

(defun proclaim-ftype (name type where-from)
  (unless (legal-fun-name-p name)
    (error "Cannot declare FTYPE of illegal function name ~S" name))
  (unless (csubtypep type (specifier-type 'function))
    (error "Not a function type: ~S" (type-specifier type)))

  (with-single-package-locked-error
      (:symbol name "globally declaring the FTYPE of ~A")
    (when (eq (info :function :where-from name) :declared)
      (let ((old-type (info :function :type name)))
        (cond
          ((not (type/= type old-type))) ; not changed
          ((not (info :function :info name)) ; not a known function
           (ftype-proclamation-mismatch-warn
            name (type-specifier old-type) (type-specifier type)))
          ((csubtypep type old-type)) ; tighten known function type
          (t
           (cerror "Continue"
                   'ftype-proclamation-mismatch-error
                   :name name
                   :old (type-specifier old-type)
                   :new (type-specifier type))))))
    ;; Now references to this function shouldn't be warned about as
    ;; undefined, since even if we haven't seen a definition yet, we
    ;; know one is planned.
    ;;
    ;; Other consequences of we-know-you're-a-function-now are
    ;; appropriate too, e.g. any MACRO-FUNCTION goes away.
    (proclaim-as-fun-name name)
    (note-name-defined name :function)

    ;; The actual type declaration.
    (setf (info :function :type name) type
          (info :function :where-from name) where-from)))

(defun sb!xc:proclaim (raw-form)
  #+sb-xc (/show0 "entering PROCLAIM, RAW-FORM=..")
  #+sb-xc (/hexstr raw-form)
  (destructuring-bind (&whole form &optional kind &rest args)
      (canonized-decl-spec raw-form)
    (case kind
      ((special global)
       (let ((kind/keyword (ecase kind
                             (special :special)
                             (global :global))))
         (dolist (name args)
           (unless (symbolp name)
             (error "Can't declare a non-symbol as ~(~A~): ~S" kind name))
           (let ((old (info :variable :kind name)))
             (unless (member old (list kind/keyword :unknown))
               (error "Cannot proclaim a ~(~A~) variable ~(~A~): ~S" old kind name)))
           (with-single-package-locked-error
               (:symbol name "globally declaring ~A ~(~A~)" kind)
             (setf (info :variable :kind name) kind/keyword)))))
      (always-bound
       (dolist (name args)
         (unless (symbolp name)
           (error "Can't proclaim a non-symbol as ~S: ~S" kind name))
         (unless (boundp name)
           (error "Can't proclaim an unbound symbol as ~S: ~S" kind name))
         (when (eq :constant (info :variable :kind name))
           (error "Can't proclaim a constant variable as ~S: ~S" kind name))
         (with-single-package-locked-error
             (:symbol name "globally declaring ~A always bound")
           (setf (info :variable :always-bound name) t))))
      ((type ftype)
       (if *type-system-initialized*
           (destructuring-bind (type &rest names) args
             (let ((ctype (specifier-type type)))
               (dolist (name names)
                 (ecase kind
                   (type (proclaim-type name ctype :declared))
                   (ftype (proclaim-ftype name ctype :declared))))))
           (push raw-form *queued-proclaims*)))
      (freeze-type
       (dolist (type args)
         (let ((class (specifier-type type)))
           (when (typep class 'classoid)
             (setf (classoid-state class) :sealed)
             (let ((subclasses (classoid-subclasses class)))
               (when subclasses
                 (dohash ((subclass layout) subclasses :locked t)
                   (declare (ignore layout))
                   (setf (classoid-state subclass) :sealed))))))))
      (optimize
       (setq *policy* (process-optimize-decl form *policy*)))
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
       (dolist (name args)
         ;; since implicitly it is a function, also scrubs *FREE-FUNS*
         (proclaim-as-fun-name name)
         (setf (info :function :inlinep name)
               (ecase kind
                 (inline :inline)
                 (notinline :notinline)
                 (maybe-inline :maybe-inline)))))
      (declaration
       (dolist (decl args)
         (unless (symbolp decl)
           (error "In~%  ~S~%the declaration to be recognized is not a ~
                  symbol:~%  ~S"
                  form decl))
         (with-single-package-locked-error
             (:symbol decl "globally declaring ~A as a declaration proclamation"))
         (setf (info :declaration :recognized decl) t)))
      (t
       (unless (info :declaration :recognized kind)
         (compiler-warn "unrecognized declaration ~S" raw-form)))))
  #+sb-xc (/show0 "returning from PROCLAIM")
  (values))
