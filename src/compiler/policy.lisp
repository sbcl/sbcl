;;;; compiler optimization policy stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; a value for an optimization declaration
(def!type policy-quality () '(integer 0 3))

(defvar *macro-policy* nil)
;;; global policy restrictions as a POLICY object or nil
(!defvar *policy-restrictions* nil)

(defun restrict-compiler-policy (&optional quality (min 0))
  #!+sb-doc
  "Assign a minimum value to an optimization quality. QUALITY is the name of
the optimization quality to restrict, and MIN (defaulting to zero) is the
minimum allowed value.

Returns the alist describing the current policy restrictions.

If QUALITY is NIL or not given, nothing is done.

Otherwise, if MIN is zero or not given, any existing restrictions of QUALITY
are removed. If MIN is between one and three inclusive, it becomes the new
minimum value for the optimization quality: any future proclamations or
declarations of the quality with a value less then MIN behave as if the value
was MIN instead.

This is intended to be used interactively, to facilitate recompiling large
bodies of code with eg. a known minimum safety.

See also :POLICY option in WITH-COMPILATION-UNIT.

EXPERIMENTAL INTERFACE: Subject to change."
  (declare (type policy-quality min))
  (when quality
    (unless (policy-quality-name-p quality)
      (error "~S is not a policy quality" quality))
    ;; The dynamic policy object is immutable, otherwise a construct like
    ;;  (let ((*policy-restrictions* *policy-restrictions*)) ...)
    ;; could allow alterations inside the LET to leak out.
    ;; The structure itself does not declare slots its read-only, because
    ;; OPTIMIZE declaration processing uses it as a scratchpad.
    (setf *policy-restrictions*
          (acond (*policy-restrictions* (copy-structure it))
                 (t (make-policy 0 0))))
    (alter-policy *policy-restrictions* (policy-quality-name-p quality)
                  min (plusp min)))
  ;; Return dotted pairs, not elements that look declaration-like.
  (if *policy-restrictions*
      (mapc (lambda (x) (rplacd x (cadr x)))
            (policy-to-decl-spec *policy-restrictions*))
      '()))

(defstruct (policy-dependent-quality (:copier nil))
  (name nil :type symbol :read-only t)
  (expression nil :read-only t)
  (getter nil :read-only t)
  (values-documentation nil :read-only t))

;;; names of recognized optimization policy qualities
(declaim (simple-vector **policy-dependent-qualities**))
(defglobal **policy-dependent-qualities** #())

;; Return POLICY as a list suitable to the OPTIMIZE declaration.
;; If FORCE-ALL then include qualities without an explicit value too.
(defun policy-to-decl-spec (policy &optional (raw t) force-all)
  (loop with presence = (policy-presence-bits policy)
        for index from (- n-policy-primary-qualities)
        below (length **policy-dependent-qualities**)
        when (or force-all (logbitp (mod index max-policy-qualities) presence))
        collect
       (list (if (minusp index)
                 (elt **policy-primary-qualities** (lognot index))
                 (policy-dependent-quality-name
                  (elt **policy-dependent-qualities** index)))
             (if raw
                 ;; Raw values are insensitive to *POLICY-RESTRICTIONS*.
                 (%%policy-quality policy index)
                 ;; Otherwise take the adjusted quality.
                 (%policy-quality policy index)))))

;; Return T if P1 and P2 are policies which are specified to be the same.
;; A result of NIL does not imply that definitely P1 /= P2
;; because a multitude of policies can be effectively equal.
;; [Any dependent quality might be specified the same as its computed
;; value in the absence of an explicit value.]
(defun policy= (p1 p2)
  (or (and p1 p2
           (= (policy-primary-qualities p1) (policy-primary-qualities p2))
           (= (policy-dependent-qualities p1) (policy-dependent-qualities p2))
           (= (policy-presence-bits p1) (policy-presence-bits p2)))
      (and (null p1) (null p2))))

;;; Is X the name of an optimization policy quality?
;;; If it is, return the integer identifier for the quality name.
(defun policy-quality-name-p (x)
  ;; Standard (and non-standard) primary qualities are numbered from -1 down.
  (or (awhen (position x **policy-primary-qualities** :test #'eq)
        (lognot it))
      ;; Dependent qualities are numbered from 0 up.
      (position x **policy-dependent-qualities**
                :key #'policy-dependent-quality-name)))

;; Destructively modify POLICY such that quality INDEX has VALUE,
;; and the specified PRESENTP bit.
(defun alter-policy (policy index value &optional (presentp t))
  (if (minusp index) ; a primary quality
      (setf (ldb (byte 2 (* 2 (lognot index)))
                 (policy-primary-qualities policy)) value)
      (setf (ldb (byte 2 (* 2 index))
                 (policy-dependent-qualities policy)) value))
  ;; Some cross-compilation hosts can't execute (SETF (LOGBITP ...)).
  (setf (ldb (byte 1 (mod index max-policy-qualities))
             (policy-presence-bits policy)) (if presentp 1 0))
  policy)

;;; Is it deprecated?
(declaim (ftype function deprecation-warn))
(defun policy-quality-deprecation-warning (quality)
  (case quality
    ((stack-allocate-dynamic-extent stack-allocate-vector stack-allocate-value-cells)
     (deprecation-warn :late "SBCL" "1.0.19.7" 'policy quality '*stack-allocate-dynamic-extent*
                       :runtime-error nil)
     t)
    ((merge-tail-calls)
     (deprecation-warn :early "SBCL" "1.0.53.74" 'policy quality nil :runtime-error nil)
     t)
    (otherwise
     nil)))

;;; *POLICY* holds the current global compiler policy information, as
;;; a POLICY object mapping from the compiler-assigned index (unique per
;;; quality name) to quality value.
;;; This used to be an alist, but tail-sharing was never really possible
;;; because for deterministic comparison the list was always freshly
;;; consed so that destructive sorting could be done for canonicalization.
(declaim (type policy *policy*)
         (type (or policy null) *policy-restrictions*))

;; ANSI-specified default of 1 for each quality.
(defglobal **baseline-policy** nil)
;; Baseline policy altered with (TYPE-CHECK 0)
(defglobal **zero-typecheck-policy** nil)
#-sb-xc-host (declaim (type policy **baseline-policy**))

;;; This is to be called early in cold init to set things up, and may
;;; also be called again later in cold init in order to reset default
;;; optimization policy back to default values after toplevel PROCLAIM
;;; OPTIMIZE forms have messed with it.
(defun !policy-cold-init-or-resanify ()
  (setq **baseline-policy**
        (make-policy (loop for i below n-policy-primary-qualities
                           sum (ash #b01 (* i 2))))
        **zero-typecheck-policy**
        (alter-policy (copy-policy **baseline-policy**)
                      #-sb-xc (policy-quality-name-p 'type-check)
                      ;; Eval in the host since cold-init won't have
                      ;; executed any forms in 'policies.lisp'
                      #+sb-xc #.(policy-quality-name-p 'type-check)
                      0))

  ;; CMU CL didn't use 1 as the default for everything,
  ;; but since ANSI says 1 is the ordinary value, we do.
  (setf *policy* (copy-policy **baseline-policy**)))

;;; Look up a named optimization quality in POLICY. This is only
;;; called by compiler code for known-valid QUALITY-NAMEs, e.g. SPEED;
;;; it's an error if it's called for a quality which isn't defined.
(defun policy-quality (policy quality-name)
  (%policy-quality policy
                   (the fixnum (policy-quality-name-p quality-name))))

(define-compiler-macro policy-quality (&whole form policy quality-name)
  (acond ((and (constantp quality-name)
               ;; CONSTANT-FORM-VALUE can not be called here when building
               ;; the cross-compiler, but EVAL can safely be used
               ;; since our own source code is known not to be screwy.
               (policy-quality-name-p (#-sb-xc-host constant-form-value
                                       #+sb-xc-host eval quality-name)))
          `(%policy-quality ,policy ,it))
         (t
          form)))

(macrolet ((extract-field (floor-expression-primary
                           floor-expression-dependent)
             `(if (minusp index)
                  (let ((byte-pos (* (lognot index) 2)))
                    (max (ldb (byte 2 byte-pos)
                              (policy-primary-qualities policy))
                         ,floor-expression-primary))
                  (let ((byte-pos (* index 2)))
                    (max (if (logbitp index (policy-presence-bits policy))
                             (ldb (byte 2 byte-pos)
                                  (policy-dependent-qualities policy))
                             1)
                         ,floor-expression-dependent))))
           (define-getter (name &body body)
             `(defun ,name (policy index)
                (declare (type policy policy)
                         (type (integer
                                #.(- n-policy-primary-qualities)
                                #.(- max-policy-qualities
                                     n-policy-primary-qualities 1))
                               index))
                ,@body)))

  ;; Return the value for quality INDEX in POLICY, using *POLICY-RESTRICTIONS*
  ;; Primary qualities are assumed to exist, however policy-restricting functions
  ;; can create a POLICY that indicates absence of primary qualities.
  ;; This does not affect RESTRICT-COMPILER-POLICY because a lower bound of 0
  ;; can be assumed for everything. SET-MACRO-POLICY might care though.
  (define-getter %policy-quality
    (let ((floor *policy-restrictions*))
      (macrolet ((quality-floor (get-byte)
                   `(if floor (ldb (byte 2 byte-pos) (,get-byte floor)) 0)))
        (extract-field (quality-floor policy-primary-qualities)
                       (quality-floor policy-dependent-qualities)))))

  ;; Return the unadjusted value for quality INDEX in POLICY.
  ;; This is used for converting a policy to a list of elements for display
  ;; and for verifying that after processing declarations, the new policy
  ;; matches the given declarations, thus implying no ambiguity.
  (define-getter %%policy-quality
    (extract-field 0 0))) ; floor is always 0

;;; Forward declaration of %COERCE-TO-POLICY.
;;; Definition is in 'node' so that FUNCTIONAL and NODE types are defined.
;;; Arg is declared of type T because the function explicitly checks it.
(declaim (ftype (function (t) (values policy &optional)) %coerce-to-policy))

;;; syntactic sugar for querying optimization policy qualities
;;;
;;; Evaluate EXPR in terms of the optimization policy associated with
;;; THING. EXPR is a form which accesses optimization qualities by
;;; referring to them by name, e.g. (> SPEED SPACE).
(defmacro policy (thing expr &optional (coercion-fn '%coerce-to-policy))
  (let* ((n-policy (make-symbol "P"))
         (binds (loop for name across **policy-primary-qualities**
                      for index downfrom -1
                      collect `(,name (%policy-quality ,n-policy ,index))))
         (dependent-binds
          (loop for info across **policy-dependent-qualities**
                for name = (policy-dependent-quality-name info)
               collect `(,name (let ((,name (policy-quality ,n-policy ',name)))
                                 (if (= ,name 1)
                                     ,(policy-dependent-quality-expression info)
                                     ,name))))))
    `(let ((,n-policy (,coercion-fn ,thing)))
       ;; FIXME: automatically inserted IGNORABLE decls are
       ;; often suggestive of poor style, as is this one.
       (declare (ignorable ,n-policy))
       (symbol-macrolet (,@binds ,@dependent-binds)
         ,expr))))

;;; Dependent qualities
(defmacro define-optimization-quality
    (name expression &optional values-documentation documentation)
  (declare (ignorable documentation))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((number (policy-quality-name-p ',name))
           (item (make-policy-dependent-quality
                  :name ',name
                  :expression ',expression
                  ;; DESCRIBE-COMPILER-POLICY uses the getter
                  :getter (named-lambda ,(string name) (policy)
                            (policy policy ,expression))
                  :values-documentation ',values-documentation)))
       (if number
           (setf (svref **policy-dependent-qualities** number) item)
           ;; This array is reallocated every time a policy is added,
           ;; but that's fine - it's not a performance issue.
           (let ((size (1+ (length **policy-dependent-qualities**))))
             ;; Don't overrun the packed bit fields.
             (when (> (+ n-policy-primary-qualities size) max-policy-qualities)
               (error "Maximum number of policy qualities exceeded."))
             (setf **policy-dependent-qualities**
                   (replace (make-array size :initial-element item)
                            **policy-dependent-qualities**)))))
     #-sb-xc-host
     ,@(when documentation `((setf (fdocumentation ',name 'optimize) ,documentation)))
     ',name))

;;; Return a new POLICY containing the policy information represented
;;; by the optimize declaration SPEC. Any parameters not specified are
;;; defaulted from the POLICY argument.
(declaim (ftype (function (list (or policy null)) (values policy list))
                process-optimize-decl))
(defun process-optimize-decl (spec policy)
  (let ((result (copy-policy (or policy **baseline-policy**)))
        (specified-qualities))
    ;; Add new entries from SPEC.
    (dolist (q-and-v-or-just-q (cdr spec) (values result specified-qualities))
      (multiple-value-bind (quality raw-value)
          (if (atom q-and-v-or-just-q)
              (values q-and-v-or-just-q 3)
            (destructuring-bind (quality raw-value) q-and-v-or-just-q
              (values quality raw-value)))
        (let ((index (policy-quality-name-p quality)))
          (cond ((not index)
                 (or (policy-quality-deprecation-warning quality)
                     (compiler-warn
                      "~@<Ignoring unknown optimization quality ~S in:~_ ~S~:>"
                      quality spec)))
                ((not (typep raw-value 'policy-quality))
                 (compiler-warn
                  "~@<Ignoring bad optimization value ~S in:~_ ~S~:>"
                  raw-value spec))
                (t
                 ;; we can't do this yet, because CLOS macros expand
                 ;; into code containing INHIBIT-WARNINGS.
                 #+nil
                 (when (eql quality 'inhibit-warnings)
                   (compiler-style-warn "~S is deprecated: use ~S instead"
                                        quality 'muffle-conditions))
                 (push (cons quality raw-value) specified-qualities)
                 (alter-policy result index raw-value))))))))

(defvar *macro-policy* nil)
;; Set an alternate policy that is used to compile all code within DEFMACRO,
;; MACROLET, DEFINE-COMPILER-MARO - whether they occur at toplevel or not -
;; as well as execute all toplevel code in eval-when situation :COMPILE-TOPLEVEL,
;; including such code as emitted into a '.cfasl' file.
;; e.g. (SET-MACRO-POLICY '((SPEED 0) (SAFETY 3))) ensures full error checking
;; regardless of prevailing local policy in situations such as
;;   (macrolet ((frob (a b) (declare (type (member :up :down) a)) ...)
;;
;; Todo: it would be nice to allow NOTINLINE, which can be broadly achieved by
;; setting (SPEED 0), but nonetheless more targeted settings should be possible.
;; Same for {UN}MUFFLE-CONDITIONS or anything else that can be proclaimed.
;;
(defun set-macro-policy (list)
  ;: Note that *MACRO-POLICY* does not represent absence of any primary quality,
  ;; and therefore whenever it is injected into a macro, you get all baseline
  ;; values of 1, augmented by the specified changes.
  ;; There are two alternative behaviors that might make sense:
  ;; - use the value of *POLICY* when SET-MACRO-POLICY is called as the baseline
  ;;   augmented by the specifiers in LIST
  ;; - use the lexical policy at the time of expansion, augmented by LIST
  ;; But most probably the current behavior is entirely reasonable.
  (setq *macro-policy* (process-optimize-decl `(optimize ,@list)
                                              **baseline-policy**)))

;; Turn the macro policy into an OPTIMIZE declaration for insertion
;; into a macro body for DEFMACRO, MACROLET, or DEFINE-COMPILER-MACRO.
;; Note that despite it being a style-warning to insert a duplicate,
;; we need no precaution against that even though users may write
;;  (DEFMACRO FOO (X) (DECLARE (OPTIMIZE (SAFETY 1))) ...)
;; The expansion of macro-defining forms is such that the macro-policy
;; appears in a different lexical scope from the user's declarations.
(defun macro-policy-decls ()
  (and *macro-policy*
       `((declare (optimize ,@(policy-to-decl-spec *macro-policy*))))))
