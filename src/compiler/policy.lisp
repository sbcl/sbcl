;;;; compiler optimization policy stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; a value for an optimization declaration
(deftype policy-quality () '(integer 0 3))

(defconstant-eqx +policy-primary-qualities+
        #(;; ANSI standard qualities
          compilation-speed
          debug
          safety
          space
          speed
          ;; SBCL extensions
          ;;
          ;; FIXME: INHIBIT-WARNINGS is a misleading name for this.
          ;; Perhaps BREVITY would be better. But the ideal name would
          ;; have connotations of suppressing not warnings but only
          ;; optimization-related notes, which is already mostly the
          ;; behavior, and should probably become the exact behavior.
          ;; Perhaps INHIBIT-NOTES?
          inhibit-warnings)
    #'equalp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant n-policy-primary-qualities (length +policy-primary-qualities+))
  ;; 1 bit per quality is stored to indicate whether it was explicitly given
  ;; a value in a lexical policy. In addition to the 5 ANSI-standard qualities,
  ;; SBCL defines one more "primary" quality and 16 dependent qualities.
  ;; Both kinds take up 1 bit in the mask of specified qualities.
  (defconstant max-policy-qualities 32))

;; Each primary and dependent quality policy is assigned a small integer index.
;; The POLICY struct represents a set of policies in an order-insensitive way
;; that facilitates quicker lookup than scanning an alist.
(defstruct (policy (:constructor make-policy
                       (primary-qualities &optional
                                          presence-bits dependent-qualities)))
  ;; Mask with a 1 for each quality that has an explicit value in this policy.
  ;; Primary qualities fill the mask from left-to-right and dependent qualities
  ;; from right-to-left.
  ;; xc has trouble folding this MASK-FIELD, but it works when host-evaluated.
  (presence-bits #.(mask-field
                    (byte n-policy-primary-qualities
                          (- max-policy-qualities n-policy-primary-qualities))
                    -1)
                 :type (unsigned-byte #.max-policy-qualities))
  ;; For efficiency, primary qualities are segregated because there are few
  ;; enough of them to fit in a fixnum.
  (primary-qualities 0 :type (unsigned-byte #.(* 2 n-policy-primary-qualities)))
  ;; 2 bits per dependent quality is a fixnum on 64-bit build, not on 32-bit.
  ;; It would certainly be possible to constrain this to storing exactly
  ;; the 16 currently defined dependent qualities,
  ;; but that would be overly limiting.
  (dependent-qualities 0
   :type (unsigned-byte #.(* (- max-policy-qualities n-policy-primary-qualities)
                             2))))
(declaim (freeze-type policy))

;;; *POLICY* holds the current global compiler policy information, as
;;; a POLICY object mapping from the compiler-assigned index (unique per
;;; quality name) to quality value.
;;; This used to be an alist, but tail-sharing was never really possible
;;; because for deterministic comparison the list was always freshly
;;; consed so that destructive sorting could be done for canonicalization.
(defvar *policy*)
(defvar *macro-policy* nil)
;;; global policy restrictions as a POLICY object or nil
(defvar *policy-min* nil)
(defvar *policy-max* nil)

(declaim (type policy *policy*)
         (type (or policy null) *policy-min* *policy-max*))

(defun restrict-compiler-policy (&optional quality (min 0) (max 3))
  "Assign a minimum value to an optimization quality. QUALITY is the name of
the optimization quality to restrict, MIN (defaulting to zero) is the
minimum allowed value, and MAX (defaults to 3) is the maximum.

Returns the alist describing the current policy restrictions.

If QUALITY is NIL or not given, nothing is done.

Otherwise, if MIN is zero or MAX is 3 or neither are given, any
existing restrictions of QUALITY are removed.

See also :POLICY option in WITH-COMPILATION-UNIT."
  (declare (type policy-quality min max))
  (when quality
    (let ((quality-id (policy-quality-name-p quality)))
      (unless quality-id
        (error "~S is not a policy quality" quality))
      (when (> min max)
        (error "MIN ~s should be not be greater than MAX ~s." min max))
      ;; The dynamic policy object is immutable, otherwise a construct like
      ;;  (let ((*policy-min* *policy-max*)) ...)
      ;; could allow alterations inside the LET to leak out.
      ;; The structure itself does not declare slots its read-only, because
      ;; OPTIMIZE declaration processing uses it as a scratchpad.
      (setf *policy-min*
            (if *policy-min*
                (copy-structure *policy-min*)
                (make-policy 0 0))
            *policy-max*
            (if *policy-max*
                (copy-structure *policy-max*)
                (let ((policy (make-policy
                               (1- (expt 2 (* 2 n-policy-primary-qualities)))
                               0)))
                  (setf (policy-dependent-qualities policy)
                        (1- (expt 2
                                  (* (- max-policy-qualities n-policy-primary-qualities)
                                     2))))
                  policy)))
      (alter-policy *policy-min* quality-id min (plusp min))
      (alter-policy *policy-max* quality-id max (< max 3))))
  ;; Return dotted pairs, not elements that look declaration-like.
  (flet ((policy-to-result (policy)
           (and policy
                (mapc (lambda (x) (rplacd x (cadr x)))
                      (policy-to-decl-spec policy)))))
    (values (policy-to-result *policy-min*)
            (policy-to-result *policy-max*))))

(defstruct (policy-dependent-quality (:copier nil))
  (name nil :type symbol :read-only t)
  (expression nil :read-only t)
  (getter nil :read-only t)
  (documentation nil :read-only t)
  (values-documentation nil :read-only t))
(declaim (freeze-type policy-dependent-quality))

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
                 (elt +policy-primary-qualities+ (lognot index))
                 (policy-dependent-quality-name
                  (elt **policy-dependent-qualities** index)))
             (if raw
                 ;; Raw values are insensitive to *POLICY-MIN/MAX*.
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
  (or (awhen (position x +policy-primary-qualities+ :test #'eq)
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

;; ANSI-specified default of 1 for each quality.
(defglobal **baseline-policy** nil)
;; Baseline policy altered with (TYPE-CHECK 0)
(defglobal **zero-typecheck-policy** nil)
#-sb-xc-host (declaim (type policy **baseline-policy**))

;;; This is to be called early in cold init to set things up, and may
;;; also be called again later in cold init in order to reset default
;;; optimization policy back to default values after toplevel PROCLAIM
;;; OPTIMIZE forms have messed with it.
#-sb-xc-host
(defun !policy-cold-init-or-resanify ()
  (macrolet ((reflect-host-value (target-policy &optional (host-policy target-policy))
               ;; Act like MAKE-LOAD-FORM essentially
               (let ((policy (symbol-value host-policy)))
                 `(setq ,target-policy
                        (make-policy ,(policy-primary-qualities policy)
                                     ,(policy-presence-bits policy)
                                     ,(policy-dependent-qualities policy))))))
    (reflect-host-value **baseline-policy**)
    (reflect-host-value **zero-typecheck-policy**)
    (reflect-host-value *policy* **baseline-policy**)))

#+sb-xc-host
(defun init-xc-policy (&optional baseline-qualities)
  ;; ANSI says that 1 is the initial value for all policy qualities
  ;; so we establish that as both the baseline and the active default.
  (setq **baseline-policy**
        (make-policy (loop for i below n-policy-primary-qualities
                           sum (ash #b01 (* i 2))))
        *policy* (copy-policy **baseline-policy**))
  (when baseline-qualities
    (proclaim `(optimize ,@baseline-qualities))
    ;; Copy altered policy back as the baseline policy
    (setq **baseline-policy** (copy-policy *policy*)))
  (let ((*policy* *policy*))
    (proclaim '(optimize (type-check 0)))
    (setq **zero-typecheck-policy** *policy*)))

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

(macrolet ((extract-field (min-expression-primary min-expression-dependent
                           max-expression-primary max-expression-dependent)
             `(if (minusp index)
                  (let ((byte-pos (* (lognot index) 2)))
                    (min ,max-expression-primary
                         (max ,min-expression-primary
                              (ldb (byte 2 byte-pos)
                                   (policy-primary-qualities policy)))))
                  (let ((byte-pos (* index 2)))
                    (min ,max-expression-dependent
                         (max ,min-expression-dependent
                              (if (logbitp index (policy-presence-bits policy))
                                  (ldb (byte 2 byte-pos)
                                       (policy-dependent-qualities policy))
                                  1))))))
           (define-getter (name &body body)
             `(progn
                (declaim (ftype (sfunction (policy fixnum) (unsigned-byte 2)) ,name))
                (defun ,name (policy index)
                  (declare (type policy policy)
                         (type (integer
                                #.(- n-policy-primary-qualities)
                                #.(- max-policy-qualities
                                     n-policy-primary-qualities 1))
                               index))
                  ,@body))))

  ;; Return the value for quality INDEX in POLICY, using *POLICY-MIN/MAX*
  ;; Primary qualities are assumed to exist, however policy-restricting functions
  ;; can create a POLICY that indicates absence of primary qualities.
  ;; This does not affect RESTRICT-COMPILER-POLICY because a lower bound of 0
  ;; can be assumed for everything. SET-MACRO-POLICY might care though.
  (define-getter %policy-quality
    (let ((min *policy-min*)
          (max *policy-max*))
      (macrolet ((quality-min (get-byte)
                   `(if min
                        (ldb (byte 2 byte-pos) (,get-byte min))
                        0))
                 (quality-max (get-byte)
                   `(if max
                        (ldb (byte 2 byte-pos) (,get-byte max))
                        3)))
        (extract-field (quality-min policy-primary-qualities)
                       (quality-min policy-dependent-qualities)
                       (quality-max policy-primary-qualities)
                       (quality-max policy-dependent-qualities)))))

  ;; Return the unadjusted value for quality INDEX in POLICY.
  ;; This is used for converting a policy to a list of elements for display
  ;; and for verifying that after processing declarations, the new policy
  ;; matches the given declarations, thus implying no ambiguity.
  (define-getter %%policy-quality
    (extract-field 0 0 3 3)))

;;; Forward declaration of %COERCE-TO-POLICY.
;;; Definition is in 'node' so that FUNCTIONAL and NODE types are defined.
;;; Arg is declared of type T because the function explicitly checks it.
(declaim (ftype (sfunction (t) policy) %coerce-to-policy))

;;; syntactic sugar for querying optimization policy qualities
;;;
;;; Evaluate EXPR in terms of the optimization policy associated with
;;; THING. EXPR is a form which accesses optimization qualities by
;;; referring to them by name, e.g. (> SPEED SPACE).
(defmacro policy (thing expr &optional (coercion-fn '%coerce-to-policy))
  (let* ((n-policy (make-symbol "P"))
         (binds (loop for index downfrom -1
                      for name across +policy-primary-qualities+
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
                  :documentation ',documentation
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
     ',name))

#-sb-xc-host
(defmethod documentation ((x symbol) (doc-type (eql 'optimize)))
  (awhen (find x **policy-dependent-qualities**
               :key #'policy-dependent-quality-name)
    (policy-dependent-quality-documentation it)))

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
                 (compiler-warn
                  "~@<Ignoring unknown optimization quality ~S in:~_ ~S~:>"
                  quality spec))
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

;;; Same as above but slightly more efficient
(defun %augment-policy (quality value policy)
  (let ((result (copy-policy policy)))
    (alter-policy result quality value)))

(defmacro augment-policy (quality value policy)
  (let ((value-sym (gensym "VALUE"))
        (policy-sym (gensym "POLICY")))
    `(let ((,value-sym ,value)
           (,policy-sym ,policy))
       (if (policy ,policy-sym (> ,quality ,value-sym))
           (%augment-policy ,(policy-quality-name-p quality)
                            ,value-sym
                            ,policy-sym)
           ,policy-sym))))

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
  ;; Unlike most constructs that involve a policy, assume total absence of all
  ;; standard qualities. Record only the qualities expressed in LIST.
  (setq *macro-policy* (process-optimize-decl `(optimize ,@list)
                                              (make-policy 0 0 0))))

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
