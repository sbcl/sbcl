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

;;; global policy restrictions
(defvar *policy-restrictions* nil)

(defun restrict-compiler-policy (&optional quality (min 0))
  #!+sb-doc
  "Assing a minimum value to an optimization quality. QUALITY is the name of
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
    (aver (policy-quality-name-p quality))
    (if (zerop min)
        (setf *policy-restrictions*
              (remove quality *policy-restrictions* :key #'car))
        (let ((cell (assoc quality *policy-restrictions*)))
          (if cell
              (setf (cdr cell) min)
              (push (cons quality min) *policy-restrictions*)))))
  *policy-restrictions*)

;;; CMU CL used a special STRUCTURE-OBJECT type POLICY to represent
;;; the state of optimization policy at any point in compilation. This
;;; was a natural choice, but in SBCL it became a little troublesome
;;; because of stupid technicalities involving the cold initialization
;;; of structure LAYOUTs and structure accessors, so now we just use
;;; alists instead.
(def!type policy () 'list)

(defstruct policy-dependent-quality
  name
  expression
  getter
  values-documentation)

;;; names of recognized optimization policy qualities
(defvar *policy-qualities*) ; (initialized at cold init)
(defvar *policy-dependent-qualities* nil) ; alist of POLICY-DEPENDENT-QUALITYs

;;; Is X the name of an optimization policy quality?
(defun policy-quality-name-p (x)
  (or (memq x *policy-qualities*)
      (assq x *policy-dependent-qualities*)))

;;; Is it deprecated?
(defun policy-quality-deprecation-warning (quality spec)
  (when (member quality '(stack-allocate-dynamic-extent stack-allocate-vector
                          stack-allocate-value-cells))
    (make-instance 'simple-reference-warning
                   :format-control "~@<Ignoring deprecated optimization quality ~S in:~_ ~S~:>"
                   :format-arguments (list quality spec)
                   :references (list '(:sbcl :variable *stack-allocate-dynamic-extent*)
                                     '(:sbcl :node "Dynamic-extent allocation")))))

;;; *POLICY* holds the current global compiler policy information, as
;;; an alist mapping from optimization quality name to quality value.
;;; Inside the scope of declarations, new entries are added at the
;;; head of the alist.
(declaim (type policy *policy*))
(defvar *policy*)          ; initialized in cold init

(defun sort-policy (policy)
  ;; We occasionally want to compare policies using EQL, hence we
  ;; canonize the order.
  (sort policy #'string< :key #'car))

;;; This is to be called early in cold init to set things up, and may
;;; also be called again later in cold init in order to reset default
;;; optimization policy back to default values after toplevel PROCLAIM
;;; OPTIMIZE forms have messed with it.
(defun !policy-cold-init-or-resanify ()
  (setf *policy-qualities*
        '(;; ANSI standard qualities
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
          inhibit-warnings))
  (setf *policy*
        (sort-policy (mapcar (lambda (name)
                               ;; CMU CL didn't use 1 as the default for
                               ;; everything, but since ANSI says 1 is the ordinary
                               ;; value, we do.
                               (cons name 1))
                             *policy-qualities*)))
  (setf *policy-restrictions* nil)
  ;; not actually POLICY, but very similar
  (setf *handled-conditions* nil
        *disabled-package-locks* nil))

;;; On the cross-compilation host, we initialize immediately (not
;;; waiting for "cold init", since cold init doesn't exist on
;;; cross-compilation host).
#+sb-xc-host (!policy-cold-init-or-resanify)

;;; Look up a named optimization quality in POLICY. This is only
;;; called by compiler code for known-valid QUALITY-NAMEs, e.g. SPEED;
;;; it's an error if it's called for a quality which isn't defined.
(defun policy-quality (policy quality-name)
  (aver (policy-quality-name-p quality-name))
  (%policy-quality policy quality-name))

(defun %policy-quality (policy quality-name)
  (let* ((acons (assoc quality-name policy))
         (min (or (cdr (assoc quality-name *policy-restrictions*)) 0))
         (result (or (cdr acons) 1)))
    (max result min)))

;;; syntactic sugar for querying optimization policy qualities
;;;
;;; Evaluate EXPR in terms of the optimization policy associated with
;;; THING. EXPR is a form which accesses optimization qualities by
;;; referring to them by name, e.g. (> SPEED SPACE).
(defmacro policy (thing expr)
  (let* ((n-policy (gensym "N-POLICY-"))
         (binds (mapcar (lambda (name)
                          `(,name (policy-quality ,n-policy ',name)))
                        *policy-qualities*))
         (dependent-binds
          (loop for (name . info) in *policy-dependent-qualities*
               collect `(,name (let ((,name (policy-quality ,n-policy ',name)))
                                 (if (= ,name 1)
                                     ,(policy-dependent-quality-expression info)
                                     ,name))))))
    `(let* ((,n-policy (%coerce-to-policy ,thing)))
       (declare (ignorable ,n-policy))
       (symbol-macrolet (,@binds
                         ,@dependent-binds)
         ,expr))))

;;; Dependent qualities
(defmacro define-optimization-quality
    (name expression &optional values-documentation documentation)
  (declare (ignorable documentation))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((acons (assoc ',name *policy-dependent-qualities*))
           (item (make-policy-dependent-quality
                  :name ',name
                  :expression ',expression
                  :getter (lambda (policy) (policy policy ,expression))
                  :values-documentation ',values-documentation)))
       (if acons
           (setf (cdr acons) item)
           (setf *policy-dependent-qualities*
                 (nconc *policy-dependent-qualities* (list `(,',name . ,item))))))
     #-sb-xc-host
     ,@(when documentation `((setf (fdocumentation ',name 'optimize) ,documentation)))
     ',name))
