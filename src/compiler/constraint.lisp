;;;; This file implements the constraint propagation phase of the
;;;; compiler, which uses global flow analysis to obtain dynamic type
;;;; information.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; TODO:
;;;
;;; -- documentation
;;;
;;; -- MV-BIND, :ASSIGNMENT
;;;
;;; Note: The functions in this file that accept constraint sets are
;;; actually receiving the constraint sets associated with nodes,
;;; blocks, and lambda-vars.  It might be make CP easier to understand
;;; and work on if these functions traded in nodes, blocks, and
;;; lambda-vars directly.

;;; Problems:
;;;
;;; -- Constraint propagation badly interacts with bottom-up type
;;; inference. Consider
;;;
;;; (defun foo (n &aux (i 42))
;;;   (declare (optimize speed))
;;;   (declare (fixnum n)
;;;            #+nil (type (integer 0) i))
;;;   (tagbody
;;;      (setq i 0)
;;;    :loop
;;;      (when (>= i n) (go :exit))
;;;      (setq i (1+ i))
;;;      (go :loop)
;;;    :exit))
;;;
;;; In this case CP cannot even infer that I is of class INTEGER.
;;;
;;; -- In the above example if we place the check after SETQ, CP will
;;; fail to infer (< I FIXNUM): it does not understand that this
;;; constraint follows from (TYPEP I (INTEGER 0 0)).

(in-package "SB-C")

;;; *CONSTRAINT-UNIVERSE* gets bound in IR1-PHASES to a fresh,
;;; zero-length, non-zero-total-size vector-with-fill-pointer.
(declaim (type (and (vector t) (not simple-array)) *constraint-universe*))
(defvar *constraint-universe*)

(deftype constraint-y () '(or ctype lvar lambda-var constant))

(defstruct (constraint
            (:include sset-element)
            (:constructor make-constraint (number kind x y not-p))
            (:copier nil))
  ;; the kind of constraint we have:
  ;;
  ;; TYPEP
  ;;     X is a LAMBDA-VAR and Y is a CTYPE. The value of X is
  ;;     constrained to be of type Y.
  ;;
  ;; >, <, or =
  ;;     X is a lambda-var and Y is a CTYPE. The relation holds
  ;;     between X and some object of type Y.
  ;;
  ;; EQL
  ;;     X is a LAMBDA-VAR and Y is a LVAR, a LAMBDA-VAR or a CONSTANT.
  ;;     The relation is asserted to hold.
  ;;
  ;; ARRAY-IN-BOUNDS-P
  ;;     To handle (array-in-bounds-p array var) and
  ;;     (array-in-bounds-p array 10) X can be either the lambda-var
  ;;     of ARRAY or VAR, while Y is either the lambda-var of VAR or a
  ;;     constant.
  (kind nil :type (member typep < > = eql
                          array-in-bounds-p
                          equality))
  ;; The operands to the relation.
  (x nil :type lambda-var)
  (y nil :type constraint-y)
  ;; If true, negates the sense of the constraint, so the relation
  ;; does *not* hold.
  (not-p nil :type boolean))


;;; Historically, CMUCL and SBCL have used a sparse set implementation
;;; for which most operations are O(n) (see sset.lisp), but at the
;;; cost of at least a full word of pointer for each constraint set
;;; element.  Using bit-vectors instead of pointer structures saves a
;;; lot of space and thus GC time (particularly on 64-bit machines),
;;; and saves time on copy, union, intersection, and difference
;;; operations; but makes iteration slower.  Circa September 2008,
;;; switching to bit-vectors gave a modest (5-10%) improvement in real
;;; compile time for most Lisp systems, and as much as 20-30% for some
;;; particularly CP-dependent systems.

;;; It's bad to leave commented code in files, but if some clever
;;; person comes along and makes SSETs better than bit-vectors as sets
;;; for constraint propagation, or if bit-vectors on some XC host
;;; really lose compared to SSETs, here's the conset API as a wrapper
;;; around SSETs:
#+nil
(progn
  (deftype conset () 'sset)
  (declaim (ftype (sfunction (conset) boolean) conset-empty))
  (declaim (ftype (sfunction (conset) conset) copy-conset))
  (declaim (ftype (sfunction (constraint conset) boolean) conset-member))
  (declaim (ftype (sfunction (constraint conset) boolean) conset-adjoin))
  (declaim (ftype (sfunction (conset conset) boolean) conset=))
  (declaim (ftype (sfunction (conset conset) (values)) conset-union))
  (declaim (ftype (sfunction (conset conset) (values)) conset-intersection))
  (declaim (ftype (sfunction (conset conset) (values)) conset-difference))
  (defun make-conset () (make-sset))
  (defmacro do-conset-elements ((constraint conset &optional result) &body body)
    `(do-sset-elements (,constraint ,conset ,result) ,@body))
  (defmacro do-conset-intersection
      ((constraint conset1 conset2 &optional result) &body body)
    `(do-conset-elements (,constraint ,conset1 ,result)
       (when (conset-member ,constraint ,conset2)
         ,@body)))
  (defun conset-empty (conset) (sset-empty conset))
  (defun copy-conset (conset) (copy-sset conset))
  (defun conset-member (constraint conset) (sset-member constraint conset))
  (defun conset-adjoin (constraint conset) (sset-adjoin constraint conset))
  (defun conset= (conset1 conset2) (sset= conset1 conset2))
  ;; Note: CP doesn't ever care whether union, intersection, and
  ;; difference change the first set.  (This is an important degree of
  ;; freedom, since some ways of implementing sets lose a great deal
  ;; when these operations are required to track changes.)
  (defun conset-union (conset1 conset2)
    (sset-union conset1 conset2) (values))
  (defun conset-intersection (conset1 conset2)
    (sset-intersection conset1 conset2) (values))
  (defun conset-difference (conset1 conset2)
    (sset-difference conset1 conset2) (values)))

(locally
    ;; This is performance critical for the compiler, and benefits
    ;; from the following declarations.  Probably you'll want to
    ;; disable these declarations when debugging consets.
    (declare #-sb-xc-host (optimize (speed 3) (safety 0) (space 0)))
  (declaim (inline %constraint-number))
  (defun %constraint-number (constraint)
    (sset-element-number constraint))
  (defstruct (conset
              (:constructor make-conset ())
              (:copier %copy-conset))
    (vector (make-array
             (power-of-two-ceiling (length *constraint-universe*))
             :element-type 'bit :initial-element 0)
            :type simple-bit-vector)
    ;; Bit-vectors win over lightweight hashes for copy, union,
    ;; intersection, difference, but lose for iteration if you iterate
    ;; over the whole vector.  Tracking extrema helps a bit.
    (min 0 :type fixnum)
    (max 0 :type fixnum))

  (defun conset-empty (conset)
    (or (= (conset-min conset) (conset-max conset))
        (not (find 1 (conset-vector conset)
                   :start (conset-min conset)
                   :end (conset-max conset)))))

  (defun copy-conset (conset)
    (let ((ret (%copy-conset conset)))
      (setf (conset-vector ret) (copy-seq (conset-vector conset)))
      ret))

  (defun %conset-grow (conset new-size)
    (declare (type index new-size))
    (setf (conset-vector conset)
          (replace (the simple-bit-vector
                      (make-array
                       (power-of-two-ceiling new-size)
                      :element-type 'bit
                      :initial-element 0))
                   (the simple-bit-vector
                     (conset-vector conset)))))

  (declaim (inline conset-grow))
  (defun conset-grow (conset new-size)
    (declare (type index new-size))
    (when (< (length (conset-vector conset)) new-size)
      (%conset-grow conset new-size))
    (values))

  (defun conset-member (constraint conset)
    (let ((number (%constraint-number constraint))
          (vector (conset-vector conset)))
      (when (< number (length vector))
        (plusp (sbit vector number)))))

  (defun conset-adjoin (constraint conset)
    (let ((number (%constraint-number constraint)))
      (conset-grow conset (1+ number))
      (setf (sbit (conset-vector conset) number) 1)
      (setf (conset-min conset) (min number (conset-min conset)))
      (when (>= number (conset-max conset))
        (setf (conset-max conset) (1+ number))))
    conset)

  (defun conset= (conset1 conset2)
    (let* ((vector1 (conset-vector conset1))
           (vector2 (conset-vector conset2))
           (length1 (length vector1))
           (length2 (length vector2)))
      (if (= length1 length2)
          ;; When the lengths are the same, we can rely on EQUAL being
          ;; nicely optimized on bit-vectors.
          (equal vector1 vector2)
          (multiple-value-bind (shorter longer)
              (if (< length1 length2)
                  (values vector1 vector2)
                  (values vector2 vector1))
            ;; FIXME: make MISMATCH fast on bit-vectors.
            (dotimes (index (length shorter))
              (when (/= (sbit vector1 index) (sbit vector2 index))
                (return-from conset= nil)))
            (if (find 1 longer :start (length shorter))
                nil
                t)))))

  (macrolet
      ((defconsetop (name bit-op)
           `(defun ,name (conset-1 conset-2)
              (declare (optimize (speed 3) (safety 0)))
              (let* ((size-1 (length (conset-vector conset-1)))
                     (size-2 (length (conset-vector conset-2)))
                     (new-size (max size-1 size-2)))
                (conset-grow conset-1 new-size)
                (conset-grow conset-2 new-size))
              (let ((vector1 (conset-vector conset-1))
                    (vector2 (conset-vector conset-2)))
                (declare (simple-bit-vector vector1 vector2))
                (setf (conset-vector conset-1) (,bit-op vector1 vector2 t))
                ;; Update the extrema.
                ,(ecase name
                   ((conset-union)
                    `(setf (conset-min conset-1)
                           (min (conset-min conset-1)
                                (conset-min conset-2))
                           (conset-max conset-1)
                           (max (conset-max conset-1)
                                (conset-max conset-2))))
                   ((conset-intersection)
                    `(let ((start (max (conset-min conset-1)
                                       (conset-min conset-2)))
                           (end (min (conset-max conset-1)
                                     (conset-max conset-2))))
                       (setf (conset-min conset-1)
                             (if (> start end)
                                 0
                                 (or (position 1 (conset-vector conset-1)
                                               :start start :end end)
                                     0))
                             (conset-max conset-1)
                             (if (> start end)
                                 0
                                 (let ((position
                                        (position
                                         1 (conset-vector conset-1)
                                         :start start :end end :from-end t)))
                                   (if position
                                       (1+ position)
                                       0))))))
                   ((conset-difference)
                    `(setf (conset-min conset-1)
                           (or (position 1 (conset-vector conset-1)
                                         :start (conset-min conset-1)
                                         :end (conset-max conset-1))
                               0)
                           (conset-max conset-1)
                           (let ((position
                                  (position
                                   1 (conset-vector conset-1)
                                   :start (conset-min conset-1)
                                   :end (conset-max conset-1)
                                   :from-end t)))
                             (if position
                                 (1+ position)
                                 0))))))
              (values))))
    (defconsetop conset-union bit-ior)
    (defconsetop conset-intersection bit-and)
    (defconsetop conset-difference bit-andc2)))

;;; Constraints are hash-consed. Unfortunately, types aren't, so we have
;;; to over-approximate and then linear search through the potential hits.
;;; LVARs can only be found in EQL (not-p = NIL) constraints, while constant
;;; and lambda-vars can only be found in EQL constraints.
(defun find-constraint (kind x y not-p)
  (declare (type lambda-var x) (type constraint-y y) (type boolean not-p))
  (etypecase y
    (ctype
       (awhen (lambda-var-ctype-constraints x)
         (dolist (con (gethash (sb-kernel::type-class y) it) nil)
           (when (and (eq (constraint-kind con) kind)
                      (eq (constraint-not-p con) not-p)
                      (type= (constraint-y con) y))
             (return-from find-constraint con)))
         nil))
    (lvar
       (awhen (lambda-var-eq-constraints x)
         (gethash y it)))
    ((or constant lambda-var)
       (awhen (lambda-var-eq-constraints x)
         (let ((cache (gethash y it)))
           (declare (type list cache))
           (if not-p (cdr cache) (car cache)))))))

;;; The most common operations on consets are iterating through the constraints
;;; that are related to a certain variable in a given conset.  Storing the
;;; constraints related to each variable in vectors allows us to easily iterate
;;; through the intersection of such constraints and the constraints in a conset.
;;;
;;; EQL-var constraints assert that two lambda-vars are EQL.
;;; Private constraints assert that a lambda-var is EQL or not EQL to a constant.
;;; Inheritable constraints are constraints that may be propagated to EQL
;;; lambda-vars (along with EQL-var constraints).
;;;
;;; Lambda-var -- lvar EQL constraints only serve one purpose: remember whether
;;; an lvar is (only) written to by a ref to that lambda-var, and aren't ever
;;; propagated.
;;;
;;; Finally, the lambda-var conset is only used to track the whole set of
;;; constraints associated with a given lambda-var, and thus easily delete
;;; such constraints from a conset.
(defun register-constraint (x con y)
  (declare (type lambda-var x) (type constraint con) (type constraint-y y))
  (conset-adjoin con (lambda-var-constraints x))
  (macrolet ((ensuref (place default)
               `(or ,place (setf ,place ,default)))
             (ensure-hash (place)
               `(ensuref ,place (make-hash-table :test #'eq)))
             (ensure-vec (place)
               `(ensuref ,place (make-array 8 :adjustable t :fill-pointer 0))))
    (etypecase y
      (ctype
       (let ((index (ensure-hash (lambda-var-ctype-constraints x)))
             (vec   (ensure-vec  (lambda-var-inheritable-constraints x))))
         (push con (gethash (sb-kernel::type-class y) index))
         (vector-push-extend con vec)))
      (lvar
       (let ((index (ensure-hash (lambda-var-eq-constraints x))))
         (setf (gethash y index) con)))
      ((or constant lambda-var)
       (let* ((index (ensure-hash (lambda-var-eq-constraints x)))
              (cons  (ensuref (gethash y index) (list nil))))
         (if (constraint-not-p con)
             (setf (cdr cons) con)
             (setf (car cons) con)))
       (typecase y
         (constant
          (let ((vec (ensure-vec (lambda-var-private-constraints x))))
            (vector-push-extend con vec)))
         (lambda-var
          (let ((vec (if (or (constraint-not-p con)
                             (eq (constraint-kind con) 'array-in-bounds-p))
                         (ensure-vec (lambda-var-inheritable-constraints x))
                         (ensure-vec (lambda-var-eql-var-constraints x)))))
            (vector-push-extend con vec)))))))
  nil)

;;; Return a constraint for the specified arguments. We only create a
;;; new constraint if there isn't already an equivalent old one,
;;; guaranteeing that all equivalent constraints are EQ. This
;;; shouldn't be called on LAMBDA-VARs with no CONSTRAINTS set.
(defun find-or-create-constraint (kind x y not-p)
  (declare (type lambda-var x) (type constraint-y y) (type boolean not-p))
  (or (find-constraint kind x y not-p)
      (let ((new (make-constraint (length *constraint-universe*)
                                  kind x y not-p)))
        (vector-push-extend new *constraint-universe*
                            (1+ (length *constraint-universe*)))
        (register-constraint x new y)
        (when (lambda-var-p y)
          (register-constraint y new x))
        new)))

;;; Actual conset interface
;;;
;;; Constraint propagation needs to iterate over the set of lambda-vars known to
;;; be EQL to a given variable (including itself), via DO-EQL-VARS.
;;;
;;; It also has to iterate through constraints that are inherited by EQL variables
;;; (DO-INHERITABLE-CONSTRAINTS), and through constraints used by
;;; CONSTRAIN-REF-TYPE (to derive the type of a REF to a lambda-var).
;;;
;;; Consets must keep track of which lvars are EQL to a given lambda-var (result
;;; from a REF to the lambda-var): CONSET-LVAR-LAMBDA-VAR-EQL-P and
;;; CONSET-ADD-LVAR-LAMBDA-VAR-EQL.  This, as all other constraints, must of
;;; course be cleared when a lambda-var's constraints are dropped because of
;;; assignment.
;;;
;;; Consets must be able to add constraints to a given lambda-var
;;; (CONSET-ADD-CONSTRAINT), and to the set of variables EQL to a given
;;; lambda-var (CONSET-ADD-CONSTRAINT-TO-EQL).
;;;
;;; When a lambda-var is assigned to, all the constraints involving that variable
;;; must be dropped: constraint propagation is flow-sensitive, so the constraints
;;; relate to the variable at a given range of program point.  In such cases,
;;; constraint propagation calls CONSET-CLEAR-LAMBDA-VAR.
;;;
;;; Finally, one of the main strengths of constraint propagation in SBCL is the
;;; tracking of EQL variables to help constraint propagation.  When two variables
;;; are known to be EQL (e.g. after a branch), ADD-EQL-VAR-VAR-CONSTRAINT is
;;; called to add the EQL constraint, but also have each equality class inherit
;;; the other's (inheritable) constraints.
;;;
;;; On top of that, we have the usual bulk set operations: intersection, copy,
;;; equality or emptiness testing.  There's also union, but that's only an
;;; optimisation to avoid useless copies in ADD-TEST-CONSTRAINTS and
;;; FIND-BLOCK-TYPE-CONSTRAINTS.
(defmacro do-conset-constraints-intersection ((symbol (conset constraints) &optional result)
                                              &body body)
  (let ((min (gensym "MIN"))
        (max (gensym "MAX")))
    (once-only ((conset conset)
                (constraints constraints))
      `(flet ((body (,symbol)
                (declare (type constraint ,symbol))
                ,@body))
         (when ,constraints
           (let ((,min (conset-min ,conset))
                 (,max (conset-max ,conset)))
             (declare (optimize speed))
             (map nil (lambda (constraint)
                        (declare (type constraint constraint))
                        (let ((number (constraint-number constraint)))
                          (when (and (<= ,min number)
                                     (< number ,max)
                                     (conset-member constraint ,conset))
                            (body constraint))))
                  ,constraints)))
         ,result))))

(defmacro do-eql-vars ((symbol (var constraints) &optional result) &body body)
  (once-only ((var         var)
              (constraints constraints))
    `(flet ((body-fun (,symbol)
              ,@body))
       (body-fun ,var)
       (do-conset-constraints-intersection
           (con (,constraints (lambda-var-eql-var-constraints ,var)) ,result)
         (let ((x (constraint-x con))
               (y (constraint-y con)))
           (body-fun (if (eq ,var x) y x)))))))

(defmacro do-inheritable-constraints ((symbol (conset variable) &optional result)
                                      &body body)
  (once-only ((conset   conset)
              (variable variable))
    `(block nil
       (flet ((body-fun (,symbol)
                ,@body))
         (do-conset-constraints-intersection
             (con (,conset (lambda-var-inheritable-constraints ,variable)))
           (body-fun con))
         (do-conset-constraints-intersection
             (con (,conset (lambda-var-eql-var-constraints ,variable)) ,result)
           (body-fun con))))))

(defmacro do-propagatable-constraints ((symbol (conset variable) &optional result)
                                       &body body)
  (once-only ((conset conset)
              (variable variable))
    `(block nil
       (flet ((body-fun (,symbol)
                ,@body))
         (do-conset-constraints-intersection
             (con (,conset (lambda-var-private-constraints ,variable)))
           (body-fun con))
         (do-conset-constraints-intersection
             (con (,conset (lambda-var-eql-var-constraints ,variable)))
           (body-fun con))
         (do-conset-constraints-intersection
             (con (,conset (lambda-var-inheritable-constraints ,variable)) ,result)
           (body-fun con))
         (do-conset-constraints-intersection
             (con (,conset (lambda-var-equality-constraints ,variable)) ,result)
           (body-fun con))))))

(declaim (inline conset-lvar-lambda-var-eql-p conset-add-lvar-lambda-var-eql))
(defun conset-lvar-lambda-var-eql-p (conset lvar lambda-var)
  (let ((constraint (find-constraint 'eql lambda-var lvar nil)))
    (and constraint
         (conset-member constraint conset))))

(defun conset-add-lvar-lambda-var-eql (conset lvar lambda-var)
  (let ((constraint (find-or-create-constraint 'eql lambda-var lvar nil)))
    (conset-adjoin constraint conset)))

(declaim (inline conset-add-constraint conset-add-constraint-to-eql))
(defun conset-add-constraint (conset kind x y not-p)
  (declare (type conset conset)
           (type lambda-var x))
  (conset-adjoin (find-or-create-constraint kind x y not-p)
                 conset))

(defun conset-add-constraint-to-eql (conset kind x y not-p &optional (target conset))
  (declare (type conset target conset)
           (type lambda-var x))
  (do-eql-vars (x (x conset))
    (conset-add-constraint target kind x y not-p)))

(declaim (inline conset-clear-lambda-var))
(defun conset-clear-lambda-var (conset var)
  (conset-difference conset (lambda-var-constraints var)))

;;; Copy all CONSTRAINTS involving FROM-VAR - except the (EQL VAR
;;; LVAR) ones - to all of the variables in the VARS list.
(defun inherit-constraints (vars from-var constraints target)
  (do-inheritable-constraints (con (constraints from-var))
    (let ((eq-x (eq from-var (constraint-x con)))
          (eq-y (eq from-var (constraint-y con))))
      (dolist (var vars)
        (let ((x (if eq-x var (constraint-x con)))
              (y (if eq-y var (constraint-y con))))
          (unless (eq x y)
            (conset-add-constraint target
                                   (constraint-kind con)
                                   x
                                   y
                                   (constraint-not-p con))))))))

;; Add an (EQL LAMBDA-VAR LAMBDA-VAR) constraint on VAR1 and VAR2 and
;; inherit each other's constraints.
(defun add-eql-var-var-constraint (var1 var2 constraints
                                   &optional (target constraints))
  (let ((constraint (find-or-create-constraint 'eql var1 var2 nil)))
    (unless (conset-member constraint target)
      (conset-adjoin constraint target)
      (collect ((eql1) (eql2))
        (do-eql-vars (var1 (var1 constraints))
          (eql1 var1))
        (do-eql-vars (var2 (var2 constraints))
          (eql2 var2))
        (inherit-constraints (eql1) var2 constraints target)
        (inherit-constraints (eql2) var1 constraints target))
      t)))

;;; If REF is to a LAMBDA-VAR with CONSTRAINTs (i.e. we can do flow
;;; analysis on it), then return the LAMBDA-VAR, otherwise NIL.
(declaim (inline ok-ref-lambda-var))
(defun ok-ref-lambda-var (ref)
  (declare (type ref ref))
  (let ((leaf (ref-leaf ref)))
    (when (and (lambda-var-p leaf)
               (lambda-var-constraints leaf))
      leaf)))

;;; See if LVAR's single USE is a REF to a LAMBDA-VAR and they are EQL
;;; according to CONSTRAINTS. Return LAMBDA-VAR if so.
(defun ok-lvar-lambda-var (lvar constraints)
  (declare (type lvar lvar))
  (let ((use (lvar-uses lvar)))
    (cond ((ref-p use)
           (let ((lambda-var (ok-ref-lambda-var use)))
             (and lambda-var
                  (conset-lvar-lambda-var-eql-p constraints lvar lambda-var)
                  lambda-var)))
          ((cast-p use)
           (ok-lvar-lambda-var (cast-value use) constraints)))))
;;;; Searching constraints

;;; Add the indicated test constraint to TARGET.
(declaim (inline precise-add-test-constraint))
(defun precise-add-test-constraint (fun x y not-p constraints target)
  (if (and (eq 'eql fun) (lambda-var-p y) (not not-p))
      (add-eql-var-var-constraint x y constraints target)
      (conset-add-constraint-to-eql constraints fun x y not-p target))
  (values))

(defun add-test-constraint (quick-p fun x y not-p constraints target)
  (cond (quick-p
         (conset-add-constraint target fun x y not-p))
        (t
         (precise-add-test-constraint fun x y not-p constraints target))))
;;; Add complementary constraints to the consequent and alternative
;;; blocks of IF. We do nothing if X is NIL.
(declaim (inline quick-add-complement-constraints))
(defun precise-add-complement-constraints (fun x y not-p constraints
                                           consequent-constraints
                                           alternative-constraints)
  (when x
    (precise-add-test-constraint fun x y not-p constraints
                                consequent-constraints)
    (precise-add-test-constraint fun x y (not not-p) constraints
                                 alternative-constraints))
  (values))

(defun quick-add-complement-constraints (fun x y not-p
                                         consequent-constraints
                                         alternative-constraints)
  (when x
    (conset-add-constraint consequent-constraints fun x y not-p)
    (conset-add-constraint alternative-constraints fun x y (not not-p)))
  (values))

(defun add-complement-constraints (quick-p fun x y not-p constraints
                                   consequent-constraints
                                   alternative-constraints)
  (if quick-p
      (quick-add-complement-constraints fun x y not-p
                                        consequent-constraints
                                        alternative-constraints)
      (precise-add-complement-constraints fun x y not-p constraints
                                          consequent-constraints
                                          alternative-constraints)))

(defun add-combination-test-constraints (use constraints
                                         consequent-constraints
                                         alternative-constraints
                                         quick-p)
  (flet ((add (fun x y not-p)
           (add-complement-constraints quick-p
                                       fun x y not-p
                                       constraints
                                       consequent-constraints
                                       alternative-constraints))
         (prop (triples target)
           (map nil (lambda (constraint)
                      (destructuring-bind (kind x y &optional not-p)
                          constraint
                        (when (and kind x y)
                          (add-test-constraint quick-p
                                               kind x y
                                               not-p constraints
                                               target))))
                triples)))
    (when (eq (combination-kind use) :known)
      (binding* ((info (combination-fun-info use) :exit-if-null)
                 (propagate (fun-info-constraint-propagate-if
                             info)
                            :exit-if-null))
        (multiple-value-bind (lvar type if else)
            (funcall propagate use constraints)
          (prop if consequent-constraints)
          (prop else alternative-constraints)
          (when (and lvar type)
            (add 'typep (ok-lvar-lambda-var lvar constraints)
                 type nil)
            (return-from add-combination-test-constraints)))))
    (let* ((name (lvar-fun-name
                  (basic-combination-fun use)))
           (args (basic-combination-args use))
           (ptype (gethash name *backend-predicate-types*)))
      (when ptype
        (add 'typep (ok-lvar-lambda-var (first args)
                                        constraints)
             ptype nil)))))

(defun array-in-bounds-p-constraints (constraints index-lvar index-var
                                      length-lvar)
  (let ((index-constant
          (and (not index-var)
               (let ((use (principal-lvar-use index-lvar)))
                 (and (ref-p use)
                      (constant-p (ref-leaf use))
                      (ref-leaf use)))))
        (array-lvar
          (let ((use (principal-lvar-ref-use length-lvar)))
            (and (combination-p use)
                 (lvar-fun-is (combination-fun use)
                              '(vector-length))
                 (car (combination-args use))))))
    (when (and (or index-var index-constant)
               array-lvar)
      (let ((array-var (ok-lvar-lambda-var array-lvar constraints)))
        (when array-var
          (if index-constant
              ;; Attach the constaraint to the array if
              ;; the index is constant
              (values 'array-in-bounds-p array-var index-constant)
              (values 'array-in-bounds-p index-var array-var)))))))

;;; Add test constraints to the consequent and alternative blocks of
;;; the test represented by USE.
(defun add-test-constraints (use if constraints)
  (declare (type node use) (type cif if))
  ;; Note: Even if we do (IF test exp exp) => (PROGN test exp)
  ;; optimization, the *MAX-OPTIMIZE-ITERATIONS* cutoff means that we
  ;; can't guarantee that the optimization will be done, so we still
  ;; need to avoid barfing on this case.
  (unless (eq (if-consequent if) (if-alternative if))
    (let ((consequent-constraints (make-conset))
          (alternative-constraints (make-conset))
          (quick-p (policy if (> compilation-speed speed))))
      (macrolet ((add (fun x y not-p)
                   `(add-complement-constraints quick-p
                                                ,fun ,x ,y ,not-p
                                                constraints
                                                consequent-constraints
                                                alternative-constraints)))
        (typecase use
          (ref
           (add 'typep (ok-lvar-lambda-var (ref-lvar use) constraints)
                (specifier-type 'null) t))
          (combination
           (unless (eq (combination-kind use)
                       :error)
             (let ((name (uncross
                          (lvar-fun-name
                           (basic-combination-fun use))))
                   (args (basic-combination-args use)))
               (add-equality-constraints name args
                                         constraints consequent-constraints alternative-constraints)
               (case name
                 ((%typep %instance-typep)
                  (let ((type (second args)))
                    (when (constant-lvar-p type)
                      (let ((val (lvar-value type)))
                        (add 'typep
                             (ok-lvar-lambda-var (first args) constraints)
                             (if (ctype-p val)
                                 val
                                 (let ((*compiler-error-context* use))
                                   (specifier-type val)))
                             nil)))))
                 ((eq eql)
                  (let* ((arg1 (first args))
                         (var1 (ok-lvar-lambda-var arg1 constraints))
                         (arg2 (second args))
                         (var2 (ok-lvar-lambda-var arg2 constraints)))
                    ;; The code below assumes that the constant is the
                    ;; second argument in case of variable to constant
                    ;; comparison which is sometimes true (see source
                    ;; transformations for EQ, EQL and CHAR=). Fixing
                    ;; that would result in more constant substitutions
                    ;; which is not a universally good thing, thus the
                    ;; unnatural asymmetry of the tests.
                    (cond ((not var1)
                           (when var2
                             (add-test-constraint quick-p
                                                  'typep var2 (lvar-type arg1)
                                                  nil constraints
                                                  consequent-constraints)))
                          (var2
                           (add 'eql var1 var2 nil))
                          ((constant-lvar-p arg2)
                           (add 'eql var1
                                (find-constant (lvar-value arg2))
                                nil))
                          (t
                           (add-test-constraint quick-p
                                                'typep var1 (lvar-type arg2)
                                                nil constraints
                                                consequent-constraints)))))
                 ((< >)
                  (when (= (length args) 2)
                    (flet ((handle-array-in-bounds-p (index-arg index-var length-arg)
                             (multiple-value-bind (kind x y)
                                 (array-in-bounds-p-constraints constraints index-arg index-var
                                                                length-arg)
                               (when kind
                                 (add-test-constraint quick-p
                                                      kind x y
                                                      nil constraints
                                                      consequent-constraints)))))
                      (let* ((arg1 (first args))
                             (var1 (ok-lvar-lambda-var arg1 constraints))
                             (arg2 (second args))
                             (var2 (ok-lvar-lambda-var arg2 constraints)))
                        (case name
                          (<
                           (handle-array-in-bounds-p arg1 var1 arg2))
                          (>
                           (handle-array-in-bounds-p arg2 var2 arg1)))
                        (when var1
                          (add name var1 (lvar-type arg2) nil))
                        (when var2
                          (add (if (eq name '<) '> '<) var2 (lvar-type arg1) nil))))))
                 (=
                  (when (= (length args) 2)
                    (let* ((arg1 (first args))
                           (var1 (ok-lvar-lambda-var arg1 constraints))
                           (arg2 (second args))
                           (var2 (ok-lvar-lambda-var arg2 constraints)))
                      (when var1
                        (add name var1 (lvar-type arg2) nil))
                      (when var2
                        (add name var2 (lvar-type arg1) nil)))))
                 (t
                  (add-combination-test-constraints use constraints
                                                    consequent-constraints
                                                    alternative-constraints
                                                    quick-p))))))))
      (values consequent-constraints alternative-constraints))))

;;;; Applying constraints

;;; Return true if X is an integer NUMERIC-TYPE.
(defun integer-type-p (x)
  (declare (type ctype x))
  (and (numeric-type-p x)
       (eq (numeric-type-class x) 'integer)
       (eq (numeric-type-complexp x) :real)))

;;; Given that an inequality holds on values of type X and Y, return a
;;; new type for X. If GREATER is true, then X was greater than Y,
;;; otherwise less. If OR-EQUAL is true, then the inequality was
;;; inclusive, i.e. >=.
;;;
;;; If GREATER (or not), then we max (or min) in Y's lower (or upper)
;;; bound into X and return that result. If not OR-EQUAL, we can go
;;; one greater (less) than Y's bound.
(defun constrain-integer-type (x y greater or-equal)
  (declare (type numeric-type x y))
  (flet ((exclude (x)
           (cond ((not x) nil)
                 (or-equal x)
                 (greater (1+ x))
                 (t (1- x))))
         (bound (x)
           (if greater (numeric-type-low x) (numeric-type-high x))))
    (let* ((x-bound (bound x))
           (y-bound (exclude (bound y)))
           (new-bound (cond ((not x-bound) y-bound)
                            ((not y-bound) x-bound)
                            (greater (max x-bound y-bound))
                            (t (min x-bound y-bound)))))
      (if greater
          (modified-numeric-type x :low new-bound)
          (modified-numeric-type x :high new-bound)))))

;;; Return true if X is a float NUMERIC-TYPE.
(defun float-type-p (x)
  (declare (type ctype x))
  (and (numeric-type-p x)
       (eq (numeric-type-class x) 'float)
       (eq (numeric-type-complexp x) :real)))

;;; Exactly the same as CONSTRAIN-INTEGER-TYPE, but for float numbers.
;;;
;;; In contrast to the integer version, here the input types can have
;;; open bounds in addition to closed ones and we don't increment or
;;; decrement a bound to honor OR-EQUAL being NIL but put an open bound
;;; into the result instead, if appropriate.
(defun constrain-float-type (x y greater or-equal)
  (declare (type numeric-type x y))
  (aver (eql (numeric-type-class x) 'float))
  (aver (eql (numeric-type-class y) 'float))
  (labels ((exclude (x)
             (cond ((not x) nil)
                   (or-equal x)
                   (t
                    (if (consp x)
                        x
                        (list x)))))
           (bound (x)
             (if greater (numeric-type-low x) (numeric-type-high x)))
           (tighter-p (x ref)
             (cond ((null x) nil)
                   ((null ref) t)
                   ((sb-xc:= (type-bound-number x) (type-bound-number ref))
                    ;; X is tighter if X is an open bound and REF is not
                    (and (consp x) (not (consp ref))))
                   (greater
                    (sb-xc:< (type-bound-number ref) (type-bound-number x)))
                   (t
                    (sb-xc:> (type-bound-number ref) (type-bound-number x))))))
    (let* ((x-bound (bound x))
           (y-bound (exclude (bound y)))
           (new-bound (cond ((not x-bound)
                             y-bound)
                            ((not y-bound)
                             x-bound)
                            ((tighter-p y-bound x-bound)
                             y-bound)
                            (t
                             x-bound))))
      (if greater
          (modified-numeric-type x :low new-bound)
          (modified-numeric-type x :high new-bound)))))

(defun constrain-real-to-integer (y greater or-equal)
  (declare (type numeric-type y))
  (flet ((exclude (x)
           (cond ((not x) nil)
                 (or-equal x)
                 (t (list x))))
         (bound (x)
           (if greater
               (numeric-type-low x)
               (numeric-type-high x))))
    (let ((bound (exclude (bound y))))
      (when bound
        (if greater
            (make-numeric-type :low bound)
            (make-numeric-type :high bound))))))

;;; Return true if LEAF is "visible" from NODE.
(defun leaf-visible-from-node-p (leaf node)
  (cond
    ((lambda-var-p leaf)
     (and (find leaf (lexenv-vars (node-lexenv node))
                :key #'cdr :test #'eq)
          t))
   ;; FIXME: Check on FUNCTIONALs (CLAMBDAs and OPTIONAL-DISPATCHes),
   ;; not just LAMBDA-VARs.
   (t
    ;; Assume everything else is globally visible.
    t)))

(defun contiguous-numeric-set-type (xset)
  (cond ((xset-empty-p xset)
         nil)
        ;; Is XSET a contiguous integer range?
        ((block nil
           (let ((count 0)
                 (min nil)
                 (max nil))
             (declare (type fixnum count))
             (map-xset (lambda (value)
                         (unless (integerp value)
                           (return))
                         (incf count)
                         (when (or (null min) (< value min))
                           (setf min value))
                         (when (or (null max) (> value max))
                           (setf max value)))
                       xset)
             (when (= (- max min) (1- count))
               (make-numeric-type :class 'integer :low min :high max)))))
        ;; It's useful to know when something is not zero
        ((xset-member-p 0 xset)
         (make-numeric-type :class 'integer :low 0 :high 0))))

;;; Given the set of CONSTRAINTS for a variable and the current set of
;;; restrictions from flow analysis IN, set the type for REF
;;; accordingly.
(defun constrain-ref-type (ref in)
  (declare (type ref ref) (type conset in))
  ;; KLUDGE: The NOT-SET and NOT-FPZ here are so that we don't need to
  ;; cons up endless union types when propagating large number of EQL
  ;; constraints -- eg. from large CASE forms -- instead we just
  ;; directly accumulate one XSET, and a set of fp zeroes, which we at
  ;; the end turn into a MEMBER-TYPE.
  ;;
  ;; Since massive symbol cases are an especially atrocious pattern
  ;; and the (NOT (MEMBER ...ton of symbols...)) will never turn into
  ;; a more useful type, don't propagate their negation except for NIL
  ;; unless SPEED > COMPILATION-SPEED.
  (let ((res (single-value-type (node-derived-type ref)))
        (constrain-symbols (policy ref (> speed compilation-speed)))
        (not-set (alloc-xset))
        (not-numeric (alloc-xset))
        (not-fpz nil)
        (not-res *empty-type*)
        (leaf (ref-leaf ref)))
    (declare (type lambda-var leaf))
    (flet ((note-not (x)
             (if (fp-zero-p x)
                 (push x not-fpz)
                 (when (or constrain-symbols (null x) (not (symbolp x)))
                   (add-to-xset x not-set)))))
      (do-propagatable-constraints (con (in leaf))
        (let* ((x (constraint-x con))
               (y (constraint-y con))
               (not-p (constraint-not-p con))
               (other (if (eq x leaf) y x))
               (kind (constraint-kind con)))
          (case kind
            (equality
             (unless (eq (ref-constraints ref)
                         (pushnew con (ref-constraints ref)))
               (let ((lvar (node-lvar ref))
                     (principal-lvar (nth-value 1 (principal-lvar-dest-and-lvar (node-lvar ref)))))
                 (reoptimize-lvar lvar)
                 (unless (eq lvar principal-lvar)
                   (reoptimize-lvar principal-lvar)))))
            (typep
             (if not-p
                 (if (member-type-p other)
                     (mapc-member-type-members #'note-not other)
                     (setq not-res (type-union not-res other)))
                 (setq res (type-approx-intersection2 res other))))
            (array-in-bounds-p
             (unless (eq (ref-constraints ref)
                         (pushnew con (ref-constraints ref)))
               (reoptimize-lvar (node-lvar ref))))
            (eql
             (let ((other-type (leaf-type other)))
               (if not-p
                   (when (constant-p other)
                     (cond ((member-type-p other-type)
                            (note-not (constant-value other)))
                           ;; Numeric types will produce interesting
                           ;; negations, other than just "not equal"
                           ;; which can be handled by the equality
                           ;; constraints.
                           ((numeric-type-p other-type)
                            (add-to-xset (constant-value other) not-numeric))))
                   (let ((leaf-type (leaf-type leaf)))
                     (cond
                       ((or (constant-p other)
                            (and (leaf-refs other) ; protect from
                                        ; deleted vars
                                 (csubtypep other-type leaf-type)
                                 (not (type= other-type leaf-type))
                                 ;; Don't change to a LEAF not visible here.
                                 (leaf-visible-from-node-p other ref)))
                        (change-ref-leaf ref other)
                        (when (constant-p other) (return)))
                       (t
                        (setq res (type-approx-intersection2
                                   res other-type))))))))
            ((< >)
             (let* ((greater (eq kind '>))
                    (greater (if not-p (not greater) greater)))
               (cond
                 ((and (integer-type-p res) (integer-type-p y))
                  (setq res
                        (constrain-integer-type res y greater not-p)))
                 ((and (float-type-p res) (float-type-p y))
                  (setq res
                        (constrain-float-type res y greater not-p)))
                 ((integer-type-p y)
                  (let ((type (constrain-real-to-integer y greater not-p)))
                    (when type
                      (setf res
                            (type-approx-intersection2 res type))))))))
            (=
             (when (and (numeric-type-p y)
                        (not not-p))
               (setf res
                     (type-approx-intersection2 res
                                                (type-union (make-numeric-type :low (numeric-type-low y)
                                                                               :high (numeric-type-high y))
                                                            (make-numeric-type :complexp :complex
                                                                               :low (numeric-type-low y)
                                                                               :high (numeric-type-high y)))))))))))
    (cond ((and (if-p (node-dest ref))
                (or (xset-member-p nil not-set)
                    (csubtypep (specifier-type 'null) not-res)))
           (setf (node-derived-type ref) *wild-type*)
           (change-ref-leaf ref (find-constant t)))
          (t
           (let* ((union
                    (type-union not-res
                                (make-member-type not-set not-fpz)))
                  (numeric (contiguous-numeric-set-type not-numeric))
                  (type (type-difference res
                                         (if  numeric
                                              (type-union union numeric)
                                             union))))
             ;; CHANGE-CLASS can change the type, lower down to standard-object,
             ;; type propagation for classes is not as important anyway.
             (cond #-sb-xc-host
                   ((and
                     (eq sb-pcl::**boot-state** 'sb-pcl::complete)
                     (block nil
                       (let ((standard-object (find-classoid 'standard-object)))
                         (sb-kernel::map-type
                          (lambda (type)
                            (when (and (classoid-p type)
                                       (csubtypep type standard-object))
                              (return t)))
                          type)))))
                   (t
                    (derive-node-type ref
                                      (make-single-value-type type))
                    (maybe-terminate-block ref nil)))))))
  (values))

;;;; Flow analysis

(defun maybe-add-eql-var-lvar-constraint (ref gen)
  (let ((lvar (ref-lvar ref))
        (leaf (ref-leaf ref)))
    (when (and (lambda-var-p leaf) lvar)
      (conset-add-lvar-lambda-var-eql gen lvar leaf))))

;; Add an (EQL LAMBDA-VAR LAMBDA-VAR) constraint on VAR and LVAR's
;; LAMBDA-VAR if possible.
(defun maybe-add-eql-var-var-constraint (var lvar constraints
                                         &optional (target constraints))
  (declare (type lambda-var var) (type lvar lvar))
  (let ((lambda-var (ok-lvar-lambda-var lvar constraints)))
    (when lambda-var
      (add-eql-var-var-constraint var lambda-var constraints target))))

;;; Local propagation
;;; -- [TODO: For any LAMBDA-VAR ref with a type check, add that
;;;    constraint.]
;;; -- For any LAMBDA-VAR set, delete all constraints on that var; add
;;;    a type constraint based on the new value type.
(declaim (ftype (function (cblock conset boolean)
                          conset)
                constraint-propagate-in-block))
(defun constraint-propagate-in-block (block gen preprocess-refs-p)
  (do-nodes (node nil block)
    (typecase node
      (bind
       (let ((fun (bind-lambda node)))
         (when (eq (functional-kind fun) :let)
           (loop with call = (lvar-dest (node-lvar (first (lambda-refs fun))))
                 for var in (lambda-vars fun)
                 and val in (combination-args call)
                 when (and val (lambda-var-constraints var))
                 do (let ((type (lvar-type val)))
                      (unless (eq type *universal-type*)
                        (conset-add-constraint gen 'typep var type nil)))
                    (maybe-add-eql-var-var-constraint var val gen)))))
      (ref
       (when (ok-ref-lambda-var node)
         (maybe-add-eql-var-lvar-constraint node gen)
         (when preprocess-refs-p
           (constrain-ref-type node gen))))
      (cast

       (let* ((lvar (cast-value node))
              (var (ok-lvar-lambda-var lvar gen)))
         (when var
           (let ((atype (single-value-type (cast-derived-type node)))) ;FIXME
             (unless (eq atype *universal-type*)
               (conset-add-constraint-to-eql gen 'typep var atype nil))))
         (when (and (bound-cast-p node)
                    (bound-cast-check node)
                    (not (node-deleted (bound-cast-check node))))
           (let ((check-bound (bound-cast-check node)))
             (destructuring-bind (array dim index)
                 (combination-args check-bound)
               (declare (ignore array))
               (multiple-value-bind (kind x y)
                   (array-in-bounds-p-constraints gen index var dim)
                 (when kind
                   (conset-add-constraint-to-eql gen kind x y nil))))))))
      (cset
       (binding* ((var (set-var node))
                  (nil (lambda-var-p var) :exit-if-null)
                  (nil (lambda-var-constraints var) :exit-if-null))
         (when (policy node (and (= speed 3) (> speed compilation-speed)))
           (let ((type (lambda-var-type var)))
             (unless (eq *universal-type* type)
               (do-eql-vars (other (var gen))
                 (unless (eql other var)
                   (conset-add-constraint gen 'typep other type nil))))))
         (conset-clear-lambda-var gen var)
         (let ((type (single-value-type (node-derived-type node))))
           (unless (eq type *universal-type*)
             (conset-add-constraint gen 'typep var type nil)))
         (unless (policy node (> compilation-speed speed))
           (maybe-add-eql-var-var-constraint var (set-value node) gen))
         (add-eq-constraint var (set-value node) gen)))
      (combination
       (when (eq (combination-kind node) :known)
         (binding* ((info (combination-fun-info node) :exit-if-null)
                    (propagate (fun-info-constraint-propagate info)
                               :exit-if-null)
                    (constraints (funcall propagate node gen))
                    (register (if (policy node
                                          (> compilation-speed speed))
                                  #'conset-add-constraint
                                  #'conset-add-constraint-to-eql)))
           (map nil (lambda (constraint)
                      (destructuring-bind (kind x y &optional not-p)
                          constraint
                        (when (and kind x y)
                          (funcall register gen
                                   kind x y
                                   not-p))))
                constraints))))))
  gen)

(defun constraint-propagate-if (block gen)
  (let ((node (block-last block)))
    (when (if-p node)
      (let ((use (lvar-uses (if-test node))))
        (when (node-p use)
          (add-test-constraints use node gen))))))

;;; Starting from IN compute OUT and (consequent/alternative
;;; constraints if the block ends with an IF). Return the list of
;;; successors that may need to be recomputed.
(defun find-block-type-constraints (block final-pass-p)
  (declare (type cblock block))
  (let ((gen (constraint-propagate-in-block
              block
              (if final-pass-p
                  (block-in block)
                  (copy-conset (block-in block)))
              final-pass-p)))
    (multiple-value-bind (consequent-constraints alternative-constraints)
        (constraint-propagate-if block gen)
      (if consequent-constraints
          (let* ((node (block-last block))
                 (old-consequent-constraints (if-consequent-constraints node))
                 (old-alternative-constraints (if-alternative-constraints node))
                 (no-consequent (conset-empty consequent-constraints))
                 (no-alternative (conset-empty alternative-constraints))
                 (succ ()))
            ;; Add the consequent and alternative constraints to GEN.
            (cond ((and no-consequent no-alternative)
                   (setf (if-consequent-constraints node) gen)
                   (setf (if-alternative-constraints node) gen))
                  (t
                   (setf (if-consequent-constraints node) (copy-conset gen))
                   (unless no-consequent
                     (conset-union (if-consequent-constraints node)
                                   consequent-constraints))
                   (setf (if-alternative-constraints node) gen)
                   (unless no-alternative
                     (conset-union (if-alternative-constraints node)
                                   alternative-constraints))))
            ;; Has the consequent been changed?
            (unless (and old-consequent-constraints
                         (conset= (if-consequent-constraints node)
                                  old-consequent-constraints))
              (push (if-consequent node) succ))
            ;; Has the alternative been changed?
            (unless (and old-alternative-constraints
                         (conset= (if-alternative-constraints node)
                                  old-alternative-constraints))
              (push (if-alternative node) succ))
            succ)
          ;; There is no IF.
          (unless (and (block-out block)
                       (conset= gen (block-out block)))
            (setf (block-out block) gen)
            (block-succ block))))))

;;; Deliver the results of constraint propagation to REFs in BLOCK.
;;; During this pass, we also do local constraint propagation by
;;; adding in constraints as we see them during the pass through the
;;; block.
(defun use-result-constraints (block)
  (declare (type cblock block))
  (constraint-propagate-in-block block (block-in block) t))

;;; Give an empty constraints set to any var that doesn't have one and
;;; isn't a set closure var. Since a var that we previously rejected
;;; looks identical to one that is new, so we optimistically keep
;;; hoping that vars stop being closed over or lose their sets.
(defun init-var-constraints (component)
  (declare (type component component))
  (dolist (fun (component-lambdas component))
    (flet ((frob (x)
             (dolist (var (lambda-vars x))
               (unless (or (lambda-var-constraints var)
                           (lambda-var-no-constraints var))
                 (when (or (null (lambda-var-sets var))
                           (not (closure-var-p var)))
                   (setf (lambda-var-constraints var) (make-conset)))))))
      (frob fun)
      (dolist (let (lambda-lets fun))
        (frob let)))))

;;; Return the constraints that flow from PRED to SUCC. This is
;;; BLOCK-OUT unless PRED ends with an IF and test constraints were
;;; added.
(defun block-out-for-successor (pred succ)
  (declare (type cblock pred succ))
  (let ((last (block-last pred)))
    (or (when (if-p last)
          (cond ((eq succ (if-consequent last))
                 (if-consequent-constraints last))
                ((eq succ (if-alternative last))
                 (if-alternative-constraints last))))
        (block-out pred))))

(defun compute-block-in (block)
  (let ((in nil))
    (dolist (pred (block-pred block))
      ;; If OUT has not been calculated, assume it to be the universal
      ;; set.
      (let ((out (block-out-for-successor pred block)))
        (when out
          (if in
              (conset-intersection in out)
              (setq in (copy-conset out))))))
    (or in (make-conset))))

(defun update-block-in (block)
  (let ((in (compute-block-in block)))
    (cond ((and (block-in block) (conset= in (block-in block)))
           nil)
          (t
           (setf (block-in block) in)))))

;;; Return two lists: one of blocks that precede all loops and
;;; therefore require only one constraint propagation pass and the
;;; rest. This implementation does not find all such blocks.
;;;
;;; A more complete implementation would be:
;;;
;;;     (do-blocks (block component)
;;;       (if (every #'(lambda (pred)
;;;                      (or (member pred leading-blocks)
;;;                          (eq pred head)))
;;;                  (block-pred block))
;;;           (push block leading-blocks)
;;;           (push block rest-of-blocks)))
;;;
;;; Trailing blocks that succeed all loops could be found and handled
;;; similarly. In practice though, these more complex solutions are
;;; slightly worse performancewise.
(defun leading-component-blocks (component)
  (declare (type component component))
  (flet ((loopy-p (block)
           (let ((n (block-number block)))
             (dolist (pred (block-pred block))
               (unless (< n (block-number pred))
                 (return t))))))
    (let ((leading-blocks ())
          (rest-of-blocks ())
          (seen-loop-p ()))
      (do-blocks (block component)
        (when (block-type-check block)
          (when (and (not seen-loop-p) (loopy-p block))
            (setq seen-loop-p t))
          (if seen-loop-p
              (push block rest-of-blocks)
              (push block leading-blocks))))
      (values (nreverse leading-blocks) (nreverse rest-of-blocks)))))

;;; Append OBJ to the end of LIST as if by NCONC but only if it is not
;;; a member already.
(defun nconc-new (obj list)
  (do ((x list (cdr x))
       (prev nil x))
      ((endp x) (if prev
                    (progn
                      (setf (cdr prev) (list obj))
                      list)
                    (list obj)))
    (when (eql (car x) obj)
      (return-from nconc-new list))))

(defun find-and-propagate-constraints (component)
  (let ((blocks-to-process ()))
    (flet ((enqueue (blocks)
             (dolist (block blocks)
               (when (block-type-check block)
                 (setq blocks-to-process (nconc-new block blocks-to-process))))))
      (multiple-value-bind (leading-blocks rest-of-blocks)
          (leading-component-blocks component)
        ;; Update every block once to account for changes in the
        ;; IR1. The constraints of the lead blocks cannot be changed
        ;; after the first pass so we might as well use them and skip
        ;; USE-RESULT-CONSTRAINTS later.
        (dolist (block leading-blocks)
          (setf (block-in block) (compute-block-in block))
          (find-block-type-constraints block t))
        (setq blocks-to-process (copy-list rest-of-blocks))
        ;; The rest of the blocks.
        (dolist (block rest-of-blocks)
          (aver (eq block (pop blocks-to-process)))
          (setf (block-in block) (compute-block-in block))
          (enqueue (find-block-type-constraints block nil)))
        ;; Propagate constraints
        (loop for block = (pop blocks-to-process)
              while block do
              (unless (eq block (component-tail component))
                (when (update-block-in block)
                  (enqueue (find-block-type-constraints block nil)))))
        rest-of-blocks))))

(defun constraint-propagate (component)
  (declare (type component component))
  (init-var-constraints component)
  ;; Previous results can confuse propagation and may loop forever
  (do-blocks (block component)
    (setf (block-out block) nil)
    (let ((last (block-last block)))
      (when (if-p last)
        (setf (if-alternative-constraints last) nil)
        (setf (if-consequent-constraints last) nil))))
  (setf (block-out (component-head component)) (make-conset))
  (dolist (block (find-and-propagate-constraints component))
    (unless (block-delete-p block)
      (use-result-constraints block)))

  (values))
