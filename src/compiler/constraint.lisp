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
(defvar *blocks-to-terminate*)

(defstruct (vector-length-constraint
            (:constructor make-vector-length-constraint (var))
            (:copier nil))
  (var nil :type lambda-var :read-only t))

(deftype constraint-y () '(or ctype lvar lambda-var constant
                           vector-length-constraint))

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
  ;; >, <, >=, <=, or =
  ;;     X is a lambda-var and Y is a CTYPE. The relation holds
  ;;     between X and some object of type Y.
  ;;
  ;; EQL
  ;;     X is a LAMBDA-VAR and Y is a LVAR, a LAMBDA-VAR or a CONSTANT.
  ;;     The relation is asserted to hold.
  ;;
  ;; EQUALITY
  ;;     Relations between two variables.
  ;;
  ;; SET
  ;;    A set indicating that the initial binding is not in effect.
  (kind nil :type (member typep < > = >= <= eql equality set))
  ;; The operands to the relation.
  (x nil :type (or lambda-var vector-length-constraint))
  (y nil :type constraint-y)
  ;; If true, negates the sense of the constraint, so the relation
  ;; does *not* hold.
  (not-p nil :type boolean))

(defstruct (equality-constraint
            (:include constraint)
            (:constructor make-equality-constraint
                (number operator x y not-p &optional (amount 0)
                 &aux (kind 'equality))))
  (operator nil :type symbol)
  (amount 0 :type integer))

;;; The basic interval type. It can handle open and closed intervals.
;;; A bound is open if it is a list containing a number, just like
;;; Lisp says. NIL means unbounded.
(defstruct (interval (:constructor %make-interval (low high))
                     (:copier nil))
  low high)
(declaim (freeze-type interval))


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

  #+sb-devel
  (defprinter (conset)
    vector)

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

  (declaim (inline conset-member))
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

  (defun conset-delete (constraint conset)
    (setf (sbit (conset-vector conset) (%constraint-number constraint)) 0)
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
              conset-1)))
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
          (let ((vec (ensure-vec (lambda-var-inheritable-constraints x))))
            (vector-push-extend con vec)))
         (lambda-var
          (let ((vec (if (constraint-not-p con)
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

(declaim (inline type-for-constraints-p))
(defun type-for-constraints-p (type)
  (not (or (eq type *universal-type*)
           (contains-hairy-type-p type))))

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
(defmacro do-conset-elements ((constraint conset &optional result) &body body)
  (let ((index (gensym "INDEX"))
        (conset-vector (gensym "CONSET-VECTOR"))
        (universe (gensym "UNIVERSE")))
    `(let ((,conset-vector (conset-vector ,conset))
           (,universe *constraint-universe*))
       (declare (optimize speed))
       (loop for ,index from (conset-min ,conset) below (conset-max ,conset)
             do (when (plusp (sbit ,conset-vector ,index))
                  (let ((,constraint (aref ,universe ,index)))
                    ,@body))
             finally (return ,result)))))

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
                 (,max (conset-max ,conset))
                 (vector #-sb-xc-host (truly-the simple-vector (%array-data ,constraints))
                          #+sb-xc-host ,constraints))
             #-sb-xc-host
             (declare (optimize (insert-array-bounds-checks 0)))
             (loop for i below (length ,constraints)
                   for constraint = (aref vector i)
                   do (let ((number (truly-the index (constraint-number (truly-the constraint constraint)))))
                        (when (and (<= ,min number)
                                   (< number ,max)
                                   (conset-member constraint ,conset))
                          (body constraint))))))
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
             (con (,conset (lambda-var-eql-var-constraints ,variable)))
           (body-fun con))
         (do-conset-constraints-intersection
             (con (,conset (lambda-var-inheritable-constraints ,variable)) ,result)
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
                                   (constraint-not-p con)))))))
  (inherit-equality-constraints vars from-var constraints target))

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
           (ok-lvar-lambda-var (cast-value use) constraints))
          ((set-p use)
           (let ((leaf (set-var use)))
             (when (and (lambda-var-p leaf)
                        (lambda-var-constraints leaf))
               (and leaf
                    (conset-lvar-lambda-var-eql-p constraints lvar leaf)
                    leaf)))))))
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
                                         quick-p &optional (nth-value 0))
  (flet ((add (fun lvar y &optional no-complement)
           (let ((x (ok-lvar-lambda-var lvar constraints)))
             (if no-complement
                 (when x
                   (add-test-constraint quick-p
                                        fun x y nil
                                        constraints
                                        consequent-constraints))
                 (add-complement-constraints quick-p
                                             fun x y nil
                                             constraints
                                             consequent-constraints
                                             alternative-constraints)))
           (constraint-propagate-back lvar fun y constraints consequent-constraints
                                      (and (not no-complement)
                                           alternative-constraints)))
         (prop (triples target)
           (map nil (lambda (constraint)
                      (cond ((not constraint))
                            ((eq (car constraint) 'equality)
                             (destructuring-bind (op x y) (cdr constraint)
                               (add-equality-constraint op x y constraints target nil)))
                            (t
                             (destructuring-bind (kind x y &optional not-p)
                                 constraint
                               (when (and kind x y)
                                 (let ((x (if (lvar-p x)
                                              (ok-lvar-lambda-var x constraints)
                                              x)))
                                   (when x
                                     (add-test-constraint quick-p
                                                          kind x y
                                                          not-p constraints
                                                          target))))))))
                triples)))
    (when (eq (combination-kind use) :known)
      (binding* ((info (combination-fun-info use) :exit-if-null)
                 (propagate (fun-info-constraint-propagate-if info) :exit-if-null))
        (multiple-value-bind (lvar type if else no-complement)
            (funcall propagate use constraints nth-value)
          (prop if consequent-constraints)
          (prop else alternative-constraints)
          (when (and lvar type)
            (add 'typep lvar type no-complement)
            (return-from add-combination-test-constraints)))))
    (let* ((name (lvar-fun-name
                  (basic-combination-fun use)))
           (args (basic-combination-args use))
           (ptype (gethash name *backend-predicate-types*)))
      (when ptype
        (add 'typep (first args) ptype)))))

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
      (labels ((add (fun x y not-p &optional lvar)
                 (add-complement-constraints quick-p
                                             fun x y not-p
                                             constraints
                                             consequent-constraints
                                             alternative-constraints)
                 (when lvar
                   (constraint-propagate-back lvar fun y constraints consequent-constraints alternative-constraints)))
               (process-node (node)
                 (typecase node
                   (ref
                    (add 'typep (ok-lvar-lambda-var (ref-lvar node) constraints)
                         (specifier-type 'null) t)
                    (let ((use (principal-lvar-ref-use (ref-lvar use))))
                      (if use
                          (unless (ref-p use)
                            (process-node use))
                          (multiple-value-bind (node nth-value) (mv-principal-lvar-ref-use (ref-lvar node))
                            (when (combination-p node)
                              (add-combination-test-constraints node constraints
                                                                consequent-constraints
                                                                alternative-constraints
                                                                quick-p
                                                                nth-value))))))
                   (combination
                    (unless (eq (combination-kind node) :error)
                      (let ((name (uncross
                                   (lvar-fun-name
                                    (basic-combination-fun node))))
                            (args (basic-combination-args node)))
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
                                          (let ((*compiler-error-context* node))
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
                                         (nth-value 1 (lvar-value arg2))
                                         nil
                                         arg1))
                                   (t
                                    (add-test-constraint quick-p
                                                         'typep var1 (lvar-type arg2)
                                                         nil constraints
                                                         consequent-constraints)))))
                          ((< >)
                           (when (= (length args) 2)
                             (let* ((arg1 (first args))
                                    (var1 (ok-lvar-lambda-var arg1 constraints))
                                    (arg2 (second args))
                                    (var2 (ok-lvar-lambda-var arg2 constraints)))
                               (when var1
                                 (add name var1 (lvar-type arg2) nil))
                               (when var2
                                 (add (if (eq name '<) '> '<) var2 (lvar-type arg1) nil)))))
                          ((<= >=)
                           (when (= (length args) 2)
                             (let* ((arg1 (first args))
                                    (var1 (ok-lvar-lambda-var arg1 constraints))
                                    (arg2 (second args))
                                    (var2 (ok-lvar-lambda-var arg2 constraints)))
                               (when var1
                                 (add name var1 (lvar-type arg2) nil))
                               (when var2
                                 (add (if (eq name '<=) '>= '<=) var2 (lvar-type arg1) nil)))))
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
                           (add-combination-test-constraints node constraints
                                                             consequent-constraints
                                                             alternative-constraints
                                                             quick-p)))))))))
        (process-node use))
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

(defun constrain-real (y greater or-equal)
  (let ((int (type-approximate-interval y)))
    (when int
      (flet ((exclude (x)
               (cond ((not x) nil)
                     (or-equal x)
                     ((consp x) x)
                     (t (list x))))
             (bound (x)
               (if greater
                   (interval-low x)
                   (interval-high x))))
        (let ((bound (exclude (bound int))))
          (when bound
            (if greater
                (make-numeric-type :low bound)
                (make-numeric-type :high bound))))))))

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

;;; Compute the tightest type possible for a variable given a set of
;;; CONSTRAINTS.
(defun type-from-constraints (variable constraints initial-type &optional ref)
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
  (let ((type        initial-type)
        (constrain-symbols
          (and ref
               (policy ref (or (> speed compilation-speed)
                               (> debug 1)))))
        (not-type    *empty-type*)
        (not-xset     nil)
        (not-numeric nil)
        not-characters
        (not-fpz     '())
        set)
    (flet ((note-not (x)
             (if (fp-zero-p x)
                 (push x not-fpz)
                 (when (or constrain-symbols (null x) (not (symbolp x)))
                   (when (null not-xset)
                     (setf not-xset (alloc-xset)))
                   (add-to-xset x not-xset))))
           (intersect-result (other-type)
             (setf type (type-approx-intersection2 type other-type))))
      (declare (inline intersect-result))
      (do-propagatable-constraints (con (constraints variable))
        (let* ((kind (constraint-kind con))
               (x (constraint-x con))
               (y (constraint-y con))
               (not-p (constraint-not-p con))
               (other (if (eq x variable) y x)))
          (case kind
            (typep
             (cond ((not not-p)
                    (intersect-result other))
                   ((member-type-p other)
                    (mapc-member-type-members #'note-not other))
                   (t
                    (setq not-type (type-union not-type other)))))
            (eql
             (let ((other-type (leaf-type other)))
               (cond ((not (type-for-constraints-p other-type)))
                     ((not not-p)
                      (when (and ref
                                 (constant-p other))
                        (change-ref-leaf ref other)
                        (return-from type-from-constraints))
                      (intersect-result other-type))
                     ((constant-p other)
                      (cond ((member-type-p other-type)
                             (note-not (constant-value other)))
                            ;; Numeric types will produce interesting
                            ;; negations, other than just "not equal"
                            ;; which can be handled by the equality
                            ;; constraints.
                            ((numeric-type-p other-type)
                             (when (null not-numeric)
                               (setf not-numeric (alloc-xset)))
                             (add-to-xset (constant-value other) not-numeric))
                            ((typep other-type 'character-set-type)
                             (push (constant-value other) not-characters)))))))
            ((< > <= >= =)
             (let ((after (type-after-comparison kind not-p type y)))
               (when after
                 (setf type after))))
            (set
             (setf set t))))))
    (cond ((and ref
                (and (if-p (node-dest ref))
                     (or (and not-xset
                              (xset-member-p nil not-xset))
                         (csubtypep (specifier-type 'null) not-type))))
           (setf (node-derived-type ref) *wild-type*)
           (change-ref-leaf ref (find-constant t)))
          (t
           (let* ((not-union not-type)
                  (not-union (if (and (null not-xset) (null not-fpz))
                               not-union
                               (let ((excluded (make-member-type
                                                (or not-xset (alloc-xset)) not-fpz)))
                                 (type-union not-union excluded))))
                  (numeric (when not-numeric
                             (contiguous-numeric-set-type not-numeric)))
                  (not-union (if numeric
                               (type-union not-union numeric)
                               not-union))
                  (not-union (if not-characters
                                 (type-union not-union
                                             (sb-kernel::character-set-type-from-characters not-characters))
                                 not-union)))
             (values (if (eq not-union *empty-type*)
                         type
                         (type-difference type not-union))
                     set))))))

(defun type-after-comparison (operator not-p current-type type)
  (case operator
    ((= eq)
     (multiple-value-bind (lo hi)
         (if (numeric-type-p type)
             (values (numeric-type-low type)
                     (numeric-type-high type))
             ;; Doesn't handle infinities
             (let ((int (type-approximate-interval type)))
               (and int
                    (values
                     (interval-low int)
                     (interval-high int)))))
       (if not-p
           (when (and (csubtypep current-type (specifier-type 'integer))
                      (integerp hi)
                      (eql lo hi))
             ;; Cut off an adjacent bound if it's not EQ
             (multiple-value-bind (c-lo)
                 (let ((int (type-approximate-interval current-type)))
                   (and int
                        (values
                         (interval-low int)
                         (interval-high int))))
               (when (and c-lo
                          (= hi c-lo))
                 (type-intersection current-type
                                    (make-numeric-type :low (1+ lo))))))
           (when (or lo hi)
             (type-intersection current-type
                                (type-union (make-numeric-type :low lo
                                                               :high hi)
                                            (make-numeric-type :complexp :complex
                                                               :low lo
                                                               :high hi)))))))
    (t
     (multiple-value-bind (greater equal)
         (if not-p
             (case operator
               (> (values nil t))
               (< (values t t))
               (>= (values nil nil))
               (<= (values t nil)))
             (case operator
               (> (values t nil))
               (< (values nil nil))
               (>= (values t t))
               (<= (values nil t))))
       (cond
         ((and (integer-type-p current-type) (integer-type-p type))
          (constrain-integer-type current-type type greater equal))
         ((and (float-type-p current-type) (float-type-p type))
          (constrain-float-type current-type type greater equal))
         ((integer-type-p type)
          (let ((type (constrain-real-to-integer type greater equal)))
            (when type
              (type-intersection current-type type))))
         (t
          (let ((type (constrain-real type greater equal)))
            (when type
              (type-intersection current-type type)))))))))

;;; Given the set of CONSTRAINTS for a variable and the current set of
;;; restrictions from flow analysis IN, set the type for REF
;;; accordingly.
(defun constrain-ref-type (ref in)
  (declare (type ref ref) (type conset in))
  (let ((leaf (ref-leaf ref)))
    (multiple-value-bind (type set)
        (type-from-constraints leaf in (single-value-type (node-derived-type ref))
                               ref)
      (when type
        (unless set
          (setf (lambda-var-unused-initial-value leaf) nil))
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
               (when (eq (node-derived-type ref) *empty-type*)
                 ;; Terminating blocks early may leave loops with
                 ;; just one entry point, resulting in
                 ;; monotonically growing variables without a
                 ;; starting point which will propagate new
                 ;; constraints for each increment.
                 (pushnew ref *blocks-to-terminate*))))
        ;; Find unchanged eql refs to a set variable.
        (when (lambda-var-sets leaf)
          (let (mark
                (eq (lambda-var-eq-constraints leaf)))
            (when eq
              (loop for other-ref in (leaf-refs leaf)
                    unless (eq other-ref ref)
                    do (let ((constraint (gethash (ref-lvar other-ref) eq)))
                         (when (and constraint
                                    (conset-member constraint in))
                           (unless mark
                             (setf mark (list 0))
                             (setf (ref-same-refs ref) mark))
                           (setf (ref-same-refs other-ref) mark)))))
            (when mark
              (reoptimize-node ref))))))))

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
         (functional-kind-case fun
           (let
            (loop with call = (lvar-dest (node-lvar (first (lambda-refs fun))))
                  for var in (lambda-vars fun)
                  and val in (combination-args call)
                  when (and val (lambda-var-constraints var))
                  do (let ((type (lvar-type val)))
                       (when (type-for-constraints-p type)
                         (conset-add-constraint gen 'typep var type nil)))
                     (maybe-add-eql-var-var-constraint var val gen)
                     (add-var-result-constraints var val gen)))
           (mv-let
            (add-mv-let-result-constraints (lvar-dest (node-lvar (first (lambda-refs fun)))) fun gen)))))
      (ref
       (when (ok-ref-lambda-var node)
         (maybe-add-eql-var-lvar-constraint node gen)
         (when preprocess-refs-p
           (constrain-ref-type node gen))))
      (delay)
      (cast
       (let* ((lvar (cast-value node))
              (var (ok-lvar-lambda-var lvar gen))
              (atype (single-value-type (cast-derived-type node))))
         (when (and var
                    (type-for-constraints-p atype))
           (conset-add-constraint-to-eql gen 'typep var atype nil))
         (constraint-propagate-back lvar 'typep atype gen gen nil)))
      (cset
       (binding* ((var (set-var node))
                  (nil (lambda-var-p var) :exit-if-null)
                  (nil (lambda-var-constraints var) :exit-if-null))
         (when (policy node (or (and (= speed 3) (> speed compilation-speed))
                                (> debug 1)))
           (let ((type (lambda-var-type var)))
             (when (type-for-constraints-p type)
               (do-eql-vars (other (var gen))
                 (unless (eql other var)
                   (conset-add-constraint gen 'typep other type nil))))))

         (let ((new (add-set-constraints var (set-value node) gen)))
           (conset-clear-lambda-var gen var)
           (when new
             (conset-union gen new)))

         (let ((type (single-value-type (node-derived-type node))))
           (when (type-for-constraints-p type)
             (conset-add-constraint gen 'typep var type nil)))
         (unless (policy node (> compilation-speed speed))
           (maybe-add-eql-var-var-constraint var (set-value node) gen))
         (add-eq-constraint var (set-value node) gen)
         (conset-add-constraint gen 'set var var nil)
         (when (node-lvar node)
           (conset-add-lvar-lambda-var-eql gen (node-lvar node) var))))
      (combination
       (case (combination-kind node)
         (:known
          (unless (and preprocess-refs-p
                       (try-equality-constraint node gen))
            (let ((lvar (node-lvar node)))
              (when lvar
                (add-var-result-constraints lvar lvar gen)))
            (binding* ((info (combination-fun-info node) :exit-if-null)
                       (propagate (fun-info-constraint-propagate info)
                                  :exit-if-null)
                       (constraints (funcall propagate node gen))
                       (register (if (policy node
                                         (> compilation-speed speed))
                                     #'conset-add-constraint
                                     #'conset-add-constraint-to-eql)))
              (map nil (lambda (constraint)
                         (destructuring-bind (kind x y &optional not-p) constraint
                           (when (and kind x y)
                             (funcall register gen
                                      kind x y
                                      not-p))))
                   constraints))))
         (:local
          (let ((fun (combination-lambda node))
                (call-in (combination-constraints-in node)))

            (when (and (functional-kind-eq fun nil assignment optional cleanup)
                       (not (and call-in
                                 (conset= call-in gen))))
              (setf (combination-constraints-in node)
                    (copy-conset gen))
              (when (boundp '*constraint-blocks*)
                (enqueue-block-for-constraints (lambda-block fun))))))))))
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
                   (setf (lambda-var-constraints var) (make-conset))))
               (when (and (lambda-var-constraints var)
                          (lambda-var-sets var))
                 (when (block-type-check (lambda-block fun))
                   ;; This is optimistic, make sure it's going to be
                   ;; processed.
                   (setf (lambda-var-unused-initial-value var) t))
                 (loop for ref in (lambda-var-refs var)

                       do (setf (ref-same-refs ref) nil))))))
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

;;; Join the constraints coming from the predecessors of BLOCK on
;;; every constrained variable into the constraint set IN.
(defun join-type-constraints (in block &optional equality-only predecessor-outs)
  (let ((vars '())
        (equality-vars)
        (predecessors (block-pred block)))
    (flet ((find-vars (out)
             (do-conset-elements (con out)
               (let ((kind  (constraint-kind con))
                     (y     (constraint-y con))
                     (not-p (constraint-not-p con)))
                 (when (and (not equality-only)
                            (or (member kind '(typep < >))
                                (and (eq kind 'eql) (or (not not-p)
                                                        (constant-p y)))
                                (and (eq kind '=) (and (numeric-type-p y)
                                                       (not not-p)))))
                   (pushnew (constraint-x con) vars))
                 (when (and (eq kind 'equality)
                            (/= (equality-constraint-amount con) 0))
                   (pushnew (constraint-x con) equality-vars :test #'eq)
                   (when (lambda-var/vector-length-p y)
                     (pushnew y equality-vars)))))))
      (cond (predecessor-outs
             (find-vars (car predecessor-outs)))
            (t
             (loop for pred in predecessors
                   do
                   (let ((out (block-out-for-successor pred block)))
                     (when out
                       (find-vars out)
                       (return)))))))
    (dolist (var vars)
      (let ((in-var-type *empty-type*))
        (flet ((compute-type (out)
                 (setq in-var-type
                       (type-union in-var-type
                                   (type-from-constraints var out *universal-type*)))))
          (if predecessor-outs
              (dolist (out predecessor-outs)
                (compute-type out))
              (dolist (pred predecessors)
                (let ((out (block-out-for-successor pred block)))
                  (when out
                    (compute-type out))))))

        (when (type-for-constraints-p in-var-type)
          ;; Remove the existing constraints to avoid joining them again later.
          (do-propagatable-constraints (con (in var))
            (when (eq (constraint-kind con) 'typep)
              (conset-delete con in)))
          (conset-adjoin (find-or-create-constraint 'typep
                                                    var
                                                    in-var-type
                                                    nil)
                         in))))
    (dolist (var equality-vars)
      (join-equality-constraints var block in))))

(defun compute-block-in (block join-types-p)
  (let ((in nil)
        (bind (block-start-node block)))
    (cond
      ;; Use constraints from the local calls to this function
      ((and (bind-p bind)
            (functional-kind-eq (bind-lambda bind) nil assignment optional cleanup))
       (let ((fun (bind-lambda bind))
             (outs))
         (loop for ref in (lambda-refs fun)
               for call = (node-dest ref)
               for call-in = (and call
                                  (combination-constraints-in call))
               when call-in
               do (if in
                      (conset-intersection in call-in)
                      (setf in (copy-conset call-in)))
                  (push call-in outs))
         (when (rest outs)
           (join-type-constraints in block (not join-types-p) outs))))
      (t
       (dolist (pred (block-pred block))
         ;; If OUT has not been calculated, assume it to be the universal
         ;; set.
         (let ((out (block-out-for-successor pred block)))
           (when out
             (if in
                 (conset-intersection in out)
                 (setq in (copy-conset out))))))
       (when (rest (block-pred block))
         (join-type-constraints in block (not join-types-p)))))
    (or in (make-conset))))

(defun update-block-in (block join-types-p)
  (let ((in (compute-block-in block join-types-p)))
    (cond ((and (block-in block) (conset= in (block-in block)))
           nil)
          (t
           (setf (block-in block) in)))))

;;; Return two lists: one of blocks that precede all loops and
;;; therefore require only one constraint propagation pass and the
;;; rest.
;;;
;;; Trailing blocks that succeed all loops could be found and handled
;;; similarly. In practice though, these more complex solutions are
;;; slightly worse performancewise.
(defun leading-component-blocks (component)
  (declare (type component component))
  (collect ((leading-blocks) (rest-of-blocks))
    (do-blocks (block component)
      (let ((leading t)
            (bind (block-start-node block))
            fun)
        (if (and (bind-p bind)
                 (functional-kind-eq (setf fun (bind-lambda bind)) nil assignment optional cleanup))
            (loop for ref in (lambda-refs fun)
                  for call = (node-dest ref)
                  for call-block = (and call
                                        (node-block call))
                  do (unless (and call-block
                                  (block-flag call-block))
                       (setq leading nil)))
            (dolist (pred (block-pred block))
              (unless (block-flag pred)
                (setq leading nil))))
        (setf (block-flag block) leading)
        (when (block-type-check block)
          (if leading
              (leading-blocks block)
              (rest-of-blocks block)))))
    (values (leading-blocks) (rest-of-blocks))))

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

(defvar *constraint-blocks*)

(defun enqueue-block-for-constraints (block)
  (when (block-type-check block)
    (setq *constraint-blocks* (nconc-new block *constraint-blocks*))))

(defun find-and-propagate-constraints (component)
  (clear-flags component)
  (multiple-value-bind (leading-blocks rest-of-blocks)
      (leading-component-blocks component)
    ;; Update every block once to account for changes in the
    ;; IR1. The constraints of the lead blocks cannot be changed
    ;; after the first pass so we might as well use them and skip
    ;; USE-RESULT-CONSTRAINTS later.
    (dolist (block leading-blocks)
      (setf (block-in block) (compute-block-in block t))
      (find-block-type-constraints block t))
    ;; We can only start joining types on blocks in which
    ;; constraint propagation might have to run multiple times (to
    ;; fixpoint) once all type constraints are definitely
    ;; correct. They may not be the first time around because EQL
    ;; constraint propagation is optimistic, i.e. un-EQL variables
    ;; may be considered EQL before constraint propagation is
    ;; done, hence any inherited type constraints from such
    ;; constraints will be wrong as well.
    (dolist (join-types-p '(nil t))
      (let ((*constraint-blocks* (copy-list rest-of-blocks)))
        ;; The rest of the blocks.
        (dolist (block rest-of-blocks)
          (aver (eq block (pop *constraint-blocks*)))
          (setf (block-in block) (compute-block-in block join-types-p))
          (mapc #'enqueue-block-for-constraints
                (find-block-type-constraints block nil)))
        ;; Propagate constraints
        (loop for block = (pop *constraint-blocks*)
              while block do
              (unless (or (block-delete-p block)
                          (eq block (component-tail component)))
                (when (update-block-in block join-types-p)
                  (mapc #'enqueue-block-for-constraints
                        (find-block-type-constraints block nil)))))))

    rest-of-blocks))

(defun constraint-propagate (component)
  (declare (type component component))
  (setf (block-out (component-head component)) (make-conset))
  (init-var-constraints component)

  ;; Previous results can confuse propagation and may loop forever
  (do-blocks (block component)
    (setf (block-out block) nil)
    (let ((last (block-last block)))
      (when (if-p last)
        (setf (if-alternative-constraints last) nil)
        (setf (if-consequent-constraints last) nil))))

  (let (*blocks-to-terminate*)
    (dolist (block (find-and-propagate-constraints component))
      (unless (block-delete-p block)
        (use-result-constraints block)))
    #+sb-devel
    (when (and *compiler-trace-output*
               (memq :constraints *compile-trace-targets*))
      (print-constraints component))
    (loop for node in *blocks-to-terminate*
          do (maybe-terminate-block node nil)))
  (values))
