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

(in-package "SB!C")

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
  ;; > or <
  ;;     X is a lambda-var and Y is a CTYPE. The relation holds
  ;;     between X and some object of type Y.
  ;;
  ;; EQL
  ;;     X is a LAMBDA-VAR and Y is a LVAR, a LAMBDA-VAR or a CONSTANT.
  ;;     The relation is asserted to hold.
  (kind nil :type (member typep < > eql))
  ;; The operands to the relation.
  (x nil :type lambda-var)
  (y nil :type constraint-y)
  ;; If true, negates the sense of the constraint, so the relation
  ;; does *not* hold.
  (not-p nil :type boolean))

(defvar *constraint-number*)
(declaim (type (integer 0) *constraint-number*))

(defun find-constraint (kind x y not-p)
  (declare (type lambda-var x) (type constraint-y y) (type boolean not-p))
  (etypecase y
    (ctype
     (do-sset-elements (con (lambda-var-constraints x) nil)
       (when (and (eq (constraint-kind con) kind)
                  (eq (constraint-not-p con) not-p)
                  (type= (constraint-y con) y))
         (return con))))
    ((or lvar constant)
     (do-sset-elements (con (lambda-var-constraints x) nil)
       (when (and (eq (constraint-kind con) kind)
                  (eq (constraint-not-p con) not-p)
                  (eq (constraint-y con) y))
         (return con))))
    (lambda-var
     (do-sset-elements (con (lambda-var-constraints x) nil)
       (when (and (eq (constraint-kind con) kind)
                  (eq (constraint-not-p con) not-p)
                  (let ((cx (constraint-x con)))
                    (eq (if (eq cx x)
                            (constraint-y con)
                            cx)
                        y)))
         (return con))))))

;;; Return a constraint for the specified arguments. We only create a
;;; new constraint if there isn't already an equivalent old one,
;;; guaranteeing that all equivalent constraints are EQ. This
;;; shouldn't be called on LAMBDA-VARs with no CONSTRAINTS set.
(defun find-or-create-constraint (kind x y not-p)
  (declare (type lambda-var x) (type constraint-y y) (type boolean not-p))
  (or (find-constraint kind x y not-p)
      (let ((new (make-constraint (incf *constraint-number*) kind x y not-p)))
        (sset-adjoin new (lambda-var-constraints x))
        (when (lambda-var-p y)
          (sset-adjoin new (lambda-var-constraints y)))
        new)))

;;; If REF is to a LAMBDA-VAR with CONSTRAINTs (i.e. we can do flow
;;; analysis on it), then return the LAMBDA-VAR, otherwise NIL.
#!-sb-fluid (declaim (inline ok-ref-lambda-var))
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
             (when lambda-var
               (let ((constraint (find-constraint 'eql lambda-var lvar nil)))
                 (when (and constraint (sset-member constraint constraints))
                   lambda-var)))))
          ((cast-p use)
           (ok-lvar-lambda-var (cast-value use) constraints)))))

(defmacro do-eql-vars ((symbol (var constraints) &optional result) &body body)
  (once-only ((var var))
    `(let ((,symbol ,var))
       (flet ((body-fun ()
                ,@body))
         (body-fun)
         (do-sset-elements (con ,constraints ,result)
           (let ((other (and (eq (constraint-kind con) 'eql)
                             (eq (constraint-not-p con) nil)
                             (cond ((eq ,var (constraint-x con))
                                    (constraint-y con))
                                   ((eq ,var (constraint-y con))
                                    (constraint-x con))
                                   (t
                                    nil)))))
             (when other
               (setq ,symbol other)
               (when (lambda-var-p ,symbol)
                 (body-fun)))))))))

;;;; Searching constraints

;;; Add the indicated test constraint to BLOCK. We don't add the
;;; constraint if the block has multiple predecessors, since it only
;;; holds on this particular path.
(defun add-test-constraint (fun x y not-p constraints target)
  (cond ((and (eq 'eql fun) (lambda-var-p y) (not not-p))
         (add-eql-var-var-constraint x y constraints target))
        (t
         (do-eql-vars (x (x constraints))
           (let ((con (find-or-create-constraint fun x y not-p)))
             (sset-adjoin con target)))))
  (values))

;;; Add complementary constraints to the consequent and alternative
;;; blocks of IF. We do nothing if X is NIL.
(defun add-complement-constraints (fun x y not-p constraints
                                   consequent-constraints
                                   alternative-constraints)
  (when x
    (add-test-constraint fun x y not-p constraints
                         consequent-constraints)
    (add-test-constraint fun x y (not not-p) constraints
                         alternative-constraints))
  (values))

;;; Add test constraints to the consequent and alternative blocks of
;;; the test represented by USE.
(defun add-test-constraints (use if constraints)
  (declare (type node use) (type cif if))
  ;; Note: Even if we do (IF test exp exp) => (PROGN test exp)
  ;; optimization, the *MAX-OPTIMIZE-ITERATIONS* cutoff means that we
  ;; can't guarantee that the optimization will be done, so we still
  ;; need to avoid barfing on this case.
  (unless (eq (if-consequent if) (if-alternative if))
    (let ((consequent-constraints (make-sset))
          (alternative-constraints (make-sset)))
      (macrolet ((add (fun x y not-p)
                   `(add-complement-constraints ,fun ,x ,y ,not-p
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
             (let ((name (lvar-fun-name
                          (basic-combination-fun use)))
                   (args (basic-combination-args use)))
               (case name
                 ((%typep %instance-typep)
                  (let ((type (second args)))
                    (when (constant-lvar-p type)
                      (let ((val (lvar-value type)))
                        (add 'typep
                             (ok-lvar-lambda-var (first args) constraints)
                             (if (ctype-p val)
                                 val
                                 (specifier-type val))
                             nil)))))
                 ((eq eql)
                  (let* ((arg1 (first args))
                         (var1 (ok-lvar-lambda-var arg1 constraints))
                         (arg2 (second args))
                         (var2 (ok-lvar-lambda-var arg2 constraints)))
                    ;; The code below assumes that the constant is the
                    ;; second argument in case of variable to constant
                    ;; comparision which is sometimes true (see source
                    ;; transformations for EQ, EQL and CHAR=). Fixing
                    ;; that would result in more constant substitutions
                    ;; which is not a universally good thing, thus the
                    ;; unnatural asymmetry of the tests.
                    (cond ((not var1)
                           (when var2
                             (add-test-constraint 'typep var2 (lvar-type arg1)
                                                  nil constraints
                                                  consequent-constraints)))
                          (var2
                           (add 'eql var1 var2 nil))
                          ((constant-lvar-p arg2)
                           (add 'eql var1 (ref-leaf (principal-lvar-use arg2))
                                nil))
                          (t
                           (add-test-constraint 'typep var1 (lvar-type arg2)
                                                nil constraints
                                                consequent-constraints)))))
                 ((< >)
                  (let* ((arg1 (first args))
                         (var1 (ok-lvar-lambda-var arg1 constraints))
                         (arg2 (second args))
                         (var2 (ok-lvar-lambda-var arg2 constraints)))
                    (when var1
                      (add name var1 (lvar-type arg2) nil))
                    (when var2
                      (add (if (eq name '<) '> '<) var2 (lvar-type arg1) nil))))
                 (t
                  (let ((ptype (gethash name *backend-predicate-types*)))
                    (when ptype
                      (add 'typep (ok-lvar-lambda-var (first args) constraints)
                           ptype nil))))))))))
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
(defun constrain-float-type (x y greater or-equal)
  (declare (type numeric-type x y))
  (declare (ignorable x y greater or-equal)) ; for CROSS-FLOAT-INFINITY-KLUDGE

  (aver (eql (numeric-type-class x) 'float))
  (aver (eql (numeric-type-class y) 'float))
  #+sb-xc-host                    ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
  x
  #-sb-xc-host                    ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
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
                   ((and or-equal
                         (= (type-bound-number x) (type-bound-number ref)))
                    ;; X is tighter if REF is not an open bound and X is
                    (and (not (consp ref)) (consp x)))
                   (greater
                    (< (type-bound-number ref) (type-bound-number x)))
                   (t
                    (> (type-bound-number ref) (type-bound-number x))))))
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

;;; Given the set of CONSTRAINTS for a variable and the current set of
;;; restrictions from flow analysis IN, set the type for REF
;;; accordingly.
(defun constrain-ref-type (ref constraints in)
  (declare (type ref ref) (type sset constraints in))
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
        (not-fpz nil)
        (not-res *empty-type*)
        (leaf (ref-leaf ref)))
    (flet ((note-not (x)
             (if (fp-zero-p x)
                 (push x not-fpz)
                 (when (or constrain-symbols (null x) (not (symbolp x)))
                   (add-to-xset x not-set)))))
      (do-sset-elements (con constraints)
        (when (sset-member con in)
          (let* ((x (constraint-x con))
                 (y (constraint-y con))
                 (not-p (constraint-not-p con))
                 (other (if (eq x leaf) y x))
                 (kind (constraint-kind con)))
            (case kind
              (typep
               (if not-p
                   (if (member-type-p other)
                       (mapc-member-type-members #'note-not other)
                       (setq not-res (type-union not-res other)))
                   (setq res (type-approx-intersection2 res other))))
              (eql
               (unless (lvar-p other)
                 (let ((other-type (leaf-type other)))
                   (if not-p
                       (when (and (constant-p other)
                                  (member-type-p other-type))
                         (note-not (constant-value other)))
                       (let ((leaf-type (leaf-type leaf)))
                         (cond
                           ((or (constant-p other)
                                (and (leaf-refs other) ; protect from
                                        ; deleted vars
                                     (csubtypep other-type leaf-type)
                                     (not (type= other-type leaf-type))))
                            (change-ref-leaf ref other)
                            (when (constant-p other) (return)))
                           (t
                            (setq res (type-approx-intersection2
                                       res other-type)))))))))
              ((< >)
               (cond
                 ((and (integer-type-p res) (integer-type-p y))
                  (let ((greater (eq kind '>)))
                    (let ((greater (if not-p (not greater) greater)))
                      (setq res
                            (constrain-integer-type res y greater not-p)))))
                 ((and (float-type-p res) (float-type-p y))
                  (let ((greater (eq kind '>)))
                    (let ((greater (if not-p (not greater) greater)))
                      (setq res
                            (constrain-float-type res y greater not-p))))))))))))
    (cond ((and (if-p (node-dest ref))
                (or (xset-member-p nil not-set)
                    (csubtypep (specifier-type 'null) not-res)))
           (setf (node-derived-type ref) *wild-type*)
           (change-ref-leaf ref (find-constant t)))
          (t
           (setf not-res
                 (type-union not-res (make-member-type :xset not-set :fp-zeroes not-fpz)))
           (derive-node-type ref
                             (make-single-value-type
                              (or (type-difference res not-res)
                                  res)))
           (maybe-terminate-block ref nil))))
  (values))

;;;; Flow analysis

(defun maybe-add-eql-var-lvar-constraint (ref gen)
  (let ((lvar (ref-lvar ref))
        (leaf (ref-leaf ref)))
    (when (and (lambda-var-p leaf) lvar)
      (sset-adjoin (find-or-create-constraint 'eql leaf lvar nil)
                   gen))))

;;; Copy all CONSTRAINTS involving FROM-VAR - except the (EQL VAR
;;; LVAR) ones - to all of the variables in the VARS list.
(defun inherit-constraints (vars from-var constraints target)
  (do-sset-elements (con constraints)
    ;; Constant substitution is controversial.
    (unless (constant-p (constraint-y con))
      (dolist (var vars)
        (let ((eq-x (eq from-var (constraint-x con)))
              (eq-y (eq from-var (constraint-y con))))
          (when (or (and eq-x (not (lvar-p (constraint-y con))))
                    eq-y)
            (sset-adjoin (find-or-create-constraint
                          (constraint-kind con)
                          (if eq-x var (constraint-x con))
                          (if eq-y var (constraint-y con))
                          (constraint-not-p con))
                         target)))))))

;; Add an (EQL LAMBDA-VAR LAMBDA-VAR) constraint on VAR1 and VAR2 and
;; inherit each other's constraints.
(defun add-eql-var-var-constraint (var1 var2 constraints
                                   &optional (target constraints))
  (let ((con (find-or-create-constraint 'eql var1 var2 nil)))
    (when (sset-adjoin con target)
      (collect ((eql1) (eql2))
        (do-eql-vars (var1 (var1 constraints))
          (eql1 var1))
        (do-eql-vars (var2 (var2 constraints))
          (eql2 var2))
        (inherit-constraints (eql1) var2 constraints target)
        (inherit-constraints (eql2) var1 constraints target))
      t)))

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
(declaim (ftype (function (cblock sset boolean)
                          sset)
                constraint-propagate-in-block))
(defun constraint-propagate-in-block (block gen preprocess-refs-p)
  (do-nodes (node lvar block)
    (typecase node
      (bind
       (let ((fun (bind-lambda node)))
         (when (eq (functional-kind fun) :let)
           (loop with call = (lvar-dest (node-lvar (first (lambda-refs fun))))
                 for var in (lambda-vars fun)
                 and val in (combination-args call)
                 when (and val (lambda-var-constraints var))
                 do (let* ((type (lvar-type val))
                           (con (find-or-create-constraint 'typep var type
                                                           nil)))
                      (sset-adjoin con gen))
                 (maybe-add-eql-var-var-constraint var val gen)))))
      (ref
       (when (ok-ref-lambda-var node)
         (maybe-add-eql-var-lvar-constraint node gen)
         (when preprocess-refs-p
           (let* ((var (ref-leaf node))
                  (con (lambda-var-constraints var)))
             (constrain-ref-type node con gen)))))
      (cast
       (let ((lvar (cast-value node)))
         (let ((var (ok-lvar-lambda-var lvar gen)))
           (when var
             (let ((atype (single-value-type (cast-derived-type node)))) ;FIXME
               (do-eql-vars (var (var gen))
                 (let ((con (find-or-create-constraint 'typep var atype nil)))
                   (sset-adjoin con gen))))))))
      (cset
       (binding* ((var (set-var node))
                  (nil (lambda-var-p var) :exit-if-null)
                  (cons (lambda-var-constraints var) :exit-if-null))
         (sset-difference gen cons)
         (let* ((type (single-value-type (node-derived-type node)))
                (con (find-or-create-constraint 'typep var type nil)))
           (sset-adjoin con gen))
         (maybe-add-eql-var-var-constraint var (set-value node) gen)))))
  gen)

(defun constraint-propagate-if (block gen)
  (let ((node (block-last block)))
    (when (if-p node)
      (let ((use (lvar-uses (if-test node))))
        (when (node-p use)
          (add-test-constraints use node gen))))))

;;; Starting from IN compute OUT and (consequent/alternative
;;; constraints if the block ends with and IF). Return the list of
;;; successors that may need to be recomputed.
(defun find-block-type-constraints (block final-pass-p)
  (declare (type cblock block))
  (let ((gen (constraint-propagate-in-block
              block
              (if final-pass-p
                  (block-in block)
                  (copy-sset (block-in block)))
              final-pass-p)))
    (setf (block-gen block) gen)
    (multiple-value-bind (consequent-constraints alternative-constraints)
        (constraint-propagate-if block gen)
      (if consequent-constraints
          (let* ((node (block-last block))
                 (old-consequent-constraints (if-consequent-constraints node))
                 (old-alternative-constraints (if-alternative-constraints node))
                 (succ ()))
            ;; Add the consequent and alternative constraints to GEN.
            (cond ((sset-empty consequent-constraints)
                   (setf (if-consequent-constraints node) gen)
                   (setf (if-alternative-constraints node) gen))
                  (t
                   (setf (if-consequent-constraints node) (copy-sset gen))
                   (sset-union (if-consequent-constraints node)
                               consequent-constraints)
                   (setf (if-alternative-constraints node) gen)
                   (sset-union (if-alternative-constraints node)
                               alternative-constraints)))
            ;; Has the consequent been changed?
            (unless (and old-consequent-constraints
                         (sset= (if-consequent-constraints node)
                                old-consequent-constraints))
              (push (if-consequent node) succ))
            ;; Has the alternative been changed?
            (unless (and old-alternative-constraints
                         (sset= (if-alternative-constraints node)
                                old-alternative-constraints))
              (push (if-alternative node) succ))
            succ)
          ;; There is no IF.
          (unless (and (block-out block)
                       (sset= gen (block-out block)))
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
               (unless (lambda-var-constraints var)
                 (when (or (null (lambda-var-sets var))
                           (not (closure-var-p var)))
                   (setf (lambda-var-constraints var) (make-sset)))))))
      (frob fun)
      (dolist (let (lambda-lets fun))
        (frob let)))))

;;; Return the constraints that flow from PRED to SUCC. This is
;;; BLOCK-OUT unless PRED ends with and IF and test constraints were
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
              (sset-intersection in out)
              (setq in (copy-sset out))))))
    (or in (make-sset))))

(defun update-block-in (block)
  (let ((in (compute-block-in block)))
    (cond ((and (block-in block) (sset= in (block-in block)))
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
        (when (and (not seen-loop-p) (loopy-p block))
          (setq seen-loop-p t))
        (if seen-loop-p
            (push block rest-of-blocks)
            (push block leading-blocks)))
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
               (setq blocks-to-process (nconc-new block blocks-to-process)))))
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

  (unless (block-out (component-head component))
    (setf (block-out (component-head component)) (make-sset)))

  (dolist (block (find-and-propagate-constraints component))
    (unless (block-delete-p block)
      (use-result-constraints block)))

  (values))
