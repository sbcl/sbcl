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
;;; fail to infer (< I FIXNUM): is does not understand that this
;;; constraint follows from (TYPEP I (INTEGER 0 0)).

;;; BUGS:
;;;
;;; -- this code does not check whether SET appears between REF and a
;;; test (bug 233b)

(in-package "SB!C")

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
  ;;     X is a LAMBDA-VAR Y is a LAMBDA-VAR or a CONSTANT. The
  ;;     relation is asserted to hold.
  (kind nil :type (member typep < > eql))
  ;; The operands to the relation.
  (x nil :type lambda-var)
  (y nil :type (or ctype lambda-var constant))
  ;; If true, negates the sense of the constraint, so the relation
  ;; does *not* hold.
  (not-p nil :type boolean))

(defvar *constraint-number*)

;;; Return a constraint for the specified arguments. We only create a
;;; new constraint if there isn't already an equivalent old one,
;;; guaranteeing that all equivalent constraints are EQ. This
;;; shouldn't be called on LAMBDA-VARs with no CONSTRAINTS set.
(defun find-constraint (kind x y not-p)
  (declare (type lambda-var x) (type (or constant lambda-var ctype) y)
	   (type boolean not-p))
  (or (etypecase y
	(ctype
	 (do-sset-elements (con (lambda-var-constraints x) nil)
	   (when (and (eq (constraint-kind con) kind)
		      (eq (constraint-not-p con) not-p)
		      (type= (constraint-y con) y))
	     (return con))))
	(constant
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
	     (return con)))))
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

;;; If LVAR's USE is a REF, then return OK-REF-LAMBDA-VAR of the USE,
;;; otherwise NIL.
#!-sb-fluid (declaim (inline ok-lvar-lambda-var))
(defun ok-lvar-lambda-var (lvar)
  (declare (type lvar lvar))
  (let ((use (lvar-uses lvar)))
    (when (ref-p use)
      (ok-ref-lambda-var use))))

;;;; Searching constraints

;;; Add the indicated test constraint to BLOCK, marking the block as
;;; having a new assertion when the constriant was not already
;;; present. We don't add the constraint if the block has multiple
;;; predecessors, since it only holds on this particular path.
(defun add-test-constraint (block fun x y not-p)
  (unless (rest (block-pred block))
    (let ((con (find-constraint fun x y not-p))
	  (old (or (block-test-constraint block)
		   (setf (block-test-constraint block) (make-sset)))))
      (when (sset-adjoin con old)
	(setf (block-type-asserted block) t))))
  (values))

;;; Add complementary constraints to the consequent and alternative
;;; blocks of IF. We do nothing if X is NIL.
(defun add-complement-constraints (if fun x y not-p)
  (when (and x
	     ;; Note: Even if we do (IF test exp exp) => (PROGN test exp)
	     ;; optimization, the *MAX-OPTIMIZE-ITERATIONS* cutoff means
	     ;; that we can't guarantee that the optimization will be
	     ;; done, so we still need to avoid barfing on this case.
             (not (eq (if-consequent if)
                      (if-alternative if))))
    (add-test-constraint (if-consequent if) fun x y not-p)
    (add-test-constraint (if-alternative if) fun x y (not not-p)))
  (values))

;;; Add test constraints to the consequent and alternative blocks of
;;; the test represented by USE.
(defun add-test-constraints (use if)
  (declare (type node use) (type cif if))
  (typecase use
    (ref
     (add-complement-constraints if 'typep (ok-ref-lambda-var use)
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
                  (add-complement-constraints if 'typep
                                              (ok-lvar-lambda-var (first args))
                                              (if (ctype-p val)
                                                  val
                                                  (specifier-type val))
                                              nil)))))
           ((eq eql)
            (let* ((var1 (ok-lvar-lambda-var (first args)))
                   (arg2 (second args))
                   (var2 (ok-lvar-lambda-var arg2)))
              (cond ((not var1))
                    (var2
                     (add-complement-constraints if 'eql var1 var2 nil))
                    ((constant-lvar-p arg2)
                     (add-complement-constraints if 'eql var1
                                                 (ref-leaf
                                                  (lvar-uses arg2))
                                                 nil)))))
           ((< >)
            (let* ((arg1 (first args))
                   (var1 (ok-lvar-lambda-var arg1))
                   (arg2 (second args))
                   (var2 (ok-lvar-lambda-var arg2)))
              (when var1
                (add-complement-constraints if name var1 (lvar-type arg2)
                                            nil))
              (when var2
                (add-complement-constraints if (if (eq name '<) '> '<)
                                            var2 (lvar-type arg1)
                                            nil))))
           (t
            (let ((ptype (gethash name *backend-predicate-types*)))
              (when ptype
                (add-complement-constraints if 'typep
                                            (ok-lvar-lambda-var (first args))
                                            ptype nil)))))))))
  (values))

;;; Set the TEST-CONSTRAINT in the successors of BLOCK according to
;;; the condition it tests.
(defun find-test-constraints (block)
  (declare (type cblock block))
  (let ((last (block-last block)))
    (when (if-p last)
      (let ((use (lvar-uses (if-test last))))
	(when (node-p use)
	  (add-test-constraints use last)))))

  (setf (block-test-modified block) nil)
  (values))

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
  #+sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
  x
  #-sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
  (labels ((exclude (x)
	     (cond ((not x) nil)
		   (or-equal x)
		   (greater
		    (if (consp x)
			(car x)
			x))
		   (t
		    (if (consp x)
			x
			(list x)))))
	   (bound (x)
	     (if greater (numeric-type-low x) (numeric-type-high x)))
	   (max-lower-bound (x y)
	     ;; Both X and Y are not null. Find the max.
	     (let ((res (max (type-bound-number x) (type-bound-number y))))
	       ;; An open lower bound is greater than a close
	       ;; lower bound because the open bound doesn't
	       ;; contain the bound, so choose an open lower
	       ;; bound.
	       (set-bound res (or (consp x) (consp y)))))
	   (min-upper-bound (x y)
	     ;; Same as above, but for the min of upper bounds
	     ;; Both X and Y are not null. Find the min.
	     (let ((res (min (type-bound-number x) (type-bound-number y))))
	       ;; An open upper bound is less than a closed
	       ;; upper bound because the open bound doesn't
	       ;; contain the bound, so choose an open lower
	       ;; bound.
	       (set-bound res (or (consp x) (consp y))))))
    (let* ((x-bound (bound x))
	   (y-bound (exclude (bound y)))
	   (new-bound (cond ((not x-bound)
			     y-bound)
			    ((not y-bound)
			     x-bound)
			    (greater
			     (max-lower-bound x-bound y-bound))
			    (t
			     (min-upper-bound x-bound y-bound)))))
      (if greater
	  (modified-numeric-type x :low new-bound)
	  (modified-numeric-type x :high new-bound)))))

;;; Given the set of CONSTRAINTS for a variable and the current set of
;;; restrictions from flow analysis IN, set the type for REF
;;; accordingly.
(defun constrain-ref-type (ref constraints in)
  (declare (type ref ref) (type sset constraints in))
  (let ((var-cons (copy-sset constraints)))
    (sset-intersection var-cons in)
    (let ((res (single-value-type (node-derived-type ref)))
	  (not-res *empty-type*)
	  (leaf (ref-leaf ref)))
      (do-sset-elements (con var-cons)
	(let* ((x (constraint-x con))
	       (y (constraint-y con))
	       (not-p (constraint-not-p con))
	       (other (if (eq x leaf) y x))
	       (kind (constraint-kind con)))
	  (case kind
	    (typep
	     (if not-p
		 (setq not-res (type-union not-res other))
		 (setq res (type-approx-intersection2 res other))))
	    (eql
	     (let ((other-type (leaf-type other)))
	       (if not-p
		   (when (and (constant-p other)
			      (member-type-p other-type))
		     (setq not-res (type-union not-res other-type)))
		   (let ((leaf-type (leaf-type leaf)))
		     (when (or (constant-p other)
			       (and (csubtypep other-type leaf-type)
				    (not (type= other-type leaf-type))))
		       (change-ref-leaf ref other)
		       (when (constant-p other) (return)))))))
	    ((< >)
	     (cond ((and (integer-type-p res) (integer-type-p y))
		    (let ((greater (eq kind '>)))
		      (let ((greater (if not-p (not greater) greater)))
			(setq res
			      (constrain-integer-type res y greater not-p)))))
		   ((and (float-type-p res) (float-type-p y))
		    (let ((greater (eq kind '>)))
		      (let ((greater (if not-p (not greater) greater)))
			(setq res
			      (constrain-float-type res y greater not-p)))))
		   )))))

      (cond ((and (if-p (node-dest ref))
                  (csubtypep (specifier-type 'null) not-res))
             (setf (node-derived-type ref) *wild-type*)
             (change-ref-leaf ref (find-constant t)))
            (t
             (derive-node-type ref
                               (make-single-value-type
                                (or (type-difference res not-res)
                                    res)))))))

  (values))

;;;; Flow analysis

;;; Local propagation
;;; -- [TODO: For any LAMBDA-VAR ref with a type check, add that
;;;    constraint.]
;;; -- For any LAMBDA-VAR set, delete all constraints on that var; add
;;;    a type constraint based on the new value type.
(declaim (ftype (function (cblock sset
                           &key (:ref-preprocessor function)
                                (:set-preprocessor function))
                          sset)
                constraint-propagate-in-block))
(defun constraint-propagate-in-block
    (block gen &key ref-preprocessor set-preprocessor)

  (let ((test (block-test-constraint block)))
    (when test
      (sset-union gen test)))

  (do-nodes (node lvar block)
    (typecase node
      (bind
       (let ((fun (bind-lambda node)))
         (when (eq (functional-kind fun) :let)
           (loop with call = (lvar-dest (node-lvar (first (lambda-refs fun))))
                 for var in (lambda-vars fun)
                 and val in (combination-args call)
                 when (and val
                           (lambda-var-constraints var)
                           ;; if VAR has no SETs, type inference is
                           ;; fully performed by IR1 optimizer
                           (lambda-var-sets var))
                 do (let* ((type (lvar-type val))
                           (con (find-constraint 'typep var type nil)))
                      (sset-adjoin con gen))))))
      (ref
       (let ((var (ok-ref-lambda-var node)))
         (when var
           (when ref-preprocessor
             (funcall ref-preprocessor node gen))
           (let ((dest (and lvar (lvar-dest lvar))))
             (when (cast-p dest)
               (let* ((atype (single-value-type (cast-derived-type dest))) ; FIXME
                      (con (find-constraint 'typep var atype nil)))
                 (sset-adjoin con gen)))))))
      (cset
       (binding* ((var (set-var node))
                  (nil (lambda-var-p var) :exit-if-null)
                  (cons (lambda-var-constraints var) :exit-if-null))
         (when set-preprocessor
           (funcall set-preprocessor var))
         (sset-difference gen cons)
         (let* ((type (single-value-type (node-derived-type node)))
                (con (find-constraint 'typep var type nil)))
           (sset-adjoin con gen))))))

  gen)

;;; BLOCK-KILL is just a list of the LAMBDA-VARs killed, so we must
;;; compute the kill set when there are any vars killed. We bum this a
;;; bit by special-casing when only one var is killed, and just using
;;; that var's constraints as the kill set. This set could possibly be
;;; precomputed, but it would have to be invalidated whenever any
;;; constraint is added, which would be a pain.
(defun compute-block-out (block)
  (declare (type cblock block))
  (let ((in (block-in block))
        (kill (block-kill block))
        (out (copy-sset (block-gen block))))
    (cond ((null kill)
	   (sset-union out in))
	  ((null (rest kill))
	   (let ((con (lambda-var-constraints (first kill))))
	     (if con
		 (sset-union-of-difference out in con)
		 (sset-union out in))))
	  (t
	   (let ((kill-set (make-sset)))
	     (dolist (var kill)
	       (let ((con (lambda-var-constraints var)))
		 (when con
		   (sset-union kill-set con))))
	     (sset-union-of-difference out in kill-set))))
    out))

;;; Compute the initial flow analysis sets for BLOCK:
;;; -- Compute IN/OUT sets; if OUT of a predecessor is not yet
;;;    computed, assume it to be a universal set (this is only
;;;    possible in a loop)
;;;
;;; Return T if we have found a loop.
(defun find-block-type-constraints (block)
  (declare (type cblock block))
  (collect ((kill nil adjoin))
    (let ((gen (constraint-propagate-in-block
                block (make-sset)
                :set-preprocessor (lambda (var)
                                    (kill var)))))
      (setf (block-gen block) gen)
      (setf (block-kill block) (kill))
      (setf (block-type-asserted block) nil)
      (let* ((n (block-number block))
             (pred (block-pred block))
             (in nil)
             (loop-p nil))
        (dolist (b pred)
          (cond ((> (block-number b) n)
                 (if in
                     (sset-intersection in (block-out b))
                     (setq in (copy-sset (block-out b)))))
                (t (setq loop-p t))))
        (unless in
          (bug "Unreachable code is found or flow graph is not ~
                properly depth-first ordered."))
        (setf (block-in block) in)
        (setf (block-out block) (compute-block-out block))
        loop-p))))

;;; BLOCK-IN becomes the intersection of the OUT of the predecessors.
;;; Our OUT is:
;;;     gen U (in - kill)
;;;
;;; Return True if we have done something.
(defun flow-propagate-constraints (block)
  (let* ((pred (block-pred block))
	 (in (progn (aver pred)
                    (let ((res (copy-sset (block-out (first pred)))))
                      (dolist (b (rest pred))
                        (sset-intersection res (block-out b)))
                      res))))
    (setf (block-in block) in)
    (let ((out (compute-block-out block)))
      (if (sset= out (block-out block))
          nil
          (setf (block-out block) out)))))

;;; Deliver the results of constraint propagation to REFs in BLOCK.
;;; During this pass, we also do local constraint propagation by
;;; adding in constraints as we seem them during the pass through the
;;; block.
(defun use-result-constraints (block)
  (declare (type cblock block))
  (constraint-propagate-in-block
   block (block-in block)
   :ref-preprocessor (lambda (node cons)
                       (let* ((var (ref-leaf node))
                              (con (lambda-var-constraints var)))
                         (constrain-ref-type node con cons)))))

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

;;; How many blocks does COMPONENT have?
(defun component-n-blocks (component)
  (let ((result 0))
    (declare (type index result))
    (do-blocks (block component :both)
      (incf result))
    result))

(defun constraint-propagate (component &aux (loop-p nil))
  (declare (type component component))
  (init-var-constraints component)

  (do-blocks (block component)
    (when (block-test-modified block)
      (find-test-constraints block)))

  (unless (block-out (component-head component))
    (setf (block-out (component-head component)) (make-sset)))

  (do-blocks (block component)
    (when (find-block-type-constraints block)
      (setq loop-p t)))

  (when loop-p
    (let (;; If we have to propagate changes more than this many times,
          ;; something is wrong.
          (max-n-changes-remaining (component-n-blocks component)))
      (declare (type fixnum max-n-changes-remaining))
      (loop (aver (>= max-n-changes-remaining 0))
         (decf max-n-changes-remaining)
         (let ((did-something nil))
           (do-blocks (block component)
             (when (flow-propagate-constraints block)
               (setq did-something t)))
           (unless did-something
             (return))))))

  (do-blocks (block component)
    (use-result-constraints block))

  (values))
