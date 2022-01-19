;;;; This file implements the IR1 optimization phase of the compiler.
;;;; IR1 optimization is a grab-bag of optimizations that don't make
;;;; major changes to the block-level control flow and don't use flow
;;;; analysis. These optimizations can mostly be classified as
;;;; "meta-evaluation", but there is a sizable top-down component as
;;;; well.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; interface for obtaining results of constant folding

;;; Return true for an LVAR whose sole use is a reference to a
;;; constant leaf.
(defun constant-lvar-p (lvar)
  (declare (type lvar lvar))
  (let* ((type (lvar-type lvar))
         (principal-lvar (principal-lvar lvar))
         (principal-use (lvar-uses principal-lvar))
         leaf)
    (or (and (ref-p principal-use)
             (constant-p (setf leaf (ref-leaf principal-use)))
             ;; LEAF may be a CONSTANT behind a cast that will
             ;; later turn out to be of the wrong type.
             ;; And ir1-transforms suffer from this because
             ;; they expect LVAR-VALUE to be of a restricted type.
             (or (not (lvar-reoptimize principal-lvar))
                 (ctypep (constant-value leaf) type)))
        ;; check for EQL types and singleton numeric types
        (values (type-singleton-p type)))))

(defun constant-lvar-ignore-types-p (lvar)
  (declare (type lvar lvar))
  (let ((use (principal-lvar-use lvar)))
    (or (and (ref-p use)
             (constant-p (ref-leaf use)))
        ;; check for EQL types and singleton numeric types
        (values (type-singleton-p (lvar-type lvar))))))

;;; Are all the uses constant?
(defun constant-lvar-uses-p (lvar)
  (declare (type lvar lvar))
  (let* ((type (lvar-type lvar))
         (principal-lvar (principal-lvar lvar))
         (uses (lvar-uses principal-lvar))
         leaf)
    (when (consp uses)
      (loop for use in uses
            always
            (and (ref-p use)
                 (constant-p (setf leaf (ref-leaf use)))
                 ;; LEAF may be a CONSTANT behind a cast that will
                 ;; later turn out to be of the wrong type.
                 ;; And ir1-transforms suffer from this because
                 ;; they expect LVAR-VALUE to be of a restricted type.
                 (or (not (lvar-reoptimize principal-lvar))
                     (ctypep (constant-value leaf) type)))))))

;;; Return the constant value for an LVAR whose only use is a constant
;;; node.
(defun lvar-value (lvar)
  (declare (type lvar lvar))
  (let ((use  (principal-lvar-use lvar))
        (type (lvar-type lvar))
        leaf)
    (if (and (ref-p use)
             (constant-p (setf leaf (ref-leaf use))))
        (constant-value leaf)
        (multiple-value-bind (constantp value) (type-singleton-p type)
          (unless constantp
            (error "~S used on non-constant LVAR ~S" 'lvar-value lvar))
          value))))

(defun lvar-value-is-nil (lvar)
  (and (constant-lvar-p lvar) (null (lvar-value lvar))))

;;; Return true if ARG is NIL, or is a constant-lvar whose
;;; value is NIL, false otherwise.
(defun unsupplied-or-nil (arg)
  (declare (type (or lvar null) arg))
  (or (not arg) (lvar-value-is-nil arg)))

(defun lvar-uses-values (lvar)
  (declare (type lvar lvar))
  (let ((uses (principal-lvar-use lvar)))
    (loop for use in uses
          for leaf = (ref-leaf use)
          collect (constant-value leaf))))

;;;; interface for obtaining results of type inference

;;; Our best guess for the type of this lvar's value. Note that this
;;; may be VALUES or FUNCTION type, which cannot be passed as an
;;; argument to the normal type operations. See LVAR-TYPE.
;;;
;;; The result value is cached in the LVAR-%DERIVED-TYPE slot. If the
;;; slot is true, just return that value, otherwise recompute and
;;; stash the value there.
(defmacro lvar-type-using (lvar accessor)
  `(let ((uses (lvar-uses ,lvar)))
     (cond ((null uses) *empty-type*)
           ((listp uses)
            (do ((res (,accessor (first uses))
                      (values-type-union (,accessor (first current))
                                         res))
                 (current (rest uses) (rest current)))
                ((or (null current) (eq res *wild-type*))
                 res)))
           (t
            (,accessor uses)))))

(defun %lvar-derived-type (lvar)
  (lvar-type-using lvar node-derived-type))

;;; Return the derived type for LVAR's first value. This is guaranteed
;;; not to be a VALUES or FUNCTION type.
(declaim (ftype (sfunction (lvar) ctype) lvar-type))
(defun lvar-type (lvar)
  (single-value-type (lvar-derived-type lvar)))

;;; LVAR-CONSERVATIVE-TYPE
;;;
;;; Certain types refer to the contents of an object, which can
;;; change without type derivation noticing: CONS types and ARRAY
;;; types suffer from this:
;;;
;;;  (let ((x (the (cons fixnum fixnum) (cons a b))))
;;;     (setf (car x) c)
;;;     (+ (car x) (cdr x)))
;;;
;;; Python doesn't realize that the SETF CAR can change the type of X -- so we
;;; cannot use LVAR-TYPE which gets the derived results. Worse, still, instead
;;; of (SETF CAR) we might have a call to a user-defined function FOO which
;;; does the same -- so there is no way to use the derived information in
;;; general.
;;;
;;; So, the conservative option is to use the derived type if the leaf has
;;; only a single ref -- in which case there cannot be a prior call that
;;; mutates it. Otherwise we use the declared type or punt to the most general
;;; type we know to be correct for sure.
(defun lvar-conservative-type (lvar)
  (let ((derived-type (lvar-type lvar))
        (t-type *universal-type*))
    ;; Recompute using NODE-CONSERVATIVE-TYPE instead of derived type if
    ;; necessary -- picking off some easy cases up front.
    (cond ((or (eq derived-type t-type)
               ;; Can't use CSUBTYPEP!
               (type= derived-type (specifier-type 'list))
               (type= derived-type (specifier-type 'null)))
           derived-type)
          ((and (cons-type-p derived-type)
                (eq t-type (cons-type-car-type derived-type))
                (eq t-type (cons-type-cdr-type derived-type)))
           derived-type)
          ((and (array-type-p derived-type)
                (or (not (array-type-complexp derived-type))
                    (let ((dimensions (array-type-dimensions derived-type)))
                      (or (eq '* dimensions)
                          (every (lambda (dim) (eq '* dim)) dimensions)))))
           derived-type)
          ((type-needs-conservation-p derived-type)
           (single-value-type (lvar-type-using lvar node-conservative-type)))
          (t
           derived-type))))

(defun node-conservative-type (node)
  (let* ((derived-values-type (node-derived-type node))
         (derived-type (single-value-type derived-values-type)))
    (if (ref-p node)
        (let ((leaf (ref-leaf node)))
          (if (and (basic-var-p leaf)
                   (cdr (leaf-refs leaf)))
              (coerce-to-values
               (if (eq :declared (leaf-where-from leaf))
                   (let ((leaf-type (leaf-type leaf))
                         (cons-type (specifier-type 'cons)))
                     ;; If LEAF-TYPE is (or null some-cons-type) and
                     ;; DERIVED-TYPE is known to be non-null, use
                     ;; SOME-CONS-TYPE in that case, because a cons
                     ;; can't become null.
                     (if (csubtypep derived-type cons-type)
                         (type-intersection leaf-type cons-type)
                         leaf-type))
                   (conservative-type derived-type)))
              derived-values-type))
        derived-values-type)))

(defun conservative-type (type)
  (cond ((or (eq type *universal-type*)
             (eq type (specifier-type 'list))
             (eq type (specifier-type 'null)))
         type)
        ((cons-type-p type)
         (specifier-type 'cons))
        ((array-type-p type)
         (if (array-type-complexp type)
             (make-array-type
              ;; ADJUST-ARRAY may change dimensions, but rank stays same.
              (let ((old (array-type-dimensions type)))
                (if (eq '* old)
                    old
                    (mapcar (constantly '*) old)))
              ;; Complexity cannot change.
              :complexp (array-type-complexp type)
              ;; Element type cannot change.
              :element-type (array-type-element-type type)
              :specialized-element-type (array-type-specialized-element-type type))
             ;; Simple arrays cannot change at all.
             type))
        ((union-type-p type)
         ;; Conservative union type is an union of conservative types.
         (let ((res *empty-type*))
           (dolist (part (union-type-types type) res)
             (setf res (type-union res (conservative-type part))))))
        (t
         ;; Catch-all.
         ;;
         ;; If the type contains some CONS types, the conservative type contains all
         ;; of them.
         (when (types-equal-or-intersect type (specifier-type 'cons))
           (setf type (type-union type (specifier-type 'cons))))
         ;; Similarly for non-simple arrays -- it should be possible to preserve
         ;; more information here, but really...
         (let ((non-simple-arrays (specifier-type '(and array (not simple-array)))))
           (when (types-equal-or-intersect type non-simple-arrays)
             (setf type (type-union type non-simple-arrays))))
         type)))

(defun type-needs-conservation-p (type)
  (cond ((eq type *universal-type*)
         ;; Excluding T is necessary, because we do want type derivation to
         ;; be able to narrow it down in case someone (most like a macro-expansion...)
         ;; actually declares something as having type T.
         nil)
        ((or (cons-type-p type) (and (array-type-p type) (array-type-complexp type)))
         ;; Covered by the next case as well, but this is a quick test.
         t)
        ((types-equal-or-intersect type (specifier-type '(or cons (and array (not simple-array)))))
         t)))

;;; If LVAR is an argument of a function, return a type which the
;;; function checks LVAR for.
(defun lvar-externally-checkable-type (lvar)
  (declare (type lvar lvar))
  (let ((dest (lvar-dest lvar)))
    (when (combination-p dest)
      ;; TODO: MV-COMBINATION
      (let* ((fun (combination-fun dest))
             (fun-type (lvar-type fun)))
        (when (and (call-full-like-p dest)
                   (fun-type-p fun-type)
                   ;; FUN-TYPE might be (AND FUNCTION (SATISFIES ...)).
                   (not (fun-type-wild-args fun-type)))
          (map-combination-args-and-types
           (lambda (arg type &rest args)
             (declare (ignore args))
             (when (eq arg lvar)
               (return-from lvar-externally-checkable-type
                 (coerce-to-values type))))
           dest))))
    *wild-type*))

;;;; interface routines used by optimizers

;;; This function is called by optimizers to indicate that something
;;; interesting has happened to the value of LVAR. Optimizers must
;;; make sure that they don't call for reoptimization when nothing has
;;; happened, since optimization will fail to terminate.
;;;
;;; We clear any cached type for the lvar and set the reoptimize flags
;;; on everything in sight.
(defun reoptimize-lvar (lvar)
  (declare (type (or lvar null) lvar))
  (when lvar
    (setf (lvar-%derived-type lvar) nil)
    (let ((dest (lvar-dest lvar)))
      (when dest
        (setf (lvar-reoptimize lvar) t)
        (setf (node-reoptimize dest) t)
        (binding* (;; Since this may be called during IR1 conversion,
                   ;; PREV may be missing.
                   (prev (node-prev dest) :exit-if-null)
                   (block (ctran-block prev))
                   (component (block-component block)))
          (setf (block-reoptimize block) t)
          (reoptimize-component component :maybe))
        (loop for cast in (lvar-dependent-nodes lvar)
              unless (or (not (node-p cast))
                         (node-deleted cast))
              do
              (setf (node-reoptimize cast) t)
              (setf (block-reoptimize (node-block cast)) t)
              (reoptimize-component (node-component cast) :maybe)))))
  (values))

(defun reoptimize-node (node)
  (let* ((block (node-block node))
         (component (block-component block)))
    (setf (node-reoptimize node) t)
    (when (cast-p node)
      (do-uses (node (cast-value node))
        (reoptimize-node node)))
    (reoptimize-component component t)
    (setf (block-reoptimize block) t)))

;;; Annotate NODE to indicate that its result has been proven to be
;;; TYPEP to RTYPE. After IR1 conversion has happened, this is the
;;; only correct way to supply information discovered about a node's
;;; type. If you screw with the NODE-DERIVED-TYPE directly, then
;;; information may be lost and reoptimization may not happen.
;;;
;;; What we do is intersect RTYPE with NODE's DERIVED-TYPE. If the
;;; intersection is different from the old type, then we do a
;;; REOPTIMIZE-LVAR on the NODE-LVAR.
(defun derive-node-type (node rtype &key from-scratch)
  (declare (type valued-node node) (type ctype rtype))
  (let* ((initial-type (node-derived-type node))
         (node-type (if from-scratch
                        *wild-type*
                        initial-type)))
    (unless (eq initial-type rtype)
      (let ((int (values-type-intersection node-type rtype))
            (lvar (node-lvar node)))
        ;; Don't use type/=, it will return NIL on unknown types.
        ;; Instead of checking the second value just negate TYPE=
        (unless (type= initial-type int)
          (when (and *check-consistency*
                     (eq int *empty-type*)
                     (not (eq rtype *empty-type*)))
            (aver (not from-scratch))
            (let ((*compiler-error-context* node))
              (compiler-warn
               "New inferred type ~S conflicts with old type:~
                ~%  ~S~%*** possible internal error? Please report this."
               (type-specifier rtype) (type-specifier node-type))))
          (setf (node-derived-type node) int)
          ;; If the new type consists of only one object, replace the
          ;; node with a constant reference.
          (when (and (ref-p node)
                     (lambda-var-p (ref-leaf node)))
            (let ((type (single-value-type int)))
              (when (and (member-type-p type)
                         (eql (member-type-size type) 1)
                         (not (preserve-single-use-debug-var-p node (ref-leaf node))))
                (change-ref-leaf node (find-constant
                                       (first (member-type-members type)))))))
          (reoptimize-lvar lvar)))))
  (values))


(defun type-asserted-p (lvar type)
  (or (values-subtypep (lvar-derived-type lvar) type)
      ;; Just doing values-subtypep is not enough since it may be an
      ;; intersection of types. Need to see if there's a cast that
      ;; actually checks that particular type.
      (do-uses (node lvar t)
        (unless
            (typecase node
              (cast
               (values-subtypep (coerce-to-values (cast-asserted-type node)) type))
              (t
               (values-subtypep (node-derived-type node) type)))
          (return)))))

;;; This is similar to DERIVE-NODE-TYPE, but asserts that it is an
;;; error for LVAR's value not to be TYPEP to TYPE. We implement it
;;; splitting off DEST a new CAST node; old LVAR will deliver values
;;; to CAST. If we improve the assertion, we set TYPE-CHECK to
;;; guarantee that the new assertion will be checked.
(defun assert-lvar-type (lvar type policy &optional context)
  (declare (type lvar lvar) (type ctype type))
  (unless (type-asserted-p lvar type)
    (let ((internal-lvar (make-lvar))
          (dest (lvar-dest lvar)))
      (substitute-lvar internal-lvar lvar)
      (let ((cast (insert-cast-before dest lvar type policy
                                      context)))
        (use-lvar cast internal-lvar)
        t))))


;;;; IR1-OPTIMIZE

(declaim (start-block ir1-optimize ir1-optimize-last-effort))

;;; Do one forward pass over COMPONENT, deleting unreachable blocks
;;; and doing IR1 optimizations. We can ignore all blocks that don't
;;; have the REOPTIMIZE flag set. If COMPONENT-REOPTIMIZE is true when
;;; we are done, then another iteration would be beneficial.
(defun ir1-optimize (component fastp)
  (declare (type component component))
  (setf (component-reoptimize component) nil)
  (loop with block = (block-next (component-head component))
        with tail = (component-tail component)
        for last-block = block
        until (eq block tail)
        do (cond
             ((not (block-type-check block)))
             ;; We delete blocks when there is either no predecessor or the
             ;; block is in a lambda that has been deleted. These blocks
             ;; would eventually be deleted by DFO recomputation, but doing
             ;; it here immediately makes the effect available to IR1
             ;; optimization.
             ((or (block-delete-p block)
                  (null (block-pred block)))
              (delete-block-lazily block)
              (setq block (clean-component component block)))
             ((eq (functional-kind (block-home-lambda block)) :deleted)
              ;; Preserve the BLOCK-SUCC invariant that almost every block has
              ;; one successor (and a block with DELETE-P set is an acceptable
              ;; exception).
              (mark-for-deletion block)
              (setq block (clean-component component block)))
             (t
              (loop
                 (let ((succ (block-succ block)))
                   (unless (singleton-p succ)
                     (return)))

                 (let ((last (block-last block)))
                   (typecase last
                     (cif
                      (flush-dest (if-test last))
                      (when (unlink-node last)
                        (return)))
                     (exit
                      (when (maybe-delete-exit last)
                        (return)))))

                 (unless (join-successor-if-possible block)
                   (return)))
              (remove-equivalent-blocks block)
              (when (and (block-reoptimize block) (block-component block))
                (aver (not (block-delete-p block)))
                (cond (fastp
                       (ir1-optimize-block-fast block))
                      (t
                       (ir1-optimize-block block)
                       ;; Force any preceding IFs to get reoptimized, since
                       ;; some optimizations depend on the shape of the
                       ;; consequent blocks, not just IF-TEST.
                       (when (block-reoptimize block)
                         (loop for pred in (block-pred block)
                               for last = (block-last pred)
                               when (and (block-type-check pred)
                                         (if-p last))
                               do (reoptimize-node last))))))

              (cond ((and (block-delete-p block) (block-component block))
                     (setq block (clean-component component block)))
                    ((and (block-flush-p block) (block-component block))
                     (flush-dead-code block)))))
        do (when (eq block last-block)
             (setq block (block-next block))))

  (values))

(defun ir1-optimize-last-effort (component)
  (declare (type component component))
  (loop while (shiftf (component-reoptimize component) nil)
        do
        (do-blocks (block component)
          (when (and (block-reoptimize block)
                     (block-type-check block))
            (ir1-optimize-block-fast block)))))

(defun ir1-optimize-block-fast (block)
  (declare (type cblock block))
  (setf (block-reoptimize block) nil)
  (do-nodes (node nil block :restart-p t)
    (when (node-reoptimize node)
      (setf (node-reoptimize node) nil)
      (typecase node
        (combination
         (ir1-optimize-combination-fast node))
        (cif
         ;; Don't want comparisons of constants against constants
         ;; from reaching the VOPs.
         (ir1-optimize-if node t))))))

;;; Only handle constant folding, some VOPs do not work
;;; on constants.
(defun ir1-optimize-combination-fast (node)
  (let ((args (basic-combination-args node))
        (info (basic-combination-fun-info node))
        (kind (basic-combination-kind node)))
    (case kind
      (:known
       (cond ((constant-fold-call-p node)
              (constant-fold-call node))
             ((and (ir1-attributep (fun-info-attributes info) commutative)
                   (= (length args) 2)
                   (constant-lvar-p (first args))
                   (not (constant-lvar-p (second args))))
              (setf (basic-combination-args node) (nreverse args))))))))

;;; Loop over the nodes in BLOCK, acting on (and clearing) REOPTIMIZE
;;; flags.
;;;
;;; Note that although they are cleared here, REOPTIMIZE flags might
;;; still be set upon return from this function, meaning that further
;;; optimization is wanted (as a consequence of optimizations we did).
(defun ir1-optimize-block (block)
  (declare (type cblock block))
  ;; We clear the node and block REOPTIMIZE flags before doing the
  ;; optimization, not after. This ensures that the node or block will
  ;; be reoptimized if necessary.
  (setf (block-reoptimize block) nil)
  (do-nodes (node nil block :restart-p t)
    (when (node-reoptimize node)
      ;; As above, we clear the node REOPTIMIZE flag before optimizing.
      (setf (node-reoptimize node) nil)
      (typecase node
        (ref)
        (combination
         ;; With a COMBINATION, we call PROPAGATE-FUN-CHANGE whenever
         ;; the function changes, and call IR1-OPTIMIZE-COMBINATION if
         ;; any argument changes.
         (ir1-optimize-combination node))
        (cif
         (ir1-optimize-if node))
        (creturn
         ;; KLUDGE: We leave the NODE-OPTIMIZE flag set going into
         ;; IR1-OPTIMIZE-RETURN, since IR1-OPTIMIZE-RETURN wants to
         ;; clear the flag itself. -- WHN 2002-02-02, quoting original
         ;; CMU CL comments
         (setf (node-reoptimize node) t)
         (ir1-optimize-return node))
        (mv-combination
         (ir1-optimize-mv-combination node))
        (exit
         ;; With an EXIT, we derive the node's type from the VALUE's
         ;; type.
         (let ((value (exit-value node)))
           (when value
             (derive-node-type node (if (lvar-single-value-p (node-lvar node))
                                        (lvar-type value)
                                        (lvar-derived-type value))))))
        (cset
         ;; PROPAGATE-FROM-SETS can do a better job if NODE-REOPTIMIZE
         ;; is accurate till the node actually has been reoptimized.
         (setf (node-reoptimize node) t)
         (ir1-optimize-set node))
        (cast
         (ir1-optimize-cast node)))))

  (values))

;;; Delete any nodes in BLOCK whose value is unused and which have no
;;; side effects. We can delete sets of lexical variables when the set
;;; variable has no references.
(defun flush-dead-code (block)
  (declare (type cblock block))
  (setf (block-flush-p block) nil)
  (do-nodes-backwards (node lvar block :restart-p t)
    (if lvar
        (do-uses (other-node lvar)
          (when (and (neq node other-node)
                     (eq block (node-block other-node)))
            ;; This must be a preceding node and the current node will
            ;; overwrite the value, unlink the lvar and the node will
            ;; get a chance to be deleted on one of the next iterations
            (delete-lvar-use other-node)))
        (typecase node
          (ref
           (delete-ref node)
           (unlink-node node))
          (combination
           (when (and (not (node-tail-p node))
                      (flushable-combination-p node))
             (flush-combination node)))
          (mv-combination
           (when (eq (basic-combination-kind node) :local)
             (let ((fun (combination-lambda node)))
               (when (dolist (var (lambda-vars fun) t)
                       (when (or (leaf-refs var)
                                 (lambda-var-sets var))
                         (return nil)))
                 (mapc #'flush-dest (basic-combination-args node))
                 (delete-let fun)))))
          (exit
           (let ((value (exit-value node)))
             (when value
               (flush-dest value)
               (setf (exit-value node) nil))))
          (cset
           (let ((var (set-var node)))
             (when (and (lambda-var-p var)
                        (null (leaf-refs var)))
               (flush-dest (set-value node))
               (setf (basic-var-sets var)
                     (delq1 node (basic-var-sets var)))
               (unlink-node node))))
          (cast
           (unless (or (cast-type-check node)
                       (and (bound-cast-p node)
                            (bound-cast-check node)))
             (flush-dest (cast-value node))
             (unlink-node node)))))))

(declaim (end-block))


;;;; local call return type propagation

;;; This function is called on RETURN nodes that have their REOPTIMIZE
;;; flag set. It iterates over the uses of the RESULT, looking for
;;; interesting stuff to update the TAIL-SET. If a use isn't a local
;;; call, then we union its type together with the types of other such
;;; uses. We assign to the RETURN-RESULT-TYPE the intersection of this
;;; type with the RESULT's asserted type. We can make this
;;; intersection now (potentially before type checking) because this
;;; assertion on the result will eventually be checked (if
;;; appropriate.)
;;;
;;; We call MAYBE-CONVERT-TAIL-LOCAL-CALL on each local non-MV
;;; combination, which may change the successor of the call to be the
;;; called function, and if so, checks if the call can become an
;;; assignment. If we convert to an assignment, we abort, since the
;;; RETURN has been deleted.
(defun find-result-type (node)
  (declare (type creturn node))
  (let ((result (return-result node)))
    (collect ((use-union *empty-type* values-type-union))
      (do-uses (use result)
        (let ((use-home (node-home-lambda use)))
          (cond ((or (eq (functional-kind use-home) :deleted)
                     (block-delete-p (node-block use))))
                ((not (and (basic-combination-p use)
                           (eq (basic-combination-kind use) :local)))
                 (use-union (node-derived-type use)))
                ((or (eq (functional-kind (combination-lambda use)) :deleted)
                     (block-delete-p (lambda-block (combination-lambda use)))))
                (t
                 (aver (eq (lambda-tail-set use-home)
                           (lambda-tail-set (combination-lambda use))))
                 (when (combination-p use)
                   (when (nth-value 1 (maybe-convert-tail-local-call use))
                     (return-from find-result-type t)))))))
      (let ((int
             ;; (values-type-intersection
             ;; (continuation-asserted-type result) ; FIXME -- APD, 2002-01-26
             (use-union)
              ;; )
              ))
        (setf (return-result-type node) int))))
  nil)

;;; Do stuff to realize that something has changed about the value
;;; delivered to a return node. Since we consider the return values of
;;; all functions in the tail set to be equivalent, this amounts to
;;; bringing the entire tail set up to date. We iterate over the
;;; returns for all the functions in the tail set, reanalyzing them
;;; all (not treating NODE specially.)
;;;
;;; When we are done, we check whether the new type is different from
;;; the old TAIL-SET-TYPE. If so, we set the type and also reoptimize
;;; all the lvars for references to functions in the tail set. This
;;; will cause IR1-OPTIMIZE-COMBINATION to derive the new type as the
;;; results of the calls.
(defun ir1-optimize-return (node)
  (declare (type creturn node))
  (let ((lambda (return-lambda node)))
    (tagbody
     :restart
       (let* ((tails (lambda-tail-set lambda))
              (funs (tail-set-funs tails)))
         (collect ((res *empty-type* values-type-union))
           (dolist (fun funs)
             (let ((return (lambda-return fun)))
               (when return
                 (when (node-reoptimize return)
                   (setf (node-reoptimize return) nil)
                   (when (find-result-type return)
                     (go :restart)))
                 (res (return-result-type return)))))

           (when (type/= (res) (tail-set-type tails))
             (setf (tail-set-type tails) (res))
             (dolist (fun (tail-set-funs tails))
               (dolist (ref (leaf-refs fun))
                 (reoptimize-lvar (node-lvar ref)))))))))

  (values))

;;;; IF optimization

(declaim (start-block ir1-optimize-if))

;;; Check whether the predicate is known to be true or false,
;;; deleting the IF node in favor of the appropriate branch when this
;;; is the case.
;;; Similarly, when both branches are equivalent, branch directly to either
;;; of them.
;;; Also, if the test has multiple uses, replicate the node when possible...
;;; in fact, splice in direct jumps to the right branch if possible.
(defun ir1-optimize-if (node &optional fast)
  (declare (type cif node))
  (let* ((test (if-test node))
         (block (node-block node))(type (lvar-type test))
         (consequent  (if-consequent  node))
         (alternative (if-alternative node))
         (victim
           (cond ((constant-lvar-p test)
                  (if (lvar-value test) alternative consequent))
                 ((not (types-equal-or-intersect type (specifier-type 'null)))
                  alternative)
                 ((type= type (specifier-type 'null))
                  consequent)
                 ((eq consequent alternative)
                  alternative)
                 ((and (not fast)
                       (or
                        (blocks-equivalent-p alternative consequent)
                        (if-test-redundant-p test consequent alternative)))
                  ;; Even if the references are the same they can have
                  ;; different derived types based on the TEST
                  ;; Don't lose the second type when killing it.
                  (let ((consequent-ref (block-start-node consequent)))
                    (derive-node-type consequent-ref
                                      (values-type-union
                                       (node-derived-type consequent-ref)
                                       (node-derived-type (block-start-node alternative)))
                                      :from-scratch t))
                  alternative))))
    (cond (victim
           (kill-if-branch-1 node test block victim))
          ((not fast)
           (tension-if-if-1 node test block)
           (duplicate-if-if-1 node test block)))))

;; When we know that we only have a single successor, kill the victim
;; ... unless the victim and the remaining successor are the same.
(defun kill-if-branch-1 (node test block victim)
  (declare (type cif node))
  (flush-dest test)
  (when (rest (block-succ block))
    (unlink-blocks block victim))
  (setf (component-reanalyze (node-component node)) t)
  (unlink-node node))

;; When if/if conversion would leave (if ... (if nil ...)) or
;; (if ... (if not-nil ...)), splice the correct successor right
;; in.
(defun tension-if-if-1 (node test block)
  (cond ((and (eq (block-start-node block) node)
              (listp (lvar-uses test)))
         (do-uses (use test)
           (when (immediately-used-p test use)
             (let* ((type (single-value-type (node-derived-type use)))
                    (target (if (type= type (specifier-type 'null))
                                (if-alternative node)
                                (multiple-value-bind (typep surep)
                                    (ctypep nil type)
                                  (and (not typep) surep
                                       (if-consequent node))))))
               (when target
                 (let ((pred (node-block use)))
                   (cond ((listp (lvar-uses test))
                          (change-block-successor pred block target)
                          (delete-lvar-use use))
                         (t
                          ;; only one use left. Just kill the now-useless
                          ;; branch to avoid spurious code deletion notes.
                          (aver (rest (block-succ block)))
                          (kill-if-branch-1
                           node test block
                           (if (eql target (if-alternative node))
                               (if-consequent node)
                               (if-alternative node)))
                          (return-from tension-if-if-1)))))))))
        ((ref-p (lvar-uses test))
         ;; TEST goes through a variable that is used by another IF
         ;; e.g. (or (and x y) z)
         (let* ((ref (lvar-uses test))
                (var (ref-leaf ref)))
           (when (and (lambda-var-p var)
                      (not (lambda-var-specvar var))
                      (eq (lambda-kind (lambda-var-home var)) :let)
                      (let-var-immediately-used-p ref var test)
                      ;; Rely on constraint propagation to determine
                      ;; that the var with the value of NIL is never
                      ;; used outside of the test itself.
                      ;; Otherwise would need to check if
                      ;; if-consequent is dominating the remaining references
                      (loop with null-type = (specifier-type 'null)
                            for other-ref in (leaf-refs var)
                            for lvar = (node-lvar other-ref)
                            always (or (eq other-ref ref)
                                       (not (types-equal-or-intersect (single-value-type (node-derived-type other-ref))
                                                                      null-type))
                                       (progn
                                         (and lvar
                                              ;; Make sure we get back here after node-derived-type
                                              (pushnew node (lvar-dependent-nodes lvar)
                                                       :test #'eq))
                                         nil))))
             (let* ((lvar (lambda-var-ref-lvar ref))
                    (lambda (lambda-var-home var))
                    (good-lambda-shape (= (length (lambda-vars lambda)) 1)))
               (when (and lvar
                          (listp (lvar-uses lvar)))
                 (do-uses (use lvar)
                   (let ((block (node-block use)))
                     (when (and (immediately-used-p lvar use)
                                (type= (single-value-type (node-derived-type use))
                                       (specifier-type 'null))
                                (eq (block-last block) use)
                                (or good-lambda-shape
                                    (setf good-lambda-shape (split-let var lambda))))
                       (change-block-successor block
                                               (car (block-succ block))
                                               (if-alternative node))
                       (delete-lvar-use use)))))))))))

;;; Split the last variable into a separate lambda.
(defun split-let (var original-lambda)
  (let* ((ref (car (leaf-refs original-lambda)))
         (call (and ref
                    (node-lvar ref)
                    (lvar-dest (node-lvar ref))))
         (all-vars (lambda-vars original-lambda)))
    (when (and call
               (eq var (car (last all-vars)))
               (notany #'lambda-var-specvar all-vars))
      (or (= (count-if #'identity (combination-args call)) 1)
          (with-ir1-environment-from-node call
            (let* ((all-args (combination-args call))
                   (penultimate-arg (find-if #'identity all-args
                                             :from-end t
                                             :end (1- (length all-args))))
                   (penultimate (lvar-uses penultimate-arg))
                   (penultimate (if (consp penultimate)
                                    (car penultimate)
                                    penultimate))
                   (next-block (or (node-ends-block penultimate)
                                   (car (block-succ (node-block penultimate)))))
                   (ctran (make-ctran :kind :block-start))
                   (new-block (make-block-key :start ctran
                                              :pred (block-pred next-block)
                                              :succ (list next-block)))
                   (bind (make-bind))
                   (vars (butlast all-vars))
                   (lambda (make-lambda :vars vars
                                        :kind :let
                                        :bind bind
                                        :home (lambda-home original-lambda)
                                        :%source-name 'split
                                        :%debug-name `(split ,(lambda-%debug-name original-lambda))))
                   (ref (make-ref lambda))
                   (args (butlast all-args)))
              (push lambda (lambda-lets (lambda-home original-lambda)))
              (push ref (lambda-refs lambda))
              (setf (combination-args call) (last all-args))
              (setf (lambda-vars original-lambda) (last all-vars)
                    (lambda-tail-set lambda) (make-tail-set :funs (list lambda)))
              (setf (bind-lambda bind) lambda)
              (loop for var in vars
                    do (setf (lambda-var-home var) lambda))
              (setf (ctran-block ctran) new-block)
              (loop for pred in (block-pred next-block)
                    do (setf (block-succ pred)
                             (list new-block)))
              (setf (block-last new-block) bind)
              (setf (block-pred next-block) (list new-block))
              (add-to-dfo new-block (block-prev next-block))
              (link-node-to-previous-ctran bind ctran)
              (let* ((lambda-lvar (make-lvar))
                     (call (make-combination lambda-lvar)))
                (setf (node-reoptimize call) nil
                      (node-reoptimize ref) nil)
                (use-lvar ref lambda-lvar)
                (setf (lvar-dest lambda-lvar) call)
                (insert-node-before bind call)
                (setf (combination-kind call) :local
                      (combination-args call) args)
                (loop for arg in args
                      when arg
                      do (setf (lvar-dest arg) call))
                (insert-node-before call ref))
              t))))))

;; Finally, duplicate EQ-nil tests
(defun duplicate-if-if-1 (node test block)
  (when (and (eq (block-start-node block) node)
             (listp (lvar-uses test)))
    (do-uses (use test)
      (when (immediately-used-p test use)
        (convert-if-if use node)
        ;; leave the last use as is, instead of replacing
        ;; the (singly-referenced) CIF node with a duplicate.
        (when (not (listp (lvar-uses test))) (return))))))

;;; Create a new copy of an IF node that tests the value of the node
;;; USE. The test must have >1 use, and must be immediately used by
;;; USE. NODE must be the only node in its block (implying that
;;; block-start = if-test).
;;;
;;; This optimization has an effect semantically similar to the
;;; source-to-source transformation:
;;;    (IF (IF A B C) D E) ==>
;;;    (IF A (IF B D E) (IF C D E))
;;;
;;; We clobber the NODE-SOURCE-PATH of both the original and the new
;;; node so that dead code deletion notes will definitely not consider
;;; either node to be part of the original source. One node might
;;; become unreachable, resulting in a spurious note.
(defun convert-if-if (use node)
  (declare (type node use) (type cif node))
  (with-ir1-environment-from-node node
    (let* ((block (node-block node))
           (test (if-test node))
           (cblock (if-consequent node))
           (ablock (if-alternative node))
           (use-block (node-block use))
           (new-ctran (make-ctran))
           (new-lvar (make-lvar))
           (new-node (make-if :test new-lvar
                              :consequent cblock
                              :alternative ablock))
           (new-block (ctran-starts-block new-ctran)))
      (link-node-to-previous-ctran new-node new-ctran)
      (setf (lvar-dest new-lvar) new-node)
      (setf (block-last new-block) new-node)

      (unlink-blocks use-block block)
      (%delete-lvar-use use)
      (add-lvar-use use new-lvar)
      (link-blocks use-block new-block)

      (link-blocks new-block cblock)
      (link-blocks new-block ablock)

      (push "<IF Duplication>" (node-source-path node))
      (push "<IF Duplication>" (node-source-path new-node))

      (reoptimize-lvar test)
      (reoptimize-lvar new-lvar)
      (setf (component-reanalyze *current-component*) t)))
  (values))

(declaim (end-block))


;;;; exit IR1 optimization

;;; This function attempts to delete an exit node, returning true if
;;; it deletes the block as a consequence:
;;; -- If the exit is degenerate (has no ENTRY), then we don't do
;;;    anything, since there is nothing to be done.
;;; -- If the exit node and its ENTRY have the same home lambda then
;;;    we know the exit is local, and can delete the exit. We change
;;;    uses of the Exit-Value to be uses of the original lvar,
;;;    then unlink the node. If the exit is to a TR context, then we
;;;    must do MERGE-TAIL-SETS on any local calls which delivered
;;;    their value to this exit.
;;; -- If there is no value (as in a GO), then we skip the value
;;;    semantics.
;;;
;;; This function is also called by environment analysis, since it
;;; wants all exits to be optimized even if normal optimization was
;;; omitted.
(defun maybe-delete-exit (node)
  (declare (type exit node))
  (let ((value (exit-value node))
        (entry (exit-entry node)))
    (when (and entry
               (eq (node-home-lambda node) (node-home-lambda entry)))
      (setf (entry-exits entry) (delq1 node (entry-exits entry)))
      (cond (value
             ;; The number of consumed values is now known, reoptimize the users.
             ;; The VALUES transform in particular benefits from this.
             (do-uses (use value)
               (reoptimize-node use))
             (delete-filter node (node-lvar node) value))
            (t
             (unlink-node node))))))


;;;; combination IR1 optimization

(declaim (start-block ir1-optimize-combination maybe-terminate-block
                      validate-call-type))

(defun check-important-result (node info)
  (when (and (null (node-lvar node))
             (ir1-attributep (fun-info-attributes info) important-result)
             (neq (combination-info node) :important-result-discarded))
    (when (lvar-fun-is (combination-fun node) '(adjust-array))
      (let ((type (lvar-type (car (combination-args node)))))
        ;; - if the array is simple, then result is important
        ;; - if non-simple, then result is not important
        ;; - if not enough information, then don't warn
        (when (or (not (array-type-p type)) (array-type-complexp type)) ; T or :MAYBE
          (return-from check-important-result))))
    (let ((*compiler-error-context* node))
      (setf (combination-info node) :important-result-discarded)
      (compiler-style-warn
       "The return value of ~A should not be discarded."
       (lvar-fun-name (basic-combination-fun node) t)))))

(defglobal *debug-auto-dx* nil)
;;; Generalized transform: For each downward funarg, unless it is
;;; a global var (like #'EQL), or the user already DXified,
;;; or this transform already ran, then try to wrap in DX-FLET.
;;; Change the call only if at least one DXable arg is known
;;; to be a fuction. If no eligible arg is, then do nothing.
;;; For now this only works on globally named functions.
(defun dxify-downward-funargs (node dxable-args fun-name)
  (when *debug-auto-dx*
    (format t "~&DXifying funargs to ~S~%" fun-name))
  (let* ((dx-flets)
         (received-args)
         (passed-args))
    ;; Experience shows that users place incorrect DYNAMIC-EXTENT declarations
    ;; without due consideration and care. Since the declaration was ignored
    ;; in more contexts than not, it was relatively harmless.
    ;; In light of that, only make this transform if willing to generate
    ;; wrong code, or if the declaration can be trusted.
    ;; [It's seems to be true that users who want this are OK with lack of
    ;; tail-callability and/or potential stack exhaustion due to the assumption
    ;; that callers should always use more stack space. You should really
    ;; only do that if you don't also need an arbitrarily long call chain.
    ;; MAP and friends are good examples where this pertains]
    (when #+sb-xc-host t ; always trust our own code
          #-sb-xc-host
          (or (let ((pkg (sb-xc:symbol-package (fun-name-block-name fun-name))))
                ;; callee "probably" won't get redefined
                (or (not pkg) (package-locked-p pkg)))
              (policy node (= safety 0)))
      (dolist (arg-spec dxable-args)
        (when (symbolp arg-spec)
          ;; If there are keywords, we had better have a FUN-TYPE
          (let ((fun-type (lvar-type (combination-fun node))))
            ;; Can't do anything unless we can ascertain where
            ;; the keyword arguments start.
            (when (fun-type-p fun-type)
              (let* ((keys-index
                      (+ (length (fun-type-required fun-type))
                         (length (fun-type-optional fun-type))))
                     (keywords-supplied
                      (nthcdr keys-index (combination-args node))))
                ;; Everything in a keyword position needs to be
                ;; constant, or else no transform occurs.
                (loop
                   (unless (cdr keywords-supplied) (return))
                   (let ((keyword (car keywords-supplied)))
                     (unless (constant-lvar-p keyword)
                       (return))
                     (when (eq (lvar-value keyword) arg-spec)
                       ;; Map it to a positional arg
                       (setq arg-spec (1+ keys-index))
                       (return))
                     (setq keywords-supplied (cddr keywords-supplied))
                     (incf keys-index 2)))))))
        (when (integerp arg-spec)
          ;; OK, turn the Nth argument into a dx-flet
          (let* ((arg (or (nth arg-spec (combination-args node))
                          (return-from dxify-downward-funargs nil)))
                 (use (principal-lvar-use arg)))
            (when (and (ref-p use)
                       (lambda-p (ref-leaf use))
                       (neq (leaf-extent (lambda-parent (ref-leaf use)))
                            'truly-dynamic-extent))
              (unless received-args
                (setq received-args
                      (make-gensym-list (length (combination-args node))))
                (setq passed-args (copy-list received-args)))
              (let ((tempname (let ((*gensym-counter* (length dx-flets)))
                                (sb-xc:gensym "LAMBDA")))
                    (original-lambda
                     (functional-inline-expansion
                      (lambda-entry-fun (ref-leaf use)))))
                (aver (typep original-lambda '(cons (eql lambda))))
                (let ((original-lambda-list (second original-lambda)))
                  ;; KISS - the closure that you're passing can have 0 or more
                  ;; mandatory args and nothing else.
                  (unless (intersection original-lambda-list lambda-list-keywords)
                    (push `(,tempname ,original-lambda-list
                             (%funcall ,(nth arg-spec received-args)
                                       ,@original-lambda-list))
                          dx-flets)
                    (setf (nth arg-spec passed-args) `#',tempname)))))))))
    (when dx-flets
      (let ((new
             `(lambda ,received-args
                (dx-flet ,(nreverse dx-flets)
                  (,@(if (symbolp fun-name) `(,fun-name) `(funcall #',fun-name))
                   ,@passed-args)))))
        (when *debug-auto-dx*
          (format t "->~%~S~%" new))
        new))))

;;; This does not work. The intent was to prepend this transform to the list
;;; of (FUN-INFO-TRANSFORMS INFO) when applicable, in the known fun case.
;;; But the reason it doesn't work isn't that the transform doesn't transform
;;; the code - it does; but compiler doesn't appear to respect the DX-FLET.
(defglobal *dxify-args-transform*
  (make-transform :type (specifier-type 'function)
                  :%fun (lambda (node)
                              "auto-DX"
                              (or (let ((name (combination-fun-source-name node)))
                                    (dxify-downward-funargs
                                     node (fun-name-dx-args name) name))
                                  (give-up-ir1-transform)))))

(defun check-proper-sequences (combination info)
  (when (fun-info-annotation info)
    (map-combination-args-and-types
     (lambda (lvar type lvars annotations)
       (declare (ignore type lvars))
       (when (constant-lvar-p lvar)
         (loop with value = (lvar-value lvar)
               for annotation in annotations
               when (improper-sequence-p annotation value)
               do
               (setf (combination-kind combination) :error)
               (return-from check-proper-sequences))))
     combination info)))

;;; Do IR1 optimizations on a COMBINATION node.
(defun ir1-optimize-combination (node &aux (show *show-transforms-p*))
  (declare (type combination node))
  (when (lvar-reoptimize (basic-combination-fun node))
    (propagate-fun-change node)
    (when (node-deleted node)
      (return-from ir1-optimize-combination))
    (maybe-terminate-block node nil))
  (let ((args (basic-combination-args node))
        (info (basic-combination-fun-info node))
        (kind (basic-combination-kind node)))
    (flet ((clear-reoptimize-args ()
             (dolist (arg args)
               (when arg
                 (setf (lvar-reoptimize arg) nil))))
           (process-info ()
             (check-important-result node info)
             (check-proper-sequences node info)
             (let ((fun (fun-info-derive-type info)))
               (when fun
                 (let ((res (funcall fun node)))
                   (when res
                     (when (eq show :derive-type)
                       (show-type-derivation node res))
                     (derive-node-type node (coerce-to-values res))
                     (maybe-terminate-block node nil)))))))
      (ecase kind
        (:local
         (let ((fun (combination-lambda node)))
           (if (eq (functional-kind fun) :let)
               (propagate-let-args node fun)
               (propagate-local-call-args node fun))))
        (:error
         (clear-reoptimize-args))
        ((:full :unknown-keys)
         (clear-reoptimize-args)
         (cond (info
                ;; This is a known function marked NOTINLINE
                (process-info))
               (t
                ;; Check against the DEFINED-TYPE unless TYPE is already good.
                (let* ((fun (basic-combination-fun node))
                       (uses (lvar-uses fun))
                       (leaf (when (ref-p uses) (ref-leaf uses))))
                  (multiple-value-bind (type defined-type)
                      (cond ((global-var-p leaf)
                             (values (leaf-type leaf) (leaf-defined-type leaf)))
                            ((eq kind :unknown-keys)
                             (values (lvar-fun-type fun t t) nil))
                            (t
                             (values nil nil)))
                    (when (or (and (eq kind :unknown-keys)
                                   (fun-type-p type))
                              (and (fun-type-p defined-type)
                                   (not (fun-type-p type))))
                      (validate-call-type node type leaf)))
                  (if (neq (basic-combination-kind node) kind)
                      (ir1-optimize-combination node)
                      (binding* ((name (and leaf ;; don't want to transform CASTs
                                            (lvar-fun-name fun))
                                       :exit-if-null)
                                 (dxable-args (fun-name-dx-args name) :exit-if-null))
                        (awhen (dxify-downward-funargs node dxable-args name)
                          (transform-call node it name))))))))
        (:known
         (aver info)
         (clear-reoptimize-args)
         (process-info)
         (unless (eq (combination-kind node) :error) ;; casued by derive-type
           (let ((attr (fun-info-attributes info)))
             (when (constant-fold-call-p node)
               (constant-fold-call node)
               (return-from ir1-optimize-combination))
             (when (fold-call-derived-to-constant node)
               (return-from ir1-optimize-combination))
             (when (and (ir1-attributep attr commutative)
                        (= (length args) 2)
                        (constant-lvar-p (first args))
                        (not (constant-lvar-p (second args))))
               (setf (basic-combination-args node) (nreverse args))))

           (let ((fun-source-name (combination-fun-source-name node))
                 (optimizer (fun-info-optimizer info)))
             (unless (and optimizer (funcall optimizer node))
               ;; First give the VM a peek at the call
               (multiple-value-bind (style transform)
                   (combination-implementation-style node)
                 (ecase style
                   (:direct
                    ;; The VM knows how to handle this.
                    )
                   (:transform
                    ;; The VM mostly knows how to handle this.  We need
                    ;; to massage the call slightly, though.
                    (transform-call node transform fun-source-name))
                   ((:default :maybe)
                    ;; Let transforms have a crack at it.
                    ;; We should always try with the dxify-args transform,
                    ;; but ironically it *does* *not* *work* for any function
                    ;; that has FUNCTION-DESIGNATOR in its arg signature
                    ;; (pretty much any CL: function). This is just sad.
                    ;; Are type checks getting in the way?
                    (or (try-equality-constraint node)
                        (dolist (x (fun-info-transforms info))
                          (when (eq show :all)
                            (let* ((lvar (basic-combination-fun node))
                                   (fname (lvar-fun-name lvar t)))
                              (format *trace-output*
                                      "~&trying transform ~s for ~s"
                                      (transform-type x) fname)))
                          (unless (ir1-transform node x show)
                            (when (eq show :all)
                              (format *trace-output*
                                      "~&quitting because IR1-TRANSFORM result was NIL"))
                            (return))))))))))))))
  (values))

(defun xep-tail-combination-p (node)
  (and (combination-p node)
       (let* ((lvar (combination-lvar node))
              (dest (when (lvar-p lvar) (lvar-dest lvar)))
              (lambda (when (return-p dest) (return-lambda dest))))
         (and (lambda-p lambda)
              (eq :external (lambda-kind lambda))))))

;;; If NODE doesn't return (i.e. return type is NIL), then terminate
;;; the block there, and link it to the component tail.
;;;
;;; Except when called during IR1 convertion, we delete the
;;; continuation if it has no other uses. (If it does have other uses,
;;; we reoptimize.)
;;;
;;; Termination on the basis of a continuation type is
;;; inhibited when:
;;; -- The continuation is deleted (hence the assertion is spurious), or
;;; -- We are in IR1 conversion (where THE assertions are subject to
;;;    weakening.) FIXME: Now THE assertions are not weakened, but new
;;;    uses can(?) be added later. -- APD, 2003-07-17
;;;
;;; Why do we need to consider LVAR type? -- APD, 2003-07-30
(defun maybe-terminate-block (node ir1-converting-not-optimizing-p)
  (declare (type (or basic-combination cast ref) node))
  (let* ((block (node-block node))
         (lvar (node-lvar node))
         (ctran (node-next node))
         (tail (component-tail (block-component block)))
         (succ (first (block-succ block))))
    (declare (ignore lvar))
    (unless (or (and (eq node (block-last block)) (eq succ tail))
                (block-delete-p block)
                ;; Even if the combination will never return, don't
                ;; terminate if this is the tail call of a XEP: doing
                ;; that would inhibit TCO.
                (xep-tail-combination-p node)
                ;; Do not consider the block for termination if this
                ;; is a LET-like combination, since the successor of
                ;; this node is the body of the LET.
                (and (combination-p node)
                     (eq (combination-kind node) :local)
                     (functional-somewhat-letlike-p (combination-lambda node))))
      (when (eq (node-derived-type node) *empty-type*)
        (cond (ir1-converting-not-optimizing-p
               (cond
                 ((block-last block)
                  (aver (eq (block-last block) node)))
                 (t
                  (setf (block-last block) node)
                  (setf (ctran-use ctran) nil)
                  (setf (ctran-kind ctran) :unused)
                  (setf (ctran-block ctran) nil)
                  (setf (node-next node) nil)
                  (link-blocks block (ctran-starts-block ctran)))))
              (t
               (node-ends-block node)))

        (let ((succ (first (block-succ block))))
          (unlink-blocks block succ)
          (setf (component-reanalyze (block-component block)) t)
          (aver (not (block-succ block)))
          (link-blocks block tail)
          (cond (ir1-converting-not-optimizing-p
                 (%delete-lvar-use node))
                (t (delete-lvar-use node)
                   (when (null (block-pred succ))
                     (mark-for-deletion succ)))))
        t))))

;;; This is called both by IR1 conversion and IR1 optimization when
;;; they have verified the type signature for the call, and are
;;; wondering if something should be done to special-case the call. If
;;; CALL is a call to a global function, then see whether it defined
;;; or known:
;;; -- If a DEFINED-FUN should be inline expanded, then convert
;;;    the expansion and change the call to call it. Expansion is
;;;    enabled if INLINE or if SPACE=0. If the FUNCTIONAL slot is
;;;    true, we never expand, since this function has already been
;;;    converted. Local call analysis will duplicate the definition
;;;    if necessary. We claim that the parent form is LABELS for
;;;    context declarations, since we don't want it to be considered
;;;    a real global function.
;;; -- If it is a known function, mark it as such by setting the KIND.
;;;
;;; We return the leaf referenced (NIL if not a leaf) and the
;;; FUN-INFO assigned.
(defun recognize-known-call (call ir1-converting-not-optimizing-p
                             &optional unknown-keys)
  (declare (type combination call))
  (let* ((ref (lvar-uses (basic-combination-fun call)))
         (leaf (when (ref-p ref) (ref-leaf ref)))
         (inlinep (if (defined-fun-p leaf)
                      (defined-fun-inlinep leaf)
                      'no-chance)))
    (cond
      ((eq (basic-combination-kind call) :error)
       (values nil nil))
      (unknown-keys
       (setf (basic-combination-kind call) :unknown-keys)
       (values leaf nil))
      ((eq inlinep 'notinline)
       (let ((info (info :function :info (leaf-source-name leaf))))
         (when info
           (setf (basic-combination-fun-info call) info))
         (values nil nil)))
      ((not (and (global-var-p leaf)
                 (eq (global-var-kind leaf) :global-function)))
       (values leaf nil))
      ((and (ecase inlinep
              (inline t)
              (no-chance nil)
              ((nil maybe-inline) (policy call (zerop space))))
            (defined-fun-p leaf)
            (defined-fun-inline-expansion leaf)
            (inline-expansion-ok call leaf))
       ;; Inline: if the function has already been converted at another call
       ;; site in this component, we point this REF to the functional. If not,
       ;; we convert the expansion.
       ;;
       ;; For INLINE case local call analysis will copy the expansion later,
       ;; but for MAYBE-INLINE and NIL cases we only get one copy of the
       ;; expansion per component.
       (with-ir1-environment-from-node call
         (let ((fun (defined-fun-functional leaf)))
           (cond ((or (not fun)
                      ;; It has already been processed by locall,
                      ;; inline again.
                      (functional-kind fun))
                  (when (eq (car *current-path*) 'original-source-start)
                    (setf (ctran-source-path (node-prev call)) *current-path*))
                  ;; Convert.
                  (let* ((name (leaf-source-name leaf))
                         (*inline-expansions*
                           (register-inline-expansion leaf call))
                         (res (ir1-convert-inline-expansion
                               name
                               (defined-fun-inline-expansion leaf)
                               leaf
                               inlinep
                               (info :function :info name))))

                    ;; Allow backward references to this function from following
                    ;; forms. (Reused only if policy matches.)
                    (push res (defined-fun-functionals leaf))
                    (change-ref-leaf ref res)
                    (unless ir1-converting-not-optimizing-p
                      (locall-analyze-component *current-component*))))
                 (t
                  ;; If we've already converted, change ref to the converted
                  ;; functional.
                  (maybe-reanalyze-functional fun)
                  (change-ref-leaf ref fun)))))
       (values (ref-leaf ref) nil))
      (t
       (let ((info (info :function :info (leaf-source-name leaf))))
         (if info
             (values leaf
                     (progn
                       (setf (basic-combination-kind call) :known)
                       (setf (basic-combination-fun-info call) info)))
             (values leaf nil)))))))

;;; Check whether CALL satisfies TYPE. If so, apply the type to the
;;; call, and do MAYBE-TERMINATE-BLOCK and return the values of
;;; RECOGNIZE-KNOWN-CALL. If an error, set the combination kind and
;;; return NIL, NIL. If the type is just FUNCTION, then skip the
;;; syntax check, arg/result type processing, but still call
;;; RECOGNIZE-KNOWN-CALL, since the call might be to a known lambda,
;;; and that checking is done by local call analysis.
(defun validate-call-type (call type fun &optional ir1-converting-not-optimizing-p)
  (declare (type combination call) (type ctype type))
  (let* ((where (when fun (leaf-where-from fun)))
         (same-file-p (eq :defined-here where)))
    (cond ((not (fun-type-p type))
           ;; Using the defined-type too early is a bit of a waste: during
           ;; conversion we cannot use the untrusted ASSERT-CALL-TYPE, etc.
           (when (and fun (not ir1-converting-not-optimizing-p))
             (let ((defined-type (leaf-defined-type fun)))
               (when (and (fun-type-p defined-type)
                          (neq fun (combination-type-validated-for-leaf call)))
                 ;; Don't validate multiple times against the same leaf --
                 ;; it doesn't add any information, but may generate the same warning
                 ;; multiple times.
                 (setf (combination-type-validated-for-leaf call) fun)
                 (when (and (valid-fun-use call defined-type
                                           :argument-test nil
                                           :result-test nil
                                           :lossage-fun (if same-file-p
                                                            #'compiler-warn
                                                            #'compiler-style-warn)
                                           :unwinnage-fun #'compiler-notify)
                            same-file-p)
                   (assert-call-type call defined-type nil)
                   (maybe-terminate-block call ir1-converting-not-optimizing-p)))))
           (recognize-known-call call ir1-converting-not-optimizing-p))
          (t
           (multiple-value-bind (valid unwinnage unknown-keys)
               (valid-fun-use call type
                              :argument-test nil
                              :result-test nil
                              :lossage-fun #'compiler-warn
                              :unwinnage-fun #'compiler-notify)
             (declare (ignore unwinnage))
             (cond (valid
                    (assert-call-type call type)
                    (maybe-terminate-block call ir1-converting-not-optimizing-p)
                    (cond ((eq (combination-kind call) :error)
                           (values nil nil))
                          (t
                           (setf (combination-kind call) :full)
                           (recognize-known-call call ir1-converting-not-optimizing-p unknown-keys))))
                   (t
                    (setf (combination-kind call) :error)
                    (values nil nil))))))))

;;; This is called by IR1-OPTIMIZE when the function for a call has
;;; changed. If the call is local, we try to LET-convert it, and
;;; derive the result type. If it is a :FULL call, we validate it
;;; against the type, which recognizes known calls, does inline
;;; expansion, etc. If a call to a predicate in a non-conditional
;;; position or to a function with a source transform, then we
;;; reconvert the form to give IR1 another chance.
(defun propagate-fun-change (call)
  (declare (type combination call))
  (let ((*compiler-error-context* call)
        (fun-lvar (basic-combination-fun call)))
    (setf (lvar-reoptimize fun-lvar) nil)
    (case (combination-kind call)
      (:local
       (let ((fun (combination-lambda call)))
         (or (maybe-let-convert fun)
             (maybe-convert-to-assignment fun))
         (unless (member (functional-kind fun) '(:let :assignment :deleted))
           (derive-node-type call (tail-set-type (lambda-tail-set fun))))))
      (:full
       (multiple-value-bind (leaf info)
           (let* ((uses (lvar-uses fun-lvar))
                  (leaf (when (ref-p uses) (ref-leaf uses))))
             (validate-call-type call (lvar-fun-type fun-lvar t t) leaf))
         (cond ((functional-p leaf)
                (convert-call-if-possible
                 (lvar-uses (basic-combination-fun call))
                 call))
               ((not leaf))
               ((and (global-var-p leaf)
                     (eq (global-var-kind leaf) :global-function)
                     (leaf-has-source-name-p leaf)
                     (or (info :function :source-transform (leaf-source-name leaf))
                         (and info
                              (ir1-attributep (fun-info-attributes info)
                                              predicate)
                              (let ((lvar (node-lvar call)))
                                (and lvar (not (if-p (lvar-dest lvar))))))))
                (let ((name (leaf-source-name leaf))
                      (dummies (make-gensym-list
                                (length (combination-args call)))))
                  (transform-call call
                                  `(lambda ,dummies
                                     (,@(if (symbolp name)
                                            `(,name)
                                            `(funcall #',name))
                                        ,@dummies))
                                  (leaf-source-name leaf)
                                  nil))))))))
  (values))

;;;; known function optimization

;;; Add a failed optimization note to FAILED-OPTIMZATIONS for NODE,
;;; FUN and ARGS. If there is already a note for NODE and TRANSFORM,
;;; replace it, otherwise add a new one.
(defun record-optimization-failure (node transform args)
  (declare (type combination node) (type transform transform)
           (type (or fun-type list) args))
  (let* ((table (component-failed-optimizations *component-being-compiled*))
         (found (assoc transform (gethash node table))))
    (if found
        (setf (cdr found) args)
        (push (cons transform args) (gethash node table))))
  (values))

;;; Attempt to transform NODE using TRANSFORM-FUNCTION, subject to the
;;; call type constraint TRANSFORM-TYPE. If we are inhibited from
;;; doing the transform for some reason and FLAME is true, then we
;;; make a note of the message in FAILED-OPTIMIZATIONS for IR1
;;; finalize to pick up. We return true if the transform failed, and
;;; thus further transformation should be attempted. We return false
;;; if either the transform succeeded or was aborted.
(defun ir1-transform (node transform show)
  (declare (type combination node) (type transform transform))
  (declare (notinline warn)) ; See COMPILER-WARN for rationale
  (let* ((type (transform-type transform))
         (fun (transform-%fun transform))
         (constrained (fun-type-p type))
         (table (component-failed-optimizations *component-being-compiled*))
         (flame (case (transform-important transform)
                  ((t) (policy node (>= speed inhibit-warnings)))
                  (:slightly (policy node (> speed inhibit-warnings)))))
         (*compiler-error-context* node)
         (policy-test (transform-policy transform)))
    (cond ((and policy-test
                (not (funcall policy-test node))))
          ((or (not constrained)
               (valid-fun-use node type))
           (multiple-value-bind (severity args)
               (catch 'give-up-ir1-transform
                 (let ((new-form (if (listp fun) ; the deftransform had :INFO
                                     (funcall (car fun) node (cdr fun))
                                     (funcall fun node)))
                       (fun-name (combination-fun-source-name node)))
                   (when (show-transform-p show fun-name)
                     (show-transform "ir" fun-name new-form node))
                   (transform-call node new-form fun-name))
                 (values :none nil))
             (ecase severity
               (:none
                (remhash node table)
                nil)
               (:aborted
                (setf (combination-kind node) :error)
                (when args
                  (apply #'warn args))
                (remhash node table)
                nil)
               (:failure
                (if args
                    (when flame
                      (record-optimization-failure node transform args))
                    (setf (gethash node table)
                          (remove transform (gethash node table) :key #'car)))
                t)
               (:delayed
                (remhash node table)
                nil))))
          ((and flame
                (valid-fun-use node
                               type
                               :argument-test #'types-equal-or-intersect
                               :result-test #'values-types-equal-or-intersect))
           (record-optimization-failure node transform type)
           t)
          (t
           t))))

(declaim (end-block))

;;; When we don't like an IR1 transform, we throw the severity/reason
;;; and args.
;;;
;;; GIVE-UP-IR1-TRANSFORM is used to throw out of an IR1 transform,
;;; aborting this attempt to transform the call, but admitting the
;;; possibility that this or some other transform will later succeed.
;;; If arguments are supplied, they are format arguments for an
;;; efficiency note.
;;;
;;; ABORT-IR1-TRANSFORM is used to throw out of an IR1 transform and
;;; force a normal call to the function at run time. No further
;;; optimizations will be attempted.
;;;
;;; DELAY-IR1-TRANSFORM is used to throw out of an IR1 transform, and
;;; delay the transform on the node until later. REASONS specifies
;;; when the transform will be later retried. The :OPTIMIZE reason
;;; causes the transform to be delayed until after the current IR1
;;; optimization pass. The :CONSTRAINT reason causes the transform to
;;; be delayed until after constraint propagation.
;;;
;;; FIXME: Now (0.6.11.44) that there are 4 variants of this (GIVE-UP,
;;; ABORT, DELAY/:OPTIMIZE, DELAY/:CONSTRAINT) and we're starting to
;;; do CASE operations on the various REASON values, it might be a
;;; good idea to go OO, representing the reasons by objects, using
;;; CLOS methods on the objects instead of CASE, and (possibly) using
;;; SIGNAL instead of THROW.
(defun give-up-ir1-transform (&rest args)
  (throw 'give-up-ir1-transform (values :failure args)))
(defun abort-ir1-transform (&rest args)
  (throw 'give-up-ir1-transform (values :aborted args)))

(defvar *delayed-ir1-transforms*)

(defun delay-ir1-transform (node &rest reasons)
  (let ((assoc (assoc node *delayed-ir1-transforms*)))
    (cond ((not assoc)
            (setf *delayed-ir1-transforms*
                    (acons node reasons *delayed-ir1-transforms*))
            (throw 'give-up-ir1-transform :delayed))
          ((cdr assoc)
            (dolist (reason reasons)
              (pushnew reason (cdr assoc)))
            (throw 'give-up-ir1-transform :delayed)))))

;;; Poor man's catching and resignalling
;;; Implicit %GIVE-UP macrolet will resignal the give-up "condition"
(defmacro catch-give-up-ir1-transform ((form &optional args) &body gave-up-body)
  (let ((block (gensym "BLOCK"))
        (kind (gensym "KIND"))
        (args (or args (gensym "ARGS"))))
    `(block ,block
       (multiple-value-bind (,kind ,args)
           (catch 'give-up-ir1-transform
             (return-from ,block ,form))
         (ecase ,kind
           (:delayed
            (throw 'give-up-ir1-transform :delayed))
           ((:failure :aborted)
            (macrolet ((%give-up ()
                         `(throw 'give-up-ir1-transform (values ,',kind
                                                                ,',args))))
              ,@gave-up-body)))))))

;;; Clear any delayed transform with no reasons - these should have
;;; been tried in the last pass. Then remove the reason from the
;;; delayed transform reasons, and if any become empty then set
;;; reoptimize flags for the node. Return true if any transforms are
;;; to be retried.
(defun retry-delayed-ir1-transforms (reason)
  (setf *delayed-ir1-transforms*
        (remove-if-not #'cdr *delayed-ir1-transforms*))
  (let ((reoptimize nil))
    (dolist (assoc *delayed-ir1-transforms*)
      (let ((reasons (remove reason (cdr assoc))))
        (setf (cdr assoc) reasons)
        (unless reasons
          (let ((node (car assoc)))
            (unless (node-deleted node)
              (setf reoptimize t)
              (setf (node-reoptimize node) t)
              (let ((block (node-block node)))
                (setf (block-reoptimize block) t)
                (reoptimize-component (block-component block) :maybe)))))))
    reoptimize))

;;; Take the lambda-expression RES, IR1 convert it in the proper
;;; environment, and then install it as the function for the call
;;; NODE. We do local call analysis so that the new function is
;;; integrated into the control flow.
;;;
;;; We require the original function source name in order to generate
;;; a meaningful debug name for the lambda we set up. (It'd be
;;; possible to do this starting from debug names as well as source
;;; names, but as of sbcl-0.7.1.5, there was no need for this
;;; generality, since source names are always known to our callers.)
(defun transform-call (call res source-name &optional (reoptimize-combination t))
  (declare (type combination call) (list res))
  (aver (and (legal-fun-name-p source-name)
             (not (eql source-name '.anonymous.))))
  (node-ends-block call)
  (setf (combination-lexenv call)
        (make-lexenv :default (combination-lexenv call)
                     :policy
                     ;; The internal variables of a transform are not going to be
                     ;; interesting to the debugger, so there's no sense in
                     ;; suppressing the substitution of variables with only one use
                     ;; (the extra variables can slow down constraint propagation).
                     (augment-policy
                      preserve-single-use-debug-variables
                      0
                      (lexenv-policy
                       (combination-lexenv call)))))
  (with-ir1-environment-from-node call
    (with-component-last-block (*current-component*
                                (block-next (node-block call)))
      (unless (or (memq 'transformed *current-path*)
                  (memq 'inlined *current-path*))
        (setf (ctran-source-path (node-prev call)) *current-path*))
      (let* ((*transforming* (1+ *transforming*))
             (new-fun (ir1-convert-inline-lambda
                       res
                       :debug-name (debug-name 'lambda-inlined source-name)
                       :system-lambda t))
             (type (node-derived-type call))
             (ref (lvar-use (combination-fun call))))
        (change-ref-leaf ref new-fun)
        (setf (combination-kind call) :full)
        ;; Don't lose the original derived type
        (let ((return (lambda-return (main-entry new-fun))))
          (when return
            (do-uses (node (return-result
                            (lambda-return (main-entry new-fun))))
              (derive-node-type node type))))))
    ;; Must be done outside of WITH-COMPONENT-LAST-BLOCK
    ;; otherwise REMOVE-FROM-DFO might remove that block
    ;; but new code still will get attached to it.
    (locall-analyze-component *current-component*))
  (when reoptimize-combination
    ;; This is mainly to call PROPAGATE-LET-ARGS so that the
    ;; newly converted code gets to better types sooner.
    (setf (node-reoptimize call) nil)
    (ir1-optimize-combination call))
  (values))

(defun constant-fold-arg-p (name)
  (typecase name
    (null
     t)
    ((or symbol cons)
     (let* ((info (info :function :info name))
            (attributes (and info
                             (fun-info-attributes info))))
       (and info
            (ir1-attributep attributes foldable)
            (not (ir1-attributep attributes call)))))))

;;; Return T if the function is foldable and if it's marked as CALL
;;; all function arguments are FOLDABLE too.
(defun constant-fold-call-p (combination)
  (let* ((info (basic-combination-fun-info combination))
         (attr (fun-info-attributes info))
         (args (basic-combination-args combination)))
    (cond ((not (ir1-attributep attr foldable))
           nil)
          ((ir1-attributep attr call)
           (map-combination-args-and-types
            (lambda (arg type lvars &optional annotation)
              (declare (ignore type lvars))
              (unless (if (eql (car annotation) 'function-designator)
                          (let ((fun (or (lvar-fun-name arg t)
                                         (and (constant-lvar-ignore-types-p arg)
                                              (lvar-value arg)))))
                            (and fun
                                 (constant-fold-arg-p fun)))
                          (constant-lvar-ignore-types-p arg))
                (return-from constant-fold-call-p)))
            combination
            info
            (lambda ()
              (return-from constant-fold-call-p)))
           t)
          (t
           (every #'constant-lvar-ignore-types-p args)))))

;;; Replace a call to a foldable function of constant arguments with
;;; the result of evaluating the form. If there is an error during the
;;; evaluation, we give a warning and leave the call alone, making the
;;; call a :ERROR call.
;;;
;;; If there is more than one value, then we transform the call into a
;;; VALUES form.
(defun constant-fold-call (call)
  (flet ((value (lvar)
           (if (lvar-p lvar)
               (let ((name (lvar-fun-name lvar t)))
                 (if name
                     (fdefinition name)
                     (lvar-value lvar)))
               lvar)))
   (let* ((fun-name (lvar-fun-name (combination-fun call) t))
          (type (info :function :type fun-name))
          (lvar-args (let ((args (combination-args call)))
                       (if (fun-type-p type)
                           (resolve-key-args args type)
                           args)))
          (args (mapcar #'value lvar-args)))
     (multiple-value-bind (values win) (careful-call fun-name args)
       (cond ((not win)
              (setf (combination-kind call) :error
                    (combination-info call)
                    (list #'compiler-style-warn "Lisp error during constant folding:~%~A" values)))
             ((and (proper-list-of-length-p values 1))
              (replace-combination-with-constant (first values) call))
             (t (let ((dummies (make-gensym-list (length args))))
                  (transform-call
                   call
                   `(lambda ,dummies
                      (declare (ignore ,@dummies))
                      (values ,@(mapcar (lambda (x)
                                          (let ((lvar
                                                  (find x lvar-args :key #'value)))
                                            ;; Don't lose any annotations
                                            (if (and lvar
                                                     (lvar-annotations lvar))
                                                `(with-annotations ,(lvar-annotations lvar) ',x)
                                                `',x)))
                                        values)))
                   fun-name)))))))
  (values))

(defun fold-call-derived-to-constant (call)
  (when (flushable-combination-p call)
    (let ((type (node-derived-type call)))
      (when (type-single-value-p type)
        (multiple-value-bind (single-p value) (type-singleton-p (single-value-type type))
          (when single-p
            (replace-combination-with-constant value call)
            t))))))

;;;; local call optimization

(declaim (start-block ir1-optimize-set constant-reference-p delete-let
                      propagate-let-args propagate-local-call-args
                      propagate-to-refs propagate-from-sets
                      ir1-optimize-mv-combination))

;;; Propagate TYPE to LEAF and its REFS, marking things changed.
;;;
;;; If the leaf type is a function type, then just leave it alone, since TYPE
;;; is never going to be more specific than that (and TYPE-INTERSECTION would
;;; choke.)
;;;
;;; Also, if the type is one requiring special care don't touch it if the leaf
;;; has multiple references -- otherwise LVAR-CONSERVATIVE-TYPE is screwed.
(defun propagate-to-refs (leaf type)
  (declare (type leaf leaf) (type ctype type))
  (let ((var-type (leaf-type leaf))
        (refs (leaf-refs leaf)))
    (unless (or (fun-type-p var-type)
                (and (cdr refs)
                     (eq :declared (leaf-where-from leaf))
                     (type-needs-conservation-p var-type)))
      (let ((int (type-approx-intersection2 var-type type)))
        (when (type/= int var-type)
          (setf (leaf-type leaf) int)
          (let ((s-int (make-single-value-type int)))
            (dolist (ref refs)
              (derive-node-type ref s-int)
              (maybe-terminate-block ref nil)
              ;; KLUDGE: LET var substitution
              (let* ((lvar (node-lvar ref)))
                (when (and lvar (combination-p (lvar-dest lvar)))
                  (reoptimize-lvar lvar)))))))
      (values))))

;;; Turn (or (integer 1 1) (integer 3 3)) to (integer 1 3)
(defun weaken-numeric-union-type (type)
  (if (union-type-p type)
      (let ((low  nil)
            (high nil)
            class
            (format :no))
        (dolist (part (union-type-types type)
                      (make-numeric-type :class class
                                         :format format
                                         :low low
                                         :high high))
          (unless (and (numeric-type-real-p part)
                       (if class
                           (eql (numeric-type-class part) class)
                           (setf class (numeric-type-class part)))
                       (cond ((eq format :no)
                              (setf format (numeric-type-format part))
                              t)
                             ((eql (numeric-type-format part) format))))
            (return type))
          (let ((this-low (numeric-type-low part))
                (this-high (numeric-type-high part)))
            (unless (and this-low this-high)
              (return type))
            (when (consp this-low)
              (setf this-low (car this-low)))
            (when (consp this-high)
              (setf this-high (car this-high)))
            (setf low  (min this-low  (or low  this-low))
                  high (max this-high (or high this-high))))))
      type))

;;; Iteration variable: only SETQs of the form:
;;;
;;; (let ((var initial))
;;;   ...
;;;   (setq var (+/- var step_1))
;;;   ...
;;;   (setq var (+/- var step_k))
;;;   ...)
;;;
;;; such that the modifications either all increment or all decrement
;;; VAR.
(declaim (inline %inc-or-dec-p))
(defun %inc-or-dec-p (node)
  (and (combination-p node)
       (eq (combination-kind node) :known)
       (fun-info-p (combination-fun-info node))
       (not (node-to-be-deleted-p node))
       (let ((source-name (combination-fun-source-name node)))
         (when (memq source-name '(- +))
           source-name))))

(defun %analyze-set-uses (sets var initial-type)
  (let ((some-plusp nil)
        (some-minusp nil)
        (set-types '())
        (every-set-type-suitable-p t))
    (dolist (set sets)
      (let* ((set-use (principal-lvar-use (set-value set)))
             (function (%inc-or-dec-p set-use)))
        (unless function ; every use must be + or -
          (return-from %analyze-set-uses nil))
        (let ((args (basic-combination-args set-use)))
          ;; Every use must be of the form ({+,-} VAR STEP).
          (unless (and (proper-list-of-length-p args 2 2)
                       (let ((first (principal-lvar-use (first args))))
                         (and (ref-p first)
                              (eq (ref-leaf first) var))))
            (return-from %analyze-set-uses nil))
          (let ((step-type (lvar-type (second args)))
                (set-type (lvar-type (set-value set))))
            ;; In ({+,-} VAR STEP), the type of STEP must be a numeric
            ;; type matching INITIAL-TYPE.
            (unless (and (numeric-type-p step-type)
                         (or (numeric-type-equal initial-type step-type)
                             ;; Detect cases like (LOOP FOR 1.0 to 5.0
                             ;; ...), where the initial and the step
                             ;; are of different types, and the step
                             ;; is less contagious.
                             (let ((contagion-type (numeric-contagion initial-type
                                                                      step-type)))
                               (and (numeric-type-p contagion-type)
                                    (numeric-type-equal initial-type contagion-type)))))
              (return-from %analyze-set-uses nil))
            ;; Track the directions of the increments/decrements.
            (let ((non-negative-p (csubtypep step-type (specifier-type '(real 0 *))))
                  (non-positive-p (csubtypep step-type (specifier-type '(real * 0)))))
              (cond ((or (and (eq function '+) non-negative-p)
                         (and (eq function '-) non-positive-p))
                     (setf some-plusp t))
                    ((or (and (eq function '-) non-negative-p)
                         (and (eq function '+) non-positive-p))
                     (setf some-minusp t))
                    (t ; Can't tell direction
                     (setf some-plusp t some-minusp t))))
            ;; Ultimately, the derived types of the sets must match
            ;; INITIAL-TYPE if we are going to derive new bounds.
            (unless (and (numeric-type-p set-type)
                         (numeric-type-equal set-type initial-type))
              (setf every-set-type-suitable-p nil))
            (push set-type set-types)))))
    (values (cond ((and some-plusp (not some-minusp)) '+)
                  ((and some-minusp (not some-plusp)) '-)
                  (t '*))
            set-types every-set-type-suitable-p)))

(defun maybe-infer-iteration-var-type (var initial-type)
  (binding* ((sets (lambda-var-sets var) :exit-if-null)
             (initial-type (weaken-numeric-union-type initial-type))
             ((direction set-types every-set-type-suitable-p)
              (when (numeric-type-p initial-type)
                (%analyze-set-uses sets var initial-type))
              :exit-if-null))
    (labels ((leftmost (x y cmp cmp=)
               (cond ((eq x nil) nil)
                     ((eq y nil) nil)
                     ((listp x)
                      (let ((x1 (first x)))
                        (cond ((listp y)
                               (let ((y1 (first y)))
                                 (if (funcall cmp x1 y1) x y)))
                              (t
                               (if (funcall cmp x1 y) x y)))))
                     ((listp y)
                      (let ((y1 (first y)))
                        (if (funcall cmp= x y1) x y)))
                     (t (if (funcall cmp x y) x y)))))
      (multiple-value-bind (low high)
          (ecase direction
            (+
             (values (numeric-type-low initial-type)
                     (when every-set-type-suitable-p
                       (reduce (lambda (x y) (leftmost x y #'> #'>=)) set-types
                               :initial-value (numeric-type-high initial-type)
                               :key #'numeric-type-high)))
             )
            (-
             (values (when every-set-type-suitable-p
                       (reduce (lambda (x y) (leftmost x y #'< #'<=)) set-types
                               :initial-value (numeric-type-low initial-type)
                               :key #'numeric-type-low))
                     (numeric-type-high initial-type)))
            (*
             (values nil nil)))
        (modified-numeric-type initial-type :low low
                                            :high high
                                            :enumerable nil)))))

(deftransform + ((x y) * * :result result)
  "check for iteration variable reoptimization"
  (let ((dest (principal-lvar-end result))
        (use (principal-lvar-use x)))
    (when (and (ref-p use)
               (set-p dest)
               (eq (ref-leaf use)
                   (set-var dest)))
      (reoptimize-lvar (set-value dest))))
  (give-up-ir1-transform))

;;; Figure out the type of a LET variable that has sets. We compute
;;; the union of the INITIAL-TYPE and the types of all the set
;;; values and do a PROPAGATE-TO-REFS with this type.
(defun propagate-from-sets (var initial-type)
  (let ((types nil))
    (dolist (set (lambda-var-sets var))
      (let ((type (lvar-type (set-value set))))
        (push type types)
        (when (and (node-reoptimize set)
                   (not (node-to-be-deleted-p set)))
          (let ((old-type (node-derived-type set)))
            (unless (values-subtypep old-type type)
              (derive-node-type set (make-single-value-type type))))
          (setf (node-reoptimize set) nil))))
    (let ((res-type (or (maybe-infer-iteration-var-type var initial-type)
                        (apply #'type-union initial-type types))))
      (propagate-to-refs var res-type)))
  (values))

;;; If a LET variable, find the initial value's type and do
;;; PROPAGATE-FROM-SETS. We also derive the VALUE's type as the node's
;;; type.
(defun ir1-optimize-set (node)
  (declare (type cset node))
  (let ((var (set-var node)))
    (when (and (lambda-var-p var) (leaf-refs var))
      (let ((home (lambda-var-home var)))
        (when (eq (functional-kind home) :let)
          (let* ((initial-value (let-var-initial-value var))
                 (initial-type (lvar-type initial-value)))
            (setf (lvar-reoptimize initial-value) nil)
            (propagate-from-sets var initial-type))))))
  (derive-node-type node (make-single-value-type
                          (lvar-type (set-value node))))
  (setf (node-reoptimize node) nil)
  (values))

;;; Return true if the value of REF will always be the same (and is
;;; thus legal to substitute.)
(defun constant-reference-p (ref)
  (declare (type ref ref))
  (let ((leaf (ref-leaf ref)))
    (typecase leaf
      ((or constant functional) t)
      (lambda-var
       (null (lambda-var-sets leaf)))
      (defined-fun
       (not (eq (defined-fun-inlinep leaf) 'notinline)))
      (global-var
       (case (global-var-kind leaf)
         (:global-function
          (let ((name (leaf-source-name leaf)))
            (or (eq (sb-xc:symbol-package (fun-name-block-name name))
                    *cl-package*)
                (info :function :info name)))))))))

;;; If we have a non-set LET var with a single use, then (if possible)
;;; replace the variable reference's LVAR with the arg lvar.
;;;
;;; We change the REF to be a reference to NIL with unused value, and
;;; let it be flushed as dead code. A side effect of this substitution
;;; is to delete the variable.
(defun substitute-single-use-lvar (arg var)
  (declare (type lvar arg) (type lambda-var var))
  (binding* ((ref (first (leaf-refs var)))
             (lvar (node-lvar ref) :exit-if-null)
             (dest (lvar-dest lvar))
             (dest-lvar (when (valued-node-p dest) (node-lvar dest))))
    (when (and
           ;; Consider
           ;; (let ((x (the integer x))
           ;;       (y (the integer y)))
           ;;   (print (if c x y)))
           ;; If both substituted X and Y will write to the same LVAR
           ;; before C is checked.
           (or (eq (lvar-uses lvar) ref)
               ;; But if ARG is used just before the LET
               ;; and then VAR is used immediately,
               ;; the lvar can be substituted.
               (let-var-immediately-used-p ref var arg))
           (not (block-delete-p (node-block ref)))
           ;; If the destinatation is dynamic extent, don't substitute unless
           ;; the source is as well.
           (or (not dest-lvar)
               (not (lvar-dynamic-extent dest-lvar))
               (lvar-dynamic-extent lvar))
           (typecase dest
             ;; we should not change lifetime of unknown values lvars
             (cast
              (and (type-single-value-p (lvar-derived-type arg))
                   (multiple-value-bind (pdest pprev)
                       (principal-lvar-end lvar)
                     (declare (ignore pdest))
                     (lvar-single-value-p pprev))
                   ;; CASTs can disappear, don't substitute if
                   ;; DEST-LVAR has other uses
                   (block nil
                     (map-lvar-dest-casts
                      (lambda (cast)
                        (when (and (node-lvar cast)
                                   (consp (lvar-uses (node-lvar cast))))
                          (return)))
                      lvar)
                     t)))
             (mv-combination
              (or (eq (basic-combination-fun dest) lvar)
                  (and (eq (basic-combination-kind dest) :local)
                       (type-single-value-p (lvar-derived-type arg)))))
             ((or creturn exit)
              ;; This use has to produce a single value,
              ;; because binding a variable is how multiple values are
              ;; turned into a single value.
              (and (type-single-value-p (lvar-derived-type arg))
                   ;; Intervening nodes may produce non local exits with the same destination,
                   ;; generating unknown values or otherwise complicating stack-analyze.
                   ;; Due to inlining and other substitutions
                   ;; only (let ((x non-inlinable-call)) x) can be transformed
                   (almost-immediately-used-p lvar (lambda-bind (lambda-var-home var)))
                   ;; Nothing else exits from here
                   (singleton-p (block-pred (node-block dest)))
                   ;; Nothing happens between the call and the return
                   (lvar-almost-immediately-used-p arg)))
             (t
              (aver (lvar-single-value-p lvar))
              t))
           (eq (node-home-lambda ref)
               (lambda-home (lambda-var-home var))))
      (let ((ref-type (single-value-type (node-derived-type ref))))
        (cond ((or (csubtypep (single-value-type (lvar-type arg)) ref-type)
                   ;; Can't impart the same type to multiple uses, as
                   ;; they are coming from different branches with
                   ;; different derived values.
                   (consp (lvar-uses lvar)))
               (substitute-lvar-uses lvar arg
                                     ;; Really it is (EQ (LVAR-USES LVAR) REF):
                                     t)
               (delete-lvar-use ref))
              (t
               (let* ((value (make-lvar))
                      (cast (insert-cast-before ref value ref-type
                                                **zero-typecheck-policy**)))
                 (setf (cast-%type-check cast) nil)
                 (substitute-lvar-uses value arg
                                       ;; FIXME
                                       t)
                 (%delete-lvar-use ref)
                 (add-lvar-use cast lvar)))))
      (delete-ref ref)
      (unlink-node ref)
      (when (return-p dest)
        (do-uses (use lvar)
          (when (and (basic-combination-p use)
                     (eq (basic-combination-kind use) :local))
            (merge-tail-sets use))))
      (reoptimize-lvar lvar)
      t)))

;;; Delete a LET, removing the call and bind nodes, and warning about
;;; any unreferenced variables. Note that FLUSH-DEAD-CODE will come
;;; along right away and delete the REF and then the lambda, since we
;;; flush the FUN lvar.
(defun delete-let (clambda)
  (declare (type clambda clambda))
  (aver (functional-letlike-p clambda))
  (note-unreferenced-fun-vars clambda)
  (let ((call (let-combination clambda))
        (bind (lambda-bind clambda)))
    (flush-dest (basic-combination-fun call))
    (when (eq (car (node-source-path bind)) 'original-source-start)
      (setf (ctran-source-path (node-prev (car (leaf-refs clambda))))
            (node-source-path bind)))
    (unlink-node call)
    (unlink-node bind)
    (setf (lambda-bind clambda) nil))
  (setf (functional-kind clambda) :zombie)
  (let ((home (lambda-home clambda)))
    (setf (lambda-lets home) (delq1 clambda (lambda-lets home))))
  (values))

;;; This function is called when one of the arguments to a LET
;;; changes. We look at each changed argument. If the corresponding
;;; variable is set, then we call PROPAGATE-FROM-SETS. Otherwise, we
;;; consider substituting for the variable, and also propagate
;;; derived-type information for the arg to all the VAR's refs.
;;;
;;; Substitution is inhibited when the arg leaf's derived type isn't a
;;; subtype of the argument's leaf type. This prevents type checking
;;; from being defeated, and also ensures that the best representation
;;; for the variable can be used.
;;;
;;; Substitution of individual references is inhibited if the
;;; reference is in a different component from the home. This can only
;;; happen with closures over top level lambda vars. In such cases,
;;; the references may have already been compiled, and thus can't be
;;; retroactively modified.
;;;
;;; If all of the variables are deleted (have no references) when we
;;; are done, then we delete the LET.
;;;
;;; Note that we are responsible for clearing the LVAR-REOPTIMIZE
;;; flags.
(defun propagate-let-args (call fun)
  (declare (type basic-combination call) (type clambda fun))
  (map-combination-arg-var
   (lambda (arg var type)
     (cond
       ((lambda-var-deleted var))
       ((lambda-var-sets var)
        (propagate-from-sets var type))
       ((and arg
             (let ((use (lvar-uses arg)))
               (when (ref-p use)
                 (let ((leaf (ref-leaf use)))
                   (when (and (constant-reference-p use)
                              (csubtypep (leaf-type leaf)
                                         ;; (NODE-DERIVED-TYPE USE) would
                                         ;; be better -- APD, 2003-05-15
                                         (leaf-type var)))
                     (propagate-lvar-annotations-to-refs arg var)
                     (propagate-to-refs var type)
                     (unless (preserve-single-use-debug-var-p call var)
                       (update-lvar-dependencies leaf arg)
                       (propagate-ref-dx use arg)
                       (let ((use-component (node-component use)))
                         (substitute-leaf-if
                          (lambda (ref)
                            ;; Some unreachable function may be in a different component,
                            ;; don't worry about it
                            (eq (node-component ref) use-component))
                          leaf var)))
                     t))))))
       ((and arg
             (null (rest (leaf-refs var)))
             (not (preserve-single-use-debug-var-p call var))
             (substitute-single-use-lvar arg var)))
       (t
         (propagate-to-refs var type))))
   call
   :reoptimize t)

  (when (every #'not (basic-combination-args call))
    (delete-let fun))

  (values))

;;; This function is called when one of the args to a non-LET local
;;; call changes. For each changed argument corresponding to an unset
;;; variable, we compute the union of the types across all calls and
;;; propagate this type information to the var's refs.
;;;
;;; If the function has an entry-fun, then we don't do anything: since
;;; it has a XEP we would not discover anything.
;;;
;;; If the function is an optional-entry-point, we will just make sure
;;; &REST lists are known to be lists. Doing the regular rigamarole
;;; can erronously propagate too strict types into refs: see
;;; BUG-655203-REGRESSION in tests/compiler.pure.lisp.
;;;
;;; We can clear the LVAR-REOPTIMIZE flags for arguments in all calls
;;; corresponding to changed arguments in CALL, since the only use in
;;; IR1 optimization of the REOPTIMIZE flag for local call args is
;;; right here.
(defun propagate-local-call-args (call fun)
  (declare (type combination call) (type clambda fun))
  (unless (functional-entry-fun fun)
    (if (lambda-optional-dispatch fun)
        ;; We can still make sure &REST is known to be a list.
        (loop for var in (lambda-vars fun)
              do (let ((info (lambda-var-arg-info var)))
                   (when (and info (eq :rest (arg-info-kind info)))
                     (propagate-from-sets var (specifier-type 'list)))))
        ;; The normal case.
        (let* ((vars (lambda-vars fun))
               (union (mapcar (lambda (arg var)
                                (when (and arg
                                           (lvar-reoptimize arg)
                                           (null (basic-var-sets var)))
                                  (lvar-type arg)))
                              (basic-combination-args call)
                              vars))
               (this-ref (lvar-use (basic-combination-fun call))))

          (dolist (arg (basic-combination-args call))
            (when arg
              (setf (lvar-reoptimize arg) nil)))

          (dolist (ref (leaf-refs fun))
            (let ((dest (node-dest ref)))
              (unless (or (eq ref this-ref) (not dest))
                (setq union
                      (mapcar (lambda (this-arg old)
                                (when old
                                  (setf (lvar-reoptimize this-arg) nil)
                                  (type-union (lvar-type this-arg) old)))
                              (basic-combination-args dest)
                              union)))))

          (loop for var in vars
                and type in union
                when type do (propagate-to-refs var type)))))

  (values))

;;;; multiple values optimization

;;; Do stuff to notice a change to a MV combination node. There are
;;; two main branches here:
;;;  -- If the call is local, then it is already a MV let, or should
;;;     become one. Note that although all :LOCAL MV calls must eventually
;;;     be converted to :MV-LETs, there can be a window when the call
;;;     is local, but has not been LET converted yet. This is because
;;;     the entry-point lambdas may have stray references (in other
;;;     entry points) that have not been deleted yet.
;;;  -- The call is full. This case is somewhat similar to the non-MV
;;;     combination optimization: we propagate return type information and
;;;     notice non-returning calls. We also have an optimization
;;;     which tries to convert MV-CALLs into MV-binds.
(defun ir1-optimize-mv-combination (node)
  (let ((fun (basic-combination-fun node)))
    (unless (and (node-p (lvar-uses fun))
                 (node-to-be-deleted-p (lvar-uses fun)))
      (ecase (basic-combination-kind node)
        (:local
         (let ((lambda (combination-lambda node)))
           (when (lvar-reoptimize fun)
             (setf (lvar-reoptimize fun) nil)
             (or (maybe-let-convert lambda)
                 (maybe-convert-to-assignment lambda)))
           (cond ((neq (functional-kind lambda) :mv-let)
                  (loop for arg in (basic-combination-args node)
                        do
                        (setf (lvar-reoptimize arg) nil)))
                 ((convert-mv-bind-to-let node))
                 (t
                  (propagate-let-args node lambda)))))
        (:full
         (let* ((fun-changed (lvar-reoptimize fun)))
           (loop for arg in (basic-combination-args node)
                 do
                 (setf (lvar-reoptimize arg) nil))
           (when fun-changed
             (setf (lvar-reoptimize fun) nil)
             (let ((type (lvar-fun-type fun t)))
               (when (fun-type-p type)
                 (derive-node-type node (fun-type-returns type))))
             (maybe-terminate-block node nil)
             (let ((use (lvar-uses fun)))
               (when (and (ref-p use) (functional-p (ref-leaf use)))
                 (convert-call-if-possible use node)
                 (when (eq (basic-combination-kind node) :local)
                   (or (maybe-let-convert (ref-leaf use))
                       (maybe-convert-to-assignment (ref-leaf use)))))))
           (unless (or (eq (basic-combination-kind node) :local)
                       (eq (lvar-fun-name fun) '%throw))
             (ir1-optimize-mv-call node))))
        (:error))))

  (values))

(declaim (end-block))

(defun count-values (call &optional min)
  (loop for arg in (basic-combination-args call)
        for nvals = (nth-value 1 (values-types (lvar-derived-type arg)))
        if (eq nvals :unknown)
        do (unless min
             (return))
        else
        sum nvals))

(defun check-mv-call-arguments (call)
  (let* ((*compiler-error-context* call)
         (fun (basic-combination-fun call))
         (min-args (count-values call t)))
    (when min-args
      (multiple-value-bind (min-accepted max-accepted) (fun-type-nargs (lvar-fun-type fun))
        (when (or (not min-accepted)
                  (and max-accepted
                       (> min-args max-accepted)))
          (assert-lvar-type fun
                            (specifier-type (list 'function
                                                  (append (make-list min-args :initial-element 't)
                                                          '(&rest t))))
                            (lexenv-policy (node-lexenv call))
                            :mv-call))))))


(defun ir1-optimize-mv-call (node)
  (let* ((fun (basic-combination-fun node))
         (uses (lvar-uses (basic-combination-fun node)))
         (count (count-values node)))
    (if count
        (with-ir1-environment-from-node node
          (let* ((dums (make-gensym-list count))
                 (ignore (gensym))
                 (ref-p (ref-p uses))
                 (new-fun (ir1-convert-lambda
                           `(lambda (&optional ,@dums &rest ,ignore)
                              (declare (ignore ,ignore))
                              ;; REFERENCE-LEAF does a better job referencing
                              ;; DEFINED-FUNs than just using the LVAR.
                              ,(cond (ref-p
                                      `(%funcall ,(ref-leaf uses) ,@dums))
                                     (t
                                      `(%funcall-lvar ,fun ,@dums)))))))
            (cond (ref-p
                   (change-ref-leaf uses new-fun))
                  (t
                   (reoptimize-lvar fun)
                   (setf (basic-combination-fun node)
                         (insert-ref-before new-fun node))))
            (aver (eq (basic-combination-kind node) :full))
            (locall-analyze-component *current-component*)
            (aver (eq (basic-combination-kind node) :local))))
        ;; The total argument count is not known, but all the known
        ;; values can already cause a conflict.
        (check-mv-call-arguments node)))
  (values))

;;; m-v-bind with where the form's lvar has multiple uses cannot be
;;; converted to LET in general, see SUBSTITUTE-SINGLE-USE-LVAR for an
;;; example why.
;;; But removing unused variables allows some optimizations to proceed.
;;;
;;; UNKNOWN-VALUES means some of the uses produce something other
;;; than a single value. Don't remove any values from the VALUES
;;; calls, but replace them with NILs in that case.
(defun remove-unused-vars-in-mv-bind (uses fun &optional unknown-values)
  (let ((vars (lambda-vars fun)))
    (loop for use in uses
          do
          (flet ((make-nil (&optional (lvar (make-lvar use)))
                   ;; Reference NIL for unsupplied arguments
                   (with-ir1-environment-from-node use
                     (let* ((node-prev (node-prev use))
                            (ctran (make-ctran)))
                       (setf (ctran-next node-prev) nil)
                       (reference-constant node-prev ctran lvar nil)
                       (link-node-to-previous-ctran use ctran)
                       lvar))))
            (cond ((and (combination-p use)
                        (eq (lvar-fun-name (combination-fun use))
                            'values))
                   (let* ((vars vars)
                          (args (combination-args use))
                          (new-args
                            (loop while (or vars args)
                                  when
                                  (let ((arg (pop args))
                                        (var (pop vars)))
                                    (cond ((not arg)
                                           (make-nil))
                                          ((and var
                                                (leaf-refs var))
                                           arg)
                                          (t
                                           (flush-dest arg)
                                           (if unknown-values
                                               (make-nil)
                                               nil))))
                                  collect it)))
                     (setf (combination-args use) new-args)
                     (derive-node-type use
                                       (make-values-type :required (mapcar #'lvar-type new-args))
                                       :from-scratch t)
                     (reoptimize-node use)))
                  ;; Doesn't return a single value, nothing can be done about that
                  ((and unknown-values
                        (not (type-single-value-p (node-derived-type use)))))
                  ;; single-value returning forms
                  ((leaf-refs (car vars))) ;; the first value is used, nothing to do
                  (t
                   ;; A single NIL will do
                   (let ((lvar (node-lvar use)))
                     (delete-lvar-use use)
                     (make-nil lvar))))))
    (unless unknown-values
      (setf (lambda-vars fun)
            (remove-if-not #'leaf-refs (lambda-vars fun))))))

;;; If we see:
;;;    (multiple-value-bind
;;;     (x y)
;;;     (values xx yy)
;;;      ...)
;;; Convert to:
;;;    (let ((x xx)
;;;          (y yy))
;;;      ...)
;;; (multiple-value-bind (unused y) (if c (values 1 2) (values 3 4)))
;;; to (let ((y (if c (values 2) (values 4)))))
;;;
;;; (multiple-value-bind (unused y z) (if c (values 1 2 3) (values 4 5)))
;;; to
;;; (multiple-value-bind (y z) (if c (values 2 3) (values 5 nil)))
;;;
;;; (multiple-value-bind (unused y) (if c unknown (values 3 4)))
;;; to
;;; (multiple-value-bind (unused y) (if c unknown (values nil 4)))
(defun convert-mv-bind-to-let (call)
  (declare (type mv-combination call))
  (let* ((args (basic-combination-args call))
         (uses (ensure-list (lvar-uses (first args))))
         (fun-lvar (mv-combination-fun call))
         (fun (ref-leaf (lvar-uses fun-lvar)))
         (vars (lambda-vars fun))
         (n-used-vars (count-if #'leaf-refs vars))
         (nvars (length vars))
         (multiple-uses (cdr uses))
         unknown-values)
    (cond ((or (cdr args)
               (not
                (if multiple-uses
                    (loop with known-values
                          for use in uses
                          do (if (or (and (combination-p use)
                                          (eq (lvar-fun-name (combination-fun use))
                                              'values))
                                     (type-single-value-p (node-derived-type use)))
                                 (setf known-values t)
                                 (setf unknown-values t))
                             ;; At least some values have to be known,
                             ;; otherwise there's nothing to remove.
                          finally (return known-values))
                    (loop for use in uses
                          always (and (combination-p use)
                                      (eq (lvar-fun-name (combination-fun use))
                                          'values)))))
               ;; TODO: make mv-combination functions handle replacing
               ;; unused variables with NIL and then this can work
               ;; properly.
               unknown-values)
           nil)
          ((and multiple-uses
                (or unknown-values
                    (/= nvars n-used-vars 1)))
           (remove-unused-vars-in-mv-bind uses fun unknown-values))
          ((or (not multiple-uses)
               (= n-used-vars 1))
           (with-ir1-environment-from-node call
             (let* ((new-call (make-combination fun-lvar))
                    (new-lvars (loop repeat (if multiple-uses
                                                1
                                                nvars)
                                     collect (make-lvar new-call))))
               (setf (functional-kind fun) :let)
               (setf (combination-kind new-call) :local)
               (setf (combination-args new-call) new-lvars)
               (setf (lvar-dest fun-lvar) new-call)
               (insert-node-before call new-call)
               (unlink-node call)
               (cond (multiple-uses
                      (remove-unused-vars-in-mv-bind uses fun)
                      (substitute-lvar-uses (car new-lvars) (car args) nil))
                     (t
                      (loop for use in uses
                            for args = (combination-args use)
                            for types = (values-type-types (node-derived-type use))
                            for lvars = new-lvars
                            do
                            (loop while (and args lvars)
                                  do
                                  (let ((arg (pop args))
                                        (new-lvar (pop lvars))
                                        (type (pop types)))
                                    (when (and type
                                               (not (type-asserted-p arg type)))
                                      ;; Propagate derived types from the VALUES call to its args:
                                      ;; transforms can leave the VALUES call with a better type
                                      ;; than its args have, so make sure not to throw that away.
                                      (do-uses (node arg)
                                        (derive-node-type node type)))
                                    (substitute-lvar-uses new-lvar arg nil)))
                            ;; Discard unused arguments
                            (loop for arg in args
                                  do (flush-dest arg))
                            ;; Reference NIL for unsupplied arguments
                            (when lvars
                              (with-ir1-environment-from-node use
                                (let ((node-prev (node-prev use)))
                                  (setf (node-prev use) nil)
                                  (setf (ctran-next node-prev) nil)
                                  (loop for lvar in lvars
                                        for prev = node-prev then ctran
                                        for ctran = (make-ctran)
                                        do
                                        (reference-constant prev ctran lvar nil)
                                        finally
                                        (link-node-to-previous-ctran use ctran)))))
                            (flush-dest (combination-fun use))
                            (unlink-node use))))
               (propagate-to-args new-call fun)
               (reoptimize-call new-call)))
           t))))

;;; If we see:
;;;    (values-list (list x y z))
;;;
;;; Convert to:
;;;    (values x y z)
;;;
;;; In implementation, this is somewhat similar to
;;; CONVERT-MV-BIND-TO-LET. We grab the args of LIST and make them
;;; args of the VALUES-LIST call, flushing the old argument lvar
;;; (allowing the LIST to be flushed.)
;;;
;;; FIXME: Thus we lose possible type assertions on (LIST ...).
(defoptimizer (values-list optimizer) ((list) node)
  (let ((use (lvar-uses list)))
    (when (combination-p use)
      (let ((name (lvar-fun-name (combination-fun use)))
            (args (combination-args use)))
        ;; Recognizing LIST* seems completely ad-hoc, however, once upon a time
        ;; (LIST x) was translated to (CONS x NIL), so in order for this optimizer to
        ;; pick off (VALUES-LIST (LIST x)), it had to instead look for (CONS x NIL).
        ;; Realistically nobody should hand-write (VALUES-LIST ({CONS | LIST*} x NIL))
        ;; so it seems very questionable in utility to preserve both spellings.
        (when (or (eq name 'list)
                  (and (eq name 'list*)
                       (let ((cdr (car (last args))))
                        (and (lvar-value-is-nil cdr)
                             (progn
                               (flush-dest cdr)
                               (setf args (butlast args))
                               t)))))

          ;; FIXME: VALUES might not satisfy an assertion on NODE-LVAR.
          (change-ref-leaf (lvar-uses (combination-fun node))
                           (find-free-fun 'values "in a strange place"))
          (setf (combination-kind node) :full)
          (dolist (arg args)
            (setf (lvar-dest arg) node))
          (setf (combination-args use) nil)
          (flush-dest list)
          (flush-combination use)
          (setf (combination-args node) args)
          t)))))

;;; If VALUES appears in a non-MV context, then effectively convert it
;;; to a PROG1. This allows the computation of the additional values
;;; to become dead code.
(deftransform values ((&rest vals) * * :node node)
  (unless (lvar-single-value-p (node-lvar node))
    (give-up-ir1-transform))
  (setf (node-derived-type node)
        (make-short-values-type (list (single-value-type
                                       (node-derived-type node)))))
  (principal-lvar-single-valuify (node-lvar node))
  (if vals
      (let ((dummies (make-gensym-list (length (cdr vals)))))
        `(lambda (val ,@dummies)
           (declare (ignore ,@dummies))
           val))
      nil))

;;; TODO:
;;; - CAST chains;
(defun delete-cast (cast)
  (declare (type cast cast))
  (let ((value (cast-value cast))
        (lvar (cast-lvar cast)))
    (when (and (bound-cast-p cast)
               (bound-cast-check cast)
               (not (node-deleted (bound-cast-check cast))))
      (flush-combination (bound-cast-check cast))
      (setf (bound-cast-check cast) nil))
    (unless (array-index-cast-p cast)
      ;; Normally the types are the same, as the cast gets its derived
      ;; type from the lvar, but it may get a different type when an
      ;; inlined function with a derived type is let-converted.
      (let ((type (node-derived-type cast)))
        (do-uses (node value)
          (derive-node-type node type))))
    (delete-filter cast lvar value)
    (when lvar
      (reoptimize-lvar lvar)
      (when (lvar-single-value-p lvar)
        (note-single-valuified-lvar lvar)))
    (values)))

(defun compile-time-type-error-context (context)
  #+sb-xc-host context
  #-sb-xc-host (source-to-string context))

(defun may-delete-bound-cast (cast)
  (when (bound-cast-check cast)
    (when (constant-lvar-p (bound-cast-bound cast))
      (setf (cast-asserted-type cast)
            (specifier-type `(integer 0 (,(lvar-value (bound-cast-bound cast)))))
            (bound-cast-derived cast) t))
    (when (policy cast (= insert-array-bounds-checks 0))
      (flush-combination (bound-cast-check cast))
      (setf (bound-cast-check cast) nil)))
  (bound-cast-derived cast))

(defun may-delete-cast (cast)
  (typecase cast
    (delay nil)
    (bound-cast
     (may-delete-bound-cast cast))
    (t t)))

(defun cast-mismatch-from-inlined-p (cast node)
  (let* ((path (node-source-path node))
         (transformed (memq 'transformed path))
         (inlined))
    (cond ((and transformed
                (not (eq (memq 'transformed (node-source-path cast))
                         transformed))))
          ((setf inlined
                 (memq 'inlined path))
           (not (eq (memq 'inlined (node-source-path cast))
                    inlined))))))

;;; Delete or move around casts when possible
(defun maybe-delete-cast (cast)
  (let ((lvar (cast-lvar cast))
        (value (cast-value cast)))
    (cond ((not (may-delete-cast cast))
           nil)
          ((values-subtypep (lvar-derived-type value)
                            (cast-asserted-type cast))
           (delete-cast cast)
           t)
          ((and (fun-type-p (cast-asserted-type cast))
                (let ((uses (lvar-uses value)))
                  (when (ref-p uses)
                    (let ((fun (ref-leaf uses)))
                      (when (and (functional-p fun)
                                 (functional-entry-fun fun))
                        ;; FIXME: is it important to compute this once?
                        (csubtypep (definition-type (functional-entry-fun fun))
                                   (cast-asserted-type cast)))))))
           (delete-cast cast)
           t)
          ((listp (lvar-uses value))
           ;; Turn (the vector (if x y #())) into
           ;; (if x (the vector y) #())
           (let ((atype (cast-asserted-type cast))
                 (ctran (node-next cast))
                 (dest (and lvar
                            (lvar-dest lvar)))
                 next-block)
             (collect ((merges))
               (do-uses (use value)
                 (when (and (values-subtypep (node-derived-type use) atype)
                            (immediately-used-p value use))
                   (unless next-block
                     (when ctran (ensure-block-start ctran))
                     (setq next-block (first (block-succ (node-block cast))))
                     (ensure-block-start (node-prev cast))
                     (reoptimize-lvar lvar)
                     (setf (lvar-%derived-type value) nil))
                   (%delete-lvar-use use)
                   (add-lvar-use use lvar)
                   (unlink-blocks (node-block use) (node-block cast))
                   (link-blocks (node-block use) next-block)
                   ;; At least one use is good, downgrade any possible
                   ;; type conflicts to style warnings.
                   (setf (cast-silent-conflict cast)
                         (if (cast-mismatch-from-inlined-p cast use)
                             t
                             :style-warning))
                   (when (and (return-p dest)
                              (basic-combination-p use)
                              (eq (basic-combination-kind use) :local))
                     (merges use))))
               (dolist (use (merges))
                 (merge-tail-sets use))))))))

(defun ir1-optimize-cast (cast)
  (declare (type cast cast))
  (unless (maybe-delete-cast cast)
    (let* ((value (cast-value cast))
           (atype (cast-asserted-type cast))
           (value-type (lvar-derived-type value))
           (int (values-type-intersection value-type atype)))
      (derive-node-type cast int)
      (cond ((or (neq int *empty-type*)
                 (eq value-type *empty-type*)))
            ;; No need to transform into an analog of
            ;; %COMPILE-TIME-TYPE-ERROR, %CHECK-BOUND will signal at
            ;; run-time and %CHECK-BOUND ir2-converter will signal at
            ;; compile-time if it survives further stages of ir1
            ;; optimization.
            ((and (bound-cast-p cast)
                  (bound-cast-check cast)))
            ((and (eq atype *empty-type*)
                  (do-uses (node value t)
                    (unless (basic-combination-p node)
                      (return))))
             ;; Combinations have nil-fun-returned-error
             (setf (cast-%type-check cast) nil))
            (t
             (let* ((source-form (node-source-form cast))
                    (detail (lvar-all-sources (cast-value cast)))
                    (context (cast-context cast))
                    (context (if (opaque-box-p context)
                                 (opaque-box-value context)
                                 context))
                    (context (if (local-call-context-p context)
                                 (local-call-context-var context)
                                 context)))
               (when (or (not (cast-silent-conflict cast))
                         (and (eq (cast-silent-conflict cast) :style-warning)
                              (not (cast-single-value-p cast))
                              (setf context :multiple-values)))
                 (filter-lvar
                  value
                  (if (cast-single-value-p cast)
                      (lambda (dummy) `(list ,dummy))
                      (lambda (dummy) `(multiple-value-call #'list ,dummy)))))
               (filter-lvar
                (cast-value cast)
                ;; FIXME: Derived type.
                (if (cast-silent-conflict cast)
                    (lambda (dummy)
                      (let ((dummy-sym (gensym)))
                        `(let ((,dummy-sym ,dummy))
                           ,@(and (eq (cast-silent-conflict cast) :style-warning)
                                  `((%compile-time-type-style-warn ,dummy-sym
                                                                   ',(type-specifier atype)
                                                                   ',(type-specifier value-type)
                                                                   ',detail
                                                                   ',(compile-time-type-error-context source-form)
                                                                   ',context)))
                           ,(internal-type-error-call dummy-sym atype context)
                           ,dummy-sym)))
                    (lambda (dummy)
                      `(%compile-time-type-error ,dummy
                                                 ',(type-specifier atype)
                                                 ',(type-specifier value-type)
                                                 ',detail
                                                 ',(compile-time-type-error-context source-form)
                                                 ',context))))
               ;; maybe-terminate-block during ir1-convert (in filter-lvar) doesn't
               ;; properly terminate blocks for NIL returning functions, do it manually here.
               (setq value (cast-value cast))
               (derive-node-type (lvar-uses value) *empty-type*)
               (maybe-terminate-block (lvar-uses value) nil))
             (return-from ir1-optimize-cast)))
      (when (eq (node-derived-type cast) *empty-type*)
        (maybe-terminate-block cast nil))

      (when (and (cast-%type-check cast)
                 (values-subtypep value-type
                                  (cast-type-to-check cast)))
        (setf (cast-%type-check cast) nil))
      (setf (node-reoptimize cast) nil))))

(defun cast-type-check (cast)
  (declare (type cast cast))
  (let ((check (cast-%type-check cast)))
    (if (and check
             (cast-reoptimize cast)
             (values-subtypep (lvar-derived-type (cast-value cast))
                              (cast-type-to-check cast)))
        (setf (cast-%type-check cast) nil)
        check)))
