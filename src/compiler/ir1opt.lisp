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

(in-package "SB!C")

;;;; interface for obtaining results of constant folding

;;; Return true for a CONTINUATION whose sole use is a reference to a
;;; constant leaf.
(defun constant-continuation-p (thing)
  (and (continuation-p thing)
       (let ((use (continuation-use thing)))
	 (and (ref-p use)
	      (constant-p (ref-leaf use))))))

;;; Return the constant value for a continuation whose only use is a
;;; constant node.
(declaim (ftype (function (continuation) t) continuation-value))
(defun continuation-value (cont)
  (aver (constant-continuation-p cont))
  (constant-value (ref-leaf (continuation-use cont))))

;;;; interface for obtaining results of type inference

;;; Return a (possibly values) type that describes what we have proven
;;; about the type of Cont without taking any type assertions into
;;; consideration. This is just the union of the NODE-DERIVED-TYPE of
;;; all the uses. Most often people use CONTINUATION-DERIVED-TYPE or
;;; CONTINUATION-TYPE instead of using this function directly.
(defun continuation-proven-type (cont)
  (declare (type continuation cont))
  (ecase (continuation-kind cont)
    ((:block-start :deleted-block-start)
     (let ((uses (block-start-uses (continuation-block cont))))
       (if uses
	   (do ((res (node-derived-type (first uses))
		     (values-type-union (node-derived-type (first current))
					res))
		(current (rest uses) (rest current)))
	       ((null current) res))
	   *empty-type*)))
    (:inside-block
     (node-derived-type (continuation-use cont)))))

;;; Our best guess for the type of this continuation's value. Note
;;; that this may be VALUES or FUNCTION type, which cannot be passed
;;; as an argument to the normal type operations. See
;;; CONTINUATION-TYPE. This may be called on deleted continuations,
;;; always returning *.
;;;
;;; What we do is call CONTINUATION-PROVEN-TYPE and check whether the
;;; result is a subtype of the assertion. If so, return the proven
;;; type and set TYPE-CHECK to nil. Otherwise, return the intersection
;;; of the asserted and proven types, and set TYPE-CHECK T. If
;;; TYPE-CHECK already has a non-null value, then preserve it. Only in
;;; the somewhat unusual circumstance of a newly discovered assertion
;;; will we change TYPE-CHECK from NIL to T.
;;;
;;; The result value is cached in the CONTINUATION-%DERIVED-TYPE slot.
;;; If the slot is true, just return that value, otherwise recompute
;;; and stash the value there.
#!-sb-fluid (declaim (inline continuation-derived-type))
(defun continuation-derived-type (cont)
  (declare (type continuation cont))
  (or (continuation-%derived-type cont)
      (%continuation-derived-type cont)))
(defun %continuation-derived-type (cont)
  (declare (type continuation cont))
  (let ((proven (continuation-proven-type cont))
	(asserted (continuation-asserted-type cont)))
    (cond ((values-subtypep proven asserted)
	   (setf (continuation-%type-check cont) nil)
	   (setf (continuation-%derived-type cont) proven))
          ((and (values-subtypep proven (specifier-type 'function))
                (values-subtypep asserted (specifier-type 'function)))
	   ;; It's physically impossible for a runtime type check to
	   ;; distinguish between the various subtypes of FUNCTION, so
	   ;; it'd be pointless to do more type checks here.
           (setf (continuation-%type-check cont) nil)
           (setf (continuation-%derived-type cont)
		 ;; FIXME: This should depend on optimization
		 ;; policy. This is for SPEED > SAFETY:
                 #+nil (values-type-intersection asserted proven)
                 ;; and this is for SAFETY >= SPEED:
                 #-nil proven))
	  (t
	   (unless (or (continuation-%type-check cont)
		       (not (continuation-dest cont))
		       (eq asserted *universal-type*))
	     (setf (continuation-%type-check cont) t))

	   (setf (continuation-%derived-type cont)
		 (values-type-intersection asserted proven))))))

;;; Call CONTINUATION-DERIVED-TYPE to make sure the slot is up to
;;; date, then return it.
#!-sb-fluid (declaim (inline continuation-type-check))
(defun continuation-type-check (cont)
  (declare (type continuation cont))
  (continuation-derived-type cont)
  (continuation-%type-check cont))

;;; Return the derived type for CONT's first value. This is guaranteed
;;; not to be a VALUES or FUNCTION type.
(declaim (ftype (function (continuation) ctype) continuation-type))
(defun continuation-type (cont)
  (single-value-type (continuation-derived-type cont)))

;;; If CONT is an argument of a function, return a type which the
;;; function checks CONT for.
#!-sb-fluid (declaim (inline continuation-externally-checkable-type))
(defun continuation-externally-checkable-type (cont)
  (or (continuation-%externally-checkable-type cont)
      (%continuation-%externally-checkable-type cont)))
(defun %continuation-%externally-checkable-type (cont)
  (declare (type continuation cont))
  (let ((dest (continuation-dest cont)))
      (if (not (and dest (combination-p dest)))
          ;; TODO: MV-COMBINATION
          (setf (continuation-%externally-checkable-type cont) *wild-type*)
          (let* ((fun (combination-fun dest))
                 (args (combination-args dest))
                 (fun-type (continuation-type fun)))
            (if (or (not (fun-type-p fun-type))
                    ;; FUN-TYPE might be (AND FUNCTION (SATISFIES ...)).
                    (fun-type-wild-args fun-type))
                (progn (dolist (arg args)
                         (when arg
                           (setf (continuation-%externally-checkable-type arg)
                                 *wild-type*)))
                       *wild-type*)
                (let* ((arg-types (append (fun-type-required fun-type)
                                          (fun-type-optional fun-type)
                                          (let ((rest (list (or (fun-type-rest fun-type)
                                                                *wild-type*))))
                                            (setf (cdr rest) rest)))))
                  ;; TODO: &KEY
                  (loop
                     for arg of-type continuation in args
                     and type of-type ctype in arg-types
                     do (when arg
                          (setf (continuation-%externally-checkable-type arg)
                                type)))
                  (continuation-%externally-checkable-type cont)))))))

;;;; interface routines used by optimizers

;;; This function is called by optimizers to indicate that something
;;; interesting has happened to the value of Cont. Optimizers must
;;; make sure that they don't call for reoptimization when nothing has
;;; happened, since optimization will fail to terminate.
;;;
;;; We clear any cached type for the continuation and set the
;;; reoptimize flags on everything in sight, unless the continuation
;;; is deleted (in which case we do nothing.)
;;;
;;; Since this can get called during IR1 conversion, we have to be
;;; careful not to fly into space when the Dest's Prev is missing.
(defun reoptimize-continuation (cont)
  (declare (type continuation cont))
  (unless (member (continuation-kind cont) '(:deleted :unused))
    (setf (continuation-%derived-type cont) nil)
    (let ((dest (continuation-dest cont)))
      (when dest
	(setf (continuation-reoptimize cont) t)
	(setf (node-reoptimize dest) t)
	(let ((prev (node-prev dest)))
	  (when prev
	    (let* ((block (continuation-block prev))
		   (component (block-component block)))
	      (when (typep dest 'cif)
		(setf (block-test-modified block) t))
	      (setf (block-reoptimize block) t)
	      (setf (component-reoptimize component) t))))))
    (do-uses (node cont)
      (setf (block-type-check (node-block node)) t)))
  (values))

;;; Annotate NODE to indicate that its result has been proven to be
;;; TYPEP to RTYPE. After IR1 conversion has happened, this is the
;;; only correct way to supply information discovered about a node's
;;; type. If you screw with the NODE-DERIVED-TYPE directly, then
;;; information may be lost and reoptimization may not happen.
;;;
;;; What we do is intersect RTYPE with NODE's DERIVED-TYPE. If the
;;; intersection is different from the old type, then we do a
;;; REOPTIMIZE-CONTINUATION on the NODE-CONT.
(defun derive-node-type (node rtype)
  (declare (type node node) (type ctype rtype))
  (let ((node-type (node-derived-type node)))
    (unless (eq node-type rtype)
      (let ((int (values-type-intersection node-type rtype)))
	(when (type/= node-type int)
	  (when (and *check-consistency*
		     (eq int *empty-type*)
		     (not (eq rtype *empty-type*)))
	    (let ((*compiler-error-context* node))
	      (compiler-warn
	       "New inferred type ~S conflicts with old type:~
		~%  ~S~%*** possible internal error? Please report this."
	       (type-specifier rtype) (type-specifier node-type))))
	  (setf (node-derived-type node) int)
	  (reoptimize-continuation (node-cont node))))))
  (values))

(defun set-continuation-type-assertion (cont atype ctype)
  (declare (type continuation cont) (type ctype atype ctype))
  (when (eq atype *wild-type*)
    (return-from set-continuation-type-assertion))
  (let* ((old-atype (continuation-asserted-type cont))
         (old-ctype (continuation-type-to-check cont))
         (new-atype (values-type-intersection old-atype atype))
         (new-ctype (values-type-intersection old-ctype ctype)))
    (when (or (type/= old-atype new-atype)
              (type/= old-ctype new-ctype))
      (setf (continuation-asserted-type cont) new-atype)
      (setf (continuation-type-to-check cont) new-ctype)
      (do-uses (node cont)
        (setf (block-attributep (block-flags (node-block node))
                                type-check type-asserted)
              t))
      (reoptimize-continuation cont)))
  (values))

;;; This is similar to DERIVE-NODE-TYPE, but asserts that it is an
;;; error for CONT's value not to be TYPEP to TYPE. If we improve the
;;; assertion, we set TYPE-CHECK and TYPE-ASSERTED to guarantee that
;;; the new assertion will be checked.
(defun assert-continuation-type (cont type policy)
  (declare (type continuation cont) (type ctype type))
  (when (eq type *wild-type*)
    (return-from assert-continuation-type))
  (set-continuation-type-assertion cont type (maybe-weaken-check type policy)))

;;; Assert that CALL is to a function of the specified TYPE. It is
;;; assumed that the call is legal and has only constants in the
;;; keyword positions.
(defun assert-call-type (call type)
  (declare (type combination call) (type fun-type type))
  (derive-node-type call (fun-type-returns type))
  (let ((args (combination-args call))
        (policy (lexenv-policy (node-lexenv call))))
    (dolist (req (fun-type-required type))
      (when (null args) (return-from assert-call-type))
      (let ((arg (pop args)))
	(assert-continuation-type arg req policy)))
    (dolist (opt (fun-type-optional type))
      (when (null args) (return-from assert-call-type))
      (let ((arg (pop args)))
	(assert-continuation-type arg opt policy)))

    (let ((rest (fun-type-rest type)))
      (when rest
	(dolist (arg args)
	  (assert-continuation-type arg rest policy))))

    (dolist (key (fun-type-keywords type))
      (let ((name (key-info-name key)))
	(do ((arg args (cddr arg)))
	    ((null arg))
	  (when (eq (continuation-value (first arg)) name)
	    (assert-continuation-type
	     (second arg) (key-info-type key)
             policy))))))
  (values))

;;;; IR1-OPTIMIZE

;;; Do one forward pass over COMPONENT, deleting unreachable blocks
;;; and doing IR1 optimizations. We can ignore all blocks that don't
;;; have the REOPTIMIZE flag set. If COMPONENT-REOPTIMIZE is true when
;;; we are done, then another iteration would be beneficial.
(defun ir1-optimize (component)
  (declare (type component component))
  (setf (component-reoptimize component) nil)
  (do-blocks (block component)
    (cond
      ;; We delete blocks when there is either no predecessor or the
      ;; block is in a lambda that has been deleted. These blocks
      ;; would eventually be deleted by DFO recomputation, but doing
      ;; it here immediately makes the effect available to IR1
      ;; optimization.
      ((or (block-delete-p block)
           (null (block-pred block)))
       (delete-block block))
      ((eq (functional-kind (block-home-lambda block)) :deleted)
       ;; Preserve the BLOCK-SUCC invariant that almost every block has
       ;; one successor (and a block with DELETE-P set is an acceptable
       ;; exception).
       (mark-for-deletion block)
       (delete-block block))
      (t
       (loop
          (let ((succ (block-succ block)))
            (unless (and succ (null (rest succ)))
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

       (when (and (block-reoptimize block) (block-component block))
         (aver (not (block-delete-p block)))
         (ir1-optimize-block block))

       (cond ((block-delete-p block)
              (delete-block block))
             ((and (block-flush-p block) (block-component block))
              (flush-dead-code block))))))

  (values))

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
  (do-nodes (node cont block :restart-p t)
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
	 ;; type. We don't propagate CONT's assertion to the VALUE,
	 ;; since if we did, this would move the checking of CONT's
	 ;; assertion to the exit. This wouldn't work with CATCH and
	 ;; UWP, where the EXIT node is just a placeholder for the
	 ;; actual unknown exit.
	 (let ((value (exit-value node)))
	   (when value
	     (derive-node-type node (continuation-derived-type value)))))
	(cset
	 (ir1-optimize-set node)))))
  (values))

;;; Try to join with a successor block. If we succeed, we return true,
;;; otherwise false.
(defun join-successor-if-possible (block)
  (declare (type cblock block))
  (let ((next (first (block-succ block))))
    (when (block-start next)
      (let* ((last (block-last block))
	     (last-cont (node-cont last))
	     (next-cont (block-start next)))
	(cond (;; We cannot combine with a successor block if:
	       (or
		;; The successor has more than one predecessor.
		(rest (block-pred next))
		;; The last node's CONT is also used somewhere else.
		(not (eq (continuation-use last-cont) last))
		;; The successor is the current block (infinite loop).
		(eq next block)
		;; The next block has a different cleanup, and thus
		;; we may want to insert cleanup code between the
		;; two blocks at some point.
		(not (eq (block-end-cleanup block)
			 (block-start-cleanup next)))
		;; The next block has a different home lambda, and
		;; thus the control transfer is a non-local exit.
		(not (eq (block-home-lambda block)
			 (block-home-lambda next))))
	       nil)
	      ;; Joining is easy when the successor's START
	      ;; continuation is the same from our LAST's CONT. 
	      ((eq last-cont next-cont)
	       (join-blocks block next)
	       t)
	      ;; If they differ, then we can still join when the last
	      ;; continuation has no next and the next continuation
	      ;; has no uses. 
	      ((and (null (block-start-uses next))
		    (eq (continuation-kind last-cont) :inside-block))
	       ;; In this case, we replace the next
	       ;; continuation with the last before joining the blocks.
	       (let ((next-node (continuation-next next-cont)))
		 ;; If NEXT-CONT does have a dest, it must be
		 ;; unreachable, since there are no USES.
		 ;; DELETE-CONTINUATION will mark the dest block as
		 ;; DELETE-P [and also this block, unless it is no
		 ;; longer backward reachable from the dest block.]
		 (delete-continuation next-cont)
		 (setf (node-prev next-node) last-cont)
		 (setf (continuation-next last-cont) next-node)
		 (setf (block-start next) last-cont)
		 (join-blocks block next))
	       t)
	      (t
	       nil))))))

;;; Join together two blocks which have the same ending/starting
;;; continuation. The code in BLOCK2 is moved into BLOCK1 and BLOCK2
;;; is deleted from the DFO. We combine the optimize flags for the two
;;; blocks so that any indicated optimization gets done.
(defun join-blocks (block1 block2)
  (declare (type cblock block1 block2))
  (let* ((last (block-last block2))
	 (last-cont (node-cont last))
	 (succ (block-succ block2))
	 (start2 (block-start block2)))
    (do ((cont start2 (node-cont (continuation-next cont))))
	((eq cont last-cont)
	 (when (eq (continuation-kind last-cont) :inside-block)
	   (setf (continuation-block last-cont) block1)))
      (setf (continuation-block cont) block1))

    (unlink-blocks block1 block2)
    (dolist (block succ)
      (unlink-blocks block2 block)
      (link-blocks block1 block))

    (setf (block-last block1) last)
    (setf (continuation-kind start2) :inside-block))

  (setf (block-flags block1)
	(attributes-union (block-flags block1)
			  (block-flags block2)
			  (block-attributes type-asserted test-modified)))

  (let ((next (block-next block2))
	(prev (block-prev block2)))
    (setf (block-next prev) next)
    (setf (block-prev next) prev))

  (values))

;;; Delete any nodes in BLOCK whose value is unused and which have no
;;; side effects. We can delete sets of lexical variables when the set
;;; variable has no references.
(defun flush-dead-code (block)
  (declare (type cblock block))
  (do-nodes-backwards (node cont block)
    (unless (continuation-dest cont)
      (typecase node
	(ref
	 (delete-ref node)
	 (unlink-node node))
	(combination
	 (let ((info (combination-kind node)))
	   (when (fun-info-p info)
	     (let ((attr (fun-info-attributes info)))
	       (when (and (not (ir1-attributep attr call))
			  ;; ### For now, don't delete potentially
			  ;; flushable calls when they have the CALL
			  ;; attribute. Someday we should look at the
			  ;; functional args to determine if they have
			  ;; any side effects.
                          (if (policy node (= safety 3))
                              (and (ir1-attributep attr flushable)
                                   (every (lambda (arg)
                                            ;; FIXME: when bug 203
                                            ;; will be fixed, remove
                                            ;; this check
                                            (member (continuation-type-check arg)
                                                    '(nil :deleted)))
                                          (basic-combination-args node))
                                   (valid-fun-use node
                                                  (info :function :type
                                                        (leaf-source-name (ref-leaf (continuation-use (basic-combination-fun node)))))
                                                  :result-test #'always-subtypep
                                                  :lossage-fun nil
                                                  :unwinnage-fun nil))
                              (ir1-attributep attr unsafely-flushable)))
		 (flush-dest (combination-fun node))
		 (dolist (arg (combination-args node))
		   (flush-dest arg))
		 (unlink-node node))))))
	(mv-combination
	 (when (eq (basic-combination-kind node) :local)
	   (let ((fun (combination-lambda node)))
	     (when (dolist (var (lambda-vars fun) t)
		     (when (or (leaf-refs var)
			       (lambda-var-sets var))
		       (return nil)))
	       (flush-dest (first (basic-combination-args node)))
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
		   (delete node (basic-var-sets var)))
	     (unlink-node node)))))))

  (setf (block-flush-p block) nil)
  (values))

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
;;; combination, which may change the succesor of the call to be the
;;; called function, and if so, checks if the call can become an
;;; assignment. If we convert to an assignment, we abort, since the
;;; RETURN has been deleted.
(defun find-result-type (node)
  (declare (type creturn node))
  (let ((result (return-result node)))
    (collect ((use-union *empty-type* values-type-union))
      (do-uses (use result)
	(cond ((and (basic-combination-p use)
		    (eq (basic-combination-kind use) :local))
	       (aver (eq (lambda-tail-set (node-home-lambda use))
			 (lambda-tail-set (combination-lambda use))))
	       (when (combination-p use)
		 (when (nth-value 1 (maybe-convert-tail-local-call use))
		   (return-from find-result-type (values)))))
	      (t
	       (use-union (node-derived-type use)))))
      (let ((int (values-type-intersection
		  (continuation-asserted-type result)
		  (use-union))))
	(setf (return-result-type node) int))))
  (values))

;;; Do stuff to realize that something has changed about the value
;;; delivered to a return node. Since we consider the return values of
;;; all functions in the tail set to be equivalent, this amounts to
;;; bringing the entire tail set up to date. We iterate over the
;;; returns for all the functions in the tail set, reanalyzing them
;;; all (not treating Node specially.)
;;;
;;; When we are done, we check whether the new type is different from
;;; the old TAIL-SET-TYPE. If so, we set the type and also reoptimize
;;; all the continuations for references to functions in the tail set.
;;; This will cause IR1-OPTIMIZE-COMBINATION to derive the new type as
;;; the results of the calls.
(defun ir1-optimize-return (node)
  (declare (type creturn node))
  (let* ((tails (lambda-tail-set (return-lambda node)))
	 (funs (tail-set-funs tails)))
    (collect ((res *empty-type* values-type-union))
      (dolist (fun funs)
	(let ((return (lambda-return fun)))
	  (when return
	    (when (node-reoptimize return)
	      (setf (node-reoptimize return) nil)
	      (find-result-type return))
	    (res (return-result-type return)))))

      (when (type/= (res) (tail-set-type tails))
	(setf (tail-set-type tails) (res))
	(dolist (fun (tail-set-funs tails))
	  (dolist (ref (leaf-refs fun))
	    (reoptimize-continuation (node-cont ref)))))))

  (values))

;;;; IF optimization

;;; If the test has multiple uses, replicate the node when possible.
;;; Also check whether the predicate is known to be true or false,
;;; deleting the IF node in favor of the appropriate branch when this
;;; is the case.
(defun ir1-optimize-if (node)
  (declare (type cif node))
  (let ((test (if-test node))
	(block (node-block node)))

    (when (and (eq (block-start block) test)
	       (eq (continuation-next test) node)
	       (rest (block-start-uses block)))
      (do-uses (use test)
	(when (immediately-used-p test use)
	  (convert-if-if use node)
	  (when (continuation-use test) (return)))))

    (let* ((type (continuation-type test))
	   (victim
	    (cond ((constant-continuation-p test)
		   (if (continuation-value test)
		       (if-alternative node)
		       (if-consequent node)))
		  ((not (types-equal-or-intersect type (specifier-type 'null)))
		   (if-alternative node))
		  ((type= type (specifier-type 'null))
		   (if-consequent node)))))
      (when victim
	(flush-dest test)
	(when (rest (block-succ block))
	  (unlink-blocks block victim))
	(setf (component-reanalyze (node-component node)) t)
	(unlink-node node))))
  (values))

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
	   (dummy-cont (make-continuation))
	   (new-cont (make-continuation))
	   (new-node (make-if :test new-cont
			      :consequent cblock
			      :alternative ablock))
	   (new-block (continuation-starts-block new-cont)))
      (link-node-to-previous-continuation new-node new-cont)
      (setf (continuation-dest new-cont) new-node)
      (setf (continuation-%externally-checkable-type new-cont) nil)
      (add-continuation-use new-node dummy-cont)
      (setf (block-last new-block) new-node)

      (unlink-blocks use-block block)
      (delete-continuation-use use)
      (add-continuation-use use new-cont)
      (link-blocks use-block new-block)

      (link-blocks new-block cblock)
      (link-blocks new-block ablock)

      (push "<IF Duplication>" (node-source-path node))
      (push "<IF Duplication>" (node-source-path new-node))

      (reoptimize-continuation test)
      (reoptimize-continuation new-cont)
      (setf (component-reanalyze *current-component*) t)))
  (values))

;;;; exit IR1 optimization

;;; This function attempts to delete an exit node, returning true if
;;; it deletes the block as a consequence:
;;; -- If the exit is degenerate (has no ENTRY), then we don't do
;;;    anything, since there is nothing to be done.
;;; -- If the exit node and its ENTRY have the same home lambda then
;;;    we know the exit is local, and can delete the exit. We change
;;;    uses of the Exit-Value to be uses of the original continuation,
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
	(entry (exit-entry node))
	(cont (node-cont node)))
    (when (and entry
	       (eq (node-home-lambda node) (node-home-lambda entry)))
      (setf (entry-exits entry) (delete node (entry-exits entry)))
      (prog1
	  (unlink-node node)
	(when value
	  (collect ((merges))
	    (when (return-p (continuation-dest cont))
	      (do-uses (use value)
		(when (and (basic-combination-p use)
			   (eq (basic-combination-kind use) :local))
		  (merges use))))
	    (substitute-continuation-uses cont value)
	    (dolist (merge (merges))
	      (merge-tail-sets merge))))))))

;;;; combination IR1 optimization

;;; Report as we try each transform?
#!+sb-show
(defvar *show-transforms-p* nil)

;;; Do IR1 optimizations on a COMBINATION node.
(declaim (ftype (function (combination) (values)) ir1-optimize-combination))
(defun ir1-optimize-combination (node)
  (when (continuation-reoptimize (basic-combination-fun node))
    (propagate-fun-change node))
  (let ((args (basic-combination-args node))
	(kind (basic-combination-kind node)))
    (case kind
      (:local
       (let ((fun (combination-lambda node)))
	 (if (eq (functional-kind fun) :let)
	     (propagate-let-args node fun)
	     (propagate-local-call-args node fun))))
      ((:full :error)
       (dolist (arg args)
	 (when arg
	   (setf (continuation-reoptimize arg) nil))))
      (t
       (dolist (arg args)
	 (when arg
	   (setf (continuation-reoptimize arg) nil)))

       (let ((attr (fun-info-attributes kind)))
	 (when (and (ir1-attributep attr foldable)
		    ;; KLUDGE: The next test could be made more sensitive,
		    ;; only suppressing constant-folding of functions with
		    ;; CALL attributes when they're actually passed
		    ;; function arguments. -- WHN 19990918
		    (not (ir1-attributep attr call))
		    (every #'constant-continuation-p args)
		    (continuation-dest (node-cont node))
		    ;; Even if the function is foldable in principle,
		    ;; it might be one of our low-level
		    ;; implementation-specific functions. Such
		    ;; functions don't necessarily exist at runtime on
		    ;; a plain vanilla ANSI Common Lisp
		    ;; cross-compilation host, in which case the
		    ;; cross-compiler can't fold it because the
		    ;; cross-compiler doesn't know how to evaluate it.
		    #+sb-xc-host
		    (fboundp (combination-fun-source-name node)))
	   (constant-fold-call node)
	   (return-from ir1-optimize-combination)))

       (let ((fun (fun-info-derive-type kind)))
	 (when fun
	   (let ((res (funcall fun node)))
	     (when res
	       (derive-node-type node res)
	       (maybe-terminate-block node nil)))))

       (let ((fun (fun-info-optimizer kind)))
	 (unless (and fun (funcall fun node))
	   (dolist (x (fun-info-transforms kind))
	     #!+sb-show 
	     (when *show-transforms-p*
	       (let* ((cont (basic-combination-fun node))
		      (fname (continuation-fun-name cont t)))
		 (/show "trying transform" x (transform-function x) "for" fname)))
	     (unless (ir1-transform node x)
	       #!+sb-show
	       (when *show-transforms-p*
		 (/show "quitting because IR1-TRANSFORM result was NIL"))
	       (return))))))))

  (values))

;;; If CALL is to a function that doesn't return (i.e. return type is
;;; NIL), then terminate the block there, and link it to the component
;;; tail. We also change the call's CONT to be a dummy continuation to
;;; prevent the use from confusing things.
;;;
;;; Except when called during IR1 [FIXME: What does this mean? Except
;;; during IR1 conversion? What about IR1 optimization?], we delete
;;; the continuation if it has no other uses. (If it does have other
;;; uses, we reoptimize.)
;;;
;;; Termination on the basis of a continuation type assertion is
;;; inhibited when:
;;; -- The continuation is deleted (hence the assertion is spurious), or
;;; -- We are in IR1 conversion (where THE assertions are subject to
;;;    weakening.)
(defun maybe-terminate-block (call ir1-converting-not-optimizing-p)
  (declare (type basic-combination call))
  (let* ((block (node-block call))
	 (cont (node-cont call))
	 (tail (component-tail (block-component block)))
	 (succ (first (block-succ block))))
    (unless (or (and (eq call (block-last block)) (eq succ tail))
		(block-delete-p block))
      (when (or (and (eq (continuation-asserted-type cont) *empty-type*)
		     (not (or ir1-converting-not-optimizing-p
			      (eq (continuation-kind cont) :deleted))))
		(eq (node-derived-type call) *empty-type*))
	(cond (ir1-converting-not-optimizing-p
	       (delete-continuation-use call)
	       (cond
		((block-last block)
		 (aver (and (eq (block-last block) call)
			    (eq (continuation-kind cont) :block-start))))
		(t
		 (setf (block-last block) call)
		 (link-blocks block (continuation-starts-block cont)))))
	      (t
	       (node-ends-block call)
	       (delete-continuation-use call)
	       (if (eq (continuation-kind cont) :unused)
		   (delete-continuation cont)
		   (reoptimize-continuation cont))))
	
	(unlink-blocks block (first (block-succ block)))
	(setf (component-reanalyze (block-component block)) t)
	(aver (not (block-succ block)))
	(link-blocks block tail)
	(add-continuation-use call (make-continuation))
	t))))

;;; This is called both by IR1 conversion and IR1 optimization when
;;; they have verified the type signature for the call, and are
;;; wondering if something should be done to special-case the call. If
;;; CALL is a call to a global function, then see whether it defined
;;; or known:
;;; -- If a DEFINED-FUN should be inline expanded, then convert
;;;    the expansion and change the call to call it. Expansion is
;;;    enabled if :INLINE or if SPACE=0. If the FUNCTIONAL slot is
;;;    true, we never expand, since this function has already been
;;;    converted. Local call analysis will duplicate the definition
;;;    if necessary. We claim that the parent form is LABELS for
;;;    context declarations, since we don't want it to be considered
;;;    a real global function.
;;; -- If it is a known function, mark it as such by setting the KIND.
;;;
;;; We return the leaf referenced (NIL if not a leaf) and the
;;; FUN-INFO assigned.
;;;
;;; FIXME: The IR1-CONVERTING-NOT-OPTIMIZING-P argument is what the
;;; old CMU CL code called IR1-P, without explanation. My (WHN
;;; 2002-01-09) tentative understanding of it is that we can call this
;;; operation either in initial IR1 conversion or in later IR1
;;; optimization, and it tells which is which. But it would be good
;;; for someone who really understands it to check whether this is
;;; really right.
(defun recognize-known-call (call ir1-converting-not-optimizing-p)
  (declare (type combination call))
  (let* ((ref (continuation-use (basic-combination-fun call)))
	 (leaf (when (ref-p ref) (ref-leaf ref)))
	 (inlinep (if (defined-fun-p leaf)
		      (defined-fun-inlinep leaf)
		      :no-chance)))
    (cond
     ((eq inlinep :notinline) (values nil nil))
     ((not (and (global-var-p leaf)
		(eq (global-var-kind leaf) :global-function)))
      (values leaf nil))
     ((and (ecase inlinep
	     (:inline t)
	     (:no-chance nil)
	     ((nil :maybe-inline) (policy call (zerop space))))
	   (defined-fun-p leaf)
	   (defined-fun-inline-expansion leaf)
	   (let ((fun (defined-fun-functional leaf)))
	     (or (not fun)
		 (and (eq inlinep :inline) (functional-kind fun))))
	   (inline-expansion-ok call))
      (flet (;; FIXME: Is this what the old CMU CL internal documentation
	     ;; called semi-inlining? A more descriptive name would
	     ;; be nice. -- WHN 2002-01-07
	     (frob ()
	       (let ((res (ir1-convert-lambda-for-defun
			   (defined-fun-inline-expansion leaf)
			   leaf t
			   #'ir1-convert-inline-lambda)))
		 (setf (defined-fun-functional leaf) res)
		 (change-ref-leaf ref res))))
	(if ir1-converting-not-optimizing-p
	    (frob)
	    (with-ir1-environment-from-node call
	      (frob)
	      (locall-analyze-component *current-component*))))

      (values (ref-leaf (continuation-use (basic-combination-fun call)))
	      nil))
     (t
      (let ((info (info :function :info (leaf-source-name leaf))))
	(if info
	    (values leaf (setf (basic-combination-kind call) info))
	    (values leaf nil)))))))

;;; Check whether CALL satisfies TYPE. If so, apply the type to the
;;; call, and do MAYBE-TERMINATE-BLOCK and return the values of
;;; RECOGNIZE-KNOWN-CALL. If an error, set the combination kind and
;;; return NIL, NIL. If the type is just FUNCTION, then skip the
;;; syntax check, arg/result type processing, but still call
;;; RECOGNIZE-KNOWN-CALL, since the call might be to a known lambda,
;;; and that checking is done by local call analysis.
(defun validate-call-type (call type ir1-converting-not-optimizing-p)
  (declare (type combination call) (type ctype type))
  (cond ((not (fun-type-p type))
	 (aver (multiple-value-bind (val win)
		   (csubtypep type (specifier-type 'function))
		 (or val (not win))))
	 (recognize-known-call call ir1-converting-not-optimizing-p))
	((valid-fun-use call type
			:argument-test #'always-subtypep
			:result-test #'always-subtypep
			;; KLUDGE: Common Lisp is such a dynamic
			;; language that all we can do here in
			;; general is issue a STYLE-WARNING. It
			;; would be nice to issue a full WARNING
			;; in the special case of of type
			;; mismatches within a compilation unit
			;; (as in section 3.2.2.3 of the spec)
			;; but at least as of sbcl-0.6.11, we
			;; don't keep track of whether the
			;; mismatched data came from the same
			;; compilation unit, so we can't do that.
			;; -- WHN 2001-02-11
			;;
			;; FIXME: Actually, I think we could
			;; issue a full WARNING if the call
			;; violates a DECLAIM FTYPE.
			:lossage-fun #'compiler-style-warn
			:unwinnage-fun #'compiler-note)
	 (assert-call-type call type)
	 (maybe-terminate-block call ir1-converting-not-optimizing-p)
	 (recognize-known-call call ir1-converting-not-optimizing-p))
	(t
	 (setf (combination-kind call) :error)
	 (values nil nil))))

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
	(fun-cont (basic-combination-fun call)))
    (setf (continuation-reoptimize fun-cont) nil)
    (case (combination-kind call)
      (:local
       (let ((fun (combination-lambda call)))
	 (maybe-let-convert fun)
	 (unless (member (functional-kind fun) '(:let :assignment :deleted))
	   (derive-node-type call (tail-set-type (lambda-tail-set fun))))))
      (:full
       (multiple-value-bind (leaf info)
	   (validate-call-type call (continuation-type fun-cont) nil)
	 (cond ((functional-p leaf)
		(convert-call-if-possible
		 (continuation-use (basic-combination-fun call))
		 call))
	       ((not leaf))
	       ((and (leaf-has-source-name-p leaf)
                     (or (info :function :source-transform (leaf-source-name leaf))
                         (and info
                              (ir1-attributep (fun-info-attributes info)
                                              predicate)
                              (let ((dest (continuation-dest (node-cont call))))
                                (and dest (not (if-p dest)))))))
                ;; FIXME: This SYMBOLP is part of a literal
                ;; translation of a test in the old CMU CL
                ;; source, and it's not quite clear what
                ;; the old source meant. Did it mean "has a
                ;; valid name"? Or did it mean "is an
                ;; ordinary function name, not a SETF
                ;; function"? Either way, the old CMU CL
                ;; code probably didn't deal with SETF
                ;; functions correctly, and neither does
                ;; this new SBCL code, and that should be fixed.
		(when (symbolp (leaf-source-name leaf))
                  (let ((dummies (make-gensym-list
                                  (length (combination-args call)))))
                    (transform-call call
                                    `(lambda ,dummies
                                      (,(leaf-source-name leaf)
                                       ,@dummies))
                                    (leaf-source-name leaf))))))))))
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
(defun ir1-transform (node transform)
  (declare (type combination node) (type transform transform))
  (let* ((type (transform-type transform))
	 (fun (transform-function transform))
	 (constrained (fun-type-p type))
	 (table (component-failed-optimizations *component-being-compiled*))
	 (flame (if (transform-important transform)
		    (policy node (>= speed inhibit-warnings))
		    (policy node (> speed inhibit-warnings))))
	 (*compiler-error-context* node))
    (cond ((or (not constrained)
	       (valid-fun-use node type :strict-result t))
	   (multiple-value-bind (severity args)
	       (catch 'give-up-ir1-transform
		 (transform-call node
				 (funcall fun node)
				 (combination-fun-source-name node))
		 (values :none nil))
	     (ecase severity
	       (:none
		(remhash node table)
		nil)
	       (:aborted
		(setf (combination-kind node) :error)
		(when args
		  (apply #'compiler-warn args))
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
(declaim (ftype (function (&rest t) nil) give-up-ir1-transform))
(defun give-up-ir1-transform (&rest args)
  (throw 'give-up-ir1-transform (values :failure args)))
(defun abort-ir1-transform (&rest args)
  (throw 'give-up-ir1-transform (values :aborted args)))
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
 		(setf (component-reoptimize (block-component block)) t)))))))
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
(defun transform-call (node res source-name)
  (declare (type combination node) (list res))
  (aver (and (legal-fun-name-p source-name)
	     (not (eql source-name '.anonymous.))))
  (with-ir1-environment-from-node node
      (let ((new-fun (ir1-convert-inline-lambda
		      res
		      :debug-name (debug-namify "LAMBDA-inlined ~A"
						(as-debug-name
						 source-name
						 "<unknown function>"))))
	    (ref (continuation-use (combination-fun node))))
	(change-ref-leaf ref new-fun)
	(setf (combination-kind node) :full)
	(locall-analyze-component *current-component*)))
  (values))

;;; Replace a call to a foldable function of constant arguments with
;;; the result of evaluating the form. We insert the resulting
;;; constant node after the call, stealing the call's continuation. We
;;; give the call a continuation with no DEST, which should cause it
;;; and its arguments to go away. If there is an error during the
;;; evaluation, we give a warning and leave the call alone, making the
;;; call a :ERROR call.
;;;
;;; If there is more than one value, then we transform the call into a
;;; VALUES form.
(defun constant-fold-call (call)
  (let ((args (mapcar #'continuation-value (combination-args call)))
	(fun-name (combination-fun-source-name call)))
    (multiple-value-bind (values win)
	(careful-call fun-name
		      args
		      call
		      ;; Note: CMU CL had COMPILER-WARN here, and that
		      ;; seems more natural, but it's probably not.
		      ;;
		      ;; It's especially not while bug 173 exists:
		      ;; Expressions like
		      ;;   (COND (END
		      ;;          (UNLESS (OR UNSAFE? (<= END SIZE)))
		      ;;            ...))
		      ;; can cause constant-folding TYPE-ERRORs (in
		      ;; #'<=) when END can be proved to be NIL, even
		      ;; though the code is perfectly legal and safe
		      ;; because a NIL value of END means that the
		      ;; #'<= will never be executed.
		      ;;
		      ;; Moreover, even without bug 173,
		      ;; quite-possibly-valid code like
		      ;;   (COND ((NONINLINED-PREDICATE END)
		      ;;          (UNLESS (<= END SIZE))
		      ;;            ...))
		      ;; (where NONINLINED-PREDICATE is something the
		      ;; compiler can't do at compile time, but which
		      ;; turns out to make the #'<= expression
		      ;; unreachable when END=NIL) could cause errors
		      ;; when the compiler tries to constant-fold (<=
		      ;; END SIZE).
		      ;;
		      ;; So, with or without bug 173, it'd be 
		      ;; unnecessarily evil to do a full
		      ;; COMPILER-WARNING (and thus return FAILURE-P=T
		      ;; from COMPILE-FILE) for legal code, so we we
		      ;; use a wimpier COMPILE-STYLE-WARNING instead.
		      #'compiler-style-warn
		      "constant folding")
      (if (not win)
	  (setf (combination-kind call) :error)
	  (let ((dummies (make-gensym-list (length args))))
	    (transform-call
	     call
	     `(lambda ,dummies
		(declare (ignore ,@dummies))
		(values ,@(mapcar (lambda (x) `',x) values)))
	     fun-name)))))
  (values))

;;;; local call optimization

;;; Propagate TYPE to LEAF and its REFS, marking things changed. If
;;; the leaf type is a function type, then just leave it alone, since
;;; TYPE is never going to be more specific than that (and
;;; TYPE-INTERSECTION would choke.)
(defun propagate-to-refs (leaf type)
  (declare (type leaf leaf) (type ctype type))
  (let ((var-type (leaf-type leaf)))
    (unless (fun-type-p var-type)
      (let ((int (type-approx-intersection2 var-type type)))
	(when (type/= int var-type)
	  (setf (leaf-type leaf) int)
	  (dolist (ref (leaf-refs leaf))
	    (derive-node-type ref int))))
      (values))))

;;; Figure out the type of a LET variable that has sets. We compute
;;; the union of the initial value Type and the types of all the set
;;; values and to a PROPAGATE-TO-REFS with this type.
(defun propagate-from-sets (var type)
  (collect ((res type type-union))
    (dolist (set (basic-var-sets var))
      (res (continuation-type (set-value set)))
      (setf (node-reoptimize set) nil))
    (propagate-to-refs var (res)))
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
	  (let ((iv (let-var-initial-value var)))
	    (setf (continuation-reoptimize iv) nil)
	    (propagate-from-sets var (continuation-type iv)))))))

  (derive-node-type node (continuation-type (set-value node)))
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
       (not (eq (defined-fun-inlinep leaf) :notinline)))
      #!+(and (not sb-fluid) (not sb-xc-host))
      (global-var
       (case (global-var-kind leaf)
	 (:global-function (let ((name (leaf-source-name leaf)))
                             (when (consp name)
                               (aver (eq (first name) 'setf))
                               (setq name (second name)))
                             (eq (symbol-package name)
                                 *cl-package*))))))))

;;; If we have a non-set LET var with a single use, then (if possible)
;;; replace the variable reference's CONT with the arg continuation.
;;; This is inhibited when:
;;; -- CONT has other uses, or
;;; -- CONT receives multiple values, or
;;; -- the reference is in a different environment from the variable, or
;;; -- either continuation has a funky TYPE-CHECK annotation.
;;; -- the continuations have incompatible assertions, so the new asserted type
;;;    would be NIL.
;;; -- the var's DEST has a different policy than the ARG's (think safety).
;;;
;;; We change the REF to be a reference to NIL with unused value, and
;;; let it be flushed as dead code. A side effect of this substitution
;;; is to delete the variable.
(defun substitute-single-use-continuation (arg var)
  (declare (type continuation arg) (type lambda-var var))
  (let* ((ref (first (leaf-refs var)))
	 (cont (node-cont ref))
	 (cont-atype (continuation-asserted-type cont))
         (cont-ctype (continuation-type-to-check cont))
	 (dest (continuation-dest cont)))
    (when (and (eq (continuation-use cont) ref)
	       dest
	       (not (typep dest '(or creturn exit mv-combination)))
	       (eq (node-home-lambda ref)
		   (lambda-home (lambda-var-home var)))
	       (member (continuation-type-check arg) '(t nil))
	       (member (continuation-type-check cont) '(t nil))
	       (not (eq (values-type-intersection
			 cont-atype
			 (continuation-asserted-type arg))
			*empty-type*))
	       (eq (lexenv-policy (node-lexenv dest))
		   (lexenv-policy (node-lexenv (continuation-dest arg)))))
      (aver (member (continuation-kind arg)
		    '(:block-start :deleted-block-start :inside-block)))
      (set-continuation-type-assertion arg cont-atype cont-ctype)
      (setf (node-derived-type ref) *wild-type*)
      (change-ref-leaf ref (find-constant nil))
      (substitute-continuation arg cont)
      (reoptimize-continuation arg)
      t)))

;;; Delete a LET, removing the call and bind nodes, and warning about
;;; any unreferenced variables. Note that FLUSH-DEAD-CODE will come
;;; along right away and delete the REF and then the lambda, since we
;;; flush the FUN continuation.
(defun delete-let (clambda)
  (declare (type clambda clambda))
  (aver (functional-letlike-p clambda))
  (note-unreferenced-vars clambda)
  (let ((call (let-combination clambda)))
    (flush-dest (basic-combination-fun call))
    (unlink-node call)
    (unlink-node (lambda-bind clambda))
    (setf (lambda-bind clambda) nil))
  (values))

;;; This function is called when one of the arguments to a LET
;;; changes. We look at each changed argument. If the corresponding
;;; variable is set, then we call PROPAGATE-FROM-SETS. Otherwise, we
;;; consider substituting for the variable, and also propagate
;;; derived-type information for the arg to all the VAR's refs.
;;;
;;; Substitution is inhibited when the arg leaf's derived type isn't a
;;; subtype of the argument's asserted type. This prevents type
;;; checking from being defeated, and also ensures that the best
;;; representation for the variable can be used.
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
;;; Note that we are responsible for clearing the
;;; CONTINUATION-REOPTIMIZE flags.
(defun propagate-let-args (call fun)
  (declare (type combination call) (type clambda fun))
  (loop for arg in (combination-args call)
	and var in (lambda-vars fun) do
    (when (and arg (continuation-reoptimize arg))
      (setf (continuation-reoptimize arg) nil)
      (cond
       ((lambda-var-sets var)
	(propagate-from-sets var (continuation-type arg)))
       ((let ((use (continuation-use arg)))
	  (when (ref-p use)
	    (let ((leaf (ref-leaf use)))
	      (when (and (constant-reference-p use)
			 (values-subtypep (leaf-type leaf)
					  (continuation-asserted-type arg)))
		(propagate-to-refs var (continuation-type arg))
		(let ((use-component (node-component use)))
		  (substitute-leaf-if
		   (lambda (ref)
		     (cond ((eq (node-component ref) use-component)
			    t)
			   (t
			    (aver (lambda-toplevelish-p (lambda-home fun)))
			    nil)))
		   leaf var))
		t)))))
       ((and (null (rest (leaf-refs var)))
	     (substitute-single-use-continuation arg var)))
       (t
	(propagate-to-refs var (continuation-type arg))))))

  (when (every #'null (combination-args call))
    (delete-let fun))

  (values))

;;; This function is called when one of the args to a non-LET local
;;; call changes. For each changed argument corresponding to an unset
;;; variable, we compute the union of the types across all calls and
;;; propagate this type information to the var's refs.
;;;
;;; If the function has an XEP, then we don't do anything, since we
;;; won't discover anything.
;;;
;;; We can clear the Continuation-Reoptimize flags for arguments in
;;; all calls corresponding to changed arguments in Call, since the
;;; only use in IR1 optimization of the Reoptimize flag for local call
;;; args is right here.
(defun propagate-local-call-args (call fun)
  (declare (type combination call) (type clambda fun))

  (unless (or (functional-entry-fun fun)
	      (lambda-optional-dispatch fun))
    (let* ((vars (lambda-vars fun))
	   (union (mapcar (lambda (arg var)
			    (when (and arg
				       (continuation-reoptimize arg)
				       (null (basic-var-sets var)))
			      (continuation-type arg)))
			  (basic-combination-args call)
			  vars))
	   (this-ref (continuation-use (basic-combination-fun call))))

      (dolist (arg (basic-combination-args call))
	(when arg
	  (setf (continuation-reoptimize arg) nil)))

      (dolist (ref (leaf-refs fun))
	(let ((dest (continuation-dest (node-cont ref))))
	  (unless (or (eq ref this-ref) (not dest))
	    (setq union
		  (mapcar (lambda (this-arg old)
			    (when old
			      (setf (continuation-reoptimize this-arg) nil)
			      (type-union (continuation-type this-arg) old)))
			  (basic-combination-args dest)
			  union)))))

      (mapc (lambda (var type)
	      (when type
		(propagate-to-refs var type)))
	    vars union)))

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
  (ecase (basic-combination-kind node)
    (:local
     (let ((fun-cont (basic-combination-fun node)))
       (when (continuation-reoptimize fun-cont)
	 (setf (continuation-reoptimize fun-cont) nil)
	 (maybe-let-convert (combination-lambda node))))
     (setf (continuation-reoptimize (first (basic-combination-args node))) nil)
     (when (eq (functional-kind (combination-lambda node)) :mv-let)
       (unless (convert-mv-bind-to-let node)
	 (ir1-optimize-mv-bind node))))
    (:full
     (let* ((fun (basic-combination-fun node))
	    (fun-changed (continuation-reoptimize fun))
	    (args (basic-combination-args node)))
       (when fun-changed
	 (setf (continuation-reoptimize fun) nil)
	 (let ((type (continuation-type fun)))
	   (when (fun-type-p type)
	     (derive-node-type node (fun-type-returns type))))
	 (maybe-terminate-block node nil)
	 (let ((use (continuation-use fun)))
	   (when (and (ref-p use) (functional-p (ref-leaf use)))
	     (convert-call-if-possible use node)
	     (when (eq (basic-combination-kind node) :local)
	       (maybe-let-convert (ref-leaf use))))))
       (unless (or (eq (basic-combination-kind node) :local)
		   (eq (continuation-fun-name fun) '%throw))
	 (ir1-optimize-mv-call node))
       (dolist (arg args)
	 (setf (continuation-reoptimize arg) nil))))
    (:error))
  (values))

;;; Propagate derived type info from the values continuation to the
;;; vars.
(defun ir1-optimize-mv-bind (node)
  (declare (type mv-combination node))
  (let ((arg (first (basic-combination-args node)))
	(vars (lambda-vars (combination-lambda node))))
    (multiple-value-bind (types nvals)
	(values-types (continuation-derived-type arg))
      (unless (eq nvals :unknown)
	(mapc (lambda (var type)
		(if (basic-var-sets var)
		    (propagate-from-sets var type)
		    (propagate-to-refs var type)))
	      vars
		(append types
			(make-list (max (- (length vars) nvals) 0)
				   :initial-element (specifier-type 'null))))))
    (setf (continuation-reoptimize arg) nil))
  (values))

;;; If possible, convert a general MV call to an MV-BIND. We can do
;;; this if:
;;; -- The call has only one argument, and
;;; -- The function has a known fixed number of arguments, or
;;; -- The argument yields a known fixed number of values.
;;;
;;; What we do is change the function in the MV-CALL to be a lambda
;;; that "looks like an MV bind", which allows
;;; IR1-OPTIMIZE-MV-COMBINATION to notice that this call can be
;;; converted (the next time around.) This new lambda just calls the
;;; actual function with the MV-BIND variables as arguments. Note that
;;; this new MV bind is not let-converted immediately, as there are
;;; going to be stray references from the entry-point functions until
;;; they get deleted.
;;;
;;; In order to avoid loss of argument count checking, we only do the
;;; transformation according to a known number of expected argument if
;;; safety is unimportant. We can always convert if we know the number
;;; of actual values, since the normal call that we build will still
;;; do any appropriate argument count checking.
;;;
;;; We only attempt the transformation if the called function is a
;;; constant reference. This allows us to just splice the leaf into
;;; the new function, instead of trying to somehow bind the function
;;; expression. The leaf must be constant because we are evaluating it
;;; again in a different place. This also has the effect of squelching
;;; multiple warnings when there is an argument count error.
(defun ir1-optimize-mv-call (node)
  (let ((fun (basic-combination-fun node))
	(*compiler-error-context* node)
	(ref (continuation-use (basic-combination-fun node)))
	(args (basic-combination-args node)))

    (unless (and (ref-p ref) (constant-reference-p ref)
		 args (null (rest args)))
      (return-from ir1-optimize-mv-call))

    (multiple-value-bind (min max)
	(fun-type-nargs (continuation-type fun))
      (let ((total-nvals
	     (multiple-value-bind (types nvals)
		 (values-types (continuation-derived-type (first args)))
	       (declare (ignore types))
	       (if (eq nvals :unknown) nil nvals))))

	(when total-nvals
	  (when (and min (< total-nvals min))
	    (compiler-warn
	     "MULTIPLE-VALUE-CALL with ~R values when the function expects ~
	     at least ~R."
	     total-nvals min)
	    (setf (basic-combination-kind node) :error)
	    (return-from ir1-optimize-mv-call))
	  (when (and max (> total-nvals max))
	    (compiler-warn
	     "MULTIPLE-VALUE-CALL with ~R values when the function expects ~
	     at most ~R."
	     total-nvals max)
	    (setf (basic-combination-kind node) :error)
	    (return-from ir1-optimize-mv-call)))

	(let ((count (cond (total-nvals)
			   ((and (policy node (zerop safety))
				 (eql min max))
			    min)
			   (t nil))))
	  (when count
	    (with-ir1-environment-from-node node
	      (let* ((dums (make-gensym-list count))
		     (ignore (gensym))
		     (fun (ir1-convert-lambda
			   `(lambda (&optional ,@dums &rest ,ignore)
			      (declare (ignore ,ignore))
			      (funcall ,(ref-leaf ref) ,@dums)))))
		(change-ref-leaf ref fun)
		(aver (eq (basic-combination-kind node) :full))
		(locall-analyze-component *current-component*)
		(aver (eq (basic-combination-kind node) :local)))))))))
  (values))

;;; If we see:
;;;    (multiple-value-bind
;;;	(x y)
;;;	(values xx yy)
;;;      ...)
;;; Convert to:
;;;    (let ((x xx)
;;;	  (y yy))
;;;      ...)
;;;
;;; What we actually do is convert the VALUES combination into a
;;; normal LET combination calling the original :MV-LET lambda. If
;;; there are extra args to VALUES, discard the corresponding
;;; continuations. If there are insufficient args, insert references
;;; to NIL.
(defun convert-mv-bind-to-let (call)
  (declare (type mv-combination call))
  (let* ((arg (first (basic-combination-args call)))
	 (use (continuation-use arg)))
    (when (and (combination-p use)
	       (eq (continuation-fun-name (combination-fun use))
		   'values))
      (let* ((fun (combination-lambda call))
	     (vars (lambda-vars fun))
	     (vals (combination-args use))
	     (nvars (length vars))
	     (nvals (length vals)))
	(cond ((> nvals nvars)
	       (mapc #'flush-dest (subseq vals nvars))
	       (setq vals (subseq vals 0 nvars)))
	      ((< nvals nvars)
	       (with-ir1-environment-from-node use
		 (let ((node-prev (node-prev use)))
		   (setf (node-prev use) nil)
		   (setf (continuation-next node-prev) nil)
		   (collect ((res vals))
		     (loop as cont = (make-continuation use)
			   and prev = node-prev then cont
			   repeat (- nvars nvals)
			   do (reference-constant prev cont nil)
			      (res cont))
		     (setq vals (res)))
		   (link-node-to-previous-continuation use
						       (car (last vals)))))))
	(setf (combination-args use) vals)
	(flush-dest (combination-fun use))
	(let ((fun-cont (basic-combination-fun call)))
	  (setf (continuation-dest fun-cont) use)
          (setf (combination-fun use) fun-cont)
	  (setf (continuation-%externally-checkable-type fun-cont) nil))
	(setf (combination-kind use) :local)
	(setf (functional-kind fun) :let)
	(flush-dest (first (basic-combination-args call)))
	(unlink-node call)
	(when vals
	  (reoptimize-continuation (first vals)))
	(propagate-to-args use fun))
      t)))

;;; If we see:
;;;    (values-list (list x y z))
;;;
;;; Convert to:
;;;    (values x y z)
;;;
;;; In implementation, this is somewhat similar to
;;; CONVERT-MV-BIND-TO-LET. We grab the args of LIST and make them
;;; args of the VALUES-LIST call, flushing the old argument
;;; continuation (allowing the LIST to be flushed.)
(defoptimizer (values-list optimizer) ((list) node)
  (let ((use (continuation-use list)))
    (when (and (combination-p use)
	       (eq (continuation-fun-name (combination-fun use))
		   'list))
      (change-ref-leaf (continuation-use (combination-fun node))
		       (find-free-fun 'values "in a strange place"))
      (setf (combination-kind node) :full)
      (let ((args (combination-args use)))
	(dolist (arg args)
	  (setf (continuation-dest arg) node)
          (setf (continuation-%externally-checkable-type arg) nil))
	(setf (combination-args use) nil)
	(flush-dest list)
	(setf (combination-args node) args))
      t)))

;;; If VALUES appears in a non-MV context, then effectively convert it
;;; to a PROG1. This allows the computation of the additional values
;;; to become dead code.
(deftransform values ((&rest vals) * * :node node)
  (when (typep (continuation-dest (node-cont node))
	       '(or creturn exit mv-combination))
    (give-up-ir1-transform))
  (setf (node-derived-type node) *wild-type*)
  (if vals
      (let ((dummies (make-gensym-list (length (cdr vals)))))
	`(lambda (val ,@dummies)
	   (declare (ignore ,@dummies))
	   val))
      nil))
