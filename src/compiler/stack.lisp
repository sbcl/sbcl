;;;; This file implements the stack analysis phase in the compiler. We
;;;; analyse lifetime of dynamically allocated object packets on stack
;;;; and insert cleanups where necessary.
;;;;
;;;; Currently there are two kinds of interesting stack packets: UVLs,
;;;; whose use and destination lie in different blocks, and LVARs of
;;;; constructors of dynamic-extent objects.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; Scan through BLOCK looking for uses of :UNKNOWN lvars that have
;;; their DEST outside of the block. We do some checking to verify the
;;; invariant that all pushes come after the last pop.
(defun find-pushed-lvars (block)
  (let* ((2block (block-info block))
         (popped (ir2-block-popped 2block))
         (last-pop (if popped
                       (lvar-dest (car (last popped)))
                       nil)))
    (collect ((pushed))
      (let ((saw-last nil))
        (do-nodes (node lvar block)
          (when (eq node last-pop)
            (setq saw-last t))

          (when (and lvar
                     (or (lvar-dynamic-extent lvar)
                         (let ((dest (lvar-dest lvar))
                               (2lvar (lvar-info lvar)))
                           (and (not (eq (node-block dest) block))
                                2lvar
                                (eq (ir2-lvar-kind 2lvar) :unknown)))))
            (aver (or saw-last (not last-pop)))
            (pushed lvar))))

      (setf (ir2-block-pushed 2block) (pushed))))
  (values))

;;;; Computation of live UVL sets
(defun nle-block-p (block)
  (and (eq (component-head (block-component block))
           (first (block-pred block)))
       (not (bind-p (block-start-node block)))))
(defun nle-block-nlx-info (block)
  (let* ((start-node (block-start-node block))
         (nlx-ref (ctran-next (node-next start-node)))
         (nlx-info (constant-value (ref-leaf nlx-ref))))
    nlx-info))
(defun nle-block-entry-block (block)
  (let* ((nlx-info (nle-block-nlx-info block))
         (mess-up (cleanup-mess-up (nlx-info-cleanup nlx-info)))
         (entry-block (node-block mess-up)))
    entry-block))

;;; Add LVARs from LATE to EARLY; use EQ to check whether EARLY has
;;; been changed.
(defun merge-uvl-live-sets (early late)
  (declare (type list early late))
  ;; FIXME: O(N^2)
  (dolist (e late early)
    (pushnew e early)))

;; Blocks are numbered in reverse DFO order, so the "lowest common
;; dominator" of a set of blocks is the closest dominator of all of
;; the blocks.
(defun find-lowest-common-dominator (blocks)
  ;; FIXME: NIL is defined as a valid value for BLOCK-DOMINATORS,
  ;; meaning "all blocks in component".  Actually handle this case.
  (let ((common-dominators (copy-sset (block-dominators (first blocks)))))
    (dolist (block (rest blocks))
      (sset-intersection common-dominators (block-dominators block)))
    (let ((lowest-dominator))
      (do-sset-elements (dominator common-dominators lowest-dominator)
        (when (or (not lowest-dominator)
                  (< (sset-element-number dominator)
                     (sset-element-number lowest-dominator)))
          (setf lowest-dominator dominator))))))

;;; Carefully back-propagate DX LVARs from the start of their
;;; environment to where they are allocated, along all code paths
;;; which actually allocate said LVARs.
(defun back-propagate-one-dx-lvar (block dx-lvar)
  (declare (type cblock block)
           (type lvar dx-lvar))
  ;; We have to back-propagate the lifetime of DX-LVAR to its USEs,
  ;; but only along the paths which actually USE it.  The naive
  ;; solution (which we're going with for now) is a depth-first search
  ;; over an arbitrarily complex chunk of flow graph that is known to
  ;; have a single entry block.
  (let* ((use-blocks (mapcar #'node-block (find-uses dx-lvar)))
         (flag use-blocks) ;; for block-flag, no need to clear-flags, as it's fresh
         (cycle (list :cycle))
         (nlx (list :nlx))
         ;; We have to back-propagate not just the DX-LVAR, but every
         ;; UVL or DX LVAR that is live wherever DX-LVAR is USEd
         ;; (allocated) because we can't move live DX-LVARs to release
         ;; them.
         (preserve-lvars (reduce #'merge-uvl-live-sets
                                 use-blocks
                                 :key (lambda (block)
                                        (let ((2block (block-info block)))
                                          ;; same as logic in UPDATE-UVL-LIVE-SETS
                                          (merge-uvl-live-sets
                                           (set-difference
                                            (ir2-block-start-stack 2block)
                                            (ir2-block-popped 2block))
                                           (member dx-lvar (reverse (ir2-block-pushed 2block))))))))
         (start-block (find-lowest-common-dominator
                       (list* block use-blocks))))
    (aver start-block)
    (labels ((revisit-cycles (block)
               (dolist (succ (block-succ block))
                 (when (eq (block-flag succ) cycle)
                   (mark succ)))
               (when (eq (block-out block) nlx)
                 (do-nested-cleanups (cleanup (block-end-lexenv block))
                   (case (cleanup-kind cleanup)
                     ((:block :tagbody :catch :unwind-protect)
                      (dolist (nlx-info (cleanup-nlx-info cleanup))
                        (let ((target (nlx-info-target nlx-info)))
                          (when (eq (block-flag target) cycle)
                            (mark target)))))))))
             (mark (block)
               (let ((2block (block-info block)))
                 (unless (eq (block-flag block) flag)
                   (setf (block-flag block) flag)
                   (setf (ir2-block-end-stack 2block)
                         (merge-uvl-live-sets
                          preserve-lvars
                          (ir2-block-end-stack 2block)))
                   (setf (ir2-block-start-stack 2block)
                         (merge-uvl-live-sets
                          preserve-lvars
                          (ir2-block-start-stack 2block)))
                   (revisit-cycles block))))
             (back-propagate-pathwise (current-block)
               (cond
                 ((member current-block use-blocks)
                  ;; The LVAR is live on exit from a use-block, but
                  ;; not on entry.
                  (pushnew dx-lvar (ir2-block-end-stack
                                    (block-info current-block)))
                  t)
                 ((eq (block-flag current-block) flag)
                  t)
                 ((eq (block-flag current-block) cycle)
                  nil)
                 ;; Don't go back past START-BLOCK.
                 ((not (eq current-block start-block))
                  (setf (block-flag current-block) cycle)
                  (let (marked)
                    (dolist (pred-block (if (nle-block-p current-block)
                                            ;; Follow backwards through
                                            ;; NLEs to the start of
                                            ;; their environment
                                            (let ((entry-block (nle-block-entry-block current-block)))
                                              ;; Mark for later if
                                              ;; revisit-cycles needs
                                              ;; to go back from here.
                                              (setf (block-out entry-block) nlx)
                                              (list* entry-block (block-pred current-block)))
                                            (block-pred current-block)))
                      (when (back-propagate-pathwise pred-block)
                        (mark current-block)
                        (setf marked t)))
                    marked)))))
      (back-propagate-pathwise block))))

(defun back-propagate-dx-lvars (block dx-lvars)
  (declare (type cblock block)
           (type list dx-lvars))
  (dolist (dx-lvar dx-lvars)
    (back-propagate-one-dx-lvar block dx-lvar)))

;;; Update information on stacks of unknown-values LVARs on the
;;; boundaries of BLOCK. Return true if the start stack has been
;;; changed.
;;;
;;; An LVAR is live at the end iff it is live at some of blocks, which
;;; BLOCK can transfer control to. There are two kind of control
;;; transfers: normal, expressed with BLOCK-SUCC, and NLX.
(defun update-uvl-live-sets (block)
  (declare (type cblock block))
  (let* ((2block (block-info block))
         (original-start (ir2-block-start-stack 2block))
         (end (ir2-block-end-stack 2block))
         (new-end end))
    (dolist (succ (block-succ block))
      (setq new-end (merge-uvl-live-sets new-end
                                         ;; Don't back-propagate DX
                                         ;; LVARs automatically,
                                         ;; they're handled specially.
                                         (remove-if #'lvar-dynamic-extent
                                                    (ir2-block-start-stack (block-info succ))))))
    (do-nested-cleanups (cleanup (block-end-lexenv block))
      (case (cleanup-kind cleanup)
        ((:block :tagbody :catch :unwind-protect)
         (dolist (nlx-info (cleanup-nlx-info cleanup))
           (let* ((nle (nlx-info-target nlx-info))
                  (nle-start-stack (ir2-block-start-stack
                                    (block-info nle)))
                  (exit-lvar (nlx-info-lvar nlx-info))
                  (next-stack (if exit-lvar
                                  (remove exit-lvar nle-start-stack)
                                  nle-start-stack)))
             (setq new-end (merge-uvl-live-sets
                            new-end next-stack)))))
        (:dynamic-extent
         (dolist (lvar (cleanup-nlx-info cleanup))
           (do-uses (generator lvar)
             (let* ((block (node-block generator))
                    (2block (block-info block)))
               ;; DX objects, living in the LVAR, are alive in
               ;; the environment, protected by the CLEANUP. We
               ;; also cannot move them (because, in general, we
               ;; cannot track all references to them).
               ;; Therefore, everything, allocated deeper than a
               ;; DX object -- that is, before the DX object --
               ;; should be kept alive until the object is
               ;; deallocated.
               (setq new-end (merge-uvl-live-sets
                              new-end
                              (set-difference
                               (ir2-block-start-stack 2block)
                               (ir2-block-popped 2block))))
               (setq new-end (merge-uvl-live-sets
                              ;; union in the lvars
                              ;; pushed before LVAR in
                              ;; this block.
                              new-end (member lvar (reverse (ir2-block-pushed 2block)))))))))))

    (setf (ir2-block-end-stack 2block) new-end)

    ;; If a block has a "entry DX" node (the start of a DX
    ;; environment) then we need to back-propagate the DX LVARs to
    ;; their allocation sites.  We need to be clever about this
    ;; because some code paths may not allocate all of the DX LVARs.
    (do-nodes (node nil block)
      (when (typep node 'entry)
        (let ((cleanup (entry-cleanup node)))
          (when (eq (cleanup-kind cleanup) :dynamic-extent)
            (back-propagate-dx-lvars block (cleanup-nlx-info cleanup))))))

    (let ((start new-end))
      (setq start (set-difference start (ir2-block-pushed 2block)))
      (setq start (merge-uvl-live-sets start (ir2-block-popped 2block)))

      ;; We cannot delete unused UVLs during NLX, so all UVLs live at
      ;; ENTRY which are not popped will be actually live at NLE.
      ;;
      ;; BUT, UNWIND-PROTECTor is called in the environment, which has
      ;; nothing in common with the environment of its entry. So we
      ;; fictively compute its stack from the containing cleanups, but
      ;; do not propagate additional LVARs from the entry, thus
      ;; preveting bogus stack cleanings.
      ;;
      ;; TODO: Insert a check that no values are discarded in UWP. Or,
      ;; maybe, we just don't need to create NLX-ENTRY for UWP?
      (when (nle-block-p block)
        (let* ((nlx-info (nle-block-nlx-info block))
               (cleanup (nlx-info-cleanup nlx-info)))
          (unless (eq (cleanup-kind cleanup) :unwind-protect)
            (let* ((entry-block (node-block (cleanup-mess-up cleanup)))
                   (entry-stack (set-difference
                                 (ir2-block-start-stack (block-info entry-block))
                                 (ir2-block-popped (block-info entry-block)))))
              (setq start (merge-uvl-live-sets start entry-stack))))))

      (when *check-consistency*
        (aver (subsetp original-start start)))
      (cond ((subsetp start original-start)
             nil)
            (t
             (setf (ir2-block-start-stack 2block) start)
             t)))))


;;;; Ordering of live UVL stacks

(defun ordered-list-intersection (ordered-list other-list)
  (loop for item in ordered-list
     when (memq item other-list)
     collect item))

(defun ordered-list-union (ordered-list-1 ordered-list-2)
  (labels ((sub-union (ol1 ol2 result)
             (cond ((and (null ol1) (null ol2))
                    result)
                   ((and (null ol1) ol2)
                    (sub-union ol1 (cdr ol2) (cons (car ol2) result)))
                   ((and ol1 (null ol2))
                    (sub-union (cdr ol1) ol2 (cons (car ol1) result)))
                   ((eq (car ol1) (car ol2))
                    (sub-union (cdr ol1) (cdr ol2) (cons (car ol1) result)))
                   ((memq (car ol1) ol2)
                    (sub-union ol1 (cdr ol2) (cons (car ol2) result)))
                   (t
                    (sub-union (cdr ol1) ol2 (cons (car ol1) result))))))
    (nreverse (sub-union ordered-list-1 ordered-list-2 nil))))

;;; Put UVLs on the start/end stacks of BLOCK in the right order. PRED
;;; is a predecessor of BLOCK with already sorted stacks; if all UVLs
;;; being live at the BLOCK start are live in PRED we just need to
;;; delete killed UVLs, otherwise we need (thanks to conditional or
;;; nested DX) to set a total order for the UVLs live at the end of
;;; all predecessors.
(defun order-block-uvl-sets (block pred)
  (let* ((2block (block-info block))
         (pred-end-stack (ir2-block-end-stack (block-info pred)))
         (start (ir2-block-start-stack 2block))
         (start-stack (ordered-list-intersection pred-end-stack start))
         (end (ir2-block-end-stack 2block)))

    (when (not (subsetp start start-stack))
      ;; If BLOCK is a control-flow join for DX allocation paths with
      ;; different sets of DX LVARs being pushed then we cannot
      ;; process it correctly until all of its predecessors have been
      ;; processed.
      (unless (every #'block-flag (block-pred block))
        (return-from order-block-uvl-sets nil))
      ;; If we are in the conditional-DX control-flow join case then
      ;; we need to find an order for START-STACK that is compatible
      ;; with all of our predecessors.
      (dolist (end-stack (mapcar #'ir2-block-end-stack
                                 (mapcar #'block-info
                                         (block-pred block))))
        (setf pred-end-stack
              (ordered-list-union pred-end-stack end-stack)))
      (setf start-stack (ordered-list-intersection pred-end-stack start)))

    (when *check-consistency*
      (aver (subsetp start start-stack)))
    (setf (ir2-block-start-stack 2block) start-stack)

    (let* ((last (block-last block))
           (tailp-lvar (if (node-tail-p last) (node-lvar last)))
           (end-stack start-stack))
      (dolist (pop (ir2-block-popped 2block))
        (aver (eq pop (car end-stack)))
        (pop end-stack))
      (dolist (push (ir2-block-pushed 2block))
        (aver (not (memq push end-stack)))
        (push push end-stack))
      (aver (subsetp end end-stack))
      (when (and tailp-lvar
                 (eq (ir2-lvar-kind (lvar-info tailp-lvar)) :unknown))
        (aver (eq tailp-lvar (first end-stack)))
        (pop end-stack))
      (setf (ir2-block-end-stack 2block) end-stack)))
  t)

(defun order-uvl-sets (component)
  (clear-flags component)
  ;; KLUDGE: Workaround for lp#308914: we keep track of number of blocks
  ;; needing repeats, and bug out if we get stuck.
  (loop with head = (component-head component)
        with todo = 0
        with last-todo = 0
        do (psetq last-todo todo
                  todo 0)
        do (do-blocks (block component)
             (unless (block-flag block)
               (let ((pred (find-if #'block-flag (block-pred block))))
                 (when (and (eq pred head)
                            (not (bind-p (block-start-node block))))
                   (let ((entry (nle-block-entry-block block)))
                     (setq pred (if (block-flag entry) entry nil))))
                 (if (and pred
                          (order-block-uvl-sets block pred))
                     (setf (block-flag block) t)
                     (incf todo)))))
        do (when (= last-todo todo)
             ;; If the todo count is the same as on last iteration and
             ;; there are still blocks to do, it means we are stuck,
             ;; which in turn means the unmarked blocks are actually
             ;; unreachable and should have been eliminated by DCE,
             ;; and will very likely cause problems with later parts
             ;; of STACK analysis, so abort now if we're in trouble.
             (aver (not (plusp todo))))
        while (plusp todo)))

;;; This is called when we discover that the stack-top unknown-values
;;; lvar at the end of BLOCK1 is different from that at the start of
;;; BLOCK2 (its successor).
;;;
;;; We insert a call to a funny function in a new cleanup block
;;; introduced between BLOCK1 and BLOCK2. Since control analysis and
;;; LTN have already run, we must do make an IR2 block, then do
;;; ADD-TO-EMIT-ORDER and LTN-ANALYZE-BELATED-BLOCK on the new
;;; block. The new block is inserted after BLOCK1 in the emit order.
;;;
;;; If the control transfer between BLOCK1 and BLOCK2 represents a
;;; tail-recursive return or a non-local exit, then the cleanup code
;;; will never actually be executed. It doesn't seem to be worth the
;;; risk of trying to optimize this, since this rarely happens and
;;; wastes only space.
(defun insert-stack-cleanups (block1 block2)
  (declare (type cblock block1 block2))
  (collect ((cleanup-code))
    (labels ((find-popped (before after)
               ;; Returns (VALUES popped last-popped rest), where
               ;; BEFORE = (APPEND popped rest) and
               ;; (EQ (FIRST rest) (FIRST after))
               (if (null after)
                   (values before (first (last before)) nil)
                   (loop with first-preserved = (car after)
                         for last-popped = nil then maybe-popped
                         for rest on before
                         for maybe-popped = (car rest)
                         while (neq maybe-popped first-preserved)
                         collect maybe-popped into popped
                         finally (return (values popped last-popped rest)))))
             (discard (before-stack after-stack)
               (cond
                 ((eq (car before-stack) (car after-stack))
                  (binding* ((moved-count (mismatch before-stack after-stack)
                                          :exit-if-null)
                             (moved
                              (loop for moved-lvar in before-stack
                                    repeat moved-count
                                    collect moved-lvar))
                             ((nil last-nipped rest)
                              (find-popped (nthcdr moved-count before-stack)
                                           (nthcdr moved-count after-stack))))
                    (cleanup-code
                     `(%nip-values ,(opaquely-quote last-nipped)
                                   ,(opaquely-quote (car (last moved)))
                                   ,@(mapcar #'opaquely-quote moved)))
                    (discard (nconc moved rest) after-stack)))
                 (t
                  (multiple-value-bind (popped last-popped rest)
                      (find-popped before-stack after-stack)
                    (declare (ignore popped))
                    (cleanup-code `(%pop-values ,(opaquely-quote last-popped)))
                    (discard rest after-stack)))))
             (dummy-allocations (before-stack after-stack)
               (loop
                  for previous-lvar = nil then lvar
                  for lvar in after-stack
                  unless (memq lvar before-stack)
                  do (cleanup-code
                      `(%dummy-dx-alloc ,(opaquely-quote lvar) ,(opaquely-quote previous-lvar))))))
      (let* ((end-stack (ir2-block-end-stack (block-info block1)))
             (start-stack (ir2-block-start-stack (block-info block2)))
             (pruned-start-stack (ordered-list-intersection
                                  start-stack end-stack)))
        (discard end-stack pruned-start-stack)
        (dummy-allocations pruned-start-stack start-stack)
        (when (cleanup-code)
          (let* ((block (insert-cleanup-code (list block1) block2
                                             (block-start-node block2)
                                             `(progn ,@(cleanup-code))))
                 (2block (make-ir2-block block)))
            (setf (block-info block) 2block)
            (add-to-emit-order 2block (block-info block1))
            (ltn-analyze-belated-block block)
            ;; Set the start and end stacks to make traces less
            ;; confusing.  Purely cosmetic.
            (setf (ir2-block-start-stack 2block) end-stack)
            (setf (ir2-block-end-stack 2block) start-stack))))))

  (values))

;;;; stack analysis

;;; Return a list of all the blocks containing genuine uses of one of
;;; the RECEIVERS (blocks) and DX-LVARS. Exits are excluded, since
;;; they don't drop through to the receiver.
(defun find-pushing-blocks (receivers dx-lvars)
  (declare (list receivers dx-lvars))
  (collect ((res nil adjoin))
    (dolist (rec receivers)
      (dolist (pop (ir2-block-popped (block-info rec)))
        (do-uses (use pop)
          (unless (exit-p use)
            (res (node-block use))))))
    (dolist (dx-lvar dx-lvars)
      (do-uses (use dx-lvar)
        (res (node-block use))))
    (res)))

;;; Analyze the use of unknown-values and DX lvars in COMPONENT,
;;; inserting cleanup code to discard values that are generated but
;;; never received and to set appropriate bounds for DX values that
;;; are cleaned up but never allocated. This phase doesn't need to be
;;; run when Values-Receivers and Dx-Lvars are null, i.e. there are no
;;; unknown-values lvars used across block boundaries and no DX LVARs.
(defun stack-analyze (component)
  (declare (type component component))
  (let* ((2comp (component-info component))
         (receivers (ir2-component-values-receivers 2comp))
         (generators (find-pushing-blocks receivers
                                          (component-dx-lvars component))))

    (dolist (block generators)
      (find-pushed-lvars block)))

  ;; Compute sets of live UVLs and DX LVARs
  (loop for did-something = nil
     do (do-blocks-backwards (block component)
          (when (update-uvl-live-sets block)
            (setq did-something t)))
     while did-something)

  (order-uvl-sets component)

  (do-blocks (block component)
    (let ((top (ir2-block-end-stack (block-info block))))
      (dolist (succ (block-succ block))
        (when (and (block-start succ)
                   (not (eq (ir2-block-start-stack (block-info succ))
                            top)))
          ;; Return resets the stack, so no need to clean anything.
          (let ((start (block-last succ)))
            (unless (and (return-p start)
                         (eq (block-start succ) (node-prev start)))
              (insert-stack-cleanups block succ)))))))

  (values))
