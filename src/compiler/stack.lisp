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

(in-package "SB!C")

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
                                         (ir2-block-start-stack (block-info succ)))))
    (map-block-nlxes (lambda (nlx-info)
                       (let* ((nle (nlx-info-target nlx-info))
                              (nle-start-stack (ir2-block-start-stack
                                                (block-info nle)))
                              (exit-lvar (nlx-info-lvar nlx-info))
                              (next-stack (if exit-lvar
                                              (remove exit-lvar nle-start-stack)
                                              nle-start-stack)))
                         (setq new-end (merge-uvl-live-sets
                                        new-end next-stack))))
                     block
                     (lambda (dx-cleanup)
                       (dolist (lvar (cleanup-info dx-cleanup))
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
                             ;;
                             ;; Since DX generators end their blocks, we can
                             ;; find out UVLs allocated before them by looking
                             ;; at the stack at the end of the block.
                             ;;
                             ;; FIXME: This is not quite true: REFs to DX
                             ;; closures don't end their blocks!
                             (setq new-end (merge-uvl-live-sets
                                            new-end (ir2-block-end-stack 2block)))
                             (setq new-end (merge-uvl-live-sets
                                            new-end (ir2-block-pushed 2block))))))))

    (setf (ir2-block-end-stack 2block) new-end)

    (let ((start new-end))
      (setq start (set-difference start (ir2-block-pushed 2block)))
      (setq start (merge-uvl-live-sets start (ir2-block-popped 2block)))

      ;; We cannot delete unused UVLs during NLX, so all UVLs live at
      ;; ENTRY will be actually live at NLE.
      ;;
      ;; BUT, UNWIND-PROTECTor is called in the environment, which has
      ;; nothing in common with the environment of its entry. So we
      ;; fictively compute its stack from the containing cleanups, but
      ;; do not propagate additional LVARs from the entry, thus
      ;; preveting bogus stack cleanings.
      ;;
      ;; TODO: Insert a check that no values are discarded in UWP. Or,
      ;; maybe, we just don't need to create NLX-ENTRY for UWP?
      (when (and (eq (component-head (block-component block))
                     (first (block-pred block)))
                 (not (bind-p (block-start-node block))))
        (let* ((nlx-info (nle-block-nlx-info block))
               (cleanup (nlx-info-cleanup nlx-info)))
          (unless (eq (cleanup-kind cleanup) :unwind-protect)
            (let* ((entry-block (node-block (cleanup-mess-up cleanup)))
                   (entry-stack (ir2-block-start-stack (block-info entry-block))))
              (setq start (merge-uvl-live-sets start entry-stack))))))

      (when *check-consistency*
        (aver (subsetp original-start start)))
      (cond ((subsetp start original-start)
             nil)
            (t
             (setf (ir2-block-start-stack 2block) start)
             t)))))


;;;; Ordering of live UVL stacks

;;; Put UVLs on the start/end stacks of BLOCK in the right order. PRED
;;; is a predecessor of BLOCK with already sorted stacks; because all
;;; UVLs being live at the BLOCK start are live in PRED, we just need
;;; to delete dead UVLs.
(defun order-block-uvl-sets (block pred)
  (let* ((2block (block-info block))
         (pred-end-stack (ir2-block-end-stack (block-info pred)))
         (start (ir2-block-start-stack 2block))
         (start-stack (loop for lvar in pred-end-stack
                            when (memq lvar start)
                            collect lvar))
         (end (ir2-block-end-stack 2block)))
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
      (setf (ir2-block-end-stack 2block) end-stack))))

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
                 (cond (pred
                        (setf (block-flag block) t)
                        (order-block-uvl-sets block pred))
                       (t
                        (incf todo))))))
        do (when (= last-todo todo)
             ;; If the todo count is the same as on last iteration, it means
             ;; we are stuck, which in turn means the unmarked blocks are
             ;; actually unreachable, so UVL set ordering for them doesn't
             ;; matter.
             (return-from order-uvl-sets))
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
(defun discard-unused-values (block1 block2)
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
                             ((moved qmoved)
                              (loop for moved-lvar in before-stack
                                    repeat moved-count
                                    collect moved-lvar into moved
                                    collect `',moved-lvar into qmoved
                                    finally (return (values moved qmoved))))
                             (q-last-moved (car (last qmoved)))
                             ((nil last-nipped rest)
                              (find-popped (nthcdr moved-count before-stack)
                                           (nthcdr moved-count after-stack))))
                    (cleanup-code
                     `(%nip-values ',last-nipped ,q-last-moved
                       ,@qmoved))
                    (discard (nconc moved rest) after-stack)))
                 (t
                  (multiple-value-bind (popped last-popped rest)
                      (find-popped before-stack after-stack)
                    (declare (ignore popped))
                    (cleanup-code `(%pop-values ',last-popped))
                    (discard rest after-stack))))))
      (discard (ir2-block-end-stack (block-info block1))
               (ir2-block-start-stack (block-info block2))))
    (when (cleanup-code)
      (let* ((block (insert-cleanup-code block1 block2
                                         (block-start-node block2)
                                         `(progn ,@(cleanup-code))))
             (2block (make-ir2-block block)))
        (setf (block-info block) 2block)
        (add-to-emit-order 2block (block-info block1))
        (ltn-analyze-belated-block block))))

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
;;; never received. This phase doesn't need to be run when
;;; Values-Receivers and Dx-Lvars are null, i.e. there are no
;;; unknown-values lvars used across block boundaries and no DX LVARs.
(defun stack-analyze (component)
  (declare (type component component))
  (let* ((2comp (component-info component))
         (receivers (ir2-component-values-receivers 2comp))
         (generators (find-pushing-blocks receivers
                                          (component-dx-lvars component))))

    (dolist (block generators)
      (find-pushed-lvars block))

    ;;; Compute sets of live UVLs and DX LVARs
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
            (discard-unused-values block succ))))))

  (values))
