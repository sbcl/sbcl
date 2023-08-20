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

          (when (and lvar (not (lvar-dynamic-extent lvar))
                     (let ((dest (lvar-dest lvar))
                           (2lvar (lvar-info lvar)))
                       (and (not (eq (node-block dest) block))
                            2lvar
                            (eq (ir2-lvar-kind 2lvar) :unknown))))
            (aver (or saw-last (not last-pop)))
            (pushed lvar))
          (when (and (basic-combination-p node)
                     (eq (basic-combination-kind node) :known)
                     (eq (combination-fun-source-name node) '%dynamic-extent-start))
            (aver (or saw-last (not last-pop)))
            (pushed (lvar-value (car (basic-combination-args node)))))))

      (setf (ir2-block-pushed 2block) (pushed))))
  (values))

;;;; Computation of live UVL sets

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
;;; An LVAR is live at the end iff it is live at any of blocks which
;;; BLOCK can transfer control to, or it is a dynamic extent object
;;; kept live within the extent of its cleanup. There are two kind of
;;; control transfers: normal, expressed with BLOCK-SUCC, and NLX.
(defun update-uvl-live-sets (block)
  (declare (type cblock block))
  (let* ((2block (block-info block))
         (original-start (ir2-block-start-stack 2block))
         (end (ir2-block-end-stack 2block))
         (new-end end))
    (dolist (succ (block-succ block))
      (setq new-end (merge-uvl-live-sets new-end
                                         (ir2-block-start-stack (block-info succ)))))
    (do-nested-cleanups (cleanup block)
      (case (cleanup-kind cleanup)
        ((:block :tagbody :catch :unwind-protect)
         (dolist (nlx-info (cleanup-nlx-info cleanup))
           (let* ((target (nlx-info-target nlx-info))
                  (target-start-stack (ir2-block-start-stack
                                       (block-info target)))
                  (exit-lvar (nlx-info-lvar nlx-info))
                  (next-stack (if exit-lvar
                                  (remove exit-lvar target-start-stack)
                                  target-start-stack)))
             (setq new-end (merge-uvl-live-sets
                            new-end next-stack)))))
        (:dynamic-extent
         (dolist (dx-info (cleanup-nlx-info cleanup))
           (when (dx-info-subparts dx-info)
             (pushnew (dx-info-value dx-info) new-end))))))

    (setf (ir2-block-end-stack 2block) new-end)

    (let ((start new-end))
      (setq start (set-difference start (ir2-block-pushed 2block)))
      (setq start (merge-uvl-live-sets start (ir2-block-popped 2block)))

      (when *check-consistency*
        (aver (subsetp original-start start)))
      (cond ((subsetp start original-start)
             nil)
            (t
             (setf (ir2-block-start-stack 2block) start)
             t)))))


;;;; Ordering of live UVL stacks

;;; Do a forward walk in the flow graph and put UVLs on the start/end
;;; stacks of BLOCK in the right order. STACK is an already sorted
;;; stack coming from a predecessor of BLOCK. Because all UVLs live at
;;; the start of BLOCK are on STACK, we just need to remove dead UVLs,
;;; but only above the top-most dynamic extent stack pointer, as stack
;;; allocated objects can't move; object identity must be preserved
;;; and we can't in general track all references. It's safe to only
;;; track the stack pointer for groups of stack allocated objects with
;;; the same lifetime as we do in the PUSHED sets, since
;;; unknown-values pushes and pops can't be interleaved with stack
;;; allocations, due to the implementation of mv-call relying on
;;; contiguous unknown-values arguments on the stack.
(defun order-uvl-sets-walk (block stack)
  (unless (block-flag block)
    (setf (block-flag block) t)
    (let* ((2block (block-info block))
           (start (ir2-block-start-stack 2block))
           (start-stack
             (collect ((prefix))
               (do ((tail stack (cdr tail)))
                   ((null tail) (prefix))
                 (let ((lvar (car tail)))
                   (when (memq lvar start)
                     (when (lvar-dynamic-extent lvar)
                       (return (append (prefix) tail)))
                     (prefix lvar)))))))
      (aver (subsetp start start-stack))
      (setf (ir2-block-start-stack 2block) start-stack)

      (let* ((last (block-last block))
             (tailp-lvar (if (and (basic-combination-p last)
                                  (node-tail-p last))
                             (node-lvar last)))
             (end-stack start-stack))
        (dolist (pop (ir2-block-popped 2block))
          (aver (eq pop (car end-stack)))
          (pop end-stack))
        (dolist (push (ir2-block-pushed 2block))
          (aver (not (memq push end-stack)))
          (push push end-stack))
        (aver (subsetp (ir2-block-end-stack 2block) end-stack))
        (when (and tailp-lvar
                   (eq (ir2-lvar-kind (lvar-info tailp-lvar)) :unknown))
          (aver (eq tailp-lvar (first end-stack)))
          (pop end-stack))
        (setf (ir2-block-end-stack 2block) end-stack)
        (let ((start-cleanup (block-start-cleanup block)))
          (do-nested-cleanups (cleanup block)
            (when (eq cleanup start-cleanup)
              (return))
            (unless (eq (cleanup-kind cleanup) :dynamic-extent)
              (dolist (nlx-info (cleanup-nlx-info cleanup))
                (order-uvl-sets-walk (nlx-info-target nlx-info)
                                     end-stack)))))
        (dolist (succ (block-succ block))
          (order-uvl-sets-walk succ end-stack)))))

  (values))

;;; Do a forward walk in the flow graph and insert calls to
;;; %DYNAMIC-EXTENT-START whenever we mess up the run-time stack by
;;; allocating a dynamic extent object. BLOCK is the block that is
;;; currently being walked and STACK is the stack of mess-ups. Objects
;;; with the same lifetime share the same mess-ups. This allows
;;; cleanup code inserted by DISCARD-UNUSED-VALUES to reset the stack
;;; to the right place.
(defun stack-mess-up-walk (block stack)
  (declare (type cblock block) (list stack))
  (unless (block-flag block)
    (setf (block-flag block) t)
    (let ((2comp (component-info (block-component block))))
      (do-nodes (node lvar block)
        (when lvar
          (let ((dx-info (lvar-dynamic-extent lvar)))
            (when dx-info
              (let ((value (dx-info-value dx-info)))
                (unless (memq value stack)
                  (push value stack)
                  (pushnew block (ir2-component-stack-mess-ups 2comp))
                  (setf (ctran-next (node-prev node)) nil)
                  (let ((ctran (make-ctran)))
                    (with-ir1-environment-from-node node
                      (ir1-convert (node-prev node) ctran nil
                                   `(%dynamic-extent-start ',value))
                      (link-node-to-previous-ctran node ctran))))))))
        (when (entry-p node)
          (dolist (nlx-info (cleanup-nlx-info (entry-cleanup node)))
            (when (nlx-info-p nlx-info)
              (stack-mess-up-walk (nlx-info-target nlx-info) stack))))))
    (dolist (succ (block-succ block))
      (stack-mess-up-walk succ stack)))

  (values))

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
      (let* ((end-stack (ir2-block-end-stack (block-info block1)))
             (start-stack (ir2-block-start-stack (block-info block2))))
        (discard end-stack start-stack)
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
;;; the RECEIVERS. Exits are excluded, since they don't drop through
;;; to the receiver.
(defun find-values-generators (receivers)
  (declare (list receivers))
  (collect ((res nil adjoin))
    (dolist (rec receivers)
      (dolist (pop (ir2-block-popped (block-info rec)))
        (do-uses (use pop)
          (unless (exit-p use)
            (res (node-block use))))))
    (res)))

;;; Analyze the use of unknown-values and DX lvars in COMPONENT,
;;; inserting cleanup code to discard values that are generated but
;;; never received and to set appropriate bounds for dynamic extent
;;; values. This phase doesn't need to be run when Values-Receivers
;;; and Dx-Lvars are null, i.e. there are no unknown-values lvars used
;;; across block boundaries and no DX LVARs.
(defun stack-analyze (component)
  (declare (type component component))
  (let* ((2comp (component-info component))
         (receivers (ir2-component-values-receivers 2comp))
         (generators (find-values-generators receivers)))

    (when (component-dx-lvars component)
      (clear-flags component)
      (dolist (ep (block-succ (component-head component)))
        (when (bind-p (block-start-node ep))
          (stack-mess-up-walk ep ()))))

    (dolist (block generators)
      (find-pushed-lvars block))

    (dolist (block (ir2-component-stack-mess-ups 2comp))
      (unless (ir2-block-pushed (block-info block))
        (find-pushed-lvars block))))

  ;; Compute sets of live UVLs and DX LVARs
  (loop for did-something = nil
     do (do-blocks-backwards (block component)
          (when (update-uvl-live-sets block)
            (setq did-something t)))
     while did-something)

  (clear-flags component)
  (dolist (ep (block-succ (component-head component)))
    (when (bind-p (block-start-node ep))
      (order-uvl-sets-walk ep ())))

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
              (discard-unused-values block succ)))))))

  (values))
