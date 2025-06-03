;;;; This file implements the stack analysis phase in the compiler. We
;;;; determine which unknown-values and stack continuations are on the
;;;; stack at each point in the program, and then we insert cleanup
;;;; code to remove unused values.

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

          (when lvar
            (let ((dest (lvar-dest lvar))
                  (2lvar (lvar-info lvar)))
              (when (and 2lvar
                         (or (and (not (eq (node-block dest) block))
                                  (eq (ir2-lvar-kind 2lvar) :unknown))
                             (eq (ir2-lvar-kind 2lvar) :stack)))
                (aver (or saw-last (not last-pop)))
                (pushed lvar))))))

      (setf (ir2-block-pushed 2block) (pushed))))
  (values))

;;;; Computation of live lvar sets

;;; Update information on stacks of unknown-values and stack LVARs on
;;; the boundaries of BLOCK. Return true if the start stack has been
;;; changed.
;;;
;;; An LVAR is live at the end iff it is live at any of blocks which
;;; BLOCK can transfer control to, or it is a stack lvar kept live
;;; within the extent of its cleanup. There are two kind of control
;;; transfers: normal, expressed with BLOCK-SUCC, and NLX.
(defun update-lvar-live-sets (block)
  (declare (type cblock block))
  (let* ((2block (block-info block))
         (end (ir2-block-end-stack 2block)))
    (dolist (succ (block-succ block))
      (let ((2succ (block-info succ)))
        (setq end (union end (ir2-block-start-stack 2succ)))))
    (do-nested-cleanups (cleanup block)
      (dolist (nlx-info (cleanup-nlx-info cleanup))
        (let ((2target (block-info (nlx-info-target nlx-info))))
          (setq end (union end (ir2-block-start-stack 2target)))))
      (when (eq (cleanup-kind cleanup) :dynamic-extent)
        (let ((info (dynamic-extent-info (cleanup-mess-up cleanup))))
          (when info
            (pushnew info end)))))

    (setf (ir2-block-end-stack 2block) end)

    (let ((start (union (set-difference end (ir2-block-pushed 2block))
                        (ir2-block-popped 2block))))
      (aver (subsetp (ir2-block-start-stack 2block) start))
      (cond ((subsetp start (ir2-block-start-stack 2block))
             nil)
            (t
             (setf (ir2-block-start-stack 2block) start)
             t)))))


;;;; Ordering of live lvar stacks

;;; Do a forward walk in the flow graph and put LVARs on the start/end
;;; stacks of BLOCK in the right order. STACK is an already sorted
;;; stack coming from a predecessor of BLOCK. Because all LVARs live
;;; at the start of BLOCK are on STACK, we just need to remove the
;;; dead LVARs in STACK to compute BLOCK's ordered start stack.
(defun order-lvar-sets-walk (block stack)
  (unless (block-flag block)
    (setf (block-flag block) t)
    (let* ((2block (block-info block))
           (start (ir2-block-start-stack 2block))
           (start-stack (remove-if-not (lambda (lvar)
                                         (memq lvar start))
                                       stack)))
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
        (do-nested-cleanups (cleanup block)
          (dolist (nlx-info (cleanup-nlx-info cleanup))
            (order-lvar-sets-walk (nlx-info-target nlx-info)
                                  end-stack)))
        (dolist (succ (block-succ block))
          (order-lvar-sets-walk succ end-stack)))))

  (values))

;;; Do a forward walk in the flow graph and insert calls to
;;; %DYNAMIC-EXTENT-START whenever we mess up the run-time stack by
;;; allocating a dynamic extent object. BLOCK is the block that is
;;; currently being walked and STACK is the stack of :STACK
;;; lvars. This allows cleanup code inserted by DISCARD-UNUSED-VALUES
;;; to reset the stack to the right place.
(defun stack-mess-up-walk (block stack)
  (declare (type cblock block) (list stack))
  (unless (block-flag block)
    (setf (block-flag block) t)
    (let ((2comp (component-info (block-component block))))
      (do-nodes (node lvar block)
        (let ((dynamic-extent
                (typecase node
                  (enclose (or (enclose-dynamic-extent node)
                               ;; They all share the same stack lvar.
                               (first
                                (enclose-derived-dynamic-extents node))))
                  (cdynamic-extent node)
                  (t (and lvar (lvar-dynamic-extent lvar))))))
          (when dynamic-extent
            (let ((info (dynamic-extent-info dynamic-extent)))
              (cond
                ((null info))
                ((eq info (first stack)))
                ;; Preserve any intervening dynamic-extents by sharing
                ;; the stack lvar (and hence the lifetime). We must do
                ;; this because stack allocated objects can't move;
                ;; object identity must be preserved and we can't in
                ;; general track all references.
                ((memq info stack)
                 (when (or (enclose-p node) (combination-p node))
                   (do-nested-cleanups (cleanup node)
                     (when (eq (cleanup-kind cleanup) :dynamic-extent)
                       (let ((mess-up (cleanup-mess-up cleanup)))
                         (when (eq dynamic-extent mess-up)
                           (return))
                         (let ((old (dynamic-extent-info mess-up)))
                           (when (and old (not (eq info old)))
                             (setf (ir2-lvar-kind (lvar-info old)) :unused)
                             (setf (dynamic-extent-info mess-up) info))))))))
                (t
                 (pushnew block (ir2-component-stack-mess-ups 2comp))
                 (setf (ctran-next (node-prev node)) nil)
                 (let ((ctran (make-ctran)))
                   (with-ir1-environment-from-node node
                     (ir1-convert (node-prev node) ctran info
                                  '(%dynamic-extent-start)))
                   (link-node-to-previous-ctran node ctran))
                 (push info stack))))))
        (when (entry-p node)
          (dolist (nlx-info (cleanup-nlx-info (entry-cleanup node)))
            (stack-mess-up-walk (nlx-info-target nlx-info) stack)))))
    (dolist (succ (block-succ block))
      (stack-mess-up-walk succ stack)))

  (values))

;;; This is called when we discover that the stack-top unknown-values
;;; or stack lvar at the end of BLOCK1 is different from that at the
;;; start of BLOCK2 (its successor).
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
  (let ((end-stack (ir2-block-end-stack (block-info block1)))
        (start-stack (ir2-block-start-stack (block-info block2))))
    (collect ((cleanup-code))
      (labels ((find-popped (before after)
                 ;; Return (VALUES last-popped rest), where
                 ;; (EQ (FIRST rest) (FIRST after)) and
                 ;; (CDR (MEMBER last-popped BEFORE) = rest
                 (do ((first-preserved (car after))
                      (rest before (rest rest))
                      (last-popped nil (first rest)))
                     ((or (eq (first rest) first-preserved)
                          (null rest))
                      (when (null rest)
                        (aver (null after)))
                      (values last-popped rest))))
               (nip-values (before after qmoved)
                 (unless (equal before after)
                   (aver (eq (car before) (car after)))
                   (do ((before before (rest before))
                        (after after (rest after))
                        (last-moved nil (first before)))
                       ((neq (first before) (first after))
                        (multiple-value-bind (last-nipped rest)
                            (find-popped before after)
                          (cleanup-code
                           `(%nip-values ',last-nipped ',last-moved ,@qmoved))
                          (nip-values rest after qmoved)))
                     (aver (first before))
                     (push `',(first before) qmoved)))))
        (multiple-value-bind (last-popped rest)
            (find-popped end-stack start-stack)
          (when last-popped
            (cleanup-code `(%pop-values ',last-popped)))
          (when rest
            (nip-values rest start-stack '()))))
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
          (setf (ir2-block-end-stack 2block) start-stack)))))

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

;;; Analyze the use of unknown-values and dynamic extents in
;;; COMPONENT, inserting cleanup code to discard values that are
;;; generated but never received and to set appropriate bounds for
;;; stack allocated objects. This phase doesn't need to be run when
;;; Values-Receivers is null and Stack-Allocates-P is false,
;;; i.e. there are no unknown-values lvars used across block
;;; boundaries and no stack allocated objects.
(defun stack-analyze (component)
  (declare (type component component))
  (let* ((2comp (component-info component))
         (receivers (ir2-component-values-receivers 2comp))
         (generators (find-values-generators receivers)))

    (when (ir2-component-stack-allocates-p 2comp)
      (clear-flags component)
      (dolist (ep (block-succ (component-head component)))
        (let ((start (block-start-node ep)))
          (when (and (bind-p start)
                     (some #'dynamic-extent-info
                           (lambda-dynamic-extents (bind-lambda start))))
            (stack-mess-up-walk ep ())))))

    (dolist (block generators)
      (find-pushed-lvars block))

    (dolist (block (ir2-component-stack-mess-ups 2comp))
      (unless (ir2-block-pushed (block-info block))
        (find-pushed-lvars block))))

  ;; Compute sets of lvars.
  (loop for did-something = nil
     do (do-blocks-backwards (block component)
          (when (update-lvar-live-sets block)
            (setq did-something t)))
     while did-something)

  (clear-flags component)
  (dolist (ep (block-succ (component-head component)))
    (when (bind-p (block-start-node ep))
      (order-lvar-sets-walk ep ())))

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
