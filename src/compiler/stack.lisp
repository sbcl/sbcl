;;;; This file implements the stack analysis phase in the compiler. We
;;;; do a graph walk to determine which unknown-values lvars are on
;;;; the stack at each point in the program, and then we insert
;;;; cleanup code to pop off unused values.

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

	  (when lvar
            (let ((dest (lvar-dest lvar))
                  (2lvar (lvar-info lvar)))
              (when (and (not (eq (node-block dest) block))
                         2lvar
                         (eq (ir2-lvar-kind 2lvar) :unknown))
                (aver (or saw-last (not last-pop)))
                (pushed lvar))))))

      (setf (ir2-block-pushed 2block) (pushed))))
  (values))

;;;; annotation graph walk

;;; Add LVARs from LATE to EARLY; use EQ to check whether EARLY has
;;; been changed.
(defun merge-stacks (early late)
  (declare (type list early late))
  (cond ((null early) late)
        ((null late) early)
        ((tailp early late) late)
        ((tailp late early) early)
        ;; FIXME
        (t (bug "Lexical unwinding of UVL stack is not implemented."))))

;;; Update information on stacks of unknown-values LVARs on the
;;; boundaries of BLOCK. Return true if the start stack has been
;;; changed.
(defun stack-update (block)
  (declare (type cblock block))
  (declare (optimize (debug 3)))
  (let* ((2block (block-info block))
         (end (ir2-block-end-stack 2block))
         (new-end end)
         (cleanup (block-end-cleanup block))
         (found-similar-p nil))
    (dolist (succ (block-succ block))
      #+nil
      (when (and (< block succ)
                 (eq cleanup (block-end-cleanup succ)))
        (setq found-similar-p t))
      (setq new-end (merge-stacks new-end (ir2-block-start-stack (block-info succ)))))
    (unless found-similar-p
      (map-block-nlxes (lambda (nlx-info)
                         (let* ((nle (nlx-info-target nlx-info))
                                (nle-start-stack (ir2-block-start-stack
                                                  (block-info nle)))
                                (exit-lvar (nlx-info-lvar nlx-info)))
                           (when (eq exit-lvar (car nle-start-stack))
                             (pop nle-start-stack))
                           (setq new-end (merge-stacks new-end
                                                       nle-start-stack))))
                       block))

    (setf (ir2-block-end-stack 2block) new-end)
    (let ((start new-end))
      (dolist (push (reverse (ir2-block-pushed 2block)))
        (if (eq (car start) push)
            (pop start)
            (aver (not (member push start)))))

      (dolist (pop (reverse (ir2-block-popped 2block)))
        (push pop start))

      (cond ((equal-but-no-car-recursion start
                                         (ir2-block-start-stack 2block))
             nil)
            (t
             (setf (ir2-block-start-stack 2block) start)
             t)))))

;;; Do stack annotation for any values generators in Block that were
;;; unreached by all walks (i.e. the lvar isn't live at the point that
;;; it is generated.)  This will only happen when the values receiver cannot be
;;; reached from this particular generator (due to an unconditional control
;;; transfer.)
;;;
;;; What we do is push on the End-Stack all lvars in Pushed that
;;; aren't already present in the End-Stack. When we find any pushed
;;; lvar that isn't live, it must be the case that all lvars
;;; pushed after (on top of) it aren't live.
;;;
;;; If we see a pushed lvar that is the LVAR of a tail call, then we
;;; ignore it, since the tail call didn't actually push anything. The
;;; tail call must always the last in the block.
;;;
;;; [This function also fixes End-Stack in NLEs.]
(defun annotate-dead-values (block)
  (declare (type cblock block))
  (let* ((2block (block-info block))
	 (stack (ir2-block-end-stack 2block))
	 (last (block-last block))
	 (tailp-lvar (if (node-tail-p last) (node-lvar last))))
    (do ((pushes (ir2-block-pushed 2block) (rest pushes))
	 (popping nil))
	((null pushes))
      (let ((push (first pushes)))
	(cond ((member push stack)
	       (aver (not popping)))
	      ((eq push tailp-lvar)
	       (aver (null (rest pushes))))
	      (t
	       (push push (ir2-block-end-stack 2block))
	       (setq popping t))))))

  (values))

;;; For every NLE block push all LVARs that are live in its ENTRY to
;;; its start stack. (We cannot pop unused LVARs on a control transfer
;;; to an NLE block, so we must do it later.)
(defun fix-nle-block-stacks (component)
  (declare (type component component))
  (dolist (block (block-succ (component-head component)))
    (let ((start-node (block-start-node block)))
      (unless (bind-p start-node)
        (let* ((2block (block-info block))
               (start-stack (block-start-stack 2block))
               (nlx-ref (ctran-next (node-next start-node)))
               (nlx-info (constant-value (ref-leaf nlx-ref)))
               (mess-up (cleanup-mess-up (nlx-info-cleanup nlx-info)))
               (entry-block (node-block mess-up))
               (entry-stack (ir2-block-start-stack (block-info entry-block)))
               (exit-lvar (nlx-info-lvar nlx-info)))
          (when (and exit-lvar
                     (eq exit-lvar (car start-stack)))
            (when *check-consistency*
              (aver (not (memq exit-lvar entry-stack))))
            (push exit-lvar entry-stack))
          (when *check-consistency*
            (aver (subsetp start-stack entry-stack)))
          (setf (ir2-block-start-stack 2block) entry-stack)
          (setf (ir2-block-end-stack 2block) entry-stack)
                                        ; ANNOTATE-DEAD-VALUES will do the rest
          )))))

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
  (let* ((block1-stack (ir2-block-end-stack (block-info block1)))
	 (block2-stack (ir2-block-start-stack (block-info block2)))
	 (last-popped (elt block1-stack
			   (- (length block1-stack)
			      (length block2-stack)
			      1))))
    (aver (tailp block2-stack block1-stack))

    (let* ((block (insert-cleanup-code block1 block2
				       (block-start-node block2)
				       `(%pop-values ',last-popped)))
	   (2block (make-ir2-block block)))
      (setf (block-info block) 2block)
      (add-to-emit-order 2block (block-info block1))
      (ltn-analyze-belated-block block)))

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

;;; Analyze the use of unknown-values lvars in COMPONENT, inserting
;;; cleanup code to discard values that are generated but never
;;; received. This phase doesn't need to be run when Values-Receivers
;;; is null, i.e. there are no unknown-values lvars used across block
;;; boundaries.
;;;
;;; Do the backward graph walk, starting at each values receiver. We
;;; ignore receivers that already have a non-null START-STACK. These
;;; are nested values receivers that have already been reached on
;;; another walk. We don't want to clobber that result with our null
;;; initial stack.
(defun stack-analyze (component)
  (declare (type component component))
  (let* ((2comp (component-info component))
	 (receivers (ir2-component-values-receivers 2comp))
	 (generators (find-values-generators receivers)))

    (dolist (block generators)
      (find-pushed-lvars block))

    (loop for did-something = nil
          do (do-blocks-backwards (block component)
               (when (stack-update block)
                 (setq did-something t)))
          while did-something)

    (when *check-consistency*
      (dolist (block (block-succ (component-head component)))
        (when (bind-p (block-start-node block))
          (aver (null (ir2-block-start-stack (block-info block)))))))

    (dolist (block generators)
      (annotate-dead-values block))

    (do-blocks (block component)
      (let ((top (car (ir2-block-end-stack (block-info block)))))
	(dolist (succ (block-succ block))
	  (when (and (block-start succ)
		     (not (eq (car (ir2-block-start-stack (block-info succ)))
			      top)))
	    (discard-unused-values block succ))))))

  (values))
