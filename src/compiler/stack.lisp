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
  (dolist (e late early)
    (pushnew e early)))

;;; Update information on stacks of unknown-values LVARs on the
;;; boundaries of BLOCK. Return true if the start stack has been
;;; changed.
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
                     block)

    (setf (ir2-block-end-stack 2block) new-end)
    (let ((start new-end))
      (setq start (set-difference start (ir2-block-pushed 2block)))
      (setq start (merge-uvl-live-sets start (ir2-block-popped 2block)))

      (when (and (eq (component-head (block-component block))
                     (first (block-pred block)))
                 (not (bind-p (block-start-node block))))
        (let* ((entry-block (nle-block-entry-block block))
               (entry-stack (ir2-block-start-stack (block-info entry-block))))
          (setq start (merge-uvl-live-sets start entry-stack))))

      (when *check-consistency*
               (aver (subsetp original-start start)))
      (values (cond ((subsetp start original-start)
                     nil)
                    (t
                     (setf (ir2-block-start-stack 2block) start)
                     t))
              start new-end))))

;;;
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
      (setq end-stack (set-difference end-stack (ir2-block-popped 2block)))
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
  (loop with head = (component-head component)
        with repeat-p do
        (setq repeat-p nil)
        (do-blocks (block component)
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
                     (setq repeat-p t))))))
        while repeat-p))

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
         (cleanup-code
          (cond ((eq (car block1-stack) (car block2-stack))
                 (binding* ((preserved-count (mismatch block1-stack block2-stack)
                              :exit-if-null)
                            (n-last-preserved (1- preserved-count))
                            (nipped-count (- (length block1-stack)
                                             (length block2-stack)))
                            (n-last-nipped (+ n-last-preserved nipped-count)))
                   (aver (equal (nthcdr (1+ n-last-nipped) block1-stack)
                                (nthcdr preserved-count block2-stack)))
                   (format t "%NIP-VALUES emitted~%")
                   `(%nip-values ',(elt block1-stack n-last-nipped)
                                 ',(elt block1-stack n-last-preserved)
                                 ,@(loop for moved in block1-stack
                                         repeat preserved-count
                                         collect `',moved))))
                (t
                 (let* ((n-popped (- (length block1-stack)
                                     (length block2-stack)))
                       (last-popped (elt block1-stack (1- n-popped))))
                   (when *check-consistency*
                     (aver (equal block2-stack (nthcdr n-popped block1-stack))))
                   `(%pop-values ',last-popped))))))
    (when cleanup-code
      (let* ((block (insert-cleanup-code block1 block2
                                         (block-start-node block2)
                                         cleanup-code))
             (2block (make-ir2-block block)))
        (setf (block-info block) 2block)
        (add-to-emit-order 2block (block-info block1))
        (ltn-analyze-belated-block block))))

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
(defun stack-analyze (component &aux (*check-consistency* t))
  (declare (type component component))
  (let* ((2comp (component-info component))
	 (receivers (ir2-component-values-receivers 2comp))
	 (generators (find-values-generators receivers)))

    (dolist (block generators)
      (find-pushed-lvars block))

    (loop for did-something = nil
          do (do-blocks-backwards (block component)
               (when (update-uvl-live-sets block)
                 (setq did-something t)))
          while did-something)

    ;;(break)
    (order-uvl-sets component)
    ;;(break)

    (do-blocks (block component)
      (let ((top (ir2-block-end-stack (block-info block))))
	(dolist (succ (block-succ block))
	  (when (and (block-start succ)
		     (not (eq (ir2-block-start-stack (block-info succ))
			      top)))
	    (discard-unused-values block succ))))))

  (values))
