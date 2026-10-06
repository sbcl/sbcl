;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; **********************************************************************
;;;
;;; Stuff to annotate the flow graph with information about the loops in it.
;;;
;;; Written by Rob MacLachlan
(in-package "SB-C")

;;; Return the lowest common ancestor of BLOCK1 and BLOCK2 in the
;;; dominator tree.
(defun lowest-common-dominator (block1 block2)
  (declare (type cblock block1 block2))
  (cond ((eq block1 block2) block1)
        ((< (block-number block1) (block-number block2))
         (lowest-common-dominator (block-dominator block1) block2))
        (t
         (lowest-common-dominator block1 (block-dominator block2)))))

;;; FIND-DOMINATORS  --  Internal
;;;
;;; Find the immediate dominator of each block in COMPONENT.  If a
;;; block is not reachable from an entry point, then its immediate
;;; dominator will still be NIL when we are done.
(defun find-dominators (component)
  (let ((head (loop-head (component-outer-loop component)))
        changed)
    (do-blocks (block component :tail)
      (setf (block-dominator block) nil))
    (setf (block-dominator head) head)
    (dfo-as-needed component)
    (loop
      (setq changed nil)
      (do-blocks (block component :tail)
        (let ((dom))
          (dolist (pred (block-pred block))
            (unless (null (block-dominator pred))
              (setq dom (if dom
                            (lowest-common-dominator pred dom)
                            pred))))
          (unless (eq (block-dominator block) dom)
            (setf (block-dominator block) dom)
            (setq changed t))))
      (unless changed (return)))
    (setf (block-dominator head) nil)))

;;; DOMINATES-P  --  Internal
;;;
;;;    Return true if BLOCK1 dominates BLOCK2, false otherwise.
(defun dominates-p (block1 block2)
  (cond ((null block2) nil)
        ((eq block1 block2) t)
        (t
         (dominates-p block1 (block-dominator block2)))))

;;; LOOP-ANALYZE  --  Interface
;;;
;;; Set up the LOOP structures which describe the loops in the flow
;;; graph for COMPONENT.  We NIL out any existing loop information,
;;; and then scan through the blocks looking for blocks which are the
;;; destination of a retreating edge: an edge that goes backward in
;;; the DFO.  We then create LOOP structures to describe the loops
;;; that have those blocks as their heads.  If find the head of a
;;; strange loop, then we do some graph walking to flag the other
;;; segments in the strange loop.  While we are finding the loop
;;; structures in reverse DFO, we walk it to initialize the block
;;; lists and initialize the nesting pointers. Then we assign loop depth.
(defun loop-analyze (component)
  (let ((outer-loop (component-outer-loop component)))
    (do-blocks (block component :both)
      (setf (block-loop block) nil))
    (setf (loop-inferiors outer-loop) ())
    (setf (loop-blocks outer-loop) ())
    ;; By traversing in reverse depth first ordering, we guarantee
    ;; that inner loop heads will be discovered before their
    ;; superiors, since dominated nodes always have lower DFNs.
    (do-blocks-backwards (block component)
      (let ((number (block-number block)))
        (dolist (pred (block-pred block))
          (when (<= (block-number pred) number)
            (let ((loop (note-loop-head block)))
              (when (eq (loop-kind loop) :strange)
                (let ((head (loop-head loop)))
                  (flag-strange-loop-blocks head head loop)))
              (find-loop-blocks loop)
              ;; Loops with no exits are unreachable by predecessor walk and
              ;; by definition belong to the component outer loop.
              (unless (or (loop-exits loop)
                          (eq outer-loop loop))
                (setf (loop-superior loop) outer-loop)
                (push loop (loop-inferiors outer-loop))))
            (return)))))
    ;; Remaining blocks belong to the outer loop.
    (find-loop-blocks outer-loop)
    (labels ((assign-depth (loop depth)
               (setf (loop-depth loop) depth)
               (dolist (inferior (loop-inferiors loop))
                 (assign-depth inferior (1+ depth)))))
      (assign-depth outer-loop 0))))

;;; FIND-LOOP-BLOCKS  --  Internal
;;;
;;; This function initializes the block lists and inferiors of LOOP.
;;; When we are done, we scan the blocks looking for exits.  An exit
;;; is always a block that has a successor which doesn't have a LOOP
;;; assigned yet, since the target of the exit must be in a superior
;;; loop.
;;;
;;; We find the blocks by doing a backward walk from the tails of the
;;; loop and from any heads of nested loops.  The walks from inferior
;;; loop heads are necessary because the walks from the tails
;;; terminate when they encounter a block in an inferior loop.
(defun find-loop-blocks (loop)
  (dolist (tail (loop-tail loop))
    (find-blocks-from-here tail loop))
  ;; For the outermost loop, new blocks can still be discovered by
  ;; walking back from loops with no exits.
  (when (eq (loop-kind loop) :outer)
    (dolist (sub-loop (loop-inferiors loop))
      (find-blocks-from-inferior sub-loop loop)))
  (collect ((exits))
    (dolist (sub-loop (loop-inferiors loop))
      (dolist (exit (loop-exits sub-loop))
        (dolist (succ (block-succ exit))
          (unless (block-loop succ)
            (exits exit)
            (return)))))

    (do ((block (loop-blocks loop) (block-loop-next block)))
        ((null block))
      (dolist (succ (block-succ block))
        (unless (block-loop succ)
          (exits block)
          (return))))
    (setf (loop-exits loop) (exits))))


;;; FIND-BLOCKS-FROM-HERE  --  Internal
;;;
;;; This function does a graph walk to find the blocks directly within
;;; LOOP that can be reached by a backward walk from BLOCK.  If BLOCK
;;; is already in LOOP or is not dominated by the LOOP-HEAD, then we
;;; return.  If another loop is already assigned to BLOCK, it must be
;;; an inferior loop.  If this loop doesn't have a superior yet, we
;;; record that it must be a direct inferior of LOOP, and recurse on
;;; the head of this loop's predecessor.  But if BLOCK's loop already
;;; has a superior, then we can directly recurse on its existing
;;; superior's head, since all predecessors of the head of BLOCK's
;;; loop are contained in its superior already.  Otherwise, we add the
;;; block to the BLOCKS for LOOP and recurse on its predecessors.  For
;;; a strange loop, its head doesn't dominate its blocks, so we check
;;; the flag that was set beforehand.
(defun find-blocks-from-here (block loop)
  (when (and (not (eq (block-loop block) loop))
             (if (eq (loop-kind loop) :strange)
                 (eq (block-flag block) loop)
                 (dominates-p (loop-head loop) block)))
    (cond ((block-loop block)
           (let* ((inner (block-loop block))
                  (inner-superior (loop-superior inner)))
             (cond ((not inner-superior)
                    (setf (loop-superior inner) loop)
                    (push inner (loop-inferiors loop))
                    (find-blocks-from-inferior inner loop))
                   ((not (eq inner-superior loop))
                    (find-blocks-from-here (loop-head inner-superior) loop)))))
          (t
           (setf (block-loop block) loop)
           (shiftf (block-loop-next block) (loop-blocks loop) block)
           (dolist (pred (block-pred block))
             (find-blocks-from-here pred loop))))))

;;; Walk back from the blocks of the inferior loop INNER to find more
;;; blocks of its superior LOOP. A strange loop can be entered from
;;; any its blocks.
(defun find-blocks-from-inferior (inner loop)
  (if (eq (loop-kind inner) :strange)
      (labels ((walk (inner)
                 (do ((block (loop-blocks inner) (block-loop-next block)))
                     ((null block))
                   (dolist (pred (block-pred block))
                     (find-blocks-from-here pred loop)))
                 (mapc #'walk (loop-inferiors inner))))
        (walk inner))
      (dolist (pred (block-pred (loop-head inner)))
        (find-blocks-from-here pred loop))))

;;; NOTE-LOOP-HEAD  --  Internal
;;;
;;; Create a loop structure to describe the loop headed by the block
;;; HEAD.  If some retreating edge into the head is from a block which
;;; isn't dominated by the head, then we have the head of a strange
;;; loop segment.
(defun note-loop-head (head)
  (let ((result (make-loop :natural head))
        (number (block-number head)))
    (dolist (pred (block-pred head))
      (when (<= (block-number pred) number)
        (push pred (loop-tail result))
        (unless (dominates-p head pred)
          (setf (loop-kind result) :strange))))
    result))

;;; FLAG-STRANGE-LOOP-BLOCKS  --  Internal
;;;
;;; Do a graph walk to flag the blocks in the strange loop which HEAD
;;; is in.  BLOCK is the block we are currently at and COMPONENT is
;;; the component we are in.  We do a walk forward from block, using
;;; only edges which are not back edges.
(defun flag-strange-loop-blocks (block head loop)
  (unless (eq (block-flag block) loop)
    (setf (block-flag block) loop)
    (dolist (succ (block-succ block))
      (when (< (block-number succ)
               (block-number head))
        (flag-strange-loop-blocks succ head loop)))))
