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
(in-package "SB!C")

;;; FIND-DOMINATORS  --  Internal
;;;
;;; Find the set of blocks that dominates each block in COMPONENT.  We
;;; assume that the DOMINATORS for each block is initially NIL, which
;;; serves to represent the set of all blocks.  If a block is not
;;; reachable from an entry point, then its dominators will still be
;;; NIL when we are done.
(defun find-dominators (component)
  (let ((head (loop-head (component-outer-loop component)))
        changed)
    (let ((set (make-sset)))
      (sset-adjoin head set)
      (setf (block-dominators head) set))
    (loop
     (setq changed nil)
     (do-blocks (block component :tail)
       (let ((dom (block-dominators block)))
         (when dom (sset-delete block dom))
         (dolist (pred (block-pred block))
           (let ((pdom (block-dominators pred)))
             (when pdom
               (if dom
                   (when (sset-intersection dom pdom)
                     (setq changed t))
                   (setq dom (copy-sset pdom) changed t)))))
         (setf (block-dominators block) dom)
         (when dom (sset-adjoin block dom))))
     (unless changed (return)))))


;;; DOMINATES-P  --  Internal
;;;
;;;    Return true if BLOCK1 dominates BLOCK2, false otherwise.
(defun dominates-p (block1 block2)
  (let ((set (block-dominators block2)))
    (if set
        (sset-member block1 set)
        t)))

;;; LOOP-ANALYZE  --  Interface
;;;
;;; Set up the LOOP structures which describe the loops in the flow
;;; graph for COMPONENT.  We NIL out any existing loop information,
;;; and then scan through the blocks looking for blocks which are the
;;; destination of a retreating edge: an edge that goes backward in
;;; the DFO.  We then create LOOP structures to describe the loops
;;; that have those blocks as their heads.  If find the head of a
;;; strange loop, then we do some graph walking to find the other
;;; segments in the strange loop.  After we have found the loop
;;; structure, we walk it to initialize the block lists.
(defun loop-analyze (component)
  (let ((loop (component-outer-loop component)))
    (do-blocks (block component :both)
      (setf (block-loop block) nil))
    (setf (loop-inferiors loop) ())
    (setf (loop-blocks loop) nil)
    (do-blocks (block component)
      (let ((number (block-number block)))
        (dolist (pred (block-pred block))
          (when (<= (block-number pred) number)
            (when (note-loop-head block component)
              (clear-flags component)
              (setf (block-flag block) :good)
              (dolist (succ (block-succ block))
                (find-strange-loop-blocks succ block))
              (find-strange-loop-segments block component))
            (return)))))
    (find-loop-blocks (component-outer-loop component))))


;;; FIND-LOOP-BLOCKS  --  Internal
;;;
;;; This function initializes the block lists for LOOP and the loops
;;; nested within it.  We recursively descend into the loop nesting
;;; and place the blocks in the appropriate loop on the way up.  When
;;; we are done, we scan the blocks looking for exits.  An exit is
;;; always a block that has a successor which doesn't have a LOOP
;;; assigned yet, since the target of the exit must be in a superior
;;; loop.
;;;
;;; We find the blocks by doing a forward walk from the head of the
;;; loop and from any exits of nested loops.  The walks from inferior
;;; loop exits are necessary because the walks from the head terminate
;;; when they encounter a block in an inferior loop.
(defun find-loop-blocks (loop)
  (dolist (sub-loop (loop-inferiors loop))
    (find-loop-blocks sub-loop))

  (find-blocks-from-here (loop-head loop) loop)
  (dolist (sub-loop (loop-inferiors loop))
    (dolist (exit (loop-exits sub-loop))
      (dolist (succ (block-succ exit))
        (find-blocks-from-here succ loop))))

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
;;; LOOP that can be reached by a forward walk from BLOCK.  If BLOCK
;;; is already in a loop or is not dominated by the LOOP-HEAD, then we
;;; return.  Otherwise, we add the block to the BLOCKS for LOOP and
;;; recurse on its successors.
(defun find-blocks-from-here (block loop)
  (when (and (not (block-loop block))
             (dominates-p (loop-head loop) block))
    (setf (block-loop block) loop)
    (shiftf (block-loop-next block) (loop-blocks loop) block)
    (dolist (succ (block-succ block))
      (find-blocks-from-here succ loop))))


;;; NOTE-LOOP-HEAD  --  Internal
;;;
;;; Create a loop structure to describe the loop headed by the block
;;; HEAD.  If there is one already, just return.  If some retreating
;;; edge into the head is from a block which isn't dominated by the
;;; head, then we have the head of a strange loop segment.  We return
;;; true if HEAD is part of a newly discovered strange loop.
(defun note-loop-head (head component)
  (let ((superior (find-superior head (component-outer-loop component))))
    (unless (eq (loop-head superior) head)
      (let ((result (make-loop :head head
                               :kind :natural
                               :superior superior
                               :depth (1+ (loop-depth superior))))
            (number (block-number head)))
        (push result (loop-inferiors superior))
        (dolist (pred (block-pred head))
          (when (<= (block-number pred) number)
            (if (dominates-p head pred)
                (push pred (loop-tail result))
                (setf (loop-kind result) :strange))))
        (eq (loop-kind result) :strange)))))


;;; FIND-SUPERIOR  --  Internal
;;;
;;; Find the loop which would be the superior of a loop headed by
;;; HEAD.  If there is already a loop with that head, then return that
;;; loop.
(defun find-superior (head loop)
  (if (eq (loop-head loop) head)
      loop
      (dolist (inferior (loop-inferiors loop) loop)
        (when (dominates-p (loop-head inferior) head)
          (return (find-superior head inferior))))))


;;; FIND-STRANGE-LOOP-BLOCKS  --  Internal
;;;
;;; Do a graph walk to find the blocks in the strange loop which HEAD
;;; is in.  BLOCK is the block we are currently at and COMPONENT is
;;; the component we are in.  We do a walk forward from block, using
;;; only edges which are not back edges.  We return true if there is a
;;; path from BLOCK to HEAD, false otherwise.  If the BLOCK-FLAG is
;;; true then we return.  We use two non-null values of FLAG to
;;; indicate whether a path from the BLOCK back to HEAD was found.
(defun find-strange-loop-blocks (block head)
  (let ((flag (block-flag block)))
    (cond (flag
           (if (eq flag :good)
               t
               nil))
          (t
           (setf (block-flag block) :bad)
           (unless (dominates-p block head)
             (dolist (succ (block-succ block))
               (when (find-strange-loop-blocks succ head)
                 (setf (block-flag block) :good))))
           (eq (block-flag block) :good)))))

;;; FIND-STRANGE-LOOP-SEGMENTS  --  Internal
;;;
;;; Do a graph walk to find the segments in the strange loop that has
;;; BLOCK in it.  We walk forward, looking only at blocks in the loop
;;; (flagged as :GOOD.)  Each block in the loop that has predecessors
;;; outside of the loop is the head of a segment.  We enter the LOOP
;;; structures in COMPONENT.
(defun find-strange-loop-segments (block component)
  (when (eq (block-flag block) :good)
    (setf (block-flag block) :done)
    (unless (every #'(lambda (x) (member (block-flag x) '(:good :done)))
                   (block-pred block))
      (note-loop-head block component))
    (dolist (succ (block-succ block))
      (find-strange-loop-segments succ component))))
