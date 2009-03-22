;;;; This file implements some optimisations at the IR2 level.
;;;; Currently, the pass converts branches to conditional moves,
;;;; deletes subsequently dead blocks and then reoptimizes jumps.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; We track pred/succ info at the IR2-block level, extrapolating
;;; most of the data from IR1 to initialise.
(declaim (type hash-table *2block-pred* *2block-succ* *label-2block*))
(defvar *2block-pred*)
(defvar *2block-succ*)
(defvar *label-2block*)

(defun initialize-ir2-blocks-flow-info (component)
  (labels ((block-last-2block (block)
             (declare (type cblock block))
             (do ((2block (block-info block)
                    (ir2-block-next 2block)))
                 (nil)
               (let ((next (ir2-block-next 2block)))
                 (when (or (null next)
                           (neq block (ir2-block-block next)))
                   (return 2block)))))
           (link-2blocks (pred succ)
             (declare (type ir2-block pred succ))
             (pushnew pred (gethash succ *2block-pred*))
             (pushnew succ (gethash pred *2block-succ*))))
    (do-blocks (block component :both)
      (let ((succ (block-succ block))
            (last (block-last-2block block)))
        (dolist (succ succ)
          (link-2blocks last (block-info succ)))
        (do ((2block (block-info block)
               (ir2-block-next 2block)))
            ((eq 2block last))
          (link-2blocks 2block (ir2-block-next 2block)))))
    (do-ir2-blocks (2block component)
      (awhen (ir2-block-%label 2block)
        (setf (gethash it *label-2block*) 2block)))))

(defun update-block-succ (2block succ)
  (declare (type ir2-block 2block)
           (type list succ))
  (flet ((blockify (x)
           (etypecase x
             (label (or (gethash x *label-2block*)
                        (error "Unknown label: ~S" x)))
             (ir2-block x))))
    (setf succ (mapcar #'blockify succ)))
  (dolist (old (gethash 2block *2block-succ*))
    (setf (gethash old *2block-pred*)
          (remove 2block (gethash old *2block-pred*))))
  (setf (gethash 2block *2block-succ*) succ)
  (dolist (new succ)
    (pushnew 2block (gethash new *2block-pred*))))

;;;; Conditional move insertion support code
#!-sb-fluid (declaim (inline vop-name))
(defun vop-name (vop &optional default)
  (declare (type vop vop))
  (let ((vop-info (vop-info vop)))
    (if vop-info
        (vop-info-name vop-info)
        default)))

(defun move-value-target (2block)
  (declare (type ir2-block 2block))
  (let* ((first  (or (ir2-block-start-vop 2block)
                     (return-from move-value-target)))
         (second (vop-next first)))
    (when (and (eq (vop-name first) 'move)
               (or (not second)
                   (eq (vop-name second) 'branch)))
      (values (tn-ref-tn (vop-args first))
              (tn-ref-tn (vop-results first))))))

;; A conditional jump may be converted to a conditional move if
;; both branches move a value to the same TN and then continue
;; execution in the same successor block.
;;
;; The label argument is used to return possible value TNs in
;; the right order (first TN if the branch would have been taken,
;; second otherwise)
(defun cmovp (label a b)
  (declare (type label label)
           (type cblock a b))
  (cond ((eq label (ir2-block-%label (block-info a))))
        ((eq label (ir2-block-%label (block-info b)))
         (rotatef a b))
        (t (return-from cmovp)))
  (let ((succ-a (block-succ a))
        (succ-b (block-succ b)))
    (unless (and (singleton-p succ-a)
                 (singleton-p succ-b)
                 (eq (car succ-a) (car succ-b)))
      (return-from cmovp))
    (multiple-value-bind (value-a target)
        (move-value-target (block-info a))
      (multiple-value-bind (value-b targetp)
          (move-value-target (block-info b))
        (and value-a value-b (eq target targetp)
             (values (block-label (car succ-a))
                     target value-a value-b))))))

;; To convert a branch to a conditional move:
;; 1. Convert both possible values to the chosen common representation
;; 2. Execute the conditional VOP
;; 3. Execute the chosen conditional move VOP
;; 4. Convert the result from the common representation
;; 5. Jump to the successor
#!-sb-fluid (declaim (inline convert-one-cmov))
(defun convert-one-cmov (cmove-vop
                         value-if arg-if
                         value-else arg-else
                         target res
                         flags info
                         label
                         vop node 2block)
  (delete-vop vop)
  (flet ((load-and-coerce (dst src)
           (when (and dst (neq dst src))
             (let ((end  (ir2-block-last-vop 2block))
                   (move (template-or-lose 'move)))
               (multiple-value-bind (first last)
                   (funcall (template-emit-function move) node 2block
                            move (reference-tn src nil)
                            (reference-tn dst t))
                 (insert-vop-sequence first last 2block end))))))
    (load-and-coerce arg-if   value-if)
    (load-and-coerce arg-else value-else))
  (emit-template node 2block (template-or-lose cmove-vop)
                 (reference-tn-list (remove nil (list arg-if arg-else))
                                    nil)
                 (reference-tn res t)
                 (list* flags info))
  (emit-move node 2block res target)
  (vop branch node 2block label)
  (update-block-succ 2block (list label)))

;; Since conditional branches are always at the end of blocks,
;; it suffices to look at the last VOP in each block.
(defun maybe-convert-one-cmov (2block)
  (let* ((block (ir2-block-block 2block))
         (succ (block-succ block))
         (a    (first succ))
         (b    (second succ))
         (vop  (or (ir2-block-last-vop 2block)
                   (return-from maybe-convert-one-cmov)))
         (node (vop-node vop)))
    (unless (eq (vop-name vop) 'branch-if)
      (return-from maybe-convert-one-cmov))
    (destructuring-bind (jump-target flags not-p) (vop-codegen-info vop)
      (multiple-value-bind (label target value-a value-b)
          (cmovp jump-target a b)
        (unless label
          (return-from maybe-convert-one-cmov))
        (multiple-value-bind (cmove-vop arg-a arg-b res info)
            (convert-conditional-move-p node target value-a value-b)
          (unless cmove-vop
            (return-from maybe-convert-one-cmov))
          (when not-p
            (rotatef value-a value-b)
            (rotatef arg-a arg-b))
          (convert-one-cmov cmove-vop value-a arg-a
                                      value-b arg-b
                                      target  res
                                      flags info
                            label vop node 2block))))))

(defun convert-cmovs (component)
  (do-ir2-blocks (2block component (values))
    (maybe-convert-one-cmov 2block)))

(defun delete-unused-ir2-blocks (component)
  (declare (type component component))
  (let ((live-2blocks (make-hash-table)))
    (labels ((mark-2block (2block)
               (declare (type ir2-block 2block))
               (when (gethash 2block live-2blocks)
                 (return-from mark-2block))
               (setf (gethash 2block live-2blocks) t)
               (map nil #'mark-2block (gethash 2block *2block-succ*))))
      (mark-2block (block-info (component-head component))))

    (flet ((delete-2block (2block)
             (declare (type ir2-block 2block))
             (do ((vop (ir2-block-start-vop 2block)
                    (vop-next vop)))
                 ((null vop))
               (delete-vop vop))))
      (do-ir2-blocks (2block component (values))
        (unless (gethash 2block live-2blocks)
          (delete-2block 2block))))))

(defun delete-fall-through-jumps (component)
  (flet ((jump-falls-through-p (2block)
           (let* ((last   (or (ir2-block-last-vop 2block)
                              (return-from jump-falls-through-p nil)))
                  (target (first (vop-codegen-info last))))
             (unless (eq (vop-name last) 'branch)
               (return-from jump-falls-through-p nil))
             (do ((2block (ir2-block-next 2block)
                    (ir2-block-next 2block)))
                 ((null 2block) nil)
               (cond ((eq target (ir2-block-%label 2block))
                      (return t))
                     ((ir2-block-start-vop 2block)
                      (return nil)))))))
    ;; Walk the blocks in reverse emission order to catch jumps
    ;; that fall-through only once another jump is deleted
    (let ((last-2block
           (do-ir2-blocks (2block component (aver nil))
             (when (null (ir2-block-next 2block))
               (return 2block)))))
      (do ((2block last-2block
             (ir2-block-prev 2block)))
          ((null 2block)
             (values))
        (when (jump-falls-through-p 2block)
          (delete-vop (ir2-block-last-vop 2block)))))))

(defun ir2-optimize (component)
  (let ((*2block-pred*  (make-hash-table))
        (*2block-succ*  (make-hash-table))
        (*label-2block* (make-hash-table)))
    (initialize-ir2-blocks-flow-info component)

    (convert-cmovs component)
    (delete-unused-ir2-blocks component)
    (delete-fall-through-jumps component))
  (values))
