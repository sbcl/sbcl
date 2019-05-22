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

(in-package "SB-C")

;;; We track pred/succ info at the IR2-block level, extrapolating
;;; most of the data from IR1 to initialise.
(declaim (type hash-table *2block-info*))
;;; For blocks it's a cons with (pred . succ)
;;; For labels it maps to the label block
(defvar *2block-info*)

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
             (pushnew pred (car (ensure-gethash succ *2block-info*
                                                (cons '() '()))))
             (pushnew succ (cdr (ensure-gethash pred *2block-info*
                                                (cons '() '()))))))
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
        (setf (gethash it *2block-info*) 2block)))))

(defun update-block-succ (2block succ)
  (declare (type ir2-block 2block)
           (type list succ))
  (flet ((blockify (x)
           (etypecase x
             (label (or (gethash x *2block-info*)
                        (error "Unknown label: ~S" x)))
             (ir2-block x))))
    (let ((succ (mapcar #'blockify succ))
          (info (gethash 2block *2block-info*)))
      (dolist (old (cdr info))
        (let ((info (gethash old *2block-info*)))
          (setf (car info)
                (remove 2block (car info)))))
      (setf (cdr info) succ)
      (dolist (new succ)
        (pushnew 2block (car (gethash new *2block-info*)))))))

;;;; Conditional move insertion support code
#-sb-fluid (declaim (inline vop-name))
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
(defun convert-one-cmov (cmove-vop
                         value-if arg-if
                         value-else arg-else
                         target res
                         flags info
                         label
                         vop node 2block)
  (let ((prev (vop-prev vop)))
    (delete-vop vop)
    (flet ((reuse-if-eq-arg (value-if vop)
             ;; Most of the time this means:
             ;; if X is already NIL, don't load it again.
             (when (and (eq (vop-name vop) 'if-eq)
                        (constant-tn-p value-if))
               (let* ((args (vop-args vop))
                      (x-tn (tn-ref-tn args))
                      (test (tn-ref-tn (tn-ref-across args))))
                 (when (and (constant-tn-p test)
                            (equal (tn-value value-if)
                                   (tn-value test))
                            (eq (tn-primitive-type x-tn)
                                (tn-primitive-type res)))
                   x-tn))))
           (load-and-coerce (dst src)
             (when (and dst (neq dst src))
               (emit-and-insert-vop node 2block
                                    (template-or-lose 'move)
                                    (reference-tn src nil)
                                    (reference-tn dst t)
                                    (ir2-block-last-vop 2block)))))
      (let ((reuse (reuse-if-eq-arg value-if prev)))
        (if reuse
            (setf arg-if reuse)
            (load-and-coerce arg-if   value-if)))
      (load-and-coerce arg-else value-else))
    (emit-template node 2block (template-or-lose cmove-vop)
                   (reference-tn-list (remove nil (list arg-if arg-else))
                                      nil)
                   (reference-tn res t)
                   (list* flags info))
    (emit-move node 2block res target)
    (vop branch node 2block label)
    (update-block-succ 2block (list label))))

;; Since conditional branches are always at the end of blocks,
;; it suffices to look at the last VOP in each block.
(defun maybe-convert-one-cmov (2block)
  (let ((vop (or (ir2-block-last-vop 2block)
                 (return-from maybe-convert-one-cmov))))
    (unless (eq (vop-name vop) 'branch-if)
      (return-from maybe-convert-one-cmov))
    ;; The test and branch-if may be split between two IR1 blocks
    ;; due to cleanups, can't use bloc-succ of the ir2-block-block
    (let* ((node (vop-node vop))
           (succ (block-succ (node-block node)))
           (a    (first succ))
           (b    (second succ)))

      (destructuring-bind (jump-target not-p flags) (vop-codegen-info vop)
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
                              label vop node 2block)))))))

(defun convert-cmovs (component)
  (do-ir2-blocks (2block component (values))
    (maybe-convert-one-cmov 2block)))

(defun delete-unused-ir2-blocks (component)
  (declare (type component component))
  (let ((live-2blocks (make-hash-table :test #'eq)))
    (labels ((mark-2block (2block)
               (declare (type ir2-block 2block))
               (when (gethash 2block live-2blocks)
                 (return-from mark-2block))
               (setf (gethash 2block live-2blocks) t)
               (map nil #'mark-2block (cdr (gethash 2block *2block-info*)))))
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
               (cond ((ir2-block-%trampoline-label 2block)
                      (return nil))
                     ((eq target (ir2-block-%label 2block))
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

(defun delete-no-op-vops (component)
  (do-ir2-blocks (block component)
    (do ((vop (ir2-block-start-vop block) (vop-next vop)))
        ((null vop))
      (let ((args (vop-args vop))
            (results (vop-results vop)))
       (case (vop-name vop)
         ((move sb-vm::sap-move)
          (let ((x (tn-ref-tn args))
                (y (tn-ref-tn results)))
            (when (location= x y)
              (delete-vop vop)))))))))

;;; Unchain BRANCHes that jump to a BRANCH.
;;; Remove BRANCHes that are jumped over by BRANCH-IF
;;; Should be run after DELETE-NO-OP-VOPS, otherwise the empty moves
;;; will interfere.
(defun ir2-optimize-jumps (component)
  (flet ((start-vop (block)
           (do ((block block (ir2-block-next block)))
               ((null block) nil)
             (let ((vop (ir2-block-start-vop block)))
               (when vop
                 (if (eq (vop-name vop) 'sb-c:note-environment-start)
                     (let ((next (vop-next vop)))
                       (when next
                         (return next)))
                     (return vop))))))
         (next-label (block)
           (do ((block (ir2-block-next block)
                  (ir2-block-next block)))
               ((null block) nil)
             (let ((label (or (ir2-block-%trampoline-label block)
                              (ir2-block-%label block))))
               (cond (label
                      (return label))
                     ((ir2-block-start-vop block)
                      (return nil)))))))
    (let ((label-block-map (make-hash-table :test #'eq)))
      (do-ir2-blocks (block component)
        (setf (gethash (ir2-block-%trampoline-label block) label-block-map)
              block)
        (setf (gethash (ir2-block-%label block) label-block-map)
              block))
      (labels ((unchain-jumps (vop)
                 (let* ((target (first (vop-codegen-info vop)))
                        (target-block (gethash target label-block-map))
                        (target-vop (start-vop target-block)))
                   (when (and target-vop
                              (eq (vop-name target-vop) 'branch)
                              (neq target
                                   (first (vop-codegen-info target-vop))))
                     (setf (first (vop-codegen-info vop))
                           (first (vop-codegen-info target-vop)))
                     ;; What if it jumps to a jump too?
                     (unchain-jumps vop))))
               (remove-jump-overs (branch-if branch)
                 ;; Turn BRANCH-IF L1 BRANCH L2 L1: into BRANCH-IF[NOT] L2
                 (when (and branch
                            (eq (vop-name branch) 'branch))
                   (let* ((branch-if-info (vop-codegen-info branch-if))
                          (branch-if-target (first branch-if-info))
                          (branch-target (first (vop-codegen-info branch)))
                          (next (next-label (vop-block branch))))
                     (when (eq branch-if-target next)
                       (setf (first branch-if-info) branch-target)
                       ;; Reverse the condition
                       (setf (second branch-if-info) (not (second branch-if-info)))
                       (delete-vop branch)))))
               (conditional-p (vop)
                 (let ((info (vop-info vop)))
                   (eq (vop-info-result-types info) :conditional))))
        (do-ir2-blocks (block component)
          (let ((last (ir2-block-last-vop block)))
            (case (and last
                       (vop-name last))
              (branch
                  (unchain-jumps last)
                ;; A block may end up having BRANCH-IF BRANCH after coverting an IF
                (let ((prev (vop-prev last)))
                  (when (and prev
                             (or (eq (vop-name prev) 'branch-if)
                                 (conditional-p prev)))
                    (unchain-jumps prev))))
              (branch-if
               (unchain-jumps last))
              (t
               (when (and last
                          (conditional-p last))
                 (unchain-jumps last))))))
        ;; Need to unchain the jumps before handling jump-overs,
        ;; otherwise the BRANCH over which BRANCH-IF jumps may be a
        ;; target of some other BRANCH
        (do-ir2-blocks (block component)
          (let ((last (ir2-block-last-vop block)))
            (case (and last
                       (vop-name last))
              (branch-if
               (remove-jump-overs last
                                  (start-vop (ir2-block-next block))))
              (branch
                  ;; A block may end up having BRANCH-IF BRANCH after coverting an IF
                  (let ((prev (vop-prev last)))
                    (when (and prev
                               (or (eq (vop-name prev) 'branch-if)
                                   (conditional-p prev)))
                      (remove-jump-overs prev last))))
              (t
               (when (and last
                          (conditional-p last))
                 (remove-jump-overs last
                                    (start-vop (ir2-block-next block))))))))
        (delete-fall-through-jumps component)))))

(defun ir2-optimize (component)
  (let ((*2block-info*  (make-hash-table :test #'eq)))
    (initialize-ir2-blocks-flow-info component)

    (convert-cmovs component)
    (delete-unused-ir2-blocks component))

  (values))
