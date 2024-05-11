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
(defmacro ir2block-predecessors (x) `(car (gethash (the ir2-block ,x) *2block-info*)))
(defmacro ir2block-successors (x) `(cdr (gethash (the ir2-block ,x) *2block-info*)))

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
        (pushnew 2block (ir2block-predecessors new))))))

;;;; Conditional move insertion support code
(defun move-value-target (2block)
  (declare (type ir2-block 2block))
  (let* ((first  (or (ir2-block-start-vop 2block)
                     (return-from move-value-target)))
         (second (vop-next first)))
    (when (and (memq (vop-name first) '(move sb-vm::double-move
                                        sb-vm::single-move))
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
  (let ((a2 (block-info a))
        (b2 (block-info b)))
   (cond ((eq label (ir2-block-%label a2)))
         ((eq label (ir2-block-%label b2))
          (rotatef a b)
          (rotatef a2 b2))
         (t (return-from cmovp)))
   (let ((succ-a (block-succ a))
         (succ-b (block-succ b)))
     (cond ((and (singleton-p succ-a)
                 (singleton-p succ-b)
                 (eq (car succ-a) (car succ-b))
                 (singleton-p (block-pred a))
                 (singleton-p (block-pred b)))
            (multiple-value-bind (value-a target)
                (move-value-target a2)
              (multiple-value-bind (value-b targetp)
                  (move-value-target b2)
                (and value-a value-b (eq target targetp)
                     (values (block-label (car succ-a))
                             target value-a value-b)))))
           ;; A branch jumping over a move.
           ((and (singleton-p succ-a)
                 (singleton-p (block-pred a))
                 (equal (car succ-a)
                        b))
            (multiple-value-bind (value target)
                (move-value-target a2)
              (when value
                (values (block-label b)
                        target value target))))
           ((and (singleton-p succ-b)
                 (singleton-p (block-pred b))
                 (equal (car succ-b)
                        a))
            (multiple-value-bind (value target)
                (move-value-target b2)
              (when value
                (values (block-label a)
                        target target value))))))))

#-x86-64
(defun sb-vm::computable-from-flags-p (res x y flags)
  (declare (ignorable res x y flags))
  nil)

;; To convert a branch to a conditional move:
;; 1. Convert both possible values to the chosen common representation
;; 2. Execute the conditional VOP
;; 3. Execute the chosen conditional move VOP
;; 4. Convert the result from the common representation
;; 5. Jump to the successor
(defun convert-one-cmov (cmove-vop
                         value-if value-else
                         res flags
                         label vop node 2block
                         &aux (prev (vop-prev vop)))
  (delete-vop vop)
  (let ((last (ir2-block-last-vop 2block)))
    (when (and last
               (eq (vop-name last) 'branch))
      (delete-vop last)))
  (cond
    ((and (constant-tn-p value-if)
          (constant-tn-p value-else)
          (sb-vm::computable-from-flags-p
           res (tn-value value-if) (tn-value value-else) flags))
     (emit-template node 2block (template-or-lose 'sb-vm::compute-from-flags)
                    (reference-tn-list (list value-if value-else) nil)
                    (reference-tn res t)
                    (list flags)))
    (t
     (when (and prev
                (eq (vop-name prev) 'if-eq)
                (constant-tn-p value-if))
       ;; Most of the time this means:
       ;; if X is already NIL don't load it again.
       (let* ((args (vop-args prev))
              (x-tn (tn-ref-tn args))
              (test (tn-ref-tn (tn-ref-across args))))
         (when (and (constant-tn-p test)
                    (equal (tn-value value-if)
                           (tn-value test))
                    (eq (tn-primitive-type x-tn)
                        (tn-primitive-type res)))
           (setf value-if x-tn))))
     (emit-template node 2block (template-or-lose cmove-vop)
                    (reference-tn-list (list value-if value-else)
                                       nil)
                    (reference-tn res t)
                    (list flags))))

  (vop branch node 2block label)
  (update-block-succ 2block (list label)))

(defun maybe-convert-one-cmov (vop)
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
        (multiple-value-bind (cmove-vop) (convert-conditional-move-p target)
          (when cmove-vop
            (when not-p
              (rotatef value-a value-b))
            (convert-one-cmov cmove-vop
                              value-a value-b target
                              flags
                              label vop node (vop-block vop))
            t))))))

(defun delete-unused-ir2-blocks (component)
  (declare (type component component))
  (let ((live-2blocks (make-hash-table :test #'eq)))
    ;; The liveness algorithm is a straightforward DFS depending on correctness
    ;; of successor links from any reachable block. Unreached blocks could have junk
    ;; in the successor and predecessor links, but it would nice if that didn't
    ;; happen, as junk makes it hard to understand the IR2 flow graph.
    ;; Mutators should try to keep things tidy.
    (labels ((mark-2block (2block)
               (declare (type ir2-block 2block))
               (when (gethash 2block live-2blocks)
                 (return-from mark-2block))
               (setf (gethash 2block live-2blocks) t)
               (map nil #'mark-2block (cdr (gethash 2block *2block-info*)))))
      (declare (dynamic-extent #'mark-2block))
      (mark-2block (block-info (component-head component))))

    (flet ((delete-2block (2block)
             (declare (type ir2-block 2block))
             (do ((vop (ir2-block-start-vop 2block) (vop-next vop)))
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
              (delete-vop vop)
              ;; Deleting the copy may make it look like that register
              ;; is not used anywhere else and some optimizations,
              ;; like combine-instructions, may incorrectly trigger.
              ;; FIXME: these tn-refs never go away, so things like
              ;; combine-instructions should use a different mechanism
              ;; for checking writes/reads.
              (let ((x-reads (tn-reads x))
                    (x-writes (tn-writes x)))
                (when (tn-reads y)
                  (reference-tn x nil))
                (when (tn-writes y)
                  (reference-tn x t))
                (when x-reads
                  (reference-tn y nil))
                (when x-writes
                  (reference-tn y t)))))))))))

;;; Unchain BRANCHes that jump to a BRANCH.
;;; Remove BRANCHes that are jumped over by BRANCH-IF
;;; Should be run after DELETE-NO-OP-VOPS, otherwise the empty moves
;;; will interfere.
(defun ir2-optimize-jumps (component)
  (flet ((start-vop (block)
           (do ((block block (ir2-block-next block)))
               ((or
                 (null block)
                 (not (singleton-p (ir2block-predecessors block)))) nil)
             (let ((vop (ir2-block-start-vop block)))
               (when vop
                 (if (eq (vop-name vop) 'sb-c:note-environment-start)
                     (let ((next (vop-next vop)))
                       (when next
                         (return next)))
                     (return vop))))))
         (delete-chain (block)
           (do ((block block (ir2-block-next block)))
               ((null block) nil)
             ;; Just pop any block, *2block-info* is only used here
             ;; and it just needs to know the number of predecessors,
             ;; not their identity.
             (pop (ir2block-predecessors block))
             (if (ir2block-predecessors block)
                 (return)
                 (let ((vop (ir2-block-start-vop block)))
                   (when vop
                     (when (eq (vop-name vop) 'sb-c:note-environment-start)
                       (setf vop (vop-next vop)))
                     (when vop
                       (aver (eq (vop-name vop) 'branch))
                       (delete-vop vop)
                       (return t)))))))
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
    (labels ((unchain-jumps (vop)
               ;; Handle any branching vop except a multiway branch
               (setf (first (vop-codegen-info vop))
                     (follow-jumps (first (vop-codegen-info vop)))))
             (follow-jumps (target-label &optional (delete t))
               (declare (type label target-label))
               (let* ((target-block (gethash target-label *2block-info*))
                      (target-vop (start-vop target-block)))
                 (cond ((and target-vop
                             (eq (vop-name target-vop) 'branch)
                             (neq (first (vop-codegen-info target-vop))
                                  target-label))
                        (when delete
                          (setf delete (delete-chain target-block)))
                        (follow-jumps (first (vop-codegen-info target-vop)) delete))
                       (t
                        target-label))))
             (remove-jump-overs (branch-if branch)
               ;; Turn BRANCH-IF #<L1>, BRANCH #<L2>, L1:
               ;; into BRANCH-IF[NOT] L2
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
      ;; Pass 1: conditional | unconditional jump to an unconditional jump
      ;; should take the label of the latter.
      (do-ir2-blocks (block component)
        (let ((last (ir2-block-last-vop block)))
          (when last
            (case (vop-name last)
              (branch
               (unchain-jumps last)
               ;; A block may end up having BRANCH-IF + BRANCH after converting an IF.
               ;; Multiway can't coexist with any other branch preceding or following
               ;; in the block, so we don't have to check for that, just a BRANCH-IF.
               (let ((prev (vop-prev last)))
                 (when (and prev
                            (or (eq (vop-name prev) 'branch-if)
                                (conditional-p prev)))
                   (unchain-jumps prev))))
              (branch-if
               (unchain-jumps last))
              (jump-table
               ;; codegen-info = (targets otherwise ...)
               (let ((info (vop-codegen-info last)))
                 ;; Don't delete the branches when they reach zero
                 ;; predecessors as multiway-branch-if-eq inserts
                 ;; extra jumps which are not in the original ir2
                 ;; and do not correspond to any predecessors.
                 (setf (first info) (map 'vector
                                         (lambda (x)
                                           (if (label-p x)
                                               (follow-jumps x nil)
                                               x))
                                         (first info)))
                 (when (second info)
                   (setf (second info) (follow-jumps (second info) nil)))))
              (t
               (when (conditional-p last)
                 (unchain-jumps last)))))))
      ;; Pass 2
      ;; Need to unchain the jumps before handling jump-overs,
      ;; otherwise the BRANCH over which BRANCH-IF jumps may be a
      ;; target of some other BRANCH
      (do-ir2-blocks (block component)
        (let ((last (ir2-block-last-vop block)))
          (case (and last (vop-name last))
            (branch-if
             (remove-jump-overs last
                                (start-vop (ir2-block-next block))))
            (branch
             ;; A block may end up having BRANCH-IF + BRANCH after coverting an IF
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
      (delete-fall-through-jumps component))))

(defun next-vop (vop)
  (or (vop-next vop)
      (do ((2block (ir2-block-next (vop-block vop))
                   (ir2-block-next 2block)))
          ((null 2block) nil)
        (cond ((or (ir2-block-%trampoline-label 2block)
                   (ir2-block-%label 2block))
               (return))
              ((ir2-block-start-vop 2block)
               (return (ir2-block-start-vop 2block)))))))

(defun prev-vop (vop)
  (or (vop-prev vop)
      (do* ((2block (vop-block vop) prev)
            (prev (ir2-block-prev 2block)
                  (ir2-block-prev 2block)))
           ((null prev) nil)
        (cond ((or (ir2-block-%trampoline-label 2block)
                   (ir2-block-%label 2block))
               (return))
              ((ir2-block-last-vop prev)
               (return (ir2-block-last-vop prev)))))))

(defun vop-arg-list (vop)
  (let ((args (loop for arg = (vop-args vop) then (tn-ref-across arg)
                    while arg
                    collect (tn-ref-tn arg))))
    (if (vop-codegen-info vop)
        (nconc args (vop-codegen-info vop))
        args)))

(defun vop-args-equal (vop1 vop2 &optional reverse)
  (let* ((args1 (vop-arg-list vop1))
         (args2 (vop-arg-list vop2)))
    (equal (if reverse
               (reverse args1)
               args1)
           args2)))

;; cmov conversion needs to know the SCs
(defoptimizer (vop-optimize branch-if select-representations) (branch-if)
  (maybe-convert-one-cmov branch-if)
  nil)

(defun next-start-vop (block)
  (loop thereis (ir2-block-start-vop block)
        while (and (= (length (ir2block-predecessors block)) 1)
                   (setf block (ir2-block-next block)))))

(defun branch-destination (branch &optional (true t))
  (unless (typep (vop-codegen-info branch) '(cons t (cons t)))
    (let ((next (vop-next branch)))
      (if (and next
               (eq (vop-name next) 'branch-if))
          (setf branch next)
          (return-from branch-destination))))
  (destructuring-bind (label not-p &rest rest) (vop-codegen-info branch)
    (declare (ignore rest))
    (if (eq not-p true)
        (if (eq branch (ir2-block-last-vop (vop-block branch)))
            (next-start-vop (ir2-block-next (vop-block branch)))
            (let ((next (next-vop branch)))
              (and (eq (vop-name next) 'branch)
                   (next-start-vop (gethash (car (vop-codegen-info next)) *2block-info*)))))
        (let ((dest (gethash label *2block-info*)))
          (when dest
            (next-start-vop dest))))))

;;; Replace (BOUNDP X) + (BRANCH-IF) + {(SYMBOL-VALUE X) | anything}
;;; by (FAST-SYMBOL-VALUE X) + (UNBOUND-MARKER-P) + BRANCH-IF
;;; and delete SYMBOL-VALUE, with the result of FAST-SYMBOL-VALUE flowing
;;; into the same TN as symbol-value's result.
;;; This is a valid substitution on any of the architectures, but
;;; it requires that UNBOUND-MARKER-P return its result in a flag,
;;; so presently is enabled only for x86-64.
#+x86-64
(defoptimizer (vop-optimize boundp) (vop)
  (let ((sym (tn-ref-tn (vop-args vop)))
        (next (vop-next vop)))
    (unless (and next (eq (vop-name next) 'branch-if))
      (return-from vop-optimize-boundp-optimizer nil))
    ;; Only replace if the BOUNDP=T consequent starts with SYMBOL-VALUE so that
    ;; a bad example such as (IF (BOUNDP '*FOO*) (PRINT 'HI) *FOO*)
    ;; - which has SYMBOL-VALUE as the wrong consequent of the IF - is unaffected.
    (let* ((successors (ir2block-successors (vop-block next)))
           (info (vop-codegen-info next))
           (label-block (gethash (car info) *2block-info*))
           (symbol-value-vop
            (ir2-block-start-vop
             (cond ((eq (second info) nil) label-block) ; not negated test.
                   ;; When negated, the BOUNDP case goes to the "other" successor
                   ;; block, i.e. whichever is not started by the #<label>.
                   ((eq label-block (first successors)) (second successors))
                   (t (first successors))))))
      (when (and symbol-value-vop
                 (or (eq (vop-name symbol-value-vop) 'symbol-value)
                     ;; Expect SYMBOL-GLOBAL-VALUE only for variables of kind :GLOBAL.
                     (and (eq (vop-name symbol-value-vop) 'symbol-global-value)
                          (constant-tn-p sym)
                          (eq (info :variable :kind (tn-value sym)) :global)))
                 (eq sym (tn-ref-tn (vop-args symbol-value-vop)))
                 ;; If the symbol is either a compile-time known symbol,
                 ;; or can only be the same thing for both vops,
                 ;; then we're fine to combine.
                 (or (constant-tn-p sym)
                     (not (tn-writes sym))
                     (not (tn-ref-next (tn-writes sym))))
                 ;; Elide the SYMBOL-VALUE only if there is exactly one way to get there.
                 ;; Technically we could split the IR2 block and peel off the SYMBOL-VALUE,
                 ;; and if coming from the BRANCH-IF, jump to the block that contained
                 ;; everything else except the SYMBOL-VALUE vop.
                 (not (cdr (ir2block-predecessors (vop-block symbol-value-vop)))))
        (let ((replacement (if (eq (vop-name symbol-value-vop) 'symbol-global-value)
                               'fast-symbol-global-value
                               'fast-symbol-value))
              (result-tn (tn-ref-tn (vop-results symbol-value-vop))))
          (emit-and-insert-vop (vop-node vop) (vop-block vop)
                               (template-or-lose replacement)
                               (reference-tn sym nil) (reference-tn result-tn t) next)
          (emit-and-insert-vop (vop-node vop) (vop-block vop)
                               (template-or-lose 'unbound-marker-p)
                               (reference-tn result-tn nil) nil next)
          ;; We need to invert the test since BOUNDP and UNBOUND-MARKER-P have
          ;; the opposite meaning in the language. But by chance, they specify
          ;; opposite flags in the vop, which means that the sense of the
          ;; BRANCH-IF test is automagically correct after substitution.
          ;; Of course, this is machine-dependent.
          (delete-vop vop)
          (delete-vop symbol-value-vop)
          next)))))

;;; Optimize (if x ... nil) to reuse the NIL coming from X.
(defoptimizer (vop-optimize if-eq regalloc) (if-eq)
  (let ((branch-if (vop-next if-eq)))
    (when (and branch-if
               (eq (vop-name branch-if) 'branch-if))
      (let* ((args (vop-args if-eq))
             (x (tn-ref-tn args))
             (y (tn-ref-tn (tn-ref-across args))))
        (flet ((constant-p (x)
                 (or (constant-tn-p x)
                     (type= (tn-type x)
                            (specifier-type 'null)))))
          (when (and
                 (not (member (sb-name (sc-sb (tn-sc x)))
                              #+(or x86 x86-64)
                              '(sb-vm::stack)
                              #-(or x86 x86-64)
                              '(sb-vm::control-stack sb-vm::non-descriptor-stack)))
                 (constant-p y))
            (let ((move (branch-destination branch-if)))
              (when (and move
                         (eq (vop-name move) 'move)
                         (singleton-p (ir2block-predecessors (vop-block move))))
                (let* ((args (vop-args move))
                       (from (tn-ref-tn args)))
                  (when (and (constant-p from)
                             (eq (tn-leaf y)
                                 (tn-leaf from))
                             ;; It might not make sense if there are NULL-TN or ZERO-TN.
                             #-(or x86 x86-64)
                             (or (not (or (type= (tn-type from)
                                                 (specifier-type 'null))
                                          #+arm64
                                          (eql (constant-value (tn-leaf from)) 0)))
                                 (location= x y)))
                    (change-tn-ref-tn args x)
                    (setf (tn-ref-load-tn args) nil)))))))
        nil))))

(defun component-remove-constant (constant constants)
  (let ((index (position constant constants)))
    (loop for i from index below (1- (length constants))
          for next = (aref constants (1+ i))
          do (setf (aref constants i) next)
             (cond ((and (constant-p next)
                         (constant-info next))
                    (decf (tn-offset (constant-info next))))
                   ((typep next '(cons t (cons t (cons t null))))
                    (decf (tn-offset (third next))))))
    (decf (fill-pointer constants))))

;;; Optimize (svref #(constant array) safe-index) into accessing the code constants directly,
;;; saving on one memory indirection.
;;; TODO: Can optimize any array
#+(and x86-64 (or)) ;; Performance benefits are unclear, while the vectors can't be shared across different code objects.
(defoptimizer (vop-optimize (sb-vm::data-vector-ref-with-offset/simple-vector
                             sb-vm::data-vector-ref-with-offset/simple-array-fixnum))
    (vop)
  (let* ((args (vop-args vop))
         (array (tn-ref-tn args))
         (index (tn-ref-tn (tn-ref-across args)))
         (constants (ir2-component-constants (component-info *component-being-compiled*)))
         (first-constant))
    (when (and (eq (tn-kind array) :constant)
               (tn-leaf array)
               (not (tn-ref-next (tn-reads array))))
      (component-remove-constant (tn-leaf array) constants)
      (setf (tn-offset array) (length constants))
      (loop for x across (tn-value array)
            for constant = (setf first-constant (make-constant x))
            then (make-constant x)
            do (vector-push-extend constant constants))
      (setf (tn-leaf array) first-constant
            (constant-info first-constant) array)
      (prog1
          (emit-and-insert-vop (vop-node vop)
                           (vop-block vop)
                           (template-or-lose 'sb-vm::data-vector-ref-with-offset/constant-simple-vector)
                           (reference-tn-list (list array index) nil)
                           (reference-tn (tn-ref-tn (vop-results vop)) t)
                           vop
                           (vop-codegen-info vop))
        (delete-vop vop)))))

#+(or arm64 x86-64)
(defoptimizer (vop-optimize #+arm64
                            (sb-vm::data-vector-ref/simple-bit-vector-c
                             sb-vm::data-vector-ref/simple-bit-vector)
                            #+x86-64
                            (sb-vm::data-vector-ref-with-offset/simple-bit-vector-c
                             sb-vm::data-vector-ref-with-offset/simple-bit-vector))
    (vop)
  (let* ((next (next-vop vop))
         (branch (and next
                      (next-vop next)))
         plusp)
    (when (and branch
               (or
                (eq (vop-name next) #+x86-64 'sb-vm::fast-if-eq-fixnum/c
                                    #+arm64 'sb-vm::fast-if-eq-integer/c)
                (and (eq (vop-name next)
                         #-arm64 'sb-vm::fast-if->-c/fixnum
                         #+arm64 'sb-vm::fast-if->-integer/c)
                     (eql (car (vop-codegen-info next)) 0)
                     (setf plusp t)))
               (eq (vop-name branch)
                   'branch-if))
      (let* ((result (tn-ref-tn (vop-results vop)))
             (value (if plusp
                        1
                        (car (vop-codegen-info next)))))
        (when (and (tn-reads result)
                   (not (tn-ref-next (tn-reads result)))
                   (eq result (tn-ref-tn (vop-args next))))
          (check-type value bit)
          (let* ((template (template-or-lose #+arm64
                                             (if (eq (vop-name vop) 'sb-vm::data-vector-ref/simple-bit-vector)
                                                 'sb-vm::data-vector-ref/simple-bit-vector-eq
                                                 'sb-vm::data-vector-ref/simple-bit-vector-c-eq)
                                             #+x86-64
                                             (if (eq (vop-name vop) 'sb-vm::data-vector-ref-with-offset/simple-bit-vector)
                                                 'sb-vm::data-vector-ref-with-offset/simple-bit-vector-eq
                                                 'sb-vm::data-vector-ref-with-offset/simple-bit-vector-c-eq)))
                 (flags (make-conditional-flags (cdr (template-result-types template)))))
            (prog1
                (emit-and-insert-vop (vop-node vop)
                                     (vop-block vop)
                                     template
                                     (reference-tn-refs (vop-args vop) nil)
                                     nil
                                     vop
                                     (append (vop-codegen-info vop) (list flags)))
              (setf (third (vop-codegen-info branch))
                    flags)
              (when (eq value 1)
                (setf (second (vop-codegen-info branch))
                      (not (second (vop-codegen-info branch)))))
              (delete-vop vop)
              (delete-vop next))))))))

(when-vop-existsp (:named sb-vm::<-integer-fixnum)
  ;; The <-integer-fixnum VOPs already perform dispatch for fixnum/bignum,
  ;; and load the header byte.
  ;; Replace the preceding INTEGERP VOP with an appropriate
  ;; <-integer-fixnum, which checks for INTEGER.
  (defoptimizer (vop-optimize integerp) (vop)
    (destructuring-bind (target not-p) (vop-codegen-info vop)
      (let ((target-block (gethash target *2block-info*)))
        (when target-block
          (let* ((next (if (eq vop (ir2-block-last-vop (vop-block vop)))
                           (ir2-block-next (vop-block vop))
                           (let ((next (next-vop vop)))
                             (and (eq (vop-name next) 'branch)
                                  (gethash (car (vop-codegen-info next)) *2block-info*)))))
                 (not-target-block (if not-p
                                       target-block
                                       next))
                 (target-block (if not-p
                                   next
                                   target-block))
                 (cmp (ir2-block-start-vop target-block)))
            (when (and cmp
                       (singleton-p (ir2block-predecessors target-block)))
              (let ((integer (vop-args vop))
                    (args (vop-args cmp))
                    tn-refs)
                (when (case (vop-name cmp)
                        ((sb-vm::>-integer-fixnum sb-vm::<-integer-fixnum)
                         (when (eq (tn-ref-tn args) (tn-ref-tn integer))
                           (setf tn-refs (list integer (tn-ref-across args)))))
                        ((sb-vm::>-fixnum-integer sb-vm::<-fixnum-integer)
                         (when (eq (tn-ref-tn (tn-ref-across args))
                                   (tn-ref-tn integer))
                           (setf tn-refs (list args integer)))))
                  (destructuring-bind (cmp-target cmp-not-p) (vop-codegen-info cmp)
                    (let* ((cmp-next-block (ir2-block-next (vop-block cmp)))
                           (cmp-target-block (gethash cmp-target *2block-info*)))
                      (flet ((invert ()
                               (template-or-lose
                                (case (vop-name cmp)
                                  (sb-vm::>-integer-fixnum 'sb-vm::<=-integer-fixnum)
                                  (sb-vm::<-integer-fixnum 'sb-vm::>=-integer-fixnum)
                                  (sb-vm::>-fixnum-integer 'sb-vm::<=-fixnum-integer)
                                  (sb-vm::<-fixnum-integer 'sb-vm::>=-fixnum-integer)))))
                        (multiple-value-bind (new-vop new-target new-not-p)
                            (cond ((and not-p
                                        (eq not-target-block cmp-next-block))
                                   (if cmp-not-p
                                       (values (invert) cmp-target nil)
                                       (values (vop-info cmp) cmp-target nil)))
                                  ((and not-p
                                        (eq not-target-block cmp-target-block))
                                   (if cmp-not-p
                                       (values (vop-info cmp) cmp-target t)
                                       (values (invert) cmp-target t)))
                                  ((and (not not-p)
                                        (eq not-target-block cmp-target-block))
                                   (if cmp-not-p
                                       (values (vop-info cmp) (ir2-block-%label target-block) nil)
                                       (values (invert) (ir2-block-%label target-block) nil)))
                                  ((and (not not-p)
                                        (eq not-target-block cmp-next-block))
                                   (if cmp-not-p
                                       (values (invert) cmp-target nil)
                                       (values (vop-info cmp) cmp-target nil)))
                                  (t
                                   (return-from vop-optimize-integerp-optimizer)))
                          (prog1
                              (emit-and-insert-vop (vop-node vop) (vop-block vop)
                                                   new-vop
                                                   (reference-tn-ref-list tn-refs nil)
                                                   nil vop
                                                   (list new-target new-not-p))
                            (delete-vop vop)
                            (delete-vop cmp))))))))))))
      nil)))

;;; No need to reset the stack pointer just before returning.
(defoptimizer (vop-optimize reset-stack-pointer) (vop)
  (loop for next = (next-vop vop) then (next-vop next)
        do (cond ((not next)
                  (return))
                 ((eq (vop-name next) 'move))
                 ((memq (vop-name next) '(return-single return known-return
                                          tail-call tail-call-named
                                          static-tail-call-named))
                  (delete-vop vop)
                  ;; Delete the VOPs that save the stack pointer too.
                  (let ((tn (tn-ref-tn (vop-args vop))))
                    (unless (tn-reads tn)
                      (do ((ref (tn-writes tn) (tn-ref-next ref)))
                          ((null ref))
                        (aver (eq (vop-name (tn-ref-vop ref))
                                  'current-stack-pointer))
                        (delete-vop (tn-ref-vop ref)))))
                  (return))
                 (t
                  (return)))))

;;; Load the WIDETAG once for a series of type tests.
(when-vop-existsp (:named sb-vm::load-other-pointer-widetag)
  (defoptimizer (vop-optimize %other-pointer-subtype-p) (vop)
    (let (vops
          stop
          null
          (value (tn-ref-tn (vop-args vop)))
          zero-extend)
      (labels ((good-vop-p (vop)
                 (and (singleton-p (ir2block-predecessors (vop-block vop)))
                      (or (getf sb-vm::*other-pointer-type-vops* (vop-name vop))
                          (eq (vop-name vop) '%other-pointer-subtype-p)
                          (and (eq (vop-name vop) '%other-pointer-widetag)
                               (setf zero-extend t)))
                      (eq (tn-ref-tn (vop-args vop)) value)))
               (chain (vop &optional (collect t))
                 (let ((next (branch-destination vop nil)))
                   (cond ((and next
                               (cond ((eq (vop-name vop) 'symbolp)
                                      (and (not null)
                                           (setf null (branch-destination vop))))
                                     ((eq (vop-name vop) 'non-null-symbol-p)
                                      (and (not null)
                                           (setf null 'stop)))
                                     (t t)))
                          (when collect
                            (push vop vops))
                          (cond ((good-vop-p next)
                                 (chain next))
                                ((not stop)
                                 (setf stop (vop-block next)))))
                         ((not stop)
                          (setf stop (vop-block vop)))))
                 (let ((true (branch-destination vop)))
                   (when (and true
                              (good-vop-p true))
                     (push true vops)
                     (chain true nil))))
               (ir2-block-label (block)
                 (or (ir2-block-%label block)
                     (setf (ir2-block-%label block) (gen-label)))))
        (chain vop)
        (when (> (length vops) 1)
          (let ((widetag (make-representation-tn (primitive-type-or-lose 'sb-vm::unsigned-byte-64)
                                                 sb-vm:unsigned-reg-sc-number))
                (block (vop-block vop)))
            (setf (tn-type value)
                  (tn-ref-type (vop-args vop)))
            (emit-and-insert-vop (vop-node vop)
                                 block
                                 (template-or-lose 'sb-vm::load-other-pointer-widetag)
                                 (reference-tn value nil)
                                 (reference-tn widetag t)
                                 vop
                                 (list (ir2-block-label stop)
                                       (and null
                                            (if (eq null 'stop)
                                                (ir2-block-label stop)
                                                (ir2-block-label (vop-block null))))
                                       #+x86-64 zero-extend))
            (update-block-succ block
                               (cons stop
                                     (ir2block-successors block)))

            (let ((test-vop (template-or-lose 'sb-vm::test-widetag)))
              (loop for vop in vops
                    for info = (vop-codegen-info vop)
                    for tags = (if (eq (vop-name vop) '%other-pointer-subtype-p)
                                   (third info)
                                   (getf sb-vm::*other-pointer-type-vops* (vop-name vop)))
                    do
                    (if (eq (vop-name vop) '%other-pointer-widetag)
                        (emit-and-insert-vop (vop-node vop)
                                             (vop-block vop)
                                             (template-or-lose 'move)
                                             (reference-tn widetag nil)
                                             (reference-tn (tn-ref-tn (vop-results vop)) t)
                                             vop)
                        (let ((next (vop-next vop)))
                          (when (and next
                                     (eq (vop-name next) 'branch-if))
                            (setf info (vop-codegen-info next))
                            (delete-vop next))
                          (when (and (eql (car tags) sb-vm:simple-array-widetag)
                                     (csubtypep (tn-ref-type (vop-args vop)) (specifier-type 'string)))
                            (pop tags))
                          (emit-and-insert-vop (vop-node vop)
                                               (vop-block vop)
                                               test-vop
                                               (reference-tn widetag nil)
                                               nil
                                               vop
                                               (list (first info) (second info) tags))))
                    (delete-vop vop)))))))
    nil)

  (loop for (vop) on sb-vm::*other-pointer-type-vops* by #'cddr
        do
        (set-vop-optimizer (template-or-lose vop)
                           #'vop-optimize-%other-pointer-subtype-p-optimizer)))

(when-vop-existsp (:named sb-vm::load-instance-layout)
  (defoptimizer (vop-optimize structure-typep) (vop)
    (let (vops
          stop
          (value (tn-ref-tn (vop-args vop))))
      (labels ((good-vop-p (vop)
                 (and (singleton-p (ir2block-predecessors (vop-block vop)))
                      (eq (vop-name vop) 'structure-typep)
                      (eq (tn-ref-tn (vop-args vop)) value)))
               (chain (vop &optional (collect t))
                 (let ((next (branch-destination vop nil)))
                   (cond (next
                          (when collect
                            (push vop vops))
                          (cond ((good-vop-p next)
                                 (chain next))
                                ((not stop)
                                 (setf stop (vop-block next)))))
                         ((not stop)
                          (setf stop (vop-block vop)))))
                 (let ((true (branch-destination vop)))
                   (when (and true
                              (good-vop-p true))
                     (push true vops)
                     (chain true nil))))
               (ir2-block-label (block)
                 (or (ir2-block-%label block)
                     (setf (ir2-block-%label block) (gen-label)))))
        (chain vop)
        (when (> (length vops) 1)
          (let ((layout (make-representation-tn *backend-t-primitive-type*
                                                sb-vm:descriptor-reg-sc-number))
                (block (vop-block vop)))
            (setf (tn-type value)
                  (tn-ref-type (vop-args vop)))
            (emit-and-insert-vop (vop-node vop)
                                 block
                                 (template-or-lose 'sb-vm::load-instance-layout)
                                 (reference-tn value nil)
                                 (reference-tn layout t)
                                 vop
                                 (list (ir2-block-label stop)))
            (update-block-succ block
                               (cons stop
                                     (ir2block-successors block)))
            (let ((test-vop (template-or-lose 'sb-vm::structure-typep*)))
              (loop for vop in vops
                    for info = (vop-codegen-info vop)
                    do
                    (emit-and-insert-vop (vop-node vop)
                                         (vop-block vop)
                                         test-vop
                                         (reference-tn layout nil)
                                         nil
                                         vop
                                         info)
                    (delete-vop vop)))))))
    nil))

(defmacro vop-bind (args results vop &body body)
  (flet ((gen (accessor operands)
           (loop for op in operands
                 for name = (gensym "TN-REF")
                 and tn-ref = `(,accessor ,vop) then `(tn-ref-across ,name)
                 until (eq op :info)
                 collect `(,name ,tn-ref)
                 collect `(,op (tn-ref-tn ,name))))
         (bind-info (body)
           (let ((info (cdr (member :info args))))
             (if info
                 `((loop named #:vop-bind
                         with ,info = (vop-codegen-info ,vop)
                         return (progn ,@body)))
                 body))))
   `(let* (,@(gen 'vop-args args)
           ,@(gen 'vop-results results))
      ,@(bind-info body))))

(defun tn-reader (tn &key single-writer
                          single-reader)
  (let ((reads (tn-reads tn))
        (writes (tn-writes tn)))
    (and reads writes
         (not (and single-reader
                   (tn-ref-next reads)))
         (not (and single-writer
                   (tn-ref-next writes)))
         (tn-ref-vop reads))))

(defun tn-single-writer-p (tn)
  (let ((writes (tn-writes tn)))
    (and writes
         (not (tn-ref-next writes)))))

(defoptimizer (vop-optimize sb-vm::move-from-word/fixnum)
    (vop)
  (vop-bind (in) (out) vop
    (when (tn-single-writer-p in)
      (let ((to (tn-reader out :single-writer t)))
        (when (and to
                   (eq (vop-name to) 'sb-vm::move-to-word/fixnum))
          (vop-bind (in2) (out2) to
            (when (eq out in2)
              (emit-and-insert-vop
               (vop-node to) (vop-block to)
               (template-or-lose 'sb-vm::word-move)
               (reference-tn in nil)
               (reference-tn out2 t)
               to)
              (delete-vop to)
              nil)))))))

(when-vop-existsp (:named sb-vm::return-values-list)
  (defoptimizer (vop-optimize values-list)
      (vop)
    (let ((return (next-vop-is vop '(return-multiple))))
      (when return
        (vop-bind (list) () vop
          (emit-and-insert-vop (vop-node return)
                               (vop-block return)
                               (template-or-lose 'sb-vm::return-values-list)
                               (reference-tn list nil)
                               nil
                               return)
          (delete-vop vop)
          (delete-vop return))
        nil))))

;; Try to combine consecutive uses of %INSTANCE-SET. This can't be
;; done prior to selecting representations because
;; SELECT-REPRESENTATIONS might insert some things like
;; MOVE-FROM-DOUBLE which makes the "consecutive" vops no longer
;; consecutive.
;; It seems like this should also supplant the #+arm64 hack in GENERATE-CODE.
(when-vop-existsp (:named sb-vm::instance-set-multiple)
  (defoptimizer (vop-optimize sb-vm::instance-index-set select-representations) (vop)
    (let ((instance (tn-ref-tn (vop-args vop)))
          (this vop)
          (pairs))
      (loop
       (let ((index (tn-ref-tn (tn-ref-across (vop-args this)))))
         (unless (constant-tn-p index) (return))
         (push (cons (tn-value index) (tn-ref-tn (sb-vm::vop-nth-arg 2 this)))
               pairs))
       (let ((next (vop-next this)))
         (unless (and next
                      (eq (vop-name next) 'sb-vm::instance-index-set)
                      (eq (tn-ref-tn (vop-args next)) instance))
           (return))
         (setq this next)))
      (when (cdr pairs)                 ; if at least 2
        (setq pairs (nreverse pairs))
        (let ((new (emit-and-insert-vop
                    (vop-node vop) (vop-block vop)
                    (template-or-lose 'sb-vm::instance-set-multiple)
                    (reference-tn-list (cons instance (mapcar #'cdr pairs)) nil)
                    nil vop (list (mapcar #'car pairs)))))
          (loop (let ((next (vop-next vop)))
                  (delete-vop vop)
                  (pop pairs)
                  (setq vop next))
                (unless pairs (return)))
          new)))))

(defun vop-label (vop)
  (let ((block (vop-block vop)))
    (or (ir2-block-%label block)
        (setf (ir2-block-%label block) (gen-label)))))

(defun next-vop-label (vop)
  (let* ((block (vop-block vop))
         (next (ir2-block-next block)))
    (when (eq (ir2-block-last-vop block) vop)
      (or (ir2-block-%label next)
          (setf (ir2-block-%label next) (gen-label))))))

(when-vop-existsp (:named sb-vm::signed-byte-64-p-move-to-word)
  (flet ((opt (vop new-vop &optional not-vop)
           (let ((dest (branch-destination vop))
                 (vop2 (branch-destination vop nil)))
             (vop-bind (in) () vop
               (when (and dest
                          (singleton-p (ir2block-predecessors (vop-block dest)))
                          (eq (vop-name dest) 'sb-vm::move-to-word/integer))
                 (vop-bind (in2) () dest
                   (when (eq in in2)
                     (cond ((and vop2
                                 (eq (vop-name vop2) not-vop)
                                 (singleton-p (ir2block-predecessors (vop-block dest)))
                                 (let ((dest2 (branch-destination vop2)))
                                   (when (and dest2
                                              (singleton-p (ir2block-predecessors (vop-block dest2)))
                                              (eq (vop-name dest2) 'sb-vm::move-to-word/integer))
                                     (vop-bind (in21) () vop2
                                       (vop-bind (in22) () dest2
                                         (when (and (eq in21 in)
                                                    (eq in22 in))
                                           (emit-and-insert-vop
                                            (vop-node vop) (vop-block vop)
                                            (template-or-lose 'sb-vm::un/signed-byte-64-p-move-to-word)
                                            (reference-tn-refs (vop-args vop) nil)
                                            (reference-tn-ref-list (list (vop-results dest)
                                                                         (vop-results dest2))
                                                                   t)
                                            vop
                                            (append (vop-codegen-info vop)
                                                    (vop-codegen-info vop2)
                                                    (list (vop-label (branch-destination vop2 nil)))))
                                           (delete-vop vop)
                                           (delete-vop vop2)
                                           (delete-vop dest)
                                           (delete-vop dest2)
                                           t)))))))
                           (t
                            (emit-and-insert-vop
                             (vop-node vop) (vop-block vop)
                             (template-or-lose new-vop)
                             (reference-tn-refs (vop-args vop) nil)
                             (reference-tn-refs (vop-results dest) t)
                             vop (vop-codegen-info vop))
                            (delete-vop vop)
                            (delete-vop dest))))))))
           nil))
    (defoptimizer (vop-optimize signed-byte-64-p select-representations) (vop)
      (opt vop 'sb-vm::signed-byte-64-p-move-to-word
           'unsigned-byte-64-p))
    (defoptimizer (vop-optimize unsigned-byte-64-p select-representations) (vop)
      (opt vop 'sb-vm::unsigned-byte-64-p-move-to-word))))

(when-vop-existsp (:named sb-vm::rebind)
  (defoptimizer (vop-optimize fast-symbol-value) (vop)
    (let ((bind (next-vop-is vop '(bind))))
      (when bind
        (vop-bind (symbol) (symbol-value) vop
          (vop-bind (bind-value :info bind-symbol) () bind
            (when (and (eq symbol-value bind-value)
                       (constant-tn-p symbol)
                       (eq bind-symbol (tn-value symbol)))
              (emit-and-insert-vop (vop-node bind)
                                   (vop-block bind)
                                   (template-or-lose 'sb-vm::rebind)
                                   nil
                                   nil
                                   bind
                                   (list bind-symbol))
              (delete-vop vop)
              (delete-vop bind))))
        nil))))

(when-vop-existsp (:named sb-vm::bind-n)
  (defoptimizer (vop-optimize bind) (vop)
    (let ((binds (loop with next = vop
                       do (setf next (next-vop-is next '(bind)))
                       while next
                       collect next)))
      (when binds
        (push vop binds)
        (let (symbols
              values)
          (loop for bind in binds
                do
                (vop-bind (value :info symbol) () bind
                  (push symbol symbols)
                  (push value values)))
          (setf symbols (nreverse symbols)
                values (nreverse values))
          (prog1 (emit-and-insert-vop (vop-node vop)
                                      (vop-block vop)
                                      (template-or-lose 'sb-vm::bind-n)
                                      (reference-tn-list values nil)
                                      nil
                                      vop
                                      (list symbols))
            (mapc #'delete-vop binds)))))))

(defun very-temporary-p (tn)
  (let ((writes (tn-writes tn))
        (reads (tn-reads tn)))
    (and writes reads (not (tn-ref-next writes)) (not (tn-ref-next reads)))))

(defun next-vop-is (vop names)
  (let ((next (next-vop vop)))
    (and next
         (let ((name (vop-name next)))
           (if (atom names) (eq name names) (memq name names)))
         next)))

(defun previous-vop-is (vop names)
  (let ((prev (vop-prev vop)))
    (and prev
         (let ((name (vop-name prev)))
           (if (atom names) (eq name names) (memq name names)))
         prev)))

;;; Possibly replace HOWMANY vops starting at FIRST with a vop named REPLACEMENT.
;;; Each deleted vop must have exactly one argument and one result.
;;; - There must be no way to begin execution in the middle of the pattern.
;;; - The argument to each successive vop must be the result of its predecessor.
;;; - There must be no other refs to TNs which are to be deleted.
;;;
;;; On the x86 backends this can potentially form the basis of an optimization
;;; which folds together a memory read from {CAR, CDR, INSTANCE-REF} etc with
;;; a following integer comparison and/or fixnum tag test as long as the base object
;;; is in a register (so we get the load as part of the instruction).
(defun replace-vops (howmany first replacement)
  (flet ((can-replace (vop &aux (result (tn-ref-tn (vop-results vop)))
                                (next (next-vop vop)))
           (and (very-temporary-p result)
                (eq (tn-ref-tn (vop-args next)) result))))
    (let ((last (ecase howmany
                  (2 (when (can-replace first)
                       (next-vop first)))
                  (3 (when (and (can-replace first)
                                (can-replace (next-vop first)))
                       (next-vop (next-vop first)))))))
      (when last
        (let ((new (emit-and-insert-vop (vop-node first)
                                        (vop-block first)
                                        (template-or-lose replacement)
                                        (reference-tn (tn-ref-tn (vop-args first)) nil)
                                        (reference-tn (tn-ref-tn (vop-results last)) t)
                                        first))) ; insert before this
          (dotimes (i (1- howmany)) ; if 3, replace 2 "NEXT"'s and then first, etc
            (delete-vop (next-vop first)))
          (delete-vop first)
          new))))) ; return suitable value for RUN-VOP-OPTIMIZERS

(defun run-vop-optimizers (component &optional stage (without-stage (not stage)))
  (do-ir2-blocks (block component)
    (let ((vop (ir2-block-start-vop block)))
      (loop while vop
            do
            ;; Avoid following the NEXT pointer of a deleted vop, which despite
            ;; possibly being accidentally correct, is dubious.
            ;; Optimizer must return NIL or a VOP whence to resume scanning.
            (setq vop (or (awhen (vop-info-optimizer (vop-info vop))
                            (if (consp it)
                                (and (eq (cdr it) stage)
                                     (funcall (car it) vop))
                                (and without-stage
                                     (funcall it vop))))
                          (vop-next vop)))))))

;;; These are the acceptable vops in a sequence that can be brought together
;;; under one pseudo-atomic invocation. In general any vop that does not
;;; allocate is OK, but I'd rather be restrictive than permissive here.
(defglobal *vops-allowed-within-pseudo-atomic*
    '(set-slot %raw-instance-set/word %raw-instance-set/signed-word %raw-instance-set/single
      %raw-instance-set/double %raw-instance-set/complex-single %raw-instance-set/complex-double
      move move-operand make-unbound-marker
      sb-vm::move-from-word/fixnum sb-vm::move-to-word/fixnum
      sb-vm::move-from-fixnum+1 sb-vm::move-from-fixnum-1))

;;; Return list of vops between and including VOP and LAST
;;; without regard to IR2 block boundaries (as long as there is
;;; no branching control flow in the specified range)
(defun collect-vops-between (vop last)
  (collect ((result))
    (loop (result vop)
          (if (eq vop last) (return))
          (setq vop
                (or (vop-next vop)
                    ;; IR2 blocks were split. Assert that the flow is straight-line
                    (let ((successors (ir2block-successors (vop-block vop))))
                      (aver (singleton-p successors))
                      (let* ((successor (car successors))
                             (predecessors (ir2block-predecessors successor)))
                        (aver (eq (car predecessors) (vop-block vop)))
                        (aver (not (cdr predecessors)))
                        (ir2-block-start-vop successor))))))
    (result)))

;;; This should be among the final IR2 optimizer passes so that no new vops get
;;; inserted that would change the decision about whether to extend
;;; the scope of pseudo-atomic to cover them.
(defun attempt-pseudo-atomic-store-bunching (component)
  (labels
      ((terminate-inits (last)
         ;; The allocator has to recognize :PSEUDO-ATOMIC in its codegen info
         ;; but the slot setters don't all have to be updated to understand how to
         ;; terminate the pseudo-atomic sequence. It's a separate vop to do that.
         (emit-and-insert-vop (vop-node last) (vop-block last)
                              (template-or-lose 'end-pseudo-atomic)
                              nil nil (vop-next last)))
       (process-closure-inits (vop)
         (let* ((result-ref (vop-results vop))
                (closure (tn-ref-tn result-ref))
                (last-init))
           (do ((init (vop-next vop) (vop-next init)))
               ((or (not init) (neq (vop-name init) 'closure-init)))
             ;; IR2-CONVERT-ENCLOSE can output more than one MAKE-CLOSURE
             ;; and then some CLOSURE-INITs.  This happens with mutually-referential
             ;; closures. Sadly we can't optimize that to move the inits underneath
             ;; the allocator's pseudo-atomic.So just beware of the pattern
             ;;   MAKE => c1 / INIT c1 / MAKE => c2 / INIT c2 / INIT c1
             (unless (eq closure (tn-ref-tn (vop-args init)))
               (return))
             (setf (vop-codegen-info init) (append (vop-codegen-info init) '(:pseudo-atomic))
                   last-init init))
           (when last-init
             (setf (vop-codegen-info vop)
                   (append (vop-codegen-info vop) '(:pseudo-atomic)))
             (terminate-inits last-init))))
       (process-general-inits (first last &aux last-init)
         (aver (neq first last))
         (dolist (init (cdr (collect-vops-between first last)))
           (cond
             ((eq (vop-name init) 'set-slot)
              (setf (vop-codegen-info init) (append (vop-codegen-info init) '(:pseudo-atomic))
                    last-init init))
             ((not (member (vop-name init) *vops-allowed-within-pseudo-atomic*))
              (return))))
         (when last-init
           (setf (vop-codegen-info first)
                 (append (butlast (vop-codegen-info first)) '(:pseudo-atomic)))
           (terminate-inits last-init))))
  (do-ir2-blocks (block component)
    (let ((vop (ir2-block-start-vop block)))
      ;; This needs to avoid processing an allocator more than once.
      ;; Here's how it could happen:
      ;; ir2 block 1 | whatever
      ;;             | allocate \
      ;;             | set-slot | -- to be bunched
      ;;             | set-slot |
      ;; ir2 block 2 | set-slot /
      ;;             | whatever
      ;;             | ..
      ;;             | allocate
      ;;             | set-slot
      ;;
      ;; Depending on where the loop continues iterating after performing the bunching
      ;; operation, we might see the second ALLOCATE twice. Consider if we pick up
      ;; at the "whatever" vop after the third SET-SLOT. The we process the next
      ;; allocate and set-slot.  When that's done, the inner loop finishes and we start
      ;; on the outer loop (in DO-IR2-BLOCKS) which takes BLOCK-NEXT of block 1 as the
      ;; starting point. So then we see block 2 again, and the 2nd allocate again.
      (loop (unless vop (return))
            (case (vop-name vop)
              (make-closure
               (let ((dx (third (vop-codegen-info vop)))
                     (already-done (eq (fourth (vop-codegen-info vop)) :pseudo-atomic)))
                 (unless (or dx already-done)
                   (process-closure-inits vop))))
              ((fixed-alloc var-alloc)
               (let ((last (car (last (vop-codegen-info vop)))))
                 (when (vop-p last)
                   (process-general-inits vop last)))))
            ;; Probably could skip over some vops if any were already processed
            ;; but it's clearer to just do the naive one-at-a-time skip
            ;; which avoids confusion when IR2 blocks were split
            ;; and the processing either did or didn't modify anything.
            ;; As long as we don't re-process an allocation vop, all it well.
            (setq vop (vop-next vop)))))))

;;; If a constant is already loaded into a register use that register.
;;; Also track stack alignment by consecutive stack-allocating VOPs.
(defun optimize-constant-loads (component)
  (let* ((register-sb (sb-or-lose 'sb-vm::registers))
         (loaded-constants
           (make-array (sb-size register-sb)
                       :initial-element nil))
         (aligned-stack))
    (do-ir2-blocks (block component)
      (fill loaded-constants nil)
      (setf aligned-stack nil)
      (do ((vop (ir2-block-start-vop block) (vop-next vop)))
          ((null vop))
        (labels ((register-p (tn)
                   (and (tn-p tn)
                        (not (eq (tn-kind tn) :unused))
                        (eq (sc-sb (tn-sc tn)) register-sb)))
                 (constant-eql-p (a b)
                   (or (eq a b)
                       (and (eq (sc-name (tn-sc a)) 'constant)
                            (eq (tn-sc a) (tn-sc b))
                            (eql (tn-offset a) (tn-offset b)))))
                 (remove-constant (tn)
                   (when (register-p tn)
                     (setf (svref loaded-constants (tn-offset tn)) nil)))
                 (remove-written-tns ()
                   (cond ((memq (vop-info-save-p (vop-info vop))
                                '(t :force-to-stack))
                          (fill loaded-constants nil))
                         (t
                          (do ((ref (vop-results vop) (tn-ref-across ref)))
                              ((null ref))
                            (remove-constant (tn-ref-tn ref))
                            (remove-constant (tn-ref-load-tn ref)))
                          (do ((ref (vop-temps vop) (tn-ref-across ref)))
                              ((null ref))
                            (remove-constant (tn-ref-tn ref)))
                          (do ((ref (vop-args vop) (tn-ref-across ref)))
                              ((null ref))
                            (remove-constant (tn-ref-load-tn ref))))))
                 (compatible-scs-p (a b)
                   (or (eql a b)
                       (and (eq (sc-name a) 'sb-vm::control-stack)
                            (eq (sc-name b) 'sb-vm::descriptor-reg))
                       (and (eq (sc-name b) 'sb-vm::control-stack)
                            (eq (sc-name a) 'sb-vm::descriptor-reg))))
                 (find-constant-tn (constant sc)
                   (loop for (saved-constant . tn) across loaded-constants
                         when (and saved-constant
                                   (constant-eql-p saved-constant constant)
                                   (compatible-scs-p (tn-sc tn) sc))
                         return tn)))
          (case (vop-name vop)
            ((move sb-vm::move-arg)
             (let* ((args (vop-args vop))
                    (results (vop-results vop))
                    (x (tn-ref-tn args))
                    (x-load-tn (tn-ref-load-tn args))
                    (y (tn-ref-tn results))
                    constant)
               (cond ((or (eq (sc-name (tn-sc x)) 'null)
                          (not (eq (tn-kind x) :constant)))
                      (remove-written-tns))
                     ((setf constant (find-constant-tn x (tn-sc y)))
                      (when (register-p y)
                        (setf (svref loaded-constants (tn-offset y))
                              (cons x y)))
                      ;; XOR is more compact on x86oids and many
                      ;; RISCs have a zero register
                      (unless (and (constant-p (tn-leaf x))
                                   (eql (tn-value x) 0)
                                   (register-p y))
                        (change-tn-ref-tn args constant)
                        (setf (tn-ref-load-tn args) nil)))
                     ((register-p y)
                      (setf (svref loaded-constants (tn-offset y))
                            (cons x y)))
                     ((and x-load-tn
                           (or (not (tn-ref-load-tn results))
                               (location= (tn-ref-load-tn results)
                                          x-load-tn)))
                      (setf (svref loaded-constants (tn-offset x-load-tn))
                            (cons x x-load-tn)))
                     (t
                      (remove-written-tns)))))
            (t
             ;; Stack allocation alignes the stack and leaves it aligned,
             ;; adjacent stack allocation doesn't need to realign it.
             (let ((node (vop-node vop)))
               (flet ((vop-dx-info ()
                        (case (vop-name vop)
                          (make-closure
                           (nthcdr 2 (vop-codegen-info vop))))))
                 (cond ((memq (vop-name vop)
                              '(multiple-call multiple-call-local
                                multiple-call-named
                                static-multiple-call-named
                                multiple-call-variable
                                push-values values-list
                                reverse-values-list %more-arg-values
                                unaligned-dx-cons))
                        (setf aligned-stack nil))
                       ((memq (vop-name vop) '(move-operand))) ;; shares vop-node
                       ((and aligned-stack
                             (neq aligned-stack node))
                        (let ((info (vop-dx-info)))
                          (if info
                              (when (car info)
                                (setf (car info) :aligned-stack))
                              (when (and (combination-p node)
                                         (node-stack-allocate-p node))
                                (setf (combination-info node) :aligned-stack)))))
                       (t
                        (let ((info (vop-dx-info)))
                          (when (if info
                                    (car info)
                                    (and (valued-node-p node)
                                         (node-stack-allocate-p node)))
                            (setf aligned-stack node)))))))
             (remove-written-tns))))))))

(defun ir2-optimize (component &optional stage)
  (let ((*2block-info* (make-hash-table :test #'eq)))
    (initialize-ir2-blocks-flow-info component)
    (case stage
      (regalloc
       (run-vop-optimizers component stage)
       (delete-no-op-vops component)
       (ir2-optimize-jumps component)
       #+x86-64 (attempt-pseudo-atomic-store-bunching component)
       (optimize-constant-loads component))
      (select-representations
       ;; Give the optimizers a second opportunity to alter newly inserted vops
       ;; by looking for patterns that have a shorter expression as a single vop.
       (run-vop-optimizers component stage t)
       (delete-unused-ir2-blocks component))
      (t
       (when (and *compiler-trace-output*
                  (member :pre-ir2-optimize *compile-trace-targets*))
         (let ((*standard-output* *compiler-trace-output*))
           ;; We really ought to print the IR1 before IR2 but this achieves its
           ;; purpose of helping figure out what changes were made to IR2.
           (format t "~&Before IR2-optimize:~%")
           (print-ir2-blocks component)))
       (run-vop-optimizers component)
       (delete-unused-ir2-blocks component))))

  (values))

(defun delete-unnecessary-move (vop)
  (when (and (vop-next vop)
             (eq (vop-name (vop-next vop)) 'move)
             ;; the source of the move is the result of this
             (eq (tn-ref-tn (vop-args (vop-next vop)))
                 (tn-ref-tn (vop-results vop)))
             ;; the destination of the move is the same as the input of this
             (eq (tn-ref-tn (vop-results (vop-next vop)))
                 (tn-ref-tn (vop-args vop)))
             ;; there is exactly one write and one read of the intermediate TN
             (very-temporary-p (tn-ref-tn (vop-results vop))))
    ;; Change my result ref to the same TN as the input and delete the MOVE
    (change-tn-ref-tn (vop-results vop) (tn-ref-tn (vop-args vop)))
    (delete-vop (vop-next vop))))
