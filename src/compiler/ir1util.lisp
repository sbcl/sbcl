;;;; This file contains miscellaneous utilities used for manipulating
;;;; the IR1 representation.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; cleanup hackery

(defun delete-lexenv-enclosing-cleanup (lexenv)
  (declare (type lexenv lexenv))
  (do ((lexenv2 lexenv
                (lambda-call-lexenv (lexenv-lambda lexenv2))))
      ((null lexenv2) nil)
    (when (lexenv-cleanup lexenv2)
      (setf (lexenv-cleanup lexenv2) nil))))

;;; Return the innermost cleanup enclosing NODE, or NIL if there is
;;; none in its function. If NODE has no cleanup, but is in a LET,
;;; then we must still check the environment that the call is in.
(defun node-enclosing-cleanup (node)
  (declare (type node node))
  (do ((lexenv (node-lexenv node)
               (lambda-call-lexenv (lexenv-lambda lexenv))))
      ((null lexenv) nil)
    (let ((cup (lexenv-cleanup lexenv)))
      (when cup (return cup)))))

(defun map-nested-cleanups (function block-or-node &optional return-value)
  (declare (type (or node cblock) block-or-node))
  (do ((cleanup (if (node-p block-or-node)
                    (node-enclosing-cleanup block-or-node)
                    (block-end-cleanup block-or-node))
                (node-enclosing-cleanup (cleanup-mess-up cleanup))))
      ((not cleanup) return-value)
    (funcall function cleanup)))

;;; Convert the FORM in a block inserted between BLOCK1 and BLOCK2 as
;;; an implicit MV-PROG1. The inserted block is returned. NODE is used
;;; for IR1 context when converting the form. Note that the block is
;;; not assigned a number, and is linked into the DFO at the
;;; beginning. We indicate that we have trashed the DFO by setting
;;; COMPONENT-REANALYZE. If CLEANUP is supplied, then convert with
;;; that cleanup.
(defun insert-cleanup-code (pred-blocks succ-block node form &optional cleanup)
  (declare  (type node node) (type (or cleanup null) cleanup))
  (setf (component-reanalyze (block-component (car pred-blocks))) t)
  (with-ir1-environment-from-node node
    (with-component-last-block (*current-component*
                                (block-next (component-head *current-component*)))
      (let* ((start (make-ctran))
             (block (ctran-starts-block start))
             (next (make-ctran))
             (*lexenv* (if cleanup
                           (make-lexenv :cleanup cleanup)
                           *lexenv*)))
        (loop for pred-block in pred-blocks
              do
              (change-block-successor pred-block succ-block block))
        (link-blocks block succ-block)
        (ir1-convert start next nil form)
        (setf (block-last block) (ctran-use next))
        (setf (node-next (block-last block)) nil)
        block))))

;;;; lvar use hacking

;;; Return a list of all the nodes which use LVAR.
(defun find-uses (lvar)
  (declare (type lvar lvar) #-sb-xc-host (values list))
  (ensure-list (lvar-uses lvar)))

(defun principal-lvar (lvar)
  (declare (type lvar lvar))
  (labels ((pl (lvar)
             (let ((use (lvar-uses lvar)))
               (if (cast-p use)
                   (pl (cast-value use))
                   lvar))))
    (pl lvar)))

(defun principal-lvar-use (lvar)
  (labels ((plu (lvar)
             (declare (type lvar lvar))
             (let ((use (lvar-uses lvar)))
               (if (cast-p use)
                   (plu (cast-value use))
                   use))))
    (plu lvar)))

(defun principal-lvar-ref-use (lvar &optional casts)
  (let (seen)
    (labels ((recurse (lvar)
               (when lvar
                 (let ((use (lvar-uses lvar)))
                   (cond ((ref-p use)
                          (push lvar seen)
                          (let ((lvar (lambda-var-ref-lvar use)))
                            (if (memq lvar seen)
                                use
                                (recurse lvar))))
                         ((and casts
                               (cast-p use))
                          (recurse (cast-value use)))
                         (t
                          use))))))
      (recurse lvar))))

(defun principal-lvar-ref (lvar &optional casts)
  (labels ((recurse (lvar ref)
             (if lvar
                 (let ((use (lvar-uses lvar)))
                   (cond ((ref-p use)
                          (recurse (lambda-var-ref-lvar use) use))
                         ((and casts
                               (cast-p use))
                          (recurse (cast-value use) ref))
                         (t
                          ref)))
                 ref)))
    (recurse lvar nil)))

(defun lvar-lambda-var (lvar)
  (let ((ref (principal-lvar-ref lvar)))
    (and (ref-p ref)
         (lambda-var-p (ref-leaf ref))
         (ref-leaf ref))))

;;; Look through casts and variables, m-v-bind+values
(defun map-all-uses (function lvar)
  (declare (dynamic-extent function))
  (let (seen)
   (labels ((recurse-lvar (lvar)
              (do-uses (use lvar)
                (recurse use)))
            (recurse (use)
              (cond ((ref-p use)
                     (let ((lvar (lambda-var-ref-lvar use)))
                       (cond (lvar
                              (recurse-lvar lvar))
                             ((let ((var (ref-leaf use)))
                                (when (and (lambda-var-p var)
                                           (not (lambda-var-sets var)))
                                  (let* ((fun (lambda-var-home var))
                                         (vars (lambda-vars fun)))
                                    (cond ((functional-kind-eq fun mv-let)
                                           (let* ((fun (lambda-var-home var))
                                                  (n-value (position-or-lose var vars))
                                                  (args (basic-combination-args (let-combination fun))))
                                             (when (singleton-p args)
                                               (let ((all-processed t))
                                                 (do-uses (use (car args))
                                                   (unless (when (and (combination-p use)
                                                                      (eq (lvar-fun-name (combination-fun use))
                                                                          'values))
                                                             (let ((lvar (nth n-value (combination-args use))))
                                                               (when lvar
                                                                 (recurse-lvar lvar)
                                                                 t)))
                                                     (setf all-processed nil)))
                                                 all-processed))))
                                          ((not (memq fun seen))
                                           (push fun seen)
                                           (dolist (ref (leaf-refs fun))
                                             (let* ((lvar (node-lvar ref))
                                                    (combination (and lvar
                                                                      (lvar-dest lvar))))
                                               (when (and (combination-p combination)
                                                          (eq (combination-kind combination) :local)
                                                          (eq (combination-fun combination)
                                                              lvar))
                                                 (loop for v in vars
                                                       for arg in (combination-args combination)
                                                       when (eq v var)
                                                       do
                                                       (do-uses (use arg)
                                                         (recurse use))))))))))))
                             (t
                              (funcall function use)))))

                    ((cast-p use)
                     (recurse-lvar (cast-value use)))
                    (t
                     (funcall function use)))))
     (recurse-lvar lvar))))

(defun mv-principal-lvar-ref-use (lvar)
  (labels ((recurse (lvar)
             (let ((use (lvar-uses lvar)))
               (if (ref-p use)
                   (let ((var (ref-leaf use)))
                     (if (and (lambda-var-p var)
                              (null (lambda-var-sets var)))
                         (functional-kind-case (lambda-var-home var)
                           (mv-let
                            (let* ((fun (lambda-var-home var))
                                   (n-value (position-or-lose var (lambda-vars fun))))
                              (loop for arg in (basic-combination-args (let-combination fun))
                                    for nvals = (nth-value 1 (values-types (lvar-derived-type arg)))
                                    when (eq nvals :unknown) return nil
                                    when (<= n-value nvals) do (return-from mv-principal-lvar-ref-use
                                                                 (values (lvar-uses arg) n-value))
                                    do (decf n-value nvals))
                              use))
                           (let
                            (recurse (let-var-initial-value var)))
                           (t
                            use))
                         use))
                   use))))
    (recurse lvar)))

(defun map-lvar-dest-casts (fun lvar)
  (labels ((pld (lvar)
             (and lvar
                  (let ((dest (lvar-dest lvar)))
                    (when (cast-p dest)
                      (funcall fun dest)
                      (pld (cast-lvar dest)))))))
    (pld lvar)))

(defun let-lvar-dest (lvar)
  (let ((dest (lvar-dest (principal-lvar lvar))))
    (if (and (combination-p dest)
             (eq (basic-combination-kind dest) :local))
        (let* ((fun (combination-lambda dest))
               (n (position-or-lose lvar
                                    (combination-args dest)))
               (var (nth n (lambda-vars fun)))
               (refs (leaf-refs var)))
          (when (and refs
                     (not (cdr refs)))
            (let-lvar-dest (node-lvar (car refs)))))
        dest)))

(defun lvar-dest-var (lvar)
  (when lvar
    (let ((dest (lvar-dest lvar)))
      (cond ((combination-p dest)
             (if (eq (basic-combination-kind dest) :local)
                 (let* ((fun (combination-lambda dest))
                        (n (position-or-lose lvar
                                             (combination-args dest)))
                        (var (nth n (lambda-vars fun))))
                   var)
                 (and (lvar-fun-is (combination-fun dest) '(sb-vm::splat))
                      (lvar-dest-var (node-lvar dest)))))
            ((cast-p dest)
             (lvar-dest-var (node-lvar dest)))))))

(defun immediately-used-let-dest (lvar node &optional flushable)
  (let ((dest (lvar-dest lvar)))
    (when (almost-immediately-used-p lvar node :flushable flushable)
      (if (and (combination-p dest)
               (eq (combination-kind dest) :local))
          (let* ((fun (combination-lambda dest))
                 (n (position-or-lose lvar
                                      (combination-args dest)))
                 (var (nth n (lambda-vars fun)))
                 (refs (leaf-refs var)))
            (loop for ref in refs
                  for lvar = (node-lvar ref)
                  when (and lvar (almost-immediately-used-p lvar (lambda-bind fun)))
                  do (return (values (lvar-dest lvar) lvar))))
          (values dest lvar)))))

(defun mv-bind-dest (lvar nth-value)
  (when lvar
    (let ((dest (lvar-dest lvar)))
      (when (and (mv-combination-p dest)
                 (eq (basic-combination-kind dest) :local))
        (let ((fun (combination-lambda dest)))
          (let* ((var (nth nth-value (lambda-vars fun)))
                 (refs (leaf-refs var)))
            (when (and refs
                       (not (cdr refs)))
              (when (functional-kind-eq fun mv-let)
                (let-lvar-dest (node-lvar (car refs)))))))))))

(defun mv-bind-unused-p (lvar nth-value)
  (when lvar
    (let ((dest (lvar-dest lvar)))
      (when (and (mv-combination-p dest)
                 (eq (basic-combination-kind dest) :local))
        (let ((fun (combination-lambda dest)))
          (when (functional-kind-eq fun mv-let)
            (let ((var (nth nth-value (lambda-vars fun))))
              (notany #'node-lvar (leaf-refs var)))))))))

(defun combination-matches (name args combination)
  (and (combination-p combination)
       (let ((fun (combination-fun combination)))
         (when (eq (lvar-fun-name fun) name)
           (loop for arg in (combination-args combination)
                 for arg-m in args
                 always (or (eq arg arg-m)
                            (eq arg-m '*)
                            (and (constant-lvar-p arg)
                                 (eql (lvar-value  arg) arg-m))))))))

(defun erase-lvar-type (lvar)
  (when lvar
    (setf (lvar-%derived-type lvar) nil)
    (loop for annotation in (lvar-annotations lvar)
          when (lvar-type-annotation-p annotation)
          do (setf (lvar-annotation-fired annotation) t))
    (let ((dest (lvar-dest lvar)))
      (cond ((cast-p dest)
             (derive-node-type dest *wild-type* :from-scratch t)
             (erase-lvar-type (node-lvar dest)))
            ((and (basic-combination-p dest)
                  (eq (basic-combination-kind dest) :local))
             (let ((fun (combination-lambda dest)))
               (flet ((erase (var)
                        (setf (lambda-var-type var) *universal-type*)
                        (loop for ref in (leaf-refs var)
                              do (derive-node-type ref *wild-type* :from-scratch t)
                                 (erase-lvar-type (node-lvar ref)))))
                 (if (functional-kind-eq fun mv-let)
                     (mapc #'erase (lambda-vars fun))
                     (erase
                      (nth (position-or-lose lvar
                                             (basic-combination-args dest))
                           (lambda-vars fun)))))))
            ((and (combination-p dest)
                  (lvar-fun-is (combination-fun dest) '(values))
                  (let ((mv (node-dest dest)))
                    (when (and (mv-combination-p mv)
                               (eq (basic-combination-kind mv) :local))
                      (let ((fun (combination-lambda mv)))
                        (when (and (functional-p fun)
                                   (functional-kind-eq fun mv-let))
                          (derive-node-type dest *wild-type* :from-scratch t)
                          (erase-lvar-type (node-lvar dest))))))))))))

;;; Update lvar use information so that NODE is no longer a use of its
;;; LVAR.
;;;
;;; Note: if you call this function, you may have to do a
;;; REOPTIMIZE-LVAR to inform IR1 optimization that something has
;;; changed.

;;; Just delete NODE from its LVAR uses; LVAR is preserved so it may
;;; be given a new use.
(defun %delete-lvar-use (node)
  (declare (type node node))
  (let ((lvar (node-lvar node)))
    (when lvar
      (if (listp (lvar-uses lvar))
          (let ((new-uses (delq1 node (lvar-uses lvar))))
            (setf (lvar-uses lvar)
                  (if (singleton-p new-uses)
                      (first new-uses)
                      new-uses)))
          (setf (lvar-uses lvar) nil))
      (flush-node node)))
  (values))
;;; Delete NODE from its LVAR uses; if LVAR has no other uses, delete
;;; its DEST's block, which must be unreachable.
(defun delete-lvar-use (node)
  (declare (type node node))
  (let ((lvar (node-lvar node)))
    (when lvar
      (%delete-lvar-use node)
      (if (null (lvar-uses lvar))
          (binding* ((dest (lvar-dest lvar) :exit-if-null)
                     (() (not (node-deleted dest)) :exit-if-null)
                     (block (node-block dest)))
            (mark-for-deletion block))
          (reoptimize-lvar lvar))))
  (values))

;;; Update lvar use information so that NODE uses LVAR.
;;;
;;; Note: if you call this function, you may have to do a
;;; REOPTIMIZE-LVAR to inform IR1 optimization that something has
;;; changed.
(defun add-lvar-use (node lvar)
  (declare (type node node) (type (or lvar null) lvar))
  (aver (not (node-lvar node)))
  (when lvar
    (let ((uses (lvar-uses lvar)))
      (setf (lvar-uses lvar)
            (cond ((null uses)
                   node)
                  ((listp uses)
                   (cons node uses))
                  (t
                   (list node uses))))
      (setf (node-lvar node) lvar)))

  (values))

(declaim (inline lvar-single-value-p))
(defun lvar-single-value-p (lvar)
  (or (not lvar) (%lvar-single-value-p lvar)))

;;; Return true if LVAR destination is executed immediately after
;;; NODE. Cleanups are ignored.
(defun immediately-used-p (lvar node &optional single-predecessor)
  (declare (type lvar lvar) (type node node))
  (aver (eq (node-lvar node) lvar))
  (let ((dest (lvar-dest lvar)))
    (acond ((node-next node)
            (eq (ctran-next it) dest))
           (t
            (let ((succ (first (block-succ (node-block node)))))
              (and (not (and single-predecessor
                             (cdr (block-pred succ))))
                   (eq (block-start succ)
                       (node-prev dest))))))))

;;; Return true if LVAR destination is executed after node with only
;;; uninteresting nodes intervening.
;;;
;;; Uninteresting nodes are nodes in the same block which are either
;;; REFs, ENCLOSEs, or external CASTs to the same destination.
(defun almost-immediately-used-p (lvar node &key flushable
                                                 no-multiple-value-lvars)
  (declare (type lvar lvar)
           (type node node))
  (unless (bind-p node)
    (aver (eq (node-lvar node) lvar)))
  (let ((dest (lvar-dest lvar))
        ctran)
    (tagbody
     :next
       (setf ctran (node-next node))
     :next-ctran
       (cond (ctran
              (setf node (ctran-next ctran))
              (if (eq node dest)
                  (return-from almost-immediately-used-p t)
                  (let ((node-lvar (and (valued-node-p node)
                                        (node-lvar node))))
                    (when (and no-multiple-value-lvars
                               node-lvar
                               (not (lvar-single-value-p node-lvar)))
                      (return-from almost-immediately-used-p))
                    (typecase node
                      (ref
                       (go :next))
                      (cast
                       (when (or (and (memq (cast-type-check node) '(:external nil))
                                      (eq dest (node-dest node)))
                                 (and flushable
                                      (not (contains-hairy-type-p (cast-type-to-check node))))
                                 ;; If the types do not match then this
                                 ;; cast is not related to the LVAR and
                                 ;; wouldn't be affected if it's
                                 ;; executed out of order.
                                 (multiple-value-bind (res true)
                                     (values-subtypep (node-derived-type node)
                                                      (lvar-derived-type lvar))
                                   (and (not res)
                                        true)))
                         (go :next)))
                      (combination
                       (when (and flushable
                                  (flushable-combination-p node))
                         (go :next))
                       (let (fun)
                         (when (and (eq (combination-kind node) :local)
                                    (functional-kind-eq (setf fun (combination-lambda node)) let))
                           (setf node (lambda-bind fun))
                           (go :next))))
                      ((or enclose entry)
                       (go :next))))))
             (t
              ;; Loops shouldn't cause a problem, either it will
              ;; encounter a not "uninteresting" node, or the destination
              ;; will be unreachable anyway.
              (let ((start (block-start (first (block-succ (node-block node))))))
                (when start
                  (setf ctran start)
                  (go :next-ctran))))))))

(defun skip-nodes-before-node-p (block before-node)
  (do-nodes (node nil block)
    (when (eq node before-node)
      (return t))
    (when (and (valued-node-p node)
               (neq (node-dest node) before-node))
      (return))
    (typecase node
      (ref)
      (combination
       (unless (flushable-combination-p node)
         (return)))
      (t
       (return)))))

;;; Check that all the uses are almost immediately used and look through CASTs,
;;; as they can be freely deleted removing the immediateness
(defun lvar-almost-immediately-used-p (lvar)
  (do-uses (use lvar t)
    (unless (and (almost-immediately-used-p lvar use)
                 (or (not (cast-p use))
                     (lvar-almost-immediately-used-p (cast-value use))))
      (return))))

(defun let-var-immediately-used-p (ref var lvar)
  (let ((bind (lambda-bind (lambda-var-home var))))
    (when bind
      (let* ((next-ctran (node-next bind))
             (next-node (and next-ctran
                             (ctran-next next-ctran))))
        (and (eq next-node ref)
             (lvar-almost-immediately-used-p lvar))))))



(declaim (inline block-to-be-deleted-p))
(defun block-to-be-deleted-p (block)
  (declare (type cblock block))
  (or (block-delete-p block)
      (functional-kind-eq (block-home-lambda block) deleted)))

;;; Checks whether NODE is in a block to be deleted
(declaim (inline node-to-be-deleted-p))
(defun node-to-be-deleted-p (node)
  (block-to-be-deleted-p (node-block node)))

(defun lambda-block (lambda)
  (declare (type clambda lambda))
  (node-block (lambda-bind lambda)))

(defun lambda-component (lambda)
  (declare (type clambda lambda))
  (block-component (lambda-block lambda)))

(defun block-start-node (block)
  (declare (type cblock block))
  (ctran-next (block-start block)))


;;;; lvar substitution

(defun update-lvar-dependencies (new old)
  (typecase old
    (ref
     (update-lvar-dependencies new (lambda-var-ref-lvar old)))
    (lvar
     (do-uses (node old)
       (when (exit-p node)
         ;; Inlined functions will try to use the lvar in the lexenv
         (loop for block in (lexenv-blocks (node-lexenv node))
               for block-lvar = (fourth block)
               when (eq old block-lvar)
               do (setf (fourth block) new))))
     (propagate-lvar-annotations new old))))

;;; In OLD's DEST, replace OLD with NEW. NEW's DEST must initially be
;;; NIL. We do not flush OLD's DEST.
(defun substitute-lvar (new old)
  (declare (type lvar old new))
  (aver (not (lvar-dest new)))
  (let ((dest (lvar-dest old)))
    (etypecase dest
      ((or ref bind))
      (cif (setf (if-test dest) new))
      (cset (setf (set-value dest) new))
      (creturn (setf (return-result dest) new))
      (exit (setf (exit-value dest) new))
      (basic-combination
       (if (eq old (basic-combination-fun dest))
           (setf (basic-combination-fun dest) new)
           (setf (basic-combination-args dest)
                 (nsubst new old (basic-combination-args dest)))))
      (cast (setf (cast-value dest) new)))

    (setf (lvar-dest old) nil)
    (setf (lvar-dest new) dest))
  (values))

;;; Replace all uses of OLD with uses of NEW, where NEW has an
;;; arbitary number of uses. NEW is supposed to be "later" than OLD.
(defun substitute-lvar-uses (new old propagate-dx)
  (declare (type lvar old)
           (type (or lvar null) new)
           (type boolean propagate-dx))
  (cond (new
         (update-lvar-dependencies new old)
         (do-uses (node old)
           (%delete-lvar-use node)
           (add-lvar-use node new))
         (reoptimize-lvar new)
         (when propagate-dx
           (propagate-lvar-dx new old)))
        (t
         (update-lvar-dependencies new old)
         (flush-dest old)))

  (values))

(defun propagate-lvar-dx (new old)
  (let ((dynamic-extent (lvar-dynamic-extent old)))
    (when dynamic-extent
      (setf (lvar-dynamic-extent old) nil)
      (setf (dynamic-extent-values dynamic-extent)
            (delq1 old (dynamic-extent-values dynamic-extent)))
      (unless (lvar-dynamic-extent new)
        (setf (lvar-dynamic-extent new) dynamic-extent)
        (push new (dynamic-extent-values dynamic-extent))))))

(defun lexenv-contains-lambda (lambda parent-lexenv)
  (loop for lexenv = (lambda-lexenv lambda)
        then (let ((lambda (lexenv-lambda lexenv)))
               (and lambda
                    (lambda-call-lexenv lambda)))
        while lexenv
        thereis
        (loop for parent = lexenv then (lexenv-parent parent)
              while parent
              thereis (eq parent parent-lexenv))))

;;; Handle
;;; (dx-let ((x (let ((m (make-array)))
;;;               (fill m)
;;;               m))))
(defun propagate-ref-dx (new-ref old-lvar var)
  (let ((dynamic-extent (lvar-dynamic-extent old-lvar))
        (leaf (ref-leaf new-ref)))
    (when dynamic-extent
      (typecase leaf
        (lambda-var
         (when (and (functional-kind-eq (lambda-var-home leaf) let)
                    ;; Make sure the let is inside the dx let
                    (lexenv-contains-lambda (lambda-var-home leaf)
                                            (node-lexenv dynamic-extent)))
           (propagate-lvar-dx (let-var-initial-value leaf) old-lvar)))
        (clambda
         (when (and (null (rest (leaf-refs leaf)))
                    (lexenv-contains-lambda leaf
                                            (node-lexenv dynamic-extent)))
           (let ((fun (functional-entry-fun leaf)))
             (setf (enclose-dynamic-extent (functional-enclose fun))
                   dynamic-extent)
             (setf (leaf-dynamic-extent fun) (leaf-dynamic-extent var))
             (setf (lvar-dynamic-extent old-lvar) nil)
             (setf (dynamic-extent-values dynamic-extent)
                   (delq1 old-lvar (dynamic-extent-values dynamic-extent)))))))
      t)))

(defun node-dominates-p (node1 node2)
  (let ((block1 (node-block node1))
        (block2 (node-block node2)))
    (if (eq block1 block2)
        (do-nodes (node nil block1)
          (cond ((eq node node1)
                 (setf node1 0))
                ((eq node node2)
                 (return (eq node1 0)))))
        (let ((component (block-component block1)))
          (unless (component-dominators-computed component)
            (find-dominators component))
          (dominates-p block1 block2)))))

(defun set-slot-old-p (node nth-object nth-value)
  (flet ((pseudo-static-value-p (lvar)
           (block nil
             (map-all-uses
              (lambda (use)
                (unless (and (ref-p use)
                             (let ((leaf (ref-leaf use)))
                               (or (and (constant-p leaf)
                                        #-sb-xc-host
                                        (let ((value (constant-value leaf)))
                                          (or (sb-xc:typep value '(or character sb-xc:fixnum #+64-bit single-float boolean))
                                              (and (eql (generation-of value) sb-vm:+pseudo-static-generation+)
                                                   (or (not (sb-c::producing-fasl-file))
                                                       (and (symbolp value)
                                                            (logtest sb-vm::+symbol-initial-core+ (get-header-data value))))))))
                                   #+sb-xc-host
                                   (and (lambda-p leaf)
                                        (not (environment-closure (get-lambda-environment leaf)))))))
                  (return)))
              lvar)
             t)))
    (cond ((set-p node)
           (pseudo-static-value-p (set-value node)))
          ((combination-p node)
           (when (lvar-fun-is (combination-fun node) '(initialize-vector))
             (return-from set-slot-old-p t))
           (let ((args (combination-args node)))
             (when (lvar-fun-is (combination-fun node) '(%%primitive))
               (pop args))
             (let* ((object-lvar (nth nth-object args))
                    (value-lvar (nth nth-value args))
                    (allocator (principal-lvar-ref-use object-lvar t)))
               (labels ((born-before-p (node)
                          (block nil
                            (map-all-uses
                             (lambda (use)
                               (cond ((and (ref-p use)
                                           (let ((leaf (ref-leaf use)))
                                             (or (constant-p leaf)
                                                 (when (and (lambda-var-p leaf)
                                                            (not (lambda-var-sets leaf)))
                                                   (let ((home (lambda-var-home leaf)))
                                                     (and (functional-kind-eq home external optional)
                                                          (let ((entry (if (functional-kind-eq home external)
                                                                           (main-entry (functional-entry-fun home))
                                                                           home))
                                                                (node-home (node-home-lambda node)))
                                                            (or (eq (lambda-environment node-home)
                                                                    (lambda-environment entry))
                                                                (lexenv-contains-lambda node-home
                                                                                        (lambda-lexenv entry))
                                                                (return))))))
                                                 (and (lambda-p leaf)
                                                      (or (not (environment-closure (get-lambda-environment leaf)))
                                                          (let ((enclose (xep-enclose leaf)))
                                                            (and enclose
                                                                 (node-dominates-p enclose node)))))))))
                                     ((not (node-dominates-p use node))
                                      (return))))
                             value-lvar)
                            t))
                        (allocator-p (allocator)
                          (or (and (combination-p allocator)
                                   (or
                                    (lvar-fun-is (combination-fun allocator) '(list* list %make-list
                                                                               %listify-rest-args
                                                                               %make-structure-instance
                                                                               %make-instance
                                                                               %make-instance/mixed
                                                                               %make-funcallable-instance
                                                                               allocate-vector
                                                                               initialize-vector
                                                                               copy-structure
                                                                               copy-list
                                                                               copy-tree
                                                                               copy-seq
                                                                               subseq
                                                                               vector-subseq*))
                                    (and (lvar-fun-is (combination-fun allocator) '(sb-vm::splat))
                                         (let ((allocator (principal-lvar-ref-use
                                                           (principal-lvar (first (combination-args allocator))))))
                                           (and (combination-p allocator)
                                                (lvar-fun-is (combination-fun allocator) '(allocate-vector)))))
                                    (let ((name (lvar-fun-name (combination-fun allocator) t)))
                                      (typep (info :function :source-transform name)
                                             '(cons * (eql :constructor)))))))))
                 (or (and (allocator-p allocator)
                          (born-before-p allocator))
                     (pseudo-static-value-p value-lvar)))))))))

;;;; block starting/creation

;;; Return the block that CTRAN is the start of, making a block if
;;; necessary. This function is called by IR1 translators which may
;;; cause a CTRAN to be used more than once. Every CTRAN which may be
;;; used more than once must start a block by the time that anyone
;;; does a USE-CTRAN on it.
;;;
;;; We also throw the block into the next/prev list for the
;;; *CURRENT-COMPONENT* so that we keep track of which blocks we have
;;; made.
(defun ctran-starts-block (ctran)
  (declare (type ctran ctran))
  (ecase (ctran-kind ctran)
    (:unused
     (aver (not (ctran-block ctran)))
     (let* ((next (component-last-block *current-component*))
            (prev (block-prev next))
            (new-block (make-block ctran)))
       (setf (block-next new-block) next
             (block-prev new-block) prev
             (block-prev next) new-block
             (block-next prev) new-block
             (ctran-block ctran) new-block
             (ctran-kind ctran) :block-start)
       (aver (not (ctran-use ctran)))
       new-block))
    (:block-start
     (ctran-block ctran))))

;;; Ensure that CTRAN is the start of a block so that the use set can
;;; be freely manipulated.
(defun ensure-block-start (ctran)
  (declare (type ctran ctran))
  (let ((kind (ctran-kind ctran)))
    (ecase kind
      ((:block-start))
      ((:unused)
       (setf (ctran-block ctran)
             (make-block-key :start ctran))
       (setf (ctran-kind ctran) :block-start))
      ((:inside-block)
       (node-ends-block (ctran-use ctran)))))
  (values))


;;;;

;;; Filter values of LVAR through the form produced by
;;; FUNCTION. FUNCTION takes one argument and returns a form with the
;;; argument spliced into the form exactly once. This argument is a
;;; placeholder which will be replaced with LVAR once the form is
;;; IR1 converted and the resulting code is spliced in before LVAR's
;;; DEST. The new lvar which represents the value of the form is
;;; called the "filtered" lvar.
(defun filter-lvar (lvar function)
  (declare (type lvar lvar)
           (type function function))
  (let* ((dest (lvar-dest lvar))
         (ctran (node-prev dest))
         ;; We pick an arbitrary unique leaf so that IR1-convert will
         ;; reference it.
         (placeholder (make-constant 0))
         (form (funcall function placeholder)))
    (with-ir1-environment-from-node dest
      (ensure-block-start ctran)
      (let* ((old-block (ctran-block ctran))
             (new-start (make-ctran))
             (filtered-lvar (make-lvar))
             (new-block (ctran-starts-block new-start)))
        ;; Splice in the new block before DEST, giving the new block
        ;; all of DEST's predecessors.
        (dolist (block (block-pred old-block))
          (change-block-successor block old-block new-block))
        (ir1-convert new-start ctran filtered-lvar form)

        ;; Replace PLACEHOLDER with the LVAR.
        (let* ((refs (leaf-refs placeholder))
               (node (first refs)))
          (cond (refs
                 (let ((victim (node-lvar node)))
                  (aver (null (rest refs))) ; PLACEHOLDER must be referenced exactly once.
                  (substitute-lvar filtered-lvar lvar)
                  (substitute-lvar lvar victim)
                  (flush-dest victim)))
                (t
                 (flush-dest lvar))))

        ;; The form may have introduced new local calls, for example,
        ;; from LET bindings, so invoke local call analysis.
        (locall-analyze-component *current-component*))))
  (values))

(defun insert-code (before form)
  (let ((ctran (node-prev before)))
    (with-ir1-environment-from-node before
      (ensure-block-start ctran)
      (let* ((old-block (ctran-block ctran))
             (new-start (make-ctran))
             (filtered-lvar (make-lvar))
             (new-block (ctran-starts-block new-start)))
        ;; Splice in the new block before DEST, giving the new block
        ;; all of DEST's predecessors.
        (dolist (block (block-pred old-block))
          (change-block-successor block old-block new-block))
        (ir1-convert new-start ctran filtered-lvar form)
        ;; The form may have introduced new local calls, for example,
        ;; from LET bindings, so invoke local call analysis.
        (locall-analyze-component *current-component*))))
  (values))

(defun replace-node (node form)
  (let ((ctran (node-prev node)))
    (with-ir1-environment-from-node node
      (ensure-block-start ctran)
      (let* ((old-block (ctran-block ctran))
             (new-start (make-ctran))
             (lvar (node-lvar node))
             (new-block (ctran-starts-block new-start)))
        ;; Splice in the new block before DEST, giving the new block
        ;; all of DEST's predecessors.
        (dolist (block (block-pred old-block))
          (change-block-successor block old-block new-block))
        (ir1-convert new-start ctran lvar form)
        (unlink-node node)
        ;; The form may have introduced new local calls, for example,
        ;; from LET bindings, so invoke local call analysis.
        (locall-analyze-component *current-component*))))
  (values))

;;; Delete NODE and VALUE. It may result in some calls becoming tail.
(defun delete-filter (node lvar value)
  (aver (eq (lvar-dest value) node))
  (aver (eq (node-lvar node) lvar))
  (cond (lvar (collect ((merges))
                (when (return-p (lvar-dest lvar))
                  (do-uses (use value)
                    (when (and (basic-combination-p use)
                               (not (node-to-be-deleted-p use))
                               (eq (basic-combination-kind use) :local))
                      (merges use))))
                (substitute-lvar-uses lvar value
                                      (eq (lvar-uses lvar) node))
                (%delete-lvar-use node)
                (prog1
                    (unlink-node node)
                  (dolist (merge (merges))
                    (merge-tail-sets merge)))))
        (t (flush-dest value)
           (unlink-node node))))

;;; Make a CAST and insert it into IR1 before node NEXT.

(defun insert-cast-before (next lvar type policy &optional context)
  (declare (type node next) (type lvar lvar) (type ctype type))
  (with-ir1-environment-from-node next
    (%insert-cast-before next (make-cast lvar type policy context))))

(defun %insert-cast-before (next cast)
  (declare (type node next) (type cast cast))
  (let ((lvar (cast-value cast)))
    (insert-node-before next cast)
    (setf (lvar-dest lvar) cast)
    (reoptimize-lvar lvar)
    cast))

(defun insert-cast-after (node lvar type policy &optional context)
  (declare (type node node) (type lvar lvar) (type ctype type))
  (with-ir1-environment-from-node node
    (let ((cast (make-cast lvar type policy context)))
      (let ((lvar (cast-value cast)))
        (insert-node-after node cast)
        (setf (lvar-dest lvar) cast)
        (reoptimize-lvar lvar)
        cast))))

(defun insert-ref-before (leaf node)
  (let ((ref (make-ref leaf))
        (lvar (make-lvar node)))
    (insert-node-before node ref)
    (push ref (leaf-refs leaf))
    (setf (leaf-ever-used leaf) t)
    (use-lvar ref lvar)
    lvar))

;;;; miscellaneous shorthand functions

;;; Return the home (i.e. enclosing non-LET) CLAMBDA for NODE. Since
;;; the LEXENV-LAMBDA may be deleted, we must chain up the
;;; LAMBDA-CALL-LEXENV thread until we find a CLAMBDA that isn't
;;; deleted, and then return its home.
(declaim (maybe-inline node-home-lambda))
(defun node-home-lambda (node)
  (declare (type node node))
  (do ((fun (lexenv-lambda (node-lexenv node))
            (lexenv-lambda (lambda-call-lexenv fun))))
      ((not (functional-kind-eq fun deleted zombie))
       (lambda-home fun))
    (when (eq (lambda-home fun) fun)
      (return fun))))

(declaim (maybe-inline node-environment))
(defun node-environment (node)
  (declare (type node node) #-sb-xc-host (inline node-home-lambda))
  (the environment (lambda-environment (node-home-lambda node))))

;;; Return the enclosing cleanup for environment of the first or last
;;; node in BLOCK.
(defun block-start-cleanup (block)
  (node-enclosing-cleanup (block-start-node block)))
(defun block-end-cleanup (block)
  (node-enclosing-cleanup (block-last block)))

;;; Return the non-LET LAMBDA that holds BLOCK's code, or NIL
;;; if there is none.
;;;
;;; There can legitimately be no home lambda in dead code early in the
;;; IR1 conversion process, e.g. when IR1-converting the SETQ form in
;;;   (BLOCK B (RETURN-FROM B) (SETQ X 3))
;;; where the block is just a placeholder during parsing and doesn't
;;; actually correspond to code which will be written anywhere.
(declaim (ftype (sfunction (cblock) (or clambda null)) block-home-lambda-or-null))
(defun block-home-lambda-or-null (block)
  #-sb-xc-host (declare (inline node-home-lambda))
  (if (node-p (block-last block))
      ;; This is the old CMU CL way of doing it.
      (node-home-lambda (block-last block))
      ;; Now that SBCL uses this operation more aggressively than CMU
      ;; CL did, the old CMU CL way of doing it can fail in two ways.
      ;;   1. It can fail in a few cases even when a meaningful home
      ;;      lambda exists, e.g. in IR1-CONVERT of one of the legs of
      ;;      an IF.
      ;;   2. It can fail when converting a form which is born orphaned
      ;;      so that it never had a meaningful home lambda, e.g. a form
      ;;      which follows a RETURN-FROM or GO form.
      (let ((pred-list (block-pred block)))
        ;; To deal with case 1, we reason that
        ;; previous-in-target-execution-order blocks should be in the
        ;; same lambda, and that they seem in practice to be
        ;; previous-in-compilation-order blocks too, so we look back
        ;; to find one which is sufficiently initialized to tell us
        ;; what the home lambda is.
        (if pred-list
            ;; We could get fancy about this, flooding through the
            ;; graph of all the previous blocks, but in practice it
            ;; seems to work just to grab the first previous block and
            ;; use it.
            (node-home-lambda (block-last (first pred-list)))
            ;; In case 2, we end up with an empty PRED-LIST and
            ;; have to punt: There's no home lambda.
            nil))))

;;; Return the non-LET LAMBDA that holds BLOCK's code.
(defun block-home-lambda (block)
  (declare (type cblock block)
           #-sb-xc-host (inline node-home-lambda))
  (node-home-lambda (block-last block)))

;;; Return the IR1 environment for BLOCK.
(defun block-environment (block)
  (declare (type cblock block))
  (lambda-environment (block-home-lambda block)))

(declaim (inline node-stack-allocate-p))
(defun node-stack-allocate-p (node)
  (awhen (node-lvar node)
    (lvar-dynamic-extent it)))

;; If there's a possibility the variable might be unbound, then its
;; references are unflushable.
(defun flushable-reference-p (node)
  (let ((leaf (ref-leaf node)))
    (not (and (global-var-p leaf)
              (member (global-var-kind leaf)
                      '(:special :global :unknown))
              (not (or (always-boundp (leaf-source-name leaf))
                       (policy node (< safety 3))))))))

(defun flushable-callable-arg-p (name arg-count)
  (typecase name
    (null
     t)
    (symbol
     (let* ((info (info :function :info name))
            (attributes (and info
                             (fun-info-attributes info))))
       (and info
            (ir1-attributep attributes flushable)
            (not (ir1-attributep attributes call))
            (let ((type (global-ftype name)))
              (or
               (not (fun-type-p type)) ;; Functions that accept anything, e.g. VALUES
               (multiple-value-bind (min max) (fun-type-arg-limits type)
                 (cond ((and (not min) (not max))
                        nil)
                       ((< arg-count min)
                        nil)
                       ((and max (> arg-count max))
                        nil)
                       (t
                        ;; Just check for T to ensure it won't signal type errors.
                        (not (find *universal-type*
                                   (fun-type-n-arg-types arg-count type)
                                   :test-not #'eql))))))))))))

(defun flushable-combination-args-p (combination info)
  (block nil
    (map-combination-args-and-types
     (lambda (arg type lvars &optional annotation)
       (declare (ignore type lvars))
       (case (car annotation)
         (function-designator
          (let ((fun (or (lvar-fun-name arg t)
                         (and (constant-lvar-p arg)
                              (lvar-value arg)))))
            (unless (and fun
                         (flushable-callable-arg-p fun (length (cadr annotation))))
              (return))))
         (inhibit-flushing
          (let* ((except (cddr annotation)))
            (unless (and except
                         (constant-lvar-p arg)
                         (memq (lvar-value arg) except))
              (return))
            nil))))
     combination
     :info info)
    t))

(defun flushable-combination-p (call)
  (declare (type combination call))
  (let ((kind (combination-kind call))
        (info (combination-fun-info call)))
    (or (when (and (eq kind :known) (fun-info-p info))
          (let ((attr (fun-info-attributes info)))
            (and (if (policy call (= safety 3))
                     (ir1-attributep attr flushable)
                     (ir1-attributep attr unsafely-flushable))
                 (flushable-combination-args-p call info))))
        ;; Is it declared flushable locally?
        (let ((name (lvar-fun-name (combination-fun call) t)))
          (memq name (lexenv-flushable (node-lexenv call)))))))

;;;; DYNAMIC-EXTENT related

;;; Insert code to establish a dynamic extent around CALL, returning
;;; the dynamic extent.
(defun insert-dynamic-extent (call)
  (let* ((dynamic-extent (with-ir1-environment-from-node call
                           (make-dynamic-extent)))
         (cleanup (make-cleanup :dynamic-extent dynamic-extent)))
    (setf (dynamic-extent-cleanup dynamic-extent) cleanup)
    (insert-node-before call dynamic-extent)
    (setf (node-lexenv call)
          (make-lexenv :default (node-lexenv call)
                       :cleanup cleanup))
    ;; Make CALL end its block, so that we have a place to
    ;; insert cleanup code.
    (node-ends-block call)
    (push dynamic-extent
          (lambda-dynamic-extents (node-home-lambda dynamic-extent)))
    dynamic-extent))

(defun use-good-for-dx-p (use dynamic-extent)
  (typecase use
    (combination
     (and (eq (combination-kind use) :known)
          (let ((info (combination-fun-info use)))
            (or (awhen (fun-info-stack-allocate-result info)
                  (funcall it use))
                (awhen (fun-info-result-arg info)
                  (lvar-good-for-dx-p (nth it (combination-args use))
                                      dynamic-extent))))))
    (cast
     (and (not (cast-type-check use))
          (lvar-good-for-dx-p (cast-value use) dynamic-extent)))
    (ref
     (let ((leaf (ref-leaf use)))
       (typecase leaf
         (lambda-var
          ;; LET lambda var with no SETS.
          (when (and (functional-kind-eq (lambda-var-home leaf) let)
                     (not (lambda-var-sets leaf))
                     (lexenv-contains-lambda (lambda-var-home leaf)
                                             (node-lexenv dynamic-extent))
                     ;; Check the other refs are good.
                     (dolist (ref (leaf-refs leaf) t)
                       (unless (eq use ref)
                         (when (not (ref-good-for-dx-p ref))
                           (return nil)))))
            (lvar-good-for-dx-p (let-var-initial-value leaf) dynamic-extent)))
         (clambda
          (aver (functional-kind-eq leaf external))
          (when (and (null (rest (leaf-refs leaf)))
                     (environment-closure (get-lambda-environment leaf))
                     (lexenv-contains-lambda leaf
                                             (node-lexenv dynamic-extent)))
            (aver (eq use (first (leaf-refs leaf))))
            t)))))))

(defun lvar-good-for-dx-p (lvar dynamic-extent)
  (aver (lvar-uses lvar))
  (do-uses (use lvar nil)
    (when (use-good-for-dx-p use dynamic-extent)
      (return t))))

;;; Check that REF delivers a value to a combination which is DX safe
;;; or whose result is that value and ends up being discarded.
(defun ref-good-for-dx-p (ref)
  (let* ((lvar (ref-lvar ref))
         (dest (when lvar (lvar-dest lvar))))
    (and (combination-p dest)
         (case (combination-kind dest)
           (:known
            (awhen (combination-fun-info dest)
              (or (ir1-attributep (fun-info-attributes it) dx-safe)
                  (and (not (combination-lvar dest))
                       (awhen (fun-info-result-arg it)
                         (eql lvar (nth it (combination-args dest))))))))
           (:local
            (loop for arg in (combination-args dest)
                  for var in (lambda-vars (combination-lambda dest))
                  do (when (eq arg lvar)
                       (return
                         (dolist (ref (lambda-var-refs var) t)
                           (unless (ref-good-for-dx-p ref)
                             (return nil)))))
                  finally (sb-impl::unreachable)))))))

(defun lambda-var-ref-lvar (ref)
  (let ((var (ref-leaf ref)))
    (when (and (lambda-var-p var)
               (not (lambda-var-sets var)))
      (let* ((fun (lambda-var-home var))
             (vars (lambda-vars fun))
             (refs (lambda-refs fun))
             (lvar (and refs
                        (null (cdr refs))
                        (ref-lvar (car refs))))
             (combination (and lvar
                               (lvar-dest lvar))))
        (when (and (combination-p combination)
                   (eq (combination-fun combination) lvar))
          (loop for v in vars
                for arg in (combination-args combination)
                when (eq v var)
                  return arg))))))

;;; Return the Top Level Form number of PATH, i.e. the ordinal number
;;; of its original source's top level form in its compilation unit.
(defun source-path-tlf-number (path)
  (declare (list path))
  (car (last path)))

;;; Return the (reversed) list for the PATH in the original source
;;; (with the Top Level Form number last).
(defun source-path-original-source (path)
  (declare (list path) (inline member)
           #-sb-xc-host (values list))
  (cddr (member 'original-source-start path :test #'eq)))

;;; Return the Form Number of PATH's original source inside the Top
;;; Level Form that contains it. This is determined by the order that
;;; we walk the subforms of the top level source form.
(defun source-path-form-number (path)
  (declare (type list path) (inline member)
           #-sb-xc-host (values (or null index)))
  (cadr (member 'original-source-start path :test #'eq)))

;;; Return a list of all the enclosing forms not in the original
;;; source that converted to get to this form, with the immediate
;;; source for node at the start of the list.
(defun source-path-forms (path)
  (subseq path 0 (position 'original-source-start path)))

;;; Return the innermost source form for NODE.
(defun node-source-form (node)
  (declare (type node node))
  (let* ((path (node-source-path node))
         (forms (source-path-forms path)))
    (if forms
        (first forms)
        (values (find-original-source path)))))

;;; Return NODE-SOURCE-FORM, T if lvar has a single use, otherwise
;;; NIL, NIL.
(defun lvar-source (lvar)
  (let ((use (lvar-uses lvar)))
    (if (listp use)
        (values nil nil)
        (values (node-source-form use) t))))

(defun common-suffix (x y)
  (let ((mismatch (mismatch x y :from-end t)))
    (if mismatch
        (subseq x mismatch)
        x)))

;;; If the LVAR has a single use, return NODE-SOURCE-FORM as a
;;; singleton.  Otherwise, return a list of the lowest common
;;; ancestor source form of all the uses (if it can be found),
;;; followed by all the uses' source forms.
(defun lvar-all-sources (lvar)
  (let ((use (principal-lvar-use lvar)))
    (if (listp use)
        (let ((forms  '())
              (path   (node-source-path (first use))))
          (dolist (use use (cons (if (find 'original-source-start path)
                                     (find-original-source path)
                                     "a hairy form")
                                 forms))
            (pushnew (node-source-form use) forms)
            (setf path (common-suffix path
                                      (node-source-path use)))))
        (list (node-source-form use)))))

(defun lvar-uses-all-sources (uses)
  (if (cdr uses)
      (let ((forms  '())
            (path   (node-source-path (first uses))))
        (dolist (use uses (cons (if (find 'original-source-start path)
                                    (find-original-source path)
                                    "a hairy form")
                                forms))
          (pushnew (node-source-form use) forms)
          (setf path (common-suffix path
                                    (node-source-path use)))))
      (list (node-source-form (car uses)))))

;;; Return the LAMBDA that is CTRAN's home, or NIL if there is none.
(declaim (ftype (sfunction (ctran) (or clambda null))
                ctran-home-lambda-or-null))
(defun ctran-home-lambda-or-null (ctran)
  ;; KLUDGE: This function is a post-CMU-CL hack by WHN, and this
  ;; implementation might not be quite right, or might be uglier than
  ;; necessary. It appears that the original Python never found a need
  ;; to do this operation. The obvious things based on
  ;; NODE-HOME-LAMBDA of CTRAN-USE usually work; then if that fails,
  ;; BLOCK-HOME-LAMBDA of CTRAN-BLOCK works, given that we
  ;; generalize it enough to grovel harder when the simple CMU CL
  ;; approach fails, and furthermore realize that in some exceptional
  ;; cases it might return NIL. -- WHN 2001-12-04
  (cond ((ctran-use ctran)
         (node-home-lambda (ctran-use ctran)))
        ((ctran-block ctran)
         (block-home-lambda-or-null (ctran-block ctran)))
        (t
         (bug "confused about home lambda for ~S" ctran))))

;;; Return the LAMBDA that is CTRAN's home.
(declaim (ftype (sfunction (ctran) clambda) ctran-home-lambda))
(defun ctran-home-lambda (ctran)
  (ctran-home-lambda-or-null ctran))

(declaim (inline cast-single-value-p))
(defun cast-single-value-p (cast)
  (not (values-type-p (cast-asserted-type cast))))

(defun %lvar-single-value-p (lvar)
  (let ((dest (lvar-dest lvar)))
    (typecase dest
      (exit
       (lvar-single-value-p (node-lvar dest)))
      (creturn
       nil)
      (mv-combination
       (eq (basic-combination-fun dest) lvar))
      (cast
       (and (cast-single-value-p dest)
            (acond ((node-lvar dest) (%lvar-single-value-p it))
                   (t t))))
      (t t))))

(defun principal-lvar-end (lvar)
  (loop for prev = lvar then (node-lvar dest)
        for dest = (and prev (lvar-dest prev))
        while (cast-p dest)
        finally (return (values dest prev))))

(defun principal-lvar-single-valuify (lvar)
  (loop for prev = lvar then (node-lvar dest)
        for dest = (and prev (lvar-dest prev))
        while (or (cast-p dest)
                  (exit-p dest))
        do (setf (node-derived-type dest)
                 (make-short-values-type (list (single-value-type
                                                (node-derived-type dest)))))
           (reoptimize-lvar prev)))

;;; Return a new LEXENV just like DEFAULT except for the specified
;;; slot values. Values for the alist slots are APPENDed to the
;;; beginning of the current value, rather than replacing it entirely.
(defun make-lexenv (&key (default *lexenv*)
                         funs vars blocks tags
                         type-restrictions
                         (lambda (lexenv-lambda default))
                         (cleanup (lexenv-cleanup default))
                         (handled-conditions (lexenv-handled-conditions default))
                         (disabled-package-locks
                          (lexenv-disabled-package-locks default))
                         (policy (lexenv-policy default))
                         (user-data (lexenv-user-data default))
                         flushable
                         (parent default))
  (macrolet ((frob (var slot)
               `(let ((old (,slot default)))
                  (if ,var
                      (append ,var old)
                      old))))
    (internal-make-lexenv
     (frob funs lexenv-funs)
     (frob vars lexenv-vars)
     (frob blocks lexenv-blocks)
     (frob tags lexenv-tags)
     (frob type-restrictions lexenv-type-restrictions)
     (frob flushable lexenv-flushable)
     lambda
     cleanup handled-conditions disabled-package-locks
     policy
     user-data
     parent)))

;;; Makes a LEXENV, suitable for using in a MACROLET introduced
;;; macroexpander
(defun make-restricted-lexenv (lexenv &optional (policy (lexenv-policy lexenv)))
  (flet ((fun-good-p (fun)
           (destructuring-bind (name . thing) fun
             (declare (ignore name))
             (etypecase thing
               (functional nil)
               (global-var t)
               (cons (aver (eq (car thing) 'macro))
                     t))))
         (var-good-p (var)
           (destructuring-bind (name . thing) var
             (declare (ignore name))
             (etypecase thing
               ;; The evaluator will mark lexicals with :BOGUS when it
               ;; translates an interpreter lexenv to a compiler
               ;; lexenv.
               ((or leaf #+sb-eval (member :bogus)) nil)
               (cons (aver (eq (car thing) 'macro))
                     t)
               (heap-alien-info nil)))))
    (internal-make-lexenv
     (remove-if-not #'fun-good-p (lexenv-funs lexenv))
     (remove-if-not #'var-good-p (lexenv-vars lexenv))
     nil
     nil
     (lexenv-type-restrictions lexenv) ; XXX
     nil
     nil
     nil
     (lexenv-handled-conditions lexenv)
     (lexenv-disabled-package-locks lexenv)
     policy
     (lexenv-user-data lexenv)
     lexenv)))

;;;; flow/DFO/component hackery

;;; Join BLOCK1 and BLOCK2.
(defun link-blocks (block1 block2)
  (declare (type cblock block1 block2))
  (setf (block-succ block1)
        (if (block-succ block1)
            (%link-blocks block1 block2)
            (list block2)))
  (push block1 (block-pred block2))
  (values))
(defun %link-blocks (block1 block2)
  (declare (type cblock block1 block2))
  (let ((succ1 (block-succ block1)))
    (aver (not (memq block2 succ1)))
    (cons block2 succ1)))

;;; This is like LINK-BLOCKS, but we separate BLOCK1 and BLOCK2.
(defun unlink-blocks (block1 block2)
  (declare (type cblock block1 block2))
  (let ((succ1 (block-succ block1)))
    (if (eq block2 (car succ1))
        (setf (block-succ block1) (cdr succ1))
        (do ((succ (cdr succ1) (cdr succ))
             (prev succ1 succ))
            ((eq (car succ) block2)
             (setf (cdr prev) (cdr succ)))
          (aver succ))))

  (setf (block-pred block2)
        (delq1 block1 (block-pred block2)))
  (values))

;;; Swing the succ/pred link between BLOCK and OLD to be between BLOCK
;;; and NEW. If BLOCK ends in an IF, then we have to fix up the
;;; consequent/alternative blocks to point to NEW.
(defun change-block-successor (block old new)
  (declare (type cblock new old block))
  (unlink-blocks block old)
  (let ((last (block-last block))
        (comp (block-component block)))
    (setf (component-reanalyze comp) t)
    (typecase last
      (cif
       (let* ((succ-left (block-succ block))
              (new (if (and (eq new (component-tail comp))
                            succ-left)
                       (first succ-left)
                       new)))
         (unless (memq new succ-left)
           (link-blocks block new))
         (macrolet ((frob (slot)
                      `(when (eq (,slot last) old)
                         (setf (,slot last) new))))
           (frob if-consequent)
           (frob if-alternative)
           (when (eq (if-consequent last)
                     (if-alternative last))
             (reoptimize-component (block-component block) t)))))
      (jump-table
       (setf (jump-table-targets last)
             (if (eq new (component-tail comp))
                 (delete old (jump-table-targets last) :key #'cdr :test #'eq)
                 (prog1
                     (loop for (index . target) in (jump-table-targets last)
                           collect (cons index (if (eq target old)
                                                   new
                                                   target)))
                   (unless (memq new (block-succ block))
                     (link-blocks block new)))))
       (unless (cdr (block-succ block))
         (flush-dest (jump-table-index last))
         (unlink-node last)))
      (t
       (unless (memq new (block-succ block))
         (link-blocks block new)))))

  (values))

(defun join-blocks-if-possible (component)
  (do-blocks (block component)
    (loop while
          (and (singleton-p (block-succ block))
               (join-successor-if-possible block t)))))

;;; Try to join with a successor block. If we succeed, we return true,
;;; otherwise false.
(defun join-successor-if-possible (block &optional local-calls)
  (declare (type cblock block))
  (let ((next (first (block-succ block))))
    (when (block-start next)  ; NEXT is not an END-OF-COMPONENT marker
      (cond ( ;; We cannot combine with a successor block if:
             (or
              ;; the successor has more than one predecessor;
              (rest (block-pred next))
              ;; the successor is the current block (infinite loop);
              (eq next block)
              ;; the next block has a different cleanup, and thus
              ;; we may want to insert cleanup code between the
              ;; two blocks at some point;
              (not (eq (block-end-cleanup block)
                       (block-start-cleanup next)))
              ;; the next block has a different home lambda, and
              ;; thus the control transfer is a non-local exit.
              (not (eq (block-home-lambda block)
                       (block-home-lambda next)))
              (neq (block-type-check block)
                   (block-type-check next))
              (and (not local-calls)
                   (let ((last (block-last block)))
                     (and (combination-p last)
                          (eq (combination-kind last) :local)
                          (functional-kind-eq (combination-lambda last)
                                              nil assignment optional cleanup)))))
             nil)
            (t
             (join-blocks block next)
             t)))))

;;; Join together two blocks. The code in BLOCK2 is moved into BLOCK1
;;; and BLOCK2 is deleted from the DFO. We combine the optimize flags
;;; for the two blocks so that any indicated optimization gets done.
(defun join-blocks (block1 block2)
  (declare (type cblock block1 block2))
  (let* ((last1 (block-last block1))
         (last2 (block-last block2))
         (succ (block-succ block2))
         (start2 (block-start block2)))
    (do ((ctran start2 (node-next (ctran-next ctran))))
        ((not ctran))
      (setf (ctran-block ctran) block1))

    (unlink-blocks block1 block2)
    (dolist (block succ)
      (unlink-blocks block2 block)
      (link-blocks block1 block))

    (setf (ctran-kind start2) :inside-block)
    (setf (node-next last1) start2)
    (setf (ctran-use start2) last1)
    (setf (block-last block1) last2))

  (setf (block-flags block1)
        (attributes-union (block-flags block1)
                          (block-flags block2)))

  (let ((next (block-next block2))
        (prev (block-prev block2)))
    (setf (block-next prev) next)
    (setf (block-prev next) prev))

  (values))

;;; Utility: return T if both argument cblocks are equivalent.  For now,
;;; detect only blocks that read the same leaf into the same lvar, and
;;; continue to the same block.
(defun blocks-equivalent-p (x y)
  (declare (type cblock x y))
  (let ((ref-x (single-ref-block-p x))
        (ref-y (single-ref-block-p y)))
    (and ref-x
         ref-y
         (equal (block-succ x) (block-succ y))
         (eq (ref-lvar ref-x) (ref-lvar ref-y))
         (let ((leaf-x (ref-leaf ref-x))
               (leaf-y (ref-leaf ref-y)))
           (or (eq leaf-x leaf-y)
               (and (constant-p leaf-x)
                    (constant-p leaf-y)
                    (eq (constant-value leaf-x)
                        (constant-value leaf-y)))))
         (eq (node-enclosing-cleanup ref-x)
             (node-enclosing-cleanup ref-y)))))

(defun single-ref-block-p (block)
  (let ((start (block-start block)))
    (when start
      (let ((node (ctran-next start)))
        (and (ref-p node)
             (eq (block-last block) node)
             node)))))

;;; (if x x nil)
(defun if-test-redundant-p (test con alt)
  (let ((ref-alt (single-ref-block-p alt))
        (ref-con (single-ref-block-p con))
        (ref-test (lvar-uses test)))
    (and (ref-p ref-test)
         ref-con
         ref-alt
         (equal (block-succ alt) (block-succ con))
         (eq (ref-lvar ref-alt) (ref-lvar ref-con))
         (same-ref-p ref-test ref-con)
         (and (constant-p (ref-leaf ref-alt))
              (null (constant-value (ref-leaf ref-alt))))
         (eq (node-enclosing-cleanup ref-alt)
             (node-enclosing-cleanup ref-con)))))

;;; If a block consisting of a single ref is equivalent to another
;;; block with the same ref and the have the same successor it can be
;;; removed.
;;;
;;; Removing more is tricky, debugging will suffer, and code relying
;;; on constraint propagation will break, e.g.
;;; (if (simple-vector-p x) (aref x 0) (aref x 0)))
;;; 344d4d778 contains some code that handles combinations and casts.
(defun remove-equivalent-blocks (block)
  (let ((pred (block-pred block)))
    (when (cdr pred)
      (loop for (block1 . rest) on pred
            when (and (not (block-delete-p block1))
                      (single-ref-block-p block1))
            do
            (loop for block2 in rest
                  when (and (not (block-delete-p block1))
                            (blocks-equivalent-p block1 block2))
                  do
                  (let* ((ref1 (block-start-node block1))
                         (ref2 (block-start-node block2))
                         (type1 (node-derived-type ref1))
                         (type2 (node-derived-type ref2)))
                    ;; Constraint propagation may have given the
                    ;; references different types. Join them back.
                    (unless (eq type1 type2)
                      (derive-node-type ref1
                                        (values-type-union type1 type2)
                                        :from-scratch t)))
                  (loop for pred in (block-pred block2)
                        do
                        (change-block-successor pred block2 block1))
                  (delete-block block2 t))))))

;;; Unlink a block from the next/prev chain. We also null out the
;;; COMPONENT.
(declaim (inline remove-from-dfo))
(defun remove-from-dfo (block)
  (declare (type cblock block))
  (let ((next (block-next block))
        (prev (block-prev block)))
    (setf (block-component block) nil)
    (setf (block-next prev) next)
    (setf (block-prev next) prev))
  (values))

;;; Add BLOCK to the next/prev chain following AFTER. We also set the
;;; COMPONENT to be the same as for AFTER.
(declaim (inline add-to-dfo))
(defun add-to-dfo (block after)
  (declare (type cblock block after))
  (let ((next (block-next after))
        (comp (block-component after)))
    (aver (not (eq (component-kind comp) :deleted)))
    (setf (block-component block) comp)
    (setf (block-next after) block)
    (setf (block-prev block) after)
    (setf (block-next block) next)
    (setf (block-prev next) block))
  (values))

;;; Set the FLAG for all the blocks in COMPONENT to NIL, except for
;;; the head and tail which are set to T.
(defun clear-flags (component)
  (declare (type component component))
  (let ((head (component-head component))
        (tail (component-tail component)))
    (setf (block-flag head) t)
    (setf (block-flag tail) t)
    (do-blocks (block component)
      (setf (block-flag block) nil)))
  (values))

;;; Make a component with no blocks in it. The BLOCK-FLAG is initially
;;; true in the head and tail blocks.
(defun make-empty-component ()
  #-sb-xc-host (declare (values component))
  (let* ((head (make-block-key :start nil :component nil))
         (tail (make-block-key :start nil :component nil))
         (res (make-component head tail)))
    (setf (block-flag head) t)
    (setf (block-flag tail) t)
    (setf (block-component head) res)
    (setf (block-component tail) res)
    (setf (block-next head) tail)
    (setf (block-prev tail) head)
    res))

;;; Make NODE the LAST node in its block, splitting the block if necessary.
;;; The new block is added to the DFO immediately following NODE's block.
;;; Returns the new block if it's created.
(defun node-ends-block (node)
  (declare (type node node))
  (let* ((block (node-block node))
         (start (node-next node))
         (last (block-last block)))
    (unless (eq last node)
      (aver (and (eq (ctran-kind start) :inside-block)
                 (not (block-delete-p block))))
      (let* ((succ (block-succ block))
             (new-block
               (make-block-key :start start
                               :component (block-component block)
                               :succ succ :last last)))
        (setf (block-type-check new-block)
              (block-type-check block))
        (setf (ctran-kind start) :block-start)
        (setf (ctran-use start) nil)
        (setf (block-last block) node)
        (setf (node-next node) nil)
        (dolist (b succ)
          (setf (block-pred b)
                (nsubstitute new-block block (block-pred b)
                             :count 1)))
        (setf (block-succ block) ())
        (link-blocks block new-block)
        (add-to-dfo new-block block)
        (setf (component-reanalyze (block-component block)) t)

        (do ((ctran start (node-next (ctran-next ctran))))
            ((not ctran))
          (setf (ctran-block ctran) new-block))
        new-block))))


;;;; deleting stuff

(declaim (start-block delete-ref delete-functional flush-node flush-dest
                      delete-lvar delete-block delete-block-lazily delete-lambda
                      mark-for-deletion))

;;; Deal with deleting the last (read) reference to a LAMBDA-VAR.
(defun delete-lambda-var (leaf)
  (declare (type lambda-var leaf))

  (setf (lambda-var-deleted leaf) t)
  ;; Iterate over all local calls flushing the corresponding argument,
  ;; allowing the computation of the argument to be deleted. We also
  ;; mark the LET for reoptimization, since it may be that we have
  ;; deleted its last variable.
  (let* ((fun (lambda-var-home leaf))
         (n (position leaf (lambda-vars fun))))
    (dolist (ref (leaf-refs fun))
      (let* ((lvar (node-lvar ref))
             (dest (and lvar (lvar-dest lvar))))
        (when (and (basic-combination-p dest)
                   (eq (basic-combination-fun dest) lvar)
                   (eq (basic-combination-kind dest) :local))
          (cond ((mv-combination-p dest)
                 ;; Let FLUSH-DEAD-CODE deal with it
                 ;; since it's a bit tricky to delete multiple-valued
                 ;; args and existing code doesn't expect to see NIL in
                 ;; mv-combination-args.
                 (reoptimize-node dest)
                 (setf (block-flush-p (node-block dest)) t))
                (t
                 (let* ((args (basic-combination-args dest))
                        (arg (elt args n)))
                   (reoptimize-lvar arg)
                   (flush-dest arg)
                   (setf (elt args n) nil))))))))

  ;; The LAMBDA-VAR may still have some SETs, but this doesn't cause
  ;; too much difficulty, since we can efficiently implement
  ;; write-only variables. We iterate over the SETs, marking their
  ;; blocks for dead code flushing, since we can delete SETs whose
  ;; value is unused.
  (dolist (set (lambda-var-sets leaf))
    (setf (block-flush-p (node-block set)) t))

  (values))

;;; Delete a function that has no references. This need only be called
;;; on functions that never had any references, since otherwise
;;; DELETE-REF will handle the deletion.
(defun delete-functional (fun)
  (aver (and (null (leaf-refs fun))
             (not (functional-entry-fun fun))))
  (etypecase fun
    (optional-dispatch (delete-optional-dispatch fun))
    (clambda (delete-lambda fun)))
  (values))

(defun lambda-ever-used-p (lambda)
  (let ((optional-dispatch (lambda-optional-dispatch lambda)))
    (if optional-dispatch
        (or (leaf-ever-used optional-dispatch)
            ;; Warn only for the main entry
            (not (eq (optional-dispatch-main-entry optional-dispatch)
                     lambda)))
        (leaf-ever-used lambda))))

;;; Deal with deleting the last reference to a CLAMBDA, which means
;;; that the lambda is unreachable, so that its body may be
;;; deleted. We set FUNCTIONAL-KIND to :DELETED and rely on
;;; IR1-OPTIMIZE to delete its blocks.
(defun delete-lambda (clambda)
  (declare (type clambda clambda))
  (let ((original-kind (functional-kind clambda))
        (bind (lambda-bind clambda)))
    (aver (not (logtest original-kind (functional-kind-attributes deleted toplevel))))
    (aver (not (functional-has-external-references-p clambda)))
    (aver (or (eql original-kind (functional-kind-attributes zombie)) bind))
    (setf (functional-kind clambda) (functional-kind-attributes deleted))
    (setf (lambda-bind clambda) nil)

    ;; (The IF test is (FUNCTIONAL-SOMEWHAT-LETLIKE-P CLAMBDA), except
    ;; that we're using the old value of the KIND slot, not the
    ;; current slot value, which has now been set to :DELETED.)
    (cond
      ((eql original-kind (functional-kind-attributes zombie)))
      ((logtest original-kind (functional-kind-attributes let mv-let assignment))
       (let ((bind-block (node-block bind)))
         (mark-for-deletion bind-block))
       (let ((home (lambda-home clambda)))
         (setf (lambda-lets home) (delete clambda (lambda-lets home)))))
      (t
       ;; Function has no reachable references.
       (dolist (ref (lambda-refs clambda))
         (mark-for-deletion (node-block ref)))
       ;; If the function isn't a LET, we unlink the function head
       ;; and tail from the component head and tail to indicate that
       ;; the code is unreachable. We also delete the function from
       ;; COMPONENT-LAMBDAS (it won't be there before local call
       ;; analysis, but no matter.) If the lambda was never
       ;; referenced, we give a note.
       (let* ((bind-block (node-block bind))
              (component (block-component bind-block))
              (return (lambda-return clambda))
              (return-block (and return (node-block return))))
         (unless (lambda-ever-used-p clambda)
           (let ((*compiler-error-context* bind))
             (compiler-notify 'code-deletion-note
                              :format-control "deleting unused function~:[.~;~:*~%  ~S~]"
                              :format-arguments (list (leaf-debug-name clambda)))))
         (unless (block-delete-p bind-block)
           (unlink-blocks (component-head component) bind-block))
         (when (and return-block (not (block-delete-p return-block)))
           (mark-for-deletion return-block)
           (unlink-blocks return-block (component-tail component)))
         (setf (component-reanalyze component) t)
         (let ((tails (lambda-tail-set clambda)))
           (setf (tail-set-funs tails)
                 (delete clambda (tail-set-funs tails)))
           (setf (lambda-tail-set clambda) nil))
         (setf (component-lambdas component)
               (delq1 clambda (component-lambdas component))))))

    ;; If the lambda is an XEP, then we null out the ENTRY-FUN in its
    ;; ENTRY-FUN so that people will know that it is not an entry
    ;; point anymore.
    (when (eql original-kind (functional-kind-attributes external))
      (let ((fun (functional-entry-fun clambda)))
        (setf (functional-entry-fun fun) nil)
        (when (optional-dispatch-p fun)
          (delete-optional-dispatch fun)))))

  (values))

;;; Deal with deleting the last reference to an OPTIONAL-DISPATCH. We
;;; have to be a bit more careful than with lambdas, since DELETE-REF
;;; is used both before and after local call analysis. Afterward, all
;;; references to still-existing OPTIONAL-DISPATCHes have been moved
;;; to the XEP, leaving it with no references at all. So we look at
;;; the XEP to see whether an optional-dispatch is still really being
;;; used. But before local call analysis, there are no XEPs, and all
;;; references are direct.
;;;
;;; When we do delete the OPTIONAL-DISPATCH, we grovel all of its
;;; entry-points, making them be normal lambdas, and then deleting the
;;; ones with no references. This deletes any e-p lambdas that were
;;; either never referenced, or couldn't be deleted when the last
;;; reference was deleted (due to their :OPTIONAL kind.)
;;;
;;; Note that the last optional entry point may alias the main entry,
;;; so when we process the main entry, its KIND may have been changed
;;; to NIL or even converted to a LETlike value.
(defun delete-optional-dispatch (leaf)
  (declare (type optional-dispatch leaf))
  (let ((entry (functional-entry-fun leaf)))
    (unless (and entry
                 (or (leaf-refs entry)
                     (functional-kind-eq entry external)))
      (aver (or (not entry) (functional-kind-eq entry deleted)))
      (setf (functional-kind leaf) (functional-kind-attributes deleted))

      (flet ((frob (fun)
               (unless (functional-kind-eq fun deleted)
                 (aver (functional-kind-eq fun optional))
                 (setf (functional-kind fun) (functional-kind-attributes nil))
                 (if (leaf-refs fun)
                     (or (maybe-let-convert fun)
                         (maybe-convert-to-assignment fun)
                         (reoptimize-lambda fun))
                     (delete-lambda fun)))))
        (dolist (ep (optional-dispatch-entry-points leaf))
          (when (promise-ready-p ep)
            (frob (force ep))))
        (when (optional-dispatch-more-entry leaf)
          (frob (optional-dispatch-more-entry leaf)))
        (let ((main (optional-dispatch-main-entry leaf)))
          (when entry
            (setf (functional-entry-fun entry) main)
            (setf (functional-entry-fun main) entry))
          (when (functional-kind-eq main optional)
            (frob main))))))

  (values))

;; Trigger PROPAGATE-LOCAL-CALL-ARGS
(defun reoptimize-lambda (fun)
  (loop for ref in (leaf-refs fun)
        for dest = (node-dest ref)
        when (basic-combination-p dest)
        do (reoptimize-node dest)
           (loop for arg in (basic-combination-args dest)
                 do (reoptimize-lvar arg))))

;;; Do stuff to delete the semantic attachments of a REF node. When
;;; this leaves zero or one reference, we do a type dispatch off of
;;; the leaf to determine if a special action is appropriate.
(defun delete-ref (ref)
  (declare (type ref ref))
  (let* ((leaf (ref-leaf ref))
         (refs (delq1 ref (leaf-refs leaf)))
         (home (node-home-lambda ref)))
    (setf (leaf-refs leaf) refs)
    (when (and (typep leaf '(or clambda lambda-var))
               (not (find home refs :key #'node-home-lambda)))
      ;; It was the last reference from this lambda, remove it
      (sset-delete leaf (lambda-calls-or-closes home)))
    (if refs
        (typecase leaf
          (clambda (or (maybe-let-convert leaf)
                       (maybe-convert-to-assignment leaf)
                       (reoptimize-lambda leaf)))
          (lambda-var (reoptimize-lambda-var leaf)))
        (typecase leaf
          (lambda-var
           (delete-lambda-var leaf))
          (clambda
           (functional-kind-ecase leaf
             ((nil let mv-let assignment escape cleanup)
              (delete-lambda leaf))
             (external
              (unless (functional-has-external-references-p leaf)
                (delete-lambda leaf)))
             ((deleted zombie optional))))
          (optional-dispatch
           (unless (functional-kind-eq leaf deleted)
             (delete-optional-dispatch leaf))))))

  (values))

;;; This function is called to unlink a node from its LVAR;
;;; we assume that the LVAR's USE list has already been updated,
;;; and that we only have to mark the node as up for dead code
;;; elimination, and to clear it LVAR slot.
(defun flush-node (node)
  (declare (type node node))
  (let* ((prev (node-prev node))
         (block (ctran-block prev)))
    (reoptimize-component (block-component block) t)
    (setf (block-flush-p block) t))
  (setf (node-lvar node) nil))

;;; This function is called by people who delete nodes; it provides a
;;; way to indicate that the value of a lvar is no longer used. We
;;; null out the LVAR-DEST, set FLUSH-P in the blocks containing uses
;;; of LVAR and set COMPONENT-REOPTIMIZE.
(defun flush-dest (lvar)
  (declare (type (or lvar null) lvar))
  (unless (null lvar)
    (setf (lvar-dest lvar) nil)
    (do-uses (use lvar)
      (flush-node use))
    (setf (lvar-uses lvar) nil))
  (values))

;;; Queue the block for deletion
(defun delete-block-lazily (block)
  (declare (type cblock block))
  (unless (block-delete-p block)
    (setf (block-delete-p block) t)
    (push block (component-delete-blocks (block-component block)))))

;;; Do a graph walk backward from BLOCK, marking all predecessor
;;; blocks with the DELETE-P flag.
(defun mark-for-deletion (block)
  (declare (type cblock block))
  (let* ((component (block-component block))
         (head (component-head component)))
    (labels ((helper (block)
               (delete-block-lazily block)
               (dolist (pred (block-pred block))
                 (unless (or (block-delete-p pred)
                             (eq pred head))
                   (helper pred)))))
      (unless (block-delete-p block)
        (helper block)
        (setf (component-reanalyze component) t))))
  (values))

;;; This function does what is necessary to eliminate the code in it
;;; from the IR1 representation. This involves unlinking it from its
;;; predecessors and successors and deleting various node-specific
;;; semantic information. BLOCK must be already removed from
;;; COMPONENT-DELETE-BLOCKS.
(defun delete-block (block &optional silent)
  (declare (type cblock block))
  (unless (block-component block)
    ;; Already deleted
    (return-from delete-block))
  #+high-security (aver (not (memq block (component-delete-blocks (block-component block)))))
  (unless silent
    (note-block-deletion block))
  (setf (block-delete-p block) t)

  (dolist (b (block-pred block))
    (unlink-blocks b block)
    ;; In bug 147 the almost-all-blocks-have-a-successor invariant was
    ;; broken when successors were deleted without setting the
    ;; BLOCK-DELETE-P flags of their predececessors. Make sure that
    ;; doesn't happen again.
    (aver (not (and (null (block-succ b))
                    (not (block-delete-p b))
                    (not (eq b (component-head (block-component b))))))))
  (dolist (b (block-succ block))
    (unlink-blocks block b))

  (do-nodes-carefully (node block)
    (when (valued-node-p node)
      (delete-lvar-use node))
    (etypecase node
      (ref (delete-ref node))
      (cif (flush-dest (if-test node)))
      (jump-table (flush-dest (jump-table-index node)))
      ;; The next two cases serve to maintain the invariant that a LET
      ;; always has a well-formed COMBINATION, REF and BIND. We delete
      ;; the lambda whenever we delete any of these, but we must be
      ;; careful that this LET has not already been partially deleted.
      (basic-combination
       (when (and (eq (basic-combination-kind node) :local)
                  ;; Guards COMBINATION-LAMBDA agains the REF being deleted.
                  (lvar-uses (basic-combination-fun node)))
         (let ((fun (combination-lambda node)))
           ;; If our REF was the second-to-last ref, and has been
           ;; deleted, then FUN may be a LET for some other
           ;; combination.
           (when (and (functional-letlike-p fun)
                      (eq (let-combination fun) node))
             (delete-lambda fun))))
       (flush-dest (basic-combination-fun node))
       (dolist (arg (basic-combination-args node))
         (when arg (flush-dest arg))))
      (bind
       (let ((lambda (bind-lambda node)))
         (unless (functional-kind-eq lambda deleted)
           (delete-lambda lambda))))
      (exit
       (let ((value (exit-value node))
             (entry (exit-entry node)))
         (when value
           (flush-dest value))
         (when entry
           (setf (entry-exits entry)
                 (delq1 node (entry-exits entry))))))
      (entry
       (dolist (exit (entry-exits node))
         (mark-for-deletion (node-block exit)))
       (let ((home (node-home-lambda node)))
         (setf (lambda-entries home) (delq1 node (lambda-entries home)))))
      (creturn
       (flush-dest (return-result node))
       (delete-return node))
      (cset
       (flush-dest (set-value node))
       (let ((var (set-var node)))
         (setf (basic-var-sets var)
               (delete node (basic-var-sets var)))))
      (cast
       (flush-dest (cast-value node)))
      (enclose)
      (cdynamic-extent)))

  (remove-from-dfo block)
  (values))

(declaim (end-block))

;;; Do stuff to indicate that the return node NODE is being deleted.
(defun delete-return (node)
  (declare (type creturn node))
  (let* ((fun (return-lambda node))
         (tail-set (lambda-tail-set fun)))
    (aver (lambda-return fun))
    (setf (lambda-return fun) nil)
    (when (and tail-set (not (find-if #'lambda-return
                                      (tail-set-funs tail-set))))
      (setf (tail-set-type tail-set) *empty-type*)))
  (values))

;;; If any of the VARS in FUN was never referenced and was not
;;; declared IGNORE, then complain.
(defun note-unreferenced-vars (vars policy)
  (dolist (var vars)
    (unless (or (eq (leaf-ever-used var) t) (lambda-var-ignorep var))
      (let ((*lexenv* (if (node-p *compiler-error-context*)
                          (node-lexenv *compiler-error-context*)
                          *lexenv*))
            (*compiler-error-context*
              (or (get-source-path (lambda-var-source-form var))
                  *compiler-error-context*)))
       (unless (policy policy (= inhibit-warnings 3))
         ;; ANSI section "3.2.5 Exceptional Situations in the Compiler"
         ;; requires this to be no more than a STYLE-WARNING.
         ;; There's no reason to accept this kind of equivocation
         ;; when compiling our own code, though.
         (#-sb-xc-host compiler-style-warn #+sb-xc-host warn
          (if (eq (leaf-ever-used var) 'set)
              "The variable ~S is assigned but never read."
              "The variable ~S is defined but never used.")
          (leaf-debug-name var))))
      (setf (leaf-ever-used var) t)))) ; to avoid repeated warnings? -- WHN

(defun note-unreferenced-fun-vars (fun)
  (declare (type clambda fun))
  (let ((*compiler-error-context* (lambda-bind fun)))
    (note-unreferenced-vars (lambda-vars fun)
                            *compiler-error-context*))
  (values))

;;; Note that something interesting has happened to VAR.
(defun reoptimize-lambda-var (var)
  (declare (type lambda-var var))
  (let ((fun (lambda-var-home var)))
    ;; We only deal with LET variables, marking the corresponding
    ;; initial value arg as needing to be reoptimized.
    (when (and (functional-kind-eq fun let)
               (leaf-refs var))
      (do ((args (basic-combination-args
                  (lvar-dest (node-lvar (first (leaf-refs fun)))))
                 (cdr args))
           (vars (lambda-vars fun) (cdr vars)))
          ((eq (car vars) var)
           (reoptimize-lvar (car args))))))
  (values))

;;; Return true if we can find OBJ in FORM, NIL otherwise. We bound
;;; our recursion so that we don't get lost in circular structures. We
;;; ignore the car of forms if they are a symbol (to prevent confusing
;;; function referencess with variables), and we also ignore anything
;;; inside ' or #'.
(defun present-in-form (obj form depth)
  (declare (type (integer 0 20) depth))
  (cond ((= depth 20) nil)
        ((eq obj form) t)
        ((atom form) nil)
        (t
         (let ((first (car form))
               (depth (1+ depth)))
           (if (member first '(quote function))
               nil
               (or (and (not (symbolp first))
                        (present-in-form obj first depth))
                   (do ((l (cdr form) (cdr l))
                        (n 0 (1+ n)))
                       ((or (atom l) (> n 100))
                        nil)
                     (declare (fixnum n))
                     (when (present-in-form obj (car l) depth)
                       (return t)))))))))

;;; This function is called on a block immediately before we delete
;;; it. We check to see whether any of the code about to die appeared
;;; in the original source, and emit a note if so.
;;;
;;; If the block was in a lambda is now deleted, then we ignore the
;;; whole block, since this case is picked off in DELETE-LAMBDA. We
;;; also ignore the deletion of CRETURN nodes, since it is somewhat
;;; reasonable for a function to not return, and there is a different
;;; note for that case anyway.
;;;
;;; If the actual source is an atom, then we use a bunch of heuristics
;;; to guess whether this reference really appeared in the original
;;; source:
;;; -- If a symbol, it must be interned and not a keyword.
;;; -- It must not be an easily introduced constant (T or NIL, a fixnum
;;;    or a character.)
;;; -- The atom must be "present" in the original source form, and
;;;    present in all intervening actual source forms.
(defun note-block-deletion (block)
  (let ((home (block-home-lambda block)))
    (unless (or (functional-kind-eq home deleted)
                (block-delete-p (lambda-block home)))
      (do-nodes (node nil block)
        (let* ((path (node-source-path node))
               (ctran-path (ctran-source-path (node-prev node))))
          (flet ((visible-p (path)
                   (let ((first (first path)))
                     (or (eq first 'original-source-start)
                         (and (atom first)
                              (or (not (symbolp first))
                                  (let ((pkg (cl:symbol-package first)))
                                    (and pkg (neq pkg *keyword-package*))))
                              (not (member first '(t nil)))
                              (not (cl:typep first '(or fixnum character
                                                     #+64-bit single-float)))
                              (every (lambda (x)
                                       (present-in-form first x 0))
                                     (source-path-forms path))
                              (present-in-form first (find-original-source path)
                                               0))))))
            (cond ((and ctran-path
                        (visible-p ctran-path))
                   (push (cons ctran-path (node-lexenv node))
                         (deleted-source-paths *compilation*))
                   (return))
                  ((and (not (return-p node))
                        ;; CASTs are just value filters and do not
                        ;; represent code and they can be moved around
                        ;; making CASTs from the original source code
                        ;; appear in code inserted by the compiler, generating
                        ;; false deletion notes.
                        ;; And if a block with the original source gets
                        ;; deleted the node that produces the value for
                        ;; the CAST will get a note, no need to note
                        ;; twice.
                        (not (cast-p node))
                        ;; Nothing interesting in BIND nodes
                        (not (bind-p node))
                        ;; Try to get the outer deleted node.
                        (not (and (valued-node-p node)
                                  (let ((dest (node-dest node)))
                                    (and dest
                                         (node-to-be-deleted-p dest)
                                         (node-source-inside-p node dest)))))
                        (visible-p path))
                   (push (cons path (node-lexenv node))
                         (deleted-source-paths *compilation*))
                   (return))))))))
  (values))

(defun node-source-inside-p (inner-node outer-node)
  (tailp (source-path-original-source (node-source-path outer-node))
         (source-path-original-source (node-source-path inner-node))))

(defun report-code-deletion ()
  (let ((forms (make-hash-table :test #'equal))
        (reversed-path))
    ;; Report only the outermost form
    (loop for pair in (shiftf (deleted-source-paths *compilation*) nil)
          for (path) = pair
          do
          (when (eq (car path) 'original-source-start)
            (setf (gethash (source-path-original-source path) forms) path))
          (push pair reversed-path))
    (loop for (path . lexenv) in reversed-path
          for original = (source-path-original-source path)
          when (loop for outer on (if (eq (car path) 'original-source-start)
                                      (cdr original)
                                      original)
                     never (gethash outer forms))
          do
          (let ((*current-path* path)
                (*lexenv* lexenv))
            (compiler-notify 'code-deletion-note
                             :format-control "deleting unreachable code"
                             :format-arguments nil)))))

(defun maybe-reoptimize-previous-node (ctran block)
  (flet ((maybe-reoptimize (node)
           (when (basic-combination-p node)
             (let ((fun-info (basic-combination-fun-info node)))
               (when (and fun-info
                          (ir1-attributep (fun-info-attributes fun-info)
                                          reoptimize-when-unlinking))
                 (reoptimize-node node))))))
   (case (ctran-kind ctran)
     (:inside-block
      (maybe-reoptimize (ctran-use ctran)))
     (:block-start
      (dolist (pred (block-pred block))
        (maybe-reoptimize (block-last pred)))))))

;;; Delete a node from a block, deleting the block if there are no
;;; nodes left. We remove the node from the uses of its LVAR.
;;;
;;; If the node is the last node, there must be exactly one successor.
;;; We link all of our precedessors to the successor and unlink the
;;; block. In this case, we return T, otherwise NIL. If no nodes are
;;; left, and the block is a successor of itself, then we replace the
;;; only node with a degenerate exit node. This provides a way to
;;; represent the bodyless infinite loop, given the prohibition on
;;; empty blocks in IR1.
(defun unlink-node (node)
  (declare (type node node))
  (when (valued-node-p node)
    (delete-lvar-use node))
  (let* ((ctran (node-next node))
         (next (and ctran (ctran-next ctran)))
         (prev (node-prev node))
         (block (ctran-block prev))
         (prev-kind (ctran-kind prev))
         (last (block-last block)))
    (maybe-reoptimize-previous-node prev block)
    (cond ((or (eq prev-kind :inside-block)
               (and (eq prev-kind :block-start)
                    (not (eq node last))))
           (cond ((eq node last)
                  (setf (block-last block) (ctran-use prev))
                  (setf (node-next (ctran-use prev)) nil))
                 (t
                  (setf (ctran-next prev) next)
                  (setf (node-prev next) prev)
                  (unless (ctran-source-path prev)
                    (setf (ctran-source-path prev) (ctran-source-path ctran)))
                  (when (if-p next)
                    (reoptimize-lvar (if-test next)))))
           (setf (node-prev node) nil)
           nil)
          (t
           (aver (eq prev-kind :block-start))
           (aver (eq node last))
           (let* ((succ (block-succ block))
                  (next (first succ)))
             (aver (singleton-p succ))
             (cond
               ((eq block (first succ))
                (with-ir1-environment-from-node node
                  (let ((exit (make-exit)))
                    (setf (ctran-next prev) nil)
                    (link-node-to-previous-ctran exit prev)
                    (setf (block-last block) exit)))
                (setf (node-prev node) nil)
                nil)
               (t
                (aver (eq (block-start-cleanup block)
                          (block-end-cleanup block)))
                (unlink-blocks block next)
                (dolist (pred (block-pred block))
                  (change-block-successor pred block next))
                (when (block-delete-p block)
                  (let ((component (block-component block)))
                    (setf (component-delete-blocks component)
                          (delq1 block (component-delete-blocks component)))))
                (remove-from-dfo block)
                (setf (block-delete-p block) t)
                (setf (node-prev node) nil)
                t)))))))

;;; Return true if CTRAN has been deleted, false if it is still a valid
;;; part of IR1.
(defun ctran-deleted-p (ctran)
  (declare (type ctran ctran))
  (let ((block (ctran-block ctran)))
    (or (not (block-component block))
        (block-delete-p block))))

;;; Return true if NODE has been deleted, false if it is still a valid
;;; part of IR1.
(defun node-deleted (node)
  (declare (type node node))
  (let ((prev (node-prev node)))
    (or (not prev)
        (ctran-deleted-p prev))))

;;; Delete all the blocks and functions in COMPONENT. We scan first
;;; marking the blocks as DELETE-P to prevent weird stuff from being
;;; triggered by deletion.
(defun delete-component (component)
  (declare (type component component))
  (aver (null (component-new-functionals component)))
  (setf (component-kind component) :deleted)
  (do-blocks (block component)
    (delete-block-lazily block))
  (dolist (fun (component-lambdas component))
    (unless (functional-kind-eq fun deleted)
      (setf (functional-kind fun) (functional-kind-attributes nil))
      (setf (functional-entry-fun fun) nil)
      (setf (leaf-refs fun) nil)
      (delete-functional fun)))
  (clean-component component)
  (values))

;;; Remove all pending blocks to be deleted. Return the nearest live
;;; block after or equal to BLOCK.
(defun clean-component (component &optional block)
  (loop while (component-delete-blocks component)
        ;; actual deletion of a block may queue new blocks
        do (let ((current (pop (component-delete-blocks component))))
             (when (eq block current)
               (setq block (block-next block)))
             (delete-block current)))
  block)

;;; Convert code of the form
;;;   (FOO ... (FUN ...) ...)
;;; to
;;;   (FOO ...    ...    ...).
;;; In other words, replace the function combination FUN by its
;;; arguments. If there are any problems with doing this, use GIVE-UP
;;; to blow out of whatever transform called this. Note, as the number
;;; of arguments changes, the transform must be prepared to return a
;;; lambda with a new lambda-list with the correct number of
;;; arguments.
(defun splice-fun-args (lvar fun num-args &optional (give-up t))
  "If LVAR is a call to FUN with NUM-ARGS args, change those arguments to feed
directly to the LVAR-DEST of LVAR, which must be a combination. If FUN
is :ANY, the function name is not checked."
  (declare (type lvar lvar)
           (type symbol fun)
           (type (or index null) num-args))
  (flet ((give-up ()
           (if give-up
               (give-up-ir1-transform)
               (return-from splice-fun-args nil))))
    (let ((outside (lvar-dest lvar))
          (inside (lvar-uses lvar)))
      (aver (combination-p outside))
      (unless (combination-p inside)
        (give-up))
      (let ((inside-fun (combination-fun inside)))
        (unless (or (eq fun :any)
                    (eq (lvar-fun-name inside-fun) fun))
          (give-up))

        (let ((inside-args (combination-args inside)))
          (when num-args
            (unless (= (length inside-args) num-args)
              (give-up)))
          (let* ((outside-args (combination-args outside))
                 (arg-position (position lvar outside-args))
                 (before-args (subseq outside-args 0 arg-position))
                 (after-args (subseq outside-args (1+ arg-position))))
            (dolist (arg inside-args)
              (setf (lvar-dest arg) outside))
            (setf (combination-args inside) nil)
            (setf (combination-args outside)
                  (append before-args inside-args after-args))
            (change-ref-leaf (lvar-uses inside-fun)
                             (find-free-fun 'list "???"))
            (setf (combination-fun-info inside) (info :function :info 'list)
                  (combination-kind inside) :known)
            (setf (node-derived-type inside) *wild-type*)
            (flush-dest lvar)
            inside-args))))))

;;; Eliminate keyword arguments from the call (leaving the
;;; parameters in place.
;;;
;;;    (FOO ... :BAR X :QUUX Y)
;;; becomes
;;;    (FOO ... X Y)
;;;
;;; SPECS is a list of (:KEYWORD PARAMETER) specifications.
;;; Returns the list of specified parameters names in the
;;; order they appeared in the call. N-POSITIONAL is the
;;; number of positional arguments in th call.
(defun eliminate-keyword-args (call n-positional specs)
  (let* ((specs (copy-tree specs))
         (all (combination-args call))
         (new-args (reverse (subseq all 0 n-positional)))
         (key-args (subseq all n-positional))
         (parameters nil)
         (flushed-keys nil))
    (loop while key-args
          do (let* ((key (pop key-args))
                    (val (pop key-args))
                    (keyword (if (constant-lvar-p key)
                                 (lvar-value key)
                                 (give-up-ir1-transform)))
                    (spec (or (assoc keyword specs :test #'eq)
                              (give-up-ir1-transform))))
               (push val new-args)
               (push key flushed-keys)
               (push (second spec) parameters)
               ;; In case of duplicate keys.
               (setf (second spec) (gensym))))
    (dolist (key flushed-keys)
      (flush-dest key))
    (setf (combination-args call) (reverse new-args))
    (reverse parameters)))

(defun extract-fun-args (lvar fun num-args)
  (declare (type lvar lvar)
           (type (or symbol list) fun)
           (type index num-args))
  (let ((inside (lvar-uses lvar)))
    (unless (combination-p inside)
      (give-up-ir1-transform))
    (let ((inside-fun (combination-fun inside)))
      (unless (member (lvar-fun-name inside-fun) (ensure-list fun))
        (give-up-ir1-transform))
      (let ((inside-args (combination-args inside)))
        (unless (= (length inside-args) num-args)
          (give-up-ir1-transform))
        (values (lvar-fun-name inside-fun) inside-args)))))

(defun flush-combination (combination)
  (declare (type combination combination))
  (flush-dest (combination-fun combination))
  (dolist (arg (combination-args combination))
    (flush-dest arg))
  (unlink-node combination)
  (values))

(defun replace-combination-with-constant (constant combination)
  (when (producing-fasl-file)
    (handler-case (maybe-emit-make-load-forms constant)
      ((or compiler-error error) ()
        (return-from replace-combination-with-constant))))
  (with-ir1-environment-from-node combination
    (let* ((lvar (node-lvar combination))
           (prev (node-prev combination))
           (intermediate-ctran (make-ctran)))
      (%delete-lvar-use combination)
      (setf (ctran-next prev) nil)
      (setf (node-prev combination) nil)
      (reference-constant prev intermediate-ctran lvar constant nil)
      (link-node-to-previous-ctran combination intermediate-ctran)
      (reoptimize-lvar lvar)
      (flush-combination combination)
      t)))


;;;; leaf hackery

;;; Change the LEAF that a REF refers to.
(defun change-ref-leaf (ref leaf &key recklessly)
  (declare (type ref ref) (type leaf leaf))
  (unless (eq (ref-leaf ref) leaf)
    (push ref (leaf-refs leaf))
    (update-lvar-dependencies leaf ref)
    (delete-ref ref)
    (setf (ref-leaf ref) leaf
          (ref-same-refs ref) nil)
    (setf (leaf-ever-used leaf) t)
    (let* ((ltype (leaf-type leaf))
           (vltype (make-single-value-type ltype)))
      (if (let* ((lvar (node-lvar ref))
                 (dest (and lvar (lvar-dest lvar))))
            (and (basic-combination-p dest)
                 (eq lvar (basic-combination-fun dest))
                 (csubtypep ltype (specifier-type 'function))))
          (setf (node-derived-type ref) vltype)
          (derive-node-type ref vltype :from-scratch recklessly)))
    (reoptimize-lvar (node-lvar ref)))
  (values))

;;; Change all REFS for OLD-LEAF to NEW-LEAF.
(defun substitute-leaf (new-leaf old-leaf)
  (declare (type leaf new-leaf old-leaf))
  (dolist (ref (leaf-refs old-leaf))
    (change-ref-leaf ref new-leaf))
  (values))

;;; like SUBSITUTE-LEAF, only there is a predicate on the REF to tell
;;; whether to substitute
(defun substitute-leaf-if (test new-leaf old-leaf)
  (declare (type leaf new-leaf old-leaf) (type function test))
  (declare (dynamic-extent test))
  (dolist (ref (leaf-refs old-leaf))
    (when (funcall test ref)
      (change-ref-leaf ref new-leaf)))
  (values))

;;; FIXME: This logic is incomplete, lacking PACKAGE, RANDOM-STATE,
;;; SIMPLE-VECTOR, HASH-TABLE, and PATHNAME, and all arrays of rank
;;; other than 1.  SIMPLE-VECTOR, PATHNAME, and possibly RANDOM-STATE,
;;; could be worthwhile to handle.
(defun coalescible-object-p (object)
  (labels ((cons-coalesce-p (x)
             (when (coalesce-tree-p x)
               (labels ((descend (x)
                          (do ((y x (cdr y)))
                              ((atom y) (atom-colesce-p y))
                            ;; Don't just call file-coalesce-p, because
                            ;; it'll invoke COALESCE-TREE-P repeatedly
                            (let ((car (car y)))
                              (unless (if (consp car)
                                          (descend car)
                                          (atom-colesce-p car))
                                (return nil))))))
                 (descend x))))
           (atom-colesce-p (x)
             (sb-xc:typep x '(or (unboxed-array (*)) number symbol instance character))))
    (if (consp object)
        (cons-coalesce-p object)
        ;; Coalescing of SYMBOL, INSTANCE, CHARACTER is not useful -
        ;; if OBJECT is one of those, it would only be findable in the
        ;; EQL table.  However, a coalescible objects with subparts
        ;; may contain those.
        (sb-xc:typep object '(or (unboxed-array (*)) number)))))

;;; Return a LEAF which represents the specified constant object.
(defun find-constant (object)
  (let* ((namespace *ir1-namespace*)
         (eql-constants (eql-constants namespace))
         (file-compile-p (producing-fasl-file)))
    (cond
      ;; CLHS 3.2.4.2.2: We are allowed to coalesce by similarity when
      ;; file-compiling.
      ((and file-compile-p (coalescible-object-p object))
       (let ((similar-constants (similar-constants namespace)))
         (or (gethash object eql-constants)
             (get-similar object similar-constants)
             (let ((new (make-constant object)))
               (setf (gethash object eql-constants) new)
               (setf (gethash object similar-constants) new)))))
      (t
       ;;  "The consequences are undefined if literal objects are destructively modified
       ;;   For this purpose, the following operations are considered destructive:
       ;;   array - Storing a new value into some element of the array ..."
       ;; so a string, once used as a literal in source, becomes logically immutable.
       #-sb-xc-host
       (when (and (not file-compile-p)
                  (sb-xc:typep object '(simple-array * (*))))
         (logically-readonlyize object nil))
       (or (gethash object eql-constants)
           (setf (gethash object eql-constants)
                 (make-constant object)))))))

;;; Return true if X and Y are lvars whose only use is a
;;; reference to the same leaf, and the value of the leaf cannot
;;; change.
(defun same-leaf-ref-p (x y)
  (declare (type lvar x y))
  (let ((x-use (principal-lvar-use x))
        (y-use (principal-lvar-use y)))
    (when (and (ref-p x-use)
               (ref-p y-use)
               (same-ref-p x-use y-use))
      y-use)))

(defun same-ref-p (x y)
  (declare (type ref x y))
  (let ((leaf (ref-leaf x)))
   (and (eq leaf (ref-leaf y))
        (or (constant-reference-p x)
            (refs-unchanged-p x y)
            (and (lambda-var-p leaf)
                 (setf (lambda-var-compute-same-refs leaf) t)
                 nil)))))

(defun refs-unchanged-p (ref1 ref2)
  (let ((same (ref-same-refs ref1)))
    (and same
         (eq same
             (ref-same-refs ref2)))))


;;; Return true if VAR would have to be closed over if environment
;;; analysis ran now (i.e. if there are any uses that have a different
;;; home lambda than VAR's home.)
(defun closure-var-p (var)
  (declare (type lambda-var var))
  (let ((home (lambda-var-home var)))
    (cond ((functional-kind-eq home deleted)
           nil)
          (t (let ((home (lambda-home home)))
               (flet ((frob (l)
                        (find home l
                              :key #'node-home-lambda
                              :test #'neq)))
                 (or (frob (leaf-refs var))
                     (frob (basic-var-sets var)))))))))

;;; If there is a non-local exit noted in ENTRY's environment that
;;; exits to CONT in that entry, then return it, otherwise return NIL.
(defun find-nlx-info (exit)
  (declare (type exit exit))
  (let* ((entry (exit-entry exit))
         (cleanup (entry-cleanup entry))
         (block (first (block-succ (node-block exit)))))
    (dolist (nlx (environment-nlx-info (node-environment entry)) nil)
      (when (and (eq (nlx-info-block nlx) block)
                 (eq (nlx-info-cleanup nlx) cleanup))
        (return nlx)))))

(defun nlx-info-lvar (nlx)
  (declare (type nlx-info nlx))
  (node-lvar (block-last (nlx-info-target nlx))))

;;;; functional hackery

(defun main-entry (functional)
  (declare (type functional functional) #-sb-xc-host (values clambda))
  (etypecase functional
    (clambda functional)
    (optional-dispatch
     (optional-dispatch-main-entry functional))))

;;; RETURN true if FUNCTIONAL is a thing that can be treated like
;;; MV-BIND when it appears in an MV-CALL. All fixed arguments must be
;;; optional with null default and no SUPPLIED-P. There must be a
;;; &REST arg with no references.
(defun looks-like-an-mv-bind (functional)
  (declare (type functional functional)
           #-sb-xc-host (values boolean))
  (and (optional-dispatch-p functional)
       (do ((arg (optional-dispatch-arglist functional) (cdr arg)))
           ((null arg) nil)
         (let ((info (lambda-var-arg-info (car arg))))
           (unless info (return nil))
           (case (arg-info-kind info)
             (:optional
              (when (or (arg-info-supplied-p info) (arg-info-default info))
                (return nil)))
             (:rest
              (return (and (null (cdr arg))
                           (null (leaf-refs (car arg)))
                           ;; Type checking will require reading the
                           ;; variable, but it's done in one of the
                           ;; dispatch functions making it invisible
                           ;; to LEAF-REFS
                           (or (neq (leaf-where-from (car arg)) :declared)
                               (values (csubtypep (specifier-type 'list)
                                                  (leaf-type (car arg))))))))
             (t
              (return nil)))))))

(defun call-all-args-fixed-p (call)
  (loop for arg in (basic-combination-args call)
        always (numberp (nth-value 1 (values-types
                                      (lvar-derived-type arg))))))

;;; Return true if function is an external entry point. This is true
;;; of normal XEPs (:EXTERNAL kind) and also of top level lambdas
;;; (:TOPLEVEL kind.)
(declaim (inline xep-p))
(defun xep-p (fun)
  (declare (type functional fun))
  (functional-kind-eq fun external toplevel))

;;; If LVAR's only use is a non-notinline global function reference,
;;; then return the referenced symbol, otherwise NIL. If NOTINLINE-OK
;;; is true, then we don't care if the leaf is NOTINLINE.
(defun lvar-fun-name (lvar &optional notinline-ok)
  (declare (type lvar lvar))
  (let ((use (principal-lvar-use lvar)))
    (if (ref-p use)
        (let ((leaf (ref-leaf use)))
          (if (and (global-var-p leaf)
                   (eq (global-var-kind leaf) :global-function)
                   (or (not (defined-fun-p leaf))
                       (not (eq (defined-fun-inlinep leaf) 'notinline))
                       notinline-ok))
              (leaf-source-name leaf)
              nil))
        nil)))

;;; As above, but allow a quoted symbol also,
;;; in which case we don't check for notinline-ness,
;;; so be careful how you use this.
;;; Also note that Case 2 in LVAR-FUN-IS for dealing with #.#'NAME
;;; has no equivalent here.
(defun lvar-fun-name* (lvar)
  (if (constant-lvar-p lvar) (lvar-value lvar) (lvar-fun-name lvar)))

(defun lvar-fun-debug-name (lvar)
  (declare (type lvar lvar))
  (let ((uses (lvar-uses lvar)))
    (flet ((name1 (use)
             (leaf-debug-name (ref-leaf use))))
      (if (ref-p uses)
        (name1 uses)
        (mapcar #'name1 uses)))))

;;; Return the source name of a combination -- or signals an error
;;; if the function leaf is anonymous.
(defun combination-fun-source-name (combination &optional (errorp t))
  (let ((uses (principal-lvar-use (combination-fun combination)))
        leaf)
    (cond ((and (ref-p uses)
                (leaf-has-source-name-p (setf leaf (ref-leaf uses))))
           (values (leaf-source-name leaf) t))
          (errorp
           (aver (not "COMBINATION-FUN is not a ref to a nameful leaf")))
          (t
           (values nil nil)))))

(defun combination-fun-debug-name (combination)
  (let ((uses (principal-lvar-use (basic-combination-fun combination))))
    (when (ref-p uses)
      (let ((leaf (ref-leaf uses)))
        (typecase leaf
          (functional
           (functional-debug-name leaf))
          (t
           (and (leaf-has-source-name-p leaf)
                (leaf-source-name leaf))))))))

;;; Return the COMBINATION node that is the call to the LET FUN.
(defun let-combination (fun)
  (declare (type clambda fun))
  (aver (functional-letlike-p fun))
  (lvar-dest (node-lvar (first (leaf-refs fun)))))

;;; Return the initial value lvar for a LET variable, or NIL if there
;;; is none.
(defun let-var-initial-value (var)
  (declare (type lambda-var var))
  (let ((fun (lambda-var-home var)))
    (elt (combination-args (let-combination fun))
         (position-or-lose var (lambda-vars fun)))))

;;; Return the LAMBDA that is called by the local CALL.
(defun combination-lambda (call)
  (declare (type basic-combination call))
  (aver (eq (basic-combination-kind call) :local))
  (ref-leaf (lvar-uses (basic-combination-fun call))))

(defun register-inline-expansion (leaf call)
  (let* ((name (leaf-%source-name leaf))
         (calls (basic-combination-inline-expansions call))
         (recursive (memq name calls)))
    (cond (recursive
           (incf (cadr recursive))
           calls)
          (t
           (list* name 1 calls)))))

;;; Check whether NODE's component has exceeded its inline expansion
;;; limit, and warn if so, returning NIL.
(defun inline-expansion-ok (combination leaf)
  (let* ((name (leaf-%source-name leaf))
         (expansions (memq name
                           (basic-combination-inline-expansions combination)))
         (expanded (cadr expansions)))
    (cond ((not expanded))
          ((> expanded *inline-expansion-limit*) nil)
          ((= expanded *inline-expansion-limit*)
           (let ((*compiler-error-context* combination))
             (compiler-notify "*INLINE-EXPANSION-LIMIT* (~W) was exceeded ~
                               while inlining ~s"
                              *inline-expansion-limit* name))
           (incf (cadr expansions))
           nil)
          (t))))

;;; Make sure that FUNCTIONAL is not let-converted or deleted.
(defun assure-functional-live-p (functional)
  (declare (type functional functional))
  (when (functional-kind-eq functional
                            ;; looks LET-converted
                            let mv-let assignment
                            ;; It's possible for a LET-converted function to end up
                            ;; deleted later. In that case, for the purposes of this
                            ;; analysis, it is LET-converted: LET-converted functionals
                            ;; are too badly trashed to expand them inline, and deleted
                            ;; LET-converted functionals are even worse.
                            deleted zombie)
    (throw 'locall-already-let-converted functional)))

(defun assure-leaf-live-p (leaf)
  (typecase leaf
    (lambda-var
     (when (lambda-var-deleted leaf)
       (throw 'locall-already-let-converted leaf)))
    (functional
     (assure-functional-live-p leaf))))

(defun call-full-like-p (call)
  (declare (type basic-combination call))
  (let ((kind (basic-combination-kind call)))
    (or (eq kind :full)
        (eq kind :unknown-keys)
        ;; It has an ir2-converter, but needs to behave like a full call.
        (eq (lvar-fun-name (basic-combination-fun call) t)
            '%coerce-callable-for-call)
        (and (eq kind :known)
             (let ((info (basic-combination-fun-info call)))
               (and
                (not (fun-info-ir2-convert info))
                (not (fun-info-ltn-annotate info))
                (dolist (template (fun-info-templates info) t)
                  (when (eq (template-ltn-policy template) :fast-safe)
                    (when (valid-fun-use call (template-type template))
                      (return))))))))))

;;;; careful call

;;; Apply a function to some arguments, returning a list of the values
;;; resulting of the evaluation. If an error is signalled during the
;;; application, then return the condition and NIL as the
;;; second value.
(defun careful-call (function args)
  (declare (type (or symbol function) function)
           (type list args))
  (handler-case (values (multiple-value-list (apply function args)) t)
    ;; When cross-compiling, being "careful" is the wrong thing - our code should
    ;; not allowed malformed or out-of-order definitions to proceed as if all is well.
    #-sb-xc-host
    (error (condition)
      (values condition nil))))

;;; Variations of SPECIFIER-TYPE for parsing possibly wrong
;;; specifiers.
(macrolet
    ((deffrob (basic careful compiler transform)
       `(progn
          (defun ,careful (specifier)
            (handler-case (,basic specifier)
              (error (condition)
                (values nil condition))))
          (defun ,compiler (specifier)
            (handler-case (,basic specifier)
              (simple-error (condition)
                (apply #'compiler-warn
                       (simple-condition-format-control condition)
                       (simple-condition-format-arguments condition)))
              (error (condition)
                (compiler-warn "~a" condition))))
          (defun ,transform (specifier)
            (multiple-value-bind (type condition) (,careful specifier)
              (or type
                  (give-up-ir1-transform
                   (princ-to-string condition))))))))
  (deffrob specifier-type careful-specifier-type compiler-specifier-type ir1-transform-specifier-type)
  (deffrob values-specifier-type careful-values-specifier-type compiler-values-specifier-type ir1-transform-values-specifier-type))


;;;; utilities used at run-time for parsing &KEY args in IR1

;;; This function is used by the result of PARSE-DEFTRANSFORM to find
;;; the lvar for the value of the &KEY argument KEY in the list of
;;; lvars ARGS. It returns the lvar if the keyword is present, or NIL
;;; otherwise. The legality and constantness of the keywords should
;;; already have been checked.
(defun find-keyword-lvar (args key)
  (declare (type list args) (type keyword key))
  (do ((arg args (cddr arg)))
      ((null arg) nil)
    (when (eq (lvar-value (first arg)) key)
      (return (second arg)))))

;;; This function is used by the result of PARSE-DEFTRANSFORM to
;;; verify that alternating lvars in ARGS are constant and that there
;;; is an even number of args.
(defun check-key-args-constant (args)
  (declare (type list args) #-sb-xc-host (values boolean))
  (do ((arg args (cddr arg)))
      ((null arg) t)
    (unless (and (rest arg)
                 (constant-lvar-p (first arg)))
      (return nil))))

;;; This function is used by the result of PARSE-DEFTRANSFORM to
;;; verify that the list of lvars ARGS is a well-formed &KEY arglist
;;; and that only keywords present in the list KEYS are supplied.
(defun check-transform-keys (args keys)
  (declare (list args keys) #-sb-xc-host (values boolean))
  (and (check-key-args-constant args)
       (do ((arg args (cddr arg)))
           ((null arg) t)
         (unless (member (lvar-value (first arg)) keys)
           (return nil)))))

;;;; miscellaneous

;;; Called by the expansion of the EVENT macro.
(defun %event (info node)
  (declare (type event-info info) (type (or node null) node))
  (incf (event-info-count info))
  (when (and (>= (event-info-level info) *event-note-threshold*)
             (policy (or node *lexenv*)
                     (= inhibit-warnings 0)))
    (let ((*compiler-error-context* node))
      (compiler-notify (event-info-description info))))

  (let ((action (event-info-action info)))
    (when action (funcall action node))))

;;;
(defun make-cast (value type policy &optional context)
  (declare (type lvar value)
           (type ctype type)
           (type policy policy))
  (when (fun-type-p type)
    ;; FUN-TYPE will be weakined into FUNCTION,
    ;; but we still want to check the full type at compile time.
    (add-annotation value
                    (make-lvar-function-annotation
                     :type type
                     :context (shiftf context nil))))
  (%make-cast type
              (maybe-weaken-check type policy)
              value
              (coerce-to-values type)
              context))

(defun note-single-valuified-lvar (lvar)
  (declare (type (or lvar null) lvar))
  (when lvar
    (let ((use (lvar-uses lvar)))
      (cond ((ref-p use)
             (let ((leaf (ref-leaf use)))
               (when (and (lambda-var-p leaf)
                          (null (rest (leaf-refs leaf))))
                 (reoptimize-lambda-var leaf))))
            ((or (listp use) (combination-p use))
             (do-uses (node lvar)
               (setf (node-reoptimize node) t)
               (setf (block-reoptimize (node-block node)) t)
               (reoptimize-component (node-component node) :maybe)))))))

;;; Return true if LVAR's only use is a reference to a global function
;;; designator with one of the specified NAMES, that hasn't been
;;; declared NOTINLINE.
(defun lvar-fun-is (lvar names)
  (declare (type lvar lvar) (list names))
  (let ((use (principal-lvar-use lvar)))
    (and (ref-p use)
         (let* ((*lexenv* (node-lexenv use))
                (leaf (ref-leaf use))
                (name
                 (cond ((global-var-p leaf)
                        ;; Case 1: #'NAME
                        (and (eq (global-var-kind leaf) :global-function)
                             (car (member (leaf-source-name leaf) names
                                          :test #'equal))))
                       ((constant-p leaf)
                        (let ((value (constant-value leaf)))
                          (car (if (functionp value)
                                   ;; Case 2: #.#'NAME
                                   (member value names
                                           :key (lambda (name)
                                                  (and (fboundp name)
                                                       (fdefinition name)))
                                           :test #'eq)
                                   ;; Case 3: 'NAME
                                   (member value names
                                           :test #'equal))))))))
           (when (and name
                      (not (fun-lexically-notinline-p name)))
             name)))))

(defun combination-is (combination names)
  (and (combination-p combination)
       (lvar-fun-is (combination-fun combination) names)))


;;; Return true if LVAR's only use is a call to one of the named functions
;;; (or any function if none are specified) with the specified number of
;;; of arguments (or any number if number is not specified)
(defun lvar-matches (lvar &key fun-names arg-count (notinline t))
  (let ((use (lvar-uses lvar)))
    (and (combination-p use)
         (or (not fun-names)
             (multiple-value-bind (name ok)
                 (combination-fun-source-name use nil)
               (and ok
                    (not (and notinline
                              (fun-lexically-notinline-p name (node-lexenv use))))
                    (member name fun-names :test #'eq))))
         (or (not arg-count)
             (let ((length (length (combination-args use))))
               (or (= arg-count length)
                   (values nil length)))))))

;;; In (a (b lvar)) (lvar-matches-calls lvar '(b a)) would return T
(defun lvar-matches-calls (lvar dest-fun-names)
  (loop for fun in dest-fun-names
        for dest = (principal-lvar-end lvar)
        when (or (not (combination-p dest))
                 (neq fun (combination-fun-source-name dest nil)))
        return nil
        do (setf lvar (combination-lvar dest))
        finally (return t)))

;;; Don't substitute single-ref variables on high-debug / low speed,
;;; to improve the debugging experience, unless it is a special
;;; variable or a temporary variable used for hairy function entries.
(defun preserve-single-use-debug-var-p (node var)
  (and (policy node (eql preserve-single-use-debug-variables 3))
       (not (lambda-var-specvar var))
       (not (and (combination-p node)
                 (typep (leaf-debug-name (combination-lambda node))
                        '(cons (member hairy-function-entry) t))))))

;;; Call (lambda (arg lambda-var type)), for a mv-combination ARG can
;;; be NIL when it produces multiple values.
;;; If REOPTIMIZE is T only the arguments for which LVAR-REOPTIMIZE is
;;; true will be examined, resetting LVAR-REOPTIMIZE to NIL before
;;; calling FUNCTION.
(defun map-combination-arg-var (function combination &key reoptimize)
  (let ((args (basic-combination-args combination))
        (vars (lambda-vars (combination-lambda combination))))
    (flet ((reoptimize-p (arg)
             (cond ((not arg) nil)
                   ((not reoptimize))
                   ((lvar-reoptimize arg)
                    (setf (lvar-reoptimize arg) nil)
                    t))))
      (cond ((combination-p combination)
             (loop for arg in args
                   for var in vars
                   when (reoptimize-p arg)
                   do
                   (funcall function arg var (lvar-type arg))))
            ((singleton-p args)
             (when (reoptimize-p (first args))
               (loop with arg = (first args)
                     for var in vars
                     for type in (values-type-in (lvar-derived-type arg)
                                                 (length vars))
                     do
                     (funcall function
                              (and (singleton-p vars)
                                   arg)
                              var
                              type))))
            (t
             (loop for arg in args
                   do (multiple-value-bind (types length) (values-types (lvar-derived-type arg))
                        (when (eq length :unknown)
                          (return))
                        (if (reoptimize-p arg)
                            (loop with singleton-arg = (and (= length 1)
                                                            arg)
                                  for type in types
                                  while vars
                                  do
                                  (funcall function singleton-arg
                                           (pop vars) type))
                            (setf vars (nthcdr length vars))))))))))

(defun if-type-check (if)
  (let ((test (lvar-uses (if-test if))))
    (when (combination-p test)
      (let ((name (combination-fun-source-name test nil)))
        (values (gethash name *backend-predicate-types*)
                (car (combination-args test)))))))


(defun proper-or-circular-list-p (x)
  (if (consp x)
      (let ((rabbit (cdr x))
            (turtle x))
        (flet ((pop-rabbit ()
                 (when (eql rabbit turtle) ; circular
                   (return-from proper-or-circular-list-p t))
                 (when (atom rabbit)
                   (return-from proper-or-circular-list-p (null rabbit)))
                 (pop rabbit)))
          (loop (pop-rabbit)
                (pop-rabbit)
                (pop turtle))))
      (null x)))

(defun proper-or-dotted-list-p (x)
  (if (consp x)
      (let ((rabbit (cdr x))
            (turtle x))
        (flet ((pop-rabbit ()
                 (when (eql rabbit turtle) ; circular
                   (return-from proper-or-dotted-list-p nil))
                 (when (atom rabbit)
                   (return-from proper-or-dotted-list-p t))
                 (pop rabbit)))
          (loop (pop-rabbit)
                (pop-rabbit)
                (pop turtle))))
      (null x)))

(defun map-lambda-var-refs-from-calls (function lambda-var)
  (when (not (lambda-var-sets lambda-var))
    (let* ((home (lambda-var-home lambda-var))
           (vars (lambda-vars home)))
      (dolist (ref (lambda-refs home))
        (let* ((lvar (node-lvar ref))
               (combination (and lvar
                                 (lvar-dest lvar))))
          (when (and (combination-p combination)
                     (eq (combination-kind combination) :local)
                     (eq (combination-fun combination)
                         lvar))
            (loop for v in vars
                  for arg in (combination-args combination)
                  when (eq v lambda-var)
                  do (funcall function combination arg))))))))

(defun map-refs (function leaf/lvar &key leaf-set
                                         multiple-uses)
  (let ((seen-calls))
    (labels ((recur (leaf/lvar)
               (typecase leaf/lvar
                 (leaf
                  (when (and leaf-set
                             (lambda-var-sets leaf/lvar))
                    (funcall leaf-set))
                  (dolist (ref (leaf-refs leaf/lvar))
                    (recur (node-lvar ref))))
                 (lvar
                  (let* ((dest (lvar-dest leaf/lvar)))
                    (cond ((and multiple-uses
                                (consp (lvar-uses leaf/lvar)))
                           (funcall multiple-uses))
                          ((and (combination-p dest)
                                (eq (combination-kind dest) :local))
                           (let ((lambda (combination-lambda dest)))
                             (when (and multiple-uses
                                        (cdr (leaf-refs lambda)))
                               (funcall multiple-uses))
                             (when (cond ((functional-kind-eq lambda let))
                                         ((memq dest seen-calls)
                                          nil)
                                         (t
                                          (push dest seen-calls)))
                               (loop for v in (lambda-vars lambda)
                                     for arg in (combination-args dest)
                                     when (eq arg leaf/lvar)
                                     do (recur v)))))
                          ((and (combination-p dest)
                                (lvar-fun-is (combination-fun dest) '(values))
                                (let ((mv (node-dest dest)))
                                  (when (and multiple-uses
                                             (consp (lvar-uses (node-lvar dest))))
                                    (funcall multiple-uses))
                                  (when (and (mv-combination-p mv)
                                             (eq (basic-combination-kind mv) :local))
                                    (let ((fun (combination-lambda mv)))
                                      (when (and (functional-p fun)
                                                 (functional-kind-eq fun mv-let))
                                        (let* ((arg (position leaf/lvar (combination-args dest)))
                                               (var (and arg (nth arg (lambda-vars fun)))))
                                          (recur var)
                                          t)))))))
                          (t
                           (funcall function dest))))))))
      (recur leaf/lvar))))

(defun propagate-lvar-annotations-to-refs (lvar var)
  (when (lvar-annotations lvar)
    (dolist (ref (leaf-refs var))
      (when (node-lvar ref)
        (propagate-lvar-annotations (node-lvar ref) lvar
                                    nil)))))

(defun propagate-lvar-annotations (new old &optional (propagate-dependencies t))
  (when propagate-dependencies
    (loop for dep in (lvar-dependent-annotations old)
          do (nsubst new old (lvar-dependent-annotation-deps dep))
          when (lvar-p new)
          do
          (pushnew dep (lvar-dependent-annotations new) :test #'eq))
    (loop for dep in (lvar-dependent-nodes old)
          when (lvar-p new)
          do
          (pushnew dep (lvar-dependent-nodes new) :test #'eq)))
  (when (lvar-p new)
    (setf
     (lvar-annotations new)
     (cond ((not (lvar-annotations new))
            (lvar-annotations old))
           ((not (lvar-annotations old))
            (lvar-annotations new))
           (t
            ;; Get only the outermost annotation, avoiding multiple
            ;; warnings coming from source transforms.
            (let ((all (union (lvar-annotations old) (lvar-annotations new))))
              (loop for annotation in all
                    for type = (type-of annotation)
                    for source-path = (lvar-annotation-source-path annotation)
                    when (or (eq (car source-path) 'original-source-start)
                             (loop for other in all
                                   for other-source-path = (lvar-annotation-source-path other)
                                   never (and (not (eq annotation other))
                                              (eq type (type-of other))
                                              (or (eq (car other-source-path) 'original-source-start)
                                                  (member (car other-source-path)
                                                          source-path)))))
                    collect annotation)))))))

(defun lvar-constants (lvar &optional walk-functions)
  (named-let recurse ((lvar lvar) (seen nil))
    (let* ((uses (lvar-uses lvar))
           (lvar (or (and (ref-p uses)
                          (let ((ref (principal-lvar-ref lvar)))
                            (and ref
                                 (or
                                  (lambda-var-ref-lvar ref)
                                  (node-lvar ref)))))
                     lvar)))
      (cond ((constant-lvar-p lvar)
             (values :values (list (lvar-value lvar))))
            ((constant-lvar-uses-p lvar)
             (values :values (lvar-uses-values lvar)))
            ((ref-p uses)
             (let* ((ref (principal-lvar-ref lvar))
                    (leaf (and ref
                               (ref-leaf ref))))
               (when (lambda-var-p leaf)
                 (let ((seen (or seen (alloc-xset)))
                       constants)
                   (add-to-xset lvar seen)
                   (map-lambda-var-refs-from-calls
                    (lambda (call lvar)
                      (unless (xset-member-p lvar seen)
                        (add-to-xset lvar seen)
                        (multiple-value-bind (type values) (recurse lvar seen)
                          (case type
                            (:values
                             (push (cons call values) constants))
                            (:calls
                             (setf constants (nconc values constants)))))))
                    leaf)
                   (when constants
                     (values :calls constants))))))
            ((and walk-functions
                  (combination-p uses)
                  (eq (combination-kind uses) :known))
             (let ((fun-info (fun-info-constants (combination-fun-info uses))))
               (when fun-info
                 (let ((constants (funcall fun-info uses)))
                   (when constants
                     (multiple-value-bind (kind constants)
                         (recurse constants seen)
                       (declare (ignore kind))
                       (when constants
                         (values :values constants))))))))))))

(defun lambda-var-original-name (leaf)
  (let ((home (lambda-var-home leaf)))
    (if (functional-kind-eq home external)
        (let* ((entry (functional-entry-fun home))
               (p (1- (or (position leaf (lambda-vars home))
                          (bug "can't find leaf")))))
          (leaf-debug-name
           (if (optional-dispatch-p entry)
               (elt (optional-dispatch-arglist entry) p)
               (elt (lambda-vars entry) p))))
        (leaf-debug-name leaf))))

(defun process-lvar-modified-annotation (lvar annotation)
  (loop for annot in (lvar-annotations lvar)
        when (lvar-lambda-var-annotation-p annot)
        do (let ((lambda-var (lvar-lambda-var-annotation-lambda-var annot)))
             (when (and (lambda-var-constant lambda-var)
                        (not (lambda-var-sets lambda-var)))
               (warn 'sb-kernel::macro-arg-modified
                     :fun-name (lvar-modified-annotation-caller annotation)
                     :variable (lambda-var-original-name lambda-var))
               (return-from process-lvar-modified-annotation))))
  (multiple-value-bind (type values) (lvar-constants lvar t)
    (labels ((modifiable-p (value)
               (or (consp value)
                   (and (arrayp value)
                        (not (typep value '(vector * 0))))
                   (hash-table-p value)))
             (report (values)
               (let ((sans-nil (remove nil values)))
                (when (and sans-nil
                           (every #'modifiable-p sans-nil))
                  (warn 'constant-modified
                        :fun-name (lvar-modified-annotation-caller annotation)
                        :values sans-nil)))))
     (case type
       (:values
        (report values)
        t)
       (:calls
        (loop for (call . values) in values
              do (let ((*compiler-error-context* call))
                   (report values)))
        t)))))

(defun improper-sequence-p (annotation value)
  (case annotation
    (proper-list
     (and (listp value)
          (not (proper-list-p value))))
    (proper-sequence
     (and (typep value 'sequence)
          (not (proper-sequence-p value))))
    (proper-or-circular-list
     (and (listp value)
          (not (proper-or-circular-list-p value))))
    (proper-or-dotted-list
     (and (listp value)
          (not (proper-or-dotted-list-p value))))))

(defun process-lvar-proper-sequence-annotation (lvar annotation)
  (multiple-value-bind (type values) (lvar-constants lvar)
    (let ((kind (lvar-proper-sequence-annotation-kind annotation)))
      (labels ((bad-p (value)
                 (improper-sequence-p kind value))
               (report (values)
                 (when (every #'bad-p values)
                   (if (singleton-p values)
                       (warn 'type-warning
                             :format-control
                             "~@<~2I~_~S ~Iis not a proper ~a.~@:>"
                             :format-arguments (list (car values)
                                                     (if (eq kind 'proper-sequence)
                                                         "sequence"
                                                         "list")))
                       (warn 'type-warning
                             :format-control
                             "~@<~2I~_~{~s~^, ~} ~Iare not proper ~as.~@:>"
                             :format-arguments (list values
                                                     (if (eq kind 'proper-sequence)
                                                         "sequence"
                                                         "list")))))))
        (case type
          (:values
           (report values)
           t)
          (:calls
           (loop for (call . values) in values
                 do (let ((*compiler-error-context* call))
                      (report values)))
           t))))))

(defun process-lvar-hook-annotation (lvar annotation)
  (when (constant-lvar-p lvar)
    (funcall (lvar-hook-hook annotation)
             (lvar-value lvar))
    t))

(defun process-lvar-type-annotation (lvar annotation)
  (let* ((uses (lvar-uses lvar))
         (context (lvar-type-annotation-context annotation))
         (condition (typecase context
                      (defstruct-slot-description
                       (if (policy (if (consp uses)
                                       (car uses)
                                       uses)
                               (zerop type-check))
                           'slot-initform-type-style-warning
                           context))
                      (t
                       'type-warning)))
         (type (lvar-type-annotation-type annotation)))
    (cond ((not (types-equal-or-intersect (lvar-type lvar) type))
           (if (symbolp condition)
               (%compile-time-type-error-warn annotation (type-specifier type)
                                              (type-specifier (lvar-type lvar))
                                              (let ((path (lvar-annotation-source-path annotation)))
                                                (if (eq (car path) 'detail)
                                                    (second path)
                                                    (list
                                                     (if (eq (car path) 'original-source-start)
                                                         (find-original-source path)
                                                         (car path)))))
                                              :condition condition)
               (setf (sb-kernel::dsd-bits condition)
                     (logior sb-kernel::dsd-default-error
                             (sb-kernel::dsd-bits condition)))))
          ((consp uses)
           (let ((condition (case condition
                              (type-warning 'type-style-warning)
                              (t condition)))
                 bad)
             (loop for use in uses
                   for dtype = (node-derived-type use)
                   unless (values-types-equal-or-intersect dtype type)
                   do (push use bad))
             (when bad
               (loop for bad-use in bad
                     for path = (source-path-before-transforms bad-use)
                     ;; Are all uses from the same transform bad?
                     when (or (not path)
                              (loop for use in uses
                                    always (or (memq use bad)
                                               (neq path (source-path-before-transforms use)))))
                     do
                     (if (symbolp condition)
                         (%compile-time-type-error-warn bad-use
                                                        (type-specifier type)
                                                        (type-specifier (node-derived-type bad-use))
                                                        (list (node-source-form bad-use))
                                                        :condition condition)
                         (setf (sb-kernel::dsd-bits condition)
                               (logior sb-kernel::dsd-default-error
                                       (sb-kernel::dsd-bits condition)))))))))))

(defun process-lvar-sequence-bounds-annotation (lvar annotation)
  (destructuring-bind (start end) (lvar-dependent-annotation-deps annotation)
    (check-sequence-ranges lvar start end annotation)))

(defun process-annotations (lvar)
  (unless (and (combination-p (lvar-dest lvar))
               (lvar-fun-is
                (combination-fun (lvar-dest lvar))
                ;; KLUDGE: after some type derivation and merging with other types
                ;; a path can emerge which is erronous and has a bad constant,
                ;; but another value can still be good.
                ;; see compiler.pure/generate-type-checks-on-dead-blocks
                '(%type-check-error %type-check-error/c)))
    (loop for annot in (lvar-annotations lvar)
          unless (lvar-annotation-fired annot)
          do
          (let ((*compiler-error-context* annot))
            (when (typecase annot
                    (lvar-modified-annotation
                     (process-lvar-modified-annotation lvar annot))
                    (lvar-proper-sequence-annotation
                     (process-lvar-proper-sequence-annotation lvar annot))
                    (lvar-hook
                     (process-lvar-hook-annotation lvar annot))
                    (lvar-function-designator-annotation
                     (check-function-designator-lvar lvar annot))
                    (lvar-function-annotation
                     (check-function-lvar lvar annot))
                    (lvar-type-annotation
                     (process-lvar-type-annotation lvar annot))
                    (lvar-sequence-bounds-annotation
                     (process-lvar-sequence-bounds-annotation lvar annot)))
              (setf (lvar-annotation-fired annot) t))))))

(defun add-annotation (lvar annotation)
  (unless (eq (lvar-annotations lvar)
              (pushnew annotation (lvar-annotations lvar)
                       :key #'type-of))
    (when (typep annotation 'lvar-dependent-annotation)
      (loop for lvar in (lvar-dependent-annotation-deps annotation)
            when (lvar-p lvar)
            do (push annotation (lvar-dependent-annotations lvar))))
    (unless (lvar-annotation-source-path annotation)
      (setf (lvar-annotation-source-path annotation)
            (if (boundp '*current-path*)
                *current-path*
                (node-source-path (lvar-dest lvar)))))
    (unless (lvar-annotation-lexenv annotation)
      (setf (lvar-annotation-lexenv annotation)
            (or (and
                 (lvar-dest lvar)
                 (node-lexenv (lvar-dest lvar)))
                *lexenv*)))
    t))

(defun compiling-p (environment)
  (and (boundp 'sb-c:*compilation*)
       environment
       #+sb-fasteval
       (not (typep environment 'sb-interpreter:basic-env))
       #+sb-eval
       (not (typep environment 'sb-eval::eval-lexenv))
       (not (null-lexenv-p environment))))

;;; Return T if SYMBOL will always have a value in its TLS cell that is
;;; not EQ to NO-TLS-VALUE-MARKER-WIDETAG. As an optimization, set and ref
;;; are permitted (but not required) to avoid checking for it.
;;; This will be true of all C interface symbols, 'struct thread' slots,
;;; and any variable defined by DEFINE-THREAD-LOCAL.
;;;
;;; Or if there's a binding around NODE.
(defun sb-vm::symbol-always-has-tls-value-p (symbol node)
  (let ((symbol (if (symbolp symbol)
                    symbol
                    (let ((tn (if (tn-p symbol)
                                  symbol
                                  (tn-ref-tn symbol))))
                      (sc-case tn
                        ((constant sb-vm::immediate)
                         (tn-value tn))
                        (t
                         (return-from sb-vm::symbol-always-has-tls-value-p)))))))
    (or (typep (info :variable :wired-tls symbol)
               '(or (eql :always-thread-local) fixnum))
        (when node
          (do-nested-cleanups (cleanup node)
            (when (eq (cleanup-kind cleanup) :special-bind)
              (let* ((node (cleanup-mess-up cleanup))
                     (args (when (basic-combination-p node)
                             (basic-combination-args node))))
                (when (and args
                           (eq (leaf-source-name (lvar-value (car args))) symbol))
                  (return t)))))))))

(defun internal-name-p (name)
  (and #-sb-xc-host (fboundp name)
       (named-let internal-p ((what name))
         (typecase what
           (list (every #'internal-p what))
           (symbol
            (let ((pkg (sb-xc:symbol-package what)))
              (or (and pkg (system-package-p pkg))
                  (eq pkg *cl-package*))))
           (t t)))))

(defun common-inline-point (node1 node2)
  (let ((path1 (member-if (lambda (x)
                            (memq x '(inlined transformed)))
                          (node-source-path node1)))
        (path2 (member-if (lambda (x)
                            (memq x '(inlined transformed)))
                          (node-source-path node2))))
    (loop for path on path1
          when (and (memq (car path) '(transformed inlined))
                    (tailp path path2))
          return path)))

(defun cast-mismatch-from-inlined-p (cast path)
  (let* ((transformed (memq 'transformed path))
         (inlined))
    (cond ((and transformed
                (not (eq (memq 'transformed (node-source-path cast))
                         transformed))))
          ((setf inlined
                 (memq 'inlined path))
           (not (eq (memq 'inlined (node-source-path cast))
                    inlined))))))

(defun source-path-before-transforms (node)
  (let* ((path (node-source-path node))
         (first (position-if (lambda (x) (memq x '(transformed inlined)))
                             path :from-end t)))
    (if first
        (nthcdr (+ first 2) path))))

(defun combination-derive-type-for-arg-types (combination types)
  (let* ((info (combination-fun-info combination))
         (deriver (and info
                       (fun-info-derive-type info))))
    (when deriver
      (handler-bind ((warning #'muffle-warning))
        (let ((mock (copy-structure combination)))
          (setf (combination-args mock)
                (loop for type in types
                      collect (if (lvar-p type)
                                  type
                                  (let ((lvar (make-lvar)))
                                    (setf (lvar-%derived-type lvar) type)
                                    lvar))))
          (funcall deriver mock))))))
