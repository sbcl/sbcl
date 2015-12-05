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

(in-package "SB!C")

;;;; cleanup hackery

;;; Return the innermost cleanup enclosing NODE, or NIL if there is
;;; none in its function. If NODE has no cleanup, but is in a LET,
;;; then we must still check the environment that the call is in.
(defun node-enclosing-cleanup (node)
  (declare (type node node))
  (do ((lexenv (node-lexenv node)
               (lambda-call-lexenv (lexenv-lambda lexenv))))
      ((null lexenv) nil)
    (awhen (lexenv-cleanup lexenv)
      (return it))))

(defun map-nested-cleanups (function block &optional return-value)
  (declare (type cblock block))
  (do ((cleanup (block-end-cleanup block)
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
(defun insert-cleanup-code (block1 block2 node form &optional cleanup)
  (declare (type cblock block1 block2) (type node node)
           (type (or cleanup null) cleanup))
  (setf (component-reanalyze (block-component block1)) t)
  (with-ir1-environment-from-node node
    (with-component-last-block (*current-component*
                                (block-next (component-head *current-component*)))
      (let* ((start (make-ctran))
             (block (ctran-starts-block start))
             (next (make-ctran))
             (*lexenv* (if cleanup
                           (make-lexenv :cleanup cleanup)
                           *lexenv*)))
        (change-block-successor block1 block2 block)
        (link-blocks block block2)
        (ir1-convert start next nil form)
        (setf (block-last block) (ctran-use next))
        (setf (node-next (block-last block)) nil)
        block))))

;;;; lvar use hacking

;;; Return a list of all the nodes which use LVAR.
(declaim (ftype (sfunction (lvar) list) find-uses))
(defun find-uses (lvar)
  (ensure-list (lvar-uses lvar)))

(declaim (ftype (sfunction (lvar) lvar) principal-lvar))
(defun principal-lvar (lvar)
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

(defun principal-lvar-dest (lvar)
  (labels ((pld (lvar)
             (declare (type lvar lvar))
             (let ((dest (lvar-dest lvar)))
               (if (cast-p dest)
                   (pld (cast-lvar dest))
                   dest))))
    (pld lvar)))

;;; Update lvar use information so that NODE is no longer a use of its
;;; LVAR.
;;;
;;; Note: if you call this function, you may have to do a
;;; REOPTIMIZE-LVAR to inform IR1 optimization that something has
;;; changed.
(declaim (ftype (sfunction (node) (values))
                delete-lvar-use
                %delete-lvar-use))
;;; Just delete NODE from its LVAR uses; LVAR is preserved so it may
;;; be given a new use.
(defun %delete-lvar-use (node)
  (let ((lvar (node-lvar node)))
    (when lvar
      (if (listp (lvar-uses lvar))
          (let ((new-uses (delq node (lvar-uses lvar))))
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
(declaim (ftype (sfunction (node (or lvar null)) (values)) add-lvar-use))
(defun add-lvar-use (node lvar)
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

;;; Return true if LVAR destination is executed immediately after
;;; NODE. Cleanups are ignored.
(defun immediately-used-p (lvar node)
  (declare (type lvar lvar) (type node node))
  (aver (eq (node-lvar node) lvar))
  (let ((dest (lvar-dest lvar)))
    (acond ((node-next node)
            (eq (ctran-next it) dest))
           (t (eq (block-start (first (block-succ (node-block node))))
                  (node-prev dest))))))

;;; Returns the defined (usually untrusted) type of the combination,
;;; or NIL if we couldn't figure it out.
(defun combination-defined-type (combination)
  (let ((use (principal-lvar-use (basic-combination-fun combination))))
    (or (when (ref-p use)
          (let ((type (leaf-defined-type (ref-leaf use))))
            (when (fun-type-p type)
              (fun-type-returns type))))
        *wild-type*)))

;;; Return true if LVAR destination is executed after node with only
;;; uninteresting nodes intervening.
;;;
;;; Uninteresting nodes are nodes in the same block which are either
;;; REFs, external CASTs to the same destination, or known combinations
;;; that never unwind.
(defun almost-immediately-used-p (lvar node)
  (declare (type lvar lvar)
           (type node node))
  (aver (eq (node-lvar node) lvar))
  (let ((dest (lvar-dest lvar)))
    (tagbody
     :next
       (let ((ctran (node-next node)))
         (cond (ctran
                (setf node (ctran-next ctran))
                (if (eq node dest)
                    (return-from almost-immediately-used-p t)
                    (typecase node
                      (ref
                       (go :next))
                      (cast
                       (when (and (eq :external (cast-type-check node))
                                  (eq dest (node-dest node)))
                         (go :next)))
                      (combination
                       ;; KLUDGE: Unfortunately we don't have an attribute for
                       ;; "never unwinds", so we just special case
                       ;; %ALLOCATE-CLOSURES: it is easy to run into with eg.
                       ;; FORMAT and a non-constant first argument.
                       (when (eq '%allocate-closures (combination-fun-source-name node nil))
                         (go :next))))))
               (t
                (when (eq (block-start (first (block-succ (node-block node))))
                          (node-prev dest))
                  (return-from almost-immediately-used-p t))))))))

;;;; lvar substitution

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
    (setf (lvar-dest new) dest)
    (flush-lvar-externally-checkable-type new))
  (values))

;;; Replace all uses of OLD with uses of NEW, where NEW has an
;;; arbitary number of uses. NEW is supposed to be "later" than OLD.
(defun substitute-lvar-uses (new old propagate-dx)
  (declare (type lvar old)
           (type (or lvar null) new)
           (type boolean propagate-dx))

  (cond (new
         (do-uses (node old)
           (%delete-lvar-use node)
           (add-lvar-use node new))
         (reoptimize-lvar new)
         (awhen (and propagate-dx (lvar-dynamic-extent old))
           (setf (lvar-dynamic-extent old) nil)
           (unless (lvar-dynamic-extent new)
             (setf (lvar-dynamic-extent new) it)
             (setf (cleanup-info it) (subst new old (cleanup-info it)))))
         (when (lvar-dynamic-extent new)
           (do-uses (node new)
             (node-ends-block node))))
        (t (flush-dest old)))

  (values))

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

;;; CTRAN must be the last ctran in an incomplete block; finish the
;;; block and start a new one if necessary.
(defun start-block (ctran)
  (declare (type ctran ctran))
  (aver (not (ctran-next ctran)))
  (ecase (ctran-kind ctran)
    (:inside-block
     (let ((block (ctran-block ctran))
           (node (ctran-use ctran)))
       (aver (not (block-last block)))
       (aver node)
       (setf (block-last block) node)
       (setf (node-next node) nil)
       (setf (ctran-use ctran) nil)
       (setf (ctran-kind ctran) :unused)
       (setf (ctran-block ctran) nil)
       (link-blocks block (ctran-starts-block ctran))))
    (:block-start)))

;;;;

;;; Filter values of LVAR through FORM, which must be an ordinary/mv
;;; call. Exactly one argument must be 'DUMMY, which will be replaced
;;; with LVAR. In case of an ordinary call the function should not
;;; have return type NIL. We create a new "filtered" lvar.
;;;
;;; TODO: remove preconditions.
(defun filter-lvar (lvar form)
  (declare (type lvar lvar) (type list form))
  (let* ((dest (lvar-dest lvar))
         (ctran (node-prev dest)))
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

        ;; KLUDGE: Comments at the head of this function in CMU CL
        ;; said that somewhere in here we
        ;;   Set the new block's start and end cleanups to the *start*
        ;;   cleanup of PREV's block. This overrides the incorrect
        ;;   default from WITH-IR1-ENVIRONMENT-FROM-NODE.
        ;; Unfortunately I can't find any code which corresponds to this.
        ;; Perhaps it was a stale comment? Or perhaps I just don't
        ;; understand.. -- WHN 19990521

        ;; Replace 'DUMMY with the LVAR. (We can find 'DUMMY because
        ;; no LET conversion has been done yet.) The [mv-]combination
        ;; code from the call in the form will be the use of the new
        ;; check lvar. We substitute exactly one argument.
        (let* ((node (lvar-use filtered-lvar))
               victim)
          (dolist (arg (basic-combination-args node) (aver victim))
            (let* ((arg (principal-lvar arg))
                   (use (lvar-use arg))
                   leaf)
              (when (and (ref-p use)
                         (constant-p (setf leaf (ref-leaf use)))
                         (eql (constant-value leaf) 'dummy))
                (aver (not victim))
                (setf victim arg))))
          (aver (eq (constant-value (ref-leaf (lvar-use victim)))
                    'dummy))

          (substitute-lvar filtered-lvar lvar)
          (substitute-lvar lvar victim)
          (flush-dest victim))

        ;; Invoking local call analysis converts this call to a LET.
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
                               (eq (basic-combination-kind use) :local))
                      (merges use))))
                (substitute-lvar-uses lvar value
                                      (and lvar (eq (lvar-uses lvar) node)))
                (%delete-lvar-use node)
                (prog1
                    (unlink-node node)
                  (dolist (merge (merges))
                    (merge-tail-sets merge)))))
        (t (flush-dest value)
           (unlink-node node))))

;;; Make a CAST and insert it into IR1 before node NEXT.
(defun insert-cast-before (next lvar type policy)
  (declare (type node next) (type lvar lvar) (type ctype type))
  (with-ir1-environment-from-node next
    (let* ((ctran (node-prev next))
           (cast (make-cast lvar type policy))
           (internal-ctran (make-ctran)))
      (setf (ctran-next ctran) cast
            (node-prev cast) ctran)
      (use-ctran cast internal-ctran)
      (link-node-to-previous-ctran next internal-ctran)
      (setf (lvar-dest lvar) cast)
      (reoptimize-lvar lvar)
      (when (return-p next)
        (node-ends-block cast))
      (setf (block-attributep (block-flags (node-block cast))
                              type-check type-asserted)
            t)
      cast)))

;;;; miscellaneous shorthand functions

;;; Return the home (i.e. enclosing non-LET) CLAMBDA for NODE. Since
;;; the LEXENV-LAMBDA may be deleted, we must chain up the
;;; LAMBDA-CALL-LEXENV thread until we find a CLAMBDA that isn't
;;; deleted, and then return its home.
(defun node-home-lambda (node)
  (declare (type node node))
  (do ((fun (lexenv-lambda (node-lexenv node))
            (lexenv-lambda (lambda-call-lexenv fun))))
      ((not (memq (functional-kind fun) '(:deleted :zombie)))
       (lambda-home fun))
    (when (eq (lambda-home fun) fun)
      (return fun))))

(declaim (ftype (sfunction (node) component) node-component))
(defun node-component (node)
  (block-component (node-block node)))
(declaim (ftype (sfunction (node) physenv) node-physenv))
(defun node-physenv (node)
  (lambda-physenv (node-home-lambda node)))

#!-sb-fluid (declaim (inline node-stack-allocate-p))
(defun node-stack-allocate-p (node)
  (awhen (node-lvar node)
    (lvar-dynamic-extent it)))

(defun flushable-combination-p (call)
  (declare (type combination call))
  (let ((kind (combination-kind call))
        (info (combination-fun-info call)))
    (when (and (eq kind :known) (fun-info-p info))
      (let ((attr (fun-info-attributes info)))
        (when (and (not (ir1-attributep attr call))
                   ;; FIXME: For now, don't consider potentially flushable
                   ;; calls flushable when they have the CALL attribute.
                   ;; Someday we should look at the functional args to
                   ;; determine if they have any side effects.
                   (if (policy call (= safety 3))
                       (ir1-attributep attr flushable)
                       (ir1-attributep attr unsafely-flushable)))
          t)))))

;;;; DYNAMIC-EXTENT related

(defun lambda-var-original-name (leaf)
  (let ((home (lambda-var-home leaf)))
    (if (eq :external (functional-kind home))
        (let* ((entry (functional-entry-fun home))
               (p (1- (position leaf (lambda-vars home)))))
          (leaf-debug-name
           (if (optional-dispatch-p entry)
               (elt (optional-dispatch-arglist entry) p)
               (elt (lambda-vars entry) p))))
        (leaf-debug-name leaf))))

(defun note-no-stack-allocation (lvar &key flush)
  (do-uses (use (principal-lvar lvar))
    (unless (or
             ;; Don't complain about not being able to stack allocate constants.
             (and (ref-p use) (constant-p (ref-leaf use)))
             ;; If we're flushing, don't complain if we can flush the combination.
             (and flush (combination-p use) (flushable-combination-p use))
             ;; Don't report those with homes in :OPTIONAL -- we'd get doubled
             ;; reports that way.
             (and (ref-p use) (lambda-var-p (ref-leaf use))
                  (eq :optional (lambda-kind (lambda-var-home (ref-leaf use))))))
      ;; FIXME: For the first leg (lambda-bind (lambda-var-home ...))
      ;; would be a far better description, but since we use
      ;; *COMPILER-ERROR-CONTEXT* for muffling we can't -- as that node
      ;; can have different handled conditions.
      (let ((*compiler-error-context* use))
        (if (and (ref-p use) (lambda-var-p (ref-leaf use)))
            (compiler-notify "~@<could~2:I not stack allocate ~S in: ~S~:@>"
                             (lambda-var-original-name (ref-leaf use))
                             (find-original-source (node-source-path use)))
            (compiler-notify "~@<could~2:I not stack allocate: ~S~:@>"
                             (find-original-source (node-source-path use))))))))

(defun use-good-for-dx-p (use dx &optional component)
  ;; FIXME: Can casts point to LVARs in other components?
  ;; RECHECK-DYNAMIC-EXTENT-LVARS assumes that they can't -- that is, that the
  ;; PRINCIPAL-LVAR is always in the same component as the original one. It
  ;; would be either good to have an explanation of why casts don't point
  ;; across components, or an explanation of when they do it. ...in the
  ;; meanwhile AVER that our assumption holds true.
  (aver (or (not component) (eq component (node-component use))))
  (or (dx-combination-p use dx)
      (and (cast-p use)
           (not (cast-type-check use))
           (lvar-good-for-dx-p (cast-value use) dx component))
      (and (trivial-lambda-var-ref-p use)
           (let ((uses (lvar-uses (trivial-lambda-var-ref-lvar use))))
             (or (eq use uses)
                 (lvar-good-for-dx-p (trivial-lambda-var-ref-lvar use) dx component))))))

(defun lvar-good-for-dx-p (lvar dx &optional component)
  (let ((uses (lvar-uses lvar))) ; TODO use ENSURE-LIST? or is it too slow?
    (if (listp uses)
        (when uses
          (every (lambda (use)
                   (use-good-for-dx-p use dx component))
                 uses))
        (use-good-for-dx-p uses dx component))))

(defun known-dx-combination-p (use dx)
  (and (eq (combination-kind use) :known)
       (let ((info (combination-fun-info use)))
         (or (awhen (fun-info-stack-allocate-result info)
               (funcall it use dx))
             (awhen (fun-info-result-arg info)
               (let ((args (combination-args use)))
                 (lvar-good-for-dx-p (if (zerop it)
                                         (car args)
                                         (nth it args))
                                     dx)))))))

(defun dx-combination-p (use dx)
  (and (combination-p use)
       (or
        ;; Known, and can do DX.
        (known-dx-combination-p use dx)
        ;; Possibly a not-yet-eliminated lambda which ends up returning the
        ;; results of an actual known DX combination.
        (let* ((fun (combination-fun use))
               (ref (principal-lvar-use fun))
               (clambda (when (ref-p ref)
                          (ref-leaf ref)))
               (creturn (when (lambda-p clambda)
                          (lambda-return clambda)))
               (result-use (when (return-p creturn)
                             (principal-lvar-use (return-result creturn)))))
          ;; FIXME: We should be able to deal with multiple uses here as well.
          (and (dx-combination-p result-use dx)
               (combination-args-flow-cleanly-p use result-use dx))))))

(defun combination-args-flow-cleanly-p (combination1 combination2 dx)
  (labels ((recurse (combination)
             (or (eq combination combination2)
                 (if (known-dx-combination-p combination dx)
                     (let ((dest (lvar-dest (combination-lvar combination))))
                       (and (combination-p dest)
                            (recurse dest)))
                     (let* ((fun1 (combination-fun combination))
                            (ref1 (principal-lvar-use fun1))
                            (clambda1 (when (ref-p ref1) (ref-leaf ref1))))
                       (when (lambda-p clambda1)
                         (dolist (var (lambda-vars clambda1) t)
                           (dolist (var-ref (lambda-var-refs var))
                             (let ((dest (principal-lvar-dest (ref-lvar var-ref))))
                               (unless (and (combination-p dest) (recurse dest))
                                 (return-from combination-args-flow-cleanly-p nil)))))))))))
    (recurse combination1)))

(defun ref-good-for-dx-p (ref)
 (let* ((lvar (ref-lvar ref))
        (dest (when lvar (lvar-dest lvar))))
   (and (combination-p dest)
        (eq :known (combination-kind dest))
        (awhen (combination-fun-info dest)
          (or (ir1-attributep (fun-info-attributes it) dx-safe)
              (and (not (combination-lvar dest))
                   (awhen (fun-info-result-arg it)
                     (eql lvar (nth it (combination-args dest))))))))))

(defun trivial-lambda-var-ref-p (use)
  (and (ref-p use)
       (let ((var (ref-leaf use)))
         ;; lambda-var, no SETS, not explicitly indefinite-extent.
         (when (and (lambda-var-p var) (not (lambda-var-sets var))
                    (neq :indefinite (lambda-var-extent var)))
           (let ((home (lambda-var-home var))
                 (refs (lambda-var-refs var)))
             ;; bound by a non-XEP system lambda, no other REFS that aren't
             ;; DX-SAFE, or are result-args when the result is discarded.
             (when (and (lambda-system-lambda-p home)
                        (neq :external (lambda-kind home))
                        (dolist (ref refs t)
                          (unless (or (eq use ref) (ref-good-for-dx-p ref))
                            (return nil))))
               ;; the LAMBDA this var is bound by has only a single REF, going
               ;; to a combination
               (let* ((lambda-refs (lambda-refs home))
                      (primary (car lambda-refs)))
                 (and (ref-p primary)
                      (not (cdr lambda-refs))
                      (combination-p (lvar-dest (ref-lvar primary)))))))))))

(defun trivial-lambda-var-ref-lvar (use)
  (let* ((this (ref-leaf use))
         (fun (lambda-var-home this))
         (vars (lambda-vars fun))
         (combination (lvar-dest (ref-lvar (car (lambda-refs fun)))))
         (args (combination-args combination)))
    (aver (= (length vars) (length args)))
    (loop for var in vars
          for arg in args
          when (eq var this)
          return arg)))

;;; This needs to play nice with LVAR-GOOD-FOR-DX-P and friends.
(defun handle-nested-dynamic-extent-lvars (dx lvar &optional recheck-component)
  (let ((uses (lvar-uses lvar)))
    ;; DX value generators must end their blocks: see UPDATE-UVL-LIVE-SETS.
    ;; Uses of mupltiple-use LVARs already end their blocks, so we just need
    ;; to process uses of single-use LVARs.
    (when (node-p uses)
      (node-ends-block uses))
    ;; If this LVAR's USE is good for DX, it is either a CAST, or it
    ;; must be a regular combination whose arguments are potentially DX as well.
    (flet ((recurse (use)
             (etypecase use
               (cast
                (handle-nested-dynamic-extent-lvars
                 dx (cast-value use) recheck-component))
               (combination
                (loop for arg in (combination-args use)
                      ;; deleted args show up as NIL here
                      when (and arg
                                (lvar-good-for-dx-p arg dx recheck-component))
                      append (handle-nested-dynamic-extent-lvars
                              dx arg recheck-component)))
               (ref
                (let* ((other (trivial-lambda-var-ref-lvar use)))
                  (unless (eq other lvar)
                    (handle-nested-dynamic-extent-lvars
                     dx other recheck-component)))))))
      (cons (cons dx lvar)
            (if (listp uses) ; TODO use ENSURE-LIST? or is it too slow?
                (loop for use in uses
                      when (use-good-for-dx-p use dx recheck-component)
                      nconc (recurse use))
                (when (use-good-for-dx-p uses dx recheck-component)
                  (recurse uses)))))))

;;;;; BLOCK UTILS

(declaim (inline block-to-be-deleted-p))
(defun block-to-be-deleted-p (block)
  (or (block-delete-p block)
      (eq (functional-kind (block-home-lambda block)) :deleted)))

;;; Checks whether NODE is in a block to be deleted
(declaim (inline node-to-be-deleted-p))
(defun node-to-be-deleted-p (node)
  (block-to-be-deleted-p (node-block node)))

(declaim (ftype (sfunction (clambda) cblock) lambda-block))
(defun lambda-block (clambda)
  (node-block (lambda-bind clambda)))
(declaim (ftype (sfunction (clambda) component) lambda-component))
(defun lambda-component (clambda)
  (block-component (lambda-block clambda)))

(declaim (ftype (sfunction (cblock) node) block-start-node))
(defun block-start-node (block)
  (ctran-next (block-start block)))

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
(declaim (ftype (sfunction (cblock) clambda) block-home-lambda))
(defun block-home-lambda (block)
  (block-home-lambda-or-null block))

;;; Return the IR1 physical environment for BLOCK.
(declaim (ftype (sfunction (cblock) physenv) block-physenv))
(defun block-physenv (block)
  (lambda-physenv (block-home-lambda block)))

;;; Return the Top Level Form number of PATH, i.e. the ordinal number
;;; of its original source's top level form in its compilation unit.
(defun source-path-tlf-number (path)
  (declare (list path))
  (car (last path)))

;;; Return the (reversed) list for the PATH in the original source
;;; (with the Top Level Form number last).
(defun source-path-original-source (path)
  (declare (list path) (inline member))
  (cddr (member 'original-source-start path :test #'eq)))

;;; Return the Form Number of PATH's original source inside the Top
;;; Level Form that contains it. This is determined by the order that
;;; we walk the subforms of the top level source form.
(defun source-path-form-number (path)
  (declare (list path) (inline member))
  (cadr (member 'original-source-start path :test #'eq)))

;;; Return a list of all the enclosing forms not in the original
;;; source that converted to get to this form, with the immediate
;;; source for node at the start of the list.
(defun source-path-forms (path)
  (subseq path 0 (position 'original-source-start path)))

(defun tree-some (predicate tree)
  (let ((seen (make-hash-table)))
    (labels ((walk (tree)
               (cond ((funcall predicate tree))
                     ((and (consp tree)
                           (not (gethash tree seen)))
                      (setf (gethash tree seen) t)
                      (or (walk (car tree))
                          (walk (cdr tree)))))))
      (walk tree))))

;;; Return the innermost source form for NODE.
(defun node-source-form (node)
  (declare (type node node))
  (let* ((path (node-source-path node))
         (forms (remove-if (lambda (x)
                             (tree-some #'leaf-p x))
                           (source-path-forms path))))
    ;; another option: if first form includes a leaf, return
    ;; find-original-source instead.
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
  (let ((use (lvar-uses lvar)))
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

#!-sb-fluid (declaim (inline lvar-single-value-p))
(defun lvar-single-value-p (lvar)
  (or (not lvar) (%lvar-single-value-p lvar)))
(defun %lvar-single-value-p (lvar)
  (let ((dest (lvar-dest lvar)))
    (typecase dest
      ((or creturn exit)
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
        while (cast-p dest)
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
                         (user-data (lexenv-user-data default)))
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
     lambda
     cleanup handled-conditions disabled-package-locks
     policy
     user-data
     default)))

;;; Makes a LEXENV, suitable for using in a MACROLET introduced
;;; macroexpander
(defun make-restricted-lexenv (lexenv)
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
               ((or leaf #!+sb-eval (member :bogus)) nil)
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
     (lexenv-handled-conditions lexenv)
     (lexenv-disabled-package-locks lexenv)
     (lexenv-policy lexenv)
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

;;; This is like LINK-BLOCKS, but we separate BLOCK1 and BLOCK2. If
;;; this leaves a successor with a single predecessor that ends in an
;;; IF, then set BLOCK-TEST-MODIFIED so that any test constraint will
;;; now be able to be propagated to the successor.
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

  (let ((new-pred (delq block1 (block-pred block2))))
    (setf (block-pred block2) new-pred)
    (when (singleton-p new-pred)
      (let ((pred-block (first new-pred)))
        (when (if-p (block-last pred-block))
          (setf (block-test-modified pred-block) t)))))
  (values))

;;; Swing the succ/pred link between BLOCK and OLD to be between BLOCK
;;; and NEW. If BLOCK ends in an IF, then we have to fix up the
;;; consequent/alternative blocks to point to NEW. We also set
;;; BLOCK-TEST-MODIFIED so that any test constraint will be applied to
;;; the new successor.
(defun change-block-successor (block old new)
  (declare (type cblock new old block))
  (unlink-blocks block old)
  (let ((last (block-last block))
        (comp (block-component block)))
    (setf (component-reanalyze comp) t)
    (typecase last
      (cif
       (setf (block-test-modified block) t)
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
             (reoptimize-component (block-component block) :maybe)))))
      (t
       (unless (memq new (block-succ block))
         (link-blocks block new)))))

  (values))

;;; Unlink a block from the next/prev chain. We also null out the
;;; COMPONENT.
(declaim (ftype (sfunction (cblock) (values)) remove-from-dfo))
(defun remove-from-dfo (block)
  (let ((next (block-next block))
        (prev (block-prev block)))
    (setf (block-component block) nil)
    (setf (block-next prev) next)
    (setf (block-prev next) prev))
  (values))

;;; Add BLOCK to the next/prev chain following AFTER. We also set the
;;; COMPONENT to be the same as for AFTER.
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

;;; List all NLX-INFOs which BLOCK can exit to.
;;;
;;; We hope that no cleanup actions are performed in the middle of
;;; BLOCK, so it is enough to look only at cleanups in the block
;;; end. The tricky thing is a special cleanup block; all its nodes
;;; have the same cleanup info, corresponding to the start, so the
;;; same approach returns safe result.
(defun map-block-nlxes (fun block &optional dx-cleanup-fun)
  (do-nested-cleanups (cleanup block)
    (let ((mess-up (cleanup-mess-up cleanup)))
      (case (cleanup-kind cleanup)
        ((:block :tagbody)
         (aver (entry-p mess-up))
         (loop for exit in (entry-exits mess-up)
            for nlx-info = (exit-nlx-info exit)
            do (funcall fun nlx-info)))
        ((:catch :unwind-protect)
         (aver (combination-p mess-up))
         (let* ((arg-lvar (first (basic-combination-args mess-up)))
                (nlx-info (constant-value (ref-leaf (lvar-use arg-lvar)))))
           (funcall fun nlx-info)))
        ((:dynamic-extent)
         (when dx-cleanup-fun
           (funcall dx-cleanup-fun cleanup)))))))

;;; Set the FLAG for all the blocks in COMPONENT to NIL, except for
;;; the head and tail which are set to T.
(declaim (ftype (sfunction (component) (values)) clear-flags))
(defun clear-flags (component)
  (let ((head (component-head component))
        (tail (component-tail component)))
    (setf (block-flag head) t)
    (setf (block-flag tail) t)
    (do-blocks (block component)
      (setf (block-flag block) nil)))
  (values))

;;; Make a component with no blocks in it. The BLOCK-FLAG is initially
;;; true in the head and tail blocks.
(declaim (ftype (sfunction () component) make-empty-component))
(defun make-empty-component ()
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
(defun node-ends-block (node)
  (declare (type node node))
  (let* ((block (node-block node))
         (start (node-next node))
         (last (block-last block)))
    (check-type last node)
    (unless (eq last node)
      (aver (and (eq (ctran-kind start) :inside-block)
                 (not (block-delete-p block))))
      (let* ((succ (block-succ block))
             (new-block
              (make-block-key :start start
                              :component (block-component block)
                              :succ succ :last last)))
        (setf (ctran-kind start) :block-start)
        (setf (ctran-use start) nil)
        (setf (block-last block) node)
        (setf (node-next node) nil)
        (dolist (b succ)
          (setf (block-pred b)
                (cons new-block (remove block (block-pred b)))))
        (setf (block-succ block) ())
        (link-blocks block new-block)
        (add-to-dfo new-block block)
        (setf (component-reanalyze (block-component block)) t)

        (do ((ctran start (node-next (ctran-next ctran))))
            ((not ctran))
          (setf (ctran-block ctran) new-block))

        (setf (block-type-asserted block) t)
        (setf (block-test-modified block) t))))
  (values))

;;;; deleting stuff

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
        (when (and (combination-p dest)
                   (eq (basic-combination-fun dest) lvar)
                   (eq (basic-combination-kind dest) :local))
          (let* ((args (basic-combination-args dest))
                 (arg (elt args n)))
            (reoptimize-lvar arg)
            (flush-dest arg)
            (setf (elt args n) nil))))))

  ;; The LAMBDA-VAR may still have some SETs, but this doesn't cause
  ;; too much difficulty, since we can efficiently implement
  ;; write-only variables. We iterate over the SETs, marking their
  ;; blocks for dead code flushing, since we can delete SETs whose
  ;; value is unused.
  (dolist (set (lambda-var-sets leaf))
    (setf (block-flush-p (node-block set)) t))

  (values))

;;; Note that something interesting has happened to VAR.
(defun reoptimize-lambda-var (var)
  (declare (type lambda-var var))
  (let ((fun (lambda-var-home var)))
    ;; We only deal with LET variables, marking the corresponding
    ;; initial value arg as needing to be reoptimized.
    (when (and (eq (functional-kind fun) :let)
               (leaf-refs var))
      (do ((args (basic-combination-args
                  (lvar-dest (node-lvar (first (leaf-refs fun)))))
                 (cdr args))
           (vars (lambda-vars fun) (cdr vars)))
          ((eq (car vars) var)
           (reoptimize-lvar (car args))))))
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

;;; Deal with deleting the last reference to a CLAMBDA, which means
;;; that the lambda is unreachable, so that its body may be
;;; deleted. We set FUNCTIONAL-KIND to :DELETED and rely on
;;; IR1-OPTIMIZE to delete its blocks.
(defun delete-lambda (clambda)
  (declare (type clambda clambda))
  (let ((original-kind (functional-kind clambda))
        (bind (lambda-bind clambda)))
    (aver (not (member original-kind '(:deleted :toplevel))))
    (aver (not (functional-has-external-references-p clambda)))
    (aver (or (eq original-kind :zombie) bind))
    (setf (functional-kind clambda) :deleted)
    (setf (lambda-bind clambda) nil)

    (labels ((delete-children (lambda)
               (dolist (child (lambda-children lambda))
                 (cond ((eq (functional-kind child) :deleted)
                        (delete-children child))
                       (t
                        (delete-lambda child))))
               (setf (lambda-children lambda) nil)
               (setf (lambda-parent lambda) nil)))
      (delete-children clambda))

    ;; (The IF test is (FUNCTIONAL-SOMEWHAT-LETLIKE-P CLAMBDA), except
    ;; that we're using the old value of the KIND slot, not the
    ;; current slot value, which has now been set to :DELETED.)
    (case original-kind
      (:zombie)
      ((:let :mv-let :assignment)
       (let ((bind-block (node-block bind)))
         (mark-for-deletion bind-block))
       (let ((home (lambda-home clambda)))
         (setf (lambda-lets home) (delete clambda (lambda-lets home))))
       ;; KLUDGE: In presence of NLEs we cannot always understand that
       ;; LET's BIND dominates its body [for a LET "its" body is not
       ;; quite its]; let's delete too dangerous for IR2 stuff. --
       ;; APD, 2004-01-01
       (dolist (var (lambda-vars clambda))
         (flet ((delete-node (node)
                  (mark-for-deletion (node-block node))))
         (mapc #'delete-node (leaf-refs var))
         (mapc #'delete-node (lambda-var-sets var)))))
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
         (unless (leaf-ever-used clambda)
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
               (delq clambda (component-lambdas component))))))

    ;; If the lambda is an XEP, then we null out the ENTRY-FUN in its
    ;; ENTRY-FUN so that people will know that it is not an entry
    ;; point anymore.
    (when (eq original-kind :external)
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
    (unless (and entry (leaf-refs entry))
      (aver (or (not entry) (eq (functional-kind entry) :deleted)))
      (setf (functional-kind leaf) :deleted)

      (flet ((frob (fun)
               (unless (eq (functional-kind fun) :deleted)
                 (aver (eq (functional-kind fun) :optional))
                 (setf (functional-kind fun) nil)
                 (let ((refs (leaf-refs fun)))
                   (cond ((null refs)
                          (delete-lambda fun))
                         ((null (rest refs))
                          (or (maybe-let-convert fun)
                              (maybe-convert-to-assignment fun)))
                         (t
                          (maybe-convert-to-assignment fun)))))))

        (dolist (ep (optional-dispatch-entry-points leaf))
          (when (promise-ready-p ep)
            (frob (force ep))))
        (when (optional-dispatch-more-entry leaf)
          (frob (optional-dispatch-more-entry leaf)))
        (let ((main (optional-dispatch-main-entry leaf)))
          (when entry
            (setf (functional-entry-fun entry) main)
            (setf (functional-entry-fun main) entry))
          (when (eq (functional-kind main) :optional)
            (frob main))))))

  (values))

(defun note-local-functional (fun)
  (declare (type functional fun))
  (when (and (leaf-has-source-name-p fun)
             (eq (leaf-source-name fun) (functional-debug-name fun)))
    (let ((name (leaf-source-name fun)))
      (let ((defined-fun (gethash name *free-funs*)))
        (when (and defined-fun
                   (defined-fun-p defined-fun)
                   (eq (defined-fun-functional defined-fun) fun))
          (remhash name *free-funs*))))))

;;; Return functional for DEFINED-FUN which has been converted in policy
;;; corresponding to the current one, or NIL if no such functional exists.
;;;
;;; Also check that the parent of the functional is visible in the current
;;; environment.
(defun defined-fun-functional (defined-fun)
  (let ((functionals (defined-fun-functionals defined-fun)))
    (when functionals
      (let* ((sample (car functionals))
             (there (lambda-parent (if (lambda-p sample)
                                       sample
                                       (optional-dispatch-main-entry sample)))))
        (when there
          (labels ((lookup (here)
                     (unless (eq here there)
                       (if here
                           (lookup (lambda-parent here))
                           ;; We looked up all the way up, and didn't find the parent
                           ;; of the functional -- therefore it is nested in a lambda
                           ;; we don't see, so return nil.
                           (return-from defined-fun-functional nil)))))
            (lookup (lexenv-lambda *lexenv*)))))
      ;; Now find a functional whose policy matches the current one, if we already
      ;; have one.
      (let ((policy (lexenv-%policy *lexenv*)))
        (dolist (functional functionals)
          (when (policy= policy (lexenv-%policy (functional-lexenv functional)))
            (return functional)))))))

;;; Do stuff to delete the semantic attachments of a REF node. When
;;; this leaves zero or one reference, we do a type dispatch off of
;;; the leaf to determine if a special action is appropriate.
(defun delete-ref (ref)
  (declare (type ref ref))
  (let* ((leaf (ref-leaf ref))
         (refs (delq ref (leaf-refs leaf))))
    (setf (leaf-refs leaf) refs)

    (cond ((null refs)
           (typecase leaf
             (lambda-var
              (delete-lambda-var leaf))
             (clambda
              (ecase (functional-kind leaf)
                ((nil :let :mv-let :assignment :escape :cleanup)
                 (aver (null (functional-entry-fun leaf)))
                 (delete-lambda leaf))
                (:external
                 (unless (functional-has-external-references-p leaf)
                   (delete-lambda leaf)))
                ((:deleted :zombie :optional))))
             (optional-dispatch
              (unless (eq (functional-kind leaf) :deleted)
                (delete-optional-dispatch leaf)))))
          ((null (rest refs))
           (typecase leaf
             (clambda (or (maybe-let-convert leaf)
                          (maybe-convert-to-assignment leaf)))
             (lambda-var (reoptimize-lambda-var leaf))))
          (t
           (typecase leaf
             (clambda (maybe-convert-to-assignment leaf))))))

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
    (setf (block-attributep (block-flags block)
                            flush-p type-asserted type-check)
          t))
  (setf (node-lvar node) nil))

;;; This function is called by people who delete nodes; it provides a
;;; way to indicate that the value of a lvar is no longer used. We
;;; null out the LVAR-DEST, set FLUSH-P in the blocks containing uses
;;; of LVAR and set COMPONENT-REOPTIMIZE.
(defun flush-dest (lvar)
  (declare (type (or lvar null) lvar))
  (unless (null lvar)
    (when (lvar-dynamic-extent lvar)
      (note-no-stack-allocation lvar :flush t))
    (setf (lvar-dest lvar) nil)
    (flush-lvar-externally-checkable-type lvar)
    (do-uses (use lvar)
      (flush-node use))
    (setf (lvar-uses lvar) nil))
  (values))

(defun delete-dest (lvar)
  (when lvar
    (let* ((dest (lvar-dest lvar))
           (prev (node-prev dest)))
      (let ((block (ctran-block prev)))
        (unless (block-delete-p block)
          (mark-for-deletion block))))))

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
  (aver (block-component block))      ; else block is already deleted!
  #!+high-security (aver (not (memq block (component-delete-blocks (block-component block)))))
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
         (unless (eq (functional-kind lambda) :deleted)
           (delete-lambda lambda))))
      (exit
       (let ((value (exit-value node))
             (entry (exit-entry node)))
         (when value
           (flush-dest value))
         (when entry
           (setf (entry-exits entry)
                 (delq node (entry-exits entry))))))
      (entry
       (dolist (exit (entry-exits node))
         (mark-for-deletion (node-block exit)))
       (let ((home (node-home-lambda node)))
         (setf (lambda-entries home) (delq node (lambda-entries home)))))
      (creturn
       (flush-dest (return-result node))
       (delete-return node))
      (cset
       (flush-dest (set-value node))
       (let ((var (set-var node)))
         (setf (basic-var-sets var)
               (delete node (basic-var-sets var)))))
      (cast
       (flush-dest (cast-value node)))))

  (remove-from-dfo block)
  (values))

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
    (unless (or (leaf-ever-used var)
                (lambda-var-ignorep var))
      (unless (policy policy (= inhibit-warnings 3))
        ;; ANSI section "3.2.5 Exceptional Situations in the Compiler"
        ;; requires this to be no more than a STYLE-WARNING.
        #-sb-xc-host
        (compiler-style-warn "The variable ~S is defined but never used."
                             (leaf-debug-name var))
        ;; There's no reason to accept this kind of equivocation
        ;; when compiling our own code, though.
        #+sb-xc-host
        (warn "The variable ~S is defined but never used."
              (leaf-debug-name var)))
      (setf (leaf-ever-used var) t)))) ; to avoid repeated warnings? -- WHN

(defun note-unreferenced-fun-vars (fun)
  (declare (type clambda fun))
  (let ((*compiler-error-context* (lambda-bind fun)))
    (note-unreferenced-vars (lambda-vars fun)
                            *compiler-error-context*))
  (values))

(defvar *deletion-ignored-objects* '(t nil))

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
    (unless (eq (functional-kind home) :deleted)
      (do-nodes (node nil block)
        (let* ((path (node-source-path node))
               (first (first path)))
          (when (or (eq first 'original-source-start)
                    (and (atom first)
                         (or (not (symbolp first))
                             (let ((pkg (symbol-package first)))
                               (and pkg
                                    (not (eq pkg (symbol-package :end))))))
                         (not (member first *deletion-ignored-objects*))
                         (not (typep first '(or fixnum character)))
                         (every (lambda (x)
                                  (present-in-form first x 0))
                                (source-path-forms path))
                         (present-in-form first (find-original-source path)
                                          0)))
            (unless (return-p node)
              (let ((*compiler-error-context* node))
                (compiler-notify 'code-deletion-note
                                 :format-control "deleting unreachable code"
                                 :format-arguments nil)))
            (return))))))
  (values))

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

    (setf (block-type-asserted block) t)
    (setf (block-test-modified block) t)

    (cond ((or (eq prev-kind :inside-block)
               (and (eq prev-kind :block-start)
                    (not (eq node last))))
           (cond ((eq node last)
                  (setf (block-last block) (ctran-use prev))
                  (setf (node-next (ctran-use prev)) nil))
                 (t
                  (setf (ctran-next prev) next)
                  (setf (node-prev next) prev)
                  (when (if-p next) ; AOP wanted
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
                         (delq block (component-delete-blocks component)))))
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
    (unless (eq (functional-kind fun) :deleted)
      (setf (functional-kind fun) nil)
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
(defun splice-fun-args (lvar fun num-args)
  #!+sb-doc
  "If LVAR is a call to FUN with NUM-ARGS args, change those arguments to feed
directly to the LVAR-DEST of LVAR, which must be a combination. If FUN
is :ANY, the function name is not checked."
  (declare (type lvar lvar)
           (type symbol fun)
           (type index num-args))
  (let ((outside (lvar-dest lvar))
        (inside (lvar-uses lvar)))
    (aver (combination-p outside))
    (unless (combination-p inside)
      (give-up-ir1-transform))
    (let ((inside-fun (combination-fun inside)))
      (unless (or (eq fun :any)
                  (eq (lvar-fun-name inside-fun) fun))
        (give-up-ir1-transform))
      (let ((inside-args (combination-args inside)))
        (unless (= (length inside-args) num-args)
          (give-up-ir1-transform))
        (let* ((outside-args (combination-args outside))
               (arg-position (position lvar outside-args))
               (before-args (subseq outside-args 0 arg-position))
               (after-args (subseq outside-args (1+ arg-position))))
          (dolist (arg inside-args)
            (setf (lvar-dest arg) outside)
            (flush-lvar-externally-checkable-type arg))
          (setf (combination-args inside) nil)
          (setf (combination-args outside)
                (append before-args inside-args after-args))
          (change-ref-leaf (lvar-uses inside-fun)
                           (find-free-fun 'list "???"))
          (setf (combination-fun-info inside) (info :function :info 'list)
                (combination-kind inside) :known)
          (setf (node-derived-type inside) *wild-type*)
          (flush-dest lvar)
          inside-args)))))

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


;;;; leaf hackery

;;; Change the LEAF that a REF refers to.
(defun change-ref-leaf (ref leaf &key recklessly)
  (declare (type ref ref) (type leaf leaf))
  (unless (eq (ref-leaf ref) leaf)
    (push ref (leaf-refs leaf))
    (delete-ref ref)
    (setf (ref-leaf ref) leaf)
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
  (dolist (ref (leaf-refs old-leaf))
    (when (funcall test ref)
      (change-ref-leaf ref new-leaf)))
  (values))

;;; Return a LEAF which represents the specified constant object. If
;;; the object is not in *CONSTANTS*, then we create a new constant
;;; LEAF and enter it. If we are producing a fasl file, make sure that
;;; MAKE-LOAD-FORM gets used on any parts of the constant that it
;;; needs to be.
;;;
;;; We are allowed to coalesce things like EQUAL strings and bit-vectors
;;; when file-compiling, but not when using COMPILE.
(defun find-constant (object &optional (name nil namep))
  (let ((faslp (producing-fasl-file)))
    (labels ((make-it ()
               (when faslp
                 (if namep
                     (maybe-emit-make-load-forms object name)
                     (maybe-emit-make-load-forms object)))
               (make-constant object))
             (core-coalesce-p (x)
               ;; True for things which retain their identity under EQUAL,
               ;; so we can safely share the same CONSTANT leaf between
               ;; multiple references.
               (or (typep x '(or symbol number character))
                   ;; Amusingly enough, we see CLAMBDAs --among other things--
                   ;; here, from compiling things like %ALLOCATE-CLOSUREs forms.
                   ;; No point in stuffing them in the hash-table.
                   (and (typep x 'instance)
                        (not (or (leaf-p x) (node-p x))))))
             (file-coalesce-p (x)
               ;; CLHS 3.2.4.2.2: We are also allowed to coalesce various
               ;; other things when file-compiling.
               (or (core-coalesce-p x)
                   (if (consp x)
                       (if (eq +code-coverage-unmarked+ (cdr x))
                           ;; These are already coalesced, and the CAR should
                           ;; always be OK, so no need to check.
                           t
                           (unless (maybe-cyclic-p x) ; safe for EQUAL?
                             (do ((y x (cdr y)))
                                 ((atom y) (file-coalesce-p y))
                               (unless (file-coalesce-p (car y))
                                 (return nil)))))
                       ;; We *could* coalesce base-strings as well,
                       ;; but we'd need a separate hash-table for
                       ;; that, since we are not allowed to coalesce
                       ;; base-strings with non-base-strings.
                       (typep x
                              '(or bit-vector
                                ;; in the cross-compiler, we coalesce
                                ;; all strings with the same contents,
                                ;; because we will end up dumping them
                                ;; as base-strings anyway.  In the
                                ;; real compiler, we're not allowed to
                                ;; coalesce regardless of string
                                ;; specialized element type, so we
                                ;; KLUDGE by coalescing only character
                                ;; strings (the common case) and
                                ;; punting on the other types.
                                #+sb-xc-host
                                string
                                #-sb-xc-host
                                (vector character))))))
             (coalescep (x)
               (if faslp (file-coalesce-p x) (core-coalesce-p x))))
      (if (and (boundp '*constants*) (coalescep object))
          (or (gethash object *constants*)
              (setf (gethash object *constants*)
                    (make-it)))
          (make-it)))))

;;; Return true if VAR would have to be closed over if environment
;;; analysis ran now (i.e. if there are any uses that have a different
;;; home lambda than VAR's home.)
(defun closure-var-p (var)
  (declare (type lambda-var var))
  (let ((home (lambda-var-home var)))
    (cond ((eq (functional-kind home) :deleted)
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
    (dolist (nlx (physenv-nlx-info (node-physenv entry)) nil)
      (when (and (eq (nlx-info-block nlx) block)
                 (eq (nlx-info-cleanup nlx) cleanup))
        (return nlx)))))

(defun nlx-info-lvar (nlx)
  (declare (type nlx-info nlx))
  (node-lvar (block-last (nlx-info-target nlx))))

;;;; functional hackery

(declaim (ftype (sfunction (functional) clambda) main-entry))
(defun main-entry (functional)
  (etypecase functional
    (clambda functional)
    (optional-dispatch
     (optional-dispatch-main-entry functional))))

;;; RETURN true if FUNCTIONAL is a thing that can be treated like
;;; MV-BIND when it appears in an MV-CALL. All fixed arguments must be
;;; optional with null default and no SUPPLIED-P. There must be a
;;; &REST arg with no references.
(declaim (ftype (sfunction (functional) boolean) looks-like-an-mv-bind))
(defun looks-like-an-mv-bind (functional)
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
              (return (and (null (cdr arg)) (null (leaf-refs (car arg))))))
             (t
              (return nil)))))))

;;; Return true if function is an external entry point. This is true
;;; of normal XEPs (:EXTERNAL kind) and also of top level lambdas
;;; (:TOPLEVEL kind.)
(defun xep-p (fun)
  (declare (type functional fun))
  (not (null (member (functional-kind fun) '(:external :toplevel)))))

;;; If LVAR's only use is a non-notinline global function reference,
;;; then return the referenced symbol, otherwise NIL. If NOTINLINE-OK
;;; is true, then we don't care if the leaf is NOTINLINE.
(defun lvar-fun-name (lvar &optional notinline-ok)
  (declare (type lvar lvar))
  (let ((use (lvar-uses lvar)))
    (if (ref-p use)
        (let ((leaf (ref-leaf use)))
          (if (and (global-var-p leaf)
                   (eq (global-var-kind leaf) :global-function)
                   (or (not (defined-fun-p leaf))
                       (not (eq (defined-fun-inlinep leaf) :notinline))
                       notinline-ok))
              (leaf-source-name leaf)
              nil))
        nil)))

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
  (leaf-debug-name (ref-leaf (lvar-uses (combination-fun combination)))))

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

(defvar *inline-expansion-limit* 200
  #!+sb-doc
  "an upper limit on the number of inline function calls that will be expanded
   in any given code object (single function or block compilation)")

;;; Check whether NODE's component has exceeded its inline expansion
;;; limit, and warn if so, returning NIL.
(defun inline-expansion-ok (node)
  (let ((expanded (incf (component-inline-expansions
                         (block-component
                          (node-block node))))))
    (cond ((> expanded *inline-expansion-limit*) nil)
          ((= expanded *inline-expansion-limit*)
           ;; FIXME: If the objective is to stop the recursive
           ;; expansion of inline functions, wouldn't it be more
           ;; correct to look back through surrounding expansions
           ;; (which are, I think, stored in the *CURRENT-PATH*, and
           ;; possibly stored elsewhere too) and suppress expansion
           ;; and print this warning when the function being proposed
           ;; for inline expansion is found there? (I don't like the
           ;; arbitrary numerical limit in principle, and I think
           ;; it'll be a nuisance in practice if we ever want the
           ;; compiler to be able to use WITH-COMPILATION-UNIT on
           ;; arbitrarily huge blocks of code. -- WHN)
           (let ((*compiler-error-context* node))
             (compiler-notify "*INLINE-EXPANSION-LIMIT* (~W) was exceeded, ~
                               probably trying to~%  ~
                               inline a recursive function."
                              *inline-expansion-limit*))
           nil)
          (t t))))

;;; Make sure that FUNCTIONAL is not let-converted or deleted.
(defun assure-functional-live-p (functional)
  (declare (type functional functional))
  (when (and (or
              ;; looks LET-converted
              (functional-somewhat-letlike-p functional)
              ;; It's possible for a LET-converted function to end up
              ;; deleted later. In that case, for the purposes of this
              ;; analysis, it is LET-converted: LET-converted functionals
              ;; are too badly trashed to expand them inline, and deleted
              ;; LET-converted functionals are even worse.
              (memq (functional-kind functional) '(:deleted :zombie))))
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
        (and (eq kind :known)
             (let ((info (basic-combination-fun-info call)))
               (and
                (not (fun-info-ir2-convert info))
                (dolist (template (fun-info-templates info) t)
                  (when (eq (template-ltn-policy template) :fast-safe)
                    (multiple-value-bind (val win)
                       (valid-fun-use call (template-type template))
                      (when (or val (not win)) (return nil)))))))))))

;;;; careful call

;;; Apply a function to some arguments, returning a list of the values
;;; resulting of the evaluation. If an error is signalled during the
;;; application, then we produce a warning message using WARN-FUN and
;;; return NIL as our second value to indicate this. NODE is used as
;;; the error context for any error message, and CONTEXT is a string
;;; that is spliced into the warning.
(declaim (ftype (sfunction ((or symbol function) list node function string)
                          (values list boolean))
                careful-call))
(defun careful-call (function args node warn-fun context)
  (values
   (multiple-value-list
    (handler-case (apply function args)
      (error (condition)
        (let ((*compiler-error-context* node))
          (funcall warn-fun "Lisp error during ~A:~%~A" context condition)
          (return-from careful-call (values nil nil))))))
   t))

;;; Variations of SPECIFIER-TYPE for parsing possibly wrong
;;; specifiers.
(macrolet
    ((deffrob (basic careful compiler transform)
       `(progn
          (defun ,careful (specifier)
            (handler-case (,basic specifier)
              ((or sb!kernel::arg-count-error
                type-error) (condition)
                (values nil (list (princ-to-string condition))))
              (simple-error (condition)
                (values nil (list* (simple-condition-format-control condition)
                                   (simple-condition-format-arguments condition))))))
          (defun ,compiler (specifier)
            (multiple-value-bind (type error-args) (,careful specifier)
              (or type
                  (apply #'compiler-error error-args))))
          (defun ,transform (specifier)
            (multiple-value-bind (type error-args) (,careful specifier)
              (or type
                  (apply #'give-up-ir1-transform
                         error-args)))))))
  (deffrob specifier-type careful-specifier-type compiler-specifier-type ir1-transform-specifier-type)
  (deffrob values-specifier-type careful-values-specifier-type compiler-values-specifier-type ir1-transform-values-specifier-type))


;;;; utilities used at run-time for parsing &KEY args in IR1

;;; This function is used by the result of PARSE-DEFTRANSFORM to find
;;; the lvar for the value of the &KEY argument KEY in the list of
;;; lvars ARGS. It returns the lvar if the keyword is present, or NIL
;;; otherwise. The legality and constantness of the keywords should
;;; already have been checked.
(declaim (ftype (sfunction (list keyword) (or lvar null))
                find-keyword-lvar))
(defun find-keyword-lvar (args key)
  (do ((arg args (cddr arg)))
      ((null arg) nil)
    (when (eq (lvar-value (first arg)) key)
      (return (second arg)))))

;;; This function is used by the result of PARSE-DEFTRANSFORM to
;;; verify that alternating lvars in ARGS are constant and that there
;;; is an even number of args.
(declaim (ftype (sfunction (list) boolean) check-key-args-constant))
(defun check-key-args-constant (args)
  (do ((arg args (cddr arg)))
      ((null arg) t)
    (unless (and (rest arg)
                 (constant-lvar-p (first arg)))
      (return nil))))

;;; This function is used by the result of PARSE-DEFTRANSFORM to
;;; verify that the list of lvars ARGS is a well-formed &KEY arglist
;;; and that only keywords present in the list KEYS are supplied.
(declaim (ftype (sfunction (list list) boolean) check-transform-keys))
(defun check-transform-keys (args keys)
  (and (check-key-args-constant args)
       (do ((arg args (cddr arg)))
           ((null arg) t)
         (unless (member (lvar-value (first arg)) keys)
           (return nil)))))

;;;; miscellaneous

;;; Called by the expansion of the EVENT macro.
(declaim (ftype (sfunction (event-info (or node null)) *) %event))
(defun %event (info node)
  (incf (event-info-count info))
  (when (and (>= (event-info-level info) *event-note-threshold*)
             (policy (or node *lexenv*)
                     (= inhibit-warnings 0)))
    (let ((*compiler-error-context* node))
      (compiler-notify (event-info-description info))))

  (let ((action (event-info-action info)))
    (when action (funcall action node))))

;;;
(defun make-cast (value type policy)
  (declare (type lvar value)
           (type ctype type)
           (type policy policy))
  (%make-cast :asserted-type type
              :type-to-check (maybe-weaken-check type policy)
              :value value
              :derived-type (coerce-to-values type)))

(defun cast-type-check (cast)
  (declare (type cast cast))
  (when (cast-reoptimize cast)
    (ir1-optimize-cast cast t))
  (cast-%type-check cast))

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
  (let ((use (lvar-uses lvar)))
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
           (and name
                (not (fun-lexically-notinline-p name)))))))

;;; Return true if LVAR's only use is a call to one of the named functions
;;; (or any function if none are specified) with the specified number of
;;; of arguments (or any number if number is not specified)
(defun lvar-matches (lvar &key fun-names arg-count)
  (let ((use (lvar-uses lvar)))
    (and (combination-p use)
         (or (not fun-names)
             (multiple-value-bind (name ok)
                 (combination-fun-source-name use nil)
               (and ok (member name fun-names :test #'eq))))
         (or (not arg-count)
             (= arg-count (length (combination-args use)))))))

;;; True if the optional has a rest-argument.
(defun optional-rest-p (opt)
  (dolist (var (optional-dispatch-arglist opt) nil)
    (let* ((info (when (lambda-var-p var)
                   (lambda-var-arg-info var)))
           (kind (when info
                   (arg-info-kind info))))
      (when (eq :rest kind)
        (return t)))))

;;; Don't substitute single-ref variables on high-debug / low speed, to
;;; improve the debugging experience. ...but don't bother keeping those
;;; from system lambdas.
(defun preserve-single-use-debug-var-p (call var)
  (and (policy call (eql preserve-single-use-debug-variables 3))
       (or (not (lambda-var-p var))
           (not (lambda-system-lambda-p (lambda-var-home var))))))
