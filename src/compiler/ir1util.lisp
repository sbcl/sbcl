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
    (let ((cup (lexenv-cleanup lexenv)))
      (when cup (return cup)))))

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
  (let ((uses (lvar-uses lvar)))
    (if (listp uses)
        uses
        (list uses))))

(defun principal-lvar-use (lvar)
  (let ((use (lvar-uses lvar)))
    (if (cast-p use)
        (principal-lvar-use (cast-value use))
        use)))

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
  (let* ((lvar (node-lvar node)))
    (when lvar
      (if (listp (lvar-uses lvar))
          (let ((new-uses (delq node (lvar-uses lvar))))
            (setf (lvar-uses lvar)
                  (if (singleton-p new-uses)
                      (first new-uses)
                      new-uses)))
          (setf (lvar-uses lvar) nil))
      (setf (node-lvar node) nil)))
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
;;; arbitary number of uses.
(defun substitute-lvar-uses (new old)
  (declare (type lvar old)
           (type (or lvar null) new))

  (do-uses (node old)
    (%delete-lvar-use node)
    (when new
      (add-lvar-use node new)))

  (when new (reoptimize-lvar new))
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

;;;;

;;; Filter values of LVAR through FORM, which must be an ordinary/mv
;;; call. First argument must be 'DUMMY, which will be replaced with
;;; LVAR. In case of an ordinary call the function should not have
;;; return type NIL. We create a new "filtered" lvar.
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
        ;; check lvar. We substitute for the first argument of
        ;; this node.
        (let* ((node (lvar-use filtered-lvar))
               (args (basic-combination-args node))
               (victim (first args)))
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
                (%delete-lvar-use node)
                (substitute-lvar-uses lvar value)
                (prog1
                    (unlink-node node)
                  (dolist (merge (merges))
                    (merge-tail-sets merge)))))
        (t (flush-dest value)
           (unlink-node node))))

;;;; miscellaneous shorthand functions

;;; Return the home (i.e. enclosing non-LET) CLAMBDA for NODE. Since
;;; the LEXENV-LAMBDA may be deleted, we must chain up the
;;; LAMBDA-CALL-LEXENV thread until we find a CLAMBDA that isn't
;;; deleted, and then return its home.
(defun node-home-lambda (node)
  (declare (type node node))
  (do ((fun (lexenv-lambda (node-lexenv node))
	    (lexenv-lambda (lambda-call-lexenv fun))))
      ((not (eq (functional-kind fun) :deleted))
       (lambda-home fun))
    (when (eq (lambda-home fun) fun)
      (return fun))))

#!-sb-fluid (declaim (inline node-block))
(defun node-block (node)
  (ctran-block (node-prev node)))
(declaim (ftype (sfunction (node) component) node-component))
(defun node-component (node)
  (block-component (node-block node)))
(declaim (ftype (sfunction (node) physenv) node-physenv))
(defun node-physenv (node)
  (lambda-physenv (node-home-lambda node)))
#!-sb-fluid (declaim (inline node-dest))
(defun node-dest (node)
  (awhen (node-lvar node) (lvar-dest it)))

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

;;; Return the unique node, delivering a value to LVAR.
#!-sb-fluid (declaim (inline lvar-use))
(defun lvar-use (lvar)
  (the (not list) (lvar-uses lvar)))

#!-sb-fluid (declaim (inline lvar-has-single-use-p))
(defun lvar-has-single-use-p (lvar)
  (typep (lvar-uses lvar) '(not list)))

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

#!-sb-fluid (declaim (inline lvar-single-value-p))
(defun lvar-single-value-p (lvar)
  (or (not lvar)
      (let ((dest (lvar-dest lvar)))
        (typecase dest
          ((or creturn exit)
           nil)
          (mv-combination
           (eq (basic-combination-fun dest) lvar))
          (cast
           (locally
               (declare (notinline lvar-single-value-p))
             (and (not (values-type-p (cast-asserted-type dest)))
                  (lvar-single-value-p (node-lvar dest)))))
          (t
           t)))))

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
;;; slot values. Values for the alist slots are NCONCed to the
;;; beginning of the current value, rather than replacing it entirely.
(defun make-lexenv (&key (default *lexenv*)
			 funs vars blocks tags
                         type-restrictions
			 (lambda (lexenv-lambda default))
			 (cleanup (lexenv-cleanup default))
			 (policy (lexenv-policy default)))
  (macrolet ((frob (var slot)
	       `(let ((old (,slot default)))
		  (if ,var
		      (nconc ,var old)
		      old))))
    (internal-make-lexenv
     (frob funs lexenv-funs)
     (frob vars lexenv-vars)
     (frob blocks lexenv-blocks)
     (frob tags lexenv-tags)
     (frob type-restrictions lexenv-type-restrictions)
     lambda cleanup policy)))

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
               (leaf nil)
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
     (lexenv-policy lexenv))))

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
             (setf (component-reoptimize (block-component block)) t)))))
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

;;; Deal with deleting the last reference to a CLAMBDA. Since there is
;;; only one way into a CLAMBDA, deleting the last reference to a
;;; CLAMBDA ensures that there is no way to reach any of the code in
;;; it. So we just set the FUNCTIONAL-KIND for FUN and its LETs to
;;; :DELETED, causing IR1 optimization to delete blocks in that
;;; CLAMBDA.
(defun delete-lambda (clambda)
  (declare (type clambda clambda))
  (let ((original-kind (functional-kind clambda))
	(bind (lambda-bind clambda)))
    (aver (not (member original-kind '(:deleted :optional :toplevel))))
    (aver (not (functional-has-external-references-p clambda)))
    (setf (functional-kind clambda) :deleted)
    (setf (lambda-bind clambda) nil)
    (dolist (let (lambda-lets clambda))
      (setf (lambda-bind let) nil)
      (setf (functional-kind let) :deleted))

    ;; LET may be deleted if its BIND is unreachable. Autonomous
    ;; function may be deleted if it has no reachable references.
    (unless (member original-kind '(:let :mv-let :assignment))
      (dolist (ref (lambda-refs clambda))
        (mark-for-deletion (node-block ref))))

    ;; (The IF test is (FUNCTIONAL-SOMEWHAT-LETLIKE-P CLAMBDA), except
    ;; that we're using the old value of the KIND slot, not the
    ;; current slot value, which has now been set to :DELETED.)
    (if (member original-kind '(:let :mv-let :assignment))
	(let ((home (lambda-home clambda)))
	  (setf (lambda-lets home) (delete clambda (lambda-lets home))))
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
		(delete clambda (component-lambdas component)))))

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
	  (when (eq (functional-kind main) :optional)
	    (frob main))))))

  (values))

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
		 (delete-lambda leaf))
		((:deleted :optional))))
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

;;; This function is called by people who delete nodes; it provides a
;;; way to indicate that the value of a lvar is no longer used. We
;;; null out the LVAR-DEST, set FLUSH-P in the blocks containing uses
;;; of LVAR and set COMPONENT-REOPTIMIZE.
(defun flush-dest (lvar)
  (declare (type (or lvar null) lvar))
  (unless (null lvar)
    (setf (lvar-dest lvar) nil)
    (flush-lvar-externally-checkable-type lvar)
    (do-uses (use lvar)
      (let ((prev (node-prev use)))
	(let ((block (ctran-block prev)))
          (setf (component-reoptimize (block-component block)) t)
          (setf (block-attributep (block-flags block) flush-p type-asserted)
                t)))
      (setf (node-lvar use) nil))
    (setf (lvar-uses lvar) nil))
  (values))

(defun delete-dest (lvar)
  (when lvar
    (let* ((dest (lvar-dest lvar))
           (prev (node-prev dest)))
      (let ((block (ctran-block prev)))
        (unless (block-delete-p block)
          (mark-for-deletion block))))))

;;; Do a graph walk backward from BLOCK, marking all predecessor
;;; blocks with the DELETE-P flag.
(defun mark-for-deletion (block)
  (declare (type cblock block))
  (let* ((component (block-component block))
         (head (component-head component)))
    (labels ((helper (block)
               (setf (block-delete-p block) t)
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
;;; semantic information.
(defun delete-block (block &optional silent)
  (declare (type cblock block))
  (aver (block-component block))      ; else block is already deleted!
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
    (typecase node
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
(defun note-unreferenced-vars (fun)
  (declare (type clambda fun))
  (dolist (var (lambda-vars fun))
    (unless (or (leaf-ever-used var)
		(lambda-var-ignorep var))
      (let ((*compiler-error-context* (lambda-bind fun)))
	(unless (policy *compiler-error-context* (= inhibit-warnings 3))
	  ;; ANSI section "3.2.5 Exceptional Situations in the Compiler"
	  ;; requires this to be no more than a STYLE-WARNING.
	  (compiler-style-warn "The variable ~S is defined but never used."
			       (leaf-debug-name var)))
	(setf (leaf-ever-used var) t)))) ; to avoid repeated warnings? -- WHN
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
	       (remove-from-dfo block)
               (setf (block-delete-p block) t)
	       (setf (node-prev node) nil)
	       t)))))))

;;; Return true if NODE has been deleted, false if it is still a valid
;;; part of IR1.
(defun node-deleted (node)
  (declare (type node node))
  (let ((prev (node-prev node)))
    (not (and prev
	      (let ((block (ctran-block prev)))
		(and (block-component block)
		     (not (block-delete-p block))))))))

;;; Delete all the blocks and functions in COMPONENT. We scan first
;;; marking the blocks as DELETE-P to prevent weird stuff from being
;;; triggered by deletion.
(defun delete-component (component)
  (declare (type component component))
  (aver (null (component-new-functionals component)))
  (setf (component-kind component) :deleted)
  (do-blocks (block component)
    (setf (block-delete-p block) t))
  (dolist (fun (component-lambdas component))
    (setf (functional-kind fun) nil)
    (setf (functional-entry-fun fun) nil)
    (setf (leaf-refs fun) nil)
    (delete-functional fun))
  (do-blocks (block component)
    (delete-block block))
  (values))

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
(defun extract-fun-args (lvar fun num-args)
  #!+sb-doc
  "If LVAR is a call to FUN with NUM-ARGS args, change those arguments
   to feed directly to the LVAR-DEST of LVAR, which must be a
   combination."
  (declare (type lvar lvar)
	   (type symbol fun)
	   (type index num-args))
  (let ((outside (lvar-dest lvar))
	(inside (lvar-uses lvar)))
    (aver (combination-p outside))
    (unless (combination-p inside)
      (give-up-ir1-transform))
    (let ((inside-fun (combination-fun inside)))
      (unless (eq (lvar-fun-name inside-fun) fun)
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
	  (setf (combination-kind inside)
                (info :function :info 'list))
	  (setf (node-derived-type inside) *wild-type*)
	  (flush-dest lvar)
	  (values))))))

(defun flush-combination (combination)
  (declare (type combination combination))
  (flush-dest (combination-fun combination))
  (dolist (arg (combination-args combination))
    (flush-dest arg))
  (unlink-node combination)
  (values))


;;;; leaf hackery

;;; Change the LEAF that a REF refers to.
(defun change-ref-leaf (ref leaf)
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
	  (derive-node-type ref vltype)))
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
;;; LEAF and enter it.
(defun find-constant (object)
  (if (typep object
	     ;; FIXME: What is the significance of this test? ("things
	     ;; that are worth uniquifying"?)
	     '(or symbol number character instance))
      (or (gethash object *constants*)
	  (setf (gethash object *constants*)
		(make-constant :value object
			       :%source-name '.anonymous.
			       :type (ctype-of object)
			       :where-from :defined)))
      (make-constant :value object
		     :%source-name '.anonymous.
		     :type (ctype-of object)
		     :where-from :defined)))

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
                              :test-not #'eq)))
                 (or (frob (leaf-refs var))
                     (frob (basic-var-sets var)))))))))

;;; If there is a non-local exit noted in ENTRY's environment that
;;; exits to CONT in that entry, then return it, otherwise return NIL.
(defun find-nlx-info (exit)
  (declare (type exit exit))
  (let* ((entry (exit-entry exit))
         (entry-cleanup (entry-cleanup entry)))
    (dolist (nlx (physenv-nlx-info (node-physenv entry)) nil)
      (when (eq (nlx-info-exit nlx) exit)
	(return nlx)))))

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

;;; Return the source name of a combination. (This is an idiom
;;; which was used in CMU CL. I gather it always works. -- WHN)
(defun combination-fun-source-name (combination)
  (let ((ref (lvar-uses (combination-fun combination))))
    (leaf-source-name (ref-leaf ref))))

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
              (eql (functional-kind functional) :deleted)))
    (throw 'locall-already-let-converted functional)))

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
	      (sb!kernel::arg-count-error (condition)
		(values nil (list (format nil "~A" condition))))
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
               (setf (component-reoptimize (node-component node)) t)))))))
