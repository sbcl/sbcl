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
  (with-ir1-environment node
    (let* ((start (make-continuation))
	   (block (continuation-starts-block start))
	   (cont (make-continuation))
	   (*lexenv* (if cleanup
			 (make-lexenv :cleanup cleanup)
			 *lexenv*)))
      (change-block-successor block1 block2 block)
      (link-blocks block block2)
      (ir1-convert start cont form)
      (setf (block-last block) (continuation-use cont))
      block)))

;;;; continuation use hacking

;;; Return a list of all the nodes which use Cont.
(declaim (ftype (function (continuation) list) find-uses))
(defun find-uses (cont)
  (ecase (continuation-kind cont)
    ((:block-start :deleted-block-start)
     (block-start-uses (continuation-block cont)))
    (:inside-block (list (continuation-use cont)))
    (:unused nil)
    (:deleted nil)))

;;; Update continuation use information so that NODE is no longer a
;;; use of its CONT. If the old continuation doesn't start its block,
;;; then we don't update the BLOCK-START-USES, since it will be
;;; deleted when we are done.
;;;
;;; Note: if you call this function, you may have to do a
;;; REOPTIMIZE-CONTINUATION to inform IR1 optimization that something
;;; has changed.
(declaim (ftype (function (node) (values)) delete-continuation-use))
(defun delete-continuation-use (node)
  (let* ((cont (node-cont node))
	 (block (continuation-block cont)))
    (ecase (continuation-kind cont)
      (:deleted)
      ((:block-start :deleted-block-start)
       (let ((uses (delete node (block-start-uses block))))
	 (setf (block-start-uses block) uses)
	 (setf (continuation-use cont)
	       (if (cdr uses) nil (car uses)))))
      (:inside-block
       (setf (continuation-kind cont) :unused)
       (setf (continuation-block cont) nil)
       (setf (continuation-use cont) nil)
       (setf (continuation-next cont) nil)))
    (setf (node-cont node) nil))
  (values))

;;; Update continuation use information so that NODE uses CONT. If
;;; CONT is :UNUSED, then we set its block to NODE's NODE-BLOCK (which
;;; must be set.)
;;;
;;; Note: if you call this function, you may have to do a
;;; REOPTIMIZE-CONTINUATION to inform IR1 optimization that something
;;; has changed.
(declaim (ftype (function (node continuation) (values)) add-continuation-use))
(defun add-continuation-use (node cont)
  (aver (not (node-cont node)))
  (let ((block (continuation-block cont)))
    (ecase (continuation-kind cont)
      (:deleted)
      (:unused
       (aver (not block))
       (let ((block (node-block node)))
	 (aver block)
	 (setf (continuation-block cont) block))
       (setf (continuation-kind cont) :inside-block)
       (setf (continuation-use cont) node))
      ((:block-start :deleted-block-start)
       (let ((uses (cons node (block-start-uses block))))
	 (setf (block-start-uses block) uses)
	 (setf (continuation-use cont)
	       (if (cdr uses) nil (car uses)))))))
  (setf (node-cont node) cont)
  (values))

;;; Return true if CONT is the NODE-CONT for NODE and CONT is
;;; transferred to immediately after the evaluation of NODE.
(defun immediately-used-p (cont node)
  (declare (type continuation cont) (type node node))
  (and (eq (node-cont node) cont)
       (not (eq (continuation-kind cont) :deleted))
       (let ((cblock (continuation-block cont))
	     (nblock (node-block node)))
	 (or (eq cblock nblock)
	     (let ((succ (block-succ nblock)))
	       (and (= (length succ) 1)
		    (eq (first succ) cblock)))))))

;;;; continuation substitution

;;; In OLD's DEST, replace OLD with NEW. NEW's DEST must initially be
;;; NIL. When we are done, we call FLUSH-DEST on OLD to clear its DEST
;;; and to note potential optimization opportunities.
(defun substitute-continuation (new old)
  (declare (type continuation old new))
  (aver (not (continuation-dest new)))
  (let ((dest (continuation-dest old)))
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
		 (nsubst new old (basic-combination-args dest))))))

    (flush-dest old)
    (setf (continuation-dest new) dest))
  (values))

;;; Replace all uses of OLD with uses of NEW, where NEW has an
;;; arbitary number of uses. If NEW will end up with more than one
;;; use, then we must arrange for it to start a block if it doesn't
;;; already.
(defun substitute-continuation-uses (new old)
  (declare (type continuation old new))
  (unless (and (eq (continuation-kind new) :unused)
	       (eq (continuation-kind old) :inside-block))
    (ensure-block-start new))

  (do-uses (node old)
    (delete-continuation-use node)
    (add-continuation-use node new))
  (dolist (lexenv-use (continuation-lexenv-uses old))
    (setf (cadr lexenv-use) new))

  (reoptimize-continuation new)
  (values))

;;;; block starting/creation

;;; Return the block that CONT is the start of, making a block if
;;; necessary. This function is called by IR1 translators which may
;;; cause a continuation to be used more than once. Every continuation
;;; which may be used more than once must start a block by the time
;;; that anyone does a USE-CONTINUATION on it.
;;;
;;; We also throw the block into the next/prev list for the
;;; *CURRENT-COMPONENT* so that we keep track of which blocks we have
;;; made.
(defun continuation-starts-block (cont)
  (declare (type continuation cont))
  (ecase (continuation-kind cont)
    (:unused
     (aver (not (continuation-block cont)))
     (let* ((head (component-head *current-component*))
	    (next (block-next head))
	    (new-block (make-block cont)))
       (setf (block-next new-block) next)
       (setf (block-prev new-block) head)
       (setf (block-prev next) new-block)
       (setf (block-next head) new-block)
       (setf (continuation-block cont) new-block)
       (setf (continuation-use cont) nil)
       (setf (continuation-kind cont) :block-start)
       new-block))
    (:block-start
     (continuation-block cont))))

;;; Ensure that Cont is the start of a block (or deleted) so that the use
;;; set can be freely manipulated.
;;; -- If the continuation is :Unused or is :Inside-Block and the Cont of Last
;;;    in its block, then we make it the start of a new deleted block.
;;; -- If the continuation is :Inside-Block inside a block, then we split the
;;;    block using Node-Ends-Block, which makes the continuation be a
;;;    :Block-Start.
(defun ensure-block-start (cont)
  (declare (type continuation cont))
  (let ((kind (continuation-kind cont)))
    (ecase kind
      ((:deleted :block-start :deleted-block-start))
      ((:unused :inside-block)
       (let ((block (continuation-block cont)))
	 (cond ((or (eq kind :unused)
		    (eq (node-cont (block-last block)) cont))
		(setf (continuation-block cont)
		      (make-block-key :start cont
				      :component nil
				      :start-uses (find-uses cont)))
		(setf (continuation-kind cont) :deleted-block-start))
	       (t
		(node-ends-block (continuation-use cont))))))))
  (values))

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
      ((not (eq (functional-kind fun) :deleted))
       (lambda-home fun))
    (when (eq (lambda-home fun) fun)
      (return fun))))

#!-sb-fluid (declaim (inline node-block node-tlf-number))
(declaim (maybe-inline node-physenv))
(defun node-block (node)
  (declare (type node node))
  (the cblock (continuation-block (node-prev node))))
(defun node-physenv (node)
  (declare (type node node))
  #!-sb-fluid (declare (inline node-home-lambda))
  (the physenv (lambda-physenv (node-home-lambda node))))

;;; Return the enclosing cleanup for environment of the first or last node
;;; in BLOCK.
(defun block-start-cleanup (block)
  (declare (type cblock block))
  (node-enclosing-cleanup (continuation-next (block-start block))))
(defun block-end-cleanup (block)
  (declare (type cblock block))
  (node-enclosing-cleanup (block-last block)))

;;; Return the non-LET LAMBDA that holds BLOCK's code.
(defun block-home-lambda (block)
  (declare (type cblock block))
  #!-sb-fluid (declare (inline node-home-lambda))
  (node-home-lambda (block-last block)))

;;; Return the IR1 physical environment for BLOCK.
(defun block-physenv (block)
  (declare (type cblock block))
  #!-sb-fluid (declare (inline node-home-lambda))
  (lambda-physenv (node-home-lambda (block-last block))))

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

;;; Return NODE-SOURCE-FORM, T if continuation has a single use,
;;; otherwise NIL, NIL.
(defun continuation-source (cont)
  (let ((use (continuation-use cont)))
    (if use
	(values (node-source-form use) t)
	(values nil nil))))

;;; Return a new LEXENV just like DEFAULT except for the specified
;;; slot values. Values for the alist slots are NCONCed to the
;;; beginning of the current value, rather than replacing it entirely.
(defun make-lexenv (&key (default *lexenv*)
			 functions variables blocks tags type-restrictions
			 options
			 (lambda (lexenv-lambda default))
			 (cleanup (lexenv-cleanup default))
			 (policy (lexenv-policy default)))
  (macrolet ((frob (var slot)
	       `(let ((old (,slot default)))
		  (if ,var
		      (nconc ,var old)
		      old))))
    (internal-make-lexenv
     (frob functions lexenv-functions)
     (frob variables lexenv-variables)
     (frob blocks lexenv-blocks)
     (frob tags lexenv-tags)
     (frob type-restrictions lexenv-type-restrictions)
     lambda cleanup policy 
     (frob options lexenv-options))))

;;;; flow/DFO/component hackery

;;; Join BLOCK1 and BLOCK2.
#!-sb-fluid (declaim (inline link-blocks))
(defun link-blocks (block1 block2)
  (declare (type cblock block1 block2))
  (setf (block-succ block1)
	(if (block-succ block1)
	    (%link-blocks block1 block2)
	    (list block2)))
  (push block1 (block-pred block2))
  (values))
(defun %link-blocks (block1 block2)
  (declare (type cblock block1 block2) (inline member))
  (let ((succ1 (block-succ block1)))
    (aver (not (member block2 succ1 :test #'eq)))
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
    (when (and new-pred (null (rest new-pred)))
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
  (declare (type cblock new old block) (inline member))
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
	 (unless (member new succ-left :test #'eq)
	   (link-blocks block new))
	 (macrolet ((frob (slot)
		      `(when (eq (,slot last) old)
			 (setf (,slot last) new))))
	   (frob if-consequent)
	   (frob if-alternative))))
      (t
       (unless (member new (block-succ block) :test #'eq)
	 (link-blocks block new)))))

  (values))

;;; Unlink a block from the next/prev chain. We also null out the
;;; COMPONENT.
(declaim (ftype (function (cblock) (values)) remove-from-dfo))
(defun remove-from-dfo (block)
  (let ((next (block-next block))
	(prev (block-prev block)))
    (setf (block-component block) nil)
    (setf (block-next prev) next)
    (setf (block-prev next) prev))
  (values))

;;; Add BLOCK to the next/prev chain following AFTER. We also set the
;;; Component to be the same as for AFTER.
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
(declaim (ftype (function (component) (values)) clear-flags))
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
(declaim (ftype (function nil component) make-empty-component))
(defun make-empty-component ()
  (let* ((head (make-block-key :start nil :component nil))
	 (tail (make-block-key :start nil :component nil))
	 (res (make-component :head head :tail tail)))
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
	 (start (node-cont node))
	 (last (block-last block))
	 (last-cont (node-cont last)))
    (unless (eq last node)
      (aver (and (eq (continuation-kind start) :inside-block)
		   (not (block-delete-p block))))
      (let* ((succ (block-succ block))
	     (new-block
	      (make-block-key :start start
			      :component (block-component block)
			      :start-uses (list (continuation-use start))
			      :succ succ :last last)))
	(setf (continuation-kind start) :block-start)
	(dolist (b succ)
	  (setf (block-pred b)
		(cons new-block (remove block (block-pred b)))))
	(setf (block-succ block) ())
	(setf (block-last block) node)
	(link-blocks block new-block)
	(add-to-dfo new-block block)
	(setf (component-reanalyze (block-component block)) t)
	
	(do ((cont start (node-cont (continuation-next cont))))
	    ((eq cont last-cont)
	     (when (eq (continuation-kind last-cont) :inside-block)
	       (setf (continuation-block last-cont) new-block)))
	  (setf (continuation-block cont) new-block))

	(setf (block-type-asserted block) t)
	(setf (block-test-modified block) t))))

  (values))

;;;; deleting stuff

;;; Deal with deleting the last (read) reference to a LAMBDA-VAR. We
;;; iterate over all local calls flushing the corresponding argument,
;;; allowing the computation of the argument to be deleted. We also
;;; mark the let for reoptimization, since it may be that we have
;;; deleted the last variable.
;;;
;;; The LAMBDA-VAR may still have some SETs, but this doesn't cause
;;; too much difficulty, since we can efficiently implement write-only
;;; variables. We iterate over the sets, marking their blocks for dead
;;; code flushing, since we can delete sets whose value is unused.
(defun delete-lambda-var (leaf)
  (declare (type lambda-var leaf))
  (let* ((fun (lambda-var-home leaf))
	 (n (position leaf (lambda-vars fun))))
    (dolist (ref (leaf-refs fun))
      (let* ((cont (node-cont ref))
	     (dest (continuation-dest cont)))
	(when (and (combination-p dest)
		   (eq (basic-combination-fun dest) cont)
		   (eq (basic-combination-kind dest) :local))
	  (let* ((args (basic-combination-args dest))
		 (arg (elt args n)))
	    (reoptimize-continuation arg)
	    (flush-dest arg)
	    (setf (elt args n) nil))))))

  (dolist (set (lambda-var-sets leaf))
    (setf (block-flush-p (node-block set)) t))

  (values))

;;; Note that something interesting has happened to VAR. We only deal
;;; with LET variables, marking the corresponding initial value arg as
;;; needing to be reoptimized.
(defun reoptimize-lambda-var (var)
  (declare (type lambda-var var))
  (let ((fun (lambda-var-home var)))
    (when (and (eq (functional-kind fun) :let)
	       (leaf-refs var))
      (do ((args (basic-combination-args
		  (continuation-dest
		   (node-cont
		    (first (leaf-refs fun)))))
		 (cdr args))
	   (vars (lambda-vars fun) (cdr vars)))
	  ((eq (car vars) var)
	   (reoptimize-continuation (car args))))))
  (values))

;;; Delete a function that has no references. This need only be called
;;; on functions that never had any references, since otherwise
;;; DELETE-REF will handle the deletion.
(defun delete-functional (fun)
  (aver (and (null (leaf-refs fun))
	     (not (functional-entry-function fun))))
  (etypecase fun
    (optional-dispatch (delete-optional-dispatch fun))
    (clambda (delete-lambda fun)))
  (values))

;;; Deal with deleting the last reference to a LAMBDA. Since there is
;;; only one way into a LAMBDA, deleting the last reference to a
;;; LAMBDA ensures that there is no way to reach any of the code in
;;; it. So we just set the FUNCTIONAL-KIND for FUN and its LETs to
;;; :DELETED, causing IR1 optimization to delete blocks in that
;;; lambda.
;;;
;;; If the function isn't a LET, we unlink the function head and tail
;;; from the component head and tail to indicate that the code is
;;; unreachable. We also delete the function from COMPONENT-LAMBDAS
;;; (it won't be there before local call analysis, but no matter.) If
;;; the lambda was never referenced, we give a note.
;;;
;;; If the lambda is an XEP, then we null out the ENTRY-FUNCTION in its
;;; ENTRY-FUNCTION so that people will know that it is not an entry point
;;; anymore.
(defun delete-lambda (leaf)
  (declare (type clambda leaf))
  (let ((kind (functional-kind leaf))
	(bind (lambda-bind leaf)))
    (aver (not (member kind '(:deleted :optional :toplevel))))
    (aver (not (functional-has-external-references-p leaf)))
    (setf (functional-kind leaf) :deleted)
    (setf (lambda-bind leaf) nil)
    (dolist (let (lambda-lets leaf))
      (setf (lambda-bind let) nil)
      (setf (functional-kind let) :deleted))

    (if (member kind '(:let :mv-let :assignment))
	(let ((home (lambda-home leaf)))
	  (setf (lambda-lets home) (delete leaf (lambda-lets home))))
	(let* ((bind-block (node-block bind))
	       (component (block-component bind-block))
	       (return (lambda-return leaf)))
	  (aver (null (leaf-refs leaf)))
	  (unless (leaf-ever-used leaf)
	    (let ((*compiler-error-context* bind))
	      (compiler-note "deleting unused function~:[.~;~:*~%  ~S~]"
			     (leaf-name leaf))))
	  (unlink-blocks (component-head component) bind-block)
	  (when return
	    (unlink-blocks (node-block return) (component-tail component)))
	  (setf (component-reanalyze component) t)
	  (let ((tails (lambda-tail-set leaf)))
	    (setf (tail-set-functions tails)
		  (delete leaf (tail-set-functions tails)))
	    (setf (lambda-tail-set leaf) nil))
	  (setf (component-lambdas component)
		(delete leaf (component-lambdas component)))))

    (when (eq kind :external)
      (let ((fun (functional-entry-function leaf)))
	(setf (functional-entry-function fun) nil)
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
;;; deference was deleted (due to their :OPTIONAL kind.)
;;;
;;; Note that the last optional ep may alias the main entry, so when
;;; we process the main entry, its kind may have been changed to NIL
;;; or even converted to a let.
(defun delete-optional-dispatch (leaf)
  (declare (type optional-dispatch leaf))
  (let ((entry (functional-entry-function leaf)))
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
	  (frob ep))
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
	 (refs (delete ref (leaf-refs leaf))))
    (setf (leaf-refs leaf) refs)

    (cond ((null refs)
	   (typecase leaf
	     (lambda-var (delete-lambda-var leaf))
	     (clambda
	      (ecase (functional-kind leaf)
		((nil :let :mv-let :assignment :escape :cleanup)
		 (aver (not (functional-entry-function leaf)))
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
;;; way to indicate that the value of a continuation is no longer
;;; used. We null out the CONTINUATION-DEST, set FLUSH-P in the blocks
;;; containing uses of CONT and set COMPONENT-REOPTIMIZE. If the PREV
;;; of the use is deleted, then we blow off reoptimization.
;;;
;;; If the continuation is :Deleted, then we don't do anything, since
;;; all semantics have already been flushed. :DELETED-BLOCK-START
;;; start continuations are treated just like :BLOCK-START; it is
;;; possible that the continuation may be given a new dest (e.g. by
;;; SUBSTITUTE-CONTINUATION), so we don't want to delete it.
(defun flush-dest (cont)
  (declare (type continuation cont))

  (unless (eq (continuation-kind cont) :deleted)
    (aver (continuation-dest cont))
    (setf (continuation-dest cont) nil)
    (do-uses (use cont)
      (let ((prev (node-prev use)))
	(unless (eq (continuation-kind prev) :deleted)
	  (let ((block (continuation-block prev)))
	    (setf (component-reoptimize (block-component block)) t)
	    (setf (block-attributep (block-flags block) flush-p type-asserted)
		  t))))))

  (setf (continuation-%type-check cont) nil)

  (values))

;;; Do a graph walk backward from BLOCK, marking all predecessor
;;; blocks with the DELETE-P flag.
(defun mark-for-deletion (block)
  (declare (type cblock block))
  (unless (block-delete-p block)
    (setf (block-delete-p block) t)
    (setf (component-reanalyze (block-component block)) t)
    (dolist (pred (block-pred block))
      (mark-for-deletion pred)))
  (values))

;;; Delete CONT, eliminating both control and value semantics. We set
;;; FLUSH-P and COMPONENT-REOPTIMIZE similarly to in FLUSH-DEST. Here
;;; we must get the component from the use block, since the
;;; continuation may be a :DELETED-BLOCK-START.
;;;
;;; If CONT has DEST, then it must be the case that the DEST is
;;; unreachable, since we can't compute the value desired. In this
;;; case, we call MARK-FOR-DELETION to cause the DEST block and its
;;; predecessors to tell people to ignore them, and to cause them to
;;; be deleted eventually.
(defun delete-continuation (cont)
  (declare (type continuation cont))
  (aver (not (eq (continuation-kind cont) :deleted)))

  (do-uses (use cont)
    (let ((prev (node-prev use)))
      (unless (eq (continuation-kind prev) :deleted)
	(let ((block (continuation-block prev)))
	  (setf (block-attributep (block-flags block) flush-p type-asserted) t)
	  (setf (component-reoptimize (block-component block)) t)))))

  (let ((dest (continuation-dest cont)))
    (when dest
      (let ((prev (node-prev dest)))
	(when (and prev
		   (not (eq (continuation-kind prev) :deleted)))
	  (let ((block (continuation-block prev)))
	    (unless (block-delete-p block)
	      (mark-for-deletion block)))))))

  (setf (continuation-kind cont) :deleted)
  (setf (continuation-dest cont) nil)
  (setf (continuation-next cont) nil)
  (setf (continuation-asserted-type cont) *empty-type*)
  (setf (continuation-%derived-type cont) *empty-type*)
  (setf (continuation-use cont) nil)
  (setf (continuation-block cont) nil)
  (setf (continuation-reoptimize cont) nil)
  (setf (continuation-%type-check cont) nil)
  (setf (continuation-info cont) nil)

  (values))

;;; This function does what is necessary to eliminate the code in it
;;; from the IR1 representation. This involves unlinking it from its
;;; predecessors and successors and deleting various node-specific
;;; semantic information.
;;;
;;; We mark the START as has having no next and remove the last node
;;; from its CONT's uses. We also flush the DEST for all continuations
;;; whose values are received by nodes in the block.
(defun delete-block (block)
  (declare (type cblock block))
  (aver (block-component block)) ; else block is already deleted!
  (note-block-deletion block)
  (setf (block-delete-p block) t)

  (let* ((last (block-last block))
	 (cont (node-cont last)))
    (delete-continuation-use last)
    (if (eq (continuation-kind cont) :unused)
	(delete-continuation cont)
	(reoptimize-continuation cont)))

  (dolist (b (block-pred block))
    (unlink-blocks b block))
  (dolist (b (block-succ block))
    (unlink-blocks block b))

  (do-nodes (node cont block)
    (typecase node
      (ref (delete-ref node))
      (cif
       (flush-dest (if-test node)))
      ;; The next two cases serve to maintain the invariant that a LET
      ;; always has a well-formed COMBINATION, REF and BIND. We delete
      ;; the lambda whenever we delete any of these, but we must be
      ;; careful that this LET has not already been partially deleted.
      (basic-combination
       (when (and (eq (basic-combination-kind node) :local)
		  ;; Guards COMBINATION-LAMBDA agains the REF being deleted.
		  (continuation-use (basic-combination-fun node)))
	 (let ((fun (combination-lambda node)))
	   ;; If our REF was the 2'nd to last ref, and has been deleted, then
	   ;; Fun may be a LET for some other combination.
	   (when (and (member (functional-kind fun) '(:let :mv-let))
		      (eq (let-combination fun) node))
	     (delete-lambda fun))))
       (flush-dest (basic-combination-fun node))
       (dolist (arg (basic-combination-args node))
	 (when arg (flush-dest arg))))
      (bind
       (let ((lambda (bind-lambda node)))
	 (unless (eq (functional-kind lambda) :deleted)
	   (aver (member (functional-kind lambda) '(:let :mv-let :assignment)))
	   (delete-lambda lambda))))
      (exit
       (let ((value (exit-value node))
	     (entry (exit-entry node)))
	 (when value
	   (flush-dest value))
	 (when entry
	   (setf (entry-exits entry)
		 (delete node (entry-exits entry))))))
      (creturn
       (flush-dest (return-result node))
       (delete-return node))
      (cset
       (flush-dest (set-value node))
       (let ((var (set-var node)))
	 (setf (basic-var-sets var)
	       (delete node (basic-var-sets var))))))

    (delete-continuation (node-prev node)))

  (remove-from-dfo block)
  (values))

;;; Do stuff to indicate that the return node Node is being deleted.
;;; We set the RETURN to NIL.
(defun delete-return (node)
  (declare (type creturn node))
  (let ((fun (return-lambda node)))
    (aver (lambda-return fun))
    (setf (lambda-return fun) nil))
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
	  ;; requires this to be a STYLE-WARNING.
	  (compiler-style-warning "The variable ~S is defined but never used."
				  (leaf-name var)))
	(setf (leaf-ever-used var) t))))
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
      (do-nodes (node cont block)
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
			 (every #'(lambda (x)
				    (present-in-form first x 0))
				(source-path-forms path))
			 (present-in-form first (find-original-source path)
					  0)))
	    (unless (return-p node)
	      (let ((*compiler-error-context* node))
		(compiler-note "deleting unreachable code")))
	    (return))))))
  (values))

;;; Delete a node from a block, deleting the block if there are no
;;; nodes left. We remove the node from the uses of its CONT, but we
;;; don't deal with cleaning up any type-specific semantic
;;; attachments. If the CONT is :UNUSED after deleting this use, then
;;; we delete CONT. (Note :UNUSED is not the same as no uses. A
;;; continuation will only become :UNUSED if it was :INSIDE-BLOCK
;;; before.)
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
  (let* ((cont (node-cont node))
	 (next (continuation-next cont))
	 (prev (node-prev node))
	 (block (continuation-block prev))
	 (prev-kind (continuation-kind prev))
	 (last (block-last block)))

    (unless (eq (continuation-kind cont) :deleted)
      (delete-continuation-use node)
      (when (eq (continuation-kind cont) :unused)
	(aver (not (continuation-dest cont)))
	(delete-continuation cont)))

    (setf (block-type-asserted block) t)
    (setf (block-test-modified block) t)

    (cond ((or (eq prev-kind :inside-block)
	       (and (eq prev-kind :block-start)
		    (not (eq node last))))
	   (cond ((eq node last)
		  (setf (block-last block) (continuation-use prev))
		  (setf (continuation-next prev) nil))
		 (t
		  (setf (continuation-next prev) next)
		  (setf (node-prev next) prev)))
	   (setf (node-prev node) nil)
	   nil)
	  (t
	   (aver (eq prev-kind :block-start))
	   (aver (eq node last))
	   (let* ((succ (block-succ block))
		  (next (first succ)))
	     (aver (and succ (null (cdr succ))))
	     (cond
	      ((member block succ)
	       (with-ir1-environment node
		 (let ((exit (make-exit))
		       (dummy (make-continuation)))
		   (setf (continuation-next prev) nil)
		   (prev-link exit prev)
		   (add-continuation-use exit dummy)
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
	       (cond ((continuation-dest prev)
		      (setf (continuation-next prev) nil)
		      (setf (continuation-kind prev) :deleted-block-start))
		     (t
		      (delete-continuation prev)))
	       (setf (node-prev node) nil)
	       t)))))))

;;; Return true if NODE has been deleted, false if it is still a valid
;;; part of IR1.
(defun node-deleted (node)
  (declare (type node node))
  (let ((prev (node-prev node)))
    (not (and prev
	      (not (eq (continuation-kind prev) :deleted))
	      (let ((block (continuation-block prev)))
		(and (block-component block)
		     (not (block-delete-p block))))))))

;;; Delete all the blocks and functions in COMPONENT. We scan first
;;; marking the blocks as delete-p to prevent weird stuff from being
;;; triggered by deletion.
(defun delete-component (component)
  (declare (type component component))
  (aver (null (component-new-functions component)))
  (setf (component-kind component) :deleted)
  (do-blocks (block component)
    (setf (block-delete-p block) t))
  (dolist (fun (component-lambdas component))
    (setf (functional-kind fun) nil)
    (setf (functional-entry-function fun) nil)
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
(defun extract-function-args (cont fun num-args)
  #!+sb-doc
  "If CONT is a call to FUN with NUM-ARGS args, change those arguments
   to feed directly to the continuation-dest of CONT, which must be
   a combination."
  (declare (type continuation cont)
	   (type symbol fun)
	   (type index num-args))
  (let ((outside (continuation-dest cont))
	(inside (continuation-use cont)))
    (aver (combination-p outside))
    (unless (combination-p inside)
      (give-up-ir1-transform))
    (let ((inside-fun (combination-fun inside)))
      (unless (eq (continuation-fun-name inside-fun) fun)
	(give-up-ir1-transform))
      (let ((inside-args (combination-args inside)))
	(unless (= (length inside-args) num-args)
	  (give-up-ir1-transform))
	(let* ((outside-args (combination-args outside))
	       (arg-position (position cont outside-args))
	       (before-args (subseq outside-args 0 arg-position))
	       (after-args (subseq outside-args (1+ arg-position))))
	  (dolist (arg inside-args)
	    (setf (continuation-dest arg) outside))
	  (setf (combination-args inside) nil)
	  (setf (combination-args outside)
		(append before-args inside-args after-args))
	  (change-ref-leaf (continuation-use inside-fun)
			   (find-free-function 'list "???"))
	  (setf (combination-kind inside) :full)
	  (setf (node-derived-type inside) *wild-type*)
	  (flush-dest cont)
	  (setf (continuation-asserted-type cont) *wild-type*)
	  (values))))))

;;;; leaf hackery

;;; Change the Leaf that a Ref refers to.
(defun change-ref-leaf (ref leaf)
  (declare (type ref ref) (type leaf leaf))
  (unless (eq (ref-leaf ref) leaf)
    (push ref (leaf-refs leaf))
    (delete-ref ref)
    (setf (ref-leaf ref) leaf)
    (let ((ltype (leaf-type leaf)))
      (if (fun-type-p ltype)
	  (setf (node-derived-type ref) ltype)
	  (derive-node-type ref ltype)))
    (reoptimize-continuation (node-cont ref)))
  (values))

;;; Change all REFS for OLD-LEAF to NEW-LEAF.
(defun substitute-leaf (new-leaf old-leaf)
  (declare (type leaf new-leaf old-leaf))
  (dolist (ref (leaf-refs old-leaf))
    (change-ref-leaf ref new-leaf))
  (values))

;;; Like SUBSITUTE-LEAF, only there is a predicate on the Ref to tell
;;; whether to substitute.
(defun substitute-leaf-if (test new-leaf old-leaf)
  (declare (type leaf new-leaf old-leaf) (type function test))
  (dolist (ref (leaf-refs old-leaf))
    (when (funcall test ref)
      (change-ref-leaf ref new-leaf)))
  (values))

;;; Return a LEAF which represents the specified constant object. If
;;; the object is not in *CONSTANTS*, then we create a new constant
;;; LEAF and enter it.
#!-sb-fluid (declaim (maybe-inline find-constant))
(defun find-constant (object)
  (if (typep object '(or symbol number character instance))
    (or (gethash object *constants*)
	(setf (gethash object *constants*)
	      (make-constant :value object
			     :name nil
			     :type (ctype-of object)
			     :where-from :defined)))
    (make-constant :value object
		   :name nil
		   :type (ctype-of object)
		   :where-from :defined)))

;;; If there is a non-local exit noted in ENTRY's environment that
;;; exits to CONT in that entry, then return it, otherwise return NIL.
(defun find-nlx-info (entry cont)
  (declare (type entry entry) (type continuation cont))
  (let ((entry-cleanup (entry-cleanup entry)))
    (dolist (nlx (physenv-nlx-info (node-physenv entry)) nil)
      (when (and (eq (nlx-info-continuation nlx) cont)
		 (eq (nlx-info-cleanup nlx) entry-cleanup))
	(return nlx)))))

;;;; functional hackery

(declaim (ftype (function (functional) clambda) main-entry))
(defun main-entry (functional)
  (etypecase functional
    (clambda functional)
    (optional-dispatch
     (optional-dispatch-main-entry functional))))

;;; RETURN true if FUNCTIONAL is a thing that can be treated like
;;; MV-BIND when it appears in an MV-CALL. All fixed arguments must be
;;; optional with null default and no SUPPLIED-P. There must be a
;;; &REST arg with no references.
(declaim (ftype (function (functional) boolean) looks-like-an-mv-bind))
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

;;; Return true if function is an XEP. This is true of normal XEPs
;;; (:EXTERNAL kind) and top level lambdas (:TOPLEVEL kind.)
(defun external-entry-point-p (fun)
  (declare (type functional fun))
  (not (null (member (functional-kind fun) '(:external :toplevel)))))

;;; If CONT's only use is a non-notinline global function reference,
;;; then return the referenced symbol, otherwise NIL. If NOTINLINE-OK
;;; is true, then we don't care if the leaf is NOTINLINE.
(defun continuation-fun-name (cont &optional notinline-ok)
  (declare (type continuation cont))
  (let ((use (continuation-use cont)))
    (if (ref-p use)
	(let ((leaf (ref-leaf use)))
	  (if (and (global-var-p leaf)
		   (eq (global-var-kind leaf) :global-function)
		   (or (not (defined-fun-p leaf))
		       (not (eq (defined-fun-inlinep leaf) :notinline))
		       notinline-ok))
	      (leaf-name leaf)
	      nil))
	nil)))

;;; Return the COMBINATION node that is the call to the LET FUN.
(defun let-combination (fun)
  (declare (type clambda fun))
  (aver (member (functional-kind fun) '(:let :mv-let)))
  (continuation-dest (node-cont (first (leaf-refs fun)))))

;;; Return the initial value continuation for a LET variable, or NIL
;;; if there is none.
(defun let-var-initial-value (var)
  (declare (type lambda-var var))
  (let ((fun (lambda-var-home var)))
    (elt (combination-args (let-combination fun))
	 (position-or-lose var (lambda-vars fun)))))

;;; Return the LAMBDA that is called by the local Call.
#!-sb-fluid (declaim (inline combination-lambda))
(defun combination-lambda (call)
  (declare (type basic-combination call))
  (aver (eq (basic-combination-kind call) :local))
  (ref-leaf (continuation-use (basic-combination-fun call))))

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
	     (compiler-note "*INLINE-EXPANSION-LIMIT* (~D) was exceeded, ~
			     probably trying to~%  ~
			     inline a recursive function."
			    *inline-expansion-limit*))
	   nil)
	  (t t))))

;;;; careful call

;;; Apply a function to some arguments, returning a list of the values
;;; resulting of the evaluation. If an error is signalled during the
;;; application, then we print a warning message and return NIL as our
;;; second value to indicate this. Node is used as the error context
;;; for any error message, and Context is a string that is spliced
;;; into the warning.
(declaim (ftype (function ((or symbol function) list node string)
			  (values list boolean))
		careful-call))
(defun careful-call (function args node context)
  (values
   (multiple-value-list
    (handler-case (apply function args)
      (error (condition)
	(let ((*compiler-error-context* node))
	  (compiler-warning "Lisp error during ~A:~%~A" context condition)
	  (return-from careful-call (values nil nil))))))
   t))

;;;; utilities used at run-time for parsing &KEY args in IR1

;;; This function is used by the result of PARSE-DEFTRANSFORM to find
;;; the continuation for the value of the &KEY argument KEY in the
;;; list of continuations ARGS. It returns the continuation if the
;;; keyword is present, or NIL otherwise. The legality and
;;; constantness of the keywords should already have been checked.
(declaim (ftype (function (list keyword) (or continuation null))
		find-keyword-continuation))
(defun find-keyword-continuation (args key)
  (do ((arg args (cddr arg)))
      ((null arg) nil)
    (when (eq (continuation-value (first arg)) key)
      (return (second arg)))))

;;; This function is used by the result of PARSE-DEFTRANSFORM to
;;; verify that alternating continuations in ARGS are constant and
;;; that there is an even number of args.
(declaim (ftype (function (list) boolean) check-key-args-constant))
(defun check-key-args-constant (args)
  (do ((arg args (cddr arg)))
      ((null arg) t)
    (unless (and (rest arg)
		 (constant-continuation-p (first arg)))
      (return nil))))

;;; This function is used by the result of PARSE-DEFTRANSFORM to
;;; verify that the list of continuations ARGS is a well-formed &KEY
;;; arglist and that only keywords present in the list KEYS are
;;; supplied.
(declaim (ftype (function (list list) boolean) check-transform-keys))
(defun check-transform-keys (args keys)
  (and (check-key-args-constant args)
       (do ((arg args (cddr arg)))
	   ((null arg) t)
	 (unless (member (continuation-value (first arg)) keys)
	   (return nil)))))

;;;; miscellaneous

;;; Called by the expansion of the EVENT macro.
(declaim (ftype (function (event-info (or node null)) *) %event))
(defun %event (info node)
  (incf (event-info-count info))
  (when (and (>= (event-info-level info) *event-note-threshold*)
	     (policy (or node *lexenv*)
		     (= inhibit-warnings 0)))
    (let ((*compiler-error-context* node))
      (compiler-note (event-info-description info))))

  (let ((action (event-info-action info)))
    (when action (funcall action node))))
