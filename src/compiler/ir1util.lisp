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

;;; Return the innermost cleanup enclosing Node, or NIL if there is none in
;;; its function. If Node has no cleanup, but is in a let, then we must still
;;; check the environment that the call is in.
(defun node-enclosing-cleanup (node)
  (declare (type node node))
  (do ((lexenv (node-lexenv node)
	       (lambda-call-lexenv (lexenv-lambda lexenv))))
      ((null lexenv) nil)
    (let ((cup (lexenv-cleanup lexenv)))
      (when cup (return cup)))))

;;; Convert the Form in a block inserted between Block1 and Block2 as an
;;; implicit MV-Prog1. The inserted block is returned. Node is used for IR1
;;; context when converting the form. Note that the block is not assigned a
;;; number, and is linked into the DFO at the beginning. We indicate that we
;;; have trashed the DFO by setting Component-Reanalyze. If Cleanup is
;;; supplied, then convert with that cleanup.
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

;;; Update continuation use information so that Node is no longer a
;;; use of its Cont. If the old continuation doesn't start its block,
;;; then we don't update the Block-Start-Uses, since it will be
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

;;; Update continuation use information so that Node uses Cont. If
;;; Cont is :Unused, then we set its block to Node's Node-Block (which
;;; must be set.)
;;;
;;; Note: if you call this function, you may have to do a
;;; REOPTIMIZE-CONTINUATION to inform IR1 optimization that something
;;; has changed.
(declaim (ftype (function (node continuation) (values)) add-continuation-use))
(defun add-continuation-use (node cont)
  (assert (not (node-cont node)))
  (let ((block (continuation-block cont)))
    (ecase (continuation-kind cont)
      (:deleted)
      (:unused
       (assert (not block))
       (let ((block (node-block node)))
	 (assert block)
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

;;; Return true if Cont is the Node-Cont for Node and Cont is transferred to
;;; immediately after the evaluation of Node.
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

;;; In Old's Dest, replace Old with New. New's Dest must initially be NIL.
;;; When we are done, we call Flush-Dest on Old to clear its Dest and to note
;;; potential optimization opportunities.
(defun substitute-continuation (new old)
  (declare (type continuation old new))
  (assert (not (continuation-dest new)))
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
     (assert (not (continuation-block cont)))
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

;;; Return the home (i.e. enclosing non-let) lambda for Node. Since the
;;; LEXENV-LAMBDA may be deleted, we must chain up the LAMBDA-CALL-LEXENV
;;; thread until we find a lambda that isn't deleted, and then return its home.
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
(declaim (maybe-inline node-environment))
(defun node-block (node)
  (declare (type node node))
  (the cblock (continuation-block (node-prev node))))
(defun node-environment (node)
  (declare (type node node))
  #!-sb-fluid (declare (inline node-home-lambda))
  (the environment (lambda-environment (node-home-lambda node))))

;;; Return the enclosing cleanup for environment of the first or last node
;;; in Block.
(defun block-start-cleanup (block)
  (declare (type cblock block))
  (node-enclosing-cleanup (continuation-next (block-start block))))
(defun block-end-cleanup (block)
  (declare (type cblock block))
  (node-enclosing-cleanup (block-last block)))

;;; Return the non-let lambda that holds Block's code.
(defun block-home-lambda (block)
  (declare (type cblock block))
  #!-sb-fluid (declare (inline node-home-lambda))
  (node-home-lambda (block-last block)))

;;; Return the IR1 environment for Block.
(defun block-environment (block)
  (declare (type cblock block))
  #!-sb-fluid (declare (inline node-home-lambda))
  (lambda-environment (node-home-lambda (block-last block))))

;;; Return the Top Level Form number of path, i.e. the ordinal number of
;;; its orignal source's top-level form in its compilation unit.
(defun source-path-tlf-number (path)
  (declare (list path))
  (car (last path)))

;;; Return the (reversed) list for the path in the orignal source (with the
;;; TLF number last.)
(defun source-path-original-source (path)
  (declare (list path) (inline member))
  (cddr (member 'original-source-start path :test #'eq)))

;;; Return the Form Number of Path's orignal source inside the Top Level
;;; Form that contains it. This is determined by the order that we walk the
;;; subforms of the top level source form.
(defun source-path-form-number (path)
  (declare (list path) (inline member))
  (cadr (member 'original-source-start path :test #'eq)))

;;; Return a list of all the enclosing forms not in the original
;;; source that converted to get to this form, with the immediate
;;; source for node at the start of the list.
(defun source-path-forms (path)
  (subseq path 0 (position 'original-source-start path)))

;;; Return the innermost source form for Node.
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
			 (policy (lexenv-policy default))
			 (interface-policy (lexenv-interface-policy default)))
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
     lambda cleanup policy interface-policy
     (frob options lexenv-options))))

;;; Return a POLICY that defaults any unsupplied optimize qualities in
;;; the INTERFACE-POLICY with the corresponding ones from the POLICY.
(defun make-interface-policy (lexenv)
  (declare (type lexenv lexenv))
  (let ((ipolicy (lexenv-interface-policy lexenv))
	(policy (lexenv-policy lexenv)))
    (let ((result policy))
      (dolist (quality '(speed safety space))
	(let ((iquality-entry (assoc quality ipolicy)))
	  (when iquality-entry
	    (push iquality-entry result))))
      result)))

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
    (assert (not (member block2 succ1 :test #'eq)))
    (cons block2 succ1)))

;;; Like LINK-BLOCKS, but we separate BLOCK1 and BLOCK2. If this leaves a
;;; successor with a single predecessor that ends in an IF, then set
;;; BLOCK-TEST-MODIFIED so that any test constraint will now be able to be
;;; propagated to the successor.
(defun unlink-blocks (block1 block2)
  (declare (type cblock block1 block2))
  (let ((succ1 (block-succ block1)))
    (if (eq block2 (car succ1))
	(setf (block-succ block1) (cdr succ1))
	(do ((succ (cdr succ1) (cdr succ))
	     (prev succ1 succ))
	    ((eq (car succ) block2)
	     (setf (cdr prev) (cdr succ)))
	  (assert succ))))

  (let ((new-pred (delq block1 (block-pred block2))))
    (setf (block-pred block2) new-pred)
    (when (and new-pred (null (rest new-pred)))
      (let ((pred-block (first new-pred)))
	(when (if-p (block-last pred-block))
	  (setf (block-test-modified pred-block) t)))))
  (values))

;;; Swing the succ/pred link between Block and Old to be between Block and
;;; New. If Block ends in an IF, then we have to fix up the
;;; consequent/alternative blocks to point to New. We also set
;;; BLOCK-TEST-MODIFIED so that any test constraint will be applied to the new
;;; successor.
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
;;; Component.
(declaim (ftype (function (cblock) (values)) remove-from-dfo))
#!-sb-fluid (declaim (inline remove-from-dfo))
(defun remove-from-dfo (block)
  (let ((next (block-next block))
	(prev (block-prev block)))
    (setf (block-component block) nil)
    (setf (block-next prev) next)
    (setf (block-prev next) prev))
  (values))

;;; Add Block to the next/prev chain following After. We also set the
;;; Component to be the same as for After.
#!-sb-fluid (declaim (inline add-to-dfo))
(defun add-to-dfo (block after)
  (declare (type cblock block after))
  (let ((next (block-next after))
	(comp (block-component after)))
    (assert (not (eq (component-kind comp) :deleted)))
    (setf (block-component block) comp)
    (setf (block-next after) block)
    (setf (block-prev block) after)
    (setf (block-next block) next)
    (setf (block-prev next) block))
  (values))

;;; Set the Flag for all the blocks in Component to NIL, except for the head
;;; and tail which are set to T.
(declaim (ftype (function (component) (values)) clear-flags))
(defun clear-flags (component)
  (let ((head (component-head component))
	(tail (component-tail component)))
    (setf (block-flag head) t)
    (setf (block-flag tail) t)
    (do-blocks (block component)
      (setf (block-flag block) nil)))
  (values))

;;; Make a component with no blocks in it. The Block-Flag is initially
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

;;; Makes Node the Last node in its block, splitting the block if necessary.
;;; The new block is added to the DFO immediately following Node's block.
(defun node-ends-block (node)
  (declare (type node node))
  (let* ((block (node-block node))
	 (start (node-cont node))
	 (last (block-last block))
	 (last-cont (node-cont last)))
    (unless (eq last node)
      (assert (and (eq (continuation-kind start) :inside-block)
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

;;; Deal with deleting the last (read) reference to a lambda-var. We
;;; iterate over all local calls flushing the corresponding argument, allowing
;;; the computation of the argument to be deleted. We also mark the let for
;;; reoptimization, since it may be that we have deleted the last variable.
;;;
;;; The lambda-var may still have some sets, but this doesn't cause too much
;;; difficulty, since we can efficiently implement write-only variables. We
;;; iterate over the sets, marking their blocks for dead code flushing, since
;;; we can delete sets whose value is unused.
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

;;; Note that something interesting has happened to Var. We only deal with
;;; LET variables, marking the corresponding initial value arg as needing to be
;;; reoptimized.
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

;;; This function deletes functions that have no references. This need only
;;; be called on functions that never had any references, since otherwise
;;; DELETE-REF will handle the deletion.
(defun delete-functional (fun)
  (assert (and (null (leaf-refs fun))
	       (not (functional-entry-function fun))))
  (etypecase fun
    (optional-dispatch (delete-optional-dispatch fun))
    (clambda (delete-lambda fun)))
  (values))

;;; Deal with deleting the last reference to a lambda. Since there is only
;;; one way into a lambda, deleting the last reference to a lambda ensures that
;;; there is no way to reach any of the code in it. So we just set the
;;; Functional-Kind for Fun and its Lets to :Deleted, causing IR1 optimization
;;; to delete blocks in that lambda.
;;;
;;; If the function isn't a Let, we unlink the function head and tail from
;;; the component head and tail to indicate that the code is unreachable. We
;;; also delete the function from Component-Lambdas (it won't be there before
;;; local call analysis, but no matter.)  If the lambda was never referenced,
;;; we give a note.
;;;
;;; If the lambda is an XEP, then we null out the Entry-Function in its
;;; Entry-Function so that people will know that it is not an entry point
;;; anymore.
(defun delete-lambda (leaf)
  (declare (type clambda leaf))
  (let ((kind (functional-kind leaf))
	(bind (lambda-bind leaf)))
    (assert (not (member kind '(:deleted :optional :top-level))))
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
	  (assert (null (leaf-refs leaf)))
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

;;; Deal with deleting the last reference to an Optional-Dispatch. We have
;;; to be a bit more careful than with lambdas, since Delete-Ref is used both
;;; before and after local call analysis. Afterward, all references to
;;; still-existing optional-dispatches have been moved to the XEP, leaving it
;;; with no references at all. So we look at the XEP to see whether an
;;; optional-dispatch is still really being used. But before local call
;;; analysis, there are no XEPs, and all references are direct.
;;;
;;; When we do delete the optional-dispatch, we grovel all of its
;;; entry-points, making them be normal lambdas, and then deleting the ones
;;; with no references. This deletes any e-p lambdas that were either never
;;; referenced, or couldn't be deleted when the last deference was deleted (due
;;; to their :OPTIONAL kind.)
;;;
;;; Note that the last optional ep may alias the main entry, so when we process
;;; the main entry, its kind may have been changed to NIL or even converted to
;;; a let.
(defun delete-optional-dispatch (leaf)
  (declare (type optional-dispatch leaf))
  (let ((entry (functional-entry-function leaf)))
    (unless (and entry (leaf-refs entry))
      (assert (or (not entry) (eq (functional-kind entry) :deleted)))
      (setf (functional-kind leaf) :deleted)

      (flet ((frob (fun)
	       (unless (eq (functional-kind fun) :deleted)
		 (assert (eq (functional-kind fun) :optional))
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

;;; Do stuff to delete the semantic attachments of a Ref node. When this
;;; leaves zero or one reference, we do a type dispatch off of the leaf to
;;; determine if a special action is appropriate.
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
		 (assert (not (functional-entry-function leaf)))
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

;;; This function is called by people who delete nodes; it provides a way to
;;; indicate that the value of a continuation is no longer used. We null out
;;; the Continuation-Dest, set Flush-P in the blocks containing uses of Cont
;;; and set Component-Reoptimize. If the Prev of the use is deleted, then we
;;; blow off reoptimization.
;;;
;;; If the continuation is :Deleted, then we don't do anything, since all
;;; semantics have already been flushed. :Deleted-Block-Start start
;;; continuations are treated just like :Block-Start; it is possible that the
;;; continuation may be given a new dest (e.g. by SUBSTITUTE-CONTINUATION), so
;;; we don't want to delete it.
(defun flush-dest (cont)
  (declare (type continuation cont))

  (unless (eq (continuation-kind cont) :deleted)
    (assert (continuation-dest cont))
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

;;; Do a graph walk backward from Block, marking all predecessor blocks with
;;; the DELETE-P flag.
(defun mark-for-deletion (block)
  (declare (type cblock block))
  (unless (block-delete-p block)
    (setf (block-delete-p block) t)
    (setf (component-reanalyze (block-component block)) t)
    (dolist (pred (block-pred block))
      (mark-for-deletion pred)))
  (values))

;;;    Delete Cont, eliminating both control and value semantics. We set
;;; FLUSH-P and COMPONENT-REOPTIMIZE similarly to in FLUSH-DEST. Here we must
;;; get the component from the use block, since the continuation may be a
;;; :DELETED-BLOCK-START.
;;;
;;;    If Cont has DEST, then it must be the case that the DEST is unreachable,
;;; since we can't compute the value desired. In this case, we call
;;; MARK-FOR-DELETION to cause the DEST block and its predecessors to tell
;;; people to ignore them, and to cause them to be deleted eventually.
(defun delete-continuation (cont)
  (declare (type continuation cont))
  (assert (not (eq (continuation-kind cont) :deleted)))

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

;;; This function does what is necessary to eliminate the code in it from
;;; the IR1 representation. This involves unlinking it from its predecessors
;;; and successors and deleting various node-specific semantic information.
;;;
;;; We mark the Start as has having no next and remove the last node from
;;; its Cont's uses. We also flush the DEST for all continuations whose values
;;; are received by nodes in the block.
(defun delete-block (block)
  (declare (type cblock block))
  (assert (block-component block) () "Block is already deleted.")
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
      ;; The next two cases serve to maintain the invariant that a LET always
      ;; has a well-formed COMBINATION, REF and BIND. We delete the lambda
      ;; whenever we delete any of these, but we must be careful that this LET
      ;; has not already been partially deleted.
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
	   (assert (member (functional-kind lambda)
			   '(:let :mv-let :assignment)))
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

;;; Do stuff to indicate that the return node Node is being deleted. We set
;;; the RETURN to NIL.
(defun delete-return (node)
  (declare (type creturn node))
  (let ((fun (return-lambda node)))
    (assert (lambda-return fun))
    (setf (lambda-return fun) nil))
  (values))

;;; If any of the Vars in fun were never referenced and was not declared
;;; IGNORE, then complain.
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

;;; Return true if we can find Obj in Form, NIL otherwise. We bound our
;;; recursion so that we don't get lost in circular structures. We ignore the
;;; car of forms if they are a symbol (to prevent confusing function
;;; referencess with variables), and we also ignore anything inside ' or #'.
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

;;; This function is called on a block immediately before we delete it. We
;;; check to see whether any of the code about to die appeared in the original
;;; source, and emit a note if so.
;;;
;;; If the block was in a lambda is now deleted, then we ignore the whole
;;; block, since this case is picked off in DELETE-LAMBDA. We also ignore
;;; the deletion of CRETURN nodes, since it is somewhat reasonable for a
;;; function to not return, and there is a different note for that case anyway.
;;;
;;; If the actual source is an atom, then we use a bunch of heuristics to
;;; guess whether this reference really appeared in the original source:
;;; -- If a symbol, it must be interned and not a keyword.
;;; -- It must not be an easily introduced constant (T or NIL, a fixnum or a
;;;    character.)
;;; -- The atom must be "present" in the original source form, and present in
;;;    all intervening actual source forms.
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

;;; Delete a node from a block, deleting the block if there are no nodes
;;; left. We remove the node from the uses of its CONT, but we don't deal with
;;; cleaning up any type-specific semantic attachments. If the CONT is :UNUSED
;;; after deleting this use, then we delete CONT. (Note :UNUSED is not the
;;; same as no uses. A continuation will only become :UNUSED if it was
;;; :INSIDE-BLOCK before.)
;;;
;;; If the node is the last node, there must be exactly one successor. We
;;; link all of our precedessors to the successor and unlink the block. In
;;; this case, we return T, otherwise NIL. If no nodes are left, and the block
;;; is a successor of itself, then we replace the only node with a degenerate
;;; exit node. This provides a way to represent the bodyless infinite loop,
;;; given the prohibition on empty blocks in IR1.
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
	(assert (not (continuation-dest cont)))
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
	   (assert (eq prev-kind :block-start))
	   (assert (eq node last))
	   (let* ((succ (block-succ block))
		  (next (first succ)))
	     (assert (and succ (null (cdr succ))))
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
	       (assert (eq (block-start-cleanup block)
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

;;; Return true if NODE has been deleted, false if it is still a valid part
;;; of IR1.
(defun node-deleted (node)
  (declare (type node node))
  (let ((prev (node-prev node)))
    (not (and prev
	      (not (eq (continuation-kind prev) :deleted))
	      (let ((block (continuation-block prev)))
		(and (block-component block)
		     (not (block-delete-p block))))))))

;;; Delete all the blocks and functions in Component. We scan first marking
;;; the blocks as delete-p to prevent weird stuff from being triggered by
;;; deletion.
(defun delete-component (component)
  (declare (type component component))
  (assert (null (component-new-functions component)))
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
    (assert (combination-p outside))
    (unless (combination-p inside)
      (give-up-ir1-transform))
    (let ((inside-fun (combination-fun inside)))
      (unless (eq (continuation-function-name inside-fun) fun)
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
      (if (function-type-p ltype)
	  (setf (node-derived-type ref) ltype)
	  (derive-node-type ref ltype)))
    (reoptimize-continuation (node-cont ref)))
  (values))

;;; Change all Refs for Old-Leaf to New-Leaf.
(defun substitute-leaf (new-leaf old-leaf)
  (declare (type leaf new-leaf old-leaf))
  (dolist (ref (leaf-refs old-leaf))
    (change-ref-leaf ref new-leaf))
  (values))

;;; Like SUBSITIUTE-LEAF, only there is a predicate on the Ref to tell
;;; whether to substitute.
(defun substitute-leaf-if (test new-leaf old-leaf)
  (declare (type leaf new-leaf old-leaf) (type function test))
  (dolist (ref (leaf-refs old-leaf))
    (when (funcall test ref)
      (change-ref-leaf ref new-leaf)))
  (values))

;;; Return a LEAF which represents the specified constant object. If the
;;; object is not in *CONSTANTS*, then we create a new constant LEAF and
;;; enter it.
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

;;; If there is a non-local exit noted in Entry's environment that exits to
;;; Cont in that entry, then return it, otherwise return NIL.
(defun find-nlx-info (entry cont)
  (declare (type entry entry) (type continuation cont))
  (let ((entry-cleanup (entry-cleanup entry)))
    (dolist (nlx (environment-nlx-info (node-environment entry)) nil)
      (when (and (eq (nlx-info-continuation nlx) cont)
		 (eq (nlx-info-cleanup nlx) entry-cleanup))
	(return nlx)))))

;;;; functional hackery

;;; If Functional is a Lambda, just return it; if it is an
;;; optional-dispatch, return the main-entry.
(declaim (ftype (function (functional) clambda) main-entry))
(defun main-entry (functional)
  (etypecase functional
    (clambda functional)
    (optional-dispatch
     (optional-dispatch-main-entry functional))))

;;; Returns true if Functional is a thing that can be treated like
;;; MV-Bind when it appears in an MV-Call. All fixed arguments must be
;;; optional with null default and no supplied-p. There must be a rest
;;; arg with no references.
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
;;; (:External kind) and top-level lambdas (:Top-Level kind.)
#!-sb-fluid (declaim (inline external-entry-point-p))
(defun external-entry-point-p (fun)
  (declare (type functional fun))
  (not (null (member (functional-kind fun) '(:external :top-level)))))

;;; If Cont's only use is a non-notinline global function reference, then
;;; return the referenced symbol, otherwise NIL. If Notinline-OK is true, then
;;; we don't care if the leaf is notinline.
(defun continuation-function-name (cont &optional notinline-ok)
  (declare (type continuation cont))
  (let ((use (continuation-use cont)))
    (if (ref-p use)
	(let ((leaf (ref-leaf use)))
	  (if (and (global-var-p leaf)
		   (eq (global-var-kind leaf) :global-function)
		   (or (not (defined-function-p leaf))
		       (not (eq (defined-function-inlinep leaf) :notinline))
		       notinline-ok))
	      (leaf-name leaf)
	      nil))
	nil)))

;;; Return the COMBINATION node that is the call to the let Fun.
(defun let-combination (fun)
  (declare (type clambda fun))
  (assert (member (functional-kind fun) '(:let :mv-let)))
  (continuation-dest (node-cont (first (leaf-refs fun)))))

;;; Return the initial value continuation for a let variable or NIL if none.
(defun let-var-initial-value (var)
  (declare (type lambda-var var))
  (let ((fun (lambda-var-home var)))
    (elt (combination-args (let-combination fun))
	 (position-or-lose var (lambda-vars fun)))))

;;; Return the LAMBDA that is called by the local Call.
#!-sb-fluid (declaim (inline combination-lambda))
(defun combination-lambda (call)
  (declare (type basic-combination call))
  (assert (eq (basic-combination-kind call) :local))
  (ref-leaf (continuation-use (basic-combination-fun call))))

(defvar *inline-expansion-limit* 200
  #!+sb-doc
  "An upper limit on the number of inline function calls that will be expanded
   in any given code object (single function or block compilation.)")

;;; Check whether Node's component has exceeded its inline expansion
;;; limit, and warn if so, returning NIL.
(defun inline-expansion-ok (node)
  (let ((expanded (incf (component-inline-expansions
			 (block-component
			  (node-block node))))))
    (cond ((> expanded *inline-expansion-limit*) nil)
	  ((= expanded *inline-expansion-limit*)
	   (let ((*compiler-error-context* node))
	     (compiler-note "*INLINE-EXPANSION-LIMIT* (~D) was exceeded, ~
			     probably trying to~%  ~
			     inline a recursive function."
			    *inline-expansion-limit*))
	   nil)
	  (t t))))

;;;; compiler error context determination

(declaim (special *current-path*))

;;; We bind print level and length when printing out messages so that
;;; we don't dump huge amounts of garbage.
;;;
;;; FIXME: It's not possible to get the defaults right for everyone.
;;; So: Should these variables be in the SB-EXT package? Or should we
;;; just get rid of them completely and just use the bare
;;; CL:*PRINT-FOO* variables instead?
(declaim (type (or unsigned-byte null)
	       *compiler-error-print-level*
	       *compiler-error-print-length*
	       *compiler-error-print-lines*))
(defvar *compiler-error-print-level* 5
  #!+sb-doc
  "the value for *PRINT-LEVEL* when printing compiler error messages")
(defvar *compiler-error-print-length* 10
  #!+sb-doc
  "the value for *PRINT-LENGTH* when printing compiler error messages")
(defvar *compiler-error-print-lines* 12
  #!+sb-doc
  "the value for *PRINT-LINES* when printing compiler error messages")

(defvar *enclosing-source-cutoff* 1
  #!+sb-doc
  "The maximum number of enclosing non-original source forms (i.e. from
  macroexpansion) that we print in full. For additional enclosing forms, we
  print only the CAR.")
(declaim (type unsigned-byte *enclosing-source-cutoff*))

;;; We separate the determination of compiler error contexts from the actual
;;; signalling of those errors by objectifying the error context. This allows
;;; postponement of the determination of how (and if) to signal the error.
;;;
;;; We take care not to reference any of the IR1 so that pending potential
;;; error messages won't prevent the IR1 from being GC'd. To this end, we
;;; convert source forms to strings so that source forms that contain IR1
;;; references (e.g. %DEFUN) don't hold onto the IR.
(defstruct (compiler-error-context
	    #-no-ansi-print-object
	    (:print-object (lambda (x stream)
			     (print-unreadable-object (x stream :type t))))
	    (:copier nil))
  ;; A list of the stringified CARs of the enclosing non-original source forms
  ;; exceeding the *enclosing-source-cutoff*.
  (enclosing-source nil :type list)
  ;; A list of stringified enclosing non-original source forms.
  (source nil :type list)
  ;; The stringified form in the original source that expanded into Source.
  (original-source (required-argument) :type simple-string)
  ;; A list of prefixes of "interesting" forms that enclose original-source.
  (context nil :type list)
  ;; The FILE-INFO-NAME for the relevant FILE-INFO.
  (file-name (required-argument)
	     :type (or pathname (member :lisp :stream)))
  ;; The file position at which the top-level form starts, if applicable.
  (file-position nil :type (or index null))
  ;; The original source part of the source path.
  (original-source-path nil :type list))

;;; If true, this is the node which is used as context in compiler warning
;;; messages.
(declaim (type (or null compiler-error-context node) *compiler-error-context*))
(defvar *compiler-error-context* nil)

;;; a hashtable mapping macro names to source context parsers. Each parser
;;; function returns the source-context list for that form.
(defvar *source-context-methods* (make-hash-table))

;;; documentation originally from cmu-user.tex:
;;;   This macro defines how to extract an abbreviated source context from
;;;   the \var{name}d form when it appears in the compiler input.
;;;   \var{lambda-list} is a \code{defmacro} style lambda-list used to
;;;   parse the arguments. The \var{body} should return a list of
;;;   subforms that can be printed on about one line. There are
;;;   predefined methods for \code{defstruct}, \code{defmethod}, etc. If
;;;   no method is defined, then the first two subforms are returned.
;;;   Note that this facility implicitly determines the string name
;;;   associated with anonymous functions.
;;; So even though SBCL itself only uses this macro within this file, it's a
;;; reasonable thing to put in SB-EXT in case some dedicated user wants to do
;;; some heavy tweaking to make SBCL give more informative output about his
;;; code.
(defmacro def-source-context (name lambda-list &body body)
  #!+sb-doc
  "DEF-SOURCE-CONTEXT Name Lambda-List Form*
   This macro defines how to extract an abbreviated source context from the
   Named form when it appears in the compiler input. Lambda-List is a DEFMACRO
   style lambda-list used to parse the arguments. The Body should return a
   list of subforms suitable for a \"~{~S ~}\" format string."
  (let ((n-whole (gensym)))
    `(setf (gethash ',name *source-context-methods*)
	   #'(lambda (,n-whole)
	       (destructuring-bind ,lambda-list ,n-whole ,@body)))))

(def-source-context defstruct (name-or-options &rest slots)
  (declare (ignore slots))
  `(defstruct ,(if (consp name-or-options)
		   (car name-or-options)
		   name-or-options)))

(def-source-context function (thing)
  (if (and (consp thing) (eq (first thing) 'lambda) (consp (rest thing)))
      `(lambda ,(second thing))
      `(function ,thing)))

;;; Return the first two elements of FORM if FORM is a list. Take the
;;; CAR of the second form if appropriate.
(defun source-form-context (form)
  (cond ((atom form) nil)
	((>= (length form) 2)
	 (funcall (gethash (first form) *source-context-methods*
			   #'(lambda (x)
			       (declare (ignore x))
			       (list (first form) (second form))))
		  (rest form)))
	(t
	 form)))

;;; Given a source path, return the original source form and a description
;;; of the interesting aspects of the context in which it appeared. The
;;; context is a list of lists, one sublist per context form. The sublist is a
;;; list of some of the initial subforms of the context form.
;;;
;;; For now, we use the first two subforms of each interesting form. A form is
;;; interesting if the first element is a symbol beginning with "DEF" and it is
;;; not the source form. If there is no DEF-mumble, then we use the outermost
;;; containing form. If the second subform is a list, then in some cases we
;;; return the car of that form rather than the whole form (i.e. don't show
;;; defstruct options, etc.)
(defun find-original-source (path)
  (declare (list path))
  (let* ((rpath (reverse (source-path-original-source path)))
	 (tlf (first rpath))
	 (root (find-source-root tlf *source-info*)))
    (collect ((context))
      (let ((form root)
	    (current (rest rpath)))
	(loop
	  (when (atom form)
	    (assert (null current))
	    (return))
	  (let ((head (first form)))
	    (when (symbolp head)
	      (let ((name (symbol-name head)))
		(when (and (>= (length name) 3) (string= name "DEF" :end1 3))
		  (context (source-form-context form))))))
	  (when (null current) (return))
	  (setq form (nth (pop current) form)))
	
	(cond ((context)
	       (values form (context)))
	      ((and path root)
	       (let ((c (source-form-context root)))
		 (values form (if c (list c) nil))))
	      (t
	       (values '(unable to locate source)
		       '((some strange place)))))))))

;;; Convert a source form to a string, suitably formatted for use in
;;; compiler warnings.
(defun stringify-form (form &optional (pretty t))
  (let ((*print-level* *compiler-error-print-level*)
	(*print-length* *compiler-error-print-length*)
	(*print-lines* *compiler-error-print-lines*)
	(*print-pretty* pretty))
    (if pretty
	(format nil "~<~@;  ~S~:>" (list form))
	(prin1-to-string form))))

;;; Return a COMPILER-ERROR-CONTEXT structure describing the current
;;; error context, or NIL if we can't figure anything out. ARGS is a
;;; list of things that are going to be printed out in the error
;;; message, and can thus be blown off when they appear in the source
;;; context.
(defun find-error-context (args)
  (let ((context *compiler-error-context*))
    (if (compiler-error-context-p context)
	context
	(let ((path (or *current-path*
			(if context
			    (node-source-path context)
			    nil))))
	  (when (and *source-info* path)
	    (multiple-value-bind (form src-context) (find-original-source path)
	      (collect ((full nil cons)
			(short nil cons))
		(let ((forms (source-path-forms path))
		      (n 0))
		  (dolist (src (if (member (first forms) args)
				   (rest forms)
				   forms))
		    (if (>= n *enclosing-source-cutoff*)
			(short (stringify-form (if (consp src)
						   (car src)
						   src)
					       nil))
			(full (stringify-form src)))
		    (incf n)))

		(let* ((tlf (source-path-tlf-number path))
		       (file (find-file-info tlf *source-info*)))
		  (make-compiler-error-context
		   :enclosing-source (short)
		   :source (full)
		   :original-source (stringify-form form)
		   :context src-context
		   :file-name (file-info-name file)
		   :file-position
		   (multiple-value-bind (ignore pos)
		       (find-source-root tlf *source-info*)
		     (declare (ignore ignore))
		     pos)
		   :original-source-path
		   (source-path-original-source path))))))))))

;;;; printing error messages

;;; We save the context information that we printed out most recently
;;; so that we don't print it out redundantly.

;;; The last COMPILER-ERROR-CONTEXT that we printed.
(defvar *last-error-context* nil)
(declaim (type (or compiler-error-context null) *last-error-context*))

;;; The format string and args for the last error we printed.
(defvar *last-format-string* nil)
(defvar *last-format-args* nil)
(declaim (type (or string null) *last-format-string*))
(declaim (type list *last-format-args*))

;;; The number of times that the last error message has been emitted,
;;; so that we can compress duplicate error messages.
(defvar *last-message-count* 0)
(declaim (type index *last-message-count*))

;;; If the last message was given more than once, then print out an
;;; indication of how many times it was repeated. We reset the message count
;;; when we are done.
(defun note-message-repeats (&optional (terpri t))
  (cond ((= *last-message-count* 1)
	 (when terpri (terpri *error-output*)))
	((> *last-message-count* 1)
          (format *error-output* "~&; [Last message occurs ~D times.]~2%"
		 *last-message-count*)))
  (setq *last-message-count* 0))

;;; Print out the message, with appropriate context if we can find it.
;;; If the context is different from the context of the last message
;;; we printed, then we print the context. If the original source is
;;; different from the source we are working on, then we print the
;;; current source in addition to the original source.
;;;
;;; We suppress printing of messages identical to the previous, but
;;; record the number of times that the message is repeated.
(defun print-compiler-message (format-string format-args)

  (declare (type simple-string format-string))
  (declare (type list format-args))
  
  (let ((stream *error-output*)
	(context (find-error-context format-args)))
    (cond
     (context
      (let ((file (compiler-error-context-file-name context))
	    (in (compiler-error-context-context context))
	    (form (compiler-error-context-original-source context))
	    (enclosing (compiler-error-context-enclosing-source context))
	    (source (compiler-error-context-source context))
	    (last *last-error-context*))

	(unless (and last
		     (equal file (compiler-error-context-file-name last)))
	  (when (pathnamep file)
	    (note-message-repeats)
	    (setq last nil)
            (format stream "~2&; file: ~A~%" (namestring file))))

	(unless (and last
		     (equal in (compiler-error-context-context last)))
	  (note-message-repeats)
	  (setq last nil)
          (format stream "~&")
          (pprint-logical-block (stream nil :per-line-prefix "; ")
            (format stream "in:~{~<~%    ~4:;~{ ~S~}~>~^ =>~}" in))
          (format stream "~%"))


	(unless (and last
		     (string= form
			      (compiler-error-context-original-source last)))
	  (note-message-repeats)
	  (setq last nil)
          (format stream "~&")
          (pprint-logical-block (stream nil :per-line-prefix "; ")
            (format stream "  ~A" form))
          (format stream "~&"))

	(unless (and last
		     (equal enclosing
			    (compiler-error-context-enclosing-source last)))
	  (when enclosing
	    (note-message-repeats)
	    (setq last nil)
	    (format stream "~&; --> ~{~<~%; --> ~1:;~A~> ~}~%" enclosing)))

	(unless (and last
		     (equal source (compiler-error-context-source last)))
	  (setq *last-format-string* nil)
	  (when source
	    (note-message-repeats)
	    (dolist (src source)
              (format stream "~&")
              (write-string "; ==>" stream)
              (format stream "~&")
              (pprint-logical-block (stream nil :per-line-prefix "; ")
                (write-string src stream)))))))
     (t
       (format stream "~&")
      (note-message-repeats)
      (setq *last-format-string* nil)
       (format stream "~&")))

    (setq *last-error-context* context)

    (unless (and (equal format-string *last-format-string*)
		 (tree-equal format-args *last-format-args*))
      (note-message-repeats nil)
      (setq *last-format-string* format-string)
      (setq *last-format-args* format-args)
      (let ((*print-level*  *compiler-error-print-level*)
	    (*print-length* *compiler-error-print-length*)
	    (*print-lines*  *compiler-error-print-lines*))
        (format stream "~&")
        (pprint-logical-block (stream nil :per-line-prefix "; ")
          (format stream "~&~?" format-string format-args))
        (format stream "~&"))))

  (incf *last-message-count*)
  (values))

(defun print-compiler-condition (condition)
  (declare (type condition condition))
  (let (;; These different classes of conditions have different
	;; effects on the return codes of COMPILE-FILE, so it's nice
	;; for users to be able to pick them out by lexical search
	;; through the output.
	(what (etypecase condition
		(style-warning 'style-warning)
		(warning 'warning)
		(error 'error))))
    (multiple-value-bind (format-string format-args)
	(if (typep condition 'simple-condition)
	    (values (simple-condition-format-control condition)
		    (simple-condition-format-arguments condition))
	    (values "~A"
		    (list (with-output-to-string (s)
			    (princ condition s)))))
      (print-compiler-message (format nil
				      "caught ~S:~%  ~A"
				      what
				      format-string)
			      format-args)))
  (values))

;;; COMPILER-NOTE is vaguely like COMPILER-ERROR and the other
;;; condition-signalling functions, but it just writes some output instead of
;;; signalling. (In CMU CL, it did signal a condition, but this didn't seem to
;;; work all that well; it was weird to have COMPILE-FILE return with
;;; WARNINGS-P set when the only problem was that the compiler couldn't figure
;;; out how to compile something as efficiently as it liked.)
(defun compiler-note (format-string &rest format-args)
  (unless (if *compiler-error-context*
	      (policy *compiler-error-context* (= inhibit-warnings 3))
	      (policy nil (= inhibit-warnings 3)))
    (incf *compiler-note-count*)
    (print-compiler-message (format nil "note: ~A" format-string)
			    format-args))
  (values))

;;; Issue a note when we might or might not be in the compiler.
(defun maybe-compiler-note (&rest rest)
  (if (boundp '*lexenv*) ; if we're in the compiler
      (apply #'compiler-note rest)
      (let ((stream *error-output*))
	(pprint-logical-block (stream nil :per-line-prefix ";")
	  
	  (format stream " note: ~3I~_")
	  (pprint-logical-block (stream nil)
	    (apply #'format stream rest)))
	(fresh-line stream)))) ; (outside logical block, no per-line-prefix)

;;; The politically correct way to print out progress messages and
;;; such like. We clear the current error context so that we know that
;;; it needs to be reprinted, and we also Force-Output so that the
;;; message gets seen right away.
(declaim (ftype (function (string &rest t) (values)) compiler-mumble))
(defun compiler-mumble (format-string &rest format-args)
  (note-message-repeats)
  (setq *last-error-context* nil)
  (apply #'format *error-output* format-string format-args)
  (force-output *error-output*)
  (values))

;;; Return a string that somehow names the code in Component. We use
;;; the source path for the bind node for an arbitrary entry point to
;;; find the source context, then return that as a string.
(declaim (ftype (function (component) simple-string) find-component-name))
(defun find-component-name (component)
  (let ((ep (first (block-succ (component-head component)))))
    (assert ep () "no entry points?")
    (multiple-value-bind (form context)
	(find-original-source
	 (node-source-path (continuation-next (block-start ep))))
      (declare (ignore form))
      (let ((*print-level* 2)
	    (*print-pretty* nil))
	(format nil "~{~{~S~^ ~}~^ => ~}" context)))))

;;;; condition system interface

;;; Keep track of how many times each kind of condition happens.
(defvar *compiler-error-count*)
(defvar *compiler-warning-count*)
(defvar *compiler-style-warning-count*)
(defvar *compiler-note-count*)

;;; Keep track of whether any surrounding COMPILE or COMPILE-FILE call
;;; should return WARNINGS-P or FAILURE-P.
(defvar *failure-p*)
(defvar *warnings-p*)

;;; condition handlers established by the compiler. We re-signal the
;;; condition, then if it isn't handled, we increment our warning
;;; counter and print the error message.
(defun compiler-error-handler (condition)
  (signal condition)
  (incf *compiler-error-count*)
  (setf *warnings-p* t
	*failure-p* t)
  (print-compiler-condition condition)
  (continue condition))
(defun compiler-warning-handler (condition)
  (signal condition)
  (incf *compiler-warning-count*)
  (setf *warnings-p* t
	*failure-p* t)
  (print-compiler-condition condition)
  (muffle-warning condition))
(defun compiler-style-warning-handler (condition)
  (signal condition)
  (incf *compiler-style-warning-count*)
  (setf *warnings-p* t)
  (print-compiler-condition condition)
  (muffle-warning condition))

;;;; undefined warnings

(defvar *undefined-warning-limit* 3
  #!+sb-doc
  "If non-null, then an upper limit on the number of unknown function or type
  warnings that the compiler will print for any given name in a single
  compilation. This prevents excessive amounts of output when the real
  problem is a missing definition (as opposed to a typo in the use.)")

;;; Make an entry in the *UNDEFINED-WARNINGS* describing a reference
;;; to NAME of the specified KIND. If we have exceeded the warning
;;; limit, then just increment the count, otherwise note the current
;;; error context.
;;;
;;; Undefined types are noted by a condition handler in
;;; WITH-COMPILATION-UNIT, which can potentially be invoked outside
;;; the compiler, hence the BOUNDP check.
(defun note-undefined-reference (name kind)
  (unless (and
	   ;; (POLICY NIL ..) isn't well-defined except in IR1
	   ;; conversion. This BOUNDP test seems to be a test for
	   ;; whether IR1 conversion is going on.
	   (boundp '*lexenv*)
	   ;; FIXME: I'm pretty sure the INHIBIT-WARNINGS test below
	   ;; isn't a good idea; we should have INHIBIT-WARNINGS
	   ;; affect compiler notes, not STYLE-WARNINGs. And I'm not
	   ;; sure what the BOUNDP '*LEXENV* test above is for; it's
	   ;; likely a good idea, but it probably deserves an
	   ;; explanatory comment.
	   (policy nil (= inhibit-warnings 3)))
    (let* ((found (dolist (warning *undefined-warnings* nil)
		    (when (and (equal (undefined-warning-name warning) name)
			       (eq (undefined-warning-kind warning) kind))
		      (return warning))))
	   (res (or found
		    (make-undefined-warning :name name :kind kind))))
      (unless found (push res *undefined-warnings*))
      (when (or (not *undefined-warning-limit*)
		(< (undefined-warning-count res) *undefined-warning-limit*))
	(push (find-error-context (list name))
	      (undefined-warning-warnings res)))
      (incf (undefined-warning-count res))))
  (values))

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

;;;; utilities used at run-time for parsing keyword args in IR1

;;; This function is used by the result of Parse-Deftransform to find
;;; the continuation for the value of the keyword argument Key in the
;;; list of continuations Args. It returns the continuation if the
;;; keyword is present, or NIL otherwise. The legality and
;;; constantness of the keywords should already have been checked.
(declaim (ftype (function (list keyword) (or continuation null))
		find-keyword-continuation))
(defun find-keyword-continuation (args key)
  (do ((arg args (cddr arg)))
      ((null arg) nil)
    (when (eq (continuation-value (first arg)) key)
      (return (second arg)))))

;;; This function is used by the result of Parse-Deftransform to
;;; verify that alternating continuations in Args are constant and
;;; that there is an even number of args.
(declaim (ftype (function (list) boolean) check-keywords-constant))
(defun check-keywords-constant (args)
  (do ((arg args (cddr arg)))
      ((null arg) t)
    (unless (and (rest arg)
		 (constant-continuation-p (first arg)))
      (return nil))))

;;; This function is used by the result of Parse-Deftransform to
;;; verify that the list of continuations Args is a well-formed
;;; keyword arglist and that only keywords present in the list Keys
;;; are supplied.
(declaim (ftype (function (list list) boolean) check-transform-keys))
(defun check-transform-keys (args keys)
  (and (check-keywords-constant args)
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
	     (if node
		 (policy node (= inhibit-warnings 0))
		 (policy nil (= inhibit-warnings 0))))
    (let ((*compiler-error-context* node))
      (compiler-note (event-info-description info))))

  (let ((action (event-info-action info)))
    (when action (funcall action node))))
