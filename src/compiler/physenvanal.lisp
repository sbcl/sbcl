;;;; This file implements the environment analysis phase for the
;;;; compiler. This phase annotates IR1 with a hierarchy environment
;;;; structures, determining the physical environment that each LAMBDA
;;;; allocates its variables and finding what values are closed over
;;;; by each physical environment.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; Do environment analysis on the code in COMPONENT. This involves
;;; various things:
;;;  1. Make a PHYSENV structure for each non-LET LAMBDA, assigning 
;;;     the LAMBDA-PHYSENV for all LAMBDAs.
;;;  2. Find all values that need to be closed over by each
;;;     physical environment.
;;;  3. Scan the blocks in the component closing over non-local-exit
;;;     continuations.
;;;  4. Delete all non-top-level functions with no references. This
;;;     should only get functions with non-NULL kinds, since normal
;;;     functions are deleted when their references go to zero. 
(defun physenv-analyze (component)
  (declare (type component component))
  (aver (every (lambda (x)
		 (eq (functional-kind x) :deleted))
	       (component-new-functionals component)))
  (setf (component-new-functionals component) ())
  (dolist (clambda (component-lambdas component))
    (reinit-lambda-physenv clambda))
  (mapc #'add-lambda-vars-and-let-vars-to-closures
	(component-lambdas component))

  (find-non-local-exits component)
  (recheck-dynamic-extent-lvars component)
  (find-cleanup-points component)
  (tail-annotate component)

  (dolist (fun (component-lambdas component))
    (when (null (leaf-refs fun))
      (let ((kind (functional-kind fun)))
	(unless (or (eq kind :toplevel)
		    (functional-has-external-references-p fun))
	  (aver (member kind '(:optional :cleanup :escape)))
	  (setf (functional-kind fun) nil)
          (delete-functional fun)))))

  (setf (component-nlx-info-generated-p component) t)
  (values))

;;; This is to be called on a COMPONENT with top level LAMBDAs before
;;; the compilation of the associated non-top-level code to detect
;;; closed over top level variables. We just do COMPUTE-CLOSURE on all
;;; the lambdas. This will pre-allocate environments for all the
;;; functions with closed-over top level variables. The post-pass will
;;; use the existing structure, rather than allocating a new one. We
;;; return true if we discover any possible closure vars.
(defun pre-physenv-analyze-toplevel (component)
  (declare (type component component))
  (let ((found-it nil))
    (dolist (lambda (component-lambdas component))
      (when (add-lambda-vars-and-let-vars-to-closures lambda)
	(setq found-it t)))
    found-it))

;;; This is like old CMU CL PRE-ENVIRONMENT-ANALYZE-TOPLEVEL, except
;;;   (1) It's been brought into the post-0.7.0 world where the property
;;;       HAS-EXTERNAL-REFERENCES-P is orthogonal to the property of
;;;       being specialized/optimized for locall at top level.
;;;   (2) There's no return value, since we don't care whether we
;;;       find any possible closure variables.
;;;
;;; I wish I could find an explanation of why
;;; PRE-ENVIRONMENT-ANALYZE-TOPLEVEL is important. The old CMU CL
;;; comments said
;;;     Called on component with top level lambdas before the
;;;     compilation of the associated non-top-level code to detect
;;;     closed over top level variables. We just do COMPUTE-CLOSURE on
;;;     all the lambdas. This will pre-allocate environments for all
;;;     the functions with closed-over top level variables. The
;;;     post-pass will use the existing structure, rather than
;;;     allocating a new one. We return true if we discover any
;;;     possible closure vars.
;;; But that doesn't seem to explain either why it's important to do
;;; this for top level lambdas, or why it's important to do it only
;;; for top level lambdas instead of just doing it indiscriminately
;;; for all lambdas. I do observe that when it's not done, compiler
;;; assertions occasionally fail. My tentative hypothesis for why it's
;;; important to do it is that other environment analysis expects to
;;; bottom out on the outermost enclosing thing, and (insert
;;; mysterious reason here) it's important to set up bottomed-out-here
;;; environments before anything else. I haven't been able to guess
;;; why it's important to do it selectively instead of
;;; indiscriminately. -- WHN 2001-11-10
(defun preallocate-physenvs-for-toplevelish-lambdas (component)
  (dolist (clambda (component-lambdas component))
    (when (lambda-toplevelish-p clambda)
      (add-lambda-vars-and-let-vars-to-closures clambda)))
  (values))

;;; If CLAMBDA has a PHYSENV, return it, otherwise assign an empty one
;;; and return that.
(defun get-lambda-physenv (clambda)
  (declare (type clambda clambda))
  (let ((homefun (lambda-home clambda)))
    (or (lambda-physenv homefun)
	(let ((res (make-physenv :lambda homefun)))
	  (setf (lambda-physenv homefun) res)
	  ;; All the LETLAMBDAs belong to HOMEFUN, and share the same
	  ;; PHYSENV. Thus, (1) since HOMEFUN's PHYSENV was NIL,
	  ;; theirs should be NIL too, and (2) since we're modifying
	  ;; HOMEFUN's PHYSENV, we should modify theirs, too.
	  (dolist (letlambda (lambda-lets homefun))
	    (aver (eql (lambda-home letlambda) homefun))
	    (aver (null (lambda-physenv letlambda)))
	    (setf (lambda-physenv letlambda) res))
	  res))))

;;; If FUN has no physical environment, assign one, otherwise clean up
;;; the old physical environment, removing/flagging variables that
;;; have no sets or refs. If a var has no references, we remove it
;;; from the closure. We always clear the INDIRECT flag. This is
;;; necessary because pre-analysis is done before optimization.
(defun reinit-lambda-physenv (fun)
  (let ((old (lambda-physenv (lambda-home fun))))
    (cond (old
	   (setf (physenv-closure old)
		 (delete-if (lambda (x)
			      (and (lambda-var-p x)
				   (null (leaf-refs x))))
			    (physenv-closure old)))
	   (flet ((clear (fun)
		    (dolist (var (lambda-vars fun))
		      (setf (lambda-var-indirect var) nil))))
	     (clear fun)
	     (map nil #'clear (lambda-lets fun))))
	  (t
	   (get-lambda-physenv fun))))
  (values))

;;; Get NODE's environment, assigning one if necessary.
(defun get-node-physenv (node)
  (declare (type node node))
  (get-lambda-physenv (node-home-lambda node)))

;;; private guts of ADD-LAMBDA-VARS-AND-LET-VARS-TO-CLOSURES
;;;
;;; This is the old CMU CL COMPUTE-CLOSURE, which only works on
;;; LAMBDA-VARS directly, not on the LAMBDA-VARS of LAMBDA-LETS. It
;;; seems never to be valid to use this operation alone, so in SBCL,
;;; it's private, and the public interface,
;;; ADD-LAMBDA-VARS-AND-LET-VARS-TO-CLOSURES, always runs over all the
;;; variables, not only the LAMBDA-VARS of CLAMBDA itself but also
;;; the LAMBDA-VARS of CLAMBDA's LAMBDA-LETS.
(defun %add-lambda-vars-to-closures (clambda)
  (let ((physenv (get-lambda-physenv clambda))
	(did-something nil))
    (note-unreferenced-vars clambda)
    (dolist (var (lambda-vars clambda))
      (dolist (ref (leaf-refs var))
	(let ((ref-physenv (get-node-physenv ref)))
	  (unless (eq ref-physenv physenv)
	    (when (lambda-var-sets var)
	      (setf (lambda-var-indirect var) t))
	    (setq did-something t)
	    (close-over var ref-physenv physenv))))
      (dolist (set (basic-var-sets var))

	;; Variables which are set but never referenced can be
	;; optimized away, and closing over them here would just
	;; interfere with that. (In bug 147, it *did* interfere with
	;; that, causing confusion later. This UNLESS solves that
	;; problem, but I (WHN) am not 100% sure it's best to solve
	;; the problem this way instead of somehow solving it
	;; somewhere upstream and just doing (AVER (LEAF-REFS VAR))
	;; here.)
	(unless (null (leaf-refs var))

	  (let ((set-physenv (get-node-physenv set)))
	    (unless (eq set-physenv physenv)
              (setf did-something t
		    (lambda-var-indirect var) t)
	      (close-over var set-physenv physenv))))))
    did-something))

;;; Find any variables in CLAMBDA -- either directly in LAMBDA-VARS or
;;; in the LAMBDA-VARS of elements of LAMBDA-LETS -- with references
;;; outside of the home environment and close over them. If a
;;; closed-over variable is set, then we set the INDIRECT flag so that
;;; we will know the closed over value is really a pointer to the
;;; value cell. We also warn about unreferenced variables here, just
;;; because it's a convenient place to do it. We return true if we
;;; close over anything.
(defun add-lambda-vars-and-let-vars-to-closures (clambda)
  (declare (type clambda clambda))
  (let ((did-something nil))
    (when (%add-lambda-vars-to-closures clambda)
      (setf did-something t))
    (dolist (lambda-let (lambda-lets clambda))
      ;; There's no need to recurse through full COMPUTE-CLOSURE
      ;; here, since LETS only go one layer deep.
      (aver (null (lambda-lets lambda-let)))
      (when (%add-lambda-vars-to-closures lambda-let)
	(setf did-something t)))
    did-something))

;;; Make sure that THING is closed over in REF-PHYSENV and in all
;;; PHYSENVs for the functions that reference REF-PHYSENV's function
;;; (not just calls). HOME-PHYSENV is THING's home environment. When we
;;; reach the home environment, we stop propagating the closure.
(defun close-over (thing ref-physenv home-physenv)
  (declare (type physenv ref-physenv home-physenv))
  (let ((flooded-physenvs nil))
    (named-let flood ((flooded-physenv ref-physenv))
      (unless (or (eql flooded-physenv home-physenv)
		  (member flooded-physenv flooded-physenvs))
	(push flooded-physenv flooded-physenvs)
	(pushnew thing (physenv-closure flooded-physenv))
	(dolist (ref (leaf-refs (physenv-lambda flooded-physenv)))
	  (flood (get-node-physenv ref))))))
  (values))

;;;; non-local exit

;;; Insert the entry stub before the original exit target, and add a
;;; new entry to the PHYSENV-NLX-INFO. The %NLX-ENTRY call in the
;;; stub is passed the NLX-INFO as an argument so that the back end
;;; knows what entry is being done.
;;;
;;; The link from the EXIT block to the entry stub is changed to be a
;;; link from the component head. Similarly, the EXIT block is linked
;;; to the component tail. This leaves the entry stub reachable, but
;;; makes the flow graph less confusing to flow analysis.
;;;
;;; If a CATCH or an UNWIND-protect, then we set the LEXENV for the
;;; last node in the cleanup code to be the enclosing environment, to
;;; represent the fact that the binding was undone as a side effect of
;;; the exit. This will cause a lexical exit to be broken up if we are
;;; actually exiting the scope (i.e. a BLOCK), and will also do any
;;; other cleanups that may have to be done on the way.
(defun insert-nlx-entry-stub (exit env)
  (declare (type physenv env) (type exit exit))
  (let* ((exit-block (node-block exit))
	 (next-block (first (block-succ exit-block)))
	 (entry (exit-entry exit))
	 (cleanup (entry-cleanup entry))
	 (info (make-nlx-info cleanup exit))
	 (new-block (insert-cleanup-code exit-block next-block
					 entry
					 `(%nlx-entry ',info)
					 cleanup))
	 (component (block-component new-block)))
    (unlink-blocks exit-block new-block)
    (link-blocks exit-block (component-tail component))
    (link-blocks (component-head component) new-block)

    (setf (nlx-info-target info) new-block)
    (push info (physenv-nlx-info env))
    (push info (cleanup-nlx-info cleanup))
    (when (member (cleanup-kind cleanup) '(:catch :unwind-protect))
      (setf (node-lexenv (block-last new-block))
	    (node-lexenv entry))))

  (values))

;;; Do stuff necessary to represent a non-local exit from the node
;;; EXIT into ENV. This is called for each non-local exit node, of
;;; which there may be several per exit continuation. This is what we
;;; do:
;;; -- If there isn't any NLX-INFO entry in the environment, make
;;;    an entry stub, otherwise just move the exit block link to
;;;    the component tail.
;;; -- Close over the NLX-INFO in the exit environment.
;;; -- If the exit is from an :ESCAPE function, then substitute a
;;;    constant reference to NLX-INFO structure for the escape
;;;    function reference. This will cause the escape function to
;;;    be deleted (although not removed from the DFO.)  The escape
;;;    function is no longer needed, and we don't want to emit code
;;;    for it.
;;; -- Change the %NLX-ENTRY call to use the NLX lvar so that 1) there
;;;    will be a use to represent the NLX use; 2) make life easier for
;;;    the stack analysis.
(defun note-non-local-exit (env exit)
  (declare (type physenv env) (type exit exit))
  (let ((lvar (node-lvar exit))
	(exit-fun (node-home-lambda exit)))
    (if (find-nlx-info exit)
	(let ((block (node-block exit)))
	  (aver (= (length (block-succ block)) 1))
	  (unlink-blocks block (first (block-succ block)))
	  (link-blocks block (component-tail (block-component block))))
	(insert-nlx-entry-stub exit env))
    (let ((info (find-nlx-info exit)))
      (aver info)
      (close-over info (node-physenv exit) env)
      (when (eq (functional-kind exit-fun) :escape)
	(mapc (lambda (x)
		(setf (node-derived-type x) *wild-type*))
	      (leaf-refs exit-fun))
	(substitute-leaf (find-constant info) exit-fun))
      (when lvar
        (let ((node (block-last (nlx-info-target info))))
          (unless (node-lvar node)
            (aver (eq lvar (node-lvar exit)))
            (setf (node-derived-type node) (lvar-derived-type lvar))
            (add-lvar-use node lvar))))))
  (values))

;;; Iterate over the EXITs in COMPONENT, calling NOTE-NON-LOCAL-EXIT
;;; when we find a block that ends in a non-local EXIT node. We also
;;; ensure that all EXIT nodes are either non-local or degenerate by
;;; calling IR1-OPTIMIZE-EXIT on local exits. This makes life simpler
;;; for later phases.
(defun find-non-local-exits (component)
  (declare (type component component))
  (dolist (lambda (component-lambdas component))
    (dolist (entry (lambda-entries lambda))
      (dolist (exit (entry-exits entry))
	(let ((target-physenv (node-physenv entry)))
	  (if (eq (node-physenv exit) target-physenv)
	      (maybe-delete-exit exit)
	      (note-non-local-exit target-physenv exit))))))
  (values))

;;;; final decision on stack allocation of dynamic-extent structores
(defun recheck-dynamic-extent-lvars (component)
  (declare (type component component))
  (dolist (lambda (component-lambdas component))
    (loop for entry in (lambda-entries lambda)
            for cleanup = (entry-cleanup entry)
            do (when (eq (cleanup-kind cleanup) :dynamic-extent)
                 (collect ((real-dx-lvars))
                   (loop for lvar in (cleanup-info cleanup)
                         do (let ((use (lvar-uses lvar)))
                              (if (and (combination-p use)
                                       (eq (basic-combination-kind use) :known)
                                       (awhen (fun-info-stack-allocate-result
                                               (basic-combination-fun-info use))
                                         (funcall it use)))
                                  (real-dx-lvars lvar)
                                  (setf (lvar-dynamic-extent lvar) nil))))
                   (setf (cleanup-info cleanup) (real-dx-lvars))))))
  (values))

;;;; cleanup emission

;;; Zoom up the cleanup nesting until we hit CLEANUP1, accumulating
;;; cleanup code as we go. When we are done, convert the cleanup code
;;; in an implicit MV-PROG1. We have to force local call analysis of
;;; new references to UNWIND-PROTECT cleanup functions. If we don't
;;; actually have to do anything, then we don't insert any cleanup
;;; code. (FIXME: There's some confusion here, left over from CMU CL
;;; comments. CLEANUP1 isn't mentioned in the code of this function.
;;; It is in code elsewhere, but if the comments for this function
;;; mention it they should explain the relationship to the other code.)
;;;
;;; If we do insert cleanup code, we check that BLOCK1 doesn't end in
;;; a "tail" local call.
;;;
;;; We don't need to adjust the ending cleanup of the cleanup block,
;;; since the cleanup blocks are inserted at the start of the DFO, and
;;; are thus never scanned.
(defun emit-cleanups (block1 block2)
  (declare (type cblock block1 block2))
  (collect ((code)
	    (reanalyze-funs))
    (let ((cleanup2 (block-start-cleanup block2)))
      (do ((cleanup (block-end-cleanup block1)
		    (node-enclosing-cleanup (cleanup-mess-up cleanup))))
	  ((eq cleanup cleanup2))
	(let* ((node (cleanup-mess-up cleanup))
	       (args (when (basic-combination-p node)
		       (basic-combination-args node))))
	  (ecase (cleanup-kind cleanup)
	    (:special-bind
	     (code `(%special-unbind ',(lvar-value (first args)))))
	    (:catch
	     (code `(%catch-breakup)))
	    (:unwind-protect
	     (code `(%unwind-protect-breakup))
	     (let ((fun (ref-leaf (lvar-uses (second args)))))
	       (reanalyze-funs fun)
	       (code `(%funcall ,fun))))
	    ((:block :tagbody)
	     (dolist (nlx (cleanup-nlx-info cleanup))
	       (code `(%lexical-exit-breakup ',nlx))))
	    (:dynamic-extent
	     (when (not (null (cleanup-info cleanup)))
               (code `(%dynamic-extent-end)))))))

      (when (code)
	(aver (not (node-tail-p (block-last block1))))
	(insert-cleanup-code block1 block2
			     (block-last block1)
			     `(progn ,@(code)))
	(dolist (fun (reanalyze-funs))
	  (locall-analyze-fun-1 fun)))))

  (values))

;;; Loop over the blocks in COMPONENT, calling EMIT-CLEANUPS when we
;;; see a successor in the same environment with a different cleanup.
;;; We ignore the cleanup transition if it is to a cleanup enclosed by
;;; the current cleanup, since in that case we are just messing up the
;;; environment, hence this is not the place to clean it.
(defun find-cleanup-points (component)
  (declare (type component component))
  (do-blocks (block1 component)
    (let ((env1 (block-physenv block1))
	  (cleanup1 (block-end-cleanup block1)))
      (dolist (block2 (block-succ block1))
	(when (block-start block2)
	  (let ((env2 (block-physenv block2))
		(cleanup2 (block-start-cleanup block2)))
	    (unless (or (not (eq env2 env1))
			(eq cleanup1 cleanup2)
			(and cleanup2
			     (eq (node-enclosing-cleanup
				  (cleanup-mess-up cleanup2))
				 cleanup1)))
	      (emit-cleanups block1 block2)))))))
  (values))

;;; Mark optimizable tail-recursive uses of function result
;;; continuations with the corresponding TAIL-SET.
(defun tail-annotate (component)
  (declare (type component component))
  (dolist (fun (component-lambdas component))
    (let ((ret (lambda-return fun)))
      ;; Nodes whose type is NIL (i.e. don't return) such as calls to
      ;; ERROR are never annotated as TAIL-P, in order to preserve
      ;; debugging information.
      ;;
      ;; FIXME: It might be better to add another DEFKNOWN property
      ;; (e.g. NO-TAIL-RECURSION) and use it for error-handling
      ;; functions like ERROR, instead of spreading this special case
      ;; net so widely.
      (when ret
	(let ((result (return-result ret)))
	  (do-uses (use result)
	    (when (and (policy use merge-tail-calls)
                       (basic-combination-p use)
		       (immediately-used-p result use)
		       (or (not (eq (node-derived-type use) *empty-type*))
			   (eq (basic-combination-kind use) :local)))
	      (setf (node-tail-p use) t)))))))
  (values))
