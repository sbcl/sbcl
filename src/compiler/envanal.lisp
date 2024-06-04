;;;; This file implements the environment analysis phase for the
;;;; compiler. This phase annotates IR1 with a hierarchy environment
;;;; structures, determining the environment that each LAMBDA
;;;; allocates its variables and finding what values are closed over
;;;; by each environment.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; Do environment analysis on the code in COMPONENT. This involves
;;; various things:
;;;  1. Make an ENVIRONMENT structure for each non-LET LAMBDA, assigning
;;;     the LAMBDA-ENVIRONMENT for all LAMBDAs.
;;;  2. Find all values that need to be closed over by each
;;;     environment.
;;;  3. Scan the blocks in the component closing over non-local-exit
;;;     continuations.
;;;  4. Delete all non-top-level functions with no references. This
;;;     should only get functions with non-NULL kinds, since normal
;;;     functions are deleted when their references go to zero.
(defun environment-analyze (component)
  (declare (type component component))
  (aver (every (lambda (x)
                 (functional-kind-eq x deleted))
               (component-new-functionals component)))
  (setf (component-new-functionals component) ())
  (dolist (fun (component-lambdas component))
    (compute-closure fun)
    (dolist (let (lambda-lets fun))
      (compute-closure let)))

  (find-non-local-exits component)
  ;; Close over closures.
  (dolist (fun (component-lambdas component))
    (when (and (functional-kind-eq fun external)
               (environment-closure (lambda-environment fun)))
      (let ((enclose-env (get-node-environment (xep-enclose fun))))
        (dolist (ref (leaf-refs fun))
          (close-over fun (get-node-environment ref) enclose-env)))))

  (find-lvar-dynamic-extents component)
  (find-cleanup-points component)
  (tail-annotate component)
  (determine-lambda-var-and-nlx-extent component)

  (dolist (fun (component-lambdas component))
    (when (null (leaf-refs fun))
      (let ((kind (functional-kind fun)))
        (unless (or (eql kind (functional-kind-attributes toplevel))
                    (functional-has-external-references-p fun))
          (aver (logtest kind (functional-kind-attributes optional cleanup escape)))
          (setf (functional-kind fun) (functional-kind-attributes nil))
          (delete-functional fun)))))

  (values))

;;; If FUN has an environment, return it, otherwise assign an empty
;;; one and return that.
(defun get-lambda-environment (fun)
  (declare (type clambda fun))
  (let ((fun (lambda-home fun)))
    (or (lambda-environment fun)
        (let ((res (make-environment :lambda fun)))
          (setf (lambda-environment fun) res)
          (dolist (lambda (lambda-lets fun))
            (setf (lambda-environment lambda) res))
          res))))

;;; Get NODE's environment, assigning one if necessary.
(defun get-node-environment (node)
  (declare (type node node))
  (get-lambda-environment (node-home-lambda node)))

;;; Find any variables in FUN with references outside of the home
;;; environment and close over them. If a closed-over variable is set,
;;; then we set the INDIRECT flag so that we will know the closed over
;;; value is really a pointer to the value cell. We also warn about
;;; unreferenced variables here, just because it's a convenient place
;;; to do it. We return true if we close over anything.
(defun compute-closure (fun)
  (declare (type clambda fun))
  (let ((env (get-lambda-environment fun))
        (did-something nil))
    (note-unreferenced-fun-vars fun)
    (dolist (var (lambda-vars fun))
      (dolist (ref (leaf-refs var))
        (let ((ref-env (get-node-environment ref)))
          (unless (eq ref-env env)
            (when (lambda-var-sets var)
              (setf (lambda-var-indirect var) t))
            (setq did-something t)
            (close-over var ref-env env))))
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

          (let ((set-env (get-node-environment set)))
            (unless (eq set-env env)
              (setq did-something t)
              (setf (lambda-var-indirect var) t)
              (close-over var set-env env))))))
    did-something))

(defun xep-enclose (xep)
  (let ((entry (functional-entry-fun xep)))
    (functional-enclose entry)))

;;; Make sure that THING is closed over in REF-ENV and in all
;;; environments for the functions that reference REF-ENV's function
;;; (not just calls). HOME-ENV is THING's home environment. When we
;;; reach the home environment, we stop propagating the closure.
(defun close-over (thing ref-env home-env)
  (declare (type environment ref-env home-env))
  (cond ((eq ref-env home-env))
        ((memq thing (environment-closure ref-env)))
        (t
         (push thing (environment-closure ref-env))
         (dolist (ref (leaf-refs (environment-lambda ref-env)))
           (close-over thing (get-node-environment ref) home-env))))
  (values))

;;; Determine whether it is possible for things that can be closed
;;; over (LAMBDA-VAR and NLX-INFO) to outlive the extent of their home
;;; environment. If so, then:
;;;   -- For indirect LAMBDA-VARs, we mark them as needing to support
;;;      indirect access (SET at some point after initial creation),
;;;   -- For NLX-INFOs, we mark them as needing to check if their
;;;      tags are still in scope.
;;;
;;; Both happen to entail the creation of heap-allocated value cells
;;; in the back-end.
;;;
;;; Because we have already closed-over all LAMBDA-VARs and NLX-INFOs
;;; at this point, they already appear in the closures of all of the
;;; CLAMBDAs that need checking.
(defun determine-lambda-var-and-nlx-extent (component)
  (dolist (fun (component-lambdas component))
    (when (and (functional-kind-eq fun external)
               ;; We treat DYNAMIC-EXTENT declarations on functions as
               ;; trusted assertions that none of the values closed
               ;; over survive the extent of the function.
               (not (leaf-dynamic-extent (functional-entry-fun fun))))
      (let ((closure (environment-closure (lambda-environment fun))))
        (dolist (thing closure)
          (typecase thing
            (lambda-var
             (when (lambda-var-indirect thing)
               (setf (lambda-var-explicit-value-cell thing) t)))
            (nlx-info
             (let ((entry (cleanup-mess-up (nlx-info-cleanup thing))))
               (dolist (exit (entry-exits entry))
                 (when (eq thing (exit-nlx-info exit))
                   (unless (policy exit (zerop safety))
                     (setf (nlx-info-safe-p thing) t)
                     (note-exit-check-elision-failure exit))))))))))))

(defun note-exit-check-elision-failure (exit)
  (when (policy exit (> speed safety))
    (let ((*compiler-error-context* (exit-entry exit)))
      (compiler-notify "~@<Allocating a value-cell at runtime for ~
                           checking possibly out of extent exit via ~S. Use ~
                           GO/RETURN-FROM with SAFETY 0, or declare the exit ~
                           function DYNAMIC-EXTENT to avoid.~:@>"
                       (node-source-form exit)))))

;;;; non-local exit

;;; Insert the entry stub before the original exit target, and add a
;;; new entry to the ENVIRONMENT-NLX-INFO. The %NLX-ENTRY call in the
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
  (declare (type environment env) (type exit exit))
  (let* ((exit-block (node-block exit))
         (next-block (first (block-succ exit-block)))
         (entry (exit-entry exit))
         (cleanup (entry-cleanup entry))
         (info (make-nlx-info cleanup (first (block-succ exit-block))))
         (new-block (insert-cleanup-code (list exit-block) next-block
                                         entry
                                         `(%nlx-entry ',info)
                                         cleanup))
         (component (block-component new-block)))
    (unlink-blocks exit-block new-block)
    (link-blocks exit-block (component-tail component))
    (link-blocks (component-head component) new-block)

    (setf (exit-nlx-info exit) info)
    (setf (nlx-info-target info) new-block)
    (push info (environment-nlx-info env))
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
;;; -- Change the %NLX-ENTRY call to use the NLX lvar so that there
;;;    will be a use to represent the NLX use.
(defun note-non-local-exit (env exit)
  (declare (type environment env) (type exit exit))
  (let ((lvar (node-lvar exit))
        (exit-fun (node-home-lambda exit))
        (info (find-nlx-info exit)))
    (cond (info
           (let ((block (node-block exit)))
             (aver (= (length (block-succ block)) 1))
             (unlink-blocks block (first (block-succ block)))
             (link-blocks block (component-tail (block-component block)))
             (setf (exit-nlx-info exit) info)))
          (t
           (insert-nlx-entry-stub exit env)
           (setq info (exit-nlx-info exit))
           (aver info)))
    (close-over info (node-environment exit) env)
    (when (functional-kind-eq exit-fun escape)
      (mapc (lambda (x)
              (setf (node-derived-type x) *wild-type*))
            (leaf-refs exit-fun))
      (substitute-leaf (find-constant info) exit-fun))
    (when lvar
      (let ((node (block-last (nlx-info-target info))))
        (unless (node-lvar node)
          (aver (eq lvar (node-lvar exit)))
          (setf (node-derived-type node) (lvar-derived-type lvar))
          (add-lvar-use node lvar)))))
  (values))

;;; Iterate over the EXITs in COMPONENT, calling NOTE-NON-LOCAL-EXIT
;;; when we find a block that ends in a non-local EXIT node.
(defun find-non-local-exits (component)
  (declare (type component component))
  (dolist (lambda (component-lambdas component))
    (dolist (entry (lambda-entries lambda))
      (let ((target-env (node-environment entry)))
        (dolist (exit (entry-exits entry))
          (aver (neq (node-environment exit) target-env))
          (note-non-local-exit target-env exit)))))
  (values))


;;; For each downward funarg, mark the funarg as dynamic extent. For
;;; now this only works on globally named functions.
(defun dxify-downward-funargs (node dxable-args fun-name)
  #+sb-xc-host
  (declare (ignore fun-name))
  (let (dynamic-extent)
    ;; Experience shows that users place incorrect DYNAMIC-EXTENT declarations
    ;; without due consideration and care. Since the declaration was ignored
    ;; in more contexts than not, it was relatively harmless.
    ;; In light of that, only make this transform if willing to generate
    ;; wrong code, or if the declaration can be trusted.
    ;; [It's seems to be true that users who want this are OK with lack of
    ;; tail-callability and/or potential stack exhaustion due to the assumption
    ;; that callers should always use more stack space. You should really
    ;; only do that if you don't also need an arbitrarily long call chain.
    ;; MAP and friends are good examples where this pertains]
    (when #+sb-xc-host t                ; always trust our own code
          #-sb-xc-host
          (or (let ((pkg (sb-xc:symbol-package (fun-name-block-name fun-name))))
                ;; callee "probably" won't get redefined
                (or (not pkg)
                    (package-locked-p pkg)
                    (system-package-p pkg)
                    (eq pkg *cl-package*)
                    (basic-combination-fun-info node)))
              (policy node (= safety 0)))
          (dolist (arg-spec dxable-args)
            (when (symbolp arg-spec)
              ;; If there are keywords, we had better have a FUN-TYPE
              (let ((fun-type (lvar-type (combination-fun node))))
                ;; Can't do anything unless we can ascertain where
                ;; the keyword arguments start.
                (when (fun-type-p fun-type)
                  (let* ((keys-index
                           (+ (length (fun-type-required fun-type))
                              (length (fun-type-optional fun-type))))
                         (keywords-supplied
                           (nthcdr keys-index (combination-args node))))
                    ;; Everything in a keyword position needs to be
                    ;; constant.
                    (loop
                      (unless (cdr keywords-supplied) (return))
                      (let ((keyword (car keywords-supplied)))
                        (unless (constant-lvar-p keyword)
                          (return))
                        (when (eq (lvar-value keyword) arg-spec)
                          ;; Map it to a positional arg
                          (setq arg-spec (1+ keys-index))
                          (return))
                        (setq keywords-supplied (cddr keywords-supplied))
                        (incf keys-index 2)))))))
            (when (integerp arg-spec)
              (let* ((arg (or (nth arg-spec (combination-args node))
                              (return-from dxify-downward-funargs)))
                     (use (principal-lvar-use arg)))
                (when (and (not (lvar-dynamic-extent arg))
                           ;; We check that the use is a lambda so
                           ;; that we don't end up getting notes about
                           ;; not being able to allocate later.
                           (ref-p use)
                           (lambda-p (ref-leaf use))
                           (not (leaf-dynamic-extent (functional-entry-fun (ref-leaf use)))))
                  (let ((enclose (xep-enclose (ref-leaf use))))
                    (cond ((and enclose
                                (enclose-dynamic-extent enclose))
                           (setf (leaf-dynamic-extent (functional-entry-fun (ref-leaf use)))
                                 t))
                          (t
                           (unless dynamic-extent
                             (setq dynamic-extent (insert-dynamic-extent node)))
                           (setf (lvar-dynamic-extent arg) dynamic-extent)
                           (push arg (dynamic-extent-values dynamic-extent))))))))))))

;;; Make LVAR have DYNAMIC-EXTENT, recursively looking for otherwise
;;; inaccessible potentially stack-allocatable parts. If LVAR already
;;; has a different dynamic extent set, we don't do anything.
(defun find-stack-allocatable-parts (lvar dynamic-extent)
  (declare (type lvar lvar)
           (type cdynamic-extent dynamic-extent))
  (unless (lvar-dynamic-extent lvar)
    (setf (lvar-dynamic-extent lvar) dynamic-extent))
  (when (eq (lvar-dynamic-extent lvar) dynamic-extent)
    (do-uses (use lvar)
      (when (use-good-for-dx-p use dynamic-extent)
        (etypecase use
          (cast
           (find-stack-allocatable-parts (cast-value use) dynamic-extent))
          (combination
           ;; Don't propagate through &REST, for sanity.
           (unless (eq (combination-fun-source-name use nil)
                       '%listify-rest-args)
             (dolist (arg (combination-args use))
               (when (and arg
                          (lvar-good-for-dx-p arg dynamic-extent))
                 (find-stack-allocatable-parts arg dynamic-extent)))))
          (ref
           (let ((leaf (ref-leaf use)))
             (typecase leaf
               (lambda-var
                (find-stack-allocatable-parts (let-var-initial-value leaf)
                                              dynamic-extent))
               (clambda
                (let ((fun (functional-entry-fun leaf)))
                  (setf (enclose-dynamic-extent (functional-enclose fun))
                        dynamic-extent)
                  (setf (leaf-dynamic-extent fun) t)))))))))))

;;; Find all stack allocatable values in COMPONENT, setting
;;; appropriate dynamic extents for any lvar which may take on a stack
;;; allocatable value. If a dynamic extent is in fact associated with
;;; a stack allocatable value, note that fact by setting its info.
(defun find-lvar-dynamic-extents (component)
  (declare (type component component))
  (do-blocks (block component)
    (do-nodes (node nil block)
      (when (and (combination-p node)
                 (memq (basic-combination-kind node)
                       '(:full :unknown-keys :known)))
        (let ((name (combination-fun-source-name node nil)))
          (when name
            (let ((dxable-args (fun-name-dx-args name)))
              (when dxable-args
                (dxify-downward-funargs node dxable-args name))))))))
  ;; For each dynamic extent declared variable, each value that the
  ;; variable can take on is also dynamic extent.
  (dolist (lambda (component-lambdas component))
    (dolist (var (lambda-vars lambda))
      (when (leaf-dynamic-extent var)
        (let ((values (mapcar #'set-value (basic-var-sets var))))
          (when values
            ;; This dynamic extent is over the whole environment and
            ;; needs no cleanup code.
            (let ((dynamic-extent (make-dynamic-extent :values values
                                                       :cleanup nil)))
              (push dynamic-extent (lambda-dynamic-extents lambda))
              (dolist (value values)
                (setf (lvar-dynamic-extent value) dynamic-extent)))))))
    (dolist (let (lambda-lets lambda))
      (dolist (var (lambda-vars let))
        (when (leaf-dynamic-extent var)
          (let ((initial-value (let-var-initial-value var)))
            ;; FIXME: This is overly pessimistic; a flushed initial
            ;; value should not inhibit stack allocation.
            (when initial-value
              (let ((dynamic-extent (lvar-dynamic-extent initial-value)))
                (dolist (set (basic-var-sets var))
                  ;; The environments of the SET and the dynamic
                  ;; extent must be the same to ensure the set value
                  ;; lives on the stack long enough.
                  (when (eq (node-environment set)
                            (node-environment dynamic-extent))
                    (let ((set-value (set-value set)))
                      (unless (lvar-dynamic-extent set-value)
                        (setf (lvar-dynamic-extent set-value) dynamic-extent)
                        (push set-value (dynamic-extent-values dynamic-extent)))))))))))))
  (dolist (lambda (component-lambdas component))
    (dolist (dynamic-extent (lambda-dynamic-extents lambda))
      (dolist (lvar (dynamic-extent-values dynamic-extent))
        (aver (eq dynamic-extent (lvar-dynamic-extent lvar)))
        ;; Check that the value hasn't been flushed somehow.
        (when (lvar-uses lvar)
          (cond ((lvar-good-for-dx-p lvar dynamic-extent)
                 (find-stack-allocatable-parts lvar dynamic-extent)
                 (unless (or (dynamic-extent-info dynamic-extent)
                             (null (dynamic-extent-cleanup dynamic-extent)))
                   (setf (dynamic-extent-info dynamic-extent) (make-lvar))))
                (t
                 (setf (lvar-dynamic-extent lvar) nil)))))))
  (dolist (lambda (component-lambdas component))
    (let ((fun (if (functional-kind-eq lambda optional)
                   (lambda-optional-dispatch lambda)
                   lambda)))
      (when (leaf-dynamic-extent fun)
        (let ((xep (functional-entry-fun fun)))
          ;; We need to have a closure environment to dynamic-extent
          ;; allocate.
          (when (and xep (environment-closure (get-lambda-environment xep)))
            (let ((dynamic-extent
                    (enclose-dynamic-extent (functional-enclose fun))))
              (unless (dynamic-extent-info dynamic-extent)
                (setf (dynamic-extent-info dynamic-extent) (make-lvar)))))))))
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
(defun emit-cleanups (pred-blocks succ-block)
  (collect ((code)
            (reanalyze-funs))
    (let ((succ-cleanup (block-start-cleanup succ-block)))
      (do-nested-cleanups (cleanup (car pred-blocks))
        (when (eq cleanup succ-cleanup)
          (return))
        (let* ((node (cleanup-mess-up cleanup))
               (args (when (basic-combination-p node)
                       (basic-combination-args node))))
          (ecase (cleanup-kind cleanup)
            (:special-bind
             (code `(%special-unbind ',(leaf-source-name (lvar-value (car args))))))
            (:catch
             (code `(%catch-breakup ',(car (cleanup-nlx-info cleanup)))))
            (:unwind-protect
             (code `(%unwind-protect-breakup ',(car (cleanup-nlx-info cleanup))))
             (let ((fun (ref-leaf (lvar-uses (second args)))))
                (when (functional-p fun)
                  (reanalyze-funs fun)
                  (code `(%funcall ,fun)))))
            ((:block :tagbody)
             (dolist (nlx (cleanup-nlx-info cleanup))
               (code `(%lexical-exit-breakup ',nlx))))
            (:dynamic-extent
             (when (dynamic-extent-info node)
               (code `(%cleanup-point))))
            (:restore-nsp
             (code `(%primitive set-nsp ,(ref-leaf node))))))))
    (flet ((coalesce-unbinds (code)
             (if (vop-existsp :named sb-c:unbind-n)
              (loop with cleanup
                    while code
                    do (setf cleanup (pop code))
                    collect (if (eq (car cleanup) '%special-unbind)
                                `(%special-unbind
                                  ,(cadr cleanup)
                                  ,@(loop while (eq (caar code) '%special-unbind)
                                          collect (cadar code)
                                          do (pop code)))
                                cleanup))
                 code)))
     (when (code)
       (aver (not (node-tail-p (block-last (car pred-blocks)))))
       (insert-cleanup-code
        pred-blocks succ-block (block-last (car pred-blocks))
        `(progn ,@(coalesce-unbinds (code))))
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
    (unless (block-to-be-deleted-p block1)
      (let ((env1 (block-environment block1))
            (cleanup1 (block-end-cleanup block1)))
        (dolist (block2 (block-succ block1))
          (when (block-start block2)
            (let ((env2 (block-environment block2))
                  (cleanup2 (block-start-cleanup block2)))
              (unless (or (not (eq env2 env1))
                          (eq cleanup1 cleanup2)
                          (and cleanup2
                               (eq (node-enclosing-cleanup
                                    (cleanup-mess-up cleanup2))
                                   cleanup1)))
                ;; If multiple blocks with the same cleanups end up at the same block
                ;; issue only one cleanup, e.g. (let (*) (if x 1 2))
                ;;
                ;; Possible improvement: (let (*) (if x (let (**) 1) 2))
                ;; unbinding * only once.
                (emit-cleanups (loop for pred in (block-pred block2)
                                     when (or (eq pred block1)
                                              (and
                                               (block-start pred)
                                               (eq (block-end-cleanup pred) cleanup1)
                                               (eq (block-environment pred) env2)))
                                     collect pred)
                               block2))))))))
  (values))

;;; Mark all tail-recursive uses of function result continuations with
;;; the corresponding TAIL-SET.
(defun tail-annotate (component)
  (declare (type component component))
  (dolist (fun (component-lambdas component))
    (let ((ret (lambda-return fun)))
      (when ret
        (let ((result (return-result ret)))
          (do-uses (use result)
            (when (and (immediately-used-p result use)
                       (or (not (eq (node-derived-type use) *empty-type*))
                           (not (basic-combination-p use))
                           ;; This prevents external entry points from
                           ;; showing up in the backtrace: we always
                           ;; want tail calls inside XEPs to the
                           ;; functions they are the entry point for.
                           (eq (basic-combination-kind use) :local)))
              (setf (node-tail-p use) t)))))))
  ;; Tail call non returning functions if no debugging is wanted.
  (dolist (block (block-pred (component-tail component)))
    (let ((last (block-last block)))
      (when (and (combination-p last)
                 (combination-fun-info last)
                 (policy last (= debug 0))
                 (do-nested-cleanups (cleanup block t)
                   (case (cleanup-kind cleanup)
                     ((:block :tagbody)
                      (when (entry-exits (cleanup-mess-up cleanup))
                        (return nil)))
                     (t (return nil)))))
        (setf (node-tail-p last) t))))
  (values))
