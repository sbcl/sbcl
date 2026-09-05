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
        (unless (eql kind (functional-kind-attributes toplevel))
          (aver (logtest kind (functional-kind-attributes optional cleanup escape)))
          (setf (functional-kind fun) (functional-kind-attributes nil))
          (delete-functional fun)))))

  (values))

;;; If FUN has an environment, return it, otherwise assign an empty
;;; one and return that.
(defun get-lambda-environment (fun)
  (declare (type clambda fun)
           (inline make-environment))
  (let ((fun (lambda-home fun)))
    (or (lambda-environment fun)
        (let ((res (make-environment fun)))
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
  (declare (type environment env) (type exit exit)
           (inline make-nlx-info))
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
(defun dxify-downward-funargs (node)
  (let* ((fun-name (combination-fun-source-name node nil))
         (dxable-args (and fun-name
                           (fun-name-dx-args fun-name)))
         dynamic-extent)
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
    (when (and dxable-args
               #-sb-xc-host                   ; always trust our own code
               (or (let ((pkg (sb-xc:symbol-package
                               (fun-name-block-name fun-name))))
                     ;; callee "probably" won't get redefined
                     (or (not pkg)
                         (package-locked-p pkg)
                         (system-package-p pkg)
                         (eq pkg *cl-package*)
                         (basic-combination-fun-info node)))
                   (policy node (= safety 0))))
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
                 (lvar (principal-lvar arg)))
            (do-uses (use lvar)
              (when (and (not (lvar-dynamic-extent arg))
                         (ref-p use)
                         (lambda-p (ref-leaf use))
                         (not (leaf-dynamic-extent (functional-entry-fun (ref-leaf use)))))
                (unless dynamic-extent
                  (setq dynamic-extent (insert-dynamic-extent node)))
                (setf (lvar-dynamic-extent arg) dynamic-extent)
                (push arg (dynamic-extent-values dynamic-extent))))))))))

;;; Check if all references to LEAF (other than USE) are bounded by
;;; dynamic extents or safely discarded and therefore do not escape.
(defun leaf-refs-not-escape-elsewhere-p (leaf use &optional visited)
  (dolist (ref (leaf-refs leaf) t)
    (unless (eq use ref)
      (multiple-value-bind (dest p-lvar) (principal-lvar-end (node-lvar ref))
        (when (and dest
                   (not (or (lvar-dynamic-extent p-lvar)
                            (ref-good-for-dx-p ref visited))))
          (return nil))))))

;;; Check that REF delivers a value to a combination which is DX safe
;;; or whose result is that value and ends up being discarded.
(defun ref-good-for-dx-p (ref &optional visited)
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
            (or (memq ref visited)
                (progn
                  (push ref visited)
                  (loop for arg in (combination-args dest)
                        for var in (lambda-vars (combination-lambda dest))
                        do (when (eq arg lvar)
                             (return (leaf-refs-not-escape-elsewhere-p var nil visited)))
                        finally (sb-impl::unreachable)))))))))

;;; Find which environments escape because they are closed over by
;;; other external entry points which themselves escape. An entry
;;; point is considered to escape if it is closed over by a non
;;; dynamic extent lambda whose references escape. If an environment's
;;; lambda closes over itself, we do not take that into account here,
;;; to make it easier for others to check whether the lambda escapes
;;; even when excluding one of its references.
(defun analyze-escaping-closure-environments (component)
  (declare (type component component))
  (dolist (xep (component-lambdas component))
    (let ((closure (environment-closure (lambda-environment xep))))
      (when (and (functional-kind-eq xep external)
                 (not (leaf-dynamic-extent (functional-entry-fun xep)))
                 closure
                 (not (leaf-refs-not-escape-elsewhere-p xep nil)))
        (dolist (thing closure)
          (when (and (lambda-p thing)
                     (neq thing xep))
            (setf (environment-escapes-elsewhere-p (lambda-environment thing))
                  t))))))
  (values))

;;; Recursively look for otherwise inaccessible potentially
;;; stack-allocatable parts in the uses of LVAR. If there is one,
;;; bound LVAR's extent by DYNAMIC-EXTENT and return T. If LVAR
;;; already has a different dynamic extent set, we don't do anything.
(defun find-stack-allocatable-parts (lvar dynamic-extent &optional check-nesting)
  (declare (type lvar lvar)
           (type cdynamic-extent dynamic-extent))
  (when (lvar-dynamic-extent lvar)
    (aver (not (eq (lvar-dynamic-extent lvar) dynamic-extent)))
    (return-from find-stack-allocatable-parts nil))
  (let ((found-subpart-p nil))
    (do-uses (use lvar)
      (typecase use
        (cast
         (unless (cast-type-check use)
           (when (find-stack-allocatable-parts (cast-value use) dynamic-extent)
             (setq found-subpart-p t))))
        (combination
         (when (eq (combination-kind use) :known)
           (let* ((info (combination-fun-info use))
                  (stack-alloc-result (fun-info-stack-allocate-result info))
                  (result-arg
                    (let ((i (fun-info-result-arg info)))
                      (and i (nth i (combination-args use))))))
             (when (or (and result-arg
                            (find-stack-allocatable-parts result-arg dynamic-extent))
                       (and stack-alloc-result
                            (funcall stack-alloc-result use)))
               (setq found-subpart-p t)
               (dolist (arg (combination-args use))
                 (when (and arg (not (eq result-arg arg)))
                   (find-stack-allocatable-parts arg dynamic-extent t)))))))
        (ref
         (let ((leaf (ref-leaf use)))
           (typecase leaf
             (lambda-var
              ;; LET lambda var with no SETS.
              (when (and (functional-kind-eq (lambda-var-home leaf) let)
                         (not (lambda-var-sets leaf))
                         (lexenv-contains-lambda (lambda-var-home leaf)
                                                 (node-lexenv dynamic-extent))
                         (leaf-refs-not-escape-elsewhere-p leaf use))
                (when (find-stack-allocatable-parts (let-var-initial-value leaf)
                                                    dynamic-extent)
                  (setq found-subpart-p t))))
             (clambda
              (when (functional-kind-eq leaf external)
                (let* ((fun (functional-entry-fun leaf))
                       (enclose (functional-enclose fun))
                       (environment (get-lambda-environment leaf)))
                  (when (and (or (not check-nesting)
                                 ;; Allow (let ((x (lambda () v))) (let ((d x)) (dynamic-extent d)))
                                 ;; but not (let ((x (lambda () v))) (let ((d (list x))) (dynamic-extent d)))
                                 (lexenv-contains-lambda leaf (node-lexenv dynamic-extent)))
                             (environment-closure environment)
                             ;; At this point, DXIFY-DOWNWARD-FUNARGS
                             ;; and PROPAGATE-REF-DX should have
                             ;; marked the p-lvar-ends of FUN's refs.
                             (leaf-refs-not-escape-elsewhere-p leaf use)
                             (not (environment-escapes-elsewhere-p environment)))
                    (unless (enclose-dynamic-extent enclose)
                      (pushnew dynamic-extent
                               (enclose-derived-dynamic-extents enclose)))
                    (setf (leaf-dynamic-extent fun) t)
                    (setq found-subpart-p t))))))))))
    (when found-subpart-p
      (setf (lvar-dynamic-extent lvar) dynamic-extent)
      t)))

;;; Return the return node of FUN, creating it if it no longer exists.
(defun ensure-lambda-return (fun)
  (declare (type clambda fun))
  (or (lambda-return fun)
      (with-ir1-environment-from-node (lambda-bind fun)
        (let* ((result-ctran (make-ctran))
               (result-lvar (make-lvar))
               (return (make-return result-lvar fun))
               (block (ctran-starts-block result-ctran)))
          (link-node-to-previous-ctran return result-ctran)
          (setf (block-last block) return)
          (setf (lvar-dest result-lvar) return)
          (setf (lambda-return fun) return)
          (link-blocks block (component-tail (lambda-component fun)))
          return))))

;;; Revoke the tail-call status of CALL to FUN. This unlinks the call
;;; from FUN's bind node and routes it to a proper return node,
;;; creating it if necessary.
(defun revoke-tail-call (call fun)
  (declare (type combination call)
           (type clambda fun))
  (aver (node-tail-p call))
  (setf (node-tail-p call) nil)
  (unlink-blocks (node-block call)
                 (node-block (lambda-bind fun)))
  (let ((return (ensure-lambda-return (node-home-lambda call))))
    (link-blocks (node-block call) (node-block return))
    (add-lvar-use call (return-result return))))

;;; For each local call to FUN which shares a home lambda with
;;; ENCLOSE, insert a DYNAMIC-EXTENT node to bound the lifetime of
;;; ENCLOSE.
;;;
;;; If the call is a tail call, we have to revoke its tail-call
;;; status, since the dynamic extent cleanup action makes the call
;;; non-tail.
(defun insert-local-call-dynamic-extents (fun enclose)
  (let ((enclose-home (node-home-lambda enclose)))
    (dolist (ref (leaf-refs fun))
      (let* ((lvar (node-lvar ref))
             (dest (and lvar (lvar-dest lvar))))
        (when (and (eq (node-home-lambda ref) enclose-home)
                   (combination-p dest)
                   (eq (combination-kind dest) :local)
                   (eq lvar (combination-fun dest)))
          (pushnew (insert-dynamic-extent dest)
                   (enclose-derived-dynamic-extents enclose))
          (when (node-tail-p dest)
            (revoke-tail-call dest fun)))))))

;;; For each lambda in COMPONENT which has been determined to be
;;; eligible for stack allocation and does not have an explicit
;;; dynamic extent lifetime, annotate its derived lifetime. This is
;;; done by inserting appropriate dynamic extents around local calls
;;; in lambda's allocation environment and inheriting any lifetime
;;; annotations in the same environment from functions with XEPs which
;;; close over the lambda. Because a function might be invoked
;;; transitively by another local function that closes over it, we
;;; scan the component's lambdas and check if the lambda's local calls
;;; are in the same allocation environment of itself or any of the
;;; functions it closes over when annotating derived lifetimes around
;;; local calls.
(defun annotate-lambda-derived-extents (component)
  (dolist (fun (component-lambdas component))
    (let ((enclose (functional-enclose fun)))
      (when enclose
        (when (leaf-dynamic-extent fun)
          (unless (enclose-dynamic-extent enclose)
            (insert-local-call-dynamic-extents fun enclose)))
        (dolist (thing (environment-closure (lambda-environment fun)))
          (when (and (lambda-p thing)
                     (leaf-dynamic-extent (functional-entry-fun thing)))
            (let ((captured-enclose (xep-enclose thing)))
              (unless (enclose-dynamic-extent captured-enclose)
                (insert-local-call-dynamic-extents fun captured-enclose))))))))
  (dolist (xep (component-lambdas component))
    (when (and (functional-kind-eq xep external)
               (leaf-dynamic-extent (functional-entry-fun xep)))
      (let* ((enclose (xep-enclose xep))
             (dynamic-extent (enclose-dynamic-extent enclose))
             (enclose-home (node-home-lambda enclose)))
        (dolist (thing (environment-closure (lambda-environment xep)))
          (when (and (lambda-p thing)
                     (leaf-dynamic-extent (functional-entry-fun thing)))
            (let ((captured-enclose (xep-enclose thing)))
              (when (and (not (enclose-dynamic-extent captured-enclose))
                         (eq (node-home-lambda captured-enclose) enclose-home))
                (cond (dynamic-extent
                       (pushnew dynamic-extent (enclose-derived-dynamic-extents captured-enclose)))
                      (t
                       (setf (enclose-derived-dynamic-extents captured-enclose)
                             (union (enclose-derived-dynamic-extents enclose)
                                    (enclose-derived-dynamic-extents captured-enclose)))))))))))))

;;; Determine which values and closures in COMPONENT may be stack
;;; allocated. We do so by starting a recursive walk from the values
;;; and closures explicitly declared dynamic extent and transitively
;;; marking the otherwise-inaccessible parts of these values as
;;; potentially stack allocatable. If a dynamic extent is in fact
;;; associated with a stack allocatable thing, note that fact by
;;; setting the dynamic extent's info.
;;;
;;; We do this during environment analysis once all major changes to
;;; the dataflow in IR1 have been done and it becomes whether a
;;; combination can actually stack allocate its value. In particular,
;;; a value must share the same environment as its dynamic extent in
;;; order for stack allocation to make sense.
(defun find-lvar-dynamic-extents (component)
  (declare (type component component))
  (do-blocks (block component)
    (do-nodes (node nil block)
      (when (and (combination-p node)
                 (memq (basic-combination-kind node)
                       '(:full :unknown-keys :known)))
        (dxify-downward-funargs node))))

  (analyze-escaping-closure-environments component)

  (dolist (lambda (component-lambdas component))
    (dolist (dynamic-extent (lambda-dynamic-extents lambda))
      (let ((environment (node-environment dynamic-extent)))
        (dolist (lvar (dynamic-extent-values dynamic-extent))
          (aver (eq dynamic-extent (lvar-dynamic-extent lvar)))
          (setf (lvar-dynamic-extent lvar) nil)
          (when (and (do-uses (use lvar t)
                       (unless (eq environment (node-environment use))
                         (return nil)))
                     (find-stack-allocatable-parts lvar dynamic-extent))
            (setf (dynamic-extent-info dynamic-extent) (make-lvar)))))))

  (annotate-lambda-derived-extents component)

  (dolist (xep (component-lambdas component))
    (when (and (functional-kind-eq xep external)
               (leaf-dynamic-extent (functional-entry-fun xep))
               ;; We need to have a closure environment to
               ;; stack allocate.
               (environment-closure (get-lambda-environment xep)))
      (let* ((enclose (xep-enclose xep))
             (dynamic-extent (enclose-dynamic-extent enclose))
             (derived-dynamic-extents
               (enclose-derived-dynamic-extents enclose)))
        (cond (dynamic-extent
               (aver (null derived-dynamic-extents))
               (unless (dynamic-extent-info dynamic-extent)
                 (setf (dynamic-extent-info dynamic-extent) (make-lvar))))
              (derived-dynamic-extents
               (aver (null (enclose-dynamic-extent enclose)))
               (let ((lvar (make-lvar)))
                 (dolist (dynamic-extent derived-dynamic-extents)
                   (setf (dynamic-extent-info dynamic-extent) lvar))))))))
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
                       (not (and (combination-p use)
                                 (lvar-fun-is (combination-fun use) '(break))))
                       (basic-combination-p use)
                       (or (not (eq (node-derived-type use) *empty-type*))
                           ;; This prevents external entry points from
                           ;; showing up in the backtrace: we always
                           ;; want tail calls inside XEPs to the
                           ;; functions they are the entry point for.
                           (eq (basic-combination-kind use) :local)))
              (setf (node-tail-p use) t)))))))
  ;; Tail call non-returning functions if no debugging is wanted.
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
