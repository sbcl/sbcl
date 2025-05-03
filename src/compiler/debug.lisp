;;;; This file contains utilities for debugging the compiler --
;;;; currently only stuff for checking the consistency of the IR1.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; A definite inconsistency has been detected. Signal an error.
(defun barf (string &rest args)
  (declare (string string))
  (apply #'bug string args))

;;; *SEEN-BLOCKS* is a hashtable with true values for all blocks which
;;; appear in the DFO for one of the specified components.
;;;
;;; *SEEN-FUNS* is similar, but records all the lambdas we
;;; reached by recursing on top level functions.
;;; FIXME: Is it really only LAMBDAs, not e.g. FUNCTIONALs? Then
;;; shouldn't it be *SEEN-LAMBDAS*?
(defvar *seen-blocks*)
(defvar *seen-funs*)

;;; Barf if NODE is in a block which wasn't reached during the graph
;;; walk.
(defun check-node-reached (node)
  (declare (type node node))
  (unless (gethash (ctran-block (node-prev node)) *seen-blocks*)
    (barf "~S was not reached." node))
  (values))

;;; Check everything that we can think of for consistency. When a
;;; definite inconsistency is detected, we BARF.
;;; Our argument is a list of components, but
;;; we also look at the *IR1-NAMESPACE*.
;;;
;;; First we do a pre-pass which finds all the CBLOCKs and CLAMBDAs,
;;; testing that they are linked together properly and entering them
;;; in hashtables. Next, we iterate over the blocks again, looking at
;;; the actual code and control flow. Finally, we scan the global leaf
;;; hashtables, looking for lossage.
(defun check-ir1-consistency (components &aux (ns *ir1-namespace*))
  (declare (type list components))
  (let ((*seen-blocks* (make-hash-table :test 'eq))
        (*seen-funs* (make-hash-table :test 'eq)))
    (unwind-protect
         (progn
           (dolist (c components)
             (let* ((head (component-head c))
                    (tail (component-tail c)))
               (unless (and (null (block-pred head))
                            (null (block-succ tail)))
                 (barf "~S is malformed." c))

               (do ((prev nil block)
                    (block head (block-next block)))
                   ((null block)
                    (unless (eq prev tail)
                      (barf "wrong TAIL for DFO, ~S in ~S" prev c)))
                 (setf (gethash block *seen-blocks*) t)
                 (unless (eq (block-prev block) prev)
                   (barf "bad PREV for ~S, should be ~S" block prev))
                 (unless (or (eq block tail)
                             (eq (block-component block) c))
                   (barf "~S is not in ~S." block c)))
               #+(or)
               (let ((component-outer-loop (component-outer-loop c)))
                 (when (or (loop-blocks component-outer-loop)
                           (loop-inferiors component-outer-loop))
                   (do-blocks (block c :both)
                     (setf (block-flag block) nil))
                   (check-loop-consistency component-outer-loop nil)
                   (do-blocks (block c :both)
                     (unless (block-flag block)
                       (barf "~S was not in any loop." block)))))))
           (check-fun-consistency components)

           (dolist (c components)
             (do ((block (block-next (component-head c)) (block-next block)))
                 ((null (block-next block)))
               (check-block-consistency block)))

           (maphash (lambda (k v)
                      (declare (ignore k))
                      (unless (or (eq v :deprecated)
                                  (typep v '(cons (eql macro)))
                                  (heap-alien-info-p v)
                                  (constant-p v)
                                  (and (global-var-p v)
                                       (member (global-var-kind v)
                                               '(:global :special :unknown))))
                        (barf "strange FREE-VARS entry: ~S" v))
                      (when (leaf-p v)
                        (dolist (n (leaf-refs v))
                          (check-node-reached n)))
                      (when (basic-var-p v)
                        (dolist (n (basic-var-sets v))
                          (check-node-reached n))))
                    (free-vars ns))

           (maphash (lambda (k v)
                      (declare (ignore k))
                      (unless (constant-p v)
                        (barf "strange CONSTANTS entry: ~S" v))
                      (dolist (n (leaf-refs v))
                        (check-node-reached n)))
                    (eql-constants ns))

           (maphash (lambda (k v)
                      (declare (ignore k))
                      (unless (or (functional-p v)
                                  (and (global-var-p v)
                                       (eq (global-var-kind v) :global-function)))
                        (barf "strange FREE-FUNS entry: ~S" v))
                      (dolist (n (leaf-refs v))
                        (check-node-reached n)))
                    (free-funs ns)))
      (clrhash *seen-blocks*)
      (clrhash *seen-funs*))
    (values)))

;;;; function consistency checking

(defun observe-functional (x)
  (declare (type functional x))
  (when (gethash x *seen-funs*)
    (barf "~S was seen more than once." x))
  (unless (functional-kind-eq x deleted)
    (setf (gethash x *seen-funs*) t)))

;;; Check that the specified function has been seen.
(defun check-fun-reached (fun where)
  (declare (type functional fun))
  (unless (gethash fun *seen-funs*)
    (barf "unseen function ~S in ~S" fun where)))

;;; In a CLAMBDA, check that the associated nodes are in seen blocks.
;;; In an OPTIONAL-DISPATCH, check that the entry points were seen. If
;;; the function is deleted, ignore it.
(defun check-fun-stuff (functional)
  (functional-kind-case functional
    (external
     (let ((fun (functional-entry-fun functional)))
       (check-fun-reached fun functional)
       (unless (functional-kind-eq fun nil)
         (barf "The function for XEP ~S has kind." functional))
       (unless (eq (functional-entry-fun fun) functional)
         (barf "bad back-pointer in function for XEP ~S" functional))))
    ((let mv-let assignment)            ; i.e. SOMEWHAT-LETLIKE-P
     (check-fun-reached (lambda-home functional) functional)
     (when (functional-entry-fun functional)
       (barf "The LET ~S has entry function." functional))
     (unless (member functional (lambda-lets (lambda-home functional)))
       (barf "The LET ~S is not in LETs for HOME." functional))
     (unless (functional-kind-eq functional assignment)
       (when (rest (leaf-refs functional))
         (barf "The LET ~S has multiple references." functional)))
     (when (lambda-lets functional)
       (barf "LETs in a LET: ~S" functional)))
    (optional
     (when (functional-entry-fun functional)
       (barf ":OPTIONAL ~S has an ENTRY-FUN." functional))
     (let ((ef (lambda-optional-dispatch functional)))
       (check-fun-reached ef functional)
       (unless (or (member functional (optional-dispatch-entry-points ef)
                           :key (lambda (ep)
                                  (when (promise-ready-p ep)
                                    (force ep))))
                   (eq functional (optional-dispatch-more-entry ef))
                   (eq functional (optional-dispatch-main-entry ef)))
         (barf ":OPTIONAL ~S is not an e-p for its OPTIONAL-DISPATCH ~S."
               functional ef))))
    (toplevel
     (unless (eq (functional-entry-fun functional) functional)
       (barf "The ENTRY-FUN in ~S isn't a self-pointer." functional)))
    ((nil escape cleanup)
     (let ((ef (functional-entry-fun functional)))
       (when ef
         (check-fun-reached ef functional)
         (unless (functional-kind-eq ef external)
           (barf "The ENTRY-FUN in ~S isn't an XEP: ~S." functional ef)))))
    (deleted
     (return-from check-fun-stuff)))

  (when (functional-kind-eq functional nil optional external toplevel escape cleanup)
    (when (lambda-p functional)
      (dolist (fun (lambda-lets functional))
        (unless (eq (lambda-home fun) functional)
          (barf "The home in ~S is not ~S." fun functional))
        (check-fun-reached fun functional))
      (unless (eq (lambda-home functional) functional)
        (barf "home not self-pointer in ~S" functional))))

  (etypecase functional
    (clambda
     (when (lambda-bind functional)
       (check-node-reached (lambda-bind functional)))
     (when (lambda-return functional)
       (check-node-reached (lambda-return functional)))

     (dolist (var (lambda-vars functional))
       (dolist (ref (leaf-refs var))
         (check-node-reached ref))
       (dolist (set (basic-var-sets var))
         (check-node-reached set))
       (unless (eq (lambda-var-home var) functional)
         (barf "HOME in ~S should be ~S." var functional))))
    (optional-dispatch
     (dolist (ep (optional-dispatch-entry-points functional))
       (when (promise-ready-p ep)
         (check-fun-reached (force ep) functional)))
     (let ((more (optional-dispatch-more-entry functional)))
       (when more (check-fun-reached more functional)))
     (check-fun-reached (optional-dispatch-main-entry functional)
                        functional))))

(defun check-fun-consistency (components)
  (dolist (c components)
    (dolist (new-fun (component-new-functionals c))
      (observe-functional new-fun))
    (dolist (fun (component-lambdas c))
      (when (functional-kind-eq fun external)
        (let ((ef (functional-entry-fun fun)))
          (when (optional-dispatch-p ef)
            (observe-functional ef))))
      (observe-functional fun)
      (dolist (let (lambda-lets fun))
        (observe-functional let))))

  (dolist (c components)
    (dolist (new-fun (component-new-functionals c))
      (check-fun-stuff new-fun))
    (dolist (fun (component-lambdas c))
      (when (functional-kind-eq fun deleted)
        (barf "deleted lambda ~S in Lambdas for ~S" fun c))
      (check-fun-stuff fun)
      (dolist (let (lambda-lets fun))
        (check-fun-stuff let)))))

;;;; loop consistency checking

;;; Descend through the loop nesting and check that the tree is well-formed
;;; and that all blocks in the loops are known blocks. We also mark each block
;;; that we see so that we can do a check later to detect blocks that weren't
;;; in any loop.
(defun check-loop-consistency (loop superior)
  (declare (type cloop loop) (type (or cloop null) superior))
  (unless (eq (loop-superior loop) superior)
    (barf "wrong superior in ~S, should be ~S" loop superior))
  (when (and superior
             (/= (loop-depth loop) (1+ (loop-depth superior))))
    (barf "wrong depth in ~S" loop))

  (dolist (tail (loop-tail loop))
    (check-loop-block tail loop))
  (dolist (exit (loop-exits loop))
    (check-loop-block exit loop))
  (check-loop-block (loop-head loop) loop)
  (unless (eq (block-loop (loop-head loop)) loop)
    (barf "The head of ~S is not directly in the loop." loop))

  (do ((block (loop-blocks loop) (block-loop-next block)))
      ((null block))
    (setf (block-flag block) t)
    (unless (gethash block *seen-blocks*)
      (barf "unseen block ~S in Blocks for ~S" block loop))
    (unless (eq (block-loop block) loop)
      (barf "wrong loop in ~S, should be ~S" block loop)))

  (dolist (inferior (loop-inferiors loop))
    (check-loop-consistency inferior loop))
  (values))

;;; Check that Block is either in Loop or an inferior.
(defun check-loop-block (block loop)
  (declare (type cblock block) (type cloop loop))
  (unless (gethash block *seen-blocks*)
    (barf "unseen block ~S in loop info for ~S" block loop))
  (labels ((walk (l)
             (if (eq (block-loop block) l)
                 t
                 (dolist (inferior (loop-inferiors l) nil)
                   (when (walk inferior) (return t))))))
    (unless (walk loop)
      (barf "~S is in loop info for ~S but not in the loop." block loop)))
  (values))

;;; Check a block for consistency at the general flow-graph level, and
;;; call CHECK-NODE-CONSISTENCY on each node to locally check for
;;; semantic consistency.
(defun check-block-consistency (block)
  (declare (type cblock block))
  (dolist (pred (block-pred block))
    (unless (gethash pred *seen-blocks*)
      (barf "unseen predecessor ~S in ~S" pred block))
    (unless (member block (block-succ pred))
      (barf "bad predecessor link ~S in ~S" pred block)))

  (let* ((fun (block-home-lambda block))
         (fun-deleted (functional-kind-eq fun deleted))
         (this-ctran (block-start block))
         (last (block-last block)))
    (unless fun-deleted
      (check-fun-reached fun block))
    (when (not this-ctran)
      (barf "~S has no START." block))
    (when (not last)
      (barf "~S has no LAST." block))
    (unless (eq (ctran-kind this-ctran) :block-start)
      (barf "The START of ~S has the wrong kind." block))

    (when (ctran-use this-ctran)
      (barf "The ctran ~S is used." this-ctran))

    (when (node-next last)
      (barf "Last node ~S of ~S has next ctran." last block))

    (loop
      (unless (eq (ctran-block this-ctran) block)
        (barf "BLOCK of ~S should be ~S." this-ctran block))

      (let ((node (ctran-next this-ctran)))
        (unless (node-p node)
          (barf "~S has strange NEXT." this-ctran))
        (unless (eq (node-prev node) this-ctran)
          (barf "PREV in ~S should be ~S." node this-ctran))

        (when (valued-node-p node)
          (binding* ((lvar (node-lvar node) :exit-if-null))
            (unless (memq node (find-uses lvar))
              (barf "~S is not used by its LVAR ~S." node lvar))
            (when (singleton-p (lvar-uses lvar))
              (barf "~S has exactly 1 use, but LVAR-USES is a list."
                    lvar))
            (unless (lvar-dest lvar)
              (barf "~S does not have dest." lvar))))

        (check-node-reached node)
        (unless fun-deleted
          (check-node-consistency node))

        (let ((next (node-next node)))
          (when (and (not next) (not (eq node last)))
            (barf "~S has no NEXT." node))
          (when (eq node last) (return))
          (unless (eq (ctran-kind next) :inside-block)
            (barf "The interior ctran ~S in ~S has the wrong kind."
                  next
                  block))
          (unless (ctran-next next)
            (barf "~S has no NEXT." next))
          (unless (eq (ctran-use next) node)
            (barf "USE in ~S should be ~S." next node))
          (setq this-ctran next))))

    (check-block-successors block))
  (values))

;;; Check that BLOCK is properly terminated. Each successor must be
;;; accounted for by the type of the last node.
(defun check-block-successors (block)
  (declare (type cblock block))
  (let ((last (block-last block))
        (succ (block-succ block)))

    (let* ((comp (block-component block)))
      (dolist (b succ)
        (unless (gethash b *seen-blocks*)
          (barf "unseen successor ~S in ~S" b block))
        (unless (member block (block-pred b))
          (barf "bad successor link ~S in ~S" b block))
        (unless (eq (block-component b) comp)
          (barf "The successor ~S in ~S is in a different component."
                b
                block))))

    (typecase last
      (cif
       (unless (proper-list-of-length-p succ 1 2)
         (barf "~S ends in an IF, but doesn't have one or two successors."
               block))
       (unless (member (if-consequent last) succ)
         (barf "The CONSEQUENT for ~S isn't in SUCC for ~S." last block))
       (unless (member (if-alternative last) succ)
         (barf "The ALTERNATIVE for ~S isn't in SUCC for ~S." last block)))
      (creturn
       (unless (if (functional-kind-eq (return-lambda last) deleted)
                   (null succ)
                   (and (= (length succ) 1)
                        (eq (first succ)
                            (component-tail (block-component block)))))
         (barf "strange successors for RETURN in ~S" block)))
      (exit
       (unless (proper-list-of-length-p succ 0 1)
         (barf "EXIT node with strange number of successors: ~S" last)))
      (t
       (unless (or (= (length succ) 1) (node-tail-p last)
                   (and (block-delete-p block) (null succ)))
         (barf "~S ends in normal node, but doesn't have one successor."
               block)))))
  (values))

;;;; node consistency checking

;;; Check that the DEST for LVAR is the specified NODE. We also mark
;;; the block LVAR is in as SEEN.
(defun check-dest (lvar node)
  (declare (type lvar lvar) (type node node))
  (do-uses (use lvar)
    (unless (gethash (node-block use) *seen-blocks*)
      (barf "Node ~S using ~S is in an unknown block." use lvar)))
  (unless (eq (lvar-dest lvar) node)
    (barf "DEST for ~S should be ~S." lvar node))
  (values))

;;; This function deals with checking for consistency of the
;;; type-dependent information in a node.
(defun check-node-consistency (node)
  (declare (type node node))
  (etypecase node
    (ref
     (let ((leaf (ref-leaf node)))
       (when (functional-p leaf)
         (if (functional-kind-eq leaf toplevel-xep)
             (unless (component-toplevelish-p (block-component (node-block node)))
               (barf ":TOPLEVEL-XEP ref in non-top-level component: ~S"
                     node))
             (check-fun-reached leaf node)))))
    (basic-combination
     (check-dest (basic-combination-fun node) node)
     (when (and (mv-combination-p node)
                (eq (basic-combination-kind node) :local))
       (let ((fun-lvar (basic-combination-fun node)))
         (unless (ref-p (lvar-uses fun-lvar))
           (barf "function in a local mv-combination is not a LEAF: ~S" node))
         (let ((fun (ref-leaf (lvar-use fun-lvar))))
           (unless (lambda-p fun)
             (barf "function ~S in a local mv-combination ~S is not local"
                   fun node))
           (unless (functional-kind-eq fun mv-let)
             (barf "function ~S in a local mv-combination ~S is not of kind :MV-LET"
                   fun node)))))
     (dolist (arg (basic-combination-args node))
       (cond
         (arg (check-dest arg node))
         ((not (and (eq (basic-combination-kind node) :local)
                    (combination-p node)))
          (barf "flushed arg not in local call: ~S" node))
         (t
          (locally
              ;; KLUDGE: In sbcl-0.6.11.37, the compiler doesn't like
              ;; (DECLARE (TYPE INDEX POS)) after the inline expansion of
              ;; POSITION. It compiles it correctly, but it issues a type
              ;; mismatch warning because it can't eliminate the
              ;; possibility that control will flow through the
              ;; NIL-returning branch. So we punt here. -- WHN 2001-04-15
              (declare (notinline position))
            (let ((fun (ref-leaf (lvar-use
                                  (basic-combination-fun node))))
                  (pos (position arg (basic-combination-args node))))
              (declare (type index pos))
              (when (leaf-refs (elt (lambda-vars fun) pos))
                (barf "flushed arg for referenced var in ~S" node)))))))
     (let* ((lvar (node-lvar node))
            (dest (and lvar (lvar-dest lvar))))
       (when (and (return-p dest)
                  (eq (basic-combination-kind node) :local)
                  (not (eq (lambda-tail-set (combination-lambda node))
                           (lambda-tail-set (return-lambda dest)))))
         (barf "tail local call to function with different tail set:~%  ~S"
               node))))
    (cif
     (check-dest (if-test node) node)
     (unless (eq (block-last (node-block node)) node)
       (barf "IF not at block end: ~S" node)))
    (jump-table
     (check-dest (jump-table-index node) node)
     (unless (eq (block-last (node-block node)) node)
       (barf "JUMP-TABLE not at block end: ~S" node)))
    (cset
     (check-dest (set-value node) node))
    (cast
     (check-dest (cast-value node) node))
    (bind
     (check-fun-reached (bind-lambda node) node))
    (creturn
     (check-fun-reached (return-lambda node) node)
     (check-dest (return-result node) node)
     (unless (eq (block-last (node-block node)) node)
       (barf "RETURN not at block end: ~S" node)))
    (entry
     (unless (member node (lambda-entries (node-home-lambda node)))
       (barf "~S is not in ENTRIES for its home LAMBDA." node))
     (dolist (exit (entry-exits node))
       (unless (node-deleted exit)
         (check-node-reached node))))
    (enclose
     (dolist (fun (enclose-funs node))
       (let ((enclose (functional-enclose fun)))
         (unless (eq node enclose)
           (barf "~S is not the ENCLOSE for its FUN ~S." node enclose)))))
    (cdynamic-extent)
    (exit
     (let ((entry (exit-entry node))
           (value (exit-value node)))
       (cond (entry
              (check-node-reached entry)
              (unless (member node (entry-exits entry))
                (barf "~S is not in its ENTRY's EXITS." node))
              (when value
                (check-dest value node)))
             (t
              (when value
                (barf "~S has VALUE but no ENTRY." node)))))))

  (values))

;;;; IR2 consistency checking

;;; Check for some kind of consistency in some REFs linked together by
;;; TN-REF-ACROSS. VOP is the VOP that the references are in. WRITE-P
;;; is the value of WRITE-P that should be present. COUNT is the
;;; minimum number of operands expected. If MORE-P is true, then any
;;; larger number will also be accepted. WHAT is a string describing
;;; the kind of operand in error messages.
(defun check-tn-refs (refs vop write-p count more-p what)
  (let ((vop-refs (vop-refs vop)))
    (do ((ref refs (tn-ref-across ref))
         (num 0 (1+ num)))
        ((null ref)
         (when (< num count)
           (barf "There should be at least ~W ~A in ~S, but there are only ~W."
                 count what vop num))
         (when (and (not more-p) (> num count))
           (barf "There should be ~W ~A in ~S, but are ~W."
                 count what vop num)))
      (unless (eq (tn-ref-vop ref) vop)
        (barf "VOP is ~S isn't ~S." ref vop))
      (unless (eq (tn-ref-write-p ref) write-p)
        (barf "The WRITE-P in ~S isn't ~S." vop write-p))
      (unless (find-in #'tn-ref-next-ref ref vop-refs)
        (barf "~S not found in REFS for ~S" ref vop))
      (unless (or (eq (tn-kind (tn-ref-tn ref)) :unused)
                  (find-in #'tn-ref-next ref
                           (if (tn-ref-write-p ref)
                               (tn-writes (tn-ref-tn ref))
                               (tn-reads (tn-ref-tn ref)))))
        (barf "~S not found in reads/writes for its TN" ref))

      (let ((target (tn-ref-target ref)))
        (when target
          (unless (eq (tn-ref-write-p target) (not (tn-ref-write-p ref)))
            (barf "The target for ~S isn't complementary WRITE-P." ref))
          (unless (find-in #'tn-ref-next-ref target vop-refs)
            (barf "The target for ~S isn't in REFS for ~S." ref vop)))))))

;;; Verify the sanity of the VOP-REFS slot in VOP. This involves checking
;;; that each referenced TN appears as an argument, result or temp, and also
;;; basic checks for the plausibility of the specified ordering of the refs.
(defun check-vop-refs (vop)
  (declare (type vop vop))
  (do ((ref (vop-refs vop) (tn-ref-next-ref ref)))
      ((null ref))
    (cond
     ((find-in #'tn-ref-across ref (vop-args vop)))
     ((find-in #'tn-ref-across ref (vop-results vop)))
     ((not (eq (tn-ref-vop ref) vop))
      (barf "VOP in ~S isn't ~S." ref vop))
     ((find-in #'tn-ref-across ref (vop-temps vop)))
     ((tn-ref-write-p ref)
      (barf "stray ref that isn't a READ: ~S" ref))
     (t
      (let* ((tn (tn-ref-tn ref))
             (temp (find-in #'tn-ref-across tn (vop-temps vop)
                            :key #'tn-ref-tn)))
        (unless temp
          (barf "stray ref with no corresponding temp write: ~S" ref))
        (unless (find-in #'tn-ref-next-ref temp (tn-ref-next-ref ref))
          (barf "Read is after write for temp ~S in refs of ~S."
                tn vop))))))
  (values))

;;; Check the basic sanity of the VOP linkage, then call some other
;;; functions to check on the TN-REFS. We grab some info out of the
;;; VOP-INFO to tell us what to expect.
;;;
;;; [### Check that operand type restrictions are met?]
(defun check-ir2-block-consistency (2block)
  (declare (type ir2-block 2block))
  (do ((vop (ir2-block-start-vop 2block)
            (vop-next vop))
       (prev nil vop))
      ((null vop)
       (unless (eq prev (ir2-block-last-vop 2block))
         (barf "The last VOP in ~S should be ~S." 2block prev)))
    (unless (eq (vop-prev vop) prev)
      (barf "PREV in ~S should be ~S." vop prev))

    (unless (eq (vop-block vop) 2block)
      (barf "BLOCK in ~S should be ~S." vop 2block))

    (check-vop-refs vop)

    (let* ((info (vop-info vop))
           (atypes (template-arg-types info))
           (rtypes (template-result-types info)))
      (check-tn-refs (vop-args vop) vop nil
                     (count-if-not (lambda (x)
                                     (and (consp x)
                                          (eq (car x) :constant)))
                                   atypes)
                     (template-more-args-type info) "args")
      (check-tn-refs (vop-results vop) vop t
                     (if (template-conditional-p info) 0 (length rtypes))
                     (template-more-results-type info) "results")
      (check-tn-refs (vop-temps vop) vop t 0 t "temps")
      (unless (or (= (length (vop-codegen-info vop))
                     (+ (template-info-arg-count info)
                        (if (vop-info-gc-barrier info)
                            1
                            0)))
                  ;; Allow these 2 allocator vops to take an undeclared info arg
                  (member (vop-info-name info) '(sb-vm::fixed-alloc sb-vm::var-alloc))
                  ;; FIXME: The current representation for conditional
                  ;; flag setting VOPs makes it difficult to check
                  ;; agreement between declared and actual info args.
                  (typep (template-result-types info) '(cons (eql :conditional))))
        (barf "wrong number of codegen info args in ~S" vop))))
  (values))

;;; Check stuff about the IR2 representation of COMPONENT. This assumes the
;;; sanity of the basic flow graph.
;;;
;;; [### Also grovel global TN data structures?  Assume pack not
;;; done yet?  Have separate CHECK-TN-CONSISTENCY for pre-pack and
;;; CHECK-PACK-CONSISTENCY for post-pack?]
(defun check-ir2-consistency (component)
  (declare (type component component))
  (do-ir2-blocks (block component)
    (check-ir2-block-consistency block))
  (values))

;;;; lifetime analysis checking

;;; Dump some info about how many TNs there, and what the conflicts data
;;; structures are like.
(defun pre-pack-tn-stats (component &optional (stream *standard-output*))
  (declare (type component component))
  (let ((wired 0)
        (global 0)
        (local 0)
        (confs 0)
        (unused 0)
        (const 0)
        (temps 0)
        (environment 0)
        (comp 0))
    (do-packed-tns (tn component)
      (let ((reads (tn-reads tn))
            (writes (tn-writes tn)))
        (when (and reads writes
                   (not (tn-ref-next reads)) (not (tn-ref-next writes))
                   (eq (tn-ref-vop reads) (tn-ref-vop writes)))
          (incf temps)))
      (when (tn-offset tn)
        (incf wired))
      (unless (or (tn-reads tn) (tn-writes tn))
        (incf unused))
      (cond ((eq (tn-kind tn) :component)
             (incf comp))
            ((tn-global-conflicts tn)
             (case (tn-kind tn)
               ((:environment :debug-environment) (incf environment))
               (t (incf global)))
             (do ((conf (tn-global-conflicts tn)
                        (global-conflicts-next-tnwise conf)))
                 ((null conf))
               (incf confs)))
            (t
             (incf local))))

    (do ((tn (ir2-component-constant-tns (component-info component))
             (tn-next tn)))
        ((null tn))
      (incf const))

    (format stream
     "~%TNs: ~W local, ~W temps, ~W constant, ~W env, ~W comp, ~W global.~@
       Wired: ~W, Unused: ~W. ~W block~:P, ~W global conflict~:P.~%"
       local temps const environment comp global wired unused
       (ir2-block-count component)
       confs))
  (values))

;;; If the entry in Local-TNs for TN in BLOCK is :MORE, then do some checks
;;; for the validity of the usage.
(defun check-more-tn-entry (tn block)
  (let* ((vop (ir2-block-start-vop block))
         (info (vop-info vop)))
    (macrolet ((frob (more-p ops)
                 `(and (,more-p info)
                       (find-in #'tn-ref-across tn (,ops vop)
                                :key #'tn-ref-tn))))
      (unless (and (eq vop (ir2-block-last-vop block))
                   (or (frob template-more-args-type vop-args)
                       (frob template-more-results-type vop-results)))
        (barf "strange :MORE LTN entry for ~S in ~S" tn block))))
  (values))

(defun check-tn-conflicts (component)
  (do-packed-tns (tn component)
    (unless (or (not (eq (tn-kind tn) :normal))
                (tn-reads tn)
                (tn-writes tn))
      (barf "no references to ~S" tn))

    (unless (tn-sc tn) (barf "~S has no SC." tn))

    (let ((conf (tn-global-conflicts tn))
          (kind (tn-kind tn)))
      (cond
       ((eq kind :component)
        (unless (member tn (ir2-component-component-tns
                            (component-info component)))
          (barf "~S not in COMPONENT-TNs for ~S" tn component)))
       (conf
        (do ((conf conf (global-conflicts-next-tnwise conf))
             (prev nil conf))
            ((null conf))
          (unless (eq (global-conflicts-tn conf) tn)
            (barf "TN in ~S should be ~S." conf tn))

          (unless (eq (global-conflicts-kind conf) :live)
            (let* ((block (global-conflicts-block conf))
                   (ltn (svref (ir2-block-local-tns block)
                               (global-conflicts-number conf))))
              (cond ((eq ltn tn))
                    ((eq ltn :more) (check-more-tn-entry tn block))
                    (t
                     (barf "~S wrong in LTN map for ~S" conf tn)))))

          (when prev
            (unless (> (ir2-block-number (global-conflicts-block conf))
                       (ir2-block-number (global-conflicts-block prev)))
              (barf "~s and ~s out of order" prev conf)))))
       ((member (tn-kind tn) '(:constant :specified-save)))
       (t
        (let ((local (tn-local tn)))
          (unless local
            (barf "~S has no global conflicts, but isn't local either." tn))
          (unless (eq (svref (ir2-block-local-tns local)
                             (tn-local-number tn))
                      tn)
            (barf "~S wrong in LTN map" tn))
          (do ((ref (tn-reads tn) (tn-ref-next ref)))
              ((null ref))
            (unless (eq (vop-block (tn-ref-vop ref)) local)
              (barf "~S has references in blocks other than its LOCAL block."
                    tn)))
          (do ((ref (tn-writes tn) (tn-ref-next ref)))
              ((null ref))
            (unless (eq (vop-block (tn-ref-vop ref)) local)
              (barf "~S has references in blocks other than its LOCAL block."
                    tn))))))))
  (values))

(defun check-block-conflicts (component)
  (do-ir2-blocks (block component)
    (do ((conf (ir2-block-global-tns block)
               (global-conflicts-next-blockwise conf)))
        ((null conf))
      (unless (find-in #'global-conflicts-next-tnwise
                       conf
                       (tn-global-conflicts
                        (global-conflicts-tn conf)))
        (barf "~S missing from global conflicts of its TN" conf)))

    (let ((map (ir2-block-local-tns block)))
      (dotimes (i (ir2-block-local-tn-count block))
        (let ((tn (svref map i)))
          (unless (or (eq tn :more)
                      (null tn)
                      (tn-global-conflicts tn)
                      (eq (tn-local tn) block))
            (barf "strange TN ~S in LTN map for ~S" tn block)))))))

;;; All TNs live at the beginning of an environment must be passing
;;; locations associated with that environment. We make an exception
;;; for wired TNs in XEP functions, since we randomly reference wired
;;; TNs to access the full call passing locations.
(defun check-environment-lifetimes (component)
  (dolist (fun (component-lambdas component))
    (let* ((env (lambda-environment fun))
           (2env (environment-info env))
           (vars (lambda-vars fun))
           (closure (ir2-environment-closure 2env))
           (pc (ir2-environment-return-pc-pass 2env))
           (fp (ir2-environment-old-fp 2env))
           (2block (block-info (lambda-block (environment-lambda env)))))
      (do ((conf (ir2-block-global-tns 2block)
                 (global-conflicts-next-blockwise conf)))
          ((null conf))
        (let ((tn (global-conflicts-tn conf)))
          (unless (or (eq (global-conflicts-kind conf) :write)
                      (eq tn pc)
                      (eq tn fp)
                      (and (xep-p fun) (tn-offset tn))
                      (member (tn-kind tn) '(:environment :debug-environment))
                      (member tn vars :key #'leaf-info)
                      (member tn closure :key #'cdr))
            (barf "strange TN live at head of ~S: ~S" env tn))))))
  (values))

;;; Check for some basic sanity in the TN conflict data structures,
;;; and also check that no TNs are unexpectedly live at environment
;;; entry.
(defun check-life-consistency (component)
  (check-tn-conflicts component)
  (check-block-conflicts component)
  (check-environment-lifetimes component))

;;;; pack consistency checking

(defun check-pack-consistency (component)
  (flet ((check (scs ops)
           (do ((scs scs (cdr scs))
                (op ops (tn-ref-across op)))
               ((null scs))
             (let ((tn (tn-ref-tn op))
                   (load-tn (tn-ref-load-tn op)))
               (unless (or (eq (tn-kind tn) :unused)
                           (typep (svref (car scs)
                                         (sc-number
                                          (tn-sc
                                           (or load-tn tn))))
                                  '(or (eql t) function)))
                 (barf "operand restriction not satisfied: ~S" op))))))
    (do-ir2-blocks (block component)
      (do ((vop (ir2-block-last-vop block) (vop-prev vop)))
          ((null vop))
        (let ((info (vop-info vop)))
          (check (vop-info-result-load-scs info) (vop-results vop))
          (check (vop-info-arg-load-scs info) (vop-args vop))))))
  (values))

;;;; data structure dumping routines

;;; IR2 blocks don't get a convenient number until LIFETIME-ANALYZE which is
;;; too late when trying to talk about an ir2 block during IR2-OPTIMIZE.
;;; So instead of accessing them by the block-number, use an opaque ID.
;;; And avoid confusion with all the other opaque IDs flying around
;;; by using alphabetics instead.
;;; Generate this numbering sequence for up to 5 alphabetic "digits":
;;; A..Z, AA,AB ... AZ, BA ... BZ, ZA .. ZZ, AAA, AAB ...
(defun id->string (id)
  (declare (type (integer 1 *) id))
  (flet ((n-digit-string (n-digits val)
           (let ((str (make-string n-digits :element-type 'base-char)))
             (loop for i downfrom (1- n-digits) to 0
                   do (multiple-value-bind (quo rem) (floor val 26)
                        (setf (aref str i) (code-char (+ (char-code #\A) rem))
                              val quo)))
             str)))
    (do ((min 0) (n-digits 1 (1+ n-digits)))
        (nil)
      (let ((span (expt 26 n-digits)))
        (when (<= id (+ min span))
          (return (n-digit-string n-digits (- id (1+ min)))))
        (incf min span)))))
;;; Invert the above mapping
(defun string->id (string)
  (let ((val 0))
    (dotimes (i (length string))
      (setq val (+ (* val 26) (- (char-code (char string i)) (char-code #\A)))))
    (dotimes (i (1- (length string)))
      (incf val (expt 26 (1+ i))))
    (1+ val)))

(eval-when (#+sb-xc :compile-toplevel)
  (loop for i from 1 to 1000 ; sanity check the stringifying algorithm
        do (assert (= (string->id (id->string i)) i))))

;;; When we print CONTINUATIONs and TNs, we assign them small numeric
;;; IDs so that we can get a handle on anonymous objects given a
;;; printout.
(macrolet ((def (array-getter fto ffrom type) ; "to" = to number/id, resp. "from"
             (let ((xform (if (eq array-getter 'objmap-id-to-ir2block)
                              'id->string
                              'identity)))
               `(progn
                  (export '(,fto ,ffrom))
                  (defun ,fto (x)
                    (declare (type ,type x))
                    (let* ((map *compilation*)
                           (ht (objmap-obj-to-id map)))
                      (or (gethash x ht)
                          (let ((array (,array-getter map)))
                            (when (null array)
                              (setf array (make-array 20 :fill-pointer 0
                                                      :adjustable t)
                                    (,array-getter map) array))
                            (setf (gethash x ht)
                                  (,xform (1+ (vector-push-extend x array))))))))
                  (defun ,ffrom (num)
                    (let ((array (,array-getter *compilation*)))
                      ,@(when (eq array-getter 'objmap-id-to-ir2block)
                          '((setq num (string->id num))))
                      ;; numbers start from 1, not 0
                      (and (< (1- num) (length array)) (aref array (1- num)))))))))
  (def objmap-id-to-node node-id id-node node)
  (def objmap-id-to-comp component-id id-component component)
  (def objmap-id-to-leaf leaf-id id-leaf leaf)
  (def objmap-id-to-cont %cont-num num-cont (or ctran lvar))
  (def objmap-id-to-ir2block ir2-block-id id-ir2-block ir2-block)
  (def objmap-id-to-tn tn-id id-tn tn)
  (def objmap-id-to-label label-id id-label label))
;; Do not give NIL a continuation number
(defun cont-num (x) (if x (%cont-num x)))

;;; Print a terse one-line description of LEAF.
(defun print-leaf (leaf &optional (stream *standard-output*))
  (declare (type leaf leaf) (type stream stream))
  (etypecase leaf
    (lambda-var (prin1 (leaf-debug-name leaf) stream))
    (constant (format stream "'~S" (constant-value leaf)))
    (global-var
     (format stream "~S {~A}" (leaf-debug-name leaf) (global-var-kind leaf)))
    (clambda
     (format stream "lambda ~@[~S ~]~:S"
             (if (leaf-has-source-name-p leaf)
                 (leaf-source-name leaf)
                 (functional-debug-name leaf))
             (mapcar #'leaf-debug-name (lambda-vars leaf))))
    (optional-dispatch
     (format stream "optional-dispatch ~a ~S"
             (if (leaf-has-source-name-p leaf)
                 (leaf-source-name leaf)
                 (functional-debug-name leaf))
             (mapcar #'leaf-debug-name (optional-dispatch-arglist leaf))))
    (functional
     (functional-kind-case leaf
       (toplevel-xep
        (format stream "TL-XEP ~S" (entry-info-name (leaf-info leaf))))
       (t
        (format stream "~S ~S" (type-of leaf) (functional-debug-name leaf)))))))

;;; Attempt to find a block given some thing that has to do with it.
(declaim (ftype (sfunction (t) cblock) block-or-lose))
(defun block-or-lose (thing)
  (etypecase thing
    (cblock thing)
    (ir2-block (ir2-block-block thing))
    (vop (block-or-lose (vop-block thing)))
    (tn-ref (block-or-lose (tn-ref-vop thing)))
    (ctran (ctran-block thing))
    (node (node-block thing))
    (component (component-head thing))
    (cloop (loop-head thing))
    (integer (ctran-block (num-cont thing)))
    (functional (lambda-block (main-entry thing)))
    (null (error "Bad thing: ~S." thing))
    (symbol (block-or-lose (gethash thing (free-funs *ir1-namespace*))))))

;;; Print cN.
(defun print-ctran (cont)
  (declare (type ctran cont))
  (format t "c~D " (cont-num cont))
  (values))
(defun print-lvar (cont)
  (declare (type lvar cont))
  (if (and (lvar-info cont)
           (eq (ir2-lvar-kind (lvar-info cont))
               :unknown))
      (format t "uv~D " (cont-num cont))
      (format t "v~D " (cont-num cont)))
  (values))

(defun print-lvar-stack (stack &optional (stream *standard-output*))
  (loop for (lvar . rest) on stack
        do (format stream "~:[u~;d~]v~D~@[ ~]"
                   (eq (ir2-lvar-kind (lvar-info lvar)) :stack)
                   (cont-num lvar)
                   rest)))

(defvar *debug-print-types* nil)
(defvar *debug-print-vop-temps* nil)

;;; Print out the nodes in BLOCK in a format oriented toward
;;; representing what the code does.
(defun print-nodes (block)
  (setq block (block-or-lose block))
  (pprint-logical-block (nil nil)
    (format t "~:@_IR1 block ~D~@[ start c~D~]"
            (block-number block) (cont-num (block-start block)))
    (when (block-delete-p block)
      (format t " <deleted>"))

    (pprint-newline :mandatory)
    (let ((pred (block-pred block)))
      (format t "predecessors~{ c~D~}~%"
              (mapcar (lambda (x) (cont-num (block-start x))) pred)))
    (pprint-newline :mandatory)
    (when (block-start-cleanup block)
      (format t "cleanup ~s~%" (cleanup-kind (block-start-cleanup block))))
    (awhen (block-info block)
      (format t "start stack: ")
      (print-lvar-stack (ir2-block-start-stack it))
      (pprint-newline :mandatory))
    (do ((ctran (block-start block) (node-next (ctran-next ctran))))
        ((not ctran))
      (let ((node (ctran-next ctran)))
        (format t "~3D>~:[    ~;~:*~3D:~] "
                (cont-num ctran)
                (when (and (valued-node-p node) (node-lvar node))
                  (cont-num (node-lvar node))))
        (etypecase node
          (ref (print-leaf (ref-leaf node)))
          (basic-combination
           (let ((kind (basic-combination-kind node)))
             (format t "~(~A~A ~A~) "
                     (if (node-tail-p node) "tail " "")
                     kind
                     (type-of node))
             (format t "{~a " (combination-fun-debug-name node))
             (print-lvar (basic-combination-fun node))
             (format t "} ")
             (dolist (arg (basic-combination-args node))
               (if arg
                   (print-lvar arg)
                   (format t "<none> "))))
           (format t "{derived ~a}" (type-specifier (basic-combination-derived-type node))))
          (cset
           (write-string "set ")
           (print-leaf (set-var node))
           (write-char #\space)
           (print-lvar (set-value node)))
          (cif
           (write-string "if ")
           (print-lvar (if-test node))
           (print-ctran (block-start (if-consequent node)))
           (print-ctran (block-start (if-alternative node))))
          (jump-table
           (write-string "jump-table ")
           (print-lvar (jump-table-index node))
           (loop for (index . target) in (jump-table-targets node)
                 do (format t "~a>" index)
                    (print-ctran (block-start target))))
          (bind
           (write-string "bind ")
           (print-leaf (bind-lambda node))
           (unless (functional-kind-eq (bind-lambda node) nil)
             (format t " ~S ~S" :kind (decode-functional-kind-attributes
                                       (functional-kind (bind-lambda node))))))
          (creturn
           (write-string "return ")
           (print-lvar (return-result node))
           (print-leaf (return-lambda node)))
          (entry
           (format t "entry ~S" (entry-exits node)))
          (exit
           (let ((value (exit-value node)))
             (cond (value
                    (format t "exit ")
                    (print-lvar value))
                   ((exit-entry node)
                    (format t "exit <no value>"))
                   (t
                    (format t "exit <degenerate>")))))
          (delay
           (write-string "delay ")
           (print-lvar (delay-value node)))
          (cast
           (let ((value (cast-value node)))
             (format t "cast v~D ~A[~S -> ~S]" (cont-num value)
                     (if (cast-%type-check node) #\+ #\-)
                     (cast-type-to-check node)
                     (cast-asserted-type node))))
          (enclose
           (write-string "enclose ")
           (dolist (leaf (enclose-funs node))
             (print-leaf leaf)
             (when (lambda-p leaf)
               (write-char #\space)
               (let ((env (lambda-environment leaf)))
                 (when env
                   (write-string "{env:")
                   (dolist (thing (environment-closure env))
                     (write-char #\space)
                     (etypecase thing
                       (leaf (print-leaf thing))
                       (nlx-info (princ thing))))
                   (write-string "}"))))
             (write-char #\space)))
          (cdynamic-extent
           (format t "dynamic extent ~S" (dynamic-extent-values node))))
        (when (and *debug-print-types*
                   (valued-node-p node))
          (write-char #\space)
          (princ (type-specifier (node-derived-type node))))
        (pprint-newline :mandatory)))

    (awhen (block-info block)
      (format t "end stack: ")
      (print-lvar-stack (ir2-block-end-stack it))
      (pprint-newline :mandatory))
    (let ((succ (block-succ block)))
      (format t "successors~{ c~D~}~%"
              (mapcar (lambda (x) (cont-num (block-start x))) succ)))
    (when (block-end-cleanup block)
      (format t "cleanup ~s~%" (cleanup-kind (block-end-cleanup block)))))
  (values))

(defun tn-write-count (tn)
  (loop for ref = (tn-writes tn) then (tn-ref-next ref)
        while ref
        count t))

(defun tn-read-count (tn)
  (loop for ref = (tn-reads tn) then (tn-ref-next ref)
        while ref
        count t))

;;; Print the guts of a TN. (logic shared between PRINT-OBJECT (TN T)
;;; and printers for compound objects which contain TNs)
(defun print-tn-guts (tn &optional (stream *standard-output*))
  (declare (type tn tn))
  (flet ((print-tn-name (tn)
           (let ((leaf (tn-leaf tn)))
             (cond (leaf
                    (print-leaf leaf stream)
                    (format stream "!~D" (tn-id tn)))
                   (t
                    (format stream "t~D" (tn-id tn))))
             (when (and (tn-sc tn) (tn-offset tn))
               (format stream "[~A]" (location-print-name tn))))))
    (print-tn-name tn)
    (format stream " ~s" (tn-kind tn))
    (when (eq (tn-kind tn) :alias)
      (format stream " to ")
      (print-tn-name (tn-save-tn tn)))
    (when *debug-print-types*
      (format stream "~@[ ~a~]~@[ ~a~]"
              (and (tn-sc tn)
                   (sc-name (tn-sc tn)))
              (and (tn-primitive-type tn)
                   (primitive-type-name (tn-primitive-type tn)))))
    #+nil
    (format stream "/r~aw~a/" (tn-read-count tn) (tn-write-count tn))))

;;; Print the TN-REFs representing some operands to a VOP, linked by
;;; TN-REF-ACROSS.
(defun print-operands (refs)
  (declare (type (or tn-ref null) refs))
  (pprint-logical-block (*standard-output* nil)
    (do ((ref refs (tn-ref-across ref)))
        ((null ref))
      (let ((tn (tn-ref-tn ref))
            (ltn (tn-ref-load-tn ref)))
        (awhen (tn-ref-memory-access ref) (format t "@~A" it))
        (cond ((not ltn)
               (print-tn-guts tn))
              (t
               (print-tn-guts tn)
               (princ (if (tn-ref-write-p ref) #\< #\>))
               (print-tn-guts ltn)))
        (princ #\space)
        (pprint-newline :fill)))))

;;; Print the VOP, putting args, info and results on separate lines, if
;;; necessary.
(defun print-vop (vop)
  (pprint-logical-block (*standard-output* nil)
    (princ (vop-name vop))
    (princ #\space)
    (pprint-indent :current 0)
    (print-operands (vop-args vop))
    (when *debug-print-vop-temps*
      (pprint-newline :linear)
      (write-string "{temp: ")
      (print-operands (vop-temps vop))
      (princ #\}))
    (pprint-newline :linear)
    (when (vop-codegen-info vop)
      (princ (%with-output-to-string (stream)
               ;; Current print depth varies based on whether PRINT-VOP
               ;; is called by DESCRIBE-IR2-COMPONENT or TRACE-INSTRUCTION,
               ;; so any fixed value of *PRINT-LEVEL* changes its effect
               ;; depending on the call context. Resetting depth to 0 seems
               ;; like the best way to get consistent output.
               ;; We shouldn't bind the printer limits to NIL, because
               ;; hairy internal objects such as ENVIRONMENT can be printed.
               ;; See also the comment above FUNCALL-WITH-DEBUG-IO-SYNTAX.
               (let (#-sb-xc-host (*current-level-in-print* 0)
                     (*print-level* 2)
                     (*print-length* 15))
                 (format stream "{~{~S~^ ~}} " (vop-codegen-info vop)))))
      (pprint-newline :linear))
    (when (vop-results vop)
      (princ "=> ")
      (print-operands (vop-results vop))))
  (pprint-newline :mandatory))

;;; Print the VOPs in the specified IR2 block.
(defun print-ir2-block (block)
  (declare (type ir2-block block))
  (pprint-logical-block (*standard-output* nil)
    (cond
      ((eq (block-info (ir2-block-block block)) block)
       (format t "~:@_IR2 block ~A~@[(#~d)~]~@[ start c~D~]~:@_"
               (ir2-block-id block)
               (ir2-block-number block)
               (cont-num (block-start (ir2-block-block block))))
       (when (boundp '*2block-info*)
         (let ((info (gethash block *2block-info*)))
           (format t "pred=~:A succ=~:A~:@_"
                   (mapcar #'ir2-block-id (car info))
                   (mapcar #'ir2-block-id (cdr info)))))
       (let ((label (ir2-block-%label block)))
         (when label
           (format t "L~D:~:@_" (label-id label)))))
      (t
       (format t "<overflow>~:@_")))

    (do ((vop (ir2-block-start-vop block)
              (vop-next vop))
         (number 0 (1+ number)))
        ((null vop))
      (format t "~W: " number)
      (print-vop vop))))

;;; This is like PRINT-NODES, but dumps the IR2 representation of the
;;; code in BLOCK.
(defun print-vops (block)
  (setq block (block-or-lose block))
  (let ((2block (block-info block)))
    (print-ir2-block 2block)
    (do ((b (ir2-block-next 2block) (ir2-block-next b)))
        ((not (eq (ir2-block-block b) block)))
      (print-ir2-block b)))
  (values))

;;; Scan the IR2 blocks in emission order.
(defun print-ir2-blocks (thing &optional full)
  (let* ((block (component-head (block-component (block-or-lose thing))))
         (2block (block-info block)))
    (pprint-logical-block (nil nil)
      (loop while 2block
         do (setq block (ir2-block-block 2block))
         do (pprint-logical-block (*standard-output* nil)
              (if full
                  (print-nodes block)
                  (format t "IR1 block ~D~@[ start c~D~]"
                          (block-number block)
                          (cont-num (block-start block))))
              (pprint-indent :block 4)
              (pprint-newline :mandatory)
              (loop while (and 2block (eq (ir2-block-block 2block) block))
                 do (print-ir2-block 2block)
                 do (setq 2block (ir2-block-next 2block))))
         do (pprint-newline :mandatory))))
  (values))

;;; Do a PRINT-NODES on BLOCK and all blocks reachable from it by
;;; successor links.
(defun print-blocks (block)
  (setq block (block-or-lose block))
  (do-blocks (block (block-component block) :both)
    (setf (block-flag block) nil))
  (labels ((walk (block)
             (unless (block-flag block)
               (setf (block-flag block) t)
               (when (block-start block)
                 (print-nodes block))
               (dolist (block (block-succ block))
                 (walk block)))))
    (walk block))
  (values))

;;; Print all blocks in BLOCK's component in DFO.
(defun print-all-blocks (thing)
  (do-blocks (block (block-component (block-or-lose thing)))
    (handler-case (print-nodes block)
      (error (condition)
        (format t "~&~A...~%" condition))))
  (values))

(defvar *list-conflicts-table*)

;;; Add all ALWAYS-LIVE TNs in BLOCK to the conflicts. TN is ignored
;;; when it appears in the global conflicts.
(defun add-always-live-tns (block tn)
  (declare (type ir2-block block) (type tn tn))
  (do ((conf (ir2-block-global-tns block)
             (global-conflicts-next-blockwise conf)))
      ((null conf))
    (when (eq (global-conflicts-kind conf) :live)
      (let ((btn (global-conflicts-tn conf)))
        (unless (eq btn tn)
          (setf (gethash btn *list-conflicts-table*) t)))))
  (values))

;;; Add all local TNs in BLOCK to the conflicts.
(defun add-all-local-tns (block)
  (declare (type ir2-block block))
  (let ((ltns (ir2-block-local-tns block)))
    (dotimes (i (ir2-block-local-tn-count block))
      (setf (gethash (svref ltns i) *list-conflicts-table*) t)))
  (values))

;;; Make a list out of all of the recorded conflicts.
(defun listify-conflicts-table ()
  (collect ((res))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when k
                 (res k)))
             *list-conflicts-table*)
    (res)))

;;; Return a list of a the TNs that conflict with TN. Sort of, kind
;;; of. For debugging use only. Probably doesn't work on :COMPONENT TNs.
(defun list-conflicts (tn)
  (aver (member (tn-kind tn) '(:normal :environment :debug-environment)))
  (let ((confs (tn-global-conflicts tn)))
    (cond (confs
           (let ((*list-conflicts-table* (make-hash-table :test 'eq)))
             (unwind-protect
                  (do ((conf confs (global-conflicts-next-tnwise conf)))
                      ((null conf)
                       (listify-conflicts-table))
                    (format t "~&#<block ~D kind ~S>~%"
                            (block-number (ir2-block-block (global-conflicts-block
                                                            conf)))
                            (global-conflicts-kind conf))
                    (let ((block (global-conflicts-block conf)))
                      (add-always-live-tns block tn)
                      (if (eq (global-conflicts-kind conf) :live)
                          (add-all-local-tns block)
                          (let ((bconf (global-conflicts-conflicts conf))
                                (ltns (ir2-block-local-tns block)))
                            (dotimes (i (ir2-block-local-tn-count block))
                              (when (/= (sbit bconf i) 0)
                                (setf (gethash (svref ltns i) *list-conflicts-table*)
                                      t)))))))
               (clrhash *list-conflicts-table*))))
          (t
           (let* ((block (tn-local tn))
                  (ltns (ir2-block-local-tns block))
                  (confs (tn-local-conflicts tn)))
             (collect ((res))
               (dotimes (i (ir2-block-local-tn-count block))
                 (when (/= (sbit confs i) 0)
                   (let ((tn (svref ltns i)))
                     (when (and tn (not (eq tn :more))
                                (not (tn-global-conflicts tn)))
                       (res tn)))))
               (do ((gtn (ir2-block-global-tns block)
                         (global-conflicts-next-blockwise gtn)))
                   ((null gtn))
                 (when (or (eq (global-conflicts-kind gtn) :live)
                           (/= (sbit confs (global-conflicts-number gtn)) 0))
                   (res (global-conflicts-tn gtn))))
               (res)))))))

(defun nth-vop (thing n)
  "Return the Nth VOP in the IR2-BLOCK pointed to by THING."
  (let ((block (block-info (block-or-lose thing))))
    (do ((i 0 (1+ i))
         (vop (ir2-block-start-vop block) (vop-next vop)))
        ((= i n) vop))))

(defun show-transform-p (showp fun-name)
  (or (and (listp showp) (member fun-name showp :test 'equal))
      (eq showp t)))

(defun show-transform (kind name new-form &optional combination)
  (let ((*print-length* 100)
        (*print-level* 50)
        (*print-right-margin* 128))
    (format *trace-output* "~&xform (~a) ~S ~% -> ~S~%"
            kind
            (if combination
                (cons name
                      (loop for arg in (combination-args combination)
                            collect (if (constant-lvar-p arg)
                                        (lvar-value arg)
                                        (type-specifier (lvar-type arg)))))
                name)
            new-form)))

(defun show-type-derivation (combination type)
  (let ((*print-length* 100)
        (*print-level* 50)
        (*print-right-margin* 128))
    (unless (type= (node-derived-type combination)
                   (coerce-to-values type))
      (format *trace-output* "~&~a derived to ~a"
              (cons (combination-fun-source-name combination)
                    (loop for arg in (combination-args combination)
                          collect (if (constant-lvar-p arg)
                                      (lvar-value arg)
                                      (type-specifier (lvar-type arg)))))
              (type-specifier type)))))

;;;; producing a graphviz file

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
            while pos)))

(defun ir1-to-dot (component output-file)
  (with-open-file (stream output-file :if-exists :supersede
                                      :if-does-not-exist :create
                                      :direction :output)
    (write-line "digraph G {" stream)
    (write-line "node [fontname = \"monospace\"];" stream)
    (write-line "node [shape=box];" stream)
    ;; Give a unique label to every block, since BLOCK-NUMBERs may be
    ;; uninitialized during optimization.
    (let ((label 0)
          (block-labels (make-hash-table :test #'eq)))
      (do-blocks (block component :both)
        (setf (gethash block block-labels) label)
        (incf label))
      (flet ((block-label (block)
               (gethash block block-labels)))
        (do-blocks (block component :both)
          (cond ((eq block (component-head component))
                 (format stream "~a [label=\"head of component ~a\"];"
                         (block-label block)
                         (component-name component)))
                ((eq block (component-tail component))
                 (format stream "~a [label=tail];"
                         (block-label block)))
                (t
                 (format stream "~a [label=\"~a\"];~%"
                         (block-label block)
                         (replace-all
                          (replace-all (with-output-to-string (*standard-output*)
                                         (print-nodes block))
                                       (string #\Newline)
                                       "\\l")
                          "\""
                          "\\"))))
          (let ((succ (block-succ block)))
            (when succ
              (loop for succ in succ
                    for attr = "[style=bold]" then ""
                    do
                       (format stream "~a -> ~a~a;~%"
                               (block-label block)
                               (block-label succ)
                               attr)))))
        (dolist (ep (block-succ (component-head component)))
          (when (not (bind-p (block-start-node ep)))
            (format stream "~a -> ~a [style=dotted];~%"
                    (block-label ep)
                    (block-label (node-block
                                  (cleanup-mess-up
                                   (block-start-cleanup ep)))))))))
    (write-line "}" stream)))

(defun print-constraint (constraint &optional kind)
  (let ((kind* (constraint-kind constraint)))
    (when (or (not kind)
              (eq kind* kind))
      (flet ((f (x)
               (cond ((lambda-var-p x)
                      (leaf-source-name x))
                     ((ctype-p x)
                      (type-specifier x))
                     (t x))))
        (format t "   ~a~a ~a ~a~%" (if (eq kind* 'equality)
                                        (format nil "~a ~a~@[ ~a~]" kind* (equality-constraint-operator constraint)
                                                (when (/= (equality-constraint-amount constraint) 0)
                                                  (equality-constraint-amount constraint)))
                                        kind*)
                (if (constraint-not-p constraint)
                    " NOT"
                    "")
                (f (constraint-x constraint))
                (f (constraint-y constraint)))))))

(defun print-conset (conset &optional kind)
  (do-conset-elements (con conset)
    (print-constraint con kind)))

(defun print-constraints (component &optional kind)
  (do-blocks (block component)
    (handler-case (progn
                    (terpri)
                    (terpri)
                    ;(print-conset (block-in block) kind)
                    (print-nodes block)
                    (let ((last (block-last block)))
                      (cond ((if-p last)
                             (format t "  CONSEQ~%")
                             (print-conset (if-consequent-constraints last) kind)
                             (format t "  ALT~%")
                             (print-conset (if-alternative-constraints last) kind))
                            (t
                             (print-conset (block-out block) kind)))))
      (error (condition)
        (format t "~&~A...~%" condition)))))
