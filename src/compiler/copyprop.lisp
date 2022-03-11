;;;; This file implements the copy propagation phase of the compiler,
;;;; which uses global flow analysis to eliminate unnecessary copying
;;;; of variables.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; In copy propagation, we manipulate sets of TNs. We only consider
;;; TNs whose sole write is by a MOVE VOP. This allows us to use a
;;; degenerate version of reaching definitions: since each such TN has
;;; only one definition, the TN can stand for the definition. We can
;;; get away with this simplification, since the TNs that would be
;;; subject to copy propagation are nearly always single-writer
;;; (mostly temps allocated to ensure evaluation order is perserved).
;;; Only TNs written by MOVEs are interesting, since all we do with
;;; this information is delete spurious MOVEs.
;;;
;;; There are additional semantic constraints on whether a TN can be
;;; considered to be a copy. See TN-IS-A-COPY-OF.
;;;
;;; If a TN is in the IN set for a block, that TN is a copy of a TN
;;; which still has the same value it had at the time the move was
;;; done. Any reference to a TN in the IN set can be replaced with a
;;; reference to the TN moved from. When we delete all reads of such a
;;; TN, we can delete the MOVE VOP. IN is computed as the intersection
;;; of OUT for all the predecessor blocks.
;;;
;;; In this flow analysis scheme, the KILL set is the set of all
;;; interesting TNs where the copied TN is modified by the block (in
;;; any way.)
;;;
;;; GEN is the set of all interesting TNs that are copied in the block
;;; (whose write appears in the block.)
;;;
;;; OUT is (union (difference IN KILL) GEN)

;;; If TN is subject to copy propagation, then return the TN it is a copy
;;; of, otherwise NIL.
;;;
;;; We also only consider TNs where neither the TN nor the copied TN
;;; are wired or restricted. If we extended the life of a wired or
;;; restricted TN, register allocation might fail, and we can't
;;; substitute arbitrary things for references to wired or restricted
;;; TNs, since the reader may be expencting the argument to be in a
;;; particular place (as in a passing location.)
;;;
;;; The TN must be a :NORMAL TN. Other TNs might have hidden
;;; references or be otherwise bizarre.
;;;
;;; A TN is also inelegible if we want to preserve it to facilitate
;;; debugging.
;;;
;;; The SCs of the TN's primitive types is a subset of the SCs of the
;;; copied TN. Moves between TNs of different primitive type SCs may
;;; need to be changed into coercions, so we can't squeeze them out.
;;; The reason for testing for subset of the SCs instead of the same
;;; primitive type is that this test lets T be substituted for LIST,
;;; POSITIVE-FIXNUM for FIXNUM, etc. Note that more SCs implies fewer
;;; possible values, or a subtype relationship, since more SCs implies
;;; more possible representations.
(defun tn-is-copy-of (tn)
  (declare (type tn tn))
  (declare (inline subsetp))
  (let ((writes (tn-writes tn)))
    (and (eq (tn-kind tn) :normal)
         (not (tn-sc tn))               ; Not wired or restricted.
         (and writes (null (tn-ref-next writes)))
         (let ((vop (tn-ref-vop writes)))
           (and (eq (vop-name vop) 'move)
                (let ((arg-tn (tn-ref-tn (vop-args vop))))
                  (and (or (not (tn-sc arg-tn))
                           (eq (tn-kind arg-tn) :constant))
                       (or
                        (subsetp (primitive-type-scs (tn-primitive-type tn))
                                 (primitive-type-scs (tn-primitive-type arg-tn)))
                        (let ((reads (tn-reads tn)))
                          ;; For moves to another moves the SC does
                          ;; not matter, the intermediate SC will
                          ;; match and there's no VOP selection to be
                          ;; performed based on the SC.
                          (and reads
                               (null (tn-ref-next reads))
                               (eq (vop-name (tn-ref-vop reads)) 'move)
                               ;; Except for fixnums which may benifit from improved representation selection
                               ;; if there is a way to go through an any-reg "adapter"
                               (not (memq sb-vm:any-reg-sc-number (primitive-type-scs (tn-primitive-type tn))))
                               (not (memq sb-vm:any-reg-sc-number (primitive-type-scs (tn-primitive-type arg-tn)))))))
                       (let ((leaf (tn-leaf tn)))
                         (or (not leaf)
                             (and
                              ;; Do we not care about preserving this this
                              ;; TN for debugging?
                              (or
                               (not (cl:symbol-package (leaf-debug-name leaf)))
                               (policy (vop-node vop)
                                       (or (= speed 3) (< debug 2))))
                              ;; arguments of local functions have hidden write
                              (not (and (lambda-var-p leaf)
                                        (memq (functional-kind (lambda-var-home leaf))
                                                   '(nil :optional)))))))
                       arg-tn)))))))

(defun reads-within-block-p (tn block)
  (do ((ref (tn-reads tn) (tn-ref-next ref)))
      ((null ref) t)
    (let ((vop (tn-ref-vop ref)))
      ;; MOVEs can be chained and read in other blocks. This is done
      ;; not for correctness but for memory usage reduction, so there
      ;; is no need to follow all the chains, just be conservative.
      (when (or (eq (vop-name vop) 'move)
                (neq (ir2-block-block (vop-block vop)) block))
        (return)))))

;;; Init the sets in BLOCK for copy propagation. To find GEN, we just
;;; look for MOVE vops, and then see whether the result is a eligible
;;; copy TN. To find KILL, we must look at all VOP results, seeing
;;; whether any of the reads of the written TN are copies for eligible
;;; TNs.
(defun init-copy-sets (block)
  (declare (type cblock block))
  (let ((kill (make-sset))
        (out (make-sset)))
    (do ((vop (ir2-block-start-vop (block-info block)) (vop-next vop)))
        ((null vop))
      (unless (and (eq (vop-name vop) 'move)
                   (let ((y (tn-ref-tn (vop-results vop))))
                     (when (tn-is-copy-of y)
                       (unless (reads-within-block-p y block)
                        (sset-adjoin y out))
                       t)))
        ;; WANTED: explanation of UNLESS above.
        (do ((res (vop-results vop) (tn-ref-across res)))
            ((not res))
          (let ((res-tn (tn-ref-tn res)))
            (do ((read (tn-reads res-tn) (tn-ref-next read)))
                ((null read))
              (let ((read-vop (tn-ref-vop read)))
                (when (eq (vop-name read-vop) 'move)
                  (let ((y (tn-ref-tn (vop-results read-vop))))
                    (when (tn-is-copy-of y)
                      (sset-delete y out)
                      (sset-adjoin y kill))))))))))
    (setf (block-out block) out
          (block-kill block) kill))
  (values))

;;; Do the flow analysis step for copy propagation on BLOCK. We rely
;;; on OUT being initialized to GEN, and use SSET-UNION-OF-DIFFERENCE
;;; to incrementally build the union in OUT, rather than replacing OUT
;;; each time.
(defun copy-flow-analysis (block)
  (declare (type cblock block))
  (let* ((pred (block-pred block))
         (in (copy-sset (block-out (first pred)))))
    (dolist (pred-block (rest pred))
      (sset-intersection in (block-out pred-block)))
    (setf (block-in block) in)
    (sset-union-of-difference (block-out block)
                              in
                              (block-kill block))))

(defevent copy-deleted-move "Copy propagation deleted a move.")

;;; Return true if ARG is a reference to a TN that we can copy
;;; propagate to. In addition to dealing with copy chains (as
;;; discussed below), we also discard references that are arguments
;;; to a local call, since IR2tran introduces temps in that context
;;; to preserve parallel assignment semantics.
(defun ok-copy-ref (vop arg in original-copy-of)
  (declare (type vop vop) (type tn arg) (type sset in)
           (type hash-table original-copy-of))
  (and (sset-member arg in)
       (do ((original (gethash arg original-copy-of)
                      (gethash original original-copy-of)))
           ((not original) t)
         (unless (sset-member original in)
           (return nil)))
       (let ((info (vop-info vop)))
         (not (or (eq (vop-info-move-args info) :local-call)
                  (>= (or (position-in #'tn-ref-across arg (vop-args vop)
                                       :key #'tn-ref-tn)
                          (error "Couldn't find REF?"))
                      (length (template-arg-types info))))))))

;;; Make use of the result of flow analysis to eliminate copies. We
;;; scan the VOPs in block, propagating copies and keeping our IN set
;;; in sync.
;;;
;;; Original-Copy-Of is an EQ hash table that we use to keep track of
;;; renamings when there are copy chains, i.e. copies of copies. When
;;; we see copy of a copy, we enter the first copy in the table with
;;; the second copy as a key. When we see a reference to a TN in a
;;; copy chain, we can only substitute the first copied TN for the
;;; reference when all intervening copies in the copy chain are also
;;; available. Otherwise, we just leave the reference alone. It is
;;; possible that we might have been able to reference one of the
;;; intermediate copies instead, but that copy might have already been
;;; deleted, since we delete the move immediately when the references
;;; go to zero.
;;;
;;; To understand why we always can to the substitution when the copy
;;; chain recorded in the Original-Copy-Of table hits NIL, note that
;;; we make an entry in the table iff we change the arg of a copy. If
;;; an entry is not in the table, it must be that we hit a move which
;;; *originally* referenced our Copy-Of TN. If all the intervening
;;; copies reach our reference, then Copy-Of must reach the reference.
;;;
;;; Note that due to our restricting copies to single-writer TNs, it
;;; will always be the case that when the first copy in a chain
;;; reaches the reference, all intervening copies reach also reach the
;;; reference. We don't exploit this, since we have to work backward
;;; from the last copy.
;;;
;;; In this discussion, we are really only playing with the tail of
;;; the true copy chain for which all of the copies have already had
;;; PROPAGATE-COPIES done on them. But, because we do this pass in
;;; DFO, it is virtually always the case that we will process earlier
;;; copies before later ones. In perverse cases (non-reducible flow
;;; graphs), we just miss some optimization opportinities.
(defun propagate-copies (block original-copy-of)
  (declare (type cblock block) (type hash-table original-copy-of))
  (let ((in (block-in block)))
    ;; Don't retain garbage
    (setf (block-in block) nil
          (block-out block) nil
          (block-kill block) nil)
    (do ((vop (ir2-block-start-vop (block-info block)) (vop-next vop)))
        ((null vop))
      (let ((this-copy (and (eq (vop-name vop) 'move)
                            (let ((y (tn-ref-tn (vop-results vop))))
                              (when (tn-is-copy-of y) y)))))
        ;; Substitute copied TN for copy when we find a reference to a copy.
        ;; If the copy is left with no reads, delete the move to the copy.
        (do ((arg-ref (vop-args vop) (tn-ref-across arg-ref)))
            ((null arg-ref))
          (let* ((arg (tn-ref-tn arg-ref))
                 (copy-of (tn-is-copy-of arg)))
            (when (and copy-of (ok-copy-ref vop arg in original-copy-of))
              (when this-copy
                (setf (gethash this-copy original-copy-of) arg))
              (change-tn-ref-tn arg-ref copy-of)
              (when (null (tn-reads arg))
                (event copy-deleted-move)
                (delete-vop (tn-ref-vop (tn-writes arg)))))))
        ;; Kill any elements in IN that are copies of a TN we are clobbering.
        (do ((res-ref (vop-results vop) (tn-ref-across res-ref)))
            ((null res-ref))
          (do-sset-elements (tn in)
            (when (eq (tn-is-copy-of tn) (tn-ref-tn res-ref))
              (sset-delete tn in))))
        ;; If this VOP is a copy, add the copy TN to IN.
        (when this-copy (sset-adjoin this-copy in)))))

  (values))

;;; Do copy propagation on COMPONENT by initializing the flow analysis
;;; sets, doing flow analysis, and then propagating copies using the
;;; results.
(defun copy-propagate (component)
  (setf (block-out (component-head component)) (make-sset))
  (do-blocks (block component)
    (init-copy-sets block))

  (loop
    (let ((did-something nil))
      (do-blocks (block component)
        (when (copy-flow-analysis block)
          (setq did-something t)))
      (unless did-something (return))))

  (let ((original-copies (make-hash-table :test 'eq)))
    (do-blocks (block component)
      (propagate-copies block original-copies)))

  (values))
