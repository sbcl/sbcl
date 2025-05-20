;;;; This file contains the lifetime analysis phase in the compiler.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; utilities

;;; Link in a GLOBAL-CONFLICTS structure for TN in BLOCK with NUMBER
;;; as the LTN number. The conflict is inserted in the per-TN
;;; GLOBAL-CONFLICTS thread after the TN's CURRENT-CONFLICT. We change
;;; the CURRENT-CONFLICT to point to the new conflict. Since we scan
;;; the blocks in reverse DFO, this list is automatically built in
;;; order. We have to actually scan the current GLOBAL-TNs for the
;;; block in order to keep that thread sorted.
(defun add-global-conflict (kind tn block number)
  (declare (type (member :read :write :read-only :live) kind)
           (type tn tn) (type ir2-block block)
           (type (or local-tn-number null) number))
  (let ((new (make-global-conflicts kind tn block number)))
    (let ((last (tn-current-conflict tn)))
      (if last
          (shiftf (global-conflicts-next-tnwise new)
                  (global-conflicts-next-tnwise last)
                  new)
          (shiftf (global-conflicts-next-tnwise new)
                  (tn-global-conflicts tn)
                  new)))
    (setf (tn-current-conflict tn) new)

    (insert-block-global-conflict new block))
  (values))

;;; Do the actual insertion of the conflict NEW into BLOCK's global
;;; conflicts.
(defun insert-block-global-conflict (new block)
  ;; This used to keep the TNs sorted by TN-NUMBER, but appears to be
  ;; unnecessary.
  (shiftf (global-conflicts-next-blockwise new) (ir2-block-global-tns block) new)
  (values))

;;; Reset the CURRENT-CONFLICT slot in all packed TNs to point to the
;;; head of the GLOBAL-CONFLICTS thread.
(defun reset-current-conflict (component)
  (do-packed-tns (tn component)
    (setf (tn-current-conflict tn) (tn-global-conflicts tn))))

;;; Cache the results of BLOCK-ENVIRONMENT during lifetime analysis.
;;;
;;; Fetching the home-lambda of a block (needed in block-environment) can
;;; be an expensive operation under some circumstances, and it needs
;;; to be done a lot during lifetime analysis when compiling with high
;;; DEBUG (e.g. 30% of the total compilation time for CL-PPCRE with
;;; DEBUG 3 just for that).
(defun cached-block-environment (block)
  (let ((env (block-environment-cache block)))
    (if (eq env :none)
        (setf (block-environment-cache block)
              (block-environment block))
        env)))

;;;; pre-pass

;;; Convert TN (currently local) to be a global TN, since we
;;; discovered that it is referenced in more than one block. We just
;;; add a global-conflicts structure with a kind derived from the KILL
;;; and LIVE sets.
(defun convert-to-global (tn)
  (declare (type tn tn))
  (let ((block (tn-local tn))
        (num (tn-local-number tn)))
    (add-global-conflict
     (cond ((zerop (sbit (ir2-block-written block) num))
            :read-only)
           ((zerop (sbit (ir2-block-live-out block) num))
            :write)
           (t
            :read))
     tn block num))
  (values))

;;; Scan all references to packed TNs in block. We assign LTN numbers
;;; to each referenced TN, and also build the Kill and Live sets that
;;; summarize the references to each TN for purposes of lifetime
;;; analysis.
;;;
;;; It is possible that we will run out of LTN numbers. If this
;;; happens, then we return the VOP that we were processing at the
;;; time we ran out, otherwise we return NIL.
;;;
;;; If a TN is referenced in more than one block, then we must
;;; represent references using GLOBAL-CONFLICTS structures. When we
;;; first see a TN, we assume it will be local. If we see a reference
;;; later on in a different block, then we go back and fix the TN to
;;; global.
;;;
;;; We must globalize TNs that have a block other than the current one
;;; in their LOCAL slot and have no GLOBAL-CONFLICTS. The latter
;;; condition is necessary because we always set Local and
;;; LOCAL-NUMBER when we process a reference to a TN, even when the TN
;;; is already known to be global.
;;;
;;; When we see reference to global TNs during the scan, we add the
;;; global-conflict as :READ-ONLY, since we don't know the correct
;;; kind until we are done scanning the block.
(defun find-local-references (block)
  (declare (type ir2-block block))
  (let ((kill (ir2-block-written block))
        (live (ir2-block-live-out block))
        (tns (ir2-block-local-tns block)))
    (let ((ltn-num (ir2-block-local-tn-count block)))
      (do ((vop (ir2-block-last-vop block)
                (vop-prev vop)))
          ((null vop))
        (do ((ref (vop-refs vop) (tn-ref-next-ref ref)))
            ((null ref))
          (let* ((tn (tn-ref-tn ref))
                 (local (tn-local tn))
                 (kind (tn-kind tn)))
            (unless (member kind '(:component :environment :constant :unused))
              (unless (eq local block)
                (when (= ltn-num local-tn-limit)
                  (return-from find-local-references vop))
                (when local
                  (unless (tn-global-conflicts tn)
                    (convert-to-global tn))
                  (add-global-conflict :read-only tn block ltn-num))

                (setf (tn-local tn) block)
                (setf (tn-local-number tn) ltn-num)
                (setf (svref tns ltn-num) tn)
                (incf ltn-num))

              (let ((num (tn-local-number tn)))
                (if (tn-ref-write-p ref)
                    (setf (sbit kill num) 1  (sbit live num) 0)
                    (setf (sbit live num) 1)))))))

      (setf (ir2-block-local-tn-count block) ltn-num)))
  nil)

;;; Finish up the global conflicts for TNs referenced in BLOCK
;;; according to the local Kill and Live sets.
;;;
;;; We set the kind for TNs already in the global-TNs. If not written
;;; at all, then is :READ-ONLY, the default. Must have been referenced
;;; somehow, or we wouldn't have conflicts for it.
;;;
;;; We also iterate over all the local TNs, looking for TNs local to
;;; this block that are still live at the block beginning, and thus
;;; must be global. This case is only important when a TN is read in a
;;; block but not written in any other, since otherwise the write
;;; would promote the TN to global. But this does happen with various
;;; passing-location TNs that are magically written. This also serves
;;; to propagate the lives of erroneously uninitialized TNs so that
;;; consistency checks can detect them.
(defun init-global-conflict-kind (block)
  (declare (type ir2-block block))
  (let ((live (ir2-block-live-out block)))
    (let ((kill (ir2-block-written block)))
      (do ((conf (ir2-block-global-tns block)
                 (global-conflicts-next-blockwise conf)))
          ((null conf))
        (let ((num (global-conflicts-number conf)))
          (unless (zerop (sbit kill num))
            (setf (global-conflicts-kind conf)
                  (if (zerop (sbit live num))
                      :write
                      :read))))))

    (let ((ltns (ir2-block-local-tns block)))
      (dotimes (i (ir2-block-local-tn-count block))
        (let ((tn (svref ltns i)))
          (unless (or (eq tn :more)
                      (tn-global-conflicts tn)
                      (zerop (sbit live i)))
            (convert-to-global tn))))))

  (values))

(defevent split-ir2-block "Split an IR2 block to meet LOCAL-TN-LIMIT.")

;;; Move the code after the VOP LOSE in 2BLOCK into its own block. The
;;; block is linked into the emit order following 2BLOCK. NUMBER is
;;; the block number assigned to the new block. We return the new
;;; block.
(defun split-ir2-blocks (2block lose number)
  (declare (type ir2-block 2block) (type vop lose)
           (type unsigned-byte number))
  (event split-ir2-block (vop-node lose))
  (let ((new (make-ir2-block (ir2-block-block 2block)))
        (new-start (vop-next lose)))
    (setf (ir2-block-number new) number)
    (add-to-emit-order new 2block)

    (do ((vop new-start (vop-next vop)))
        ((null vop))
      (setf (vop-block vop) new))

    (setf (ir2-block-start-vop new) new-start)
    (shiftf (ir2-block-last-vop new) (ir2-block-last-vop 2block) lose)

    (setf (vop-next lose) nil)
    (setf (vop-prev new-start) nil)

    new))

;;; Clear the global and local conflict info in BLOCK so that we can
;;; recompute it without any old cruft being retained. It is assumed
;;; that all LTN numbers are in use.
;;;
;;; First we delete all the global conflicts. The conflict we are
;;; deleting must be the last in the TN's GLOBAL-CONFLICTS, but we
;;; must scan for it in order to find the previous conflict.
;;;
;;; Next, we scan the local TNs, nulling out the LOCAL slot in all TNs
;;; with no global conflicts. This allows these TNs to be treated as
;;; local when we scan the block again.
;;;
;;; If there are conflicts, then we set LOCAL to one of the
;;; conflicting blocks. This ensures that LOCAL doesn't hold over
;;; BLOCK as its value, causing the subsequent reanalysis to think
;;; that the TN has already been seen in that block.
;;;
;;; This function must not be called on blocks that have :MORE TNs.
(defun clear-lifetime-info (block)
  (declare (type ir2-block block))
  (setf (ir2-block-local-tn-count block) 0)

  (do ((conf (ir2-block-global-tns block)
             (global-conflicts-next-blockwise conf)))
      ((null conf)
       (setf (ir2-block-global-tns block) nil))
    (let ((tn (global-conflicts-tn conf)))
      (aver (eq (tn-current-conflict tn) conf))
      (aver (null (global-conflicts-next-tnwise conf)))
      (do ((current (tn-global-conflicts tn)
                    (global-conflicts-next-tnwise current))
           (prev nil current))
          ((eq current conf)
           (if prev
               (setf (global-conflicts-next-tnwise prev) nil)
               (setf (tn-global-conflicts tn) nil))
           (setf (tn-current-conflict tn) prev)))))

  (fill (ir2-block-written block) 0)
  (let ((ltns (ir2-block-local-tns block)))
    (dotimes (i local-tn-limit)
      (let ((tn (svref ltns i)))
        (aver (not (eq tn :more)))
        (let ((conf (tn-global-conflicts tn)))
          (setf (tn-local tn)
                (if conf
                    (global-conflicts-block conf)
                    nil))))))

  (values))

;;; This provides a panic mode for assigning LTN numbers when there is
;;; a VOP with so many more operands that they can't all be assigned
;;; distinct numbers. When this happens, we recover by assigning all
;;; the &MORE operands the same LTN number. We can get away with this,
;;; since all &MORE args (and results) are referenced simultaneously
;;; as far as conflict analysis is concerned.
;;;
;;; BLOCK is the IR2-BLOCK that the MORE VOP is at the end of. OPS is
;;; the full argument or result TN-REF list. Fixed is the types of the
;;; fixed operands (used only to skip those operands.)
;;;
;;; What we do is grab a LTN number, then make a :READ-ONLY global
;;; conflict for each more operand TN. We require that there be no
;;; existing global conflict in BLOCK for any of the operands. Since
;;; conflicts must be cleared before the first call, this only
;;; prohibits the same TN being used both as a more operand and as any
;;; other operand to the same VOP.
;;;
;;; We don't have to worry about getting the correct conflict kind,
;;; since INIT-GLOBAL-CONFLICT-KIND will fix things up. Similarly,
;;; FIND-LOCAL-REFERENCES will set the local conflict bit
;;; corresponding to this call.
;;;
;;; We also set the LOCAL and LOCAL-NUMBER slots in each TN. It is
;;; possible that there are no operands in any given call to this
;;; function, but there had better be either some more args or more
;;; results.
(defun coalesce-more-ltn-numbers (block ops fixed)
  (declare (type ir2-block block) (type (or tn-ref null) ops) (list fixed))
  (let ((num (ir2-block-local-tn-count block)))
    (aver (< num local-tn-limit))
    (incf (ir2-block-local-tn-count block))
    (setf (svref (ir2-block-local-tns block) num) :more)

    (do ((op (do ((op ops (tn-ref-across op))
                  (i 0 (1+ i)))
                 ((= i (length fixed)) op)
               (declare (type index i)))
             (tn-ref-across op)))
        ((null op))
      (let ((tn (tn-ref-tn op)))
        (unless (member (tn-kind tn) '(:unused :constant))
          ;; A TN could be used more than once in :more.
          (unless (find-in #'global-conflicts-next-blockwise tn
                           (ir2-block-global-tns block)
                           :key #'global-conflicts-tn)
            (add-global-conflict :read-only tn block num))
          (setf (tn-local tn) block)
          (setf (tn-local-number tn) num)))))
  (values))

(defevent coalesce-more-ltn-numbers
  "Coalesced LTN numbers for a more operand to meet LOCAL-TN-LIMIT.")

;;; Loop over the blocks in COMPONENT, assigning LTN numbers and
;;; recording TN birth and death. The only interesting action is when
;;; we run out of local TN numbers while finding local references.
;;;
;;; If we run out of LTN numbers while processing a VOP within the
;;; block, then we just split off the VOPs we have successfully
;;; processed into their own block.
;;;
;;; If we run out of LTN numbers while processing the our first VOP
;;; (the last in the block), then it must be the case that this VOP
;;; has large more operands. We split the VOP into its own block, and
;;; then call COALESCE-MORE-LTN-NUMBERS to assign all the more
;;; args/results the same LTN number(s).
;;;
;;; In either case, we clear the lifetime information that we computed
;;; so far, recomputing it after taking corrective action.
;;;
;;; Whenever we split a block, we finish the pre-pass on the split-off
;;; block by doing FIND-LOCAL-REFERENCES and
;;; INIT-GLOBAL-CONFLICT-KIND. This can't run out of LTN numbers.
(defun lifetime-pre-pass (component)
  (declare (type component component))
  (let ((counter -1))
    (declare (type fixnum counter))
    (do-blocks-backwards (block component)
      (let ((2block (block-info block)))
        (do ((lose (find-local-references 2block)
                   (find-local-references 2block))
             (last-lose nil lose)
             (coalesced nil))
            ((not lose)
             (init-global-conflict-kind 2block)
             (setf (ir2-block-number 2block) (incf counter)))

          (clear-lifetime-info 2block)

          (cond
            ((vop-next lose)
             (aver (not (eq last-lose lose)))
             (let ((new (split-ir2-blocks 2block lose (incf counter))))
               (aver (not (find-local-references new)))
               (init-global-conflict-kind new)))
            (t
             (aver (not (eq lose coalesced)))
             (setq coalesced lose)
             (event coalesce-more-ltn-numbers (vop-node lose))
             (let ((info (vop-info lose))
                   (new (if (vop-prev lose)
                            (split-ir2-blocks 2block (vop-prev lose)
                                              (incf counter))
                            2block)))
               (coalesce-more-ltn-numbers new (vop-args lose)
                                          (vop-info-arg-types info))
               (coalesce-more-ltn-numbers new (vop-results lose)
                                          (vop-info-result-types info))
               (let ((lose (find-local-references new)))
                 (aver (not lose)))
               (init-global-conflict-kind new))))))))

  (values))

;;;; environment TN stuff

;;; Add a :LIVE global conflict for TN in 2BLOCK if there is none
;;; present. If DEBUG-P is false (a :ENVIRONMENT TN), then modify any
;;; existing conflict to be :LIVE.
(defun setup-environment-tn-conflict (tn 2block debug-p)
  (declare (type tn tn) (type ir2-block 2block))
  (let ((block-num (ir2-block-number 2block)))
    (do ((conf (tn-current-conflict tn) (global-conflicts-next-tnwise conf))
         (prev nil conf))
        ((or (null conf)
             (> (ir2-block-number (global-conflicts-block conf)) block-num))
         (setf (tn-current-conflict tn) prev)
         (add-global-conflict :live tn 2block nil))
      (when (eq (global-conflicts-block conf) 2block)
        (unless (or debug-p
                    (eq (global-conflicts-kind conf) :live))
          (setf (global-conflicts-kind conf) :live)
          (setf (svref (ir2-block-local-tns 2block)
                       (global-conflicts-number conf))
                nil)
          (setf (global-conflicts-number conf) nil))
        (setf (tn-current-conflict tn) conf)
        (return))))
  (values))

;;; Iterate over all the blocks in ENV, setting up :LIVE conflicts for
;;; TN. We make the TN global if it isn't already. The TN must have at
;;; least one reference.
(defun setup-environment-tn-conflicts (component tn env debug-p)
  (declare (type component component) (type tn tn) (type environment env))
  (when (and debug-p
             (not (tn-global-conflicts tn))
             (tn-local tn))
    (convert-to-global tn))
  (setf (tn-current-conflict tn) (tn-global-conflicts tn))
  (do-blocks-backwards (block component)
    (when (eq (cached-block-environment block) env)
      (let* ((2block (block-info block))
             (last (do ((b (ir2-block-next 2block) (ir2-block-next b))
                        (prev 2block b))
                       ((not (eq (ir2-block-block b) block))
                        prev))))
        (do ((b last (ir2-block-prev b)))
            ((not (eq (ir2-block-block b) block)))
          (setup-environment-tn-conflict tn b debug-p)))))
  (values))

;;; Iterate over all functions in the tail-set of FUN which close-over
;;; TN, adding appropriate conflict information. Indirect TNs should
;;; still be accessible within tail-called functions, even if the
;;; environment of the caller which contains the implicit value cell
;;; ceases to exist.
(defun setup-implicit-value-cell-tn-conflicts (component fun tn)
  (declare (type component component) (type clambda fun)
           (type tn tn))
  (let ((leaf (tn-leaf tn)))
    (dolist (tail-set-fun (tail-set-funs (lambda-tail-set fun)))
      (let ((env (lambda-environment tail-set-fun)))
        (when (memq leaf (environment-closure env))
          (setup-environment-tn-conflicts component tn env nil))))))

;;; Iterate over all the environment TNs, adding always-live conflicts
;;; as appropriate.
(defun setup-environment-live-conflicts (component)
  (declare (type component component))
  (dolist (fun (component-lambdas component))
    (let* ((env (lambda-environment fun))
           (2env (environment-info env)))
      (dolist (tn (ir2-environment-live-tns 2env))
        (setup-environment-tn-conflicts component tn env nil)
        (let ((leaf (tn-leaf tn)))
          (when (and (lambda-var-p leaf)
                     (lambda-var-indirect leaf)
                     (not (lambda-var-explicit-value-cell leaf)))
            (setup-implicit-value-cell-tn-conflicts component fun tn))))
      (dolist (tn (ir2-environment-debug-live-tns 2env))
        (setup-environment-tn-conflicts component tn env t))))
  (values))

;;; Convert a :NORMAL or :DEBUG-ENVIRONMENT TN to an :ENVIRONMENT TN.
;;; This requires adding :LIVE conflicts to all blocks in TN-ENV.
(defun convert-to-environment-tn (tn tn-env)
  (declare (type tn tn) (type environment tn-env))
  (aver (member (tn-kind tn) '(:normal :debug-environment)))
  (ecase (tn-kind tn)
    (:debug-environment
     (setq tn-env (tn-environment tn))
     (let* ((2env (environment-info tn-env)))
       (setf (ir2-environment-debug-live-tns 2env)
             (delete tn (ir2-environment-debug-live-tns 2env)))))
    (:normal
     (setf (tn-local tn) nil)
     (setf (tn-local-number tn) nil)))
  (setup-environment-tn-conflicts *component-being-compiled* tn tn-env nil)
  (setf (tn-kind tn) :environment)
  (setf (tn-environment tn) tn-env)
  (push tn (ir2-environment-live-tns (environment-info tn-env)))
  (values))

;;;; flow analysis

;;; For each GLOBAL-TN in BLOCK2 that is :LIVE, :READ or :READ-ONLY,
;;; ensure that there is a corresponding GLOBAL-CONFLICT in BLOCK1. If
;;; there is none, make a :LIVE GLOBAL-CONFLICT. If there is a
;;; :READ-ONLY conflict, promote it to :LIVE.
;;;
;;; If we did add a new conflict, return true, otherwise false. We
;;; don't need to return true when we promote a :READ-ONLY conflict,
;;; since it doesn't reveal any new information to predecessors of
;;; BLOCK1.
;;;
;;; We use the TN-CURRENT-CONFLICT to walk through the global
;;; conflicts. Since the global conflicts for a TN are ordered by
;;; block, we can be sure that the CURRENT-CONFLICT always points at
;;; or before the block that we are looking at. This allows us to
;;; quickly determine if there is a global conflict for a given TN in
;;; BLOCK1.
;;;
;;; When we scan down the conflicts, we know that there must be at
;;; least one conflict for TN, since we got our hands on TN by picking
;;; it out of a conflict in BLOCK2.
;;;
;;; We leave the CURRENT-CONFLICT pointing to the conflict for BLOCK1.
;;; The CURRENT-CONFLICT must be initialized to the head of the
;;; GLOBAL-CONFLICTS for the TN between each flow analysis iteration.

;;; FASTP is a KLUDGE: SBCL used to update the current-conflict only
;;; for the read-only case, but switched at one point to always
;;; updating it. This generally speeds up the compiler nicely, but
;;; sometimes it causes an infinite loop in the updating machinery,
;;; We cheat by switching off the fast path if it seems we're looping
;;; longer then expected.
(defun propagate-live-tns (block1 block2 fastp)
  (declare (type ir2-block block1 block2))
  (let ((live-in (ir2-block-live-in block1))
        (did-something nil))
    (do ((conf2 (ir2-block-global-tns block2)
                (global-conflicts-next-blockwise conf2)))
        ((null conf2))
      (ecase (global-conflicts-kind conf2)
        ((:live :read :read-only)
         (let* ((tn (global-conflicts-tn conf2))
                (tn-conflicts (tn-current-conflict tn))
                (number1 (ir2-block-number block1)))
           (aver tn-conflicts)
           (when (> (ir2-block-number (global-conflicts-block tn-conflicts))
                    number1)
             ;; The TN-CURRENT-CONFLICT finger overshot.  Reset it
             ;; conservatively.
             (setf tn-conflicts (tn-global-conflicts tn)
                   (tn-current-conflict tn) tn-conflicts)
             (aver tn-conflicts))
           (do ((current tn-conflicts (global-conflicts-next-tnwise current))
                (prev nil current))
               ((or (null current)
                    (> (ir2-block-number (global-conflicts-block current))
                       number1))
                (setf (tn-current-conflict tn) prev)
                (add-global-conflict :live tn block1 nil)
                (setq did-something t))
             (when (eq (global-conflicts-block current) block1)
               (case (global-conflicts-kind current)
                 (:live)
                 (:read-only
                  (setf (global-conflicts-kind current) :live)
                  (setf (svref (ir2-block-local-tns block1)
                               (global-conflicts-number current))
                        nil)
                  (setf (global-conflicts-number current) nil)
                  (unless fastp
                    (setf (tn-current-conflict tn) current)))
                 (t
                  (setf (sbit live-in (global-conflicts-number current)) 1)))
               (when fastp
                 (setf (tn-current-conflict tn) current))
               (return)))))
        (:write)))
    did-something))

;;; Do backward global flow analysis to find all TNs live at each
;;; block boundary.
(defparameter *max-fast-propagate-live-tn-passes* 10)
(defun lifetime-flow-analysis (component)
  ;; KLUDGE: This is the second part of the FASTP kludge in
  ;; propagate-live-tns: we pass fastp for ten first attempts,
  ;; and then switch to the works-for-sure version.
  ;;
  ;; The upstream uses the fast version always, but sometimes
  ;; that gets stuck in a loop...
  (loop for i = 0 then (1+ i)
        do
    (reset-current-conflict component)
    (let ((did-something nil))
      (do-blocks-backwards (block component)
        (let* ((2block (block-info block))
               (last (do ((b (ir2-block-next 2block) (ir2-block-next b))
                          (prev 2block b))
                         ((not (eq (ir2-block-block b) block))
                          prev))))

          (dolist (b (block-succ block))
            (when (and (block-start b)
                       (propagate-live-tns
                        last (block-info b)
                        (< i *max-fast-propagate-live-tn-passes*)))
              (setq did-something t)))

          (do ((b (ir2-block-prev last) (ir2-block-prev b))
               (prev last b))
              ((not (eq (ir2-block-block b) block)))
            (when (propagate-live-tns b prev
                                      (< i *max-fast-propagate-live-tn-passes*))
              (setq did-something t)))))

      (unless did-something (return))))

  (values))

;;;; post-pass

;;; Note that TN conflicts with all current live TNs. NUM is TN's LTN
;;; number. We bit-ior LIVE-BITS with TN's LOCAL-CONFLICTS, and set TN's
;;; number in the conflicts of all TNs in LIVE-LIST.
(defun note-conflicts (live-bits live-list tn num)
  (declare (type tn tn) (type (or tn null) live-list)
           (type local-tn-bit-vector live-bits)
           (type local-tn-number num))
  (let ((lconf (tn-local-conflicts tn)))
    (bit-ior live-bits lconf lconf))
  (do ((live live-list (tn-next* live)))
      ((null live))
    (setf (sbit (tn-local-conflicts live) num) 1))
  (values))

;;; Compute a bit vector of the TNs live after VOP that aren't results.
(defun compute-save-set (vop live-bits)
  (declare (type vop vop) (type local-tn-bit-vector live-bits))
  (let ((live (bit-vector-copy live-bits)))
    (do ((r (vop-results vop) (tn-ref-across r)))
        ((null r))
      (let ((tn (tn-ref-tn r)))
        (ecase (tn-kind tn)
          ((:normal :debug-environment)
           (setf (sbit live (tn-local-number tn)) 0))
          (:environment :component)
          (:unused))))
    live))

;;; This is used to determine whether a :DEBUG-ENVIRONMENT TN should
;;; be considered live at block end. We return true if a VOP with
;;; non-null SAVE-P appears before the first read of TN (hence is seen
;;; first in our backward scan.)
(defun saved-after-read (tn block)
  (do ((vop (ir2-block-last-vop block) (vop-prev vop)))
      ((null vop) t)
    (when (vop-info-save-p (vop-info vop)) (return t))
    (when (find-in #'tn-ref-across tn (vop-args vop) :key #'tn-ref-tn)
      (return nil))))

;;; If the block has no successors, or its successor is the component
;;; tail, then all :DEBUG-ENVIRONMENT TNs are always added, regardless
;;; of whether they appeared to be live. This ensures that these TNs
;;; are considered to be live throughout blocks that read them, but
;;; don't have any interesting successors (such as a return or tail
;;; call.) In this case, we set the corresponding bit in LIVE-IN as
;;; well.
(defun make-debug-environment-tns-live (block live-bits live-list)
  (let* ((1block (ir2-block-block block))
         (live-in (ir2-block-live-in block))
         (succ (block-succ 1block))
         (next (ir2-block-next block)))
    (when (and next
               (not (eq (ir2-block-block next) 1block))
               (or (null succ)
                   (eq (first succ)
                       (component-tail (block-component 1block)))
                   (let ((node (block-last 1block)))
                     (and (combination-p node)
                          (eq (combination-kind node) :local)
                          (node-tail-p node)))))
      (do ((conf (ir2-block-global-tns block)
                 (global-conflicts-next-blockwise conf)))
          ((null conf))
        (let* ((tn (global-conflicts-tn conf))
               (num (global-conflicts-number conf)))
          (when (and num (zerop (sbit live-bits num))
                     (eq (tn-kind tn) :debug-environment)
                     (eq (tn-environment tn) (cached-block-environment 1block))
                     (saved-after-read tn block))
            (note-conflicts live-bits live-list tn num)
            (setf (sbit live-bits num) 1)
            (push-in tn-next* tn live-list)
            (setf (sbit live-in num) 1))))))

  (values live-bits live-list))

;;; Return as values, a LTN bit-vector and a list (threaded by
;;; TN-NEXT*) representing the TNs live at the end of BLOCK (exclusive
;;; of :LIVE TNs).
;;;
;;; We iterate over the TNs in the global conflicts that are live at
;;; the block end, setting up the TN-LOCAL-CONFLICTS and
;;; TN-LOCAL-NUMBER, and adding the TN to the live list.
;;;
;;; If a :MORE result is not live, we effectively fake a read to it.
;;; This is part of the action described in ENSURE-RESULTS-LIVE.
;;;
;;; At the end, we call MAKE-DEBUG-ENVIRONEMNT-TNS-LIVE to make debug
;;; environment TNs appear live when appropriate, even when they
;;; aren't.
;;;
;;; ### Note: we alias the global-conflicts-conflicts here as the
;;; tn-local-conflicts.
(defun compute-initial-conflicts (block)
  (declare (type ir2-block block))
  (let* ((live-in (ir2-block-live-in block))
         (ltns (ir2-block-local-tns block))
         (live-bits (bit-vector-copy live-in))
         (live-list nil))

    (do ((conf (ir2-block-global-tns block)
               (global-conflicts-next-blockwise conf)))
        ((null conf))
      (let ((bits (global-conflicts-conflicts conf))
            (tn (global-conflicts-tn conf))
            (num (global-conflicts-number conf))
            (kind (global-conflicts-kind conf)))
        (setf (tn-local-number tn) num)
        (unless (eq kind :live)
          (cond ((not (zerop (sbit live-bits num)))
                 (bit-vector-replace bits live-bits)
                 (setf (sbit bits num) 0)
                 (push-in tn-next* tn live-list))
                ((and (eq (svref ltns num) :more)
                      (eq kind :write))
                 (note-conflicts live-bits live-list tn num)
                 (setf (sbit live-bits num) 1)
                 (push-in tn-next* tn live-list)
                 (setf (sbit live-in num) 1)))

          (setf (tn-local-conflicts tn) bits))))

    (make-debug-environment-tns-live block live-bits live-list)))

;;; A function called in CONFLICT-ANALYZE-1-BLOCK when we have a VOP
;;; with SAVE-P true. We compute the save-set, and if :FORCE-TO-STACK,
;;; force all the live TNs to be stack environment TNs.
(defun conflictize-save-p-vop (vop block live-bits)
  (declare (type vop vop) (type ir2-block block)
           (type local-tn-bit-vector live-bits))
  (let ((ss (compute-save-set vop live-bits)))
    (setf (vop-save-set vop) ss)
    (when (eq (vop-info-save-p (vop-info vop)) :force-to-stack)
      (do-live-tns (tn ss block)
        (unless (eq (tn-kind tn) :component)
          (force-tn-to-stack tn)
          (unless (eq (tn-kind tn) :environment)
            (convert-to-environment-tn
             tn
             (cached-block-environment (ir2-block-block block))))))))
  (values))

;;; This is used in SCAN-VOP-REFS to simultaneously do something to
;;; all of the TNs referenced by a big more arg. We have to treat
;;; these TNs specially, since when we set or clear the bit in the
;;; live TNs, the represents a change in the liveness of all the more
;;; TNs. If we iterated as normal, the next more ref would be thought
;;; to be not live when it was, etc. We update Ref to be the last
;;; :more ref we scanned, so that the main loop will step to the next
;;; non-more ref.
(defmacro frob-more-tns (action)
  `(when (eq (svref ltns num) :more)
     (let ((prev ref))
       (do ((mref (tn-ref-next-ref ref) (tn-ref-next-ref mref)))
           ((null mref))
         (let* ((mtn (tn-ref-tn mref))
                (tn-number (tn-local-number mtn)))
           (when tn-number
             (unless (eql tn-number num)
               (return))
             ,action
             (setq prev mref)
             (setq ref prev)))))))

;;; Handle the part of CONFLICT-ANALYZE-1-BLOCK that scans the REFs
;;; for the current VOP. This macro shamelessly references free
;;; variables in C-A-1-B.
(defmacro scan-vop-refs ()
  '(do ((ref (vop-refs vop) (tn-ref-next-ref ref)))
       ((null ref))
     (let* ((tn (tn-ref-tn ref))
            (num (tn-local-number tn)))
       (cond
        ((not num))
        ((not (zerop (sbit live-bits num)))
         (when (tn-ref-write-p ref)
           (setf (sbit live-bits num) 0)
           (deletef-in tn-next* live-list tn)
           (frob-more-tns (deletef-in tn-next* live-list mtn))))
        (t
         (aver (not (tn-ref-write-p ref)))
         (note-conflicts live-bits live-list tn num)
         (frob-more-tns (note-conflicts live-bits live-list mtn num))
         (setf (sbit live-bits num) 1)
         (push-in tn-next* tn live-list)
         (frob-more-tns (push-in tn-next* mtn live-list)))))))

;;; This macro is called by CONFLICT-ANALYZE-1-BLOCK to scan the
;;; current VOP's results, and make any dead ones live. This is
;;; necessary, since even though a result is dead after the VOP, it
;;; may be in use for an extended period within the VOP (especially if
;;; it has :FROM specified.) During this interval, temporaries must be
;;; noted to conflict with the result. More results are finessed in
;;; COMPUTE-INITIAL-CONFLICTS, so we ignore them here.
(defmacro ensure-results-live ()
  '(do ((res (vop-results vop) (tn-ref-across res)))
       ((null res))
     (let* ((tn (tn-ref-tn res))
            (num (tn-local-number tn)))
       (when (and num (zerop (sbit live-bits num)))
         (unless (eq (svref ltns num) :more)
           (note-conflicts live-bits live-list tn num)
           (setf (sbit live-bits num) 1)
           (push-in tn-next* tn live-list))))))

;;; Compute the block-local conflict information for BLOCK. We iterate
;;; over all the TN-REFs in a block in reference order, maintaining
;;; the set of live TNs in both a list and a bit-vector
;;; representation.
(defun conflict-analyze-1-block (block)
  (declare (type ir2-block block))
  (multiple-value-bind (live-bits live-list)
      (compute-initial-conflicts block)
    (let ((ltns (ir2-block-local-tns block)))
      (do ((vop (ir2-block-last-vop block)
                (vop-prev vop)))
          ((null vop))
        (when (vop-info-save-p (vop-info vop))
          (conflictize-save-p-vop vop block live-bits))
        (ensure-results-live)
        (scan-vop-refs)))))

;;; Conflict analyze each block, and also add it.
(defun lifetime-post-pass (component)
  (declare (type component component))
  (do-ir2-blocks (block component)
    (conflict-analyze-1-block block)))

;;;; alias TN stuff

;;; Destructively modify OCONF to include the conflict information in CONF.
(defun merge-alias-block-conflicts (conf oconf)
  (declare (type global-conflicts conf oconf))
  (let* ((kind (global-conflicts-kind conf))
         (num (global-conflicts-number conf))
         (okind (global-conflicts-kind oconf))
         (onum (global-conflicts-number oconf))
         (block (global-conflicts-block oconf))
         (ltns (ir2-block-local-tns block)))
    (cond
     ((eq okind :live))
     ((eq kind :live)
      (setf (global-conflicts-kind oconf) :live)
      (setf (svref ltns onum) nil)
      (setf (global-conflicts-number oconf) nil))
     (t
      (unless (eq kind okind)
        (setf (global-conflicts-kind oconf) :read))
      ;; Make original conflict with all the local TNs the alias
      ;; conflicted with.
      (bit-ior (global-conflicts-conflicts oconf)
               (global-conflicts-conflicts conf)
               t)
      (flet ((frob (x)
               (unless (zerop (sbit x num))
                 (setf (sbit x onum) 1))))
        ;; Make all the local TNs that conflicted with the alias
        ;; conflict with the original.
        (dotimes (i (ir2-block-local-tn-count block))
          (let ((tn (svref ltns i)))
            (when (and tn (not (eq tn :more))
                       (null (tn-global-conflicts tn)))
              (frob (tn-local-conflicts tn)))))
        ;; Same for global TNs...
        (do ((current (ir2-block-global-tns block)
                      (global-conflicts-next-blockwise current)))
            ((null current))
          (unless (eq (global-conflicts-kind current) :live)
            (frob (global-conflicts-conflicts current))))
        ;; Make the original TN live everywhere that the alias was live.
        (frob (ir2-block-written block))
        (frob (ir2-block-live-in block))
        (frob (ir2-block-live-out block))
        (do ((vop (ir2-block-start-vop block)
                  (vop-next vop)))
            ((null vop))
          (let ((sset (vop-save-set vop)))
            (when sset (frob sset)))))))
    ;; Delete the alias's conflict info.
    (when num
      (setf (svref ltns num) nil))
    (deletef-in global-conflicts-next-blockwise
                (ir2-block-global-tns block)
                conf))

  (values))

;;; Co-opt CONF to be a conflict for TN.
(defun change-global-conflicts-tn (conf new)
  (declare (type global-conflicts conf) (type tn new))
  (setf (global-conflicts-tn conf) new)
  (let ((ltn-num (global-conflicts-number conf))
        (block (global-conflicts-block conf)))
    (deletef-in global-conflicts-next-blockwise
                (ir2-block-global-tns block)
                conf)
    (setf (global-conflicts-next-blockwise conf) nil)
    (insert-block-global-conflict conf block)
    (when ltn-num
      (setf (svref (ir2-block-local-tns block) ltn-num) new)))
  (values))

;;; Do CONVERT-TO-GLOBAL on TN if it has no global conflicts. Copy the
;;; local conflicts into the global bit vector.
(defun ensure-global-tn (tn)
  (declare (type tn tn))
  (cond ((tn-global-conflicts tn))
        ((tn-local tn)
         (convert-to-global tn)
         (bit-ior (global-conflicts-conflicts (tn-global-conflicts tn))
                  (tn-local-conflicts tn)
                  t))
        (t
         (aver (and (null (tn-reads tn)) (null (tn-writes tn))))))
  (values))

;;; For each :ALIAS TN, destructively merge the conflict info into the
;;; original TN and replace the uses of the alias.
;;;
;;; For any block that uses only the alias TN, just insert that
;;; conflict into the conflicts for the original TN, changing the LTN
;;; map to refer to the original TN. This gives a result
;;; indistinguishable from the what there would have been if the
;;; original TN had always been referenced. This leaves no sign that
;;; an alias TN was ever involved.
;;;
;;; If a block has references to both the alias and the original TN,
;;; then we call MERGE-ALIAS-BLOCK-CONFLICTS to combine the conflicts
;;; into the original conflict.
(defun merge-alias-conflicts (component)
  (declare (type component component))
  (do ((tn (ir2-component-alias-tns (component-info component))
           (tn-next tn)))
      ((null tn))
    (let ((original (tn-save-tn tn)))
      (ensure-global-tn tn)
      (ensure-global-tn original)
      (setf (tn-vertex original) nil)
      (let ((conf (tn-global-conflicts tn))
            (oconf (tn-global-conflicts original))
            (oprev nil))
        (loop
          (unless oconf
            (if oprev
                (setf (global-conflicts-next-tnwise oprev) conf)
                (setf (tn-global-conflicts original) conf))
            (do ((current conf (global-conflicts-next-tnwise current)))
                ((null current))
              (change-global-conflicts-tn current original))
            (return))
          (let* ((block (global-conflicts-block conf))
                 (num (ir2-block-number block))
                 (onum (ir2-block-number (global-conflicts-block oconf))))

            (cond ((< onum num)
                   (shiftf oprev oconf (global-conflicts-next-tnwise oconf)))
                  ((> onum num)
                   (if oprev
                       (setf (global-conflicts-next-tnwise oprev) conf)
                       (setf (tn-global-conflicts original) conf))
                   (change-global-conflicts-tn conf original)
                   (shiftf oprev
                           conf
                           (global-conflicts-next-tnwise conf)
                           oconf))
                  (t
                   (merge-alias-block-conflicts conf oconf)
                   (shiftf oprev oconf (global-conflicts-next-tnwise oconf))
                   (setf conf (global-conflicts-next-tnwise conf)))))
          (unless conf (return))))

      (flet ((frob (refs)
               (let ((ref refs)
                     (next nil))
                 (loop
                   (unless ref (return))
                   (setq next (tn-ref-next ref))
                   (change-tn-ref-tn ref original)
                   (setq ref next)))))
        (frob (tn-reads tn))
        (frob (tn-writes tn)))
      (setf (tn-global-conflicts tn) nil)))

  (values))

;;; On high debug levels, for all variables that a lambda closes over
;;; convert the TNs to :ENVIRONMENT TNs (in the environment of that
;;; lambda). This way the debugger can display the variables.
(defun maybe-environmentalize-closure-tns (component)
  (dolist (lambda (component-lambdas component))
    (when (policy lambda (>= debug 2))
      (let ((env (lambda-environment lambda)))
        (dolist (closure-var (environment-closure env))
          (let ((tn (find-in-environment closure-var env)))
            (when (member (tn-kind tn) '(:normal :debug-environment))
              (convert-to-environment-tn tn env))))))))


(defun lifetime-analyze (component)
  (lifetime-pre-pass component)
  (maybe-environmentalize-closure-tns component)
  (setup-environment-live-conflicts component)
  (lifetime-flow-analysis component)
  (lifetime-post-pass component)
  (merge-alias-conflicts component))

;;;; conflict testing

;;; Test for a conflict between the local TN X and the global TN Y. We
;;; just look for a global conflict of Y in X's block, and then test
;;; for conflict in that block.
;;;
;;; [### Might be more efficient to scan Y's global conflicts. This
;;; depends on whether there are more global TNs than blocks.]
(defun tns-conflict-local-global (x y)
  (let ((block (tn-local x)))
    (do ((conf (ir2-block-global-tns block)
               (global-conflicts-next-blockwise conf)))
        ((null conf) nil)
      (when (eq (global-conflicts-tn conf) y)
        (let ((num (global-conflicts-number conf)))
          (return (or (not num)
                      (not (zerop (sbit (tn-local-conflicts x)
                                        num))))))))))

;;; Test for conflict between two global TNs X and Y.
(defun tns-conflict-global-global (x y)
  (declare (type tn x y))
  (let* ((x-conf (tn-global-conflicts x))
         (x-num (ir2-block-number (global-conflicts-block x-conf)))
         (y-conf (tn-global-conflicts y))
         (y-num (ir2-block-number (global-conflicts-block y-conf))))

    (macrolet ((advance (n c)
                 `(progn
                    (setq ,c (global-conflicts-next-tnwise ,c))
                    (unless ,c (return-from tns-conflict-global-global nil))
                    (setq ,n (ir2-block-number (global-conflicts-block ,c)))))
               (scan (g l lc)
                 `(do ()
                      ((>= ,l ,g))
                    (advance ,l ,lc))))

      (loop
        ;; x-conf, y-conf true, x-num, y-num corresponding block numbers.
        (scan x-num y-num y-conf)
        (scan y-num x-num x-conf)
        (when (= x-num y-num)
          (let ((ltn-num-x (global-conflicts-number x-conf)))
            (unless (and ltn-num-x
                         (global-conflicts-number y-conf)
                         (zerop (sbit (global-conflicts-conflicts y-conf)
                                      ltn-num-x)))
              (return t))
            (advance x-num x-conf)
            (advance y-num y-conf)))))))

;;; Return true if X and Y are distinct and the lifetimes of X and Y
;;; overlap at any point.
(defun tns-conflict (x y)
  (declare (type tn x y))
  (let ((x-kind (tn-kind x))
        (y-kind (tn-kind y)))
    (cond ((eq x y) nil)
          ((or (eq x-kind :component) (eq y-kind :component)) t)
          ((tn-global-conflicts x)
           (if (tn-global-conflicts y)
               (tns-conflict-global-global x y)
               (tns-conflict-local-global y x)))
          ((tn-global-conflicts y)
           (tns-conflict-local-global x y))
          (t
           (and (eq (tn-local x) (tn-local y))
                (not (zerop (sbit (tn-local-conflicts x)
                                  (tn-local-number y)))))))))
