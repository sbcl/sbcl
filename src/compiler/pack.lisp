;;;; This file contains the implementation-independent code for Pack
;;;; phase in the compiler. Pack is responsible for assigning TNs to
;;;; storage allocations or "register allocation".

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-REGALLOC")

;;; for debugging: some parameters controlling which optimizations we
;;; attempt
(defvar *pack-assign-costs* t)
(defvar *pack-optimize-saves* t)

(declaim (ftype (function (component) index) ir2-block-count))

;;;; conflict determination

;;; Return true if TN has a conflict in SC at the specified offset.
;;; -- If a component-live TN (:COMPONENT kind), then iterate over
;;;    all the blocks. If the element at OFFSET is used anywhere in
;;;    any of the component's blocks (always-live /= 0), then there
;;;    is a conflict.
;;; -- If TN is global (Confs true), then iterate over the blocks TN
;;;    is live in (using TN-GLOBAL-CONFLICTS). If the TN is live
;;;    everywhere in the block (:LIVE), then there is a conflict
;;;    if the element at offset is used anywhere in the block
;;;    (Always-Live /= 0). Otherwise, we use the local TN number for
;;;    TN in block to find whether TN has a conflict at Offset in
;;;    that block.
;;; -- If TN is local, then we just check for a conflict in the block
;;;    it is local to.
;;;
;;; If there is a conflict, returns the first such conflicting offset.
(declaim (ftype (sfunction (tn storage-class index) (or null index)) conflicts-in-sc))
(defun conflicts-in-sc (tn sc offset)
  (let* ((confs (tn-global-conflicts tn))
         (kind (tn-kind tn))
         (sb (sc-sb sc))
         (sb-conflicts (finite-sb-conflicts sb))
         (sb-always-live (finite-sb-always-live sb))
         (size (sc-element-size sc)))
    (macrolet ((do-offsets ((var) &body body)
                 `(loop for ,var upfrom offset
                        repeat size
                        thereis (progn ,@body))))
      (cond
        ((eq kind :component)
         (do-offsets (offset-iter)
             (let ((loc-live (svref sb-always-live offset-iter)))
               (dotimes (i (ir2-block-count *component-being-compiled*))
                 (when (/= (sbit loc-live i) 0)
                   (return offset-iter))))))
        (confs
         ;; TN is global, iterate over the blocks TN is live in.
         (do ((conf confs (global-conflicts-next-tnwise conf)))
             ((null conf)
              nil)
           (let* ((block (global-conflicts-block conf))
                  (num (ir2-block-number block)))
             (if (eq (global-conflicts-kind conf) :live)
                 (do-offsets (offset-iter)
                     (let ((loc-live (svref sb-always-live offset-iter)))
                       (when (/= (sbit loc-live num) 0)
                         (return-from conflicts-in-sc offset-iter))))
                 (do-offsets (offset-iter)
                     (let ((loc-confs (svref sb-conflicts offset-iter)))
                       (when (/= (sbit (svref loc-confs num)
                                       (global-conflicts-number conf))
                                 0)
                         (return-from conflicts-in-sc offset-iter))))))))
        (t
         (do-offsets (offset-iter)
             (and (/= (sbit (svref (svref sb-conflicts offset-iter)
                                   (ir2-block-number (tn-local tn)))
                            (tn-local-number tn))
                      0)
                  offset-iter)))))))

;;; Add TN's conflicts into the conflicts for the location at OFFSET
;;; in SC. We iterate over each location in TN, adding to the
;;; conflicts for that location:
;;; -- If TN is a :COMPONENT TN, then iterate over all the blocks,
;;;    setting all of the local conflict bits and the always-live bit.
;;;    This records a conflict with any TN that has a LTN number in
;;;    the block, as well as with :ALWAYS-LIVE and :ENVIRONMENT TNs.
;;; -- If TN is global, then iterate over the blocks TN is live in. In
;;;    addition to setting the always-live bit to represent the conflict
;;;    with TNs live throughout the block, we also set bits in the
;;;    local conflicts. If TN is :ALWAYS-LIVE in the block, we set all
;;;    the bits, otherwise we OR in the local conflict bits.
;;; -- If the TN is local, then we just do the block it is local to,
;;;    setting always-live and OR'ing in the local conflicts.
(defun add-location-conflicts (tn sc offset)
  (declare (type tn tn) (type storage-class sc) (type index offset))
  (let ((confs (tn-global-conflicts tn))
        (sb (sc-sb sc))
        (kind (tn-kind tn)))
    (dotimes (i (sc-element-size sc))
      (declare (type index i))
      (let* ((this-offset (+ offset i))
             (loc-confs (svref (finite-sb-conflicts sb) this-offset))
             (loc-live (svref (finite-sb-always-live sb) this-offset)))
        (cond
         ((eq kind :component)
          (dotimes (num (ir2-block-count *component-being-compiled*))
            (declare (type index num))
            (setf (sbit loc-live num) 1)
            (set-bit-vector (svref loc-confs num))))
         (confs
          (do ((conf confs (global-conflicts-next-tnwise conf)))
              ((null conf))
            (let* ((block (global-conflicts-block conf))
                   (num (ir2-block-number block))
                   (local-confs (svref loc-confs num)))
              (declare (type local-tn-bit-vector local-confs))
              (setf (sbit loc-live num) 1)
              (if (eq (global-conflicts-kind conf) :live)
                  (set-bit-vector local-confs)
                  (bit-ior local-confs (global-conflicts-conflicts conf) t)))))
         (t
          (let ((num (ir2-block-number (tn-local tn))))
            (setf (sbit loc-live num) 1)
            (bit-ior (the local-tn-bit-vector (svref loc-confs num))
                     (tn-local-conflicts tn) t)))))))
  (values))

;; A rought measure of how much a given OFFSET in SB is currently
;; used. Current implementation counts the amount of blocks where the
;; offset has been marked as ALWAYS-LIVE.
(defun find-location-usage (sb offset)
  (declare (optimize speed))
  (declare (type storage-base sb) (type index offset))
  (let* ((always-live (svref (finite-sb-always-live sb) offset)))
    (declare (simple-bit-vector always-live))
    (count 1 always-live)))

;;; Return the total number of IR2-BLOCKs in COMPONENT.
(defun ir2-block-count (component)
  (declare (type component component))
  (do ((2block (block-info (block-next (component-head component)))
               (ir2-block-next 2block)))
      ((null 2block)
       (error "What?  No ir2 blocks have a non-nil number?"))
    (when (ir2-block-number 2block)
      (return (1+ (ir2-block-number 2block))))))

;;; Ensure that the conflicts vectors for each :FINITE SB are large
;;; enough for the number of blocks allocated. Also clear any old
;;; conflicts and reset the current size to the initial size.
(defun init-sb-vectors (component)
  (let ((nblocks (ir2-block-count component)))
    (dovector (sb *backend-sbs*)
      (unless (eq (sb-kind sb) :non-packed)
        (let* ((conflicts (finite-sb-conflicts sb))
               (always-live (finite-sb-always-live sb))
               (max-locs (length conflicts))
               (last-count (finite-sb-last-block-count sb)))
          (unless (zerop max-locs)
            (let ((current-size (length (the simple-vector
                                             (svref conflicts 0)))))
              (cond
               ((> nblocks current-size)
                (let ((new-size (max nblocks (* current-size 2))))
                  (declare (type index new-size))
                  (dotimes (i max-locs)
                    (declare (type index i))
                    (let ((new-vec (make-array new-size)))
                      (let ((old (svref conflicts i)))
                        (declare (simple-vector old))
                        (dotimes (j current-size)
                          (declare (type index j))
                          (setf (svref new-vec j)
                                (clear-bit-vector (svref old j)))))

                      (do ((j current-size (1+ j)))
                          ((= j new-size))
                        (declare (type index j))
                        (setf (svref new-vec j)
                              (make-array local-tn-limit :element-type 'bit
                                          :initial-element 0)))
                      (setf (svref conflicts i) new-vec))
                    (setf (svref always-live i)
                          (make-array new-size :element-type 'bit
                                               :initial-element 0)))))
               (t
                (dotimes (i (finite-sb-current-size sb))
                  (declare (type index i))
                  (let ((conf (svref conflicts i)))
                    (declare (simple-vector conf))
                    (dotimes (j last-count)
                      (declare (type index j))
                      (clear-bit-vector (svref conf j))))
                  (clear-bit-vector (svref always-live i)))))))

          (setf (finite-sb-last-block-count sb) nblocks)
          (setf (finite-sb-current-size sb) (sb-size sb))
          (setf (finite-sb-last-offset sb) 0)
          (setf (finite-sb-wired-map sb) 0))))))

;;; Expand the :UNBOUNDED SB backing SC by either the initial size or
;;; the SC element size, whichever is larger. If NEEDED-SIZE is
;;; larger, then use that size.
(defun grow-sc (sc &optional (needed-size 0))
  (declare (type storage-class sc) (type index needed-size))
  (let* ((sb (sc-sb sc))
         (size (finite-sb-current-size sb))
         (align-mask (1- (sc-alignment sc)))
         (inc (max (finite-sb-size-increment sb)
                   (+ (sc-element-size sc)
                      (- (logandc2 (+ size align-mask) align-mask)
                         size))
                   (- needed-size size)))
         (new-size (let ((align-mask (1- (finite-sb-size-alignment sb))))
                     (logandc2 (+  size inc align-mask) align-mask)))
         (conflicts (finite-sb-conflicts sb))
         (block-size (if (zerop (length conflicts))
                         (ir2-block-count *component-being-compiled*)
                         (length (the simple-vector (svref conflicts 0)))))
         (padded-size (ash 1 (integer-length (1- new-size)))))
    (declare (type index inc new-size padded-size))
    (aver (eq (sb-kind sb) :unbounded))

    (when (> padded-size (length conflicts))
      (let ((new-conf (make-array padded-size)))
        (replace new-conf conflicts)
        (do ((i size (1+ i)))
            ((= i padded-size))
          (declare (type index i))
          (let ((loc-confs (make-array block-size)))
            (dotimes (j block-size)
              (setf (svref loc-confs j)
                    (make-array local-tn-limit
                                :initial-element 0
                                :element-type 'bit)))
            (setf (svref new-conf i) loc-confs)))
        (setf (finite-sb-conflicts sb) new-conf))

      (let ((new-live (make-array padded-size)))
        (replace new-live (finite-sb-always-live sb))
        (do ((i size (1+ i)))
            ((= i padded-size))
          (setf (svref new-live i)
                (make-array block-size
                            :initial-element 0
                            :element-type 'bit)))
        (setf (finite-sb-always-live sb) new-live))

      (let ((new-tns (make-array padded-size :initial-element nil)))
        (replace new-tns (finite-sb-live-tns sb))
        (fill (finite-sb-live-tns sb) nil)
        (setf (finite-sb-live-tns sb) new-tns)))

    (setf (finite-sb-current-size sb) new-size))
  (values))


;;;; internal errors

;;; Give someone a hard time because there isn't any load function
;;; defined to move from SRC to DEST.
(defun no-load-fun-error (src dest)
  (let* ((src-sc (tn-sc src))
         (src-name (sc-name src-sc))
         (dest-sc (tn-sc dest))
         (dest-name (sc-name dest-sc)))
    (cond ((eq (sb-kind (sc-sb src-sc)) :non-packed)
           (unless (member src-sc (sc-constant-scs dest-sc))
             (error "loading from an invalid constant SC?~@
                     VM definition inconsistent, try recompiling."))
           (error "no load function defined to load SC ~S ~
                   from its constant SC ~S"
                  dest-name src-name))
          ((member src-sc (sc-alternate-scs dest-sc))
           (error "no load function defined to load SC ~S from its ~
                   alternate SC ~S"
                  dest-name src-name))
          ((member dest-sc (sc-alternate-scs src-sc))
           (error "no load function defined to save SC ~S in its ~
                   alternate SC ~S"
                  src-name dest-name))
          (t
           ;; FIXME: "VM definition is inconsistent" shouldn't be a
           ;; possibility in SBCL.
           (error "loading to/from SCs that aren't alternates?~@
                   VM definition is inconsistent, try recompiling.")))))

;;; Called when we failed to pack TN. If RESTRICTED is true, then we
;;; are restricted to pack TN in its SC.
(defun failed-to-pack-error (tn restricted)
  (declare (type tn tn))
  (let* ((sc (tn-sc tn))
         (scs (cons sc (sc-alternate-scs sc))))
    (cond
     (restricted
      (error "failed to pack restricted TN ~S in its SC ~S"
             tn (sc-name sc)))
     (t
      (aver (not (find :unbounded scs
                       :key (lambda (x) (sb-kind (sc-sb x))))))
      (let ((ptype (tn-primitive-type tn)))
        (cond
         (ptype
          (aver (member (sc-number sc) (primitive-type-scs ptype)))
          (error "SC ~S doesn't have any :UNBOUNDED alternate SCs, but is~@
                  a SC for primitive-type ~S."
                 (sc-name sc) (primitive-type-name ptype)))
         (t
          (error "SC ~S doesn't have any :UNBOUNDED alternate SCs."
                 (sc-name sc)))))))))

;;; Return a list of format arguments describing how TN is used in
;;; OP's VOP.
(defun describe-tn-use (loc tn op)
  (let* ((vop (tn-ref-vop op))
         (args (vop-args vop))
         (results (vop-results vop))
         (name (%with-output-to-string (stream)
                 (print-tn-guts tn stream)))
         (2comp (component-info *component-being-compiled*))
         temp)
    (cond
     ((setq temp (position-in #'tn-ref-across tn args :key #'tn-ref-tn))
      `("~2D: ~A (~:R argument)" ,loc ,name ,(1+ temp)))
     ((setq temp (position-in #'tn-ref-across tn results :key #'tn-ref-tn))
      `("~2D: ~A (~:R result)" ,loc ,name ,(1+ temp)))
     ((setq temp (position-in #'tn-ref-across tn args :key #'tn-ref-load-tn))
      `("~2D: ~A (~:R argument load TN)" ,loc ,name ,(1+ temp)))
     ((setq temp (position-in #'tn-ref-across tn results :key
                              #'tn-ref-load-tn))
      `("~2D: ~A (~:R result load TN)" ,loc ,name ,(1+ temp)))
     ((setq temp (position-in #'tn-ref-across tn (vop-temps vop)
                              :key #'tn-ref-tn))
      `("~2D: ~A (temporary ~A)" ,loc ,name
        ,(operand-parse-name (elt (vop-parse-temps (vop-parse-or-lose (vop-name vop)))
                                  temp))))
     ((eq (tn-kind tn) :component)
      `("~2D: ~A (component live)" ,loc ,name))
     ((position-in #'tn-next tn (ir2-component-wired-tns 2comp))
      `("~2D: ~A (wired)" ,loc ,name))
     ((position-in #'tn-next tn (ir2-component-restricted-tns 2comp))
      `("~2D: ~A (restricted)" ,loc ,name))
     (t
      `("~2D: not referenced?" ,loc)))))

(eval-when (:compile-toplevel)
  (assert (zerop (rem sb-vm:finite-sc-offset-limit 8)))) ; multiple of 8

(defmacro do-sc-locations ((location locations &optional result (increment 1))
                           &body body)
  (let ((inc '#:increment)
        (bitmap '#:bits)
        (bias '#:bias))
    (declare (ignorable inc))
    `(let ((,bitmap ,locations)
           (,inc (truly-the (member 1 2 4) ,increment))
           (,bias 0))
       ;; This bitmap won't be a machine word if #+sparc but who cares?
       (declare (type sb-vm:finite-sc-offset-map ,bitmap))
       ;; BIAS can take on the limiting value at its final iteration
       (declare (type (integer 0 ,sb-vm:finite-sc-offset-limit) ,bias))
       (block nil
         #|
         ;; This should be faster since there is no loop nesting
         ;; and only the 1 bits are visited. But it didn't benchmark better.
         ;; Must dig deeper.
         #+(and (or x86 x86-64) (not sb-xc-host))
         (unless (zerop ,bitmap)
           (loop named #:noname
                 do
             (let ((.index. (truly-the
                             (integer 0 (,sb-vm:n-word-bits))
                             (%primitive sb-vm::unsigned-word-find-first-bit ,bitmap))))
               ;; BIAS is the bit index in the original LOCATIONS map
               ;; which is at index 0 in BITMAP
               (let ((,location (truly-the sb-vm:finite-sc-offset (+ .index. ,bias))))
                 ,@body
                 ;; Shift using 2 steps due to the type constraint on digit-logical-shift-right
                 ;; and/or CPU behavior of oversized shift.
                 ;; Consider: 32 bit word and bit 31 (and only that) is on. After processing it,
                 ;; we need to shift out 32 bits to get to the "next" bit. There is no next bit,
                 ;; so the word should become 0. But shifting by 32 = shifting by 0 which won't do
                 ;; the right thing.
                 (setq ,bitmap (sb-bignum:%digit-logical-shift-right
                                 (sb-bignum:%digit-logical-shift-right ,bitmap .index.)
                                 ,inc))
                 (when (zerop ,bitmap) (loop-finish))
                 (incf ,bias (truly-the (integer 0 ,sb-vm:n-word-bits)
                                        (+ .index. ,inc)))))))
         #-(and (or x86 x86-64) (not sb-xc-host))
         |#
         (loop named #:outer repeat (/ sb-vm:finite-sc-offset-limit 8)
               do (when (ldb-test (byte 8 ,bias) ,bitmap)
                    ;; scan 8 bits starting at BIAS
                    (loop named #:inner
                          for ,location from ,bias to (+ ,bias 7) by ,inc
                          when (logbitp ,location ,bitmap) do (progn ,@body)))
                  (incf ,bias 8))
         ,result))))

;;; If load TN packing fails, try to give a helpful error message. We
;;; find a TN in each location that conflicts, and print it.
(defun failed-to-pack-load-tn-error (scs op)
  (declare (list scs) (type tn-ref op))
  (collect ((used)
            (unused))
    (dolist (sc scs)
      (let* ((sb (sc-sb sc))
             (confs (finite-sb-live-tns sb)))
        (aver (eq (sb-kind sb) :finite))
        (do-sc-locations (el (sc-locations sc) nil (sc-element-size sc))
          (let ((conf (load-tn-conflicts-in-sc op sc el t)))
            (if conf
                (used (describe-tn-use el conf op))
                (do ((i el (1+ i))
                     (end (+ el (sc-element-size sc))))
                    ((= i end)
                     (unused el))
                  (declare (type index i end))
                  (let ((victim (svref confs i)))
                    (when victim
                      (used (describe-tn-use el victim op))
                      (return t)))))))))

    (multiple-value-bind (arg-p n more-p costs load-scs incon)
        (get-operand-info op)
      (declare (ignore costs load-scs))
        (aver (not more-p))
        (error "unable to pack a Load-TN in SC ~{~A~#[~^~;, or ~:;,~]~} ~
                for the ~:R ~:[result~;argument~] to~@
                the ~S VOP,~@
                ~:[since all SC elements are in use:~:{~%~@?~}~%~;~
                ~:*but these SC elements are not in use:~%  ~S~%Bug?~*~]~
                ~:[~;~@
                Current cost info inconsistent with that in effect at compile ~
                time. Recompile.~%Compilation order may be incorrect.~]"
               (mapcar #'sc-name scs)
               n arg-p
               (vop-name (tn-ref-vop op))
               (unused) (used)
               incon))))

;;; This is called when none of the SCs that we can load OP into are
;;; allowed by OP's primitive-type.
(defun no-load-scs-allowed-by-primitive-type-error (ref)
  (declare (type tn-ref ref))
  (let* ((tn (tn-ref-tn ref))
         (ptype (tn-primitive-type tn)))
    (multiple-value-bind (arg-p pos more-p costs load-scs incon)
        (get-operand-info ref)
      (declare (ignore costs))
      (aver (not more-p))
      (error "~S is not valid as the ~:R ~:[result~;argument~] to VOP:~
              ~%  ~S,~@
              since the TN's primitive type ~S doesn't allow any of the SCs~@
              allowed by the operand restriction:~%  ~S~
              ~:[~;~@
              Current cost info inconsistent with that in effect at compile ~
              time. Recompile.~%Compilation order may be incorrect.~]"
             tn pos arg-p
             (template-name (vop-info (tn-ref-vop ref)))
             (primitive-type-name ptype)
             (mapcar #'sc-name (listify-restrictions load-scs))
             incon))))

;;;; register saving

(declaim (start-block optimized-emit-saves emit-saves assign-tn-costs
                      pack-save-tn))

;;; Do stuff to note that TN is spilled at VOP for the debugger's benefit.
(defun note-spilled-tn (tn vop)
  (when (and (tn-leaf tn) (vop-save-set vop))
    (let ((2comp (component-info *component-being-compiled*)))
      (setf (gethash tn (ir2-component-spilled-tns 2comp)) t)
      (pushnew tn (gethash vop (ir2-component-spilled-vops 2comp)))))
  (values))

;;; Make a save TN for TN, pack it, and return it. We copy various
;;; conflict information from the TN so that pack does the right
;;; thing.
(defun pack-save-tn (tn)
  (declare (type tn tn))
  (let ((res (make-tn 0 :save nil nil)))
    (dolist (alt (sc-alternate-scs (tn-sc tn))
                 (error "no unbounded alternate for SC ~S"
                        (sc-name (tn-sc tn))))
      (when (eq (sb-kind (sc-sb alt)) :unbounded)
        (setf (tn-save-tn tn) res)
        (setf (tn-save-tn res) tn)
        (setf (tn-sc res) alt)
        (pack-tn res t)
        (return res)))))

;;; Find the load function for moving from SRC to DEST and emit a
;;; MOVE-OPERAND VOP with that function as its info arg.
(defun emit-operand-load (node block src dest before)
  (declare (type node node) (type ir2-block block)
           (type tn src dest) (type (or vop null) before))
  (emit-load-template node block
                      (template-or-lose 'move-operand)
                      src dest
                      (list (or (svref (sc-move-funs (tn-sc dest))
                                       (sc-number (tn-sc src)))
                                (no-load-fun-error src dest)))
                      before)
  (values))

;;; Find the preceding use of the VOP NAME in the emit order, starting
;;; with VOP. We must find the VOP in the same IR1 block.
(defun reverse-find-vop (name vop)
  (do* ((block (vop-block vop) (ir2-block-prev block))
        (last vop (ir2-block-last-vop block)))
       (nil)
    (aver (eq (ir2-block-block block) (ir2-block-block (vop-block vop))))
    (do ((current last (vop-prev current)))
        ((null current))
      (when (eq (vop-name current) name)
        (return-from reverse-find-vop current)))))

;;; For TNs that have other than one writer, we save the TN before
;;; each call. If a local call (MOVE-ARGS is :LOCAL-CALL), then we
;;; scan back for the ALLOCATE-FRAME VOP, and emit the save there.
;;; This is necessary because in a self-recursive local call, the
;;; registers holding the current arguments may get trashed by setting
;;; up the call arguments. The ALLOCATE-FRAME VOP marks a place at
;;; which the values are known to be good.
(defun save-complex-writer-tn (tn vop)
  (let ((save (or (tn-save-tn tn)
                  (pack-save-tn tn)))
        (node (vop-node vop))
        (block (vop-block vop))
        (next (vop-next vop)))
    (when (eq (tn-kind save) :specified-save)
      (setf (tn-kind save) :save))
    (aver (eq (tn-kind save) :save))
    (multiple-value-bind (before block)
        (if (eq (vop-info-move-args (vop-info vop)) :local-call)
            (let ((before (reverse-find-vop 'allocate-frame vop)))
              ;; Because of SPLIT-IR2-BLOCKS the ALLOCATE-FRAME VOP
              ;; may be in a different block.
              (values before (vop-block before)))
            (values vop block))
      (emit-operand-load node block tn save before))
    (emit-operand-load node block save tn next)))

;;; Return a VOP after which is an OK place to save the value of TN.
;;; For correctness, it is only required that this location be after
;;; any possible write and before any possible restore location.
;;;
;;; In practice, we return the unique writer VOP, but give up if the
;;; TN is ever read by a VOP with MOVE-ARGS :LOCAL-CALL. This prevents
;;; us from being confused by non-tail local calls.
;;;
;;; When looking for writes, we have to ignore uses of MOVE-OPERAND,
;;; since they will correspond to restores that we have already done.
(defun find-single-writer (tn)
  (declare (type tn tn))
  (do ((write (tn-writes tn) (tn-ref-next write))
       (res nil))
      ((null write)
       (when (and res
                  (do ((read (tn-reads tn) (tn-ref-next read)))
                      ((not read) t)
                    (when (eq (vop-info-move-args
                               (vop-info
                                (tn-ref-vop read)))
                              :local-call)
                      (return nil))))
         (tn-ref-vop res)))

    (unless (eq (vop-name (tn-ref-vop write)) 'move-operand)
      (when res (return nil))
      (setq res write))))

;;; Try to save TN at a single location. If we succeed, return T,
;;; otherwise NIL.
(defun save-single-writer-tn (tn)
  (declare (type tn tn))
  (let* ((old-save (tn-save-tn tn))
         (save (or old-save (pack-save-tn tn)))
         (writer (find-single-writer tn)))
    (when (and writer
               (or (not old-save)
                   (eq (tn-kind old-save) :specified-save)))
      (emit-operand-load (vop-node writer) (vop-block writer)
                         tn save (vop-next writer))
      (setf (tn-kind save) :save-once)
      t)))

;;; Restore a TN with a :SAVE-ONCE save TN.
(defun restore-single-writer-tn (tn vop)
  (declare (type tn) (type vop vop))
  (let ((save (tn-save-tn tn)))
    (aver (eq (tn-kind save) :save-once))
    (emit-operand-load (vop-node vop) (vop-block vop) save tn (vop-next vop)))
  (values))

;;; Save a single TN that needs to be saved, choosing save-once if
;;; appropriate. This is also called by SPILL-AND-PACK-LOAD-TN.
(defun basic-save-tn (tn vop)
  (declare (type tn tn) (type vop vop))
  (let* ((save (tn-save-tn tn))
         (node (vop-node vop)))
    (flet ((restore ()
             (not
              (and (sb-c::combination-p node)
                   ;; Don't restore if the function doesn't return.
                   (let ((type (sb-c::lvar-fun-type (sb-c::combination-fun node))))
                     (and (fun-type-p type)
                          (eq (sb-kernel:fun-type-returns type) *empty-type*)))
                   #-sb-xc-host
                   (or
                    (sb-c::combination-fun-info node)
                    (policy node (zerop safety)))))))
      (cond ((and save (eq (tn-kind save) :save-once))
             (when (restore)
               (restore-single-writer-tn tn vop)))
            ((save-single-writer-tn tn)
             (when (restore)
               (restore-single-writer-tn tn vop)))
            (t
             (save-complex-writer-tn tn vop))))))

;;; Scan over the VOPs in BLOCK, emiting saving code for TNs noted in
;;; the codegen info that are packed into saved SCs.
(defun emit-saves (block)
  (declare (type ir2-block block))
  (do ((vop (ir2-block-start-vop block) (vop-next vop)))
      ((null vop))
    (when (eq (vop-info-save-p (vop-info vop)) t)
      (do-live-tns (tn (vop-save-set vop) block)
        (when (and (sc-save-p (tn-sc tn))
                   (not (eq (tn-kind tn) :component))
                   ;; Ignore closed over but not read values (due to
                   ;; type propagation)
                   (tn-offset tn))
          (basic-save-tn tn vop)))))

  (values))

;;;; optimized saving

;;; Save TN if it isn't a single-writer TN that has already been
;;; saved. If multi-write, we insert the save BEFORE the specified
;;; VOP. CONTEXT is a VOP used to tell which node/block to use for the
;;; new VOP.
(defun save-if-necessary (tn before context)
  (declare (type tn tn) (type (or vop null) before) (type vop context))
  (let ((save (tn-save-tn tn)))
    (when (eq (tn-kind save) :specified-save)
      (setf (tn-kind save) :save))
    (aver (member (tn-kind save) '(:save :save-once)))
    (unless (eq (tn-kind save) :save-once)
      (or (save-single-writer-tn tn)
          (emit-operand-load (vop-node context) (vop-block context)
                             tn save before))))
  (values))

;;; Load the TN from its save location, allocating one if necessary.
;;; The load is inserted BEFORE the specified VOP. CONTEXT is a VOP
;;; used to tell which node/block to use for the new VOP.
(defun restore-tn (tn before context)
  (declare (type tn tn) (type (or vop null) before) (type vop context))
  (let ((save (or (tn-save-tn tn) (pack-save-tn tn))))
    (emit-operand-load (vop-node context) (vop-block context)
                       save tn before))
  (values))

;;; Start scanning backward at the end of BLOCK, looking which TNs are
;;; live and looking for places where we have to save. We manipulate
;;; two sets: SAVES and RESTORES.
;;;
;;; SAVES is a set of all the TNs that have to be saved because they
;;; are restored after some call. We normally delay saving until the
;;; beginning of the block, but we must save immediately if we see a
;;; write of the saved TN. We also immediately save all TNs and exit
;;; when we see a NOTE-ENVIRONMENT-START VOP, since saves can't be
;;; done before the environment is properly initialized.
;;;
;;; RESTORES is a set of all the TNs read (and not written) between
;;; here and the next call, i.e. the set of TNs that must be restored
;;; when we reach the next (earlier) call VOP. Unlike SAVES, this set
;;; is cleared when we do the restoring after a call. Any TNs that
;;; were in RESTORES are moved into SAVES to ensure that they are
;;; saved at some point.
;;;
;;; SAVES and RESTORES are represented using both a list and a
;;; bit-vector so that we can quickly iterate and test for membership.
;;; The incoming SAVES and RESTORES args are used for computing these
;;; sets (the initial contents are ignored.)
;;;
;;; When we hit a VOP with :COMPUTE-ONLY SAVE-P (an internal error
;;; location), we pretend that all live TNs were read, unless (= speed
;;; 3), in which case we mark all the TNs that are live but not
;;; restored as spilled.
(defun optimized-emit-saves-block (block saves restores)
  (declare (type ir2-block block) (type simple-bit-vector saves restores))
  (let ((1block (ir2-block-block block))
        (saves-list ())
        (restores-list ())
        (skipping nil))
    (declare (list saves-list restores-list))
    (clear-bit-vector saves)
    (clear-bit-vector restores)
    (do-live-tns (tn (ir2-block-live-in block) block)
      (when (and (sc-save-p (tn-sc tn))
                 (not (eq (tn-kind tn) :component)))
        (let ((num (tn-number tn)))
          (setf (sbit restores num) 1)
          (push tn restores-list))))

    (do ((block block (ir2-block-prev block))
         (prev nil block))
        ((not (eq (ir2-block-block block) 1block))
         (aver (not skipping))
         (dolist (save saves-list)
           (let ((start (ir2-block-start-vop prev)))
             (save-if-necessary save start start)))
         prev)
      (do ((vop (ir2-block-last-vop block) (vop-prev vop)))
          ((null vop))
        (let ((info (vop-info vop)))
          (case (vop-name vop)
            (allocate-frame
             (aver skipping)
             (setq skipping nil))
            (note-environment-start
             (aver (not skipping))
             (dolist (save saves-list)
               (save-if-necessary save (vop-next vop) vop))
             (return-from optimized-emit-saves-block block)))

          (unless skipping
            (do ((write (vop-results vop) (tn-ref-across write)))
                ((null write))
              (let* ((tn (tn-ref-tn write))
                     (num (tn-number tn)))
                (unless (zerop (sbit restores num))
                  (setf (sbit restores num) 0)
                  (setq restores-list
                        (delete tn restores-list :test #'eq)))
                (unless (zerop (sbit saves num))
                  (setf (sbit saves num) 0)
                  (save-if-necessary tn (vop-next vop) vop)
                  (setq saves-list
                        (delete tn saves-list :test #'eq))))))

          (macrolet ((save-note-read (tn)
                       `(let* ((tn ,tn)
                               (num (tn-number tn)))
                          (when (and (sc-save-p (tn-sc tn))
                                     (zerop (sbit restores num))
                                     (not (eq (tn-kind tn) :component)))
                          (setf (sbit restores num) 1)
                          (push tn restores-list)))))

            (case (vop-info-save-p info)
              ((t)
               (dolist (tn restores-list)
                 ;; Ignore closed over but not read values (due to
                 ;; type propagation)
                 (when (tn-offset tn)
                   (restore-tn tn (vop-next vop) vop)
                   (let ((num (tn-number tn)))
                     (when (zerop (sbit saves num))
                       (push tn saves-list)
                       (setf (sbit saves num) 1)))))
               (setq restores-list nil)
               (clear-bit-vector restores))
              (:compute-only
               (cond ((policy (vop-node vop) (= speed 3))
                      (do-live-tns (tn (vop-save-set vop) block)
                        (when (zerop (sbit restores (tn-number tn)))
                          (note-spilled-tn tn vop))))
                     (t
                      (do-live-tns (tn (vop-save-set vop) block)
                        (save-note-read tn))))))

            (if (eq (vop-info-move-args info) :local-call)
                (setq skipping t)
                (do ((read (vop-args vop) (tn-ref-across read)))
                    ((null read))
                  (save-note-read (tn-ref-tn read))))))))))

;;; This is like EMIT-SAVES, only different. We avoid redundant saving
;;; within the block, and don't restore values that aren't used before
;;; the next call. This function is just the top level loop over the
;;; blocks in the component, which locates blocks that need saving
;;; done.
(defun optimized-emit-saves (component)
  (declare (type component component))
  (let* ((gtn-count (1+ (ir2-component-global-tn-counter
                         (component-info component))))
         (saves (make-array gtn-count :element-type 'bit))
         (restores (make-array gtn-count :element-type 'bit))
         (block (ir2-block-prev (block-info (component-tail component))))
         (head (block-info (component-head component))))
    (loop
      (when (eq block head) (return))
      (when (do ((vop (ir2-block-start-vop block) (vop-next vop)))
                ((null vop) nil)
              (when (eq (vop-info-save-p (vop-info vop)) t)
                (return t)))
        (setq block (optimized-emit-saves-block block saves restores)))
      (setq block (ir2-block-prev block)))))

;;; Iterate over the normal TNs, finding the cost of packing on the
;;; stack in units of the number of references. We count all read
;;; references as +1, write references as + *tn-write-cost*, and
;;; subtract out REGISTER-SAVE-PENALTY for each place where we would
;;; have to save a register.
;;; The subtraction reflects the fact that having a value in a
;;; register around a call means that code to spill and unspill must
;;; be inserted.
;;;
;;; The costs also take into account the loop depth at which each
;;; reference occurs: the penalty or cost is incremented by the depth
;;; scaled by *tn-loop-depth-multiplier*.  The default (NIL) is to let
;;; this be one more than the max of the cost for reads (1), for write
;;; references and for being live across a call.
(defvar *tn-write-cost* 2)
(defvar *tn-loop-depth-multiplier* nil)

(defun assign-tn-costs (component)
  (let* ((save-penalty *backend-register-save-penalty*)
         (write-cost *tn-write-cost*)
         (depth-scale (or *tn-loop-depth-multiplier*
                          (1+ (max 1 write-cost save-penalty)))))
    (flet ((vop-depth-cost (vop)
             (let ((loop (block-loop
                          (ir2-block-block
                           (vop-block vop)))))
               (if loop
                   (* depth-scale (loop-depth loop))
                   0))))
      (do-ir2-blocks (block component)
        (do ((vop (ir2-block-start-vop block) (vop-next vop)))
            ((null vop))
          (when (eq (vop-info-save-p (vop-info vop)) t)
            (let ((penalty (+ save-penalty (vop-depth-cost vop))))
              (do-live-tns (tn (vop-save-set vop) block)
                #-fp-and-pc-standard-save
                (let ((save-tn (tn-save-tn tn)))
                  (when (and save-tn (eq :specified-save (tn-kind save-tn)))
                    ;; If we're expecting to spill a TN with a
                    ;; specified save slot (the OCFP or LRA save TNs),
                    ;; force it to the stack now.  If we don't, then
                    ;; it might not end up on the stack, which may
                    ;; lead to some useless loads in some code, and
                    ;; will lead to race conditions in the debugger
                    ;; involving backtraces from asynchronous
                    ;; interrupts.
                    (setf (tn-sc tn) (tn-sc save-tn))))
                (decf (tn-cost tn) penalty))))))

      (do ((tn (ir2-component-normal-tns (component-info component))
               (tn-next tn)))
          ((null tn))
        (let ((cost (tn-cost tn)))
          (declare (fixnum cost))
          (do ((ref (tn-reads tn) (tn-ref-next ref)))
              ((null ref))
            (incf cost (1+ (vop-depth-cost (tn-ref-vop ref)))))
          (do ((ref (tn-writes tn) (tn-ref-next ref)))
              ((null ref))
            (incf cost (+ write-cost (vop-depth-cost (tn-ref-vop ref)))))
          (setf (tn-cost tn) cost))))))

;;; If we're not assigning costs, and on a system where it matters, go
;;; through and force TNs with specified save locations (OCFP and LRA
;;; save locations) to the stack if they are going to be spilled.  See
;;; the comment in ASSIGN-TN-COSTS for consequences of not doing so.
#-fp-and-pc-standard-save
(defun maybe-force-specified-saves-to-stack (component)
  (do-ir2-blocks (block component)
    (do ((vop (ir2-block-start-vop block) (vop-next vop)))
        ((null vop))
      (when (eq (vop-info-save-p (vop-info vop)) t)
        (do-live-tns (tn (vop-save-set vop) block)
          (let ((save-tn (tn-save-tn tn)))
            (when (and save-tn (eq :specified-save (tn-kind save-tn)))
              ;; If we're expecting to spill a TN with a
              ;; specified save slot (the OCFP or LRA save TNs),
              ;; force it to the stack now.  If we don't, then
              ;; it might not end up on the stack, which may
              ;; lead to some useless loads in some code, and
              ;; race conditions in the debugger involving
              ;; backtraces from asynchronous interrupts.
              (setf (tn-sc tn) (tn-sc save-tn)))))))))

(declaim (end-block))

;; Misc. utilities
(declaim (maybe-inline unbounded-sc-p))
(defun unbounded-sc-p (sc)
  (eq (sb-kind (sc-sb sc)) :unbounded))

(defun unbounded-tn-p (tn)
  #-sb-xc-host (declare (inline unbounded-sc-p))
  (unbounded-sc-p (tn-sc tn)))


;;;; load TN packing

(declaim (start-block pack-load-tns load-tn-conflicts-in-sc))

;;; These variables indicate the last location at which we computed
;;; the Live-TNs. They hold the BLOCK and VOP values that were passed
;;; to COMPUTE-LIVE-TNS.
(defvar *live-block*)
(defvar *live-vop*)

;;; If we unpack some TNs, then we mark all affected blocks by
;;; sticking them in this hash-table. This is initially null. We
;;; create the hashtable if we do any unpacking.
(defvar *repack-blocks*)
(declaim (type list *repack-blocks*))

;;; Set the LIVE-TNS vectors in all :FINITE SBs to represent the TNs
;;; live at the end of BLOCK.
(defun init-live-tns (block)
  (dovector (sb *backend-sbs*)
    (when (eq (sb-kind sb) :finite)
      (fill (finite-sb-live-tns sb) nil)))

  (do-live-tns (tn (ir2-block-live-in block) block)
    (let* ((sc (tn-sc tn))
           (sb (sc-sb sc)))
      (when (eq (sb-kind sb) :finite)
        ;; KLUDGE: we can have "live" TNs that are neither read
        ;; to nor written from, due to more aggressive (type-
        ;; directed) constant propagation.  Such TNs will never
        ;; be assigned an offset nor be in conflict with anything.
        ;;
        ;; Ideally, it seems to me we could make sure these TNs
        ;; are never allocated in the first place in
        ;; ASSIGN-LAMBDA-VAR-TNS.
        (if (tn-offset tn)
            (do ((offset (tn-offset tn) (1+ offset))
                 (end (+ (tn-offset tn) (sc-element-size sc))))
                ((= offset end))
              (declare (type index offset end))
              (setf (svref (finite-sb-live-tns sb) offset) tn))
            (aver (and (null (tn-reads tn)) (null (tn-writes tn))))))))

  (setq *live-block* block)
  (setq *live-vop* (ir2-block-last-vop block))

  (values))

;;; Set the LIVE-TNs in :FINITE SBs to represent the TNs live
;;; immediately after the evaluation of VOP in BLOCK, excluding
;;; results of the VOP. If VOP is null, then compute the live TNs at
;;; the beginning of the block. Sequential calls on the same block
;;; must be in reverse VOP order.
(defun compute-live-tns (block vop)
  (declare (type ir2-block block) (type vop vop))
  (unless (eq block *live-block*)
    (init-live-tns block))

  (do ((current *live-vop* (vop-prev current)))
      ((eq current vop)
       (do ((res (vop-results vop) (tn-ref-across res)))
           ((null res))
         (unless (eq (tn-kind (tn-ref-tn res)) :unused)
           (let* ((tn (tn-ref-tn res))
                  (sc (tn-sc tn))
                  (sb (sc-sb sc)))
             (when (eq (sb-kind sb) :finite)
               (do ((offset (tn-offset tn) (1+ offset))
                    (end (+ (tn-offset tn) (sc-element-size sc))))
                   ((= offset end))
                 (declare (type index offset end))
                 (setf (svref (finite-sb-live-tns sb) offset) nil)))))))
    (do ((ref (vop-refs current) (tn-ref-next-ref ref)))
        ((null ref))
      (unless (eq (tn-kind (tn-ref-tn ref)) :unused)
       (let ((ltn (tn-ref-load-tn ref)))
         (when ltn
           (let* ((sc (tn-sc ltn))
                  (sb (sc-sb sc)))
             (when (eq (sb-kind sb) :finite)
               (let ((tns (finite-sb-live-tns sb)))
                 (do ((offset (tn-offset ltn) (1+ offset))
                      (end (+ (tn-offset ltn) (sc-element-size sc))))
                     ((= offset end))
                   (declare (type index offset end))
                   (aver (null (svref tns offset)))))))))

       (let* ((tn (tn-ref-tn ref))
              (sc (tn-sc tn))
              (sb (sc-sb sc)))
         (when (eq (sb-kind sb) :finite)
           (let ((tns (finite-sb-live-tns sb)))
             (do ((offset (tn-offset tn) (1+ offset))
                  (end (+ (tn-offset tn) (sc-element-size sc))))
                 ((= offset end))
               (declare (type index offset end))
               (if (tn-ref-write-p ref)
                   (setf (svref tns offset) nil)
                   (let ((old (svref tns offset)))
                     (aver (or (null old) (eq old tn)))
                     (setf (svref tns offset) tn))))))))))

  (setq *live-vop* vop)
  (values))

;;; This is kind of like OFFSET-CONFLICTS-IN-SB, except that it uses
;;; the VOP refs to determine whether a Load-TN for OP could be packed
;;; in the specified location, disregarding conflicts with TNs not
;;; referenced by this VOP. There is a conflict if either:
;;;  1. The reference is a result, and the same location is either:
;;;     -- Used by some other result.
;;;     -- Used in any way after the reference (exclusive).
;;;  2. The reference is an argument, and the same location is either:
;;;     -- Used by some other argument.
;;;     -- Used in any way before the reference (exclusive).
;;;
;;; In 1 (and 2) above, the first bullet corresponds to result-result
;;; (and argument-argument) conflicts. We need this case because there
;;; aren't any TN-REFs to represent the implicit reading of results or
;;; writing of arguments.
;;;
;;; The second bullet corresponds to conflicts with temporaries or
;;; between arguments and results.
;;;
;;; We consider both the TN-REF-TN and the TN-REF-LOAD-TN (if any) to
;;; be referenced simultaneously and in the same way. This causes
;;; load-TNs to appear live to the beginning (or end) of the VOP, as
;;; appropriate.
;;;
;;; We return a conflicting TN if there is a conflict.
(defun load-tn-offset-conflicts-in-sb (op sb offset)
  (declare (type tn-ref op) (type sb-c::finite-sb-template sb) (type index offset))
  (aver (eq (sb-kind sb) :finite))
  (let ((vop (tn-ref-vop op)))
    (labels ((tn-overlaps (tn)
               (let ((sc (tn-sc tn))
                     (tn-offset (tn-offset tn)))
                 (when (and (eq (sc-sb sc) sb)
                            (<= tn-offset offset)
                            (< offset
                               (the index
                                    (+ tn-offset (sc-element-size sc)))))
                   tn)))
             (same (ref)
               (let ((tn (tn-ref-tn ref))
                     (ltn (tn-ref-load-tn ref)))
                 (and (not (eq (tn-kind tn) :unused))
                      (or (tn-overlaps tn)
                          (and ltn (tn-overlaps ltn))))))
             (is-op (ops)
               (do ((ops ops (tn-ref-across ops)))
                   ((null ops) nil)
                 (let ((found (same ops)))
                   (when (and found (not (eq ops op)))
                     (return found)))))
             (is-ref (refs end)
               (do ((refs refs (tn-ref-next-ref refs)))
                   ((eq refs end) nil)
                 (let ((found (same refs)))
                 (when found (return found))))))
      (declare (inline is-op is-ref tn-overlaps))
      (if (tn-ref-write-p op)
          (or (is-op (vop-results vop))
              (is-ref (vop-refs vop) op))
          (or (is-op (vop-args vop))
              (is-ref (tn-ref-next-ref op) nil))))))

;;; Iterate over all the elements in the SB that would be allocated by
;;; allocating a TN in SC at Offset, checking for conflict with
;;; load-TNs or other TNs (live in the LIVE-TNS, which must be set
;;; up.) We also return true if there aren't enough locations after
;;; Offset to hold a TN in SC. If Ignore-Live is true, then we ignore
;;; the live-TNs, considering only references within Op's VOP.
;;;
;;; We return a conflicting TN, or :OVERFLOW if the TN won't fit.
(defun load-tn-conflicts-in-sc (op sc offset ignore-live)
  (let* ((sb (sc-sb sc))
         (size (finite-sb-current-size sb)))
    (do ((i offset (1+ i))
         (end (+ offset (sc-element-size sc))))
        ((= i end) nil)
      (declare (type index i end))
      (let ((res (or (when (>= i size) :overflow)
                     (and (not ignore-live)
                          (svref (finite-sb-live-tns sb) i))
                     (load-tn-offset-conflicts-in-sb op sb i))))
        (when res (return res))))))

;;; If a load-TN for OP is targeted to a legal location in SC, then
;;; return the offset, otherwise return NIL. We see whether the target
;;; of the operand is packed, and try that location. There isn't any
;;; need to chain down the target path, since everything is packed
;;; now.
;;;
;;; We require the target to be in SC (and not merely to overlap with
;;; SC). This prevents SC information from being lost in load TNs (we
;;; won't pack a load TN in ANY-REG when it is targeted to a
;;; DESCRIPTOR-REG.) This shouldn't hurt the code as long as all
;;; relevant overlapping SCs are allowed in the operand SC
;;; restriction.
(defun find-load-tn-target (op sc)
  (declare (inline member))
  (let ((target (tn-ref-target op)))
    (when target
      (let* ((tn (tn-ref-tn target))
             (loc (tn-offset tn)))
        (if (and (eq (tn-sc tn) sc)
                 (sc-locations-member (the sb-vm:finite-sc-offset loc)
                                      (sc-locations sc))
                 (not (load-tn-conflicts-in-sc op sc loc nil)))
            loc
            nil)))))

;;; Select a legal location for a load TN for Op in SC. We just
;;; iterate over the SC's locations. If we can't find a legal
;;; location, return NIL.
(defun select-load-tn-location (op sc)
  (declare (type tn-ref op) (type storage-class sc))

  ;; Check any target location first.
  (let ((target (tn-ref-target op)))
    (when target
      (let* ((tn (tn-ref-tn target))
             (loc (tn-offset tn)))
        (when (and (eq (sc-sb sc) (sc-sb (tn-sc tn)))
                   (sc-locations-member (the sb-vm:finite-sc-offset loc)
                                        (sc-locations sc))
                   (not (load-tn-conflicts-in-sc op sc loc nil)))
              (return-from select-load-tn-location loc)))))

  (do-sc-locations (loc (sc-locations sc) nil (sc-element-size sc))
    (unless (load-tn-conflicts-in-sc op sc loc nil)
      (return loc))))

(defevent unpack-tn "Unpacked a TN to satisfy operand SC restriction.")

;;; Make TN's location the same as for its save TN (allocating a save
;;; TN if necessary.) Delete any save/restore code that has been
;;; emitted thus far. Mark all blocks containing references as needing
;;; to be repacked.
(defun unpack-tn (tn)
  (event unpack-tn)
  (let ((stn (or (tn-save-tn tn)
                 (pack-save-tn tn))))
    (setf (tn-sc tn) (tn-sc stn))
    (setf (tn-offset tn) (tn-offset stn))
    (flet ((zot (refs)
             (do ((ref refs (tn-ref-next ref)))
                 ((null ref))
               (let ((vop (tn-ref-vop ref)))
                 (if (eq (vop-name vop) 'move-operand)
                     (delete-vop vop)
                     (pushnew (vop-block vop) *repack-blocks*))))))
      (zot (tn-reads tn))
      (zot (tn-writes tn))))

  (values))

(defevent unpack-fallback "Unpacked some operand TN.")

;;; This is called by PACK-LOAD-TN where there isn't any location free
;;; that we can pack into. What we do is move some live TN in one of
;;; the specified SCs to memory, then mark all blocks that reference
;;; the TN as needing repacking. If we succeed, we throw to UNPACKED-TN.
;;; If we fail, we return NIL.
;;;
;;; We can unpack any live TN that appears in the NORMAL-TNs list
;;; (isn't wired or restricted.) We prefer to unpack TNs that are not
;;; used by the VOP. If we can't find any such TN, then we unpack some
;;; argument or result TN. The only way we can fail is if all
;;; locations in SC are used by load-TNs or temporaries in VOP.
(defun unpack-for-load-tn (sc op)
  (declare (type storage-class sc) (type tn-ref op))
  (let ((sb (sc-sb sc))
        (normal-tns (ir2-component-normal-tns
                     (component-info *component-being-compiled*)))
        (node (vop-node (tn-ref-vop op)))
        (fallback nil))
    (flet ((unpack-em (victims)
             (pushnew (vop-block (tn-ref-vop op)) *repack-blocks*)
             (dolist (victim victims)
               (event unpack-tn node)
               (unpack-tn victim))
             (throw 'unpacked-tn nil)))
      (do-sc-locations (loc (sc-locations sc) nil (sc-element-size sc))
        (block SKIP
          (collect ((victims nil adjoin))
            (do ((i loc (1+ i))
                 (end (+ loc (sc-element-size sc))))
                ((= i end))
              (declare (type index i end))
              (let ((victim (svref (finite-sb-live-tns sb) i)))
                (when victim
                  (unless (find-in #'tn-next victim normal-tns)
                    (return-from SKIP))
                  (victims victim))))

            (let ((conf (load-tn-conflicts-in-sc op sc loc t)))
              (cond ((not conf)
                     (unpack-em (victims)))
                    ((eq conf :overflow))
                    ((not fallback)
                     (cond ((find conf (victims))
                            (setq fallback (victims)))
                           ((find-in #'tn-next conf normal-tns)
                            (setq fallback (list conf))))))))))

      (when fallback
        (event unpack-fallback node)
        (unpack-em fallback))))

  nil)

;;; Try to pack a load TN in the SCs indicated by Load-SCs. If we run
;;; out of SCs, then we unpack some TN and try again. We return the
;;; packed load TN.
;;;
;;; Note: we allow a Load-TN to be packed in the target location even
;;; if that location is in a SC not allowed by the primitive type.
;;; (The SC must still be allowed by the operand restriction.) This
;;; makes move VOPs more efficient, since we won't do a move from the
;;; stack into a non-descriptor any-reg through a descriptor argument
;;; load-TN. This does give targeting some real semantics, making it
;;; not a pure advisory to pack. It allows pack to do some packing it
;;; wouldn't have done before.
(defun pack-load-tn (load-scs op)
  (declare (type list load-scs) (type tn-ref op))
  (let ((vop (tn-ref-vop op)))
    (compute-live-tns (vop-block vop) vop))

  (let* ((tn (tn-ref-tn op))
         (ptype (tn-primitive-type tn)))
    (let ((current-scs load-scs)
          (allowed ()))
      (loop
        (cond
         ((null current-scs)
          (unless allowed
            (no-load-scs-allowed-by-primitive-type-error op))
          (dolist (sc allowed)
            (unpack-for-load-tn sc op))
          (failed-to-pack-load-tn-error allowed op))
        (t
         (let* ((sc (svref *backend-sc-numbers* (pop current-scs)))
                (target (find-load-tn-target op sc)))
           (when (or target (sc-allowed-by-primitive-type sc ptype))
             (let ((loc (or target
                            (select-load-tn-location op sc))))
               (when loc
                 (let ((res (make-tn 0 :load nil sc)))
                   (setf (tn-offset res) loc)
                   (return res))))
             (push sc allowed)))))))))

;;; Scan a list of load-SCs vectors and a list of TN-REFS threaded by
;;; TN-REF-ACROSS. When we find a reference whose TN doesn't satisfy
;;; the restriction, we pack a Load-TN and load the operand into it.
;;; If a load-tn has already been allocated, we can assume that the
;;; restriction is satisfied.
(declaim (inline check-operand-restrictions))
(defun check-operand-restrictions (scs ops)
  (declare (list scs) (type (or tn-ref null) ops))

  ;; Check the targeted operands first.
  (do ((scs scs (cdr scs))
       (op ops (tn-ref-across op)))
      ((null scs))
    (let ((target (tn-ref-target op))
          (tn (tn-ref-tn op)))
      (when (and target
                 (not (eq (tn-kind tn) :unused))
                 (tn-primitive-type tn))
        (let* ((load-tn (tn-ref-load-tn op))
               (load-scs (svref (car scs)
                                (sc-number
                                 (tn-sc (or load-tn tn))))))
          (if load-tn
              (aver (eq load-scs t))
              (unless (eq load-scs t)
                (setf (tn-ref-load-tn op)
                      (pack-load-tn load-scs op))))))))

  (do ((scs scs (cdr scs))
       (op ops (tn-ref-across op)))
      ((null scs))
    (let ((target (tn-ref-target op))
          (tn (tn-ref-tn op)))
      (unless (or target
                  (eq (tn-kind tn) :unused)
                  (not (tn-primitive-type tn)))
        (let* ((load-tn (tn-ref-load-tn op))
               (load-scs (svref (car scs)
                                (sc-number
                                 (tn-sc (or load-tn tn))))))
          (cond (load-tn
                 (aver (eq load-scs t)))
                (t
                 ;; conditional sc
                 (when (functionp load-scs)
                   (setf load-scs (funcall load-scs tn)))
                 (unless (eq load-scs t)
                   (setf (tn-ref-load-tn op)
                         (pack-load-tn load-scs op)))))))))

  (values))

;;; Scan the VOPs in BLOCK, looking for operands whose SC restrictions
;;; aren't satisfied. We do the results first, since they are
;;; evaluated later, and our conflict analysis is a backward scan.
(defun pack-load-tns (block)
  (catch 'unpacked-tn
    (let ((*live-block* nil)
          (*live-vop* nil))
      (do ((vop (ir2-block-last-vop block) (vop-prev vop)))
          ((null vop))
        (let ((info (vop-info vop)))
          (check-operand-restrictions (vop-info-result-load-scs info)
                                      (vop-results vop))
          (check-operand-restrictions (vop-info-arg-load-scs info)
                                      (vop-args vop))))))
  (values))

;;;; targeting

(declaim (start-block pack pack-tn target-if-desirable
                      ;; needed for pack-iterative
                      pack-wired-tn))

;;; Link the TN-REFS READ and WRITE together using the TN-REF-TARGET
;;; when this seems like a good idea. Currently we always do, as this
;;; increases the success of load-TN targeting.
(defun target-if-desirable (read write)
  (declare (type tn-ref read write))
  ;; As per the comments at the definition of TN-REF-TARGET, read and
  ;; write refs are always paired, with TARGET in the read pointing to
  ;; the write and vice versa.
  (aver (eq (tn-ref-write-p read)
            (not (tn-ref-write-p write))))
  (unless (or (eq (tn-kind (tn-ref-tn write)) :unused)
              (eq (tn-kind (tn-ref-tn read)) :unused))
    (setf (tn-ref-target read) write)
    (setf (tn-ref-target write) read)))

;;; If TN can be packed into SC so as to honor a preference to TARGET,
;;; then return the offset to pack at, otherwise return NIL. TARGET
;;; must be already packed.
(defun check-ok-target (target tn sc)
  (declare (type tn target tn) (type storage-class sc) (inline member))
  (let* ((loc (tn-offset target))
         (target-sc (tn-sc target))
         (target-sb (sc-sb target-sc)))
    (declare (type index loc))
    ;; We can honor a preference if:
    ;; -- TARGET's location is in SC's locations.
    ;; -- The element sizes of the two SCs are the same.
    ;; -- TN doesn't conflict with target's location.
    (if (and (eq target-sb (sc-sb sc))
             (or (eq (sb-kind target-sb) :unbounded)
                 (sc-locations-member loc (sc-locations sc)))
             (= (sc-element-size target-sc) (sc-element-size sc))
             (not (conflicts-in-sc tn sc loc))
             (zerop (mod loc (sc-alignment sc))))
        loc
        nil)))

;;; Scan along the target path from TN, looking at readers or writers.
;;; When we find a TN, call CALLEE with that TN, and then resume
;;; walking down that TN's target.  As soon as there is no target, or
;;; if the TN has multiple readers (writers), we stop walking the
;;; targetting chain. We also always stop after 10 iterations to get
;;; around potential circularity problems.
;;;
;;; Why the single-reader/writer constraint?  As far as I can tell,
;;; this is concerned with straight pipeline of data, e.g. CASTs.  In
;;; that case, limiting to chains of length 10 seems to be more than
;;; enough.
(declaim (inline %call-with-target-tns))
(defun %call-with-target-tns (tn callee
                              &key (limit 10) (reads t) (writes t))
  (declare (type tn tn) (type function callee) (type index limit)
           (dynamic-extent callee))
  (flet ((frob-slot (slot-function)
           (declare (type function slot-function))
           (let ((count limit)
                 (current tn))
             (declare (type index count))
             (loop
              (let ((refs (funcall slot-function current)))
                (unless (and (plusp count)
                             refs
                             (not (tn-ref-next refs)))
                  (return nil))
                (let ((target (tn-ref-target refs)))
                  (unless target (return nil))
                  (setq current (tn-ref-tn target))
                  (funcall callee current)
                  (decf count)))))))
    (when reads
      (frob-slot #'tn-reads))
    (when writes
      (frob-slot #'tn-writes))
    nil))

(defmacro do-target-tns ((target-variable source-tn
                          &rest keys &key limit reads writes)
                         &body body)
  (declare (ignore limit reads writes))
  `(%call-with-target-tns ,source-tn
                          (lambda (,target-variable)
                            ,@body)
                          ,@keys))

(defun find-ok-target-offset (tn sc)
  (declare (type tn tn) (type storage-class sc))
  (do-target-tns (target tn)
    (awhen (and (tn-offset target)
                (neq (tn-kind target) :arg-pass)
                (check-ok-target target tn sc))
      (return-from find-ok-target-offset it))))

;;;; location selection

;;; Select some location for TN in SC, returning the offset if we
;;; succeed, and NIL if we fail.
;;;
;;; For :UNBOUNDED SCs just find the smallest correctly aligned offset
;;; where the TN doesn't conflict with the TNs that have already been
;;; packed. For :FINITE SCs try to pack the TN into the most heavily
;;; used locations first (as estimated in FIND-LOCATION-USAGE).
(defun select-location (tn sc &key use-reserved-locs)
  (declare (type tn tn) (type storage-class sc))
  (let* ((sb (sc-sb sc))
         (element-size (sc-element-size sc))
         (alignment (sc-alignment sc))
         (align-mask (1- alignment)))
    (labels ((attempt-location (start-offset)
               (let ((conflict (conflicts-in-sc tn sc start-offset)))
                 (if conflict
                     (logandc2 (+ conflict align-mask 1)
                               align-mask)
                     (return-from select-location start-offset))))
             (try (locations)
               (do-sc-locations (location locations nil element-size)
                 (attempt-location location))))
      (if (eq (sb-kind sb) :unbounded)
          (let ((size (finite-sb-current-size sb)))
            (loop with offset = 0
                  until (> (+ offset element-size) size) do
                  (setf offset (attempt-location offset))))
          (let* ((locations (sc-locations sc))
                 (reserved (sc-reserve-locations sc))
                 (wired (logandc2 (finite-sb-wired-map sb) reserved)))
            ;; Try non wired locatiions first
            (try (logandc2 locations wired))
            ;; Then the wired locations that are present in this SC.
            (try (logand locations wired))
            ;; And only then when requested try the reserved locations.
            (when use-reserved-locs
              (try reserved)))))))

;;; If a save TN, return the saved TN, otherwise return TN. This is
;;; useful for getting the conflicts of a TN that might be a save TN.
(defun original-tn (tn)
  (declare (type tn tn))
  (if (member (tn-kind tn) '(:save :save-once :specified-save))
      (tn-save-tn tn)
      tn))

;;;; pack interface

;;; Attempt to pack TN in all possible SCs, first in the SC chosen by
;;; representation selection, then in the alternate SCs in the order
;;; they were specified in the SC definition. If the TN-COST is
;;; negative, then we don't attempt to pack in SCs that must be saved.
;;; If Restricted, then we can only pack in TN-SC, not in any
;;; Alternate-SCs.
;;;
;;; If we are attempting to pack in the SC of the save TN for a TN
;;; with a :SPECIFIED-SAVE TN, then we pack in that location, instead
;;; of allocating a new stack location.
(defun pack-tn (tn restricted &key (allow-unbounded-sc t))
  (declare (type tn tn))
  (aver (not (tn-offset tn)))
  (let* ((original (original-tn tn))
         (fsc (tn-sc tn))
         (alternates (unless restricted (sc-alternate-scs fsc)))
         (save (tn-save-tn tn))
         (specified-save-sc
          (when (and save
                     (eq (tn-kind save) :specified-save))
            (tn-sc save))))
    (do ((sc fsc (pop alternates)))
        ((null sc)
         (failed-to-pack-error tn restricted))
      (unless (or allow-unbounded-sc
                  (not (unbounded-sc-p sc)))
        (return nil))
      (when (eq sc specified-save-sc)
        (unless (tn-offset save)
          (pack-tn save nil))
        (setf (tn-offset tn) (tn-offset save))
        (setf (tn-sc tn) (tn-sc save))
        (return t))
      (when (or restricted
                (not (and (minusp (tn-cost tn)) (sc-save-p sc))))
        (let ((loc (or (find-ok-target-offset original sc)
                       (select-location original sc :use-reserved-locs restricted)
                       (when (unbounded-sc-p sc)
                         (grow-sc sc)
                         (or (select-location original sc)
                             (error "failed to pack after growing SC?"))))))
          (when loc
            (add-location-conflicts original sc loc)
            (setf (tn-sc tn) sc)
            (setf (tn-offset tn) loc)
            (return t))))))
  (values))

;;; Pack a wired TN, checking that the offset is in bounds for the SB,
;;; and that the TN doesn't conflict with some other TN already packed
;;; in that location. If the TN is wired to a location beyond the end
;;; of a :UNBOUNDED SB, then grow the SB enough to hold the TN.
;;;
;;; ### Checking for conflicts is disabled for :SPECIFIED-SAVE TNs.
;;; This is kind of a hack to make specifying wired stack save
;;; locations for local call arguments (such as OLD-FP) work, since
;;; the caller and callee OLD-FP save locations may conflict when the
;;; save locations don't really (due to being in different frames.)
(defun pack-wired-tn (tn)
  (declare (type tn tn))
  (let* ((sc (tn-sc tn))
         (sb (sc-sb sc))
         (offset (tn-offset tn))
         (end (+ offset (sc-element-size sc)))
         (original (original-tn tn)))
    (when (> end (finite-sb-current-size sb))
      (unless (eq (sb-kind sb) :unbounded)
        (error "~S is wired to a location that is out of bounds." tn))
      (grow-sc sc end))

    ;; For non-x86 ports the presence of a save-tn associated with a
    ;; tn is used to identify the old-fp and return-pc tns. It depends
    ;; on the old-fp and return-pc being passed in registers.
    (when (and #-fp-and-pc-standard-save
               (not (eq (tn-kind tn) :specified-save))
               (conflicts-in-sc original sc offset))
      (error "~S is wired to location ~D in SC ~A of kind ~S that it conflicts with."
             tn offset sc (tn-kind tn)))

    ;; Use the above check, but only print a verbose warning. This can
    ;; be helpful for debugging the x86 port.
    #+nil
    (when (and (not (eq (tn-kind tn) :specified-save))
               (conflicts-in-sc original sc offset))
      (format t "~&* Pack-wired-tn possible conflict:~%  ~
                     tn: ~S; tn-kind: ~S~%  ~
                     sc: ~S~%  ~
                     sb: ~S; sb-name: ~S; sb-kind: ~S~%  ~
                     offset: ~S; end: ~S~%  ~
                     original ~S~%  ~
                     tn-save-tn: ~S; tn-kind of tn-save-tn: ~S~%"
              tn (tn-kind tn) sc
              sb (sb-name sb) (sb-kind sb)
              offset end
              original
              (tn-save-tn tn) (tn-kind (tn-save-tn tn))))

    (unless (eq (sb-kind sb) :unbounded)
      (setf (ldb (byte 1 (truly-the sb-vm:finite-sc-offset offset))
                 (finite-sb-wired-map sb))
            1))
    (add-location-conflicts original sc offset)))

(defevent repack-block "Repacked a block due to TN unpacking.")

;;; KLUDGE: Prior to SBCL version 0.8.9.xx, this function was known as
;;; PACK-BEFORE-GC-HOOK, but was non-functional since approximately
;;; version 0.8.3.xx since the removal of GC hooks from the system.
;;; This currently (as of 2004-04-12) runs now after every call to
;;; PACK, rather than -- as was originally intended -- once per GC
;;; cycle; this is probably non-optimal, and might require tuning,
;;; maybe to be called when the data structures exceed a certain size,
;;; or maybe once every N times.  The KLUDGE is that this rewrite has
;;; done nothing to improve the reentrance or threadsafety of the
;;; compiler; it still fails to be callable from several threads at
;;; the same time.
;;; But (FIXME) - we should not make new arrays here, just clear the old
;;; because new ones are made on each call to COMPILE or COMPILE-FILE.
;;;
;;; Brief experiments indicate that during a compilation cycle this
;;; causes about 10% more consing, and takes about 1%-2% more time.
;;;
;;; -- CSR, 2004-04-12
(defun clean-up-pack-structures ()
  (dovector (sb *backend-sbs*)
    (unless (eq (sb-kind sb) :non-packed)
      (let ((size (sb-size sb)))
        (fill (finite-sb-always-live sb) nil)
        (setf (finite-sb-always-live sb)
              (make-array size :initial-element #*))
        (setf (finite-sb-wired-map sb) 0)

        (fill (finite-sb-conflicts sb) nil)
        (setf (finite-sb-conflicts sb)
              (make-array size :initial-element '#()))

        (fill (finite-sb-live-tns sb) nil)
        (setf (finite-sb-live-tns sb)
              (make-array size :initial-element nil))))))

(defun tn-lexical-depth (tn)
  (let ((path t)) ; dummy initial value
    (labels ((path (lambda)
               (do ((acc '())
                    (lambda lambda (lexenv-lambda (sb-c::lambda-lexenv lambda))))
                   ((null lambda) acc)
                 (push lambda acc)))
             (register-scope (lambda)
               (let ((new-path (path lambda)))
                 (setf path (if (eql path t)
                                new-path
                                (subseq path
                                        0 (mismatch path new-path))))))
             (walk-tn-refs (ref)
               (do ((ref ref (tn-ref-next ref)))
                   ((or (null ref)
                        (null path)))
                 (awhen (vop-node (tn-ref-vop ref))
                   (register-scope (lexenv-lambda (node-lexenv it)))))))
      (walk-tn-refs (tn-reads tn))
      (walk-tn-refs (tn-writes tn))
      (if (eql path t)
          most-positive-fixnum
          (length path)))))

(declaim (type (member :iterative :greedy :adaptive)
               *register-allocation-method*))
(defvar *register-allocation-method* :adaptive)

(declaim (ftype function pack-greedy pack-iterative))

(defun pack (component)
  (unwind-protect
       (let ((optimize nil)
             (speed-3 nil)
             (2comp (component-info component)))
         (init-sb-vectors component)

         ;; Determine whether we want to do more expensive packing by
         ;; checking whether any blocks in the component have (> SPEED
         ;; COMPILE-SPEED).
         ;;
         ;; Also, determine if any such block also declares (speed 3),
         ;; in which case :adaptive register allocation will switch to
         ;; the iterative Chaitin-Briggs spilling/coloring algorithm.
         ;;
         ;; FIXME: This means that a declaration can have a minor
         ;; effect even outside its scope, and as the packing is done
         ;; component-globally it'd be tricky to use strict scoping. I
         ;; think this is still acceptable since it's just a tradeoff
         ;; between compilation speed and allocation quality and
         ;; doesn't affect the semantics of the generated code in any
         ;; way. -- JES 2004-10-06
         (do-ir2-blocks (block component)
           (let ((block (block-last (ir2-block-block block))))
             (when (policy block (> speed compilation-speed))
               (setf optimize t)
               (when (policy block (= speed 3))
                 (setf speed-3 t)
                 (return)))))

         ;; Assign costs to normal TNs so we know which ones should always
         ;; be packed on the stack, and which are important not to spill.
         (if *pack-assign-costs*
             (assign-tn-costs component)
             #-fp-and-pc-standard-save
             (maybe-force-specified-saves-to-stack component))

         ;; Actually allocate registers for most TNs. After this, only
         ;; :normal tns may be left unallocated (or TNs :restricted to
         ;; an unbounded SC).
         (funcall (ecase *register-allocation-method*
                    (:greedy #'pack-greedy)
                    (:iterative #'pack-iterative)
                    (:adaptive (if speed-3 #'pack-iterative #'pack-greedy)))
                  component 2comp)

         ;; Pack any leftover normal/restricted TN that is not already
         ;; allocated to a finite SC, or TNs that do not appear in any
         ;; local TN map (e.g. :MORE TNs).  Since we'll likely be
         ;; allocating on the stack, first allocate TNs that are
         ;; associated with code at shallow lexical depths: this will
         ;; allocate long live ranges (i.e. TNs with more conflicts)
         ;; first, and hopefully minimise stack fragmentation.
         ;; Component TNs are a degenerate case: they are always live.
         (let ((component-tns '())
               (contiguous-tns '())
               (tns '()))
           (flet ((register-tn (tn)
                    (unless (tn-offset tn)
                      (case (tn-kind tn)
                        (:component
                         (push tn component-tns))
                        ((:environment :debug-environment)
                         (push tn contiguous-tns))
                        (t
                         (push tn tns))))))
             (do ((tn (ir2-component-restricted-tns 2comp) (tn-next tn)))
                 ((null tn))
               ;; by this time, restricted TNs must either be
               ;; allocated in the right SC or unbounded
               (aver (or (tn-offset tn) (unbounded-tn-p tn)))
               (register-tn tn))
             (do ((tn (ir2-component-normal-tns 2comp) (tn-next tn)))
                 ((null tn))
               (register-tn tn)))
           (flet ((pack-tns (tns &optional in-order)
                    (dolist (tn (if in-order
                                    tns
                                    (schwartzian-stable-sort-list
                                     tns #'< :key #'tn-lexical-depth)))
                      (unless (tn-offset tn)
                        (pack-tn tn nil)))))
             ;; first pack TNs that are known to have simple live
             ;; ranges (contiguous lexical scopes)
             (pack-tns component-tns t)
             (pack-tns contiguous-tns)
             (pack-tns tns)))

         ;; Do load TN packing and emit saves.
         (let ((*repack-blocks* nil))
           (cond ((and optimize *pack-optimize-saves*)
                  (optimized-emit-saves component)
                  (do-ir2-blocks (block component)
                    (pack-load-tns block)))
                 (t
                  (do-ir2-blocks (block component)
                    (emit-saves block)
                    (pack-load-tns block))))
           (loop
              (unless *repack-blocks* (return))
              (let ((orpb *repack-blocks*))
                (setq *repack-blocks* nil)
                (dolist (block orpb)
                  (event repack-block)
                  (pack-load-tns block)))))

         (values))
    (clean-up-pack-structures)))

(defun pack-greedy (component 2comp)
  (declare (type component component)
           (type ir2-component 2comp))
  ;; Pack wired TNs first.
  (do ((tn (ir2-component-wired-tns 2comp) (tn-next tn)))
      ((null tn))
    (unless (eq (tn-kind tn) :arg-pass)
      (pack-wired-tn tn)))

  ;; Then, pack restricted TNs, ones that are live over the whole
  ;; component first (they cause no fragmentation).  Sort by TN cost
  ;; to help important TNs get good targeting.
  (collect ((component)
            (normal))
    (do ((tn (ir2-component-restricted-tns 2comp) (tn-next tn)))
        ((null tn))
      (unless (or (tn-offset tn) (unbounded-tn-p tn))
        (if (eq :component (tn-kind tn))
            (component tn)
            (normal tn))))
    (flet ((pack-tns (tns)
             (dolist (tn (stable-sort tns #'> :key #'tn-cost))
               (pack-tn tn t))))
      (pack-tns (component))
      (pack-tns (normal))))

  (cond (*pack-assign-costs*
         ;; Allocate normal TNs, starting with the TNs that are
         ;; heavily used in deep loops (which is taken into account in
         ;; TN spill costs).  Only allocate in finite SCs (i.e. not on
         ;; the stack).
         (collect ((tns))
           (do ((tn (ir2-component-normal-tns 2comp) (tn-next tn)))
               ((null tn))
             (unless (or (tn-offset tn)
                         (unbounded-tn-p tn)
                         (and (sc-save-p (tn-sc tn))  ; SC caller-save, but TN
                              (minusp (tn-cost tn)))) ; lives over many calls
               (tns tn)))
           (dolist (tn (stable-sort (tns) #'> :key #'tn-cost))
             (unless (tn-offset tn)
               ;; if it can't fit in a bounded SC, the final pass will
               ;; take care of stack packing.
               (pack-tn tn nil :allow-unbounded-sc nil)))))
        (t
         ;; If loop analysis has been disabled we might as well revert
         ;; to the old behaviour of just packing TNs linearly as they
         ;; appear.
         (do-ir2-blocks (block component)
           (let ((ltns (ir2-block-local-tns block)))
             (do ((i (1- (ir2-block-local-tn-count block)) (1- i)))
                 ((minusp i))
               (declare (fixnum i))
               (let ((tn (svref ltns i)))
                 (unless (or (null tn)
                             (eq tn :more)
                             (tn-offset tn)
                             (unbounded-tn-p tn))
                   (pack-tn tn nil :allow-unbounded-sc nil)))))))))
