(in-package "SB!VM")

;;; Make a TN for the argument count passing location for a
;;; non-local entry.
(defun make-nlx-entry-arg-start-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn ocfp-offset))

;;; Save and restore dynamic environment.
;;;
;;; These VOPs are used in the reentered function to restore the appropriate
;;; dynamic environment.  Currently we only save the Current-Catch and binding
;;; stack pointer.  We don't need to save/restore the current unwind-protect,
;;; since unwind-protects are implicitly processed during unwinding.  If there
;;; were any additional stacks, then this would be the place to restore the top
;;; pointers.

(define-vop (save-dynamic-state)
  (:results (catch :scs (descriptor-reg))
            (nfp :scs (descriptor-reg))
            (nsp :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 13
    (load-symbol-value catch *current-catch-block*)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (move cur-nfp nfp)))
    (move nsp-tn nsp)))

(define-vop (restore-dynamic-state)
  (:args (catch :scs (descriptor-reg))
         (nfp :scs (descriptor-reg))
         (nsp :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 10
    (store-symbol-value catch *current-catch-block*)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (move nfp cur-nfp)))
    (move nsp nsp-tn)))

(define-vop (current-stack-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move csp-tn res)))

(define-vop (current-binding-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move bsp-tn res)))


;;;; Unwind block hackery:

;;; Compute the address of the catch block from its TN, then store into the
;;; block the current Fp, Env, Unwind-Protect, and the entry PC.
;;;
(define-vop (make-unwind-block)
  (:args (tn))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 22
    (inst ldo (* (tn-offset tn) n-word-bytes) cfp-tn block)
    (load-symbol-value temp *current-unwind-protect-block*)
    (storew temp block unwind-block-current-uwp-slot)
    (storew cfp-tn block unwind-block-current-cont-slot)
    (storew code-tn block unwind-block-current-code-slot)
    (inst compute-lra-from-code code-tn entry-label ndescr temp)
    (storew temp block catch-block-entry-pc-slot)))

;;; Like Make-Unwind-Block, except that we also store in the specified tag, and
;;; link the block into the Current-Catch list.
;;;
(define-vop (make-catch-block)
  (:args (tn)
         (tag :scs (any-reg descriptor-reg)))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :target block :to (:result 0)) result)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 44
    (inst ldo (* (tn-offset tn) n-word-bytes) cfp-tn result)
    (load-symbol-value temp *current-unwind-protect-block*)
    (storew temp result catch-block-current-uwp-slot)
    (storew cfp-tn result catch-block-current-cont-slot)
    (storew code-tn result catch-block-current-code-slot)
    (inst compute-lra-from-code code-tn entry-label ndescr temp)
    (storew temp result catch-block-entry-pc-slot)

    (storew tag result catch-block-tag-slot)
    (load-symbol-value temp *current-catch-block*)
    (storew temp result catch-block-previous-catch-slot)
    (store-symbol-value result *current-catch-block*)
    (move result block)))

;;; Just set the current unwind-protect to TN's address.  This instantiates an
;;; unwind block as an unwind-protect.
;;;
(define-vop (set-unwind-protect)
  (:args (tn))
  (:temporary (:scs (descriptor-reg)) new-uwp)
  (:generator 7
    (inst ldo (* (tn-offset tn) n-word-bytes) cfp-tn new-uwp)
    (store-symbol-value new-uwp *current-unwind-protect-block*)))


(define-vop (unlink-catch-block)
  (:temporary (:scs (any-reg)) block)
  (:policy :fast-safe)
  (:translate %catch-breakup)
  (:generator 17
    (load-symbol-value block *current-catch-block*)
    (loadw block block catch-block-previous-catch-slot)
    (store-symbol-value block *current-catch-block*)))

(define-vop (unlink-unwind-protect)
  (:temporary (:scs (any-reg)) block)
  (:policy :fast-safe)
  (:translate %unwind-protect-breakup)
  (:generator 17
    (load-symbol-value block *current-unwind-protect-block*)
    (loadw block block unwind-block-current-uwp-slot)
    (store-symbol-value block *current-unwind-protect-block*)))


;;;; NLX entry VOPs:


(define-vop (nlx-entry)
  (:args (sp) ;; Note: we can't list an sc-restriction, 'cause any load vops
              ;; would be inserted before the LRA.
         (start)
         (count))
  (:results (values :more t))
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:info label nvals)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)
    (cond ((zerop nvals))
          ((= nvals 1)
           (loadw (tn-ref-tn values) start)
           (inst comclr count zero-tn zero-tn :<>)
           (move null-tn (tn-ref-tn values) t))
          (t
           (collect ((defaults))
             (do ((i 0 (1+ i))
                  (tn-ref values (tn-ref-across tn-ref)))
                 ((null tn-ref))
               (let ((default-lab (gen-label))
                     (tn (tn-ref-tn tn-ref)))
                 (defaults (cons default-lab tn))
                 (inst comb := zero-tn count default-lab)
                 (inst addi (fixnumize -1) count count)
                 (sc-case tn
                   ((descriptor-reg any-reg)
                     (loadw tn start i))
                   (control-stack
                     (loadw move-temp start i)
                     (store-stack-tn tn move-temp)))))
             (let ((defaulting-done (gen-label)))
               (emit-label defaulting-done)
               (assemble (*elsewhere*)
                 (dolist (def (defaults))
                   (emit-label (car def))
                   (let ((tn (cdr def)))
                     (sc-case tn
                       ((descriptor-reg any-reg)
                         (move null-tn tn))
                       (control-stack
                         (store-stack-tn tn null-tn)))))
                 (inst b defaulting-done :nullify t))))))
    (load-stack-tn csp-tn sp)))


(define-vop (nlx-entry-multiple)
  (:args (top :target dst) (start :target src) (count :target num))
  ;; Again, no SC restrictions for the args, 'cause the loading would
  ;; happen before the entry label.
  (:info label)
  (:temporary (:scs (any-reg) :from (:argument 0)) dst)
  (:temporary (:scs (any-reg) :from (:argument 1)) src)
  (:temporary (:scs (any-reg) :from (:argument 2)) num)
  (:temporary (:scs (descriptor-reg)) temp)
  (:results (new-start) (new-count))
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)
    (let ((loop (gen-label))
          (done (gen-label)))

      ;; Copy args.
      (load-stack-tn dst top)
      (move start src)
      (move count num)

      ;; Establish results.
      (sc-case new-start
        (any-reg (move dst new-start))
        (control-stack (store-stack-tn new-start dst)))
      (inst comb := num zero-tn done :nullify t)
      (sc-case new-count
        (any-reg (move num new-count))
        (control-stack (store-stack-tn new-count num)))

      ;; Copy stuff on stack.
      (emit-label loop)
      (inst ldwm n-word-bytes src temp)
      (inst addib :<> (fixnumize -1) num loop)
      (inst stwm temp n-word-bytes dst)

      (emit-label done)
      (move dst csp-tn))))

;;; This VOP is just to force the TNs used in the cleanup onto the stack.
;;;
(define-vop (uwp-entry)
  (:info label)
  (:save-p :force-to-stack)
  (:results (block) (start) (count))
  (:ignore block start count)
  (:vop-var vop)
  (:generator 0
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)))
