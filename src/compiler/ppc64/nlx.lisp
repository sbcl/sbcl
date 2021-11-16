;;;; the PPC definitions of VOPs used for non-local exit (throw,
;;;; lexical exit, etc.)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Make a TN for the argument count passing location for a
;;; non-local entry.
(defun make-nlx-entry-arg-start-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn ocfp-offset))


;;; These VOPs are used in the reentered function to restore the
;;; appropriate dynamic environment. Currently we only save the
;;; CURRENT-CATCH and binding stack pointer. We don't need to
;;; save/restore the current unwind-protect, since UNWIND-PROTECTs are
;;; implicitly processed during unwinding. If there were any
;;; additional stacks, then this would be the place to restore the top
;;; pointers.

(define-vop (save-dynamic-state)
  (:results (catch :scs (descriptor-reg))
            (nfp :scs (descriptor-reg))
            (nsp :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 13
    (load-tl-symbol-value catch *current-catch-block*)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (move nfp cur-nfp)))
    (move nsp nsp-tn)))

(define-vop (restore-dynamic-state)
  (:args (catch :scs (descriptor-reg))
         (nfp :scs (descriptor-reg))
         (nsp :scs (descriptor-reg)))
  (:temporary (:scs (any-reg)) temp)
  (:vop-var vop)
  (:generator 10
    (store-tl-symbol-value catch *current-catch-block* temp)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (move cur-nfp nfp)))
    (move nsp-tn nsp)))

(define-vop (current-stack-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move res csp-tn)))

(define-vop (current-binding-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move res bsp-tn)))

(define-vop (current-nsp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move res nsp-tn)))

(define-vop (set-nsp)
  (:args (nsp :scs (any-reg descriptor-reg)))
  (:generator 1
    (move nsp-tn nsp)))


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
    (inst addi block cfp-tn (tn-byte-offset tn))
    (load-tl-symbol-value temp *current-unwind-protect-block*)
    (storew temp block unwind-block-uwp-slot)
    (storew cfp-tn block unwind-block-cfp-slot)
    (storew code-tn block unwind-block-code-slot)
    (inst compute-lra-from-code temp code-tn entry-label ndescr)
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
    (inst addi result cfp-tn (tn-byte-offset tn))
    (load-tl-symbol-value temp *current-unwind-protect-block*)
    (storew temp result catch-block-uwp-slot)
    (storew cfp-tn result catch-block-cfp-slot)
    (storew code-tn result catch-block-code-slot)
    (inst compute-lra-from-code temp code-tn entry-label ndescr)
    (storew temp result catch-block-entry-pc-slot)

    (storew tag result catch-block-tag-slot)
    (load-tl-symbol-value temp *current-catch-block*)
    (storew temp result catch-block-previous-catch-slot)
    (store-tl-symbol-value result *current-catch-block* temp)

    (move block result)))


;;; Just set the current unwind-protect to UWP.  This instantiates an
;;; unwind block as an unwind-protect.
;;;
(define-vop (set-unwind-protect)
  (:args (uwp :scs (any-reg)))
  (:temporary (:scs (any-reg)) temp)
  (:generator 7
    (store-tl-symbol-value uwp *current-unwind-protect-block* temp)))


(define-vop (%catch-breakup)
  (:args (current-block))
  (:ignore current-block)
  (:temporary (:scs (any-reg)) block)
  (:temporary (:scs (any-reg)) temp)
  (:policy :fast-safe)
  (:generator 17
    (load-tl-symbol-value block *current-catch-block*)
    (loadw block block catch-block-previous-catch-slot)
    (store-tl-symbol-value block *current-catch-block* temp)))

(define-vop (%unwind-protect-breakup)
  (:args (current-block))
  (:ignore current-block)
  (:temporary (:scs (any-reg)) block)
  (:temporary (:scs (any-reg)) temp)
  (:policy :fast-safe)
  (:generator 17
    (load-tl-symbol-value block *current-unwind-protect-block*)
    (loadw block block unwind-block-uwp-slot)
    (store-tl-symbol-value block *current-unwind-protect-block* temp)))


;;;; NLX entry VOPs:


(define-vop (nlx-entry)
  (:args (sp) ; Note: we can't list an sc-restriction, 'cause any load vops
              ; would be inserted before the LRA.
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
           (let ((no-values (gen-label)))
             (inst cmpwi count 0)
             (move (tn-ref-tn values) null-tn)
             (inst beq no-values)
             (loadw (tn-ref-tn values) start)
             (emit-label no-values)))
          (t
           (collect ((defaults))
             (inst addic. count count (- (fixnumize 1)))
             (do ((i 0 (1+ i))
                  (tn-ref values (tn-ref-across tn-ref)))
                 ((null tn-ref))
               (let ((default-lab (gen-label))
                     (tn (tn-ref-tn tn-ref)))
                 (defaults (cons default-lab tn))

                 (inst subi count count (fixnumize 1))
                 (inst blt default-lab)
                 (sc-case tn
                          ((descriptor-reg any-reg)
                           (loadw tn start i))
                          (control-stack
                           (loadw move-temp start i)
                           (store-stack-tn tn move-temp)))
                 (inst cmpwi count 0)))

             (let ((defaulting-done (gen-label)))

               (emit-label defaulting-done)

               (assemble (:elsewhere)
                 (dolist (def (defaults))
                   (emit-label (car def))
                   (let ((tn (cdr def)))
                     (sc-case tn
                              ((descriptor-reg any-reg)
                               (move tn null-tn))
                              (control-stack
                               (store-stack-tn tn null-tn)))))
                 (inst b defaulting-done))))))
    (load-stack-tn csp-tn sp)))

(define-vop (nlx-entry-single)
  (:args (sp)
         (value))
  (:results (res :from :load))
  (:info label)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)
    (move res value)
    (load-stack-tn csp-tn sp)))

(define-vop (nlx-entry-multiple)
  (:args (top :target result) (src) (count :target limit))
  ;; Again, no SC restrictions for the args, 'cause the loading would
  ;; happen before the entry label.
  (:info label)
  (:temporary (:scs (any-reg)) dst)
  (:temporary (:scs (descriptor-reg)) temp limit)
  (:results (result :scs (any-reg) :from (:argument 0))
            (num :scs (any-reg) :from (:argument 0)))
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)

    ;; Setup results, and test for the zero value case.
    (load-stack-tn result top)
    (inst cmpwi count 0)
    (inst li num 0)
    (inst beq done)
    (inst slwi limit count (- word-shift n-fixnum-tag-bits))

    ;; Compute dst as one slot down from result, because we inc the index
    ;; before we use it.
    (inst subi dst result n-word-bytes)

    ;; Copy stuff down the stack.
    LOOP
    (inst ldx temp src num)
    (inst addi num num n-word-bytes)
    (inst cmpw num limit)
    (inst stdx temp dst num)
    (inst bne loop)

    ;; Reset the CSP.
    DONE
    (inst add csp-tn result num)
    (inst srwi num num (- word-shift n-fixnum-tag-bits))))


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

