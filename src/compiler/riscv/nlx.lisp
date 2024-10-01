;;;; the definition of non-local exit for the RISC-V VM

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
    (load-current-catch-block catch)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (move nfp cur-nfp)))
    (move nsp nsp-tn)))

(define-vop (restore-dynamic-state)
  (:args (catch :scs (descriptor-reg))
         (nfp :scs (descriptor-reg))
         (nsp :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 10
    (store-current-catch-block catch)
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
    (load-binding-stack-pointer res)))

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
  (:temporary (:scs (interior-reg)) lip)
  (:generator 22
    (add-imm block cfp-tn (tn-byte-offset tn) 'make-unwind-block temp)
    (load-current-unwind-protect-block temp)
    (storew temp block unwind-block-uwp-slot)
    (storew cfp-tn block unwind-block-cfp-slot)
    (storew code-tn block unwind-block-code-slot)
    (inst compute-ra-from-code temp code-tn lip entry-label)
    (storew temp block catch-block-entry-pc-slot)))

;;; Like Make-Unwind-Block, except that we also store in the specified tag, and
;;; link the block into the Current-Catch list.
;;;
(define-vop (make-catch-block)
  (:args (tn) (tag :scs (any-reg descriptor-reg)))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :target block :to (:result 0)) result)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 44
    ;; ADD-IMM needs 3 instructions usually, but this way almost always needs
    ;; at most 2 instructions in all likelihood.
    (do ((src-operand cfp-tn)
         (imm (tn-byte-offset tn)))
        ((zerop imm))
      (let ((short-imm (min imm 2040))) ; 2040 = maximum short immediate
        (inst addi result src-operand short-imm)
        (setq src-operand result)
        (zerop (decf imm short-imm))))
    (load-current-unwind-protect-block temp)
    (storew temp result catch-block-uwp-slot)
    (storew cfp-tn result catch-block-cfp-slot)
    (storew code-tn result catch-block-code-slot)
    (inst compute-ra-from-code temp code-tn lip entry-label)
    (storew temp result catch-block-entry-pc-slot)

    (storew tag result catch-block-tag-slot)
    (load-current-catch-block temp)
    (storew temp result catch-block-previous-catch-slot)
    (store-current-catch-block result)

    (move block result)))

;;; Just set the current unwind-protect to UWP.  This
;;; instantiates an unwind block as an unwind-protect.
(define-vop (set-unwind-protect)
  (:args (uwp :scs (any-reg)))
  (:generator 7
    (store-current-unwind-protect-block uwp)))

(define-vop (%catch-breakup)
  (:args (current-block))
  (:ignore current-block)
  (:temporary (:scs (any-reg)) block)
  (:policy :fast-safe)
  (:generator 17
    (load-current-catch-block block)
    (loadw block block catch-block-previous-catch-slot)
    (store-current-catch-block block)))

(define-vop (%unwind-protect-breakup)
  (:args (current-block))
  (:ignore current-block)
  (:temporary (:scs (any-reg)) block)
  (:policy :fast-safe)
  (:generator 17
    (load-current-unwind-protect-block block)
    (loadw block block unwind-block-uwp-slot)
    (store-current-unwind-protect-block block)))

;;;; NLX entry VOPs:

(define-vop (nlx-entry)
  (:args (sp) ; Note: we can't list an sc-restriction, 'cause any load vops
              ; would be inserted before the LRA.
         (start)
         (count))
  (:results (values :more t :from :load))
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:info label nvals)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-label label)
    (note-this-location vop :non-local-entry)
    (cond ((zerop nvals))
          ((= nvals 1)
           (let ((no-values (gen-label)))
             (move (tn-ref-tn values) null-tn)
             (inst beq count zero-tn no-values)
             (loadw (tn-ref-tn values) start)
             (emit-label no-values)))
          (t
           (do ((i 0 (1+ i))
                (tn-ref values (tn-ref-across tn-ref)))
               ((null tn-ref))
             (let ((tn (tn-ref-tn tn-ref)))
               (inst subi count count (fixnumize 1))
               (sc-case tn
                 ((descriptor-reg any-reg)
                  (assemble ()
                    (move tn null-tn)
                    (inst blt count zero-tn LESS-THAN)
                    (loadw tn start i)
                    LESS-THAN))
                 (control-stack
                  (assemble ()
                    (move move-temp null-tn)
                    (inst blt count zero-tn LESS-THAN)
                    (loadw move-temp start i)
                    LESS-THAN
                    (store-stack-tn tn move-temp))))))))
    (load-stack-tn csp-tn sp)))

(define-vop (nlx-entry-single)
  (:args (sp)
         (value))
  (:results (res :from :load))
  (:info label)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-label label)
    (note-this-location vop :non-local-entry)
    (move res value)
    (load-stack-tn csp-tn sp)))

(define-vop (nlx-entry-multiple)
  (:args (top :target result)
         (src)
         (count . #.(cl:when sb-vm::fixnum-as-word-index-needs-temp
                      '(:target count-words))))
  ;; Again, no SC restrictions for the args, 'cause the loading would
  ;; happen before the entry label.
  (:info label)
  (:temporary (:scs (any-reg)) dst)
  (:temporary (:scs (descriptor-reg)) temp)
  #+#.(cl:if sb-vm::fixnum-as-word-index-needs-temp '(and) '(or))
  (:temporary (:scs (any-reg) :from (:argument 2)) count-words)
  (:results (result :scs (any-reg) :from (:argument 0))
            (num :scs (any-reg) :from (:argument 0)))
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-label label)
    (note-this-location vop :non-local-entry)

    (let ((loop (gen-label))
          (done (gen-label)))

      ;; Setup results, and test for the zero value case.
      (load-stack-tn result top)
      (move num count)
      ;; Reset the CSP.
      (with-fixnum-as-word-index (count count-words)
        (inst add csp-tn result count))
      (inst beq count zero-tn done)

      (move dst result)
      ;; Copy stuff on the stack
      (emit-label loop)
      (loadw temp src)
      (inst addi src src n-word-bytes)
      (storew temp dst)
      (inst addi dst dst n-word-bytes)
      (inst bne dst csp-tn loop)

      (emit-label done))))

;;; This VOP is just to force the TNs used in the cleanup onto the stack.
;;;
(define-vop (uwp-entry)
  (:info label)
  (:save-p :force-to-stack)
  (:results (block) (start) (count))
  (:ignore block start count)
  (:vop-var vop)
  (:generator 0
    (emit-label label)
    (note-this-location vop :non-local-entry)))
