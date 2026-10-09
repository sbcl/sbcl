;;;; the definition of non-local exit for the LoongArch VM

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
  (make-wired-tn *fixnum-primitive-type* any-reg-sc-number ocfp-offset))

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
(define-vop (make-unwind-block)
  (:args (tn))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) lip)
  (:vop-var vop)
  (:generator 22
    (add-imm block cfp-tn (tn-byte-offset tn) 'make-unwind-block temp)
    (load-current-unwind-protect-block temp)
    (storew temp block unwind-block-uwp-slot)
    (storew cfp-tn block unwind-block-cfp-slot)
    (storew code-tn block unwind-block-code-slot)
    (inst compute-ra-from-code temp code-tn lip entry-label)
    (storew temp block catch-block-entry-pc-slot)
    (load-binding-stack-pointer temp)
    (storew temp block unwind-block-bsp-slot)
    (load-current-catch-block temp)
    (storew temp block unwind-block-current-catch-slot)
    (let ((nfp (current-nfp-tn vop)))
      (when nfp
        (storew nfp block unwind-block-nfp-slot))
      (storew nsp-tn block unwind-block-nsp-slot))))

(define-vop (make-catch-block)
  (:args (tn) (tag :scs (any-reg descriptor-reg) :to :save))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) lip)
  (:vop-var vop)
  (:generator 44
    (do ((src-operand cfp-tn)
         (imm (tn-byte-offset tn)))
        ((zerop imm))
      (let ((short-imm (min imm 2040)))
        (inst addi.d block src-operand short-imm)
        (setq src-operand block)
        (zerop (decf imm short-imm))))
    (load-current-unwind-protect-block temp)
    (storew temp block catch-block-uwp-slot)
    (storew cfp-tn block catch-block-cfp-slot)
    (storew code-tn block catch-block-code-slot)
    (inst compute-ra-from-code temp code-tn lip entry-label)
    (storew temp block catch-block-entry-pc-slot)

    (storew tag block catch-block-tag-slot)
    (load-current-catch-block temp)
    (storew temp block catch-block-previous-catch-slot)
    (load-binding-stack-pointer temp)
    (storew temp block catch-block-bsp-slot)
    (let ((nfp (current-nfp-tn vop)))
      (when nfp
        (storew nfp block catch-block-nfp-slot))
      (storew nsp-tn block catch-block-nsp-slot))
    (store-current-catch-block block)))

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
  (:generator 17
    (load-current-catch-block block)
    (loadw block block catch-block-previous-catch-slot)
    (store-current-catch-block block)))

(define-vop (%unwind-protect-breakup)
  (:args (current-block))
  (:ignore current-block)
  (:temporary (:scs (any-reg)) block)
  (:generator 17
    (load-current-unwind-protect-block block)
    (loadw block block unwind-block-uwp-slot)
    (store-current-unwind-protect-block block)))

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
        (inst add.d csp-tn result count))
      (inst beq count zero-tn done)

      (move dst result)
      ;; Copy stuff on the stack
      (emit-label loop)
      (loadw temp src)
      (inst addi.d src src n-word-bytes)
      (storew temp dst)
      (inst addi.d dst dst n-word-bytes)
      (inst bne dst csp-tn loop)

      (emit-label done))))

;;; Unwind-Protect
(define-vop (uwp-entry)
  (:info label)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 0
    (emit-label label)
    (note-this-location vop :non-local-entry)))

(define-vop (uwp-entry-block)
  (:info label)
  (:save-p :force-to-stack)
  (:results (block))
  (:vop-var vop)
  (:generator 0
    (emit-label label)
    (note-this-location vop :non-local-entry)
    ;; Get the block saved in UNWIND
    (loadw block csp-tn -4)))
