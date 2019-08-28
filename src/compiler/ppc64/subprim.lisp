;;;
;;; Written by William Lott.
;;;
(in-package "SB-VM")



;;;; Length

(define-vop (length/list)
  (:translate length)
  (:args (object :scs (descriptor-reg) :target ptr))
  (:arg-types list)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) ptr)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (any-reg) :to (:result 0) :target result)
              count)
  (:results (result :scs (any-reg descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 50
    (let ((done (gen-label))
          (loop (gen-label))
          (not-list (generate-error-code vop 'object-not-list-error ptr)))
      (move ptr object)
      (inst li count 0)

      (inst cmpd ptr null-tn)
      (inst beq done)

      (emit-label loop)

      (test-type ptr temp not-list t (list-pointer-lowtag))

      (loadw ptr ptr cons-cdr-slot list-pointer-lowtag)
      (inst addi count count (fixnumize 1))

      (inst cmpd ptr null-tn)
      (inst bne loop)

      (emit-label done)
      (move result count))))
