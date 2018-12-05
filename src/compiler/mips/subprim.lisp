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
          (not-list (gen-label)))
      (move ptr object)
      (move count zero-tn)

      (emit-label loop)

      (inst beq ptr null-tn done)
      (inst and temp ptr lowtag-mask)
      (inst xor temp list-pointer-lowtag)
      (inst bne temp not-list)
      (inst nop)

      (loadw ptr ptr cons-cdr-slot list-pointer-lowtag)
      (inst b loop)
      (inst addu count count (fixnumize 1))

      (emit-label not-list)
      (cerror-call vop 'object-not-list-error ptr)

      (emit-label done)
      (move result count))))
