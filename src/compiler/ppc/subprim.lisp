;;;
;;; Written by William Lott.
;;;
(in-package "SB!VM")



;;;; Length

(define-vop (length/list)
  (:translate length)
  (:args (object :scs (descriptor-reg) :target ptr))
  (:arg-types list)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) ptr)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (any-reg) :type fixnum :to (:result 0) :target result)
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

      (inst cmpw ptr null-tn)
      (inst beq done)

      (test-type ptr not-list t (list-pointer-lowtag) :temp temp)

      (loadw ptr ptr cons-cdr-slot list-pointer-lowtag)
      (inst addi count count (fixnumize 1))
      (test-type ptr loop nil (list-pointer-lowtag) :temp temp)

      (emit-label not-list)
      (error-call vop 'object-not-list-error ptr)

      (emit-label done)
      (move result count))))


(define-static-fun length (object) :translate length)

