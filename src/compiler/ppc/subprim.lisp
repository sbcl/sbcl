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
	  (not-list (generate-cerror-code vop object-not-list-error object)))
      (move ptr object)
      (move count zero-tn)

      (emit-label loop)

      (inst cmpw ptr null-tn)
      (inst beq done)

      (test-type ptr temp not-list t sb!vm:list-pointer-lowtag)

      (loadw ptr ptr sb!vm:cons-cdr-slot sb!vm:list-pointer-lowtag)
      (inst addi count count (fixnumize 1))
      (test-type ptr temp loop nil sb!vm:list-pointer-lowtag)

      (cerror-call vop done object-not-list-error ptr)

      (emit-label done)
      (move result count))))
       

(define-static-fun length (object) :translate length)

