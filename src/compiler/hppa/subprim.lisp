(in-package "SB!VM")



;;;; Length

(define-vop (length/list)
  (:translate length)
  (:args (object :scs (descriptor-reg) :target ptr))
  (:arg-types list)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) ptr)
  (:temporary (:scs (non-descriptor-reg) :type random) temp)
  (:temporary (:scs (any-reg) :type fixnum :to (:result 0) :target result)
              count)
  (:results (result :scs (any-reg descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 50
    (move object ptr)
    (inst comb := ptr null-tn done)
    (inst li 0 count)

    (inst extru ptr 31 3 temp)
    (inst comib :<> list-pointer-lowtag temp lose :nullify t)
    (loadw ptr ptr cons-cdr-slot list-pointer-lowtag)

    LOOP
    (inst addi (fixnumize 1) count count)
    (inst comb := ptr null-tn done :nullify t)
    (inst extru ptr 31 3 temp)
    (inst comib := list-pointer-lowtag temp loop :nullify t)
    (loadw ptr ptr cons-cdr-slot list-pointer-lowtag)

    LOSE
    (cerror-call vop done 'object-not-list-error ptr)

    DONE
    (move count result)))

(define-static-fun length (object) :translate length)
