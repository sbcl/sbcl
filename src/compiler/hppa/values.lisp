(in-package "SB!VM")

(define-vop (reset-stack-pointer)
  (:args (ptr :scs (any-reg)))
  (:generator 1
    (move ptr csp-tn)))

(define-vop (%%pop-dx)
  (:args (ptr :scs (any-reg)))
  (:ignore ptr)
  (:generator 1
    (bug "VOP %%POP-DX is not implemented.")))

(define-vop (%%nip-dx)
  (:args (last-nipped-ptr :scs (any-reg) :target dest)
         (last-preserved-ptr :scs (any-reg) :target src)
         (moved-ptrs :scs (any-reg) :more t))
  (:results (r-moved-ptrs :scs (any-reg) :more t))
  (:temporary (:sc any-reg) src)
  (:temporary (:sc any-reg) dest)
  (:temporary (:sc non-descriptor-reg) temp)
  (:ignore r-moved-ptrs
           last-nipped-ptr last-preserved-ptr moved-ptrs
           src dest temp)
  (:generator 1
    (bug "VOP %%NIP-DX is not implemented.")))

(define-vop (%%nip-values)
  (:args (last-nipped-ptr :scs (any-reg) :target dest)
         (last-preserved-ptr :scs (any-reg) :target src)
         (moved-ptrs :scs (any-reg) :more t))
  (:results (r-moved-ptrs :scs (any-reg) :more t))
  (:temporary (:sc any-reg) src)
  (:temporary (:sc any-reg) dest)
  (:temporary (:sc non-descriptor-reg) temp)
  (:ignore r-moved-ptrs)
  (:generator 1
    (move last-preserved-ptr src)
    (move last-nipped-ptr dest)
    (inst comb :>= src csp-tn DONE :nullify t)
    LOOP
    (inst ldwm n-word-bytes src temp)
    (inst addi n-word-bytes dest dest)
    (storew temp dest -1)
    (inst comb :> csp-tn src LOOP)
    (inst nop)
    DONE
    (move dest csp-tn)
    (inst sub src dest src)
    (loop for moved = moved-ptrs then (tn-ref-across moved)
          while moved do
      (sc-case (tn-ref-tn moved)
        ((descriptor-reg any-reg)
          (inst sub (tn-ref-tn moved) src (tn-ref-tn moved)))
        ((control-stack)
          (load-stack-tn temp (tn-ref-tn moved))
          (inst sub temp src temp)
          (store-stack-tn (tn-ref-tn moved) temp))))))

;;; Push some values onto the stack, returning the start and number of values
;;; pushed as results.  It is assumed that the Vals are wired to the standard
;;; argument locations.  Nvals is the number of values to push.
;;;
;;; The generator cost is pseudo-random.  We could get it right by defining a
;;; bogus SC that reflects the costs of the memory-to-memory moves for each
;;; operand, but this seems unworthwhile.
;;;
(define-vop (push-values)
  (:args
   (vals :more t))
  (:results (start :scs (any-reg))
            (count :scs (any-reg)))
  (:info nvals)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg)
               :to (:result 0)
               :target start)
              start-temp)
  (:generator 20
    (move csp-tn start-temp)
    (inst addi (* nvals n-word-bytes) csp-tn csp-tn)
    (do ((val vals (tn-ref-across val))
         (i 0 (1+ i)))
        ((null val))
      (let ((tn (tn-ref-tn val)))
        (sc-case tn
          (descriptor-reg
           (storew tn start-temp i))
          (control-stack
           (load-stack-tn temp tn)
           (storew temp start-temp i)))))
    (move start-temp start)
    (inst li (fixnumize nvals) count)))

;;; Push a list of values on the stack, returning Start and Count as used in
;;; unknown values continuations.
;;;
(define-vop (values-list)
  (:args (arg :scs (descriptor-reg) :target list))
  (:arg-types list)
  (:policy :fast-safe)
  (:results (start :scs (any-reg))
            (count :scs (any-reg)))
  (:temporary (:scs (descriptor-reg) :type list :from (:argument 0)) list)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 0
    (move arg list)
    (move csp-tn start)
    LOOP
    (inst comb := list null-tn done)
    (loadw temp list cons-car-slot list-pointer-lowtag)
    (loadw list list cons-cdr-slot list-pointer-lowtag)
    (inst addi n-word-bytes csp-tn csp-tn)
    (storew temp csp-tn -1)
    (inst extru list 31 n-lowtag-bits ndescr)
    (inst comib := list-pointer-lowtag ndescr loop)
    (inst nop)
    (error-call vop 'bogus-arg-to-values-list-error list)
    DONE
    (inst sub csp-tn start count)))

;;; Copy the more arg block to the top of the stack so we can use them
;;; as function arguments.
;;;
(define-vop (%more-arg-values)
  (:args (context :scs (descriptor-reg any-reg) :target src)
         (skip :scs (any-reg zero immediate))
         (num :scs (any-reg) :target count))
  (:arg-types * positive-fixnum positive-fixnum)
  (:temporary (:sc any-reg :from (:argument 0)) src)
  (:temporary (:sc any-reg :from (:argument 2)) dst end)
  (:temporary (:sc descriptor-reg :from (:argument 1)) temp)
  (:results (start :scs (any-reg))
            (count :scs (any-reg)))
  (:generator 20
    (sc-case skip
      (zero
       (move context src))
      (immediate
       (inst addi (* (tn-value skip) n-word-bytes) context src))
      (any-reg
       (inst add skip context src)))
    (move num count)
    (inst comb := num zero-tn done)
    (move csp-tn start t)
    (move csp-tn dst)
    (inst add count csp-tn csp-tn)
    (inst addi (- n-word-bytes) csp-tn end)
    LOOP
    (inst ldwm n-word-bytes src temp)
    (inst comb :<> dst end loop)
    (inst stwm temp n-word-bytes dst)
    DONE))
