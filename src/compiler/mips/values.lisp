(in-package "SB!VM")

(define-vop (reset-stack-pointer)
  (:args (ptr :scs (any-reg)))
  (:generator 1
    (move csp-tn ptr)))


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
  (:results
   (start :scs (any-reg))
   (count :scs (any-reg)))
  (:info nvals)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg)
	       :to (:result 0)
	       :target start)
	      start-temp)
  (:generator 20
    (move start-temp csp-tn)
    (inst addu csp-tn csp-tn (* nvals n-word-bytes))
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
    (move start start-temp)
    (inst li count (fixnumize nvals))))


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
    (move list arg)
    (move start csp-tn)
    
    LOOP
    (inst beq list null-tn done)
    (loadw temp list cons-car-slot list-pointer-lowtag)
    (loadw list list cons-cdr-slot list-pointer-lowtag)
    (inst addu csp-tn csp-tn n-word-bytes)
    (storew temp csp-tn -1)
    (inst and ndescr list lowtag-mask)
    (inst xor ndescr list-pointer-lowtag)
    (inst beq ndescr zero-tn loop)
    (inst nop)
    (error-call vop bogus-arg-to-values-list-error list)
    
    DONE
    (inst subu count csp-tn start)))


;;; Copy the more arg block to the top of the stack so we can use them
;;; as function arguments.
;;;
(define-vop (%more-arg-values)
  (:args (context :scs (descriptor-reg any-reg) :target src)
	 (skip :scs (any-reg zero immediate))
	 (num :scs (any-reg) :target count))
  (:arg-types * positive-fixnum positive-fixnum)
  (:temporary (:sc any-reg :from (:argument 0)) src)
  (:temporary (:sc any-reg :from (:argument 2)) dst)
  (:temporary (:sc descriptor-reg :from (:argument 1)) temp)
  (:results (start :scs (any-reg))
	    (count :scs (any-reg)))
  (:generator 20
    (sc-case skip
      (zero
       (move src context))
      (immediate
       (inst addu src context (* (tn-value skip) n-word-bytes)))
      (any-reg
       (inst addu src context skip)))
    (move count num)
    (inst beq num zero-tn done)
    (inst move start csp-tn)
    (inst move dst csp-tn)
    (inst addu csp-tn count)
    LOOP
    (inst lw temp src)
    (inst addu src 4)
    (inst addu dst 4)
    (inst bne dst csp-tn loop)
    (inst sw temp dst -4)
    DONE))
