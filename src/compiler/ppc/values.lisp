;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted for SPARC by William Lott.
;;; 

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
  (:args (vals :more t))
  (:results (start :scs (any-reg) :from :load)
	    (count :scs (any-reg)))
  (:info nvals)
  (:temporary (:scs (descriptor-reg)) temp)
  (:generator 20
    (inst mr start csp-tn)
    (inst addi csp-tn csp-tn (* nvals sb!vm:n-word-bytes))
    (do ((val vals (tn-ref-across val))
	 (i 0 (1+ i)))
	((null val))
      (let ((tn (tn-ref-tn val)))
	(sc-case tn
	  (descriptor-reg
	   (storew tn start i))
	  (control-stack
	   (load-stack-tn temp tn)
	   (storew temp start i)))))
    (inst lr count (fixnumize nvals))))

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
    (let ((loop (gen-label))
	  (done (gen-label)))

      (move list arg)
      (move start csp-tn)

      (emit-label loop)
      (inst cmpw list null-tn)
      (loadw temp list sb!vm:cons-car-slot sb!vm:list-pointer-lowtag)
      (inst beq done)
      (loadw list list sb!vm:cons-cdr-slot sb!vm:list-pointer-lowtag)
      (inst addi csp-tn csp-tn sb!vm:n-word-bytes)
      (storew temp csp-tn -1)
      (test-type list ndescr loop nil sb!vm:list-pointer-lowtag)
      (error-call vop bogus-arg-to-values-list-error list)

      (emit-label done)
      (inst sub count csp-tn start))))


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
  (:temporary (:sc any-reg) i)
  (:results (start :scs (any-reg))
	    (count :scs (any-reg)))
  (:generator 20
    (sc-case skip
      (zero
       (inst mr src context))
      (immediate
       (inst addi src context (* (tn-value skip) n-word-bytes)))
      (any-reg
       (inst add src context skip)))
    (inst mr. count num)
    (inst mr start csp-tn)
    (inst beq done)
    (inst mr dst csp-tn)
    (inst add csp-tn csp-tn count)
    (inst mr i count)
    LOOP
    (inst cmpwi i 4)
    (inst subi i i 4)
    (inst lwzx temp src i)
    (inst stwx temp dst i)
    (inst bne loop)
    DONE))
