;;;; the Alpha implementation of unknown-values VOPs

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(define-vop (reset-stack-pointer)
  (:args (ptr :scs (any-reg)))
  (:generator 1
    (move ptr csp-tn)))

;;; Push some values onto the stack, returning the start and number of
;;; values pushed as results. It is assumed that the Vals are wired to
;;; the standard argument locations. Nvals is the number of values to
;;; push.
;;;
;;; The generator cost is pseudo-random. We could get it right by
;;; defining a bogus SC that reflects the costs of the
;;; memory-to-memory moves for each operand, but this seems
;;; unworthwhile.
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
    (move csp-tn start-temp)
    (inst lda csp-tn (* nvals n-word-bytes) csp-tn)
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

;;; Push a list of values on the stack, returning Start and Count as
;;; used in unknown values continuations.
(define-vop (values-list)
  (:args (arg :scs (descriptor-reg) :target list))
  (:arg-types list)
  (:policy :fast-safe)
  (:results (start :scs (any-reg))
	    (count :scs (any-reg)))
  (:temporary (:scs (descriptor-reg) :type list :from (:argument 0)) list)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 0
    (move arg list)
    (move csp-tn start)
    
    LOOP
    (inst cmpeq list null-tn temp)
    (inst bne temp done)
    (loadw temp list cons-car-slot list-pointer-lowtag)
    (loadw list list cons-cdr-slot list-pointer-lowtag)
    (inst lda csp-tn n-word-bytes csp-tn)
    (storew temp csp-tn -1)
    (inst and list lowtag-mask ndescr)
    (inst xor ndescr list-pointer-lowtag ndescr)
    (inst beq ndescr loop)
    (error-call vop bogus-argument-to-values-list-error list)
    
    DONE
    (inst subq csp-tn start count)))

;;; Copy the &MORE arg block to the top of the stack so we can use
;;; them as function arguments.
(define-vop (%more-arg-values)
  (:args (context :scs (descriptor-reg any-reg) :target src)
         (skip :scs (any-reg zero immediate))
         (num :scs (any-reg) :target count))
  (:arg-types * positive-fixnum positive-fixnum)
  (:temporary (:sc any-reg :from (:argument 0)) src)
  (:temporary (:sc any-reg :from (:argument 2)) dst)
  (:temporary (:sc descriptor-reg :from (:argument 1)) temp)
  (:temporary (:sc non-descriptor-reg) temp1)
  (:results (start :scs (any-reg))
            (count :scs (any-reg)))
  (:generator 20
    (sc-case skip
      (zero
       (move context src))
      (immediate
       (inst lda src (* (tn-value skip) n-word-bytes) context))
      (any-reg
       (inst addq context skip src)))
    (move num count)
    (inst move csp-tn start)
    (inst beq num done)
    (inst move csp-tn dst)
    (inst addq csp-tn count csp-tn)
    LOOP
    (inst ldl temp 0 src)
    (inst addq src 4 src)
    (inst addq dst 4 dst)
    (inst stl temp -4 dst)
    (inst cmpeq dst csp-tn temp1)
    (inst beq temp1 loop)
    DONE))
