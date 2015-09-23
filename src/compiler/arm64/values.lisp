;;;; unknown-values VOPs for the ARM VM

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
    (move csp-tn ptr)))

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
    (move src last-preserved-ptr)
    (move dest last-nipped-ptr)
    (inst cmp csp-tn src)
    (inst b :le DONE)
    LOOP
    (loadw temp src)
    (inst add dest dest n-word-bytes)
    (inst add src src n-word-bytes)
    (storew temp dest -1)
    (inst cmp csp-tn src)
    (inst b :gt LOOP)
    DONE
    (move csp-tn dest)
    (inst sub src src dest)
    (loop for moved = moved-ptrs then (tn-ref-across moved)
          while moved
          do (sc-case (tn-ref-tn moved)
               ((descriptor-reg any-reg)
                (inst sub (tn-ref-tn moved) (tn-ref-tn moved) src))
               ((control-stack)
                (load-stack-tn temp (tn-ref-tn moved))
                (inst sub temp temp src)
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
  (:args (vals :more t))
  (:results (start :scs (any-reg) :from :load)
            (count :scs (any-reg)))
  (:info nvals)
  (:temporary (:scs (descriptor-reg)) temp)
  (:generator 20
    (move start csp-tn)
    (inst add csp-tn csp-tn (* nvals n-word-bytes))
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
    (inst mov count (fixnumize nvals))))

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
    (inst cmp list null-tn)
    (loadw temp list cons-car-slot list-pointer-lowtag)
    (inst b :eq DONE)
    (loadw list list cons-cdr-slot list-pointer-lowtag)
    (inst add csp-tn csp-tn n-word-bytes)
    (storew temp csp-tn -1)
    (test-type list LOOP nil (list-pointer-lowtag) :temp ndescr)
    (error-call vop 'bogus-arg-to-values-list-error list)

    DONE
    (inst sub count csp-tn start)
    (inst asr count count (- word-shift n-fixnum-tag-bits))))


;;; Copy the more arg block to the top of the stack so we can use them
;;; as function arguments.
;;;
(define-vop (%more-arg-values)
  (:args (context :scs (descriptor-reg any-reg) :target src)
         (skip :scs (any-reg immediate))
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
      (immediate
       (inst add src context (* (tn-value skip) n-word-bytes)))
      (any-reg
       (inst add src context (lsl skip (- word-shift n-fixnum-tag-bits)))))
    (inst adds count num 0)
    (move start csp-tn)
    (inst b :eq DONE)
    (inst mov dst start)
    (inst lsl i count (- word-shift n-fixnum-tag-bits))
    (inst add csp-tn start i)
    LOOP
    (inst subs i i n-word-bytes)
    (inst ldr temp (@ src i))
    (inst str temp (@ dst i))
    (inst b :ne LOOP)
    DONE))
