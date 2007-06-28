;;;; MIPS compiler support for the debugger

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")


(define-vop (debug-cur-sp)
  (:translate sb!di::current-sp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res csp-tn)))

(define-vop (debug-cur-fp)
  (:translate sb!di::current-fp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res cfp-tn)))

(define-vop (read-control-stack)
  (:translate sb!kernel:stack-ref)
  (:policy :fast-safe)
  (:args (object :scs (sap-reg) :target sap)
         (offset :scs (any-reg)))
  (:arg-types system-area-pointer positive-fixnum)
  (:temporary (:scs (sap-reg) :from :eval) sap)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 5
    (inst addu sap object offset)
    (inst lw result sap 0)
    (inst nop)))

(define-vop (read-control-stack-c)
  (:translate sb!kernel:stack-ref)
  (:policy :fast-safe)
  (:args (object :scs (sap-reg)))
  (:info offset)
  (:arg-types system-area-pointer (:constant (signed-byte 14)))
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 4
    (inst lw result object (* offset n-word-bytes))
    (inst nop)))

(define-vop (write-control-stack)
  (:translate sb!kernel:%set-stack-ref)
  (:policy :fast-safe)
  (:args (object :scs (sap-reg) :target sap)
         (offset :scs (any-reg))
         (value :scs (descriptor-reg) :target result))
  (:arg-types system-area-pointer positive-fixnum *)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:temporary (:scs (sap-reg) :from (:argument 1)) sap)
  (:generator 2
    (inst addu sap object offset)
    (inst sw value sap 0)
    (move result value)))

(define-vop (write-control-stack-c)
  (:translate %set-stack-ref)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
         (value :scs (descriptor-reg) :target result))
  (:info offset)
  (:arg-types system-area-pointer (:constant (signed-byte 14)) *)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 1
    (inst sw value sap (* offset n-word-bytes))
    (move result value)))


(define-vop (code-from-mumble)
  (:policy :fast-safe)
  (:args (thing :scs (descriptor-reg)))
  (:results (code :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:variant-vars lowtag)
  (:generator 5
    (let ((bogus (gen-label))
          (done (gen-label)))
      (loadw temp thing 0 lowtag)
      (inst srl temp n-widetag-bits)
      (inst beq temp bogus)
      (inst sll temp (1- (integer-length n-word-bytes)))
      (unless (= lowtag other-pointer-lowtag)
        (inst addu temp (- lowtag other-pointer-lowtag)))
      (inst subu code thing temp)
      (emit-label done)
      (assemble (*elsewhere*)
        (emit-label bogus)
        (inst b done)
        (move code null-tn t)))))

(define-vop (code-from-lra code-from-mumble)
  (:translate sb!di::lra-code-header)
  (:variant other-pointer-lowtag))

(define-vop (code-from-fun code-from-mumble)
  (:translate sb!di::fun-code-header)
  (:variant fun-pointer-lowtag))

(define-vop (%make-lisp-obj)
  (:policy :fast-safe)
  (:translate %make-lisp-obj)
  (:args (value :scs (unsigned-reg) :target result))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:generator 1
    (move result value)))

(define-vop (get-lisp-obj-address)
  (:policy :fast-safe)
  (:translate sb!di::get-lisp-obj-address)
  (:args (thing :scs (descriptor-reg) :target result))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (move result thing)))

(define-vop (fun-word-offset)
  (:policy :fast-safe)
  (:translate sb!di::fun-word-offset)
  (:args (fun :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 5
    (loadw res fun 0 fun-pointer-lowtag)
    (inst srl res n-widetag-bits)))
