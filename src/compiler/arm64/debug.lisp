;;;; ARM compiler support for the debugger

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(define-vop ()
  (:translate current-sp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res csp-tn)))

(define-vop (current-fp-sap)
  (:translate current-fp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res cfp-tn)))

(define-vop ()
  (:translate stack-ref)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
         (offset :scs (any-reg) :target temp))
  (:arg-types system-area-pointer positive-fixnum)
  (:temporary (:scs (any-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 5
    (inst lsl temp offset (- word-shift n-fixnum-tag-bits))
    (inst ldr result (@ sap temp))))

(define-vop ()
  (:translate %set-stack-ref)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
         (offset :scs (any-reg) :target temp)
         (value :scs (descriptor-reg)))
  (:arg-types system-area-pointer positive-fixnum *)
  (:temporary (:scs (any-reg)) temp)
  (:generator 5
    (inst lsl temp offset (- word-shift n-fixnum-tag-bits))
    (inst str value (@ sap temp))))

(define-vop (code-from-mumble)
  (:policy :fast-safe)
  (:args (thing :scs (descriptor-reg) :to :result))
  (:results (code :scs (descriptor-reg) :from :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:variant-vars lowtag)
  (:generator 5
    (move code null-tn)
    (loadw temp thing 0 lowtag)
    (inst lsr temp temp n-widetag-bits)
    (inst cbz temp DONE)
    (inst lsl temp temp word-shift)
    (unless (= lowtag other-pointer-lowtag)
      (inst sub temp temp (- other-pointer-lowtag lowtag)))
    (inst sub code thing temp)
    DONE))

(define-vop (code-from-fun code-from-mumble)
  (:translate sb-di::fun-code-header)
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
  (:translate sb-di::get-lisp-obj-address)
  (:args (thing :scs (descriptor-reg any-reg) :target result))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (move result thing)))
