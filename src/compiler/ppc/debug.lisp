;;;; PPC compiler support for the debugger

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
         (offset :scs (any-reg)))
  (:arg-types system-area-pointer positive-fixnum)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 5
    (inst lwzx result sap offset)))

(define-vop ()
  (:translate %set-stack-ref)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
         (offset :scs (any-reg))
         (value :scs (descriptor-reg)))
  (:arg-types system-area-pointer positive-fixnum *)
  (:generator 5
    (inst stwx value sap offset)))

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
      (inst srwi temp temp n-widetag-bits)
      (inst cmpwi temp 0)
      (inst slwi temp temp word-shift)
      (inst beq bogus)
      (unless (= lowtag other-pointer-lowtag)
        (inst addi temp temp (- lowtag other-pointer-lowtag)))
      (inst sub code thing temp)
      (emit-label done)
      (assemble (:elsewhere)
        (emit-label bogus)
        (move code null-tn)
        (inst b done)))))

(define-vop (code-from-lra code-from-mumble)
  (:translate sb-di::lra-code-header)
  (:variant other-pointer-lowtag))

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
