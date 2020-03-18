;;;; Alpha VM definitions of various system hacking operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; type frobbing VOPs

(define-vop (widetag-of)
  (:translate widetag-of)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    ;; Pick off objects with headers.
    (inst and object lowtag-mask result)
    (inst cmpeq result other-pointer-lowtag ndescr)
    (inst bne ndescr other-ptr)
    (inst cmpeq result fun-pointer-lowtag ndescr)
    (inst bne ndescr function-ptr)

    ;; Pick off structure and list pointers.
    (inst blbs object done)

    ;; Pick off fixnums.
    (inst and object fixnum-tag-mask result)
    (inst beq result done)

    ;; Must be an other immediate.
    (inst and object widetag-mask result)
    (inst br zero-tn done)

    FUNCTION-PTR
    (load-type result object (- fun-pointer-lowtag))
    (inst br zero-tn done)

    OTHER-PTR
    (load-type result object (- other-pointer-lowtag))

    DONE))

(define-vop (%other-pointer-widetag)
  (:translate %other-pointer-widetag)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type result object (- other-pointer-lowtag))))

(define-vop (fun-subtype)
  (:translate fun-subtype)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type result function (- fun-pointer-lowtag))))

(define-vop (fun-header-data)
  (:translate fun-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 fun-pointer-lowtag)
    (inst srl res n-widetag-bits res)))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 other-pointer-lowtag)
    (inst srl res n-widetag-bits res)))

(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target res)
         (data :scs (any-reg immediate zero)))
  (:arg-types * positive-fixnum)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) t1 t2)
  (:generator 6
    (loadw t1 x 0 other-pointer-lowtag)
    (inst and t1 widetag-mask t1)
    (sc-case data
      (any-reg
       (inst sll data (- n-widetag-bits n-fixnum-tag-bits) t2)
       (inst bis t1 t2 t1))
      (immediate
       (let ((c (ash (tn-value data) n-widetag-bits)))
         (cond ((<= 0 c (1- (ash 1 8)))
                (inst bis t1 c t1))
               (t
                (inst li c t2)
                (inst bis t1 t2 t1)))))
      (zero))
    (storew t1 x 0 other-pointer-lowtag)
    (move x res)))

(define-vop (pointer-hash)
  (:translate pointer-hash)
  (:args (ptr :scs (any-reg descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:policy :fast-safe)
  (:generator 1
    ;; FIXME: It would be better if this would mask the lowtag,
    ;; and shift the result into a positive fixnum like on x86.
    (inst sll ptr 35 res)
    (inst srl res 33 res)))


;;;; allocation

(define-vop (dynamic-space-free-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate dynamic-space-free-pointer)
  (:policy :fast-safe)
  (:generator 1
    (move alloc-tn int)))

(define-vop (binding-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate binding-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move bsp-tn int)))

(define-vop (control-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate control-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move csp-tn int)))


;;;; code object frobbing

(define-vop (code-instructions)
  (:translate code-instructions)
  (:policy :fast-safe)
  (:args (code :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 10
    (loadw ndescr code 0 other-pointer-lowtag)
    (inst srl ndescr n-widetag-bits ndescr)
    (inst sll ndescr word-shift ndescr)
    (inst subq ndescr other-pointer-lowtag ndescr)
    (inst addq code ndescr sap)))

(define-vop (compute-fun)
  (:args (code :scs (descriptor-reg))
         (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (loadw ndescr code code-boxed-size-slot other-pointer-lowtag)
    (inst addq ndescr offset ndescr)
    (inst subq ndescr (- other-pointer-lowtag fun-pointer-lowtag) ndescr)
    (inst addq code ndescr func)))

;;;; other random VOPs.

(defknown sb-unix::receive-pending-interrupt () (values))
(define-vop (sb-unix::receive-pending-interrupt)
  (:policy :fast-safe)
  (:translate sb-unix::receive-pending-interrupt)
  (:generator 1
    (inst gentrap pending-interrupt-trap)))


(define-vop (halt)
  (:generator 1
    (inst gentrap halt-trap)))

(define-vop (istream-memory-barrier)
  (:generator 1
    (inst imb)))

;;;; dynamic vop count collection support

(define-vop (count-me)
  (:args (count-vector :scs (descriptor-reg)))
  (:info index)
  (:temporary (:scs (non-descriptor-reg)) count)
  (:generator 1
    (let ((offset
           (- (* (+ index vector-data-offset) n-word-bytes)
              other-pointer-lowtag)))
      (inst ldl count offset count-vector)
      (inst addq count 1 count)
      (inst stl count offset count-vector))))

;;;; Dummy definition for a spin-loop hint VOP
(define-vop ()
  (:translate spin-loop-hint)
  (:policy :fast-safe)
  (:generator 0))
