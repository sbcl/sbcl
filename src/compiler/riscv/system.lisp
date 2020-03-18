;;;; RISC-V VM definitions of various system hacking operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; Type frobbing VOPs

(define-vop (widetag-of)
  (:translate widetag-of)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (interior-reg)) lip)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    ;; First, pick off the immediate types, starting with FIXNUM.
    (inst andi result object fixnum-tag-mask)
    (inst beq result zero-tn done)
    ;; If it wasn't a fixnum, start with the full widetag.
    (inst andi result object widetag-mask)
    ;; Now, we have our result for an immediate type, but we might
    ;; have a pointer object instead, in which case we need to do more
    ;; work.  Check for a pointer type.
    (inst andi ndescr object #-64-bit #b1 #+64-bit #b10)
    (inst beq ndescr zero-tn done)
    ;; If we have a pointer type, we need to compute a different
    ;; answer.  For lists and instances, we just need the lowtag.  For
    ;; functions and "other", we need to load the widetag from the
    ;; object header.  In both cases, having just the widetag
    ;; available is handy.
    (inst andi result object lowtag-mask)

    ;; We now have the correct answer for list-pointer-lowtag and
    ;; instance-pointer-lowtag, but need to pick off the case for the
    ;; other two pointer types.  KLUDGE: FUN-POINTER-LOWTAG and
    ;; OTHER-POINTER-LOWTAG are both in the upper half of the lowtag
    ;; space, while LIST-POINTER-LOWTAG and INSTANCE-POINTER-LOWTAG
    ;; are in the lower half, so we distinguish with a bit test.
    (inst andi ndescr object #-64-bit #b100 #+64-bit #b1000)
    (inst beq ndescr zero-tn done)

    ;; And, finally, pick out the widetag from the header.
    (inst sub lip object result)
    (load-type result lip)
    DONE))

#+64-bit
(define-vop (layout-depthoid)
  (:translate layout-depthoid)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:results (values :scs (any-reg)))
  (:result-types fixnum)
  (:generator 1
    (inst lw values object
          (- (+ (ash (+ instance-slots-offset
                        (get-dsd-index layout sb-kernel::%bits))
                     word-shift)
                4)
             instance-pointer-lowtag))))

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
    (inst srli res res n-widetag-bits)))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 other-pointer-lowtag)
    (inst srli res res n-widetag-bits)))

(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target res)
         (data :scs (any-reg immediate)))
  (:arg-types * positive-fixnum)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) t1 t2)
  (:generator 6
    (load-type t1 x (- other-pointer-lowtag))
    (sc-case data
      (any-reg
       (inst slli t2 data (- n-widetag-bits n-fixnum-tag-bits))
       (inst or t1 t1 t2))
      (immediate
       (let ((val (ash (tn-value data) n-widetag-bits)))
         (cond ((typep val 'short-immediate)
                (inst ori t1 t1 val))
               (t
                (inst li t2 val)
                (inst or t1 t1 t2))))))
    (storew t1 x 0 other-pointer-lowtag)
    (move res x)))

(define-vop (pointer-hash)
  (:translate pointer-hash)
  (:args (ptr :scs (any-reg descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:policy :fast-safe)
  (:generator 1
    (inst andi res ptr (lognot fixnum-tag-mask))))


;;;; Allocation

(define-vop (dynamic-space-free-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate dynamic-space-free-pointer)
  (:policy :fast-safe)
  (:generator 1
    (load-symbol-value int *allocation-pointer*)))

(define-vop (binding-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate binding-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (load-binding-stack-pointer int)))

(define-vop (control-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate control-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int csp-tn)))


;;;; Code object frobbing.

(define-vop (code-instructions)
  (:translate code-instructions)
  (:policy :fast-safe)
  (:args (code :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 10
    ;; 4 byte load, ignoring serial# in the high bits.
    (inst #-64-bit lw #+64-bit lwu ndescr code (- (* n-word-bytes code-boxed-size-slot) other-pointer-lowtag))
    (inst subi ndescr ndescr other-pointer-lowtag)
    (inst add sap code ndescr)))

(eval-when (:compile-toplevel)
  (aver (not (logtest code-header-widetag #b11000000))))

(define-vop (code-trailer-ref)
  (:translate code-trailer-ref)
  (:policy :fast-safe)
  (:args (code :scs (descriptor-reg) :to (:result 0))
         (offset :scs (signed-reg) :to (:result 0)))
  (:arg-types * fixnum)
  (:results (res :scs (unsigned-reg) :from (:argument 0)))
  (:temporary (:scs (interior-reg)) lip)
  (:result-types unsigned-num)
  (:generator 10
    #-64-bit
    (progn
      (loadw res code 0 other-pointer-lowtag)
      (inst slli res res 2) ; shift out the GC bits
      ;; Then shift right to clear the widetag, plus 2 more to the right since we just
      ;; left-shifted to zeroize bits. Then shift left 2 to convert words to bytes.
      ;; The '>>2' and '<<2' cancel out because we don't need to clear all 8 bits
      ;; of the widetag, as CODE-HEADER-WIDETAG already has bits 6 and 7 clear.
      ;; Other places assume the same, though without much commentary.
      ;; It's brittle magic, save for the AVER above which ensures that it works.
      (inst srli res res n-widetag-bits))
    #+64-bit
    (progn
      (inst lwu res code (- 4 other-pointer-lowtag))
      (inst slli res res word-shift))
    (inst add res offset res)
    (inst add lip code res)
    (inst #-64-bit lw #+64-bit lwu res lip (- other-pointer-lowtag))))

(define-vop (compute-fun)
  (:args (code :scs (descriptor-reg))
         (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (inst #-64-bit lw #+64-bit lwu ndescr code (- (* n-word-bytes code-boxed-size-slot) other-pointer-lowtag))
    (inst add ndescr ndescr offset)
    (inst subi ndescr ndescr (- other-pointer-lowtag fun-pointer-lowtag))
    (inst add func code ndescr)))
;;;
(define-vop (symbol-info-vector)
  (:policy :fast-safe)
  (:translate symbol-info-vector)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) temp)
  (:generator 1
    (loadw res x symbol-info-slot other-pointer-lowtag)
    ;; If RES has list-pointer-lowtag, take its CDR. If not, use it as-is.
    (inst andi temp res lowtag-mask)
    (inst xori temp temp list-pointer-lowtag)
    (inst bne temp zero-tn not-equal)
    (loadw res res cons-cdr-slot list-pointer-lowtag)
    NOT-EQUAL))

(define-vop (symbol-plist)
  (:policy :fast-safe)
  (:translate symbol-plist)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (descriptor-reg)))
  (:temporary (:sc non-descriptor-reg) temp)
  (:generator 1
    (loadw res x symbol-info-slot other-pointer-lowtag)
    ;; Instruction pun: (CAR x) is the same as (VECTOR-LENGTH x)
    ;; so if the info slot holds a vector, this gets a fixnum- it's not a plist.
    (loadw res res cons-car-slot list-pointer-lowtag)
    (inst andi temp res fixnum-tag-mask)
    (inst bne temp zero-tn not-equal)
    (move res null-tn)
    NOT-EQUAL))


;;;; Other random VOPs.

(defknown sb-unix::receive-pending-interrupt () (values))
(define-vop (sb-unix::receive-pending-interrupt)
  (:policy :fast-safe)
  (:translate sb-unix::receive-pending-interrupt)
  (:generator 1
    (inst ebreak pending-interrupt-trap)
    (emit-alignment 2)))

(define-vop (halt)
  (:generator 1
    (inst ebreak halt-trap)
    (emit-alignment 2)))

;;;; Dummy definition for a spin-loop hint VOP
(define-vop ()
  (:translate spin-loop-hint)
  (:policy :fast-safe)
  (:generator 0))
