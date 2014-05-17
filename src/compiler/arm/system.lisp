;;;; ARM VM definitions of various system hacking operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; Type frobbing VOPs

(define-vop (lowtag-of)
  (:translate lowtag-of)
  (:policy :fast-safe)
  (:args (object :scs (any-reg descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst and result object lowtag-mask)))

(define-vop (widetag-of)
  (:translate widetag-of)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 1)))
  (:results (result :scs (unsigned-reg) :from (:eval 0)))
  (:result-types positive-fixnum)
  (:generator 6
    ;; First, pick off the immediate types, starting with FIXNUM.
    (inst ands result object fixnum-tag-mask)
    ;; If it wasn't a fixnum, start with the full widetag.
    (inst and :ne result object widetag-mask)

    ;; Now, we have our result for an immediate type, but we might
    ;; have a pointer object instead, in which case we need to do more
    ;; work.  Check for a pointer type.

    ;; KLUDGE: We're a 32-bit port, so all pointer lowtags have the
    ;; low bit set, but there's no obvious named constant for this.
    ;; On 64-bit ports, all pointer lowtags have the low two bits set,
    ;; so this wouldn't work as easily.
    (inst tst object 1)

    ;; If we have a pointer type, we need to compute a different
    ;; answer.  For lists and instances, we just need the lowtag.  For
    ;; functions and "other", we need to load the widetag from the
    ;; object header.  In both cases, having just the widetag
    ;; available is handy.
    (inst and :ne result object lowtag-mask)

    ;; We now have the correct answer for list-pointer-lowtag and
    ;; instance-pointer-lowtag, but need to pick off the case for the
    ;; other two pointer types.  KLUDGE: FUN-POINTER-LOWTAG and
    ;; OTHER-POINTER-LOWTAG are both in the upper half of the lowtag
    ;; space, while LIST-POINTER-LOWTAG and INSTANCE-POINTER-LOWTAG
    ;; are in the lower half, so we distinguish with a bit test.
    (inst tst :ne object 4)

    ;; We can't use both register and immediate offsets in the same
    ;; load/store instruction, so we need to bias our register offset
    ;; on big-endian systems.
    (when (eq *backend-byte-order* :big-endian)
      (inst sub :ne result (1- n-word-bytes)))

    ;; And, finally, pick out the widetag from the header.
    (inst ldrb :ne result (@ object (- result)))))


(define-vop (fun-subtype)
  (:translate fun-subtype)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type result function (- fun-pointer-lowtag))))

(define-vop (set-fun-subtype)
  (:translate (setf fun-subtype))
  (:policy :fast-safe)
  (:args (type :scs (unsigned-reg) :target result)
         (function :scs (descriptor-reg)))
  (:arg-types positive-fixnum *)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (inst strb type (@ function (- (ecase *backend-byte-order*
                                     (:little-endian 0)
                                     (:big-endian (1- n-word-bytes)))
                                   fun-pointer-lowtag)))
    (move result type)))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 other-pointer-lowtag)
    (inst mov res (lsr res n-widetag-bits))))

(define-vop (get-closure-length)
  (:translate get-closure-length)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 fun-pointer-lowtag)
    (inst mov res (lsr res n-widetag-bits))))

(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target res)
         (data :scs (any-reg immediate)))
  (:arg-types * positive-fixnum)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) t1)
  (:generator 6
    (load-type t1 x (- other-pointer-lowtag))
    (sc-case data
      (any-reg
       (inst orr t1 t1 (lsl data (- n-widetag-bits n-fixnum-tag-bits))))
      (immediate
       ;; FIXME: This will break if DATA has bits spread over more
       ;; than an eight bit range aligned on an even bit position.
       ;; See SYS:SRC;COMPILER;ARM;MOVE.LISP for a partial fix...  And
       ;; maybe it should be promoted to an instruction-macro?
       (inst orr t1 t1 (ash (tn-value data) n-widetag-bits))))
    (storew t1 x 0 other-pointer-lowtag)
    (move res x)))


(define-vop (pointer-hash)
  (:translate pointer-hash)
  (:args (ptr :scs (any-reg descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:policy :fast-safe)
  (:generator 1
    (inst bic res ptr lowtag-mask)
    (inst mov res (lsr res 1))))

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
    (load-symbol-value int *binding-stack-pointer*)))

(define-vop (control-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate control-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (load-csp int)))

;;;; Code object frobbing.

(define-vop (code-instructions)
  (:translate code-instructions)
  (:policy :fast-safe)
  (:args (code :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 10
    (loadw ndescr code 0 other-pointer-lowtag)
    ;; CODE-HEADER-WIDETAG is #x38, which has the top two bits clear,
    ;; so we don't to clear the low bits here.  If we do, use BIC.
    (inst mov ndescr (lsr ndescr (- n-widetag-bits word-shift)))
    (inst sub ndescr ndescr other-pointer-lowtag)
    (inst add sap code ndescr)))

(define-vop (compute-fun)
  (:args (code :scs (descriptor-reg))
         (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (loadw ndescr code 0 other-pointer-lowtag)
    ;; CODE-HEADER-WIDETAG is #x38, which has the top two bits clear,
    ;; so we don't to clear the low bits here.  If we do, use BIC.
    (inst add ndescr offset (lsr ndescr (- n-widetag-bits word-shift)))
    (inst sub ndescr ndescr (- other-pointer-lowtag fun-pointer-lowtag))
    (inst add func code ndescr)))
;;;
#!+symbol-info-vops
(define-vop (symbol-info-vector)
  (:policy :fast-safe)
  (:translate symbol-info-vector)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) temp)
  (:generator 1
    (loadw res x symbol-info-slot other-pointer-lowtag)
    ;; If RES has list-pointer-lowtag, take its CDR. If not, use it as-is.
    (inst and temp res lowtag-mask)
    (inst cmp temp list-pointer-lowtag)
    (loadw res res cons-cdr-slot list-pointer-lowtag :eq)))

#!+symbol-info-vops
(define-vop (symbol-plist)
  (:policy :fast-safe)
  (:translate symbol-plist)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (descriptor-reg)))
  (:generator 1
    (loadw res x symbol-info-slot other-pointer-lowtag)
    ;; Instruction pun: (CAR x) is the same as (VECTOR-LENGTH x)
    ;; so if the info slot holds a vector, this gets a fixnum- it's not a plist.
    (loadw res res cons-car-slot list-pointer-lowtag)
    (inst tst res fixnum-tag-mask)
    (inst mov :eq res null-tn)))

;;;; other miscellaneous VOPs

(defknown sb!unix::receive-pending-interrupt () (values))
(define-vop (sb!unix::receive-pending-interrupt)
  (:policy :fast-safe)
  (:translate sb!unix::receive-pending-interrupt)
  (:generator 1
    (inst debug-trap)
    (inst byte pending-interrupt-trap)
    (emit-alignment word-shift)))

(define-vop (halt)
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) error-temp)
  (:generator 1
    ;; See macros.lisp, EMIT-ERROR-BREAK, for an explanation.
    (inst mov error-temp #x000f0000)
    (inst add error-temp error-temp 1)
    (inst swi 0)
    (inst byte halt-trap)
    ;; Re-align to the next instruction boundary.
    (emit-alignment word-shift)))

;;;; Dummy definition for a spin-loop hint VOP
(define-vop (spin-loop-hint)
  (:translate spin-loop-hint)
  (:policy :fast-safe)
  (:generator 0))
