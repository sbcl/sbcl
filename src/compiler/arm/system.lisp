;;;; ARM VM definitions of various system hacking operations

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

(define-vop ()
  (:translate sb-c::%structure-is-a)
  (:args (x :scs (descriptor-reg)))
  (:arg-types * (:constant t))
  (:policy :fast-safe)
  (:conditional :eq)
  (:info test-layout)
  (:temporary (:sc unsigned-reg) this-id)
  (:generator 4
    (let ((test-id (layout-id test-layout))
          (offset (+ (id-bits-offset)
                     (ash (- (layout-depthoid test-layout) 2) 2)
                     (- instance-pointer-lowtag))))
      (inst ldr this-id (@ x offset))
      ;; 8-bit IDs are permanently assigned, so no fixup ever needed for those.
      (cond ((typep test-id '(and (unsigned-byte 8) (not (eql 0))))
             (inst cmp this-id test-id))
            (t
             (inst .layout-id test-layout)
             ;; The fixupper will rewrite these three instructions.
             (inst eor this-id this-id (ash 255 16))
             (inst eor this-id this-id (ash 255 8))
             (inst cmp this-id 255))))))

(define-vop (%other-pointer-widetag)
  (:translate %other-pointer-widetag)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type result object (- other-pointer-lowtag))))

(define-vop ()
  (:translate %fun-pointer-widetag)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type result function (- fun-pointer-lowtag))))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 other-pointer-lowtag)
    (inst mov res (lsr res n-widetag-bits))))

(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (data :scs (any-reg immediate)))
  (:arg-types * positive-fixnum)
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
    (storew t1 x 0 other-pointer-lowtag)))

;;;; Allocation

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
    (loadw ndescr code code-boxed-size-slot other-pointer-lowtag)
    (inst sub ndescr ndescr other-pointer-lowtag)
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
  (:result-types unsigned-num)
  (:generator 10
    (loadw res code 0 other-pointer-lowtag)
    (inst mov res (lsl res 2)) ; shift out the GC bits
    ;; Then shift right to clear the widetag, plus 2 more to the right since we just
    ;; left-shifted to zeroize bits. Then shift left 2 to convert words to bytes.
    ;; The '>>2' and '<<2' cancel out because we don't need to clear all 8 bits
    ;; of the widetag, as CODE-HEADER-WIDETAG already has bits 6 and 7 clear.
    ;; Other places assume the same, though without much commentary.
    ;; It's brittle magic, save for the AVER above which ensures that it works.
    (inst add res offset (lsr res n-widetag-bits))
    (inst sub res res other-pointer-lowtag)
    (inst ldr res (@ code res))))

(define-vop (compute-fun)
  (:args (code :scs (descriptor-reg))
         (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (loadw ndescr code code-boxed-size-slot other-pointer-lowtag)
    (inst add ndescr offset ndescr)
    (inst sub ndescr ndescr (- other-pointer-lowtag fun-pointer-lowtag))
    (inst add func code ndescr)))

;;;; other miscellaneous VOPs

(defknown sb-unix::receive-pending-interrupt () (values))
(define-vop (sb-unix::receive-pending-interrupt)
  (:policy :fast-safe)
  (:translate sb-unix::receive-pending-interrupt)
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
(define-vop ()
  (:translate spin-loop-hint)
  (:policy :fast-safe)
  (:generator 0))

(define-vop (sb-c::mark-covered)
 (:info x)
 (:temporary (:sc unsigned-reg) tmp)
 (:generator 4
   ;; Can't compute code-tn-relative index until the boxed header length
   ;; is known. Some vops emit new boxed words via EMIT-CONSTANT.
   (inst store-coverage-mark x tmp)))
