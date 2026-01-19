;;;; LoongArch VM definitions of various system hacking operations

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

(define-vop (descriptor-hash32)
  (:translate descriptor-hash32)
  (:args (arg :scs (any-reg descriptor-reg)))
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:policy :fast-safe)
  (:generator 1
    (inst s_andi res arg (lognot fixnum-tag-mask))
    ;; now shift left and shift right so that:
    ;;   on 32-bit, the sign bit is clear, yielding 29 bits of precision
    ;;   on 64-bit, the upper 31 bits are clear, yielding 32 bits of precision
    (let ((amount 31))
      (inst slli.d res res amount)
      (inst srli.d res res amount))))

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
    (inst s_andi result object fixnum-tag-mask)
    (inst beq result zero-tn done)
    ;; If it wasn't a fixnum, start with the full widetag.
    (inst s_andi result object widetag-mask)
    ;; Now, we have our result for an immediate type, but we might
    ;; have a pointer object instead, in which case we need to do more
    ;; work.  Check for a pointer type.
    (inst s_andi ndescr object #b10)
    (inst beq ndescr zero-tn done)
    ;; If we have a pointer type, we need to compute a different
    ;; answer.  For lists and instances, we just need the lowtag.  For
    ;; functions and "other", we need to load the widetag from the
    ;; object header.  In both cases, having just the widetag
    ;; available is handy.
    (inst s_andi result object lowtag-mask)

    ;; We now have the correct answer for list-pointer-lowtag and
    ;; instance-pointer-lowtag, but need to pick off the case for the
    ;; other two pointer types.  KLUDGE: FUN-POINTER-LOWTAG and
    ;; OTHER-POINTER-LOWTAG are both in the upper half of the lowtag
    ;; space, while LIST-POINTER-LOWTAG and INSTANCE-POINTER-LOWTAG
    ;; are in the lower half, so we distinguish with a bit test.
    (inst s_andi ndescr object #b1000)
    (inst beq ndescr zero-tn done)

    ;; And, finally, pick out the widetag from the header.
    (inst sub.d lip object result)
    (load-type result lip)
    DONE))


(define-vop ()
  (:translate sb-c::%structure-is-a)
  (:args (x :scs (descriptor-reg)))
  (:arg-types * (:constant t))
  (:policy :fast-safe)
  (:conditional)
  ;; "extra" info in conditional vops follows the 2 super-magical info args
  (:info target not-p test-layout)
  (:temporary (:sc unsigned-reg) this-id temp)
  (:generator 4
    (let ((offset (+ (id-bits-offset)
                     (ash (- (layout-depthoid test-layout) 2) 2)
                     (- instance-pointer-lowtag))))
      (inst ld.w this-id x offset)
      (if (or (typep (layout-id test-layout) '(and (signed-byte 8) (not (eql 0))))
              (not (sb-c::producing-fasl-file)))
          (inst li temp (layout-id test-layout))
          (inst load-layout-id temp test-layout))
      (inst* (if not-p 'bne 'beq) this-id temp target))))

(define-vop (layout-depthoid)
  (:translate layout-depthoid)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:results (values :scs (any-reg)))
  (:result-types fixnum)
  (:generator 1
    (inst ld.w values object
          (- (+ (ash (+ instance-slots-offset
                        (get-dsd-index layout sb-kernel::flags))
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
    (inst srli.d res res n-widetag-bits)))

(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (data :scs (any-reg immediate zero)))
  (:arg-types * positive-fixnum)
  (:temporary (:scs (non-descriptor-reg)) t1 t2)
  (:generator 6
    (load-type t1 x (- other-pointer-lowtag))
    (sc-case data
      (any-reg
       (inst slli.d t2 data (- n-widetag-bits n-fixnum-tag-bits))
       (inst or t1 t1 t2))
      (immediate
       (let ((val (ash (tn-value data) n-widetag-bits)))
         (cond ((typep val 'short-immediate)
                (inst s_ori t1 t1 val))
               (t
                (inst li t2 val)
                (inst or t1 t1 t2)))))
      (zero))
    (storew t1 x 0 other-pointer-lowtag)))

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
    (inst ld.wu ndescr code (- (* n-word-bytes code-boxed-size-slot) other-pointer-lowtag))
    (inst subi ndescr ndescr other-pointer-lowtag)
    (inst add.d sap code ndescr)))

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
    (progn
      (inst ld.wu res code (- 4 other-pointer-lowtag))
      (inst slli.d res res word-shift))
    (inst add.d res offset res)
    (inst add.d lip code res)
    (inst ld.wu res lip (- other-pointer-lowtag))))

(define-vop (compute-fun)
  (:args (code :scs (descriptor-reg))
         (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (inst ld.wu ndescr code (- (* n-word-bytes code-boxed-size-slot) other-pointer-lowtag))
    (inst add.d ndescr ndescr offset)
    (inst subi ndescr ndescr (- other-pointer-lowtag fun-pointer-lowtag))
    (inst add.d func code ndescr)))

;;;; Other random VOPs.
(defknown sb-unix::receive-pending-interrupt () (values))
(define-vop (sb-unix::receive-pending-interrupt)
  (:policy :fast-safe)
  (:translate sb-unix::receive-pending-interrupt)
  (:generator 1
    (inst break pending-interrupt-trap)))

#+sb-thread
(define-vop (current-thread-offset-sap)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate current-thread-offset-sap)
  (:args (n :scs (signed-reg) :target sap))
  (:temporary (:scs (interior-reg)) lip)
  (:arg-types signed-num)
  (:policy :fast-safe)
  (:generator 3
    (inst slli.d n n word-shift)
    (inst add.d lip thread-base-tn n)
    (loadw sap lip)))

#+sb-thread
(define-vop (current-thread-offset-sap/c)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate current-thread-offset-sap)
  (:info n)
  (:arg-types (:constant short-immediate))
  (:policy :fast-safe)
  (:generator 1
    (loadw sap thread-base-tn n)))

(define-vop (halt)
  (:generator 1
    (inst break halt-trap)))

;;;; Dummy definition for a spin-loop hint VOP
(define-vop ()
  (:translate spin-loop-hint)
  (:policy :fast-safe)
  (:generator 0))

;;; Barriers
(define-vop (%compiler-barrier)
  (:policy :fast-safe)
  (:translate %compiler-barrier)
  (:generator 3))

(define-vop (%memory-barrier)
  (:policy :fast-safe)
  (:translate %memory-barrier)
  (:generator 3
    (inst dbar 0)))

(define-vop (%read-barrier)
  (:policy :fast-safe)
  (:translate %read-barrier)
  (:generator 3
    (inst dbar #x14)))

(define-vop (%write-barrier)
  (:policy :fast-safe)
  (:translate %write-barrier)
  (:generator 3
    (inst dbar #x12)))

(define-vop (%data-dependency-barrier)
  (:policy :fast-safe)
  (:translate %data-dependency-barrier)
  (:generator 3))

(define-vop (sb-c::mark-covered)
 (:info index)
 (:generator 4
   ;; Can't convert index to a code-relative index until the boxed header length
   ;; has been determined.
   (inst store-coverage-mark index)))
