;;;; MIPS VM definitions of various system hacking operations

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
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    ;; Pick off objects with headers.
    (inst and ndescr object lowtag-mask)
    (inst xor ndescr other-pointer-lowtag)
    (inst beq ndescr other-ptr)
    (inst xor ndescr (logxor other-pointer-lowtag fun-pointer-lowtag))
    (inst beq ndescr function-ptr)

    ;; Pick off fixnums.
    (inst and result object fixnum-tag-mask)
    (inst beq result done)

    ;; Pick off structure and list pointers.
    (inst and result object 1)
    (inst bne result lowtag-only)
    (inst nop)

      ;; Must be an other immediate.
    (inst b done)
    (inst and result object widetag-mask)

    FUNCTION-PTR
    (load-type result object (- fun-pointer-lowtag))
    (inst b done)
    (inst nop)

    LOWTAG-ONLY
    (inst b done)
    (inst and result object lowtag-mask)

    OTHER-PTR
    (load-type result object (- other-pointer-lowtag))

    DONE))

(define-vop ()
  (:translate sb-c::%structure-is-a)
  (:args (x :scs (descriptor-reg)))
  (:arg-types * (:constant t))
  (:policy :fast-safe)
  (:conditional)
  ;; "extra" info in conditional vops follows the 2 super-magical info args
  (:info target not-p test-layout)
  (:temporary (:sc unsigned-reg) this-id test-id)
  (:generator 4
    (let ((label (register-inline-constant :layout-id test-layout))
          (offset (+ (id-bits-offset)
                     (ash (- (layout-depthoid test-layout) 2) 2)
                     (- instance-pointer-lowtag))))
      (inst lw test-id sb-vm::code-tn label)
      (inst lw this-id x offset)
      (inst nop)
      (inst* (if not-p 'bne 'beq) this-id test-id target)
      (inst nop))))

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
    (inst srl res res n-widetag-bits)))

(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (data :scs (any-reg immediate zero)))
  (:arg-types * positive-fixnum)
  (:temporary (:scs (non-descriptor-reg)) t1 t2)
  (:generator 6
    (loadw t1 x 0 other-pointer-lowtag)
    (inst and t1 widetag-mask)
    (sc-case data
      (any-reg
       (inst sll t2 data (- n-widetag-bits n-fixnum-tag-bits))
       (inst or t1 t2))
      (immediate
       (let ((val (ash (tn-value data) n-widetag-bits)))
         (cond ((typep val '(unsigned-byte 16))
                (inst or t1 val))
               (t
                (inst li t2 val)
                (inst or t1 t2)))))
      (zero))
    (storew t1 x 0 other-pointer-lowtag)))


;;;; Allocation

(define-vop (binding-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate binding-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int bsp-tn)))

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
    (loadw ndescr code code-boxed-size-slot other-pointer-lowtag)
    (inst subu ndescr other-pointer-lowtag)
    (inst addu sap code ndescr)))

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
  (:temporary (:scs (interior-reg)) lip)
  (:generator 10
    (loadw res code 0 other-pointer-lowtag)
    (inst sll res res 2) ; shift out the GC bits
    ;; Then shift right to clear the widetag, plus 2 more to the right since we just
    ;; left-shifted to zeroize bits. Then shift left 2 to convert words to bytes.
    ;; The '>>2' and '<<2' cancel out because we don't need to clear all 8 bits
    ;; of the widetag, as CODE-HEADER-WIDETAG already has bits 6 and 7 clear.
    ;; Other places assume the same, though without much commentary.
    ;; It's brittle magic, save for the AVER above which ensures that it works.
    (inst srl res res n-widetag-bits)
    (inst add res offset res)
    (inst subu res other-pointer-lowtag)
    (inst add lip code res)
    (inst lw res lip 0)
    (inst nop)))

(define-vop (compute-fun)
  (:args (code :scs (descriptor-reg))
         (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (loadw ndescr code code-boxed-size-slot other-pointer-lowtag)
    (inst addu ndescr offset)
    (inst addu ndescr (- fun-pointer-lowtag other-pointer-lowtag))
    (inst addu func code ndescr)))


;;;; Other random VOPs.


(defknown sb-unix::receive-pending-interrupt () (values))
(define-vop (sb-unix::receive-pending-interrupt)
  (:policy :fast-safe)
  (:translate sb-unix::receive-pending-interrupt)
  (:generator 1
    (inst break 0 pending-interrupt-trap)))


(define-vop (halt)
  (:generator 1
    (inst break 0 halt-trap)))


;;;; Dynamic vop count collection support

(define-vop (count-me)
  (:args (count-vector :scs (descriptor-reg)))
  (:info index)
  (:temporary (:scs (non-descriptor-reg)) count)
  (:generator 1
    (let ((offset
           (- (* (+ index vector-data-offset) n-word-bytes) other-pointer-lowtag)))
      (inst lw count count-vector offset)
      (inst nop)
      (inst addu count 1)
      (inst sw count count-vector offset))))

;;;; Dummy definition for a spin-loop hint VOP
(define-vop (spin-loop-hint)
  (:translate spin-loop-hint)
  (:policy :fast-safe)
  (:generator 0))

(define-vop (sb-c::mark-covered)
 (:info index)
 (:generator 4
   ;; Can't convert index to a code-relative index until the boxed header length
   ;; has been determined.
   (inst store-coverage-mark index)))
