;;;; Sparc VM definitions of various system hacking operations

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
  (:args (object :scs (descriptor-reg) :to (:eval 1)))
  (:results (result :scs (unsigned-reg) :from (:eval 0)))
  (:result-types positive-fixnum)
  (:generator 6
    ;; Grab the lowtag.
    (inst andcc result object lowtag-mask)
    ;; Check for various pointer types.
    (inst cmp result list-pointer-lowtag)
    (inst b :eq done)
    (inst cmp result other-pointer-lowtag)
    (inst b :eq other-pointer)
    (inst cmp result fun-pointer-lowtag)
    (inst b :eq function-pointer)
    (inst cmp result instance-pointer-lowtag)
    (inst b :eq done)
    ;; Okay, it is an immediate.  If fixnum, we want zero.  Otherwise,
    ;; we want the low 8 bits.
    (inst andcc zero-tn object fixnum-tag-mask)
    (inst b :eq done)
    (inst li result 0)
    ;; It wasn't a fixnum, so get the low 8 bits.
    (inst b done)
    (inst and result object widetag-mask)

    FUNCTION-POINTER
    (inst b done)
    (load-type result object (- fun-pointer-lowtag))

    OTHER-POINTER
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
  (:temporary (:sc unsigned-reg) this-id temp)
  (:generator 4
    (let ((offset (+ (id-bits-offset)
                     (ash (- (wrapper-depthoid test-layout) 2) 2)
                     (- instance-pointer-lowtag))))
      (inst ld this-id x offset)
      (if (or (typep (layout-id test-layout) '(and (signed-byte 8) (not (eql 0))))
              (not (sb-c::producing-fasl-file)))
          (inst li temp (layout-id test-layout))
          (inst load-layout-id temp test-layout))
      (inst cmp this-id temp)
      (inst b (if not-p :ne :eq) target)
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
         (cond ((typep val '(signed-byte 13))
                (inst or t1 val))
               (t
                (inst li t2 val)
                (inst or t1 t2)))))
      (zero))
    (storew t1 x 0 other-pointer-lowtag)))


(define-vop (pointer-hash)
  (:translate pointer-hash)
  (:args (ptr :scs (any-reg descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:policy :fast-safe)
  (:generator 1
    ;; FIXME: It would be better if this would mask the lowtag,
    ;; and shift the result into a positive fixnum like on x86.
    (inst sll res ptr 3)
    (inst srl res res 1)))


;;;; allocation

(define-vop (dynamic-space-free-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate dynamic-space-free-pointer)
  (:policy :fast-safe)
  (:generator 1
    (move int alloc-tn)))

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


;;;; code object frobbing.

(define-vop (code-instructions)
  (:translate code-instructions)
  (:policy :fast-safe)
  (:args (code :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 10
    (loadw ndescr code code-boxed-size-slot other-pointer-lowtag)
    (inst sub ndescr other-pointer-lowtag)
    (inst add sap code ndescr)))

(define-vop (compute-fun)
  (:args (code :scs (descriptor-reg))
         (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (loadw ndescr code code-boxed-size-slot other-pointer-lowtag)
    (inst add ndescr offset)
    (inst add ndescr (- fun-pointer-lowtag other-pointer-lowtag))
    (inst add func code ndescr)))



;;;; other random VOPs.


(defknown sb-unix::receive-pending-interrupt () (values))
(define-vop (sb-unix::receive-pending-interrupt)
  (:policy :fast-safe)
  (:translate sb-unix::receive-pending-interrupt)
  (:generator 1
    (inst unimp pending-interrupt-trap)))

#+sb-thread
(error "write a VOP for CURRENT-THREAD-OFFSET-SAP")

(define-vop (halt)
  (:generator 1
    (inst unimp halt-trap)))



;;;; dynamic VOP count collection support

(define-vop (count-me)
  (:args (count-vector :scs (descriptor-reg)))
  (:info index)
  (:temporary (:scs (non-descriptor-reg)) count)
  (:generator 1
    (let ((offset
           (- (* (+ index vector-data-offset) n-word-bytes)
              other-pointer-lowtag)))
      (aver (typep offset '(signed-byte 13)))
      (inst ld count count-vector offset)
      (inst add count 1)
      (inst st count count-vector offset))))

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
