;;;; Sparc VM definitions of various system hacking operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; type frobbing VOPs

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


(define-vop (fun-subtype)
  (:translate fun-subtype)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type result function (- fun-pointer-lowtag))))

;;; Is this VOP dead? I can't see anywhere that it is used... -- CSR,
;;; 2002-06-21
(define-vop (set-fun-subtype)
  (:translate (setf fun-subtype))
  (:policy :fast-safe)
  (:args (type :scs (unsigned-reg) :target result)
         (function :scs (descriptor-reg)))
  (:arg-types positive-fixnum *)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    ;; FIXME: I don't understand what this hardcoded 3 is doing
    ;; here. -- CSR, 2002-02-08
    (inst stb type function (- 3 fun-pointer-lowtag))
    (move result type)))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 other-pointer-lowtag)
    (inst srl res res n-widetag-bits)))

(define-vop (get-closure-length)
  (:translate get-closure-length)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 fun-pointer-lowtag)
    (inst srl res res n-widetag-bits)))

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
    (inst and t1 widetag-mask)
    (sc-case data
      (any-reg
       (inst sll t2 data (- n-widetag-bits n-fixnum-tag-bits))
       (inst or t1 t2))
      (immediate
       (inst or t1 (ash (tn-value data) n-widetag-bits)))
      (zero))
    (storew t1 x 0 other-pointer-lowtag)
    (move res x)))


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

(define-vop (make-other-immediate-type)
  (:args (val :scs (any-reg descriptor-reg))
         (type :scs (any-reg descriptor-reg immediate)
               :target temp))
  (:results (res :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 2
    (sc-case type
      (immediate
       (inst sll temp val n-widetag-bits)
       (inst or res temp (tn-value type)))
      (t
       (inst sra temp type n-fixnum-tag-bits)
       (inst sll res val (- n-widetag-bits n-fixnum-tag-bits))
       (inst or res res temp)))))


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
    (loadw ndescr code 0 other-pointer-lowtag)
    (inst srl ndescr n-widetag-bits)
    (inst sll ndescr word-shift)
    (inst sub ndescr other-pointer-lowtag)
    (inst add sap code ndescr)))

(define-vop (compute-fun)
  (:args (code :scs (descriptor-reg))
         (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (loadw ndescr code 0 other-pointer-lowtag)
    (inst srl ndescr n-widetag-bits)
    (inst sll ndescr word-shift)
    (inst add ndescr offset)
    (inst add ndescr (- fun-pointer-lowtag other-pointer-lowtag))
    (inst add func code ndescr)))



;;;; other random VOPs.


(defknown sb!unix::receive-pending-interrupt () (values))
(define-vop (sb!unix::receive-pending-interrupt)
  (:policy :fast-safe)
  (:translate sb!unix::receive-pending-interrupt)
  (:generator 1
    (inst unimp pending-interrupt-trap)))

#!+sb-thread
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
