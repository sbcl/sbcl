;;;; PPC VM definitions of various system hacking operations

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
    (inst andi. result object lowtag-mask)))

(define-vop (widetag-of)
  (:translate widetag-of)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 1)))
  (:results (result :scs (unsigned-reg) :from (:eval 0)))
  (:result-types positive-fixnum)
  (:generator 6
    ;; Grab the lowtag.
    (inst andi. result object lowtag-mask)
    ;; Check for various pointer types.
    (inst cmpwi result list-pointer-lowtag)
    (inst beq done)
    (inst cmpwi result other-pointer-lowtag)
    (inst beq other-pointer)
    (inst cmpwi result fun-pointer-lowtag)
    (inst beq function-pointer)
    (inst cmpwi result instance-pointer-lowtag)
    (inst beq done)
    ;; Okay, it is an immediate.  If fixnum, we want zero.  Otherwise,
    ;; we want the low 8 bits.
    (inst andi. result object fixnum-tag-mask)
    (inst beq done)
    ;; It wasn't a fixnum, so get the low 8 bits.
    (inst andi. result object widetag-mask)
    (inst b done)

    FUNCTION-POINTER
    (load-type result object (- fun-pointer-lowtag))
    (inst b done)

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

(define-vop (set-fun-subtype)
  (:translate (setf fun-subtype))
  (:policy :fast-safe)
  (:args (type :scs (unsigned-reg) :target result)
         (function :scs (descriptor-reg)))
  (:arg-types positive-fixnum *)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
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
    (inst srwi res res n-widetag-bits)))

(define-vop (get-closure-length)
  (:translate get-closure-length)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 fun-pointer-lowtag)
    (inst srwi res res n-widetag-bits)))

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
    (inst andi. t1 t1 widetag-mask)
    (sc-case data
      (any-reg
       (inst slwi t2 data (- n-widetag-bits n-fixnum-tag-bits))
       (inst or t1 t1 t2))
      (immediate
       (inst ori t1 t1 (ash (tn-value data) n-widetag-bits)))
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
    (inst rlwinm res ptr n-fixnum-tag-bits 1 n-positive-fixnum-bits)))

(define-vop (make-other-immediate-type)
  (:args (val :scs (any-reg descriptor-reg))
         (type :scs (any-reg descriptor-reg immediate)
               :target temp))
  (:results (res :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 2
    (sc-case type
      (immediate
       (inst slwi temp val n-widetag-bits)
       (inst ori res temp (tn-value type)))
      (t
       (inst srawi temp type n-fixnum-tag-bits)
       (inst slwi res val (- n-widetag-bits n-fixnum-tag-bits))
       (inst or res res temp)))))


;;;; Allocation

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
    (inst srwi ndescr ndescr n-widetag-bits)
    (inst slwi ndescr ndescr word-shift)
    (inst subi ndescr ndescr other-pointer-lowtag)
    (inst add sap code ndescr)))

(define-vop (compute-fun)
  (:args (code :scs (descriptor-reg))
         (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (loadw ndescr code 0 other-pointer-lowtag)
    (inst srwi ndescr ndescr n-widetag-bits)
    (inst slwi ndescr ndescr word-shift)
    (inst add ndescr ndescr offset)
    (inst addi ndescr ndescr (- fun-pointer-lowtag other-pointer-lowtag))
    (inst add func code ndescr)))



;;;; Other random VOPs.


(defknown sb!unix::receive-pending-interrupt () (values))
(define-vop (sb!unix::receive-pending-interrupt)
  (:policy :fast-safe)
  (:translate sb!unix::receive-pending-interrupt)
  (:generator 1
    (inst unimp pending-interrupt-trap)))

#!+sb-thread
(defknown current-thread-offset-sap ((unsigned-byte 64))
  system-area-pointer (flushable))

#!+sb-thread
(define-vop (current-thread-offset-sap)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate current-thread-offset-sap)
  (:args (n :scs (unsigned-reg) :target sap))
  (:arg-types unsigned-num)
  (:policy :fast-safe)
  (:generator 2
    (inst slwi n n word-shift)
    (inst lwzx sap thread-base-tn n)))

(define-vop (halt)
  (:generator 1
    (inst unimp halt-trap)))

;;;; Dynamic vop count collection support

(define-vop (count-me)
  (:args (count-vector :scs (descriptor-reg)))
  (:info index)
  (:temporary (:scs (non-descriptor-reg)) count)
  (:generator 1
    (let ((offset
           (- (* (+ index vector-data-offset) n-word-bytes) other-pointer-lowtag)))
      (aver (typep offset '(signed-byte 16)))
      (inst lwz count count-vector offset)
      (inst addi count count 1)
      (inst stw count count-vector offset))))

;;;; Memory barrier support

#!+memory-barrier-vops
(define-vop (%compiler-barrier)
  (:policy :fast-safe)
  (:translate %compiler-barrier)
  (:generator 3))

#!+memory-barrier-vops
(define-vop (%memory-barrier)
  (:policy :fast-safe)
  (:translate %memory-barrier)
  (:generator 3
     (inst sync)))

#!+memory-barrier-vops
(define-vop (%read-barrier)
  (:policy :fast-safe)
  (:translate %read-barrier)
  (:generator 3
     (inst sync)))

#!+memory-barrier-vops
(define-vop (%write-barrier)
  (:policy :fast-safe)
  (:translate %write-barrier)
  (:generator 3
    (inst sync)))

#!+memory-barrier-vops
(define-vop (%data-dependency-barrier)
  (:policy :fast-safe)
  (:translate %data-dependency-barrier)
  (:generator 3))
