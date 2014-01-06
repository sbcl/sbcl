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
    ;; Deal with pointers out-of-line.
    ;; KLUDGE: We're a 32-bit port, so all pointer lowtags have the
    ;; low bit set, but there's no obvious named constant for this.
    ;; On 64-bit ports, all pointer lowtags have the low two bits set,
    ;; so this wouldn't work as easily.
    (inst tst object 1)
    (inst b :ne POINTER-TYPE)

    ;; Okay, it is an immediate.  If fixnum, we want zero.  Otherwise,
    ;; we want the low 8 bits.
    (inst ands result object fixnum-tag-mask)
    ;; If it wasn't a fixnum, get the low 8 bits.
    (inst and :ne result object widetag-mask)
    (inst b DONE)

    ;; Check for various pointer types.
    POINTER-TYPE
    ;; The default answer, for lists and instances, is the lowtag
    ;; (which we need to be able to check anyway).
    (inst and result object lowtag-mask)
    ;; Check for and conditionally load the widetags of the two cases
    ;; where we actually want the widetag.  The widetag space doesn't
    ;; overlap the pointer lowtag space, so the tag loaded in the
    ;; other-pointer case won't be a fun-pointer-lowtag.  This could
    ;; be tightened up further for little-endian systems (and should
    ;; be no worse on big-endian systems) if we inline LOAD-TYPE.
    (inst cmp result other-pointer-lowtag)
    (load-type result object (- other-pointer-lowtag) :eq)
    (inst cmp result fun-pointer-lowtag)
    (load-type result object (- fun-pointer-lowtag) :eq)

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
  (:temporary (:scs (non-descriptor-reg)) t1 t2)
  (:generator 6
    ;; FIXME: Using LDRB here would save us an instruction.
    (loadw t1 x 0 other-pointer-lowtag)
    (inst and t1 t1 widetag-mask)
    (sc-case data
      (any-reg
       (inst orr t1 t2 (lsl data (- n-widetag-bits n-fixnum-tag-bits))))
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

;;;; other miscellaneous VOPs

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
