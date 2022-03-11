;;;; the ARM definitions of some general purpose memory reference VOPs
;;;; inherited by basic memory reference operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Cell-Ref and Cell-Set are used to define VOPs like CAR, where the
;;; offset to be read or written is a property of the VOP used.

(define-vop (cell-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (loadw value object offset lowtag)))

(define-vop (cell-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (storew value object offset lowtag)))

;;;
(define-vop (word-index-cas)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (old-value :scs (any-reg descriptor-reg))
         (new-value :scs (any-reg descriptor-reg)))
  (:arg-types * tagged-num * *)
  (:temporary (:sc interior-reg) lip)
  (:results (result :scs (any-reg descriptor-reg) :from :load))
  (:result-types *)
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 5
    (inst add lip object (lsl index (- word-shift n-fixnum-tag-bits)))
    (inst add-sub lip lip (- (* offset n-word-bytes) lowtag))
    (cond ((member :arm-v8.1 *backend-subfeatures*)
           (move result old-value)
           (inst casal result new-value lip))
          (t
           (assemble ()
             (inst dsb)
             LOOP
             ;; If this were 'ldaxr' instead of 'ldxr' maybe we wouldn't need the 'dsb' ?
             (inst ldxr result lip)
             (inst cmp result old-value)
             (inst b :ne EXIT)
             (inst stlxr tmp-tn new-value lip)
             (inst cbnz (32-bit-reg tmp-tn) LOOP)
             EXIT
             (inst clrex)
             (inst dmb))))))

(define-vop (set-instance-hashed)
  (:args (object :scs (descriptor-reg)))
  ;; oject must be pinned to mark it, so temp reg needn't be the LIP register
  (:temporary (:sc unsigned-reg) baseptr header)
  (:generator 5
    (inst sub baseptr object instance-pointer-lowtag)
    LOOP
    (cond ((member :arm-v8.1 *backend-subfeatures*)
           (inst movz header (ash 1 stable-hash-required-flag))
           (inst ldset header header baseptr))
          (t
           (inst ldaxr header baseptr)
           (inst orr header header (ash 1 stable-hash-required-flag))
           (inst stlxr tmp-tn header baseptr)
           (inst cbnz (32-bit-reg tmp-tn) LOOP)))))
