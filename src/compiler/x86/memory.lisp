;;;; the x86 definitions of some general purpose memory reference VOPs
;;;; inherited by basic memory reference operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; CELL-REF and CELL-SET are used to define VOPs like CAR, where the
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

;;; X86 special
(define-vop (cell-xadd)
  (:args (object :scs (descriptor-reg) :to :result)
         (value :scs (any-reg) :target result))
  (:results (result :scs (any-reg) :from (:argument 1)))
  (:result-types tagged-num)
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (move result value)
    (inst xadd (make-ea-for-object-slot object offset lowtag)
          value)))

(define-vop (slot-set-conditional)
  (:args (object :scs (descriptor-reg) :to :eval)
         (old-value :scs (descriptor-reg any-reg) :target eax)
         (new-value :scs (descriptor-reg any-reg) :target temp))
  (:temporary (:sc descriptor-reg :offset eax-offset
                   :from (:argument 1) :to :result :target result)  eax)
  (:temporary (:sc descriptor-reg :from (:argument 2) :to :result) temp)
  (:variant-vars base lowtag)
  (:results (result :scs (descriptor-reg)))
  (:info offset)
  (:generator 4
    (move eax old-value)
    (move temp new-value)
    (inst cmpxchg (make-ea-for-object-slot object (+ base offset) lowtag)
          temp)
    (move result eax)))

;;; X86 special
(define-vop (slot-xadd)
  (:args (object :scs (descriptor-reg) :to :result)
         (value :scs (any-reg) :target result))
  (:results (result :scs (any-reg) :from (:argument 1)))
  (:result-types tagged-num)
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
    (move result value)
    (inst xadd (make-ea-for-object-slot object (+ base offset) lowtag)
          value)))
