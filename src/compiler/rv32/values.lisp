;;;; unknown-values VOPs for the RV32 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(define-vop (reset-stack-pointer)
  (:args (ptr :scs (any-reg)))
  (:generator 1))

(define-vop (%%nip-values)
  (:args (last-nipped-ptr :scs (any-reg))
         (last-preserved-ptr :scs (any-reg))
         (moved-ptrs :scs (any-reg) :more t))
  (:results (r-moved-ptrs :scs (any-reg) :more t))
  (:generator 1))

(define-vop (push-values)
  (:args (vals :more t))
  (:results (start) (count))
  (:info nvals)
  (:generator 20))

(define-vop (values-list)
  (:args (arg :scs (descriptor-reg)))
  (:arg-types list)
  (:results (start :scs (any-reg))
            (count :scs (any-reg)))
  (:generator 0))

(define-vop (%more-arg-values)
  (:args (context :scs (descriptor-reg any-reg))
         (skip :scs (any-reg immediate))
         (num :scs (any-reg)))
  (:arg-types * positive-fixnum positive-fixnum)
  (:results (start :scs (any-reg)) (count :scs (any-reg)))
  (:generator 20))
