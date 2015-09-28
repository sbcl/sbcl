;;;; linkage information for standard static functions, and
;;;; miscellaneous VOPs

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(define-vop (length/list)
  (:translate length)
  (:args (object :scs (descriptor-reg) :target ptr))
  (:arg-types list)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) ptr)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (any-reg) :type fixnum :to (:result 0) :target result)
              count)
  (:results (result :scs (any-reg descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 50
    (let ((done (gen-label))
          (loop (gen-label))
          (not-list (gen-label)))
      (move ptr object)
      (inst mov count 0)

      (emit-label loop)

      (inst cmp ptr null-tn)
      (inst b :eq done)

      (test-type ptr not-list t (list-pointer-lowtag) :temp temp)

      (loadw ptr ptr cons-cdr-slot list-pointer-lowtag)
      (inst add count count (fixnumize 1))
      (test-type ptr loop nil (list-pointer-lowtag) :temp temp)

      (emit-label not-list)

      (error-call vop 'object-not-list-error ptr)

      (emit-label done)
      (move result count))))
