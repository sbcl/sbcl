;;;; linkage information for standard static functions, and random vops

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; LENGTH

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
    (move object ptr)
    (move zero-tn count)

    LOOP

    (inst cmpeq ptr null-tn temp)
    (inst bne temp done)

    (inst and ptr lowtag-mask temp)
    (inst xor temp list-pointer-lowtag temp)
    (inst bne temp not-list)

    (loadw ptr ptr cons-cdr-slot list-pointer-lowtag)
    (inst addq count (fixnumize 1) count)
    (inst br zero-tn loop)

    NOT-LIST
    (cerror-call vop done 'object-not-list-error ptr)

    DONE
    (move count result)))

(define-static-fun length (object) :translate length)
