;;;; temporary printing utilities and similar noise

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(define-vop (print)
  (:args (object :scs (descriptor-reg) :target a0))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:temporary (:sc any-reg :offset cfunc-offset :target result :to (:result 0))
              cfunc)
  (:temporary (:sc descriptor-reg :offset nl0-offset :from (:argument 0)) a0)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (current-nfp-tn vop)))
      (move object a0)
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (inst li (make-fixup "debug_print" :foreign) cfunc)
      (inst li (make-fixup "call_into_c" :foreign) temp)
      (inst jsr lip-tn temp (make-fixup "call_into_c" :foreign))
      (when cur-nfp
        (maybe-load-stack-nfp-tn cur-nfp nfp-save temp))
      (move cfunc result))))
