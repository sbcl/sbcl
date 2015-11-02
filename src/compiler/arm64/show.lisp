;;;; VOPs which are useful for following the progress of the system
;;;; early in boot

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
  (:args (object :scs (descriptor-reg any-reg) :target nl0))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 0)) nl0)
  (:temporary (:sc any-reg :offset r8-offset) cfunc)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 100
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (move nl0 object)
      (load-inline-constant temp '(:fixup "call_into_c" :foreign) lip)
      (load-inline-constant cfunc '(:fixup "debug_print" :foreign) lip)
      (inst blr temp)
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save))
      (move result nl0))))
