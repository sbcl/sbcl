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

(in-package "SB-VM")


(define-vop (print)
  (:args (object :scs (descriptor-reg any-reg) :target ocfp))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:temporary (:sc any-reg :offset ocfp-offset :from (:argument 0)) ocfp)
  (:temporary (:sc any-reg :offset r8-offset) cfunc)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nargs-offset) nargs)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:sc interior-reg) lip)
  (:vop-var vop)
  (:generator 100
    (let ((call-into-c-fixup (gen-label))
          (debug-print-fixup (gen-label))
          (cur-nfp (current-nfp-tn vop)))
      (assemble (:elsewhere)
        (emit-label call-into-c-fixup)
        (inst word (make-fixup "call_into_c" :foreign))
        (emit-label debug-print-fixup)
        (inst word (make-fixup "debug_print" :foreign)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (move ocfp object)
      (inst load-from-label temp lip call-into-c-fixup)
      (inst load-from-label cfunc lip debug-print-fixup)
      (inst blx temp)
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save))
      (move result nargs))))
