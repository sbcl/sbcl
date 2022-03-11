;;;; temporary printing utilities and similar noise

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
  (:args (object :scs (descriptor-reg any-reg) :target nl0))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 0)) nl0)
  (:temporary (:sc any-reg :offset cfunc-offset) cfunc)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:sc any-reg :offset nl3-offset) tramp)
  (:vop-var vop)
  (:generator 100
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (move nl0 object)
      ;; (linkage-table-entry-address 0) is "call-into-c" in mips-assem.S
      (inst lw tramp null-tn (- (linkage-table-entry-address 0) nil-value))
      (inst li cfunc (make-fixup "debug_print" :foreign))
      (inst jal tramp)
      (inst subu nsp-tn 16)
      (inst addu nsp-tn 16)
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save))
      (move result nl0))))
