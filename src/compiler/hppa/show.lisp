(in-package "SB!VM")

(define-vop (print)
  (:args (object :scs (descriptor-reg any-reg) :target nl0))
  (:results)
  (:save-p t)
  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 0)) nl0)
  (:temporary (:sc any-reg :offset cfunc-offset) cfunc)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:generator 100
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (move object nl0)
      (inst li (make-fixup "debug_print" :foreign) cfunc)
      (let ((fixup (make-fixup "call_into_c" :foreign)))
        (inst ldil fixup temp)
        (inst ble fixup c-text-space temp))
      (inst addi  64 nsp-tn nsp-tn)
      (inst addi -64 nsp-tn nsp-tn)
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save)))))

