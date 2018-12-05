;;; Written by William Lott.

(in-package "SB-VM")


(define-vop (print)
  (:args (object :scs (descriptor-reg any-reg) :target nl0))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 0)) nl0)
  (:temporary (:sc any-reg :offset cfunc-offset) cfunc)
  (:temporary (:sc interior-reg :offset lip-offset) lip)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 100
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (move nl0 object)
      (inst lr temp  (make-fixup "call_into_c" :foreign))
      (inst mr lip temp)
      (inst mtctr lip)
      (inst lr cfunc (make-fixup "debug_print" :foreign))
      (inst bctrl)
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save))
      (move result nl0))))
