(in-package "SB!VM")


(define-vop (print)
  (:args (object :scs (descriptor-reg) :target arg))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:temporary (:sc non-descriptor-reg :offset cfunc-offset) cfunc)
  (:temporary (:sc non-descriptor-reg :offset nl0-offset :from (:argument 0))
	      arg)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset :to (:result 0))
	      res)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (current-nfp-tn vop)))
      (move object arg)
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      ;; Allocate 64 bytes, the minimum stack size.
      (inst addi 64 nsp-tn nsp-tn)
      (inst li (make-fixup "debug_print" :foreign) cfunc)
      (let ((fixup (make-fixup "call_into_c" :foreign)))
	(inst ldil fixup temp)
	(inst ble fixup c-text-space temp :nullify t)
	(inst nop))
      (inst addi -64 nsp-tn nsp-tn)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save))
      (move res result))))
