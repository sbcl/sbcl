;;;; Inline register/SP swap for SWITCH-FIBER

(in-package "SB-VM")

(sb-c:defknown sb-fiber::%swap-regs
    (system-area-pointer system-area-pointer) (values)
    (sb-c:always-translatable))

(define-vop (fiber-swap-regs)
  (:translate sb-fiber::%swap-regs)
  (:policy :fast-safe)
  (:args (from :scs (sap-reg) :target from-tmp)
         (to :scs (sap-reg) :target to-tmp))
  (:arg-types system-area-pointer system-area-pointer)
  (:temporary (:sc sap-reg :offset nl0-offset
               :from (:argument 0) :to (:result 0))
              from-tmp)
  (:temporary (:sc sap-reg :offset nl1-offset
               :from (:argument 1) :to (:result 0))
              to-tmp)
  (:temporary (:sc unsigned-reg :offset nl2-offset) temp)
  (:generator 10
    (move from-tmp from)
    (move to-tmp to)
    (inst adr lr-tn RESUME)
    (emit-save-fiber-regs from-tmp temp)
    (emit-restore-fiber-regs to-tmp temp)
    (inst ret)
    RESUME
    (inst dmb :ishst)
    (inst str wzr-tn
          (@ thread-tn (* n-word-bytes thread-pseudo-atomic-bits-slot)))
    (inst ldr (32-bit-reg temp)
          (@ thread-tn (+ (* n-word-bytes thread-pseudo-atomic-bits-slot) 4)))
    (let ((not-interrupted (gen-label)))
      (inst cbz temp not-interrupted)
      (inst brk pending-interrupt-trap)
      (emit-label not-interrupted))))
