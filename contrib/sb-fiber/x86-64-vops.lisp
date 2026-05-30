;;;; Inline register/SP/MXCSR swap for SWITCH-FIBER

(in-package "SB-VM")

(sb-c:defknown sb-fiber::%swap-regs
    (system-area-pointer system-area-pointer) (values)
    (sb-c:always-translatable))

(define-vop (fiber-swap-regs)
  (:translate sb-fiber::%swap-regs)
  (:policy :fast-safe)
  (:args (from :scs (sap-reg) :target from-tmp)
         (to   :scs (sap-reg) :target to-tmp))
  (:arg-types system-area-pointer system-area-pointer)
  (:temporary (:sc sap-reg :offset rax-offset
               :from (:argument 0) :to (:result 0))
              from-tmp)
  (:temporary (:sc sap-reg :offset rcx-offset
               :from (:argument 1) :to (:result 0))
              to-tmp)
  (:temporary (:sc unsigned-reg :offset rdx-offset) temp)
  (:generator 10
    (move from-tmp from)
    (move to-tmp to)
    (inst lea temp (rip-relative-ea RESUME))
    (inst push temp)
    (emit-save-fiber-regs from-tmp)
    (emit-restore-fiber-regs to-tmp)
    (inst ret)
    RESUME
    (emit-end-pseudo-atomic)))
