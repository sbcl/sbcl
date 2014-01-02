;;;; VOPs and other machine-specific support routines for call-out to C

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(defconstant +number-stack-allocation-granularity+ n-word-bytes)

(define-vop (call-out)
  (:args (function :scs (sap-reg) :target cfunc)
         (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:temporary (:sc any-reg :offset r8-offset
                   :from (:argument 0) :to (:result 0)) cfunc)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:generator 0
    (let ((call-into-c-fixup (gen-label))
          (cur-nfp (current-nfp-tn vop)))
      (assemble (*elsewhere*)
        (emit-label call-into-c-fixup)
        (inst word (make-fixup "call_into_c" :foreign)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (inst ldr temp (@ call-into-c-fixup))
      (move cfunc function)
      (inst blx temp)
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save)))))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:result-types system-area-pointer)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (load-symbol-value result number-stack-pointer)
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount (1- +number-stack-allocation-granularity+))
                             (1- +number-stack-allocation-granularity+))))
        (inst sub result result delta)
        (store-symbol-value result number-stack-pointer)))))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount (1- +number-stack-allocation-granularity+))
                             (1- +number-stack-allocation-granularity+))))
        (load-symbol-value temp number-stack-pointer)
        (inst add temp temp delta)
        (store-symbol-value temp number-stack-pointer)))))
