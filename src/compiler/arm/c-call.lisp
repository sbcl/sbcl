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
