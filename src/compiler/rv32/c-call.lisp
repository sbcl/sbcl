;;;; function call for the RV32 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(define-vop (call-out)
  (:args (function :scs (sap-reg)) (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:generator 0))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:result-types system-area-pointer)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:generator 0))

(defun make-call-out-tns (type))
