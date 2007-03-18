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

(in-package "SB!VM")

;;; FIXME: should probably become conditional on #!+SB-SHOW
;;; FIXME: should be called DEBUG-PRINT or COLD-PRINT
(define-vop (print)
  (:args (object :scs (descriptor-reg any-reg)))
  (:temporary (:sc unsigned-reg
               :offset rax-offset
               :target result
               :from :eval
               :to (:result 0))
              rax)
  (:temporary (:sc unsigned-reg) call-target)
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:generator 100
    (move rax object)
    (inst push rbp-tn)
    (inst mov rbp-tn rsp-tn)
    (inst push rbp-tn)
    (inst and rsp-tn -16)
    (storew rax rsp-tn)
    (inst lea rax (make-fixup "debug_print" :foreign))
    (inst lea call-target
          (make-ea :qword
                   :disp (make-fixup "call_into_c" :foreign)))
    (inst call call-target)
    (inst mov rsp-tn rbp-tn)
    (inst pop rbp-tn)
    (move result rax)))
