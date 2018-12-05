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

(in-package "SB-VM")

(define-vop (print)
  (:args (object :scs (descriptor-reg any-reg)
                 :target rdi))
  (:temporary (:sc unsigned-reg
               :offset rdi-offset
               :from :eval)
              rdi)
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
    (move rdi object)
    (inst mov call-target rsp-tn) ; save RSP temporarily elsewhere
    (inst and rsp-tn -16) ; align as required for ABI
    (inst push call-target) ; push twice to preserve alignment
    (inst push call-target)
    (inst mov call-target (make-fixup "debug_print" :foreign))
    (inst call call-target)
    (inst pop rsp-tn) ; restore the original RSP
    (move result rax)))
