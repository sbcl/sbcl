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
  (:temporary (:sc unsigned-reg :offset r13-offset) r13)
  (:temporary (:sc unsigned-reg) call-target)
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:generator 100
    (move rdi object)
    (inst mov r13 rsp-tn)
    (inst and rsp-tn -16)
    (inst mov call-target (make-fixup "debug_print" :foreign))
    (inst call call-target)
    (inst mov rsp-tn r13)
    (move result rax)))
