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
  (:args (object :scs (descriptor-reg any-reg)))
  (:temporary (:sc unsigned-reg
               :offset eax-offset
               :target result
               :from :eval
               :to (:result 0))
              eax)
  #+darwin
  (:temporary (:sc unsigned-reg
                   :offset esi-offset)
              prev-stack-pointer)
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:generator 100
    #-darwin
    (inst push object)
    #+darwin
    (progn
      ;; the stack should be 16-byte aligned on Darwin
      (inst mov prev-stack-pointer esp-tn)
      (inst sub esp-tn n-word-bytes)
      (align-stack-pointer esp-tn)
      (storew object esp-tn))
    (inst mov eax (make-fixup "debug_print" :foreign))
    (inst call (make-fixup "call_into_c" :foreign))
    #-darwin
    (inst add esp-tn n-word-bytes)
    #+darwin
    (inst mov esp-tn prev-stack-pointer)
    (move result eax)))
