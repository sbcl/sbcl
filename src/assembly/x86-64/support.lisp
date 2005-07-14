;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(!def-vm-support-routine generate-call-sequence (name style vop)
  (ecase style
    (:raw
     (values
      `((inst lea r13-tn
              (make-ea :qword :disp (make-fixup ',name :assembly-routine)))
        (inst call r13-tn))
      nil))
    (:full-call
     (values
      `((note-this-location ,vop :call-site)
        (inst lea r13-tn
              (make-ea :qword :disp (make-fixup ',name :assembly-routine)))
        (inst call r13-tn)
        (note-this-location ,vop :single-value-return)
        (move rsp-tn rbx-tn))
      '((:save-p :compute-only))))
    (:none
     (values
      `((inst lea r13-tn
              (make-ea :qword :disp (make-fixup ',name :assembly-routine)))
        (inst jmp r13-tn))
      nil))))

(!def-vm-support-routine generate-return-sequence (style)
  (ecase style
    (:raw
     `(inst ret))
    (:full-call
     `(
       (inst pop rax-tn)

       (inst add rax-tn 3)
       (inst jmp rax-tn)))
    (:none)))
