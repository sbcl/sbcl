;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(defun generate-call-sequence (name style vop)
  (ecase style
    (:raw
     ;; Selection of RCX or TEMP-REG-TN for indirect call is a bit of a hack,
     ;; but some folks don't like to assume existence of a universal wired temp,
     ;; so in the interest of emulating what the code did prior to hand-written
     ;; vops around GENERIC-foo routines being removed, we call via a TN that
     ;; the vop explicitly requests. The hacky part is that we only do it when
     ;; contextually we know that RCX should be used, based on the vop name.
     (let ((call-tn (if (member name '(generic-< generic-> generic-=
                                       generic-eql))
                        'rcx
                        'temp-reg-tn)))
       (values
        `((inst mov ,call-tn (make-fixup ',name :assembly-routine))
          (inst call ,call-tn))
        ;; This is a damned-if-you/damned-if-you-don't scenario -
        ;; When DEFINE-ASSEMBLY-ROUTINE declares RCX as a temp, then assembly
        ;; code may use RCX, but the vop that wraps assembly code may not,
        ;; because of an undesired automatically injected :IGNORE.
        ;; Contrariwise, if we return RCX as a call-temp, then the assembly
        ;; code may not use it. And if both places declare it, then
        ;; the register allocator throws a tantrum.
        ;; The empty :IGNORE option tells DEFINE-ASSEMBLY-ROUTINE to shut up
        ;; and allow its vop to use a TN that was allocated on behalf
        ;; of the underlying assembly routine, instead of insisting that
        ;; the vop not be allowed to.
        (if (eq call-tn 'rcx)
            '((:ignore))
            '()))))
    (:full-call
     (values
      `((note-this-location ,vop :call-site)
        (inst mov temp-reg-tn (make-fixup ',name :assembly-routine))
        (inst call temp-reg-tn)
        (note-this-location ,vop :single-value-return))
      '((:save-p :compute-only))))
    (:none
     (values
      `((inst mov temp-reg-tn (make-fixup ',name :assembly-routine))
        (inst jmp temp-reg-tn))
      nil))))

(defun generate-return-sequence (style)
  (ecase style
    (:raw
     `((inst ret)))
    (:full-call
     `((inst clc)
       (inst ret)))
    (:none)))
