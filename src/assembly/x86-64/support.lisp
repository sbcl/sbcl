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
     (values
      `((inst mov temp-reg-tn (make-fixup ',name :assembly-routine))
        (inst call temp-reg-tn))
      nil))
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
     `(inst ret))
    (:full-call
     `((inst clc)
       (inst ret)))
    (:none)))
