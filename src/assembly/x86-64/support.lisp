;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(defun generate-call-sequence (name style vop options)
  ;; It will be nice if we can eliminate the global assumption that
  ;; a certain register (TEMP-REG-TN - currently R11) is always available.
  (let ((call-tn (or (second (assoc :call-temps options)) 'temp-reg-tn)))
    (ecase style
      (:raw
       (values
        `((note-this-location ,vop :call-site)
          (inst mov ,call-tn (make-fixup ',name :assembly-routine))
          (inst call ,call-tn)
          (note-this-location ,vop :single-value-return))
        nil))
      (:full-call
       (values
        `((note-this-location ,vop :call-site)
          (inst mov ,call-tn (make-fixup ',name :assembly-routine))
          (inst call ,call-tn)
          (note-this-location ,vop :single-value-return))
        '((:save-p :compute-only))))
      (:none
       (values
        `((inst mov ,call-tn (make-fixup ',name :assembly-routine))
          (inst jmp ,call-tn))
        nil)))))

(defun generate-return-sequence (style)
  (ecase style
    (:raw
     `((inst ret)))
    (:full-call
     `((inst clc)
       (inst ret)))
    (:none)))
