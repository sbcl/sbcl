;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defun invoke-asm-routine (inst routine vop)
  (declare (ignorable vop))
  (let ((fixup
         (cond ((sb-c::code-immobile-p vop)
                (make-fixup routine :assembly-routine))
               (t
                (ea (make-fixup routine :assembly-routine*))))))
    (ecase inst
      (jmp  (inst jmp fixup))
      (call (inst call fixup)))))

(defun generate-call-sequence (name style vop options)
  (declare (ignore options))
  (ecase style
      (:raw
       (values
        `((note-this-location ,vop :call-site)
          (invoke-asm-routine 'call ',name ,vop)
          (note-this-location ,vop :single-value-return))
        nil))
      ((:full-call :full-call-no-return)
       (values
        `((note-this-location ,vop :call-site)
          (invoke-asm-routine 'call ',name ,vop)
          (note-this-location ,vop :single-value-return))
        '((:save-p :compute-only))))
      (:none
       (values
        `((invoke-asm-routine 'jmp ',name ,vop))
        nil))))

(defun generate-return-sequence (style)
  (ecase style
    (:raw
     `((inst ret)))
    (:full-call
     `((inst clc)
       (inst ret)))
    ((:none :full-call-no-return))))
