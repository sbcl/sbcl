;;;; the machine-specific support routines needed by the file assembler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defun invoke-asm-routine (routine &optional tailp)
  (inst jal (if tailp zero-tn lr-tn) (make-fixup routine :assembly-routine)))

(defun generate-call-sequence (name style vop options)
  (declare (ignore vop options))
  (ecase style
    (:none
     (values
      `((inst j (make-fixup ',name :assembly-routine)))
      `()))
    (:raw
     (values
      `((inst jal lr-tn (make-fixup ',name :assembly-routine)))
      `()))))

(defun generate-return-sequence (style)
  (ecase style
    (:none)
    (:raw
     `((inst jalr zero-tn lr-tn 0)))))

#-sb-xc-host ; CONTEXT-REGISTER is not defined at xc-time
(defun return-machine-address (scp)
  (context-register scp lr-offset))
