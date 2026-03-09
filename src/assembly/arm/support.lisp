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

(defun generate-call-sequence (name style vop options)
  (declare (ignore options))
  (ecase style
    ((:none :raw)
     (let ((fixup-address (make-symbol "FIXUP-ADDRESS")))
       (values
        `((let ((,fixup-address (gen-label)))
            ,@(if (eq style :none)
                  `((inst load-from-label pc-tn lr-tn ,fixup-address))
                  `((inst load-from-label lr-tn lr-tn ,fixup-address)
                    (inst blx lr-tn)))
            (assemble (:elsewhere ,vop)
              (emit-label ,fixup-address)
              (inst word (make-fixup ',name :assembly-routine)))))
        nil)))))

(defun generate-return-sequence (style)
  (ecase style
    (:raw
     `((inst bx lr-tn)))
    (:none)))
