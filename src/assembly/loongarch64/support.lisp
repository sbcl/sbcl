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
  (ecase style
    (:raw
     (let ((ra (make-symbol "RA")))
       (values
        `((inst jal ,ra (make-fixup ',name :assembly-routine)))
        `((:temporary (:sc descriptor-reg :from (:eval 0) :to (:eval 1)
                       :offset ra-offset)
                      ,ra)))))
    (:full-call-no-return
     (let ((ra (make-symbol "RA")))
       (values
        `((inst jal ,ra (make-fixup ',name :assembly-routine))
          ,@(when (and vop (assoc :save-p options))
              `((note-this-location ,vop :single-value-return))))
        `((:temporary (:sc descriptor-reg :from (:eval 0) :to (:eval 1)
                        :offset ra-offset)
                      ,ra)))))
    (:none
     (values
      `((inst jal zero-tn (make-fixup ',name :assembly-routine)))
      `()))))


(defun generate-return-sequence (style)
  (ecase style
    (:raw
     `((inst jirl zero-tn ra-tn 0)))
    ((:none :full-call-no-return))))
