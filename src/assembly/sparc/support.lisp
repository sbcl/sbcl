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
    ((:raw :none)
     (let ((temp (make-symbol "TEMP"))
           (lip (make-symbol "LIP")))
       (values
        `((inst jali ,lip ,temp (make-fixup ',name :assembly-routine))
          (inst nop))
        `((:temporary (:scs (non-descriptor-reg) :from (:eval 0) :to (:eval 1))
                      ,temp)
          (:temporary (:scs (interior-reg) :from (:eval 0) :to (:eval 1))
                      ,lip)))))
    (:full-call
     (let ((temp (make-symbol "TEMP"))
           (nfp-save (make-symbol "NFP-SAVE"))
           (lra (make-symbol "LRA")))
       (values
        `((let ((lra-label (gen-label))
                (cur-nfp (current-nfp-tn ,vop)))
            (when cur-nfp
              (store-stack-tn ,nfp-save cur-nfp))
            (inst compute-lra-from-code ,lra code-tn lra-label ,temp)
            (note-next-instruction ,vop :call-site)
            (inst ji ,temp (make-fixup ',name :assembly-routine))
            (inst nop)
            (emit-return-pc lra-label)
            (note-this-location ,vop :single-value-return)
            (without-scheduling ()
              (move csp-tn ocfp-tn)
              (inst nop))
            (inst compute-code-from-lra code-tn code-tn
                  lra-label ,temp)
            (when cur-nfp
              (load-stack-tn cur-nfp ,nfp-save))))
        `((:temporary (:scs (non-descriptor-reg) :from (:eval 0) :to (:eval 1))
                      ,temp)
          (:temporary (:sc descriptor-reg :offset lra-offset
                           :from (:eval 0) :to (:eval 1))
                      ,lra)
          (:temporary (:scs (control-stack) :offset nfp-save-offset)
                      ,nfp-save)
          (:save-p :compute-only)))))))

(defun generate-return-sequence (style)
  (ecase style
    (:raw
     `((inst j
             (make-random-tn (sc-or-lose 'interior-reg) lip-offset)
             8)
       (inst nop)))
    (:full-call
     `((lisp-return (make-random-tn (sc-or-lose 'descriptor-reg) lra-offset)
                    :offset 2)))
    (:none)))
