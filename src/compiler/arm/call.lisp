;;;; the VM definition of function call for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; XEP hackery:

;;; We don't need to do anything special for regular functions.
;;;
(define-vop (setup-environment)
  (:info label)
  (:ignore label)
  (:generator 0
    ;; Don't bother doing anything.
    ))

;;;; Unknown values return:

;;; Return a single value using the unknown-values convention.
(define-vop (return-single)
  (:args (old-fp :scs (any-reg) :to :eval)
         (return-pc :scs (descriptor-reg))
         (value))
  (:ignore value)
  (:vop-var vop)
  (:generator 6
    (trace-table-entry trace-table-fun-epilogue)
    ;; Clear the number stack.
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (error "Don't know how to clear number stack space in RETURN-SINGLE")))
    ;; Clear the control stack, and restore the frame pointer.
    (move sp-tn fp-tn)
    (move fp-tn old-fp)
    ;; Indicate a single-valued return by clearing all of the status
    ;; flags.
    (inst msr :f 0)
    ;; Out of here.
    (inst add pc-tn return-pc (- 4 other-pointer-lowtag))
    (trace-table-entry trace-table-normal)))
