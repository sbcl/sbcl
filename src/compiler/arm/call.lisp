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

;;;; Frame hackery:

(define-vop (xep-allocate-frame)
  (:info start-lab copy-more-arg-follows)
  (:ignore copy-more-arg-follows)
  (:vop-var vop)
  (:temporary (:scs (any-reg)) temp)
  (:generator 1
    ;; Make sure the function is aligned, and drop a label pointing to this
    ;; function header.
    (emit-alignment n-lowtag-bits)
    (trace-table-entry trace-table-fun-prologue)
    (emit-label start-lab)
    ;; Allocate function header.
    (inst simple-fun-header-word)
    (dotimes (i (1- simple-fun-code-offset))
      (inst word 0))
    ;; Calculate the address of the code component.  This is an
    ;; exercise in excess cleverness.  First, we calculate (from our
    ;; program counter only) the address of START-LAB plus
    ;; OTHER-POINTER-LOWTAG.  The extra two words are to compensate
    ;; for the offset applied by ARM CPUs when reading the program
    ;; counter.
    (inst sub lip-tn pc-tn (+ other-pointer-lowtag
                              (- (* (+ 2 simple-fun-code-offset)
                                    n-word-bytes))))
    ;; Next, we read the function header.
    (loadw temp lip-tn 0 other-pointer-lowtag)
    ;; And finally we use the header value (a count in words), plus
    ;; the fact that the top two bits of SIMPLE-FUN-HEADER-LOWTAG are
    ;; clear (the value is #x2A) to compute the boxed address of the
    ;; code component.
    (inst sub code-tn lip-tn (lsr temp (- 8 word-shift)))
    ;; Build our stack frames.
    (inst add sp-tn fp-tn
          (* n-word-bytes (sb-allocated-size 'control-stack)))
    (let ((nfp-tn (current-nfp-tn vop)))
      (when nfp-tn
        (error "Don't know how to allocate number stack space")))
    (trace-table-entry trace-table-normal)))


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
