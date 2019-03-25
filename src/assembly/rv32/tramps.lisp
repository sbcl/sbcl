;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB-VM")

(define-assembly-routine
    (xundefined-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (undefined-tramp
                                (+ xundefined-tramp fun-pointer-lowtag))))
    ((:temp lra descriptor-reg lra-offset))
  (inst machine-word simple-fun-widetag)
  (inst machine-word (make-fixup 'undefined-tramp :assembly-routine))
  (dotimes (i (- simple-fun-code-offset 2))
    (inst machine-word nil-value))

  ;; Point reg_CODE to the header and tag it as function, since
  ;; the debugger regards a function pointer in reg_CODE which
  ;; doesn't point to a code object as undefined function.
  (inst li code-tn (make-fixup 'undefined-tramp :assembly-routine))
  (storew ocfp-tn cfp-tn 0)
  (storew lra cfp-tn 1)
  (error-call nil 'undefined-fun-error lexenv-tn))

(define-assembly-routine
    (xclosure-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (closure-tramp
                                (+ xclosure-tramp fun-pointer-lowtag))))
    ()
  (inst machine-word simple-fun-widetag)
  (inst machine-word (make-fixup 'closure-tramp :assembly-routine))
  (dotimes (i (- simple-fun-code-offset 2))
    (inst machine-word nil-value))

  (loadw lexenv-tn lexenv-tn fdefn-fun-slot other-pointer-lowtag)
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst jalr zero-tn code-tn (- (* simple-fun-code-offset n-word-bytes) fun-pointer-lowtag)))

(define-assembly-routine
    (xfuncallable-instance-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (funcallable-instance-tramp
                                (+ xfuncallable-instance-tramp fun-pointer-lowtag))))
    ()
  (inst machine-word simple-fun-widetag)
  (inst machine-word (make-fixup 'funcallable-instance-tramp :assembly-routine))
  (dotimes (i (- simple-fun-code-offset 2))
    (inst machine-word nil-value))

  (loadw lexenv-tn lexenv-tn funcallable-instance-function-slot fun-pointer-lowtag)
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst jalr zero-tn code-tn (- (* simple-fun-code-offset n-word-bytes) fun-pointer-lowtag)))
