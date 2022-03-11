;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB-VM")

(define-assembly-routine
    (xundefined-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export undefined-tramp
                               (undefined-tramp-tagged
                                (+ xundefined-tramp
                                   fun-pointer-lowtag))))
    ((:temp nl0 unsigned-reg nl0-offset)
     (:temp lra descriptor-reg lra-offset))
  (progn nl0) ; "use" it. Don't know if it's needed, and don't care to know.
  (inst word simple-fun-widetag)
  (inst word (make-fixup 'undefined-tramp-tagged
                          :assembly-routine))
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst word nil-value))

  UNDEFINED-TRAMP
  ;; Point reg_CODE to the header and tag it as function, since
  ;; the debugger regards a function pointer in reg_CODE which
  ;; doesn't point to a code object as undefined function.
  (inst li code-tn (make-fixup 'undefined-tramp-tagged
                               :assembly-routine))
  (storew ocfp-tn cfp-tn 0)
  (storew lra cfp-tn 1)
  (error-call nil 'undefined-fun-error fdefn-tn))

(define-assembly-routine
    (xclosure-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export closure-tramp
                               (closure-tramp-tagged
                                (+ xclosure-tramp
                                   fun-pointer-lowtag))))
    ()
  (inst word simple-fun-widetag)
  (inst word (make-fixup 'closure-tramp-tagged
                         :assembly-routine))
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst word nil-value))

  CLOSURE-TRAMP
  (loadw lexenv-tn fdefn-tn fdefn-fun-slot other-pointer-lowtag)
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst addu lip-tn code-tn (- (* simple-fun-insts-offset n-word-bytes) fun-pointer-lowtag))
  (inst j lip-tn)
  (inst nop))

(define-assembly-routine
    (xfuncallable-instance-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (funcallable-instance-tramp
                                (+ xfuncallable-instance-tramp
                                   fun-pointer-lowtag))))
    ()
  (inst word simple-fun-widetag)
  (inst word (make-fixup 'funcallable-instance-tramp :assembly-routine))
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst word nil-value))

  (loadw lexenv-tn lexenv-tn funcallable-instance-function-slot fun-pointer-lowtag)
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst addu lip-tn code-tn (- (* simple-fun-insts-offset n-word-bytes) fun-pointer-lowtag))
  (inst j lip-tn)
  (inst nop))
