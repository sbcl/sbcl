;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB-VM")

(define-assembly-routine
    (xundefined-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (undefined-tramp
                                (+ xundefined-tramp fun-pointer-lowtag))))
    ((:temp cname-tn descriptor-reg cname-offset))
  (without-scheduling ()
    (inst word simple-fun-widetag)
    (inst word (make-fixup 'undefined-tramp :assembly-routine))
    (dotimes (i (- simple-fun-insts-offset 2))
      (inst word nil-value))

    (error-call nil 'undefined-fun-error cname-tn)))

(define-assembly-routine
    (xclosure-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (closure-tramp
                                (+ xclosure-tramp fun-pointer-lowtag))))
    ((:temp lexenv-tn descriptor-reg lexenv-offset)
     (:temp cname-tn descriptor-reg cname-offset))
  (without-scheduling ()
    (inst word simple-fun-widetag)
    (inst word (make-fixup 'closure-tramp :assembly-routine))
    (dotimes (i (- simple-fun-insts-offset 2))
      (inst word nil-value))

    (loadw lexenv-tn cname-tn fdefn-fun-slot other-pointer-lowtag)
    (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
    (inst j code-tn (- (ash simple-fun-insts-offset word-shift)
                       fun-pointer-lowtag))
    (inst nop)))

(define-assembly-routine
    (xfuncallable-instance-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (funcallable-instance-tramp
                                (+ xfuncallable-instance-tramp
                                   fun-pointer-lowtag))))
    ((:temp lexenv-tn descriptor-reg lexenv-offset))
  (without-scheduling ()
    (inst word simple-fun-widetag)
    (inst word (make-fixup 'funcallable-instance-tramp :assembly-routine))
    (dotimes (i (- simple-fun-insts-offset 2))
      (inst word nil-value))

    (loadw lexenv-tn lexenv-tn funcallable-instance-function-slot fun-pointer-lowtag)
    (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
    (inst j code-tn (- (ash simple-fun-insts-offset word-shift)
                       fun-pointer-lowtag))
    (inst nop)))
