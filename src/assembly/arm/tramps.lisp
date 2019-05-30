;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB-VM")

(define-assembly-routine
    (xundefined-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (undefined-tramp
                                (+ xundefined-tramp fun-pointer-lowtag))))
    ((:temp lexenv-tn descriptor-reg lexenv-offset))
  HEADER
  (inst word simple-fun-widetag)
  (inst word (make-fixup 'undefined-tramp :assembly-routine))
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst word nil-value))

  (inst adr code-tn header fun-pointer-lowtag)
  (error-call nil 'undefined-fun-error lexenv-tn))

(define-assembly-routine
    (xundefined-alien-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export undefined-alien-tramp
                               (undefined-alien-tramp-tagged
                                (+ xundefined-alien-tramp
                                   fun-pointer-lowtag))))
    ((:temp r8 unsigned-reg r8-offset)
     (:temp r0 descriptor-reg r0-offset)
     (:temp r1 descriptor-reg r1-offset)
     (:temp r2 descriptor-reg r2-offset)
     (:temp lexenv descriptor-reg lexenv-offset))
  HEADER
  (inst word simple-fun-widetag)
  (inst word (make-fixup 'undefined-alien-tramp-tagged
                         :assembly-routine))
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst word nil-value))

  UNDEFINED-ALIEN-TRAMP
  ;; Zero out C arguments, they are not descriptors
  (inst mov r0 0)
  (inst mov r1 0)
  (inst mov r2 0)
  (inst mov lexenv 0)
  (inst adr code-tn header fun-pointer-lowtag)
  (error-call nil 'undefined-alien-fun-error r8))

(define-assembly-routine
    (xclosure-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (closure-tramp
                                (+ xclosure-tramp fun-pointer-lowtag))))
    ((:temp lexenv-tn descriptor-reg lexenv-offset))
  (inst word simple-fun-widetag)
  (inst word (make-fixup 'closure-tramp :assembly-routine))
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst word nil-value))

  (loadw lexenv-tn lexenv-tn fdefn-fun-slot other-pointer-lowtag)
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst add pc-tn code-tn (- (* simple-fun-insts-offset n-word-bytes) fun-pointer-lowtag)))

(define-assembly-routine
    (xfuncallable-instance-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (funcallable-instance-tramp
                                (+ xfuncallable-instance-tramp
                                   fun-pointer-lowtag))))
    ((:temp lexenv-tn descriptor-reg lexenv-offset))
  (inst word simple-fun-widetag)
  (inst word (make-fixup 'funcallable-instance-tramp :assembly-routine))
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst word nil-value))

  (loadw lexenv-tn lexenv-tn funcallable-instance-function-slot fun-pointer-lowtag)
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst add pc-tn code-tn (- (* simple-fun-insts-offset n-word-bytes) fun-pointer-lowtag)))
