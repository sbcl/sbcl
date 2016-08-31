;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB!VM")

(define-assembly-routine
    (xundefined-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export undefined-tramp
                               (undefined-tramp-tagged
                                (+ xundefined-tramp
                                   fun-pointer-lowtag))))
    ()
  HEADER
  (inst dword simple-fun-header-widetag)
  (inst dword (make-fixup 'undefined-tramp-tagged
                         :assembly-routine))
  (dotimes (i (- simple-fun-code-offset 2))
    (inst dword nil-value))

  UNDEFINED-TRAMP
  (inst adr code-tn header fun-pointer-lowtag)
  (error-call nil 'undefined-fun-error lexenv-tn))

(define-assembly-routine
    (xundefined-alien-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export undefined-alien-tramp
                               (undefined-alien-tramp-tagged
                                (+ xundefined-alien-tramp
                                   fun-pointer-lowtag))))
    ((:temp r8-tn descriptor-reg r8-offset))
  HEADER
  (inst dword simple-fun-header-widetag)
  (inst dword (make-fixup 'undefined-alien-tramp-tagged
                         :assembly-routine))
  (dotimes (i (- simple-fun-code-offset 2))
    (inst dword nil-value))

  UNDEFINED-ALIEN-TRAMP
  (inst adr code-tn header fun-pointer-lowtag)
  (error-call nil 'undefined-alien-fun-error r8-tn))

(define-assembly-routine
    (xclosure-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export closure-tramp
                               (closure-tramp-tagged
                                (+ xclosure-tramp
                                   fun-pointer-lowtag))))
    ()
  (inst dword simple-fun-header-widetag)
  (inst dword (make-fixup 'closure-tramp-tagged
                         :assembly-routine))
  (dotimes (i (- simple-fun-code-offset 2))
    (inst dword nil-value))

  CLOSURE-TRAMP
  (loadw lexenv-tn lexenv-tn fdefn-fun-slot other-pointer-lowtag)
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst add lr-tn code-tn (- (* simple-fun-code-offset n-word-bytes) fun-pointer-lowtag))
  (inst br lr-tn))

(define-assembly-routine
    (xfuncallable-instance-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (funcallable-instance-tramp
                                (+ xfuncallable-instance-tramp
                                   fun-pointer-lowtag))))
    ()
  (inst dword simple-fun-header-widetag)
  (inst dword (make-fixup 'funcallable-instance-tramp-tagged
                         :assembly-routine))
  (dotimes (i (- simple-fun-code-offset 2))
    (inst dword nil-value))

  (loadw lexenv-tn lexenv-tn funcallable-instance-function-slot fun-pointer-lowtag)
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst add lr-tn code-tn (- (* simple-fun-code-offset n-word-bytes) fun-pointer-lowtag))
  (inst br lr-tn))
