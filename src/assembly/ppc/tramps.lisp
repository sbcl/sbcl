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
    ((:temp fdefn-tn descriptor-reg fdefn-offset))
  (inst word simple-fun-header-widetag) ;; header
  (inst word (make-fixup 'undefined-tramp-tagged
                         :assembly-routine)) ;; self
  (inst word nil-value) ;; next
  (inst word nil-value) ;; name
  (inst word nil-value) ;; arglist
  (inst word nil-value) ;; type
  (inst word nil-value) ;; xref

  UNDEFINED-TRAMP
  (inst addi code-tn lip-tn (- fun-pointer-lowtag
                               (ash simple-fun-code-offset word-shift)))

  (inst cmpwi nargs-tn 16)
  (inst bgt NO-STACK-ARGS)
  (inst addi csp-tn cfp-tn 16)
  (inst b FINISH-FRAME-SETUP)
  NO-STACK-ARGS
  (inst add csp-tn cfp-tn nargs-tn)
  FINISH-FRAME-SETUP
  (storew ocfp-tn cfp-tn 0)
  (storew lra-tn cfp-tn 1)
  (error-call nil 'undefined-fun-error fdefn-tn))

(define-assembly-routine
    (closure-tramp (:return-style :none)
                   (:align n-lowtag-bits))
    ((:temp fdefn-tn descriptor-reg fdefn-offset))
  (loadw lexenv-tn fdefn-tn fdefn-fun-slot other-pointer-lowtag)
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst addi lip-tn code-tn (- (ash simple-fun-code-offset word-shift)
                               fun-pointer-lowtag))
  (inst mtctr lip-tn)
  (inst bctr))
