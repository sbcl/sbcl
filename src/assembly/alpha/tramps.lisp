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
    ((:temp fdefn-tn descriptor-reg fdefn-offset)
     (:temp nl0-tn non-descriptor-reg nl0-offset)
     (:temp ocfp-tn non-descriptor-reg ocfp-offset)
     (:temp lra-tn descriptor-reg lra-offset))
  (inst lword simple-fun-widetag) ;; header
  (inst lword (make-fixup 'undefined-tramp-tagged
                          :assembly-routine)) ;; self
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst lword nil-value))

  ;; If we are called with stack arguments (or in a tail-call
  ;; scenario), we end up with an allocated stack frame, but the frame
  ;; link information is uninitialized.  Fix things by allocating and
  ;; initializing our stack frame "properly".

  UNDEFINED-TRAMP
  (inst lda code-tn
        (- fun-pointer-lowtag
                       (ash simple-fun-insts-offset word-shift))
        lip-tn)

  (inst cmpule nargs-tn (fixnumize register-arg-count) nl0-tn)
  (inst beq nl0-tn NO-STACK-ARGS)
  (inst addq cfp-tn (fixnumize register-arg-count) csp-tn)
  (inst br zero-tn FINISH-FRAME-SETUP)

  NO-STACK-ARGS
  (inst addq cfp-tn nargs-tn csp-tn)

  FINISH-FRAME-SETUP
  (storew ocfp-tn cfp-tn ocfp-save-offset)
  (storew lra-tn cfp-tn lra-save-offset)
  (error-call nil 'undefined-fun-error fdefn-tn))

(define-assembly-routine
    (closure-tramp (:return-style :none)
                   (:align n-lowtag-bits))
    ((:temp fdefn-tn descriptor-reg fdefn-offset)
     (:temp lexenv-tn descriptor-reg lexenv-offset))
  (loadw lexenv-tn fdefn-tn fdefn-fun-slot other-pointer-lowtag)
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst lda lip-tn (- (ash simple-fun-insts-offset word-shift)
                      fun-pointer-lowtag)
        code-tn)
  (inst jsr zero-tn lip-tn 1))

(define-assembly-routine
    (xfuncallable-instance-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (funcallable-instance-tramp
                                (+ xfuncallable-instance-tramp
                                   fun-pointer-lowtag))))
    ((:temp code-tn descriptor-reg code-offset)
     (:temp lexenv-tn descriptor-reg lexenv-offset))
  (inst lword simple-fun-widetag)
  (inst lword (make-fixup 'funcallable-instance-tramp
                          :assembly-routine))
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst lword nil-value))

  (loadw lexenv-tn lexenv-tn funcallable-instance-function-slot fun-pointer-lowtag)
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst lda lip-tn (- (ash simple-fun-insts-offset word-shift)
                      fun-pointer-lowtag)
        code-tn)
  (inst jsr zero-tn lip-tn 1))
