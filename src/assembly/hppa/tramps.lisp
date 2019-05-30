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
    ((:temp lra-tn descriptor-reg lra-offset)
     (:temp temp non-descriptor-reg nl0-offset))
  (inst word simple-fun-widetag) ;; header
  (inst word (make-fixup 'undefined-tramp-tagged
                         :assembly-routine)) ;; self
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst word nil-value))

  UNDEFINED-TRAMP
  ;; Point reg_CODE to the header and tag it as function, since the
  ;; debugger regards a function pointer in reg_CODE which doesn't
  ;; point to a code object as undefined function.  This is (BACKTRACE
  ;; UNDEFINED-FUNCTION BUG-353) in the test suite.
  (inst addi (- fun-pointer-lowtag
                (ash simple-fun-insts-offset word-shift))
        lip-tn code-tn)

  ;; If we are called with stack arguments (or in a tail-call
  ;; scenario), we end up with an allocated stack frame, but the frame
  ;; link information is uninitialized.  Fix things by allocating and
  ;; initializing our stack frame "properly".  This is (BACKTRACE
  ;; UNDEFINED-FUNCTION BUG-346) in the test suite.

  ;; The :NULLIFY T here is technically optional: If we don't nullify
  ;; then we execute the following branch, which also doesn't nullify,
  ;; and its branch-delay slot becomes (due to the COMIB) the
  ;; instruction at NO-STACK-ARGS, and its target is the following
  ;; instruction.  But that's a bug waiting to happen in the unlikely
  ;; event that someone needs to change this logic.
  (inst addi (- (fixnumize register-arg-count)) nargs-tn temp)
  (inst comb :< temp zero-tn NO-STACK-ARGS :nullify t)

  (inst b FINISH-FRAME-SETUP)
  (inst addi (fixnumize register-arg-count) cfp-tn csp-tn)

  NO-STACK-ARGS
  (inst add cfp-tn nargs-tn csp-tn)

  FINISH-FRAME-SETUP
  (storew ocfp-tn cfp-tn ocfp-save-offset)
  (storew lra-tn cfp-tn lra-save-offset)

  ;; Now that the preliminaries are dealt with, actually trap.
  (error-call nil 'undefined-fun-error fdefn-tn))

; we need closure-tramp and funcallable-instance-tramp in
; same space as other lisp-code, because caller is doing
; normal lisp-calls where we doesnt specify space.
; if we doesnt have the lisp-function (code from defun, closure, lambda etc..)
; machine-address, resolve it here and jump to it.
(define-assembly-routine
  (closure-tramp (:return-style :none))
  ((:temp lip interior-reg lip-offset)
   (:temp nl0 descriptor-reg nl0-offset))
  (inst ldw (- (* fdefn-fun-slot n-word-bytes)
               other-pointer-lowtag)
            fdefn-tn lexenv-tn)
  (inst ldw (- (* closure-fun-slot n-word-bytes)
                  fun-pointer-lowtag)
            lexenv-tn nl0)
  (inst addi (- (* simple-fun-insts-offset n-word-bytes)
                fun-pointer-lowtag)
        nl0 lip)
  (inst bv lip :nullify t))

(define-assembly-routine
    (funcallable-instance-tramp-header
     (:return-style :none)
     (:align n-lowtag-bits)
     (:export (funcallable-instance-tramp
               (+ funcallable-instance-tramp-header
                  fun-pointer-lowtag))))
  nil
  (inst word simple-fun-widetag) ;;header
  (inst word (make-fixup 'funcallable-instance-tramp :assembly-routine)) ;; self
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst word nil-value))
  (loadw lexenv-tn lexenv-tn
         funcallable-instance-function-slot
         fun-pointer-lowtag)
  (loadw code-tn lexenv-tn
         closure-fun-slot
         fun-pointer-lowtag)
  (inst addi (- (* simple-fun-insts-offset n-word-bytes)
                fun-pointer-lowtag) code-tn lip-tn)
  (inst bv lip-tn :nullify t))
