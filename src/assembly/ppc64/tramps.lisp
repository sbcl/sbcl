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
    ((:temp fdefn-tn descriptor-reg fdefn-offset))
  (inst dword simple-fun-widetag) ;; header
  (inst dword (make-fixup 'undefined-tramp-tagged
                          :assembly-routine)) ;; self
  (dotimes (i (- simple-fun-code-offset 2))
    (inst dword nil-value))

  UNDEFINED-TRAMP
  (inst addi code-tn lip-tn (- fun-pointer-lowtag
                               (ash simple-fun-code-offset word-shift)))

  (inst cmpwi nargs-tn (fixnumize register-arg-count))
  (inst bgt NO-STACK-ARGS)
  (inst addi csp-tn cfp-tn (* register-arg-count n-word-bytes))
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

(define-assembly-routine
    (xfuncallable-instance-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (funcallable-instance-tramp
                                (+ xfuncallable-instance-tramp
                                   fun-pointer-lowtag))))
    ((:temp fdefn-tn descriptor-reg fdefn-offset))
  (inst dword simple-fun-widetag)
  (inst dword (make-fixup 'funcallable-instance-tramp :assembly-routine))
  (dotimes (i (- simple-fun-code-offset 2))
    (inst dword nil-value))

  (loadw lexenv-tn lexenv-tn funcallable-instance-function-slot fun-pointer-lowtag)
  (loadw fdefn-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst addi lip-tn fdefn-tn (- (ash simple-fun-code-offset word-shift)
                                fun-pointer-lowtag))
  (inst mtctr lip-tn)
  (inst bctr))

;;; This is like call_into_c, but it does not affect the pseudo-atomic state,
;;; and it preserves every FPR and GPR, because the trampoline must resemble
;;; an allocation trap as much as possible - affecting no machine state except for
;;; the register into which allocation occurs, and the allocation region.
;;; We use a static area in which to perform the spill/restore just prior to
;;; this code, making it accessible relative to the instruction pointer on entry.
(define-assembly-routine
    (xalloc-tramp (:return-style :raw)
                  (:align n-lowtag-bits)
                  (:export (alloc-tramp (+ xalloc-tramp (* 68 n-word-bytes)))))
    ((:temp r0 unsigned-reg 0)
     (:temp machine-sp unsigned-reg 1)
     (:temp r2 unsigned-reg 2)
     (:temp r3 unsigned-reg 3)
     (:temp r12 unsigned-reg 12)
     (:temp r13 unsigned-reg 13)
     (:temp lip unsigned-reg 31)) ; lisp LIP reg needn't be saved
  ;; Reserve space for spilling 32 GPRs, 32 FPRs, the argument/result,
  ;; the link register, and the caller's link register (already spilled).
  ;; And double-lispword align the start of the routine.
  ;; This works only because alloc-tramp is not re-entrant
  ;; and we're not using threads (yet).
  (inst .skip (* 68 n-word-bytes))
  (flet ((reg (offset sc)
           (make-random-tn :kind :normal :sc (sc-or-lose sc) :offset offset)))
    ;;  -8 = arg
    ;; -16 = caller's LR spill (for calling into me)
    ;; -24 = my LR spill (for calling out to C)
    ;; -32 ... = GPR/FPR spill
    (let ((disp -24))
      (dotimes (i 32)
        (inst stfd (reg i 'double-reg) lip (decf disp 8)))
      ;; don't bother saving LIP register, we'll reload it as a constant
      (dotimes (i 31)
        (inst std (reg i 'unsigned-reg) lip (decf disp 8))))
    ;; Everything else has been spilled now, except CTR which is volatile across calls.
    ;; Normally you store _our_ LR into the _caller's_ frame, but I'm not sure we can
    ;; do that here. The lisp number stack is the C stack and I think we don't create
    ;; a correct minimal C frame each time we consume number stack space.
    ;; Instead save LR into the spill area preceding this chunk of code.
    (inst mflr r0)
    (inst std r0 lip -24)
    ;; Allocate minimal C frame and update back-chain. As noted, I doubt that the
    ;; back-chain is correct if we are in a function that allocated any number
    ;; stack space. (It would be better if we could play nice with the C stack.)
    (inst stdu machine-sp machine-sp -32)
    ;; restore r13 from the value cell of the R13-VALUE static lisp symbol.
    ;; I very much suspect that you're not allowed to save and restore R13,
    ;; but in fact must NEVER touch it at all.
    ;; Anyway this seems to work for now.
    (inst addi r13 null-tn (+ (static-symbol-offset 'r13-value)
                              (- other-pointer-lowtag)
                              (ash symbol-value-slot word-shift)))
    (inst ld r13 r13 0)
    (inst lr r12 (make-fixup "alloc_tramp" :foreign))
    (inst ld r2 r12 8) ; TODO: SEE WHAT HAPPENS IF THIS IS REMOVED
    (inst ld r12 r12 0)
    ;; load the size argument into the first C argument register
    (inst ld r3 lip -8)
    (inst mtctr r12)
    (inst bctrl)
    ;; We're back.
    (inst mtctr r3) ; stash the result in a reg that won't be clobbered
    ;; Reload a pointer to this asm routine
    (inst lr lip (make-fixup 'alloc-tramp :assembly-routine))
    (inst ld r0 lip -24) ; restore the return address
    (inst mtlr r0)
    ;; Restore all registers except LIP
    (let ((disp -24))
      (dotimes (i 32)
        (inst lfd (reg i 'double-reg) lip (decf disp 8)))
      (dotimes (i 31)
        (inst ld (reg i 'unsigned-reg) lip (decf disp 8))))
    ;; Return is automatic
    ))

;;; This is essentially just a translation of call_into_c from
;;; src/runtime/ppc-assem.S which can not easily be a C-style assembly
;;; routine because it has to obey a local linkage convention, not
;;; the global linkage convention. If it were an ordinary C-linkage routine,
;;; sking for dlsym() of it would return the Power ABI function descriptor.
;;; While we can (and do) hack arch_write_linkage_table_entry
;;; to unwrap the descriptor, it would be nice not to have to do that.
#|
#+sb-assembling
(define-assembly-routine (call-into-c (:return-style :none))
    ((:temp reg_zero   signed-reg  0)
     (:temp reg_nsp    signed reg  1)
     (:temp reg_toc    signed reg  2)
     (:temp reg_nl0    signed reg  3)
     (:temp reg_nl1    signed reg  4)
     (:temp reg_nl2    signed reg  5)
     (:temp reg_nl3    signed reg  6)
     (:temp reg_nl4    signed reg  7)
     (:temp reg_nl5    signed reg  8)
     (:temp reg_nl6    signed reg  9)
     (:temp reg_fdefn  signed-reg 10)
     (:temp reg_nargs  signed-reg 11)
     (:temp reg_cfunc  signed reg 12)
     (:temp reg_nfp    signed reg 13)
     (:temp reg_bsp    signed reg 14)
     (:temp reg_cfp    signed reg 15)
     (:temp reg_csp    signed reg 16)
     (:temp reg_alloc  signed reg 17)
     (:temp reg_null   signed-reg 18)
     (:temp reg_code   signed-reg 19)
     (:temp reg_cname  signed-reg 20)
     (:temp reg_lexenv signed-reg 21)
     (:temp reg_ocfp   signed reg 22)
     (:temp reg_lra    signed reg 23)
     (:temp reg_a0     signed-reg 24)
     (:temp reg_a1     signed-reg 25)
     (:temp reg_a2     signed-reg 26)
     (:temp reg_a3     signed-reg 27)
     (:temp reg_l0     signed-reg 28)
     (:temp reg_l1     signed-reg 29)
     (:temp reg_thread signed-reg 30)
     (:temp reg_lip    signed-reg 31))
  ;;; Most of the commentary is omitted except for the changed parts.
  (inst mtctr reg_CFUNC)
  (inst mflr reg_LIP)

  (inst mr reg_OCFP reg_CFP)
  (inst mr reg_CFP reg_CSP)
  (inst addi reg_CSP reg_CSP 32)
  (inst std reg_OCFP reg_CFP 0)
  (inst std reg_CODE reg_CFP 16)

  (inst mr reg_NARGS reg_NL3)

  (inst addi reg_ALLOC reg_ALLOC pseudo-atomic-flag) ; become pseudo-atomic

  ;; Convert return address to an offset and save it on the stack
  (inst sub reg_NFP reg_LIP reg_CODE)
  (inst add reg_NFP reg_NFP OTHER-POINTER-LOWTAG)
  (inst std reg_NFP reg_CFP 8)

  ;; NOT FINISHED
  )
|#
