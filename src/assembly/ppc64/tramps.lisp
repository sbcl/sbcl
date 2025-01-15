;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB-VM")

(define-assembly-routine
    (xundefined-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export undefined-tramp
                               (undefined-tramp-tagged ; can this junk go away?
                                (+ xundefined-tramp
                                   fun-pointer-lowtag))))
    ;; I very much suspect this doesn't have to be wired to fdefn-tn any more.
    ;; (Same goes for funcallable-instance-tramp)
    ((:temp fdefn-tn descriptor-reg fdefn-offset))
  (inst dword simple-fun-widetag) ;; header
  (inst dword (make-fixup 'undefined-tramp-tagged :assembly-routine)) ;; self
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst dword nil-value))

  UNDEFINED-TRAMP
  ;; I see no purpose to computing CODE-TN here. Normally we need it in order
  ;; to load header constants since there is no PC-relative load form,
  ;; but there are no constants, and this routine can't be GC'ed so it's
  ;; not for that either.
  (inst addi code-tn lip-tn (- fun-pointer-lowtag
                               (ash simple-fun-insts-offset word-shift)))

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
    (xfuncallable-instance-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export funcallable-instance-tramp))
    ()
  (inst dword simple-fun-widetag)
  (inst dword (make-fixup 'funcallable-instance-tramp :assembly-routine))
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst dword nil-value))

  FUNCALLABLE-INSTANCE-TRAMP
  (loadw lexenv-tn lexenv-tn funcallable-instance-function-slot fun-pointer-lowtag)
  (loadw lip-tn lexenv-tn closure-fun-slot fun-pointer-lowtag) ; RAW ADDR
  (inst mtctr lip-tn)
  (inst bctr))

;;; This is the super-slow out-of-line allocator in which all allocations
;;; go through C. It acts like call_into_c without affecting the pseudo-atomic
;;; state or any CPU register except the one into which allocation occurs.
;;; We use a static area in which to perform the spill/restore just prior to
;;; this code, making it accessible relative to the instruction pointer on entry.
#+nil
(define-assembly-routine
    (xalloc-tramp (:return-style :raw)
                  (:align n-lowtag-bits)
                  (:export (alloc-tramp (+ xalloc-tramp (* 68 n-word-bytes)))))
    ((:temp r0 unsigned-reg 0)
     (:temp machine-sp unsigned-reg 1)
     (:temp r2 unsigned-reg 2)
     (:temp r3 unsigned-reg 3)
     (:temp r12 unsigned-reg 12)
     (:temp lip unsigned-reg 31))      ; lisp LIP reg needn't be saved
  ;; Reserve space for spilling 32 GPRs, 32 FPRs, the argument/result,
  ;; the link register, and the caller's link register (already spilled).
  ;; And double-lispword align the start of the routine.
  ;; This works only because alloc-tramp is not re-entrant
  ;; and we're not using threads (yet).
  (inst .skip (* 68 n-word-bytes))
  (flet ((reg (offset sc)
           (make-random-tn (sc-or-lose sc) offset)))
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
    ;; Store the LR that we received in the normal save location of 16(sp).
    ;; This works because whenever Lisp adjusts the C stack pointer, it reserves
    ;; the mandatory 4 words.
    (inst mflr r0)
    (inst std r0 machine-sp 16)
    ;; Allocate a new minimal C frame.
    (inst stdu machine-sp machine-sp -32)
    (inst lr r12 (make-fixup "alloc" :foreign))
    ;; We assume that little-endian machines use V2 of the ABI.
    ;; In V2 you can call a function via its global entry point
    ;; without loading r2. Just r12 is enough, because the callee
    ;; can compute r2 from r12 if it needs to.
    ;; big-endian would more accurately be: if this is the v1 ABI
    #+big-endian (progn (inst ld r2 r12 8) ; TODO: SEE WHAT HAPPENS IF WE DON'T DO THIS AT ALL
                        (inst ld r12 r12 0))
    ;; load the size argument into the first C argument register
    (inst ld r3 lip -8)
    (inst mtctr r12)
    (inst bctrl)
    ;; We're back.
    (inst mtctr r3) ; stash the result in a reg that won't be clobbered
    ;; Reload a pointer to this asm routine
    (inst addi lip null-tn (make-fixup 'alloc-tramp :assembly-routine))
    ;; Restore the return address from the caller's frame.
    ;; 'sp' hasn't been restored yet, so add our frame size.
    (inst ld r0 machine-sp (+ 32 16))
    (inst mtlr r0)
    ;; Restore all registers except LIP (register 31).
    ;; LIP is a lisp-volatile register for lisp.
    ;; Also note that we implicitly restore r1 (the C stack pointer)
    ;; so there is no "add 1, 1, 32" to undo the 'stdu'.
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
