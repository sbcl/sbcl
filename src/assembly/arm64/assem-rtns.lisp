(in-package "SB-VM")

;;;; Return-multiple with other than one value

#+sb-assembling ;; we don't want a vop for this one.
(define-assembly-routine
    (return-multiple
     (:return-style :none))

    ;; These four are really arguments.
    ((:temp nvals any-reg nargs-offset)
     (:temp vals any-reg nl1-offset)
     (:temp old-fp any-reg nl2-offset)
     (:temp lra interior-reg lr-offset)

     ;; These are just needed to facilitate the transfer
     (:temp count any-reg nl3-offset)
     (:temp src any-reg nl4-offset)
     (:temp dst descriptor-reg r4-offset)
     (:temp temp descriptor-reg r5-offset)

     ;; These are needed so we can get at the register args.
     (:temp r0 descriptor-reg r0-offset)
     (:temp r1 descriptor-reg r1-offset)
     (:temp r2 descriptor-reg r2-offset)
     (:temp r3 descriptor-reg r3-offset))

  ;; Note, because of the way the return-multiple vop is written, we
  ;; can assume that we are never called with nvals == 1 (not that it
  ;; helps overmuch).
  (inst cmp nvals 0)
  (inst b :le default-r0-and-on)
  (inst cmp nvals (fixnumize 2))
  (inst ldp r0 r1 (@ vals))
  (inst b :le default-r2-and-on)
  (inst cmp nvals (fixnumize 3))
  (loadw r2 vals 2)
  (inst b :le default-r3-and-on)
  (inst cmp nvals (fixnumize 4))
  (loadw r3 vals 3)
  (inst b :le DONE)

  ;; Copy the remaining args over the outbound stack frame.
  (inst add src vals (* 4 n-word-bytes))
  (inst add dst cfp-tn (* 4 n-word-bytes))
  (inst sub count nvals (fixnumize 4))

  LOOP
  (inst subs count count (fixnumize 1))
  (inst ldr temp (@ src n-word-bytes :post-index))
  (inst str temp (@ dst n-word-bytes :post-index))
  (inst b :ge LOOP)
  (inst b DONE)

  DEFAULT-R0-AND-ON
  (move r0 null-tn)
  (move r1 null-tn)
  DEFAULT-R2-AND-ON
  (move r2 null-tn)
  DEFAULT-R3-AND-ON
  (move r3 null-tn)

  DONE

  ;; Deallocate the unused stack space.
  (move ocfp-tn cfp-tn)
  (move cfp-tn old-fp)
  (inst add csp-tn ocfp-tn (lsl nvals (- word-shift n-fixnum-tag-bits)))

  ;; Return.
  (lisp-return lra :multiple-values))

;;;; tail-call-variable.
(defun prepare-for-tail-call-variable (nargs args count dest temp r0 r1 r2 r3)
  (assemble ()
    ;; We're in a tail-call scenario, so we use the existing LRA and
    ;; OCFP, both already set up in the stack frame.  We have a set of
    ;; arguments, represented as the address of the first argument
    ;; (ARGS) and the address just beyond the last argument (CSP-TN),
    ;; and need to set up the arg-passing-registers, any stack arguments
    ;; (the fourth and subsequent arguments, if such exist), and the
    ;; total arg count (NARGS).

    ;; Calculate NARGS
    (inst sub nargs csp-tn args)

    ;; Load the argument regs (must do this now, 'cause the blt might
    ;; trash these locations, and we need ARGS to be dead for the blt)
    (inst ldp r0 r1 (@ args))
    (inst ldp r2 r3 (@ args (* n-word-bytes 2)))

    ;; ARGS is now dead, we access the remaining arguments by offset
    ;; from CSP-TN.

    ;; Figure out how many arguments we really need to shift.
    (inst subs count nargs (* register-arg-count n-word-bytes))
    ;; If there aren't any stack args then we're done.
    (inst b :le DONE)

    ;; Find where our shifted arguments need to go.
    (inst add dest cfp-tn nargs)

    (inst neg count count)
    LOOP
    ;; Copy one arg.
    (inst ldr temp (@ csp-tn count))
    (inst str temp (@ dest count))
    (inst adds count count n-word-bytes)
    (inst b :ne LOOP)

    DONE
    (inst asr nargs nargs (- word-shift n-fixnum-tag-bits))))

#+sb-assembling
(define-assembly-routine
    (tail-call-variable
     (:return-style :none))

    ;; These are really args.
    ((:temp args any-reg nl2-offset)
     (:temp lexenv descriptor-reg lexenv-offset)

     ;; We need to compute this
     (:temp nargs any-reg nargs-offset)

     ;; These are needed by the blitting code.
     (:temp dest any-reg nl2-offset) ;; Not live concurrent with ARGS.
     (:temp count any-reg nl3-offset)
     (:temp temp descriptor-reg r9-offset)
     (:temp lip interior-reg lr-offset)

     ;; These are needed so we can get at the register args.
     (:temp r0 descriptor-reg r0-offset)
     (:temp r1 descriptor-reg r1-offset)
     (:temp r2 descriptor-reg r2-offset)
     (:temp r3 descriptor-reg r3-offset))

  (prepare-for-tail-call-variable nargs args count dest temp r0 r1 r2 r3)
  (loadw lip lexenv closure-fun-slot fun-pointer-lowtag)
  (lisp-jump lip))

#+sb-assembling
(define-assembly-routine
    (tail-call-callable-variable
     (:return-style :none))

    ;; These are really args.
    ((:temp args any-reg nl2-offset)
     (:temp lexenv descriptor-reg lexenv-offset)

     ;; We need to compute this
     (:temp nargs any-reg nargs-offset)

     ;; These are needed by the blitting code.
     (:temp dest any-reg nl2-offset) ;; Not live concurrent with ARGS.
     (:temp count any-reg nl3-offset)
     (:temp temp descriptor-reg r9-offset)
     (:temp lip interior-reg lr-offset)

     ;; These are needed so we can get at the register args.
     (:temp r0 descriptor-reg r0-offset)
     (:temp r1 descriptor-reg r1-offset)
     (:temp r2 descriptor-reg r2-offset)
     (:temp r3 descriptor-reg r3-offset))

  (prepare-for-tail-call-variable nargs args count dest temp r0 r1 r2 r3)
  (inst and tmp-tn lexenv lowtag-mask)
  (inst cmp tmp-tn fun-pointer-lowtag)
  (inst b :eq call)
  (inst b (make-fixup 'tail-call-symbol :assembly-routine))
  call
  (loadw lip lexenv closure-fun-slot fun-pointer-lowtag)
  (lisp-jump lip))

#+sb-assembling
(define-assembly-routine (call-symbol
                          (:return-style :none)
                          (:export tail-call-symbol))
    ((:temp fun (any-reg descriptor-reg) lexenv-offset)
     (:temp temp (any-reg descriptor-reg) nl1-offset))
  (inst str lr-tn (@ cfp-tn 8))
  TAIL-CALL-SYMBOL
  (inst and temp fun lowtag-mask)
  (inst cmp temp other-pointer-lowtag)
  (inst b :ne not-callable)

  (inst ldrb temp (@ fun (- other-pointer-lowtag)))
  (inst cmp temp symbol-widetag)
  (inst b :ne not-callable)

  (loadw temp fun symbol-fdefn-slot other-pointer-lowtag)
  (inst cbz temp undefined)
  (move fun temp)
  (loadw lr-tn fun fdefn-raw-addr-slot other-pointer-lowtag)
  (inst add lr-tn lr-tn 4)
  (inst br lr-tn)
  UNDEFINED
  (inst b (make-fixup 'undefined-tramp :assembly-routine))
  NOT-CALLABLE
  (inst cmp fun null-tn) ;; NIL doesn't have SYMBOL-WIDETAG
  (inst b :eq undefined)

  (emit-error-break nil error-trap (error-number-or-lose 'sb-kernel::object-not-callable-error)
                    (list fun)))


;;;; Non-local exit noise.

(define-assembly-routine (throw
                          (:return-style :none))
    ((:arg target descriptor-reg r0-offset)
     (:arg start any-reg r9-offset)
     (:arg count any-reg nargs-offset)
     (:temp catch any-reg r1-offset)
     (:temp tag descriptor-reg r2-offset))
  (declare (ignore start count))

  (load-tl-symbol-value catch *current-catch-block*)

  LOOP

  (let ((error (gen-label)))
    (assemble (:elsewhere)
      (emit-label error)

      ;; Fake up a stack frame so that backtraces come out right.
      (inst mov ocfp-tn cfp-tn)
      (inst mov cfp-tn csp-tn)
      (inst stp ocfp-tn lr-tn (@ csp-tn 16 :post-index))

      (emit-error-break nil error-trap
                        (error-number-or-lose 'unseen-throw-tag-error)
                        (list target)))
    (inst cbz catch error))

  (loadw-pair tmp-tn catch-block-previous-catch-slot tag catch-block-tag-slot catch)
  (inst cmp tag target)
  (inst b :eq DONE)
  (inst mov catch tmp-tn)
  (inst b LOOP)
  DONE
  (move target catch) ;; TARGET coincides with UNWIND's BLOCK argument
  (inst b (make-fixup 'unwind :assembly-routine)))

(define-assembly-routine (unwind
                          (:translate %unwind)
                          (:policy :fast-safe)
                          (:return-style :none))
    ((:arg block (any-reg descriptor-reg) r0-offset)
     (:arg start (any-reg descriptor-reg) r9-offset)
     (:arg count (any-reg descriptor-reg) nargs-offset)
     (:temp ocfp any-reg ocfp-offset)
     (:temp cur-uwp any-reg nl2-offset)
     (:temp lip interior-reg lr-offset)
     (:temp next-uwp any-reg nl3-offset)
     ;; for unbind-to-here
     (:temp where any-reg r1-offset)
     (:temp symbol descriptor-reg r2-offset)
     (:temp value descriptor-reg r3-offset))
  AGAIN
  (let ((error (generate-error-code nil 'invalid-unwind-error)))
    (inst cbz block error))
  (load-tl-symbol-value cur-uwp *current-unwind-protect-block*)
  (loadw ocfp block unwind-block-uwp-slot)
  (inst cmp cur-uwp ocfp)
  (inst b :eq DO-EXIT)

  (inst stp block start (@ csp-tn 32 :post-index))
  (inst adr next-uwp RET) ;; Avoids saving the link register on uwp entry
  (inst stp count next-uwp (@ csp-tn -16))

  ;; Need to perform unbinding before unlinking the UWP so that if
  ;; interrupted here it can still run the clean up form. While the
  ;; cleanup form itself cannot be protected from interrupts (can't
  ;; run it twice) one of the variables being unbound can be
  ;; *interrupts-enabled*
  (loadw where cur-uwp unwind-block-bsp-slot)
  (unbind-to-here where symbol value tmp-tn)

  ;; Set next unwind protect context.

  (loadw next-uwp cur-uwp unwind-block-uwp-slot)
  (store-tl-symbol-value next-uwp *current-unwind-protect-block*)

  (loadw-pair cfp-tn unwind-block-cfp-slot lip unwind-block-entry-pc-slot cur-uwp)
  (loadw next-uwp cur-uwp unwind-block-current-catch-slot)
  (store-tl-symbol-value next-uwp *current-catch-block*)
  (loadw-pair (make-random-tn :kind :normal :sc (sc-or-lose 'any-reg) :offset nfp-offset)
              unwind-block-nfp-slot next-uwp unwind-block-nsp-slot cur-uwp)
  (inst mov-sp nsp-tn next-uwp)
  (inst br lip)
  RET
  (inst ldr count (@ csp-tn -8 :pre-index))
  (inst ldp block start (@ csp-tn -16 :pre-index))
  (inst b AGAIN)

  DO-EXIT
  (loadw where block unwind-block-bsp-slot)
  (unbind-to-here where symbol value tmp-tn)
  (loadw-pair cfp-tn unwind-block-cfp-slot lip unwind-block-entry-pc-slot block)
  (loadw next-uwp block unwind-block-current-catch-slot)
  (store-tl-symbol-value next-uwp *current-catch-block*)
  (loadw-pair (make-random-tn :kind :normal :sc (sc-or-lose 'any-reg) :offset nfp-offset)
              unwind-block-nfp-slot next-uwp unwind-block-nsp-slot block)
  (inst mov-sp nsp-tn next-uwp)

  (inst br lip))

(define-vop ()
  (:translate %continue-unwind)
  (:policy :fast-safe)
  (:generator 0
    (inst ldr lr-tn (@ csp-tn -8 :pre-index))
    (inst ret)))
