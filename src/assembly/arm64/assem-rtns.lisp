(in-package "SB!VM")

;;;; Return-multiple with other than one value

#+sb-assembling ;; we don't want a vop for this one.
(define-assembly-routine
    (return-multiple
     (:return-style :none))

    ;; These four are really arguments.
    ((:temp nvals any-reg nargs-offset)
     (:temp vals any-reg nl1-offset)
     (:temp old-fp any-reg nl2-offset)
     (:temp lra descriptor-reg r6-offset)

     ;; These are just needed to facilitate the transfer
     (:temp count any-reg nl3-offset)
     (:temp src any-reg nl4-offset)
     (:temp dst descriptor-reg r4-offset)
     (:temp temp descriptor-reg r5-offset)

     ;; These are needed so we can get at the register args.
     (:temp r0 descriptor-reg r0-offset)
     (:temp r1 descriptor-reg r1-offset)
     (:temp r2 descriptor-reg r2-offset)
     (:temp r3 descriptor-reg r3-offset)
     (:temp lip interior-reg lr-offset))

  ;; Note, because of the way the return-multiple vop is written, we
  ;; can assume that we are never called with nvals == 1 (not that it
  ;; helps overmuch).
  (inst cmp nvals 0)
  (inst b :le default-r0-and-on)
  (inst cmp nvals (fixnumize 2))
  (loadw r0 vals)
  (loadw r1 vals 1)
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
  (lisp-return lra lip :multiple-values))

;;;; tail-call-variable.

#+sb-assembling ;; no vop for this one either.
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
     (:temp temp descriptor-reg r8-offset)
     (:temp lip interior-reg lr-offset)

     ;; These are needed so we can get at the register args.
     (:temp r0 descriptor-reg r0-offset)
     (:temp r1 descriptor-reg r1-offset)
     (:temp r2 descriptor-reg r2-offset)
     (:temp r3 descriptor-reg r3-offset))

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
  (loadw r0 args 0)
  (loadw r1 args 1)
  (loadw r2 args 2)
  (loadw r3 args 3)

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
  ;; The call frame is all set up, so all that remains is to jump to
  ;; the new function.  We need a boxed register to hold the actual
  ;; function object (in case of closure functions or funcallable
  ;; instances)
  (inst asr nargs nargs (- word-shift n-fixnum-tag-bits))
  (loadw temp lexenv closure-fun-slot fun-pointer-lowtag)
  (lisp-jump temp lip))

;;;; Non-local exit noise.

(define-assembly-routine (throw
                          (:return-style :none))
                         ((:arg target descriptor-reg r0-offset)
                          (:arg start any-reg r8-offset)
                          (:arg count any-reg nargs-offset)
                          (:temp catch any-reg r1-offset)
                          (:temp tag descriptor-reg r2-offset))
  (declare (ignore start count))

  (load-tl-symbol-value catch *current-catch-block*)

  LOOP

  (let ((error (generate-error-code nil 'unseen-throw-tag-error target)))
    (inst cbz catch error))

  (assemble ()
    (loadw tag catch catch-block-tag-slot)
    (inst cmp tag target)
    (inst b :eq DONE)
    (loadw catch catch catch-block-previous-catch-slot)
    (inst b LOOP)
    DONE
    (move target catch)  ;; TARGET coincides with UNWIND's BLOCK argument
    (inst b (make-fixup 'unwind :assembly-routine))))

(define-assembly-routine (unwind
                          (:return-style :none)
                          (:translate %continue-unwind)
                          (:policy :fast-safe))
    ((:arg block (any-reg descriptor-reg) r0-offset)
     (:arg start (any-reg descriptor-reg) r8-offset)
     (:arg count (any-reg descriptor-reg) nargs-offset)
     (:temp ocfp any-reg ocfp-offset)
     (:temp lra descriptor-reg lexenv-offset)
     (:temp cur-uwp any-reg nl2-offset)
     (:temp lip interior-reg lr-offset))
  (declare (ignore start count))
  (let ((error (generate-error-code nil 'invalid-unwind-error)))
    (inst cbz block error))
  (load-tl-symbol-value cur-uwp *current-unwind-protect-block*)
  (loadw ocfp block unwind-block-current-uwp-slot)
  (inst cmp cur-uwp ocfp)
  (inst b :eq EQ)
  (loadw ocfp cur-uwp unwind-block-current-uwp-slot)
  (store-tl-symbol-value ocfp *current-unwind-protect-block*)
  EQ
  (inst csel cur-uwp block cur-uwp :eq)

  (loadw cfp-tn cur-uwp unwind-block-current-cont-slot)
  (loadw code-tn cur-uwp unwind-block-current-code-slot)
  (loadw lra cur-uwp unwind-block-entry-pc-slot)
  (lisp-return lra lip :known))
