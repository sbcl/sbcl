(in-package "SB-VM")

;;;; Return-multiple with other than one value

#+sb-assembling ;; we don't want a vop for this one.
(define-assembly-routine
    (return-multiple
     (:return-style :none))

     ;; These four are really arguments.
    ((:temp nvals any-reg nargs-offset)
     (:temp vals any-reg ocfp-offset)
     (:temp old-fp any-reg nl2-offset)
     (:temp lra descriptor-reg lexenv-offset)

     ;; These are just needed to facilitate the transfer
     (:temp count any-reg nfp-offset)
     (:temp src any-reg code-offset)
     (:temp dst descriptor-reg r8-offset)

     ;; These are needed so we can get at the register args.
     (:temp r0 descriptor-reg r0-offset)
     (:temp r1 descriptor-reg r1-offset)
     (:temp r2 descriptor-reg r2-offset))

  ;; Note, because of the way the return-multiple vop is written, we
  ;; can assume that we are never called with nvals == 1 (not that it
  ;; helps overmuch).

  ;; If there are more return values than there are arg-passing
  ;; registers, then we need to arrange for the excess values to be
  ;; moved.
  (inst cmp nvals (fixnumize 3))
  (inst b :gt MOVE-STACK-VALUES)

  ;; We don't need to copy stack values at this point, so default any
  ;; unsupplied values that should be in arg-passing registers.  First
  ;; piece of deep magic: A computed jump.
  (inst add pc-tn pc-tn nvals)
  ;; Eat a word of padding for the computed jump.
  (inst word 0)

  ;; The computed jump above will land on one of the next four
  ;; instructions, based on the number of values to return.
  (inst mov r0 null-tn)
  (inst mov r1 null-tn)
  (inst mov r2 null-tn)

  ;; We've defaulted any unsupplied parameters, but now we need to
  ;; load the supplied parameters.  Second piece of deep magic: A
  ;; hairier computed jump.
  (inst rsb count nvals (fixnumize 2))
  (inst add pc-tn pc-tn count)

  ;; The computed jump above will land on one of the next four
  ;; instructions, based on the number of values to return, in reverse
  ;; order.
  (inst ldr r2 (@ vals (* 2 n-word-bytes)))

  ;; If we need to copy stack values, we land here so as to load the
  ;; first two register values (the third will be loaded after the
  ;; values are copied, due to register pressure).
  MOVE-STACK-VALUES
  (inst ldr r1 (@ vals n-word-bytes))
  (inst ldr r0 (@ vals))

  ;; The last instruction to set the flags was the CMP to check to see
  ;; if we needed to move the values on the stack.  If we do not need
  ;; to move the values on the stack then we're almost done.
  (inst b :le DONE)

  ;; Copy the remaining args (including the future R2 register value)
  ;; over the outbound stack frame.
  (inst add src vals (* 2 n-word-bytes))
  (inst add dst cfp-tn (* 2 n-word-bytes))
  (inst sub count nvals (fixnumize 2))

  LOOP
  (inst subs count count (fixnumize 1))
  (inst ldr r2 (@ src n-word-bytes :post-index))
  (inst str r2 (@ dst n-word-bytes :post-index))
  (inst b :ge LOOP)

  ;; Load the last remaining register result.
  (inst ldr r2 (@ cfp-tn (* 2 n-word-bytes)))

  DONE

  ;; Deallocate the unused stack space.
  (move ocfp-tn cfp-tn)
  (move cfp-tn old-fp)
  (inst add dst ocfp-tn nvals)
  (store-csp dst)

  ;; Return.
  (lisp-return lra :multiple-values))

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
     (:temp stack-top non-descriptor-reg ocfp-offset)

     ;; These are needed so we can get at the register args.
     (:temp r0 descriptor-reg r0-offset)
     (:temp r1 descriptor-reg r1-offset)
     (:temp r2 descriptor-reg r2-offset))

  ;; We're in a tail-call scenario, so we use the existing LRA and
  ;; OCFP, both already set up in the stack frame.  We have a set of
  ;; arguments, represented as the address of the first argument
  ;; (ARGS) and the address just beyond the last argument (CSP-TN),
  ;; and need to set up the arg-passing-registers (R0, R1, and R2),
  ;; any stack arguments (the fourth and subsequent arguments, if such
  ;; exist), and the total arg count (NARGS).

  ;; Calculate NARGS (as a fixnum)
  (load-csp nargs)
  (inst sub nargs nargs args)

  ;; Load the argument regs (must do this now, 'cause the blt might
  ;; trash these locations, and we need ARGS to be dead for the blt)
  (loadw r0 args 0)
  (loadw r1 args 1)
  (loadw r2 args 2)

  ;; ARGS is now dead, we access the remaining arguments by offset
  ;; from CSP-TN.

  ;; Figure out how many arguments we really need to shift.
  (inst subs count nargs (fixnumize register-arg-count))
  ;; If there aren't any stack args then we're done.
  (inst b :le DONE)

  ;; Find where our shifted arguments ned to go.
  (inst add dest cfp-tn nargs)

  ;; And come from.
  (load-csp stack-top)

  LOOP
  ;; Copy one arg.
  (inst ldr temp (@ stack-top (- count)))
  (inst str temp (@ dest (- count)))
  (inst subs count count n-word-bytes)
  (inst b :ne LOOP)

  DONE
  ;; The call frame is all set up, so all that remains is to jump to
  ;; the new function.  We need a boxed register to hold the actual
  ;; function object (in case of closure functions or funcallable
  ;; instances), and R8 (known as TEMP) and, technically, CODE happen
  ;; to be the only ones available.
  (loadw temp lexenv closure-fun-slot fun-pointer-lowtag)
  (lisp-jump temp))

;;;; Non-local exit noise.

(define-assembly-routine (throw
                          (:return-style :none))
                         ((:arg target descriptor-reg r0-offset)
                          (:arg start any-reg r8-offset)
                          (:arg count any-reg nargs-offset)
                          (:temp catch any-reg r1-offset)
                          (:temp tag descriptor-reg r2-offset))
  (declare (ignore start count))

  (load-symbol-value catch *current-catch-block*)

  LOOP

  (let ((error (generate-error-code nil 'unseen-throw-tag-error target)))
    (inst cmp catch 0)
    (inst b :eq error))

  (loadw tag catch catch-block-tag-slot)
  (inst cmp tag target)
  (loadw catch catch catch-block-previous-catch-slot 0 :ne)
  (inst b :ne LOOP)

  ;; As a dreadful cleverness, make use of the fact that assembly
  ;; routines are emitted in order, with no padding, and that the body
  ;; of UNWIND follows to arrange for the stack to be unwound to our
  ;; chosen destination.
  (move target catch) ;; TARGET coincides with UNWIND's BLOCK argument
  )

(define-assembly-routine (unwind
                          (:return-style :none)
                          (:translate %unwind)
                          (:policy :fast-safe))
                         ((:arg block (any-reg descriptor-reg) r0-offset)
                          (:arg start (any-reg descriptor-reg) r8-offset)
                          (:arg count (any-reg descriptor-reg) nargs-offset)
                          (:temp ocfp non-descriptor-reg ocfp-offset)
                          (:temp lra descriptor-reg lexenv-offset)
                          (:temp cur-uwp any-reg nl2-offset))
  (declare (ignore start count))

  (let ((error (generate-error-code nil 'invalid-unwind-error)))
    (inst cmp block 0)
    (inst b :eq error))

  (load-symbol-value cur-uwp *current-unwind-protect-block*)
  (loadw ocfp block unwind-block-uwp-slot)
  (inst cmp cur-uwp ocfp)

  (loadw ocfp cur-uwp unwind-block-uwp-slot 0 :ne)
  (store-symbol-value ocfp *current-unwind-protect-block* :ne)

  (move cur-uwp block :eq)

  (loadw cfp-tn cur-uwp unwind-block-cfp-slot)
  (loadw code-tn cur-uwp unwind-block-code-slot)
  (loadw lra cur-uwp unwind-block-entry-pc-slot)
  (lisp-return lra :known))
