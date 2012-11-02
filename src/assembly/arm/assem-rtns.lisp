(in-package "SB!VM")

;;;; Return-multiple with other than one value

#+sb-assembling ;; we don't want a vop for this one.
(define-assembly-routine
    (return-multiple
     (:return-style :none))

     ;; These four are really arguments.
    ((:temp nvals any-reg nargs-offset)
     (:temp vals any-reg ocfp-offset)
     (:temp old-fp any-reg lexenv-offset)
     (:temp lra descriptor-reg lra-offset)

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
  ;; piece of black magic: A computed jump.
  (inst add pc-tn pc-tn nvals)
  ;; Eat a word of padding for the computed jump.
  (inst word 0)

  ;; The computed jump above will land on one of the next four
  ;; instructions, based on the number of values to return.
  (inst mov r0 null-tn)
  (inst mov r1 null-tn)
  (inst mov r2 null-tn)

  ;; We've defaulted any unsupplied parameters, but now we need to
  ;; load the supplied parameters.  Second piece of black magic: A
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
  (inst add dst fp-tn (* 2 n-word-bytes))
  (inst sub count nvals (fixnumize 2))

  LOOP
  (inst subs count count (fixnumize 1))
  (inst ldr r2 (@ src n-word-bytes :post-index))
  (inst str r2 (@ dst n-word-bytes :post-index))
  (inst b :ge LOOP)

  ;; Load the last remaining register result.
  (inst ldr r2 (@ fp-tn (* 2 n-word-bytes)))

  DONE

  ;; Deallocate the unused stack space.
  (move ocfp-tn fp-tn)
  (move fp-tn old-fp)
  (inst add sp-tn ocfp-tn nvals)

  ;; Return.
  (lisp-return lra nil))

;;;; Non-local exit noise.

(define-assembly-routine (throw
                          (:return-style :none))
                         ((:arg target descriptor-reg r0-offset)
                          (:arg start any-reg r8-offset)
                          (:arg count any-reg nargs-offset)
                          (:temp ocfp non-descriptor-reg ocfp-offset)
                          (:temp catch any-reg r1-offset)
                          (:temp tag descriptor-reg r2-offset))
  (declare (ignore start count))

  (load-symbol-value catch *current-catch-block*)

  LOOP

  (let ((error (generate-error-code nil ocfp 'unseen-throw-tag-error target)))
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
                          (:translate %continue-unwind)
                          (:policy :fast-safe))
                         ((:arg block (any-reg descriptor-reg) r0-offset)
                          (:arg start (any-reg descriptor-reg) r8-offset)
                          (:arg count (any-reg descriptor-reg) nargs-offset)
                          (:temp ocfp non-descriptor-reg ocfp-offset)
                          (:temp lra descriptor-reg lra-offset)
                          (:temp cur-uwp any-reg nfp-offset))
  (declare (ignore start count))

  (let ((error (generate-error-code nil ocfp 'invalid-unwind-error)))
    (inst cmp block 0)
    (inst b :eq error))

  (load-symbol-value cur-uwp *current-unwind-protect-block*)
  (loadw ocfp block unwind-block-current-uwp-slot)
  (inst cmp cur-uwp ocfp)

  (loadw ocfp cur-uwp unwind-block-current-uwp-slot 0 :ne)
  (store-symbol-value ocfp *current-unwind-protect-block* :ne)

  (move cur-uwp block :eq)

  (loadw fp-tn cur-uwp unwind-block-current-cont-slot)
  (loadw code-tn cur-uwp unwind-block-current-code-slot)
  (loadw lra cur-uwp unwind-block-entry-pc-slot)
  ;; Shouldn't use LISP-RETURN here because we don't need to signal
  ;; single / multiple values.
  (inst sub pc-tn lra (- other-pointer-lowtag 4)))
