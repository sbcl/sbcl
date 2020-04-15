(in-package "SB-VM")


;;;; Return-multiple with other than one value

#+sb-assembling ;; we don't want a vop for this one.
(define-assembly-routine
    (return-multiple
     (:return-style :none))

     ;; These four are really arguments.
    ((:temp nvals any-reg nargs-offset)
     (:temp vals any-reg nl0-offset)
     (:temp ocfp any-reg nl1-offset)
     (:temp lra descriptor-reg lra-offset)

     ;; These are just needed to facilitate the transfer
     (:temp lip interior-reg lip-offset)
     (:temp count any-reg nl2-offset)
     (:temp src any-reg nl3-offset)
     (:temp dst any-reg cfunc-offset)
     (:temp temp descriptor-reg l0-offset)


     ;; These are needed so we can get at the register args.
     (:temp a0 descriptor-reg a0-offset)
     (:temp a1 descriptor-reg a1-offset)
     (:temp a2 descriptor-reg a2-offset)
     (:temp a3 descriptor-reg a3-offset))

  ;; Note, because of the way the return-multiple vop is written, we can
  ;; assume that we are never called with nvals == 1 and that a0 has already
  ;; been loaded.
  (inst cmpwi nvals 0)
  (inst ble default-a0-and-on)
  (inst cmpwi nvals (fixnumize 2))
  (inst lwz a1 vals (* 1 n-word-bytes))
  (inst ble default-a2-and-on)
  (inst cmpwi nvals (fixnumize 3))
  (inst lwz a2 vals (* 2 n-word-bytes))
  (inst ble default-a3-and-on)
  (inst cmpwi nvals (fixnumize 4))
  (inst lwz a3 vals (* 3 n-word-bytes))
  (inst ble done)

  ;; Copy the remaining args to the top of the stack.
  (inst addi src vals (* 4 n-word-bytes))
  (inst addi dst cfp-tn (* 4 n-word-bytes))
  (inst addic. count nvals (- (fixnumize 4)))

  LOOP
  (inst subic. count count (fixnumize 1))
  (inst lwz temp src 0)
  (inst addi src src n-word-bytes)
  (inst stw temp dst 0)
  (inst addi dst dst n-word-bytes)
  (inst bge loop)

  (inst b done)

  DEFAULT-A0-AND-ON
  (inst mr a0 null-tn)
  (inst mr a1 null-tn)
  DEFAULT-A2-AND-ON
  (inst mr a2 null-tn)
  DEFAULT-A3-AND-ON
  (inst mr a3 null-tn)
  DONE

  ;; Clear the stack.
  (move ocfp-tn cfp-tn)
  (move cfp-tn ocfp)
  (inst add csp-tn ocfp-tn nvals)

  ;; Return.
  (lisp-return lra lip))



;;;; tail-call-variable.

#+sb-assembling ;; no vop for this one either.
(define-assembly-routine
    (tail-call-variable
     (:return-style :none))

    ;; These are really args.
    ((:temp args any-reg nl0-offset)
     (:temp lexenv descriptor-reg lexenv-offset)

     ;; We need to compute this
     (:temp nargs any-reg nargs-offset)

     ;; These are needed by the blitting code.
     (:temp src any-reg nl1-offset)
     (:temp dst any-reg nl2-offset)
     (:temp count any-reg nl3-offset)
     (:temp temp descriptor-reg l0-offset)
     (:temp lip interior-reg lip-offset)

     ;; These are needed so we can get at the register args.
     (:temp a0 descriptor-reg a0-offset)
     (:temp a1 descriptor-reg a1-offset)
     (:temp a2 descriptor-reg a2-offset)
     (:temp a3 descriptor-reg a3-offset))


  ;; Calculate NARGS (as a fixnum)
  (inst sub nargs csp-tn args)

  ;; Load the argument regs (must do this now, 'cause the blt might
  ;; trash these locations)
  (inst lwz a0 args (* 0 n-word-bytes))
  (inst lwz a1 args (* 1 n-word-bytes))
  (inst lwz a2 args (* 2 n-word-bytes))
  (inst lwz a3 args (* 3 n-word-bytes))

  ;; Calc SRC, DST, and COUNT
  (inst addic. count nargs (fixnumize (- register-arg-count)))
  (inst addi src args (* n-word-bytes register-arg-count))
  (inst ble done)
  (inst addi dst cfp-tn (* n-word-bytes register-arg-count))

  LOOP
  ;; Copy one arg.
  (inst lwz temp src 0)
  (inst addi src src n-word-bytes)
  (inst stw temp dst 0)
  (inst addic. count count (fixnumize -1))
  (inst addi dst dst n-word-bytes)
  (inst bgt loop)

  DONE
  ;; We are done.  Do the jump.
  (loadw temp lexenv closure-fun-slot fun-pointer-lowtag)
  (lisp-jump temp lip))



;;;; Non-local exit noise.

(define-assembly-routine (unwind
                          (:return-style :none)
                          (:translate %unwind)
                          (:policy :fast-safe))
                         ((:arg block (any-reg descriptor-reg) a0-offset)
                          (:arg start (any-reg descriptor-reg) ocfp-offset)
                          (:arg count (any-reg descriptor-reg) nargs-offset)
                          (:temp lra descriptor-reg lra-offset)
                          (:temp lip interior-reg lip-offset)
                          (:temp cur-uwp any-reg nl0-offset)
                          (:temp next-uwp any-reg nl1-offset)
                          (:temp target-uwp any-reg nl2-offset))
  (declare (ignore start count))

  (let ((error (generate-error-code nil 'invalid-unwind-error)))
    (inst cmpwi block 0)
    (inst beq error))

  (load-tl-symbol-value cur-uwp *current-unwind-protect-block*)
  (loadw target-uwp block unwind-block-uwp-slot)
  (inst cmpw cur-uwp target-uwp)
  (inst bne do-uwp)

  (move cur-uwp block)

  DO-EXIT

  (loadw cfp-tn cur-uwp unwind-block-cfp-slot)
  (loadw code-tn cur-uwp unwind-block-code-slot)
  (loadw lra cur-uwp unwind-block-entry-pc-slot)
  (lisp-return lra lip)

  DO-UWP

  (loadw next-uwp cur-uwp unwind-block-uwp-slot)
  (store-tl-symbol-value next-uwp *current-unwind-protect-block* cfp-tn)
  (inst b do-exit))

(define-assembly-routine (throw
                          (:return-style :none))
                         ((:arg target descriptor-reg a0-offset)
                          (:arg start any-reg ocfp-offset)
                          (:arg count any-reg nargs-offset)
                          (:temp catch any-reg a1-offset)
                          (:temp tag descriptor-reg a2-offset))

  (declare (ignore start count))

  (load-tl-symbol-value catch *current-catch-block*)

  loop

  (let ((error (generate-error-code nil 'unseen-throw-tag-error target)))
    (inst cmpwi catch 0)
    (inst beq error))

  (loadw tag catch catch-block-tag-slot)
  (inst cmpw tag target)
  (inst beq exit)
  (loadw catch catch catch-block-previous-catch-slot)
  (inst b loop)

  exit

  (move target catch)
  ;; reuse catch
  (load-asm-rtn-addr catch 'unwind)
  (inst mtlr catch)
  (inst blr))
