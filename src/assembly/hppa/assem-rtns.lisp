(in-package "SB!VM")


;;;; Return-multiple with other than one value

#+sb-assembling ;; we don't want a vop for this one.
(define-assembly-routine
    (return-multiple
     (:return-style :none))

     ;; These four are really arguments.
    ((:temp nvals any-reg nargs-offset)
     (:temp vals any-reg nl0-offset)
     (:temp old-fp any-reg nl1-offset)
     (:temp lra descriptor-reg lra-offset)

     ;; These are just needed to facilitate the transfer
     (:temp count any-reg nl2-offset)
     (:temp src any-reg nl3-offset)
     (:temp dst any-reg nl4-offset)
     (:temp temp descriptor-reg l0-offset)

     ;; These are needed so we can get at the register args.
     (:temp a0 descriptor-reg a0-offset)
     (:temp a1 descriptor-reg a1-offset)
     (:temp a2 descriptor-reg a2-offset)
     (:temp a3 descriptor-reg a3-offset)
     (:temp a4 descriptor-reg a4-offset)
     (:temp a5 descriptor-reg a5-offset))

  (inst movb := nvals count default-a0-and-on :nullify t)
  (loadw a0 vals 0)
  (inst addib := (fixnumize -1) count default-a1-and-on :nullify t)
  (loadw a1 vals 1)
  (inst addib := (fixnumize -1) count default-a2-and-on :nullify t)
  (loadw a2 vals 2)
  (inst addib := (fixnumize -1) count default-a3-and-on :nullify t)
  (loadw a3 vals 3)
  (inst addib := (fixnumize -1) count default-a4-and-on :nullify t)
  (loadw a4 vals 4)
  (inst addib := (fixnumize -1) count default-a5-and-on :nullify t)
  (loadw a5 vals 5)
  (inst addib := (fixnumize -1) count done :nullify t)  

  ;; Copy the remaining args to the top of the stack.
  (inst addi (* 6 n-word-bytes) vals src)
  (inst addi (* 6 n-word-bytes) cfp-tn dst)

  LOOP
  (inst ldwm 4 src temp)
  (inst addib :> (fixnumize -1) count loop)
  (inst stwm temp 4 dst)

  (inst b done :nullify t)

  DEFAULT-A0-AND-ON
  (inst move null-tn a0)
  DEFAULT-A1-AND-ON
  (inst move null-tn a1)
  DEFAULT-A2-AND-ON
  (inst move null-tn a2)
  DEFAULT-A3-AND-ON
  (inst move null-tn a3)
  DEFAULT-A4-AND-ON
  (inst move null-tn a4)
  DEFAULT-A5-AND-ON
  (inst move null-tn a5)

  DONE
  ;; Clear the stack.
  (move cfp-tn ocfp-tn)
  (move old-fp cfp-tn)
  (inst add ocfp-tn nvals csp-tn)
  
  ;; Return.
  (lisp-return lra))



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

     ;; These are needed so we can get at the register args.
     (:temp a0 descriptor-reg a0-offset)
     (:temp a1 descriptor-reg a1-offset)
     (:temp a2 descriptor-reg a2-offset)
     (:temp a3 descriptor-reg a3-offset)
     (:temp a4 descriptor-reg a4-offset)
     (:temp a5 descriptor-reg a5-offset))


  ;; Calculate NARGS (as a fixnum)
  (inst sub csp-tn args nargs)
     
  ;; Load the argument regs (must do this now, 'cause the blt might
  ;; trash these locations)
  (loadw a0 args 0)
  (loadw a1 args 1)
  (loadw a2 args 2)
  (loadw a3 args 3)
  (loadw a4 args 4)
  (loadw a5 args 5)

  ;; Calc SRC, DST, and COUNT
  (inst addi (fixnumize (- register-arg-count)) nargs count)
  (inst comb :<= count zero-tn done :nullify t)
  (inst addi (* n-word-bytes register-arg-count) args src)
  (inst addi (* n-word-bytes register-arg-count) cfp-tn dst)
	
  LOOP
  ;; Copy one arg.
  (inst ldwm 4 src temp)
  (inst addib :> (fixnumize -1) count loop)
  (inst stwm temp 4 dst)
	
  DONE
  ;; We are done.  Do the jump.
  (loadw temp lexenv closure-fun-slot fun-pointer-lowtag)
  (lisp-jump temp))



;;;; Non-local exit noise.

;;; FIXME: Really?
#+sb-assembling
(defparameter *unwind-entry-point* (gen-label))

(define-assembly-routine
    (unwind
     (:translate %continue-unwind)
     (:policy :fast-safe))
    ((:arg block (any-reg descriptor-reg) a0-offset)
     (:arg start (any-reg descriptor-reg) ocfp-offset)
     (:arg count (any-reg descriptor-reg) nargs-offset)
     (:temp lra descriptor-reg lra-offset)
     (:temp cur-uwp any-reg nl0-offset)
     (:temp next-uwp any-reg nl1-offset)
     (:temp target-uwp any-reg nl2-offset))
  (declare (ignore start count))

  (emit-label *unwind-entry-point*)

  (let ((error (generate-error-code nil invalid-unwind-error)))
    (inst bc := nil block zero-tn error))
  
  (load-symbol-value cur-uwp *current-unwind-protect-block*)
  (loadw target-uwp block unwind-block-current-uwp-slot)
  (inst bc :<> nil cur-uwp target-uwp do-uwp)
      
  (move block cur-uwp)

  DO-EXIT
      
  (loadw cfp-tn cur-uwp unwind-block-current-cont-slot)
  (loadw code-tn cur-uwp unwind-block-current-code-slot)
  (loadw lra cur-uwp unwind-block-entry-pc-slot)
  (lisp-return lra :frob-code nil)

  DO-UWP

  (loadw next-uwp cur-uwp unwind-block-current-uwp-slot)
  (inst b do-exit)
  (store-symbol-value next-uwp *current-unwind-protect-block*))


(define-assembly-routine
    throw
    ((:arg target descriptor-reg a0-offset)
     (:arg start any-reg ocfp-offset)
     (:arg count any-reg nargs-offset)
     (:temp catch any-reg a1-offset)
     (:temp tag descriptor-reg a2-offset))
  (declare (ignore start count)) ; We just need them in the registers.

  (load-symbol-value catch *current-catch-block*)
  
  LOOP
  (let ((error (generate-error-code nil unseen-throw-tag-error target)))
    (inst bc := nil catch zero-tn error))
  (loadw tag catch catch-block-tag-slot)
  (inst comb :<> tag target loop :nullify t)
  (loadw catch catch catch-block-previous-catch-slot)
  
  (inst b *unwind-entry-point*)
  (inst move catch target))
