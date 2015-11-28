(in-package "SB!VM")

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
     (:temp count any-reg nl2-offset)
     (:temp dst any-reg nl3-offset)
     (:temp temp descriptor-reg l0-offset)
     ;; These are needed so we can get at the register args.
     (:temp a0 descriptor-reg a0-offset)
     (:temp a1 descriptor-reg a1-offset)
     (:temp a2 descriptor-reg a2-offset)
     (:temp a3 descriptor-reg a3-offset)
     (:temp a4 descriptor-reg a4-offset)
     (:temp a5 descriptor-reg a5-offset))
  ;; Note, because of the way the return-multiple vop is written, we can
  ;; assume that we are never called with nvals == 1 and that a0 has already
  ;; been loaded. ;FIX-lav: look at old hppa , replace comb+addi with addib
  (inst comb :<= nvals zero-tn DEFAULT-A0-AND-ON)
  (inst addi (- (fixnumize 2)) nvals count)
  (inst comb :<= count zero-tn DEFAULT-A2-AND-ON)
  (inst ldw (* 1 n-word-bytes) vals a1)
  (inst addib :<= (- (fixnumize 1)) count DEFAULT-A3-AND-ON)
  (inst ldw (* 2 n-word-bytes) vals a2)
  (inst addib :<= (- (fixnumize 1)) count DEFAULT-A4-AND-ON)
  (inst ldw (* 3 n-word-bytes) vals a3)
  (inst addib :<= (- (fixnumize 1)) count DEFAULT-A5-AND-ON)
  (inst ldw (* 4 n-word-bytes) vals a4)
  (inst addib :<= (- (fixnumize 1)) count done)
  (inst ldw (* 5 n-word-bytes) vals a5)
  ;; Copy the remaining args to the top of the stack.
  (inst addi (fixnumize register-arg-count) vals vals)
  (inst addi (fixnumize register-arg-count) cfp-tn dst)
  LOOP
  (inst ldwm n-word-bytes vals temp)
  (inst addib :<> (- (fixnumize 1)) count LOOP)
  (inst stwm temp n-word-bytes dst)
  (inst b DONE :nullify t)

  DEFAULT-A0-AND-ON
  (move null-tn a0)
  (move null-tn a1)
  DEFAULT-A2-AND-ON
  (move null-tn a2)
  DEFAULT-A3-AND-ON
  (move null-tn a3)
  DEFAULT-A4-AND-ON
  (move null-tn a4)
  DEFAULT-A5-AND-ON
  (move null-tn a5)
  DONE
  ;; Clear the stack.
  (move cfp-tn ocfp-tn)
  (move ocfp cfp-tn)
  (inst add ocfp-tn nvals csp-tn)
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
  (inst addi (- (fixnumize register-arg-count)) nargs count)
  (inst comb :<= count zero-tn done)
  (inst addi (fixnumize register-arg-count) args src)
  (inst addi (fixnumize register-arg-count) cfp-tn dst)
  LOOP
  ;; Copy one arg and increase src
  (inst ldwm n-word-bytes src temp)
  (inst addib :<> (- (fixnumize 1)) count LOOP)
  (inst stwm temp n-word-bytes dst)
  DONE
  ;; We are done.  Do the jump.
  (loadw temp lexenv closure-fun-slot fun-pointer-lowtag)
  (lisp-jump temp))


;;;; Non-local exit noise.

(define-assembly-routine
    (unwind
     (:translate %continue-unwind)
     (:return-style :none)
     (:policy :fast-safe))
    ((:arg block (any-reg descriptor-reg) a0-offset)
     (:arg start (any-reg descriptor-reg) ocfp-offset)
     (:arg count (any-reg descriptor-reg) nargs-offset)
     (:temp lra descriptor-reg lra-offset)
     (:temp cur-uwp any-reg nl0-offset)
     (:temp next-uwp any-reg nl1-offset)
     (:temp target-uwp any-reg nl2-offset))
  (declare (ignore start count))


  (let ((error (generate-error-code nil 'invalid-unwind-error)))
    (inst bc := nil block zero-tn error))

  (load-symbol-value cur-uwp *current-unwind-protect-block*)
  (loadw target-uwp block unwind-block-current-uwp-slot)
  (inst bc :<> nil cur-uwp target-uwp DO-UWP)

  (move block cur-uwp)

  DO-EXIT
  (loadw cfp-tn cur-uwp unwind-block-current-cont-slot)
  (loadw code-tn cur-uwp unwind-block-current-code-slot)
  (loadw lra cur-uwp unwind-block-entry-pc-slot)
  (lisp-return lra :frob-code nil)

  DO-UWP
  (loadw next-uwp cur-uwp unwind-block-current-uwp-slot)
  (inst b DO-EXIT)
  (store-symbol-value next-uwp *current-unwind-protect-block*))

(define-assembly-routine
    (throw
     (:return-style :none))
    ((:arg target descriptor-reg a0-offset)
     (:arg start any-reg ocfp-offset)
     (:arg count any-reg nargs-offset)
     (:temp catch any-reg a1-offset)
     (:temp tag descriptor-reg a2-offset)
     (:temp fix descriptor-reg nl0-offset))
  (declare (ignore start count)) ; We just need them in the registers.

  (load-symbol-value catch *current-catch-block*)

  LOOP
  (let ((error (generate-error-code nil 'unseen-throw-tag-error target)))
    (inst bc := nil catch zero-tn error))
  (loadw tag catch catch-block-tag-slot)
  (inst comb := tag target EXIT :nullify t)
  (inst b LOOP)
  (loadw catch catch catch-block-previous-catch-slot)
  EXIT
  (let ((fixup (make-fixup 'unwind :assembly-routine)))
    (inst ldil fixup fix)
    (inst ble fixup lisp-heap-space fix))
  (move catch target t))

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
  (inst addi (- (* simple-fun-code-offset n-word-bytes)
                fun-pointer-lowtag)
        nl0 lip)
  (inst bv lip :nullify t))

#+sb-assembling ;; No VOP for this one
(define-assembly-routine
    (funcallable-instance-tramp-header
     (:return-style :none)
     (:align n-lowtag-bits)
     (:export (funcallable-instance-tramp
               (+ funcallable-instance-tramp-header
                  fun-pointer-lowtag))))
  nil
  (inst word simple-fun-header-widetag) ;;header
  (inst word (make-fixup 'funcallable-instance-tramp :assembly-routine)) ;; self
  (inst word nil-value) ;; next
  (inst word nil-value) ;; name
  (inst word nil-value) ;; arglist
  (inst word nil-value) ;; type
  (inst word nil-value) ;; info
  (loadw lexenv-tn lexenv-tn
         funcallable-instance-function-slot
         fun-pointer-lowtag)
  (loadw code-tn lexenv-tn
         closure-fun-slot
         fun-pointer-lowtag)
  (inst addi (- (* simple-fun-code-offset n-word-bytes)
                fun-pointer-lowtag) code-tn lip-tn)
  (inst bv lip-tn :nullify t))

#!+hpux
(define-assembly-routine
  (return-from-lisp-stub (:return-style :none))
  ((:temp lip interior-reg lip-offset)
   (:temp nl0 descriptor-reg nl0-offset)
   (:temp nl1 descriptor-reg nl1-offset)
   (:temp lra descriptor-reg lra-offset))
  ; before calling into lisp we must save our return address (reg_LRA)
  (store-symbol-value lra *c-lra*)
  ; note the lra we calculate next must "simulate" an fixnum,
  ; because compute-calling-frame will use fixnump on this value.
  ; either use 16 or 20, finetune it...
  (inst addi 19 nl0 lra) ; then setup the new LRA (rest of this routine after branch)
  (inst bv lip :nullify t)
  (inst word return-pc-header-widetag)
  ; ok, we are back from the lisp-call, lets return to c
  ; FIX-lav: steal more stuff from call_into_lisp here, ideally the whole thing
  (inst move ocfp-tn csp-tn) ; dont think we should ever get here
  (inst nop)
  (load-symbol-value nl0 *c-lra*)
  (inst addi 1 nl0 nl0)
  (inst ble 0 c-text-space nl0 :nullify t))
