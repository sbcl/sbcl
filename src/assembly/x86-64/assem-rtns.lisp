;;;; the machine specific support routines needed by the file assembler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; RETURN-MULTIPLE

;;; For RETURN-MULTIPLE, we have to move the results from the end of
;;; the frame for the function that is returning to the end of the
;;; frame for the function being returned to.

#+sb-assembling ;; We don't want a vop for this one.
(define-assembly-routine
    (return-multiple (:return-style :none))
    (;; These are really arguments.
     (:temp rcx unsigned-reg rcx-offset)
     (:temp rsi unsigned-reg rsi-offset)

     ;; These we need as temporaries.
     (:temp rax unsigned-reg rax-offset)
     (:temp rbx unsigned-reg rbx-offset)
     (:temp rdx unsigned-reg rdx-offset)
     (:temp rdi unsigned-reg rdi-offset)
     (:temp temp unsigned-reg r8-offset)
     (:temp loop-index unsigned-reg r9-offset))

  ;; Pick off the cases where everything fits in register args.
  (inst cmp :dword rcx (fixnumize 1))
  (inst jmp :e ONE-VALUE)
  (inst jmp :b ZERO-VALUES)
  (inst cmp :dword rcx (fixnumize 3))
  (inst jmp :b TWO-VALUES)
  (inst jmp :e THREE-VALUES)

  ;; As per the calling convention RBX is expected to point at the SP
  ;; before the stack frame.
  (inst lea rbx (ea (* sp->fp-offset n-word-bytes) rbp-tn))

  ;; Save the count, the return address and restore the frame pointer,
  ;; because the loop is going to destroy them.
  (inst mov rdx rcx)
  (inst mov rax (ea (frame-byte-offset return-pc-save-offset) rbp-tn))
  (inst mov rbp-tn (ea (frame-byte-offset ocfp-save-offset) rbp-tn))
  ;; Blit the values down the stack. Note: there might be overlap, so
  ;; we have to be careful not to clobber values before we've read
  ;; them. Because the stack builds down, we are copying to a larger
  ;; address. Therefore, we need to iterate from larger addresses to
  ;; smaller addresses.
  (zeroize loop-index)
  LOOP
  (inst sub loop-index n-word-bytes)
  (inst mov temp (ea rsi loop-index))
  (inst mov (ea rbx loop-index) temp)

  (inst sub rdx (fixnumize 1))
  (inst jmp :nz LOOP)

  ;; Set the stack top to the last result.
  (inst lea rsp-tn (ea rbx loop-index))

  ;; Load the register args.
  (loadw rdx rbx -1)
  (loadw rdi rbx -2)
  (loadw rsi rbx -3)

  ;; And back we go.
  (inst stc)
  (inst push rax)
  (inst ret)

  ;; Handle the register arg cases.
  ZERO-VALUES
  (inst lea rbx (ea (* sp->fp-offset n-word-bytes) rbp-tn))
  (inst mov rdx nil-value)
  (inst mov rdi rdx)
  (inst mov rsi rdx)
  (inst stc)
  (inst leave)
  (inst ret)

  ;; Note: we can get this, because the return-multiple vop doesn't
  ;; check for this case when size > speed.
  ONE-VALUE
  (loadw rdx rsi -1)
  (inst clc)
  (inst leave)
  (inst ret)

  TWO-VALUES
  (inst lea rbx (ea (* sp->fp-offset n-word-bytes) rbp-tn))
  (loadw rdx rsi -1)
  (loadw rdi rsi -2)
  (inst mov rsi nil-value)
  (inst stc)
  (inst leave)
  (inst ret)

  THREE-VALUES
  (inst lea rbx (ea (* sp->fp-offset n-word-bytes) rbp-tn))
  (loadw rdx rsi -1)
  (loadw rdi rsi -2)
  (loadw rsi rsi -3)
  (inst stc)
  (inst leave)
  (inst ret))

;;;; TAIL-CALL-VARIABLE

;;; For tail-call-variable, we have to copy the arguments from the end
;;; of our stack frame (where args are produced) to the start of our
;;; stack frame (where args are expected).
;;;
;;; We take the function to call in RAX and a pointer to the arguments in
;;; RSI. EBP stays the same over the jump, and the old frame pointer is
;;; still saved in the first stack slot. The return-pc is saved in
;;; the second stack slot, so we have to push it to make it look like
;;; we actually called. We also have to compute RCX from the difference
;;; between RSI and the stack top.
#-sb-assembling ; avoid "Redefinition" warning (this file is processed twice)
(defun prepare-for-tail-call-variable (fun temp nargs rdx rdi rsi
                                        r8 r9 r10
                                        &optional jump-to-the-end)
  (assemble ()
    ;; Calculate NARGS (as a fixnum)
    (move nargs rsi)
    (inst sub nargs rsp-tn)
    #-#.(cl:if (cl:= sb-vm:word-shift sb-vm:n-fixnum-tag-bits) '(and) '(or))
    (inst shr nargs (- word-shift n-fixnum-tag-bits))

    ;; Check for all the args fitting the registers.
    (inst cmp nargs (fixnumize register-arg-count))
    (inst jmp :le REGISTER-ARGS)

    (inst mov r8 rsi)

    ;; Register args
    (loadw rdx rsi -1)
    (loadw rdi rsi -2)
    (loadw rsi rsi -3)

    ;; Do the blit. Because we are coping from smaller addresses to
    ;; larger addresses, we have to start at the largest pair and work
    ;; our way down.
    (zeroize temp)
    (inst lea r9 (ea (- (fixnumize register-arg-count)) nargs))
    LOOP
    (inst sub temp n-word-bytes)
    (inst mov r10 (ea (- (* register-arg-count n-word-bytes)) r8 temp))
    (inst mov (ea (frame-byte-offset (1- register-arg-count)) rbp-tn temp) r10)

    (inst sub r9 (fixnumize 1))
    (inst jmp :nz LOOP)

    ;; Blow off the stack above the arguments.
    (inst lea rsp-tn (ea (frame-byte-offset (1- register-arg-count)) rbp-tn temp))

    ;; Push the return-pc so it looks like we just called.
    (pushw rbp-tn (frame-word-offset return-pc-save-offset))

    ;; And jump into the function.
    (if jump-to-the-end
        (inst jmp end)
        (inst jmp (ea (- (* closure-fun-slot n-word-bytes) fun-pointer-lowtag) fun)))

    ;; All the arguments fit in registers, so load them.
    REGISTER-ARGS
    (loadw rdx rsi -1)
    (loadw rdi rsi -2)
    (loadw rsi rsi -3)

    ;; Clear most of the stack.
    (inst lea rsp-tn (ea (* (- sp->fp-offset 3) n-word-bytes) rbp-tn))

    ;; Push the return-pc so it looks like we just called.
    (pushw rbp-tn (frame-word-offset return-pc-save-offset))
    END))

#+sb-assembling ;; No vop for this one either.
(define-assembly-routine
    (tail-call-variable
     (:return-style :none))
    ((:temp fun unsigned-reg rax-offset)
     (:temp temp unsigned-reg rbx-offset)
     (:temp nargs unsigned-reg rcx-offset)
     (:temp rdx unsigned-reg rdx-offset)
     (:temp rdi unsigned-reg rdi-offset)
     (:temp rsi unsigned-reg rsi-offset)
     (:temp r8 unsigned-reg r8-offset)
     (:temp r9 unsigned-reg r9-offset)
     (:temp r10 unsigned-reg r10-offset))
  (prepare-for-tail-call-variable fun temp nargs rdx rdi rsi r8 r9 r10)

  (inst jmp (ea (- (* closure-fun-slot n-word-bytes) fun-pointer-lowtag) fun)))

#+sb-assembling
(define-assembly-routine
    (tail-call-callable-variable
     (:return-style :none))
    ((:temp fun unsigned-reg rax-offset)
     (:temp temp unsigned-reg rbx-offset)
     (:temp nargs unsigned-reg rcx-offset)
     (:temp rdx unsigned-reg rdx-offset)
     (:temp rdi unsigned-reg rdi-offset)
     (:temp rsi unsigned-reg rsi-offset)
     (:temp r8 unsigned-reg r8-offset)
     (:temp r9 unsigned-reg r9-offset)
     (:temp r10 unsigned-reg r10-offset))
  (prepare-for-tail-call-variable fun temp nargs rdx rdi rsi r8 r9 r10 t)

  (%lea-for-lowtag-test rbx-tn fun fun-pointer-lowtag)
  (inst test :byte rbx-tn lowtag-mask)
  (inst jmp :nz (make-fixup 'call-symbol :assembly-routine))
  (inst jmp (ea (- (* closure-fun-slot n-word-bytes) fun-pointer-lowtag) fun)))

#+sb-assembling
(define-assembly-routine (call-symbol
                          (:export undefined-tramp)
                          (:return-style :none))
    ((:temp fun (any-reg descriptor-reg) rax-offset) ; FUN = the symbol
     (:temp tmp (any-reg descriptor-reg) rbx-offset))
  RETRY
  (%lea-for-lowtag-test tmp fun other-pointer-lowtag) ; test SYMBOLP
  (inst test :byte tmp lowtag-mask)
  (inst jmp :nz not-callable)
  (inst cmp :byte (ea (- other-pointer-lowtag) fun) symbol-widetag)
  (inst jmp :ne not-callable)

  (loadw tmp fun symbol-fdefn-slot other-pointer-lowtag)
  (inst test :byte tmp tmp) ; if fdefn is nullptr, it has no lowtag
  (inst jmp :z UNDEFINED-TRAMP)
  (inst mov fun tmp) ; needed if the JMP invokes UNDEFINED-TRAMP
  (inst jmp (ea (- (* fdefn-raw-addr-slot n-word-bytes) other-pointer-lowtag) tmp))
  NOT-CALLABLE
  (inst cmp fun nil-value) ;; NIL doesn't have SYMBOL-WIDETAG
  (inst jmp :e UNDEFINED-TRAMP)
  ;; Not a symbol
  (inst pop (ea n-word-bytes rbp-tn))
  (cerror-call nil 'sb-kernel::object-not-callable-error fun)
  (inst push (ea n-word-bytes rbp-tn))
  (%lea-for-lowtag-test tmp fun fun-pointer-lowtag) ; test FUNCTIONP
  (inst test :byte tmp lowtag-mask)
  (inst jmp :nz RETRY)
  (inst jmp (ea (- (* closure-fun-slot n-word-bytes) fun-pointer-lowtag) fun))
  (inst .align 3)
  UNDEFINED-TRAMP
  (inst pop (ea n-word-bytes rbp-tn))
  (emit-error-break nil cerror-trap (error-number-or-lose 'undefined-fun-error) (list fun))
  (inst push (ea n-word-bytes rbp-tn))
  (inst jmp (ea (- (* closure-fun-slot n-word-bytes) fun-pointer-lowtag) fun)))


(define-assembly-routine (throw
                          (:return-style :full-call-no-return))
                         ((:arg target (descriptor-reg any-reg) rdx-offset)
                          (:arg start any-reg rbx-offset)
                          (:arg count any-reg rcx-offset)
                          (:temp bsp-temp any-reg r11-offset)
                          (:temp catch any-reg rax-offset))

  (declare (ignore start count bsp-temp))

  (load-tl-symbol-value catch *current-catch-block*)
  LOOP

  (let ((error (gen-label)))
    (assemble (:elsewhere)
      (emit-label error)

      ;; Fake up a stack frame so that backtraces come out right.
      (inst push rbp-tn)
      (inst mov rbp-tn rsp-tn)

      (emit-error-break nil error-trap
                        (error-number-or-lose 'unseen-throw-tag-error)
                        (list target)))
    (inst test catch catch)             ; check for NULL pointer
    (inst jmp :z error))

  (inst cmp target (object-slot-ea catch catch-block-tag-slot 0))
  ;; Here RAX points to catch block containing symbol pointed to by RDX.
  (inst jmp :e (make-fixup 'unwind :assembly-routine))

  (loadw catch catch catch-block-previous-catch-slot)
  (inst jmp LOOP))

;;; Simply return and enter the loop in UNWIND instead of calling
;;; UNWIND directly
(define-vop ()
  (:translate %continue-unwind)
  (:policy :fast-safe)
  (:generator 0
    (inst ret)))

(define-assembly-routine (unwind
                          (:return-style :none)
                          (:translate %unwind)
                          (:policy :fast-safe))
                         ((:arg block (any-reg descriptor-reg) rax-offset)
                          (:arg start (any-reg descriptor-reg) rbx-offset)
                          (:arg count (any-reg descriptor-reg) rcx-offset)
                          (:temp uwp unsigned-reg rsi-offset)
                          ;; for unbind-to-here
                          (:temp where unsigned-reg r8-offset)
                          (:temp symbol unsigned-reg r9-offset)
                          (:temp value unsigned-reg r10-offset)
                          (:temp bsp-temp unsigned-reg r11-offset)
                          (:temp zero complex-double-reg float0-offset))
  AGAIN
  (let ((error (generate-error-code nil 'invalid-unwind-error)))
    (inst test block block)             ; check for NULL pointer
    (inst jmp :z error))

  (load-tl-symbol-value uwp *current-unwind-protect-block*)

  ;; Does *CURRENT-UNWIND-PROTECT-BLOCK* match the value stored in
  ;; argument's CURRENT-UWP-SLOT?
  (inst cmp uwp
        (object-slot-ea block unwind-block-uwp-slot 0))
  ;; If a match, return to context in arg block.
  (inst jmp :e DO-EXIT)

  ;; Not a match - return to *CURRENT-UNWIND-PROTECT-BLOCK* context.
  (inst push block)
  (inst push start)
  (inst push count)

  ;; Need to perform unbinding before unlinking the UWP so that if
  ;; interrupted here it can still run the clean up form. While the
  ;; cleanup form itself cannot be protected from interrupts (can't
  ;; run it twice) one of the variables being unbound can be
  ;; *interrupts-enabled*
  (loadw where uwp unwind-block-bsp-slot)
  (unbind-to-here where symbol value bsp-temp zero)

  ;; Set next unwind protect context.
  (loadw block uwp unwind-block-uwp-slot)
  (store-tl-symbol-value block *current-unwind-protect-block*)

  (loadw rbp-tn uwp unwind-block-cfp-slot)

  (loadw block uwp unwind-block-current-catch-slot)
  (store-tl-symbol-value block *current-catch-block*)

  ;; Go to uwp-entry, it can fetch the pushed above values if it needs
  ;; to.
  (inst call (ea (* unwind-block-entry-pc-slot n-word-bytes) uwp))
  (inst pop count)
  (inst pop start)
  (inst pop block)
  (inst jmp AGAIN)

  DO-EXIT
  (loadw where block unwind-block-bsp-slot)
  (unbind-to-here where symbol value bsp-temp zero)

  (loadw rbp-tn block unwind-block-cfp-slot)

  (loadw uwp block unwind-block-current-catch-slot)
  (store-tl-symbol-value uwp *current-catch-block*)

  ;; nlx-entry expects start in RBX and count in RCX
  (inst jmp (ea (* unwind-block-entry-pc-slot n-word-bytes) block)))

;;; These are trampolines, but they benefit from not being in the 'tramps' file
;;; because they'll automatically get a vop and an assembly routine this way,
;;; where tramps only get the assembly routine.
(define-assembly-routine (update-object-layout
                          (:policy :fast-safe)
                          (:translate update-object-layout)
                          (:return-style :raw))
    ((:arg x (descriptor-reg) rdx-offset)
     (:res r (descriptor-reg) rdx-offset))
  (progn x r)
  (with-registers-preserved (lisp :except rdx)
    (call-static-fun 'update-object-layout 1)))

(define-assembly-routine (sb-impl::install-hash-table-lock
                          (:policy :fast-safe)
                          (:translate sb-impl::install-hash-table-lock)
                          (:return-style :raw))
    ((:arg x (descriptor-reg) rdx-offset)
     (:res r (descriptor-reg) rdx-offset))
  (progn x r)
  (with-registers-preserved (lisp :except rdx)
    (call-static-fun 'sb-impl::install-hash-table-lock 1)))

(define-assembly-routine
    (return-values-list (:return-style :none))
    ((:arg list descriptor-reg rax-offset)

     (:temp rbx unsigned-reg rbx-offset)
     (:temp rdx unsigned-reg rdx-offset)
     (:temp rdi unsigned-reg rdi-offset)
     (:temp rsi unsigned-reg rsi-offset)
     (:temp count unsigned-reg rcx-offset)
     (:temp null unsigned-reg r8-offset)
     (:temp temp unsigned-reg r9-offset)
     (:temp return unsigned-reg r10-offset))
  (flet ((check (label)
           (assemble ()
             (%test-lowtag list temp skip nil list-pointer-lowtag)
             (cerror-call nil 'bogus-arg-to-values-list-error list)
             (inst jmp label)
             skip)))
    (assemble ()
      (inst mov null nil-value)
      (%test-lowtag list temp ZERO-VALUES-ERROR t list-pointer-lowtag)
      (inst cmp list null)
      (inst jmp :e ZERO-VALUES)

      (loadw rdx list cons-car-slot list-pointer-lowtag)
      (loadw list list cons-cdr-slot list-pointer-lowtag)
      (inst cmp list null)
      (inst jmp :ne CONTINUE)
      ONE-VALUE
      (inst clc)
      (inst leave)
      (inst ret)

      CONTINUE
      (check ONE-VALUE)

      (inst mov count (fixnumize 2))
      (loadw rdi list cons-car-slot list-pointer-lowtag)
      (loadw list list cons-cdr-slot list-pointer-lowtag)
      (inst cmp list null)
      (inst jmp :e TWO-VALUES)
      (check TWO-VALUES)

      (inst mov count (fixnumize 3))
      (loadw rsi list cons-car-slot list-pointer-lowtag)
      (loadw list list cons-cdr-slot list-pointer-lowtag)
      (inst cmp list null)
      (inst jmp :e THREE-VALUES)
      (check THREE-VALUES)

      ;; As per the calling convention RBX is expected to point at the SP
      ;; before the stack frame.
      (inst lea rbx (ea (* sp->fp-offset n-word-bytes) rbp-tn))

      (inst mov return (ea (frame-byte-offset return-pc-save-offset) rbp-tn))
      (inst lea rsp-tn (ea (frame-byte-offset sp->fp-offset) rbp-tn))
      (inst mov rbp-tn (ea (frame-byte-offset ocfp-save-offset) rbp-tn))

      LOOP
      (inst add count (fixnumize 1))
      (pushw list cons-car-slot list-pointer-lowtag)
      (loadw list list cons-cdr-slot list-pointer-lowtag)
      (check DONE)
      (inst cmp list nil-value)
      (inst jmp :ne LOOP)

      DONE
      (inst stc)
      (inst push return)
      (inst ret)

      ZERO-VALUES-ERROR
      (cerror-call nil 'bogus-arg-to-values-list-error list)
      ZERO-VALUES
      (zeroize count)
      (inst mov rdx null)
      (inst mov rdi null)

      TWO-VALUES
      (inst mov rsi null)

      THREE-VALUES
      (inst lea rbx (ea (* sp->fp-offset n-word-bytes) rbp-tn))
      (inst stc)
      (inst leave)
      (inst ret))))
