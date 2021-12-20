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
  (inst test rcx rcx)
  (inst jmp :z ZERO-VALUES)
  (inst cmp rcx (fixnumize 1))
  (inst jmp :e ONE-VALUE)
  (inst cmp rcx (fixnumize 2))
  (inst jmp :e TWO-VALUES)
  (inst cmp rcx (fixnumize 3))
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
  (inst mov rsp-tn rbp-tn)
  (inst stc)
  (inst pop rbp-tn)
  (inst ret)

  ;; Note: we can get this, because the return-multiple vop doesn't
  ;; check for this case when size > speed.
  ONE-VALUE
  (loadw rdx rsi -1)
  (inst mov rsp-tn rbp-tn)
  (inst clc)
  (inst pop rbp-tn)
  (inst ret)

  TWO-VALUES
  (inst lea rbx (ea (* sp->fp-offset n-word-bytes) rbp-tn))
  (loadw rdx rsi -1)
  (loadw rdi rsi -2)
  (inst mov rsi nil-value)
  (inst mov rsp-tn rbp-tn)
  (inst stc)
  (inst pop rbp-tn)
  (inst ret)

  THREE-VALUES
  (inst lea rbx (ea (* sp->fp-offset n-word-bytes) rbp-tn))
  (loadw rdx rsi -1)
  (loadw rdi rsi -2)
  (loadw rsi rsi -3)
  (inst mov rsp-tn rbp-tn)
  (inst stc)
  (inst pop rbp-tn)
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
(defun !prepare-for-tail-call-variable (fun temp nargs rdx rdi rsi
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
  (!prepare-for-tail-call-variable fun temp nargs rdx rdi rsi r8 r9 r10)

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
  (!prepare-for-tail-call-variable fun temp nargs rdx rdi rsi r8 r9 r10 t)

  (%lea-for-lowtag-test rbx-tn fun fun-pointer-lowtag)
  (inst test :byte rbx-tn lowtag-mask)
  (inst jmp :nz (make-fixup 'call-symbol :assembly-routine))
  (inst jmp (ea (- (* closure-fun-slot n-word-bytes) fun-pointer-lowtag) fun)))

#+sb-assembling
(define-assembly-routine (call-symbol
                          (:return-style :none))
    ((:temp fun (any-reg descriptor-reg) rax-offset) ; FUN = the symbol
     (:temp fdefn (any-reg descriptor-reg) rbx-offset))
  (%lea-for-lowtag-test fdefn fun other-pointer-lowtag)
  (inst test :byte fdefn lowtag-mask)
  (inst jmp :nz not-callable)
  (inst cmp :byte (ea (- other-pointer-lowtag) fun) symbol-widetag)
  (inst jmp :ne not-callable)

  (loadw fdefn fun symbol-fdefn-slot other-pointer-lowtag)
  (inst test :dword fdefn fdefn)
  (inst jmp :z UNDEFINED)
  ;; I think we need this MOV because the undefined-function trap examines RAX
  ;; to see what FDEFN you tried to call through.
  (inst mov fun fdefn)
  (inst jmp (ea (- (* fdefn-raw-addr-slot n-word-bytes) other-pointer-lowtag) fdefn))
  UNDEFINED
  (inst jmp (make-fixup 'undefined-tramp :assembly-routine))
  NOT-CALLABLE
  (inst cmp fun nil-value) ;; NIL doesn't have SYMBOL-WIDETAG
  (inst jmp :e undefined)

  (inst pop (ea n-word-bytes rbp-tn))
  (emit-error-break nil error-trap (error-number-or-lose 'sb-kernel::object-not-callable-error)
                    (list fun)))


(define-assembly-routine (throw
                          (:return-style :raw))
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
  (inst jmp :e EXIT)

  (loadw catch catch catch-block-previous-catch-slot)
  (inst jmp LOOP)

  EXIT

  ;; Here RAX points to catch block containing symbol pointed to by RDX.
  ;; An extra RET gets stuffed after the JMP, but oh well. You can't just change
  ;; the :return-style to :none because that also affects the call sequence.
  (inst jmp (make-fixup 'unwind :assembly-routine)))

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

#+sb-assembling
(defun ensure-thread-base-tn-loaded ()
  #-sb-thread
  (progn
    ;; Load THREAD-BASE-TN from the all_threads. Does not need to be spilled
    ;; to stack, because we do do not give the register allocator access to it.
    ;; And call_into_lisp saves it as per convention, not that it matters,
    ;; because there's no way to get back into C code anyhow.
    (inst mov thread-tn (ea (make-fixup "all_threads" :foreign-dataref)))
    (inst mov thread-tn (ea thread-tn))))

;;; Perform a store to code, updating the GC card mark bit.
;;; This has two additional complications beyond the ordinary
;;; generational barrier:
;;; 1. immobile code uses its own card table which maps linearly
;;;    with the page index, unlike the dynamic space card table
;;;    that has a different way of computing a card address.
;;; 2. code objects are so seldom written that it behooves us to
;;;    track within each object whether it has been written,
;;;    thereby avoiding scanning of unwritten objects.
;;;    This is especially important for immobile space where
;;;    it is likely that new code will be co-located on a page
;;;    with old code due to the non-moving allocator.
#+sb-assembling
(define-assembly-routine (code-header-set (:return-style :none)) ()
  ;; stack: ret-pc, object, index, value-to-store
  (symbol-macrolet ((object (ea 8 rsp-tn))
                    (word-index (ea 16 rsp-tn))
                    (newval (ea 24 rsp-tn))
                    ;; these are declared as vop temporaries
                    (rax rax-tn)
                    (rdx rdx-tn)
                    (rdi rdi-tn))
  (ensure-thread-base-tn-loaded)
  (pseudo-atomic ()
    (assemble ()
      #+immobile-space
      (progn
        (inst mov rax object)
        (inst sub rax (thread-slot-ea thread-varyobj-space-addr-slot))
        (inst shr rax (1- (integer-length immobile-card-bytes)))
        (inst cmp rax (thread-slot-ea thread-varyobj-card-count-slot))
        (inst jmp :ae try-dynamic-space)
        (inst mov rdi (thread-slot-ea thread-varyobj-card-marks-slot))
        (inst bts :dword :lock (ea rdi-tn) rax)
        (inst jmp store))
      TRY-DYNAMIC-SPACE
      (inst mov rax object)
      (inst shr rax gencgc-card-shift)
      (inst and :dword rax card-index-mask)
      (inst mov :byte (ea gc-card-table-reg-tn rax) 0)
      STORE
      (inst mov rdi object)
      (inst mov rdx word-index)
      (inst mov rax newval)
      ;; set 'written' flag in the code header
      (inst or :byte :lock (ea (- 3 other-pointer-lowtag) rdi) #x40)
      ;; store newval into object
      (inst mov (ea (- other-pointer-lowtag) rdi rdx n-word-bytes) rax))))
  (inst ret 24)) ; remove 3 stack args

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
  (with-registers-preserved (lisp :except rdx-tn)
    (call-static-fun 'update-object-layout 1)))

(define-assembly-routine (sb-impl::install-hash-table-lock
                          (:policy :fast-safe)
                          (:translate sb-impl::install-hash-table-lock)
                          (:return-style :raw))
    ((:arg x (descriptor-reg) rdx-offset)
     (:res r (descriptor-reg) rdx-offset))
  (progn x r)
  (with-registers-preserved (lisp :except rdx-tn)
    (call-static-fun 'sb-impl::install-hash-table-lock 1)))

;;; From a perspective of reducing code bloat, this asm routine does not merit
;;; being one at all. A vop would suffice, because it's a single-use vop.
;;; However the WITH-REGISTERS-PRESERVED macro seems to think that it is utilized
;;; only by asm code in *ASSEMBLER-ROUTINES*. I had trouble with it otherwise.
(define-assembly-routine (alloc-code (:return-style :raw))
    ((:arg arg (signed-reg) rdx-offset)
     (:res res (descriptor-reg) rdx-offset))
  (with-registers-preserved (c :except rdx-tn)
    (inst mov rdi-tn arg)
    (pseudo-atomic ()
      (inst call (make-fixup "alloc_code_object" :foreign)))
    (move res rax-tn)))
