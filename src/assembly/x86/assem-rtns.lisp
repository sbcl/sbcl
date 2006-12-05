;;;; the machine specific support routines needed by the file assembler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; RETURN-MULTIPLE

;;; For RETURN-MULTIPLE, we have to move the results from the end of
;;; the frame for the function that is returning to the end of the
;;; frame for the function being returned to.

#+sb-assembling ;; We don't want a vop for this one.
(define-assembly-routine
    (return-multiple (:return-style :none))
    (;; These four are really arguments.
     (:temp eax unsigned-reg eax-offset)
     (:temp ebx unsigned-reg ebx-offset)
     (:temp ecx unsigned-reg ecx-offset)
     (:temp esi unsigned-reg esi-offset)

     ;; These we need as temporaries.
     (:temp edx unsigned-reg edx-offset)
     (:temp edi unsigned-reg edi-offset))

  ;; Pick off the cases where everything fits in register args.
  (inst jecxz zero-values)
  (inst cmp ecx (fixnumize 1))
  (inst jmp :e one-value)
  (inst cmp ecx (fixnumize 2))
  (inst jmp :e two-values)
  (inst cmp ecx (fixnumize 3))
  (inst jmp :e three-values)

  ;; Save the count, because the loop is going to destroy it.
  (inst mov edx ecx)

  ;; Blit the values down the stack. Note: there might be overlap, so
  ;; we have to be careful not to clobber values before we've read
  ;; them. Because the stack builds down, we are coping to a larger
  ;; address. Therefore, we need to iterate from larger addresses to
  ;; smaller addresses. pfw-this says copy ecx words from esi to edi
  ;; counting down.
  (inst shr ecx 2)                      ; fixnum to raw word count
  (inst std)                            ; count down
  (inst sub esi 4)                      ; ?
  (inst lea edi (make-ea :dword :base ebx :disp (- n-word-bytes)))
  (inst rep)
  (inst movs :dword)

  ;; solaris requires DF being zero.
  #!+sunos (inst cld)

  ;; Restore the count.
  (inst mov ecx edx)

  ;; Set the stack top to the last result.
  (inst lea esp-tn (make-ea :dword :base edi :disp n-word-bytes))

  ;; Load the register args.
  (loadw edx ebx -1)
  (loadw edi ebx -2)
  (loadw esi ebx -3)

  ;; And back we go.
  (inst stc)
  (inst jmp eax)

  ;; Handle the register arg cases.
  ZERO-VALUES
  (move esp-tn ebx)
  (inst mov edx nil-value)
  (inst mov edi edx)
  (inst mov esi edx)
  (inst stc)
  (inst jmp eax)

  ONE-VALUE ; Note: we can get this, because the return-multiple vop
            ; doesn't check for this case when size > speed.
  (loadw edx esi -1)
  (inst mov esp-tn ebx)
  (inst clc)
  (inst jmp eax)

  TWO-VALUES
  (loadw edx esi -1)
  (loadw edi esi -2)
  (inst mov esi nil-value)
  (inst lea esp-tn (make-ea :dword :base ebx :disp (* -2 n-word-bytes)))
  (inst stc)
  (inst jmp eax)

  THREE-VALUES
  (loadw edx esi -1)
  (loadw edi esi -2)
  (loadw esi esi -3)
  (inst lea esp-tn (make-ea :dword :base ebx :disp (* -3 n-word-bytes)))
  (inst stc)
  (inst jmp eax))

;;;; TAIL-CALL-VARIABLE

;;; For tail-call-variable, we have to copy the arguments from the end
;;; of our stack frame (were args are produced) to the start of our
;;; stack frame (were args are expected).
;;;
;;; We take the function to call in EAX and a pointer to the arguments in
;;; ESI. EBP says the same over the jump, and the old frame pointer is
;;; still saved in the first stack slot. The return-pc is saved in
;;; the second stack slot, so we have to push it to make it look like
;;; we actually called. We also have to compute ECX from the difference
;;; between ESI and the stack top.
#+sb-assembling ;; No vop for this one either.
(define-assembly-routine
    (tail-call-variable
     (:return-style :none))

    ((:temp eax unsigned-reg eax-offset)
     (:temp ebx unsigned-reg ebx-offset)
     (:temp ecx unsigned-reg ecx-offset)
     (:temp edx unsigned-reg edx-offset)
     (:temp edi unsigned-reg edi-offset)
     (:temp esi unsigned-reg esi-offset))

  ;; Calculate NARGS (as a fixnum)
  (move ecx esi)
  (inst sub ecx esp-tn)

  ;; Check for all the args fitting the registers.
  (inst cmp ecx (fixnumize 3))
  (inst jmp :le REGISTER-ARGS)

  ;; Save the OLD-FP and RETURN-PC because the blit it going to trash
  ;; those stack locations. Save the ECX, because the loop is going
  ;; to trash it.
  (pushw ebp-tn -1)
  (loadw ebx ebp-tn -2)
  (inst push ecx)

  ;; Do the blit. Because we are coping from smaller addresses to
  ;; larger addresses, we have to start at the largest pair and work
  ;; our way down.
  (inst shr ecx 2)                      ; fixnum to raw words
  (inst std)                            ; count down
  (inst lea edi (make-ea :dword :base ebp-tn :disp (- n-word-bytes)))
  (inst sub esi (fixnumize 1))
  (inst rep)
  (inst movs :dword)

  ;; solaris requires DF being zero.
  #!+sunos (inst cld)

  ;; Load the register arguments carefully.
  (loadw edx ebp-tn -1)

  ;; Restore OLD-FP and ECX.
  (inst pop ecx)
  (popw ebp-tn -1)                      ; overwrites a0

  ;; Blow off the stack above the arguments.
  (inst lea esp-tn (make-ea :dword :base edi :disp n-word-bytes))

  ;; remaining register args
  (loadw edi ebp-tn -2)
  (loadw esi ebp-tn -3)

  ;; Push the (saved) return-pc so it looks like we just called.
  (inst push ebx)

  ;; And jump into the function.
    (inst jmp
          (make-ea :byte :base eax
                   :disp (- (* closure-fun-slot n-word-bytes)
                            fun-pointer-lowtag)))

  ;; All the arguments fit in registers, so load them.
  REGISTER-ARGS
  (loadw edx esi -1)
  (loadw edi esi -2)
  (loadw esi esi -3)

  ;; Clear most of the stack.
  (inst lea esp-tn
        (make-ea :dword :base ebp-tn :disp (* -3 n-word-bytes)))

  ;; Push the return-pc so it looks like we just called.
  (pushw ebp-tn -2)

  ;; And away we go.
  (inst jmp (make-ea :byte :base eax
                     :disp (- (* closure-fun-slot n-word-bytes)
                              fun-pointer-lowtag))))

(define-assembly-routine (throw
                          (:return-style :none))
                         ((:arg target (descriptor-reg any-reg) edx-offset)
                          (:arg start any-reg ebx-offset)
                          (:arg count any-reg ecx-offset)
                          (:temp catch any-reg eax-offset))

  (declare (ignore start count))

  (load-tl-symbol-value catch *current-catch-block*)

  LOOP

  (let ((error (generate-error-code nil unseen-throw-tag-error target)))
    (inst or catch catch)               ; check for NULL pointer
    (inst jmp :z error))

  (inst cmp target (make-ea-for-object-slot catch catch-block-tag-slot 0))
  (inst jmp :e exit)

  (loadw catch catch catch-block-previous-catch-slot)
  (inst jmp loop)

  EXIT

  ;; Here EAX points to catch block containing symbol pointed to by EDX.
  (inst jmp (make-fixup 'unwind :assembly-routine)))

;;;; non-local exit noise

(define-assembly-routine (unwind
                          (:return-style :none)
                          (:translate %continue-unwind)
                          (:policy :fast-safe))
                         ((:arg block (any-reg descriptor-reg) eax-offset)
                          (:arg start (any-reg descriptor-reg) ebx-offset)
                          (:arg count (any-reg descriptor-reg) ecx-offset)
                          (:temp uwp unsigned-reg esi-offset))
  (declare (ignore start count))

  (let ((error (generate-error-code nil invalid-unwind-error)))
    (inst or block block)               ; check for NULL pointer
    (inst jmp :z error))

  (load-tl-symbol-value uwp *current-unwind-protect-block*)

  ;; Does *CURRENT-UNWIND-PROTECT-BLOCK* match the value stored in
  ;; argument's CURRENT-UWP-SLOT?
  (inst cmp uwp
        (make-ea-for-object-slot block unwind-block-current-uwp-slot 0))
  ;; If a match, return to context in arg block.
  (inst jmp :e do-exit)

  ;; Not a match - return to *CURRENT-UNWIND-PROTECT-BLOCK* context.
  ;; Important! Must save (and return) the arg 'block' for later use!!
  (move edx-tn block)
  (move block uwp)

  ;; We need to check for Win32 exception frames before overwriting
  ;; *C-U-P-B* (if the Win32 frames NLX, we need the UWP to still be
  ;; live.)  As of this writing, we can't take a Win32 NLX across our
  ;; frames, but the frame can NLX to another foreign frame that
  ;; doesn't cross ours and then return normally, and if we drop the
  ;; UWP beforehand then we just broke UWP semantics.
  #!+win32
  (assemble ()
    (inst fs-segment-prefix)
    (inst cmp block (make-ea :dword))
    (inst jmp :le NO-WIN32-UNWIND)
    (inst call WIN32-UNWIND)
    NO-WIN32-UNWIND)

  ;; Set next unwind protect context.
  (loadw uwp uwp unwind-block-current-uwp-slot)
  ;; we're about to reload ebp anyway, so let's borrow it here as a
  ;; temporary.  Hope this works
  (store-tl-symbol-value uwp *current-unwind-protect-block* ebp-tn)

  DO-EXIT

  ;; Same as above with *C-U-P-B*, except that this is for our target
  ;; block, not a UWP.  Still need to check for Win32 exception frames.
  #!+win32
  (assemble ()
    (inst fs-segment-prefix)
    (inst cmp block (make-ea :dword))
    (inst jmp :le NO-WIN32-UNWIND)
    (inst call WIN32-UNWIND)
    NO-WIN32-UNWIND)

  (loadw ebp-tn block unwind-block-current-cont-slot)

  ;; Uwp-entry expects some things in known locations so that they can
  ;; be saved on the stack: the block in edx-tn, start in ebx-tn, and
  ;; count in ecx-tn.

  (inst jmp (make-ea :byte :base block
                     :disp (* unwind-block-entry-pc-slot n-word-bytes)))

  #!+win32
  WIN32-UNWIND
  ;; At this point we need to call RtlUnwind@16 to clear up one or
  ;; more Win32 exception frames on the stack.  This is an unusual FFI
  ;; in that it kills most of the registers, and it returns to the
  ;; address at [EBP+4].
  #!+win32
  (assemble ()
    ;; Regs get clobbered by this process, so save the lot of them.
    (inst pusha)

    ;; Okay, our current unwind target is in BLOCK (EAX). All of our
    ;; other regs are on the stack.  We need to find the first Win32
    ;; exception frame that we -aren't- going to unwind.
    (inst fs-segment-prefix)
    (inst mov ecx-tn (make-ea :dword))
    FIND-TARGET-FRAME
    (inst cmp block ecx-tn)
    (inst jmp :le FOUND-TARGET-FRAME)
    (inst mov ecx-tn (make-ea :dword :base ecx-tn))
    (inst jmp FIND-TARGET-FRAME)
    FOUND-TARGET-FRAME

    ;; This section copied from VOP CALL-OUT.
    ;; Setup the NPX for C; all the FP registers need to be
    ;; empty; pop them all.
    (dotimes (i 8)
      (inst fstp fr0-tn))

    ;; I'm unlikely to ever forget this again.
    (inst cld)

    ;; Set up a bogus stack frame for RtlUnwind to pick its return
    ;; address from.  (Yes, this is how RtlUnwind works.)
    (inst push (make-fixup 'win32-unwind-tail :assembly-routine))
    (inst push ebp-tn)
    (inst mov ebp-tn esp-tn)

    ;; Actually call out for the unwind.
    (inst push 0)
    (inst push 0)
    (inst push 0)
    (inst push ecx-tn)
    (inst call (make-fixup "RtlUnwind@16" :foreign))))

;; We want no VOP for this one and for it to only happen on Win32
;; targets.  Hence the following disaster.
#!+win32
#-sb-assembling nil
#+sb-assembling
(define-assembly-routine
    (win32-unwind-tail (:return-style :none))
    ()

    ;; The unwind returns here.  Had to use a VOP for this because
    ;; PUSH won't accept a label as an argument.

    ;; Clean up the bogus stack frame we pushed for the unwind.
    (inst pop ebp-tn)
    (inst pop esi-tn) ;; Random scratch register.

    ;; This section based on VOP CALL-OUT.
    ;; Restore the NPX for lisp; ensure no regs are empty
    (dotimes (i 8)
      (inst fldz))

    ;; Restore our regs and pick up where we left off.
    (inst popa)
    (inst ret))
