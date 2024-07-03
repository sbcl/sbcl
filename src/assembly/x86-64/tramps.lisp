;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB-VM")

;;; The SYNCHRONOUS-TRAP routine has nearly the same effect as executing INT3
;;; but is more friendly to gdb. There may be some subtle bugs with regard to
;;; blocking/unblocking of async signals which arrive nearly around the same
;;; time as a synchronous trap.
#+sw-int-avoidance ; "software interrupt avoidance"
(define-assembly-routine (synchronous-trap) ()
  (inst pushf)
  (inst push rbp-tn)
  (inst mov rbp-tn rsp-tn)
  (inst and rsp-tn (- 16))
  (inst sub rsp-tn 8) ; PUSHing an odd number of GPRs
  ;; Arrange in the utterly confusing order that a linux signal context has them
  ;; so that we can memcpy() into a context. Push RBX twice to maintain alignment.
  (regs-pushlist rcx rax rdx rbx rbx rsi rdi r15 r14 r13 r12 r11 r10 r9 r8)
  ;;                             ^^^ technically this is the slot for RBP
  (inst sub rsp-tn (* 16 16))
  (dotimes (i 16) (inst movdqa (ea (* i 16) rsp-tn) (sb-x86-64-asm::get-fpr :xmm i)))
  (inst lea rdi-tn (ea 24 rbp-tn)) ; stack-pointer at moment of "interrupt"
  (inst mov rsi-tn rsp-tn)         ; pointer to saved CPU state
  (inst call (make-fixup "synchronous_trap" :foreign))
  (dotimes (i 16) (inst movdqa (sb-x86-64-asm::get-fpr :xmm i) (ea (* i 16) rsp-tn)))
  (inst add rsp-tn (* 16 16))
  (regs-poplist rcx rax rdx rbx rbx rsi rdi r15 r14 r13 r12 r11 r10 r9 r8)
  (inst leave)
  (inst popf))

(macrolet ((do-fprs (operation regset &aux (displacement 0))
             ;; The YMM case could be removed now I suppose, since we use XSAVE + XRSTOR
             (multiple-value-bind (mnemonic fpr-align)
                 (ecase regset
                   (:xmm (values 'movaps 16))
                   (:ymm (values 'vmovaps 32)))
               (collect ((insts))
                 ;; RAX as the base register encodes shorter than RSP in an EA.
                 (insts '(inst lea rax-tn (ea 8 rsp-tn)))
                 (dotimes (regno 16 `(progn ,@(insts)))
                   (when (>= displacement 128)
                     (insts '(inst add rax-tn 128))
                     (decf displacement 128)) ; EA displacement stays 1-byte this way
                   (let ((fpr `(sb-x86-64-asm::get-fpr ,regset ,regno)))
                     (insts (ecase operation
                              (push `(inst ,mnemonic (ea ,displacement rax-tn) ,fpr))
                              (pop `(inst ,mnemonic ,fpr (ea ,displacement rax-tn))))))
                   (incf displacement fpr-align))))))
  ;; Caller will have allocated 512+64+256 bytes above the stack-pointer
  ;; prior to the CALL. Use that as the save area.
  (define-assembly-routine (fpr-save) ()
    (test-cpu-feature cpu-has-ymm-registers)
    (inst jmp :nz have-ymm)
    (do-fprs push :xmm)
    (inst ret)
    HAVE-YMM
    ;; Although most of the time RDX can be clobbered, some of the time it can't.
    ;; If WITH-REGISTERS-PRESERVED wraps a lisp function to make it appear to preserve
    ;; all registers, we obviously need to return its primary value in RDX.
    ;; RAX need not be saved though.
    (inst push rdx-tn)
    (zeroize rdx-tn)
    ;; After PUSH the save area is at RSP+16 with the return-PC at [RSP+8]
    ;; Zero the header
    (inst lea rax-tn (ea (+ 512 16) rsp-tn))
    (dotimes (i 8)
      (inst mov (ea (ash i word-shift) rax-tn) rdx-tn))
    (inst mov rax-tn 7)
    (inst xsave (ea 16 rsp-tn))
    (inst pop rdx-tn))

  (define-assembly-routine (fpr-restore) ()
    (test-cpu-feature cpu-has-ymm-registers) (inst jmp :nz have-ymm)
    (do-fprs pop :xmm)
    (inst ret)
    HAVE-YMM
    (inst push rdx-tn)
    (inst mov rax-tn 7) ; OK to clobber RAX
    (zeroize rdx-tn)
    (inst xrstor (ea 16 rsp-tn))
    (inst pop rdx-tn)))

(define-assembly-routine (switch-to-arena (:return-style :raw)) ()
  ;; RSI and RDI are vop temps, so don't bother preserving them
  (with-registers-preserved (c :except (rsi rdi))
    (pseudo-atomic ()
      #-system-tlabs (inst break halt-trap)
      #+system-tlabs (call-c "switch_to_arena" #+win32 rdi-tn #+win32 rsi-tn))))

(macrolet ((def-routine-pair (name&options vars &body code)
             `(progn
                (symbol-macrolet ((system-tlab-p 0))
                  (define-assembly-routine ,name&options ,vars ,@code))
                ;; In absence of this feature, don't define extra routines.
                ;; (Don't want to have a way to mess things up)
                #+system-tlabs
                (symbol-macrolet ((system-tlab-p 2))
                  (define-assembly-routine
                      (,(symbolicate "SYS-" (car name&options)) . ,(cdr name&options))
                    ,vars ,@code)))))

(def-routine-pair (alloc-tramp) ()
  (with-registers-preserved (c)
    (call-c "alloc" (ea 16 rbp-tn) system-tlab-p)
    (inst mov (ea 16 rbp-tn) rax-tn))) ; result onto stack

(def-routine-pair (list-alloc-tramp) () ; CONS, ACONS, LIST, LIST*
  (with-registers-preserved (c)
    (call-c "alloc_list" (ea 16 rbp-tn) system-tlab-p)
    (inst mov (ea 16 rbp-tn) rax-tn))) ; result onto stack

(def-routine-pair (listify-&rest (:return-style :none)) ()
  (with-registers-preserved (c)
    (call-c "listify_rest_arg" (ea 16 rbp-tn) (ea 24 rbp-tn) system-tlab-p)
    (inst mov (ea 24 rbp-tn) rax-tn))   ; result
  (inst ret 8)) ; pop one argument; the unpopped word now holds the result

(def-routine-pair (make-list (:return-style :none)) ()
  (with-registers-preserved (c)
    (call-c "make_list" (ea 16 rbp-tn) (ea 24 rbp-tn) system-tlab-p)
    (inst mov (ea 24 rbp-tn) rax-tn)) ; result
  (inst ret 8)) ; pop one argument; the unpopped word now holds the result
)

(define-assembly-routine (alloc-funinstance) ()
  (with-registers-preserved (c)
    (call-c "alloc_funinstance" (ea 16 rbp-tn))
    (inst mov (ea 16 rbp-tn) rax-tn)))

;;; These routines are for the deterministic consing profiler.
;;; The C support routine's argument is the return PC.
(define-assembly-routine (enable-alloc-counter) ()
  (with-registers-preserved (c)
    #+sb-thread
    (pseudo-atomic ()
      (call-c "allocation_tracker_counted" (* (ea 8 rbp-tn))))))

(define-assembly-routine (enable-sized-alloc-counter) ()
  (with-registers-preserved (c)
    #+sb-thread
    (pseudo-atomic ()
      (call-c "allocation_tracker_sized" (* (ea 8 rbp-tn))))))

#+win32
(define-assembly-routine
    (undefined-alien-tramp (:return-style :none))
    ()
  (error-call nil 'undefined-alien-fun-error rbx-tn))

#-win32
(define-assembly-routine
    (undefined-alien-tramp (:return-style :none))
    ()
  ;; This routine computes into RBX the address of the linkage table entry that was called,
  ;; corresponding to the undefined alien function.
  (inst push rax-tn) ; save registers in case we want to see the old values
  (inst push rbx-tn)
  ;; load RAX with the PC after the call site
  (inst mov rax-tn (ea 16 rsp-tn))
  ;; load RBX with the signed 32-bit immediate from the call instruction
  (inst movsx '(:dword :qword) rbx-tn (ea -4 rax-tn))
  ;; The decoding seems scary, but it's actually not. Any C call-out instruction has
  ;; a 4-byte trailing operand, with the preceding byte being unique.
  ;; if at [PC-5] we see #x25 then it was a call with 32-bit mem addr
  ;; if ...              #xE8 then ...                32-bit offset
  ;; if ...              #x92 then it was "call *DISP(%r10)" where r10 is the table base
  #-immobile-space ; only non-relocatable alien linkage table can use "CALL [ABS]" form
  (progn (inst cmp :byte (ea -5 rax-tn) #x25)
         (inst jmp :e ABSOLUTE))
  #+immobile-space ; only relocatable alien linkage table can use "CALL rel32" form
  (progn (inst cmp :byte (ea -5 rax-tn) #xE8)
         (inst jmp :e RELATIVE)
         (inst cmp :byte (ea -5 rax-tn) #x92)
         (inst jmp :e ABSOLUTE))
  ;; failing those, assume RBX was valid. ("can't happen")
  (inst mov rbx-tn (ea rsp-tn)) ; restore pushed value of RBX
  (inst jmp trap)
  ABSOLUTE
  #-immobile-space (inst sub rbx-tn 8)
  #+immobile-space (inst lea rbx-tn (ea -8 r10-tn rbx-tn))
  (inst jmp TRAP)
  RELATIVE
  (inst add rbx-tn rax-tn)
  TRAP
  ;; XXX: why aren't we adding something to the stack pointer to balance the two pushes?
  ;; (I guess we can only THROW at this point, so it doesn't matter)
  (error-call nil 'undefined-alien-fun-error rbx-tn))

#+debug-gc-barriers
(define-assembly-routine (check-barrier (:return-style :none)) ()
  (inst push rax-tn)
  (inst mov rax-tn (ea 16 rsp-tn))
  (inst not rax-tn)
  (inst test :byte rax-tn 3)
  (inst pop rax-tn)
  (inst jmp :e check)
  (inst ret 24)
  check
  (with-registers-preserved (c :except fp) ;; shouldn't have any fp operations
    (call-c "check_barrier" (ea 16 rbp-tn) (ea 24 rbp-tn) (ea 32 rbp-tn)))
  (inst ret 24))

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
(define-assembly-routine (code-header-set (:return-style :none)) ()
  ;; stack: ret-pc, object, index, value-to-store
  (symbol-macrolet ((object (ea 8 rsp-tn))
                    (word-index (ea 16 rsp-tn))
                    (newval (ea 24 rsp-tn))
                    ;; these are declared as vop temporaries
                    (rax rax-tn)
                    (rdx rdx-tn)
                    (rdi rdi-tn))
    (pseudo-atomic ()
      #+immobile-space
      (progn
        #-sb-thread
        (let ((fixup (make-fixup "all_threads" :foreign-dataref)))
          ;; Load THREAD-BASE-TN from the all_threads. Does not need to be spilled
          ;; to stack, because we do do not give the register allocator access to it.
          (inst mov thread-tn (rip-relative-ea fixup))
          (inst mov thread-tn (ea thread-tn)))
        (inst mov rax object)
        (inst sub rax (thread-slot-ea thread-text-space-addr-slot))
        (inst shr rax (1- (integer-length immobile-card-bytes)))
        (inst cmp rax (thread-slot-ea thread-text-card-count-slot))
        (inst jmp :ae try-dynamic-space)
        (inst mov rdi (thread-slot-ea thread-text-card-marks-slot))
        (inst bts :dword :lock (ea rdi-tn) rax)
        (inst jmp store))
      TRY-DYNAMIC-SPACE
      (inst mov rax object)
      (inst shr rax gencgc-card-shift)
      (inst and :dword rax card-index-mask)
      (inst mov :byte (ea gc-card-table-reg-tn rax) CARD-MARKED)
      STORE
      (inst mov rdi object)
      (inst mov rdx word-index)
      (inst mov rax newval)
      ;; set 'written' flag in the code header
      (inst or :byte :lock (ea (- 3 other-pointer-lowtag) rdi) #x40)
      ;; store newval into object
      (inst mov (ea (- other-pointer-lowtag) rdi rdx n-word-bytes) rax)))
  (inst ret 24)) ; remove 3 stack args

#+ultrafutex
(progn
(define-assembly-routine (mutex-wake-waiter (:return-style :raw)) ()
  (with-registers-preserved (lisp)
    (inst call (make-fixup "lispmutex_wake_waiter" :foreign)))) ; no args!

(define-assembly-routine (mutex-unlock (:return-style :raw)) ()
  ;; There are no registers reserved for this asm routine
  (inst push rax-tn)
  (inst mov rax-tn (thread-tls-ea (load-time-tls-offset '*current-mutex*)))
  (inst mov :qword (mutex-slot rax-tn %owner) 0)
  (inst dec :lock :byte (mutex-slot rax-tn state))
  (inst jmp :z uncontested) ; if ZF then previous value was 1, no waiters
  (inst call (make-fixup 'mutex-wake-waiter :assembly-routine))
  uncontested
  (inst pop rax-tn))
) ; end PROGN
