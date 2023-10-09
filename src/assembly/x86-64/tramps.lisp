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
  (inst and rsp-tn -16)
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

;; If reg holds a pointer and does not point to static or readonly space,
;; then invoke the barrier. Should also check not pointing to a stack
;; or arena etc. But this is adequate for now, and avoids the most common
;; effectless barrier of setting a slot to NIL or T.
;; This should use the pointer tag for something
;; (because lists can't be large objects, etc)
;;
(defun test-pointer-interesting (reg &key yep nope)
  (aver (not (location= reg rax-tn)))
  (assemble ()
    ;; Either the true or false branch but not both can be :DROPTHRU
    (when (eq nope :dropthru) (setq nope DONE))
    (when (eq yep :dropthru) (setq yep DONE))
    ;; First check that reg holds a pointer.
    (inst lea rax-tn (ea -3 reg))
    ;; low 2 bits are 0 if it was a pointer. cf. is_lisp_pointer() in runtime.h
    (inst test :byte rax-tn 3)
    (inst jmp :nz NOPE) ; if nonzero then it's not a tagged pointer
    ;; This is super easy if the object is in a small object subheap.
    (inst sub rax-tn (static-symbol-value-ea 'bitmap-heap-base))
    (inst cmp rax-tn (static-symbol-value-ea 'bitmap-heap-size))
    (inst jmp :b YEP)
    ;; Rule out readonly space
    (inst mov rax-tn (ea (make-fixup "read_only_space_free_pointer" :foreign-dataref)))
    (inst cmp reg (ea rax-tn))
    (inst jmp :ae TEST-STATIC) ; above read-only space, so not in that space
    (inst mov rax-tn (ea (make-fixup "READ_ONLY_SPACE_START" :foreign-dataref)))
    (inst cmp reg (ea rax-tn))
    ;; >= space start implies it is in the R/O space, therefore NOT a relevant pointer
    (inst jmp :ae NOPE)
    TEST-STATIC
    ;; Rule out static addresses
    (inst lea rax-tn (ea (- static-space-start) reg))
    ;; If "below" after this comparison, that's a "nope", because
    ;; the pointer sees static space and hence is uninteresting.
    (inst cmp rax-tn (- static-space-end static-space-start))
    ;; Figure out which way is the dropthru, and branch in the opposite case
    (if (eq yep done) (inst jmp :b NOPE) (inst jmp :ae YEP))
    DONE))

;; TODOs:
;; * remember() could be rewritten into Lisp or Lisp assembly so that we don't
;;    have to save/restore all floating-point registers.
;; * Probably want to add routines for:
;;   - gc-barrier-store-car and gc-barrier-store-cdr which need no index
;;   - gc-barrier-svset which takes the 2nd arg as a vector index
;;   - gc-barrier-store-wordindexed which will take a constant 2nd arg

(symbol-macrolet ((c-arg1 rdi-tn) (c-arg2 rsi-tn) (c-arg3 rdx-tn) (c-arg4 rcx-tn))
(macrolet
    ((define (name c-name)
       `(define-assembly-routine (,name (:return-style :none)) ()
          #+smlgc-telemetry (inst inc :qword (thread-slot-ea thread-ct-store-barriers-slot))
          ;(inst ud1 rax-tn (ea rax-tn)) ; capture mcontext for debugging
          (inst push rbp-tn) (inst mov rbp-tn rsp-tn) (regs-pushlist rax rcx)
          ;; Stack:
          ;;    saved-RCX  saved-RAX  saved-RBP  return-pc  Object   EA       val
          ;; @  -16(rbp)   -8(rbp)    0(rbp)     8(rbp)     16(rbp)  24(rbp)  32(rbp)
          (inst mov rax-tn (ea 24 rbp-tn)) ; EA
          ;; Skip the barrier if storing into a dynamic-extent object by testing whether
          ;; EA is within the control stack.
          ;; untagged pointer case doesn't need to try to rule out stores to stack
          ;; since there is no case where we create a dynamic-extent object containing
          ;; a precise pointer that lacks tag bits.
          ;; TODO: skip also if storing to static or pseudostatic space
          ;; (basically the corefile-mapped ranges)
          ,@(when (eq name 'gc-barrier-store)
              '((inst cmp rax-tn rsp-tn) ; can't be a stack address if below RSP
                (inst jmp :b continue)
                (inst cmp rax-tn (thread-slot-ea thread-control-stack-end-slot))
                (inst jmp :ae continue)
                ;; Storing to a dynamic-extent object. No barrier needed
                (inst mov rcx-tn (ea 32 rbp-tn)) ; newval into RCX
                (inst mov (ea rax-tn) rcx-tn) ; newval to memory
                (regs-poplist rbp rax rcx)
                (inst ret 24))) ; remove 3 args
          CONTINUE
          ;; The barrier routines must be pseudo-atomic because testing the GC phase
          ;; commits us to performing 0, 1, or 2 "remember" operations.
          ;; Any pending phase change must not be acknowledged by the mutator
          ;; until after the operations are done.
          (pseudo-atomic ()
            (inst mov rcx-tn (ea rax-tn)) ; get oldval into RCX
            (inst mov rax-tn (ea 32 rbp-tn)) ; load newval
            ;; Return quickly when (EQ OLD NEW) - consider that sml_cmpswap performs
            ;; no barrier if its comparison failed.
            (inst cmp rcx-tn rax-tn) ; old and new respectively
            (inst jmp :e ELIDE)
            ;; For untagged pointers, assume that the barrier is always needed.
            ,@(unless (search "-UNTAGGED" (string name))
               ;; Consider whether 'oldval' needs the deletion barrier
               '((test-pointer-interesting rcx-tn :yep BARRIER :nope :dropthru)
                 ;; Comment from C:
                 ;; /* In either SYNC1, PRESYNC2, or SYNC2 phase, snooping
                 ;;  * write barrier is required. */
                 (inst cmp :byte (thread-slot-ea thread-gc-phase-slot) GC-PHASE-MARK)
                 (inst jmp :e ELIDE)
                 ;; reload newval. It's OK to use RCX because if 'oldval' needed
                 ;; a deletion barrier, then we'd have already jumped to BARRIER.
                 (inst mov rcx-tn (ea 32 rbp-tn))
                 (test-pointer-interesting rcx-tn :yep :dropthru :nope ELIDE)
                 ;; pass oldval as 0 when dropping through from here
                 (zeroize rcx-tn)))
            BARRIER
            (with-registers-preserved (c :frame-reg nil :except (rax rcx #|:fprs|#))
              (inst mov c-arg1 rcx-tn)         ; oldval
              (inst mov c-arg2 (ea 32 rbp-tn)) ; newval
              (inst mov c-arg3 (ea 16 rbp-tn)) ; object
              (inst mov c-arg4 (ea 24 rbp-tn)) ; EA - for debugging only
              (inst call (make-fixup ,c-name :foreign)))
            (inst lea rsp-tn (ea -16 rbp-tn))
            ELIDE
            (inst mov rax-tn (ea 24 rbp-tn)) ; EA
            (inst mov rcx-tn (ea 32 rbp-tn)) ; newval into RCX
            (inst mov (ea rax-tn) rcx-tn))
          (regs-poplist rbp rax rcx)
          (inst ret 24)))) ; remove 3 args
  (define gc-barrier-store "lisp_gcbar")
  (define gc-barrier-store-untagged "lisp_gcbar_untagged"))

;; compare-and-swap to a stack address is rare, don't try to optimize for it.
;; Object, EA, newval are passed on stack; oldval in RAX.

;; Performing the barrier _before_ doing the compare-exchange is bad for performance
;; as it causes thousands of instructions to execute in between the user code's reading
;; of 'oldval' and using it as the operand.
;; it is necessary for correctness unless we can acquire the collector's spinlock
;; that guards the objects_from_mutators.
;; On the other hand, all threads suffer equally.
(macrolet
    ((define (name c-name)
       `(define-assembly-routine (,name (:return-style :none)) ()
          ;; Stack:
          ;;    saved-RAX  saved-RCX  saved-RDX  saved-RBP  return-pc  Object   EA       val
          ;; @  -24(rbp)   -16(rbp)   -8(rbp)    0(rbp)     8(rbp)     16(rbp)  24(rbp)  32(rbp)
          (inst push rbp-tn) (inst mov rbp-tn rsp-tn) (regs-pushlist rdx rcx rax)
          (inst mov rcx-tn rax-tn) ; RCX gets oldval
          (inst mov rdx-tn (ea 32 rbp-tn)) ; RDX gets newval
          (pseudo-atomic ()
            ,@(unless (search "-UNTAGGED" (string name))
                ;; Consider whether 'oldval' needs the deletion barrier
                '((test-pointer-interesting rcx-tn :yep BARRIER :nope :dropthru)
                  ;; If strictly after SYNC2 phase then the insertion barrier is disabled
                  (inst cmp :byte (thread-slot-ea thread-gc-phase-slot) GC-PHASE-SYNC2)
                  (inst jmp :a ELIDE)
                  ;; Consider whether 'newval' needs the insertion barrier
                  (test-pointer-interesting rdx-tn :yep :dropthru :nope ELIDE)))
            BARRIER
            (with-registers-preserved (c :frame-reg nil :except (rax rcx rdx #|:fprs|#))
              (move c-arg1 rcx-tn) ; oldval
              (move c-arg2 rdx-tn) ; newval
              (inst mov c-arg3 (ea 16 rbp-tn)) ; object
              (inst mov c-arg4 (ea 24 rbp-tn)) ; EA
              (inst call (make-fixup ,c-name :foreign)))
            (inst lea rsp-tn (ea -24 rbp-tn))
            ELIDE
            (inst pop rax-tn) ; oldval
            (inst mov rcx-tn (ea 24 rbp-tn)) ; EA
            (inst mov rdx-tn (ea 32 rbp-tn)) ; newval
            (inst cmpxchg :lock (ea rcx-tn) rdx-tn)) ; actual oldval goes to RAX
          (regs-poplist rbp rdx rcx)
          (inst ret 24)))) ; remove 3 args
  (define gc-barrier-cmpxchg "lisp_gcbar")
  (define gc-barrier-cmpxchg-untagged "lisp_gcbar_untagged"))

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

(macrolet
    ((choose-allocptr ()
       '(progn
          (inst cmp size smlgc-blocksize-max)
          (inst jmp :a LARGE)
          (inst lea :dword ap (ea -1 size)) ; size 16 becomes 15 etc
          (inst bsr :dword ap ap) ; size 16 -> 3, 32 -> 4, etc
          (inst shl :dword ap 5) ; ap *= sizeof (struct alloc_ptr)
          (inst lea ap (ea (+ (ash thread-ap4-slot word-shift) (* -3 32))
                           thread-tn ap))))
      (allocate ()
       '(let ((ok (gen-label)) (skip (gen-label)) (continue (gen-label)))
          (inst mov bmwordptr-reg (freebit.ptr))
          (inst mov :dword mask (freebit.mask)) ; MASK has the bit we want next
          (inst test :dword (ea bmwordptr-reg) mask) ; can we have it?
          (inst jmp :z OK)
          ;; RDI holds the allocation pointer
          (inst call (make-fixup 'bitmap-alloc-fallback :assembly-routine))
          (inst jmp continue)
          (emit-label OK)
          ;; advance the bit, w/wraparound
          (inst rol :dword mask 1)
          (inst mov :dword (freebit.mask) mask) ; writeback
          ;; possibly increment bitmap word ptr
          (inst jmp :nc SKIP)
          (inst add bmwordptr-reg 4)
          (inst mov (freebit.ptr) bmwordptr-reg)
          (emit-label SKIP)
          (inst mov rax-tn (segment.blocksize))
          (inst xadd (freeptr) rax-tn)
          (emit-label CONTINUE)
          (assert-word-unused (ea rax-tn)))))
(symbol-macrolet ((size rax-tn)
                  (bmwordptr-reg rax-tn) ; same physical reg as SIZE
                  (ap rdi-tn)
                  (mask rcx-tn)
                  (xmm-temp float7-tn)) ; declared as a vop temp
;;; For both of these routes: RAX recives SIZE and returns the result
(with-bitmap-ap (ap)
(define-assembly-routine (bitmap-vect-alloc (:return-style :none)) ()
  (inst push rbp-tn) (inst mov rbp-tn rsp-tn) (regs-pushlist rcx rdi)
  ;; Stack:
  ;;     saved-RDI  saved-RCX  saved-RBP  return-pc  WIDETAG  LENGTH
  ;;; @  -16(rbp)   -8(rbp)    0(rbp)     8(rbp)     16(rbp)  24(rbp)
  (choose-allocptr)
  (pseudo-atomic ()
    (allocate)
    ;; Store the first 2 words, and OR in the lowtag
    (inst movdqu xmm-temp (ea 16 rbp-tn)) ; load 2 lispwords
    (inst movdqa (ea rax-tn) xmm-temp)
    (inst or :byte rax-tn other-pointer-lowtag))
  DONE
  (regs-poplist rbp rcx rdi)
  (inst ret 16) ; Clean args
  LARGE
  ;; Pretty much just like VAR-ALLOC now
  (move rdi-tn rax-tn)
  (with-registers-preserved (lisp :except (rax rcx rdi) :frame-reg nil)
    (inst lea rsi-tn (ea 16 rbp-tn)) ; 2nd C arg = pointer to header words
    (pseudo-atomic ()
      (inst call (make-fixup "vect_alloc_large" :foreign)))
    (inst mov rcx-tn rax-tn)) ; protect from FPR-restore which clobbers RAX
  (inst mov rax-tn rcx-tn) ; Restore the result
  (inst lea rsp-tn (ea -16 rbp-tn)) ; Restore RSP to a known value
  (inst jmp DONE))

(define-assembly-routine (bitmap-var-alloc (:return-style :none)) ()
  (inst push rbp-tn) (inst mov rbp-tn rsp-tn) (regs-pushlist rcx rdi)
  ;; Stack:
  ;;     saved-RDI  saved-RCX  saved-RBP  return-pc  HEADER   LOWTAG
  ;;; @  -16(rbp)   -8(rbp)    0(rbp)     8(rbp)     16(rbp)  24(rbp)
  (choose-allocptr)
  (pseudo-atomic ()
    (allocate)
    (inst mov rcx-tn (ea 16 rbp-tn)) ; load the header
    (inst mov (ea rax-tn) rcx-tn) ; write the header into the new object
    (inst or :byte rax-tn (ea 24 rbp-tn))) ; tag the pointer
  DONE
  (regs-poplist rbp rcx rdi)
  (inst ret 16) ; Clean args
  LARGE
  ;; This routine calls C and Lisp, so in theory we need only save C registers
  ;; because C's funcall would save the ones that Lisp may clobber, in order to
  ;; preserve C convention. But we in fact need to see all GPRs on the stack
  ;; in case GC requests a sync2.
  ;; So save all registers except RAX and the two already saved,
  ;; but first put RAX (the size) in the 1st C arg register
  (move rdi-tn rax-tn)
  (with-registers-preserved (lisp :except (rax rcx rdi) :frame-reg nil)
    (inst mov rsi-tn (ea 16 rbp-tn)) ; 2nd C arg = header
    (pseudo-atomic ()
      (inst call (make-fixup "var_alloc_large" :foreign))
      ;; C call returned an untagged pointer to the large object in RAX.
      (inst mov rcx-tn rax-tn) ; protect from FPR-restore which clobbers RAX
      (inst or :byte rcx-tn (ea 24 rbp-tn)))) ; tag the pointer
  (inst mov rax-tn rcx-tn) ; Restore the result
  (inst lea rsp-tn (ea -16 rbp-tn)) ; Restore RSP to a known value
  (inst jmp DONE)))))

;;; Take alloc-ptr in RDI, return a free block in RAX. Preserve all other registers.
(macrolet
    ((alignment-padding ()
       ;; after pushing the nonvolatile GPRs, subtract this much more from RSP
       ;; to preserve alignment as needed for FPR save/restore
       (* 5 n-word-bytes))
     (define-fallback (name c-fallback2)
       `(define-assembly-routine (,name) ()
          (inst push rbp-tn)
          (inst mov rbp-tn rsp-tn)
          ;; align stack to required boundary in case we're going to save/restore FPRs later
          (inst and rsp-tn -64)
           ;; save the volatile registers, preserving 64-byte stack alignment
          (regs-pushlist rcx rdx rsi r8 r9 r10 r11 rdi)
          ;; 8 registers were pushed, so the stack is correctly aligned for C call
          (inst call (make-fixup "smlgc_search_freebit" :foreign))
          (inst test rax-tn rax-tn)
          (inst jmp :nz success)
          (inst mov rdi-tn (ea rsp-tn)) ; reload the allocation-ptr
          ;; Now even though the callee-saved ("nonvolatile") regs won't be clobbered by
          ;; the C call, they must be spilled to the stack so that a root scan sees them
          ;; above the stack pointer.
          ;; There is no interrupt context to scan if GC decides to run, and in fact
          ;; if this thread suspends itself waiting for GC, then GC may perform the sync2
          ;; action (graying of roots) on behalf of the lisp thread, in which case it has
          ;; no way to obtain the register contents. (And it would need libunwind to
          ;; get the register context of each frame. Way too complicated)
          (regs-pushlist rbx r14 r15) ; 12 and 13 won't point to lisp objects
          ;; Inform the collector where the roots start.
          (inst mov rsi-tn rsp-tn)
          ;; Create the FPR save area
          (inst sub rsp-tn (+ xsave-area-size (alignment-padding)))
          (call-fpr-save/restore-routine :save)
          (inst call (make-fixup ,c-fallback2 :foreign))
          (inst mov rcx-tn rax-tn) ; save a backup of RAX since FPR-restore clobbers it
          (call-fpr-save/restore-routine :restore)
          (inst add rsp-tn (+ xsave-area-size (alignment-padding))) ; Unallocate FPR save area
          (inst mov rax-tn rcx-tn) ; restore RAX
          (regs-poplist rbx r14 r15) ; 12 and 13 won't point to lisp objects
          SUCCESS
          (regs-poplist rcx rdx rsi r8 r9 r10 r11 rdi)
          (assert-word-unused (ea rax-tn))
          (inst leave))))
  (define-fallback bitmap-alloc-fallback "sml_alloc_fallback2")
  (define-fallback bitmap-cons-fallback "sml_cons_fallback2"))

;;; This attempts to claim N consecutive cons cells at the current index
;;; based on the number of bytes desired, supplied in RCX.
;;; On success, RAX is the result and ZF is clear.
;;; On failure, RAX is 0 and ZF is set.
(symbol-macrolet ((rax rax-tn) (rcx rcx-tn))
(with-bitmap-ap (:thread thread-ap4-slot)
(define-assembly-routine (bitmap-reserve-&rest) ()
  ;; Convert RCX back to a count of conses, not a byte count.
  (inst shr :dword rcx (1+ word-shift))
  ;; Always take the slow path if the request exceeds 15 conses. The rationale is
  ;; that half the time, the next bit to be examined in the current mask is in the
  ;; upper 16 (assuming uniform distribution), which means that consecutive bits
  ;; would wrap to the next bitmap word, leaving aside the question of whether
  ;; those cells are actually available.
  (inst cmp :dword rcx 16)
  ;; If NOT profiling &REST list sizes:
  (inst jmp :ae FAIL)
  ;; If profiling &REST list sizes:
  ;(inst jmp :b SMALL)
  ;(inst inc :lock :dword (ea (cons-stats-v))) ; too big
  ;(inst jmp FAIL)
  ;SMALL
  ;(inst inc :lock :dword (ea (cons-stats-v) nil rcx 8))
  (inst mov :dword rax (freebit.mask))
  (let ((temp rdx-tn))
    (inst push temp)
    ;; Calculate the full mask of bits that we'd like to see available
    ;; as (LOGXOR (1- (ASH MASK NBITS)) (1- MASK)). This sets all bits below
    ;; and including the highest desired, and clears below the current mask.
    (inst lea :dword temp (ea -1 rax)) ; temp := (1- MASK)
    (inst shl rax :cl)   ; = (ASH MASK NBITS), won't overflow a :QWORD
    (inst dec rax)       ; = (1- (ASH MASK NBITS))
    (inst xor rax temp)
    ;; See if all the bits in RAX are clear in the allocator bitmap
    (inst mov temp (freebit.ptr))
    (inst test :dword (ea temp) rax)
    (inst pop temp)) ; done using TEMP
  (inst jmp :nz FAIL)
  ;; Verify that none of the upper 32 bits of RAX are 1. If any are,
  ;; then the :DWORD-sized TEST just performed was not valid.
  (inst shr rax 32)
  (inst jmp :nz FAIL) ; oops, RAX had some upper bit on
  ;; success
  (inst rol :dword (freebit.mask) :cl) ; update the mask
  (inst sbb :dword rax rax)
  (inst and :dword rax 4) ; = 0 or 4 depending on CF prior to SBB
  (inst add :qword (freebit.ptr) rax)
  (inst shl :dword rcx (1+ word-shift)) ; convert to byte count once again
  (inst mov :dword rax rcx)
  ;; RAX gets the free pointer, and free pointer is bumped by N bytes
  (inst xadd (freeptr) rax) ; ZF will be cleared
  (inst ret)
  FAIL
  ;; When exiting with failure, RCX is a count of elements, not bytes.
  ;; That's exactly what the fallback handler wants.
  (zeroize rax)))) ; sets ZF

;;; These are the fallback routines for a single cons at a time.
;;; Registers are unaffected except for the destination.
(macrolet ((define-cons-fallbacks (&rest regs)
             `(progn
                ,@(mapcar
                   (lambda (reg)
                     `(define-assembly-routine (,(symbolicate "BITMAP-CONS-TO-" reg "-FALLBACK")) ()
                        (count-alloc :cons1-slow)
                        ,@(unless (eq reg 'rdi) '((inst push rdi-tn)))
                        ,@(unless (eq reg 'rax) '((inst push rax-tn)))
                        (inst lea rdi-tn (thread-slot-ea thread-ap4-slot))
                        (inst call (make-fixup 'bitmap-alloc-fallback :assembly-routine))
                        ,@(unless (eq reg 'rax)
                            `((inst mov ,(symbolicate reg "-TN") rax-tn)
                              (inst pop rax-tn)))
                        ,@(unless (eq reg 'rdi) '((inst pop rdi-tn)))))
                   regs))))
  (define-cons-fallbacks rax rcx rdx rbx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))

;;; For all these, the args were pushed left-to-right.
;;; Compute the address of the highest arg, load the byte count,
;;; call BITMAP-LISTIFY, then clean the stack.
(macrolet ((call (n routine)
             `(progn
                (inst push rcx-tn)
                (inst mov rcx-tn ,n) ; how many conses to make (NOT how many args)
                (inst call (make-fixup ',routine :assembly-routine))
                (inst pop rcx-tn))))
(define-assembly-routine (bitmap-list2-fallback (:return-style :none)) () ; 2 conses
  (count-alloc :cons2-slow)
  (inst lea rax-tn (ea 16 rsp-tn))
  (call 2 bitmap-listify)
  (inst ret (* 2 n-word-bytes))) ; pop args
(define-assembly-routine (bitmap-list3-fallback (:return-style :none)) () ; 3 conses
  (count-alloc :cons3-slow)
  (inst lea rax-tn (ea 24 rsp-tn))
  (call 3 bitmap-listify)
  (inst ret (* 3 n-word-bytes))) ; pop args
(define-assembly-routine (bitmap-list4-fallback (:return-style :none)) () ; 4 conses
  (count-alloc :cons4-slow)
  (inst lea rax-tn (ea 32 rsp-tn))
  (call 4 bitmap-listify)
  (inst ret (* 4 n-word-bytes))) ; pop args
(define-assembly-routine (bitmap-list5-fallback (:return-style :none)) () ; 5 conses
  (count-alloc :cons5-slow)
  (inst lea rax-tn (ea 40 rsp-tn))
  (call 5 bitmap-listify)
  (inst ret (* 5 n-word-bytes)))

(define-assembly-routine (bitmap-list*3-fallback (:return-style :none)) () ; 2 conses
  (count-alloc :cons2-slow)
  (inst lea rax-tn (ea 24 rsp-tn))
  (call 2 bitmap-listify*)
  (inst ret (* 3 n-word-bytes))) ; pop args
(define-assembly-routine (bitmap-list*4-fallback (:return-style :none)) () ; 3 conses
  (count-alloc :cons3-slow)
  (inst lea rax-tn (ea 32 rsp-tn))
  (call 3 bitmap-listify*)
  (inst ret (* 4 n-word-bytes))) ; pop args
(define-assembly-routine (bitmap-list*5-fallback (:return-style :none)) () ; 4 conses
  (count-alloc :cons4-slow)
  (inst lea rax-tn (ea 40 rsp-tn))
  (call 4 bitmap-listify*)
  (inst ret (* 5 n-word-bytes))) ; pop args
(define-assembly-routine (bitmap-list*6-fallback (:return-style :none)) () ; 5 conses
  (count-alloc :cons5-slow)
  (inst lea rax-tn (ea 48 rsp-tn))
  (call 5 bitmap-listify*)
  (inst ret (* 6 n-word-bytes))) ; pop args
) ; end MACROLET

;;;; End of bitmap allocator trampolines

(define-assembly-routine (switch-to-arena) ()
  (inst mov rsi-tn (ea rsp-tn)) ; explicitly  pass the return PC
  ;; RSI and RDI are vop temps, so don't bother preserving them
  (with-registers-preserved (c :except (rsi rdi))
    (pseudo-atomic (:sml-check nil)
      #-system-tlabs (inst break halt-trap)
      #+system-tlabs (call-c (make-fixup "switch_to_arena" :foreign) #+win32 rdi-tn #+win32 rsi-tn))))

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
    (call-c (make-fixup "alloc" :foreign)
            (ea 16 rbp-tn)
            system-tlab-p)
    (inst mov (ea 16 rbp-tn) rax-tn))) ; result onto stack

(def-routine-pair (list-alloc-tramp) () ; CONS, ACONS, LIST, LIST*
  (with-registers-preserved (c)
    (call-c (make-fixup "alloc_list" :foreign)
            (ea 16 rbp-tn)
            system-tlab-p)
    (inst mov (ea 16 rbp-tn) rax-tn))) ; result onto stack

(def-routine-pair (listify-&rest (:return-style :none)) ()
  (with-registers-preserved (c)
    (call-c (make-fixup "listify_rest_arg" :foreign)
            (ea 16 rbp-tn)
            (ea 24 rbp-tn)
            system-tlab-p)
    (inst mov (ea 24 rbp-tn) rax-tn))   ; result
  (inst ret 8)) ; pop one argument; the unpopped word now holds the result

(def-routine-pair (make-list (:return-style :none)) ()
  (with-registers-preserved (c)
    (call-c (make-fixup "make_list" :foreign)
            (ea 16 rbp-tn)
            (ea 24 rbp-tn)
            system-tlab-p)
    (inst mov (ea 24 rbp-tn) rax-tn)) ; result
  (inst ret 8)) ; pop one argument; the unpopped word now holds the result
)

(define-assembly-routine (alloc-funinstance) ()
  (with-registers-preserved (c)
    (call-c (make-fixup "alloc_funinstance" :foreign)
            (ea 16 rbp-tn))
    (inst mov (ea 16 rbp-tn) rax-tn)))
(define-assembly-routine (set-funinstance-ref) ()
  (with-registers-preserved (c)
    (pseudo-atomic ()
      (call-c (make-fixup "set_funinstance_slot" :foreign)
              (ea 16 rbp-tn) (ea 24 rbp-tn) (ea 32 rbp-tn)))))

;;; These routines are for the deterministic consing profiler.
;;; The C support routine's argument is the return PC.
(define-assembly-routine (enable-alloc-counter) ()
  (with-registers-preserved (c)
    #+sb-thread
    (pseudo-atomic ()
      (call-c (make-fixup "allocation_tracker_counted" :foreign)
              (* (ea 8 rbp-tn))))))

(define-assembly-routine (enable-sized-alloc-counter) ()
  (with-registers-preserved (c)
    #+sb-thread
    (pseudo-atomic ()
      (call-c (make-fixup "allocation_tracker_sized" :foreign)
              (* (ea 8 rbp-tn))))))

(define-assembly-routine (undefined-tramp (:return-style :none))
    ((:temp rax descriptor-reg rax-offset))
  (inst pop (ea n-word-bytes rbp-tn))
  (emit-error-break nil cerror-trap (error-number-or-lose 'undefined-fun-error) (list rax))
  (inst push (ea n-word-bytes rbp-tn))
  (inst jmp (ea (- (* closure-fun-slot n-word-bytes) fun-pointer-lowtag) rax)))

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

;;; the closure trampoline - entered when a global function is a closure
;;; and the function is called "by name" (normally, as when it is the
;;; head of a form) via an FDEFN. Register %RAX holds the fdefn address,
;;; but the simple-fun which underlies the closure expects %RAX to be the
;;; closure itself. So we grab the closure out of the fdefn pointed to,
;;; then jump to the simple-fun that the closure points to.
;;;
;;; Immobile code uses a different strategy to call a closure that has been
;;; installed as a globally named function. The fdefn contains a jump opcode
;;; to a tiny code component specific to the particular closure.
;;; The trampoline is responsible for loading RAX, since named calls don't.
;;; However, #+immobile-code might still need CLOSURE-TRAMP for any fdefn
;;; for which the compiler chooses not to use "direct" call convention.
(define-assembly-routine
    (closure-tramp (:return-style :none))
    ()
  (loadw rax-tn rax-tn fdefn-fun-slot other-pointer-lowtag)
  (inst jmp (object-slot-ea rax-tn closure-fun-slot fun-pointer-lowtag)))

#-compact-instance-header
(define-assembly-routine
    (funcallable-instance-tramp (:return-style :none))
    ()
  (loadw rax-tn rax-tn funcallable-instance-function-slot fun-pointer-lowtag)
  (inst jmp (object-slot-ea rax-tn closure-fun-slot fun-pointer-lowtag)))

(define-assembly-routine (ensure-symbol-hash) ()
  (with-registers-preserved (lisp)
    (inst mov rdx-tn (ea 16 rbp-tn)) ; arg
    (call-static-fun 'ensure-symbol-hash 1)
    (inst mov (ea 16 rbp-tn) rdx-tn))) ; result to arg passing loc

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

;; stack: ret-pc, object, index, value-to-store
(symbol-macrolet ((object (ea 8 rsp-tn))
                  (word-index (ea 16 rsp-tn))
                  (newval (ea 24 rsp-tn))
                  ;; these are declared as vop temporaries
                  (rax rax-tn)
                  (rdx rdx-tn)
                  (rdi rdi-tn))
(define-assembly-routine (code-header-set (:return-style :none)) ()
  (inst cmp :dword (thread-slot-ea (1+ thread-ap4-slot)) -1)
  (inst jmp :e GENCGC-ALLOC)
  ;; Just tail-call GC-BARRIER-STORE after changing the 2nd arg to an EA.
  ;; (Don't bother trying to optimize out the call)
  (inst mov rdi object)
  (inst mov rdx word-index)
  (inst lea rdx (ea (- other-pointer-lowtag) rdi rdx n-word-bytes))
  (inst mov word-index rdx)
  (inst jmp (make-fixup 'gc-barrier-store :assembly-routine))
  GENCGC-ALLOC
  (pseudo-atomic (:sml-check nil)
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
      (inst mov (ea (- other-pointer-lowtag) rdi rdx n-word-bytes) rax))
  (inst ret 24))) ; remove 3 stack args

(define-assembly-routine (set-fdefn-fun (:return-style :none)) ()
  ;; Stack:
  ;;    saved-RBP  return-pc  Fdefn    Fun      RawFun
  ;;; @ 0(rbp)     8(rbp)     16(rbp)  24(rbp)  32(rbp)
  (with-registers-preserved (c)
    (inst mov c-arg1 (ea 16 rbp-tn))
    (inst mov c-arg2 (ea 24 rbp-tn))
    (inst mov c-arg3 (ea 32 rbp-tn))
    (pseudo-atomic (:sml-check nil)
     (inst call (make-fixup "set_fdefn_fun" :foreign))))
  (inst ret 24))

(define-assembly-routine (weak-pointer-ref (:return-style :none)) ()
  ;; weak pointer is on the stack
  (with-registers-preserved (c)
    (inst mov rax-tn (ea 16 rbp-tn))
    (inst lea c-arg1 (object-slot-ea rax-tn weak-pointer-value-slot other-pointer-lowtag))
    (inst call (make-fixup "weak_pointer_ref" :foreign)))
  (inst ret 8))

(define-assembly-routine (weak-vector-ref (:return-style :none)) ()
  ;; weak vector was pushed first, then the index
  (with-registers-preserved (c)
    (inst mov rax-tn (ea 24 rbp-tn))
    (inst mov rcx-tn (ea 16 rbp-tn))
    (inst lea c-arg1 (ea (- (ash vector-data-offset word-shift) other-pointer-lowtag)
                         rax-tn rcx-tn (ash 1 (- word-shift n-fixnum-tag-bits))))
    (inst call (make-fixup "weak_vector_ref" :foreign)))
  (inst ret 16))

(define-assembly-routine (weak-vector-set (:return-style :none)) ()
  ;; stack: RA Obj EA Val
  (with-registers-preserved (c)
    (inst mov c-arg1 (ea 24 rbp-tn))
    (inst mov c-arg2 (ea 32 rbp-tn))
    ;; For the non-weak store barrier we don't use the PSEUDO-ATOMIC macro,
    ;; but instead the instruction is "implicitly pseudo-atomic".
    ;; The weak barrier is not implicitly pseudo-atomic.
    (pseudo-atomic () (inst call (make-fixup "weak_vector_set" :foreign))))
  (inst ret 24))

(define-assembly-routine (gc-check) ()
  (with-registers-preserved (lisp)
    (inst mov c-arg1 rsp-tn)
    (inst call (make-fixup "lisp_gc_check" :foreign))))
) ; end PROGN

(define-assembly-routine (release-malloc-segments) ()
  (with-registers-preserved (lisp)
    (call-static-fun 'release-malloc-segments 0)))
