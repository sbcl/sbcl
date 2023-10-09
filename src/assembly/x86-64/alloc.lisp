;;;; allocating simple objects

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

#+sb-assembling
(symbol-macrolet ((this-cons rax-tn)
                  (ap rdi-tn)
                  (num-conses rcx-tn)
                  (curbit rdx-tn)
                  (bmwordptr rsi-tn)
                  (allocated rbx-tn) ; value loaded from *bmwordptr
                  (element r8-tn)
                  (last r9-tn)
                  (context r10-tn))
(with-bitmap-ap (ap)

;;; Take CONTEXT in RAX and count (number of cons cells) in ECX,
;;; returning a list in RAX. No other registers are affected.
;;;      return-address    <-- RSP on entry
;;;      [saved registers]  7 words
;;;      dummy cons car
;;;      dummy cons cdr    <-- RSP during execution
(defun generate-list-allocator ()
  (assemble ()
   (inst mov context rax-tn)
   ;;;(inst mov ap (thread-slot-ea thread-allocptr16-slot))
   (inst lea ap (thread-slot-ea thread-ap4-slot))
   (inst sub rsp-tn 16) ; rsp := dummy (struct cons)
   (inst lea last (ea list-pointer-lowtag rsp-tn))
   OUTER
   (inst mov bmwordptr (freebit.ptr))
   (inst mov :dword curbit (freebit.mask))
   (inst mov :dword allocated (ea bmwordptr))
   ;; if (!(allocated & curbit))
   (inst test :dword allocated curbit)
   (inst jmp :nz SLOW-PATH)
   ;; quick path: allocate at least 1 cons
   (inst mov this-cons (freeptr))
   (inst or :byte this-cons list-pointer-lowtag) ; compute tagged ptr
   INNER
   (assert-word-unused (ea (- list-pointer-lowtag) this-cons))
   (storew this-cons last cons-cdr-slot list-pointer-lowtag)
   (inst mov element (ea context))
   (inst sub context 8)
   (storew element this-cons cons-car-slot list-pointer-lowtag)
   (inst mov last this-cons)
   (inst add this-cons 16)
   (inst rol :dword curbit 1)
   (inst jmp :nc NO-CARRY)
   (inst add bmwordptr 4)
   (inst mov :dword allocated (ea bmwordptr))
   NO-CARRY
   (inst dec :dword num-conses)
   (inst jmp :z WRITEBACK) ; done
   (inst test :dword allocated curbit)
   (inst jmp :z INNER)
   WRITEBACK ; exit from fast loop, flushing cached fields of 'struct alloc_ptr'
   (inst mov (freebit.ptr) bmwordptr)
   (inst mov (freebit.mask) curbit)
   (inst sub :byte this-cons list-pointer-lowtag)
   (inst mov (freeptr) this-cons)
   (inst jrcxz TERMINATE)
   SLOW-PATH
   ;; Although this is a constructor, the optimization to elide snooping barriers
   ;; is inadmissible. The _previously_ _allocated_ cell has a store to it,
   ;; therefore this cell, being the new value, has to be remembered.
   (inst call (make-fixup 'bitmap-cons-fallback :assembly-routine))
   (assert-word-unused (ea this-cons))
   (inst mov element (ea context))
   (inst sub context 8)
   (storew element this-cons cons-car-slot 0)
   (inst or :byte this-cons list-pointer-lowtag) ; compute tagged ptr
   (storew this-cons last cons-cdr-slot list-pointer-lowtag)
   (inst mov last this-cons)
   (inst dec :dword num-conses)
   (inst jmp :nz OUTER)
   TERMINATE))

(define-assembly-routine (bitmap-listify) ()
  (regs-pushlist rbx rdx rsi rdi r8 r9 r10)
  (generate-list-allocator)
  (storew nil-value last cons-cdr-slot list-pointer-lowtag)
  (loadw rax-tn rsp-tn cons-cdr-slot 0) ; (CDR dummy)
  (inst add rsp-tn 16)
  (regs-poplist rbx rdx rsi rdi r8 r9 r10))

(define-assembly-routine (bitmap-listify*) ()
  (regs-pushlist rbx rdx rsi rdi r8 r9 r10)
  (generate-list-allocator)
  (inst mov element (ea context))
  (storew element last cons-cdr-slot list-pointer-lowtag)
  (loadw rax-tn rsp-tn cons-cdr-slot 0) ; (CDR dummy)
  (inst add rsp-tn 16)
  (regs-poplist rbx rdx rsi rdi r8 r9 r10))

;;; Take RAX = element, RCX = count and return (MAKE-LIST ELEMENT COUNT)
;;; in RAX, affecting no other registers.
;;;      return-address    <-- RSP on entry
;;;      [saved registers]  7 words
;;;      dummy cons car
;;;      dummy cons cdr    <-- RSP during execution
#+nil (define-assembly-routine (make-list-helper) ()
  (count-hit make-list-helper)
  (regs-pushlist rbx rdx rsi rdi r8 r9)
  (inst mov element rax-tn)
  (inst lea ap (thread-slot-ea thread-ap4-slot))
  (inst sub rsp-tn 16) ; rsp := dummy (struct cons)
  (inst lea last (ea list-pointer-lowtag rsp-tn))
  ;; top of outer do { } loop
  OUTER
  (inst mov bmwordptr (freebit.ptr))
  (inst mov :dword curbit (freebit.mask))
  (inst mov :dword allocated (ea bmwordptr))
  ;; if (!(allocated & curbit))
  (inst test :dword allocated curbit)
  (inst jmp :nz SLOW-PATH)
  ;; quick path: allocate at least 1 cons
  (inst mov this-cons (freeptr))
  (inst or :byte this-cons list-pointer-lowtag) ; compute tagged ptr
  ;; top of inner do {} loop
  INNER
  (assert-word-unused (ea (- list-pointer-lowtag) this-cons))
  (storew this-cons last cons-cdr-slot list-pointer-lowtag)
  (storew element this-cons cons-car-slot list-pointer-lowtag)
  (inst mov last this-cons)
  (inst add this-cons 16)
  (inst rol :dword curbit 1)
  (inst jmp :nc NO-CARRY)
  (inst add bmwordptr 4)
  (inst mov :dword allocated (ea bmwordptr))
  NO-CARRY
  (inst dec :dword num-conses)
  (inst jmp :z WRITEBACK) ; done
  (inst test :dword allocated curbit)
  (inst jmp :z INNER)
  WRITEBACK ; exit from fast loop, flushing cached fields of 'struct alloc_ptr'
  (inst mov (freebit.ptr) bmwordptr)
  (inst mov (freebit.mask) curbit)
  (inst sub :byte this-cons list-pointer-lowtag)
  (inst mov (freeptr) this-cons)
  (inst jrcxz TERMINATE)
  SLOW-PATH
  (inst call (make-fixup 'bitmap-alloc-fallback :assembly-routine))
  (assert-word-unused (ea this-cons))
  (storew element this-cons cons-car-slot 0)
  (inst or :byte this-cons list-pointer-lowtag) ; compute tagged ptr
  (storew this-cons last cons-cdr-slot list-pointer-lowtag)
  (inst mov last this-cons)
  (inst dec :dword num-conses)
  (inst jmp :nz OUTER)
  TERMINATE
  (storew nil-value last cons-cdr-slot list-pointer-lowtag)
  (loadw rax-tn rsp-tn cons-cdr-slot 0) ; (CDR dummy)
  (inst add rsp-tn 16)
  (regs-poplist rbx rdx rsi rdi r8 r9))
)) ; end SYMBOL-MACROLET

;;; Fallback routine are called after the inline code has tested 'freebit'
;;; and found it to be unavailable.
;;; All registers are preserved except for the destination.
;;; The general routine takes alloc-ptr in the destination reg.
#+sb-assembling
(macrolet
    ((gen-fallbacks ()
       `(progn
          ,@(loop
              for reg in '(rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r14 r15)
              collect
              `(define-assembly-routine
                   (,(symbolicate reg "-ALLOC-FALLBACK")
                    (:return-style :none)
                    (:export ,(symbolicate reg "-ALLOC16-FALLBACK")
                             ,(symbolicate reg "-ALLOC32-FALLBACK")
                             ,(symbolicate reg "-ALLOC64-FALLBACK")))
                   ((:temp arg/res unsigned-reg ,(symbolicate reg "-OFFSET")))
                 ENTRY
                 (count-hit ,(symbolicate reg "-ALLOC-FALLBACK"))
                 ;; Save RDI unless it is the arg/result register
                 ,@(unless (eq reg 'rdi)
                     '((inst push rdi-tn)
                       (inst mov rdi-tn arg/res)))
                 ;; Save RAX unless it is the destination register
                 ,@(unless (eq reg 'rax)
                     '((inst push rax-tn)))
                 (inst call (make-fixup 'bitmap-alloc-fallback :assembly-routine))
                 ,@(unless (eq reg 'rax) ; move the result if needed
                     `((inst mov arg/res rax-tn)
                       (inst pop rax-tn)))
                 ;; Restore RDI
                 ,@(unless (eq reg 'rdi)
                     '((inst pop rdi-tn)))
                 (inst ret)
                 ,(symbolicate reg "-ALLOC16-FALLBACK")
                 (count-hit ,(symbolicate reg "-ALLOC16-FALLBACK"))
                 (inst lea arg/res (thread-slot-ea thread-ap4-slot))
                 (inst jmp entry)
                 ,(symbolicate reg "-ALLOC32-FALLBACK")
                 (count-hit ,(symbolicate reg "-ALLOC32-FALLBACK"))
                 (inst lea arg/res (thread-slot-ea thread-ap5-slot))
                 (inst jmp entry)
                 ,(symbolicate reg "-ALLOC64-FALLBACK")
                 (count-hit ,(symbolicate reg "-ALLOC64-FALLBACK"))
                 (inst lea arg/res (thread-slot-ea thread-ap6-slot))
                 (inst jmp entry)
                 )))))
  (gen-fallbacks))

;;;; Signed and unsigned bignums from word-sized integers. Argument
;;;; and return in the same register. No VOPs, as these are only used
;;;; when called from a vop.
;;;;
;;;; ALLOC-SIGNED-BIGNUM-IN-Rxx : make a 1-digit bignum from signed-reg argument
;;;;      returning the result in the same register.
;;;;      unsigned-reg arg is OK too, provided that the top bit is 0.
;;;; ALLOC-UNSIGNED-BIGNUM-IN-Rxx : make a 1 or 2 digit bignum from unsigned-reg
;;;;      after undoing the bit rotation performed in MOVE-FROM-UNSIGNED.
;;;; TWO-WORD-BIGNUM-IN-Rxx : make a 2-digit bignum from unsigned reg
;;;;      also storing the carry flag into the high digit.
;;;; BIGNUM-TO-Rxx : choose a 1 or 2 digit bignum given [high:low] on the stack
;;;; +BIGNUM-TO-Rxx : choose a 1, 2, or 3-digit bignum given [high:low] on stack,
;;;;      ensuring that if the sign bit of the high word is on, the third digit
;;;;      is zeroized to ensure that the result is a positive bignum.
#+sb-assembling
(macrolet
    ((signed (reg)
       `(define-assembly-routine (,(symbolicate "ALLOC-SIGNED-BIGNUM-IN-" reg))
            ((:temp number unsigned-reg ,(symbolicate reg "-OFFSET")))
          (inst push number)
          (count-hit ,(symbolicate "ALLOC-SIGNED-BIGNUM-IN-" reg))
          (alloc-other bignum-widetag (+ bignum-digits-offset 1) number nil nil nil)
          (popw number bignum-digits-offset other-pointer-lowtag)))
     (unsigned (reg)
       `(define-assembly-routine (,(symbolicate "ALLOC-UNSIGNED-BIGNUM-IN-" reg))
            ((:temp number unsigned-reg ,(symbolicate reg "-OFFSET")))
          (inst ror number (1+ n-fixnum-tag-bits)) ; restore unrotated value
          (inst test number number)     ; rotates do not update SF
          (inst push number)
          (inst jmp :ns one-word-bignum)
          ;; Two word bignum
          (count-hit ,(symbolicate "ALLOC-UNSIGNED-BIGNUM2-IN-" reg))
          (alloc-other bignum-widetag (+ bignum-digits-offset 2) number nil nil nil)
          (popw number bignum-digits-offset other-pointer-lowtag)
          (inst ret)
          ONE-WORD-BIGNUM
          (count-hit ,(symbolicate "ALLOC-UNSIGNED-BIGNUM1-IN-" reg))
          (alloc-other bignum-widetag (+ bignum-digits-offset 1) number nil nil nil)
          (popw number bignum-digits-offset other-pointer-lowtag)))
     (from-digits (reg)
       ;; stack args:
       ;; +16   high-digit
       ;;  +8   low-digit
       ;; rsp : return-pc
       `(define-assembly-routine (,(symbolicate "BIGNUM-TO-" reg) (:return-style :none))
            ((:temp result unsigned-reg ,(symbolicate reg "-OFFSET")))
          (inst test :byte result result) ; is-two-digit flag
          (inst jmp :z one-word-bignum)
          (count-hit ,(symbolicate "BIGNUM2-TO-" reg))
          (alloc-other bignum-widetag (+ bignum-digits-offset 2) result nil nil nil)
          (inst movdqu float0-tn (ea 8 rsp-tn))
          (inst movdqu (ea (- (ash 1 word-shift) other-pointer-lowtag) result) float0-tn)
          (inst ret 16) ; pop args
          ONE-WORD-BIGNUM
          (count-hit ,(symbolicate "BIGNUM1-TO-" reg))
          (alloc-other bignum-widetag (+ bignum-digits-offset 1) result nil nil nil)
          (inst movq float0-tn (ea 8 rsp-tn))
          (inst movq (ea (- (ash 1 word-shift) other-pointer-lowtag) result) float0-tn)
          (inst ret 16)))
     ;; "from unsigned" might need to allocate 3 digits, but it receives only high:low
     ;; because the highest digit if needed must be all 0.
     (from-digits-unsigned (reg)
       ;; stack args:
       ;; +16   high-digit
       ;;  +8   low-digit
       ;; rsp : return-pc
       `(define-assembly-routine (,(symbolicate "+BIGNUM-TO-" reg) (:return-style :none))
            ((:temp result unsigned-reg ,(symbolicate reg "-OFFSET")))
          (count-hit ,(symbolicate "+BIGNUM-TO-" reg))
          (inst test :byte result result) ; is-two-or-three-digit flag
          (inst jmp :z one-word-bignum)
          ;; Since 2 digits and 3 digits consume the same number of bytes
          ;; due to padding, they can share the allocation request.
          (alloc-other bignum-widetag (+ bignum-digits-offset 3) result nil nil nil)
          (inst movdqu float0-tn (ea 8 rsp-tn))
          (inst movdqu (ea (- (ash 1 word-shift) other-pointer-lowtag) result) float0-tn)
          ;; don't assume prezeroed unboxed pages. (zeroize word even if 2-digit result)
          (inst mov :qword (ea (- (ash 3 word-shift) other-pointer-lowtag) result) 0)
          ;; Test sign bit of digit index 1
          (inst test :byte (ea (+ 7 (ash (+ bignum-digits-offset 1) word-shift)
                                  (- other-pointer-lowtag)) result) #xff)
          (inst jmp :s SKIP) ; if signed, then keep all 3 digits
          ;; else, no sign bit, so change it to 2-digit bignum
          (inst mov :byte (ea (- 1 other-pointer-lowtag) result) 2)
          SKIP
          (inst ret 16) ; pop args
          ONE-WORD-BIGNUM
          (alloc-other bignum-widetag (+ bignum-digits-offset 1) result nil nil nil)
          (inst movq float0-tn (ea 8 rsp-tn))
          (inst movq (ea (- (ash 1 word-shift) other-pointer-lowtag) result) float0-tn)
          (inst ret 16)))
     ;; The high bit is in the carry flag.
     (two-word-bignum (reg)
       `(define-assembly-routine (,(symbolicate "TWO-WORD-BIGNUM-TO-" reg) (:return-style :none))
            ((:temp number unsigned-reg ,(symbolicate reg "-OFFSET")))
          (inst push number)
          (inst set :c number)
          (inst movzx '(:byte :dword) number number)
          (inst push number)
          (count-hit ,(symbolicate "TWO-WORD-BIGNUM-TO-" reg))
          (alloc-other bignum-widetag (+ bignum-digits-offset 2) number nil nil nil)
          (inst pop (ea (- (ash 2 word-shift) other-pointer-lowtag) number))
          (inst pop (ea (- (ash 1 word-shift) other-pointer-lowtag) number))
          (inst ret)))
     (define (op)
       ;; R12 is not usable for the time being.
       ;; R13 is usually the thread register, but might not be
       `(progn
          ,@(loop for reg in '(rax rcx rdx rbx rsi rdi
                               r8 r9 r10 r11
                               ;; the register allocator will never select r12
                               #+gs-seg r13
                               r14 r15)
                  collect `(,op ,reg)))))
  (define from-digits)
  (define from-digits-unsigned)
  (define two-word-bignum)
  (define signed)
  (define unsigned))

#+sb-thread
(define-assembly-routine (alloc-tls-index
                          (:translate ensure-symbol-tls-index)
                          (:result-types positive-fixnum)
                          (:policy :fast-safe))
    ;; The vop result is unsigned-reg because the assembly routine does not
    ;; fixnumize its answer, which is confusing because it looks like a fixnum.
    ;; But the result of the function ENSURE-SYMBOL-TLS-INDEX is a fixnum whose
    ;; value in Lisp is the number that the assembly code computes,
    ;; *not* the fixnum whose representation it computes.
    ((:arg symbol (descriptor-reg) rax-offset) ; both input and output
     (:res result (unsigned-reg) rax-offset))
  (let* ((scratch-reg rcx-tn) ; RCX gets callee-saved, not declared as a temp
         ;; The free-index and lock are in the low and high halves of 1 qword.
         (free-tls-index-ea (static-symbol-value-ea '*free-tls-index*))
         (lock-bit 63) ; the qword's sign bit (any bit > 31 would work fine)
         (tls-full (gen-label)))
    ;; A pseudo-atomic section avoids bad behavior if the current thread were
    ;; to receive an interrupt causing it to do a slow operation between
    ;; acquisition and release of the spinlock. Preventing GC is irrelevant,
    ;; but would not be if we recycled tls indices of garbage symbols.
    (pseudo-atomic ()
     RETRY
       (inst bts :qword :lock free-tls-index-ea lock-bit)
       (inst jmp :nc got-tls-index-lock)
       (inst pause) ; spin loop hint
       ;; TODO: yielding the CPU here might be a good idea
       (inst jmp retry)
     GOT-TLS-INDEX-LOCK
       ;; Now we hold the spinlock. With it held, see if the symbol's
       ;; tls-index has been set in the meantime.
       (inst cmp :dword (tls-index-of symbol) 0)
       (inst jmp :e new-tls-index)
       ;; CMP against memory showed the tls-index to be valid, so clear the lock
       ;; and re-read the memory (safe because transition can only occur to
       ;; a nonzero value), then jump out to end the PA section.
       (inst btr :qword :lock free-tls-index-ea lock-bit)
       (inst mov :dword symbol (tls-index-of symbol))
       (inst jmp done)
     NEW-TLS-INDEX
       ;; Allocate a new tls-index.
       (inst push scratch-reg)
       (inst mov scratch-reg free-tls-index-ea)
       ;; Must ignore the semaphore bit in the register's high half.
       (inst cmp :dword scratch-reg (thread-slot-ea thread-tls-size-slot))
       (inst jmp :ae tls-full)
       ;; scratch-reg goes into symbol's TLS and into the arg/result reg.
       (inst mov :dword (tls-index-of symbol) scratch-reg)
       (inst mov :dword result scratch-reg)
       ;; Load scratch-reg with a constant that clears the lock bit
       ;; and bumps the free index in one go.
       (inst mov scratch-reg (+ (- (ash 1 lock-bit)) n-word-bytes))
       (inst add :qword :lock free-tls-index-ea scratch-reg)
       (inst pop scratch-reg)
     DONE) ; end PSEUDO-ATOMIC
    (inst ret)
    (emit-label tls-full)
    ;; The disassembly of this code looks nicer when the failure path
    ;; immediately follows the ordinary path vs. being in *ELSEWHERE*.
    (inst pop scratch-reg) ; balance the stack
    (inst btr :qword :lock free-tls-index-ea lock-bit)
    (%clear-pseudo-atomic)
    ;; There's a spurious RET instruction auto-inserted, but no matter.
    (error-call nil 'tls-exhausted-error)))
