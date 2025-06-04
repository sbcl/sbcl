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
    ((alloc-other (&rest rest)
       `(emit-alloc-other nil thread-tn ,@rest))
     (signed (reg)
       `(define-assembly-routine (,(symbolicate "ALLOC-SIGNED-BIGNUM-IN-" reg))
            ((:temp number unsigned-reg ,(symbolicate reg "-OFFSET")))
          (inst push number)
          (alloc-other bignum-widetag (+ bignum-digits-offset 1) number)
          (popw number bignum-digits-offset other-pointer-lowtag)))
     (unsigned (reg)
       `(define-assembly-routine (,(symbolicate "ALLOC-UNSIGNED-BIGNUM-IN-" reg))
            ((:temp number unsigned-reg ,(symbolicate reg "-OFFSET")))
          (inst ror number (1+ n-fixnum-tag-bits)) ; restore unrotated value
          (inst test number number)     ; rotates do not update SF
          (inst push number)
          (inst jmp :ns one-word-bignum)
          ;; Two word bignum
          (alloc-other bignum-widetag (+ bignum-digits-offset 2) number)
          (popw number bignum-digits-offset other-pointer-lowtag)
          (inst ret)
          ONE-WORD-BIGNUM
          (alloc-other bignum-widetag (+ bignum-digits-offset 1) number)
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
          (alloc-other bignum-widetag (+ bignum-digits-offset 2) result)
          (inst movdqu float0-tn (ea 8 rsp-tn))
          (inst movdqu (object-slot-ea result 1 other-pointer-lowtag) float0-tn)
          (inst ret 16) ; pop args
          ONE-WORD-BIGNUM
          (alloc-other bignum-widetag (+ bignum-digits-offset 1) result)
          (inst movq float0-tn (ea 8 rsp-tn))
          (inst movq (object-slot-ea result 1 other-pointer-lowtag) float0-tn)
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
          (inst test :byte result result) ; is-two-or-three-digit flag
          (inst jmp :z one-word-bignum)
          ;; Since 2 digits and 3 digits consume the same number of bytes
          ;; due to padding, they can share the allocation request.
          (alloc-other bignum-widetag (+ bignum-digits-offset 3) result)
          (inst movdqu float0-tn (ea 8 rsp-tn))
          (inst movdqu (object-slot-ea result 1 other-pointer-lowtag) float0-tn)
          ;; don't assume prezeroed unboxed pages. (zeroize word even if 2-digit result)
          (inst mov :qword (object-slot-ea result 3 other-pointer-lowtag) 0)
          ;; Test sign bit of digit index 1
          (inst test :byte (ea (+ 7 (ash (+ bignum-digits-offset 1) word-shift)
                                  (- other-pointer-lowtag)) result) #xff)
          (inst jmp :s SKIP) ; if signed, then keep all 3 digits
          ;; else, no sign bit, so change it to 2-digit bignum
          (inst mov :byte (ea (- 1 other-pointer-lowtag) result) 2)
          SKIP
          (inst ret 16) ; pop args
          ONE-WORD-BIGNUM
          (alloc-other bignum-widetag (+ bignum-digits-offset 1) result)
          (inst movq float0-tn (ea 8 rsp-tn))
          (inst movq (object-slot-ea result 1 other-pointer-lowtag) float0-tn)
          (inst ret 16)))
     ;; The high bit is in the carry flag.
     (two-word-bignum (reg)
       `(define-assembly-routine (,(symbolicate "TWO-WORD-BIGNUM-TO-" reg) (:return-style :none))
            ((:temp number unsigned-reg ,(symbolicate reg "-OFFSET")))
          (inst push number)
          (inst set :c number)
          (inst movzx '(:byte :dword) number number)
          (inst push number)
          (alloc-other bignum-widetag (+ bignum-digits-offset 2) number)
          (inst pop (object-slot-ea number 2 other-pointer-lowtag))
          (inst pop (object-slot-ea number 1 other-pointer-lowtag))
          (inst ret)))
     (define (op)
       ;; R13 is usually the thread register, but might not be
       `(progn
        ,@(loop for reg in ; can't cons into card-table or thread register
                (remove (intern (aref +qword-register-names+ card-table-reg))
                        '(rax rcx rdx rbx rsi rdi r8 r9 r10 r11 r12 #+gs-seg r13 r14 r15))
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
