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
;;;; as out-of-line versions: MOVE-FROM-[UN]SIGNED VOPs handle the
;;;; fixnum cases inline.

;;; #+SB-ASSEMBLING as we don't need VOPS, just the asm routines:
;;; these are out-of-line versions called by VOPs.

#+sb-assembling
(macrolet ((def (reg)
             (let ((tn (symbolicate reg "-TN")))
               `(define-assembly-routine (,(symbolicate "ALLOC-SIGNED-BIGNUM-IN-" reg)) ()
                  (inst push ,tn)
                  (alloc-other ,tn bignum-widetag (+ bignum-digits-offset 1) nil)
                  (popw ,tn bignum-digits-offset other-pointer-lowtag)))))
  (def eax)
  (def ebx)
  (def ecx)
  (def edx)
  (def edi)
  (def esi))

#+sb-assembling
(macrolet ((def (reg)
             (let ((tn (symbolicate reg "-TN")))
               `(define-assembly-routine (,(symbolicate "ALLOC-UNSIGNED-BIGNUM-IN-" reg)) ()
                  (inst push ,tn)
                  ;; Sign flag is set by the caller! Note: The inline
                  ;; version always allocates space for two words, but
                  ;; here we minimize garbage.
                  (inst jmp :ns one-word-bignum)
                  ;; Two word bignum
                  (alloc-other ,tn bignum-widetag (+ bignum-digits-offset 2) nil)
                  (popw ,tn bignum-digits-offset other-pointer-lowtag)
                  (inst ret)
                  ONE-WORD-BIGNUM
                  (alloc-other ,tn bignum-widetag (+ bignum-digits-offset 1) nil)
                  (popw ,tn bignum-digits-offset other-pointer-lowtag)))))
  (def eax)
  (def ebx)
  (def ecx)
  (def edx)
  (def edi)
  (def esi))

#+sb-assembling
(defun frob-allocation-assembly-routine (obj lowtag arg-tn)
  `(define-assembly-routine (,(intern (format nil "ALLOCATE-~A-TO-~A" obj arg-tn)))
     ((:temp ,arg-tn descriptor-reg ,(intern (format nil "~A-OFFSET" arg-tn))))
     (pseudo-atomic ()
      (allocation ,arg-tn (pad-data-block ,(intern (format nil "~A-SIZE" obj))) nil)
      (inst lea ,arg-tn (make-ea :byte :base ,arg-tn :disp ,lowtag)))))

#+sb-assembling
(macrolet ((frob-cons-routines ()
             (let ((routines nil))
               (dolist (tn-offset *dword-regs*
                        `(progn ,@routines))
                 (push (frob-allocation-assembly-routine 'cons
                                                         list-pointer-lowtag
                                                         (intern (aref +dword-register-names+ tn-offset)))
                       routines)))))
  (frob-cons-routines))

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
    ((:arg symbol (descriptor-reg) eax-offset) ; both input and output
     (:res result (unsigned-reg) eax-offset))
  (let ((scratch-reg ecx-tn) ; ECX gets callee-saved, not declared as a temp
        (free-tls-index-ea (make-ea-for-symbol-value *free-tls-index*))
        (lock-bit 31) ; sign bit
        (tls-full (gen-label)))
    ;; A pseudo-atomic section avoids bad behavior if the current thread were
    ;; to receive an interrupt causing it to do a slow operation between
    ;; acquisition and release of the spinlock. Preventing GC is irrelevant,
    ;; but would not be if we recycled tls indices of garbage symbols.
    (pseudo-atomic ()
     (assemble () ; for conversion of tagbody-like labels to assembler labels
     RETRY
       (inst bts free-tls-index-ea lock-bit :lock)
       (inst jmp :nc got-tls-index-lock)
       (inst pause) ; spin loop hint
       ;; TODO: yielding the CPU here might be a good idea
       (inst jmp retry)
     GOT-TLS-INDEX-LOCK
       ;; Now we hold the spinlock. With it held, see if the symbol's
       ;; tls-index has been set in the meantime.
       (inst cmp (tls-index-of symbol) 0)
       (inst jmp :e new-tls-index)
       ;; TLS index is valid, so use it.
       (inst and (make-ea :byte :disp (+ (ea-disp free-tls-index-ea) 3)) #x7F
             :lock) ; set the spinlock bit to 0
       (inst jmp done)
     NEW-TLS-INDEX
       ;; Allocate a new tls-index.
       (inst push scratch-reg)
       (inst mov scratch-reg free-tls-index-ea)
       (inst and scratch-reg #x7FFFFFFF) ; mask off the sign
       #+win32
       (progn ; need to use another register
         (inst push edx-tn)
         (inst mov edx-tn (make-fixup "dynamic_values_bytes" :foreign-dataref))
         (inst cmp scratch-reg (make-ea :dword :base edx-tn))
         (inst pop edx-tn))
       #-win32 (inst cmp scratch-reg
                     (make-ea :dword :disp (ash thread-tls-size-slot word-shift)) :fs)
       (inst jmp :ae tls-full)
       ;; Assign the tls-index into the symbol
       (inst mov (tls-index-of symbol) scratch-reg)
       ;; Bump the free index and clear the lock.
       (inst add scratch-reg n-word-bytes)
       (inst mov free-tls-index-ea scratch-reg)
       (inst pop scratch-reg)
     DONE
       (inst mov result (tls-index-of symbol)))) ; end PSEUDO-ATOMIC
    (inst ret)
    (emit-label tls-full)
    (inst mov free-tls-index-ea scratch-reg) ; unlock
    (inst pop scratch-reg) ; balance the stack
    (%clear-pseudo-atomic)
    ;; There's a spurious RET instruction auto-inserted, but no matter.
    (error-call nil 'tls-exhausted-error)))
