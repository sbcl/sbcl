;;;; simple cases for generic arithmetic

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

#-sb-assembling ; avoid redefinition warning
(macrolet ((static-fun-addr (name)
             #+immobile-code `(rip-relative-ea (make-fixup ,name :linkage-cell))
             #-immobile-code
             `(progn
                (inst mov rax-tn (thread-slot-ea sb-vm::thread-linkage-table-slot))
                (ea (make-fixup ,name :linkage-cell) rax-tn))))
(defun both-fixnum-p (temp x y)
  (inst mov :dword temp x)
  (inst or :dword temp y)
  (inst test :byte temp fixnum-tag-mask))

(defun some-fixnum-p (temp x y)
  (inst mov :dword temp x)
  (inst and :dword temp y)
  (inst test :byte temp fixnum-tag-mask))

(defun call-static-fun (fun arg-count)
  (inst push rbp-tn)
  (inst mov rbp-tn rsp-tn)
  (inst sub rsp-tn (* n-word-bytes 2))
  (inst mov (ea rsp-tn) rbp-tn)
  (inst mov rbp-tn rsp-tn)
  (inst mov rcx-tn (fixnumize arg-count))
  (inst call (static-fun-addr fun))
  (inst pop rbp-tn))

(defun tail-call-static-fun (fun arg-count)
  (inst push rbp-tn)
  (inst mov rbp-tn rsp-tn)
  (inst sub rsp-tn n-word-bytes)
  (inst push (ea (frame-byte-offset return-pc-save-offset) rbp-tn))
  (inst mov rcx-tn (fixnumize arg-count))
  (inst jmp (static-fun-addr fun))))


;;;; addition, subtraction, and multiplication

#+sb-assembling
(defun return-single-word-bignum (dest alloc-tn source)
  (let ((header (logior (ash 1 n-widetag-bits) bignum-widetag))
        (nbytes #+bignum-assertions 32 #-bignum-assertions 16))
    (instrument-alloc bignum-widetag nbytes nil alloc-tn)
    (pseudo-atomic ()
      (allocation bignum-widetag nbytes 0 alloc-tn nil nil nil)
      (storew* header alloc-tn 0 0 t)
      (storew source alloc-tn bignum-digits-offset 0)
      (if (eq dest alloc-tn)
          (inst or :byte dest other-pointer-lowtag)
          (inst lea dest (ea other-pointer-lowtag alloc-tn))))))

(macrolet ((define-generic-arith-routine ((fun cost) &body body)
             `(define-assembly-routine (,(symbolicate "GENERIC-" fun)
                                        (:cost ,cost)
                                        (:return-style :full-call-no-return)
                                        (:translate ,fun)
                                        (:policy :safe)
                                        (:save-p t))
                ((:arg x (descriptor-reg any-reg) rdx-offset)
                 (:arg y (descriptor-reg any-reg) rdi-offset)

                 (:res res (descriptor-reg any-reg) rdx-offset)

                 ;; + and - can make do with only 1 temp.
                 ;; RCX is always needed for lisp call.
                 ,@(if (eq fun '*)
                       '((:temp rax unsigned-reg rax-offset)))
                 (:temp rcx unsigned-reg rcx-offset))
                ;; AL encodes in one byte less than CL
                (both-fixnum-p ,(if (eq fun '*) 'rax 'rcx) x y)
                (inst jmp :nz DO-STATIC-FUN)    ; no - do generic

                ,@body
                (inst clc) ; single-value return
                (inst ret)

                DO-STATIC-FUN
                (tail-call-static-fun ',(symbolicate "TWO-ARG-" fun) 2))))

  (define-generic-arith-routine (+ 10)
    (move res x)
    (inst add res y)
    (inst jmp :o BIGNUM)
    (inst clc) (inst ret)
    BIGNUM
    ;; Unbox the overflowed result, recovering the correct sign from
    ;; the carry flag, then re-box as a bignum.
    (inst rcr res 1)
    (when (> n-fixnum-tag-bits 1)   ; don't shift by 0
      (inst sar res (1- n-fixnum-tag-bits)))
    (return-single-word-bignum res rcx res))

  (define-generic-arith-routine (- 10)
    (move res x)
    (inst sub res y)
    (inst jmp :o BIGNUM)
    (inst clc) (inst ret)
    BIGNUM
    ;; Unbox the overflowed result, recovering the correct sign from
    ;; the carry flag, then re-box as a bignum.
    (inst cmc)                        ; carry has correct sign now
    (inst rcr res 1)
    (when (> n-fixnum-tag-bits 1)   ; don't shift by 0
      (inst sar res (1- n-fixnum-tag-bits)))
    (return-single-word-bignum res rcx res))

  (define-generic-arith-routine (* 30)
    (move rax x)                     ; must use eax for 64-bit result
    (inst sar rax n-fixnum-tag-bits) ; remove *8 fixnum bias
    (inst imul y)                    ; result in edx:eax
    (inst jmp :o BIGNUM)
    (move res rax)
    (inst clc) (inst ret)

    BIGNUM
    (inst shrd rax x n-fixnum-tag-bits) ; high bits from edx
    (inst sar x n-fixnum-tag-bits)      ; now shift edx too

    (move rcx x)                   ; save high bits from cqo
    (inst cqo)                     ; edx:eax <- sign-extend of eax
    (inst cmp x rcx)
    (inst jmp :e SINGLE-WORD-BIGNUM)

    (alloc-other bignum-widetag (+ bignum-digits-offset 2) res nil nil nil)
    (storew rax res bignum-digits-offset other-pointer-lowtag)
    (storew rcx res (1+ bignum-digits-offset) other-pointer-lowtag)
    (inst clc) (inst ret)

    SINGLE-WORD-BIGNUM
    (return-single-word-bignum res res rax)))

;;;; negation

(define-assembly-routine (generic-negate
                          (:cost 10)
                          (:return-style :full-call-no-return)
                          (:policy :safe)
                          (:translate %negate)
                          (:save-p t))
                         ((:arg x (descriptor-reg any-reg) rdx-offset)
                          (:res res (descriptor-reg any-reg) rdx-offset))
  (inst test :byte x fixnum-tag-mask)
  (inst jmp :nz GENERIC)
  (move res x)
  (inst neg res)                        ; (- most-negative-fixnum) is BIGNUM
  ;; This constant isn't really a fixup, but it's easiest for me to think about it
  ;; that way for now. It should really do whatever EMIT-EA does for a CONSTANT.
  (inst cmov :o res (ea (make-fixup nil :code-object
                                    (+ (ash code-constants-offset word-shift)
                                       (- other-pointer-lowtag)))
                        rip-tn))
  (inst clc) (inst ret)
  GENERIC
  (tail-call-static-fun '%negate 1))

;;;; comparison

(macrolet ((define-cond-assem-rtn (name translate static-fn test)
             `(define-assembly-routine (,name
                                        (:translate ,translate)
                                        (:policy :safe)
                                        (:save-p t)
                                        (:conditional ,test)
                                        (:cost 10))
                  ((:arg x (descriptor-reg any-reg) rdx-offset)
                   (:arg y (descriptor-reg any-reg) rdi-offset)

                   (:temp rcx unsigned-reg rcx-offset))

                (both-fixnum-p rcx x y)
                (inst jmp :nz DO-STATIC-FUN)

                (inst cmp x y)
                (inst ret)

                DO-STATIC-FUN
                (call-static-fun ',static-fn 2)
                ;; HACK: We depend on NIL having the lowest address of all
                ;; static symbols (including T)
                ,@(ecase test
                    (:l `((inst mov y (1+ nil-value))
                          (inst cmp y x)))
                    (:g `((inst cmp x (1+ nil-value))))))))
  (define-cond-assem-rtn generic-< < two-arg-< :l)
  (define-cond-assem-rtn generic-> > two-arg-> :g))

(define-assembly-routine (generic-=
                          (:translate =)
                          (:policy :safe)
                          (:save-p t)
                          (:conditional :e)
                          (:cost 10))
                         ((:arg x (descriptor-reg any-reg) rdx-offset)
                          (:arg y (descriptor-reg any-reg) rdi-offset)

                          (:temp rcx unsigned-reg rcx-offset))
  (both-fixnum-p rcx x y)
  (inst jmp :nz DO-STATIC-FUN)

  ;; Both fixnums
  (inst cmp x y)
  (inst ret)

  DO-STATIC-FUN
  (call-static-fun 'two-arg-= 2)
  (inst cmp x (+ nil-value (static-symbol-offset t))))

#+sb-assembling
(define-assembly-routine (logcount)
                         ((:arg arg (descriptor-reg any-reg) rdx-offset)
                          (:temp mask unsigned-reg rcx-offset)
                          (:temp temp unsigned-reg rax-offset))
  (inst push temp)
  (inst push mask)

  (let ((result arg))
    ;; See the comments below for how the algorithm works. The tricks
    ;; used can be found for example in AMD's software optimization
    ;; guide or at "http://www.hackersdelight.org/HDcode/pop.cc" in the
    ;; function "pop1", for 32-bit words. The extension to 64 bits is
    ;; straightforward.
    ;; Calculate 2-bit sums. Note that the value of a two-digit binary
    ;; number is the sum of the right digit and twice the left digit.
    ;; Thus we can calculate the sum of the two digits by shifting the
    ;; left digit to the right position and doing a two-bit subtraction.
    ;; This subtraction will never create a borrow and thus can be made
    ;; on all 32 2-digit numbers at once.
    (move temp arg)
    (inst shr result 1)
    (inst mov mask #x5555555555555555)
    (inst and result mask)
    (inst sub temp result)
    ;; Calculate 4-bit sums by straightforward shift, mask and add.
    ;; Note that we shift the source operand of the MOV and not its
    ;; destination so that the SHR and the MOV can execute in the same
    ;; clock cycle.
    (inst mov result temp)
    (inst shr temp 2)
    (inst mov mask #x3333333333333333)
    (inst and result mask)
    (inst and temp mask)
    (inst add result temp)
    ;; Calculate 8-bit sums. Since each sum is at most 8, which fits
    ;; into 4 bits, we can apply the mask after the addition, saving one
    ;; instruction.
    (inst mov temp result)
    (inst shr result 4)
    (inst add result temp)
    (inst mov mask #x0f0f0f0f0f0f0f0f)
    (inst and result mask)
    ;; Add all 8 bytes at once by multiplying with #256r11111111.
    ;; We need to calculate only the lower 8 bytes of the product.
    ;; Of these the most significant byte contains the final result.
    ;; Note that there can be no overflow from one byte to the next
    ;; as the sum is at most 64 which needs only 7 bits.
    (inst mov mask #x0101010101010101)
    (inst imul result mask)
    (inst shr result 56))
  (inst pop mask)
  (inst pop temp))

;; To perform logcount on small integers, we test whether to use the
;; builtin opcode, or an assembly routine. I benchmarked this against
;; an approach that always used the assembly routine via "call [addr]"
;; where the contents of the address reflected one implementation
;; or the other, chosen at startup - and this is faster.
#-sb-assembling
(macrolet
    ((def-it (name cost arg-sc arg-type &key signed)
      `(define-vop (,name)
         (:translate logcount)
         (:note ,(format nil "inline ~a logcount" arg-type))
         (:policy :fast-safe)
         (:args (arg :scs (,arg-sc)))
         (:arg-types ,arg-type)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         ;; input/output of assembly routine
         (:temporary (:sc unsigned-reg :offset rdx-offset
                          :from (:argument 0) :to (:result 0)) rdx)
         (:vop-var vop)
         (:generator ,cost
           ,@(when signed
               `((move rdx arg)
                 (setf arg rdx)
                 ,(if (eq arg-type 'fixnum)
                      `(inst sar rdx n-fixnum-tag-bits)
                      `(inst test rdx rdx))
                 (inst jmp :ge POSITIVE)
                 (inst not rdx)
                 POSITIVE))
           (when (memq :popcnt *backend-subfeatures*) ; always use POPCNT
           ;; Intel's implementation of POPCNT on some models treats it as
           ;; a 2-operand ALU op in the manner of ADD,SUB,etc which means that
           ;; it falsely appears to need data from the destination register.
           ;; The workaround is to clear the destination.
           ;; See http://stackoverflow.com/questions/25078285
             (unless (location= result arg)
             ;; We only break the spurious dep. chain if result isn't the same
             ;; register as arg. (If they're location=, don't trash the arg!)
               (inst xor :dword result result))
             (inst popcnt result arg)
             (return-from ,name))

           ;; Conditionally use POPCNT
           (test-cpu-feature cpu-has-popcnt)
           (inst jmp :z slow)
           (unless (location= result arg)
             (inst xor :dword result result))
           (inst popcnt result arg)
           (inst jmp done)
         slow
           (move rdx arg)
           (invoke-asm-routine 'call 'logcount vop)
           (move result rdx)
         done))))
  (def-it unsigned-byte-64-count 14 unsigned-reg unsigned-num)
  (def-it signed-byte-64-count 15 signed-reg signed-num :signed t)
  (def-it positive-fixnum-count 12 any-reg positive-fixnum)
  (def-it signed-fixnum-count 13 any-reg fixnum :signed t))

;;; General case of EQL

(defconstant cplx-dfloat-real-offs (+ (ash complex-double-float-real-slot word-shift)
                                   (- other-pointer-lowtag)))
(defconstant cplx-dfloat-imag-offs (+ (ash complex-double-float-imag-slot word-shift)
                                   (- other-pointer-lowtag)))

#+sb-assembling ; the vop lives in compiler/x86-64/pred (for historical reasons)
(progn
;;; KLUDGE: The ASSEMBLE macro can't pick assembler statement labels out of
;;; a LET body, so having a global variable for the eql-dispatch table seems like
;;; the lesser evil, allowing use of the convenient syntax afforded by ASSEMBLE
;;; to label statements. The alternative, a lexical var, would require
;;; the GEN-LABEL/EMIT-LABEL pattern everywhere below.
;;; EMIT-ASSEMBLE could accept an initial LET form, but ultimately ASSEMBLE would
;;; still have to understand that the binding occurs after it calls GEN-LABEL
;;; for all implicit labels. In source code it would look something like:
;;;   (define-assembly-routine ... ((:temp ...)) (:let ((c (register-inline-constant ...)))))
;;; It just doesn't seem worth the effort to do all that.
(defparameter eql-dispatch nil)
(define-assembly-routine (generic-eql (:return-style :none)
                                      (:export eql-ratio))
    ((:temp rcx unsigned-reg rcx-offset)  ; callee-saved
     (:temp rax unsigned-reg rax-offset)  ; vop temps
     (:temp rsi unsigned-reg rsi-offset)
     (:temp rdi unsigned-reg rdi-offset)
     (:temp r11 unsigned-reg r11-offset))
  ;; SINGLE-FLOAT is included in this table because its widetag within the range
  ;; of accepted widetags in the precondition for calling this routine.
  ;; Technically it would be an error to see an other-pointer object with that tag.
  ;; Would it be right for EQL to fail and report heap corruption? Maybe.
  ;; Or, better, would it be possible on 64-bit platforms to give single-float a
  ;; widetag that is numerically less than bignum?
  ;; We're obliged to special-case single-float in NUMBERP etc anyway.
  (setq eql-dispatch
        (register-inline-constant :jump-table
                                  `(,eql-bignum
                                    ,eql-ratio
                                    ,eql-single-float
                                    ,eql-double-float
                                    ,eql-complex-rational
                                    ,eql-complex-single
                                    ,eql-complex-double)))

  ;; widetags are spaced 4 apart, and we've subtracted BIGNUM_WIDETAG,
  ;; so scaling by 2 gets a multiple of 8 with bignum at offset 0 etc.
  ;; But the upper 3 bytes of EAX hold junk presently, so clear them.
  (inst and :dword rax #x7f)
  (inst lea r11 eql-dispatch)
  (inst jmp (ea r11 rax 2))

  EQL-SINGLE-FLOAT       ; should not get here
  (inst xor :dword rax rax)
  FLIP-ZF-AND-RETURN
  (inst xor :byte rax 1) ;  ZF <- 0, i.e. not EQL
  (inst ret)

  ;; for either of these two types, do a bitwise comparison of 8 bytes
  (inst .align 4 :long-nop)
  EQL-DOUBLE-FLOAT
  EQL-COMPLEX-SINGLE
  (inst mov rax (ea -7 rsi))
  (inst cmp rax (ea -7 rdi))
  (inst ret) ; if ZF set then EQL

  (inst .align 4 :long-nop)
  EQL-COMPLEX-DOUBLE
  ;;; This could be done as one XMM register comparison.
  ;;; It's just as well to use GPRs though since the vop
  ;;; does not specify any xmm register temps.
  (inst mov rax (ea cplx-dfloat-real-offs rsi))
  (inst xor rax (ea cplx-dfloat-real-offs rdi))
  ;; rsi,rdi are clobberable as they are vop :temps, not :args
  (inst mov rsi (ea cplx-dfloat-imag-offs rsi))
  (inst xor rsi (ea cplx-dfloat-imag-offs rdi))
  (inst or rsi rax)
  (inst ret) ; if ZF set then realpart and imagpart were each EQL

  (inst .align 4 :long-nop)
  EQL-RATIO
  (inst push rsi)
  (inst push rdi)
  (loadw rdi rdi ratio-numerator-slot other-pointer-lowtag)
  (loadw rsi rsi ratio-numerator-slot other-pointer-lowtag)
  (inst call eql-integer)
  (inst pop rdi) ; restore stack before jne
  (inst pop rsi)
  (inst jmp :ne done)
  (loadw rdi rdi ratio-denominator-slot other-pointer-lowtag)
  (loadw rsi rsi ratio-denominator-slot other-pointer-lowtag)
  ;; fall into eql-integer
  EQL-INTEGER
  (inst cmp rdi rsi)
  (inst jmp :e done)
  ;; both have to be non-fixnum to be EQL now. To test for that,
  ;; it suffices to check that the AND has a 1 in the low bit.
  (inst mov :dword rax rdi)
  (inst and :dword rax rsi)
  (inst and :byte rax 1)
  ;; %al is 1 if they are both bignums, or 0 if not. 0 is failure (*not* EQL)
  ;; so we have to do an operation that inverts the Z flag.
  (inst jmp :e flip-ZF-and-return)
  EQL-BIGNUM
  (inst mov rax (ea (- other-pointer-lowtag) rdi))
  (inst xor rax (ea (- other-pointer-lowtag) rsi))
  (inst jmp :ne done) ; not equal
  ;; Preserve rcx and compute the length in words.
  ;; MAXIMUM-BIGNUM-LENGTH is a 4-byte quantity. Probably better if this would use
  ;; the high 4 bytes of the header to naturally align the load.
  (inst push rcx)
  (inst mov :dword rcx (ea (- 1 other-pointer-lowtag) rdi))
  #+bignum-assertions
  (progn (inst mov r11 rcx)
         (inst or r11 1)) ; align to start of ubsan bits
  compare-loop
  #+bignum-assertions
  (let ((ok1 (gen-label)) (ok2 (gen-label)))
    (inst dec rcx)
    (inst bt (ea (- n-word-bytes other-pointer-lowtag) rsi r11 8) rcx)
    (inst jmp :nc ok1)
    (inst break halt-trap)
    (emit-label ok1)
    (inst bt (ea (- n-word-bytes other-pointer-lowtag) rdi r11 8) rcx)
    (inst jmp :nc ok2)
    (inst break halt-trap)
    (emit-label ok2)
    (inst inc rcx))
  (inst mov rax (ea (- other-pointer-lowtag) rsi rcx 8))
  (inst cmp rax (ea (- other-pointer-lowtag) rdi rcx 8))
  (inst jmp :ne epilogue)
  (inst dec rcx)
  (inst jmp :ne compare-loop) ; exiting with ZF set means we win
  epilogue
  (inst pop rcx)
  done
  (inst ret)

  (inst .align 4 :long-nop)
  EQL-COMPLEX-RATIONAL
  (inst push rsi)
  (inst push rdi)
  (loadw rdi rdi complex-real-slot other-pointer-lowtag)
  (loadw rsi rsi complex-real-slot other-pointer-lowtag)
  (inst call eql-rational)
  (inst pop rdi) ; restore stack balance prior to possibly returning
  (inst pop rsi)
  (inst jmp :ne done)
  (loadw rdi rdi complex-imag-slot other-pointer-lowtag)
  (loadw rsi rsi complex-imag-slot other-pointer-lowtag)
  ;; fall into eql-rational
  EQL-RATIONAL
  ;; First pick off fixnums like in eql_integer
  (inst cmp rdi rsi)
  (inst jmp :e done)
  (inst mov :dword rax rdi)
  (inst and :dword rax rsi)
  (inst and :dword rax 1)
  ;; %eax is 1 if they are both other-pointer objects, 0 if not
  (inst jmp :e flip-ZF-and-return)
  (inst mov :byte rax (ea (- other-pointer-lowtag) rsi))
  (inst cmp :byte rax (ea (- other-pointer-lowtag) rdi))
  (inst jmp :ne done)
  ;; widetags matched, so they are both bignums or both ratios
  (inst lea r11 eql-dispatch)
  (inst jmp (ea (* -2 bignum-widetag) r11 rax 2))))

;;; EQL for integers that are either fixnum or bignum

;; The restriction on use of this assembly routine can't be expressed a
;; constraints on vop args: it may be called when at *least* one arg
;; is known to be an integer; the other can be anything.
;
;; Logic: we succeed quickly in the EQ case when possible.
;; Otherwise, check if both are OTHER-POINTER objects, failing if not.
;; Given that at least one is an integer, and both are OTHER-POINTERs,
;; then if their widetags match, both are BIGNUMs to be compared word-for-word.
;;
;; If you call this with two other-pointer objects with
;; the same widetag, but not bignum-widetag, the behavior is undefined.
;;
(define-assembly-routine (%eql/integer
                          (:translate eql)
                          ;; :safe would imply signaling an error
                          ;; if the args are not integer, which this doesn't.
                          (:policy :fast-safe)
                          (:conditional :e)
                          (:cost 10)
                          (:arg-types (:or integer bignum) *))
                         ((:arg x (descriptor-reg) rdx-offset)
                          (:arg y (descriptor-reg any-reg) rdi-offset)
                          (:temp rcx unsigned-reg rcx-offset)
                          (:temp rax unsigned-reg rax-offset))
  (inst cmp x y)
  (inst jmp :e done) ; Z condition flag contains the answer
  ;; check that both have other-pointer-lowtag
  (inst lea :dword rax (ea (- other-pointer-lowtag) x))
  (inst lea :dword rcx (ea (- other-pointer-lowtag) y))
  (inst or :dword rax rcx)
  (inst test :byte rax lowtag-mask)
  (inst jmp :ne done)
  ;; Compare the entire header word, ensuring that if at least one
  ;; argument is a bignum, then both are.
  (inst mov rcx (ea (- other-pointer-lowtag) x))
  (inst cmp rcx (ea (- other-pointer-lowtag) y))
  (inst jmp :ne done)
  (inst shr rcx n-widetag-bits)
  loop
  (inst mov rax (ea (- other-pointer-lowtag) x rcx 8))
  (inst cmp rax (ea (- other-pointer-lowtag) y rcx 8))
  ;; These next 3 instructions are the equivalent of "LOOPNZ LOOP"
  ;; but had significantly better performance for me, consistent with claims
  ;; of most optimization guides saying that LOOP was deliberately pessimized
  ;; because of its use in timing-related code in the win32 kernel.
  (inst jmp :ne done)
  (inst dec rcx)
  (inst jmp :ne loop)
  ;; If the Z flag is set, the integers were EQL
  done)

#-sb-assembling
(define-vop (%eql/integer2 %eql/integer)
  (:args (x :scs (descriptor-reg any-reg) :target x-arg-temp)
         (y :scs (descriptor-reg) :target y-arg-temp))
  (:arg-types * (:or integer bignum)))
