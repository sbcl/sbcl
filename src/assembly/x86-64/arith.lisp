;;;; simple cases for generic arithmetic

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(defun !both-fixnum-p (temp x y)
  (inst mov (reg-in-size temp :dword)
        (reg-in-size x :dword))
  (inst or (reg-in-size temp :dword)
        (reg-in-size y :dword))
  (inst test (reg-in-size temp :byte)
        fixnum-tag-mask))

(defun !some-fixnum-p (temp x y)
  (inst mov (reg-in-size temp :dword)
        (reg-in-size x :dword))
  (inst and (reg-in-size temp :dword)
        (reg-in-size y :dword))
  (inst test (reg-in-size temp :byte)
        fixnum-tag-mask))


;;;; addition, subtraction, and multiplication

(macrolet ((define-generic-arith-routine ((fun cost) &body body)
             `(define-assembly-routine (,(symbolicate "GENERIC-" fun)
                                        (:cost ,cost)
                                        (:return-style :full-call)
                                        (:translate ,fun)
                                        (:policy :safe)
                                        (:save-p t))
                ((:arg x (descriptor-reg any-reg) rdx-offset)
                 (:arg y (descriptor-reg any-reg) rdi-offset)

                 (:res res (descriptor-reg any-reg) rdx-offset)

                 (:temp rax unsigned-reg rax-offset)
                 (:temp rcx unsigned-reg rcx-offset))
                (!both-fixnum-p rax x y)
                (inst jmp :nz DO-STATIC-FUN)    ; no - do generic

                ,@body
                (inst clc) ; single-value return
                (inst ret)

                DO-STATIC-FUN
                ;; Same as: (inst enter (* n-word-bytes 1))
                (inst push rbp-tn)
                (inst mov rbp-tn rsp-tn)
                (inst sub rsp-tn (* n-word-bytes 1))
                (inst push (make-ea :qword :base rbp-tn
                            :disp (frame-byte-offset return-pc-save-offset)))
                (inst mov rcx (fixnumize 2)) ; arg count
                (inst jmp
                      (make-ea :qword
                               :disp (+ nil-value
                                        (static-fun-offset
                                         ',(symbolicate "TWO-ARG-" fun))))))))

  #.`
  (define-generic-arith-routine (+ 10)
    (move res x)
    (inst add res y)
    (inst jmp :no OKAY)
    ;; Unbox the overflowed result, recovering the correct sign from
    ;; the carry flag, then re-box as a bignum.
    (inst rcr res 1)
    ,@(when (> n-fixnum-tag-bits 1)   ; don't shift by 0
            '((inst sar res (1- n-fixnum-tag-bits))))

    (move rcx res)

    (with-fixed-allocation (res bignum-widetag (1+ bignum-digits-offset))
      (storew rcx res bignum-digits-offset other-pointer-lowtag))

    OKAY)

  #.`
  (define-generic-arith-routine (- 10)
    (move res x)
    (inst sub res y)
    (inst jmp :no OKAY)
    ;; Unbox the overflowed result, recovering the correct sign from
    ;; the carry flag, then re-box as a bignum.
    (inst cmc)                        ; carry has correct sign now
    (inst rcr res 1)
    ,@(when (> n-fixnum-tag-bits 1)   ; don't shift by 0
            '((inst sar res (1- n-fixnum-tag-bits))))

    (move rcx res)

    (with-fixed-allocation (res bignum-widetag (1+ bignum-digits-offset))
      (storew rcx res bignum-digits-offset other-pointer-lowtag))
    OKAY)

  (define-generic-arith-routine (* 30)
    (move rax x)                     ; must use eax for 64-bit result
    (inst sar rax n-fixnum-tag-bits) ; remove *8 fixnum bias
    (inst imul y)                    ; result in edx:eax
    (inst jmp :no OKAY)              ; still fixnum

    (inst shrd rax x n-fixnum-tag-bits) ; high bits from edx
    (inst sar x n-fixnum-tag-bits)      ; now shift edx too

    (move rcx x)                   ; save high bits from cqo
    (inst cqo)                     ; edx:eax <- sign-extend of eax
    (inst cmp x rcx)
    (inst jmp :e SINGLE-WORD-BIGNUM)

    (with-fixed-allocation (res bignum-widetag (+ bignum-digits-offset 2))
      (storew rax res bignum-digits-offset other-pointer-lowtag)
      (storew rcx res (1+ bignum-digits-offset) other-pointer-lowtag))
    (inst jmp DONE)

    SINGLE-WORD-BIGNUM

    (with-fixed-allocation (res bignum-widetag (1+ bignum-digits-offset))
      (storew rax res bignum-digits-offset other-pointer-lowtag))
    (inst jmp DONE)

    OKAY
    (move res rax)
    DONE))

;;;; negation

(define-assembly-routine (generic-negate
                          (:cost 10)
                          (:return-style :full-call)
                          (:policy :safe)
                          (:translate %negate)
                          (:save-p t))
                         ((:arg x (descriptor-reg any-reg) rdx-offset)
                          (:res res (descriptor-reg any-reg) rdx-offset)
                          (:temp rcx unsigned-reg rcx-offset))
  (inst test (reg-in-size x :byte) fixnum-tag-mask)
  (inst jmp :z FIXNUM)

  (inst push rbp-tn)
  (inst mov rbp-tn rsp-tn)
  (inst sub rsp-tn n-word-bytes)
  (inst push (make-ea :qword :base rbp-tn
                      :disp (frame-byte-offset return-pc-save-offset)))
  (inst mov rcx (fixnumize 1))    ; arg count
  (inst jmp (make-ea :qword
                     :disp (+ nil-value (static-fun-offset '%negate))))

  FIXNUM
  (move res x)
  (inst neg res)                        ; (- most-negative-fixnum) is BIGNUM
  (inst jmp :no OKAY)
  (inst shr res n-fixnum-tag-bits)      ; sign bit is data - remove type bits
  (move rcx res)

  (with-fixed-allocation (res bignum-widetag (1+ bignum-digits-offset))
    (storew rcx res bignum-digits-offset other-pointer-lowtag))

  OKAY)

;;;; comparison

(macrolet ((define-cond-assem-rtn (name translate static-fn test)
             `(define-assembly-routine (,name
                                        (:translate ,translate)
                                        (:policy :safe)
                                        (:save-p t)
                                        (:conditional ,test)
                                        (:cost 10)
                                        (:call-temps rcx))
                  ((:arg x (descriptor-reg any-reg) rdx-offset)
                   (:arg y (descriptor-reg any-reg) rdi-offset)

                   (:temp rcx unsigned-reg rcx-offset))

                (!both-fixnum-p rcx x y)
                (inst jmp :nz DO-STATIC-FUN)

                (inst cmp x y)
                (inst ret)

                DO-STATIC-FUN
                (inst push rbp-tn)
                (inst mov rbp-tn rsp-tn)
                (inst sub rsp-tn (* n-word-bytes 3))
                (inst mov (make-ea :qword :base rsp-tn
                                   :disp (frame-byte-offset
                                          (+ sp->fp-offset
                                             -3
                                             ocfp-save-offset)))
                      rbp-tn)
                (inst lea rbp-tn (make-ea :qword :base rsp-tn
                                          :disp (frame-byte-offset
                                          (+ sp->fp-offset
                                             -3
                                             ocfp-save-offset))))
                (inst mov rcx (fixnumize 2))
                (inst call (make-ea :qword
                                    :disp (+ nil-value
                                             (static-fun-offset ',static-fn))))
                ;; HACK: We depend on NIL having the lowest address of all
                ;; static symbols (including T)
                ,@(ecase test
                    (:l `((inst mov y (1+ nil-value))
                          (inst cmp y x)))
                    (:g `((inst cmp x (1+ nil-value)))))
                (inst pop rbp-tn))))
  (define-cond-assem-rtn generic-< < two-arg-< :l)
  (define-cond-assem-rtn generic-> > two-arg-> :g))

(define-assembly-routine (generic-eql
                          (:translate eql)
                          (:policy :safe)
                          (:save-p t)
                          (:conditional :e)
                          (:cost 10)
                          (:call-temps rcx))
                         ((:arg x (descriptor-reg any-reg) rdx-offset)
                          (:arg y (descriptor-reg any-reg) rdi-offset)

                          (:temp rcx unsigned-reg rcx-offset))

  (!some-fixnum-p rcx x y)
  (inst jmp :nz DO-STATIC-FUN)

  ;; At least one fixnum
  (inst cmp x y)
  (inst ret)

  DO-STATIC-FUN
  (inst push rbp-tn)
  (inst mov rbp-tn rsp-tn)
  (inst sub rsp-tn (* n-word-bytes 3))
  (inst mov (make-ea :qword :base rsp-tn
                     :disp (frame-byte-offset
                            (+ sp->fp-offset
                               -3
                               ocfp-save-offset)))
        rbp-tn)
  (inst lea rbp-tn (make-ea :qword :base rsp-tn
                            :disp (frame-byte-offset
                                   (+ sp->fp-offset
                                      -3
                                      ocfp-save-offset))))
  (inst mov rcx (fixnumize 2))
  (inst call (make-ea :qword
                      :disp (+ nil-value (static-fun-offset 'eql))))
  (inst cmp x (+ nil-value (static-symbol-offset t)))
  (inst pop rbp-tn))

(define-assembly-routine (generic-=
                          (:translate =)
                          (:policy :safe)
                          (:save-p t)
                          (:conditional :e)
                          (:cost 10)
                          (:call-temps rcx))
                         ((:arg x (descriptor-reg any-reg) rdx-offset)
                          (:arg y (descriptor-reg any-reg) rdi-offset)

                          (:temp rcx unsigned-reg rcx-offset))
  (!both-fixnum-p rcx x y)
  (inst jmp :nz DO-STATIC-FUN)

  ;; Both fixnums
  (inst cmp x y)
  (inst ret)

  DO-STATIC-FUN
  (inst push rbp-tn)
  (inst mov rbp-tn rsp-tn)
  (inst sub rsp-tn (* n-word-bytes 3))
  (inst mov (make-ea :qword :base rsp-tn
                     :disp (frame-byte-offset
                            (+ sp->fp-offset
                               -3
                               ocfp-save-offset)))
        rbp-tn)
  (inst lea rbp-tn (make-ea :qword :base rsp-tn
                            :disp (frame-byte-offset
                                   (+ sp->fp-offset
                                      -3
                                      ocfp-save-offset))))

  (inst mov rcx (fixnumize 2))
  (inst call (make-ea :qword
                      :disp (+ nil-value (static-fun-offset 'two-arg-=))))
  (inst cmp x (+ nil-value (static-symbol-offset t)))
  (inst pop rbp-tn))

#+sb-assembling
(define-assembly-routine (logcount)
                         ((:arg arg (descriptor-reg any-reg) rdx-offset)
                          (:temp mask unsigned-reg rcx-offset)
                          (:temp temp unsigned-reg rax-offset))
  (inst push temp) ; save RAX
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
  (inst pop temp)) ; restore RAX

(defun emit-foreign-logbitp (index foreign-symbol temp-reg) ; result in Z flag
  (declare (ignorable temp-reg))
  (multiple-value-bind (byte bit) (floor index 8)
    #!-sb-dynamic-core
    (inst test
          (make-ea :byte :disp (make-fixup foreign-symbol :foreign byte))
          (ash 1 bit))
    #!+sb-dynamic-core
    (progn
      (inst mov temp-reg
            (make-ea :qword :disp (make-fixup foreign-symbol :foreign-dataref)))
      (inst test (make-ea :byte :base temp-reg :disp byte) (ash 1 bit)))))

;; To perform logcount on small integers, we test whether to use the
;; builtin opcode, or an assembly routine. I benchmarked this against
;; an approach that always used the assembly routine via "call [addr]"
;; where the contents of the address reflected one implementation
;; or the other, chosen at startup - and this is faster.
#-sb-assembling
(macrolet
    ((def-it (name cost arg-sc arg-type)
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
         ;; Assembly routine clobbers RAX and RCX but only needs to save RAX,
         ;; as this vop clobbers RCX in the call. If changed to "CALL [ADDR]"
         ;; be sure to update the subroutine to push and pop RCX.
         (:temporary (:sc unsigned-reg :offset rcx-offset) rcx)
         (:generator ,cost
           (progn
             ;; POPCNT = ECX bit 23 = bit 7 of byte index 2
             ;; this use of 'rcx' is as the temporary register for performing
             ;; a reference to foreign data with dynamic core. It has to be
             ;; a register that conflicts with 'arg' lest we clobber it.
             (emit-foreign-logbitp 23 "cpuid_fn1_ecx" rcx)
             (inst jmp :z slow)
             ;; Intel's implementation of POPCNT on some models treats it as
             ;; a 2-operand ALU op in the manner of ADD,SUB,etc which means that
             ;; it falsely appears to need data from the destination register.
             ;; The workaround is to clear the destination.
             ;; See http://stackoverflow.com/questions/25078285
             (unless (location= result arg)
               ;; We only break the spurious dep. chain if result isn't the same
               ;; register as arg. (If they're location=, don't trash the arg!)
               (inst xor result result))
             (inst popcnt result arg)
             (inst jmp done))
         slow
           (move rdx arg)
           (inst mov rcx (make-fixup 'logcount :assembly-routine))
           (inst call rcx)
           (move result rdx)
         done))))
  (def-it unsigned-byte-64-count 14 unsigned-reg unsigned-num)
  (def-it positive-fixnum-count 13 any-reg positive-fixnum))

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
                          (:translate %eql/integer)
                          ;; :safe would imply signaling an error
                          ;; if the args are not integer, which this doesn't.
                          (:policy :fast-safe)
                          (:conditional :e)
                          (:cost 10)
                          (:call-temps rcx))
                         ((:arg x (descriptor-reg any-reg) rdx-offset)
                          (:arg y (descriptor-reg any-reg) rdi-offset)
                          (:temp rcx unsigned-reg rcx-offset)
                          (:temp rax unsigned-reg rax-offset))
  (inst cmp x y)
  (inst jmp :e done) ; Z condition flag contains the answer
  ;; check that both have other-pointer-lowtag
  (inst lea (reg-in-size rax :dword)
        (make-ea :dword :base x :disp (- other-pointer-lowtag)))
  (inst lea (reg-in-size rcx :dword)
        (make-ea :dword :base y :disp (- other-pointer-lowtag)))
  (inst or (reg-in-size rax :dword) (reg-in-size rcx :dword))
  (inst test (reg-in-size rax :byte) lowtag-mask)
  (inst jmp :ne done)
  ;; Compare the entire header word, ensuring that if at least one
  ;; argument is a bignum, then both are.
  (inst mov rcx (make-ea :qword :base x :disp (- other-pointer-lowtag)))
  (inst cmp rcx (make-ea :qword :base y :disp (- other-pointer-lowtag)))
  (inst jmp :ne done)
  (inst shr rcx n-widetag-bits)
  ;; can you have 0 payload words? probably not, but let's be safe here.
  (inst jrcxz done)
  loop
  (inst mov rax (make-ea :qword :base x :disp (- other-pointer-lowtag)
                         :index rcx :scale 8))
  (inst cmp rax (make-ea :qword :base y :disp (- other-pointer-lowtag)
                         :index rcx :scale 8))
  ;; These next 3 instructions are the equivalent of "LOOPNZ LOOP"
  ;; but had significantly better performance for me, consistent with claims
  ;; of most optimization guides saying that LOOP was deliberately pessimized
  ;; because of its use in timing-related code in the win32 kernel.
  (inst jmp :ne done)
  (inst dec rcx)
  (inst jmp :ne loop)
  ;; If the Z flag is set, the integers were EQL
  done)
