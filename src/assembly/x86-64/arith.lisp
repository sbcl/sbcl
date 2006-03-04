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

;;;; addition, subtraction, and multiplication

(macrolet ((define-generic-arith-routine ((fun cost) &body body)
             `(define-assembly-routine (,(symbolicate "GENERIC-" fun)
                                        (:cost ,cost)
                                        (:return-style :full-call)
                                        (:translate ,fun)
                                        (:policy :safe)
                                        (:save-p t))
                ((:arg x (descriptor-reg any-reg) rdx-offset)
                 (:arg y (descriptor-reg any-reg)
                       ;; this seems wrong esi-offset -- FIXME: What's it mean?
                       rdi-offset)

                 (:res res (descriptor-reg any-reg) rdx-offset)

                 (:temp rax unsigned-reg rax-offset)
                 (:temp rbx unsigned-reg rbx-offset)
                 (:temp rcx unsigned-reg rcx-offset))

                (declare (ignorable rbx))

                (inst test x 7)  ; fixnum?
                (inst jmp :nz DO-STATIC-FUN) ; no - do generic
                (inst test y 7)  ; fixnum?
                (inst jmp :z DO-BODY)   ; yes - doit here

                DO-STATIC-FUN
                (inst pop rax)
                (inst push rbp-tn)
                (inst lea
                      rbp-tn
                      (make-ea :qword :base rsp-tn :disp n-word-bytes))
                (inst sub rsp-tn (fixnumize 2))
                (inst push rax)  ; callers return addr
                (inst mov rcx (fixnumize 2)) ; arg count
                (inst jmp
                      (make-ea :qword
                               :disp (+ nil-value
                                        (static-fun-offset
                                         ',(symbolicate "TWO-ARG-" fun)))))

                DO-BODY
                ,@body)))

  (define-generic-arith-routine (+ 10)
    (move res x)
    (inst add res y)
    (inst jmp :no OKAY)
    (inst rcr res 1)                  ; carry has correct sign
    (inst sar res 2)                  ; remove type bits

    (move rcx res)

    (with-fixed-allocation (res bignum-widetag (1+ bignum-digits-offset))
      (storew rcx res bignum-digits-offset other-pointer-lowtag))

    OKAY)

  (define-generic-arith-routine (- 10)
    (move res x)
    (inst sub res y)
    (inst jmp :no OKAY)
    (inst cmc)                        ; carry has correct sign now
    (inst rcr res 1)
    (inst sar res 2)                  ; remove type bits

    (move rcx res)

    (with-fixed-allocation (res bignum-widetag (1+ bignum-digits-offset))
      (storew rcx res bignum-digits-offset other-pointer-lowtag))
    OKAY)

  (define-generic-arith-routine (* 30)
    (move rax x)                   ; must use eax for 64-bit result
    (inst sar rax 3)               ; remove *4 fixnum bias
    (inst imul y)                  ; result in edx:eax
    (inst jmp :no OKAY)            ; still fixnum

    ;; zzz jrd changed edx to ebx in here, as edx isn't listed as a temp, above
    ;;     pfw says that loses big -- edx is target for arg x and result res
    ;;     note that 'edx' is not defined -- using x
    (inst shrd rax x 3)            ; high bits from edx
    (inst sar x 3)                 ; now shift edx too

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

                          (:temp rax unsigned-reg rax-offset)
                          (:temp rcx unsigned-reg rcx-offset))
  (inst test x 7)
  (inst jmp :z FIXNUM)

  (inst pop rax)
  (inst push rbp-tn)
  (inst lea rbp-tn (make-ea :qword :base rsp-tn :disp n-word-bytes))
  (inst sub rsp-tn (fixnumize 2))
  (inst push rax)
  (inst mov rcx (fixnumize 1))    ; arg count
  (inst jmp (make-ea :qword
                     :disp (+ nil-value (static-fun-offset '%negate))))

  FIXNUM
  (move res x)
  (inst neg res)                        ; (- most-negative-fixnum) is BIGNUM
  (inst jmp :no OKAY)
  (inst shr res 3)                    ; sign bit is data - remove type bits
  (move rcx res)

  (with-fixed-allocation (res bignum-widetag (1+ bignum-digits-offset))
    (storew rcx res bignum-digits-offset other-pointer-lowtag))

  OKAY)

;;;; comparison

(macrolet ((define-cond-assem-rtn (name translate static-fn test)
             `(define-assembly-routine (,name
                                        (:cost 10)
                                        (:return-style :full-call)
                                        (:policy :safe)
                                        (:translate ,translate)
                                        (:save-p t))
                ((:arg x (descriptor-reg any-reg) rdx-offset)
                 (:arg y (descriptor-reg any-reg) rdi-offset)

                 (:res res descriptor-reg rdx-offset)

                 (:temp eax unsigned-reg rax-offset)
                 (:temp ecx unsigned-reg rcx-offset))

                ;; KLUDGE: The "3" here is a mask for the bits which will be
                ;; zero in a fixnum. It should have a symbolic name. (Actually,
                ;; it might already have a symbolic name which the coder
                ;; couldn't be bothered to use..) -- WHN 19990917
                (inst test x 7)
                (inst jmp :nz TAIL-CALL-TO-STATIC-FN)
                (inst test y 7)
                (inst jmp :z INLINE-FIXNUM-COMPARE)

                TAIL-CALL-TO-STATIC-FN
                (inst pop eax)
                (inst push rbp-tn)
                (inst lea rbp-tn (make-ea :qword
                                          :base rsp-tn
                                          :disp n-word-bytes))
                (inst sub rsp-tn (fixnumize 2)) ; FIXME: Push 2 words on stack,
                                                ; weirdly?
                (inst push eax)
                (inst mov ecx (fixnumize 2)) ; FIXME: FIXNUMIZE and
                                        ; SINGLE-FLOAT-BITS are parallel,
                                        ; should be named parallelly.
                (inst jmp (make-ea :qword
                                   :disp (+ nil-value
                                            (static-fun-offset ',static-fn))))

                INLINE-FIXNUM-COMPARE
                (inst cmp x y)
                (inst mov res nil-value)
                (inst jmp ,test RETURN-FALSE)
                RETURN-TRUE
                (load-symbol res t)
                RETURN-FALSE
                DONE)))

  (define-cond-assem-rtn generic-< < two-arg-< :ge)
  (define-cond-assem-rtn generic-> > two-arg-> :le))

(define-assembly-routine (generic-eql
                          (:cost 10)
                          (:return-style :full-call)
                          (:policy :safe)
                          (:translate eql)
                          (:save-p t))
                         ((:arg x (descriptor-reg any-reg) rdx-offset)
                          (:arg y (descriptor-reg any-reg) rdi-offset)

                          (:res res descriptor-reg rdx-offset)

                          (:temp eax unsigned-reg rax-offset)
                          (:temp ecx unsigned-reg rcx-offset))
  (inst cmp x y)
  (inst jmp :e RETURN-T)
  (inst test x 7)
  (inst jmp :z RETURN-NIL)
  (inst test y 7)
  (inst jmp :nz DO-STATIC-FN)

  RETURN-NIL
  (inst mov res nil-value)
  (inst jmp DONE)

  DO-STATIC-FN
  (inst pop eax)
  (inst push rbp-tn)
  (inst lea rbp-tn (make-ea :qword :base rsp-tn :disp n-word-bytes))
  (inst sub rsp-tn (fixnumize 2))
  (inst push eax)
  (inst mov ecx (fixnumize 2))
  (inst jmp (make-ea :qword
                     :disp (+ nil-value (static-fun-offset 'eql))))

  RETURN-T
  (load-symbol res t)
  DONE)

(define-assembly-routine (generic-=
                          (:cost 10)
                          (:return-style :full-call)
                          (:policy :safe)
                          (:translate =)
                          (:save-p t))
                         ((:arg x (descriptor-reg any-reg) rdx-offset)
                          (:arg y (descriptor-reg any-reg) rdi-offset)

                          (:res res descriptor-reg rdx-offset)

                          (:temp eax unsigned-reg rax-offset)
                          (:temp ecx unsigned-reg rcx-offset)
                          )
  (inst test x 7)                      ; descriptor?
  (inst jmp :nz DO-STATIC-FN)          ; yes, do it here
  (inst test y 7)                      ; descriptor?
  (inst jmp :nz DO-STATIC-FN)
  (inst cmp x y)
  (inst jmp :e RETURN-T)                ; ok

  (inst mov res nil-value)
  (inst jmp DONE)

  DO-STATIC-FN
  (inst pop eax)
  (inst push rbp-tn)
  (inst lea rbp-tn (make-ea :qword :base rsp-tn :disp n-word-bytes))
  (inst sub rsp-tn (fixnumize 2))
  (inst push eax)
  (inst mov ecx (fixnumize 2))
  (inst jmp (make-ea :qword
                     :disp (+ nil-value (static-fun-offset 'two-arg-=))))

  RETURN-T
  (load-symbol res t)
  DONE)


