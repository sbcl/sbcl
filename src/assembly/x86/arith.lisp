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
                ((:arg x (descriptor-reg any-reg) edx-offset)
                 (:arg y (descriptor-reg any-reg)
                       ;; this seems wrong esi-offset -- FIXME: What's it mean?
                       edi-offset)

                 (:res res (descriptor-reg any-reg) edx-offset)

                 (:temp eax unsigned-reg eax-offset)
                 (:temp ecx unsigned-reg ecx-offset))

                (inst mov ecx x)
                (inst or ecx y)
                (inst test ecx fixnum-tag-mask)  ; both fixnums?
                (inst jmp :nz DO-STATIC-FUN)     ; no - do generic

                ,@body
                (inst clc) ; single-value return
                (inst ret)

                DO-STATIC-FUN
                (inst pop eax)
                (inst push ebp-tn)
                (inst lea
                      ebp-tn
                      (make-ea :dword :base esp-tn :disp n-word-bytes))
                (inst sub esp-tn (fixnumize 2))
                (inst push eax)  ; callers return addr
                (inst mov ecx (fixnumize 2)) ; arg count
                (inst jmp
                      (make-ea :dword
                               :disp (+ nil-value
                                        (static-fun-offset
                                         ',(symbolicate "TWO-ARG-" fun))))))))

  (define-generic-arith-routine (+ 10)
    (move res x)
    (inst add res y)
    (inst jmp :no OKAY)
    (inst rcr res 1)                  ; carry has correct sign
    (inst sar res 1)                  ; remove type bits

    (move ecx res)

    (with-fixed-allocation (res bignum-widetag (1+ bignum-digits-offset))
      (storew ecx res bignum-digits-offset other-pointer-lowtag))

    OKAY)

  (define-generic-arith-routine (- 10)
    (move res x)
    (inst sub res y)
    (inst jmp :no OKAY)
    (inst cmc)                        ; carry has correct sign now
    (inst rcr res 1)
    (inst sar res 1)                  ; remove type bits

    (move ecx res)

    (with-fixed-allocation (res bignum-widetag (1+ bignum-digits-offset))
      (storew ecx res bignum-digits-offset other-pointer-lowtag))
    OKAY)

  (define-generic-arith-routine (* 30)
    (move eax x)                          ; must use eax for 64-bit result
    (inst sar eax n-fixnum-tag-bits)      ; remove *4 fixnum bias
    (inst imul y)                         ; result in edx:eax
    (inst jmp :no okay)                   ; still fixnum

    ;; zzz jrd changed edx to ebx in here, as edx isn't listed as a temp, above
    ;;     pfw says that loses big -- edx is target for arg x and result res
    ;;     note that 'edx' is not defined -- using x
    (inst shrd eax x n-fixnum-tag-bits)    ; high bits from edx
    (inst sar x n-fixnum-tag-bits)         ; now shift edx too

    (move ecx x)                           ; save high bits from cdq
    (inst cdq)                             ; edx:eax <- sign-extend of eax
    (inst cmp x ecx)
    (inst jmp :e SINGLE-WORD-BIGNUM)

    (with-fixed-allocation (res bignum-widetag (+ bignum-digits-offset 2))
      (storew eax res bignum-digits-offset other-pointer-lowtag)
      (storew ecx res (1+ bignum-digits-offset) other-pointer-lowtag))
    (inst jmp DONE)

    SINGLE-WORD-BIGNUM

    (with-fixed-allocation (res bignum-widetag (1+ bignum-digits-offset))
      (storew eax res bignum-digits-offset other-pointer-lowtag))
    (inst jmp DONE)

    OKAY
    (move res eax)
    DONE))

;;;; negation

(define-assembly-routine (generic-negate
                          (:cost 10)
                          (:return-style :full-call)
                          (:policy :safe)
                          (:translate %negate)
                          (:save-p t))
                         ((:arg x (descriptor-reg any-reg) edx-offset)
                          (:res res (descriptor-reg any-reg) edx-offset)

                          (:temp eax unsigned-reg eax-offset)
                          (:temp ecx unsigned-reg ecx-offset))
  (inst test x fixnum-tag-mask)
  (inst jmp :z FIXNUM)

  (inst pop eax)
  (inst push ebp-tn)
  (inst lea ebp-tn (make-ea :dword :base esp-tn :disp n-word-bytes))
  (inst sub esp-tn (fixnumize 2))
  (inst push eax)
  (inst mov ecx (fixnumize 1))    ; arg count
  (inst jmp (make-ea :dword
                     :disp (+ nil-value (static-fun-offset '%negate))))

  FIXNUM
  (move res x)
  (inst neg res)                        ; (- most-negative-fixnum) is BIGNUM
  (inst jmp :no OKAY)
  (inst shr res n-fixnum-tag-bits)      ; sign bit is data - remove type bits
  (move ecx res)

  (with-fixed-allocation (res bignum-widetag (1+ bignum-digits-offset))
    (storew ecx res bignum-digits-offset other-pointer-lowtag))

  OKAY)

;;;; comparison

(macrolet ((define-cond-assem-rtn (name translate static-fn test)
             `(define-assembly-routine (,name
                                        (:cost 10)
                                        (:return-style :full-call)
                                        (:policy :safe)
                                        (:translate ,translate)
                                        (:save-p t))
                ((:arg x (descriptor-reg any-reg) edx-offset)
                 (:arg y (descriptor-reg any-reg) edi-offset)

                 (:res res descriptor-reg edx-offset)

                 (:temp eax unsigned-reg eax-offset)
                 (:temp ecx unsigned-reg ecx-offset))

                (inst mov ecx x)
                (inst or ecx y)
                (inst test ecx fixnum-tag-mask)
                (inst jmp :nz DO-STATIC-FUN)  ; are both fixnums?

                (inst cmp x y)
                (cond ((member :cmov *backend-subfeatures*)
                       (load-symbol res t)
                       (inst mov eax nil-value)
                       (inst cmov ,test res eax))
                      (t
                       (inst mov res nil-value)
                       (inst jmp ,test RETURN)
                       (load-symbol res t)))
                RETURN
                (inst clc)     ; single-value return
                (inst ret)

                DO-STATIC-FUN
                (inst pop eax)
                (inst push ebp-tn)
                (inst lea ebp-tn (make-ea :dword
                                          :base esp-tn
                                          :disp n-word-bytes))
                (inst sub esp-tn (fixnumize 2)) ; FIXME: Push 2 words on stack,
                                                ; weirdly?
                (inst push eax)
                (inst mov ecx (fixnumize 2)) ; FIXME: FIXNUMIZE and
                                        ; SINGLE-FLOAT-BITS are parallel,
                                        ; should be named parallelly.
                (inst jmp (make-ea :dword
                                   :disp (+ nil-value
                                            (static-fun-offset ',static-fn)))))))

  (define-cond-assem-rtn generic-< < two-arg-< :ge)
  (define-cond-assem-rtn generic-> > two-arg-> :le))

(define-assembly-routine (generic-eql
                          (:cost 10)
                          (:return-style :full-call)
                          (:policy :safe)
                          (:translate eql)
                          (:save-p t))
                         ((:arg x (descriptor-reg any-reg) edx-offset)
                          (:arg y (descriptor-reg any-reg) edi-offset)

                          (:res res descriptor-reg edx-offset)

                          (:temp eax unsigned-reg eax-offset)
                          (:temp ecx unsigned-reg ecx-offset))
  (inst mov ecx x)
  (inst and ecx y)
  (inst test ecx fixnum-tag-mask)
  (inst jmp :nz DO-STATIC-FUN)

  ;; At least one fixnum
  (inst cmp x y)
  (load-symbol res t)
  (cond ((member :cmov *backend-subfeatures*)
         (inst mov eax nil-value)
         (inst cmov :ne res eax))
        (t
         (inst jmp :e RETURN)
         (inst mov res nil-value)))
  RETURN
  (inst clc)
  (inst ret)

  ;; FIXME: We could handle all non-numbers here easily enough: go to
  ;; TWO-ARG-EQL only if lowtags and widetags match, lowtag is
  ;; other-pointer-lowtag and widetag is < code-header-widetag.
  DO-STATIC-FUN
  (inst pop eax)
  (inst push ebp-tn)
  (inst lea ebp-tn (make-ea :dword :base esp-tn :disp n-word-bytes))
  (inst sub esp-tn (fixnumize 2))
  (inst push eax)
  (inst mov ecx (fixnumize 2))
  (inst jmp (make-ea :dword
                     :disp (+ nil-value (static-fun-offset 'eql)))))

(define-assembly-routine (generic-=
                          (:cost 10)
                          (:return-style :full-call)
                          (:policy :safe)
                          (:translate =)
                          (:save-p t))
                         ((:arg x (descriptor-reg any-reg) edx-offset)
                          (:arg y (descriptor-reg any-reg) edi-offset)

                          (:res res descriptor-reg edx-offset)

                          (:temp eax unsigned-reg eax-offset)
                          (:temp ecx unsigned-reg ecx-offset))
  (inst mov ecx x)
  (inst or ecx y)
  (inst test ecx fixnum-tag-mask)        ; both fixnums?
  (inst jmp :nz DO-STATIC-FUN)

  (inst cmp x y)
  (load-symbol res t)
  (cond ((member :cmov *backend-subfeatures*)
         (inst mov eax nil-value)
         (inst cmov :ne res eax))
        (t
         (inst jmp :e RETURN)
         (inst mov res nil-value)))
  RETURN
  (inst clc)
  (inst ret)

  DO-STATIC-FUN
  (inst pop eax)
  (inst push ebp-tn)
  (inst lea ebp-tn (make-ea :dword :base esp-tn :disp n-word-bytes))
  (inst sub esp-tn (fixnumize 2))
  (inst push eax)
  (inst mov ecx (fixnumize 2))
  (inst jmp (make-ea :dword
                     :disp (+ nil-value (static-fun-offset 'two-arg-=)))))


;;; Support for the Mersenne Twister, MT19937, random number generator
;;; due to Matsumoto and Nishimura.
;;;
;;; Makoto Matsumoto and T. Nishimura, "Mersenne twister: A
;;; 623-dimensionally equidistributed uniform pseudorandom number
;;; generator.", ACM Transactions on Modeling and Computer Simulation,
;;; 1997, to appear.
;;;
;;; State:
;;;  0-1:   Constant matrix A. [0, #x9908b0df] (not used here)
;;;  2:     Index; init. to 1.
;;;  3-626: State.

;;; This assembly routine is called from the inline VOP and updates
;;; the state vector with new random numbers. The state vector is
;;; passed in the EAX register.
#+sb-assembling ; We don't want a vop for this one.
(define-assembly-routine
    (random-mt19937-update)
    ((:temp state unsigned-reg eax-offset)
     (:temp k unsigned-reg ebx-offset)
     (:temp y unsigned-reg ecx-offset)
     (:temp tmp unsigned-reg edx-offset))

  ;; Save the temporary registers.
  (inst push k)
  (inst push y)
  (inst push tmp)

  ;; Generate a new set of results.
  (inst xor k k)
  LOOP1
  (inst mov y (make-ea-for-vector-data state :index k :offset 3))
  (inst mov tmp (make-ea-for-vector-data state :index k :offset (+ 1 3)))
  (inst and y #x80000000)
  (inst and tmp #x7fffffff)
  (inst or y tmp)
  (inst shr y 1)
  (inst jmp :nc skip1)
  (inst xor y #x9908b0df)
  SKIP1
  (inst xor y (make-ea-for-vector-data state :index k :offset (+ 397 3)))
  (inst mov (make-ea-for-vector-data state :index k :offset 3) y)
  (inst inc k)
  (inst cmp k (- 624 397))
  (inst jmp :b loop1)
  LOOP2
  (inst mov y (make-ea-for-vector-data state :index k :offset 3))
  (inst mov tmp (make-ea-for-vector-data state :index k :offset (+ 1 3)))
  (inst and y #x80000000)
  (inst and tmp #x7fffffff)
  (inst or y tmp)
  (inst shr y 1)
  (inst jmp :nc skip2)
  (inst xor y #x9908b0df)
  SKIP2
  (inst xor y (make-ea-for-vector-data state :index k :offset (+ (- 397 624) 3)))
  (inst mov (make-ea-for-vector-data state :index k :offset 3) y)
  (inst inc k)
  (inst cmp k (- 624 1))
  (inst jmp :b loop2)

  (inst mov y (make-ea-for-vector-data state :offset (+ (- 624 1) 3)))
  (inst mov tmp (make-ea-for-vector-data state :offset (+ 0 3)))
  (inst and y #x80000000)
  (inst and tmp #x7fffffff)
  (inst or y tmp)
  (inst shr y 1)
  (inst jmp :nc skip3)
  (inst xor y #x9908b0df)
  SKIP3
  (inst xor y (make-ea-for-vector-data state :offset (+ (- 397 1) 3)))
  (inst mov (make-ea-for-vector-data state :offset (+ (- 624 1) 3)) y)

  ;; Restore the temporary registers and return.
  (inst pop tmp)
  (inst pop y)
  (inst pop k)
  (inst ret))
