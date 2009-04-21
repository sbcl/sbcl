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
                 (:temp rcx unsigned-reg rcx-offset))

                (inst mov rcx x)
                (inst or rcx y)
                (inst test rcx fixnum-tag-mask) ; both fixnums?
                (inst jmp :nz DO-STATIC-FUN)    ; no - do generic

                ,@body
                (inst clc) ; single-value return
                (inst ret)

                DO-STATIC-FUN
                ;; Same as: (inst enter (fixnumize 1))
                (inst push rbp-tn)
                (inst mov rbp-tn rsp-tn)
                (inst sub rsp-tn (fixnumize 1))
                (inst push (make-ea :qword :base rbp-tn
                            :disp (frame-byte-offset return-pc-save-offset)))
                (inst mov rcx (fixnumize 2)) ; arg count
                (inst jmp
                      (make-ea :qword
                               :disp (+ nil-value
                                        (static-fun-offset
                                         ',(symbolicate "TWO-ARG-" fun))))))))

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

                          (:temp rax unsigned-reg rax-offset)
                          (:temp rcx unsigned-reg rcx-offset))
  (inst test x fixnum-tag-mask)
  (inst jmp :z FIXNUM)

  (inst push rbp-tn)
  (inst mov rbp-tn rsp-tn)
  (inst sub rsp-tn (fixnumize 1))
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
               (declare (ignorable translate static-fn))
             #+sb-assembling
             `(define-assembly-routine (,name
                                        (:return-style :none))
                  ((:arg x (descriptor-reg any-reg) rdx-offset)
                   (:arg y (descriptor-reg any-reg) rdi-offset)

                   (:temp rcx unsigned-reg rcx-offset))

                (inst mov rcx x)
                (inst or rcx y)
                (inst test rcx fixnum-tag-mask)
                (inst jmp :nz DO-STATIC-FUN)  ; are both fixnums?

                (inst cmp x y)
                (inst ret)

                DO-STATIC-FUN
                (inst sub rsp-tn (fixnumize 3))
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
                (inst ret))
             #-sb-assembling
             `(define-vop (,name)
                (:translate ,translate)
                (:policy :safe)
                (:save-p t)
                (:args (x :scs (descriptor-reg any-reg) :target rdx)
                       (y :scs (descriptor-reg any-reg) :target rdi))

                (:temporary (:sc unsigned-reg :offset rdx-offset
                                 :from (:argument 0))
                            rdx)
                (:temporary (:sc unsigned-reg :offset rdi-offset
                                 :from (:argument 1))
                            rdi)

                (:temporary (:sc unsigned-reg :offset rcx-offset
                                 :from :eval)
                            rcx)
                (:conditional ,test)
                (:generator 10
                   (move rdx x)
                   (move rdi y)
                   (inst lea rcx (make-ea :qword
                                          :disp (make-fixup ',name :assembly-routine)))
                   (inst call rcx)))))

  (define-cond-assem-rtn generic-< < two-arg-< :l)
  (define-cond-assem-rtn generic-> > two-arg-> :g))

#+sb-assembling
(define-assembly-routine (generic-eql
                          (:return-style :none))
                         ((:arg x (descriptor-reg any-reg) rdx-offset)
                          (:arg y (descriptor-reg any-reg) rdi-offset)

                          (:temp rcx unsigned-reg rcx-offset))

  (inst mov rcx x)
  (inst and rcx y)
  (inst test rcx fixnum-tag-mask)
  (inst jmp :nz DO-STATIC-FUN)

  ;; At least one fixnum
  (inst cmp x y)
  (inst ret)

  DO-STATIC-FUN
  (inst sub rsp-tn (fixnumize 3))
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
  (load-symbol y t)
  (inst cmp x y)
  (inst ret))

#-sb-assembling
(define-vop (generic-eql)
  (:translate eql)
  (:policy :safe)
  (:save-p t)
  (:args (x :scs (descriptor-reg any-reg) :target rdx)
         (y :scs (descriptor-reg any-reg) :target rdi))

  (:temporary (:sc unsigned-reg :offset rdx-offset
               :from (:argument 0))
              rdx)
  (:temporary (:sc unsigned-reg :offset rdi-offset
               :from (:argument 1))
              rdi)

  (:temporary (:sc unsigned-reg :offset rcx-offset
               :from :eval)
              rcx)
  (:conditional :e)
  (:generator 10
    (move rdx x)
    (move rdi y)
    (inst lea rcx (make-ea :qword
                           :disp (make-fixup 'generic-eql :assembly-routine)))
    (inst call rcx)))

#+sb-assembling
(define-assembly-routine (generic-=
                          (:return-style :none))
                         ((:arg x (descriptor-reg any-reg) rdx-offset)
                          (:arg y (descriptor-reg any-reg) rdi-offset)

                          (:temp rcx unsigned-reg rcx-offset))
  (inst mov rcx x)
  (inst or rcx y)
  (inst test rcx fixnum-tag-mask)
  (inst jmp :nz DO-STATIC-FUN)

  ;; Both fixnums
  (inst cmp x y)
  (inst ret)

  DO-STATIC-FUN
  (inst sub rsp-tn (fixnumize 3))
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
  (load-symbol y t)
  (inst cmp x y)
  (inst ret))

#-sb-assembling
(define-vop (generic-=)
  (:translate =)
  (:policy :safe)
  (:save-p t)
  (:args (x :scs (descriptor-reg any-reg) :target rdx)
         (y :scs (descriptor-reg any-reg) :target rdi))

  (:temporary (:sc unsigned-reg :offset rdx-offset
               :from (:argument 0))
              rdx)
  (:temporary (:sc unsigned-reg :offset rdi-offset
               :from (:argument 1))
              rdi)

  (:temporary (:sc unsigned-reg :offset rcx-offset
               :from :eval)
              rcx)
  (:conditional :e)
  (:generator 10
    (move rdx x)
    (move rdi y)
    (inst lea rcx (make-ea :qword
                           :disp (make-fixup 'generic-= :assembly-routine)))
    (inst call rcx)))
