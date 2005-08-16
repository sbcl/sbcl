(in-package "SB!VM")



;;;; Addition and subtraction.

;;; static-fun-offset returns the address of the raw_addr slot of
;;; a static function's fdefn.

;;; Note that there is only one use of static-fun-offset outside this
;;; file (in genesis.lisp)

(define-assembly-routine (generic-+
                          (:cost 10)
                          (:return-style :full-call)
                          (:translate +)
                          (:policy :safe)
                          (:save-p t))
                         ((:arg x (descriptor-reg any-reg) a0-offset)
                          (:arg y (descriptor-reg any-reg) a1-offset)

                          (:res res (descriptor-reg any-reg) a0-offset)

                          (:temp temp non-descriptor-reg nl0-offset)
                          (:temp temp1 non-descriptor-reg nl1-offset)
                          (:temp temp2 non-descriptor-reg nl2-offset)
                          (:temp pa-flag non-descriptor-reg nl4-offset)
                          (:temp lip interior-reg lip-offset)
                          (:temp nargs any-reg nargs-offset)
                          (:temp ocfp any-reg ocfp-offset))
  (inst or temp x y)
  (inst and temp fixnum-tag-mask)
  (inst beq temp DO-ADD)
  (inst sra temp1 x n-fixnum-tag-bits)

  ;; DO-STATIC-FUN
  (inst lw lip null-tn (static-fun-offset 'two-arg-+))
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (inst j lip)
  (move cfp-tn csp-tn t)

  DO-ADD
  (inst sra temp2 y n-fixnum-tag-bits)
  (inst addu temp temp1 temp2)
  ;; check for overflow
  (inst sra temp1 temp (- n-word-bits n-lowtag-bits))
  (inst beq temp1 RETURN)
  (inst nor temp1 temp1)
  (inst beq temp1 RETURN)
  (inst nop)
  (with-fixed-allocation (res pa-flag temp2 bignum-widetag (1+ bignum-digits-offset))
    (storew temp res bignum-digits-offset other-pointer-lowtag))
  (inst b DONE)
  (inst nop)

  RETURN
  (inst sll res temp n-fixnum-tag-bits)

  DONE)


(define-assembly-routine (generic--
                          (:cost 10)
                          (:return-style :full-call)
                          (:translate -)
                          (:policy :safe)
                          (:save-p t))
                         ((:arg x (descriptor-reg any-reg) a0-offset)
                          (:arg y (descriptor-reg any-reg) a1-offset)

                          (:res res (descriptor-reg any-reg) a0-offset)

                          (:temp temp non-descriptor-reg nl0-offset)
                          (:temp temp1 non-descriptor-reg nl1-offset)
                          (:temp temp2 non-descriptor-reg nl2-offset)
                          (:temp pa-flag non-descriptor-reg nl4-offset)
                          (:temp lip interior-reg lip-offset)
                          (:temp nargs any-reg nargs-offset)
                          (:temp ocfp any-reg ocfp-offset))
  (inst or temp x y)
  (inst and temp fixnum-tag-mask)
  (inst beq temp DO-SUB)
  (inst sra temp1 x n-fixnum-tag-bits)

  ;; DO-STATIC-FUN
  (inst lw lip null-tn (static-fun-offset 'two-arg--))
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (inst j lip)
  (move cfp-tn csp-tn t)

  DO-SUB
  (inst sra temp2 y n-fixnum-tag-bits)
  (inst subu temp temp1 temp2)
  ;; check for overflow
  (inst sra temp1 temp (- n-word-bits n-lowtag-bits))
  (inst beq temp1 RETURN)
  (inst nor temp1 temp1)
  (inst beq temp1 RETURN)
  (inst nop)
  (with-fixed-allocation (res pa-flag temp2 bignum-widetag (1+ bignum-digits-offset))
    (storew temp res bignum-digits-offset other-pointer-lowtag))
  (inst b DONE)
  (inst nop)

  RETURN
  (inst sll res temp n-fixnum-tag-bits)

  DONE)



;;;; Multiplication


(define-assembly-routine (generic-*
                          (:cost 25)
                          (:return-style :full-call)
                          (:translate *)
                          (:policy :safe)
                          (:save-p t))
                         ((:arg x (descriptor-reg any-reg) a0-offset)
                          (:arg y (descriptor-reg any-reg) a1-offset)

                          (:res res (descriptor-reg any-reg) a0-offset)

                          (:temp temp non-descriptor-reg nl0-offset)
                          (:temp lo non-descriptor-reg nl1-offset)
                          (:temp hi non-descriptor-reg nl2-offset)
                          (:temp pa-flag non-descriptor-reg nl4-offset)
                          (:temp lip interior-reg lip-offset)
                          (:temp nargs any-reg nargs-offset)
                          (:temp ocfp any-reg ocfp-offset))
  ;; If either arg is not a fixnum, call the static function.
  (inst or temp x y)
  (inst and temp fixnum-tag-mask)
  (inst bne temp DO-STATIC-FUN)
  ;; Remove the tag from one arg so that the result will have the correct
  ;; fixnum tag.
  (inst sra temp x n-fixnum-tag-bits)
  (inst mult temp y)
  (inst mflo res)
  (inst mfhi hi)
  ;; Check to see if the result will fit in a fixnum.  (I.e. the high word
  ;; is just 32 copies of the sign bit of the low word).
  (inst sra temp res 31)
  (inst beq temp hi DONE)
  ;; Shift the double word hi:res down two bits into hi:low to get rid of the
  ;; fixnum tag.
  (inst srl lo res n-fixnum-tag-bits)
  (inst sll temp hi (- n-word-bits n-fixnum-tag-bits))
  (inst or lo temp)
  (inst sra hi n-fixnum-tag-bits)

  ;; Do we need one word or two?  Assume two.
  (inst sra temp lo 31)
  (inst bne temp hi TWO-WORDS)
  ;; Assume a two word header.
  (inst li temp (logior (ash 2 n-widetag-bits) bignum-widetag))

  ;; Only need one word, fix the header.
  (inst li temp (logior (ash 1 n-widetag-bits) bignum-widetag))

  (pseudo-atomic (pa-flag :extra (pad-data-block (+ 1 bignum-digits-offset)))
    (inst or res alloc-tn other-pointer-lowtag)
    (storew temp res 0 other-pointer-lowtag))
  (inst b DONE)
  (storew lo res bignum-digits-offset other-pointer-lowtag)

  TWO-WORDS
  (pseudo-atomic (pa-flag :extra (pad-data-block (+ 2 bignum-digits-offset)))
    (inst or res alloc-tn other-pointer-lowtag)
    (storew temp res 0 other-pointer-lowtag))

  (storew lo res bignum-digits-offset other-pointer-lowtag)
  (inst b DONE)
  (storew hi res (1+ bignum-digits-offset) other-pointer-lowtag)

  DO-STATIC-FUN
  (inst lw lip null-tn (static-fun-offset 'two-arg-*))
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (inst j lip)
  (move cfp-tn csp-tn t)

  DONE)



;;;; Comparison routines.

(macrolet
    ((define-cond-assem-rtn (name translate static-fn cmp)
       `(define-assembly-routine (,name
                                  (:cost 10)
                                  (:return-style :full-call)
                                  (:policy :safe)
                                  (:translate ,translate)
                                  (:save-p t))
                                 ((:arg x (descriptor-reg any-reg) a0-offset)
                                  (:arg y (descriptor-reg any-reg) a1-offset)

                                  (:res res descriptor-reg a0-offset)

                                  (:temp temp non-descriptor-reg nl0-offset)
                                  (:temp lip interior-reg lip-offset)
                                  (:temp nargs any-reg nargs-offset)
                                  (:temp ocfp any-reg ocfp-offset))
          (inst or temp x y)
          (inst and temp fixnum-tag-mask)
          (inst beq temp DO-COMPARE)
          ,cmp

          ;; DO-STATIC-FUN
          (inst lw lip null-tn (static-fun-offset ',static-fn))
          (inst li nargs (fixnumize 2))
          (move ocfp cfp-tn)
          (inst j lip)
          (move cfp-tn csp-tn t)

          DO-COMPARE
          (inst beq temp DONE)
          (move res null-tn t)
          (load-symbol res t)

          DONE)))

  (define-cond-assem-rtn generic-< < two-arg-< (inst slt temp x y))
  (define-cond-assem-rtn generic-> > two-arg-> (inst slt temp y x)))


(define-assembly-routine (generic-eql
                          (:cost 10)
                          (:return-style :full-call)
                          (:policy :safe)
                          (:translate eql)
                          (:save-p t))
                         ((:arg x (descriptor-reg any-reg) a0-offset)
                          (:arg y (descriptor-reg any-reg) a1-offset)

                          (:res res descriptor-reg a0-offset)

                          (:temp temp non-descriptor-reg nl0-offset)
                          (:temp lip interior-reg lip-offset)
                          (:temp nargs any-reg nargs-offset)
                          (:temp ocfp any-reg ocfp-offset))
  (inst beq x y RETURN-T)
  (inst or temp x y)
  (inst and temp fixnum-tag-mask)
  (inst beq temp RETURN)
  (inst nop)

  ;; DO-STATIC-FUN
  (inst lw lip null-tn (static-fun-offset 'eql))
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (inst j lip)
  (move cfp-tn csp-tn t)

  RETURN
  (inst bne x y DONE)
  (move res null-tn t)

  RETURN-T
  (load-symbol res t)

  DONE)


(define-assembly-routine (generic-=
                          (:cost 10)
                          (:return-style :full-call)
                          (:policy :safe)
                          (:translate =)
                          (:save-p t))
                         ((:arg x (descriptor-reg any-reg) a0-offset)
                          (:arg y (descriptor-reg any-reg) a1-offset)

                          (:res res descriptor-reg a0-offset)

                          (:temp temp non-descriptor-reg nl0-offset)
                          (:temp lip interior-reg lip-offset)
                          (:temp nargs any-reg nargs-offset)
                          (:temp ocfp any-reg ocfp-offset))
  (inst or temp x y)
  (inst and temp fixnum-tag-mask)
  (inst beq temp RETURN)
  (inst nop)

  ;; DO-STATIC-FUN
  (inst lw lip null-tn (static-fun-offset 'two-arg-=))
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (inst j lip)
  (move cfp-tn csp-tn t)

  RETURN
  (inst bne x y DONE)
  (move res null-tn t)
  (load-symbol res t)

  DONE)


(define-assembly-routine (generic-/=
                          (:cost 10)
                          (:return-style :full-call)
                          (:policy :safe)
                          (:translate /=)
                          (:save-p t))
                         ((:arg x (descriptor-reg any-reg) a0-offset)
                          (:arg y (descriptor-reg any-reg) a1-offset)

                          (:res res descriptor-reg a0-offset)

                          (:temp temp non-descriptor-reg nl0-offset)
                          (:temp lip interior-reg lip-offset)
                          (:temp nargs any-reg nargs-offset)
                          (:temp ocfp any-reg ocfp-offset))
  (inst or temp x y)
  (inst and temp fixnum-tag-mask)
  (inst beq temp RETURN)
  (inst nop)

  ;; DO-STATIC-FUN
  (inst lw lip null-tn (static-fun-offset 'two-arg-/=))
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (inst j lip)
  (move cfp-tn csp-tn t)

  RETURN
  (inst beq x y DONE)
  (move res null-tn t)
  (load-symbol res t)

  DONE)
