;;;; stuff to handle simple cases for generic arithmetic

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

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
                          (:temp lra descriptor-reg lra-offset)
                          (:temp lip interior-reg lip-offset)
                          (:temp nargs any-reg nargs-offset)
                          (:temp ocfp any-reg ocfp-offset))
  (inst or temp x y)
  (inst and temp fixnum-tag-mask)
  (inst bne temp DO-STATIC-FUN)
  (inst addu temp x y)
  ;; check for overflow
  (inst xor temp1 temp x)
  (inst xor temp2 temp y)
  (inst and temp1 temp2)
  (inst bltz temp1 DO-OVERFLOW)
  (inst sra temp1 x n-fixnum-tag-bits)
  (inst move res temp)
  (lisp-return lra lip :offset 2)

  DO-OVERFLOW
  ;; We did overflow, so do the bignum version
  (inst sra temp2 y n-fixnum-tag-bits)
  (inst addu temp temp1 temp2)
  (with-fixed-allocation (res pa-flag temp2 bignum-widetag
                          (1+ bignum-digits-offset) nil)
    (storew temp res bignum-digits-offset other-pointer-lowtag))
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (inst lw lip null-tn (static-fun-offset 'two-arg-+))
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (inst j lip)
  (move cfp-tn csp-tn t))


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
                          (:temp lra descriptor-reg lra-offset)
                          (:temp lip interior-reg lip-offset)
                          (:temp nargs any-reg nargs-offset)
                          (:temp ocfp any-reg ocfp-offset))
  (inst or temp x y)
  (inst and temp fixnum-tag-mask)
  (inst bne temp DO-STATIC-FUN)
  (inst subu temp x y)
  ;; check for overflow
  (inst xor temp1 x y)
  (inst xor temp2 x temp)
  (inst and temp1 temp2)
  (inst bltz temp1 DO-OVERFLOW)
  (inst sra temp1 x n-fixnum-tag-bits)
  (inst move res temp)
  (lisp-return lra lip :offset 2)

  DO-OVERFLOW
  ;; We did overflow, so do the bignum version
  (inst sra temp2 y n-fixnum-tag-bits)
  (inst subu temp temp1 temp2)
  (with-fixed-allocation (res pa-flag temp2 bignum-widetag
                          (1+ bignum-digits-offset) nil)
    (storew temp res bignum-digits-offset other-pointer-lowtag))
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (inst lw lip null-tn (static-fun-offset 'two-arg--))
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (inst j lip)
  (move cfp-tn csp-tn t))



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
                          (:temp lra descriptor-reg lra-offset)
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
  (inst bne temp hi DO-BIGNUM)
  (inst srl lo res n-fixnum-tag-bits)
  (lisp-return lra lip :offset 2)

  DO-BIGNUM
  ;; Shift the double word hi:res down two bits into hi:low to get rid of the
  ;; fixnum tag.
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
  (storew lo res bignum-digits-offset other-pointer-lowtag)
  (lisp-return lra lip :offset 2)

  TWO-WORDS
  (pseudo-atomic (pa-flag :extra (pad-data-block (+ 2 bignum-digits-offset)))
    (inst or res alloc-tn other-pointer-lowtag)
    (storew temp res 0 other-pointer-lowtag))

  (storew lo res bignum-digits-offset other-pointer-lowtag)
  (storew hi res (1+ bignum-digits-offset) other-pointer-lowtag)
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (inst lw lip null-tn (static-fun-offset 'two-arg-*))
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (inst j lip)
  (move cfp-tn csp-tn t))


(macrolet
    ((frob (name note cost type sc signed-p)
       `(define-assembly-routine (,name
                                  (:note ,note)
                                  (:cost ,cost)
                                  (:translate *)
                                  (:policy :fast-safe)
                                  (:arg-types ,type ,type)
                                  (:result-types ,type))
                                 ((:arg x ,sc nl0-offset)
                                  (:arg y ,sc nl1-offset)
                                  (:res res ,sc nl0-offset))
          ,@(when (eq type 'tagged-num)
              `((inst sra x 2)))
          (inst ,(if signed-p 'mult 'multu) x y)
          (inst mflo res))))
  (frob unsigned-* "unsigned *" 40 unsigned-num unsigned-reg nil)
  (frob signed-* "signed *" 41 signed-num signed-reg t)
  (frob fixnum-* "fixnum *" 30 tagged-num any-reg t))



;;;; Division.


(define-assembly-routine (positive-fixnum-truncate
                          (:note "unsigned fixnum truncate")
                          (:cost 45)
                          (:translate truncate)
                          (:policy :fast-safe)
                          (:arg-types positive-fixnum positive-fixnum)
                          (:result-types positive-fixnum positive-fixnum))
                         ((:arg dividend any-reg nl0-offset)
                          (:arg divisor any-reg nl1-offset)

                          (:res quo any-reg nl2-offset)
                          (:res rem any-reg nl3-offset))
  (let ((error (generate-error-code nil 'division-by-zero-error
                                    dividend divisor)))
    (inst beq divisor error)
    (inst nop))

    (inst divu dividend divisor)
    (inst mflo quo)
    (inst mfhi rem)
    (inst sll quo 2))


(define-assembly-routine (fixnum-truncate
                          (:note "fixnum truncate")
                          (:cost 50)
                          (:policy :fast-safe)
                          (:translate truncate)
                          (:arg-types tagged-num tagged-num)
                          (:result-types tagged-num tagged-num))
                         ((:arg dividend any-reg nl0-offset)
                          (:arg divisor any-reg nl1-offset)

                          (:res quo any-reg nl2-offset)
                          (:res rem any-reg nl3-offset))
  (let ((error (generate-error-code nil 'division-by-zero-error
                                    dividend divisor)))
    (inst beq divisor error)
    (inst nop))

    (inst div dividend divisor)
    (inst mflo quo)
    (inst mfhi rem)
    (inst sll quo 2))


(define-assembly-routine (signed-truncate
                          (:note "(signed-byte 32) truncate")
                          (:cost 60)
                          (:policy :fast-safe)
                          (:translate truncate)
                          (:arg-types signed-num signed-num)
                          (:result-types signed-num signed-num))

                         ((:arg dividend signed-reg nl0-offset)
                          (:arg divisor signed-reg nl1-offset)

                          (:res quo signed-reg nl2-offset)
                          (:res rem signed-reg nl3-offset))
  (let ((error (generate-error-code nil 'division-by-zero-error
                                    dividend divisor)))
    (inst beq divisor error)
    (inst nop))

    (inst div dividend divisor)
    (inst mflo quo)
    (inst mfhi rem))



;;;; Comparison routines.

(macrolet
    ((define-cond-assem-rtn (name translate static-fn cmp not-p)
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
                                  (:temp lra descriptor-reg lra-offset)
                                  (:temp lip interior-reg lip-offset)
                                  (:temp nargs any-reg nargs-offset)
                                  (:temp ocfp any-reg ocfp-offset))
          (inst or temp x y)
          (inst and temp fixnum-tag-mask)
          (inst bne temp DO-STATIC-FUN)
          ,cmp

          (inst ,(if not-p 'beq 'bne) temp DONE)
          (move res null-tn t)
          (load-symbol res t)

          DONE
          (lisp-return lra lip :offset 2)

          DO-STATIC-FUN
          (inst lw lip null-tn (static-fun-offset ',static-fn))
          (inst li nargs (fixnumize 2))
          (move ocfp cfp-tn)
          (inst j lip)
          (move cfp-tn csp-tn t))))

  (define-cond-assem-rtn generic-< < two-arg-< (inst slt temp x y) t)
  (define-cond-assem-rtn generic-<= <= two-arg-<= (inst slt temp x y) nil)
  (define-cond-assem-rtn generic-> > two-arg-> (inst slt temp y x) t)
  (define-cond-assem-rtn generic->= >= two-arg->= (inst slt temp y x) nil))


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
                          (:temp lra descriptor-reg lra-offset)
                          (:temp lip interior-reg lip-offset)
                          (:temp nargs any-reg nargs-offset)
                          (:temp ocfp any-reg ocfp-offset))
  (inst beq x y RETURN-T)
  (inst or temp x y)
  (inst and temp fixnum-tag-mask)
  (inst bne temp DO-STATIC-FUN)
  (inst nop)

  (inst bne x y DONE)
  (move res null-tn t)

  RETURN-T
  (load-symbol res t)

  DONE
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (inst lw lip null-tn (static-fun-offset 'eql))
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (inst j lip)
  (move cfp-tn csp-tn t))


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
                          (:temp lra descriptor-reg lra-offset)
                          (:temp lip interior-reg lip-offset)
                          (:temp nargs any-reg nargs-offset)
                          (:temp ocfp any-reg ocfp-offset))
  (inst or temp x y)
  (inst and temp fixnum-tag-mask)
  (inst bne temp DO-STATIC-FUN)
  (inst nop)

  (inst bne x y DONE)
  (move res null-tn t)
  (load-symbol res t)

  DONE
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (inst lw lip null-tn (static-fun-offset 'two-arg-=))
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (inst j lip)
  (move cfp-tn csp-tn t))


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
                          (:temp lra descriptor-reg lra-offset)
                          (:temp lip interior-reg lip-offset)
                          (:temp nargs any-reg nargs-offset)
                          (:temp ocfp any-reg ocfp-offset))
  (inst or temp x y)
  (inst and temp fixnum-tag-mask)
  (inst bne temp DO-STATIC-FUN)
  (inst nop)

  (inst beq x y DONE)
  (move res null-tn t)
  (load-symbol res t)

  DONE
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (inst lw lip null-tn (static-fun-offset 'two-arg-/=))
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (inst j lip)
  (move cfp-tn csp-tn t))
