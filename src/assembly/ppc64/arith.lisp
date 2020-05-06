(in-package "SB-VM")



;;;; Addition and subtraction.

;;; static-fun-offset returns the address of the raw_addr slot of
;;; a static function's fdefn.

;;; Note that there is only one use of static-fun-offset outside this
;;; file (in genesis.lisp)

(defmacro maybe-call-static-fun ()
  '(progn (inst or temp x y)
          (inst andi. temp temp fixnum-tag-mask)
          (inst bne DO-STATIC-FUN)))

(define-assembly-routine
  (generic-+
   (:cost 10)
   (:return-style :full-call)
   (:translate +)
   (:policy :safe)
   (:save-p t))
  ((:arg x (descriptor-reg any-reg) a0-offset)
   (:arg y (descriptor-reg any-reg) a1-offset)

   (:res res (descriptor-reg any-reg) a0-offset)

   (:temp temp non-descriptor-reg nl0-offset)
   (:temp temp2 non-descriptor-reg nl1-offset)
   (:temp flag non-descriptor-reg nl3-offset)
   (:temp lra descriptor-reg lra-offset)
   (:temp nargs any-reg nargs-offset)
   (:temp lip interior-reg lip-offset)
   (:temp ocfp any-reg ocfp-offset))

  (maybe-call-static-fun)
  (inst li temp-reg-tn 0) (inst mtxer temp-reg-tn) ; clear XER
  (inst addo. temp x y)
  (inst bns done)

  (inst sradi temp x n-fixnum-tag-bits)
  (inst sradi temp2 y n-fixnum-tag-bits)
  (inst add temp2 temp2 temp)
  (with-fixed-allocation (res flag temp bignum-widetag (1+ bignum-digits-offset))
    (storew temp2 res bignum-digits-offset other-pointer-lowtag))
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (inst addi lexenv-tn null-tn (static-fdefn-offset 'two-arg-+))
  (loadw code-tn lexenv-tn fdefn-fun-slot other-pointer-lowtag)
  (loadw lip lexenv-tn fdefn-raw-addr-slot other-pointer-lowtag)
  (inst li nargs (fixnumize 2))
  (inst mr ocfp cfp-tn)
  (inst mr cfp-tn csp-tn)
  (inst j lip 0)

  DONE
  (move res temp))


(define-assembly-routine
  (generic--
   (:cost 10)
   (:return-style :full-call)
   (:translate -)
   (:policy :safe)
   (:save-p t))
  ((:arg x (descriptor-reg any-reg) a0-offset)
   (:arg y (descriptor-reg any-reg) a1-offset)

   (:res res (descriptor-reg any-reg) a0-offset)

   (:temp temp non-descriptor-reg nl0-offset)
   (:temp temp2 non-descriptor-reg nl1-offset)
   (:temp flag non-descriptor-reg nl3-offset)
   (:temp lip interior-reg lip-offset)
   (:temp lra descriptor-reg lra-offset)
   (:temp nargs any-reg nargs-offset)
   (:temp ocfp any-reg ocfp-offset))

  (maybe-call-static-fun)
  (inst li temp-reg-tn 0) (inst mtxer temp-reg-tn) ; clear XER
  (inst subo. temp x y)
  (inst bns done)

  (inst sradi temp x n-fixnum-tag-bits)
  (inst sradi temp2 y n-fixnum-tag-bits)
  (inst sub temp2 temp temp2)
  (with-fixed-allocation (res flag temp bignum-widetag (1+ bignum-digits-offset))
    (storew temp2 res bignum-digits-offset other-pointer-lowtag))
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (inst addi lexenv-tn null-tn (static-fdefn-offset 'two-arg--))
  (loadw code-tn lexenv-tn fdefn-fun-slot other-pointer-lowtag)
  (loadw lip lexenv-tn fdefn-raw-addr-slot other-pointer-lowtag)
  (inst li nargs (fixnumize 2))
  (inst mr ocfp cfp-tn)
  (inst mr cfp-tn csp-tn)
  (inst j lip 0)

  DONE
  (move res temp))



;;;; Multiplication


(define-assembly-routine
  (generic-*
   (:cost 50)
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
   (:temp pa-flag non-descriptor-reg nl3-offset)
   (:temp lip interior-reg lip-offset)
   (:temp lra descriptor-reg lra-offset)
   (:temp nargs any-reg nargs-offset)
   (:temp ocfp any-reg ocfp-offset))

  (maybe-call-static-fun)
  (inst sradi temp x n-fixnum-tag-bits) ; Remove the tag from X
  (inst li temp-reg-tn 0) (inst mtxer temp-reg-tn) ; clear XER
  (inst mulldo. lo y temp)
  (inst bns DONE)

  ;; Compute upper bits of the double-precision product.
  (inst mulhd hi y temp)
  ;; Perform double-precision right shift of hi:lo by n-fixnum-tag-bits.
  (inst sldi temp hi (- 64 n-fixnum-tag-bits))
  (inst sradi hi hi n-fixnum-tag-bits)
  (inst srdi lo lo n-fixnum-tag-bits)
  (inst or lo temp lo)
  ;; Determine whether 'hi' is just the extension of lo's sign bit.
  (inst sradi temp lo 63) ; temp <- 64 replicas of sign bit
  (inst cmpd hi temp)
  (inst bne TWO-WORD-BIGNUM)
  ;; one word bignum
  (with-fixed-allocation (res pa-flag temp bignum-widetag (1+ bignum-digits-offset))
    (storew lo res bignum-digits-offset other-pointer-lowtag))
  (lisp-return lra lip :offset 2)
  TWO-WORD-BIGNUM
  (with-fixed-allocation (res pa-flag temp bignum-widetag (+ bignum-digits-offset 2))
    (storew lo res bignum-digits-offset other-pointer-lowtag)
    (storew hi res (1+ bignum-digits-offset) other-pointer-lowtag))
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (inst addi lexenv-tn null-tn (static-fdefn-offset 'two-arg-*))
  (loadw code-tn lexenv-tn fdefn-fun-slot other-pointer-lowtag)
  (loadw lip lexenv-tn fdefn-raw-addr-slot other-pointer-lowtag)
  (inst li nargs (fixnumize 2))
  (inst mr ocfp cfp-tn)
  (inst mr cfp-tn csp-tn)
  (inst j lip 0)

  DONE
  (move res lo))

(macrolet
    ((frob (name note cost type sc)
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
              `((inst sradi x x n-fixnum-tag-bits)))
          (inst mulld res x y))))
  (frob unsigned-* "unsigned *" 40 unsigned-num unsigned-reg)
  (frob signed-* "signed *" 41 signed-num signed-reg)
  (frob fixnum-* "fixnum *" 30 tagged-num any-reg))



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
                          (:res rem any-reg nl0-offset))
  (aver (location= rem dividend))
  (let ((error (generate-error-code nil 'division-by-zero-error
                                    dividend divisor)))
    (inst cmpdi divisor 0)
    (inst beq error))
    (inst divdu quo dividend divisor)
    (inst mulld divisor quo divisor)
    (inst sub rem dividend divisor)
    (inst sldi quo quo n-fixnum-tag-bits))

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
                          (:res rem any-reg nl0-offset))

  (aver (location= rem dividend))
  (let ((error (generate-error-code nil 'division-by-zero-error
                                    dividend divisor)))
    (inst cmpdi divisor 0)
    (inst beq error))
    (inst divd quo dividend divisor)
    (inst mulld divisor quo divisor)
    (inst subf rem divisor dividend)
    (inst sldi quo quo n-fixnum-tag-bits))

(define-assembly-routine (signed-truncate
                          (:note "(signed-byte 64) truncate")
                          (:cost 60)
                          (:policy :fast-safe)
                          (:translate truncate)
                          (:arg-types signed-num signed-num)
                          (:result-types signed-num signed-num))

                         ((:arg dividend signed-reg nl0-offset)
                          (:arg divisor signed-reg nl1-offset)

                          (:res quo signed-reg nl2-offset)
                          (:res rem signed-reg nl0-offset))

  (let ((error (generate-error-code nil 'division-by-zero-error
                                    dividend divisor)))
    (inst cmpdi divisor 0)
    (inst beq error))
    (inst divd quo dividend divisor)
    (inst mulld divisor quo divisor)
    (inst subf rem divisor dividend))


;;;; Comparison

(macrolet
    ((define-cond-assem-rtn (name translate static-fn inst)
       `(define-assembly-routine
          (,name
           (:cost 10)
           (:return-style :full-call)
           (:policy :safe)
           (:translate ,translate)
           (:save-p t))
          ((:arg x (descriptor-reg any-reg) a0-offset)
           (:arg y (descriptor-reg any-reg) a1-offset)

           (:res res descriptor-reg a0-offset)

           (:temp lip interior-reg lip-offset)
           (:temp nargs any-reg nargs-offset)
           (:temp ocfp any-reg ocfp-offset))

          (inst or nargs x y) ; (x|y) tag is 0 if both are fixnums
          (inst andi. nargs nargs fixnum-tag-mask)
          (inst beq FIXNUM)

          (inst addi lexenv-tn null-tn (static-fdefn-offset ',static-fn))
          (loadw code-tn lexenv-tn fdefn-fun-slot other-pointer-lowtag)
          (loadw lip lexenv-tn fdefn-raw-addr-slot other-pointer-lowtag)
          (inst li nargs (fixnumize 2))
          (inst mr ocfp cfp-tn)
          (inst mr cfp-tn csp-tn)
          (inst j lip 0)

          FIXNUM
          (inst cmpd x y) ; RES and X are the same register, so do this first
          (load-symbol res t)
          ,inst
          DONE)))

  (define-cond-assem-rtn generic-= = two-arg-=    (inst isel res res null-tn :eq))
  (define-cond-assem-rtn generic-/= /= two-arg-/= (inst isel res null-tn res :eq))
  (define-cond-assem-rtn generic-< < two-arg-<    (inst isel res res null-tn :lt))
  (define-cond-assem-rtn generic-<= <= two-arg-<= (inst isel res null-tn res :gt))
  (define-cond-assem-rtn generic-> > two-arg->    (inst isel res res null-tn :gt))
  (define-cond-assem-rtn generic->= >= two-arg->= (inst isel res null-tn res :lt)))

(define-assembly-routine (generic-eql
                          (:cost 10)
                          (:return-style :full-call)
                          (:policy :safe)
                          (:translate eql)
                          (:save-p t))
                         ((:arg x (descriptor-reg any-reg) a0-offset)
                          (:arg y (descriptor-reg any-reg) a1-offset)

                          (:res res descriptor-reg a0-offset)

                          (:temp lip interior-reg lip-offset)
                          (:temp nargs any-reg nargs-offset)
                          (:temp ocfp any-reg ocfp-offset))
  (inst and nargs x y) ; (x&y) tag is 0 if at least one is fixnum
  (inst andi. nargs nargs fixnum-tag-mask)
  (inst beq FIXNUM)

  (inst addi lexenv-tn null-tn (static-fdefn-offset 'eql))
  (loadw code-tn lexenv-tn fdefn-fun-slot other-pointer-lowtag)
  (loadw lip lexenv-tn fdefn-raw-addr-slot other-pointer-lowtag)
  (inst li nargs (fixnumize 2))
  (inst mr ocfp cfp-tn)
  (inst mr cfp-tn csp-tn)
  (inst j lip 0)

  FIXNUM
  (inst cmpd x y)
  (load-symbol res t)
  (inst isel res res null-tn :eq))
