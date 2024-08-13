(in-package "SB-VM")



;;;; Addition and subtraction.

;;; static-fun-offset returns the address of the raw_addr slot of
;;; a static function's fdefn.

;;; Note that there is only one use of static-fun-offset outside this
;;; file (in genesis.lisp)

(defmacro tail-call-fallback-fun (name)
  `(progn
     (inst addi lexenv-tn null-tn (static-fdefn-offset ',name))
     (loadw code-tn lexenv-tn fdefn-fun-slot other-pointer-lowtag)
     (loadw lip lexenv-tn fdefn-raw-addr-slot other-pointer-lowtag)     
     (inst li nargs (fixnumize 2))
     (inst mr ocfp cfp-tn)
     (inst mr cfp-tn csp-tn)
     (inst j lip 0)))

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

  ; Clear the damned "sticky overflow" bit in :cr0 and :xer
  (inst mtxer zero-tn)
  (inst or temp x y)
  (inst andi. temp temp 3)
  (inst bne DO-STATIC-FUN)
  (inst addo. temp x y)
  (inst bns done)

  (inst srawi temp x 2)
  (inst srawi temp2 y 2)
  (inst add temp2 temp2 temp)
  (with-fixed-allocation (res flag temp bignum-widetag (1+ bignum-digits-offset))
    (storew temp2 res bignum-digits-offset other-pointer-lowtag))
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN (tail-call-fallback-fun two-arg-+)

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

  ; Clear the damned "sticky overflow" bit in :cr0
  (inst mtxer zero-tn)

  (inst or temp x y)
  (inst andi. temp temp 3)
  (inst bne DO-STATIC-FUN)

  (inst subo. temp x y)
  (inst bns done)

  (inst srawi temp x 2)
  (inst srawi temp2 y 2)
  (inst sub temp2 temp temp2)
  (with-fixed-allocation (res flag temp bignum-widetag (1+ bignum-digits-offset))
    (storew temp2 res bignum-digits-offset other-pointer-lowtag))
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN (tail-call-fallback-fun two-arg--)

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

  ;; If either arg is not a fixnum, call the static function.  But first ...
  (inst mtxer zero-tn)

  (inst or temp x y)
  (inst andi. temp temp 3)
  ;; Remove the tag from both args, so I don't get so confused.
  (inst srawi temp x 2)
  (inst srawi nargs y 2)
  (inst bne DO-STATIC-FUN)


  (inst mullwo. lo nargs temp)
  (inst srawi hi lo 31)                 ; hi = 32 copies of lo's sign bit
  (inst bns ONE-WORD-ANSWER)
  (inst mulhw hi nargs temp)
  (inst b CONS-BIGNUM)

  ONE-WORD-ANSWER                       ; We know that all of the overflow bits are clear.
  (inst addo temp lo lo)
  (inst addo. res temp temp)
  (inst bns GO-HOME)

  CONS-BIGNUM
  ;; Allocate a BIGNUM for the result.
  (with-fixed-allocation (res pa-flag temp bignum-widetag
                              (+ bignum-digits-offset 2))
    (let ((one-word (gen-label)))
      ;; We start out assuming that we need one word.  Is that correct?
      (inst srawi temp lo 31)
      (inst xor. temp temp hi)
      (inst li temp (logior (ash 1 n-widetag-bits) bignum-widetag))
      (inst beq one-word)
      (inst li temp (logior (ash 2 n-widetag-bits) bignum-widetag))
      (storew hi res (1+ bignum-digits-offset) other-pointer-lowtag)
      (emit-label one-word)
      (storew temp res 0 other-pointer-lowtag)
      (storew lo res bignum-digits-offset other-pointer-lowtag)))
  ;; Out of here
  GO-HOME
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN (tail-call-fallback-fun two-arg-*)

  LOW-FITS-IN-FIXNUM
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
              `((inst srawi x x 2)))
          (inst mullw res x y))))
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
  (let ((error (generate-error-code nil 'division-by-zero-error dividend)))
    (inst cmpwi divisor 0)
    (inst beq error))
    (inst divwu quo dividend divisor)
    (inst mullw divisor quo divisor)
    (inst sub rem dividend divisor)
    (inst slwi quo quo 2))



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
  (let ((error (generate-error-code nil 'division-by-zero-error dividend)))
    (inst cmpwi divisor 0)
    (inst beq error))

    (inst divw quo dividend divisor)
    (inst mullw divisor quo divisor)
    (inst subf rem divisor dividend)
    (inst slwi quo quo 2))


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
                          (:res rem signed-reg nl0-offset))

  (let ((error (generate-error-code nil 'division-by-zero-error dividend)))
    (inst cmpwi divisor 0)
    (inst beq error))

    (inst divw quo dividend divisor)
    (inst mullw divisor quo divisor)
    (inst subf rem divisor dividend))


;;;; Comparison

(macrolet
    ((define-cond-assem-rtn (name translate static-fn cmp)
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

          (inst or nargs x y)
          (inst andi. nargs nargs 3)
          (inst cmpw :cr1 x y)
          (inst beq DO-COMPARE)

          DO-STATIC-FN (tail-call-fallback-fun ,static-fn)

          DO-COMPARE
          (load-symbol res t)
          (inst b? :cr1 ,cmp done)
          (inst mr res null-tn)
          DONE)))

  (define-cond-assem-rtn generic-< < two-arg-< :lt)
  (define-cond-assem-rtn generic-<= <= two-arg-<= :le)
  (define-cond-assem-rtn generic-> > two-arg-> :gt)
  (define-cond-assem-rtn generic->= >= two-arg->= :ge))


(define-assembly-routine (generic-eql
                          (:cost 10)
                          (:return-style :full-call)
                          (:policy :safe)
                          (:translate eql)
                          (:save-p t))
                         ((:arg x (descriptor-reg any-reg) a0-offset)
                          (:arg y (descriptor-reg any-reg) a1-offset)

                          (:res res descriptor-reg a0-offset)

                          (:temp lra descriptor-reg lra-offset)
                          (:temp lip interior-reg lip-offset)
                          (:temp nargs any-reg nargs-offset)
                          (:temp ocfp any-reg ocfp-offset))
  (inst cmpw :cr1 x y)
  (inst andi. nargs x 3)
  (inst beq :cr1 RETURN-T)
  (inst beq RETURN-NIL)                 ; x was fixnum, not eq y
  (inst andi. nargs y 3)
  (inst bne DO-STATIC-FN)

  RETURN-NIL
  (inst mr res null-tn)
  (lisp-return lra lip :offset 2)

  DO-STATIC-FN (tail-call-fallback-fun eql)

  RETURN-T
  (load-symbol res t))

(define-assembly-routine
  (generic-=
   (:cost 10)
   (:return-style :full-call)
   (:policy :safe)
   (:translate =)
   (:save-p t))
  ((:arg x (descriptor-reg any-reg) a0-offset)
   (:arg y (descriptor-reg any-reg) a1-offset)

   (:res res descriptor-reg a0-offset)

   (:temp lip interior-reg lip-offset)
   (:temp lra descriptor-reg lra-offset)
   (:temp nargs any-reg nargs-offset)
   (:temp ocfp any-reg ocfp-offset))

  (inst or nargs x y)
  (inst andi. nargs nargs 3)
  (inst cmpw :cr1 x y)
  (inst bne DO-STATIC-FN)
  (inst beq :cr1 RETURN-T)

  (inst mr res null-tn)
  (lisp-return lra lip :offset 2)

  DO-STATIC-FN (tail-call-fallback-fun two-arg-=)

  RETURN-T
  (load-symbol res t))

(define-assembly-routine (generic-/=
                          (:cost 10)
                          (:return-style :full-call)
                          (:policy :safe)
                          (:translate /=)
                          (:save-p t))
                         ((:arg x (descriptor-reg any-reg) a0-offset)
                          (:arg y (descriptor-reg any-reg) a1-offset)

                          (:res res descriptor-reg a0-offset)

                          (:temp lra descriptor-reg lra-offset)
                          (:temp lip interior-reg lip-offset)

                          (:temp nargs any-reg nargs-offset)
                          (:temp ocfp any-reg ocfp-offset))
  (inst or nargs x y)
  (inst andi. nargs nargs 3)
  (inst cmpw :cr1 x y)
  (inst bne DO-STATIC-FN)
  (inst beq :cr1 RETURN-NIL)

  (load-symbol res t)
  (lisp-return lra lip :offset 2)

  DO-STATIC-FN (tail-call-fallback-fun two-arg-/=)

  RETURN-NIL
  (inst mr res null-tn))
