;;;; the VM definition arithmetic VOPs for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; unary operations.

(define-vop (fast-safe-arith-op)
  (:policy :fast-safe)
  (:effects)
  (:affected))

(define-vop (fixnum-unop fast-safe-arith-op)
  (:args (x :scs (any-reg)))
  (:results (res :scs (any-reg)))
  (:note "inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num))

(define-vop (signed-unop fast-safe-arith-op)
  (:args (x :scs (signed-reg)))
  (:results (res :scs (signed-reg)))
  (:note "inline (signed-byte 32) arithmetic")
  (:arg-types signed-num)
  (:result-types signed-num))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (inst rsb res x 0)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 1
    (inst rsb res x 0)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:translate lognot)
  (:generator 2
    (inst mvn res x)
    (inst eor res res fixnum-tag-mask)))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 1
    (inst mvn res x)))


;;;; Binary fixnum operations.

;;; Assume that any constant operand is the second arg...

(define-vop (fast-fixnum-binop fast-safe-arith-op)
  (:args (x :target r :scs (any-reg))
         (y :target r :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg))
         (y :target r :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-signed-binop fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg))
         (y :target r :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic"))

(define-vop (fast-fixnum-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg)))
  (:info y)
  (:arg-types tagged-num
              (:constant (signed-byte #.n-fixnum-bits)))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg)))
  (:info y)
  (:arg-types unsigned-num
              (:constant (unsigned-byte 32)))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg)))
  (:info y)
  (:arg-types signed-num
              (:constant (signed-byte 32)))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic"))

(defmacro define-binop (translate untagged-penalty op
                        &key cop arg-swap neg-op invert-y invert-r try-single-op)
  (let ((cop (or cop op)))
    `(progn
       (define-vop (,(symbolicate 'fast translate '/fixnum=>fixnum)
                     fast-fixnum-binop)
         (:translate ,translate)
         (:generator 2
                     ,(if arg-swap
                          `(inst ,op r y x)
                          `(inst ,op r x y))))
       (define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum)
                     fast-fixnum-binop-c)
         (:translate ,translate)
         (:generator 1
          (composite-immediate-instruction ,cop r x y :fixnumize t :neg-op ,neg-op :invert-y ,invert-y :invert-r ,invert-r :single-op-op ,(when try-single-op op))))
       (define-vop (,(symbolicate 'fast- translate '/signed=>signed)
                     fast-signed-binop)
         (:translate ,translate)
         (:generator ,(1+ untagged-penalty)
                     ,(if arg-swap
                          `(inst ,op r y x)
                          `(inst ,op r x y))))
       (define-vop (,(symbolicate 'fast- translate '-c/signed=>signed)
                     fast-signed-binop-c)
         (:translate ,translate)
         (:generator ,untagged-penalty
          (composite-immediate-instruction ,cop r x y :neg-op ,neg-op :invert-y ,invert-y :invert-r ,invert-r :single-op-op ,(when try-single-op op))))
       (define-vop (,(symbolicate 'fast- translate '/unsigned=>unsigned)
                     fast-unsigned-binop)
         (:translate ,translate)
         (:generator ,(1+ untagged-penalty)
                     ,(if arg-swap
                          `(inst ,op r y x)
                          `(inst ,op r x y))))
       (define-vop (,(symbolicate 'fast- translate '-c/unsigned=>unsigned)
                     fast-unsigned-binop-c)
         (:translate ,translate)
         (:generator ,untagged-penalty
          (composite-immediate-instruction ,cop r x y :neg-op ,neg-op :invert-y ,invert-y :invert-r ,invert-r :single-op-op ,(when try-single-op op)))))))

(define-binop + 4 add :neg-op sub)
(define-binop - 4 sub :neg-op add)
(define-binop logand 2 and :cop bic :invert-y t :try-single-op t)
(define-binop logandc1 2 bic :cop orr :arg-swap t :invert-y t :invert-r t)
(define-binop logandc2 2 bic)
(define-binop logior 2 orr)
(define-binop logxor 2 eor)

(define-vop (fast-lognor/fixnum=>fixnum fast-fixnum-binop)
  (:translate lognor)
  (:args (x :target r :scs (any-reg))
         (y :target r :scs (any-reg)))
  (:temporary (:sc non-descriptor-reg) temp)
  (:generator 3
    (inst orr temp x y)
    (inst mvn temp temp)
    (inst eor r temp fixnum-tag-mask)))

(define-vop (fast-logand/signed-unsigned=>unsigned fast-logand/unsigned=>unsigned)
  (:args (x :scs (signed-reg) :target r)
         (y :scs (unsigned-reg) :target r))
  (:arg-types signed-num unsigned-num)
  (:translate logand))

(define-source-transform logeqv (&rest args)
  (if (oddp (length args))
      `(logxor ,@args)
      `(lognot (logxor ,@args))))
(define-source-transform logorc1 (x y)
  `(logior (lognot ,x) ,y))
(define-source-transform logorc2 (x y)
  `(logior ,x (lognot ,y)))

;;; Shifting

(define-vop (fast-ash/unsigned=>unsigned)
  (:note "inline ASH")
  (:args (number :scs (unsigned-reg) :to :save)
         (amount :scs (signed-reg) :to :save))
  (:arg-types unsigned-num signed-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) ndesc)
  (:generator 3
    (inst cmp amount 0)
    (inst rsb :lt ndesc amount 0)
    (inst mov :lt result (lsr number ndesc))
    (inst mov :ge result (lsl number amount))))

(define-vop (fast-ash/signed=>signed)
  (:note "inline ASH")
  (:args (number :scs (signed-reg) :to :save)
         (amount :scs (signed-reg) :to :save))
  (:arg-types signed-num signed-num)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) ndesc)
  (:generator 3
    (inst cmp amount 0)
    (inst rsb :lt ndesc amount 0)
    (inst mov :lt result (asr number ndesc))
    (inst mov :ge result (lsl number amount))))

(macrolet ((def (name sc-type type result-type cost)
             `(define-vop (,name)
                (:note "inline ASH")
                (:translate ash)
                (:args (number :scs (,sc-type))
                       (amount :scs (signed-reg unsigned-reg immediate)))
                (:arg-types ,type positive-fixnum)
                (:results (result :scs (,result-type)))
                (:result-types ,type)
                (:policy :fast-safe)
                (:generator ,cost
                   (sc-case amount
                     ((signed-reg unsigned-reg)
                      (inst mov result (lsl number amount)))
                     (immediate
                      (let ((amount (tn-value amount)))
                        (aver (> amount 0))
                        (inst mov result (lsl number amount)))))))))
  ;; FIXME: There's the opportunity for a sneaky optimization here, I
  ;; think: a FAST-ASH-LEFT-C/FIXNUM=>SIGNED vop.  -- CSR, 2003-09-03
  (def fast-ash-left/fixnum=>fixnum any-reg tagged-num any-reg 2)
  (def fast-ash-left/signed=>signed signed-reg signed-num signed-reg 3)
  (def fast-ash-left/unsigned=>unsigned unsigned-reg unsigned-num unsigned-reg 3))

(define-vop (signed-byte-32-len)
  (:translate integer-length)
  (:note "inline (signed-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg) :target shift))
  (:arg-types signed-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) shift)
  (:generator 30
    (let ((loop (gen-label))
          (test (gen-label)))
      (move shift arg)
      (inst eor res res res)
      (inst cmp shift 0)
      (inst b :ge test)
      (inst mvn shift shift)
      (inst b test)

      (emit-label loop)
      (inst adds res res (fixnumize 1))

      (emit-label test)
      (inst movs shift (lsr shift 1))
      (inst b :ne loop))))

(define-vop (unsigned-byte-32-count)
  (:translate logcount)
  (:note "inline (unsigned-byte 32) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg) :target num))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0) :to (:result 0)
                    :target res) num)
  (:temporary (:scs (non-descriptor-reg)) mask temp)
  (:generator 30
    (load-immediate-word mask #x55555555)
    (inst mov temp (lsr arg 1))
    (inst and num arg mask)
    (inst and temp temp mask)
    (inst add num num temp)
    (load-immediate-word mask #x33333333)
    (inst mov temp (lsr arg 2))
    (inst and num arg mask)
    (inst and temp temp mask)
    (inst add num num temp)
    (load-immediate-word mask #x0f0f0f0f)
    (inst mov temp (lsr arg 4))
    (inst and num arg mask)
    (inst and temp temp mask)
    (inst add num num temp)
    (load-immediate-word mask #x00ff00ff)
    (inst mov temp (lsr arg 8))
    (inst and num arg mask)
    (inst and temp temp mask)
    (inst add num num temp)
    (load-immediate-word mask #x0000ffff)
    (inst mov temp (lsr arg 16))
    (inst and num arg mask)
    (inst and temp temp mask)
    (inst add res num temp)))

;;; Modular functions
(define-modular-fun lognot-mod32 (x) lognot :untagged nil 32)
(define-vop (lognot-mod32/unsigned=>unsigned)
  (:translate lognot-mod32)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (inst mvn res x)))

;;;; Binary conditional VOPs:

(define-vop (fast-conditional)
  (:conditional)
  (:info target not-p)
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (unsigned-byte 8)))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 32) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num (:constant (unsigned-byte 8)))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num (:constant (unsigned-byte 8)))
  (:info target not-p y))

(defmacro define-conditional-vop (tran cond unsigned not-cond not-unsigned)
  `(progn
     ,@(mapcar (lambda (suffix cost signed)
                 (unless (and (member suffix '(/fixnum -c/fixnum))
                              (eq tran 'eql))
                   `(define-vop (,(intern (format nil "~:@(FAST-IF-~A~A~)"
                                                  tran suffix))
                                 ,(intern
                                   (format nil "~:@(FAST-CONDITIONAL~A~)"
                                           suffix)))
                     (:translate ,tran)
                     (:generator ,cost
                      (inst cmp x
                       ,(if (eq suffix '-c/fixnum) '(fixnumize y) 'y))
                      (inst b (if not-p
                                  ,(if signed not-cond not-unsigned)
                                  ,(if signed cond unsigned))
                       target)))))
               '(/fixnum -c/fixnum /signed -c/signed /unsigned -c/unsigned)
               '(4 3 6 5 6 5)
               '(t t t t nil nil))))

(define-conditional-vop < :lt :lo :ge :hs)
(define-conditional-vop > :gt :hi :le :ls)
(define-conditional-vop eql :eq :eq :ne :ne)

;;; EQL/FIXNUM is funny because the first arg can be of any type, not
;;; just a known fixnum.

;;; These versions specify a fixnum restriction on their first arg.
;;; We have also generic-eql/fixnum VOPs which are the same, but have
;;; no restriction on the first arg and a higher cost.  The reason for
;;; doing this is to prevent fixnum specific operations from being
;;; used on word integers, spuriously consing the argument.

(define-vop (fast-eql/fixnum fast-conditional)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql)
  (:generator 4
    (inst cmp x y)
    (inst b (if not-p :ne :eq) target)))
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg)))
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (unsigned-byte 8)))
  (:info target not-p y)
  (:translate eql)
  (:generator 2
    (inst cmp x (fixnumize y))
    (inst b (if not-p :ne :eq) target)))
(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant (unsigned-byte 8)))
  (:variant-cost 6))

(define-source-transform lognand (x y)
  `(lognot (logand ,x ,y)))

;;;; Bignum stuff.

(define-vop (bignum-length get-header-data)
  (:translate sb!bignum:%bignum-length)
  (:policy :fast-safe))

(define-vop (bignum-set-length set-header-data)
  (:translate sb!bignum:%bignum-set-length)
  (:policy :fast-safe))

(define-full-reffer bignum-ref * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb!bignum:%bignum-ref)

(define-full-setter bignum-set * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb!bignum:%bignum-set)

(define-vop (add-w/carry)
  (:translate sb!bignum:%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target result) temp)
  (:results (result :scs (unsigned-reg))
            (carry :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num positive-fixnum)
  (:generator 3
    (inst rsbs temp c 0) ;; Clear carry if c=0, else set.
    (inst adcs result a b)
    (inst mov :cs carry 1)
    (inst mov :cc carry 0)))

(define-vop (sub-w/borrow)
  (:translate sb!bignum:%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target result) temp)
  (:results (result :scs (unsigned-reg))
            (borrow :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (inst rsbs temp c 0) ;; Clear carry if c=0, else set.
    (inst sbcs result a b)
    (inst mov :cs borrow 1)
    (inst mov :cc borrow 0)))

(define-vop (bignum-mult)
  (:translate sb!bignum:%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg))
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 1
    (inst umull lo hi x y)))

(define-vop (bignum-lognot lognot-mod32/unsigned=>unsigned)
  (:translate sb!bignum:%lognot))

(define-vop (bignum-floor)
  (:translate sb!bignum:%bigfloor)
  (:policy :fast-safe)
  (:args (div-high :scs (unsigned-reg) :target rem)
         (div-low :scs (unsigned-reg) :target quo)
         (divisor :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (quo :scs (unsigned-reg) :from (:argument 1))
            (rem :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num unsigned-num)
  (:generator 300
    (move rem div-high)
    (move quo div-low)
    (dotimes (i 33)
      (inst cmp rem divisor)
      (inst sub :ge rem rem divisor)
      (inst adcs quo quo quo)
      (unless (= i 32)
        (inst adc rem rem rem)))
    (inst mvn quo quo)))

(define-vop (digit-ashr)
  (:translate sb!bignum:%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg))
         (count :scs (unsigned-reg)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst mov result (asr digit count))))

(define-vop (digit-lshr digit-ashr)
  (:translate sb!bignum:%digit-logical-shift-right)
  (:generator 1
    (inst mov result (lsr digit count))))

(define-vop (digit-ashl digit-ashr)
  (:translate sb!bignum:%ashl)
  (:generator 1
    (inst mov result (lsl digit count))))

;;;; Static functions.

(define-static-fun two-arg-< (x y) :translate <)
(define-static-fun two-arg-> (x y) :translate >)
