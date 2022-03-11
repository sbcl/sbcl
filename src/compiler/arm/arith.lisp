;;;; the VM definition arithmetic VOPs for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; unary operations.

(define-vop (fast-safe-arith-op)
  (:policy :fast-safe))

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

(define-vop (unsigned-unop fast-safe-arith-op)
  (:args (x :scs (unsigned-reg)))
  (:results (res :scs (unsigned-reg)))
  (:note "inline (unsigned-byte 32) arithmetic")
  (:arg-types unsigned-num)
  (:result-types unsigned-num))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (inst rsb res x 0)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (inst rsb res x 0)))

(define-vop (fast-negate/unsigned signed-unop)
  (:args (x :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:translate %negate)
  (:generator 3
    (inst rsb res x 0)))

(define-vop (fast-lognot/fixnum signed-unop)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num)
  (:translate lognot)
  (:generator 1
    (inst mvn res (asr x n-fixnum-tag-bits))))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 2
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
       (define-vop (,(symbolicate 'fast- translate '/fixnum=>fixnum)
                     fast-fixnum-binop)
         (:translate ,translate)
         (:generator 2
                     ,(if arg-swap
                          `(inst ,op r y x)
                          `(inst ,op r x y))))
       (define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum)
                    fast-fixnum-binop-c)
         ,@(when invert-r `((:temporary (:sc non-descriptor-reg :target r) temp)))
         (:translate ,translate)
         (:generator 1
          (composite-immediate-instruction ,cop r x y :fixnumize t :neg-op ,neg-op :invert-y ,invert-y :invert-r ,invert-r ,@(when invert-r `(:temporary temp)) :single-op-op ,(when try-single-op op))))
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

(define-vop (fast-logior-unsigned-signed=>signed fast-safe-arith-op)
  (:args (x :scs (unsigned-reg))
         (y :target r :scs (signed-reg)))
  (:arg-types unsigned-num signed-num)
  (:results (r :scs (signed-reg) :from (:argument 1)))
  (:result-types signed-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:translate logior)
  (:generator 3
    (inst orr r x y)))

(define-vop (fast-logior-signed-unsigned=>signed fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg))
         (y :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:results (r :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:translate logior)
  (:generator 3
    (inst orr r x y)))

;;; Multiplication

(define-vop (fast-*/fixnum=>fixnum fast-fixnum-binop)
  (:args (x :scs (signed-reg)) ;; one operand needs to be untagged
         (y :target r :scs (any-reg)))
  (:translate *)
  (:generator 2
    (inst mul r x y)))

(define-vop (fast-*-c/fixnum=>fixnum fast-fixnum-binop-c)
  (:args (x :scs (any-reg) :to :result))
  (:results (r :scs (any-reg) :from :eval))
  (:temporary (:sc non-descriptor-reg :target r) temp)
  (:translate *)
  (:generator 1
    (load-immediate-word temp y)
    (inst mul r temp x)))

(define-vop (fast-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:generator 3
    (inst mul r x y)))

(define-vop (fast-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:generator 3
    (inst mul r x y)))

;;;
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

(define-vop (fast-ash-left-c/fixnum=>fixnum)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (any-reg) :target result))
  (:info amount)
  (:arg-types tagged-num (:constant unsigned-byte))
  (:results (result :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline ASH")
  (:generator 1
    (if (< amount 32)
        (inst mov result (lsl number amount))
        (inst mov result 0))))

(define-vop (fast-ash-right-c/fixnum=>fixnum)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (any-reg) :target result))
  (:info amount)
  (:arg-types tagged-num (:constant (integer * -1)))
  (:results (result :scs (any-reg)))
  (:result-types tagged-num)
  (:temporary (:sc unsigned-reg :target result) temp)
  (:note "inline ASH")
  (:generator 1
    (inst mov temp (asr number (min (- amount) 31)))
    (inst bic result temp fixnum-tag-mask)))

(define-vop (fast-ash-left-modfx-c/fixnum=>fixnum
             fast-ash-left-c/fixnum=>fixnum)
  (:translate ash-left-modfx))

(define-vop (fast-ash-left-mod32-c/fixnum=>fixnum
             fast-ash-left-c/fixnum=>fixnum)
  (:translate ash-left-mod32))

(define-vop (fast-ash-c/unsigned=>unsigned)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg) :target result))
  (:info amount)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline ASH")
  (:generator 3
    (cond ((< -32 amount 32)
           (if (plusp amount)
               (inst mov result (lsl number amount))
               (inst mov result (lsr number (- amount)))))
          (t
           (inst mov result 0)))))

(define-vop (fast-ash-c/signed=>signed)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg) :target result))
  (:info amount)
  (:arg-types signed-num (:constant integer))
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline ASH")
  (:generator 3
    (cond ((< -32 amount 32)
           (if (plusp amount)
               (inst mov result (lsl number amount))
               (inst mov result (asr number (- amount)))))
          ((<= amount -32)
           (inst mov result (asr number 31)))
          (t
           (inst mov result 0)))))

(define-vop (fast-ash-left-mod32-c/unsigned=>unsigned
             fast-ash-c/unsigned=>unsigned)
  (:translate ash-left-mod32))

(define-vop (fast-ash-left-mod32-c/signed=>signed
             fast-ash-c/signed=>signed)
  (:translate ash-left-mod32))

(define-vop (fast-ash/signed/unsigned)
  (:note "inline ASH")
  (:args (number)
         (amount))
  (:results (result))
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) temp)
  (:variant-vars variant)
  (:generator 5
    (move temp amount)
    (inst cmp temp 0)
    (inst b :ge LEFT)
    (inst rsb temp temp 0) ;; negate
    (inst cmp temp n-word-bits)
    (inst mov :gt temp n-word-bits)
    (inst mov result (ecase variant
                       (:signed (asr number temp))
                       (:unsigned (lsr number temp))))
    (inst b END)
    LEFT
    (inst cmp temp n-word-bits)
    (inst mov :gt temp n-word-bits)
    (inst mov result (lsl number temp))
    END))

(define-vop (fast-ash/signed=>signed fast-ash/signed/unsigned)
  (:args (number :scs (signed-reg) :to :save)
         (amount :scs (signed-reg) :to :save :target temp))
  (:arg-types signed-num signed-num)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:translate ash)
  (:variant :signed))

(define-vop (fast-ash/unsigned=>unsigned fast-ash/signed/unsigned)
  (:args (number :scs (unsigned-reg) :to :save)
         (amount :scs (signed-reg) :to :save))
  (:arg-types unsigned-num signed-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash)
  (:variant :unsigned))

(macrolet ((def (name sc-type type result-type cost)
             `(define-vop (,name)
                (:note "inline ASH")
                (:translate ash)
                (:args (number :scs (,sc-type))
                       (amount :scs (signed-reg unsigned-reg)
                               :target temp))
                (:temporary (:sc non-descriptor-reg) temp)
                (:arg-types ,type positive-fixnum)
                (:results (result :scs (,result-type)))
                (:result-types ,type)
                (:policy :fast-safe)
                (:generator ,cost
                  (move temp amount)
                  (inst cmp temp n-word-bits)
                  (inst mov :gt temp n-word-bits)
                  (inst mov result (lsl number temp))))))
  ;; FIXME: There's the opportunity for a sneaky optimization here, I
  ;; think: a FAST-ASH-LEFT-C/FIXNUM=>SIGNED vop.  -- CSR, 2003-09-03
  (def fast-ash-left/fixnum=>fixnum any-reg tagged-num any-reg 2)
  (def fast-ash-left/signed=>signed signed-reg signed-num signed-reg 3)
  (def fast-ash-left/unsigned=>unsigned unsigned-reg unsigned-num unsigned-reg 3))

(define-vop (fast-ash-left-mod32/unsigned=>unsigned
             fast-ash-left/unsigned=>unsigned)
  (:translate ash-left-mod32))

(define-vop (fast-%ash/right/unsigned)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg) :target result)
         (amount :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (result :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:generator 4
    (inst mov result (lsr number amount))))

(define-vop (fast-%ash/right/signed)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg) :target result)
         (amount :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:results (result :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:generator 4
    (inst mov result (asr number amount))))

(define-vop (fast-%ash/right/fixnum)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (any-reg) :target result)
         (amount :scs (unsigned-reg) :target temp))
  (:arg-types tagged-num unsigned-num)
  (:results (result :scs (any-reg) :from (:argument 0)))
  (:result-types tagged-num)
  (:temporary (:sc unsigned-reg :target result) temp)
  (:generator 3
    (inst mov temp (asr number amount))
    (inst bic result temp fixnum-tag-mask)))

;;; Only the lower 5 bits of the shift amount are significant.
(macrolet ((define (translate operation)
             `(define-vop ()
                (:translate ,translate)
                (:note ,(string translate))
                (:policy :fast-safe)
                (:args (num :scs (unsigned-reg))
                       (amount :scs (signed-reg)))
                (:arg-types unsigned-num tagged-num)
                (:temporary (:sc signed-reg) temp)
                (:results (r :scs (unsigned-reg)))
                (:result-types unsigned-num)
                (:generator 1
                  (inst and temp amount #b11111)
                  (inst mov r (,operation num temp))))))
  (define shift-towards-start lsr)
  (define shift-towards-end   lsl))

(define-vop (signed-byte-32-len)
  (:translate integer-length)
  (:note "inline (signed-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg) :target temp))
  (:arg-types signed-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) temp)
  (:generator 30
    (move temp arg)
    (inst cmp temp 0)
    (inst mvn :lt temp temp)
    (inst clz temp temp)
    (inst rsb temp temp 32)
    (inst mov res (lsl temp n-fixnum-tag-bits))))

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
    (move num arg)
    (load-immediate-word mask #x55555555)
    (inst and temp mask (lsr num 1))
    (inst and num num mask)
    (inst add num num temp)
    (load-immediate-word mask #x33333333)
    (inst and temp mask (lsr num 2))
    (inst and num num mask)
    (inst add num num temp)
    (load-immediate-word mask #x0f0f0f0f)
    (inst and temp mask (lsr num 4))
    (inst and num num mask)
    (inst add num num temp)
    (inst add num num (lsr num 8))
    (inst add num num (lsr num 16))
    (inst and res num #xff)))

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

(macrolet
    ((define-modular-backend (fun &optional constantp)
       (let ((mfun-name (symbolicate fun '-mod32))
             (modvop (symbolicate 'fast- fun '-mod32/unsigned=>unsigned))
             (modcvop (symbolicate 'fast- fun 'mod32-c/unsigned=>unsigned))
             (vop (symbolicate 'fast- fun '/unsigned=>unsigned))
             (cvop (symbolicate 'fast- fun '-c/unsigned=>unsigned)))
         `(progn
            (define-modular-fun ,mfun-name (x y) ,fun :untagged nil 32)
            (define-vop (,modvop ,vop)
              (:translate ,mfun-name))
            ,@(when constantp
                `((define-vop (,modcvop ,cvop)
                    (:translate ,mfun-name))))))))
  (define-modular-backend + t)
  (define-modular-backend - t)
  (define-modular-backend *)
  ;; (define-modular-backend logeqv)
  ;; (define-modular-backend lognand)
  ;; (define-modular-backend lognor)
  (define-modular-backend logandc1)
  (define-modular-backend logandc2)
  ;; (define-modular-backend logorc1)
  ;; (define-modular-backend logorc2)
  )

;;;; Binary conditional VOPs:

(define-vop (fast-conditional)
  (:conditional :eq)
  (:policy :fast-safe))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (unsigned-byte 8)))
  (:info y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 32) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num (:constant (unsigned-byte 8)))
  (:info y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num (:constant (unsigned-byte 8)))
  (:info y))

(defmacro define-conditional-vop (tran cond unsigned)
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
                     (:conditional ,(if signed cond unsigned))
                     (:generator ,cost
                      (inst cmp x
                       ,(if (eq suffix '-c/fixnum) '(fixnumize y) 'y))))))
               '(/fixnum -c/fixnum /signed -c/signed /unsigned -c/unsigned)
               '(4 3 6 5 6 5)
               '(t t t t nil nil))))

(define-conditional-vop < :lt :lo)
(define-conditional-vop > :gt :hi)
(define-conditional-vop eql :eq :eq)

;;; EQL/FIXNUM is funny because the first arg can be of any type, not
;;; just a known fixnum.

;;; These versions specify a fixnum restriction on their first arg.
;;; We have also generic-eql/fixnum VOPs which are the same, but have
;;; no restriction on the first arg and a higher cost.  The reason for
;;; doing this is to prevent fixnum specific operations from being
;;; used on word integers, spuriously consing the argument.

(define-vop (fast-eql/fixnum)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql)
  (:conditional :eq)
  (:policy :fast-safe)
  (:generator 4
    (inst cmp x y)))

(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg)))
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (signed-byte 9)))
  (:info y)
  (:translate eql)
  (:policy :fast-safe)
  (:conditional :eq)
  (:generator 3
    (if (minusp y)
        (inst cmn x (fixnumize (abs y)))
        (inst cmp x (fixnumize y)))))

(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant (signed-byte 9)))
  (:variant-cost 6))

(macrolet ((define-logtest-vops ()
             `(progn
                ,@(loop for suffix in '(/fixnum -c/fixnum
                                        /signed -c/signed
                                        /unsigned -c/unsigned)
                        for cost in '(4 3 6 5 6 5)
                        collect
                        `(define-vop (,(symbolicate "FAST-LOGTEST" suffix)
                                      ,(symbolicate "FAST-CONDITIONAL" suffix))
                           (:translate logtest)
                           (:conditional :ne)
                           (:generator ,cost
                                       (inst tst x
                                             ,(case suffix
                                                (-c/fixnum
                                                 `(fixnumize y))
                                                ((-c/signed -c/unsigned)
                                                 `y)
                                                (t
                                                 'y)))))))))
  (define-logtest-vops))

(define-source-transform lognand (x y)
  `(lognot (logand ,x ,y)))

(defknown %logbitp (integer unsigned-byte) boolean
  (movable foldable flushable always-translatable))

;;; For constant folding
(defun %logbitp (integer index)
  (logbitp index integer))

(define-vop (fast-logbitp-c/fixnum fast-conditional-c/fixnum)
  (:translate %logbitp)
  (:conditional :ne)
  (:arg-types tagged-num (:constant (integer 0 29)))
  (:generator 4
    (inst tst x (ash 1 (+ y n-fixnum-tag-bits)))))

(define-vop (fast-logbitp-c/signed fast-conditional-c/signed)
  (:translate %logbitp)
  (:conditional :ne)
  (:arg-types signed-num (:constant (integer 0 31)))
  (:generator 5
    (inst tst x (ash 1 y))))

(define-vop (fast-logbitp-c/unsigned fast-conditional-c/unsigned)
  (:translate %logbitp)
  (:conditional :ne)
  (:arg-types unsigned-num (:constant (integer 0 31)))
  (:generator 5
    (inst tst x (ash 1 y))))

(define-vop (fast-signum-fixnum fixnum-unop)
  (:args (x :scs (any-reg) :target res))
  (:translate signum)
  (:generator 4
    (move res x)
    (inst cmp x 0)
    (inst mov :ne res (fixnumize 1))
    (inst mvn :mi res (lognot (fixnumize -1)))))

(define-vop (fast-signum-signed signed-unop)
  (:args (x :scs (signed-reg) :target res))
  (:translate signum)
  (:generator 5
    (move res x)
    (inst cmp x 0)
    (inst mov :ne res 1)
    (inst mvn :mi res 0)))

(define-vop (fast-signum-unsigned unsigned-unop)
  (:args (x :scs (unsigned-reg) :target res))
  (:translate signum)
  (:generator 5
    (move res x)
    (inst cmp x 0)
    (inst mov :ne res 1)))

;; Specialised mask-signed-field VOPs.
(define-vop (mask-signed-field-word/c)
  (:translate sb-c::mask-signed-field)
  (:policy :fast-safe)
  (:args (x :scs (signed-reg unsigned-reg) :target r))
  (:arg-types (:constant (integer 0 32)) untagged-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:info width)
  (:generator 3
    (cond ((zerop width)
           (inst mov r 0))
          ((= width 32)
           (move r x))
          (t
           (let ((delta (- n-word-bits width)))
             (inst mov r (lsl x delta))
             (inst mov r (asr r delta)))))))

(define-vop (mask-signed-field-bignum/c)
  (:translate sb-c::mask-signed-field)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target r))
  (:arg-types (:constant (integer 0 32)) bignum)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:info width)
  (:generator 4
    (cond ((zerop width)
           (inst mov r 0))
          (t
           (loadw r x bignum-digits-offset other-pointer-lowtag)
           (let ((delta (- n-word-bits width)))
             (inst mov r (lsl r delta))
             (inst mov r (asr r delta)))))))
;;;; Bignum stuff.

(define-vop (bignum-length get-header-data)
  (:translate sb-bignum:%bignum-length)
  (:policy :fast-safe))

(define-vop (bignum-set-length set-header-data)
  (:translate sb-bignum:%bignum-set-length)
  (:policy :fast-safe))

(define-full-reffer bignum-ref * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb-bignum:%bignum-ref)

(define-full-setter bignum-set * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb-bignum:%bignum-set)

(define-vop (digit-0-or-plus)
  (:translate sb-bignum:%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:conditional)
  (:info target not-p)
  (:generator 2
     (inst cmp digit 0)
     (inst b (if not-p :lt :ge) target)))

(define-vop (add-w/carry)
  (:translate sb-bignum:%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg))
            (carry :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num positive-fixnum)
  (:generator 3
    (inst cmp c 1) ;; Set carry if (fixnum 0 or 1) c=0, else clear.
    (inst adcs result a b)
    (inst mov :cs carry 1)
    (inst mov :cc carry 0)))

(define-vop (sub-w/borrow)
  (:translate sb-bignum:%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg))
            (borrow :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (inst cmp c 1) ;; Set carry if (fixnum 0 or 1) c=0, else clear.
    (inst sbcs result a b)
    (inst mov :cs borrow 1)
    (inst mov :cc borrow 0)))

(define-vop (bignum-mult-and-add-3-arg)
  (:translate sb-bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (unsigned-reg) :to :result)
         (carry-in :scs (unsigned-reg) :target lo))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from :eval)
            (lo :scs (unsigned-reg) :from (:argument 2)))
  (:result-types unsigned-num unsigned-num)
  (:generator 2
    (move lo carry-in)
    (inst mov hi 0)
    (inst umlal lo hi x y)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate sb-bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (unsigned-reg) :to :result)
         (prev :scs (unsigned-reg) :to :eval)
         (carry-in :scs (unsigned-reg) :to :eval))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from :eval)
            (lo :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num unsigned-num)
  (:generator 9
    (inst adds lo prev carry-in)
    (inst mov :cs hi 1)
    (inst mov :cc hi 0)
    (inst umlal lo hi x y)))

(define-vop (bignum-mult)
  (:translate sb-bignum:%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg))
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 1
    (inst umull lo hi x y)))

(define-vop (mulhi)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target hi)
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg) lo)
  (:results (hi :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 20
    (inst umull lo hi x y)))

(define-vop (mulhi/fx)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (any-reg) :target hi)
         (y :scs (unsigned-reg)))
  (:arg-types positive-fixnum unsigned-num)
  (:temporary (:sc any-reg) lo)
  (:temporary (:sc any-reg) temp)
  (:results (hi :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 15
    (inst umull lo temp x y)
    (inst bic hi temp fixnum-tag-mask)))

(define-vop (bignum-lognot lognot-mod32/unsigned=>unsigned)
  (:translate sb-bignum:%lognot))

(define-vop (bignum-floor)
  (:translate sb-bignum:%bigfloor)
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
      (inst sub :hs rem rem divisor)
      (inst adcs quo quo quo)
      (unless (= i 32)
        (inst adc rem rem rem)))))

(define-vop (signify-digit)
  (:translate sb-bignum:%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (if (sc-is res any-reg)
        (inst mov res (lsl digit n-fixnum-tag-bits))
        (inst mov res digit))))

(define-vop (digit-ashr)
  (:translate sb-bignum:%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg))
         (count :scs (unsigned-reg)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst mov result (asr digit count))))

(define-vop (digit-lshr digit-ashr)
  (:translate sb-bignum:%digit-logical-shift-right)
  (:generator 1
    (inst mov result (lsr digit count))))

(define-vop (digit-ashl digit-ashr)
  (:translate sb-bignum:%ashl)
  (:generator 1
    (inst mov result (lsl digit count))))
