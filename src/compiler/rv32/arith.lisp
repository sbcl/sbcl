;;;; the VM definition of arithmetic VOPs for the RV32

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

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

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (inst sub res zero-tn x)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (inst sub res zero-tn x)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:translate lognot)
  (:generator 1
    (inst xori res x (fixnumize -1))))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 2
    (inst xori res x -1)))

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
  (:arg-types tagged-num (:constant (signed-byte #.(- 12 n-fixnum-tag-bits))))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline arithmetic"))

(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg)))
  (:info y)
  (:arg-types unsigned-num (:constant (unsigned-byte 12)))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline unsigned unboxed arithmetic"))

(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg)))
  (:info y)
  (:arg-types signed-num (:constant (signed-byte 12)))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline signed unboxed arithmetic"))

;;; FIXME: We want to have 32-bit constant binary operations. Just define another vop with a higher translation cost?
(defmacro define-binop (translate cost untagged-cost r-op &optional i-op negate)
  `(progn
     (define-vop (,(symbolicate "FAST-" translate "/FIXNUM=>FIXNUM")
                  fast-fixnum-binop)
       (:translate ,translate)
       (:generator ,(1+ cost)
         (inst ,r-op r x y)))
     (define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
                  fast-signed-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-cost)
         (inst ,r-op r x y)))
     (define-vop (,(symbolicate "FAST-" translate "/UNSIGNED=>UNSIGNED")
                  fast-unsigned-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-cost)
         (inst ,r-op r x y)))
     ,@(when i-op
         (let ((y (if negate 'y '(- y))))
           `((define-vop (,(symbolicate "FAST-" translate "-C/FIXNUM=>FIXNUM")
                          fast-fixnum-binop-c)
               (:translate ,translate)
               (:generator ,cost
                 (inst ,i-op r x (fixnumize ,y))))
             (define-vop (,(symbolicate "FAST-" translate "-C/SIGNED=>SIGNED")
                          fast-signed-binop-c)
               (:translate ,translate)
               (:generator ,untagged-cost
                 (inst ,i-op r x ,y)))
             (define-vop (,(symbolicate "FAST-" translate
                                        "-C/UNSIGNED=>UNSIGNED")
                          fast-unsigned-binop-c)
               (:translate ,translate)
               (:generator ,untagged-cost
                 (inst ,i-op r x ,y))))))))

(define-binop + 1 5 add addi)
(define-binop - 1 5 sub addi t)
(define-binop logior 1 3 or ori)
(define-binop logxor 1 3 xor xori)
(define-binop logand 1 3 and andi)

(define-source-transform logeqv (&rest args)
  (if (oddp (length args))
      `(logxor ,@args)
      `(lognot (logxor ,@args))))
(define-source-transform logandc1 (x y)
  `(logand (lognot ,x) ,y))
(define-source-transform logandc2 (x y)
  `(logand ,x (lognot ,y)))
(define-source-transform logorc1 (x y)
  `(logior (lognot ,x) ,y))
(define-source-transform logorc2 (x y)
  `(logior ,x (lognot ,y)))
(define-source-transform lognor (x y)
  `(lognot (logior ,x ,y)))
(define-source-transform lognand (x y)
  `(lognot (logand ,x ,y)))

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
  (:temporary (:sc non-descriptor-reg :to :eval) temp)
  (:generator 3
    (inst bge amount zero-tn positive)
    (inst addi temp zero-tn 32)
    (inst sub ndesc zero-tn amount)
    (move result zero-tn)
    (inst blt temp ndesc done)
    (inst srl result number ndesc)
    (inst j done)
    
    POSITIVE
    ;; The result-type assures us that this shift will not overflow.
    (inst sll result number amount)

    DONE))

(define-vop (fast-ash/signed=>signed)
  (:note "inline ASH")
  (:args (number :scs (signed-reg) :to :save)
         (amount :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) ndesc)
  (:temporary (:sc non-descriptor-reg :to :eval) temp)
  (:generator 3
    (inst bge amount zero-tn positive)
    (inst sub ndesc zero-tn amount)
    (inst addi temp zero-tn 31)
    (inst sra result number 31)
    (inst blt temp ndesc done)
    (inst sra result number ndesc)
    (inst j done)

    POSITIVE
    ;; The result-type assures us that this shift will not overflow.
    (inst sll result number amount)

    DONE))

(define-vop (fast-ash-c/unsigned=>unsigned)
  (:policy :fast-safe)
  (:translate ash)
  (:note "inline ASH")
  (:args (number :scs (unsigned-reg)))
  (:info count)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (cond
      ((< count -31) (move result zero-tn))
      ((< count 0) (inst srli result number (min (- count) 31)))
      ((> count 0) (inst slli result number (min count 31)))
      (t (bug "identity ASH not transformed away")))))

(define-vop (fast-ash-c/signed=>signed)
  (:policy :fast-safe)
  (:translate ash)
  (:note "inline ASH")
  (:args (number :scs (signed-reg)))
  (:info count)
  (:arg-types signed-num (:constant integer))
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (cond
      ((< count 0) (inst srai result number (min (- count) 31)))
      ((> count 0) (inst slli result number (min count 31)))
      (t (bug "identity ASH not transformed away")))))

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
                      (inst sll result number amount))
                     (immediate
                      (let ((amount (tn-value amount)))
                        (aver (> amount 0))
                        (inst sll result number amount))))))))
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
      (move res zero-tn)
      (inst bge shift zero-tn test)

      (inst xori shift shift -1)
      (inst j test)

      (emit-label loop)
      (inst addi res res (fixnumize 1))

      (emit-label test)
      (inst srli shift shift 1)
      (inst bne shift zero-tn loop))))

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
    (inst li mask #x55555555)
    (inst srli temp arg 1)
    (inst and num arg mask)
    (inst and temp temp mask)
    (inst add num num temp)
    (inst li mask #x33333333)
    (inst srli temp num 2)
    (inst and num num mask)
    (inst and temp temp mask)
    (inst add num num temp)
    (inst li mask #x0f0f0f0f)
    (inst srli temp num 4)
    (inst and num num mask)
    (inst and temp temp mask)
    (inst add num num temp)
    (inst li mask #x00ff00ff)
    (inst srli temp num 8)
    (inst and num num mask)
    (inst and temp temp mask)
    (inst add num num temp)
    (inst li mask #x0000ffff)
    (inst srli temp num 16)
    (inst and num num mask)
    (inst and temp temp mask)
    (inst add res num temp)))

;;; Multiply and Divide.

(define-vop (fast-*/fixnum=>fixnum fast-fixnum-binop)
  (:translate *)
  (:temporary (:scs (non-descriptor-reg) :to :eval) temp)
  (:generator 4
    (inst sra temp y n-fixnum-tag-bits)
    (inst mul r x temp)))

(define-vop (fast-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:generator 3
    (inst mul r x y)))

(define-vop (fast-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:generator 3
    (inst mul r x y)))

(define-vop (fast-truncate/fixnum fast-fixnum-binop)
  (:translate truncate)
  (:results (q :scs (any-reg))
            (r :scs (any-reg)))
  (:result-types tagged-num tagged-num)
  (:temporary (:scs (non-descriptor-reg) :to :eval) temp)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 11
    (inst div temp x y)
    (inst rem r x y)
    ;; Division by zero check performed after division per the ISA
    ;; documentation recommendation.
    (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
      (inst beq y zero-tn zero))
    (inst slli q temp n-fixnum-tag-bits)))

(define-vop (fast-truncate/unsigned fast-unsigned-binop)
  (:translate truncate)
  (:results (q :scs (unsigned-reg))
            (r :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 12
    (inst divu q x y)
    (inst remu r x y)
    (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
      (inst beq y zero-tn zero))))

(define-vop (fast-truncate/signed fast-signed-binop)
  (:translate truncate)
  (:results (q :scs (signed-reg))
            (r :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 12
    (inst div q x y)
    (inst rem r x y)
    (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
      (inst beq y zero-tn zero))))


;;;; Binary conditional VOPs.

;;; Unlike other backends, it is not worth defining vops which
;;; specialize on a constant second argument in RISC-V. No branch
;;; instruction encodes an immediate operand.
(define-vop (fast-conditional)
  (:conditional)
  (:variant-vars condition)
  (:info target not-p)
  (:policy :fast-safe))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:generator 3
    (three-way-comparison x y condition :signed not-p target)))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 32) comparison")
  (:generator 3
    (three-way-comparison x y condition :signed not-p target)))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) comparison")
  (:generator 3
    (three-way-comparison x y condition :unsigned not-p target)))

(defmacro define-conditional-vop (translate op)
  `(progn
     ,@(mapcar (lambda (suffix)
                 (unless (and (eq suffix '/fixnum)
                              (eq translate 'eql))
                   `(define-vop (,(intern (format nil "~:@(FAST-IF-~A~A~)"
                                                  translate suffix))
                                 ,(intern
                                   (format nil "~:@(FAST-CONDITIONAL~A~)"
                                           suffix)))
                      (:translate ,translate)
                      (:variant ,op))))
               '(/fixnum /signed /unsigned))))

(define-conditional-vop < :lt)
(define-conditional-vop > :gt)
(define-conditional-vop eql :eq)

;;; EQL/FIXNUM is funny because the first arg can be of any type, not just a
;;; known fixnum.

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
  (:generator 3
    (if not-p
        (inst bne x y target)
        (inst beq x y target))))

(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg)))
  (:arg-types * tagged-num)
  (:variant-cost 7))


;;;; 32-bit logical operations

(define-vop (shift-towards-someplace)
  (:policy :fast-safe)
  (:args (num :scs (unsigned-reg))
         (amount :scs (signed-reg)))
  (:arg-types unsigned-num tagged-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (shift-towards-start shift-towards-someplace)
  (:translate shift-towards-start)
  (:note "SHIFT-TOWARDS-START")
  (:generator 1
    (inst srl r num amount)))

(define-vop (shift-towards-end shift-towards-someplace)
  (:translate shift-towards-end)
  (:note "SHIFT-TOWARDS-END")
  (:generator 1
    (inst sll r num amount)))


;;;; Modular arithmetic
(macrolet
    ((define-modular-backend (fun &optional constantp)
       (let ((mfun-name (symbolicate fun '-mod32))
             (modvop (symbolicate 'fast- fun '-mod32/unsigned=>unsigned))
             (modcvop (symbolicate 'fast- fun '-mod32-c/unsigned=>unsigned))
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
  (define-modular-backend *))

(define-vop (fast-ash-left-mod32-c/unsigned=>unsigned
             fast-ash-c/unsigned=>unsigned)
    (:translate ash-left-mod32))

(define-vop (fast-ash-left-mod32/unsigned=>unsigned
             fast-ash-left/unsigned=>unsigned))

(deftransform ash-left-mod32 ((integer count)
                              ((unsigned-byte 32) (unsigned-byte 5)))
  (when (sb-c::constant-lvar-p count)
    (sb-c::give-up-ir1-transform))
  '(%primitive fast-ash-left-mod32/unsigned=>unsigned integer count))

(define-modular-fun lognot-mod32 (x) lognot :untagged nil 32)
(define-vop (lognot-mod32/unsigned=>unsigned)
  (:translate lognot-mod32)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (inst xori r x -1)))

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
  (:generator 1
    (if not-p
        (inst blt digit zero-tn target)
        (inst bge digit zero-tn target))))

(define-vop (add-w/carry)
  (:translate sb-bignum:%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target result) res)
  (:results (result :scs (unsigned-reg))
            (carry :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num positive-fixnum)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 5
    (let ((carry-in (gen-label))
          (done (gen-label)))
      (inst add res a b)
      (inst bne c zero-tn carry-in)

      (inst sltu carry res b)
      (inst j done)

      (emit-label carry-in)
      (inst addi res res 1)
      (inst xori temp a -1)
      (inst sltu carry b temp)
      (inst xor carry carry 1)

      (emit-label done)
      (move result res))))

(define-vop (sub-w/borrow)
  (:translate sb-bignum:%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target result) res)
  (:results (result :scs (unsigned-reg))
            (borrow :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (let ((no-borrow-in (gen-label))
          (done (gen-label)))

      (inst sub res a b)
      (inst bne c no-borrow-in)

      (inst addi res res -1)
      (inst sltu borrow b a)
      (inst j done)

      (emit-label no-borrow-in)
      (inst sltu borrow a b)
      (inst xor borrow borrow 1)

      (emit-label done)
      (move result res))))

(define-vop (bignum-mult-and-add-3-arg)
  (:translate sb-bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg))
         (carry-in :scs (unsigned-reg) :to :save))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :from (:argument 1)) temp)
  (:results (hi :scs (unsigned-reg))
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 4
    (inst mulhu hi x y) 
    (inst mul temp x y)
    (inst add lo temp carry-in)
    (inst sltu temp lo carry-in)
    (inst add hi hi temp)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate sb-bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg))
         (prev :scs (unsigned-reg))
         (carry-in :scs (unsigned-reg) :to :save))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :from (:argument 2)) temp)
  (:results (hi :scs (unsigned-reg))
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 8
    (inst mulhu hi x y)
    (inst mul x x y)
    (inst add lo prev carry-in)
    (inst sltu temp lo carry-in)
    (inst add hi hi temp)
    (move temp x)
    (inst add lo lo temp)
    (inst sltu temp lo temp)
    (inst add hi hi temp)))

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
    (inst mulhu hi x y)
    (inst mul lo x y)))

(define-vop (bignum-lognot lognot-mod32/unsigned=>unsigned)
  (:translate sb-bignum:%lognot))

(define-vop (fixnum-to-digit)
  (:translate sb-bignum:%fixnum-to-digit)
  (:policy :fast-safe)
  (:args (fixnum :scs (any-reg)))
  (:arg-types tagged-num)
  (:results (digit :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst srai digit fixnum n-fixnum-tag-bits)))

(define-vop (bignum-floor)
  (:translate sb-bignum:%bigfloor)
  (:policy :fast-safe)
  (:args (num-high :scs (unsigned-reg) :target rem)
         (num-low :scs (unsigned-reg) :target rem-low)
         (denom :scs (unsigned-reg) :to (:eval 1)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :from (:argument 1)) rem-low)
  (:temporary (:scs (unsigned-reg) :from (:eval 0)) temp)
  (:results (quo :scs (unsigned-reg) :from (:eval 0))
            (rem :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num unsigned-num)
  (:generator 325 ; number of inst assuming targeting works.
    (move rem num-high)
    (move rem-low num-low)
    (flet ((maybe-subtract (&optional (guess temp))
             (inst addi temp guess -1)
             (inst and temp denom)
             (inst sub rem rem temp)))
      (inst sltu quo rem denom)
      (maybe-subtract quo)
      (dotimes (i 32)
        (inst slli rem rem 1)
        (inst srli temp rem-low 31)
        (inst or rem rem temp)
        (inst slli rem-low rem-low 1)
        (inst sltu temp rem denom)
        (inst slli quo quo 1)
        (inst or quo quo temp)
        (maybe-subtract)))
    (inst xori quo -1)))

(define-vop (signify-digit)
  (:translate sb-bignum:%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (sc-case res
      (any-reg
       (inst slli res digit n-fixnum-tag-bits))
      (signed-reg
       (move res digit)))))

(define-vop (digit-ashr)
  (:translate sb-bignum:%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg))
         (count :scs (unsigned-reg)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst sra result digit count)))

(define-vop (digit-lshr digit-ashr)
  (:translate sb-bignum:%digit-logical-shift-right)
  (:generator 1
    (inst srl result digit count)))

(define-vop (digit-ashl digit-ashr)
  (:translate sb-bignum:%ashl)
  (:generator 1
    (inst sll result digit count)))
