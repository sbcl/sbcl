;;;; the VM definition arithmetic VOPs for the PPC

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; Unary operations.

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
    (inst neg res x)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (inst neg res x)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:translate lognot)
  (:generator 2
    (inst xori res x (fixnumize -1))))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 1
    (inst not res x)))

;;;; Binary fixnum operations.

;;; Assume that any constant operand is the second arg...

(define-vop (fast-fixnum-binop fast-safe-arith-op)
  (:args (x :target r :scs (any-reg zero))
	 (y :target r :scs (any-reg zero)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg zero))
	 (y :target r :scs (unsigned-reg zero)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-signed-binop fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg zero))
	 (y :target r :scs (signed-reg zero)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic"))

(define-vop (fast-fixnum-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg zero)))
  (:info y)
  (:arg-types tagged-num
	      (:constant (and (signed-byte 14) (not (integer 0 0)))))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-fixnum-logop-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg zero)))
  (:info y)
  (:arg-types tagged-num
	      (:constant (and (unsigned-byte 14) (not (integer 0 0)))))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum logical op"))

(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg zero)))
  (:info y)
  (:arg-types unsigned-num
	      (:constant (and (signed-byte 16) (not (integer 0 0)))))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-unsigned-logop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg zero)))
  (:info y)
  (:arg-types unsigned-num
	      (:constant (and (unsigned-byte 16) (not (integer 0 0)))))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) logical op"))

(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg zero)))
  (:info y)
  (:arg-types signed-num
	      (:constant (and (signed-byte 16) (not (integer 0 0)))))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic"))

(define-vop (fast-signed-logop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg zero)))
  (:info y)
  (:arg-types signed-num
	      (:constant (and (unsigned-byte 16) (not (integer 0 0)))))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic"))


(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro define-var-binop (translate untagged-penalty op)
  `(progn
     (define-vop (,(symbolicate "FAST-" translate "/FIXNUM=>FIXNUM")
		  fast-fixnum-binop)
       (:translate ,translate)
       (:generator 2
	 (inst ,op r x y))) 
     (define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
		  fast-signed-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
	 (inst ,op r x y))) 
     (define-vop (,(symbolicate "FAST-" translate "/UNSIGNED=>UNSIGNED")
		  fast-unsigned-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
	 (inst ,op r x y)))))


(defmacro define-const-binop (translate untagged-penalty op)
  `(progn
     
     (define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum)
		  fast-fixnum-binop-c)
       (:translate ,translate)
       (:generator 1
	 (inst ,op r x (fixnumize y))))
     (define-vop (,(symbolicate 'fast- translate '-c/signed=>signed)
		  fast-signed-binop-c)
       (:translate ,translate)
       (:generator ,untagged-penalty
	 (inst ,op r x y)))
     (define-vop (,(symbolicate 'fast- translate '-c/unsigned=>unsigned)
		  fast-unsigned-binop-c)
       (:translate ,translate)
       (:generator ,untagged-penalty
	 (inst ,op r x y)))))

(defmacro define-const-logop (translate untagged-penalty op)
  `(progn
     
     (define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum)
		  fast-fixnum-logop-c)
       (:translate ,translate)
       (:generator 1
	 (inst ,op r x (fixnumize y))))
     (define-vop (,(symbolicate 'fast- translate '-c/signed=>signed)
		  fast-signed-logop-c)
       (:translate ,translate)
       (:generator ,untagged-penalty
	 (inst ,op r x y)))
     (define-vop (,(symbolicate 'fast- translate '-c/unsigned=>unsigned)
		  fast-unsigned-logop-c)
       (:translate ,translate)
       (:generator ,untagged-penalty
	 (inst ,op r x y)))))

); eval-when

(define-var-binop + 4 add)
(define-var-binop - 4 sub)
(define-var-binop logand 2 and)
(define-var-binop logandc2 2 andc)
(define-var-binop logior 2 or)
(define-var-binop logorc2 2 orc)
(define-var-binop logxor 2 xor)
(define-var-binop logeqv 2 eqv)

(define-const-binop + 4 addi)
(define-const-binop - 4 subi)
(define-const-logop logand 2 andi.)
(define-const-logop logior 2 ori)
(define-const-logop logxor 2 xori)


;;; Special case fixnum + and - that trap on overflow.  Useful when we
;;; don't know that the output type is a fixnum.
;;;
(define-vop (+/fixnum fast-+/fixnum=>fixnum)
  (:policy :safe)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note "safe inline fixnum arithmetic")
  (:generator 4
    (let* ((no-overflow (gen-label)))
      (inst mcrxr :cr0)
      (inst addo. r x y)
      (inst bns no-overflow)
      (inst unimp (logior (ash (reg-tn-encoding r) 5)
			  fixnum-additive-overflow-trap))
      (emit-label no-overflow))))


(define-vop (-/fixnum fast--/fixnum=>fixnum)
  (:policy :safe)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note "safe inline fixnum arithmetic")
  (:generator 4
    (let* ((no-overflow (gen-label)))
      (inst mcrxr :cr0)
      (inst subo. r x y)
      (inst bns no-overflow)
      (inst unimp (logior (ash (reg-tn-encoding r) 5)
			  fixnum-additive-overflow-trap))
      (emit-label no-overflow))))


;;; Shifting

(define-vop (fast-ash/unsigned=>unsigned)
  (:note "inline ASH")
  (:args (number :scs (unsigned-reg) :to :save)
	 (amount :scs (signed-reg immediate)))
  (:arg-types (:or unsigned-num) signed-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) ndesc)
  (:generator 3
    (sc-case amount
      (signed-reg
       (let ((positive (gen-label))
	     (done (gen-label)))
	 (inst cmpwi amount 0)
	 (inst neg ndesc amount)
	 (inst bge positive)
	 (inst cmpwi ndesc 31)
	 (inst srw result number ndesc)
	 (inst ble done)
	 (move result zero-tn)
	 (inst b done)

	 (emit-label positive)
	 ;; The result-type assures us that this shift will not overflow.
	 (inst slw result number amount)

	 (emit-label done)))
      (immediate
       (let ((amount (tn-value amount)))
	 (cond
	  ((and (minusp amount) (< amount -31)) (move result zero-tn))
	  ((minusp amount) (inst srwi result number (- amount)))
	  (t (inst slwi result number amount))))))))

(define-vop (fast-ash/signed=>signed)
  (:note "inline ASH")
  (:args (number :scs (signed-reg) :to :save)
	 (amount :scs (signed-reg immediate)))
  (:arg-types (:or signed-num) signed-num)
  (:results (result :scs (signed-reg)))
  (:result-types (:or signed-num))
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) ndesc)
  (:generator 3
    (sc-case amount
      (signed-reg
       (let ((positive (gen-label))
	     (done (gen-label)))
	 (inst cmpwi amount 0)
	 (inst neg ndesc amount)
	 (inst bge positive)
	 (inst cmpwi ndesc 31)
	 (inst sraw result number ndesc)
	 (inst ble done)
	 (inst srawi result number 31)
	 (inst b done)

	 (emit-label positive)
	 ;; The result-type assures us that this shift will not overflow.
	 (inst slw result number amount)

	 (emit-label done)))

      (immediate
       (let ((amount (tn-value amount)))
	 (if (minusp amount)
	     (let ((amount (min 31 (- amount))))
	       (inst srawi result number amount))
	     (inst slwi result number amount)))))))



(define-vop (signed-byte-32-len)
  (:translate integer-length)
  (:note "inline (signed-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg)))
  (:arg-types signed-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :to (:argument 0)) shift)
  (:generator 6
    ; (integer-length arg) = (- 32 (cntlz (if (>= arg 0) arg (lognot arg))))
    (let ((nonneg (gen-label)))
      (inst cntlzw. shift arg)
      (inst bne nonneg)
      (inst not shift arg)
      (inst cntlzw shift shift)
      (emit-label nonneg)
      (inst slwi shift shift 2)
      (inst subfic res  shift (fixnumize 32)))))

(define-vop (unsigned-byte-32-count)
  (:translate logcount)
  (:note "inline (unsigned-byte 32) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg) :target shift))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) shift temp)
  (:generator 30
    (let ((loop (gen-label))
	  (done (gen-label)))
      (inst add. shift zero-tn arg)
      (move res zero-tn)
      (inst beq done)

      (emit-label loop)
      (inst subi temp shift 1)
      (inst and. shift shift temp)
      (inst addi res res (fixnumize 1))
      (inst bne loop)

      (emit-label done))))


;;;; Binary conditional VOPs:

(define-vop (fast-conditional)
  (:conditional)
  (:info target not-p)
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg zero))
	 (y :scs (any-reg zero)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg zero)))
  (:arg-types tagged-num (:constant (signed-byte 14)))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg zero))
	 (y :scs (signed-reg zero)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 32) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg zero)))
  (:arg-types signed-num (:constant (signed-byte 16)))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg zero))
	 (y :scs (unsigned-reg zero)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg zero)))
  (:arg-types unsigned-num (:constant (unsigned-byte 16)))
  (:info target not-p y))


(define-vop (fast-if-</fixnum fast-conditional/fixnum)
  (:translate <)
  (:generator 4
    (inst cmpw x y)
    (inst b? (if not-p :ge :lt) target)))

(define-vop (fast-if-<-c/fixnum fast-conditional-c/fixnum)
  (:translate <)
  (:generator 3
    (inst cmpwi x (fixnumize y))
    (inst b? (if not-p :ge :lt) target)))

(define-vop (fast-if-</signed fast-conditional/signed)
  (:translate <)
  (:generator 6
    (inst cmpw x y)
    (inst b? (if not-p :ge :lt) target)))

(define-vop (fast-if-<-c/signed fast-conditional-c/signed)
  (:translate <)
  (:generator 5
    (inst cmpwi x y)
    (inst b? (if not-p :ge :lt) target)))

(define-vop (fast-if-</unsigned fast-conditional/unsigned)
  (:translate <)
  (:generator 6
    (inst cmplw x y)
    (inst b? (if not-p :ge :lt) target)))

(define-vop (fast-if-<-c/unsigned fast-conditional-c/unsigned)
  (:translate <)
  (:generator 5
    (inst cmplwi x y)
    (inst b? (if not-p :ge :lt) target)))

(define-vop (fast-if->/fixnum fast-conditional/fixnum)
  (:translate >)
  (:generator 4
    (inst cmpw x y)
    (inst b? (if not-p :le :gt) target)))

(define-vop (fast-if->-c/fixnum fast-conditional-c/fixnum)
  (:translate >)
  (:generator 3
    (inst cmpwi x (fixnumize y))
    (inst b? (if not-p :le :gt) target)))

(define-vop (fast-if->/signed fast-conditional/signed)
  (:translate >)
  (:generator 6
    (inst cmpw x y)
    (inst b? (if not-p :le :gt) target)))

(define-vop (fast-if->-c/signed fast-conditional-c/signed)
  (:translate >)
  (:generator 5
    (inst cmpwi x y)
    (inst b? (if not-p :le :gt) target)))

(define-vop (fast-if->/unsigned fast-conditional/unsigned)
  (:translate >)
  (:generator 6
    (inst cmplw x y)
    (inst b? (if not-p :le :gt) target)))

(define-vop (fast-if->-c/unsigned fast-conditional-c/unsigned)
  (:translate >)
  (:generator 5
    (inst cmplwi x y)
    (inst b? (if not-p :le :gt) target)))

(define-vop (fast-if-eql/signed fast-conditional/signed)
  (:translate eql)
  (:generator 6
    (inst cmpw x y)
    (inst b? (if not-p :ne :eq) target)))

(define-vop (fast-if-eql-c/signed fast-conditional-c/signed)
  (:translate eql)
  (:generator 5
    (inst cmpwi x y)
    (inst b? (if not-p :ne :eq) target)))

(define-vop (fast-if-eql/unsigned fast-conditional/unsigned)
  (:translate eql)
  (:generator 6
    (inst cmplw x y)
    (inst b? (if not-p :ne :eq) target)))

(define-vop (fast-if-eql-c/unsigned fast-conditional-c/unsigned)
  (:translate eql)
  (:generator 5
    (inst cmplwi x y)
    (inst b? (if not-p :ne :eq) target)))


;;; EQL/FIXNUM is funny because the first arg can be of any type, not just a
;;; known fixnum.

;;; These versions specify a fixnum restriction on their first arg.  We have
;;; also generic-eql/fixnum VOPs which are the same, but have no restriction on
;;; the first arg and a higher cost.  The reason for doing this is to prevent
;;; fixnum specific operations from being used on word integers, spuriously
;;; consing the argument.
;;;

(define-vop (fast-eql/fixnum fast-conditional)
  (:args (x :scs (any-reg descriptor-reg zero))
	 (y :scs (any-reg zero)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql)
  (:generator 4
    (inst cmpw x y)
    (inst b? (if not-p :ne :eq) target)))
;;;
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg descriptor-reg zero)))
  (:arg-types tagged-num (:constant (signed-byte 14)))
  (:info target not-p y)
  (:translate eql)
  (:generator 2
    (inst cmpwi x (fixnumize y))
    (inst b? (if not-p :ne :eq) target)))
;;;
(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:arg-types * (:constant (signed-byte 11)))
  (:variant-cost 6))


;;;; 32-bit logical operations

(define-vop (merge-bits)
  (:translate merge-bits)
  (:args (shift :scs (signed-reg unsigned-reg))
	 (prev :scs (unsigned-reg))
	 (next :scs (unsigned-reg)))
  (:arg-types tagged-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target result) res)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 4
    (let ((done (gen-label)))
      (inst cmpwi shift 0)
      (inst beq done)
      (inst srw res next shift)
      (inst sub temp zero-tn shift)
      (inst slw temp prev temp)
      (inst or res res temp)
      (emit-label done)
      (move result res))))


(define-vop (32bit-logical)
  (:args (x :scs (unsigned-reg zero))
	 (y :scs (unsigned-reg zero)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe))

(define-vop (32bit-logical-not 32bit-logical)
  (:translate 32bit-logical-not)
  (:args (x :scs (unsigned-reg zero)))
  (:arg-types unsigned-num)
  (:generator 1
    (inst not r x)))

(define-vop (32bit-logical-and 32bit-logical)
  (:translate 32bit-logical-and)
  (:generator 1
    (inst and r x y)))

(deftransform 32bit-logical-nand ((x y) (* *))
  '(32bit-logical-not (32bit-logical-and x y)))

(define-vop (32bit-logical-or 32bit-logical)
  (:translate 32bit-logical-or)
  (:generator 1
    (inst or r x y)))

(deftransform 32bit-logical-nor ((x y) (* *))
  '(32bit-logical-not (32bit-logical-or x y)))

(define-vop (32bit-logical-xor 32bit-logical)
  (:translate 32bit-logical-xor)
  (:generator 1
    (inst xor r x y)))

(define-vop (32bit-logical-eqv 32bit-logical)
  (:translate 32bit-logical-eqv)
  (:generator 1
    (inst eqv r x y)))

(define-vop (32bit-logical-orc2 32bit-logical)
  (:translate 32bit-logical-orc2)
  (:generator 1
    (inst orc r x y)))

(deftransform 32bit-logical-orc1 ((x y) (* *))
  '(32bit-logical-orc2 y x))

(define-vop (32bit-logical-andc2 32bit-logical)
  (:translate 32bit-logical-andc2)
  (:generator 1
    (inst andc r x y)))

(deftransform 32bit-logical-andc1 ((x y) (* *))
  '(32bit-logical-andc2 y x))


(define-vop (shift-towards-someplace)
  (:policy :fast-safe)
  (:args (num :scs (unsigned-reg))
	 (amount :scs (signed-reg)))
  (:arg-types unsigned-num tagged-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (shift-towards-start shift-towards-someplace)
  (:translate shift-towards-start)
  (:note "shift-towards-start")
  (:generator 1
    (inst rlwinm amount amount 0 27 31)
    (inst slw r num amount)))

(define-vop (shift-towards-end shift-towards-someplace)
  (:translate shift-towards-end)
  (:note "shift-towards-end")
  (:generator 1
    (inst rlwinm amount amount 0 27 31)
    (inst srw r num amount)))




;;;; Bignum stuff.

(define-vop (bignum-length get-header-data)
  (:translate sb!bignum::%bignum-length)
  (:policy :fast-safe))

(define-vop (bignum-set-length set-header-data)
  (:translate sb!bignum::%bignum-set-length)
  (:policy :fast-safe))

(define-vop (bignum-ref word-index-ref)
  (:variant sb!vm:bignum-digits-offset sb!vm:other-pointer-lowtag)
  (:translate sb!bignum::%bignum-ref)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (bignum-set word-index-set)
  (:variant sb!vm:bignum-digits-offset sb!vm:other-pointer-lowtag)
  (:translate sb!bignum::%bignum-set)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg immediate zero))
	 (value :scs (unsigned-reg)))
  (:arg-types t positive-fixnum unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (digit-0-or-plus)
  (:translate sb!bignum::%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:generator 3
    (let ((done (gen-label)))
      (inst cmpwi digit 0)
      (move result null-tn)
      (inst blt done)
      (load-symbol result t)
      (emit-label done))))

(define-vop (add-w/carry)
  (:translate sb!bignum::%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
	 (b :scs (unsigned-reg))
	 (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:temporary (:scs (unsigned-reg)) temp)
  (:results (result :scs (unsigned-reg))
	    (carry :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:generator 3
    (inst addic temp c -1)
    (inst adde result a b)
    (inst addze carry zero-tn)))

(define-vop (sub-w/borrow)
  (:translate sb!bignum::%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
	 (b :scs (unsigned-reg))
	 (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:temporary (:scs (unsigned-reg)) temp)
  (:results (result :scs (unsigned-reg))
	    (borrow :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (inst addic temp c -1)
    (inst sube result a b)
    (inst addze borrow zero-tn)))

(define-vop (bignum-mult-and-add-3-arg)
  (:translate sb!bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg))
	 (carry-in :scs (unsigned-reg) :to (:eval 1)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target hi) hi-temp)
  (:temporary (:scs (unsigned-reg) :from (:eval 0) :to (:result 1)
		    :target lo) lo-temp)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 40
    (inst mulhwu hi-temp x y)
    (inst mullw lo-temp x y)
    (inst addc lo lo-temp carry-in)
    (inst addze hi hi-temp)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate sb!bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg))
	 (prev :scs (unsigned-reg) :to (:eval 1))
	 (carry-in :scs (unsigned-reg) :to (:eval 1)))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target hi) hi-temp)
  (:temporary (:scs (unsigned-reg) :from (:eval 0) :to (:result 1)
		    :target lo) lo-temp)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 40
    (inst mulhwu hi-temp x y)
    (inst mullw lo-temp x y)
    (inst addc lo-temp lo-temp carry-in)
    (inst addze hi-temp hi-temp)
    (inst addc lo lo-temp prev)
    (inst addze hi hi-temp)))

(define-vop (bignum-mult)
  (:translate sb!bignum::%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to (:result 1))
	 (y :scs (unsigned-reg) :to (:result 1)))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 40
    (inst mullw lo x y)
    (inst mulhwu hi x y)))

(define-vop (bignum-lognot)
  (:translate sb!bignum::%lognot)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst not r x)))

(define-vop (fixnum-to-digit)
  (:translate sb!bignum::%fixnum-to-digit)
  (:policy :fast-safe)
  (:args (fixnum :scs (any-reg)))
  (:arg-types tagged-num)
  (:results (digit :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst srawi digit fixnum 2)))


(define-vop (bignum-floor)
  (:translate sb!bignum::%floor)
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
	     (inst subi temp guess 1)
	     (inst and temp temp denom)
	     (inst sub rem rem temp))
	   (sltu (res x y)
	     (inst subfc res y x)
	     (inst subfe res res res)
	     (inst neg res res)))
      (sltu quo rem denom)
      (maybe-subtract quo)
      (dotimes (i 32)
	(inst slwi rem rem 1)
	(inst srwi temp rem-low 31)
	(inst or rem rem temp)
	(inst slwi rem-low rem-low 1)
	(sltu temp rem denom)
	(inst slwi quo quo 1)
	(inst or quo quo temp)
	(maybe-subtract)))
    (inst not quo quo)))

#|

(define-vop (bignum-floor)
  (:translate sb!bignum::%floor)
  (:policy :fast-safe)
  (:args (div-high :scs (unsigned-reg) :target rem)
	 (div-low :scs (unsigned-reg) :target quo)
	 (divisor :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (quo :scs (unsigned-reg) :from (:argument 1))
	    (rem :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num unsigned-num)
  (:generator 300
    (inst mtmq div-low)
    (inst div quo div-high divisor)
    (inst mfmq rem)))
|#

(define-vop (signify-digit)
  (:translate sb!bignum::%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (sc-case res
      (any-reg
       (inst slwi res digit 2))
      (signed-reg
       (move res digit)))))


(define-vop (digit-ashr)
  (:translate sb!bignum::%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg))
	 (count :scs (unsigned-reg)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst sraw result digit count)))

(define-vop (digit-lshr digit-ashr)
  (:translate sb!bignum::%digit-logical-shift-right)
  (:generator 1
    (inst srw result digit count)))

(define-vop (digit-ashl digit-ashr)
  (:translate sb!bignum::%ashl)
  (:generator 1
    (inst slw result digit count)))


;;;; Static funs.

(define-static-fun two-arg-gcd (x y) :translate gcd)
(define-static-fun two-arg-lcm (x y) :translate lcm)

(define-static-fun two-arg-+ (x y) :translate +)
(define-static-fun two-arg-- (x y) :translate -)
(define-static-fun two-arg-* (x y) :translate *)
(define-static-fun two-arg-/ (x y) :translate /)

(define-static-fun two-arg-< (x y) :translate <)
(define-static-fun two-arg-<= (x y) :translate <=)
(define-static-fun two-arg-> (x y) :translate >)
(define-static-fun two-arg->= (x y) :translate >=)
(define-static-fun two-arg-= (x y) :translate =)
(define-static-fun two-arg-/= (x y) :translate /=)

(define-static-fun %negate (x) :translate %negate)

(define-static-fun two-arg-and (x y) :translate logand)
(define-static-fun two-arg-ior (x y) :translate logior)
(define-static-fun two-arg-xor (x y) :translate logxor)
