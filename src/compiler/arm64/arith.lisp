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
  (:note "inline (signed-byte 64) arithmetic")
  (:arg-types signed-num)
  (:result-types signed-num))

(define-vop (unsigned-unop fast-safe-arith-op)
  (:args (x :scs (unsigned-reg)))
  (:results (res :scs (unsigned-reg)))
  (:note "inline (unsigned-byte 64) arithmetic")
  (:arg-types unsigned-num)
  (:result-types unsigned-num))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (inst neg res x)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (inst neg res x)))

(define-vop (fast-negate/unsigned signed-unop)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:translate %negate)
  (:generator 3
    (inst neg res x)))

(define-vop (fast-negate/signed-unsigned signed-unop)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate %negate)
  (:generator 3
    (inst neg res x)))

(define-modular-fun %negate-mod64 (x) %negate :untagged nil 64)
(define-vop (%negate-mod64)
  (:translate %negate-mod64)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 3
    (inst neg r x)))

(define-modular-fun %negate-modfx (x) %negate :tagged t #.n-fixnum-bits)
(define-vop (%negate-modfx fast-negate/fixnum)
  (:translate %negate-modfx))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num)
  (:translate lognot)
  (:generator 1
    (inst eor res x (lognot n-fixnum-tag-bits))))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 2
    (inst mvn res x)))


;;;; Binary fixnum operations.

;;; Assume that any constant operand is the second arg...

(define-vop (fast-fixnum-binop fast-safe-arith-op)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop fast-safe-arith-op)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic"))

(define-vop (fast-signed-binop fast-safe-arith-op)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 64) arithmetic"))

(define-vop (fast-fixnum-binop-c fast-safe-arith-op)
  (:args (x :scs (any-reg)))
  (:info y)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :scs (unsigned-reg)))
  (:info y)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic"))

(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :scs (signed-reg)))
  (:info y)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 64) arithmetic"))

(defun bic-encode-immediate (x)
  (encode-logical-immediate (bic-mask x)))

(defun bic-fixnum-encode-immediate (x)
  (and (fixnump x)
       (encode-logical-immediate (bic-mask (fixnumize x)))))

(defun fixnum-encode-logical-immediate-ignore-tag (integer)
  (and (fixnump integer)
       (let ((f (fixnumize integer)))
         (or (encode-logical-immediate f)
             (encode-logical-immediate (logior f fixnum-tag-mask))))))

(defmacro define-binop (translate untagged-penalty op
                        &key
                          (constant-test 'encode-logical-immediate)
                          (constant-fixnum-test 'fixnum-encode-logical-immediate)
                          (constant-op op)
                          (constant-transform 'identity)
                          (constant-fixnum-transform constant-transform)
                          negative-op)
  `(progn
     (define-vop (,(symbolicate 'fast- translate '/fixnum=>fixnum)
                  fast-fixnum-binop)
       (:translate ,translate)
       (:generator 2
         (inst ,op r x y)))
     (define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum)
                  fast-fixnum-binop-c)
       (:arg-types tagged-num
                   (:constant ,(if (eq constant-fixnum-test 'fixnum)
                                  'fixnum
                                  `(satisfies ,constant-fixnum-test))))
       (:translate ,translate)
       (:generator 1
         (cond ,@(and negative-op
                      `(((minusp y)
                         (let ((imm (,constant-fixnum-transform (fixnumize (- y)))))
                          (inst ,negative-op r x imm)))))
               (t
                (let ((imm (,constant-fixnum-transform (fixnumize y))))
                  (inst ,constant-op r x imm))))))
     (define-vop (,(symbolicate 'fast- translate '/signed=>signed)
                  fast-signed-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
         (inst ,op r x y)))
     (define-vop (,(symbolicate 'fast- translate '-c/signed=>signed)
                  fast-signed-binop-c)
       (:translate ,translate)
       (:arg-types signed-num
                     (:constant (satisfies ,constant-test)))
       (:generator ,untagged-penalty
         (cond ,@(and negative-op
                      `(((minusp (setf y (sb-c::mask-signed-field n-word-bits y)))
                         (inst ,negative-op r x (,constant-transform (- y))))))
               (t
                (inst ,constant-op r x (,constant-transform y))))))
     (define-vop (,(symbolicate 'fast- translate '/unsigned=>unsigned)
                  fast-unsigned-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
         (inst ,op r x y)))
     (define-vop (,(symbolicate 'fast- translate '-c/unsigned=>unsigned)
                  fast-unsigned-binop-c)
       (:translate ,translate)
       (:arg-types unsigned-num
                   (:constant (satisfies ,constant-test)))
       (:generator ,untagged-penalty
         (cond ,@(and negative-op
                      `(((minusp (setf y (sb-c::mask-signed-field n-word-bits y)))
                         (inst ,negative-op r x (,constant-transform (- y))))))
               (t
                (inst ,constant-op r x (,constant-transform y))))))))

(define-binop + 4 add :constant-test abs-add-sub-immediate-p :constant-fixnum-test fixnum-abs-add-sub-immediate-p
  :negative-op sub)
(define-binop - 4 sub :constant-test abs-add-sub-immediate-p :constant-fixnum-test fixnum-abs-add-sub-immediate-p
  :negative-op add)
(define-binop logand 2 and
  :constant-fixnum-test fixnum
  :constant-fixnum-transform (lambda (x)
                               (cond ((encode-logical-immediate x)
                                      x)
                                     ((let ((tagged (logior x fixnum-tag-mask)))
                                        (and (encode-logical-immediate tagged)
                                             tagged)))
                                     ((and (typep x '(unsigned-byte 32))
                                           (encode-logical-immediate x 32))
                                      (setf r (32-bit-reg r))
                                      x)
                                     ((and (typep x '(unsigned-byte 32))
                                           (let ((tagged (logior x fixnum-tag-mask)))
                                             (when (encode-logical-immediate tagged 32)
                                               (setf r (32-bit-reg r))
                                               tagged))))
                                     (t
                                      (load-immediate-word tmp-tn x nil t)))))
(define-binop logior 2 orr)
(define-binop logxor 2 eor)

(define-vop (fast-/unsigned-signed fast-safe-arith-op)
  (:translate -)
  (:args (x :scs (unsigned-reg signed-reg))
         (y :scs (unsigned-reg signed-reg)))
  (:arg-types (:or unsigned-num signed-num) (:or unsigned-num signed-num))
  (:results (r :scs (unsigned-reg signed-reg)))
  (:result-types (:or unsigned-num signed-num))
  (:generator 6
    (inst sub r x y)))

(define-vop (fast+/unsigned-signed fast-/unsigned-signed)
  (:translate +)
  (:generator 6
    (inst add r x y)))

(define-binop logandc2 2 bic
  :constant-test bic-encode-immediate
  :constant-fixnum-test bic-fixnum-encode-immediate
  :constant-op and
  :constant-transform bic-mask)

(define-vop (fast-logandc2/unsigned-signed=>unsigned fast-logandc2/unsigned=>unsigned)
  (:args (x :scs (unsigned-reg))
         (y :scs (signed-reg)))
  (:arg-types unsigned-num signed-num))

;; (define-binop logorc2 2 orn
;;   :constant-test bic-encode-immediate
;;   :constant-fixnum-test bic-fixnum-encode-immediate
;;   :constant-op orr
;;   :constant-transform bic-mask)

(define-vop (fast-logior-unsigned-signed=>signed fast-safe-arith-op)
  (:args (x :scs (unsigned-reg))
         (y :scs (signed-reg)))
  (:arg-types unsigned-num signed-num)
  (:results (r :scs (signed-reg) :from (:argument 1)))
  (:result-types signed-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:translate logior)
  (:generator 3
    (inst orr r x y)))

(define-vop (fast-logior-signed-unsigned=>signed fast-safe-arith-op)
  (:args (x :scs (signed-reg))
         (y :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:results (r :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:translate logior)
  (:generator 3
    (inst orr r x y)))

(define-vop (logior-signed-unsigned=>integer)
  (:args (x :scs (signed-reg))
         (y :scs (unsigned-reg)))
  (:arg-refs x-ref)
  (:arg-types signed-num unsigned-num)
  (:results (r :scs (descriptor-reg)))
  (:translate logior)
  (:temporary (:sc unsigned-reg) low)
  (:temporary (:sc unsigned-reg) header)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:vop-var vop)
  (:policy :fast-safe)
  (:generator 10
    (let ((fixnum (csubtypep (tn-ref-type x-ref) (specifier-type 'fixnum))))
      (assemble ()
        (inst orr low x y)
        (if fixnum
            (inst add r low low)
            (inst adds r low low))
        (inst mov header (bignum-header-for-length 1))
        (inst tbnz x 63 (if fixnum
                            done
                            negative))
        (inst tst low (ash (1- (ash 1 (- n-word-bits
                                         n-positive-fixnum-bits)))
                           n-positive-fixnum-bits))
        (inst b :eq done)
        (inst tbz low 63 allocate)
        (inst mov header (bignum-header-for-length 2))
        (inst b allocate)
        negative
        (unless fixnum
          (inst b :vc DONE))
        allocate
        (with-fixed-allocation
            (r lr nil (+ 2 bignum-digits-offset))
          (storew-pair header 0 low bignum-digits-offset tmp-tn)
          (storew zr-tn tmp-tn 2))
        DONE))))

(define-vop (logior-unsigned-signed=>integer logior-signed-unsigned=>integer)
  (:args (y :scs (unsigned-reg))
         (x :scs (signed-reg)))
  (:arg-refs nil x-ref)
  (:arg-types unsigned-num signed-num))

;;; Multiplication

(define-vop (fast-*/fixnum=>fixnum fast-fixnum-binop)
  (:args (x :scs (any-reg))
         (y :scs (signed-reg immediate))) ;; one operand needs to be untagged
  (:translate *)
  (:generator 2
    (let (value)
      (cond ((and (sc-is y immediate)
                  (= (logcount (abs (setf value (tn-value y)))) 1))
             (let ((shift (1- (integer-length (abs value)))))
               (if (minusp value)
                   (inst neg r (lsl x shift))
                   (inst lsl r x shift))))
            ((and (typep value '(integer 1))
                  (= (logcount (1- value)) 1))
             (inst add r x (lsl x (1- (integer-length (1- value))))))
            ((and (typep value '(integer * -1))
                  (= (logcount (- 1 value)) 1))
             (inst sub r x (lsl x (1- (integer-length (- 1 value))))))
            (t
             (when value
               (load-immediate-word tmp-tn value)
               (setf y tmp-tn))
             (inst mul r x y))))))

(define-vop (fast-*/signed=>signed fast-*/fixnum=>fixnum)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 64) arithmetic")
  (:variant-cost 3))

(define-vop (fast-*/unsigned=>unsigned fast-*/fixnum=>fixnum)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg immediate)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:variant-cost 3))

;;; Division
(define-vop (fast-truncate/signed=>signed fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg) :to :result)
         (y :scs (signed-reg) :to :result))
  (:arg-types signed-num signed-num)
  (:arg-refs nil y-ref)
  (:results (quo :scs (signed-reg) :from :eval)
            (rem :scs (signed-reg) :from :eval))
  (:optional-results rem)
  (:result-types signed-num signed-num)
  (:note "inline (signed-byte 64) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 33
    (when (types-equal-or-intersect (tn-ref-type y-ref)
                                    (specifier-type '(eql 0)))
      (let ((zero (generate-error-code vop 'division-by-zero-error x)))
        (inst cbz y zero)))
    (inst sdiv quo x y)
    (unless (eq (tn-kind rem) :unused)
     (inst msub rem quo y x))))

(define-vop (fast-truncate/unsigned=>unsigned fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (unsigned-reg) :to :result))
  (:arg-types unsigned-num unsigned-num)
  (:arg-refs nil y-ref)
  (:results (quo :scs (unsigned-reg) :from :eval)
            (rem :scs (unsigned-reg) :from :eval))
  (:optional-results rem)
  (:result-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 33
    (when (types-equal-or-intersect (tn-ref-type y-ref)
                                    (specifier-type '(eql 0)))
      (let ((zero (generate-error-code vop 'division-by-zero-error x)))
        (inst cbz y zero)))
    (inst udiv quo x y)
    (unless (eq (tn-kind rem) :unused)
      (inst msub rem quo y x))))

(define-vop (fast-truncate/signed-unsigned=>signed fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg) :to :result)
         (y :scs (unsigned-reg) :to :result))
  (:arg-types signed-num unsigned-num)
  (:arg-refs nil y-ref)
  (:results (quo :scs (signed-reg) :from :eval)
            (rem :scs (signed-reg) :from :eval))
  (:optional-results rem)
  (:result-types signed-num signed-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 34
    (when (types-equal-or-intersect (tn-ref-type y-ref)
                                    (specifier-type '(eql 0)))
      (let ((zero (generate-error-code vop 'division-by-zero-error x)))
        (inst cbz y zero)))
    (inst cmp x 0)
    (inst csneg tmp-tn x x :ge)
    (inst udiv quo tmp-tn y)
    (inst csneg quo quo quo :ge)
    (unless (eq (tn-kind rem) :unused)
      (inst msub rem quo y x))))

(defun power-of-two-p (x)
  (and (typep x 'signed-word)
       (let ((abs (abs x)))
         (and (> abs 1)
              (= (logcount abs) 1)))))

(define-vop (truncate/signed-power-of-two fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg) :to :result))
  (:arg-types signed-num (:constant (satisfies power-of-two-p)))
  (:arg-refs nil y-ref)
  (:info y)
  (:results (quo :scs (signed-reg) :from :eval)
            (rem :scs (signed-reg) :from :eval))
  (:optional-results quo rem)
  (:result-types signed-num signed-num)
  (:note "inline (signed-byte 64) arithmetic")
  (:vop-var vop)
  (:generator 10
    (let* ((abs-y (abs y))
           (shift (1- (integer-length abs-y))))
      (if (eq abs-y 2)
          (cond ((eq (tn-kind rem) :unused)
                 (inst add quo x (lsr x 63))
                 (if (minusp y)
                     (inst neg quo (asr quo 1))
                     (inst asr quo quo 1)))
                ((eq (tn-kind quo) :unused)
                 (inst cmp x 0)
                 (inst and rem x 1)
                 (inst csneg rem rem rem :ge))
                (t
                 (inst cmp x 0)
                 (inst and rem x 1)
                 (inst add quo x (lsr x 63))
                 (inst csneg rem rem rem :ge)
                 (if (minusp y)
                     (inst neg quo (asr quo shift))
                     (inst asr quo quo shift))))
          (cond ((eq (tn-kind rem) :unused)
                 (inst add quo x (add-sub-immediate (1- abs-y)))
                 (inst cmp x 0)
                 (inst csel quo quo x :lt)
                 (if (minusp y)
                     (inst neg quo (asr quo shift))
                     (inst asr quo quo shift)))
                ((eq (tn-kind quo) :unused)
                 (inst negs rem x)
                 (inst and tmp-tn x (1- abs-y))
                 (inst and rem rem (1- abs-y))
                 (inst csneg rem tmp-tn rem :mi))
                ((minusp y)
                 (let ((not-y (add-sub-immediate (lognot y))))
                  (inst negs rem x)
                  (inst and quo x not-y)
                  (inst and rem rem not-y)
                  (inst csneg rem quo rem :mi)
                  (inst add quo x not-y)
                  (inst cmp x 0)
                  (inst csel quo quo x :lt)
                  (inst neg quo (asr quo shift))))
                (t
                 (inst add quo x (add-sub-immediate (1- y)))
                 (inst cmp x 0)
                 (inst csel rem quo x :lt)
                 (inst asr quo rem shift)
                 (inst and rem rem (- y))
                 (inst sub rem x rem)))))))

;;;
(define-vop (fast-lognor/fixnum=>fixnum fast-fixnum-binop)
  (:translate lognor)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:generator 3
    (inst orr r x y)
    (inst eor r r (lognot fixnum-tag-mask))))

(define-vop (fast-logand/signed-unsigned=>unsigned fast-logand/unsigned=>unsigned)
  (:args (x :scs (signed-reg))
         (y :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:translate logand))

(define-vop (fast-logand/unsigned-signed=>unsigned fast-logand/unsigned=>unsigned)
  (:args (x :scs (unsigned-reg))
         (y :scs (signed-reg)))
  (:arg-types unsigned-num signed-num)
  (:translate logand))

(defun logical-immediate-or-word-mask (x)
  (and (integerp x)
       (or (encode-logical-immediate x)
           (= x most-positive-word))))

(define-vop (fast-logand-c/signed-unsigned=>unsigned fast-logand-c/unsigned=>unsigned)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num (:constant (satisfies logical-immediate-or-word-mask)))
  (:translate logand)
  (:generator 2
    (if (= y most-positive-word)
        (move r x)
        (inst and r x y))))

(define-source-transform logeqv (&rest args)
  (if (oddp (length args))
      `(logxor ,@args)
      `(lognot (logxor ,@args))))
(define-source-transform logorc1 (x y)
  `(logior (lognot ,x) ,y))
(define-source-transform logorc2 (x y)
  `(logior ,x (lognot ,y)))

;;; Shifting

(define-vop (fast-ash-right-c/fixnum=>fixnum)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (any-reg)))
  (:info amount)
  (:arg-types tagged-num (:constant (integer * -1)))
  (:results (result :scs (any-reg)))
  (:result-types tagged-num)
  (:temporary (:sc unsigned-reg :target result) temp)
  (:note "inline ASH")
  (:generator 1
    (inst asr temp number (min (- amount) 63))
    (inst and result temp (bic-mask fixnum-tag-mask))))

(define-vop (fast-ash-c/unsigned=>unsigned)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg)))
  (:info amount)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline ASH")
  (:generator 3
    (cond ((< -64 amount 64)
           (if (plusp amount)
               (inst lsl result number amount)
               (inst lsr result number (- amount))))
          (t
           (inst mov result 0)))))

(define-vop (fast-ash-c/signed=>signed)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg)))
  (:info amount)
  (:arg-types signed-num (:constant integer))
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline ASH")
  (:generator 3
    (cond ((< -64 amount 64)
           (if (plusp amount)
               (inst lsl result number amount)
               (inst asr result number (- amount))))
          ((= amount 64)
           (inst mov result 0))
          (t
           (inst asr result number 63)))))

(define-vop (fast-ash/signed/unsigned)
  (:note "inline ASH")
  (:args (number)
         (amount))
  (:results (result))
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) temp)
  (:arg-refs nil amount-ref)
  (:variant-vars variant)
  (:generator 5
    (let ((negative (csubtypep (tn-ref-type amount-ref)
                               (specifier-type `(integer * 0)))))
      (cond
        ((csubtypep (tn-ref-type amount-ref)
                    (specifier-type `(integer -63 63)))
         (inst neg temp amount)
         (unless negative
           (inst lsl result number amount)
           (inst tbz amount 63 done))
         (ecase variant
           (:signed (inst asr result number temp))
           (:unsigned (inst lsr result number temp))))
        ((not negative)
         (inst cmp amount 0)
         (inst csneg temp amount amount :ge)
         (inst cmp temp n-word-bits)
         ;; Only the first 6 bits count for shifts.
         ;; This sets all bits to 1 if AMOUNT is larger than 63,
         ;; cutting the amount to 63.
         (inst csinv temp temp zr-tn :lo)
         (inst lsl result number temp)
         (inst tbz amount 63 done)
         (ecase variant
           (:signed (inst asr result number temp))
           (:unsigned
            (unless (csubtypep (tn-ref-type amount-ref)
                               (specifier-type `(integer -63 *)))
              (inst csel result number zr-tn :lo))
            (inst lsr result result temp))))
        (t
         (inst neg temp amount)
         (inst cmp temp n-word-bits)
         (ecase variant
           (:signed
            (inst csinv temp temp zr-tn :lo)
            (inst asr result number temp))
           (:unsigned
            (inst csel result number zr-tn :lo)
            (inst lsr result result temp))))))
    done))

(define-vop (ash-inverted/signed/unsigned)
  (:note "inline ASH")
  (:args (number)
         (amount))
  (:results (result))
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) temp)
  (:arg-refs nil amount-ref)
  (:variant-vars variant)
  (:generator 5
    (let ((positive (csubtypep (tn-ref-type amount-ref)
                               (specifier-type '(integer 0)))))
      (cond
        ((csubtypep (tn-ref-type amount-ref)
                    (specifier-type `(integer -63 63)))
         (ecase variant
           (:signed (inst asr result number amount))
           (:unsigned (inst lsr result number amount)))
         (unless positive
           (inst tbz amount 63 done)
           (inst neg temp amount)
           (inst lsl result number amount)))
        (positive
         (inst cmp amount n-word-bits)
         (ecase variant
           (:signed
            (inst csinv temp amount zr-tn :lo)
            (inst asr result number temp))
           (:unsigned
            (inst csel result number zr-tn :lo)
            (inst lsr result result amount))))
        (t
         (inst cmp amount 0)
         (inst csneg temp amount amount :ge)
         (inst cmp temp n-word-bits)
         ;; Only the first 6 bits count for shifts.
         ;; This sets all bits to 1 if AMOUNT is larger than 63,
         ;; cutting the amount to 63.
         (inst csinv temp temp zr-tn :lo)
         (ecase variant
           (:signed (inst asr result number temp))
           (:unsigned
            (inst csel result number zr-tn :lo)
            (inst lsr result result temp)))
         (inst tbz amount 63 done)
         (inst lsl result number temp))))
    done))

(define-vop (fast-ash-modfx/signed/unsigned=>fixnum)
  (:note "inline ASH")
  (:translate ash-modfx)
  (:args (number :scs (signed-reg unsigned-reg) :to :save)
         (amount :scs (signed-reg) :to :save :target temp))
  (:arg-types (:or signed-num unsigned-num) signed-num)
  (:results (result :scs (any-reg)))
  (:arg-refs nil amount-ref)
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) temp temp-result)
  (:generator 5
    (inst subs temp amount zr-tn)
    (inst b :ge LEFT)
    (inst neg temp temp)
    (cond ((csubtypep (tn-ref-type amount-ref)
                      (specifier-type `(integer -63 *)))
           (sc-case number
             (signed-reg (inst asr temp-result number temp))
             (unsigned-reg (inst lsr temp-result number temp))))
          (t
           (inst cmp temp n-word-bits)
           (sc-case number
             (signed-reg
              ;; Only the first 6 bits count for shifts.
              ;; This sets all bits to 1 if AMOUNT is larger than 63,
              ;; cutting the amount to 63.
              (inst csinv temp temp zr-tn :lo)
              (inst asr temp-result number temp))
             (unsigned-reg
              (inst csel temp-result number zr-tn :lo)
              (inst lsr temp-result temp-result temp)))))

    (inst b END)
    LEFT
    (cond ((csubtypep (tn-ref-type amount-ref)
                      (specifier-type `(integer * 63)))
           (inst lsl temp-result number temp))
          (t
           (inst cmp temp n-word-bits)
           (inst csel temp-result number zr-tn :lo)
           (inst lsl temp-result temp-result temp)))
    END
    (inst lsl result temp-result n-fixnum-tag-bits)))

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

(define-vop (ash-inverted/signed=>signed ash-inverted/signed/unsigned)
  (:args (number :scs (signed-reg) :to :save)
         (amount :scs (unsigned-reg signed-reg) :to :save))
  (:arg-types signed-num (:or unsigned-num signed-num))
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:translate sb-c::ash-inverted)
  (:variant :signed))

(define-vop (ash-inverted/unsigned=>unsigned ash-inverted/signed/unsigned)
  (:args (number :scs (unsigned-reg) :to :save)
         (amount :scs (unsigned-reg signed-reg) :to :save))
  (:arg-types unsigned-num (:or unsigned-num signed-num))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate sb-c::ash-inverted)
  (:variant :unsigned))

(macrolet ((def (name name-c sc-type type result-type cost)
             `(progn
                (define-vop (,name)
                  (:note "inline ASH")
                  (:translate ash)
                  (:args (number :scs (,sc-type))
                         (amount :scs (signed-reg unsigned-reg)))
                  ;; For modular variants
                  (:variant-vars cut)
                  (:arg-types ,type unsigned-num)
                  (:arg-refs nil amount-ref)
                  (:results (result :scs (,result-type)))
                  (:result-types ,type)
                  (:policy :fast-safe)
                  (:generator ,cost
                    (cond ((and cut
                                (not (csubtypep (tn-ref-type amount-ref)
                                                (specifier-type `(mod ,n-word-bits)))))
                           (inst cmp amount n-word-bits)
                           (cond ((location= amount result)
                                  (inst csel tmp-tn number zr-tn :lo)
                                  (inst lsl result tmp-tn amount))
                                 (t
                                  (inst csel result number zr-tn :lo)
                                  (inst lsl result result amount))))
                          (t
                           (inst lsl result number amount)))))
                (define-vop (,name-c)
                  (:note "inline ASH")
                  (:translate ash)
                  (:args (number :scs (,sc-type)))
                  (:info amount)
                  (:arg-types ,type (:constant unsigned-byte))
                  (:results (result :scs (,result-type)))
                  (:result-types ,type)
                  (:policy :fast-safe)
                  (:generator ,(1- cost)
                    (if (< amount 64)
                        (inst lsl result number amount)
                        (inst mov result 0)))))))
  ;; FIXME: There's the opportunity for a sneaky optimization here, I
  ;; think: a FAST-ASH-LEFT-C/FIXNUM=>SIGNED vop.  -- CSR, 2003-09-03
  (def fast-ash-left/fixnum=>fixnum fast-ash-left-c/fixnum=>fixnum any-reg tagged-num any-reg 2)
  (def fast-ash-left/signed=>signed fast-ash-left-c/signed=>signed signed-reg signed-num signed-reg 3)
  (def fast-ash-left/unsigned=>unsigned fast-ash-left-c/unsigned=>unsigned unsigned-reg unsigned-num unsigned-reg 3))

(define-vop (fast-ash-left-modfx/fixnum=>fixnum
             fast-ash-left/fixnum=>fixnum)
  (:variant t)
  (:translate ash-left-modfx))

(define-vop (fast-ash-left-modfx-c/fixnum=>fixnum
             fast-ash-left-c/fixnum=>fixnum)
  (:translate ash-left-modfx))

(define-vop (fast-ash-left-mod64-c/fixnum=>fixnum
             fast-ash-left-c/fixnum=>fixnum)
  (:translate ash-left-mod64))

(define-vop (fast-ash-left-mod64/fixnum=>fixnum
             fast-ash-left/fixnum=>fixnum)
  (:variant t)
  (:translate ash-left-mod64))

(define-vop (fast-ash-left-mod64/unsigned=>unsigned
             fast-ash-left/unsigned=>unsigned)
  (:variant t)
  (:translate ash-left-mod64))

(define-vop (fast-ash-left-mod64-c/unsigned=>unsigned
             fast-ash-left-c/unsigned=>unsigned)
  (:translate ash-left-mod64))

(define-vop (fast-ash-modfx/signed=>signed
             fast-ash/signed=>signed)
  (:translate ash-modfx))

(define-vop (fast-ash-mod64/signed=>unsigned
             fast-ash/signed=>signed)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash-mod64))

(define-vop (fast-ash-mod64/unsigned=>unsigned
             fast-ash/unsigned=>unsigned)
  (:translate ash-mod64))

;;; Only the lower 6 bits of the shift amount are significant.
(macrolet ((define (translate operation)
             `(define-vop ()
                (:translate ,translate)
                (:note ,(string translate))
                (:policy :fast-safe)
                (:args (num :scs (unsigned-reg))
                       (amount :scs (signed-reg)))
                (:arg-types unsigned-num tagged-num)
                (:results (r :scs (unsigned-reg)))
                (:result-types unsigned-num)
                (:generator 1
                  (inst ,operation r num amount)))))
  (define shift-towards-start lsr)
  (define shift-towards-end   lsl))

(define-vop (signed-byte-64-len)
  (:translate integer-length)
  (:note "inline (signed-byte 64) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg) :target temp))
  (:arg-types signed-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) temp)
  (:generator 5
    (inst cls temp arg)
    (inst mov res (fixnumize 63))
    (inst sub res res (lsl temp n-fixnum-tag-bits))))

(define-vop (unsigned-byte-64-len)
  (:translate integer-length)
  (:note "inline (unsigned-byte 64) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg) :target temp))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) temp)
  (:generator 5
    (inst clz temp arg)
    (inst mov res (fixnumize 64))
    (inst sub res res (lsl temp n-fixnum-tag-bits))))

(define-vop (unsigned-byte-64-count)
  (:translate logcount)
  (:note "inline (unsigned-byte 64) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg any-reg)))
  (:arg-types (:or unsigned-num positive-fixnum))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (double-reg)) v)
  (:variant-vars signed)
  (:generator 29
    (when signed
      (sc-case arg
        (any-reg
         ;; Don't invert the tag bit
         (inst asr res arg n-fixnum-tag-bits)
         (setf arg res))
        (t))
      ;; Invert when negative
      (inst eor res arg (asr arg 63))
      (setf arg res))
    (inst fmov v arg)
    (inst cnt v v :8b)
    ;; GCC uses (inst addv v v :8b)
    ;; but clang uses:
    (inst uaddlv v :h v :8b)
    (inst fmov res v)))

(define-vop (signed-byte-64-count unsigned-byte-64-count)
    (:note "inline (signed-byte 64) logcount")
    (:args (arg :scs (signed-reg any-reg)))
    (:arg-types (:or signed-num fixnum))
    (:variant t)
    (:variant-cost 30))


(deftransform %ldb ((size posn integer) (:or (((constant-arg (integer 1 #.(1- n-word-bits)))
                                               (constant-arg integer)
                                               word) unsigned-byte)
                                             (((constant-arg (integer 1 #.(1- n-word-bits)))
                                               (constant-arg integer)
                                               signed-word) unsigned-byte)) *
                    :vop t)
  (and (plusp (lvar-value posn))
       (or (= (lvar-value size) 1)
           (<= (+ (lvar-value size)
                  (lvar-value posn))
               n-word-bits))))

(define-vop (ldb-c/fixnum)
  (:translate %ldb)
  (:args (x :scs (any-reg)))
  (:arg-types (:constant integer) (:constant integer) tagged-num)
  (:info size posn)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 2
    (cond ((<= (+ posn size) n-fixnum-bits)
           (inst ubfm res x (1+ posn) (+ posn size)))
          ((= size 1)
           (inst lsr res x n-fixnum-bits))
          (t
           ;; Can't constrain two constant args to avoid this VOP and
           ;; go to the signed variant, so do it manually.
           (inst asr res x (1+ posn))
           (inst and res res (ash most-positive-word (- size sb-vm:n-word-bits)))))))

(define-vop (ldb-c)
  (:translate %ldb)
  (:args (x :scs (unsigned-reg signed-reg)))
  (:arg-types (:constant integer) (:constant integer) (:or unsigned-num signed-num))
  (:info size posn)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 3
    (if (and (>= (+ posn size) n-word-bits)
             (= size 1))
        (inst lsr res x (1- n-word-bits))
        (inst ubfm res x posn (+ posn size -1)))))

(deftransform %dpb ((new size posn integer) (:or ((word
                                                   (constant-arg (integer 1 #.(1- n-word-bits)))
                                                   (constant-arg (mod #.n-word-bits))
                                                   word) *)
                                                 ((signed-word
                                                   (constant-arg (integer 1 #.(1- n-word-bits)))
                                                   (constant-arg (mod #.n-word-bits))
                                                   signed-word) *)) *
                    :vop t)
  (not (and (constant-lvar-p new)
            (let* ((size (lvar-value size))
                   (new (ldb (byte size 0) (lvar-value new))))
              (or (zerop new)
                  (= (logcount new) size))))))

(define-vop (dpb-c/fixnum)
  (:translate %dpb)
  (:args (x :scs (signed-reg) :to :save)
         (y :scs (any-reg) :target res))
  (:arg-types signed-num
              (:constant integer) (:constant integer)
              tagged-num)
  (:info size posn)
  (:results (res :scs (any-reg)))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:generator 2
    (move res y)
    (inst bfm res x (- (1- n-word-bits) posn) (1- (if (>= (+ size posn)
                                                          n-word-bits)
                                                      (- (1- n-word-bits) posn)
                                                      size)))))

(define-vop (dpb-c/signed)
  (:translate %dpb)
  (:args (x :scs (signed-reg) :to :save)
         (y :scs (signed-reg) :target res))
  (:arg-types signed-num
              (:constant integer) (:constant integer)
              signed-num)
  (:info size posn)
  (:results (res :scs (signed-reg)))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:generator 3
    (move res y)
    (inst bfm res x (if (= posn 0)
                        0
                        (- n-word-bits posn)) (1- (if (>= (+ size posn)
                                                          n-word-bits)
                                                      (- n-word-bits posn)
                                                      size)))))

(define-vop (dpb-c/unsigned)
  (:translate %dpb)
  (:args (x :scs (unsigned-reg) :to :save)
         (y :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num
              (:constant integer) (:constant integer)
              unsigned-num)
  (:info size posn)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 3
    (move res y)
    (inst bfm res x (if (= posn 0)
                        0
                        (- n-word-bits posn)) (1- (if (>= (+ size posn)
                                                          n-word-bits)
                                                      (- n-word-bits posn)
                                                      size)))))

;;; Modular functions
(define-modular-fun lognot-mod64 (x) lognot :untagged nil 64)
(define-vop (lognot-mod64/unsigned=>unsigned)
  (:translate lognot-mod64)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (inst mvn res x)))

(defmacro define-mod-binop ((name prototype) function)
  `(define-vop (,name ,prototype)
     (:args (x :scs (unsigned-reg signed-reg))
            (y :scs (unsigned-reg signed-reg)))
     (:arg-types untagged-num untagged-num)
     (:results (r :scs (unsigned-reg signed-reg) :from (:argument 0)))
     (:result-types unsigned-num)
     (:translate ,function)))

(defmacro define-mod-binop-c ((name prototype) function)
  `(define-vop (,name ,prototype)
     (:args (x :scs (unsigned-reg signed-reg)))
     (:info y)
     (:arg-types untagged-num (:constant (satisfies abs-add-sub-immediate-p)))
     (:results (r :scs (unsigned-reg signed-reg) :from (:argument 0)))
     (:result-types unsigned-num)
     (:translate ,function)))

(macrolet ((def (name -c-p)
             (let ((fun64   (symbolicate name "-MOD64"))
                   (funfx   (symbolicate name "-MODFX"))
                   (vopu    (symbolicate "FAST-" name "/UNSIGNED=>UNSIGNED"))
                   (vopcu   (symbolicate "FAST-" name "-C/UNSIGNED=>UNSIGNED"))
                   (vopf    (symbolicate "FAST-" name "/FIXNUM=>FIXNUM"))
                   (vopcf   (symbolicate "FAST-" name "-C/FIXNUM=>FIXNUM"))
                   (vop64u  (symbolicate "FAST-" name "-MOD64/WORD=>UNSIGNED"))
                   (vop64f  (symbolicate "FAST-" name "-MOD64/FIXNUM=>FIXNUM"))
                   (vop64cu (symbolicate "FAST-" name "-MOD64-C/WORD=>UNSIGNED"))
                   (vopfxf  (symbolicate "FAST-" name "-MODFX/FIXNUM=>FIXNUM"))
                   (vopfxcf (symbolicate "FAST-" name "-MODFX-C/FIXNUM=>FIXNUM")))
               `(progn
                  (define-modular-fun ,fun64 (x y) ,name :untagged nil 64)
                  (define-modular-fun ,funfx (x y) ,name :tagged t ,n-fixnum-bits)
                  (define-mod-binop (,vop64u ,vopu) ,fun64)
                  (define-vop (,vop64f ,vopf) (:translate ,fun64))
                  (define-vop (,vopfxf ,vopf) (:translate ,funfx))
                  ,@(when -c-p
                      `((define-mod-binop-c (,vop64cu ,vopcu) ,fun64)
                        (define-vop (,vopfxcf ,vopcf) (:translate ,funfx))))))))
  (def + t)
  (def - t)
  (def * nil))

;;;; Binary conditional VOPs:

(define-vop (fast-conditional)
  (:conditional :eq)
  (:policy :fast-safe))

(defun fixnum-abs-add-sub-immediate-p (n)
  (fixnum-add-sub-immediate-p (abs n)))

(defun abs-add-sub-immediate-p (n)
  ;; In the cross-compiler, stack traces such as the following can occur:
  ;; 0: (HOST-SB-KERNEL:%NEGATE #.(MAKE-DOUBLE-FLOAT #x7FEFFFFF #xFFFFFFFF))
  ;; 1: (SB-VM::ABS-ADD-SUB-IMMEDIATE-P #.(MAKE-DOUBLE-FLOAT #x7FEFFFFF #xFFFFFFFF))
  ;; 2: ((LABELS RECURSE :IN CROSS-TYPEP) #.(MAKE-DOUBLE-FLOAT #x7FEFFFFF #xFFFFFFFF)
  ;;     #<HAIRY-TYPE (SATISFIES SB-VM::ABS-ADD-SUB-IMMEDIATE-P)>)
  ;; 3: (CTYPEP #.(MAKE-DOUBLE-FLOAT #x7FEFFFFF #xFFFFFFFF)
  ;;     #<HAIRY-TYPE (SATISFIES SB-VM::ABS-ADD-SUB-IMMEDIATE-P)>)
  ;; 4: (SB-C::CHECK-ARG-TYPE #<SB-C::LVAR 1 {1008A71AD3}>
  ;;     #<CONSTANT-TYPE (CONSTANT-ARG (SATISFIES SB-VM::ABS-ADD-SUB-IMMEDIATE-P))> 2)
  ;; The target compiler will avoid using this predicate, for the wrong reason.
  ;; It avoids it because it thinks it's not foldable.
  ;; If it did call it, then it would get a bogus (but permissible) answer, namely:
  ;;  (ctypep 0 (specifier-type '(satisfies abs-add-sub-immediate-p))) => NIL,NIL
  (and (integerp n)
       (or (add-sub-immediate-p (abs n))
           (add-sub-immediate-p (ldb (byte n-word-bits 0) (- n))))))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 64) comparison"))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 64) comparison"))

(defmacro define-conditional-vop (tran signed unsigned &optional addend addend-signed addend-unsigned)
  `(progn
     ,@(loop for (suffix cost signed-p) in
             '((/fixnum 4 t)
               (/signed 6 t)
               (/unsigned 6 nil))
             collect
             `(define-vop (,(intern (format nil "~:@(FAST-IF-~A~A~)" tran suffix))
                           ,(intern (format nil "~:@(FAST-CONDITIONAL~A~)" suffix)))
                (:translate ,tran)
                (:conditional ,(if signed-p signed unsigned))
                (:generator ,cost
                  (inst cmp x y))))

     (define-vop (,(symbolicate "FAST-IF-" tran "-INTEGER/C") )
       (:translate ,tran)
       (:args (x :scs (any-reg signed-reg unsigned-reg)))
       (:arg-types (:or tagged-num signed-num unsigned-num)
                   (:constant (or signed-word word)))
       (:info y)
       (:vop-var vop)
       (:policy :fast-safe)
       (:conditional ,signed)
       (:generator 2
         ,(unless (eq signed unsigned)
            `(flet ((try (y)
                      (let ((y (if (sc-is x any-reg)
                                   (fixnumize y)
                                   y)))
                        (flet ((try (constant)
                                 (add-sub-immediate-p constant)))
                          (or (try y)
                              (try (ldb (byte 64 0) (- y))))))))
               (change-vop-flags
                vop
                (cond ((or (zerop (+ y ,addend))
                           (and (not (try y))
                                (try (+ y ,addend))))
                       (setf y (+ y ,addend))
                       (if (sc-is x unsigned-reg)
                           '(,addend-unsigned)
                           '(,addend-signed)))
                      (t
                       (if (sc-is x unsigned-reg)
                           '(,unsigned)
                           '(,signed)))))))
         (let ((y (if (sc-is x any-reg)
                      (fixnumize y)
                      y)))
           (flet ((try (constant negate)
                    (when (add-sub-immediate-p constant)
                      (if negate
                          (inst cmn x constant)
                          (inst cmp x constant))
                      t)))
             (or (try y nil)
                 (try (ldb (byte 64 0) (- y)) t)
                 (inst cmp x (load-immediate-word tmp-tn y)))))))))

(define-conditional-vop < :lt :lo -1 :le :ls)
(define-conditional-vop > :gt :hi 1 :ge :hs)
(define-conditional-vop eql :eq :eq)

(define-vop (<-unsigned-signed)
  (:translate <)
  (:args (unsigned :scs (unsigned-reg))
         (signed :scs (signed-reg)))
  (:arg-types unsigned-num signed-num)
  (:conditional :lo)
  (:policy :fast-safe)
  (:generator 7
    (inst cmp signed 0)
    (inst ccmp unsigned signed :ge #b0010)))

(define-vop (>-unsigned-signed <-unsigned-signed)
  (:translate >)
  (:conditional :hi))

(define-vop (<-signed-unsigned)
  (:translate <)
  (:args (signed :scs (signed-reg))
         (unsigned :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:conditional :lo)
  (:policy :fast-safe)
  (:generator 7
    (inst cmp signed 0)
    (inst ccmp signed unsigned :ge #b0000)))

(define-vop (>-signed-unsigned <-signed-unsigned)
  (:translate >)
  (:conditional :hi))

(define-vop (eql-unsigned-signed)
  (:translate eql)
  (:args (unsigned :scs (unsigned-reg))
         (signed :scs (signed-reg)))
  (:arg-types unsigned-num signed-num)
  (:conditional :eq)
  (:policy :fast-safe)
  (:generator 7
    (inst cmp signed 0)
    (inst ccmp unsigned signed :ge #b0000)))

(define-vop (eql-signed-unsigned eql-unsigned-signed)
  (:args (signed :scs (signed-reg))
         (unsigned :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num))

(define-vop (eq-unsigned-signed eql-unsigned-signed)
  (:variant-cost 6)
  (:translate eq))

(define-vop (eq-signed-unsigned eql-signed-unsigned)
  (:variant-cost 6)
  (:translate eq))

(define-vop (generic-eql/fixnum fast-if-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg)))
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (generic-eql-c/fixnum fast-if-eql-integer/c)
  (:args (x :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant (satisfies fixnum-add-sub-immediate-p)))
  (:variant-cost 6))

(deftransform logtest ((x y) (:or ((signed-word signed-word) *)
                                  ((word word) *)
                                  ((signed-word word) *)
                                  ((word signed-word) *)) * :vop t)
  t)

(define-vop (fast-logtest)
  (:translate logtest)
  (:args (x :scs (signed-reg unsigned-reg any-reg))
         (y :scs (signed-reg unsigned-reg any-reg)))
  (:arg-types (:or signed-num unsigned-num tagged-num)
              (:or signed-num unsigned-num tagged-num))
  (:policy :fast-safe)
  (:conditional :ne)
  (:generator 3
    (cond ((and (sc-is x any-reg)
                (not (sc-is y any-reg)))
           (inst tst y (asr x n-fixnum-tag-bits)))
          ((and (sc-is y any-reg)
                (not (sc-is x any-reg)))
           (inst tst x (asr y n-fixnum-tag-bits)))
          (t
           (inst tst x y)))))

(define-vop (fast-logtest-c)
  (:translate logtest)
  (:args (x :scs (any-reg signed-reg unsigned-reg)))
  (:arg-types (:or tagged-num signed-num unsigned-num)
              (:constant (or signed-word word)))
  (:info y)
  (:policy :fast-safe)
  (:conditional :ne)
  (:generator 2
    (block nil
      (if (= y most-positive-word)
          (inst cmp x 0)
          (let ((y (if (sc-is x any-reg)
                       (cond ((fixnump y)
                              (fixnumize y))
                             ((let ((y (ldb (byte 64 0) y)))
                                ;; Only a negative fixnum will match that bit,
                                ;; so if the next bit is also 1 it can be shifted left.
                                (when (and (= (ldb (byte 1 62) y) 1)
                                           (encode-logical-immediate
                                            (ldb (byte 64 0) (ash y 1))))
                                  (return (inst tst x (ldb (byte 64 0) (ash y 1)))))))
                             (t
                              (return
                                (inst tst (load-immediate-word tmp-tn y)
                                      (asr x n-fixnum-tag-bits)))))
                       y)))
            (if (or (encode-logical-immediate y)
                    (and (sc-is x any-reg)
                         (let ((masked (logior y fixnum-tag-mask)))
                           (and (encode-logical-immediate masked)
                                (setf y masked)))))
                (inst tst x y)
                (inst tst x (load-immediate-word tmp-tn y))))))))

(define-source-transform lognand (x y)
  `(lognot (logand ,x ,y)))

(deftransform logbitp ((x y) (:or (((constant-arg (mod #.n-word-bits)) signed-word) *)
                                  (((constant-arg (mod #.n-word-bits)) word) *)) * :vop t)
  t)

(define-vop ()
  (:translate logbitp)
  (:policy :fast-safe)
  (:args (x :scs (any-reg signed-reg unsigned-reg)))
  (:info y)
  (:arg-types (:constant (mod #.n-word-bits)) (:or tagged-num signed-num unsigned-num))
  (:conditional :ne)
  (:generator 2
    (inst tst x (ash 1 (min (if (sc-is x any-reg)
                                (+ y n-fixnum-tag-bits)
                                y)
                         (1- n-word-bits))))))

;; Specialised mask-signed-field VOPs.
(define-vop (mask-signed-field-word/c)
  (:translate sb-c::mask-signed-field)
  (:policy :fast-safe)
  (:args (x :scs (signed-reg unsigned-reg)))
  (:arg-types (:constant (integer 0 64)) untagged-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:info width)
  (:generator 3
    (aver (/= width 0))
    (cond ((= width 64)
           (move r x))
          (t
           (inst sbfm r x 0 (1- width))))))

(define-vop (mask-signed-field-bignum/c)
  (:translate sb-c::mask-signed-field)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:arg-types (:constant (integer 0 64)) bignum)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:info width)
  (:generator 4
    (aver (/= width 0))
    (loadw r x bignum-digits-offset other-pointer-lowtag)
    (inst sbfm r r 0 (1- width))))

(define-vop (mask-signed-field-fixnum)
  (:translate sb-c::mask-signed-field)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target r))
  (:arg-types (:constant (eql #.n-fixnum-bits)) t)
  (:results (r :scs (any-reg)))
  (:result-types fixnum)
  (:info width)
  (:ignore width)
  (:generator 5
    (move r x)
    (inst tbz r 0 DONE)
    (loadw tmp-tn r bignum-digits-offset other-pointer-lowtag)
    (inst lsl r tmp-tn (- n-word-bits n-fixnum-bits))
    DONE))

(define-vop (logand-word-mask)
  (:translate logand)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:arg-types t (:constant word))
  (:results (r :scs (unsigned-reg)))
  (:info mask)
  (:result-types unsigned-num)
  (:generator 10
    (inst asr r x n-fixnum-tag-bits)
    (inst tbz x 0 AND)
    (loadw r x bignum-digits-offset other-pointer-lowtag)
    AND
    (cond ((= mask most-positive-word))
          ((encode-logical-immediate mask)
           (inst and r r mask))
          (t
           (inst and r r (load-immediate-word tmp-tn mask))))
    DONE))

;;;; Bignum stuff.

(define-vop (bignum-length get-header-data)
  (:translate sb-bignum:%bignum-length)
  (:results (res :scs (unsigned-reg any-reg)))
  (:policy :fast-safe)
  (:generator 6
    (loadw res x 0 other-pointer-lowtag)
    #.(assert (zerop (ash bignum-widetag
                       (- n-fixnum-tag-bits n-widetag-bits))))
    (inst lsr res res (if (sc-is res any-reg)
                          (- n-widetag-bits n-fixnum-tag-bits)
                          n-widetag-bits))))

(define-vop (bignum-set-length set-header-data)
  (:translate sb-bignum:%bignum-set-length)
  (:policy :fast-safe))

(define-full-reffer bignum-ref * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb-bignum:%bignum-ref)

(define-full-setter bignum-set * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb-bignum:%bignum-set)

(define-partial-reffer sb-c::%half-bignum-ref bignum
  :word nil bignum-digits-offset other-pointer-lowtag (unsigned-reg signed-reg)
  positive-fixnum
  sb-bignum:%half-bignum-ref)

(define-partial-setter sb-c::%half-bignum-set bignum
  :word bignum-digits-offset other-pointer-lowtag (unsigned-reg signed-reg)
  positive-fixnum
  (setf sb-bignum:%half-bignum-ref))


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
         (c :scs (unsigned-reg any-reg immediate)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg))
            (carry :scs (unsigned-reg) :from :eval))
  (:optional-results carry)
  (:result-types unsigned-num positive-fixnum)
  (:generator 3
    (cond ((and (sc-is c immediate)
                (zerop (tn-value c)))
           (inst adds result a b))
          (t
           (inst cmp c 1)
           (inst adcs result a b)))
    (unless (eq (tn-kind carry) :unused)
      (inst cset carry :cs))))

(define-vop (sub-w/borrow)
  (:translate sb-bignum:%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (unsigned-reg any-reg immediate)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg))
            (borrow :scs (unsigned-reg) :from :eval))
  (:optional-results borrow)
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (cond ((and (sc-is c immediate)
                (eql (tn-value c) 1))
           (inst subs result a b))
          (t
           (inst cmp c 1)
           (inst sbcs result a b)))

    (unless (eq (tn-kind borrow) :unused)
      (inst cset borrow :cs))))

(define-vop (bignum-add-loop)
  (:args (a* :scs (descriptor-reg))
         (b* :scs (descriptor-reg))
         (la :scs (unsigned-reg))
         (lb :scs (unsigned-reg))
         (r* :scs (descriptor-reg)))
  (:arg-types bignum bignum unsigned-num bignum unsigned-num)
  (:temporary (:sc unsigned-reg) length)
  (:temporary (:sc unsigned-reg) n index digit-a digit-b a b r)
  (:generator 10
    (inst add-sub a a* #1=(- (* bignum-digits-offset n-word-bytes) other-pointer-lowtag))
    (inst add-sub b b* #1#)
    (inst add-sub r r* #1#)
    (move length lb)

    (inst adds index zr-tn zr-tn) ;; clear the carry flag

    LOOP-B
    (inst ldr digit-b (@ b (extend index :lsl 3)))
    (inst ldr digit-a (@ a (extend index :lsl 3)))
    (inst adcs n digit-a digit-b)
    (inst str n (@ r (extend index :lsl 3)))
    (inst add index index 1)
    (inst sub length length 1)
    (inst cbnz length LOOP-B)

    (inst asr digit-b digit-b 63) ;; has the last digit of B
    (inst sub length la lb)
    (inst cbz length DONE)

    ;; Add the sign digit with carry to the remaining digits of the longest bignum
    LOOP-A
    (inst ldr digit-a (@ a (extend index :lsl 3)))
    (inst adcs n digit-a digit-b)
    (inst str n (@ r (extend index :lsl 3)))

    (inst add index index 1)
    (inst sub length length 1)
    (inst cbnz length LOOP-A)

    DONE
    (inst asr digit-a digit-a 63) ;; has the last digit of A
    (inst adc n digit-a digit-b)
    (inst str n (@ r (extend index :lsl 3)))))

(define-vop (bignum-add-word-loop)
  (:args (a* :scs (descriptor-reg))
         (b :scs (unsigned-reg))
         (la :scs (unsigned-reg))
         (r* :scs (descriptor-reg)))
  (:arg-types bignum unsigned-num unsigned-num bignum)
  (:temporary (:sc unsigned-reg) length)
  (:temporary (:sc unsigned-reg) n index digit-a digit-b a r)
  (:generator 10
    (inst add-sub a a* #1=(- (* bignum-digits-offset n-word-bytes) other-pointer-lowtag))
    (inst add-sub r r* #1#)

    (inst ldr digit-a (@ a))
    (inst adds n digit-a b)
    (inst str n (@ r))
    (inst mov index 1)
    (inst sub length la 1)

    (inst asr digit-b b 63)
    (inst cbz length DONE)

    ;; Add the sign digit with carry to the remaining digits of the longest bignum
    LOOP-A
    (inst ldr digit-a (@ a (extend index :lsl 3)))
    (inst adcs n digit-a digit-b)
    (inst str n (@ r (extend index :lsl 3)))

    (inst add index index 1)
    (inst sub length length 1)
    (inst cbnz length LOOP-A)

    DONE
    (inst asr digit-a digit-a 63) ;; has the last digit of A
    (inst adc n digit-a digit-b)
    (inst str n (@ r (extend index :lsl 3)))))

(define-vop (bignum-negate-loop)
  (:args (a* :scs (descriptor-reg) :to :save)
         (l :scs (unsigned-reg) :target length)
         (r* :scs (descriptor-reg) :to :save))
  (:arg-types bignum unsigned-num bignum)
  (:temporary (:sc unsigned-reg :from (:argument 1)) length)
  (:temporary (:sc unsigned-reg) index a r)
  (:results (last1 :scs (unsigned-reg))
            (last2 :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 10
    (inst add-sub a a* #1=(- (* bignum-digits-offset n-word-bytes) other-pointer-lowtag))
    (inst add-sub r r* #1#)
    (inst mov last2 0)
    (inst subs index zr-tn zr-tn) ;; set carry
    (move length l)
    LOOP
    (move last1 last2)
    (inst ldr last2 (@ a (extend index :lsl 3)))
    (inst sbcs last2 zr-tn last2)
    (inst str last2 (@ r (extend index :lsl 3)))
    (inst add index index 1)
    (inst sub length length 1)
    (inst cbnz length LOOP)))

(define-vop (bignum-negate-in-place-loop)
  (:args (a* :scs (descriptor-reg) :to :save)
         (l :scs (unsigned-reg) :target length))
  (:arg-types bignum unsigned-num)
  (:temporary (:sc unsigned-reg :from (:argument 1)) length)
  (:temporary (:sc unsigned-reg) a)
  (:generator 10
    (inst subs a a* (- other-pointer-lowtag (* bignum-digits-offset n-word-bytes))) ;; set carry
    (move length l)
    LOOP
    (inst ldr tmp-tn (@ a))
    (inst sbcs tmp-tn zr-tn tmp-tn)
    (inst str tmp-tn (@ a n-word-bytes :post-index))
    (inst sub length length 1)
    (inst cbnz length LOOP)))

(define-vop (bignum-negate-last-two-loop)
  (:args (a* :scs (descriptor-reg) :to :save)
         (l :scs (unsigned-reg) :target length))
  (:arg-types bignum unsigned-num)
  (:temporary (:sc unsigned-reg :from (:argument 1)) length)
  (:temporary (:sc unsigned-reg) a)
  (:results (last1 :scs (unsigned-reg))
            (last2 :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 10
    (inst subs a a* (- other-pointer-lowtag (* bignum-digits-offset n-word-bytes))) ;; set carry
    (move length l)
    (inst mov last2 0)
    LOOP
    (move last1 last2)
    (inst ldr last2 (@ a n-word-bytes :post-index))
    (inst sbcs last2 zr-tn last2)
    (inst sub length length 1)
    (inst cbnz length LOOP)))

(define-vop (bignum-mult-and-add-3-arg)
  (:translate sb-bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (unsigned-reg) :to :result)
         (carry-in :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from (:argument 2))
            (lo :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num unsigned-num)
  (:generator 2
    (inst mul lo x y)
    (inst adds lo lo carry-in)
    (inst umulh hi x y)
    (inst adc hi hi zr-tn)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate sb-bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (unsigned-reg) :target lo)
         (prev :scs (unsigned-reg) :to :result)
         (carry-in :scs (unsigned-reg) :to :result))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from :eval)
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 9
    (inst umulh hi x y)
    (inst mul lo x y)
    (inst adds lo lo prev)
    (inst adc hi hi zr-tn)
    (inst adds lo lo carry-in)
    (inst adc hi hi zr-tn)))

(define-vop (bignum-mult)
  (:translate sb-bignum:%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (unsigned-reg) :to :result))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from :eval)
            (lo :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num unsigned-num)
  (:generator 1
    (inst umulh hi x y)
    (inst mul lo x y)))

(define-vop (mulhi)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 20
    (inst umulh hi x y)))

(define-vop (mulhi/fx)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (any-reg))
         (y :scs (unsigned-reg)))
  (:arg-types positive-fixnum unsigned-num)
  (:temporary (:sc unsigned-reg) temp)
  (:results (hi :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 15
    (inst umulh temp x y)
    (inst and hi temp (bic-mask fixnum-tag-mask))))

(define-vop ()
  (:translate %signed-multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (hi :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 20
    (inst smulh hi x y)))

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
    (dotimes (i 65)
      (assemble ()
        (inst cmp rem divisor)
        (inst b :cc CC)
        (inst sub rem rem divisor)
        CC
        (inst adcs quo quo quo)
        (unless (= i 64)
          (inst adc rem rem rem))))))

(define-vop (half-bignum-floor bignum-floor)
  (:translate sb-bignum:%half-bigfloor)
  (:args (div-high :scs (unsigned-reg) :to :save)
         (div-low :scs (unsigned-reg) :target x)
         (divisor :scs (unsigned-reg) :to (:result 1)))
  (:temporary (:sc unsigned-reg :from (:argument 1)) x)
  (:generator 30
    (move x div-low)
    (inst bfm x div-high 32 31)
    (inst udiv quo x divisor)
    (inst msub rem quo divisor x)))

(define-vop (signify-digit)
  (:translate sb-bignum:%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (if (sc-is res any-reg)
        (inst lsl res digit n-fixnum-tag-bits)
        (move res digit))))

(define-vop (digit-ashr)
  (:translate sb-bignum:%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg))
         (count :scs (unsigned-reg immediate)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst asr result digit (sc-case count
                             (immediate
                              (tn-value count))
                             (t count)))))

(define-vop (digit-lshr digit-ashr)
  (:translate sb-bignum:%digit-logical-shift-right)
  (:generator 1
    (inst lsr result digit count)))

(define-vop (digit-ashl digit-ashr)
  (:translate sb-bignum:%ashl)
  (:generator 1
    (inst lsl result digit count)))

(define-vop (*/signed=>integer)
  (:translate *)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types signed-num signed-num)
  (:temporary (:sc signed-reg) high low)
  (:temporary (:sc signed-reg :from (:argument 2)) header)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (r :scs (descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 10
    (let ((value (and (sc-is y immediate)
                      (tn-value y))))
      (cond ((and value
                  (plusp value)
                  (= (logcount value) 1))
             (let ((shift (1- (integer-length value))))
               (inst lsl low x shift)
               (inst asr high x (- 64 shift))))
            (t
             (when value
               (load-immediate-word high value)
               (setf y high))
             (inst mul low x y)
             (inst smulh high x y))))
    (inst mov header (bignum-header-for-length 2))
    (inst cmp high (asr low 63))
    (inst b :ne allocate)
    (inst adds r low low)
    (inst b :vc done)
    (inst mov header (bignum-header-for-length 1))
    #+bignum-assertions
    (inst mov high 0)
    allocate
    (with-fixed-allocation
        (r lr nil (+ 2 bignum-digits-offset))
      (storew-pair header 0 low bignum-digits-offset tmp-tn)
      (storew high tmp-tn 2))
    DONE))

(define-vop (*/unsigned=>integer)
  (:translate *)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg immediate)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg) high low)
  (:temporary (:sc unsigned-reg :from (:argument 2)) header)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (r :scs (descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 12
    (let ((value (and (sc-is y immediate)
                        (tn-value y))))
        (cond ((and value
                    (plusp value)
                    (= (logcount value) 1))
               (let ((shift (1- (integer-length value))))
                 (inst lsl low x shift)
                 (inst lsr high x (- n-word-bits shift))))
              (t
               (when (sc-is y immediate)
                 (load-immediate-word high value)
                 (setf y high))
               (inst mul low x y)
               (inst umulh high x y))))
    (inst mov header (bignum-header-for-length 3))
    (inst tbnz high 63 allocate)
    (inst mov header (bignum-header-for-length 2))
    (inst cbnz high allocate)
    (inst tbnz low 63 allocate)
    (inst adds r low low)
    (inst b :vc done)
    (inst mov header (bignum-header-for-length 1))
    allocate
    (with-fixed-allocation
        (r lr nil (+ 2 bignum-digits-offset))
      (storew-pair header 0 low bignum-digits-offset tmp-tn)
      (storew high tmp-tn 2))
    DONE))

(define-vop (+/signed=>integer)
  (:translate +)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types signed-num signed-num)
  (:temporary (:sc signed-reg) high low)
  (:temporary (:sc signed-reg :from (:argument 2)) header)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (r :scs (descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 10
    (if (sc-is y immediate)
        (let ((y (tn-value y)))
          (if (minusp y)
              (inst subs low x (add-sub-immediate (- y) high))
              (inst adds low x (add-sub-immediate y high))))
        (inst adds low x y))
    (inst csetm high :cs)
    (inst mov header (bignum-header-for-length 2))
    (inst b :vs allocate)
    (inst adds r low low)
    (inst b :vc done)
    (inst mov header (bignum-header-for-length 1))
    allocate
    (with-fixed-allocation
        (r lr nil (+ 2 bignum-digits-offset))
      (storew-pair header 0 low bignum-digits-offset tmp-tn)
      (storew high tmp-tn 2))
    DONE))

(define-vop (-/signed=>integer)
  (:translate -)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types signed-num signed-num)
  (:temporary (:sc signed-reg) high low)
  (:temporary (:sc signed-reg :from (:argument 2)) header)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (r :scs (descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 10
    (if (sc-is y immediate)
        (let ((y (tn-value y)))
          (if (minusp y)
              (inst adds low x (add-sub-immediate (- y) high))
              (inst subs low x (add-sub-immediate y high))))
        (inst subs low x y))
    (inst csetm high :cs)
    (inst mov header (bignum-header-for-length 2))
    (inst b :vs allocate)
    (inst adds r low low)
    (inst b :vc done)
    (inst mov header (bignum-header-for-length 1))
    allocate
    (with-fixed-allocation
        (r lr nil (+ 2 bignum-digits-offset))
      (storew-pair header 0 low bignum-digits-offset tmp-tn)
      (storew high tmp-tn 2))
    DONE))

(define-vop (+/unsigned=>integer)
  (:translate +)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg immediate)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg) high low)
  (:temporary (:sc unsigned-reg :from (:argument 2)) header)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (r :scs (descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 10
    (inst adds low x (if (sc-is y immediate)
                         (add-sub-immediate (tn-value y) high)
                         y))
    (inst mov header (bignum-header-for-length 2))
    (inst cset high :cs)
    (inst b :cs allocate)
    (inst b :mi allocate)
    (inst adds r low low)
    (inst b :vc done)
    (inst mov header (bignum-header-for-length 1))
    allocate
    (with-fixed-allocation
        (r lr nil (+ 2 bignum-digits-offset))
      (storew-pair header 0 low bignum-digits-offset tmp-tn)
      (storew high tmp-tn 2))
    DONE))

(define-vop (-/unsigned=>integer)
  (:translate -)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg immediate)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg) high low)
  (:temporary (:sc unsigned-reg :from (:argument 2)) header)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (r :scs (descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 10
    (inst subs low x (if (sc-is y immediate)
                         (add-sub-immediate (tn-value y) high)
                         y))
    (inst mov header (bignum-header-for-length 2))
    (inst csetm high :cc)
    (inst b :cc negative)
    (inst b :mi allocate)
    (inst b positive)
    negative
    (inst b :pl allocate)
    positive
    (inst adds r low low)
    (inst b :vc done)
    (inst mov header (bignum-header-for-length 1))
    allocate
    (with-fixed-allocation
        (r lr nil (+ 2 bignum-digits-offset))
      (storew-pair header 0 low bignum-digits-offset tmp-tn)
      (storew high tmp-tn 2))
    DONE))

(define-vop (%negate/unsigned=>integer)
  (:translate %negate)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:temporary (:sc unsigned-reg) high low)
  (:temporary (:sc unsigned-reg :from (:argument 2)) header)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (r :scs (descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 10
    (inst negs low x)
    (inst mov header (bignum-header-for-length 2))
    (inst csetm high :cc)
    (inst b :cc negative)
    (inst b :mi allocate)
    (inst b positive)
    negative
    (inst b :pl allocate)
    positive
    (inst adds r low low)
    (inst b :vc done)
    (inst mov header (bignum-header-for-length 1))
    allocate
    (with-fixed-allocation
        (r lr nil (+ 2 bignum-digits-offset))
      (storew-pair header 0 low bignum-digits-offset tmp-tn)
      (storew high tmp-tn 2))
    DONE))

(define-vop (%negate/signed=>integer)
  (:translate %negate)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num)
  (:temporary (:sc signed-reg) high low)
  (:temporary (:sc signed-reg :from (:argument 2)) header)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (r :scs (descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 10
    (inst negs low x)
    (inst csetm high :cs)
    (inst mov header (bignum-header-for-length 2))
    (inst b :vs allocate)
    (inst adds r low low)
    (inst b :vc done)
    (inst mov header (bignum-header-for-length 1))
    allocate
    (with-fixed-allocation
        (r lr nil (+ 2 bignum-digits-offset))
      (storew-pair header 0 low bignum-digits-offset tmp-tn)
      (storew high tmp-tn 2))
    DONE))

(define-vop (overflow*-fixnum)
  (:translate overflow*)
  (:args (x :scs (any-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types tagged-num tagged-num)
  (:info type)
  (:temporary (:sc signed-reg) high)
  (:results (r :scs (any-reg) :from :load))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 1
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::mul-overflow-error r high)))
      (let ((value (and (sc-is y immediate)
                        (tn-value y))))
        (cond ((and value
                    (plusp value)
                    (= (logcount value) 1))
               (let ((shift (1- (integer-length value))))
                 (inst lsl r x shift)
                 (inst asr high x (- 64 shift))))
              (t
               (when value
                 (load-immediate-word high value)
                 (setf y high))
               (inst mul r x y)
               (inst smulh high x y))))
      (inst cmp high (asr r 63))
      (inst b :ne error))))

(define-vop (overflow*-signed)
  (:translate overflow*)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types signed-num signed-num)
  (:info type)
  (:temporary (:sc signed-reg) high)
  (:results (r :scs (signed-reg) :from :load))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::mul-overflow-error r high)))
      (let ((value (and (sc-is y immediate)
                        (tn-value y))))
        (cond ((and value
                    (plusp value)
                    (= (logcount value) 1))
               (let ((shift (1- (integer-length value))))
                 (inst lsl r x shift)
                 (inst asr high x (- 64 shift))))
              (t
               (when value
                 (load-immediate-word high value)
                 (setf y high))
               (inst mul r x y)
               (inst smulh high x y))))
      (inst cmp high (asr r 63))
      (inst b :ne error))))

(define-vop (overflow*-fixnum)
  (:translate overflow*)
  (:args (x :scs (any-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types tagged-num tagged-num)
  (:info type)
  (:temporary (:sc signed-reg) high)
  (:results (r :scs (any-reg) :from :load))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 1
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::mul-overflow-error r high)))
      (let ((value (and (sc-is y immediate)
                        (tn-value y))))
        (cond ((and value
                    (plusp value)
                    (= (logcount value) 1))
               (let ((shift (1- (integer-length value))))
                 (inst lsl r x shift)
                 (inst asr high x (- 64 shift))))
              (t
               (when value
                 (load-immediate-word high value)
                 (setf y high))
               (inst mul r x y)
               (inst smulh high x y))))
      (inst cmp high (asr r 63))
      (inst b :ne error))))

(define-vop (overflow*-signed=>unsigned)
  (:translate overflow*)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg ;; immediate
                             )))
  (:arg-types signed-num signed-num)
  (:info type)
  (:temporary (:sc signed-reg) high)
  (:results (r :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::mul-overflow2-error x y)))
      (inst mul r x y)
      (inst smulh high x y)
      (inst cbnz high error))))

(define-vop (overflow*-unsigned)
  (:translate overflow*)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg immediate)))
  (:arg-types unsigned-num unsigned-num)
  (:info type)
  (:temporary (:sc unsigned-reg) high)
  (:results (r :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::mul-overflow-error r high)))
      (let ((value (and (sc-is y immediate)
                        (tn-value y))))

        (cond ((and value
                    (plusp value)
                    (= (logcount value) 1))
               (let ((shift (1- (integer-length value))))
                 (inst lsl r x shift)
                 (inst lsr high x (- n-word-bits shift))))
              (t
               (when (sc-is y immediate)
                 (load-immediate-word high value)
                 (setf y high))
               (inst mul r x y)
               (inst umulh high x y))))
      (inst cbnz high error))))

(define-vop (overflow*-signed-unsigned=>unsigned)
  (:translate overflow*)
  (:args (x :scs (signed-reg))
         (y :scs (unsigned-reg ;; immediate
                  )))
  (:arg-types signed-num unsigned-num)
  (:info type)
  (:temporary (:sc unsigned-reg) high)
  (:results (r :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::mul-overflow2-error x y)))
      (assemble ()
        (inst cbz y SKIP)
        (inst tbnz* x 63 error)
        SKIP
        (inst mul r x y)
        (inst umulh high x y)
        (inst cbnz high error)))))

(define-vop (overflow*-signed-unsigned=>signed)
  (:translate overflow*)
  (:args (x :scs (signed-reg))
         (y :scs (unsigned-reg ;; immediate
                  )))
  (:arg-types signed-num unsigned-num)
  (:info type)
  (:temporary (:sc unsigned-reg) high)
  (:results (r :scs (signed-reg) :from :load))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 6
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::mul-overflow2-error x y)))
      (assemble ()
        (inst mul r x y)
        (inst smulh high x y)
        (inst tbz* y 63 SKIP)
        (inst cbz x DONE)
        (inst cmn x 1)
        (inst b :ne error)
        (inst cmp r y)
        (inst b :eq DONE)
        (inst b error)
        SKIP
        (inst cmp high (asr r 63))
        (inst b :ne error)
        DONE))))

(define-vop (overflow+-signed)
  (:translate overflow+)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types signed-num signed-num)
  (:info type)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::add-sub-overflow-error r)))
      (if (sc-is y immediate)
          (let ((y (tn-value y)))
            (if (minusp y)
                (inst subs r x (add-sub-immediate (- y)))
                (inst adds r x (add-sub-immediate y))))
          (inst adds r x y))
      (inst b :vs error))))

(define-vop (overflow+-fixnum)
  (:translate overflow+)
  (:args (x :scs (any-reg))
         (y :scs (any-reg immediate)))
  (:arg-types tagged-num tagged-num)
  (:info type)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 1
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::add-sub-overflow-error r)))
      (if (sc-is y immediate)
          (let ((y (fixnumize (tn-value y))))
            (if (and (minusp y)
                     (/= y #.(fixnumize most-negative-fixnum)))
                (inst subs r x (add-sub-immediate (- y)))
                (inst adds r x (add-sub-immediate y))))
          (inst adds r x y))
      (inst b :vs error))))

(define-vop (overflow+-signed=>unsigned)
  (:translate overflow+)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg ;; immediate
                             )))
  (:arg-types signed-num signed-num)
  (:info type)
  (:temporary (:sc unsigned-reg) temp1 temp2)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::add-sub-overflow-error
                                       (make-random-tn :kind :normal
                                                       :sc (sc-or-lose 'signed-reg)
                                                       :offset (tn-offset r)))))
      (inst asr temp1 x 63)
      (inst asr temp2 y 63)
      (inst adds r x y)
      (inst adc temp1 temp1 temp2)
      (inst tbnz* temp1 0 error))))

(define-vop (overflow+unsigned)
  (:translate overflow+)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg immediate)))
  (:arg-types unsigned-num unsigned-num)
  (:info type)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::add-sub-overflow-error r)))
      (when (sc-is y immediate)
        (setf y (add-sub-immediate (tn-value y))))
      (inst adds r x y)
      (inst b :cs error))))

(define-vop (overflow+unsigned-signed=>unsigned)
  (:translate overflow+)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (signed-reg immediate)))
  (:arg-types unsigned-num signed-num)
  (:info type)
  (:results (r :scs (unsigned-reg)))
  (:temporary (:scs (unsigned-reg) :from (:argument 2)) temp)
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 8
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::add-sub-overflow-error r)))
      (if (sc-is y immediate)
          (assemble ()
            (let ((value (tn-value y)))
              (setf y (add-sub-immediate (abs value)))
              (cond ((plusp value)
                     (inst adds r x y)
                     (inst b :cs error))
                    (t
                     (inst subs r x y)
                     (inst b :cc error)))))
          (assemble ()
            (inst tbnz y 63 neg)
            (inst adds r x y)
            (inst b :cs error)
            (inst b done)
            neg
            (inst neg temp y)
            (inst subs r x temp)
            (inst b :cc error)
            done)))))

(define-vop (overflow+unsigned-signed=>signed)
  (:translate overflow+)
  (:args (x :scs (unsigned-reg) :to :save)
         (y :scs (signed-reg ;; immediate
                  )))
  (:arg-types unsigned-num signed-num)
  (:info type)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::signed-unsigned-add-overflow-error r)))
      (inst add r x y)
      (inst mov tmp-tn -9223372036854775808)
      (inst add tmp-tn r tmp-tn)
      (inst cmp tmp-tn x)
      (inst b :cc error))))

(define-vop (overflow-signed)
  (:translate overflow-)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types signed-num signed-num)
  (:info type)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::add-sub-overflow-error r)))
      (if (sc-is y immediate)
          (let ((y (tn-value y)))
            (if (minusp y)
                (inst adds r x (add-sub-immediate (- y)))
                (inst subs r x (add-sub-immediate y))))
          (inst subs r x y))
      (inst b :vs error))))

(define-vop (overflow-fixnum)
  (:translate overflow-)
  (:args (x :scs (any-reg))
         (y :scs (any-reg immediate)))
  (:arg-types tagged-num tagged-num)
  (:info type)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 1
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::add-sub-overflow-error r)))
      (if (sc-is y immediate)
          (let ((y (fixnumize (tn-value y))))
            (if (and (minusp y)
                     (/= y #.(fixnumize most-negative-fixnum)))
                (inst adds r x (add-sub-immediate (- y)))
                (inst subs r x (add-sub-immediate y))))
          (inst subs r x y))
      (inst b :vs error))))

(define-vop (overflow-unsigned)
  (:translate overflow-)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg immediate)))
  (:arg-types unsigned-num unsigned-num)
  (:info type)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::add-sub-overflow-error r)))
      (when (sc-is y immediate)
        (setf y (add-sub-immediate (tn-value y))))
      (inst subs r x y)
      (inst b :cc error))))

(define-vop (overflow-unsigned-=>signed)
  (:translate overflow-)
  (:args (x :scs (unsigned-reg) :to :save)
         (y :scs (unsigned-reg ;; immediate
                               ) :to :save))
  (:arg-types unsigned-num unsigned-num)
  (:info type)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::sub-overflow2-error x y)))
      (inst subs r x y)
      (inst sbc tmp-tn zr-tn zr-tn)
      (inst and tmp-tn tmp-tn 1)
      (inst eor tmp-tn tmp-tn (lsr r 63))
      (inst cbnz tmp-tn error))))

(define-vop (overflow-unsigned-signed=>signed)
  (:translate overflow-)
  (:args (x :scs (unsigned-reg))
         (y :scs (signed-reg)))
  (:arg-types unsigned-num signed-num)
  (:info type)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::signed-unsigned-add-overflow-error r)))
      (inst mov tmp-tn -9223372036854775808)
      (inst add tmp-tn y tmp-tn)
      (inst cmp x tmp-tn)
      (inst sub r x y)
      (inst b :cs error))))

(define-vop (overflow-signed-unsigned=>signed)
  (:translate overflow-)
  (:args (x :scs (signed-reg) :to :save)
         (y :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:info type)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::sub-overflow2-error x y)))
      (inst mov tmp-tn -9223372036854775808)
      (inst add tmp-tn x tmp-tn)
      (inst cmp tmp-tn y)
      (inst b :cc error)
      (inst sub r x y))))

(define-vop (overflow-signed-unsigned=>unsigned)
  (:translate overflow-)
  (:args (x :scs (signed-reg) :to :save)
         (y :scs (unsigned-reg) :to :save))
  (:arg-types signed-num unsigned-num)
  (:info type)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::sub-overflow2-error x y)))
      (inst subs r x y)
      (inst tbnz* x 63 error)
      (inst b :lo error))))

(define-vop (overflow-signed=>unsigned)
  (:translate overflow-)
  (:args (x :scs (signed-reg) :to :save)
         (y :scs (signed-reg) :to :save))
  (:arg-types signed-num signed-num)
  (:info type)
  (:temporary (:scs (unsigned-reg)) temp)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::sub-overflow2-error x y)))
      (inst asr temp x 63)
      (inst asr tmp-tn y 63)
      (inst subs r x y)
      (inst sbc temp temp tmp-tn)
      (inst tbnz* temp 0 error))))

(define-vop (overflow-unsigned-signed=>unsigned)
  (:translate overflow-)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (signed-reg immediate)))
  (:arg-types unsigned-num signed-num)
  (:info type)
  (:results (r :scs (unsigned-reg)))
  (:temporary (:scs (unsigned-reg) :from (:argument 2)) temp)
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 7
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::add-sub-overflow-error r)))
      (if (sc-is y immediate)
          (assemble ()
            (let ((value (tn-value y)))
              (setf y (add-sub-immediate (abs value)))
              (cond ((minusp value)
                     (inst adds r x y)
                     (inst b :cs error))
                    (t
                     (inst subs r x y)
                     (inst b :cc error)))))
          (assemble ()
            (when (sc-is y immediate)
              (setf y (add-sub-immediate (tn-value y))))
            (inst tbz y 63 pos)
            (inst neg temp y)
            (inst adds r x temp)
            (inst b :cs error)
            (inst b done)
            pos
            (inst subs r x y)
            (inst b :cc error)
            done)))))

(define-vop (overflow-negate-signed)
  (:translate overflow-negate)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num)
  (:info type)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::add-sub-overflow-error r)))
      (inst negs r x)
      (inst b :vs error))))

(define-vop (overflow-negate-unsigned)
  (:translate overflow-negate)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:info type)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::negate-overflow-error r)))
      (move r x)
      (inst cbnz x error))))

(define-vop (overflow-negate-unsigned=>signed)
  (:translate overflow-negate)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:info type)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::negate-overflow-error x)))
      (inst mov tmp-tn 9223372036854775808)
      (inst cmp x tmp-tn)
      (inst b :hi error)
      (inst neg r x))))

(define-vop (overflow-negate-signed=>unsigned)
  (:translate overflow-negate)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num)
  (:info type)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::negate-overflow-error x)))
      (inst cmp x 1)
      (inst b :ge error)
      (inst neg r x))))

(define-vop (overflow-ash-signed)
  (:translate overflow-ash)
  (:args (number :scs (signed-reg))
         (amount :scs (unsigned-reg signed-reg immediate)))
  (:arg-types signed-num untagged-num)
  (:arg-refs nil amount-ref)
  (:info type)
  (:results (r :scs (signed-reg) :from :load))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (amount-error amount)
           (error (generate-error-code+
                   (when (sc-is amount immediate)
                     (setf amount (tn-value amount))
                     (cond ((typep amount 'sc-offset)
                            (setf amount-error (make-sc+offset immediate-sc-number amount))
                            nil)
                           (t
                            (setf amount-error
                                  (make-random-tn :kind :normal
                                                  :sc (sc-or-lose (if (typep amount 'word)
                                                                      'unsigned-reg
                                                                      'signed-reg))
                                                  :offset (tn-offset tmp-tn)))

                            (lambda ()
                              (load-immediate-word amount-error amount)))))
                   vop 'sb-kernel::ash-overflow2-error number amount-error))
           (fits (csubtypep (tn-ref-type amount-ref)
                            (specifier-type `(integer -63 63)))))
      (cond ((numberp amount)
             (cond ((minusp amount)
                    (setf amount (min (- amount) 63))
                    (inst asr r number amount))
                   ((> amount 63)
                    (inst cbnz number error)
                    (move r number))
                   ((zerop amount)
                    (move r number))
                   (t
                    (inst lsl r number amount)
                    (inst asr tmp-tn number (- 64 amount))
                    (inst cmp tmp-tn (asr r 63))
                    (inst b :ne error))))
            ((csubtypep (tn-ref-type amount-ref)
                        (specifier-type 'unsigned-byte))
             (unless fits
               (move r number)
               (inst cbz number done)
               (inst cmp amount n-word-bits)
               (inst b :ge error))
             (inst neg tmp-tn amount)
             (inst lsl r number amount)
             (inst cbz amount done)
             (inst asr tmp-tn number tmp-tn)
             (inst cmp tmp-tn (asr r 63))
             (inst b :ne error))
            (t
             (inst neg tmp-tn amount)
             (cond (fits
                    (inst asr r number tmp-tn))
                   (t
                    (inst cmp tmp-tn n-word-bits)
                    (inst csinv r tmp-tn zr-tn :lo)
                    (inst asr r number r)))
             (inst cmp amount 0)
             (inst b :le done)
             (unless fits
               (inst cbz number done)
               (inst cmp amount n-word-bits)
               (inst b :ge error))
             (inst lsl r number amount)
             (inst asr tmp-tn number tmp-tn) ;; a negated shift is the same as (- 64 shift)
             (inst cmp tmp-tn (asr r 63))
             (inst b :ne error))))
    done))

(define-vop (overflow-ash-unsigned)
  (:translate overflow-ash)
  (:args (number :scs (unsigned-reg))
         (amount :scs (unsigned-reg signed-reg immediate)))
  (:arg-types unsigned-num untagged-num)
  (:arg-refs nil amount-ref)
  (:info type)
  (:results (r :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:variant-vars signed fixnum)
  (:generator 3
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (amount-error amount)
           (error (generate-error-code+
                   (when (sc-is amount immediate)
                     (setf amount (tn-value amount))
                     (cond ((typep amount 'sc-offset)
                            (setf amount-error (make-sc+offset immediate-sc-number amount))
                            nil)
                           (t
                            (setf amount-error
                                  (make-random-tn :kind :normal
                                                  :sc (sc-or-lose (if (typep amount 'word)
                                                                      'unsigned-reg
                                                                      'signed-reg))
                                                  :offset (tn-offset tmp-tn)))

                            (lambda ()
                              (load-immediate-word amount-error amount)))))
                   vop 'sb-kernel::ash-overflow2-error number amount-error))
           (fits (csubtypep (tn-ref-type amount-ref)
                            (specifier-type `(integer -63 63)))))
      (when signed
        (inst tbnz* number 63 error))
      (cond ((numberp amount)
             (cond ((< amount -63)
                    (inst mov r 0))
                   ((minusp amount)
                    (inst lsr r number amount))
                   ((> amount 63)
                    (inst cbnz number error)
                    (move r number))
                   ((zerop amount)
                    (move r number))
                   (t
                    (inst lsl r number amount)
                    (cond ((= amount 1)
                           (inst tbnz* number (if fixnum
                                                  62
                                                  63) error))
                          (t
                           (inst cmp zr-tn (lsr number (- (if fixnum
                                                              n-fixnum-bits
                                                              64)
                                                          amount)))
                           (inst b :ne error))))))
            ((csubtypep (tn-ref-type amount-ref)
                        (specifier-type 'unsigned-byte))
             (unless fits
               (move r number)
               (inst cbz number done)
               (inst cmp amount n-word-bits)
               (inst b :ge error))
             (if fixnum
                 (inst mvn tmp-tn amount)  ; (- 63 amount)
                 (inst neg tmp-tn amount)) ; (- 64 amount)
             (inst lsl r number amount)
             (unless fixnum
               (inst cbz amount done))
             (inst asr tmp-tn number tmp-tn)
             (inst cbnz tmp-tn error))
            (t
             (inst neg tmp-tn amount)
             (cond (fits
                    (inst lsr r number tmp-tn))
                   (t
                    (inst cmp tmp-tn n-word-bits)
                    (inst csel r number zr-tn :lo)
                    (inst lsr r r tmp-tn)))
             (inst cmp amount 0)
             (inst b :le done)
             (unless fits
               (inst cbz number done)
               (inst cmp amount n-word-bits)
               (inst b :ge error))
             (inst lsl r number amount)
             (inst asr tmp-tn number tmp-tn)
             (inst cbnz tmp-tn error))))
    done))

(define-vop (overflow-ash-fixnum overflow-ash-signed)
  (:args (number :scs (any-reg))
         (amount :scs (unsigned-reg immediate)))
  (:arg-types tagged-num unsigned-num)
  (:results (r :scs (any-reg) :from :load))
  (:result-types tagged-num)
  (:variant-cost 2))

(define-vop (overflow-ash-unsigned-fixnum overflow-ash-unsigned)
  (:args (number :scs (any-reg))
         (amount :scs (unsigned-reg immediate)))
  (:arg-types positive-fixnum unsigned-num)
  (:results (r :scs (any-reg) :from :load))
  (:result-types positive-fixnum)
  (:variant nil t)
  (:variant-cost 1))

(define-vop (overflow-ash-signed=>unsigned overflow-ash-unsigned)
  (:args (number :scs (signed-reg))
         (amount :scs (unsigned-reg signed-reg immediate)))
  (:arg-types signed-num untagged-num)
  (:variant t nil)
  (:variant-cost 4))

(define-vop (overflow+t)
  (:translate overflow+)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg signed-reg
                          (immediate
                           (typep (tn-value tn) '(and sc-offset
                                                  (satisfies fixnum-add-sub-immediate-p)))))))
  (:arg-types (:or t tagged-num) tagged-num)
  (:arg-refs x-ref)
  (:info type)
  (:results (r :scs (any-reg) :from :load))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::add-overflow2-error x y)))
      (unless (csubtypep (tn-ref-type x-ref) (specifier-type 'fixnum))
        (inst tbnz* x 0 error))
      (inst adds r x (sc-case y
                       (any-reg y)
                       (immediate
                        (fixnumize (tn-value y)))
                       (t
                        (lsl y n-fixnum-tag-bits))))
      (inst b :vs error))))

(define-vop (overflow-t)
  (:translate overflow-)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg signed-reg
                          (immediate
                           (typep (tn-value tn) '(and sc-offset
                                                  (satisfies fixnum-add-sub-immediate-p)))))))
  (:arg-types (:or t tagged-num) tagged-num)
  (:arg-refs x-ref)
  (:info type)
  (:results (r :scs (any-reg) :from :load))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::sub-overflow2-error x y)))
      (unless (csubtypep (tn-ref-type x-ref) (specifier-type 'fixnum))
        (inst tbnz* x 0 error))
      (inst subs r x (sc-case y
                       (any-reg y)
                       (immediate
                        (fixnumize (tn-value y)))
                       (t
                        (lsl y n-fixnum-tag-bits))))
      (inst b :vs error))))

(define-vop (overflow-t-y)
  (:translate overflow-)
  (:args (x :scs (any-reg))
         (y :scs (any-reg descriptor-reg)))
  (:arg-types tagged-num (:or t tagged-num))
  (:arg-refs nil y-ref)
  (:info type)
  (:results (r :scs (any-reg) :from :load))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::sub-overflow2-error x y)))
      (unless (csubtypep (tn-ref-type y-ref) (specifier-type 'fixnum))
        (inst tbnz* y 0 error))
      (inst subs r x y)
      (inst b :vs error))))

(define-vop (overflow*t)
  (:translate overflow*)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types (:or t tagged-num) tagged-num)
  (:arg-refs x-ref)
  (:info type)
  (:temporary (:sc signed-reg
               :unused-if
               (or (not (sc-is y immediate))
                   (and (plusp (tn-value y))
                        (= (logcount (tn-value y)) 1))))
              temp)
  (:results (r :scs (any-reg) :from :load))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (value (and (sc-is y immediate)
                       (tn-value y)))
           (shift (and value
                       (plusp value)
                       (= (logcount value) 1)
                       (1- (integer-length value))))
           (error (generate-error-code+ (cond (shift
                                               (setf y r)
                                               (lambda ()
                                                 (load-immediate-word y (fixnumize value))))
                                              (value
                                               (setf y temp)
                                               (load-immediate-word y value)
                                               nil))
                                        vop
                                        'sb-kernel::mul-overflow2-error x y)))
      (unless (csubtypep (tn-ref-type x-ref) (specifier-type 'fixnum))
        (inst tbnz* x 0 error))
      (cond ((eql shift 0)
             (move r x))
            (shift
             (inst lsl r x shift)
             (inst asr tmp-tn x (- 64 shift))
             (inst cmp tmp-tn (asr r 63))
             (inst b :ne error))
            (t
             (inst smulh tmp-tn x y)
             (inst mul r x y)
             (inst cmp tmp-tn (asr r 63))
             (inst b :ne error))))))

(deftransform signum ((x) (:or ((signed-word) *)
                               ((word) *)) * :vop t)
  t)

(define-vop (signum-signed signed-unop)
  (:args (x :scs (signed-reg any-reg) :target res))
  (:translate signum)
  (:generator 4
    (inst cmp x 0)
    (inst cset tmp-tn :ne)
    (inst csinv res tmp-tn zr-tn :ge)))

(define-vop (signum-unsigned unsigned-unop)
  (:args (x :scs (unsigned-reg any-reg) :target res))
  (:translate signum)
  (:generator 3
    (inst cmp x 0)
    (inst cset res :ne)))

(define-vop ()
  (:translate fastrem-32)
  (:policy :fast-safe)
  (:args (dividend :scs (unsigned-reg))
         (c :scs (unsigned-reg))
         (divisor :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (remainder :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 10
    (inst mul (32-bit-reg temp) dividend c) ; drop the high 32 bits, keep the low 32 bits
    (inst mul temp temp divisor)
    (inst lsr remainder temp 32))) ; take the high 32 bits
(define-vop ()
  (:translate fastrem-64)
  (:policy :fast-safe)
  (:args (dividend :scs (unsigned-reg))
         (c :scs (unsigned-reg))
         (divisor :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (remainder :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 10
    (inst mul temp dividend c) ; want only the low 64 bits
    (inst umulh remainder temp divisor))) ; want only the high 64 bits

(macrolet ((def (name excl-low excl-high &optional check)
               `(progn
                  ,@(unless check
                      `((define-vop (,(symbolicate name '/c))
                          (:translate ,name)
                          (:args (x :scs (any-reg signed-reg unsigned-reg)))
                          (:arg-refs x-ref)
                          (:arg-types (:constant t)
                                      (:or tagged-num signed-num unsigned-num)
                                      (:constant t))
                          (:info lo hi)
                          (:temporary (:sc signed-reg
                                       :unused-if
                                       (cond ((or (= lo ,(if excl-low
                                                             -1
                                                             0))
                                                  (= hi
                                                     ,(if excl-high
                                                          0
                                                          -1))))))
                                      temp)
                          (:conditional :ls)
                          (:vop-var vop)
                          (:policy :fast-safe)
                          (:generator 2
                            (aver (>= hi lo))
                            (let ((lo (+ lo ,@(and excl-low
                                                   '(1))))
                                  (hi (+ hi ,@(and excl-high
                                                   '(-1))))
                                  (int (sb-c::type-approximate-interval (tn-ref-type x-ref))))
                              (multiple-value-bind (flo fhi one)
                                  (if (sc-is x any-reg)
                                      (values (fixnumize lo) (fixnumize hi) ,(fixnumize 1))
                                      (values lo hi 1))
                                (cond
                                  ((and (sc-is x unsigned-reg)
                                        (< fhi 0))
                                   (inst cmp null-tn 0))
                                  ((sb-c::interval-high<=n int hi)
                                   (change-vop-flags vop '(:ge))
                                   (inst cmp x (add-sub-immediate flo)))
                                  ((and (sb-c::interval-low>=n int lo)
                                        (cond ((< lo 0))
                                              (t
                                               (setf lo 0
                                                     flo 0)
                                               nil)))
                                   (change-vop-flags vop '(:le))
                                   (inst cmp x (add-sub-immediate fhi)))
                                  ((= lo hi)
                                   (change-vop-flags vop '(:eq))
                                   (inst cmp x (add-sub-immediate flo)))
                                  ((= hi -1)
                                   (setf flo (- flo))
                                   (cond ((add-sub-immediate-p (+ flo one))
                                          (incf flo one)
                                          (change-vop-flags vop '(:hi)))
                                         (t
                                          (change-vop-flags vop '(:hs))))
                                   (inst cmn x (add-sub-immediate flo)))
                                  (t
                                   (if (= lo 0)
                                       (setf temp x)
                                       (if (plusp flo)
                                           (inst sub temp x (add-sub-immediate flo))
                                           (inst add temp x (add-sub-immediate (abs flo)))))
                                   (let ((cmp (- fhi flo)))
                                     (cond ((and (sc-is x any-reg)
                                                 (= hi most-positive-fixnum))
                                            (change-vop-flags vop '(:ge))
                                            (inst cmp x 0))
                                           ((= (logcount (+ cmp one)) 1)
                                            (change-vop-flags vop '(:eq))
                                            (inst tst temp (lognot cmp)))
                                           (t
                                            (when (add-sub-immediate-p (+ cmp one))
                                              (incf cmp one)
                                              (change-vop-flags vop '(:lo)))
                                            (inst cmp temp (add-sub-immediate cmp)))))))))))

                        (define-vop ()
                          (:translate ,name)
                          (:args (lo :scs (any-reg immediate))
                                 (x :scs (any-reg signed-reg unsigned-reg))
                                 (hi :scs (any-reg immediate)))
                          (:arg-types tagged-num
                                      (:or tagged-num signed-num unsigned-num)
                                      tagged-num)
                          (:arg-refs lo-ref nil hi-ref)
                          (:conditional ,(if excl-high :lt :le))
                          (:vop-var vop)
                          (:policy :fast-safe)
                          (:generator 4
                            (flet ((imm (i &optional (ccmp t))
                                     (let ((i (if (and (tn-p i)
                                                       (sc-is i immediate))
                                                  (tn-value i)
                                                  i)))
                                       (cond ((integerp i)
                                              (funcall (if ccmp
                                                           'ccmp-immediate
                                                           'add-sub-immediate)
                                                       (if (sc-is x any-reg)
                                                           (fixnumize i)
                                                           i)))
                                             ((sc-is x any-reg)
                                              i)
                                             (ccmp
                                              (inst asr tmp-tn i n-fixnum-tag-bits)
                                              tmp-tn)
                                             (t
                                              (asr i n-fixnum-tag-bits))))))
                              (cond
                                ((sc-is x unsigned-reg)
                                 (inst tst x (ash 1 (- n-word-bits 1)))
                                 (inst ccmp x (imm lo) :eq 1)
                                 (inst ccmp x (imm hi) ,(if excl-low
                                                            :gt
                                                            :ge)))
                                ((sc-is hi immediate)
                                 (let ((hi (tn-value hi)))
                                   (change-vop-flags vop '(,(if excl-low
                                                                :gt
                                                                :ge)))
                                   (if (typep hi `(integer (,most-negative-fixnum) -1))
                                       (inst cmn x (imm (- hi) nil))
                                       (inst cmp x (imm hi nil)))
                                   (inst ccmp x (imm lo) ,(if excl-high :lt :le) 1)))
                                ((sc-is lo immediate)
                                 (let ((lo (tn-value lo)))
                                   (cond
                                     ((and (= lo ,(if excl-low
                                                      -1
                                                      0))
                                           (csubtypep (tn-ref-type hi-ref)
                                                      (specifier-type 'unsigned-byte)))
                                      (change-vop-flags vop '(,(if excl-high :lo :ls)))
                                      (inst cmp x (imm hi nil)))
                                     (t
                                      (if (typep lo `(integer (,most-negative-fixnum) -1))
                                          (inst cmn x (imm (- lo) nil))
                                          (inst cmp x (imm lo nil)))
                                      (inst ccmp x (imm hi) ,(if excl-low :gt :ge))))))
                                (t
                                 (inst cmp x (imm lo nil))
                                 (inst ccmp x (imm hi) ,(if excl-low :gt :ge)))))))))

                  (define-vop (,(symbolicate name '-integer/c))
                    (:translate ,name)
                    (:args (x :scs (descriptor-reg)))
                    (:arg-refs x-ref)
                    (:arg-types (:constant t) ,(if check
                                                t
                                                `(:or integer bignum)) (:constant t))
                    (:info lo hi)
                    (:temporary (:sc signed-reg
                                 :unused-if
                                 (cond ((or (= lo ,(if excl-low
                                                       -1
                                                       0))
                                            (= hi
                                               ,(if excl-high
                                                    0
                                                    -1))))))
                                temp)
                    (:conditional :ls)
                    (:vop-var vop)
                    (:policy :fast-safe)
                    (:generator 5
                      (let* ((orig-lo (+ lo ,@(and excl-low
                                                   `(1))))
                             (orig-hi (+ hi ,@(and excl-high
                                                   `(-1))))
                             (lo (fixnumize orig-lo))
                             (hi (fixnumize orig-hi))
                             (lowest-bignum-address (cond
                                                      ,@(and check
                                                             #.(progn (assert
                                                                       (< single-float-widetag character-widetag))
                                                                      t)
                                                             `(((types-equal-or-intersect (tn-ref-type x-ref) (specifier-type 'single-float))
                                                                single-float-widetag)
                                                               ((types-equal-or-intersect (tn-ref-type x-ref) (specifier-type 'character))
                                                                character-widetag)))
                                                      (t
                                                       #+darwin (expt 2 32)
                                                       #-darwin +backend-page-bytes+)))
                             (branch (sb-c::next-vop-is sb-assem::*current-vop* '(branch-if)))
                             (int (sb-c::type-approximate-interval (tn-ref-type x-ref))))
                        (when (sb-c::interval-low>=n int orig-lo)
                          (setf lo (if (>= orig-lo 0)
                                       0
                                       ,(fixnumize most-negative-fixnum))))
                        (when (sb-c::interval-high<=n int orig-hi)
                          (setf hi ,(fixnumize most-positive-fixnum)))
                        (destructuring-bind (&optional branch-label branch-not flags)
                            (and branch
                                 (sb-c::vop-codegen-info branch))
                          (declare (ignore flags))
                          (let ((tbz-label (and branch
                                                (if branch-not
                                                    branch-label
                                                    (sb-c::next-vop-label branch)))))
                            (cond
                              ((> lo hi)
                               (inst cmp null-tn 0))
                              ((= lo hi)
                               (change-vop-flags vop '(:eq))
                               (inst cmp x (add-sub-immediate lo)))
                              ((= hi ,(fixnumize -1))
                               (change-vop-flags vop '(:hs))
                               (cond (tbz-label
                                      (inst tbnz* x 0 tbz-label)
                                      (inst cmn x (add-sub-immediate (- lo))))
                                     (t
                                      (inst tst x fixnum-tag-mask)
                                      (inst ccmn x (ccmp-immediate (- lo)) :eq))))
                              ((eq lo ,(fixnumize most-positive-fixnum))
                               (change-vop-flags vop '(:le))
                               (cond (tbz-label
                                      (inst tbnz* x 0 tbz-label)
                                      (inst cmp x (add-sub-immediate hi)))
                                     (t
                                      (inst tst x fixnum-tag-mask)
                                      (inst ccmp x (ccmp-immediate hi) :eq))))
                              ((and (/= lo 0)
                                    (= hi ,(fixnumize most-positive-fixnum)))
                               (change-vop-flags vop '(:ge))
                               (cond (tbz-label
                                      (inst tbnz* x 0 tbz-label)
                                      (inst cmp x (add-sub-immediate lo)))
                                     (t
                                      (inst tst x fixnum-tag-mask)
                                      (inst ccmp x (ccmp-immediate lo) :eq #b1000))))
                              ((and (> lo 0)
                                    (= (logcount (+ hi (fixnumize 1))) 1))
                               (inst tst x (lognot hi))
                               (change-vop-flags vop '(:hs))
                               (inst ccmp x (ccmp-immediate lo) :eq))
                              (t
                               (if (zerop lo)
                                   (setf temp x)
                                   (if (plusp lo)
                                       (inst sub temp x (add-sub-immediate lo))
                                       (inst add temp x (add-sub-immediate (abs lo)))))
                               (let* ((diff (- hi lo))
                                      (loaded diff))
                                 (cond ((= (logcount (+ diff (fixnumize 1))) 1)
                                        (change-vop-flags vop '(:eq))
                                        (inst tst temp (lognot diff)))
                                       (t
                                        (unless (add-sub-immediate-p diff)
                                          (if (load-immediate-word tmp-tn (+ diff (fixnumize 1)) t)
                                              (change-vop-flags vop '(:lo))
                                              (load-immediate-word tmp-tn diff))
                                          (setf loaded tmp-tn))
                                        (cond
                                          ((< diff lowest-bignum-address)
                                           (inst cmp temp loaded))
                                          (tbz-label
                                           (inst tbnz* x 0 (if branch-not
                                                               branch-label
                                                               (sb-c::next-vop-label branch)))
                                           (inst cmp temp loaded))
                                          (t
                                           (inst tst x fixnum-tag-mask)
                                           (inst ccmp temp (ccmp-immediate loaded) :eq #b10)))))))))))))

                  (define-vop (,(symbolicate name '-integer))
                    (:translate ,name)
                    (:args (lo :scs (any-reg immediate))
                           (x :scs (descriptor-reg))
                           (hi :scs (any-reg immediate)))
                    (:arg-types tagged-num ,(if check
                                                t
                                                `(:or integer bignum)) tagged-num)
                    (:arg-refs lo-ref nil hi-ref)
                    (:conditional ,(if excl-high
                                       :lt
                                       :le))
                    (:vop-var vop)
                    (:policy :fast-safe)
                    (:generator 6
                      (labels ((imm (x)
                                 (if (sc-is x immediate)
                                     (fixnumize (tn-value x))
                                     x))
                               (ccmp (c cond &optional (flags 0))
                                 (if (typep c `(integer (,most-negative-fixnum) -1))
                                     (inst ccmn x (ccmp-immediate (- c)) cond flags)
                                     (inst ccmp x (ccmp-immediate c) cond flags))))
                        (inst tst x fixnum-tag-mask)
                        (cond ,@(unless excl-high
                                  `(((and (sc-is hi immediate)
                                          (= (tn-value hi) most-positive-fixnum))
                                     (change-vop-flags vop '(,(if excl-low
                                                                  :gt
                                                                  :ge)))
                                     (ccmp (imm lo) :eq #b1))))
                              ,@(unless excl-low
                                  `(((and (sc-is lo immediate)
                                          (= (tn-value lo) most-negative-fixnum))
                                     (ccmp (imm hi) :eq #b10))))
                              ((and (sc-is lo immediate)
                                    (csubtypep (tn-ref-type hi-ref)
                                               (specifier-type 'unsigned-byte))
                                    (eql (tn-value lo)
                                         ,(if excl-low
                                              -1
                                              0)))
                               (change-vop-flags vop '(,(if excl-high :lo :ls)))
                               (ccmp (imm hi) :eq #b10))
                              (t
                               (ccmp (imm lo) :eq 1)
                               (ccmp (imm hi) ,(if excl-low
                                                   :gt
                                                   :ge))))))))))

  (def range< t t)
  (def range<= nil nil)
  (def range<<= t nil)
  (def range<=< nil t)

  (def check-range<= nil nil t)
  (def check-range<<= t nil t)
  (def check-range<=< nil t t))


(define-vop (signed-multiply-low-high)
  (:policy :fast-safe)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:results (lo :scs (unsigned-reg))
            (hi :scs (signed-reg)))
  (:result-types unsigned-num signed-num)
  (:generator 2
    (inst smulh hi x y)
    (inst mul lo x y)))
