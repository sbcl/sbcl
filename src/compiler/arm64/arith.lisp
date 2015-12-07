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
  (:args (x :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:translate %negate)
  (:generator 3
    (inst neg res x)))

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
  (:note "inline (unsigned-byte 64) arithmetic"))

(define-vop (fast-signed-binop fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg))
         (y :target r :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 64) arithmetic"))

(define-vop (fast-fixnum-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg)))
  (:info y)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg)))
  (:info y)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic"))

(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg)))
  (:info y)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 64) arithmetic"))

(defun bic-encode-immediate (x)
  (encode-logical-immediate (bic-mask x)))

(defun bic-fixnum-encode-immediate (x)
  (and (fixnump x)
       (encode-logical-immediate (bic-mask (fixnumize x)))))

(defmacro define-binop (translate untagged-penalty op
                        &key
                             (constant-test 'encode-logical-immediate)
                             (constant-fixnum-test 'fixnum-encode-logical-immediate)
                             swap
                             (constant-op op)
                             (constant-transform 'identity))
  `(progn
     (define-vop (,(symbolicate 'fast- translate '/fixnum=>fixnum)
                  fast-fixnum-binop)
       (:translate ,translate)
       (:generator 2
                   ,(if swap
                        `(inst ,op r y x)
                        `(inst ,op r x y))))
     (define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum)
                    fast-fixnum-binop-c)
       ,(if swap
            `(:arg-types (:constant (satisfies ,constant-fixnum-test))
                         tagged-num)
            `(:arg-types tagged-num
                         (:constant (satisfies ,constant-fixnum-test))))
         (:translate ,translate)
         (:generator 1
                     (inst ,constant-op r x (,constant-transform (fixnumize y)))))
     (define-vop (,(symbolicate 'fast- translate '/signed=>signed)
                  fast-signed-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
                   ,(if swap
                        `(inst ,op r y x)
                        `(inst ,op r x y))))
     (define-vop (,(symbolicate 'fast- translate '-c/signed=>signed)
                   fast-signed-binop-c)
       (:translate ,translate)
       ,(if swap
            `(:arg-types (:constant (satisfies ,constant-test))
                         signed-num)
            `(:arg-types signed-num
                         (:constant (satisfies ,constant-test))))
       (:generator ,untagged-penalty
                   (inst ,constant-op r x (,constant-transform y))))
     (define-vop (,(symbolicate 'fast- translate '/unsigned=>unsigned)
                  fast-unsigned-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
                   ,(if swap
                        `(inst ,op r y x)
                        `(inst ,op r x y))))
     (define-vop (,(symbolicate 'fast- translate '-c/unsigned=>unsigned)
                   fast-unsigned-binop-c)
       (:translate ,translate)
       ,(if swap
            `(:arg-types (:constant (satisfies ,constant-test))
                         unsigned-num)
            `(:arg-types unsigned-num
                         (:constant (satisfies ,constant-test))))
       (:generator ,untagged-penalty
                   (inst ,constant-op r x (,constant-transform y))))))

(define-binop + 4 add :constant-test add-sub-immediate-p :constant-fixnum-test fixnum-add-sub-immediate-p)
(define-binop - 4 sub :constant-test add-sub-immediate-p :constant-fixnum-test fixnum-add-sub-immediate-p)
(define-binop logand 2 and)
(define-binop logior 2 orr)
(define-binop logxor 2 eor)

(define-binop logandc1 2 bic :swap t
  :constant-test bic-encode-immediate
  :constant-fixnum-test bic-fixnum-encode-immediate
  :constant-op and
  :constant-transform bic-mask)
(define-binop logandc2 2 bic
  :constant-test bic-encode-immediate
  :constant-fixnum-test bic-fixnum-encode-immediate
  :constant-op and
  :constant-transform bic-mask)

;; (define-binop logorc1 2 orn :swap t
;;   :constant-test bic-encode-immediate
;;   :constant-fixnum-test bic-fixnum-encode-immediate
;;   :constant-op orr
;;   :constant-transform bic-mask)
;; (define-binop logorc2 2 orn
;;   :constant-test bic-encode-immediate
;;   :constant-fixnum-test bic-fixnum-encode-immediate
;;   :constant-op orr
;;   :constant-transform bic-mask)

(define-vop (fast-logior-unsigned-signed=>signed fast-safe-arith-op)
  (:args (x :scs (unsigned-reg))
         (y :target r :scs (signed-reg)))
  (:arg-types unsigned-num signed-num)
  (:results (r :scs (signed-reg) :from (:argument 1)))
  (:result-types signed-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:translate logior)
  (:generator 3
    (inst orr r x y)))

(define-vop (fast-logior-signed-unsigned=>signed fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg))
         (y :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:results (r :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:note "inline (unsigned-byte 64) arithmetic")
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

(define-vop (fast-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:generator 3
    (inst mul r x y)))

(define-vop (fast-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:generator 3
    (inst mul r x y)))

;;; Division
(define-vop (fast-truncate/signed=>signed fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg) :to :result)
         (y :scs (signed-reg) :to :result))
  (:arg-types signed-num signed-num)
  (:results (quo :scs (signed-reg) :from :eval)
            (rem :scs (signed-reg) :from :eval))
  (:result-types signed-num signed-num)
  (:note "inline (signed-byte 64) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 33
    (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
      (inst cbz y zero))
    (inst sdiv quo x y)
    (inst msub rem quo y x)))

(define-vop (fast-truncate/unsigned=>unsigned fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (unsigned-reg) :to :result))
  (:arg-types unsigned-num unsigned-num)
  (:results (quo :scs (unsigned-reg) :from :eval)
            (rem :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 33
    (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
      (inst cbz y zero))
    (inst udiv quo x y)
    (inst msub rem quo y x)))

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
    (if (< amount 64)
        (inst lsl result number amount)
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
    (inst asr temp number (min (- amount) 63))
    (inst and result temp (bic-mask fixnum-tag-mask))))

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
    (cond ((< -64 amount 64)
           (if (plusp amount)
               (inst lsl result number amount)
               (inst lsr result number (- amount))))
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
    (cond ((< -64 amount 64)
           (if (plusp amount)
               (inst lsl result number amount)
               (inst asr result number (- amount))))
          (t
           (inst mov result 0)))))

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
    (inst neg temp temp)
    (inst cmp temp sb!vm:n-word-bits)
    (inst b :lt DO)
    (inst mov temp (1- sb!vm:n-word-bits))
    DO
    (ecase variant
      (:signed (inst asr result number temp))
      (:unsigned (inst lsr result number temp)))
    (inst b END)
    LEFT
    (inst cmp temp sb!vm:n-word-bits)
    (inst b :lt DO2)
    (inst mov temp (1- sb!vm:n-word-bits))
    DO2
    (inst lsl result number temp)
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
                       (amount :scs (signed-reg unsigned-reg)))
                ;; For modular variants
                (:variant-vars cut)
                (:arg-types ,type positive-fixnum)
                (:results (result :scs (,result-type)))
                (:result-types ,type)
                (:policy :fast-safe)
                (:generator ,cost
                  (let ((amount (cond (cut
                                       (inst cmp amount sb!vm:n-word-bits)
                                       ;; Only the first 6 bits count for shifts.
                                       ;; This sets all bits to 1 if AMOUNT is larger than 63,
                                       ;; cutting the amount to 63.
                                       (inst csinv tmp-tn amount zr-tn :lt)
                                       tmp-tn)
                                      (t
                                       amount))))
                    (inst lsl result number amount))))))
  ;; FIXME: There's the opportunity for a sneaky optimization here, I
  ;; think: a FAST-ASH-LEFT-C/FIXNUM=>SIGNED vop.  -- CSR, 2003-09-03
  (def fast-ash-left/fixnum=>fixnum any-reg tagged-num any-reg 2)
  (def fast-ash-left/signed=>signed signed-reg signed-num signed-reg 3)
  (def fast-ash-left/unsigned=>unsigned unsigned-reg unsigned-num unsigned-reg 3))

#!+ash-right-vops
(define-vop (fast-%ash/right/unsigned)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg) :target result)
         (amount :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (result :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:generator 4
     (inst lsr result number amount)))

#!+ash-right-vops
(define-vop (fast-%ash/right/signed)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg) :target result)
         (amount :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:results (result :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:generator 4
    (inst asr result number amount)))

#!+ash-right-vops
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
    (inst asr temp number amount)
    (inst and result temp (bic-mask fixnum-tag-mask))))

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
             fast-ash-c/unsigned=>unsigned)
  (:translate ash-left-mod64))

;;; Only the lower 6 bits of the shift amount are significant.
(define-vop (shift-towards-someplace)
  (:policy :fast-safe)
  (:args (num :scs (unsigned-reg))
         (amount :scs (signed-reg)))
  (:arg-types unsigned-num tagged-num)
  (:temporary (:sc signed-reg) temp)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (shift-towards-start shift-towards-someplace)
  (:translate shift-towards-start)
  (:note "SHIFT-TOWARDS-START")
  (:generator 1
    (inst and temp amount #b111111)
    (inst lsr r num temp)))

(define-vop (shift-towards-end shift-towards-someplace)
  (:translate shift-towards-end)
  (:note "SHIFT-TOWARDS-END")
  (:generator 1
    (inst and temp amount #b111111)
    (inst lsl r num temp)))

(define-vop (signed-byte-64-len)
  (:translate integer-length)
  (:note "inline (signed-byte 64) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg) :target temp))
  (:arg-types signed-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) temp)
  (:generator 30
    (move temp arg)
    (inst cmp temp 0)
    (inst csinv temp temp temp :ge)
    (inst clz temp temp)
    (inst mov res (fixnumize 64))
    (inst sub res res (lsl temp n-fixnum-tag-bits))))

(define-vop (unsigned-byte-64-count)
  (:translate logcount)
  (:note "inline (unsigned-byte 64) logcount")
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
    (load-immediate-word mask #x5555555555555555)
    (inst and temp mask (lsr num 1))
    (inst and num num mask)
    (inst add num num temp)
    (load-immediate-word mask #x3333333333333333)
    (inst and temp mask (lsr num 2))
    (inst and num num mask)
    (inst add num num temp)
    (load-immediate-word mask #x0f0f0f0f0f0f0f0f)
    (inst and temp mask (lsr num 4))
    (inst and num num mask)
    (inst add num num temp)
    (inst add num num (lsr num 8))
    (inst add num num (lsr num 16))
    (inst add num num (lsr num 32))
    (inst and res num #xff)))

(defknown %%ldb (integer unsigned-byte unsigned-byte) unsigned-byte
  (movable foldable flushable always-translatable))

(defknown %%dpb (integer unsigned-byte unsigned-byte integer) integer
  (movable foldable flushable always-translatable))

;;; Constant folding
(defun %%ldb (integer size posn)
  (%ldb size posn integer))

(defun %%dpb (newbyte integer size posn)
  (%dpb newbyte size posn integer))

(define-vop (ldb-c/fixnum)
  (:translate %%ldb)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num
              (:constant integer) (:constant integer))
  (:info size posn)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 2
    (inst ubfm res x (1+ posn) (+ posn size))))

(define-vop (ldb-c)
  (:translate %%ldb)
  (:args (x :scs (unsigned-reg signed-reg)))
  (:arg-types (:or unsigned-num signed-num)
              (:constant integer) (:constant integer))
  (:info size posn)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 3
    (inst ubfm res x posn (+ posn size -1))))

(define-vop (dpb-c/fixnum)
  (:translate %%dpb)
  (:args (x :scs (signed-reg) :to :save)
         (y :scs (any-reg)))
  (:arg-types signed-num
              (:constant integer) (:constant integer)
              tagged-num)
  (:info size posn)
  (:results (res :scs (any-reg)))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:generator 2
    (move res y)
    (inst bfm res x (- (1- n-word-bits) posn) (1- size))))

(define-vop (dpb-c/signed)
  (:translate %%dpb)
  (:args (x :scs (signed-reg) :to :save)
         (y :scs (signed-reg)))
  (:arg-types signed-num
              (:constant integer) (:constant integer)
              signed-num)
  (:info size posn)
  (:results (res :scs (signed-reg)))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:generator 3
    (move res y)
    (inst bfm res x (- n-word-bits posn) (1- size))))

(define-vop (dpb-c/unsigned)
  (:translate %%dpb)
  (:args (x :scs (unsigned-reg) :to :save)
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num
              (:constant integer) (:constant integer)
              unsigned-num)
  (:info size posn)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 3
    (move res y)
    (inst bfm res x (- n-word-bits posn) (1- size))))

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
     (:args (x :target r :scs (unsigned-reg signed-reg))
            (y :scs (unsigned-reg signed-reg)))
     (:arg-types untagged-num untagged-num)
     (:results (r :scs (unsigned-reg signed-reg) :from (:argument 0)))
     (:result-types unsigned-num)
     (:translate ,function)))

(defmacro define-mod-binop-c ((name prototype) function)
  `(define-vop (,name ,prototype)
     (:args (x :target r :scs (unsigned-reg signed-reg)))
     (:info y)
     (:arg-types untagged-num (:constant (or word
                                             signed-word)))
     (:results (r :scs (unsigned-reg signed-reg) :from (:argument 0)))
     (:result-types unsigned-num)
     (:translate ,function)))

(macrolet ((def (name -c-p)
             (let ((fun64 (intern (format nil "~S-MOD64" name)))
                   (vopu (intern (format nil "FAST-~S/UNSIGNED=>UNSIGNED" name)))
                   (vopcu (intern (format nil "FAST-~S-C/UNSIGNED=>UNSIGNED" name)))
                   (vopf (intern (format nil "FAST-~S/FIXNUM=>FIXNUM" name)))
                   (vopcf (intern (format nil "FAST-~S-C/FIXNUM=>FIXNUM" name)))
                   (vop64u (intern (format nil "FAST-~S-MOD64/WORD=>UNSIGNED" name)))
                   (vop64f (intern (format nil "FAST-~S-MOD64/FIXNUM=>FIXNUM" name)))
                   (vop64cu (intern (format nil "FAST-~S-MOD64-C/WORD=>UNSIGNED" name)))
                   (funfx (intern (format nil "~S-MODFX" name)))
                   (vopfxf (intern (format nil "FAST-~S-MODFX/FIXNUM=>FIXNUM" name)))
                   (vopfxcf (intern (format nil "FAST-~S-MODFX-C/FIXNUM=>FIXNUM" name))))
               `(progn
                  (define-modular-fun ,fun64 (x y) ,name :untagged nil 64)
                  (define-modular-fun ,funfx (x y) ,name :tagged t
                                      #.(- n-word-bits n-fixnum-tag-bits))
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
  (:arg-types tagged-num (:constant (satisfies fixnum-add-sub-immediate-p)))
  (:info y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 64) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num (:constant (satisfies add-sub-immediate-p)))
  (:info y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 64) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num (:constant (satisfies add-sub-immediate-p)))
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
  (:arg-types tagged-num (:constant (satisfies fixnum-add-sub-immediate-p)))
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
  (:arg-types * (:constant (satisfies fixnum-add-sub-immediate-p)))
  (:variant-cost 6))

;; (macrolet ((define-logtest-vops ()
;;              `(progn
;;                 ,@(loop for suffix in '(/fixnum -c/fixnum
;;                                         /signed -c/signed
;;                                         /unsigned -c/unsigned)
;;                         for cost in '(4 3 6 5 6 5)
;;                         collect
;;                         `(define-vop (,(symbolicate "FAST-LOGTEST" suffix)
;;                                       ,(symbolicate "FAST-CONDITIONAL" suffix))
;;                            (:translate logtest)
;;                            (:conditional :ne)
;;                            (:generator ,cost
;;                                        (inst tst x
;;                                              ,(case suffix
;;                                                 (-c/fixnum
;;                                                  `(fixnumize y))
;;                                                 ((-c/signed -c/unsigned)
;;                                                  `y)
;;                                                 (t
;;                                                  'y)))))))))
;;   (define-logtest-vops))

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
  (:arg-types tagged-num (:constant (integer 0 63)))
  (:generator 4
    (inst tst x (ash 1 (+ y n-fixnum-tag-bits)))))

(define-vop (fast-logbitp-c/signed fast-conditional-c/signed)
  (:translate %logbitp)
  (:conditional :ne)
  (:arg-types signed-num (:constant (integer 0 63)))
  (:generator 5
    (inst tst x (ash 1 y))))

(define-vop (fast-logbitp-c/unsigned fast-conditional-c/unsigned)
  (:translate %logbitp)
  (:conditional :ne)
  (:arg-types unsigned-num (:constant (integer 0 63)))
  (:generator 5
    (inst tst x (ash 1 y))))

;; Specialised mask-signed-field VOPs.
(define-vop (mask-signed-field-word/c)
  (:translate sb!c::mask-signed-field)
  (:policy :fast-safe)
  (:args (x :scs (signed-reg unsigned-reg) :target r))
  (:arg-types (:constant (integer 0 64)) untagged-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:info width)
  (:generator 3
    (cond ((zerop width)
           (inst mov r 0))
          ((= width 64)
           (move r x))
          (t
           (let ((delta (- n-word-bits width)))
             (inst lsl r x delta)
             (inst asr r r delta))))))

(define-vop (mask-signed-field-bignum/c)
  (:translate sb!c::mask-signed-field)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target r))
  (:arg-types (:constant (integer 0 64)) bignum)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:info width)
  (:generator 4
    (cond ((zerop width)
           (inst mov r 0))
          (t
           (loadw r x bignum-digits-offset other-pointer-lowtag)
           (let ((delta (- n-word-bits width)))
             (inst lsl r r delta)
             (inst asr r r delta))))))
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

(define-vop (digit-0-or-plus)
  (:translate sb!bignum:%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:conditional)
  (:info target not-p)
  (:generator 2
     (inst cmp digit 0)
     (inst b (if not-p :lt :ge) target)))

(define-vop (add-w/carry)
  (:translate sb!bignum:%add-with-carry)
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
    (inst cset carry :cs)))

(define-vop (sub-w/borrow)
  (:translate sb!bignum:%subtract-with-borrow)
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
    (inst cset borrow :cs)))

(define-vop (bignum-mult-and-add-3-arg)
  (:translate sb!bignum:%multiply-and-add)
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
  (:translate sb!bignum:%multiply-and-add)
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
  (:translate sb!bignum:%multiply)
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

#!+multiply-high-vops
(define-vop (mulhi)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target hi)
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 20
    (inst umulh hi x y)))

#!+multiply-high-vops
(define-vop (mulhi/fx)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (any-reg) :target hi)
         (y :scs (unsigned-reg)))
  (:arg-types positive-fixnum unsigned-num)
  (:temporary (:sc unsigned-reg) temp)
  (:results (hi :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 15
    (inst umulh temp x y)
    (inst and hi temp (bic-mask fixnum-tag-mask))))

(define-vop (bignum-lognot lognot-mod64/unsigned=>unsigned)
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
    (dotimes (i 65)
      (assemble ()
        (inst cmp rem divisor)
        (inst b :cc CC)
        (inst sub rem rem divisor)
        CC
        (inst adcs quo quo quo)
        (unless (= i 64)
          (inst adc rem rem rem))))))

(define-vop (signify-digit)
  (:translate sb!bignum:%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (if (sc-is res any-reg)
        (inst lsl res digit n-fixnum-tag-bits)
        (inst mov res digit))))

(define-vop (digit-ashr)
  (:translate sb!bignum:%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg))
         (count :scs (unsigned-reg)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst asr result digit count)))

(define-vop (digit-lshr digit-ashr)
  (:translate sb!bignum:%digit-logical-shift-right)
  (:generator 1
    (inst lsr result digit count)))

(define-vop (digit-ashl digit-ashr)
  (:translate sb!bignum:%ashl)
  (:generator 1
    (inst lsl result digit count)))

;;;; Static functions.

(define-static-fun two-arg-gcd (x y) :translate gcd)
(define-static-fun two-arg-lcm (x y) :translate lcm)

(define-static-fun two-arg-+ (x y) :translate +)
(define-static-fun two-arg-- (x y) :translate -)
(define-static-fun two-arg-* (x y) :translate *)
(define-static-fun two-arg-/ (x y) :translate /)

(define-static-fun two-arg-< (x y) :translate <)
(define-static-fun two-arg-> (x y) :translate >)
(define-static-fun two-arg-= (x y) :translate =)

(define-static-fun two-arg-and (x y) :translate logand)
(define-static-fun two-arg-ior (x y) :translate logior)
(define-static-fun two-arg-xor (x y) :translate logxor)
(define-static-fun two-arg-eqv (x y) :translate logeqv)

(define-static-fun eql (x y) :translate eql)

(define-static-fun %negate (x) :translate %negate)
