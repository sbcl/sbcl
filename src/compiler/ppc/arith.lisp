;;;; the VM definition arithmetic VOPs for the PPC

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; Unary operations.

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
    (inst neg res x)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (inst neg res x)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:translate lognot)
  (:generator 1
    (inst subfic res x (fixnumize -1))))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 2
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

(define-vop (fast-fixnum-binop30-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg zero)))
  (:info y)
  (:arg-types tagged-num
              (:constant (and (signed-byte 30) (not (integer 0 0)))))
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

(define-vop (fast-fixnum-logop30-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg zero)))
  (:info y)
  (:arg-types tagged-num
              (:constant (and (unsigned-byte 16) (not (integer 0 0)))))
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

(define-vop (fast-unsigned-binop32-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg zero)))
  (:info y)
  (:arg-types unsigned-num
              (:constant (and (unsigned-byte 32) (not (integer 0 0)))))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-signed-binop32-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg zero)))
  (:info y)
  (:arg-types signed-num
              (:constant (and (signed-byte 32) (not (integer 0 0)))))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic"))

(define-vop (fast-unsigned-logop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg zero)))
  (:info y)
  (:arg-types unsigned-num
              (:constant (and (unsigned-byte 16) (not (integer 0 0)))))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) logical op"))

(define-vop (fast-unsigned-logop32-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg zero)))
  (:info y)
  (:arg-types unsigned-num
              (:constant (and (unsigned-byte 32) (not (integer 0 0)))))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) logical op"))

(define-vop (fast-signed-logop32-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg zero)))
  (:info y)
  (:arg-types signed-num
              (:constant (and (unsigned-byte 32) (not (integer 0 0)))))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) logical op"))

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
  (:note "inline (signed-byte 32) logical op"))

(defmacro !define-var-binop (translate untagged-penalty op
                             &optional arg-swap restore-fixnum-mask)
  `(progn
     (define-vop (,(symbolicate "FAST-" translate "/FIXNUM=>FIXNUM")
                  fast-fixnum-binop)
       ,@(when restore-fixnum-mask
           `((:temporary (:sc non-descriptor-reg) temp)))
       (:translate ,translate)
       (:generator 2
         ,(if arg-swap
             `(inst ,op ,(if restore-fixnum-mask 'temp 'r) y x)
             `(inst ,op ,(if restore-fixnum-mask 'temp 'r) x y))
         ;; FIXME: remind me what convention we used for 64bitizing
         ;; stuff?  -- CSR, 2003-08-27
         ,@(when restore-fixnum-mask
             `((inst clrrwi r temp n-fixnum-tag-bits)))))
     (define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
                  fast-signed-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
         ,(if arg-swap
             `(inst ,op r y x)
             `(inst ,op r x y))))
     (define-vop (,(symbolicate "FAST-" translate "/UNSIGNED=>UNSIGNED")
                  fast-unsigned-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
         ,(if arg-swap
             `(inst ,op r y x)
             `(inst ,op r x y))))))

;;; FIXME: the code has really only been checked for adds; we could do
;;; subtracts, too, but my brain is not up to the task of figuring out
;;; signs and borrows.
(defmacro !define-const-binop (translate untagged-penalty op &optional (shifted-op nil))
  `(progn
     (define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum)
                  ,(if shifted-op
                       'fast-fixnum-binop30-c
                       'fast-fixnum-binop-c))
       (:translate ,translate)
       ,@(when shifted-op
          `((:temporary (:sc any-reg :target r) temp)))
       (:generator 1
        ,(if shifted-op
             `(let* ((y (fixnumize y))
                     (high-half (ldb (byte 16 16) y))
                     (low-half (ldb (byte 16 0) y)))
               ;; Compare %LR in insts.lisp.
               (cond
                 ((and (logbitp 15 low-half) (= high-half #xffff))
                  ;; Let sign-extension do the work for us, but make sure
                  ;; to turn LOW-HALF into a signed integer.
                  (inst ,op r x (dpb low-half (byte 16 0) -1)))
                 ((and (not (logbitp 15 low-half)) (zerop high-half))
                  (inst ,op r x low-half))
                 ((zerop low-half)
                  (inst ,shifted-op r x (if (logbitp 15 high-half)
                                            (dpb high-half (byte 16 0) -1)
                                            high-half)))
                 (t
                  ;; Check to see whether compensating for the sign bit
                  ;; of LOW-HALF is necessary.
                  (let ((high-half (let ((top (if (logbitp 15 low-half)
                                                  (ldb (byte 16 0)
                                                       (1+ high-half))
                                                  high-half)))
                                     (if (logbitp 15 top)
                                         (dpb top (byte 16 0) -1)
                                         top))))
                    (inst ,shifted-op temp x high-half)
                    (inst ,op r temp low-half)))))
             `(inst ,op r x (fixnumize y)))))
     (define-vop (,(symbolicate 'fast- translate '-c/signed=>signed)
                  ,(if shifted-op
                       'fast-signed-binop32-c
                       'fast-signed-binop-c))
       (:translate ,translate)
       ,@(when shifted-op
          `((:temporary (:sc non-descriptor-reg :target r) temp)))
       (:generator ,untagged-penalty
        ,(if shifted-op
             `(let ((high-half (ldb (byte 16 16) y))
                    (low-half (ldb (byte 16 0) y)))
               ;; Compare %LR in insts.lisp.
               (cond
                 ((and (logbitp 15 low-half) (= high-half #xffff))
                  ;; Let sign-extension do the work for us, but make sure
                  ;; to turn LOW-HALF into a signed integer.
                  (inst ,op r x (dpb low-half (byte 16 0) -1)))
                 ((and (not (logbitp 15 low-half)) (zerop high-half))
                  (inst ,op r x low-half))
                 ((zerop low-half)
                  (inst ,shifted-op r x (if (logbitp 15 high-half)
                                            (dpb high-half (byte 16 0) -1)
                                            high-half)))
                 (t
                  ;; Check to see whether compensating for the sign bit
                  ;; of LOW-HALF is necessary.
                  (let ((high-half (let ((top (if (logbitp 15 low-half)
                                                  (ldb (byte 16 0)
                                                       (1+ high-half))
                                                  high-half)))
                                     (if (logbitp 15 top)
                                         (dpb top (byte 16 0) -1)
                                         top))))
                    (inst ,shifted-op temp x high-half)
                    (inst ,op r temp low-half)))))
             `(inst ,op r x y))))
     (define-vop (,(symbolicate 'fast- translate '-c/unsigned=>unsigned)
                  ,(if shifted-op
                       'fast-unsigned-binop32-c
                       'fast-unsigned-binop-c))
       (:translate ,translate)
       ,@(when shifted-op
          `((:temporary (:sc non-descriptor-reg :target r) temp)))
       (:generator ,untagged-penalty
        ,(if shifted-op
             `(let ((high-half (ldb (byte 16 16) y))
                    (low-half (ldb (byte 16 0) y)))
               ;; Compare %LR in insts.lisp.
               (cond
                 ((and (logbitp 15 low-half) (= high-half #xffff))
                  ;; Let sign-extension do the work for us, but make sure
                  ;; to turn LOW-HALF into a signed integer.
                  (inst ,op r x (dpb low-half (byte 16 0) -1)))
                 ((and (not (logbitp 15 low-half)) (zerop high-half))
                  (inst ,op r x low-half))
                 ((zerop low-half)
                  (inst ,shifted-op r x (if (logbitp 15 high-half)
                                            (dpb high-half (byte 16 0) -1)
                                            high-half)))
                 (t
                  ;; Check to see whether compensating for the sign bit
                  ;; of LOW-HALF is necessary.
                  (let ((high-half (let ((top (if (logbitp 15 low-half)
                                                  (ldb (byte 16 0)
                                                       (1+ high-half))
                                                  high-half)))
                                     (if (logbitp 15 top)
                                         (dpb top (byte 16 0) -1)
                                         top))))
                    (inst ,shifted-op temp x high-half)
                    (inst ,op r temp low-half)))))
             `(inst ,op r x y))))))

;;; For logical operations, we don't have to worry about signed bit
;;; propagation from the lower half of a 32-bit operand.
(defmacro !define-const-logop (translate untagged-penalty op &optional (shifted-op nil))
  `(progn
     (define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum)
                  ,(if shifted-op
                       'fast-fixnum-logop30-c
                       'fast-fixnum-logop-c))
       (:translate ,translate)
       ,@(when shifted-op
          `((:temporary (:sc any-reg :target r) temp)))
       (:generator 1
        ,(if shifted-op
             `(let* ((y (fixnumize y))
                     (high-half (ldb (byte 16 16) y))
                     (low-half (ldb (byte 16 0) y)))
               (cond
                 ((zerop high-half) (inst ,op r x low-half))
                 ((zerop low-half) (inst ,shifted-op r x high-half))
                 (t
                  (inst ,shifted-op temp x high-half)
                  (inst ,op r temp low-half))))
             `(inst ,op r x (fixnumize y)))))
     (define-vop (,(symbolicate 'fast- translate '-c/signed=>signed)
                  ,(if shifted-op
                       'fast-signed-logop32-c
                       'fast-signed-logop-c))
       (:translate ,translate)
       ,@(when shifted-op
          `((:temporary (:sc non-descriptor-reg :target r) temp)))
       (:generator ,untagged-penalty
        ,(if shifted-op
             `(let ((high-half (ldb (byte 16 16) y))
                    (low-half (ldb (byte 16 0) y)))
               (cond
                 ((zerop high-half) (inst ,op r x low-half))
                 ((zerop low-half) (inst ,shifted-op r x high-half))
                 (t
                  (inst ,shifted-op temp x high-half)
                  (inst ,op r temp low-half))))
             `(inst ,op r x y))))
     (define-vop (,(symbolicate 'fast- translate '-c/unsigned=>unsigned)
                  ,(if shifted-op
                       'fast-unsigned-logop32-c
                       'fast-unsigned-logop-c))
       (:translate ,translate)
       ,@(when shifted-op
          `((:temporary (:sc non-descriptor-reg :target r) temp)))
       (:generator ,untagged-penalty
        ,(if shifted-op
             `(let ((high-half (ldb (byte 16 16) y))
                    (low-half (ldb (byte 16 0) y)))
               (cond
                 ((zerop high-half) (inst ,op r x low-half))
                 ((zerop low-half) (inst ,shifted-op r x high-half))
                 (t
                  (inst ,shifted-op temp x high-half)
                  (inst ,op r temp low-half))))
             `(inst ,op r x y))))))

(!define-var-binop + 4 add)
(!define-var-binop - 4 sub)
(!define-var-binop logand 2 and)
(!define-var-binop logandc2 2 andc)
(!define-var-binop logior 2 or)
(!define-var-binop logorc1 2 orc t t)
(!define-var-binop logorc2 2 orc nil t)
(!define-var-binop logxor 2 xor)
(!define-var-binop logeqv 2 eqv nil t)
(!define-var-binop lognand 2 nand nil t)
(!define-var-binop lognor 2 nor nil t)

(!define-const-binop + 4 addi addis)
(!define-const-binop - 4 subi)
;;; Implementing a 32-bit immediate version of LOGAND wouldn't be any
;;; better than loading the 32-bit constant via LR and then performing
;;; an /AND/.  So don't bother.  (It would be better in some cases, such
;;; as when one half of the word is zeros--we save a register--but we
;;; would have specified one temporary register in the VOP, so we lose
;;; any possible advantage.)
(!define-const-logop logand 2 andi.)
(!define-const-logop logior 2 ori oris)
(!define-const-logop logxor 2 xori xoris)

(define-vop (fast-logandc2/unsigned-signed=>unsigned fast-logandc2/unsigned=>unsigned)
  (:args (x :scs (unsigned-reg))
         (y :scs (signed-reg)))
  (:arg-types unsigned-num signed-num))

(define-vop (fast-logand/signed-unsigned=>unsigned fast-logand/unsigned=>unsigned)
  (:args (x :scs (signed-reg) :target r)
         (y :scs (unsigned-reg) :target r))
  (:arg-types signed-num unsigned-num))

(define-vop (fast-logand-c/signed-unsigned=>unsigned fast-logand-c/unsigned=>unsigned)
  (:args (x :scs (signed-reg) :target r))
  (:arg-types signed-num (:constant (eql #.most-positive-word)))
  (:ignore y)
  (:generator 1
    (move r x)))

(define-vop (fast-*/fixnum=>fixnum fast-fixnum-binop)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:translate *)
  (:generator 2
    (inst srawi temp y n-fixnum-tag-bits)
    (inst mullw r x temp)))

(define-vop (fast-*-c/fixnum=>fixnum fast-fixnum-binop-c)
  (:translate *)
  (:arg-types tagged-num
              (:constant (and (signed-byte 16) (not (integer 0 0)))))
  (:generator 1
    (inst mulli r x y)))

(define-vop (fast-*-bigc/fixnum=>fixnum fast-fixnum-binop-c)
  (:translate *)
  (:arg-types tagged-num
              (:constant (and fixnum (not (signed-byte 16)))))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 1
    (inst lr temp y)
    (inst mullw r x temp)))

(define-vop (fast-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:generator 4
    (inst mullw r x y)))

(define-vop (fast-*-c/signed=>signed fast-signed-binop-c)
  (:translate *)
  (:generator 3
    (inst mulli r x y)))

(define-vop (fast-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:generator 4
    (inst mullw r x y)))

(define-vop (fast-*-c/unsigned=>unsigned fast-unsigned-binop-c)
  (:translate *)
  (:generator 3
    (inst mulli r x y)))

;;; Shifting

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
                      (inst slw result number amount))
                     (immediate
                      (let ((amount (tn-value amount)))
                        (aver (> amount 0))
                        (inst slwi result number amount))))))))
  ;; FIXME: There's the opportunity for a sneaky optimization here, I
  ;; think: a FAST-ASH-LEFT-C/FIXNUM=>SIGNED vop.  -- CSR, 2003-09-03
  (def fast-ash-left/fixnum=>fixnum any-reg tagged-num any-reg 2)
  (def fast-ash-left/signed=>signed signed-reg signed-num signed-reg 3)
  (def fast-ash-left/unsigned=>unsigned unsigned-reg unsigned-num unsigned-reg 3))

(define-vop (fast-ash/unsigned=>unsigned)
  (:note "inline ASH")
  (:args (number :scs (unsigned-reg) :to :save)
         (amount :scs (signed-reg)))
  (:arg-types (:or unsigned-num) signed-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) ndesc)
  (:generator 5
    (let ((positive (gen-label))
          (done (gen-label)))
      (inst cmpwi amount 0)
      (inst neg ndesc amount)
      (inst bge positive)
      (inst cmplwi ndesc 31)
      (inst srw result number ndesc)
      (inst ble done)
      (move result zero-tn)
      (inst b done)

      (emit-label positive)
      ;; The result-type assures us that this shift will not overflow.
      (inst slw result number amount)

      (emit-label done))))

(define-vop (fast-ash-c/unsigned=>unsigned)
  (:note "inline constant ASH")
  (:args (number :scs (unsigned-reg)))
  (:info amount)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash)
  (:policy :fast-safe)
  (:generator 4
    (cond
      ((and (minusp amount) (< amount -31)) (move result zero-tn))
      ((minusp amount) (inst srwi result number (- amount)))
      ;; possible because this is used in the modular version too
      ((> amount 31) (move result zero-tn))
      (t (inst slwi result number amount)))))

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
         (inst cmplwi ndesc 31)
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

(define-vop (fast-%ash/right/unsigned)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg)) (amount :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst srw result number amount)))

(define-vop (fast-%ash/right/signed)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg)) (amount :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (inst sraw result number amount)))

(define-vop (fast-%ash/right/fixnum)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (any-reg)) (amount :scs (unsigned-reg)))
  (:arg-types tagged-num unsigned-num)
  (:results (result :scs (any-reg)))
  (:result-types tagged-num)
  (:temporary (:sc signed-reg) temp)
  (:generator 2
    (inst sraw temp number amount)
    (inst clrrwi result temp n-fixnum-tag-bits)))

(define-vop (signed-byte-32-len)
  (:translate integer-length)
  (:note "inline (signed-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg)))
  (:arg-types signed-num)
  (:results (res :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num)
  (:generator 6
    ; (integer-length arg) = (- 32 (cntlz (if (>= arg 0) arg (lognot arg))))
    (let ((nonneg (gen-label)))
      (inst cntlzw. res arg)
      (inst bne nonneg)
      (inst not res arg)
      (inst cntlzw res res)
      (emit-label nonneg)
      (inst subfic res res 32))))

(define-vop (unsigned-byte-32-len)
  (:translate integer-length)
  (:note "inline (unsigned-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
    (inst cntlzw res arg)
    (inst subfic res res 32)))

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


;;;; %LDB

(define-vop (ldb-c/fixnum)
  (:translate %ldb)
  (:args (x :scs (any-reg)))
  (:arg-types (:constant (integer 1 29)) (:constant (integer 0 29)) tagged-num)
  (:info size posn)
  (:results (res :scs (any-reg)))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:generator 2
    (let ((phantom-bits (- (+ size posn) 30)))
      (cond
        ((plusp phantom-bits)
         ;; The byte to be loaded into RES includes sign bits which are not
         ;; present in the input X physically.  RLWINM as used below would
         ;; mask these out with 0 even for negative inputs.
         (inst srawi res x phantom-bits)
         (inst rlwinm res res
               (mod (- 32 posn (- phantom-bits)) 32)
               (- 32 size n-fixnum-tag-bits)
               (- 31 n-fixnum-tag-bits)))
        (t
         (inst rlwinm res x
               (mod (- 32 posn) 32)     ; effectively rotate right
               (- 32 size n-fixnum-tag-bits)
               (- 31 n-fixnum-tag-bits)))))))

(deftransform %ldb ((size posn integer) (:or (((constant-arg (integer 1 #.(1- n-word-bits)))
                                               (constant-arg integer)
                                               word) unsigned-byte)
                                             (((constant-arg (integer 1 #.(1- n-word-bits)))
                                               (constant-arg integer)
                                               signed-word) unsigned-byte)) *
                    :vop t)
  (<= (+ (sb-c:lvar-value size)
         (sb-c:lvar-value posn))
      n-word-bits))

(define-vop (ldb-c/signed)
  (:translate %ldb)
  (:args (x :scs (signed-reg)))
  (:arg-types (:constant (integer 1 29)) (:constant (integer 0 31)) signed-num)
  (:info size posn)
  (:results (res :scs (any-reg)))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:generator 3
    (inst rlwinm res x
          (mod (- (+ 32 n-fixnum-tag-bits) posn) 32)
          (- 32 size n-fixnum-tag-bits)
          (- 31 n-fixnum-tag-bits))))

(define-vop (ldb-c/unsigned)
  (:translate %ldb)
  (:args (x :scs (unsigned-reg)))
  (:arg-types (:constant (integer 1 29)) (:constant (integer 0 31)) unsigned-num)
  (:info size posn)
  (:results (res :scs (any-reg)))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:generator 3
    (inst rlwinm res x
          (mod (- (+ 32 n-fixnum-tag-bits) posn) 32)
          (- 32 size n-fixnum-tag-bits)
          (- 31 n-fixnum-tag-bits))))

;;;; Modular functions:
(define-modular-fun lognot-mod32 (x) lognot :untagged nil 32)
(define-vop (lognot-mod32/unsigned=>unsigned)
  (:translate lognot-mod32)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (inst not res x)))

(define-vop (fast-ash-left-mod32-c/unsigned=>unsigned
             fast-ash-c/unsigned=>unsigned)
  (:translate ash-left-mod32))

(define-vop (fast-ash-left-mod32/unsigned=>unsigned
             fast-ash-left/unsigned=>unsigned))
(deftransform ash-left-mod32 ((integer count)
                              ((unsigned-byte 32) (unsigned-byte 5)))
  (when (sb-c:constant-lvar-p count)
    (sb-c::give-up-ir1-transform))
  '(%primitive fast-ash-left-mod32/unsigned=>unsigned integer count))

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
  (define-modular-backend * t)
  (define-modular-backend logeqv)
  (define-modular-backend lognand)
  (define-modular-backend lognor)
  (define-modular-backend logandc2)
  (define-modular-backend logorc1)
  (define-modular-backend logorc2))

;;;; Binary conditional VOPs:

(define-vop (fast-conditional)
  (:conditional)
  (:info target not-p)
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

(deftransform logtest ((x y) (:or ((signed-word signed-word) *)
                                  ((word word) *)) * :vop t)
  t)

(macrolet ((define-logtest-vops ()
             `(progn
               ,@(loop for suffix in '(/fixnum -c/fixnum
                                       /signed -c/signed
                                       /unsigned -c/unsigned)
                       for sc in '(any-reg any-reg
                                   signed-reg signed-reg
                                   unsigned-reg unsigned-reg)
                       for cost in '(4 3 6 5 6 5)
                       collect
                       `(define-vop (,(symbolicate "FAST-LOGTEST" suffix)
                                     ,(symbolicate "FAST-CONDITIONAL" suffix))
                         (:translate logtest)
                         (:temporary (:scs (,sc) :to (:result 0)) test)
                         ,@(case suffix
                             (-c/unsigned
                              `((:arg-types unsigned-num
                                            (:constant (and (unsigned-byte 16) (not (integer 0 0)))))))
                             (-c/signed
                              `((:arg-types signed-num
                                            (:constant (and (unsigned-byte 16) (not (integer 0 0)))))))
                             (-c/fixnum
                              `((:arg-types tagged-num
                                            (:constant (and (unsigned-byte 14) (not (integer 0 0))))))))
                         (:generator ,cost
                          ;; We could be a lot more sophisticated here and
                          ;; check for possibilities with ANDIS..
                          ,(if (string= "-C" suffix :end2 2)
                               `(inst andi. test x ,(if (eq suffix '-c/fixnum)
                                                        '(fixnumize y)
                                                        'y))
                               `(inst and. test x y))
                          (inst b? (if not-p :eq :ne) target)))))))
  (define-logtest-vops))

(deftransform logbitp ((x y) (:or (((constant-arg (mod #.n-word-bits)) signed-word) *)
                                  (((constant-arg (mod #.n-word-bits)) word) *)) * :vop t)
  t)

(define-vop (fast-logbitp-c/fixnum fast-conditional-c/fixnum)
  (:translate logbitp)
  (:arg-types (:constant (integer 0 29)) tagged-num)
  (:temporary (:scs (any-reg) :to (:result 0)) test)
  (:generator 4
    (if (< y 14)
        (inst andi. test x (ash 1 (+ y n-fixnum-tag-bits)))
        (inst andis. test x (ash 1 (- y 14))))
    (inst b? (if not-p :eq :ne) target)))

(define-vop (fast-logbitp-c/signed fast-conditional-c/signed)
  (:translate logbitp)
  (:arg-types (:constant (integer 0 31)) signed-num)
  (:temporary (:scs (signed-reg) :to (:result 0)) test)
  (:generator 4
    (if (< y 16)
        (inst andi. test x (ash 1 y))
        (inst andis. test x (ash 1 (- y 16))))
    (inst b? (if not-p :eq :ne) target)))

(define-vop (fast-logbitp-c/unsigned fast-conditional-c/unsigned)
  (:translate logbitp)
  (:arg-types (:constant (integer 0 31)) unsigned-num)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) test)
  (:generator 4
    (if (< y 16)
        (inst andi. test x (ash 1 y))
        (inst andis. test x (ash 1 (- y 16))))
    (inst b? (if not-p :eq :ne) target)))

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
  (:args (x :scs (any-reg zero))
         (y :scs (any-reg zero)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql)
  (:generator 4
    (inst cmpw x y)
    (inst b? (if not-p :ne :eq) target)))
;;;
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg)))
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg zero)))
  (:arg-types tagged-num (:constant (signed-byte 14)))
  (:info target not-p y)
  (:translate eql)
  (:generator 2
    (inst cmpwi x (fixnumize y))
    (inst b? (if not-p :ne :eq) target)))
;;;
(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant (signed-byte 11)))
  (:variant-cost 6))


;;;; 32-bit logical operations

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
                 (inst rlwinm amount amount 0 27 31)
                 (inst ,operation r num amount)))))
  (define shift-towards-start slw)
  (define shift-towards-end   srw))

;;;; Bignum stuff.

(define-vop (bignum-length get-header-data)
  (:translate sb-bignum:%bignum-length)
  (:policy :fast-safe))

(define-vop (bignum-set-length set-header-data)
  (:translate sb-bignum:%bignum-set-length)
  (:policy :fast-safe))

#-bignum-assertions
(define-vop (bignum-ref word-index-ref)
  (:variant bignum-digits-offset other-pointer-lowtag)
  (:translate sb-bignum:%bignum-ref)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (bignum-set word-index-set)
  (:variant bignum-digits-offset other-pointer-lowtag)
  (:translate #+bignum-assertions sb-bignum:%%bignum-set
              #-bignum-assertions sb-bignum:%bignum-set)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate zero))
         (value :scs (unsigned-reg)))
  (:arg-types t positive-fixnum unsigned-num))

(define-vop (digit-0-or-plus)
  (:translate sb-bignum:%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:generator 3
      (inst cmpwi digit 0)
      (move result null-tn)
      (inst blt done)
      (load-symbol result t)
      DONE))

(define-vop (add-w/carry)
  (:translate sb-bignum:%add-with-carry)
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
  (:translate sb-bignum:%subtract-with-borrow)
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
  (:translate sb-bignum:%multiply-and-add)
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
  (:translate sb-bignum:%multiply-and-add)
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
  (:translate sb-bignum:%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to (:eval 1))
         (y :scs (unsigned-reg) :to (:eval 1)))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from (:eval 1))
            (lo :scs (unsigned-reg) :from (:eval 0)))
  (:result-types unsigned-num unsigned-num)
  (:generator 40
    (inst mullw lo x y)
    (inst mulhwu hi x y)))

(define-vop (mulhi)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 20
    (inst mulhwu hi x y)))

(define-vop (mulhi/fx)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (any-reg))
         (y :scs (unsigned-reg)))
  (:arg-types positive-fixnum unsigned-num)
  (:temporary (:sc non-descriptor-reg :from :eval :to :result) temp)
  (:temporary (:sc non-descriptor-reg :from :eval :to :result) mask)
  (:results (hi :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 15
    (inst mulhwu temp x y)
    (inst lr mask fixnum-tag-mask)
    (inst andc hi temp mask)))

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
       (inst slwi res digit n-fixnum-tag-bits))
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
    (inst sraw result digit count)))

(define-vop (digit-lshr digit-ashr)
  (:translate sb-bignum:%digit-logical-shift-right)
  (:generator 1
    (inst srw result digit count)))

(define-vop (digit-ashl digit-ashr)
  (:translate sb-bignum:%ashl)
  (:generator 1
    (inst slw result digit count)))

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
    (inst mullw temp dividend c)
    (inst mulhwu remainder temp divisor)))

(in-package "SB-C")

(deftransform * ((x y)
                 ((unsigned-byte 32) (constant-arg (unsigned-byte 32)))
                 (unsigned-byte 32))
  "recode as shifts and adds"
  (let ((y (lvar-value y)))
    (multiple-value-bind (result adds shifts)
        (ub32-strength-reduce-constant-multiply 'x y)
      (cond
       ((typep y '(signed-byte 16))
        ;; a mulli instruction has a latency of 5.
        (when (> (+ adds shifts) 4)
          (give-up-ir1-transform)))
       (t
        ;; a mullw instruction also has a latency of 5, plus two
        ;; instructions (in general) to load the immediate into a
        ;; register.
        (when (> (+ adds shifts) 6)
          (give-up-ir1-transform))))
      (or result 0))))
