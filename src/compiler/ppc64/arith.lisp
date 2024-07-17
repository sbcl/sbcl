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
  (:note "inline word arithmetic"))

(define-vop (fast-signed-binop fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg))
         (y :target r :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline signed-word arithmetic"))

(define-vop (fast-fixnum-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg)))
  (:info y)
  (:arg-types tagged-num (:constant fixnum))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-fixnum-logop-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg)))
  (:info y)
  (:arg-types tagged-num (:constant fixnum))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum logical op"))

(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg)))
  (:info y)
  (:arg-types unsigned-num (:constant word))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline word arithmetic"))

(define-vop (fast-unsigned-logop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg)))
  (:info y)
  (:arg-types unsigned-num (:constant word))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline word logical op"))

(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg)))
  (:info y)
  (:arg-types signed-num (:constant signed-word))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline signed-word arithmetic"))

(define-vop (fast-signed-logop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg)))
  (:info y)
  (:arg-types signed-num (:constant signed-word))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline signed-word logical op"))

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
             `((inst clrrdi r temp n-fixnum-tag-bits)))))
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

(defun generate-fast-+-c (r x y)
  (cond ((typep y '(signed-byte 16))
         (inst addi r x y))
        ;; See if this can be done as an addis + addi.
        ;; If bit 15 is on, 1 is added to the high part to undo the
        ;; effect of sign-extension from the low. The post-adjustment
        ;; high part needs to fit in (signed-byte 16)
        ((typep (+ (ash y -16) (ldb (byte 1 15) y)) '(signed-byte 16))
         (inst addis r x (+ (ash y -16) (ldb (byte 1 15) y)))
         (let ((low (ldb (byte 16 0) y)))
           (unless (zerop low)
             (inst addi r r low))))
        (t
         (inst lr temp-reg-tn y)
         (inst add r x temp-reg-tn))))

(macrolet ((define-const-binop (translate untagged-penalty)
             `(progn
                (define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum) fast-fixnum-binop-c)
                  (:translate ,translate)
                  (:generator 1
                    (cond ((and (eq ',translate '-)
                                (= y most-negative-fixnum))
                           (inst lr temp-reg-tn (fixnumize y))
                           (inst sub r x temp-reg-tn))
                          (t
                           (generate-fast-+-c r x (fixnumize (,translate y)))))))
                (define-vop (,(symbolicate 'fast- translate '-c/signed=>signed) fast-signed-binop-c)
                  (:translate ,translate)
                  (:generator ,untagged-penalty
                    (cond ((and (eq ',translate '-)
                                (= y (- (expt 2 (1- n-word-bits)))))
                           (inst lr temp-reg-tn y)
                           (inst sub r x temp-reg-tn))
                          (t
                           (generate-fast-+-c r x (,translate y))))))
                (define-vop (,(symbolicate 'fast- translate '-c/unsigned=>unsigned) fast-unsigned-binop-c)
                  (:translate ,translate)
                  (:generator ,untagged-penalty
                    (generate-fast-+-c r x (,translate y)))))))
  (define-const-binop + 4)
  (define-const-binop - 4))

(macrolet ((define-const-logop (translate untagged-penalty op shifted-op general-op)
  `(flet ((emit (r x y)
            (cond ((typep y '(unsigned-byte 16))
                   (inst ,op r x y))
                  ((and (typep (ash y -16) '(unsigned-byte 16)) ; effectively (unsigned-byte 32)
                        ;; logical AND can't be split into two instructions
                        ,@(if (eq translate 'logand) '((zerop (ldb (byte 16 0) y)))))
                   (inst ,shifted-op r x (ash y -16))
                   (when (ldb-test (byte 16 0) y)
                     (inst ,op r r (ldb (byte 16 0) y)))) ; not sign-extended
                  (t ; everything else: just load the constant from memory
                   (inst lr temp-reg-tn y)
                   (inst ,general-op r x temp-reg-tn)))))
     (define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum) fast-fixnum-logop-c)
       (:translate ,translate)
       (:generator 1 (emit r x (fixnumize y))))
     (define-vop (,(symbolicate 'fast- translate '-c/signed=>signed) fast-signed-logop-c)
       (:translate ,translate)
       (:generator ,untagged-penalty (emit r x y)))
     (define-vop (,(symbolicate 'fast- translate '-c/unsigned=>unsigned) fast-unsigned-logop-c)
       (:translate ,translate)
       (:generator ,untagged-penalty (emit r x y))))))

  (define-const-logop logand 2 andi. andis. and)
  (define-const-logop logior 2 ori oris or)
  (define-const-logop logxor 2 xori xoris xor))

(define-vop (fast-logandc2/unsigned-signed=>unsigned fast-logandc2/unsigned=>unsigned)
  (:args (x :scs (unsigned-reg))
         (y :scs (signed-reg)))
  (:arg-types unsigned-num signed-num))

(define-vop (fast-logand/signed-unsigned=>unsigned fast-logand/unsigned=>unsigned)
  (:args (x :scs (signed-reg) :target r)
         (y :scs (unsigned-reg) :target r))
  (:arg-types signed-num unsigned-num))

(define-vop (fast-logand-c/signed-unsigned=>unsigned fast-logand/unsigned=>unsigned)
  (:args (x :scs (signed-reg) :target r))
  (:arg-types signed-num (:constant (eql #.most-positive-word)))
  (:info y)
  (:ignore y)
  (:generator 1
    (move r x)))

(define-vop (fast-*/fixnum=>fixnum fast-fixnum-binop)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:translate *)
  (:generator 2
    (inst sradi temp y n-fixnum-tag-bits)
    (inst mulld r x temp)))

(define-vop (fast-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:generator 4
    (inst mulld r x y)))

(define-vop (fast-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:generator 4
    (inst mulld r x y)))

(macrolet ((generate ()
             '(cond ((typep y '(signed-byte 16))
                     (inst mulli r x y))
                    (t
                     (inst lr temp-reg-tn y)
                     (inst mulld r x temp-reg-tn)))))
  (define-vop (fast-*-c/fixnum=>fixnum fast-fixnum-binop-c)
    (:translate *)
    (:generator 1 (generate)))
  (define-vop (fast-*-c/signed=>signed fast-signed-binop-c)
    (:translate *)
    (:generator 3 (generate)))
  (define-vop (fast-*-c/unsigned=>unsigned fast-unsigned-binop-c)
    (:translate *)
    (:generator 3 (generate))))

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
                      (inst sld result number amount))
                     (immediate
                      (let ((amount (tn-value amount)))
                        (aver (>= amount 0))
                        (inst sldi result number amount))))))))
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
      (inst cmpdi amount 0)
      (inst neg ndesc amount)
      (inst bge positive)
      (inst cmpldi ndesc 63)
      (inst srd result number ndesc)
      (inst ble done)
      (inst li result 0)
      (inst b done)

      POSITIVE
      ;; The result-type assures us that this shift will not overflow.
      (inst sld result number amount)

      DONE))

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
      ((and (minusp amount) (< amount -63)) (inst li result 0))
      ((minusp amount) (inst srdi result number (- amount)))
      ;; possible because this is used in the modular version too
      ((> amount 63) (inst li result 0))
      (t (inst sldi result number amount)))))

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
         (inst cmpdi amount 0)
         (inst neg ndesc amount)
         (inst bge positive)
         (inst cmpldi ndesc 63)
         (inst srad result number ndesc)
         (inst ble done)
         ;; smear the sign bit into all bits
         (inst sradi result number 63)
         (inst b done)

         (emit-label positive)
         ;; The result-type assures us that this shift will not overflow.
         (inst sld result number amount)

         (emit-label done)))

      (immediate
       (let ((amount (tn-value amount)))
         (aver (typep amount '(integer * 63)))
         (if (minusp amount)
             (inst sradi result number (min 63 (- amount)))
             (inst sldi result number amount)))))))

(define-vop (fast-%ash/right/unsigned)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg)) (amount :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst srd result number amount)))

(define-vop (fast-%ash/right/signed)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg)) (amount :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (inst srad result number amount)))

(define-vop (fast-%ash/right/fixnum)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (any-reg)) (amount :scs (unsigned-reg)))
  (:arg-types tagged-num unsigned-num)
  (:results (result :scs (any-reg)))
  (:result-types tagged-num)
  (:temporary (:sc signed-reg) temp)
  (:generator 2
    (inst srad temp number amount)
    (inst clrrdi result temp n-fixnum-tag-bits)))

(define-vop (signed-byte-64-len)
  (:translate integer-length)
  (:note "inline (signed-byte 64) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg)))
  (:arg-types signed-num)
  (:results (res :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num)
  (:generator 6
    ; (integer-length arg) = (- 64 (cntlz (if (>= arg 0) arg (lognot arg))))
    (inst cntlzd. res arg)
    (inst bne nonneg)
    (inst not res arg)
    (inst cntlzd res res)
    NONNEG
    (inst subfic res res 64)))

(define-vop (unsigned-byte-64-len)
  (:translate integer-length)
  (:note "inline (unsigned-byte 64) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
    (inst cntlzd res arg)
    (inst subfic res res 64)))

(define-vop (unsigned-byte-64-count)
  (:translate logcount)
  (:note "inline (unsigned-byte 64) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 2
    (inst popcntd res arg)))

;;;; %LDB
#+nil
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

#+nil
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

#+nil
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

#+nil
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

;;; FIXME: This should all be correctly ported to 64 bit

(define-modular-fun lognot-mod64 (x) lognot :untagged nil 64)
(define-vop (lognot-mod64/unsigned=>unsigned)
  (:translate lognot-mod64)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (inst not res x)))

(define-vop (fast-ash-left-mod64-c/unsigned=>unsigned
             fast-ash-c/unsigned=>unsigned)
  (:translate ash-left-mod64))

(define-vop (fast-ash-left-mod64/unsigned=>unsigned
             fast-ash-left/unsigned=>unsigned))
(deftransform ash-left-mod64 ((integer count)
                              ((unsigned-byte 64) (unsigned-byte 6)))
  (when (sb-c:constant-lvar-p count)
    (sb-c::give-up-ir1-transform))
  '(%primitive fast-ash-left-mod64/unsigned=>unsigned integer count))

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
     (:arg-types untagged-num (:constant (and (signed-byte 30) (not (integer 0 0)))))
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
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (signed-byte #.(- 16 n-fixnum-tag-bits))))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 64) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num (:constant (signed-byte 16)))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 64) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num (:constant (unsigned-byte 16)))
  (:info target not-p y))

#+nil
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

#+nil
(deftransform logbitp ((x y) (:or (((constant-arg (mod #.n-word-bits)) signed-word) *)
                                  (((constant-arg (mod #.n-word-bits)) word) *)) * :vop t)
  t)

#+nil (define-vop (fast-logbitp-c/fixnum fast-conditional-c/fixnum)
  (:translate logbitp)
  (:arg-types (:constant (integer 0 29)) tagged-num)
  (:temporary (:scs (any-reg) :to (:result 0)) test)
  (:generator 4
    (if (< y 14)
        (inst andi. test x (ash 1 (+ y n-fixnum-tag-bits)))
        (inst andis. test x (ash 1 (- y 14))))
    (inst b? (if not-p :eq :ne) target)))

#+nil (define-vop (fast-logbitp-c/signed fast-conditional-c/signed)
  (:translate logbitp)
  (:arg-types (:constant (integer 0 31)) signed-num)
  (:temporary (:scs (signed-reg) :to (:result 0)) test)
  (:generator 4
    (if (< y 16)
        (inst andi. test x (ash 1 y))
        (inst andis. test x (ash 1 (- y 16))))
    (inst b? (if not-p :eq :ne) target)))

#+nil (define-vop (fast-logbitp-c/unsigned fast-conditional-c/unsigned)
  (:translate logbitp)
  (:arg-types (:constant (integer 0 31)) unsigned-num)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) test)
  (:generator 4
    (if (< y 16)
        (inst andi. test x (ash 1 y))
        (inst andis. test x (ash 1 (- y 16))))
    (inst b? (if not-p :eq :ne) target)))
(define-vop (mask-signed-field-word/c)
  (:translate sb-c::mask-signed-field)
  (:policy :fast-safe)
  (:args (x :scs (signed-reg unsigned-reg) :target r))
  (:arg-types (:constant (integer 0 64)) untagged-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:info width)
  (:generator 3
    (aver (/= width 0))
    (cond ((= width 64)
           (move r x))
          (t
           (inst sldi r x (- n-word-bits width))
           (inst sradi r r (- n-word-bits width))))))

(define-vop (mask-signed-field-bignum/c)
  (:translate sb-c::mask-signed-field)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target r))
  (:arg-types (:constant (integer 0 64)) bignum)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:info width)
  (:generator 4
    (aver (/= width 0))
    (loadw r x bignum-digits-offset other-pointer-lowtag)
    (inst sldi r r (- n-word-bits width))
    (inst sradi r r (- n-word-bits width))))

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
    (inst andi. temp-reg-tn r fixnum-tag-mask)
    (inst beq DONE)
    (loadw temp-reg-tn r bignum-digits-offset other-pointer-lowtag)
    (inst sldi r temp-reg-tn (- n-word-bits n-fixnum-bits))
    DONE))

(define-vop (fast-if-</fixnum fast-conditional/fixnum)
  (:translate <)
  (:generator 4
    (inst cmpd x y)
    (inst b? (if not-p :ge :lt) target)))

(define-vop (fast-if-<-c/fixnum fast-conditional-c/fixnum)
  (:translate <)
  (:generator 3
    (inst cmpdi x (fixnumize y))
    (inst b? (if not-p :ge :lt) target)))

(define-vop (fast-if-</signed fast-conditional/signed)
  (:translate <)
  (:generator 6
    (inst cmpd x y)
    (inst b? (if not-p :ge :lt) target)))

(define-vop (fast-if-<-c/signed fast-conditional-c/signed)
  (:translate <)
  (:generator 5
    (inst cmpdi x y)
    (inst b? (if not-p :ge :lt) target)))

(define-vop (fast-if-</unsigned fast-conditional/unsigned)
  (:translate <)
  (:generator 6
    (inst cmpld x y)
    (inst b? (if not-p :ge :lt) target)))

(define-vop (fast-if-<-c/unsigned fast-conditional-c/unsigned)
  (:translate <)
  (:generator 5
    (inst cmpldi x y)
    (inst b? (if not-p :ge :lt) target)))

(define-vop (fast-if->/fixnum fast-conditional/fixnum)
  (:translate >)
  (:generator 4
    (inst cmpd x y)
    (inst b? (if not-p :le :gt) target)))

(define-vop (fast-if->-c/fixnum fast-conditional-c/fixnum)
  (:translate >)
  (:generator 3
    (inst cmpdi x (fixnumize y))
    (inst b? (if not-p :le :gt) target)))

(define-vop (fast-if->/signed fast-conditional/signed)
  (:translate >)
  (:generator 6
    (inst cmpd x y)
    (inst b? (if not-p :le :gt) target)))

(define-vop (fast-if->-c/signed fast-conditional-c/signed)
  (:translate >)
  (:generator 5
    (inst cmpdi x y)
    (inst b? (if not-p :le :gt) target)))

(define-vop (fast-if->/unsigned fast-conditional/unsigned)
  (:translate >)
  (:generator 6
    (inst cmpld x y)
    (inst b? (if not-p :le :gt) target)))

(define-vop (fast-if->-c/unsigned fast-conditional-c/unsigned)
  (:translate >)
  (:generator 5
    (inst cmpldi x y)
    (inst b? (if not-p :le :gt) target)))

(define-vop (fast-if-eql/signed fast-conditional/signed)
  (:translate eql)
  (:generator 6
    (inst cmpd x y)
    (inst b? (if not-p :ne :eq) target)))

(define-vop (fast-if-eql-c/signed fast-conditional-c/signed)
  (:translate eql)
  (:generator 5
    (inst cmpdi x y)
    (inst b? (if not-p :ne :eq) target)))

(define-vop (fast-if-eql/unsigned fast-conditional/unsigned)
  (:translate eql)
  (:generator 6
    (inst cmpld x y)
    (inst b? (if not-p :ne :eq) target)))

(define-vop (fast-if-eql-c/unsigned fast-conditional-c/unsigned)
  (:translate eql)
  (:generator 5
    (inst cmpldi x y)
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
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql)
  (:generator 4
    (inst cmpd x y)
    (inst b? (if not-p :ne :eq) target)))
;;;
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg)))
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (signed-byte #.(- 16 n-fixnum-tag-bits))))
  (:info target not-p y)
  (:translate eql)
  (:generator 2
    (inst cmpdi x (fixnumize y))
    (inst b? (if not-p :ne :eq) target)))
;;;
#+nil (define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:arg-types * (:constant (signed-byte 11))) ; wtf is 11?
  (:variant-cost 6))


;;;; 64-bit logical operations

(macrolet ((define (translate operation)
             `(define-vop ()
                (:translate ,translate)
                (:note ,(string translate))
                (:policy :fast-safe)
                (:args (num :scs (unsigned-reg))
                       (amount :scs (signed-reg)))
                (:arg-types unsigned-num tagged-num)
                (:temporary (:sc unsigned-reg) temp)
                (:results (r :scs (unsigned-reg)))
                (:result-types unsigned-num)
                (:generator 1
                 (inst andi. temp amount #b111111)
                 (inst ,operation r num temp)))))
  (define shift-towards-start #+big-endian sld #+little-endian srd)
  (define shift-towards-end   #+big-endian srd #+little-endian sld))

;;;; Bignum stuff.

(define-vop (bignum-length get-header-data)
  (:translate sb-bignum:%bignum-length)
  (:policy :fast-safe))

(define-vop (bignum-set-length set-header-data)
  (:translate sb-bignum:%bignum-set-length)
  (:policy :fast-safe))

(define-vop (bignum-ref word-index-ref)
  (:variant bignum-digits-offset other-pointer-lowtag)
  (:translate sb-bignum:%bignum-ref)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (bignum-set word-index-set)
  (:variant bignum-digits-offset other-pointer-lowtag)
  (:translate sb-bignum:%bignum-set)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (value :scs (unsigned-reg)))
  (:arg-types t positive-fixnum unsigned-num))

(define-vop (digit-0-or-plus)
  (:translate sb-bignum:%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:generator 3
      (inst cmpdi digit 0)
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
    (inst addic temp c -1) ; set CA flag if 'c' is nonzero
    (inst adde result a b) ; add a + b + CA and carry out
    (inst li temp 0)
    (inst addze carry temp)))

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
    (inst li temp 0)
    (inst addze borrow temp)))

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
    (inst mulhdu hi-temp x y)
    (inst mulld lo-temp x y)
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
    (inst mulhdu hi-temp x y)
    (inst mulld lo-temp x y)
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
    (inst mulld lo x y)
    (inst mulhdu hi x y)))

(define-vop (mulhi)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 20
    (inst mulhdu hi x y)))

(define-vop (mulhi/fx)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (any-reg))
         (y :scs (unsigned-reg)))
  (:arg-types positive-fixnum unsigned-num)
  (:temporary (:sc non-descriptor-reg :from :eval :to :result) temp)
  (:results (hi :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 15
    (inst mulhdu temp x y)
    (inst clrrdi hi temp n-fixnum-tag-bits)))

;;; Algorithm from page 74 of of Power ISA version 2.07
;;; under "Programming Note" for the divweu instruction
;;; with modifications as described at the divdeu instruction.
(define-vop (bignum-floor)
  (:translate sb-bignum:%bigfloor)
  (:policy :fast-safe)
  ;; I tried to tighten up the TN lifetimes for better packing
  ;; (in fewer physical registers), but I got it wrong and everything broke.
  ;; We have enough non-descriptor registers that this isn't a problem.
  (:args (Dh :scs (unsigned-reg)) ; dividend high
         (Dl :scs (unsigned-reg)) ; dividend low
         (Dv :scs (unsigned-reg) :to :save)) ; divisor
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (Q :scs (unsigned-reg))  ; quotient
            (R :scs (unsigned-reg))) ; remainder
  (:result-types unsigned-num unsigned-num)
  (:temporary (:scs (non-descriptor-reg)) q1)
  (:temporary (:scs (non-descriptor-reg)) q2)
  (:temporary (:scs (non-descriptor-reg)) -r1)
  (:temporary (:scs (non-descriptor-reg)) r2)
  (:generator 15
    ;; I don't know how we know when utilizing this vop
    ;; that overflow won't occur. But the code for x86[-64]
    ;; does not check for overflow either.
    (inst divdeu q1 Dh Dv)   ; q1
    (inst divdu  q2 Dl Dv)   ; q2
    (inst mulld -r1 q1 Dv)   ; -r1 = q1 * Dv
    (inst mulld r2 q2 Dv)    ; temp = q2 * Dv
    (inst subf r2 r2 Dl)     ; r2 = Dl - (q2 * Dv)
    ;; move to result TNs
    (inst add Q q1 q2)       ; Q = q1 + q2
    (inst subf R -r1 r2)     ; R = r1 + r2
    (inst cmpld R r2)        ; R < r2 ?
    (inst blt ADJ)           ; must adjust Q and R if yes
    (inst cmpld R Dv)        ; R >= Dv ?
    (inst blt DONE)          ; must adjust Q and R if yes
    ADJ
    (inst addi Q Q 1)        ; Q = Q + 1
    (inst subf R Dv R)       ; R = R - Dv
    DONE))

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
       (inst sldi res digit n-fixnum-tag-bits))
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
    (inst srad result digit count)))

(define-vop (digit-lshr digit-ashr)
  (:translate sb-bignum:%digit-logical-shift-right)
  (:generator 1
    (inst srd result digit count)))

(define-vop (digit-ashl digit-ashr)
  (:translate sb-bignum:%ashl)
  (:generator 1
    (inst sld result digit count)))

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
    (inst mulld temp dividend c)
    (inst rldicl temp temp 0 32) ; drop the high 32 bits, keep the low 32 bits
    (inst mulld temp temp divisor)
    (inst srdi remainder temp 32))) ; take the high 32 bits
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
    (inst mulld temp dividend c) ; want only the low 64 bits
    (inst mulhdu remainder temp divisor))) ; want only the high 64 bits

(in-package "SB-C")

#+nil
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
