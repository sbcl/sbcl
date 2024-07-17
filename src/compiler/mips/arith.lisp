;;;; the VM definition arithmetic VOPs for MIPS

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
    (inst subu res zero-tn x)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (inst subu res zero-tn x)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:temporary (:scs (any-reg) :to (:result 0))
              temp)
  (:translate lognot)
  (:generator 1
    (inst li temp (fixnumize -1))
    (inst xor res x temp)))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 2
    (inst nor res x zero-tn)))

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

(define-vop (fast-fixnum-c-binop fast-fixnum-binop)
  (:args (x :target r :scs (any-reg)))
  (:info y)
  (:arg-types tagged-num (:constant integer)))

(define-vop (fast-signed-c-binop fast-signed-binop)
  (:args (x :target r :scs (signed-reg)))
  (:info y)
  (:arg-types tagged-num (:constant integer)))

(define-vop (fast-unsigned-c-binop fast-unsigned-binop)
  (:args (x :target r :scs (unsigned-reg)))
  (:info y)
  (:arg-types tagged-num (:constant integer)))

(defmacro define-binop (translate cost untagged-cost op
                                  tagged-type untagged-type)
  `(progn
     (define-vop (,(symbolicate "FAST-" translate "/FIXNUM=>FIXNUM")
                  fast-fixnum-binop)
       (:args (x :target r :scs (any-reg))
              (y :target r :scs (any-reg)))
       (:translate ,translate)
       (:generator ,(1+ cost)
         (inst ,op r x y)))
     (define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
                  fast-signed-binop)
       (:args (x :target r :scs (signed-reg))
              (y :target r :scs (signed-reg)))
       (:translate ,translate)
       (:generator ,(1+ untagged-cost)
         (inst ,op r x y)))
     (define-vop (,(symbolicate "FAST-" translate "/UNSIGNED=>UNSIGNED")
                  fast-unsigned-binop)
       (:args (x :target r :scs (unsigned-reg))
              (y :target r :scs (unsigned-reg)))
       (:translate ,translate)
       (:generator ,(1+ untagged-cost)
         (inst ,op r x y)))
     ,@(when tagged-type
         `((define-vop (,(symbolicate "FAST-" translate "-C/FIXNUM=>FIXNUM")
                        fast-fixnum-c-binop)
                       (:arg-types tagged-num (:constant ,tagged-type))
             (:translate ,translate)
             (:generator ,cost
                         (inst ,op r x (fixnumize y))))))
     ,@(when untagged-type
         `((define-vop (,(symbolicate "FAST-" translate "-C/SIGNED=>SIGNED")
                        fast-signed-c-binop)
                       (:arg-types signed-num (:constant ,untagged-type))
             (:translate ,translate)
             (:generator ,untagged-cost
                         (inst ,op r x y)))
           (define-vop (,(symbolicate "FAST-" translate
                                      "-C/UNSIGNED=>UNSIGNED")
                        fast-unsigned-c-binop)
                       (:arg-types unsigned-num (:constant ,untagged-type))
             (:translate ,translate)
             (:generator ,untagged-cost
                         (inst ,op r x y)))))))

(define-binop + 1 5 addu (signed-byte 14) (signed-byte 16))
(define-binop - 1 5 subu
  (integer #.(- 1 (ash 1 13)) #.(ash 1 13))
  (integer #.(- 1 (ash 1 15)) #.(ash 1 15)))
(define-binop logior 1 3 or (unsigned-byte 14) (unsigned-byte 16))
(define-binop logand 1 3 and (unsigned-byte 14) (unsigned-byte 16))
(define-binop logxor 1 3 xor (unsigned-byte 14) (unsigned-byte 16))

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

;;; No -C/ VOPs for LOGNOR because the NOR instruction doesn't take
;;; immediate args.  -- CSR, 2003-09-11
(define-vop (fast-lognor/fixnum=>fixnum fast-fixnum-binop)
  (:translate lognor)
  (:args (x :target r :scs (any-reg))
         (y :target r :scs (any-reg)))
  (:temporary (:sc non-descriptor-reg) temp)
  (:generator 4
    (inst nor temp x y)
    (inst addu r temp (- fixnum-tag-mask))))

(define-vop (fast-lognor/signed=>signed fast-signed-binop)
  (:translate lognor)
  (:args (x :target r :scs (signed-reg))
         (y :target r :scs (signed-reg)))
  (:generator 4
    (inst nor r x y)))

(define-vop (fast-lognor/unsigned=>unsigned fast-unsigned-binop)
  (:translate lognor)
  (:args (x :target r :scs (unsigned-reg))
         (y :target r :scs (unsigned-reg)))
  (:generator 4
    (inst nor r x y)))

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
    (inst bgez amount positive)
    (inst subu ndesc zero-tn amount)
    (inst slt temp ndesc 32) ; FIXME: should be sltu here ?
    (inst bne temp done)
    (inst srl result number ndesc)
    (inst b done)
    (zeroize result)

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
    (inst bgez amount positive)
    (inst subu ndesc zero-tn amount)
    (inst sltu temp ndesc 31)
    (inst bne temp done)
    (inst sra result number ndesc)
    (inst b done)
    (inst sra result number 31)

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
      ((< count 0) (inst srl result number (min (- count) 31)))
      ((> count 0) (inst sll result number (min count 31)))
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
      ((< count 0) (inst sra result number (min (- count) 31)))
      ((> count 0) (inst sll result number (min count 31)))
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
      (move shift arg)
      (inst bgez shift test)
      (zeroize res)
      (inst b test)
      (inst nor shift shift)

      LOOP
      (inst addu res (fixnumize 1))

      TEST
      (inst bne shift loop)
      (inst srl shift 1)))

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
    (inst srl temp arg 1)
    (inst and num arg mask)
    (inst and temp mask)
    (inst addu num temp)
    (inst li mask #x33333333)
    (inst srl temp num 2)
    (inst and num mask)
    (inst and temp mask)
    (inst addu num temp)
    (inst li mask #x0f0f0f0f)
    (inst srl temp num 4)
    (inst and num mask)
    (inst and temp mask)
    (inst addu num temp)
    (inst li mask #x00ff00ff)
    (inst srl temp num 8)
    (inst and num mask)
    (inst and temp mask)
    (inst addu num temp)
    (inst li mask #x0000ffff)
    (inst srl temp num 16)
    (inst and num mask)
    (inst and temp mask)
    (inst addu res num temp)))


;;; Multiply and Divide.

(define-vop (fast-*/fixnum=>fixnum fast-fixnum-binop)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:translate *)
  (:generator 4
    (inst sra temp y n-fixnum-tag-bits)
    (inst mult x temp)
    (inst mflo r)))

(define-vop (fast-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:generator 3
    (inst mult x y)
    (inst mflo r)))

(define-vop (fast-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:generator 3
    (inst multu x y)
    (inst mflo r)))



(define-vop (fast-truncate/fixnum fast-fixnum-binop)
  (:translate truncate)
  (:results (q :scs (any-reg))
            (r :scs (any-reg)))
  (:result-types tagged-num tagged-num)
  (:temporary (:scs (non-descriptor-reg) :to :eval) temp)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 11
    (let ((zero (generate-error-code vop 'division-by-zero-error x)))
      (inst beq y zero))
    (inst nop)
    (inst div x y)
    (inst mflo temp)
    (inst sll q temp n-fixnum-tag-bits)
    (inst mfhi r)))

(define-vop (fast-truncate/unsigned fast-unsigned-binop)
  (:translate truncate)
  (:results (q :scs (unsigned-reg))
            (r :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 12
    (let ((zero (generate-error-code vop 'division-by-zero-error x)))
      (inst beq y zero))
    (inst nop)
    (inst divu x y)
    (inst mflo q)
    (inst mfhi r)))

(define-vop (fast-truncate/signed fast-signed-binop)
  (:translate truncate)
  (:results (q :scs (signed-reg))
            (r :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 12
    (let ((zero (generate-error-code vop 'division-by-zero-error x)))
      (inst beq y zero))
    (inst nop)
    (inst div x y)
    (inst mflo q)
    (inst mfhi r)))



;;;; Binary conditional VOPs:

(define-vop (fast-conditional)
  (:conditional)
  (:info target not-p)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:policy :fast-safe))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (signed-byte-with-a-bite-out 14 4)))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 32) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num (:constant (signed-byte-with-a-bite-out 16 1)))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num (:constant (and (signed-byte-with-a-bite-out 16 1)
                                           unsigned-byte)))
  (:info target not-p y))


(defmacro define-conditional-vop (translate &rest generator)
  `(progn
     ,@(mapcar #'(lambda (suffix cost signed)
                   (unless (and (member suffix '(/fixnum -c/fixnum))
                                (eq translate 'eql))
                     `(define-vop (,(intern (format nil "~:@(FAST-IF-~A~A~)"
                                                    translate suffix))
                                   ,(intern
                                     (format nil "~:@(FAST-CONDITIONAL~A~)"
                                             suffix)))
                        (:translate ,translate)
                        (:generator ,cost
                          (let* ((signed ,signed)
                                 (-c/fixnum ,(eq suffix '-c/fixnum))
                                 (y (if -c/fixnum (fixnumize y) y)))
                            (declare (ignorable signed -c/fixnum y))
                            ,@generator)))))
               '(/fixnum -c/fixnum /signed -c/signed /unsigned -c/unsigned)
               '(3 2 5 4 5 4)
               '(t t t t nil nil))))

(define-conditional-vop <
  (cond ((and signed (eql y 0))
         (if not-p
             (inst bgez x target)
             (inst bltz x target)))
        (t
         (if signed
             (inst slt temp x y)
             (inst sltu temp x y))
         (if not-p
             (inst beq temp target)
             (inst bne temp target))))
  (inst nop))

(define-conditional-vop >
  (cond ((and signed (eql y 0))
         (if not-p
             (inst blez x target)
             (inst bgtz x target)))
        ((integerp y)
         (let ((y (+ y (if -c/fixnum (fixnumize 1) 1))))
           (if signed
               (inst slt temp x y)
               (inst sltu temp x y))
           (if not-p
               (inst bne temp target)
               (inst beq temp target))))
        (t
         (if signed
             (inst slt temp y x)
             (inst sltu temp y x))
         (if not-p
             (inst beq temp target)
             (inst bne temp target))))
  (inst nop))

;;; EQL/FIXNUM is funny because the first arg can be of any type, not just a
;;; known fixnum.

(define-conditional-vop eql
  (declare (ignore signed))
  (when (integerp y)
    (inst li temp y)
    (setf y temp))
  (if not-p
      (inst bne x y target)
      (inst beq x y target))
  (inst nop))

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
  (:ignore temp)
  (:generator 3
    (if not-p
        (inst bne x y target)
        (inst beq x y target))
    (inst nop)))
;;;
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg)))
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (signed-byte 14)))
  (:info target not-p y)
  (:translate eql)
  (:generator 2
    (let ((y (cond ((eql y 0) zero-tn)
                   (t
                    (inst li temp (fixnumize y))
                    temp))))
      (if not-p
          (inst bne x y target)
          (inst beq x y target))
      (inst nop))))
;;;
(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant (signed-byte 14)))
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
                (:generator 1 (inst ,operation r num amount)))))
  (define shift-towards-start #+big-endian sll #+little-endian srl)
  (define shift-towards-end   #+big-endian srl #+little-endian sll))

;;;; Modular arithmetic
(define-modular-fun +-mod32 (x y) + :untagged nil 32)
(define-vop (fast-+-mod32/unsigned=>unsigned fast-+/unsigned=>unsigned)
  (:translate +-mod32))
(define-vop (fast-+-mod32-c/unsigned=>unsigned fast-+-c/unsigned=>unsigned)
  (:translate +-mod32))
(define-modular-fun --mod32 (x y) - :untagged nil 32)
(define-vop (fast---mod32/unsigned=>unsigned fast--/unsigned=>unsigned)
  (:translate --mod32))
(define-vop (fast---mod32-c/unsigned=>unsigned fast---c/unsigned=>unsigned)
  (:translate --mod32))

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

(define-vop (fast-%ash/right/unsigned)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg)) (amount :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst srl result number amount)))

(define-vop (fast-%ash/right/signed)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg)) (amount :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (inst sra result number amount)))

;;; I could not causes this vop to get selected
(define-vop (fast-%ash/right/fixnum)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (any-reg)) (amount :scs (unsigned-reg)))
  (:arg-types tagged-num unsigned-num)
  (:results (result :scs (any-reg)))
  (:result-types tagged-num)
  ;; An ANY-REG must not leak out (on an interrupt) if it has 1 bits under the fixnum
  ;; tag mask when it was supposed to hold a fixnum. But there's no "AND dst, src, Imm"
  ;; operation to clear the bits, so this can't really be fewer than 3 instructions.
  (:temporary (:sc signed-reg) temp)
  (:generator 2
    (inst sra temp number amount)
    (inst li result (lognot fixnum-tag-mask))
    (inst and result temp result)))

;;; logical operations
(define-modular-fun lognot-mod32 (x) lognot :untagged nil 32)
(define-vop (lognot-mod32/unsigned=>unsigned)
  (:translate lognot-mod32)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (inst nor r x zero-tn)))

(define-modular-fun lognor-mod32 (x y) lognor :untagged nil 32)
(define-vop (fast-lognor-mod32/unsigned=>unsigned
             fast-lognor/unsigned=>unsigned)
  (:translate lognor-mod32))

(define-source-transform logeqv (&rest args)
  (if (oddp (length args))
      `(logxor ,@args)
      `(lognot (logxor ,@args))))
(define-source-transform logandc2 (x y)
  `(logand ,x (lognot ,y)))
(define-source-transform logorc1 (x y)
  `(logior (lognot ,x) ,y))
(define-source-transform logorc2 (x y)
  `(logior ,x (lognot ,y)))
(define-source-transform lognand (x y)
  `(lognot (logand ,x ,y)))
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
    (if not-p
        (inst bltz digit target)
        (inst bgez digit target))
    (inst nop)))

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
      (inst bne c carry-in)
      (inst addu res a b)

      (inst b done)
      (inst sltu carry res b)

      CARRY-IN
      (inst addu res 1)
      (inst nor temp a zero-tn)
      (inst sltu carry b temp)
      (inst xor carry 1)

      DONE
      (move result res)))

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
      (inst bne c no-borrow-in)
      (inst subu res a b)

      (inst subu res 1)
      (inst b done)
      (inst sltu borrow b a)

      NO-BORROW-IN
      (inst sltu borrow a b)
      (inst xor borrow 1)

      DONE
      (move result res)))

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
  (:generator 6
    (inst multu x y)
    (inst mflo temp)
    (inst addu lo temp carry-in)
    (inst sltu temp lo carry-in)
    (inst mfhi hi)
    (inst addu hi temp)))

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
  (:generator 9
    (inst multu x y)
    (inst addu lo prev carry-in)
    (inst sltu temp lo carry-in)
    (inst mfhi hi)
    (inst addu hi temp)
    (inst mflo temp)
    (inst addu lo temp)
    (inst sltu temp lo temp)
    (inst addu hi temp)))

(define-vop (bignum-mult)
  (:translate sb-bignum:%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg))
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 3
    (inst multu x y)
    (inst mflo lo)
    (inst mfhi hi)))

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
             (inst subu temp guess 1)
             (inst and temp denom)
             (inst subu rem temp)))
      (inst sltu quo rem denom)
      (maybe-subtract quo)
      (dotimes (i 32)
        (inst sll rem 1)
        (inst srl temp rem-low 31)
        (inst or rem temp)
        (inst sll rem-low 1)
        (inst sltu temp rem denom)
        (inst sll quo 1)
        (inst or quo temp)
        (maybe-subtract)))
    (inst nor quo zero-tn)))

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
       (inst sll res digit n-fixnum-tag-bits))
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
    (inst multu dividend c) ; keep only the low 32 bits
    (inst mflo temp)
    (inst multu temp divisor)
    (inst mfhi remainder)))
