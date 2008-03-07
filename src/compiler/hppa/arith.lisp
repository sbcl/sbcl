;;;; the VM definition arithmetic VOPs for HPPA

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

(define-vop (fixnum-unop)
  (:args (x :scs (any-reg)))
  (:results (res :scs (any-reg)))
  (:note "inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num)
  (:policy :fast-safe))

(define-vop (signed-unop)
  (:args (x :scs (signed-reg)))
  (:results (res :scs (signed-reg)))
  (:note "inline (signed-byte 32) arithmetic")
  (:arg-types signed-num)
  (:result-types signed-num)
  (:policy :fast-safe))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (inst sub zero-tn x res)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (inst sub zero-tn x res)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:temporary (:scs (any-reg) :type fixnum :to (:result 0))
              temp)
  (:translate lognot)
  (:generator 1
    (inst li (fixnumize -1) temp)
    (inst xor x temp res)))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 2
    (inst uaddcm zero-tn x res)))

;;;; Binary fixnum operations.

;;; Assume that any constant operand is the second arg...

(define-vop (fast-fixnum-binop)
  (:args (x :target r :scs (any-reg))
         (y :target r :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic")
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-unsigned-binop)
  (:args (x :target r :scs (unsigned-reg))
         (y :target r :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-signed-binop)
  (:args (x :target r :scs (signed-reg))
         (y :target r :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:effects)
  (:affected)
  (:policy :fast-safe))

(defmacro define-binop (translate cost untagged-cost op &optional arg-swap)
  `(progn
     (define-vop (,(symbolicate "FAST-" translate "/FIXNUM=>FIXNUM")
                  fast-fixnum-binop)
       (:args (x :target r :scs (any-reg))
              (y :target r :scs (any-reg)))
       (:translate ,translate)
       (:generator ,cost
         ,(if arg-swap
              `(inst ,op y x r)
              `(inst ,op x y r))))
     (define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
                  fast-signed-binop)
       (:args (x :target r :scs (signed-reg))
              (y :target r :scs (signed-reg)))
       (:translate ,translate)
       (:generator ,untagged-cost
         ,(if arg-swap
              `(inst ,op y x r)
              `(inst ,op x y r))))
     (define-vop (,(symbolicate "FAST-" translate "/UNSIGNED=>UNSIGNED")
                  fast-unsigned-binop)
       (:args (x :target r :scs (unsigned-reg))
              (y :target r :scs (unsigned-reg)))
       (:translate ,translate)
       (:generator ,untagged-cost
         ,(if arg-swap
              `(inst ,op y x r)
              `(inst ,op x y r))))))

(define-binop + 2 6 add)
(define-binop - 2 6 sub)
(define-binop logior 1 2 or)
(define-binop logand 1 2 and)
(define-binop logandc1 1 2 andcm t)
(define-binop logandc2 1 2 andcm)
(define-binop logxor 1 2 xor)

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

(defmacro define-c-binop (translate cost untagged-cost tagged-type
                                    untagged-type inst)
  `(progn
     (define-vop (,(symbolicate "FAST-" translate "-C/FIXNUM=>FIXNUM")
                  fast-fixnum-c-binop)
       (:arg-types tagged-num (:constant ,tagged-type))
       (:translate ,translate)
       (:generator ,cost
         (let ((y (fixnumize y)))
           ,inst)))
     (define-vop (,(symbolicate "FAST-" translate "-C/SIGNED=>SIGNED")
                  fast-signed-c-binop)
       (:arg-types signed-num (:constant ,untagged-type))
       (:translate ,translate)
       (:generator ,untagged-cost
         ,inst))
     (define-vop (,(symbolicate "FAST-" translate "-C/UNSIGNED=>UNSIGNED")
                  fast-unsigned-c-binop)
       (:arg-types unsigned-num (:constant ,untagged-type))
       (:translate ,translate)
       (:generator ,untagged-cost
         ,inst))))

(define-c-binop + 1 3 (signed-byte 9) (signed-byte 11)
  (inst addi y x r))
(define-c-binop - 1 3
  (integer #.(- (1- (ash 1 9))) #.(ash 1 9))
  (integer #.(- (1- (ash 1 11))) #.(ash 1 11))
  (inst addi (- y) x r))

;;; Special case fixnum + and - that trap on overflow.  Useful when we don't
;;; know that the result is going to be a fixnum.

(define-vop (fast-+/fixnum fast-+/fixnum=>fixnum)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types (:or signed-num unsigned-num))
  (:note nil)
  (:generator 4
    (inst addo x y r)))

(define-vop (fast-+-c/fixnum fast-+-c/fixnum=>fixnum)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types (:or signed-num unsigned-num))
  (:note nil)
  (:generator 3
    (inst addio (fixnumize y) x r)))

(define-vop (fast--/fixnum fast--/fixnum=>fixnum)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types (:or signed-num unsigned-num))
  (:note nil)
  (:generator 4
    (inst subo x y r)))

(define-vop (fast---c/fixnum fast---c/fixnum=>fixnum)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types (:or signed-num unsigned-num))
  (:note nil)
  (:generator 3
    (inst addio (- (fixnumize y)) x r)))

;;; Shifting

(define-vop (fast-ash/unsigned=>unsigned)
  (:policy :fast-safe)
  (:translate ash)
  (:note "inline word ASH")
  (:args (number :scs (unsigned-reg))
         (count :scs (signed-reg)))
  (:arg-types unsigned-num tagged-num)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 8
    (inst comb :>= count zero-tn positive :nullify t)
    (inst sub zero-tn count temp)
    (inst comiclr 31 temp zero-tn :>=)
    (inst li 31 temp)
    (inst mtctl temp :sar)
    (inst extrs number 0 1 temp)
    (inst b done)
    (inst shd temp number :variable result)
    POSITIVE
    (inst subi 31 count temp)
    (inst mtctl temp :sar)
    (inst zdep number :variable 32 result)
    DONE))

(define-vop (fast-ash/signed=>signed)
  (:policy :fast-safe)
  (:translate ash)
  (:note "inline word ASH")
  (:args (number :scs (signed-reg))
         (count :scs (signed-reg)))
  (:arg-types signed-num tagged-num)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 8
    (inst comb :>= count zero-tn positive :nullify t)
    (inst sub zero-tn count temp)
    (inst comiclr 31 temp zero-tn :>=)
    (inst li 31 temp)
    (inst mtctl temp :sar)
    (inst extrs number 0 1 temp)
    (inst b done)
    (inst shd temp number :variable result)
    POSITIVE
    (inst subi 31 count temp)
    (inst mtctl temp :sar)
    (inst zdep number :variable 32 result)
    DONE))

(define-vop (fast-ash-c/unsigned=>unsigned)
  (:policy :fast-safe)
  (:translate ash)
  (:note nil)
  (:args (number :scs (unsigned-reg)))
  (:info count)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (cond ((< count 0)
           ;; It is a right shift.
           (inst srl number (min (- count) 31) result))
          ((> count 0)
           ;; It is a left shift.
           (inst sll number (min count 31) result))
          (t
           ;; Count=0?  Shouldn't happen, but it's easy:
           (move number result)))))

(define-vop (fast-ash-c/signed=>signed)
  (:policy :fast-safe)
  (:translate ash)
  (:note nil)
  (:args (number :scs (signed-reg)))
  (:info count)
  (:arg-types signed-num (:constant integer))
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (cond ((< count 0)
           ;; It is a right shift.
           (inst sra number (min (- count) 31) result))
          ((> count 0)
           ;; It is a left shift.
           (inst sll number (min count 31) result))
          (t
           ;; Count=0?  Shouldn't happen, but it's easy:
           (move number result)))))

;;; FIXME: implement FAST-ASH-LEFT/UNSIGNED=>UNSIGNED and friends, for
;;; use in modular ASH (and because they're useful anyway).  -- CSR,
;;; 2004-08-16

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
    (inst move arg shift :>=)
    (inst uaddcm zero-tn shift shift)
    (inst comb := shift zero-tn done)
    (inst li 0 res)
    LOOP
    (inst srl shift 1 shift)
    (inst comb :<> shift zero-tn loop)
    (inst addi (fixnumize 1) res res)
    DONE))

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
    (inst li #x55555555 mask)
    (inst srl arg 1 temp)
    (inst and arg mask num)
    (inst and temp mask temp)
    (inst add num temp num)
    (inst li #x33333333 mask)
    (inst srl num 2 temp)
    (inst and num mask num)
    (inst and temp mask temp)
    (inst add num temp num)
    (inst li #x0f0f0f0f mask)
    (inst srl num 4 temp)
    (inst and num mask num)
    (inst and temp mask temp)
    (inst add num temp num)
    (inst li #x00ff00ff mask)
    (inst srl num 8 temp)
    (inst and num mask num)
    (inst and temp mask temp)
    (inst add num temp num)
    (inst li #x0000ffff mask)
    (inst srl num 16 temp)
    (inst and num mask num)
    (inst and temp mask temp)
    (inst add num temp res)))

;;; Multiply and Divide.

(define-vop (fast-*/fixnum=>fixnum fast-fixnum-binop)
  (:args (x :scs (any-reg) :target x-pass)
         (y :scs (any-reg) :target y-pass))
  (:temporary (:sc signed-reg :offset nl0-offset
                   :from (:argument 0) :to (:result 0)) x-pass)
  (:temporary (:sc signed-reg :offset nl1-offset
                   :from (:argument 1) :to (:result 0)) y-pass)
  (:temporary (:sc signed-reg :offset nl2-offset :target r
                   :from (:argument 1) :to (:result 0)) res-pass)
  (:temporary (:sc signed-reg :offset nl3-offset :to (:result 0)) tmp)
  (:temporary (:sc signed-reg :offset nl4-offset
                   :from (:argument 1) :to (:result 0)) sign)
  (:temporary (:sc interior-reg :offset lip-offset) lip)
  (:ignore lip sign)
  (:translate *)
  (:generator 30
    (unless (location= y y-pass)
      (inst sra x 2 x-pass))
    (let ((fixup (make-fixup 'multiply :assembly-routine)))
      (inst ldil fixup tmp)
      (inst ble fixup lisp-heap-space tmp))
    (if (location= y y-pass)
        (inst sra x 2 x-pass)
        (inst move y y-pass))
    (move res-pass r)))

(define-vop (fast-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:args (x :scs (signed-reg) :target x-pass)
         (y :scs (signed-reg) :target y-pass))
  (:temporary (:sc signed-reg :offset nl0-offset
                   :from (:argument 0) :to (:result 0)) x-pass)
  (:temporary (:sc signed-reg :offset nl1-offset
                   :from (:argument 1) :to (:result 0)) y-pass)
  (:temporary (:sc signed-reg :offset nl2-offset :target r
                   :from (:argument 1) :to (:result 0)) res-pass)
  (:temporary (:sc signed-reg :offset nl3-offset :to (:result 0)) tmp)
  (:temporary (:sc signed-reg :offset nl4-offset
                   :from (:argument 1) :to (:result 0)) sign)
  (:temporary (:sc interior-reg :offset lip-offset) lip)
  (:ignore lip sign)
  (:translate *)
  (:generator 31
    (let ((fixup (make-fixup 'multiply :assembly-routine)))
      (move x x-pass)
      (move y y-pass)
      (inst ldil fixup tmp)
      (inst ble fixup lisp-heap-space tmp :nullify t)
      (inst nop)
      (move res-pass r))))

(define-vop (fast-truncate/fixnum fast-fixnum-binop)
  (:translate truncate)
  (:args (x :scs (any-reg) :target x-pass)
         (y :scs (any-reg) :target y-pass))
  (:temporary (:sc signed-reg :offset nl0-offset
                   :from (:argument 0) :to (:result 0)) x-pass)
  (:temporary (:sc signed-reg :offset nl1-offset
                   :from (:argument 1) :to (:result 0)) y-pass)
  (:temporary (:sc signed-reg :offset nl2-offset :target q
                   :from (:argument 1) :to (:result 0)) q-pass)
  (:temporary (:sc signed-reg :offset nl3-offset :target r
                   :from (:argument 1) :to (:result 1)) r-pass)
  (:results (q :scs (signed-reg))
            (r :scs (any-reg)))
  (:result-types tagged-num tagged-num)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 30
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (inst bc := nil y zero-tn zero))
    (move x x-pass)
    (move y y-pass)
    (let ((fixup (make-fixup 'truncate :assembly-routine)))
      (inst ldil fixup q-pass)
      (inst ble fixup lisp-heap-space q-pass :nullify t))
    (inst nop)
    (move q-pass q)
    (move r-pass r)))

(define-vop (fast-truncate/signed fast-signed-binop)
  (:translate truncate)
  (:args (x :scs (signed-reg) :target x-pass)
         (y :scs (signed-reg) :target y-pass))
  (:temporary (:sc signed-reg :offset nl0-offset
                   :from (:argument 0) :to (:result 0)) x-pass)
  (:temporary (:sc signed-reg :offset nl1-offset
                   :from (:argument 1) :to (:result 0)) y-pass)
  (:temporary (:sc signed-reg :offset nl2-offset :target q
                   :from (:argument 1) :to (:result 0)) q-pass)
  (:temporary (:sc signed-reg :offset nl3-offset :target r
                   :from (:argument 1) :to (:result 1)) r-pass)
  (:results (q :scs (signed-reg))
            (r :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 35
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (inst bc := nil y zero-tn zero))
    (move x x-pass)
    (move y y-pass)
    (let ((fixup (make-fixup 'truncate :assembly-routine)))
      (inst ldil fixup q-pass)
      (inst ble fixup lisp-heap-space q-pass :nullify t))
    (inst nop)
    (move q-pass q)
    (move r-pass r)))


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
  (:arg-types tagged-num (:constant (signed-byte 9)))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 32) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num (:constant (signed-byte 11)))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num (:constant (signed-byte 11)))
  (:info target not-p y))


(defmacro define-conditional-vop (translate signed-cond unsigned-cond)
  `(progn
     ,@(mapcar #'(lambda (suffix cost signed imm)
                   (unless (and (member suffix '(/fixnum -c/fixnum))
                                (eq translate 'eql))
                     `(define-vop (,(intern (format nil "~:@(FAST-IF-~A~A~)"
                                                    translate suffix))
                                   ,(intern
                                     (format nil "~:@(FAST-CONDITIONAL~A~)"
                                             suffix)))
                        (:translate ,translate)
                        (:generator ,cost
                          (inst ,(if imm 'bci 'bc)
                                ,(if signed signed-cond unsigned-cond)
                                not-p
                                ,(if (eq suffix '-c/fixnum)
                                     '(fixnumize y)
                                     'y)
                                x
                                target)))))
               '(/fixnum -c/fixnum /signed -c/signed /unsigned -c/unsigned)
               '(3 2 5 4 5 4)
               '(t t t t nil nil)
               '(nil t nil t nil t))))

;; We switch < and > because the immediate has to come first.

(define-conditional-vop < :> :>>)
(define-conditional-vop > :< :<<)

;;; EQL/FIXNUM is funny because the first arg can be of any type, not just a
;;; known fixnum.
;;;
(define-conditional-vop eql := :=)

;;; These versions specify a fixnum restriction on their first arg.  We have
;;; also generic-eql/fixnum VOPs which are the same, but have no restriction on
;;; the first arg and a higher cost.  The reason for doing this is to prevent
;;; fixnum specific operations from being used on word integers, spuriously
;;; consing the argument.
;;;
(define-vop (fast-eql/fixnum fast-conditional)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql)
  (:generator 3
    (inst bc := not-p x y target)))
;;;
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:arg-types tagged-num (:constant (signed-byte 9)))
  (:info target not-p y)
  (:translate eql)
  (:generator 2
    (inst bci := not-p (fixnumize y) x target)))
;;;
(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:arg-types * (:constant (signed-byte 9)))
  (:variant-cost 6))


;;;; modular functions
(define-modular-fun +-mod32 (x y) + :unsigned 32)
(define-vop (fast-+-mod32/unsigned=>unsigned fast-+/unsigned=>unsigned)
  (:translate +-mod32))
(define-vop (fast-+-mod32-c/unsigned=>unsigned fast-+-c/unsigned=>unsigned)
  (:translate +-mod32))
(define-modular-fun --mod32 (x y) - :unsigned 32)
(define-vop (fast---mod32/unsigned=>unsigned fast--/unsigned=>unsigned)
  (:translate --mod32))
(define-vop (fast---mod32-c/unsigned=>unsigned fast---c/unsigned=>unsigned)
  (:translate --mod32))

(define-vop (fast-ash-left-mod32-c/unsigned=>unsigned
             fast-ash-c/unsigned=>unsigned)
  (:translate ash-left-mod32))
(define-vop (fast-ash-left-mod32/unsigned=>unsigned
             ;; FIXME: when FAST-ASH-LEFT/UNSIGNED=>UNSIGNED is
             ;; implemented, use it here.  -- CSR, 2004-08-16
             fast-ash/unsigned=>unsigned))
(deftransform ash-left-mod32 ((integer count)
                              ((unsigned-byte 32) (unsigned-byte 5)))
  (when (sb!c::constant-lvar-p count)
    (sb!c::give-up-ir1-transform))
  '(%primitive fast-ash-left-mod32/unsigned=>unsigned integer count))

(define-modular-fun lognot-mod32 (x) lognot :unsigned 32)
(define-vop (lognot-mod32/unsigned=>unsigned)
  (:translate lognot-mod32)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (inst uaddcm zero-tn x res)))

(macrolet
    ((define-modular-backend (fun)
       (let ((mfun-name (symbolicate fun '-mod32))
             ;; FIXME: if anyone cares, add constant-arg vops.  --
             ;; CSR, 2003-09-16
             (modvop (symbolicate 'fast- fun '-mod32/unsigned=>unsigned))
             (vop (symbolicate 'fast- fun '/unsigned=>unsigned)))
         `(progn
            (define-modular-fun ,mfun-name (x y) ,fun :unsigned 32)
            (define-vop (,modvop ,vop)
              (:translate ,mfun-name))))))
  (define-modular-backend logxor)
  (define-modular-backend logandc1)
  (define-modular-backend logandc2))

(define-source-transform logeqv (&rest args)
  (if (oddp (length args))
      `(logxor ,@args)
      `(lognot (logxor ,@args))))
(define-source-transform logorc1 (x y)
  `(logior (lognot ,x) ,y))
(define-source-transform logorc2 (x y)
  `(logior ,x (lognot ,y)))
(define-source-transform lognand (x y)
  `(lognot (logand ,x ,y)))
(define-source-transform lognor (x y)
  `(lognot (logior ,x y)))

(define-vop (shift-towards-someplace)
  (:policy :fast-safe)
  (:args (num :scs (unsigned-reg))
         (amount :scs (signed-reg)))
  (:arg-types unsigned-num tagged-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (shift-towards-start shift-towards-someplace)
  (:translate shift-towards-start)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:note "SHIFT-TOWARDS-START")
  (:generator 1
    (inst subi 31 amount temp)
    (inst mtctl temp :sar)
    (inst zdep num :variable 32 r)))

(define-vop (shift-towards-end shift-towards-someplace)
  (:translate shift-towards-end)
  (:note "SHIFT-TOWARDS-END")
  (:generator 1
    (inst mtctl amount :sar)
    (inst shd zero-tn num :variable r)))



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
  (:effects)
  (:affected)
  (:generator 1
    (inst bc :>= not-p digit zero-tn target)))

(define-vop (add-w/carry)
  (:translate sb!bignum:%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg))
            (carry :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:generator 3
    (inst addi -1 c zero-tn)
    (inst addc a b result)
    (inst addc zero-tn zero-tn carry)))

(define-vop (sub-w/borrow)
  (:translate sb!bignum:%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg))
            (borrow :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (inst addi -1 c zero-tn)
    (inst subb a b result)
    (inst addc zero-tn zero-tn borrow)))

(define-vop (bignum-mult)
  (:translate sb!bignum:%multiply)
  (:policy :fast-safe)
  (:args (x-arg :scs (unsigned-reg) :target x)
         (y-arg :scs (unsigned-reg) :target y))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:scs (signed-reg) :from (:argument 0)) x)
  (:temporary (:scs (signed-reg) :from (:argument 1)) y)
  (:temporary (:scs (signed-reg)) tmp)
  (:results (hi :scs (unsigned-reg))
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 3
    ;; Make sure X is less then Y.
    (inst comclr x-arg y-arg tmp :<<)
    (inst xor x-arg y-arg tmp)
    (inst xor x-arg tmp x)
    (inst xor y-arg tmp y)

    ;; Blow out of here if the result is zero.
    (inst li 0 hi)
    (inst comb := x zero-tn done)
    (inst li 0 lo)
    (inst li 0 tmp)

    LOOP
    (inst comb :ev x zero-tn next-bit)
    (inst srl x 1 x)
    (inst add lo y lo)
    (inst addc hi tmp hi)
    NEXT-BIT
    (inst add y y y)
    (inst comb :<> x zero-tn loop)
    (inst addc tmp tmp tmp)

    DONE))

(define-source-transform sb!bignum:%multiply-and-add (x y carry &optional (extra 0))
  #+nil ;; This would be greate if it worked, but it doesn't.
  (if (eql extra 0)
      `(multiple-value-call #'sb!bignum:%dual-word-add
         (sb!bignum:%multiply ,x ,y)
         (values ,carry))
      `(multiple-value-call #'sb!bignum:%dual-word-add
         (multiple-value-call #'sb!bignum:%dual-word-add
           (sb!bignum:%multiply ,x ,y)
           (values ,carry))
         (values ,extra)))
  (with-unique-names (hi lo)
    (if (eql extra 0)
        `(multiple-value-bind (,hi ,lo) (sb!bignum:%multiply ,x ,y)
           (sb!bignum::%dual-word-add ,hi ,lo ,carry))
        `(multiple-value-bind (,hi ,lo) (sb!bignum:%multiply ,x ,y)
           (multiple-value-bind
               (,hi ,lo)
               (sb!bignum::%dual-word-add ,hi ,lo ,carry)
             (sb!bignum::%dual-word-add ,hi ,lo ,extra))))))

(defknown sb!bignum::%dual-word-add
          (sb!bignum:bignum-element-type sb!bignum:bignum-element-type sb!bignum:bignum-element-type)
  (values sb!bignum:bignum-element-type sb!bignum:bignum-element-type)
  (flushable movable))

(define-vop (dual-word-add)
  (:policy :fast-safe)
  (:translate sb!bignum::%dual-word-add)
  (:args (hi :scs (unsigned-reg) :to (:result 1))
         (lo :scs (unsigned-reg))
         (extra :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (hi-res :scs (unsigned-reg) :from (:result 1))
            (lo-res :scs (unsigned-reg) :from (:result 0)))
  (:result-types unsigned-num unsigned-num)
  (:affected)
  (:effects)
  (:generator 3
    (inst add lo extra lo-res)
    (inst addc hi zero-tn hi-res)))

(define-vop (bignum-lognot)
  (:translate sb!bignum:%lognot)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst uaddcm zero-tn x r)))

(define-vop (fixnum-to-digit)
  (:translate sb!bignum:%fixnum-to-digit)
  (:policy :fast-safe)
  (:args (fixnum :scs (signed-reg)))
  (:arg-types tagged-num)
  (:results (digit :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (move fixnum digit)))

(define-vop (bignum-floor)
  (:translate sb!bignum:%floor)
  (:policy :fast-safe)
  (:args (hi :scs (unsigned-reg) :to (:argument 1))
         (lo :scs (unsigned-reg) :to (:argument 0))
         (divisor :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :to (:argument 1)) temp)
  (:results (quo :scs (unsigned-reg) :from (:argument 0))
            (rem :scs (unsigned-reg) :from (:argument 1)))
  (:result-types unsigned-num unsigned-num)
  (:generator 65
    (inst sub zero-tn divisor temp)
    (inst ds zero-tn temp zero-tn)
    (inst add lo lo quo)
    (inst ds hi divisor rem)
    (inst addc quo quo quo)
    (dotimes (i 31)
      (inst ds rem divisor rem)
      (inst addc quo quo quo))
    (inst comclr rem zero-tn zero-tn :>=)
    (inst add divisor rem rem)))

(define-vop (signify-digit)
  (:translate sb!bignum:%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (move digit res)))

(define-vop (digit-lshr)
  (:translate sb!bignum:%digit-logical-shift-right)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg))
         (count :scs (unsigned-reg)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 2
    (inst mtctl count :sar)
    (inst shd zero-tn digit :variable result)))

(define-vop (digit-ashr digit-lshr)
  (:translate sb!bignum:%ashr)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 1
    (inst extrs digit 0 1 temp)
    (inst mtctl count :sar)
    (inst shd temp digit :variable result)))

(define-vop (digit-ashl digit-ashr)
  (:translate sb!bignum:%ashl)
  (:generator 1
    (inst subi 31 count temp)
    (inst mtctl temp :sar)
    (inst zdep digit :variable 32 result)))


;;;; Static functions.

(define-static-fun two-arg-gcd (x y) :translate gcd)
(define-static-fun two-arg-lcm (x y) :translate lcm)

(define-static-fun two-arg-* (x y) :translate *)
(define-static-fun two-arg-/ (x y) :translate /)

(define-static-fun %negate (x) :translate %negate)

(define-static-fun two-arg-and (x y) :translate logand)
(define-static-fun two-arg-ior (x y) :translate logior)
(define-static-fun two-arg-xor (x y) :translate logxor)
