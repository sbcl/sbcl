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
    (inst sub zero-tn x res)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (inst sub zero-tn x res)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:translate lognot)
  (:temporary (:scs (any-reg) :type fixnum :to (:result 0))
              temp)
  (:generator 1
    (inst li (fixnumize -1) temp)
    (inst xor x temp res)))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 2
    (inst uaddcm zero-tn x res)))

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

(macrolet
  ((define-binop (translate cost untagged-cost op arg-swap)
    `(progn
       (define-vop (,(symbolicate "FAST-" translate "/FIXNUM=>FIXNUM")
                    fast-fixnum-binop)
         (:args (x :target r :scs (any-reg))
                (y :target r :scs (any-reg)))
         (:translate ,translate)
         (:generator ,(1+ cost)
           ,(if arg-swap
                `(inst ,op y x r)
                `(inst ,op x y r))))
       (define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
                    fast-signed-binop)
         (:args (x :target r :scs (signed-reg))
                (y :target r :scs (signed-reg)))
         (:translate ,translate)
         (:generator ,(1+ untagged-cost)
           ,(if arg-swap
                `(inst ,op y x r)
                `(inst ,op x y r))))
       (define-vop (,(symbolicate "FAST-" translate "/UNSIGNED=>UNSIGNED")
                    fast-unsigned-binop)
         (:args (x :target r :scs (unsigned-reg))
                (y :target r :scs (unsigned-reg)))
         (:translate ,translate)
         (:generator ,(1+ untagged-cost)
           ,(if arg-swap
                `(inst ,op y x r)
                `(inst ,op x y r)))))))
  (define-binop + 1 5 add nil)
  (define-binop - 1 5 sub nil)
  (define-binop logior 1 2 or nil)
  (define-binop logand 1 2 and nil)
  (define-binop logandc1 1 2 andcm t)
  (define-binop logandc2 1 2 andcm nil)
  (define-binop logxor 1 2 xor nil))

(macrolet
  ((define-c-binop (translate cost untagged-cost tagged-type untagged-type inst)
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
           ,inst)))))

  (define-c-binop + 1 3 (signed-byte 9) (signed-byte 11)
    (inst addi y x r))
  (define-c-binop - 1 3
    (integer #.(- 1 (ash 1 8)) #.(ash 1 8))
    (integer #.(- 1 (ash 1 10)) #.(ash 1 10))
    (inst addi (- y) x r)))

(define-vop (fast-lognor/fixnum=>fixnum fast-fixnum-binop)
  (:translate lognor)
  (:args (x :target r :scs (any-reg))
         (y :target r :scs (any-reg)))
  (:temporary (:sc non-descriptor-reg) temp)
  (:generator 4
    (inst or x y temp)
    (inst uaddcm zero-tn temp temp)
    (inst addi (- fixnum-tag-mask) temp r)))

(define-vop (fast-lognor/signed=>signed fast-signed-binop)
  (:translate lognor)
  (:args (x :target r :scs (signed-reg))
         (y :target r :scs (signed-reg)))
  (:generator 4
    (inst or x y r)
    (inst uaddcm zero-tn r r)))

(define-vop (fast-lognor/unsigned=>unsigned fast-unsigned-binop)
  (:translate lognor)
  (:args (x :target r :scs (unsigned-reg))
         (y :target r :scs (unsigned-reg)))
  (:generator 4
    (inst or x y r)
    (inst uaddcm zero-tn r r)))

;;; Shifting
(macrolet
  ((fast-ash (name reg num tag save)
     `(define-vop (,name)
        (:translate ash)
        (:note "inline ASH")
        (:policy :fast-safe)
        (:args (number :scs (,reg) :to :save)
               (count  :scs (signed-reg)))
        (:arg-types ,num ,tag)
        (:results (result :scs (,reg)))
        (:result-types ,num)
        (:temporary (:scs (unsigned-reg)
                          ,@(unless save
                              '(:to (:result 0)))) temp)
        (:generator 8
          (inst comb :>= count zero-tn positive :nullify t)
          (inst sub zero-tn count temp)
          ,@(if save
                '(;; Unsigned case
                  (inst comiclr 31 temp result :>=)
                  (inst b done :nullify t)
                  (inst mtctl temp :sar)
                  (inst b done)
                  (inst shd zero-tn number :variable result))
                '(;; Signed case
                  (inst comiclr 31 temp zero-tn :>=)
                  (inst li 31 temp)
                  (inst mtctl temp :sar)
                  (inst extrs number 0 1 temp)
                  (inst b done)
                  (inst shd temp number :variable result)))
          POSITIVE
          (inst subi 31 count temp)
          (inst mtctl temp :sar)
          (inst zdep number :variable 32 result)
          DONE))))
  (fast-ash fast-ash/unsigned=>unsigned unsigned-reg unsigned-num
                                        tagged-num t)
  (fast-ash fast-ash/signed=>signed signed-reg signed-num signed-num nil))

(define-vop (fast-ash-c/unsigned=>unsigned)
  (:translate ash)
  (:note "inline ASH")
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg)))
  (:info count)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (cond
      ((< count -31) (move zero-tn result))
      ((< count 0) (inst srl number (min (- count) 31) result))
      ((> count 0) (inst sll number (min count 31) result))
      (t (bug "identity ASH not transformed away")))))

(define-vop (fast-ash-c/signed=>signed)
  (:translate ash)
  (:note "inline ASH")
  (:policy :fast-safe)
  (:args (number :scs (signed-reg)))
  (:info count)
  (:arg-types signed-num (:constant integer))
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (cond
      ((< count 0) (inst sra number (min (- count) 31) result))
      ((> count 0) (inst sll number (min count 31) result))
      (t (bug "identity ASH not transformed away")))))

(macrolet ((def (name sc-type type result-type cost)
             `(define-vop (,name)
                (:translate ash)
                (:note "inline ASH")
                (:policy :fast-safe)
                (:args (number :scs (,sc-type))
                       (amount :scs (signed-reg unsigned-reg immediate)))
                (:arg-types ,type positive-fixnum)
                (:results (result :scs (,result-type)))
                (:result-types ,type)
                (:temporary (:scs (,sc-type) :to (:result 0)) temp)
                (:generator ,cost
                  (sc-case amount
                    ((signed-reg unsigned-reg)
                      (inst subi 31 amount temp)
                      (inst mtctl temp :sar)
                      (inst zdep number :variable 32 result))
                    (immediate
                      (let ((amount (tn-value amount)))
                        (aver (> amount 0))
                        (inst sll number amount result))))))))
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
  (:translate *)
  (:args (x :scs (any-reg zero) :target x-pass)
         (y :scs (any-reg zero) :target y-pass))
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
  (:ignore lip sign) ; fix-lav: why dont we ignore tmp ?
  (:generator 30
    ;; looking at the register setup above, not sure if both can clash
    ;; maybe it is ok that x and x-pass share register ? like it was
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
  (:generator 31
    (let ((fixup (make-fixup 'multiply :assembly-routine)))
      (move x x-pass)
      (move y y-pass)
      (inst ldil fixup tmp)
      (inst ble fixup lisp-heap-space tmp)
      (inst nop)
      (move res-pass r))))

(define-vop (fast-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:args (x :scs (unsigned-reg) :target x-pass)
         (y :scs (unsigned-reg) :target y-pass))
  (:temporary (:sc unsigned-reg :offset nl0-offset
                   :from (:argument 0) :to (:result 0)) x-pass)
  (:temporary (:sc unsigned-reg :offset nl1-offset
                   :from (:argument 1) :to (:result 0)) y-pass)
  (:temporary (:sc unsigned-reg :offset nl2-offset :target r
                   :from (:argument 1) :to (:result 0)) res-pass)
  (:temporary (:sc unsigned-reg :offset nl3-offset :to (:result 0)) tmp)
  (:temporary (:sc unsigned-reg :offset nl4-offset
                   :from (:argument 1) :to (:result 0)) sign)
  (:temporary (:sc interior-reg :offset lip-offset) lip)
  (:ignore lip sign)
  (:generator 31
    (let ((fixup (make-fixup 'multiply :assembly-routine)))
      (move x x-pass)
      (move y y-pass)
      (inst ldil fixup tmp)
      (inst ble fixup lisp-heap-space tmp)
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
  (:results (q :scs (any-reg))
            (r :scs (any-reg)))
  (:result-types tagged-num tagged-num)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 30
    (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
      (inst bc := nil y zero-tn zero))
    (move x x-pass)
    (move y y-pass)
    (let ((fixup (make-fixup 'truncate :assembly-routine)))
      (inst ldil fixup q-pass)
      (inst ble fixup lisp-heap-space q-pass :nullify t))
    (inst nop)
    (inst sll q-pass n-fixnum-tag-bits q)
    ;(move q-pass q)
    (move r-pass r)))

#+(or) ;; This contains two largely-inexplicable hacks, and there's no
       ;; equivalent VOP for either Alpha or ARM.  Why is this even
       ;; here?  -- AB, 2015-11-19
(define-vop (fast-truncate/unsigned fast-unsigned-binop)
  (:translate truncate)
  (:args (x :scs (unsigned-reg) :target x-pass)
         (y :scs (unsigned-reg) :target y-pass))
  (:temporary (:sc unsigned-reg :offset nl0-offset
                   :from (:argument 0) :to (:result 0)) x-pass)
  (:temporary (:sc unsigned-reg :offset nl1-offset
                   :from (:argument 1) :to (:result 0)) y-pass)
  (:temporary (:sc unsigned-reg :offset nl2-offset :target q
                   :from (:argument 1) :to (:result 0)) q-pass)
  (:temporary (:sc unsigned-reg :offset nl3-offset :target r
                   :from (:argument 1) :to (:result 1)) r-pass)
  (:results (q :scs (unsigned-reg))
            (r :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 35
    (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
      (inst bc := nil y zero-tn zero))
    (move x x-pass)
    (move y y-pass)
    ;; really dirty trick to avoid the bug truncate/unsigned vop
    ;; followed by move-from/word->fixnum where the result from
    ;; the truncate is 0xe39516a7 and move-from-word will treat
    ;; the unsigned high number as an negative number.
    ;; instead we clear the high bit in the input to truncate.
    (inst li #x1fffffff q)
    (inst comb :<> q y skip :nullify t)
    (inst addi -1 zero-tn q)
    (inst srl q 1 q) ; this should result in #7fffffff
    (inst and x-pass q x-pass)
    (inst and y-pass q y-pass)
    SKIP
    ;; fix bug#2  (truncate #xe39516a7 #x3) => #0xf687078d,#x0
    (inst li #x7fffffff q)
    (inst and x-pass q x-pass)
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
    (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
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
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql)
  (:generator 3
    (inst bc := not-p x y target)))
;;;
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg)))
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (signed-byte 9)))
  (:info target not-p y)
  (:translate eql)
  (:generator 2
    (inst bci := not-p (fixnumize y) x target)))
;;;
(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant (signed-byte 9)))
  (:variant-cost 6))


;;;; modular functions
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
  (when (sb!c::constant-lvar-p count)
    (sb!c::give-up-ir1-transform))
  '(%primitive fast-ash-left-mod32/unsigned=>unsigned integer count))

;;; logical operations
(define-modular-fun lognot-mod32 (x) lognot :untagged nil 32)
(define-vop (lognot-mod32/unsigned=>unsigned)
  (:translate lognot-mod32)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (inst uaddcm zero-tn x res)))

(define-modular-fun lognor-mod32 (x y) lognor :untagged nil 32)
(define-vop (fast-lognor-mod32/unsigned=>unsigned
             fast-lognor/unsigned=>unsigned)
  (:translate lognor-mod32))

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
  `(lognot (logior ,x ,y)))

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
  (:generator 2
    (inst bc :>= not-p digit zero-tn target)))

(define-vop (add-w/carry)
  (:translate sb!bignum:%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (any-reg)))
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

(define-vop (bignum-lognot lognot-mod32/unsigned=>unsigned)
  (:translate sb!bignum:%lognot))

(define-vop (fixnum-to-digit)
  (:translate sb!bignum:%fixnum-to-digit)
  (:policy :fast-safe)
  (:args (fixnum :scs (any-reg)))
  (:arg-types tagged-num)
  (:results (digit :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst sra fixnum n-fixnum-tag-bits digit)))

(define-vop (bignum-floor)
  (:translate sb!bignum:%bigfloor)
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
  (:results (res :scs (any-reg signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (sc-case res
      (any-reg
        (inst sll digit n-fixnum-tag-bits res))
      (signed-reg
        (move digit res)))))

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

