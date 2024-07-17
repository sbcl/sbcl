;;;; the PPC VM definition of operand loading/saving and the Move VOP

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(define-move-fun (load-immediate 1) (vop x y)
  ((null immediate zero)
   (any-reg descriptor-reg))
  (let ((val (tn-value x)))
    (etypecase val
      (integer
       (inst lr y (fixnumize val)))
      (null
       (move y null-tn))
      (symbol
       (load-symbol y val))
      (character
       (inst lr y (logior (ash (char-code val) n-widetag-bits)
                          character-widetag)))
      (structure-object
       (if (eq val sb-lockless:+tail+)
           (inst addi y null-tn (- lockfree-list-tail-value-offset
                                   nil-value-offset))
           (bug "immediate structure-object ~S" val))))))

(define-move-fun (load-number 1) (vop x y)
  ((immediate zero)
   (signed-reg unsigned-reg))
  (inst lr y (tn-value x)))

(define-move-fun (load-character 1) (vop x y)
  ((immediate) (character-reg))
  (inst lr y (char-code (tn-value x))))

(define-move-fun (load-system-area-pointer 1) (vop x y)
  ((immediate) (sap-reg))
  (inst lr y (sap-int (tn-value x))))

(define-move-fun (load-constant 5) (vop x y)
  ((constant) (descriptor-reg))
  (loadw y code-tn (tn-offset x) other-pointer-lowtag))

(define-move-fun (load-stack 5) (vop x y)
  ((control-stack) (any-reg descriptor-reg))
  (load-stack-tn y x))

(define-move-fun (load-number-stack 5) (vop x y)
  ((character-stack) (character-reg)
   (sap-stack) (sap-reg)
   (signed-stack) (signed-reg)
   (unsigned-stack) (unsigned-reg))
  (let ((nfp (current-nfp-tn vop)))
    (loadw y nfp (tn-offset x))))

(define-move-fun (store-stack 5) (vop x y)
  ((any-reg descriptor-reg) (control-stack))
  (store-stack-tn y x))

(define-move-fun (store-number-stack 5) (vop x y)
  ((character-reg) (character-stack)
   (sap-reg) (sap-stack)
   (signed-reg) (signed-stack)
   (unsigned-reg) (unsigned-stack))
  (let ((nfp (current-nfp-tn vop)))
    (storew x nfp (tn-offset y))))


;;;; The Move VOP:
(define-vop (move)
  (:args (x :target y
            :scs (any-reg descriptor-reg zero null)
            :load-if (not (location= x y))))
  (:results (y :scs (any-reg descriptor-reg control-stack)
               :load-if (not (location= x y))))
  (:generator 0
    (cond ((location= x y))
          ((sc-is y control-stack)
           (store-stack-tn y x))
          (t
           (move y x)))))

(define-move-vop move :move
  (any-reg descriptor-reg)
  (any-reg descriptor-reg))

;;; The MOVE-ARG VOP is used for moving descriptor values into another
;;; frame for argument or known value passing.
(define-vop (move-arg)
  (:args (x :target y
            :scs (any-reg descriptor-reg zero null))
         (fp :scs (any-reg)
             :load-if (not (sc-is y any-reg descriptor-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      ((any-reg descriptor-reg)
       (move y x))
      (control-stack
       (storew x fp (tn-offset y))))))
;;;
(define-move-vop move-arg :move-arg
  (any-reg descriptor-reg)
  (any-reg descriptor-reg))

;;;; Moves and coercions:

;;; These MOVE-TO-WORD VOPs move a tagged integer to a raw full-word
;;; representation.  Similarly, the MOVE-FROM-WORD VOPs converts a raw integer
;;; to a tagged bignum or fixnum.

;;; ARG is a fixnum, so just shift it.  We need a type restriction because some
;;; possible arg SCs (control-stack) overlap with possible bignum arg SCs.
(define-vop (move-to-word/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:arg-types tagged-num)
  (:note "fixnum untagging")
  (:generator 1
    (inst srawi y x n-fixnum-tag-bits)))
(define-move-vop move-to-word/fixnum :move
  (any-reg descriptor-reg) (signed-reg unsigned-reg))

;;; ARG is a non-immediate constant; load it.
(define-vop (move-to-word-c)
  (:args (x :scs (constant)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "constant load")
  (:generator 1
    (cond ((sb-c::tn-leaf x)
           (inst lr y (tn-value x)))
          (t
           (loadw y code-tn (tn-offset x) other-pointer-lowtag)
           (inst srawi y y n-fixnum-tag-bits)))))
(define-move-vop move-to-word-c :move
  (constant) (signed-reg unsigned-reg))

;;; ARG is a fixnum or bignum; figure out which and load if necessary.
(define-vop (move-to-word/integer)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "integer to untagged word coercion")
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 4
    (inst andi. temp x fixnum-tag-mask)
    (inst srawi y x n-fixnum-tag-bits)

    (inst beq done)
    (loadw y x bignum-digits-offset other-pointer-lowtag)
    DONE))
(define-move-vop move-to-word/integer :move
  (descriptor-reg) (signed-reg unsigned-reg))

;;; RESULT is a fixnum, so we can just shift.  We need the result type
;;; restriction because of the control-stack ambiguity noted above.
(define-vop (move-from-word/fixnum)
  (:args (x :scs (signed-reg unsigned-reg)))
  (:results (y :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note "fixnum tagging")
  (:generator 1
    (inst slwi y x n-fixnum-tag-bits)))
(define-move-vop move-from-word/fixnum :move
  (signed-reg unsigned-reg) (any-reg descriptor-reg))

;;; RESULT may be a bignum, so we have to check.  Use a worst-case
;;; cost to make sure people know they may be number consing.
(define-vop (move-from-signed)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:note "signed word to integer coercion")
  (:generator 20
    (move x arg)
    ;; Don't even think about using mcrxr, you couldn't make up a
    ;; slower instruction
    (inst mtxer zero-tn)              ; clear sticky overflow bit in XER, CR0
    (inst addo temp x x)              ; set XER OV if top two bits differ
    (inst addo. temp temp temp)       ; set CR0 SO if any top three bits differ
    (inst slwi y x n-fixnum-tag-bits) ; assume fixnum (tagged ok, maybe lost some high bits)
    (inst bns done)

    (with-fixed-allocation (y pa-flag temp bignum-widetag (1+ bignum-digits-offset))
      (storew x y bignum-digits-offset other-pointer-lowtag))
    DONE))
(define-move-vop move-from-signed :move
  (signed-reg) (descriptor-reg))

(define-vop (move-from-fixnum+1)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x temp)
  (:vop-var vop)
  (:generator 4
    (move x arg)
    (inst mtxer zero-tn)              ; clear sticky overflow bit in XER, CR0
    (inst addo temp x x)              ; set XER OV if top two bits differ
    (inst addo. temp temp temp)       ; set CR0 SO if any top three bits differ
    (inst slwi y x n-fixnum-tag-bits) ; assume fixnum (tagged ok, maybe lost some high bits)
    (inst bns done)
    (load-constant vop (emit-constant (1+ most-positive-fixnum))
                   y)
    DONE))

(define-vop (move-from-fixnum-1 move-from-fixnum+1)
  (:generator 4
    (move x arg)
    (inst mtxer zero-tn)              ; clear sticky overflow bit in XER, CR0
    (inst addo temp x x)              ; set XER OV if top two bits differ
    (inst addo. temp temp temp)       ; set CR0 SO if any top three bits differ
    (inst slwi y x n-fixnum-tag-bits) ; assume fixnum (tagged ok, maybe lost some high bits)
    (inst bns done)
    (load-constant vop (emit-constant (1- most-negative-fixnum))
                   y)
    DONE))

;;; Check for fixnum, and possibly allocate one or two word bignum
;;; result.  Use a worst-case cost to make sure people know they may
;;; be number consing.
(define-vop (move-from-unsigned)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:note "unsigned word to integer coercion")
  (:generator 20
    (move x arg)
    (let ((done (gen-label))
          (one-word (gen-label)))
      (inst srawi. temp x n-positive-fixnum-bits)
      (inst slwi y x n-fixnum-tag-bits)
      (inst beq done)

      (with-fixed-allocation
          (y pa-flag temp bignum-widetag (+ 2 bignum-digits-offset))
        (inst cmpwi x 0)
        (inst li temp (logior (ash 1 n-widetag-bits) bignum-widetag))
        (inst bge one-word)
        (inst li temp (logior (ash 2 n-widetag-bits) bignum-widetag))
        (emit-label one-word)
        (storew temp y 0 other-pointer-lowtag)
        (storew x y bignum-digits-offset other-pointer-lowtag))
      (emit-label done))))
(define-move-vop move-from-unsigned :move
  (unsigned-reg) (descriptor-reg))


;;; Move untagged numbers.
(define-vop (word-move)
  (:args (x :target y
            :scs (signed-reg unsigned-reg)
            :load-if (not (location= x y))))
  (:results (y :scs (signed-reg unsigned-reg)
               :load-if (not (location= x y))))
  (:note "word integer move")
  (:generator 0
    (move y x)))
(define-move-vop word-move :move
  (signed-reg unsigned-reg) (signed-reg unsigned-reg))


;;; Move untagged number arguments/return-values.
(define-vop (move-word-arg)
  (:args (x :target y
            :scs (signed-reg unsigned-reg))
         (fp :scs (any-reg)
             :load-if (not (sc-is y signed-reg unsigned-reg))))
  (:results (y))
  (:note "word integer argument move")
  (:generator 0
    (sc-case y
      ((signed-reg unsigned-reg)
       (move y x))
      ((signed-stack unsigned-stack)
       (storew x fp (tn-offset y))))))
(define-move-vop move-word-arg :move-arg
  (descriptor-reg any-reg signed-reg unsigned-reg) (signed-reg unsigned-reg))

;;; Use standard MOVE-ARG + coercion to move an untagged number to a
;;; descriptor passing location.
(define-move-vop move-arg :move-arg
  (signed-reg unsigned-reg) (any-reg descriptor-reg))
