;;;; the LoongArch VM definition of operand loading/saving and the MOVE vop

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
  ((zero immediate)
   (any-reg descriptor-reg))
  (let ((val (tn-value x)))
    (etypecase val
      (integer
       (inst li y (fixnumize val)))
      (null
       (move y null-tn))
      (symbol
       (load-symbol y val))
      (character
       (inst li y (logior (ash (char-code val) n-widetag-bits)
                          character-widetag)))
      (structure-object
       (if (eq val sb-lockless:+tail+)
           (inst addi.d y null-tn (- lockfree-list-tail-value-offset
                                   nil-value-offset))
           (bug "immediate structure-object ~S" val))))))

(define-move-fun (load-number 1) (vop x y)
  ((zero immediate)
   (signed-reg unsigned-reg))
  (inst li y (tn-value x)))

(define-move-fun (load-character 1) (vop x y)
  ((immediate) (character-reg))
  (inst li y (char-code (tn-value x))))

(define-move-fun (load-system-area-pointer 1) (vop x y)
  ((immediate) (sap-reg))
  (inst li y (sap-int (tn-value x))))

(define-move-fun (load-constant 5) (vop x y)
  ((constant) (any-reg descriptor-reg))
  (let ((offset (tn-byte-offset x)))
    (etypecase offset
      (short-immediate
       (loadw y code-tn (tn-offset x) other-pointer-lowtag))
      (u+i-immediate
       (inst load-far-constant y x)))))

(define-move-fun (load-stack 5) (vop x y)
  ((control-stack) (any-reg descriptor-reg))
  (load-stack-tn y x))

(define-move-fun (load-number-stack 5) (vop x y)
  ((character-stack) (character-reg)
   (sap-stack) (sap-reg)
   (signed-stack) (signed-reg)
   (unsigned-stack) (unsigned-reg))
  (load-frame-word y (current-nfp-tn vop) (tn-offset x) 'load-number-stack nil))

(define-move-fun (store-stack 5) (vop x y)
  ((any-reg descriptor-reg zero) (control-stack))
  (store-stack-tn y x))

(define-move-fun (store-number-stack 5) (vop x y)
  ((character-reg) (character-stack)
   (sap-reg) (sap-stack)
   (signed-reg) (signed-stack)
   (unsigned-reg) (unsigned-stack))
  (store-frame-word x (current-nfp-tn vop) (tn-offset y) 'store-number-stack nil))

(define-vop (move)
  (:args (x :target y
            :scs (any-reg zero descriptor-reg)
            :load-if (not (location= x y))))
  (:results (y :scs (any-reg descriptor-reg control-stack)
               :load-if (not (location= x y))))
  (:generator 0
    (unless (location= x y)
      (sc-case y
        ((any-reg descriptor-reg)
         (move y x))
        (control-stack
         (store-stack-tn y x))))))

(define-move-vop move :move
  (any-reg descriptor-reg zero)
  (any-reg descriptor-reg))

;;; Used for moving descriptor values into another frame for argument passing.
(define-vop (move-arg)
  (:args (x :target y
            :scs (any-reg descriptor-reg zero))
         (fp :scs (any-reg)
             :load-if (not (sc-is y any-reg descriptor-reg))))
  (:results (y))
  (:temporary (:sc unsigned-reg) tmp)
  (:generator 0
    (sc-case y
      ((any-reg descriptor-reg)
       (move y x))
      (control-stack
       (store-frame-word x fp (tn-offset y) 'move-arg tmp)))))

(define-move-vop move-arg :move-arg
  (any-reg descriptor-reg zero)
  (any-reg descriptor-reg))

;;; Move a tagged fixnum to a raw full-word representation by shifting off the tag bits.
;;; Arg is a fixnum, so just shift it. We need a type restriction because some
;;; possible arg SCs (control-stack) overlap with possible bignum arg SCs.
(define-vop (move-to-word/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:arg-types tagged-num)
  (:note "fixnum untagging")
  (:generator 1
    (inst srai.d y x n-fixnum-tag-bits)))
(define-move-vop move-to-word/fixnum :move
  (any-reg descriptor-reg) (signed-reg unsigned-reg))

;;; ARG is a non-immediate constant; load it.
(define-vop (move-to-word-c)
  (:args (x :scs (constant)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:vop-var vop)
  (:note "constant load")
  (:generator 1
    (cond ((sb-c::tn-leaf x)
           (inst li y (tn-value x)))
          (t
           (load-constant vop x y)
           (inst srai.d y y n-fixnum-tag-bits)))))

(define-move-vop move-to-word-c :move
  (constant) (signed-reg unsigned-reg))

;;; Move a fixnum or bignum to an untagged word representation
(define-vop (move-to-word/integer)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (signed-reg unsigned-reg) :from :eval))
  (:note "integer to untagged word coercion")
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 3
    (inst s_andi temp x fixnum-tag-mask)
    (sc-case y
      (signed-reg
       (inst srai.d y x n-fixnum-tag-bits))
      (unsigned-reg
       (inst srli.d y x n-fixnum-tag-bits)))
    (inst beq temp zero-tn DONE)
    (loadw y x bignum-digits-offset other-pointer-lowtag)  ; Handle bignum case
    DONE))

(define-move-vop move-to-word/integer :move
  (descriptor-reg) (signed-reg unsigned-reg))

;;; Move a raw word to a tagged fixnum representation by shifting in tag bits
(define-vop (move-from-word/fixnum)
  (:args (x :scs (signed-reg unsigned-reg) :target y))
  (:results (y :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note "fixnum tagging")
  (:generator 1
    (inst slli.d y x n-fixnum-tag-bits)))

(define-move-vop move-from-word/fixnum :move
  (signed-reg unsigned-reg) (any-reg descriptor-reg))

;;; RESULT may be a bignum, so we have to check.  Use a worst-case
;;; cost to make sure people know they may be number consing.
(define-vop (move-from-signed)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg) :from :eval))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x)
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:note "signed word to integer coercion")
  (:generator 15
    (move x arg)
    (inst srai.d pa-flag x n-positive-fixnum-bits)
    (inst slli.d y x n-fixnum-tag-bits)
    (inst beq pa-flag zero-tn done)
    (inst s_xori pa-flag pa-flag -1)
    (inst beq pa-flag zero-tn done)
    (with-fixed-allocation (y pa-flag bignum-widetag (1+ bignum-digits-offset))
      (storew x y bignum-digits-offset other-pointer-lowtag))
    DONE))

(define-move-vop move-from-signed :move
  (signed-reg) (descriptor-reg))

(define-vop (move-from-fixnum+/-1)
  (:args (x :scs (signed-reg unsigned-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (y :scs (any-reg descriptor-reg) :from :eval))
  (:vop-var vop)
  (:variant-vars constant)
  (:generator 4
    (inst srai.d temp x n-positive-fixnum-bits)
    (inst slli.d y x n-fixnum-tag-bits)
    (inst beq temp zero-tn done)
    (inst s_xori temp temp -1)
    (inst beq temp zero-tn done)
    (load-constant vop (emit-constant constant) y)
    DONE))

(define-vop (move-from-fixnum+1 move-from-fixnum+/-1)
  (:variant (1+ most-positive-fixnum)))

(define-vop (move-from-fixnum-1 move-from-fixnum+/-1)
  (:variant (1- most-negative-fixnum)))

;;; Check for fixnum, and possibly allocate one or two word bignum
;;; result.  Use a worst-case cost to make sure people know they may
;;; be number consing.
(define-vop (move-from-unsigned)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg) :from :eval))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x)
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:note "unsigned word to integer coercion")
  (:generator 20
    (move x arg)
    (inst srli.d pa-flag x n-positive-fixnum-bits)
    (inst slli.d y x n-fixnum-tag-bits)
    (inst beq pa-flag zero-tn done)
    (pseudo-atomic (pa-flag)
      (allocation nil (pad-data-block (+ bignum-digits-offset 2)) other-pointer-lowtag
                  y :flag-tn pa-flag)
      (inst slt pa-flag x zero-tn)
      (inst slli.d pa-flag pa-flag n-widetag-bits)
      (inst addi.d pa-flag pa-flag (logior (ash 1 n-widetag-bits) bignum-widetag))
      (storew pa-flag y 0 other-pointer-lowtag)
      (storew x y bignum-digits-offset other-pointer-lowtag))
    DONE))

(define-move-vop move-from-unsigned :move
  (unsigned-reg) (descriptor-reg))

;;; Move untagged numbers.
(define-vop (word-move)
  (:args (x :scs (signed-reg unsigned-reg) :target y
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

;;; Use standard MOVE-ARGUMENT + coercion to move an untagged number to a
;;; descriptor passing location.
(define-move-vop move-arg :move-arg
  (signed-reg unsigned-reg) (any-reg descriptor-reg))
