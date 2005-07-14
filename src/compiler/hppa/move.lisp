;;;; the HPPA VM definition of operand loading/saving and the Move VOP

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(define-move-fun (load-immediate 1) (vop x y)
  ((null zero immediate)
   (any-reg descriptor-reg))
  (let ((val (tn-value x)))
    (etypecase val
      (integer
       (inst li (fixnumize val) y))
      (null
       (move null-tn y))
      (symbol
       (load-symbol y val))
      (character
       (inst li (logior (ash (char-code val) n-widetag-bits)
                        character-widetag)
             y)))))

(define-move-fun (load-number 1) (vop x y)
  ((immediate zero)
   (signed-reg unsigned-reg))
  (let ((x (tn-value x)))
    (inst li (if (>= x (ash 1 31)) (logior (ash -1 32) x) x) y)))

(define-move-fun (load-character 1) (vop x y)
  ((immediate) (character-reg))
  (inst li (char-code (tn-value x)) y))

(define-move-fun (load-system-area-pointer 1) (vop x y)
  ((immediate) (sap-reg))
  (inst li (sap-int (tn-value x)) y))

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
            :scs (any-reg descriptor-reg)
            :load-if (not (location= x y))))
  (:results (y :scs (any-reg descriptor-reg)
               :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (move x y)))

(define-move-vop move :move
  (any-reg descriptor-reg)
  (any-reg descriptor-reg))

;;; Make MOVE the check VOP for T so that type check generation
;;; doesn't think it is a hairy type.  This also allows checking of a
;;; few of the values in a continuation to fall out.
(primitive-type-vop move (:check) t)

;;; The MOVE-ARG VOP is used for moving descriptor values into another
;;; frame for argument or known value passing.
(define-vop (move-arg)
  (:args (x :target y
            :scs (any-reg descriptor-reg))
         (fp :scs (any-reg)
             :load-if (not (sc-is y any-reg descriptor-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      ((any-reg descriptor-reg)
       (move x y))
      (control-stack
       (storew x fp (tn-offset y))))))
(define-move-vop move-arg :move-arg
  (any-reg descriptor-reg)
  (any-reg descriptor-reg))



;;;; ILLEGAL-MOVE

;;; This VOP exists just to begin the lifetime of a TN that couldn't
;;; be written legally due to a type error.  An error is signalled
;;; before this VOP is so we don't need to do anything (not that there
;;; would be anything sensible to do anyway.)
(define-vop (illegal-move)
  (:args (x) (type))
  (:results (y))
  (:ignore y)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 666
    (error-call vop object-not-type-error x type)))

;;;; Moves and coercions:

;;; These MOVE-TO-WORD VOPs move a tagged integer to a raw full-word
;;; representation.  Similarly, the MOVE-FROM-WORD VOPs converts a raw integer
;;; to a tagged bignum or fixnum.

;;; ARG is a fixnum, so just shift it.  We need a type restriction
;;; because some possible arg SCs (control-stack) overlap with
;;; possible bignum arg SCs.
(define-vop (move-to-word/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:arg-types tagged-num)
  (:note "fixnum untagging")
  (:generator 1
    (inst sra x 2 y)))
(define-move-vop move-to-word/fixnum :move
  (any-reg descriptor-reg) (signed-reg unsigned-reg))

;;; ARG is a non-immediate constant, load it.
(define-vop (move-to-word-c)
  (:args (x :scs (constant)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "constant load")
  (:generator 1
    (inst li (tn-value x) y)))
(define-move-vop move-to-word-c :move
  (constant) (signed-reg unsigned-reg))

;;; ARG is a fixnum or bignum, figure out which and load if necessary.
(define-vop (move-to-word/integer)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "integer to untagged word coercion")
  (:generator 3
    (inst extru x 31 2 zero-tn :<>)
    (inst sra x 2 y :tr)
    (loadw y x bignum-digits-offset other-pointer-lowtag)))
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
    (inst sll x 2 y)))
(define-move-vop move-from-word/fixnum :move
  (signed-reg unsigned-reg) (any-reg descriptor-reg))

;;; RESULT may be a bignum, so we have to check.  Use a worst-case
;;; cost to make sure people know they may be number consing.
(define-vop (move-from-signed)
  (:args (x :scs (signed-reg unsigned-reg) :to (:eval 1)))
  (:results (y :scs (any-reg descriptor-reg) :from (:eval 0)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:note "signed word to integer coercion")
  (:generator 18
    ;; Extract the top three bits.
    (inst extrs x 2 3 temp :=)
    ;; Invert them (unless they are already zero).
    (inst uaddcm zero-tn temp temp)
    ;; If we are left with zero, it will fit in a fixnum.  So branch around
    ;; the bignum-construction, doing the shift in the delay slot.
    (inst comb := temp zero-tn done)
    (inst sll x 2 y)
    ;; Make a single-digit bignum.
    (with-fixed-allocation (y temp bignum-widetag (1+ bignum-digits-offset))
      (storew x y bignum-digits-offset other-pointer-lowtag))
    DONE))
(define-move-vop move-from-signed :move
  (signed-reg) (descriptor-reg))

;;; Check for fixnum, and possibly allocate one or two word bignum
;;; result.  Use a worst-case cost to make sure people know they may
;;; be number consing.
(define-vop (move-from-unsigned)
  (:args (x :scs (signed-reg unsigned-reg) :to (:eval 1)))
  (:results (y :scs (any-reg descriptor-reg) :from (:eval 0)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:note "unsigned word to integer coercion")
  (:generator 20
    ;; Grab the top three bits.
    (inst extrs x 2 3 temp)
    ;; If zero, it will fit as a fixnum.
    (inst comib := 0 temp done)
    (inst sll x 2 y)
    ;; Make a bignum.
    (pseudo-atomic (:extra (pad-data-block (1+ bignum-digits-offset)))
      ;; Create the result pointer.
      (inst move alloc-tn y)
      (inst dep other-pointer-lowtag 31 3 y)
      ;; Check the high bit, and skip the next instruction if it's 0.
      (inst comclr x zero-tn zero-tn :>=)
      ;; The high bit is set, so allocate enough space for a two-word bignum.
      ;; We always skip the following instruction, so it is only executed
      ;; when we want one word.
      (inst addi (pad-data-block 1) alloc-tn alloc-tn :tr)
      ;; Set up the header for one word.  Use ADDI instead of LI so we can
      ;; skip the next instruction.
      (inst addi (logior (ash 1 n-widetag-bits) bignum-widetag) zero-tn temp :tr)
      ;; Set up the header for two words.
      (inst li (logior (ash 2 n-widetag-bits) bignum-widetag) temp)
      ;; Store the header and the data.
      (storew temp y 0 other-pointer-lowtag)
      (storew x y bignum-digits-offset other-pointer-lowtag))
    DONE))
(define-move-vop move-from-unsigned :move
  (unsigned-reg) (descriptor-reg))

;;; Move untagged numbers.
(define-vop (word-move)
  (:args (x :target y
            :scs (signed-reg unsigned-reg)
            :load-if (not (location= x y))))
  (:results (y :scs (signed-reg unsigned-reg)
               :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:note "word integer move")
  (:generator 0
    (move x y)))
(define-move-vop word-move :move
  (signed-reg unsigned-reg) (signed-reg unsigned-reg))

;;; Move untagged number args/return-values.
(define-vop (move-word-arg)
  (:args (x :target y
            :scs (signed-reg unsigned-reg))
         (fp :scs (any-reg)
             :load-if (not (sc-is y sap-reg))))
  (:results (y))
  (:note "word integer argument move")
  (:generator 0
    (sc-case y
      ((signed-reg unsigned-reg)
       (move x y))
      ((signed-stack unsigned-stack)
       (storew x fp (tn-offset y))))))
(define-move-vop move-word-arg :move-arg
  (descriptor-reg any-reg signed-reg unsigned-reg) (signed-reg unsigned-reg))

;;; Use standard MOVE-ARG + coercion to move an untagged number to a
;;; descriptor passing location.
(define-move-vop move-arg :move-arg
  (signed-reg unsigned-reg) (any-reg descriptor-reg))
