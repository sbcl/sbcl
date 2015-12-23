;;;; the ARM VM definition of operand loading/saving and the Move VOP

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(defun load-immediate-word (y val)
  (cond ((typep val '(unsigned-byte 16))
         (inst movz y val))
        ((typep val '(and (signed-byte 16) (integer * -1)))
         (inst movn y (lognot val)))
        ((encode-logical-immediate val)
         (inst orr y zr-tn val))
        ((minusp val)
         (loop with first = t
               for i below 64 by 16
               for part = (ldb (byte 16 i) val)
               unless (= part #xFFFF)
               do
               (if (shiftf first nil)
                   (inst movn y (ldb (byte 16 0) (lognot part)) i)
                   (inst movk y part i))))
        (t
         (loop with first = t
               for i below 64 by 16
               for part = (ldb (byte 16 i) val)
               when (plusp part)
               do
               (if (shiftf first nil)
                   (inst movz y part i)
                   (inst movk y part i)))))
  y)

(defun add-sub-immediate (x &optional (temp tmp-tn))
  (cond ((not (integerp x))
         x)
        ((add-sub-immediate-p x)
         x)
        (t
         (load-immediate-word temp x))))

(define-move-fun (load-immediate 1) (vop x y)
  ((null immediate)
   (any-reg descriptor-reg))
  (let ((val (tn-value x)))
    (etypecase val
      (integer
       ;; This is a FIXNUM, as IMMEDIATE-CONSTANT-SC only
       ;; accepts integers if they are FIXNUMs.
       (load-immediate-word y (fixnumize val)))
      (character
       (let* ((codepoint (char-code val))
              (encoded-character (dpb codepoint (byte 24 8) character-widetag)))
         (load-immediate-word y encoded-character)))
      (null
       (move y null-tn))
      (symbol
       (load-symbol y val)))))

(define-move-fun (load-number 1) (vop x y)
  ((immediate)
   (signed-reg unsigned-reg))
  (load-immediate-word y (tn-value x)))

(define-move-fun (load-character 1) (vop x y)
  ((immediate) (character-reg))
  (load-immediate-word y (char-code (tn-value x))))

(define-move-fun (load-system-area-pointer 1) (vop x y)
  ((immediate) (sap-reg))
  (let ((immediate-label (gen-label)))
    (assemble (*elsewhere*)
      (emit-label immediate-label)
      (inst dword (sap-int (tn-value x))))
    (inst ldr y (@ immediate-label))))

(define-move-fun (load-constant 5) (vop x y)
  ((constant) (descriptor-reg))
  (let ((offset (- (* (tn-offset x) n-word-bytes) other-pointer-lowtag)))
    (cond
      ((ldr-str-offset-encodable offset)
       (inst ldr y (@ code-tn offset)))
      (t
       (load-immediate-word tmp-tn offset)
       (inst ldr y (@ code-tn tmp-tn))))))

(define-move-fun (load-stack 5) (vop x y)
  ((control-stack) (any-reg descriptor-reg))
  (load-stack-tn y x))

(define-move-fun (load-number-stack 5) (vop x y)
  ((character-stack) (character-reg)
   (sap-stack) (sap-reg)
   (signed-stack) (signed-reg)
   (unsigned-stack) (unsigned-reg))
  (load-stack-offset y (current-nfp-tn vop) x))

(define-move-fun (store-stack 5) (vop x y)
  ((any-reg descriptor-reg) (control-stack))
  (store-stack-tn y x))

(define-move-fun (store-number-stack 5) (vop x y)
  ((character-reg) (character-stack)
   (sap-reg) (sap-stack)
   (signed-reg) (signed-stack)
   (unsigned-reg) (unsigned-stack))
  (store-stack-offset x (current-nfp-tn vop) y))


;;;; The Move VOP:
(define-vop (move)
  (:args (x :target y
            :scs (any-reg descriptor-reg null)
            :load-if (not (location= x y))))
  (:results (y :scs (any-reg descriptor-reg)
               :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (move y x)))

(define-move-vop move :move
  (any-reg descriptor-reg)
  (any-reg descriptor-reg))

;;; The MOVE-ARG VOP is used for moving descriptor values into another
;;; frame for argument or known value passing.
(define-vop (move-arg)
  (:args (x :target y
            :scs (any-reg descriptor-reg null))
         (fp :scs (any-reg)
             :load-if (not (sc-is y any-reg descriptor-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      ((any-reg descriptor-reg)
       (move y x))
      (control-stack
       (store-stack-offset x fp y)))))
;;;
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
    (error-call vop 'object-not-type-error x type)))

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
    (inst asr y x n-fixnum-tag-bits)))
(define-move-vop move-to-word/fixnum :move
  (any-reg descriptor-reg) (signed-reg unsigned-reg))

;;; ARG is a non-immediate constant; load it.
(define-vop (move-to-word-c)
  (:args (x :scs (constant)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:vop-var vop)
  (:note "constant load")
  (:generator 1
    (cond ((sb!c::tn-leaf x)
           (load-immediate-word y (tn-value x)))
          (t
           (load-constant vop x y)
           (inst asr y y n-fixnum-tag-bits)))))
(define-move-vop move-to-word-c :move
  (constant) (signed-reg unsigned-reg))

;;; ARG is a fixnum or bignum; figure out which and load if necessary.
(define-vop (move-to-word/integer)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "integer to untagged word coercion")
  (:generator 4
    (inst tst x fixnum-tag-mask)
    (inst b :ne BIGNUM )
    (sc-case y
      (signed-reg
       (inst asr y x n-fixnum-tag-bits))
      (unsigned-reg
       (inst lsr y x n-fixnum-tag-bits)))
    (inst b DONE)
    BIGNUM
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
     (inst lsl y x n-fixnum-tag-bits)))
(define-move-vop move-from-word/fixnum :move
  (signed-reg unsigned-reg) (any-reg descriptor-reg))


;;; RESULT may be a bignum, so we have to check.  Use a worst-case
;;; cost to make sure people know they may be number consing.
(define-vop (move-from-signed)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x)
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:temporary (:scs (interior-reg)) lip)
  (:note "signed word to integer coercion")
  (:generator 20
    (move x arg)
    (inst adds y x x)
    (inst b :vc DONE)
    (with-fixed-allocation (y pa-flag bignum-widetag (1+ bignum-digits-offset) :lip lip)
      (storew x y bignum-digits-offset other-pointer-lowtag))
    DONE))
(define-move-vop move-from-signed :move
  (signed-reg) (descriptor-reg))

;;; Check for fixnum, and possibly allocate one or two word bignum
;;; result.  Use a worst-case cost to make sure people know they may
;;; be number consing.
(define-vop (move-from-unsigned)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x)
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:temporary (:scs (interior-reg)) lip)
  (:note "unsigned word to integer coercion")
  (:generator 20
    (move x arg)
    (inst tst x (ash (1- (ash 1 (- n-word-bits
                                   n-positive-fixnum-bits)))
                     n-positive-fixnum-bits))
    (inst lsl y x n-fixnum-tag-bits)
    (inst b :eq DONE)

    (with-fixed-allocation
        (y pa-flag bignum-widetag (+ 2 bignum-digits-offset) :lip lip)
      ;; WITH-FIXED-ALLOCATION, when using a supplied type-code,
      ;; leaves PA-FLAG containing the computed header value.  In our
      ;; case, configured for a 2-word bignum.  If the sign bit in the
      ;; value we're boxing is CLEAR, we need to shrink the bignum by
      ;; one word, hence the following:
      (inst tst x x)
      (inst b :mi STORE)
      (inst sub pa-flag pa-flag #x100)
      (storew pa-flag y 0 other-pointer-lowtag)
      STORE
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
       (store-stack-offset x fp y)))))
(define-move-vop move-word-arg :move-arg
  (descriptor-reg any-reg signed-reg unsigned-reg) (signed-reg unsigned-reg))

;;; Use standard MOVE-ARG + coercion to move an untagged number to a
;;; descriptor passing location.
(define-move-vop move-arg :move-arg
  (signed-reg unsigned-reg) (any-reg descriptor-reg))
