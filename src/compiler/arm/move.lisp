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

(defun lowest-set-bit-index (integer-value)
  (max 0 (1- (integer-length (logand integer-value (- integer-value))))))

;; FIXME: This load-immediate-word / load-negative-immediate-word
;; stuff could be more clever.  First, the structure is obnoxiously
;; repetitive.  Second, the decision on loading positive or negative
;; shouldn't depend on the sign of the value, it should depend on the
;; logcount of the two's complement representation (or maybe an even
;; smarter selection criterion).

(defun load-immediate-word (y val)
  (let ((bytespec (byte 8 (logandc1 1 (lowest-set-bit-index val)))))
    (inst mov y (mask-field bytespec val))
    (setf (ldb bytespec val) 0))
  (do ((bytespec (byte 8 (logandc1 1 (lowest-set-bit-index val)))
                 (byte 8 (logandc1 1 (lowest-set-bit-index val)))))
      ((zerop val))
    (inst orr y y (mask-field bytespec val))
    (setf (ldb bytespec val) 0)))

(defun load-negative-immediate-word (y val)
  (let ((unval (lognot val)))
    (let ((bytespec (byte 8 (logandc1 1 (lowest-set-bit-index unval)))))
      (inst mvn y (mask-field bytespec unval))
      (setf (ldb bytespec unval) 0))
    (do ((bytespec (byte 8 (logandc1 1 (lowest-set-bit-index unval)))
                   (byte 8 (logandc1 1 (lowest-set-bit-index unval)))))
        ((zerop unval))
      (inst bic y y (mask-field bytespec unval))
      (setf (ldb bytespec unval) 0))))

(define-move-fun (load-immediate 1) (vop x y)
  ((null immediate)
   (any-reg descriptor-reg))
  (let ((val (tn-value x)))
    (etypecase val
      (unsigned-byte
       ;; This is a non-negative FIXNUM, as IMMEDIATE-CONSTANT-SC only
       ;; accepts integers if they are FIXNUMs.
       (let ((tagged-value (fixnumize val)))
         ;; FIXME: There should be a way to generate more optimal code
         ;; for this (for some values, there is no more optimal code,
         ;; but for some values this will tend to be terrible).
         (inst mov y (mask-field (byte 8 2) tagged-value))
         (when (> (integer-length tagged-value) 10)
           (inst orr y y (mask-field (byte 8 10) tagged-value)))
         (when (> (integer-length tagged-value) 18)
           (inst orr y y (mask-field (byte 8 18) tagged-value)))
         (when (> (integer-length tagged-value) 26)
           (inst orr y y (mask-field (byte 8 26) tagged-value)))))
      (integer
       ;; This is a negative FIXNUM, as should be obvious from the
       ;; preceding clause.
       (let ((tagged-value (lognot (fixnumize val))))
         ;; FIXME: There should be a way to generate more optimal code
         ;; for this (for some values, there is no more optimal code,
         ;; but for some values this will tend to be terrible).
         ;; NOTE: Unlike the preceeding case, we MUST set the tag
         ;; bits, and we must do so FIRST, otherwise we end up with an
         ;; OTHER-POINTER-LOWTAG with random address bits, which won't
         ;; end well.
         (inst mvn y (mask-field (byte 8 0) tagged-value))
         (when (> (integer-length tagged-value) 8)
           (inst bic y y (mask-field (byte 8 8) tagged-value)))
         (when (> (integer-length tagged-value) 16)
           (inst bic y y (mask-field (byte 8 16) tagged-value)))
         (when (> (integer-length tagged-value) 24)
           (inst bic y y (mask-field (byte 8 24) tagged-value)))))
      (character
       (let ((codepoint (char-code val)))
         ;; FIXME: There should be a way to generate more optimal code
         ;; for this (for some values, there is no more optimal code,
         ;; but for some values this will tend to be terrible).
         (inst mov y character-widetag)
         (inst orr y y (ash 8 (ldb (byte 8 0) codepoint)))
         (when (> codepoint #x100)
           (inst orr y y (ash 16 (ldb (byte 8 8) codepoint))))
         (when (> codepoint #x10000)
           (inst orr y y (ash 24 (ldb (byte 8 16) codepoint))))))
      (null
       (move y null-tn))
      (symbol
       (load-symbol y val)))))

(define-move-fun (load-number 1) (vop x y)
  ((immediate)
   (signed-reg unsigned-reg))
  (let ((val (tn-value x)))
    (if (not (minusp val))
        (load-immediate-word y val)
        (load-negative-immediate-word y val))))

(define-move-fun (load-character 1) (vop x y)
  ((immediate) (character-reg))
  (inst mov y (char-code (tn-value x))))

(define-move-fun (load-system-area-pointer 1) (vop x y)
  ((immediate) (sap-reg))
  (let ((immediate-label (gen-label)))
    (assemble (*elsewhere*)
      (emit-label immediate-label)
      (inst word (sap-int (tn-value x))))
    (inst ldr y (@ immediate-label))))

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

;;; Make MOVE the check VOP for T so that type check generation
;;; doesn't think it is a hairy type.  This also allows checking of a
;;; few of the values in a continuation to fall out.
(primitive-type-vop move (:check) t)

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
       (storew x fp (tn-offset y))))))
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
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) error-temp)
  (:ignore y)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 666
    (error-call vop error-temp 'object-not-type-error x type)))

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
    (inst mov y (asr x n-fixnum-tag-bits))))
(define-move-vop move-to-word/fixnum :move
  (any-reg descriptor-reg) (signed-reg unsigned-reg))

;;; ARG is a non-immediate constant; load it.
(define-vop (move-to-word-c)
  (:args (x :scs (constant)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "constant load")
  (:generator 1
    (cond ((sb!c::tn-leaf x)
           (inst mov y (tn-value x)))
          (t
           (loadw y code-tn (tn-offset x) other-pointer-lowtag)
           (inst mov y (asr y n-fixnum-tag-bits))))))
(define-move-vop move-to-word-c :move
  (constant) (signed-reg unsigned-reg))

;;; ARG is a fixnum or bignum; figure out which and load if necessary.
(define-vop (move-to-word/integer)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "integer to untagged word coercion")
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 4
    (let ((done (gen-label)))
      (inst tst x fixnum-tag-mask)
      (sc-case y
        (signed-reg
         (inst mov :eq y (asr x n-fixnum-tag-bits)))
        (unsigned-reg
         (inst mov :eq y (lsr x n-fixnum-tag-bits))))

      (loadw y x bignum-digits-offset other-pointer-lowtag :ne))))
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
    (inst mov y (lsl x n-fixnum-tag-bits))))
(define-move-vop move-from-word/fixnum :move
  (signed-reg unsigned-reg) (any-reg descriptor-reg))


;;; RESULT may be a bignum, so we have to check.  Use a worst-case
;;; cost to make sure people know they may be number consing.
(define-vop (move-from-signed)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x)
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:note "signed word to integer coercion")
  (:generator 20
    (move x arg)
    (inst adds pa-flag x x)
    (inst adds :vc y pa-flag pa-flag)
    (inst b :vc DONE)

    (with-fixed-allocation (y pa-flag bignum-widetag (1+ bignum-digits-offset))
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
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x temp)
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:note "unsigned word to integer coercion")
  (:generator 20
    (move x arg)
    (inst tst x (ash (1- (ash 1 (- n-word-bits
                                   n-positive-fixnum-bits)))
                     n-positive-fixnum-bits))
    (inst mov y (lsl x n-fixnum-tag-bits))
    (inst b :eq DONE)

    (with-fixed-allocation
        (y pa-flag bignum-widetag (+ 2 bignum-digits-offset))
      ;; WITH-FIXED-ALLOCATION, when using a supplied type-code,
      ;; leaves PA-FLAG containing the computed header value.  In our
      ;; case, configured for a 2-word bignum.  If the sign bit in the
      ;; value we're boxing is CLEAR, we need to shrink the bignum by
      ;; one word, hence the following:
      (inst orrs x x 0)
      (inst sub :pl pa-flag pa-flag #x100)
      (storew temp y 0 other-pointer-lowtag :pl)
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
             :load-if (not (sc-is y sap-reg))))
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
