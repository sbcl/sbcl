;;;; the ARM VM definition of operand loading/saving and the Move VOP

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defun load-immediate-word (y val &optional single-instruction
                                            ignore-tag)
  (let (single-mov
        ffff-count
        zero-count
        (val (ldb (byte 64 0) val)))
    (flet ((single-mov ()
             (loop for i below 64 by 16
                   for part = (ldb (byte 16 i) val)
                   count (/= part #xFFFF) into ffff
                   count (plusp part) into zero
                   finally
                   (setf ffff-count ffff
                         zero-count zero
                         single-mov (or (= ffff 1)
                                        (= zero 1))))
             single-mov))
      (cond ((typep val '(unsigned-byte 16))
             (inst movz y val)
             y)
            ((typep (ldb (byte 64 0) (lognot val)) '(unsigned-byte 16))
             (inst movn y (ldb (byte 64 0) (lognot val)))
             y)
            ((encode-logical-immediate val)
             (inst orr y zr-tn val)
             y)
            ((and (typep val '(unsigned-byte 32))
                  (cond ((encode-logical-immediate val 32)
                         (inst orr (32-bit-reg y) zr-tn val)
                         t)
                        ((zerop (ldb (byte 16 0) (lognot val)))
                         (inst movn (32-bit-reg y) (ldb (byte 16 16) (lognot val)) 16)
                         t)
                        ((zerop (ldb (byte 16 16) (lognot val)))
                         (inst movn (32-bit-reg y) (ldb (byte 16 0) (lognot val)))
                         t)))

             y)
            ((and ignore-tag
                  (not (logtest fixnum-tag-mask val))
                  ;; Contiguous bits are more likely to be better
                  (logbitp n-fixnum-tag-bits val))
             (load-immediate-word y (logior val fixnum-tag-mask)
                                  single-instruction))
            ((and
              (not (single-mov))
              (not single-instruction)
              (let ((descriptorp (memq (tn-offset y) descriptor-regs)))
                (flet ((try (i part fill)
                         (let ((filled (dpb fill (byte 16 i) val)))
                           (cond ((and (encode-logical-immediate filled)
                                       (not (and descriptorp
                                                 (logtest filled fixnum-tag-mask))))
                                  (inst orr y zr-tn filled)
                                  (inst movk y part i)
                                  t)))))
                  (loop for i below 64 by 16
                        for part = (ldb (byte 16 i) val)
                        thereis (or (try i part #xFFFF)
                                    (try i part 0)
                                    (try i part
                                         (ldb (byte 16 (mod (+ i 16) 64))
                                              val)))))))
             y)
            ((and (not single-mov)
                  (not single-instruction)
                  (let ((a (ldb (byte 16 0) val))
                        (b (ldb (byte 16 16) val))
                        (c (ldb (byte 16 32) val))
                        (d (ldb (byte 16 48) val)))
                    (let ((descriptorp (memq (tn-offset y) descriptor-regs)))
                      (flet ((try (part val1 hole1 val2 hole2)
                               (let* ((whole (dpb part (byte 16 16) part))
                                      (whole (dpb whole (byte 32 32) whole)))
                                 (when (and (encode-logical-immediate whole)
                                            (not (and descriptorp
                                                      (logtest whole fixnum-tag-mask))))
                                   (inst orr y zr-tn whole)
                                   (inst movk y val1 hole1)
                                   (inst movk y val2 hole2)
                                   t))))
                        (cond ((= a b)
                               (try a c 32 d 48))
                              ((= a c)
                               (try a b 16 d 48))
                              ((= a d)
                               (try a b 16 c 32))
                              ((= b c)
                               (try b a 0 d 48))
                              ((= b d)
                               (try b a 0 c 32))
                              ((= c d)
                               (try c a 0 b 16)))))))
             y)
            ((and (not single-mov)
                  (not single-instruction)
                  (= (ldb (byte 32 0) val)
                     (ldb (byte 32 32) val))
                  (let ((a (ldb (byte 16 0) val))
                        (b (ldb (byte 16 16) val)))
                    (when (and (/= a #xFFFF 0)
                               (/= b #xFFFF 0))
                      (inst movz y a)
                      (inst movk y b 16)
                      (inst orr y y (lsl y 32)))))
             y)
            ((and (< ffff-count zero-count)
                  (or single-mov
                      (not single-instruction)))
             (loop with first = t
                   for i below 64 by 16
                   for part = (ldb (byte 16 i) val)
                   unless (= part #xFFFF)
                   do
                   (if (shiftf first nil)
                       (inst movn y (ldb (byte 16 0) (lognot part)) i)
                       (inst movk y part i)))
             y)
            ((or single-mov
                 (not single-instruction))
             (loop with first = t
                   for i below 64 by 16
                   for part = (ldb (byte 16 i) val)
                   when (plusp part)
                   do
                   (if (shiftf first nil)
                       (inst movz y part i)
                       (inst movk y part i)))
             y)))))

(defun add-sub-immediate (x &optional (temp tmp-tn))
  (cond ((not (integerp x))
         x)
        ((add-sub-immediate-p x)
         x)
        (t
         (load-immediate-word temp x))))

(defun ccmp-immediate (x &optional (temp tmp-tn))
  (cond ((not (integerp x))
         x)
        ((typep x '(unsigned-byte 5))
         x)
        (t
         (load-immediate-word temp x))))

(define-move-fun (load-immediate 1) (vop x y)
  ((immediate)
   (any-reg descriptor-reg))
  (let ((val (tn-value x)))
    (etypecase val
      (integer
       ;; This is a FIXNUM, as IMMEDIATE-CONSTANT-SC only
       ;; accepts integers if they are FIXNUMs.
       (load-immediate-word y (fixnumize val)))
      (character
       (let* ((codepoint (char-code val))
              (tagged (dpb codepoint (byte 24 8) character-widetag)))
         (load-immediate-word y tagged)))
      (single-float
       (let* ((bits (single-float-bits val))
              (tagged (dpb bits (byte 32 32) single-float-widetag)))
         (load-immediate-word y tagged)))
      (symbol
       (load-symbol y val))
      (structure-object
       (if (eq val sb-lockless:+tail+)
           (inst add y null-tn (- lockfree-list-tail-value-offset
                                  nil-value-offset))
           (bug "immediate structure-object ~S" val))))))

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
    (assemble (:elsewhere)
      (emit-label immediate-label)
      (inst dword (sap-int (tn-value x))))
    (inst ldr y (@ immediate-label))))

(define-move-fun (load-constant 5) (vop x y)
  ((constant) (descriptor-reg))
  (inst load-constant y (tn-byte-offset x)))

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
            :scs (any-reg descriptor-reg zero)
            :load-if (not (location= x y))))
  (:results (y :scs (any-reg descriptor-reg control-stack)
               :load-if (not (location= x y))))
  (:generator 0
    (cond ((location= x y))
          ((sc-is y control-stack)
           (store-stack-tn y x))
          ((and (sc-is x any-reg)
                (eql (tn-offset x) zr-offset))
           (inst mov y 0))
          (t
           (move y x)))))

(define-move-vop move :move
  (any-reg descriptor-reg)
  (any-reg descriptor-reg))

;;; The MOVE-ARG VOP is used for moving descriptor values into another
;;; frame for argument or known value passing.
(define-vop (move-arg)
  (:args (x :target y
            :scs (any-reg descriptor-reg zero))
         (fp :scs (any-reg)
             :load-if (not (sc-is y any-reg descriptor-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      ((any-reg descriptor-reg)
       (if (and (sc-is x any-reg)
                (eql (tn-offset x) zr-offset))
           (inst mov y 0)
           (move y x)))
      (control-stack
       (store-stack-offset x fp y)))))
;;;
(define-move-vop move-arg :move-arg
  (any-reg descriptor-reg)
  (any-reg descriptor-reg))

;;; Use LDP/STP when possible
(defun load-store-two-words (vop1 vop2)
  (let ((register-sb (sb-or-lose 'sb-vm::registers))
        used-load-tn)
    (labels ((register-p (tn)
               (and (tn-p tn)
                    (eq (sc-sb (tn-sc tn)) register-sb)))
             (stack-p (tn)
               (and (tn-p tn)
                    (sc-is tn control-stack)))
             (source (vop)
               (tn-ref-tn (vop-args  vop)))
             (dest (vop)
               (tn-ref-tn (vop-results vop)))
             (load-tn (vop)
               (tn-ref-load-tn (vop-args vop)))
             (suitable-offsets-p (tn1 tn2)
               (and (= (abs (- (tn-offset tn1)
                               (tn-offset tn2)))
                       1)
                    (ldp-stp-offset-p (* (min (tn-offset tn1)
                                              (tn-offset tn2))
                                         n-word-bytes)
                                      n-word-bits)))
             (load-arg (x load-tn)
               (sc-case x
                 ((constant immediate control-stack)
                  (let ((load-tn (cond ((not (and used-load-tn
                                                  (location= used-load-tn load-tn)))
                                        load-tn)
                                       ((sb-c::tn-reads load-tn)
                                        (return-from load-arg))
                                       (t
                                        tmp-tn))))
                    (setf used-load-tn load-tn)
                    (sc-case x
                      (constant
                       (when (eq load-tn tmp-tn)
                         ;; TMP-TN is not a descriptor
                         (return-from load-arg))
                       (lambda ()
                         (load-constant vop1 x load-tn)
                         load-tn))
                      (control-stack
                       (when (eq load-tn tmp-tn)
                         (return-from load-arg))
                       (lambda ()
                         (load-stack vop1 x load-tn)
                         load-tn))
                      (immediate
                       (cond ((eql (tn-value x) 0)
                              (setf used-load-tn nil)
                              (lambda () zr-tn))
                             (t
                              (lambda ()
                                (load-immediate vop1 x load-tn)
                                load-tn)))))))
                 (t
                  (setf used-load-tn x)
                  (lambda () x))))
             (do-moves (source1 source2 dest1 dest2 &optional (fp cfp-tn)
                                                              fp-load-tn)
               (cond ((and (stack-p dest1)
                           (stack-p dest2)
                           (not (location= dest1 source1))
                           (not (location= dest2 source2))
                           (or (not (eq fp cfp-tn))
                               (and (not (location= dest1 source2))
                                    (not (location= dest2 source1))))
                           (suitable-offsets-p dest1 dest2))
                      ;; Load the source registers
                      (let (new-source1 new-source2)
                        (if (and (stack-p source1)
                                 (stack-p source2)
                                 ;; Can load using LDP
                                 (do-moves source1 source2
                                   (setf new-source1 (load-tn vop1))
                                   (setf new-source2
                                         (cond ((not (location= (load-tn vop1) (load-tn vop2)))
                                                (load-tn vop2))
                                               ((sc-is (load-tn vop2) descriptor-reg)
                                                (return-from do-moves))
                                               (t
                                                tmp-tn)))))
                            (setf source1 new-source1
                                  source2 new-source2)
                            ;; Load one by one
                            (let ((load1 (load-arg source1 (load-tn vop1)))
                                  (load2 (load-arg source2 (load-tn vop2))))
                              (unless (and load1 load2)
                                (return-from do-moves))
                              (setf source1 (funcall load1)
                                    source2 (funcall load2)))))
                      (when (> (tn-offset dest1)
                               (tn-offset dest2))
                        (rotatef dest1 dest2)
                        (rotatef source1 source2))
                      (when fp-load-tn
                        (load-stack-tn fp-load-tn fp)
                        (setf fp fp-load-tn))
                      (inst stp source1 source2
                            (@ fp (tn-byte-offset dest1)))
                      t)
                     ((and (stack-p source1)
                           (stack-p source2)
                           (register-p dest1)
                           (register-p dest2)
                           (not (location= dest1 dest2))
                           (suitable-offsets-p source1 source2))
                      (when (> (tn-offset source1)
                               (tn-offset source2))
                        (rotatef dest1 dest2)
                        (rotatef source1 source2))
                      (inst ldp dest1 dest2
                            (@ fp (tn-byte-offset source1)))
                      t))))
      (case (sb-c::vop-name vop1)
        (move
         (do-moves (source vop1) (source vop2) (dest vop1) (dest vop2)))
        (sb-c::move-operand
         (cond ((and (equal (sb-c::vop-codegen-info vop1)
                            (sb-c::vop-codegen-info vop2))
                     (memq (car (sb-c::vop-codegen-info vop1))
                           '(load-stack store-stack)))
                (do-moves (source vop1) (source vop2) (dest vop1) (dest vop2)))))
        (move-arg
         (let ((fp1 (tn-ref-tn (tn-ref-across (vop-args vop1))))
               (fp2 (tn-ref-tn (tn-ref-across (vop-args vop2))))
               (dest1 (dest vop1))
               (dest2 (dest vop2)))
           (when (eq fp1 fp2)
             (do-moves (source vop1) (source vop2) (dest vop1) (dest vop2)
               (if (and (stack-p dest1)
                        (stack-p dest2))
                   fp1
                   cfp-tn)
               (tn-ref-load-tn (tn-ref-across (vop-args vop1)))))))))))


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
    (cond ((sb-c::tn-leaf x)
           (load-immediate-word y (tn-value x)))
          (t
           (load-constant vop x y)
           (inst asr y y n-fixnum-tag-bits)))))
(define-move-vop move-to-word-c :move
  (constant) (signed-reg unsigned-reg))

;;; ARG is a fixnum or bignum; figure out which and load if necessary.
(define-vop (move-to-word/integer)
  (:args (x :scs (descriptor-reg)))
  (:result-refs results)
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "integer to untagged word coercion")
  (:generator 4
    #.(assert (= fixnum-tag-mask 1))
    (when (types-equal-or-intersect (tn-ref-type results) (specifier-type 'fixnum))
      (sc-case y
        (signed-reg
         (inst asr y x n-fixnum-tag-bits))
        (unsigned-reg
         (inst lsr y x n-fixnum-tag-bits)))
      (inst tbz x 0 DONE))
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
  (:args (x :scs (signed-reg unsigned-reg) :to :result))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:sc non-descriptor-reg :offset lr-offset) lr)
  (:note "signed word to integer coercion")
  (:generator 20
    (inst adds y x x)
    (inst b :vc DONE)
    (with-fixed-allocation (y lr bignum-widetag (1+ bignum-digits-offset)
                            :store-type-code nil)
      ;; TMP-TN has the untagged address coming from ALLOCATION
      ;; that way STP can be used on an aligned address.
      ;; LR has the widetag computed by WITH-FIXED-ALLOCATION
      (storew-pair lr 0 x bignum-digits-offset tmp-tn))
    DONE))
(define-move-vop move-from-signed :move
  (signed-reg) (descriptor-reg))

(define-vop (move-from-fixnum+1)
  (:args (x :scs (signed-reg unsigned-reg)))
  (:results (y :scs (any-reg descriptor-reg)))
  (:vop-var vop)
  (:generator 4
    (inst adds y x x)
    (inst b :vc DONE)
    (load-constant vop (emit-constant (1+ most-positive-fixnum))
                   y)
    DONE))

(define-vop (move-from-fixnum-1 move-from-fixnum+1)
  (:generator 4
    (inst adds y x x)
    (inst b :vc DONE)
    (load-constant vop (emit-constant (1- most-negative-fixnum))
                   y)
    DONE))

;;; Check for fixnum, and possibly allocate one or two word bignum
;;; result.  Use a worst-case cost to make sure people know they may
;;; be number consing.
(define-vop (move-from-unsigned)
  (:args (x :scs (signed-reg unsigned-reg) :to :save))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:sc non-descriptor-reg :offset lr-offset) lr)
  (:note "unsigned word to integer coercion")
  (:generator 20
    (inst tst x (ash (1- (ash 1 (- n-word-bits
                                   n-positive-fixnum-bits)))
                     n-positive-fixnum-bits))
    (inst lsl y x n-fixnum-tag-bits)
    (inst b :eq DONE)

    (with-fixed-allocation
        (y lr bignum-widetag (+ 2 bignum-digits-offset)
         :store-type-code nil)
      ;; WITH-FIXED-ALLOCATION, when using a supplied type-code,
      ;; leaves LR containing the computed header value.  In our
      ;; case, configured for a 2-word bignum.  If the sign bit in the
      ;; value we're boxing is CLEAR, we need to shrink the bignum by
      ;; one word, hence the following:
      (inst tbnz x (1- n-word-bits) STORE)
      (load-immediate-word lr (bignum-header-for-length 1))
      STORE
      ;; See the comment in move-from-signed
      (storew-pair lr 0 x bignum-digits-offset tmp-tn))
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

(define-vop (move-conditional-result)
  (:results (res :scs (descriptor-reg)))
  (:info true)
  (:generator 1
    (move res null-tn)
    (inst b done)
    (emit-label true)
    (load-symbol res t)
    done))
