;;;; the x86-64 VM definition of operand loading/saving and the MOVE vop

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defun zeroize (tn)
  (if (stack-tn-p tn)
      (inst mov tn 0)
      (inst xor :dword tn tn)))

(define-move-fun (load-immediate 1) (vop x y)
  ((immediate)
   (any-reg descriptor-reg))
  (move-immediate y (immediate-tn-repr x)))

(define-move-fun (load-number 1) (vop x y)
  ((immediate) (signed-reg unsigned-reg))
  (let ((val (tn-value x)))
    (if (zerop val)
        (zeroize y)
        (inst mov y val))))

(define-move-fun (load-character 1) (vop x y)
  ((immediate) (character-reg))
  (inst mov y (char-code (tn-value x))))

(define-move-fun (load-system-area-pointer 1) (vop x y)
  ((immediate) (sap-reg))
  (inst mov y (sap-int (tn-value x))))

(define-move-fun (load-constant 5) (vop x y)
  ((constant) (descriptor-reg any-reg))
  (inst mov y x))

(define-move-fun (load-stack 5) (vop x y)
  ((control-stack) (any-reg descriptor-reg)
   (character-stack) (character-reg)
   (sap-stack) (sap-reg)
   (signed-stack) (signed-reg)
   (unsigned-stack) (unsigned-reg))
  (inst mov y x))

(define-move-fun (store-stack 5) (vop x y)
  ((any-reg descriptor-reg) (control-stack)
   (character-reg) (character-stack)
   (sap-reg) (sap-stack)
   (signed-reg) (signed-stack)
   (unsigned-reg) (unsigned-stack))
  (inst mov y x))

;;;; the MOVE VOP
(defun is-operand-immediate (val)
  (typep val '(or (signed-byte 32) #+(or immobile-space permgen) fixup)))
(defun cpu-imm/reg-tn-p (tn)
  (let ((repr (immediate-tn-repr tn)))
    (or (eq repr null-tn) (is-operand-immediate repr))))
(defun reg-or-legal-imm32-p (tn)
  (cond ((sc-is tn any-reg descriptor-reg) t)
        ((sc-is tn immediate) (cpu-imm/reg-tn-p tn))))

(define-vop (move)
  (:args (x :scs (any-reg descriptor-reg immediate) :target y
            :load-if (not (location= x y))))
  (:results (y :scs (any-reg descriptor-reg control-stack)
               :load-if
               (not (location= x y))))
  (:temporary (:sc any-reg :from (:argument 0) :to (:result 0)
               :unused-if (or (not (sc-is x immediate)) (cpu-imm/reg-tn-p x)))
              temp)
  (:generator 0
    (if (sc-is x immediate)
        (move-immediate y (immediate-tn-repr x) temp)
        (move y x))))

(define-move-vop move :move
  (any-reg descriptor-reg immediate)
  (any-reg descriptor-reg))

(defun move-immediate (target val &optional tmp-tn zeroed)
  (when (nil-relative-p val)
    (let ((dst (if (gpr-tn-p target) target tmp-tn)))
      (inst lea dst (ea (nil-relative-disp val) null-tn))
      (when (eq dst tmp-tn) (inst mov target tmp-tn)))
    (return-from move-immediate target))
  (when (eq val null-tn)
    (inst mov target val)
    (return-from move-immediate target))
  ;; Try to emit the smallest immediate operand if the destination word
  ;; is already zeroed. Otherwise a :qword.
  (cond
    ;; If target is a register, we can just mov it there directly
    ((gpr-tn-p target)
     ;; val can be a fixup for an immobile-space symbol, i.e. not a number,
     ;; hence not acceptable to ZEROP.
     (cond ((and (numberp val) (zerop val)) (zeroize target))
           (t (inst mov target val))))
    ;; Likewise if the value is small enough.
    ((typep val '(or (signed-byte 32) #+(or immobile-space permgen) fixup))
     ;; This logic is similar to that of STOREW*.
     ;; It would be nice to pull it all together in one place.
     ;; The basic idea is that storing any byte-aligned 8-bit value
     ;; should be a single byte write, etc.
     (let ((operand-size
            (or (and zeroed
                     (ea-p target)
                     (typecase val
                       ((unsigned-byte 8) :byte)
                       ((unsigned-byte 16) :word)
                       ;; fixups can be :dword size because we don't reference
                       ;; objects that require a 64-bit address as immediate operands.
                       ;; signed-32 is no good, as it needs sign-extension.
                       ((or (unsigned-byte 32) fixup)
                        :dword)))
                :qword)))
       (inst mov operand-size target val)))
    ;; Otherwise go through the temporary register
    (tmp-tn
     (inst mov tmp-tn val)
     (inst mov target tmp-tn))
    (t
     (error "~A is not a register, no temporary given, and immediate ~A too large" target val))))

;;; The MOVE-ARG VOP is used for moving descriptor values into
;;; another frame for argument or known value passing.
;;;
;;; Note: It is not going to be possible to move a constant directly
;;; to another frame, except if the destination is a register and in
;;; this case the loading works out.
(define-vop (move-arg)
  (:args (x :scs (any-reg descriptor-reg immediate) :target y
            :load-if (not (and (sc-is y any-reg descriptor-reg)
                               (sc-is x control-stack))))
         (fp :scs (any-reg)
             :load-if (not (sc-is y any-reg descriptor-reg))))
  (:results (y))
  (:temporary (:sc unsigned-reg
               :unused-if (or (not (sc-is x immediate)) (cpu-imm/reg-tn-p x)))
              val-temp) ; for oversized immediate operand
  (:generator 0
    (let ((val (encode-value-if-immediate x)))
      (sc-case y
        ((any-reg descriptor-reg)
         (if (sc-is x immediate)
             (if (eql val 0) (zeroize y) (move-immediate y val))
             (move y x)))
        ((control-stack)
         (if (= (tn-offset fp) rsp-offset)
             ;; C-call
             (storew val fp (tn-offset y) 0 val-temp)
             ;; Lisp stack
             (storew val fp (frame-word-offset (tn-offset y)) 0 val-temp)))))))

(define-move-vop move-arg :move-arg
  (any-reg descriptor-reg)
  (any-reg descriptor-reg))

;;;; moves and coercions

;;; These MOVE-TO-WORD VOPs move a tagged integer to a raw full-word
;;; representation. Similarly, the MOVE-FROM-WORD VOPs converts a raw
;;; integer to a tagged bignum or fixnum.

;;; Arg is a fixnum, so just shift it. We need a type restriction
;;; because some possible arg SCs (control-stack) overlap with
;;; possible bignum arg SCs.
(define-vop (move-to-word/fixnum)
  (:args (x :scs (any-reg descriptor-reg) :target y
            :load-if (not (location= x y))))
  (:results (y :scs (signed-reg unsigned-reg)
               :load-if (not (location= x y))))
  (:arg-types tagged-num)
  (:note "fixnum untagging")
  (:generator 1
    (move y x)
    (inst sar y n-fixnum-tag-bits)))
(define-move-vop move-to-word/fixnum :move
  (any-reg descriptor-reg) (signed-reg unsigned-reg))

;;; Arg is a non-immediate constant, load it.
(define-vop (move-to-word-c)
  (:args (x :scs (constant)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "constant load")
  (:generator 1
    (cond ((sb-c::tn-leaf x)
           ;; some bad value might sneak in due to imperfect type
           ;; propagation, but it shouldn't be reachable, just mask it
           ;; off.
           (inst mov y (ldb (byte 64 0) (tn-value x))))
          (t
           (inst mov y x)
           (inst sar y n-fixnum-tag-bits)))))
(define-move-vop move-to-word-c :move
  (constant) (signed-reg unsigned-reg))


;;; Arg is a fixnum or bignum, figure out which and load if necessary.
#-#.(cl:if (cl:= sb-vm:n-fixnum-tag-bits 1) '(:and) '(:or))
(define-vop (move-to-word/integer)
  (:args (x :scs (descriptor-reg) :target rax))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "integer to untagged word coercion")
  ;; I'm not convinced that increasing the demand for rAX is
  ;; better than adding 1 byte to some instruction encodings.
  ;; I'll leave it alone though.
  (:temporary (:sc unsigned-reg :offset rax-offset
               :from (:argument 0) :to (:result 0) :target y) rax)
  (:generator 4
    (move rax x)
    (inst test :byte rax fixnum-tag-mask)
    (inst jmp :z FIXNUM)
    (loadw y rax bignum-digits-offset other-pointer-lowtag)
    (inst jmp DONE)
    FIXNUM
    (inst sar rax n-fixnum-tag-bits)
    (move y rax)
    DONE))

#+#.(cl:if (cl:= sb-vm:n-fixnum-tag-bits 1) '(:and) '(:or))
(define-vop (move-to-word/integer)
  (:args (x :scs (descriptor-reg) :target y))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:result-refs results)
  (:note "integer to untagged word coercion")
  (:temporary (:sc unsigned-reg) backup)
  (:generator 4
    (cond ((types-equal-or-intersect (tn-ref-type results) (specifier-type 'fixnum))
           (move y x)
           (if (location= x y)
               ;; It would be great if a principled way existed to advise GC of
               ;; algebraic transforms such as 2*R being a conservative root.
               ;; Until that is possible, emit straightforward code that uses
               ;; a copy of the potential reference.
               (move backup x)
               (setf backup x))
           (inst sar y 1)        ; optimistically assume it's a fixnum
           (inst jmp :nc DONE))  ; no carry implies tag was 0
          (t
           (setf backup x)))
    (loadw y backup bignum-digits-offset other-pointer-lowtag)
    DONE))

(define-move-vop move-to-word/integer :move
  (descriptor-reg) (signed-reg unsigned-reg))

;;; Result is a fixnum, so we can just shift. We need the result type
;;; restriction because of the control-stack ambiguity noted above.
(define-vop (move-from-word/fixnum)
  (:args (x :scs (signed-reg unsigned-reg) :target y
            :load-if (not (location= x y))))
  (:results (y :scs (any-reg descriptor-reg)
               :load-if (not (location= x y))))
  (:result-refs result-ref)
  (:result-types tagged-num)
  (:note "fixnum tagging")
  (:generator 1
    (let ((opsize (if (and (eq n-fixnum-tag-bits 1)
                           (csubtypep (tn-ref-type result-ref)
                                      (specifier-type '(unsigned-byte 31))))
                      :dword :qword)))
      (cond ((and (sc-is x signed-reg unsigned-reg)
                  (not (location= x y)))
             (if (= n-fixnum-tag-bits 1)
                 (inst lea opsize y (ea x x))
                 (inst lea y (ea nil x (ash 1 n-fixnum-tag-bits)))))
            (t
             ;; Uses: If x is a reg 2 + 3; if x = y uses only 3 bytes
             (move y x opsize)
             (inst shl opsize y n-fixnum-tag-bits))))))
(define-move-vop move-from-word/fixnum :move
  (signed-reg unsigned-reg) (any-reg descriptor-reg))

;;; Convert an untagged signed word to a lispobj -- fixnum or bignum
;;; as the case may be. Fixnum case inline, bignum case in an assembly
;;; routine.
(define-vop (move-from-signed)
  (:args (x :scs (signed-reg unsigned-reg) :to :result . #.(and (= 1 n-fixnum-tag-bits)
                                                                '(:target y))))
  (:results (y :scs (any-reg descriptor-reg) . #.(and (> n-fixnum-tag-bits 1)
                                                      '(:from :argument))))
  (:note "signed word to integer coercion")
  (:vop-var vop)
  ;; Worst case cost to make sure people know they may be number consing.
  (:generator 20
     (cond ((= 1 n-fixnum-tag-bits)
            (move y x)
            (inst shl y 1)
            (inst jmp :no DONE)
            (if (location= y x)
                (inst rcr y 1) ; we're about to cons a bignum. this RCR is noise
                (inst mov y x)))
           (t
            (aver (not (location= x y)))
            (inst imul y x #.(ash 1 n-fixnum-tag-bits))
            (inst jmp :no DONE)
            (inst mov y x)))
     (signed=>bignum-in-reg vop y)
     DONE))
(define-move-vop move-from-signed :move
  (signed-reg) (descriptor-reg))

(define-vop (move-from-fixnum+1)
  (:args (x :scs (signed-reg unsigned-reg)))
  (:results (y :scs (any-reg descriptor-reg)))
  (:arg-refs arg-ref)
  (:vop-var vop)
  (:generator 4
    (let ((const (case (vop-name vop)
                  (move-from-fixnum-1 (1- most-negative-fixnum))
                  (t                  (1+ most-positive-fixnum)))))
      (cond ((= n-fixnum-tag-bits 1)
             (cond ((csubtypep (tn-ref-type arg-ref) (specifier-type 'fixnum))
                    ;; I think MAYBE-MOVE-FROM-FIXNUM+-1 could select MOVE-FROM-WORD/FIXNUM
                    ;; if Y is known to fit in a fixnum, but this works, albeit redundant.
                    (if (location= x y) (inst shl y 1) (inst lea y (ea x x))))
                   (t
                    (move y x)
                    (inst shl y 1)
                    (inst cmov :o y (emit-constant const)))))
            (t
             ;; not worth optimizing into SHL. The processor doesn't set OF for
             ;; for shift count > 1 so we'd have to detect overflow differently.
             (inst imul y x (ash 1 n-fixnum-tag-bits))
             (inst jmp :no DONE)
             (move y (emit-constant const)))))
    DONE))

(define-vop (move-from-fixnum-1 move-from-fixnum+1))

;;; Convert an untagged unsigned word to a lispobj -- fixnum or bignum
;;; as the case may be. Fixnum case inline, bignum case in an assembly
;;; routine.
(define-vop (move-from-unsigned)
  ;; This sure seems wonky - how would this be selected for a signed arg?
  (:args (arg :scs (signed-reg unsigned-reg signed-stack unsigned-stack)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:note "unsigned word to integer coercion")
  (:vop-var vop)
  ;; Worst case cost to make sure people know they may be number consing.
  (:generator 20
    (move res arg)
    ;; the number of high bits that need to be zero is the number of bits of
    ;; precision lost due to fixnum tag, plus the sign bit.
    ;; rotate those into the least significant bits.
    (inst rol res (1+ n-fixnum-tag-bits))
    (inst test :byte res (1- (ash 1 (1+ n-fixnum-tag-bits))))
    ;; this could make use of the SHRX instruction which is only available with BMI2
    ;; support- speculatively restore the fixnum value, test previous flags and jump,
    ;; which would eliminate the 'jmp done' that jumps over the following SHR.
    (inst jmp :z fixnum)
    (unsigned=>bignum-in-reg vop res)
    (inst jmp done)
    FIXNUM
    (inst shr res 1)
    DONE))
(define-move-vop move-from-unsigned :move
  (unsigned-reg) (descriptor-reg))

;;; Move untagged numbers.
(define-vop (word-move)
  (:args (x :scs (signed-reg unsigned-reg) :target y
            :load-if (not (location= x y))))
  (:results (y :scs (signed-reg unsigned-reg)
               :load-if
               (not (or (location= x y)
                        (and (sc-is x signed-reg unsigned-reg)
                             (sc-is y signed-stack unsigned-stack))))))
  (:note "word integer move")
  (:generator 0
    (move y x)))
(define-move-vop word-move :move
  (signed-reg unsigned-reg) (signed-reg unsigned-reg))

;;; Move untagged number arguments/return-values.
(define-vop (move-word-arg)
  (:args (x :scs (signed-reg unsigned-reg) :target y)
         (fp :scs (any-reg)
             :load-if (not (sc-is y signed-reg unsigned-reg))))
  (:results (y))
  (:note "word integer argument move")
  (:generator 0
    (sc-case y
      ((signed-reg unsigned-reg)
       (move y x))
      ((signed-stack unsigned-stack)
       (if (= (tn-offset fp) rsp-offset)
           (storew x fp (tn-offset y))  ; c-call
           (storew x fp (frame-word-offset (tn-offset y))))))))
(define-move-vop move-word-arg :move-arg
  (descriptor-reg any-reg signed-reg unsigned-reg) (signed-reg unsigned-reg))

;;; Use standard MOVE-ARG and coercion to move an untagged number
;;; to a descriptor passing location.
(define-move-vop move-arg :move-arg
  (signed-reg unsigned-reg) (any-reg descriptor-reg))
