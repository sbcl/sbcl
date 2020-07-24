;;;; the x86 VM definition of operand loading/saving and the MOVE vop

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
  ((immediate)
   (any-reg descriptor-reg))
  (let ((val (encode-value-if-immediate x)))
    (if (zerop val)
        (inst xor y y)
        (inst mov y val))))

(define-move-fun (load-number 1) (vop x y)
  ((immediate) (signed-reg unsigned-reg))
  (let ((val (tn-value x)))
    (if (zerop val)
        (inst xor y y)
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
(define-vop (move)
  (:args (x :scs (any-reg descriptor-reg immediate) :target y
            :load-if (not (location= x y))))
  (:results (y :scs (any-reg descriptor-reg)
               :load-if
               (not (or (location= x y)
                        (and (sc-is x any-reg descriptor-reg immediate)
                             (sc-is y control-stack))))))
  (:generator 0
    (if (and (sc-is x immediate)
             (sc-is y any-reg descriptor-reg control-stack))
        (let ((val (encode-value-if-immediate x)))
          (if (and (zerop val) (sc-is y any-reg descriptor-reg))
              (inst xor y y)
              (inst mov y val)))
      (move y x))))

(define-move-vop move :move
  (any-reg descriptor-reg immediate)
  (any-reg descriptor-reg))

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
  (:generator 0
    (sc-case y
      ((any-reg descriptor-reg)
       (if (sc-is x immediate)
           (let ((val (encode-value-if-immediate x)))
             (if (zerop val)
                 (inst xor y y)
                 (inst mov y val)))
         (move y x)))
      ((control-stack)
       (let ((frame-offset (if (= (tn-offset fp) esp-offset)
                               ;; C-call
                               (tn-offset y)
                               ;; Lisp stack
                               (frame-word-offset (tn-offset y)))))
         (storew (encode-value-if-immediate x) fp frame-offset))))))

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
           (inst mov y (tn-value x)))
          (t
           (inst mov y x)
           (inst sar y n-fixnum-tag-bits)))))
(define-move-vop move-to-word-c :move
  (constant) (signed-reg unsigned-reg))


;;; Arg is a fixnum or bignum, figure out which and load if necessary.
(define-vop (move-to-word/integer)
  (:args (x :scs (descriptor-reg) :target eax))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "integer to untagged word coercion")
  (:temporary (:sc unsigned-reg :offset eax-offset
                   :from (:argument 0) :to (:result 0) :target y) eax)
  (:generator 4
    (move eax x)
    (inst test al-tn fixnum-tag-mask)
    (inst jmp :z fixnum)
    (loadw y eax bignum-digits-offset other-pointer-lowtag)
    (inst jmp done)
    FIXNUM
    (inst sar eax n-fixnum-tag-bits)
    (move y eax)
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
  (:result-types tagged-num)
  (:note "fixnum tagging")
  (:generator 1
    (cond ((and (sc-is x signed-reg unsigned-reg)
                (not (location= x y)))
           ;; Uses 7 bytes, but faster on the Pentium
           (inst lea y (make-ea :dword :index x
                                :scale (ash 1 n-fixnum-tag-bits))))
          (t
           ;; Uses: If x is a reg 2 + 3; if x = y uses only 3 bytes
           (move y x)
           (inst shl y n-fixnum-tag-bits)))))
(define-move-vop move-from-word/fixnum :move
  (signed-reg unsigned-reg) (any-reg descriptor-reg))

;;; Convert an untagged signed word to a lispobj -- fixnum or bignum
;;; as the case may be. Fixnum case inline, bignum case in an assembly
;;; routine.
(define-vop (move-from-signed)
  (:args (x :scs (signed-reg unsigned-reg) :to :result))
  (:results (y :scs (any-reg descriptor-reg) :from :argument))
  (:note "signed word to integer coercion")
  ;; Worst case cost to make sure people know they may be number consing.
  (:generator 20
     (aver (not (location= x y)))
     (let ((done (gen-label)))
       (inst imul y x (ash 1 n-fixnum-tag-bits))
       (inst jmp :no done)
       (inst mov y x)
       (inst call (make-fixup (ecase (tn-offset y)
                                (#.eax-offset 'alloc-signed-bignum-in-eax)
                                (#.ebx-offset 'alloc-signed-bignum-in-ebx)
                                (#.ecx-offset 'alloc-signed-bignum-in-ecx)
                                (#.edx-offset 'alloc-signed-bignum-in-edx)
                                (#.esi-offset 'alloc-signed-bignum-in-esi)
                                (#.edi-offset 'alloc-signed-bignum-in-edi))
                              :assembly-routine))
       (emit-label done))))
(define-move-vop move-from-signed :move
  (signed-reg) (descriptor-reg))

(define-vop (move-from-fixnum+1)
  (:args (x :scs (signed-reg unsigned-reg)))
  (:results (y :scs (any-reg descriptor-reg)))
  (:generator 4
    (inst imul y x (ash 1 n-fixnum-tag-bits))
    (inst jmp :no done)
    (inst mov y (emit-constant (1+ most-positive-fixnum)))
    done))

(define-vop (move-from-fixnum-1 move-from-fixnum+1)
  (:generator 4
    (inst imul y x (ash 1 n-fixnum-tag-bits))
    (inst jmp :no done)
    (inst mov y (emit-constant (1- most-negative-fixnum)))
    done))

;;; Convert an untagged unsigned word to a lispobj -- fixnum or bignum
;;; as the case may be. Fixnum case inline, bignum case in an assembly
;;; routine.
(define-vop (move-from-unsigned)
  (:args (x :scs (signed-reg unsigned-reg) :to :result))
  (:results (y :scs (any-reg descriptor-reg) :from :argument))
  (:note "unsigned word to integer coercion")
  ;; Worst case cost to make sure people know they may be number consing.
  (:generator 20
    (aver (not (location= x y)))
    (let ((done (gen-label)))
      ;; The assembly routines test the sign flag from this one, so if
      ;; you change stuff here, make sure the sign flag doesn't get
      ;; overwritten before the CALL!
      (inst test x #.(ash lowtag-mask n-positive-fixnum-bits))
      ;; Faster but bigger then SHL Y 2. The cost of doing this speculatively
      ;; is noise compared to bignum consing if that is needed.
      (inst lea y (make-ea :dword :index x :scale (ash 1 n-fixnum-tag-bits)))
      (inst jmp :z done)
      (inst mov y x)
      (inst call (make-fixup (ecase (tn-offset y)
                               (#.eax-offset 'alloc-unsigned-bignum-in-eax)
                               (#.ebx-offset 'alloc-unsigned-bignum-in-ebx)
                               (#.ecx-offset 'alloc-unsigned-bignum-in-ecx)
                               (#.edx-offset 'alloc-unsigned-bignum-in-edx)
                               (#.edi-offset 'alloc-unsigned-bignum-in-edi)
                               (#.esi-offset 'alloc-unsigned-bignum-in-esi))
                             :assembly-routine))
      (emit-label done))))
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
       (if (= (tn-offset fp) esp-offset)
           (storew x fp (tn-offset y))  ; c-call
           (storew x fp (frame-word-offset (tn-offset y))))))))
(define-move-vop move-word-arg :move-arg
  (descriptor-reg any-reg signed-reg unsigned-reg) (signed-reg unsigned-reg))

;;; Use standard MOVE-ARG and coercion to move an untagged number
;;; to a descriptor passing location.
(define-move-vop move-arg :move-arg
  (signed-reg unsigned-reg) (any-reg descriptor-reg))
