;;;; the VM definition of arithmetic VOPs for the x86-64

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")


;; If 'plausible-signed-imm32-operand-p' is true, use it; otherwise use a RIP-relative constant.
;; I couldn't think of a more accurate name for this other than maybe
;; 'signed-immediate32-or-rip-relativize' which is just too awful.
(defun constantize (x)
  (or (plausible-signed-imm32-operand-p x)
      (register-inline-constant :qword x)))

;;;; unary operations

(define-vop (fast-safe-arith-op)
  (:policy :fast-safe))

(define-vop (fixnum-unop fast-safe-arith-op)
  (:args (x :scs (any-reg control-stack) :target res :load-if nil))
  (:results (res :scs (any-reg control-stack) :load-if nil))
  (:temporary (:sc unsigned-reg) temp)
  (:note "inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num))

(define-vop (signed-unop fast-safe-arith-op)
  (:args (x :scs (signed-reg signed-stack) :target res :load-if nil))
  (:results (res :scs (signed-reg signed-stack) :load-if nil))
  (:temporary (:sc unsigned-reg) temp)
  (:note "inline (signed-byte 64) arithmetic")
  (:arg-types signed-num)
  (:result-types signed-num))

;;; logical or arithmetic negation
(defun emit-inline-neg (op arg result temp vop &optional fixnump)
  (declare (ignore vop))
  ;; If ARG and RESULT are the same location, then the initial and final MOVEs
  ;; are both no-ops. If different locations and not both memory,
  ;; then the initial move is a physical move and the final is a no-op.
  ;; If both are stack locations, then compute the answer in temp.
  ;; (REG might be a stack location, not necessarily a GPR in this emitter.
  ;; It's just a naming convention that is consistent with other emitters)
  (let ((reg (if (or (alias-p arg result) (gpr-tn-p arg) (gpr-tn-p result))
                 result
                 temp)))
    (move reg arg)
    (case op
      (not (if fixnump (inst xor reg (fixnumize -1)) (inst not reg)))
      (neg (inst neg reg)))
    (move result reg)))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:vop-var vop)
  (:generator 1 (emit-inline-neg 'neg x res temp vop)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:vop-var vop)
  (:generator 2 (emit-inline-neg 'neg x res temp vop)))

(define-vop (fast-negate/unsigned signed-unop)
  (:args (x :scs (unsigned-reg unsigned-stack) :target res :load-if nil))
  (:arg-types unsigned-num)
  (:translate %negate)
  (:vop-var vop)
  (:generator 3 (emit-inline-neg 'neg x res temp vop)))

(define-vop (fast-negate/signed-unsigned signed-unop)
  (:results (res :scs (unsigned-reg unsigned-stack) :load-if nil))
  (:result-types unsigned-num)
  (:translate %negate)
  (:vop-var vop)
  (:generator 3 (emit-inline-neg 'neg x res temp vop)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:translate lognot)
  (:vop-var vop)
  (:generator 1 (emit-inline-neg 'not x res temp vop t)))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:vop-var vop)
  (:generator 2 (emit-inline-neg 'not x res temp vop nil)))

;;;; binary fixnum operations

;;; Assume that any constant operand is the second arg...

(define-vop (fast-fixnum-binop fast-safe-arith-op)
  (:args (x :target r :scs (any-reg)
            :load-if (not (and (sc-is x control-stack)
                               (sc-is y any-reg)
                               (sc-is r control-stack)
                               (location= x r))))
         (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg) :from (:argument 0)
               :load-if (not (and (sc-is x control-stack)
                                  (sc-is y any-reg)
                                  (sc-is r control-stack)
                                  (location= x r)))))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg)
            :load-if (not (and (sc-is x unsigned-stack)
                               (sc-is y unsigned-reg)
                               (sc-is r unsigned-stack)
                               (location= x r))))
         (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg) :from (:argument 0)
            :load-if (not (and (sc-is x unsigned-stack)
                               (sc-is y unsigned-reg)
                               (sc-is r unsigned-stack)
                               (location= x r)))))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic"))

(define-vop (fast-signed-binop fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg)
            :load-if (not (and (sc-is x signed-stack)
                               (sc-is y signed-reg)
                               (sc-is r signed-stack)
                               (location= x r))))
         (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg) :from (:argument 0)
            :load-if (not (and (sc-is x signed-stack)
                               (sc-is y signed-reg)
                               (sc-is r signed-stack)
                               (location= x r)))))
  (:result-types signed-num)
  (:note "inline (signed-byte 64) arithmetic"))

(define-vop (fast-fixnum-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg) :load-if t))
  (:info y)
  (:arg-types tagged-num (:constant fixnum))
  (:results (r :scs (any-reg) :load-if t))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg) :load-if t))
  (:info y)
  (:arg-types unsigned-num (:constant (unsigned-byte 64)))
  (:results (r :scs (unsigned-reg) :load-if t))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic"))

(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg) :load-if t))
  (:info y)
  (:arg-types signed-num (:constant (signed-byte 64)))
  (:results (r :scs (signed-reg) :load-if t))
  (:result-types signed-num)
  (:note "inline (signed-byte 64) arithmetic"))

(macrolet ((define-binop (translate untagged-penalty op
                          &key fixnum=>fixnum c/fixnum=>fixnum
                               signed=>signed c/signed=>signed
                               unsigned=>unsigned c/unsigned=>unsigned)

             `(progn
                (define-vop (,(symbolicate "FAST-" translate "/FIXNUM=>FIXNUM")
                             fast-fixnum-binop)
                  (:translate ,translate)
                  (:generator 2
                   ,@(or fixnum=>fixnum `((move r x) (inst ,op r y)))))
                (define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum)
                             fast-fixnum-binop-c)
                  (:translate ,translate)
                  (:generator 1
                   ,@(or c/fixnum=>fixnum
                         `((move r x) (inst ,op r (constantize (fixnumize y)))))))
                (define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
                             fast-signed-binop)
                  (:translate ,translate)
                  (:generator ,(1+ untagged-penalty)
                   ,@(or signed=>signed `((move r x) (inst ,op r y)))))
                (define-vop (,(symbolicate 'fast- translate '-c/signed=>signed)
                             fast-signed-binop-c)
                  (:translate ,translate)
                  (:generator ,untagged-penalty
                   ,@(or c/signed=>signed `((move r x) (inst ,op r (constantize y))))))
                (define-vop (,(symbolicate "FAST-"
                                           translate
                                           "/UNSIGNED=>UNSIGNED")
                             fast-unsigned-binop)
                  (:translate ,translate)
                  (:generator ,(1+ untagged-penalty)
                   ,@(or unsigned=>unsigned `((move r x) (inst ,op r y)))))
                (define-vop (,(symbolicate 'fast-
                                           translate
                                           '-c/unsigned=>unsigned)
                             fast-unsigned-binop-c)
                  (:translate ,translate)
                  (:generator ,untagged-penalty
                   ,@(or c/unsigned=>unsigned
                         `((move r x) (inst ,op r (constantize y)))))))))

  ;; The following have microoptimizations for some special cases
  ;; not caught by the front end.

  (define-binop logand 2 and
    :c/fixnum=>fixnum
    ;; Use :dword size if the constant is (unsigned-byte 32) and destination
    ;; is a GPR; the high 32 bits get automatically zeroed.
    ((let ((y (fixnumize y)))
       (cond ((and (typep y '(unsigned-byte 32)) (gpr-tn-p x))
              ;; A 32-bit mov suffices unless to memory.
              (unless (location= x r)
                (inst mov (if (gpr-tn-p r) :dword :qword) r x))
              ;; TODO: if a :dword MOV was done, the AND is unnecessary
              ;; when Y = (ldb (byte 32 0) (fixnumize -1 n-fixnum-tag-bits))
              ;; Probably not very common, so not too important.
              (inst and :dword r y))
           (t
            (move r x)
            (inst and r (constantize y))))))
    :c/unsigned=>unsigned
    ;; Probably should give it the preceding treatment here too.
    ;; Also, if the constant is #xFFFFFFFF, then just a MOV is enough
    ;; if the destination is a register.
    ((move r x)
     (let ((y (constantize y)))
       ;; ANDing with #xFFFF_FFFF_FFFF_FFFF is a no-op, other than
       ;; the eflags state which we don't care about.
       (unless (eql y -1) ; do nothing if this is true
         (inst and r y)))))

  (define-binop logior 2 or
    :c/unsigned=>unsigned
    ((let ((y (constantize y)))
       (cond ((and (gpr-tn-p r) (eql y -1)) ; special-case "OR reg, all-ones"
              ;; I have yet to elicit this case. Can it happen?
              (inst mov r -1))
             (t
              (move r x)
              (inst or r y))))))

  (define-binop logxor 2 xor
    :c/unsigned=>unsigned
    ((move r x)
     (let ((y (constantize y)))
       (if (eql y -1) ; special-case "XOR reg, [all-ones]"
           (inst not r)
           (inst xor r y))))))

(define-vop (fast-logior-unsigned-signed=>signed fast-safe-arith-op)
  (:args (x :scs (unsigned-reg) :to (:result 1))
         (y :target r :scs (signed-reg)))
  (:arg-types unsigned-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:translate logior)
  (:generator 3
    (move r y)
    (inst or r x)))

(define-vop (fast-logior-signed-unsigned=>signed fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg))
         (y :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:results (r :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:translate logior)
  (:generator 3
    (move r x)
    (inst or r y)))

(defun prepare-alu-operands (op x y vop const-tn-xform commutative)
  (declare (ignore op))
  (let ((arg (vop-args vop)))
    (when (tn-ref-load-tn arg)
      (bug "Shouldn't have a load TN for arg0"))
    (let ((arg (tn-ref-across arg)))
      (when (and arg (tn-ref-load-tn arg))
        (bug "Shouldn't have a load TN for arg1"))))
  (let ((res (vop-results vop)))
    (when (tn-ref-load-tn res) (bug "Shouldn't have a load TN for result")))
  ;; Immediates won't be loaded since the :LOAD-IF expression is NIL.
  ;; Such value should always be placed into Y if the operation is +.
  ;; And note that because we're forgoing the automatic arg loading,
  ;; any fixnum scaling that would have happened automatically won't.
  (when (and (tn-p x) (sc-is x immediate))
    (setq x (funcall const-tn-xform (tn-value x))))
  (when (and (tn-p y) (sc-is y immediate))
    (setq y (funcall const-tn-xform (tn-value y))))
  (when (and commutative (integerp x))
    (rotatef x y)) ; weird! why did IR1 not flip the args?
  (values x y))

(defun emit-inline-smul (op x y result temp vop dummy taggedp) ; signed multiply
  (declare (ignore op dummy))
  (multiple-value-setq (x y) (prepare-alu-operands 'mul x y vop 'identity t))
  (aver (not (integerp x)))
  (let ((constant-y (integerp y))) ; don't need to unscale Y if true
    (when (and constant-y (not (typep y '(signed-byte 32))))
      (setq y (register-inline-constant :qword y)))
    (let ((reg (if (gpr-tn-p result) result temp)))
      (cond ((integerp y)
             (inst imul reg x y))
            ((alias-p reg y)
             (when taggedp
               (inst sar reg n-fixnum-tag-bits))
             (inst imul reg x)
             ;; If all operands are LOCATION= then both args got unfixnumized
             ;; by the preceding SAR, so re-fixnumize the result.
             (when (and (alias-p reg x) taggedp)
               (inst shl reg n-fixnum-tag-bits)))
            (t
             (move reg x) ; this can't clobber Y
             (when (and (not constant-y) taggedp)
               (inst sar reg n-fixnum-tag-bits))
             (inst imul reg y)))
      (move result reg))))

;;; Special handling of add on the x86; can sometimes use lea to avoid a
;;; register move, otherwise it uses add.
;;;
;;; This should more-or-less be the general skeleton for any two-operand
;;; arithmetic vop in terms of the case-by-case analysis for where the result
;;; is going.
;;; Non-commutative operations (notably SUB) need a little extra care.
;;; MUL is also a bit different due to asymmetry of the instruction.
(defun emit-inline-add-sub (op x y result temp vop const-tn-xform)
  (declare (type (member add sub) op))
  (multiple-value-setq (x y)
    (prepare-alu-operands op x y vop const-tn-xform (eq op 'add)))

  (when (and (eq op 'sub) (and (integerp y) (not (eql y (ash -1 63)))))
    ;; If Y is -2147483648 then the negation is not (signed-byte 32).
    ;; How likely is someone to subtract that?
    (setq op 'add y (- y)))

  ;; Oversized integers need to become RIP-relative constants
  (when (integerp x) (setq x (constantize x)))
  (when (integerp y) (setq y (constantize y)))

  (let* ((y-is-reg-or-imm32 (or (gpr-tn-p y) (typep y '(signed-byte 32))))
         (commutative (eq op 'add)))
      (when (alias-p result x)
        (cond ((eql y -1) (inst dec x))
              ((eql y +1) (inst inc x))
              ((or (gpr-tn-p x) y-is-reg-or-imm32)
               ;; At most one memory operand. Result could be memory or register.
               (inst* op x y))
              (t ; two memory operands: X is not a GPR, Y is neither GPR nor immm
               (inst mov temp y)
               (inst* op x temp)))
        (return-from emit-inline-add-sub))
      (when (and (alias-p result y) commutative)
        ;; Result in the same location as Y can happen because we no longer specify
        ;; that RESULT is live from (:ARGUMENT 0).
        (cond ((or (gpr-tn-p x) (gpr-tn-p y))
               (inst* op y x))
              (t
               (inst mov temp x)
               (inst* op y temp)))
        (return-from emit-inline-add-sub))
      (let ((reg (if (and (gpr-tn-p result)
                          ;; If Y aliases RESULT in SUB, then an initial (move reg x)
                          ;; could clobber Y.
                          (or commutative (not (alias-p result y))))
                     result
                     temp)))
        (cond ((and (eq op 'add) ; LEA can't do subtraction
                    (gpr-tn-p x) y-is-reg-or-imm32) ; register + (register | imm32)
               (inst lea reg (if (fixnump y) (ea y x) (ea x y))))
              (t
               ;; If commutative, then neither X nor Y is an alias of RESULT.
               ;; If non-commutative, then RESULT could be Y, in which case REG is
               ;; TEMP so that we don't trash Y by moving X into it.
               (inst mov reg x)
               (inst* op reg y)))
        (move result reg))))

;;; FIXME: we shouldn't need 12 variants, plus the modular variants, for what should
;;; be 1 vop. Certainly + and - can be done by one vop which examines lvar-fun-name.
;;; And the "/c" (constant 2nd arg) vops can be removed since there is no extra register
;;; consumed anyway. When I tried to do those simplifications, the modular vops went
;;; haywire because they inherit from vops of particular names.
(macrolet ((def (fun-name name name/c scs primtype type cost
                          &optional (val-xform 'identity) &rest extra
                          &aux (note (format nil "inline ~(~a~) arithmetic" type))
                               (op (ecase fun-name (+ 'add) (- 'sub) (* 'mul)))
                               (emit (if (eq op 'mul) 'emit-inline-smul 'emit-inline-add-sub)))
             ;; Avoid some unnecessary code in the vop generator by specifying
             ;; ":load-if nil" everywhere. This is not about semantics -
             ;; it's just removing code that would be unreachable.
             `(progn
                (define-vop (,name fast-safe-arith-op)
                  (:translate ,fun-name)
                  (:args (x :scs (,@scs immediate) :target r :load-if nil)
                         (y :scs (,@scs immediate) :load-if nil))
                  (:arg-types ,primtype ,primtype)
                  (:results (r :scs ,scs :load-if nil))
                  (:result-types ,primtype)
                  (:temporary (:sc unsigned-reg) temp)
                  (:vop-var vop) ;; (:node-var node)
                  (:note ,note)
                  (:generator ,(1+ cost)
                   (,emit ',op x y r temp vop ',val-xform ,@extra)))
                (define-vop (,name/c fast-safe-arith-op)
                  (:translate ,fun-name)
                  (:args (x :scs ,scs :target r :load-if nil))
                  (:info y)
                  (:arg-types ,primtype (:constant ,type))
                  (:results (r :scs ,scs :load-if nil))
                  (:result-types ,primtype)
                  (:temporary (:sc unsigned-reg) temp)
                  (:vop-var vop) ;; (:node-var node)
                  (:note ,note)
                  (:generator ,cost
                   (,emit ',op x (,val-xform y) r temp vop ',val-xform ,@extra))))))
  (def + fast-+/fixnum=>fixnum fast-+-c/fixnum=>fixnum
       (any-reg control-stack) tagged-num fixnum 1 fixnumize)
  (def + fast-+/signed=>signed fast-+-c/signed=>signed
       (signed-reg signed-stack) signed-num (signed-byte 64) 3)
  (def + fast-+/unsigned=>unsigned fast-+-c/unsigned=>unsigned
       (unsigned-reg unsigned-stack) unsigned-num (unsigned-byte 64) 3)

  (def - fast--/fixnum=>fixnum fast---c/fixnum=>fixnum
       (any-reg control-stack) tagged-num fixnum 1 fixnumize)
  (def - fast--/signed=>signed fast---c/signed=>signed
       (signed-reg signed-stack) signed-num (signed-byte 64) 3)
  (def - fast--/unsigned=>unsigned fast---c/unsigned=>unsigned
       (unsigned-reg unsigned-stack) unsigned-num (unsigned-byte 64) 3)

  (def * fast-*/fixnum=>fixnum fast-*-c/fixnum=>fixnum
       (any-reg control-stack) tagged-num fixnum 1 identity t)
  (def * fast-*/signed=>signed fast-*-c/signed=>signed
       (signed-reg signed-stack) signed-num (signed-byte 64) 3 identity nil)
  ;; unsigned is different because MUL always uses RAX:RDX as 1st operand.
  )

;;;; Special logand cases: (logand signed unsigned) => unsigned

(define-vop (fast-logand/signed-unsigned=>unsigned
             fast-logand/unsigned=>unsigned)
  (:args (x :target r :scs (signed-reg)
            :load-if (not (and (sc-is x signed-stack)
                               (sc-is y unsigned-reg)
                               (sc-is r unsigned-stack)
                               (location= x r))))
         (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types signed-num unsigned-num))

;; This special case benefits from the special case for c/unsigned=>unsigned.
;; In particular, converting a (signed-byte 64) to (unsigned-byte 64) by
;; way of (LDB (byte 64 0)) doesn't need an AND instruction.
(define-vop (fast-logand-c/signed-unsigned=>unsigned
             fast-logand-c/unsigned=>unsigned)
  (:args (x :target r :scs (signed-reg)))
  (:arg-types signed-num (:constant (unsigned-byte 64))))

(define-vop (fast-logand/unsigned-signed=>unsigned
             fast-logand/unsigned=>unsigned)
  (:args (x :target r :scs (unsigned-reg)
            :load-if (not (and (sc-is x unsigned-stack)
                               (sc-is y signed-reg)
                               (sc-is r unsigned-stack)
                               (location= x r))))
         (y :scs (signed-reg signed-stack)))
  (:arg-types unsigned-num signed-num))

;;;; multiplication and division

(define-vop (fast-*/unsigned=>unsigned fast-safe-arith-op)
  (:translate *)
  (:args (x :scs (unsigned-reg) :target eax)
         (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset rax-offset :target r
                   :from (:argument 0) :to :result) eax)
  (:temporary (:sc unsigned-reg :offset rdx-offset
                   :from :eval :to :result) edx)
  (:ignore edx)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 6
    (move eax x)
    (inst mul eax y)
    (move r eax)))

(define-vop (fast-*-c/unsigned=>unsigned fast-safe-arith-op)
  (:translate *)
  (:args (x :scs (unsigned-reg) :target eax))
  (:info y)
  (:arg-types unsigned-num (:constant (unsigned-byte 64)))
  (:temporary (:sc unsigned-reg :offset rax-offset :target r
                   :from (:argument 0) :to :result) eax)
  (:temporary (:sc unsigned-reg :offset rdx-offset
                   :from :eval :to :result) edx)
  (:ignore edx)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 6
    (move eax x)
    (inst mul eax (register-inline-constant :qword y))
    (move r eax)))


(define-vop (fast-truncate/fixnum=>fixnum fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (any-reg) :target eax)
         (y :scs (any-reg control-stack)))
  (:args-var args)
  (:arg-types tagged-num tagged-num)
  (:temporary (:sc signed-reg :offset rax-offset :target quo
                   :from (:argument 0) :to (:result 0)) eax)
  (:temporary (:sc unsigned-reg :offset rdx-offset :target rem
                   :from (:argument 0) :to (:result 1)) edx)
  (:results (quo :scs (any-reg))
            (rem :scs (any-reg)))
  (:result-types tagged-num tagged-num)
  (:note "inline fixnum arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 31
    (when (types-equal-or-intersect (tn-ref-type (tn-ref-across args))
                                    (specifier-type '(eql 0)))
      (if (sc-is y signed-reg)
          (inst test y y)               ; smaller instruction
          (inst cmp y 0))
      (inst jmp :eq (generate-error-code vop 'division-by-zero-error x y)))
    (move eax x)
    (inst cqo)
    (inst idiv eax y)
    (if (location= quo eax)
        (inst shl eax n-fixnum-tag-bits)
        (if (= n-fixnum-tag-bits 1)
            (inst lea quo (ea eax eax))
            (inst lea quo (ea nil eax (ash 1 n-fixnum-tag-bits)))))
    (move rem edx)))

(define-vop (fast-truncate-c/fixnum=>fixnum fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (any-reg) :target eax))
  (:info y)
  (:arg-types tagged-num (:constant fixnum))
  (:temporary (:sc signed-reg :offset rax-offset :target quo
                   :from :argument :to (:result 0)) eax)
  (:temporary (:sc any-reg :offset rdx-offset :target rem
                   :from :eval :to (:result 1)) edx)
  (:temporary (:sc any-reg :from :eval :to :result) y-arg)
  (:results (quo :scs (any-reg))
            (rem :scs (any-reg)))
  (:result-types tagged-num tagged-num)
  (:note "inline fixnum arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 30
    (move eax x)
    (inst cqo)
    (inst mov y-arg (fixnumize y))
    (inst idiv eax y-arg)
    (if (location= quo eax)
        (inst shl eax n-fixnum-tag-bits)
        (if (= n-fixnum-tag-bits 1)
            (inst lea quo (ea eax eax))
            (inst lea quo (ea nil eax (ash 1 n-fixnum-tag-bits)))))
    (move rem edx)))

(define-vop (fast-truncate/unsigned=>unsigned fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (unsigned-reg) :target eax)
         (y :scs (unsigned-reg signed-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:args-var args)
  (:temporary (:sc unsigned-reg :offset rax-offset :target quo
               :from (:argument 0) :to (:result 0)) eax)
  (:temporary (:sc unsigned-reg :offset rdx-offset :target rem
                   :from (:argument 0) :to (:result 1)) edx)
  (:results (quo :scs (unsigned-reg))
            (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 33
    (when (types-equal-or-intersect (tn-ref-type (tn-ref-across args))
                                    (specifier-type '(eql 0)))
      (if (sc-is y signed-reg)
          (inst test y y)               ; smaller instruction
          (inst cmp y 0))
      (inst jmp :eq (generate-error-code vop 'division-by-zero-error x y)))
    (move eax x)
    (zeroize edx)
    (inst div eax y)
    (move quo eax)
    (move rem edx)))

(define-vop (fast-truncate-c/unsigned=>unsigned fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (unsigned-reg) :target eax))
  (:info y)
  (:arg-types unsigned-num (:constant (unsigned-byte 64)))
  (:temporary (:sc unsigned-reg :offset rax-offset :target quo
                   :from :argument :to (:result 0)) eax)
  (:temporary (:sc unsigned-reg :offset rdx-offset :target rem
                   :from :eval :to (:result 1)) edx)
  (:temporary (:sc unsigned-reg :from :eval :to :result) y-arg)
  (:results (quo :scs (unsigned-reg))
            (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 32
    (move eax x)
    (zeroize edx)
    (inst mov y-arg y)
    (inst div eax y-arg)
    (move quo eax)
    (move rem edx)))

(define-vop (fast-truncate/signed=>signed fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg) :target eax)
         (y :scs (signed-reg signed-stack)))
  (:args-var args)
  (:arg-types signed-num signed-num)
  (:temporary (:sc signed-reg :offset rax-offset :target quo
                   :from (:argument 0) :to (:result 0)) eax)
  (:temporary (:sc signed-reg :offset rdx-offset :target rem
                   :from (:argument 0) :to (:result 1)) edx)
  (:results (quo :scs (signed-reg))
            (rem :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:note "inline (signed-byte 64) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 33
    (when (types-equal-or-intersect (tn-ref-type (tn-ref-across args))
                                    (specifier-type '(eql 0)))
      (if (sc-is y signed-reg)
          (inst test y y)               ; smaller instruction
          (inst cmp y 0))
      (inst jmp :eq (generate-error-code vop 'division-by-zero-error x y)))
    (move eax x)
    (inst cqo)
    (inst idiv eax y)
    (move quo eax)
    (move rem edx)))

(define-vop (fast-truncate-c/signed=>signed fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg) :target eax))
  (:info y)
  (:arg-types signed-num (:constant (signed-byte 64)))
  (:temporary (:sc signed-reg :offset rax-offset :target quo
                   :from :argument :to (:result 0)) eax)
  (:temporary (:sc signed-reg :offset rdx-offset :target rem
                   :from :eval :to (:result 1)) edx)
  (:temporary (:sc signed-reg :from :eval :to :result) y-arg)
  (:results (quo :scs (signed-reg))
            (rem :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:note "inline (signed-byte 64) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 32
    (move eax x)
    (inst cqo)
    (inst mov y-arg y)
    (inst idiv eax y-arg)
    (move quo eax)
    (move rem edx)))



;;;; Shifting
(macrolet ((encodable-as-lea ()
             `(and (gpr-tn-p number) (gpr-tn-p result)
                   (not (location= number result))
                   (member amount '(1 2 3))))
           (generate-lea ()
             `(case amount
                (1 (inst lea result (ea number number)))
                (2 (inst lea result (ea nil number 4)))
                (3 (inst lea result (ea nil number 8)))))
           (with-shift-operands (&body body)
             ;; If the initial "MOVE result number" is a legal instruction,
             ;; then we're OK; otherwise use the temp reg to do the shift.
             `(multiple-value-bind (save result)
                  (if (or (location= number result) (gpr-tn-p number) (gpr-tn-p result))
                      (values nil result)
                      (values result temp))
                (move result number)
                (assemble ()
                 ,@body)
                (when save (inst mov save result)))))

(define-vop (fast-ash-c/fixnum=>fixnum)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (any-reg control-stack) :target result))
  (:info amount)
  (:arg-types tagged-num (:constant integer))
  (:results (result :scs (any-reg control-stack)))
  (:result-types tagged-num)
  (:note "inline ASH")
  (:variant nil)
  (:variant-vars modularp)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 2
    (cond ((= amount 0) (bug "shifting by 0"))
          ((>= amount 64) ; shifting left (zero fill)
           (unless modularp
             (bug "Impossible: fixnum ASH left exceeds word length"))
           (zeroize result))
          ((encodable-as-lea) (generate-lea))
          (t
           (with-shift-operands
            (cond ((< -64 amount 64)
                   ;; this code is used both in ASH and ASH-MODFX, so
                   ;; be careful
                   (if (plusp amount)
                       (inst shl result amount)
                       (progn
                         (inst sar result (- amount))
                         (inst and result (lognot fixnum-tag-mask)))))
                  ;; shifting right (sign fill)
                  (t (move result number)
                     (inst sar result 63)
                     (inst and result (lognot fixnum-tag-mask)))))))))

(define-vop (fast-ash-left/fixnum=>fixnum)
  (:translate ash)
  (:args (number :scs (any-reg control-stack) :target result)
         (amount :scs (unsigned-reg) :target ecx))
  (:arg-types tagged-num positive-fixnum)
  (:temporary (:sc unsigned-reg :offset rcx-offset :from (:argument 1)) ecx)
  (:results (result :scs (any-reg control-stack) :from (:argument 0)))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:note "inline ASH")
  (:temporary (:sc unsigned-reg) temp)
  (:generator 3
    (with-shift-operands
      (move ecx amount)
      ;; The result-type ensures us that this shift will not overflow.
      (inst shl result :cl))))

(define-vop (fast-ash-c/signed=>signed)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg signed-stack) :target result))
  (:info amount)
  (:arg-types signed-num (:constant integer))
  (:results (result :scs (signed-reg signed-stack)))
  (:result-types signed-num)
  (:note "inline ASH")
  (:temporary (:sc unsigned-reg) temp)
  (:generator 3
    (cond ((encodable-as-lea) (generate-lea))
          (t
           (with-shift-operands
            (cond ((plusp amount) (inst shl result amount))
                  (t (inst sar result (min 63 (- amount))))))))))

(define-vop (fast-ash-c/unsigned=>unsigned)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg unsigned-stack) :target result))
  (:info amount)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg unsigned-stack)))
  (:result-types unsigned-num)
  (:note "inline ASH")
  (:temporary (:sc unsigned-reg) temp)
  (:generator 3
    (cond ((= amount 0) (bug "shifting by 0"))
          ((not (< -64 amount 64)) (zeroize result))
          ((encodable-as-lea) (generate-lea))
          (t
           (with-shift-operands
             (if (plusp amount)
                 (inst shl result amount)
                 (inst shr result (- amount))))))))

(define-vop (fast-ash-left/signed=>signed)
  (:translate ash)
  (:args (number :scs (signed-reg signed-stack) :target result)
         (amount :scs (unsigned-reg) :target ecx))
  (:arg-types signed-num positive-fixnum)
  (:temporary (:sc unsigned-reg :offset rcx-offset :from (:argument 1)) ecx)
  (:results (result :scs (signed-reg signed-stack) :from (:argument 0)))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:note "inline ASH")
  (:temporary (:sc unsigned-reg) temp)
  (:generator 4
    (with-shift-operands
      (move ecx amount)
      (inst shl result :cl))))

(define-vop (fast-ash-left/unsigned=>unsigned)
  (:translate ash)
  (:args (number :scs (unsigned-reg unsigned-stack) :target result)
         (amount :scs (unsigned-reg) :target ecx))
  (:arg-types unsigned-num positive-fixnum)
  (:temporary (:sc unsigned-reg :offset rcx-offset :from (:argument 1)) ecx)
  (:results (result :scs (unsigned-reg unsigned-stack) :from (:argument 0)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:note "inline ASH")
  (:temporary (:sc unsigned-reg) temp)
  (:generator 4
    (with-shift-operands
      (move ecx amount)
      (inst shl result :cl))))

(define-vop (fast-ash-left/fixnum-modfx=>fixnum
             fast-ash-left/fixnum=>fixnum)
  (:translate ash-left-modfx)
  (:args-var args)
  (:generator 3
    (with-shift-operands
      (move ecx amount)
      (unless (csubtypep (tn-ref-type (tn-ref-across args)) ;; amount
                         (specifier-type '(mod 63)))
        (inst cmp amount 63)
        (inst jmp :be OKAY)
        (zeroize result))
      OKAY
      (inst shl result :cl))))

(define-vop (fast-ash-left-mod64-c/unsigned=>unsigned
             fast-ash-c/unsigned=>unsigned)
  (:translate ash-left-mod64))

(define-vop (fast-ash-left-modfx-c/fixnum=>fixnum
             fast-ash-c/fixnum=>fixnum)
  (:variant :modular)
  (:translate ash-left-modfx))

(define-vop (fast-ash-left/unsigned-mod64=>unsigned
             fast-ash-left/unsigned=>unsigned)
  (:translate ash-left-mod64)
  (:args-var args)
  (:generator 3
    (with-shift-operands
      (move ecx amount)
      (unless (csubtypep (tn-ref-type (tn-ref-across args)) ;; amount
                         (specifier-type '(mod 63)))
        (inst cmp amount 63)
        (inst jmp :be OKAY)
        (zeroize result))
      OKAY
      (inst shl result :cl))))

(define-vop (fast-%ash/right/unsigned)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg unsigned-stack) :target result)
         (amount :scs (unsigned-reg) :target rcx))
  (:arg-types unsigned-num unsigned-num)
  (:results (result :scs (unsigned-reg unsigned-stack) :from (:argument 0)))
  (:result-types unsigned-num)
  (:temporary (:sc signed-reg :offset rcx-offset :from (:argument 1)) rcx)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 4
    (with-shift-operands
      (move rcx amount)
      (inst shr result :cl))))

(define-vop (fast-%ash/right/signed)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg signed-stack) :target result)
         (amount :scs (unsigned-reg) :target rcx))
  (:arg-types signed-num unsigned-num)
  (:results (result :scs (signed-reg signed-stack) :from (:argument 0)))
  (:result-types signed-num)
  (:temporary (:sc signed-reg :offset rcx-offset :from (:argument 1)) rcx)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 4
    (with-shift-operands
      (move rcx amount)
      (inst sar result :cl))))

(define-vop (fast-%ash/right/fixnum)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (any-reg control-stack) :target result)
         (amount :scs (unsigned-reg) :target rcx))
  (:arg-types tagged-num unsigned-num)
  (:results (result :scs (any-reg control-stack) :from (:argument 0)))
  (:result-types tagged-num)
  (:temporary (:sc signed-reg :offset rcx-offset :from (:argument 1)) rcx)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 3
    (with-shift-operands
      (move rcx amount)
      (inst sar result :cl)
      (inst and result (lognot fixnum-tag-mask)))))
) ; end MACROLET

(define-vop (fast-ash/unsigned=>unsigned)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg) :target result)
         (amount :scs (signed-reg) :target ecx))
  (:arg-types unsigned-num signed-num)
  (:results (result :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:temporary (:sc signed-reg :offset rcx-offset :from (:argument 1)) ecx)
  (:args-var args)
  (:variant-vars check-amount signed)
  (:note "inline ASH")
  (:generator 5
    (move result number)
    (move ecx amount)
    (inst test ecx ecx)
    (inst jmp :ns POSITIVE)
    (inst neg ecx)
    (unless (csubtypep (tn-ref-type (tn-ref-across args))
                       (specifier-type `(integer -63 *)))
      (inst cmp ecx 63)
      (inst jmp :be OKAY)
      (cond (signed
             (inst or ecx 63))
            (t
             (zeroize result))))
    OKAY
    (if signed
        (inst sar result :cl)
        (inst shr result :cl))
    (inst jmp DONE)

    POSITIVE
    (unless (or (not check-amount) ;; The result-type ensures us that this shift will not overflow.
                (csubtypep (tn-ref-type (tn-ref-across args))
                           (specifier-type `(integer * 63))))
      (inst cmp ecx 63)
      (inst jmp :be STILL-OKAY)
      (zeroize result))
    STILL-OKAY
    (inst shl result :cl)

    DONE))

(define-vop (fast-ash/signed=>signed
             fast-ash/unsigned=>unsigned)
  (:args (number :scs (signed-reg) :target result)
         (amount :scs (signed-reg) :target ecx))
  (:arg-types signed-num signed-num)
  (:results (result :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:variant nil t))

(define-vop (fast-ash-modfx/signed/unsigned=>fixnum)
  (:translate ash-modfx)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg unsigned-reg) :to :save)
         (amount :scs (signed-reg) :target ecx))
  (:arg-types (:or signed-num unsigned-num) signed-num)
  (:results (result :scs (any-reg) :from (:argument 0)))
  (:args-var args)
  (:result-types tagged-num)
  (:temporary (:sc signed-reg :offset rcx-offset :from (:argument 1)) ecx)
  (:note "inline ASH")
  (:generator 3
    (move result number)
    (move ecx amount)
    (inst test ecx ecx)
    (inst jmp :ns POSITIVE)
    (inst neg ecx)
    (unless (csubtypep (tn-ref-type (tn-ref-across args))
                       (specifier-type `(integer -63 *)))
      (inst cmp ecx 63)
      (inst jmp :be OKAY)
      (sc-case number
        (signed-reg
         (inst or ecx 63))
        (unsigned-reg
         (zeroize result))))
    OKAY
    (sc-case number
      (signed-reg
       (inst sar result :cl))
      (unsigned-reg
       (inst shr result :cl)))
    (inst jmp DONE)

    POSITIVE
    (unless (csubtypep (tn-ref-type (tn-ref-across args))
                       (specifier-type `(integer * 63)))
      (inst cmp ecx 63)
      (inst jmp :be STILL-OKAY)
      (zeroize result))
    STILL-OKAY
    (inst shl result :cl)
    DONE
    (inst shl result n-fixnum-tag-bits)))

(define-vop (fast-ash-modfx/signed=>signed
             fast-ash/signed=>signed)
  (:variant t t)
  (:translate ash-modfx))

(define-vop (fast-ash-mod64/unsigned=>unsigned
             fast-ash/unsigned=>unsigned)
  (:variant t nil)
  (:translate ash-mod64))

(define-vop (fast-ash-mod64/signed=>unsigned
             fast-ash/signed=>signed)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant t t)
  (:translate ash-mod64))

(in-package "SB-C")

(defknown %lea (integer integer (member 1 2 4 8 16) (signed-byte 64))
  integer
  (foldable flushable movable))

;;; FIXME: arg order should be (DISP BASE INDEX SCALE) to match EA constructor
(defun %lea (base index scale disp)
  (+ base (* index scale) disp))

(in-package "SB-VM")

(define-vop (%lea/unsigned=>unsigned)
  (:translate %lea)
  (:policy :fast-safe)
  (:args (base :scs (unsigned-reg))
         (index :scs (unsigned-reg)))
  (:info scale disp)
  (:arg-types unsigned-num unsigned-num
              (:constant (member 1 2 4 8))
              (:constant (signed-byte 64)))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 5
    (inst lea r (ea disp base index scale))))

(define-vop (%lea/signed=>signed)
  (:translate %lea)
  (:policy :fast-safe)
  (:args (base :scs (signed-reg))
         (index :scs (signed-reg)))
  (:info scale disp)
  (:arg-types signed-num signed-num
              (:constant (member 1 2 4 8))
              (:constant (signed-byte 64)))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 4
    (inst lea r (ea disp base index scale))))

(define-vop (%lea/fixnum=>fixnum)
  (:translate %lea)
  (:policy :fast-safe)
  (:args (base :scs (any-reg))
         (index :scs (any-reg)))
  (:info scale disp)
  (:arg-types tagged-num tagged-num
              (:constant (member 1 2 4 8))
              (:constant (signed-byte 64)))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:generator 3
    (inst lea r (ea disp base index scale))))

;;; FIXME: before making knowledge of this too public, it needs to be
;;; fixed so that it's actually _faster_ than the non-CMOV version; at
;;; least on my Celeron-XXX laptop, this version is marginally slower
;;; than the above version with branches.  -- CSR, 2003-09-04
(define-vop (fast-cmov-ash/unsigned=>unsigned)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg) :target result)
         (amount :scs (signed-reg) :target ecx))
  (:arg-types unsigned-num signed-num)
  (:results (result :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:temporary (:sc signed-reg :offset rcx-offset :from (:argument 1)) ecx)
  (:temporary (:sc any-reg :from (:eval 0) :to (:eval 1)) zero)
  (:note "inline ASH")
  (:guard (member :cmov *backend-subfeatures*))
  (:generator 4
    (move result number)
    (move ecx amount)
    (inst test ecx ecx)
    (inst jmp :ns POSITIVE)
    (inst neg ecx)
    (zeroize zero)
    (inst shr result :cl)
    (inst cmp ecx 63)
    (inst cmov :nbe result zero)
    (inst jmp DONE)

    POSITIVE
    ;; The result-type ensures us that this shift will not overflow.
    (inst shl result :cl)

    DONE))

(define-vop (signed-byte-64-len)
  (:translate integer-length)
  (:note "inline (signed-byte 64) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg) :target res))
  (:arg-types signed-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 28
    (move res arg)
    (inst test res res)
    (inst jmp :ge POS)
    (inst not res)
    POS
    (inst bsr res res)
    (inst jmp :z ZERO)
    (inst inc res)
    (inst jmp DONE)
    ZERO
    (zeroize res)
    DONE))

(define-vop (unsigned-byte-64-len)
  (:translate integer-length)
  (:note "inline (unsigned-byte 64) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 26
    (inst bsr res arg)
    (inst jmp :z ZERO)
    (inst inc res)
    (inst jmp DONE)
    ZERO
    (zeroize res)
    DONE))

;; The code on which this was based existed in no less than three varieties,
;; differing in response to 0 input: produce NIL, -1, or signal an error.
;; To avoid a thorny issue of proper semantics, this VOP is used only by
;; %BIT-POSITION which happens to declare zero safety, but always pre-checks
;; for zero. (the ltn-policy of :fast is actually irrelevant)
(define-vop (unsigned-word-find-first-bit)
  (:policy :fast)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst bsf res arg)))

;; INTEGER-LENGTH is implemented by using the BSR instruction, which
;; returns the position of the first 1-bit from the right. And that needs
;; to be incremented to get the width of the integer, and BSR doesn't
;; work on 0, so it needs a branch to handle 0.

;; But fixnums are tagged by being shifted left n-fixnum-tag-bits times,
;; untagging by shifting right n-fixnum-tag-bits-1 times (and if
;; n-fixnum-tag-bits = 1, no shifting is required), will make the
;; resulting integer one bit wider, making the increment unnecessary.
;; Then, to avoid calling BSR on 0, OR the result with 1. That sets the
;; first bit to 1, and if all other bits are 0, BSR will return 0,
;; which is the correct value for INTEGER-LENGTH.
(define-vop (positive-fixnum-len)
  (:translate integer-length)
  (:note "inline positive fixnum integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 24
    (move res arg)
    (when (> n-fixnum-tag-bits 1)
      (inst shr res (1- n-fixnum-tag-bits)))
    (inst or res 1)
    (inst bsr res res)))

(define-vop (fixnum-len)
  (:translate integer-length)
  (:note "inline fixnum integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (any-reg) :target res))
  (:arg-types tagged-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 25
    (move res arg)
    (when (> n-fixnum-tag-bits 1)
      (inst sar res (1- n-fixnum-tag-bits)))
    (inst test res res)
    (inst jmp :ge POS)
    (inst not res)
    POS
    (inst or res 1)
    (inst bsr res res)))

;;;; binary conditional VOPs

(define-vop (fast-conditional)
  (:conditional :e)
  (:info)
  (:temporary (:sc unsigned-reg) temp)
  (:policy :fast-safe))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg control-stack))
         (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg control-stack)))
  (:arg-types tagged-num (:constant fixnum))
  (:info y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg signed-stack))
         (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 64) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg signed-stack)))
  (:arg-types signed-num (:constant (signed-byte 64)))
  (:info y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg unsigned-stack))
         (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 64) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num (:constant (unsigned-byte 64)))
  (:info y))

(defun ensure-not-mem+mem (x y temp)
  (cond ((sc-is x immediate)
         (inst mov temp (tn-value x))
         (ensure-not-mem+mem temp y nil))
        (t
         (when (and (tn-p y)
                    (sc-is y immediate))
           (setf y (tn-value y)))
         (when (integerp y)
           (acond ((plausible-signed-imm32-operand-p y)
                   (return-from ensure-not-mem+mem (values x it)))
                  ((typep y '(unsigned-byte 32))
                   ;; Rather than a RIP-relative constant, load a dword (w/o sign-extend)
                   (inst mov :dword temp y)
                   (return-from ensure-not-mem+mem (values x temp))))
           (setq y (register-inline-constant :qword y)))
         (cond ((or (gpr-tn-p x) (gpr-tn-p y))
                (values x y))
               (t
                (inst mov temp x)
                (values temp y))))))

(defun immediate-operand-smallest-nbits (x)
  (declare (type word x))
  (typecase x
    ((unsigned-byte  8)  8)
    ((unsigned-byte 16) 16)
    ((unsigned-byte 32) 32)
    (t                  64)))

(defun bits->size (bits)
  (ecase bits
    (8  :byte)
    (16 :word)
    (32 :dword)
    (64 :qword)))

;;; Emit the most compact form of the test immediate instruction
;;; by using the smallest operand size that is the large enough to hold
;;; the immediate value Y. The operand size makes little difference since only
;;; flags are affected. However, if the msb (the sign bit) of the immediate
;;; operand at a smaller size is 1 but at its true size (always a :QWORD) is 0,
;;; the S flag value could come out 1 instead of 0.
;;; SIGN-BIT-MATTERS specifies that a shorter operand size must not be selected
;;; if doing so could affect whether the sign flag comes out the same.
;;; e.g. if EDX is #xff, "TEST EDX, #x80" indicates a non-negative result
;;; whereas "TEST DL, #x80" indicates a negative result.
(defun emit-optimized-test-inst (x y temp sign-bit-matters)
  (let* ((bits (if (or (not (integerp y)) (minusp y))
                   64
                   (immediate-operand-smallest-nbits y)))
         (size (unless (eql bits 64)
                 (when (and (logbitp (1- bits) y) sign-bit-matters)
                   (setq bits (* bits 2)))
                 (unless (eql bits 64)
                   (bits->size bits))))
         ;; A size is reducible to byte if there are at most 8 bits set
         ;; in an 8-bit-aligned field. For now I'm only dealing with
         ;; the restricted case of 8 bits at (BYTE 8 8).
         (reducible-to-byte-p
          (and (eq size :word) (not (logtest #xFF y)))))
    (cond ((not size)
           ;; Ensure that both operands are acceptable
           ;; by possibly loading one into TEMP
           (multiple-value-setq (x y) (ensure-not-mem+mem x y temp))
           (inst test :qword x y))
          ((sc-is x control-stack unsigned-stack signed-stack)
           ;; Otherwise, when using an immediate operand smaller
           ;; than 64 bits, narrow the reg/mem operand to match.
           (let ((disp (frame-byte-offset (tn-offset x))))
             (when reducible-to-byte-p
               (setq size :byte disp (1+ disp) y (ash y -8)))
             (inst test size (ea disp rbp-tn) y)))
          (t
           (aver (gpr-tn-p x))
           (if (and reducible-to-byte-p (<= (tn-offset x) rbx-offset))
               ;; Use upper byte of word reg {A,C,D,B}X -> {A,C,D,B}H
               (inst test :byte `(,x . :high-byte) (ash y -8))
               (inst test size x y))))))

(macrolet ((define-logtest-vops ()
             `(progn
               ,@(loop for suffix in '(/fixnum -c/fixnum
                                       /signed -c/signed
                                       /unsigned -c/unsigned)
                       for cost in '(4 3 6 5 6 5)
                       collect
                       `(define-vop (,(symbolicate "FAST-LOGTEST" suffix)
                                     ,(symbolicate "FAST-CONDITIONAL" suffix))
                         (:translate logtest)
                         (:conditional :ne)
                         (:generator ,cost
                          (emit-optimized-test-inst x
                           ,(if (eq suffix '-c/fixnum) `(fixnumize y) 'y)
                           temp nil)))))))
  (define-logtest-vops))

;;; This works for tagged or untagged values, but the vop optimizer
;;; has to pre-adjust Y if tagged.
(define-vop (logtest-memref fast-conditional)
  (:args (x :scs (descriptor-reg)))
  (:arg-types * (:constant integer))
  (:info y)
  (:args-var arg-ref)
  (:conditional :ne)
  (:generator 1
   (let ((disp (cdr (tn-ref-memory-access arg-ref))))
     ;; Try as :BYTE, :WORD, :DWORD
     (macrolet ((try (size bits)
                  `(let ((disp disp) (y y) (mask ,(ldb (byte bits 0) -1)))
                     (dotimes (i ,(/ 64 bits))
                       (when (zerop (logandc2 y mask))
                         (inst test ,size (ea disp x) (ash y (* i ,(- bits))))
                         (return-from logtest-memref))
                       (setq mask (ash mask ,bits))
                       (incf disp ,(/ bits 8))))))
       (try :byte   8)
       (try :word  16)
       (try :dword 32))
     (let ((val (ea disp x))
           (y (constantize y)))
       (cond ((integerp y)
              (inst test :qword val y))
             (t
              (inst mov temp val)
              (inst test :qword temp y)))))))

;;; Try to absorb a memory load into LOGTEST.
;;; This removes one instruction and possibly shortens the TEST by eliding
;;; a REX prefix.
(defoptimizer (sb-c::vop-optimize fast-logtest-c/fixnum) (vop)
  (unless (tn-ref-memory-access (vop-args vop))
    (let ((prev (sb-c::previous-vop-is
                 ;; TODO: missing data-vector-ref/simple-vector-c
                 vop '(%raw-instance-ref/signed-word
                       %raw-instance-ref/word
                       instance-index-ref-c
                       ;; This would only happen in unsafe code most likely,
                       ;; because CAR,CDR, etc would need to cast/assert the loaded
                       ;; value to fixnum. However, in practive it doesn't work anyway
                       ;; because there seems to be a spurious MOVE vop in between
                       ;; the SLOT and the FAST-LOGTEST-C/FIXNUM.
                       ;; But ironically enough we _did_ seem to want to optimize
                       ;; an expression in GENERATE-CODE along the lines of:
                       ;;  (if (oddp (length (ir2-component-constants ir2-component))) ...)
                       ;; which, if #+ubsan, would not be admissible
                       ;; because VECTOR-LENGTH is not in a slot.
                       ;; Obviously a GENERATE-CODE bug is the mother of all bugs.
                       #-ubsan slot))))
      (aver (not (vop-results vop))) ; is a :CONDITIONAL vop
      (when (and prev (eq (vop-block prev) (vop-block vop)))
        ;; If the memory ref produces a fixnum, the constant should be a fixnum
        ;; so that we don't see cases such as in lp#1939897.
        ;; In the absence of vop combining, MOVE-TO-WORD would be inserted
        ;; between INSTANCE-INDEX-REF and LOGTEST, but it did not happen yet.
        (let* ((arg (vop-args vop))
               (info-arg (car (vop-codegen-info vop)))
               (constant (if (member (vop-name prev) '(instance-index-ref-c slot))
                             (ash info-arg n-fixnum-tag-bits)
                             info-arg)))
          (when (and (eq (tn-ref-tn (vop-results prev)) (tn-ref-tn arg))
                     (sb-c::very-temporary-p (tn-ref-tn arg))
                     (typep constant '(or word signed-word)))
            (binding* ((disp (valid-memref-byte-disp prev) :exit-if-null)
                       (arg-ref (sb-c:reference-tn (tn-ref-tn (vop-args prev)) nil))
                       (new (sb-c::emit-and-insert-vop
                             (sb-c::vop-node vop) (vop-block vop)
                             (template-or-lose 'logtest-memref)
                             arg-ref nil prev (list constant))))
              (setf (tn-ref-memory-access arg-ref) `(:read . ,disp))
              (sb-c::delete-vop prev)
              (sb-c::delete-vop vop)
              new)))))))
(setf (sb-c::vop-info-optimizer (template-or-lose 'fast-logtest-c/signed))
      #'vop-optimize-fast-logtest-c/fixnum-optimizer)
(setf (sb-c::vop-info-optimizer (template-or-lose 'fast-logtest-c/unsigned))
      #'vop-optimize-fast-logtest-c/fixnum-optimizer)

;;; TODO: The TEST instruction preceding this JEQ is entirely superfluous
;;; and can be removed with a vop optimizer:
;;; E1:       25FE0F0000       AND EAX, 4094
;;; E6:       4885C0           TEST RAX, RAX
;;; E9:       74C9             JEQ L2

;;; %LOGBITP has the same argument order as ordinary LOGBITP which is * backwards *
;;; relative to every other architecture.
;;; I suspect the others have a predilection for placing codegen info args last.
(defknown %logbitp ((mod 64) (or signed-word word)) boolean
  (movable foldable flushable always-translatable))

;;; only for constant folding within the compiler
(defun %logbitp (index integer)
  (declare (notinline logbitp))
  ;; Normally an "intepreter stub" is implemented in terms of itself, not in terms
  ;; of the public function. But this has to work in the cross-compiler too.
  ;; A way to do that is define a version of %LOGBITP in cross-misc.
  ;; Then brings a new problem: inconsistent argument order across the architectures.
  (logbitp index integer))

;;; Normally we define a spectrum of vops to handle {unsigned,signed,any}-reg and
;;; constant/non-constant operands. That's often unnecessary. Certainly for this vop.
(define-vop (%logbitp fast-safe-arith-op)
  (:translate %logbitp)
  (:conditional :c)
  (:args (bit :scs (signed-reg signed-stack unsigned-reg unsigned-stack
                    any-reg control-stack) :load-if nil)
         ;; CONSTANT here is to allow integers exceeding a fixnum which get NIL
         ;; from IMMEDIATE-CONSTANT-SC. This is only an issue for vops which don't
         ;; take a codegen info for the constant.
         ;; IMMEDIATE is always allowed and pertains to fixnum-sized constants.
         (int :scs (constant signed-reg signed-stack unsigned-reg unsigned-stack)
              :load-if nil))
  (:arg-types untagged-num untagged-num)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 4
    (when (sc-is int constant immediate) (setq int (tn-value int)))
    ;; Force INT to be a RIP-relative operand if it is a constant.
    (let ((word (if (integerp int) (register-inline-constant :qword int) int))
          (bit (cond ((sc-is bit signed-reg unsigned-reg) bit)
                     (t (inst mov :dword temp bit)
                        (when (sc-is bit any-reg control-stack)
                          (inst shr :dword temp n-fixnum-tag-bits))
                        temp))))
      (inst bt word bit))))

(defun change-tested-flag (vop from-flag to-flag)
  (ecase (vop-name vop)
    (branch-if
     (let ((info (vop-codegen-info vop)))
       (aver (equal (third info) (list from-flag)))
       (setf (vop-codegen-info vop) (list (car info) (cadr info) (list to-flag)))))
    (compute-from-flags
     (let ((info (vop-codegen-info vop)))
       (aver (equal (first info) (list from-flag)))
       (setf (vop-codegen-info vop) (list (list to-flag)))))))

(define-vop (%logbitp/c fast-safe-arith-op)
  (:translate %logbitp)
  (:conditional :c)
  (:info bit)
  (:args (int :scs (signed-reg signed-stack unsigned-reg unsigned-stack
                    any-reg control-stack) :load-if nil))
  (:arg-types (:constant (mod 64)) untagged-num)
  (:vop-var vop)
  (:generator 1
    (when (sc-is int any-reg control-stack)
      ;; Acount for fixnum tag bit.
      ;; Reading beyond the sign bit is the same as reading the sign bit.
      (setf bit (min (1- n-word-bits) (+ bit n-fixnum-tag-bits))))
    (let ((next (vop-next vop)))
      (when (member (vop-name next) '(branch-if compute-from-flags))
        (cond ((not (gpr-tn-p int)) ; is in memory, issue it as a TEST
               ;; To test bit index 8: add 1 to the disp, and use immediate val 0x01
               ;;             index 9: add 1 to the disp, and use immediate val 0x02
               ;;             etc
               ;; I'm not crazy about this approach, because we lose the connection
               ;; to which TN we're reading.  There needs to be a way to emit the instruction
               ;; as byte-within-stack-tn so that it is understood by other optimizations.
               (binding* ((frame-disp  (frame-byte-offset (tn-offset int)))
                          ((extra-disp bit-shift) (floor bit 8)))
                 (inst test :byte (ea (+ frame-disp extra-disp) rbp-tn) (ash 1 bit-shift)))
               (change-tested-flag next :c :ne)
               (return-from %logbitp/c))
              ((= bit 31) ; test the sign bit of the 32-bit register
               (inst test :dword int int)
               (change-tested-flag next :c :s)
               (return-from %logbitp/c))
              ((< bit 32)
               (inst test (if (< bit 8) :byte :dword) int (ash 1 bit))
               (change-tested-flag next :c :ne)
               (return-from %logbitp/c)))))
    (inst bt (if (<= bit 31) :dword :qword) int bit)))

(define-vop (%logbitp-memref fast-conditional)
  (:args (x :scs (descriptor-reg)))
  (:arg-types (:constant (mod 64)) *)
  (:info bit)
  (:args-var arg-ref)
  (:vop-var vop)
  (:conditional :ne)
  (:ignore temp)
  (:generator 1
    ;; This resembles the above case for TN being not in a GPR
    (binding* ((slot-disp (cdr (tn-ref-memory-access arg-ref)))
               ((extra-disp bit-shift) (floor bit 8)))
      (inst test :byte (ea (+ slot-disp extra-disp) x) (ash 1 bit-shift)))))

(defoptimizer (sb-c::vop-optimize %logbitp/c) (vop)
  (unless (tn-ref-memory-access (vop-args vop))
    (let ((prev (sb-c::previous-vop-is
                 ;; TODO: missing data-vector-ref/simple-vector-c and SLOT
                 vop '(%raw-instance-ref/signed-word
                       %raw-instance-ref/word
                       instance-index-ref-c))))
      (aver (not (vop-results vop))) ; is a :CONDITIONAL vop
      (when (and prev (eq (vop-block prev) (vop-block vop)))
        (let ((arg (vop-args vop)))
          (when (and (eq (tn-ref-tn (vop-results prev)) (tn-ref-tn arg))
                     (sb-c::very-temporary-p (tn-ref-tn arg))
                     (vop-next vop)
                     ;; Ensure we can change the tested flag from CF to ZF
                     (member (vop-name (vop-next vop))
                             '(branch-if compute-from-flags)))
            (binding* ((disp (valid-memref-byte-disp prev) :exit-if-null)
                       (arg-ref
                        (sb-c:reference-tn (tn-ref-tn (vop-args prev)) nil))
                       (bit (car (vop-codegen-info vop)))
                       (info
                        (if (sb-c::previous-vop-is vop 'instance-index-ref-c) ; tagged slot
                            ;; Reading beyond the sign bit is the same as reading the sign bit.
                            (min (+ bit n-fixnum-tag-bits) (1- n-word-bits))
                            bit))
                       (new (sb-c::emit-and-insert-vop
                             (sb-c::vop-node vop) (vop-block vop)
                             (template-or-lose '%logbitp-memref)
                             arg-ref nil prev (list info))))
              (setf (tn-ref-memory-access arg-ref) `(:read . ,disp))
              (change-tested-flag (vop-next vop) :c :ne)
              (sb-c::delete-vop prev)
              (sb-c::delete-vop vop)
              new)))))))

;;; We can delete some MOVEs that seem often to get inserted with iteration constructs
;;; such as (setq i (1+ i)) where the result of 1+ creates a new TN which is moved
;;; to the same TN that is the input to 1+, but PACK chooses different physical registers
;;; for the arg and result of FAST-+-C/FIXNUM=>FIXNUM. So we "cleverly" can use the LEA
;;; instruction as a 3-operand ADD, only to move the destination of LEA back to the
;;; same register that was one of the input operands. Yet the TN which was the result
;;; had otherwise no use. Why does this happen? I don't know.
;;;
;;; So let's try to prevent it by removing the MOVE, which reduces to just the ADD
;;; instruction instead of LEA + MOV. If a vop can only take one physical representation
;;; (such as tagged fixnum) for input, and can only produce that same representation,
;;; and the TN flows back to that vop, then the move is not needed. But if a vop can take
;;; several physical representations, such as accepting either tagged or untagged,
;;; and the SC has not been chosen yet (which happens), then we can't remove.
;;;
;;; For some reason, it seems to come up a trememdous amount with FAST-+-C/FIXNUM=>FIXNUM.
;;; Maybe it comes up with others, I don't know. No harm in trying, I suppose.
;;; To do this for other vops, you have to be certain that the move isn't a coercion.
;;;
;;; [And it would be nice if every backend named their vops consistently
;;; so that this optimizer could be made architecture-independent]
;;; The SB-C::DELETE- function isn't defined yet in the build order, so wrap it in a lambda.
(flet ((optimizer (vop) (sb-c::delete-unnecessary-move vop)))
  (dolist (name '(sb-vm::fast-+-c/fixnum=>fixnum
                  sb-vm::fast-+-c/signed=>signed
                  sb-vm::fast-+-c/unsigned=>unsigned
                  sb-vm::fast---c/fixnum=>fixnum
                  sb-vm::fast---c/signed=>signed
                  sb-vm::fast---c/unsigned=>unsigned
                  sb-vm::fast-*-c/fixnum=>fixnum
                  sb-vm::fast-*-c/signed=>signed
                  sb-vm::fast-*-c/unsigned=>unsigned))
    (sb-c::set-vop-optimizer (template-or-lose name) #'optimizer)))

(defun emit-optimized-cmp (x y temp &optional x-ctype)
  ;; Shorten the encoding by eliding a REX prefix where the upper bits
  ;; can not possibly matter.
  ;; Be sure to account for N-FIXNUM-TAG-BITS in determining how many bits
  ;; of precision are in the representation of X.
  ;; Little-endian addressing makes this valid for stack TNs as well as registers.
  (let ((operand-size (if (and (numeric-type-p x-ctype)
                               (typep y '(signed-byte 32))
                               (csubtypep x-ctype
                                          (if (sc-is x any-reg descriptor-reg control-stack)
                                              (specifier-type '(signed-byte 31))
                                              (specifier-type '(signed-byte 32)))))
                          :dword
                          :qword)))
    (if (and (gpr-tn-p x) (eql y 0))
      ;; Amazingly (to me), use of TEST in lieu of CMP produces all the correct
      ;; flag bits for inequality comparison as well as EQL comparison.
      ;; You'd think that the Jxx instruction should examine _only_ the S flag,
      ;; but in fact the other flags are right too. Nonetheless this is
      ;; quite confusing, and I would prefer that we alter the branch test
      ;; when emitting TEST in place of CMP.
        (inst test operand-size x x) ; smaller instruction
        (progn (multiple-value-setq (x y) (ensure-not-mem+mem x y temp))
               (inst cmp operand-size x y)))))

(macrolet ((define-conditional-vop (tran cond unsigned not-cond not-unsigned)
             (declare (ignore not-cond not-unsigned))
             `(progn
                ,@(mapcar
                   (lambda (suffix cost signed)
                     `(define-vop (,(symbolicate "FAST-IF-" tran suffix)
                                   ,(symbolicate "FAST-CONDITIONAL"  suffix))
                        (:translate ,tran)
                        (:conditional ,(if signed cond unsigned))
                        (:args-var x-tn-ref)
                        (:generator ,cost
                          (emit-optimized-cmp
                           x ,(if (eq suffix '-c/fixnum) `(fixnumize y) 'y)
                           temp (tn-ref-type x-tn-ref)))))
                   '(/fixnum -c/fixnum /signed -c/signed /unsigned -c/unsigned)
                   '(4 3 6 5 6 5)
                   '(t t t t nil nil)))))

  (define-conditional-vop < :l :b :ge :ae)
  (define-conditional-vop > :g :a :le :be))

(declaim (inline load-bignum-length))
(defun load-bignum-length (input output)
  ;; Unaligned load
  (inst mov :dword output (ea (- 1 other-pointer-lowtag) input)))

(define-vop (fast-if->-zero)
  (:args (x :scs (descriptor-reg)))
  (:arg-types integer (:constant (integer 0 0)))
  (:info target not-p y)
  (:ignore y)
  (:temporary (:sc unsigned-reg) temp)
  (:translate >)
  (:conditional)
  (:variant nil)
  (:variant-vars bignum-only)
  (:policy :fast-safe)
  (:generator 8
    (move temp x)
    (unless bignum-only
      (generate-fixnum-test temp)
      (inst jmp :nz BIGNUM)
      (inst test temp temp)
      (inst jmp (if not-p :le :g) target)
      (inst jmp DONE))
    BIGNUM
    (load-bignum-length x temp)
    (inst cmp :qword
          (ea (- (+ (* bignum-digits-offset n-word-bytes))
                 other-pointer-lowtag
                 n-word-bytes)
              x
              temp
              (ash 1 word-shift))
          0)
    (inst jmp (if not-p :l :ge) target)
    DONE))

(define-vop (fast-if->-zero-bignum fast-if->-zero)
  (:arg-types bignum (:constant (integer 0 0)))
  (:variant t))

(define-vop (fast-if-<-zero fast-if->-zero)
  (:info y)
  (:translate <)
  (:conditional :l)
  (:variant nil)
  (:generator 9
    (unless bignum-only
      (move temp x)
      (generate-fixnum-test temp)
      (inst jmp :z TEST))
    (load-bignum-length x temp)
    (inst mov temp (ea (- (* bignum-digits-offset n-word-bytes)
                          other-pointer-lowtag
                          n-word-bytes)
                       x
                       temp
                       (ash 1 word-shift)))
    TEST
    (inst test temp temp)))

(define-vop (fast-if-<-zero-bignum fast-if-<-zero)
  (:arg-types bignum (:constant (integer 0 0)))
  (:variant t))

(define-vop (fast-if-eql/signed fast-conditional/signed)
  (:translate eql %eql/integer)
  (:generator 6 (emit-optimized-cmp x y temp)))

(define-vop (fast-if-eql-c/signed fast-conditional-c/signed)
  (:translate eql %eql/integer)
  (:generator 5 (emit-optimized-cmp x y temp)))

(define-vop (fast-if-eql/unsigned fast-conditional/unsigned)
  (:translate eql %eql/integer)
  (:generator 6 (emit-optimized-cmp x y temp)))

(define-vop (fast-if-eql-c/unsigned fast-conditional-c/unsigned)
  (:translate eql %eql/integer)
  (:generator 5 (emit-optimized-cmp x y temp)))

;;; EQL/FIXNUM is funny because the first arg can be of any type, not just a
;;; known fixnum.

;;; These versions specify a fixnum restriction on their first arg. We have
;;; also generic-eql/fixnum VOPs which are the same, but have no restriction on
;;; the first arg and a higher cost. The reason for doing this is to prevent
;;; fixnum specific operations from being used on word integers, spuriously
;;; consing the argument.

(define-vop (fast-eql/fixnum fast-conditional)
  (:args (x :scs (any-reg control-stack))
         (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql %eql/integer)
  (:generator 4 (emit-optimized-cmp x y temp)))

(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg control-stack))
         (y :scs (any-reg control-stack)))
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional-c/fixnum)
  (:args (x :scs (any-reg control-stack)))
  (:arg-types tagged-num (:constant fixnum))
  (:info y)
  (:conditional :e)
  (:policy :fast-safe)
  (:translate eql %eql/integer)
  (:args-var x-tn-ref)
  (:generator 2 (emit-optimized-cmp x (fixnumize y) temp (tn-ref-type x-tn-ref))))

;;; FIXME: this seems never to be invoked any more. What did we either break or improve?
(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:args (x :scs (any-reg descriptor-reg control-stack)))
  (:arg-types * (:constant fixnum))
  (:variant-cost 6))

;;;; 64-bit logical operations

;;; Only the lower 6 bits of the shift amount are significant.
(macrolet ((define (translate operation)
             `(define-vop ()
                (:translate ,translate)
                (:note ,(string translate))
                (:policy :fast-safe)
                (:args (num :scs (unsigned-reg) :target r)
                       (amount :scs (signed-reg) :target rcx))
                (:arg-types unsigned-num tagged-num)
                (:temporary (:sc signed-reg :offset rcx-offset :from (:argument 1)) rcx)
                (:results (r :scs (unsigned-reg) :from (:argument 0)))
                (:result-types unsigned-num)
                (:generator 1
                 (move r num)
                 (move rcx amount :dword)
                 (inst ,operation r :cl)))))
  (define shift-towards-start shr)
  (define shift-towards-end   shl))

;;;; Modular functions

(defmacro define-mod-binop ((name prototype) function)
  (unless (search "FAST-*" (string prototype)) ; fast-* doesn't accept stack locations yet
    (return-from define-mod-binop
      `(define-vop (,name ,prototype)
         (:args (x :scs (unsigned-reg signed-reg unsigned-stack signed-stack immediate)
                   :load-if nil :target r)
                (y :scs (unsigned-reg signed-reg unsigned-stack signed-stack immediate)
                   :load-if nil))
         (:arg-types untagged-num untagged-num)
         (:results (r :scs (unsigned-reg signed-reg unsigned-stack signed-stack)
                      :load-if nil))
         (:result-types unsigned-num)
         (:translate ,function))))
  `(define-vop (,name ,prototype)
       (:args (x :target r :scs (unsigned-reg signed-reg)
                 :load-if (not (and (or (sc-is x unsigned-stack)
                                        (sc-is x signed-stack))
                                    (or (sc-is y unsigned-reg)
                                        (sc-is y signed-reg))
                                    (or (sc-is r unsigned-stack)
                                        (sc-is r signed-stack))
                                    (location= x r))))
              (y :scs (unsigned-reg signed-reg unsigned-stack signed-stack)))
     (:arg-types untagged-num untagged-num)
     (:results (r :scs (unsigned-reg signed-reg) :from (:argument 0)
                  :load-if (not (and (or (sc-is x unsigned-stack)
                                         (sc-is x signed-stack))
                                     (or (sc-is y unsigned-reg)
                                         (sc-is y unsigned-reg))
                                     (or (sc-is r unsigned-stack)
                                         (sc-is r unsigned-stack))
                                     (location= x r)))))
     (:result-types unsigned-num)
     (:translate ,function)))
(defmacro define-mod-binop-c ((name prototype) function)
  (unless (search "FAST-*" (string prototype)) ; fast-* doesn't accept stack locations yet
    (return-from define-mod-binop-c
      `(define-vop (,name ,prototype)
         (:args (x :target r :scs (unsigned-reg signed-reg unsigned-stack signed-stack) :load-if nil))
         (:info y)
         (:arg-types untagged-num (:constant (or (unsigned-byte 64) (signed-byte 64))))
         (:results (r :scs (unsigned-reg signed-reg unsigned-stack signed-stack) :load-if nil))
         (:result-types unsigned-num)
         (:translate ,function))))
  `(define-vop (,name ,prototype)
       (:args (x :target r :scs (unsigned-reg signed-reg)
                 :load-if t))
     (:info y)
     (:arg-types untagged-num (:constant (or (unsigned-byte 64) (signed-byte 64))))
     (:results (r :scs (unsigned-reg signed-reg) :from (:argument 0)
                  :load-if t))
     (:result-types unsigned-num)
     (:translate ,function)))

(macrolet ((def (name -c-p &aux (inherit name))
             (let ((fun64   (symbolicate name "-MOD64"))
                   (funfx   (symbolicate name "-MODFX"))
                   (vopu    (symbolicate "FAST-" inherit "/UNSIGNED=>UNSIGNED"))
                   (vopcu   (symbolicate "FAST-" inherit "-C/UNSIGNED=>UNSIGNED"))
                   (vopf    (symbolicate "FAST-" inherit "/FIXNUM=>FIXNUM"))
                   (vopcf   (symbolicate "FAST-" inherit "-C/FIXNUM=>FIXNUM"))
                   (vop64u  (symbolicate "FAST-" name "-MOD64/WORD=>UNSIGNED"))
                   (vop64f  (symbolicate "FAST-" name "-MOD64/FIXNUM=>FIXNUM"))
                   (vop64cu (symbolicate "FAST-" name "-MOD64-C/WORD=>UNSIGNED"))
                   (vop64cf (symbolicate "FAST-" name "-MOD64-C/FIXNUM=>FIXNUM"))
                   (vopfxf  (symbolicate "FAST-" name "-MODFX/FIXNUM=>FIXNUM"))
                   (vopfxcf (symbolicate "FAST-" name "-MODFX-C/FIXNUM=>FIXNUM")))
               (declare (ignore vop64cf)) ; maybe someone will want it some day
               `(progn
                  (define-modular-fun ,fun64 (x y) ,name :untagged nil 64)
                  (define-modular-fun ,funfx (x y) ,name :tagged t #.n-fixnum-bits)
                  (define-mod-binop (,vop64u ,vopu) ,fun64)
                  ;; This seems a bit lame. Could we not just have one vop
                  ;; which which takes any combination of signed/unsigned reg
                  ;; and which translates the normal function and the modular function?
                  (define-vop (,vop64f ,vopf) (:translate ,fun64))
                  (define-vop (,vopfxf ,vopf) (:translate ,funfx))
                  ,@(when -c-p
                      `((define-mod-binop-c (,vop64cu ,vopcu) ,fun64)
                        (define-vop (,vopfxcf ,vopcf) (:translate ,funfx))))))))
  (def + t)
  (def - t)
  (def * t))

(define-modular-fun %negate-mod64 (x) %negate :untagged nil 64)
(define-vop (%negate-mod64)
  (:translate %negate-mod64)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target r))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 3
    (move r x)
    (inst neg r)))

(define-modular-fun %negate-modfx (x) %negate :tagged t #.n-fixnum-bits)
(define-vop (%negate-modfx fast-negate/fixnum)
  (:translate %negate-modfx))
(in-package "SB-C")

(defknown sb-vm::%lea-mod64 (integer integer (member 1 2 4 8) (signed-byte 64))
  (unsigned-byte 64)
  (foldable flushable movable))
(defknown sb-vm::%lea-modfx (integer integer (member 1 2 4 8) (signed-byte 64))
  fixnum
  (foldable flushable movable))

(define-modular-fun-optimizer %lea ((base index scale disp) :untagged nil :width width)
  (when (and (<= width 64)
             (constant-lvar-p scale)
             (constant-lvar-p disp))
    (cut-to-width base :untagged width nil)
    (cut-to-width index :untagged width nil)
    'sb-vm::%lea-mod64))
(define-modular-fun-optimizer %lea ((base index scale disp) :tagged t :width width)
  (when (and (<= width sb-vm:n-fixnum-bits)
             (constant-lvar-p scale)
             (constant-lvar-p disp))
    (cut-to-width base :tagged width t)
    (cut-to-width index :tagged width t)
    'sb-vm::%lea-modfx))

#+sb-xc-host
(progn
  (defun sb-vm::%lea-mod64 (base index scale disp)
    (ldb (byte 64 0) (%lea base index scale disp)))
  (defun sb-vm::%lea-modfx (base index scale disp)
    (mask-signed-field sb-vm:n-fixnum-bits
                       (%lea base index scale disp))))
#-sb-xc-host
(progn
  (defun sb-vm::%lea-mod64 (base index scale disp)
    (let ((base (logand base #xffffffffffffffff))
          (index (logand index #xffffffffffffffff)))
      ;; can't use modular version of %LEA, as we only have VOPs for
      ;; constant SCALE and DISP.
      (ldb (byte 64 0) (+ base (* index scale) disp))))
  (defun sb-vm::%lea-modfx (base index scale disp)
    (let* ((fixnum-width sb-vm:n-fixnum-bits)
           (base (mask-signed-field fixnum-width base))
           (index (mask-signed-field fixnum-width index)))
      ;; can't use modular version of %LEA, as we only have VOPs for
      ;; constant SCALE and DISP.
      (mask-signed-field fixnum-width (+ base (* index scale) disp)))))

(in-package "SB-VM")

(define-vop (%lea-mod64/unsigned=>unsigned
             %lea/unsigned=>unsigned)
  (:translate %lea-mod64))
(define-vop (%lea-modfx/fixnum=>fixnum
             %lea/fixnum=>fixnum)
  (:translate %lea-modfx))

;;; logical operations
(define-modular-fun lognot-mod64 (x) lognot :untagged nil 64)
(define-vop (lognot-mod64/unsigned=>unsigned)
  (:translate lognot-mod64)
  (:args (x :scs (unsigned-reg unsigned-stack) :target r
            :load-if (not (and (sc-is x unsigned-stack)
                               (sc-is r unsigned-stack)
                               (location= x r)))))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)
               :load-if (not (and (sc-is x unsigned-stack)
                                  (sc-is r unsigned-stack)
                                  (location= x r)))))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (move r x)
    (inst not r)))

(define-source-transform logeqv (&rest args)
  (if (oddp (length args))
      `(logxor ,@args)
      `(lognot (logxor ,@args))))
(define-source-transform logandc1 (x y)
  `(logand (lognot ,x) ,y))
(define-source-transform logandc2 (x y)
  `(logand ,x (lognot ,y)))
(define-source-transform logorc1 (x y)
  `(logior (lognot ,x) ,y))
(define-source-transform logorc2 (x y)
  `(logior ,x (lognot ,y)))
(define-source-transform lognor (x y)
  `(lognot (logior ,x ,y)))
(define-source-transform lognand (x y)
  `(lognot (logand ,x ,y)))

;;;; bignum stuff

(define-vop (bignum-length get-header-data)
  (:translate sb-bignum:%bignum-length)
  (:policy :fast-safe))

(define-vop (bignum-set-length set-header-data)
  (:translate sb-bignum:%bignum-set-length)
  (:policy :fast-safe))

#-bignum-assertions ; %BIGNUM-ref is an inline function if compiling with assertions
(define-full-reffer bignum-ref * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb-bignum:%bignum-ref)
#-bignum-assertions ; does not get called if compiling with assertions
(define-full-reffer+addend bignum-ref-with-offset * bignum-digits-offset
  other-pointer-lowtag (unsigned-reg) unsigned-num
  sb-bignum:%bignum-ref-with-offset)
(define-full-setter bignum-set * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num
  #+bignum-assertions sb-bignum:%%bignum-set
  #-bignum-assertions sb-bignum:%bignum-set)

(define-vop (digit-0-or-plus)
  (:translate sb-bignum:%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:conditional :ns)
  (:generator 3
    (inst test digit digit)))


;;; For add and sub with carry, the sc of carry argument is unsigned-reg
;;; or any-reg so that it may be passed either as tagged or untagged.
;;; This is easy to deal with and may save a fixnum-word conversion.
(define-vop (add-w/carry)
  (:translate sb-bignum:%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg) :target result)
         (b :scs (unsigned-reg unsigned-stack) :to :eval)
         (c :scs (any-reg unsigned-reg control-stack) :target temp))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:temporary (:sc any-reg :from (:argument 2) :to :eval) temp)
  (:results (result :scs (unsigned-reg) :from (:argument 0))
            (carry :scs (unsigned-reg)))
  (:optional-results carry)
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (move result a)
    (move temp c)
    (inst neg temp) ; Set the carry flag to 0 if c=0 else to 1
    (inst adc result b)
    (unless (eq (tn-kind carry) :unused)
     (inst set :c carry)
     (inst and :dword carry 1))))

;;; Note: the borrow is 1 for no borrow and 0 for a borrow, the opposite
;;; of the x86-64 convention.
(define-vop (sub-w/borrow)
  (:translate sb-bignum:%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg) :to :eval :target result)
         (b :scs (unsigned-reg unsigned-stack) :to :result)
         (c :scs (any-reg unsigned-reg control-stack)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg) :from :eval)
            (borrow :scs (unsigned-reg)))
  (:optional-results borrow)
  (:result-types unsigned-num positive-fixnum)
  (:generator 5
    (inst cmp c 1) ; Set the carry flag to 1 if c=0 else to 0
    (move result a)
    (inst sbb result b)
    (unless (eq (tn-kind borrow) :unused)
     (inst mov borrow 1)
     (inst sbb :dword borrow 0))))

(define-vop (bignum-mult-and-add-3-arg)
  (:translate sb-bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target eax)
         (y :scs (unsigned-reg unsigned-stack))
         (carry-in :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset rax-offset :from (:argument 0)
                   :to (:result 1) :target lo) eax)
  (:temporary (:sc unsigned-reg :offset rdx-offset :from (:argument 1)
                   :to (:result 0) :target hi) edx)
  (:results (hi :scs (unsigned-reg))
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 20
    (move eax x)
    (inst mul eax y)
    (inst add eax carry-in)
    (inst adc edx 0)
    (move hi edx)
    (move lo eax)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate sb-bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target eax)
         (y :scs (unsigned-reg unsigned-stack))
         (prev :scs (unsigned-reg unsigned-stack))
         (carry-in :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset rax-offset :from (:argument 0)
                   :to (:result 1) :target lo) eax)
  (:temporary (:sc unsigned-reg :offset rdx-offset :from (:argument 1)
                   :to (:result 0) :target hi) edx)
  (:results (hi :scs (unsigned-reg))
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 20
    (move eax x)
    (inst mul eax y)
    (inst add eax prev)
    (inst adc edx 0)
    (inst add eax carry-in)
    (inst adc edx 0)
    (move hi edx)
    (move lo eax)))


(define-vop (bignum-mult)
  (:translate sb-bignum:%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target eax)
         (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset rax-offset :from (:argument 0)
                   :to (:result 1) :target lo) eax)
  (:temporary (:sc unsigned-reg :offset rdx-offset :from (:argument 1)
                   :to (:result 0) :target hi) edx)
  (:results (hi :scs (unsigned-reg))
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 20
    (move eax x)
    (inst mul eax y)
    (move hi edx)
    (move lo eax)))

(define-vop (mulhi)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target eax)
         (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset rax-offset :from (:argument 0))
              eax)
  (:temporary (:sc unsigned-reg :offset rdx-offset :from (:argument 1)
                   :to (:result 0) :target hi) edx)
  (:results (hi :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 20
    (move eax x)
    (inst mul eax y)
    (move hi edx)))

(define-vop (mulhi/fx)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (any-reg) :target eax)
         (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types positive-fixnum unsigned-num)
  (:temporary (:sc any-reg :offset rax-offset :from (:argument 0)) eax)
  (:temporary (:sc any-reg :offset rdx-offset :from (:argument 1)
                   :to (:result 0) :target hi) edx)
  (:results (hi :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 15
    (move eax x)
    (inst mul eax y)
    (move hi edx)
    (inst and hi (lognot fixnum-tag-mask))))

(define-vop ()
  (:translate %signed-multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (signed-reg) :target eax)
         (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:temporary (:sc signed-reg :offset rax-offset :from (:argument 0))
              eax)
  (:temporary (:sc signed-reg :offset rdx-offset :from (:argument 1)
                   :to (:result 0) :target hi) edx)
  (:results (hi :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 20
    (move eax x)
    (inst imul y)
    (move hi edx)))

(define-vop (bignum-lognot lognot-mod64/unsigned=>unsigned)
  (:translate sb-bignum:%lognot))

(define-vop (fixnum-to-digit)
  (:translate sb-bignum:%fixnum-to-digit)
  (:policy :fast-safe)
  (:args (fixnum :scs (any-reg control-stack) :target digit))
  (:arg-types tagged-num)
  (:results (digit :scs (unsigned-reg)
                   :load-if (not (and (sc-is fixnum control-stack)
                                      (sc-is digit unsigned-stack)
                                      (location= fixnum digit)))))
  (:result-types unsigned-num)
  (:generator 1
    (move digit fixnum)
    (inst sar digit n-fixnum-tag-bits)))

(define-vop (bignum-floor)
  (:translate sb-bignum:%bigfloor)
  (:policy :fast-safe)
  (:args (div-high :scs (unsigned-reg) :target edx)
         (div-low :scs (unsigned-reg) :target eax)
         (divisor :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset rax-offset :from (:argument 1)
                   :to (:result 0) :target quo) eax)
  (:temporary (:sc unsigned-reg :offset rdx-offset :from (:argument 0)
                   :to (:result 1) :target rem) edx)
  (:results (quo :scs (unsigned-reg))
            (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 300
    (move edx div-high)
    (move eax div-low)
    (inst div eax divisor)
    (move quo eax)
    (move rem edx)))

(define-vop (signify-digit)
  (:translate sb-bignum:%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg unsigned-stack) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg signed-reg)
                 :load-if (not (and (sc-is digit unsigned-stack)
                                    (sc-is res control-stack signed-stack)
                                    (location= digit res)))))
  (:result-types signed-num)
  (:generator 1
    (move res digit)
    (when (sc-is res any-reg control-stack)
      (inst shl res n-fixnum-tag-bits))))

(define-vop (digit-ashr)
  (:translate sb-bignum:%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg unsigned-stack) :target result)
         (count :scs (unsigned-reg) :target ecx))
  (:arg-types unsigned-num positive-fixnum)
  (:temporary (:sc unsigned-reg :offset rcx-offset :from (:argument 1)) ecx)
  (:results (result :scs (unsigned-reg) :from (:argument 0)
                    :load-if (not (and (sc-is result unsigned-stack)
                                       (location= digit result)))))
  (:result-types unsigned-num)
  (:generator 2
    (move result digit)
    (move ecx count)
    (inst sar result :cl)))

(define-vop (digit-ashr/c)
  (:translate sb-bignum:%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg unsigned-stack) :target result))
  (:arg-types unsigned-num (:constant (integer 0 63)))
  (:info count)
  (:results (result :scs (unsigned-reg) :from (:argument 0)
                    :load-if (not (and (sc-is result unsigned-stack)
                                       (location= digit result)))))
  (:result-types unsigned-num)
  (:generator 1
    (move result digit)
    (inst sar result count)))

(define-vop (digit-lshr digit-ashr)
  (:translate sb-bignum:%digit-logical-shift-right)
  (:generator 1
    (move result digit)
    (move ecx count)
    (inst shr result :cl)))

(define-vop (digit-ashl digit-ashr)
  (:translate sb-bignum:%ashl)
  (:generator 1
    (move result digit)
    (move ecx count)
    (inst shl result :cl)))

(define-vop (logand-bignum/c)
  (:translate logand)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:arg-types bignum (:constant word))
  (:results (r :scs (unsigned-reg)))
  (:info mask)
  (:result-types unsigned-num)
  (:generator 4
     (cond ((or (plausible-signed-imm32-operand-p mask)
                (location= x r))
            (loadw r x bignum-digits-offset other-pointer-lowtag)
            (unless (or (eql mask -1)
                        (eql mask (ldb (byte n-word-bits 0) -1)))
              (inst and r (constantize mask))))
           (t
            (inst mov r mask)
            (inst and r (object-slot-ea x bignum-digits-offset other-pointer-lowtag))))))

;; Specialised mask-signed-field VOPs.
(flet ((shift-unshift (reg width)
         (let ((shift (- n-word-bits width)))
           ;; Shift of 64 is effectively a shift of 0 due to masking by the CPU.
           ;; It can't happen, because size = 0 was dealt with in IR1
           (aver (/= shift 64))
           (unless (= shift 0)
             (inst shl reg shift)
             (inst sar reg shift)))))
 (define-vop (mask-signed-field-word/c)
   (:translate sb-c::mask-signed-field)
   (:policy :fast-safe)
   (:args (x :scs (signed-reg unsigned-reg) :target r))
   (:arg-types (:constant (integer 0 64)) untagged-num)
   (:results (r :scs (signed-reg)))
   (:result-types signed-num)
   (:info width)
   (:generator 3
     (case width
       ((8 16 32)
        (inst movsx `(,(bits->size width) :qword) r x))
       (t
        (move r x)
        (shift-unshift r width)))))

 (define-vop (mask-signed-field-bignum/c)
   (:translate sb-c::mask-signed-field)
   (:policy :fast-safe)
   (:args (x :scs (descriptor-reg) :target r))
   (:arg-types (:constant (integer 0 64)) bignum)
   (:results (r :scs (signed-reg)))
   (:result-types signed-num)
   (:info width)
   (:generator 4
     (case width
       ((8 16 32)
        (inst movsx `(,(bits->size width) :qword)
              r
              (ea (- (* bignum-digits-offset n-word-bytes) other-pointer-lowtag) x)))
       (t
        (loadw r x bignum-digits-offset other-pointer-lowtag)
        (shift-unshift r width))))))

(define-vop (mask-signed-field-fixnum)
  (:translate sb-c::mask-signed-field)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target r))
  (:arg-types (:constant (eql #.n-fixnum-bits)) t)
  (:results (r :scs (any-reg)))
  (:result-types fixnum)
  (:info width)
  (:ignore width)
  (:generator 5
    (move r x)
    (generate-fixnum-test r)
    (inst jmp :z DONE)
    (loadw r r bignum-digits-offset other-pointer-lowtag)
    (inst shl r (- n-word-bits n-fixnum-bits))
    DONE))

(define-vop (logand-word-mask)
  (:translate logand)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:arg-types t (:constant word))
  (:results (r :scs (unsigned-reg)))
  (:info mask)
  (:result-types unsigned-num)
  (:generator 10
    (let ((fixnum-mask-p (and (= n-fixnum-tag-bits 1)
                              (= mask (ash most-positive-word -1)))))
      (assemble ()
        (move r x)
        (generate-fixnum-test r)
        (inst jmp :nz BIGNUM)
        (if fixnum-mask-p
            (inst shr r n-fixnum-tag-bits)
            (inst sar r n-fixnum-tag-bits))
        (inst jmp DONE)
        BIGNUM
        (loadw r x bignum-digits-offset other-pointer-lowtag)
        (when fixnum-mask-p
          (inst btr r (1- n-word-bits)))
        DONE
        (unless (or fixnum-mask-p
                    (= mask most-positive-word))
          (inst and r (or (plausible-signed-imm32-operand-p mask)
                          (constantize mask))))))))

(in-package "SB-C")

(defun *-transformer (y node fun)
  (cond
    ((= y (ash 1 (integer-length y)))
     ;; there's a generic transform for y = 2^k
     (give-up-ir1-transform))
    ((member y '(3 5 9))
     ;; we can do these multiplications directly using LEA
     (delay-ir1-transform node :constraint)
     `(,fun x x ,(1- y) 0))
    (t
     ;; A normal 64-bit multiplication takes 4 cycles on Athlon 64/Opteron.
     ;; Optimizing multiplications (other than the above cases) to
     ;; shifts/adds/leas gives a maximum improvement of 1 cycle, but requires
     ;; quite a lot of hairy code.
     (give-up-ir1-transform))))

;; These transforms were exceptionally noisy in an unhelpful way.
;; Reading the output would not induce the speed-conscious programmer to think
;; "I'd better code this multiply as (* (* B 2) 9) instead of (* B 18)
;;  so that the LEA transform kicks in".
(deftransform * ((x y)
                 ((unsigned-byte 64) (constant-arg (unsigned-byte 64)))
                 (unsigned-byte 64)
                 :important nil
                 :node node)
  "recode as leas, shifts and adds"
  (*-transformer (lvar-value y) node '%lea))
(deftransform sb-vm::*-mod64
    ((x y) ((unsigned-byte 64) (constant-arg (unsigned-byte 64)))
     (unsigned-byte 64)
     :important nil
     :node node)
  "recode as leas, shifts and adds"
  (*-transformer (lvar-value y) node 'sb-vm::%lea-mod64))

(deftransform * ((x y)
                 (fixnum (constant-arg (unsigned-byte 64)))
                 fixnum
                 :important nil
                 :node node)
  "recode as leas, shifts and adds"
  (*-transformer (lvar-value y) node '%lea))
(deftransform sb-vm::*-modfx
    ((x y) (fixnum (constant-arg (unsigned-byte 64)))
     fixnum
     :important nil
     :node node)
  "recode as leas, shifts and adds"
  (*-transformer (lvar-value y) node 'sb-vm::%lea-modfx))

(defun exactly-one-read-p (results)
  (when results
    (let ((refs (tn-reads (tn-ref-tn results))))
      ;; How can REFS be NIL? I don't understand.
      (and refs (not (tn-ref-next refs))))))

;;; When writing to a bitfield, msan tracks precisely which of
;;; the bits in a byte have been written, as mentioned in the paper:
;;; (https://static.googleusercontent.com/media/research.google.com/en//pubs/archive/43308.pdf)
;;; "For example, bit shifts and bit logic operations are often used
;;; to extract individual field from bitfields. As adjacent fields
;;; may be not initialized, it is important that the result shadow
;;; matches the exact bits occupied by a particular field."
;;; SAP-REF- has to respect the exactness by not complaining about bits which
;;; have not been written if the intent is not to read them.
;;; So given a VOP which is some sap-ref, if the result flows into a LOGAND,
;;; then it's a masked load and we do not read the shadow of the
;;; bits that are masked off.
(defun masked-memory-load-p (vop)
  (let ((next-vop (vop-next vop))
        next-next-vop)
    (case (and next-vop (vop-info-name (vop-info next-vop)))
      ((sb-vm::fast-logand-c/unsigned=>unsigned
        sb-vm::fast-logand-c/signed-unsigned=>unsigned)
       (when (exactly-one-read-p (vop-results vop))
         (car (vop-codegen-info next-vop))))
      (sb-vm::move-from-word/fixnum
       ;; The result of this vop has to have exactly 1 read
       ;; (the MOVE-FROM-WORD) and the result of that has to
       ;; have exactly one read (the LOGAND)
       (when (and (exactly-one-read-p (vop-results vop))
                  (setq next-next-vop (vop-next next-vop))
                  (eq (vop-info-name (vop-info next-next-vop))
                      'sb-vm::fast-logand-c/fixnum=>fixnum)
                  (exactly-one-read-p (vop-results next-vop)))
         (car (vop-codegen-info next-next-vop)))))))
