;;;; the VM definition of arithmetic VOPs for the x86-64

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")


;; If chopping X to 32 bits and sign-extending is equal to the original X,
;; return the chopped X, which the CPU will always treat as signed.
;; Notably this allows MOST-POSITIVE-WORD to be an immediate constant.
(defun immediate32-p (x)
  (typecase x
    ((signed-byte 32) x)
    ((unsigned-byte 64)
     (let ((chopped (sb!c::mask-signed-field 32 x)))
       (and (= x (ldb (byte 64 0) chopped))
            chopped)))
    (t nil)))

;; If 'immediate32-p' is true, use it; otherwise use a RIP-relative constant.
;; I couldn't think of a more accurate name for this other than maybe
;; 'signed-immediate32-or-rip-relativize' which is just too awful.
(defun constantize (x)
  (or (immediate32-p x)
      (register-inline-constant :qword x)))

;;;; unary operations

(define-vop (fast-safe-arith-op)
  (:policy :fast-safe)
  (:effects)
  (:affected))

(define-vop (fixnum-unop fast-safe-arith-op)
  (:args (x :scs (any-reg) :target res))
  (:results (res :scs (any-reg)))
  (:note "inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num))

(define-vop (signed-unop fast-safe-arith-op)
  (:args (x :scs (signed-reg) :target res))
  (:results (res :scs (signed-reg)))
  (:note "inline (signed-byte 64) arithmetic")
  (:arg-types signed-num)
  (:result-types signed-num))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (move res x)
    (inst neg res)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (move res x)
    (inst neg res)))

(define-vop (fast-negate/unsigned signed-unop)
  (:args (x :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:translate %negate)
  (:generator 3
    (move res x)
    (inst neg res)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:translate lognot)
  (:generator 1
    (move res x)
    (inst xor res (fixnumize -1))))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 2
    (move res x)
    (inst not res)))

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
                         `((move r x)
                           (inst ,op r (constantize (fixnumize y)))))))
                (define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
                             fast-signed-binop)
                  (:translate ,translate)
                  (:generator ,(1+ untagged-penalty)
                   ,@(or signed=>signed `((move r x) (inst ,op r y)))))
                (define-vop (,(symbolicate 'fast- translate '-c/signed=>signed)
                             fast-signed-binop-c)
                  (:translate ,translate)
                  (:generator ,untagged-penalty
                   ,@(or c/signed=>signed
                         `((move r x) (inst ,op r (constantize y))))))
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

  ;;(define-binop + 4 add)
  (define-binop - 4 sub)

  ;; The following have microoptimizations for some special cases
  ;; not caught by the front end.

  (define-binop logand 2 and
    :c/unsigned=>unsigned
    ((move r x)
     (let ((y (constantize y)))
       ;; ANDing with #xFFFF_FFFF_FFFF_FFFF is a no-op, other than
       ;; the eflags state which we don't care about.
       (unless (eql y -1) ; do nothing if this is true
         (inst and r y)))))

  (define-binop logior 2 or
    :c/unsigned=>unsigned
    ((let ((y (constantize y)))
       (cond ((and (register-p r) (eql y -1)) ; special-case "OR reg, all-ones"
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
  (:args (x :scs (unsigned-reg))
         (y :target r :scs (signed-reg)))
  (:arg-types unsigned-num signed-num)
  (:results (r :scs (signed-reg) :from (:argument 1)))
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

;;; Special handling of add on the x86; can use lea to avoid a
;;; register load, otherwise it uses add.
;;; FIXME: either inherit from fast-foo-binop or explain why not.
(define-vop (fast-+/fixnum=>fixnum fast-safe-arith-op)
  (:translate +)
  (:args (x :scs (any-reg) :target r
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
  (:note "inline fixnum arithmetic")
  (:generator 2
    (cond ((and (sc-is x any-reg) (sc-is y any-reg) (sc-is r any-reg)
                (not (location= x r)))
           (inst lea r (make-ea :qword :base x :index y :scale 1)))
          (t
           (move r x)
           (inst add r y)))))

(define-vop (fast-+-c/fixnum=>fixnum fast-safe-arith-op)
  (:translate +)
  (:args (x :target r :scs (any-reg) :load-if t))
  (:info y)
  (:arg-types tagged-num (:constant fixnum))
  (:results (r :scs (any-reg) :load-if t))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic")
  (:generator 1
    (let ((y (fixnumize y)))
      (cond ((and (not (location= x r))
                  (typep y '(signed-byte 32)))
             (inst lea r (make-ea :qword :base x :disp y)))
            (t
             (move r x)
             (inst add r (constantize y)))))))

(define-vop (fast-+/signed=>signed fast-safe-arith-op)
  (:translate +)
  (:args (x :scs (signed-reg) :target r
            :load-if (not (and (sc-is x signed-stack)
                               (sc-is y signed-reg)
                               (sc-is r signed-stack)
                               (location= x r))))
         (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg) :from (:argument 0)
               :load-if (not (and (sc-is x signed-stack)
                                  (sc-is y signed-reg)
                                  (location= x r)))))
  (:result-types signed-num)
  (:note "inline (signed-byte 64) arithmetic")
  (:generator 5
    (cond ((and (sc-is x signed-reg) (sc-is y signed-reg) (sc-is r signed-reg)
                (not (location= x r)))
           (inst lea r (make-ea :qword :base x :index y :scale 1)))
          (t
           (move r x)
           (inst add r y)))))

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


(define-vop (fast-+-c/signed=>signed fast-safe-arith-op)
  (:translate +)
  (:args (x :target r :scs (signed-reg)
            :load-if (or (not (typep y '(signed-byte 32)))
                         (not (sc-is r signed-reg signed-stack)))))
  (:info y)
  (:arg-types signed-num (:constant (signed-byte 64)))
  (:results (r :scs (signed-reg)
               :load-if (or (not (location= x r))
                            (not (typep y '(signed-byte 32))))))
  (:result-types signed-num)
  (:note "inline (signed-byte 64) arithmetic")
  (:generator 4
    (cond ((and (sc-is x signed-reg) (sc-is r signed-reg)
                (not (location= x r))
                (typep y '(signed-byte 32)))
           (inst lea r (make-ea :qword :base x :disp y)))
          (t
           (move r x)
           (cond ((= y 1)
                  (inst inc r))
                 (t
                  (inst add r (constantize y))))))))

(define-vop (fast-+/unsigned=>unsigned fast-safe-arith-op)
  (:translate +)
  (:args (x :scs (unsigned-reg) :target r
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
  (:note "inline (unsigned-byte 64) arithmetic")
  (:generator 5
    (cond ((and (sc-is x unsigned-reg) (sc-is y unsigned-reg)
                (sc-is r unsigned-reg) (not (location= x r)))
           (inst lea r (make-ea :qword :base x :index y :scale 1)))
          (t
           (move r x)
           (inst add r y)))))

(define-vop (fast-+-c/unsigned=>unsigned fast-safe-arith-op)
  (:translate +)
  (:args (x :target r :scs (unsigned-reg)
            :load-if (or (not (typep y '(unsigned-byte 31)))
                         (not (sc-is x unsigned-reg unsigned-stack)))))
  (:info y)
  (:arg-types unsigned-num (:constant (unsigned-byte 64)))
  (:results (r :scs (unsigned-reg)
               :load-if (or (not (location= x r))
                            (not (typep y '(unsigned-byte 31))))))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:generator 4
    (cond ((and (sc-is x unsigned-reg) (sc-is r unsigned-reg)
                (not (location= x r))
                (typep y '(unsigned-byte 31)))
           (inst lea r (make-ea :qword :base x :disp y)))
          (t
           (move r x)
           (cond ((= y 1)
                  (inst inc r))
                 (t
                  (inst add r (constantize y))))))))

;;;; multiplication and division

(define-vop (fast-*/fixnum=>fixnum fast-safe-arith-op)
  (:translate *)
  ;; We need different loading characteristics.
  (:args (x :scs (any-reg) :target r)
         (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg) :from (:argument 0)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic")
  (:generator 4
    (move r x)
    (inst sar r n-fixnum-tag-bits)
    (inst imul r y)))

(define-vop (fast-*-c/fixnum=>fixnum fast-safe-arith-op)
  (:translate *)
  ;; We need different loading characteristics.
  (:args (x :scs (any-reg)
            :load-if (or (not (typep y '(signed-byte 32)))
                         (not (sc-is x any-reg control-stack)))))
  (:info y)
  (:arg-types tagged-num (:constant fixnum))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic")
  (:generator 3
    (cond ((typep y '(signed-byte 32))
           (inst imul r x y))
          (t
           (move r x)
           (inst imul r (register-inline-constant :qword y))))))

(define-vop (fast-*/signed=>signed fast-safe-arith-op)
  (:translate *)
  ;; We need different loading characteristics.
  (:args (x :scs (signed-reg) :target r)
         (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:note "inline (signed-byte 64) arithmetic")
  (:generator 5
    (move r x)
    (inst imul r y)))

(define-vop (fast-*-c/signed=>signed fast-safe-arith-op)
  (:translate *)
  ;; We need different loading characteristics.
  (:args (x :scs (signed-reg)
            :load-if (or (not (typep y '(signed-byte 32)))
                         (not (sc-is x signed-reg signed-stack)))))
  (:info y)
  (:arg-types signed-num (:constant (signed-byte 64)))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 64) arithmetic")
  (:generator 4
    (cond ((typep y '(signed-byte 32))
           (inst imul r x y))
          (t
           (move r x)
           (inst imul r (register-inline-constant :qword y))))))

(define-vop (fast-*/unsigned=>unsigned fast-safe-arith-op)
  (:translate *)
  (:args (x :scs (unsigned-reg) :target eax)
         (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :target r
                   :from (:argument 0) :to :result) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset
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
  (:temporary (:sc unsigned-reg :offset eax-offset :target r
                   :from (:argument 0) :to :result) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset
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
  (:arg-types tagged-num tagged-num)
  (:temporary (:sc signed-reg :offset eax-offset :target quo
                   :from (:argument 0) :to (:result 0)) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :target rem
                   :from (:argument 0) :to (:result 1)) edx)
  (:results (quo :scs (any-reg))
            (rem :scs (any-reg)))
  (:result-types tagged-num tagged-num)
  (:note "inline fixnum arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 31
    (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
      (if (sc-is y any-reg)
          (inst test y y)  ; smaller instruction
          (inst cmp y 0))
      (inst jmp :eq zero))
    (move eax x)
    (inst cqo)
    (inst idiv eax y)
    (if (location= quo eax)
        (inst shl eax n-fixnum-tag-bits)
        (if (= n-fixnum-tag-bits 1)
            (inst lea quo (make-ea :qword :base eax :index eax))
            (inst lea quo (make-ea :qword :index eax
                                   :scale (ash 1 n-fixnum-tag-bits)))))
    (move rem edx)))

(define-vop (fast-truncate-c/fixnum=>fixnum fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (any-reg) :target eax))
  (:info y)
  (:arg-types tagged-num (:constant fixnum))
  (:temporary (:sc signed-reg :offset eax-offset :target quo
                   :from :argument :to (:result 0)) eax)
  (:temporary (:sc any-reg :offset edx-offset :target rem
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
            (inst lea quo (make-ea :qword :base eax :index eax))
            (inst lea quo (make-ea :qword :index eax
                                   :scale (ash 1 n-fixnum-tag-bits)))))
    (move rem edx)))

(define-vop (fast-truncate/unsigned=>unsigned fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (unsigned-reg) :target eax)
         (y :scs (unsigned-reg signed-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :target quo
                   :from (:argument 0) :to (:result 0)) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :target rem
                   :from (:argument 0) :to (:result 1)) edx)
  (:results (quo :scs (unsigned-reg))
            (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 33
    (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
      (if (sc-is y unsigned-reg)
          (inst test y y)  ; smaller instruction
          (inst cmp y 0))
      (inst jmp :eq zero))
    (move eax x)
    (inst xor edx edx)
    (inst div eax y)
    (move quo eax)
    (move rem edx)))

(define-vop (fast-truncate-c/unsigned=>unsigned fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (unsigned-reg) :target eax))
  (:info y)
  (:arg-types unsigned-num (:constant (unsigned-byte 64)))
  (:temporary (:sc unsigned-reg :offset eax-offset :target quo
                   :from :argument :to (:result 0)) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :target rem
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
    (inst xor edx edx)
    (inst mov y-arg y)
    (inst div eax y-arg)
    (move quo eax)
    (move rem edx)))

(define-vop (fast-truncate/signed=>signed fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg) :target eax)
         (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:temporary (:sc signed-reg :offset eax-offset :target quo
                   :from (:argument 0) :to (:result 0)) eax)
  (:temporary (:sc signed-reg :offset edx-offset :target rem
                   :from (:argument 0) :to (:result 1)) edx)
  (:results (quo :scs (signed-reg))
            (rem :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:note "inline (signed-byte 64) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 33
    (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
      (if (sc-is y signed-reg)
          (inst test y y)  ; smaller instruction
          (inst cmp y 0))
      (inst jmp :eq zero))
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
  (:temporary (:sc signed-reg :offset eax-offset :target quo
                   :from :argument :to (:result 0)) eax)
  (:temporary (:sc signed-reg :offset edx-offset :target rem
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
(define-vop (fast-ash-c/fixnum=>fixnum)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (any-reg) :target result
                 :load-if (not (and (sc-is number any-reg control-stack)
                                    (sc-is result any-reg control-stack)
                                    (location= number result)))))
  (:info amount)
  (:arg-types tagged-num (:constant integer))
  (:results (result :scs (any-reg)
                    :load-if (not (and (sc-is number control-stack)
                                       (sc-is result control-stack)
                                       (location= number result)))))
  (:result-types tagged-num)
  (:note "inline ASH")
  (:variant nil)
  (:variant-vars modularp)
  (:generator 2
    (cond ((and (= amount 1) (not (location= number result)))
           (inst lea result (make-ea :qword :base number :index number)))
          ((and (= amount 2) (not (location= number result)))
           (inst lea result (make-ea :qword :index number :scale 4)))
          ((and (= amount 3) (not (location= number result)))
           (inst lea result (make-ea :qword :index number :scale 8)))
          (t
           (move result number)
           (cond ((< -64 amount 64)
                  ;; this code is used both in ASH and ASH-MODFX, so
                  ;; be careful
                  (if (plusp amount)
                      (inst shl result amount)
                      (progn
                        (inst sar result (- amount))
                        (inst and result (lognot fixnum-tag-mask)))))
                 ;; shifting left (zero fill)
                 ((plusp amount)
                  (unless modularp
                    (aver (not "Impossible: fixnum ASH should not be called with
constant shift greater than word length")))
                  (if (sc-is result any-reg)
                      (zeroize result)
                      (inst mov result 0)))
                 ;; shifting right (sign fill)
                 (t (inst sar result 63)
                    (inst and result (lognot fixnum-tag-mask))))))))

(define-vop (fast-ash-left/fixnum=>fixnum)
  (:translate ash)
  (:args (number :scs (any-reg) :target result
                 :load-if (not (and (sc-is number control-stack)
                                    (sc-is result control-stack)
                                    (location= number result))))
         (amount :scs (unsigned-reg) :target ecx))
  (:arg-types tagged-num positive-fixnum)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (result :scs (any-reg) :from (:argument 0)
                    :load-if (not (and (sc-is number control-stack)
                                       (sc-is result control-stack)
                                       (location= number result)))))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:note "inline ASH")
  (:generator 3
    (move result number)
    (move ecx amount)
    ;; The result-type ensures us that this shift will not overflow.
    (inst shl result :cl)))

(define-vop (fast-ash-c/signed=>signed)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg) :target result
                 :load-if (not (and (sc-is number signed-stack)
                                    (sc-is result signed-stack)
                                    (location= number result)))))
  (:info amount)
  (:arg-types signed-num (:constant integer))
  (:results (result :scs (signed-reg)
                    :load-if (not (and (sc-is number signed-stack)
                                       (sc-is result signed-stack)
                                       (location= number result)))))
  (:result-types signed-num)
  (:note "inline ASH")
  (:generator 3
    (cond ((and (= amount 1) (not (location= number result)))
           (inst lea result (make-ea :qword :base number :index number)))
          ((and (= amount 2) (not (location= number result)))
           (inst lea result (make-ea :qword :index number :scale 4)))
          ((and (= amount 3) (not (location= number result)))
           (inst lea result (make-ea :qword :index number :scale 8)))
          (t
           (move result number)
           (cond ((plusp amount) (inst shl result amount))
                 (t (inst sar result (min 63 (- amount)))))))))

(define-vop (fast-ash-c/unsigned=>unsigned)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg) :target result
                 :load-if (not (and (sc-is number unsigned-stack)
                                    (sc-is result unsigned-stack)
                                    (location= number result)))))
  (:info amount)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg)
                    :load-if (not (and (sc-is number unsigned-stack)
                                       (sc-is result unsigned-stack)
                                       (location= number result)))))
  (:result-types unsigned-num)
  (:note "inline ASH")
  (:generator 3
    (cond ((and (= amount 1) (not (location= number result)))
           (inst lea result (make-ea :qword :base number :index number)))
          ((and (= amount 2) (not (location= number result)))
           (inst lea result (make-ea :qword :index number :scale 4)))
          ((and (= amount 3) (not (location= number result)))
           (inst lea result (make-ea :qword :index number :scale 8)))
          (t
           (move result number)
           (cond ((< -64 amount 64) ;; XXXX
                  ;; this code is used both in ASH and ASH-MOD64, so
                  ;; be careful
                  (if (plusp amount)
                      (inst shl result amount)
                      (inst shr result (- amount))))
                 (t (if (sc-is result unsigned-reg)
                        (zeroize result)
                        (inst mov result 0))))))))

(define-vop (fast-ash-left/signed=>signed)
  (:translate ash)
  (:args (number :scs (signed-reg) :target result
                 :load-if (not (and (sc-is number signed-stack)
                                    (sc-is result signed-stack)
                                    (location= number result))))
         (amount :scs (unsigned-reg) :target ecx))
  (:arg-types signed-num positive-fixnum)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (result :scs (signed-reg) :from (:argument 0)
                    :load-if (not (and (sc-is number signed-stack)
                                       (sc-is result signed-stack)
                                       (location= number result)))))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:note "inline ASH")
  (:generator 4
    (move result number)
    (move ecx amount)
    (inst shl result :cl)))

(define-vop (fast-ash-left/unsigned=>unsigned)
  (:translate ash)
  (:args (number :scs (unsigned-reg) :target result
                 :load-if (not (and (sc-is number unsigned-stack)
                                    (sc-is result unsigned-stack)
                                    (location= number result))))
         (amount :scs (unsigned-reg) :target ecx))
  (:arg-types unsigned-num positive-fixnum)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (result :scs (unsigned-reg) :from (:argument 0)
                    :load-if (not (and (sc-is number unsigned-stack)
                                       (sc-is result unsigned-stack)
                                       (location= number result)))))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:note "inline ASH")
  (:generator 4
    (move result number)
    (move ecx amount)
    (inst shl result :cl)))

(define-vop (fast-ash/signed=>signed)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg) :target result)
         (amount :scs (signed-reg) :target ecx))
  (:arg-types signed-num signed-num)
  (:results (result :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:note "inline ASH")
  (:generator 5
    (move result number)
    (move ecx amount)
    (inst test ecx ecx)
    (inst jmp :ns POSITIVE)
    (inst neg ecx)
    (inst cmp ecx 63)
    (inst jmp :be OKAY)
    (inst mov ecx 63)
    OKAY
    (inst sar result :cl)
    (inst jmp DONE)

    POSITIVE
    ;; The result-type ensures us that this shift will not overflow.
    (inst shl result :cl)

    DONE))

(define-vop (fast-ash/unsigned=>unsigned)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg) :target result)
         (amount :scs (signed-reg) :target ecx))
  (:arg-types unsigned-num signed-num)
  (:results (result :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:note "inline ASH")
  (:generator 5
    (move result number)
    (move ecx amount)
    (inst test ecx ecx)
    (inst jmp :ns POSITIVE)
    (inst neg ecx)
    (inst cmp ecx 63)
    (inst jmp :be OKAY)
    (zeroize result)
    (inst jmp DONE)
    OKAY
    (inst shr result :cl)
    (inst jmp DONE)

    POSITIVE
    ;; The result-type ensures us that this shift will not overflow.
    (inst shl result :cl)

    DONE))

#!+ash-right-vops
(define-vop (fast-%ash/right/unsigned)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg) :target result)
         (amount :scs (unsigned-reg) :target rcx))
  (:arg-types unsigned-num unsigned-num)
  (:results (result :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:temporary (:sc signed-reg :offset rcx-offset :from (:argument 1)) rcx)
  (:generator 4
    (move result number)
    (move rcx amount)
    (inst shr result :cl)))

#!+ash-right-vops
(define-vop (fast-%ash/right/signed)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg) :target result)
         (amount :scs (unsigned-reg) :target rcx))
  (:arg-types signed-num unsigned-num)
  (:results (result :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:temporary (:sc signed-reg :offset rcx-offset :from (:argument 1)) rcx)
  (:generator 4
    (move result number)
    (move rcx amount)
    (inst sar result :cl)))

#!+ash-right-vops
(define-vop (fast-%ash/right/fixnum)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (any-reg) :target result)
         (amount :scs (unsigned-reg) :target rcx))
  (:arg-types tagged-num unsigned-num)
  (:results (result :scs (any-reg) :from (:argument 0)))
  (:result-types tagged-num)
  (:temporary (:sc signed-reg :offset rcx-offset :from (:argument 1)) rcx)
  (:generator 3
    (move result number)
    (move rcx amount)
    (inst sar result :cl)
    (inst and result (lognot fixnum-tag-mask))))

(in-package "SB!C")

(defknown %lea (integer integer (member 1 2 4 8 16) (signed-byte 64))
  integer
  (foldable flushable movable))

(defoptimizer (%lea derive-type) ((base index scale disp))
  (when (and (constant-lvar-p scale)
             (constant-lvar-p disp))
    (let ((scale (lvar-value scale))
          (disp (lvar-value disp))
          (base-type (lvar-type base))
          (index-type (lvar-type index)))
      (when (and (numeric-type-p base-type)
                 (numeric-type-p index-type))
        (let ((base-lo (numeric-type-low base-type))
              (base-hi (numeric-type-high base-type))
              (index-lo (numeric-type-low index-type))
              (index-hi (numeric-type-high index-type)))
          (make-numeric-type :class 'integer
                             :complexp :real
                             :low (when (and base-lo index-lo)
                                    (+ base-lo (* index-lo scale) disp))
                             :high (when (and base-hi index-hi)
                                     (+ base-hi (* index-hi scale) disp))))))))

(defun %lea (base index scale disp)
  (+ base (* index scale) disp))

(in-package "SB!VM")

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
    (inst lea r (make-ea :qword :base base :index index
                         :scale scale :disp disp))))

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
    (inst lea r (make-ea :qword :base base :index index
                         :scale scale :disp disp))))

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
    (inst lea r (make-ea :qword :base base :index index
                         :scale scale :disp disp))))

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
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
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
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg)
            :load-if (not (and (sc-is x control-stack)
                               (sc-is y any-reg))))
         (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg) :load-if t))
  (:arg-types tagged-num (:constant fixnum))
  (:info y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg)
            :load-if (not (and (sc-is x signed-stack)
                               (sc-is y signed-reg))))
         (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 64) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg) :load-if t))
  (:arg-types signed-num (:constant (signed-byte 64)))
  (:info y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg)
            :load-if (not (and (sc-is x unsigned-stack)
                               (sc-is y unsigned-reg))))
         (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 64) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg) :load-if t))
  (:arg-types unsigned-num (:constant (unsigned-byte 64)))
  (:info y))

;; Stolen liberally from the x86 32-bit implementation.
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
                           ,(case suffix
                             (-c/fixnum
                              `(constantize (fixnumize y)))
                             ((-c/signed -c/unsigned)
                              `(constantize y))
                             (t
                              'y)))))))))
  (define-logtest-vops))

(defknown %logbitp (integer unsigned-byte) boolean
  (movable foldable flushable always-translatable))

;;; only for constant folding within the compiler
(defun %logbitp (integer index)
  (logbitp index integer))

;;; too much work to do the non-constant case (maybe?)
(define-vop (fast-logbitp-c/fixnum fast-conditional-c/fixnum)
  (:translate %logbitp)
  (:conditional :c)
  (:arg-types tagged-num (:constant (integer 0 #.(- 63 n-fixnum-tag-bits))))
  (:generator 4
    (inst bt x (+ y n-fixnum-tag-bits))))

(define-vop (fast-logbitp/signed fast-conditional/signed)
  (:args (x :scs (signed-reg signed-stack))
         (y :scs (signed-reg)))
  (:translate %logbitp)
  (:conditional :c)
  (:generator 6
    (inst bt x y)))

(define-vop (fast-logbitp-c/signed fast-conditional-c/signed)
  (:translate %logbitp)
  (:conditional :c)
  (:arg-types signed-num (:constant (integer 0 63)))
  (:generator 5
    (inst bt x y)))

(define-vop (fast-logbitp/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg unsigned-stack))
         (y :scs (unsigned-reg)))
  (:translate %logbitp)
  (:conditional :c)
  (:generator 6
    (inst bt x y)))

(define-vop (fast-logbitp-c/unsigned fast-conditional-c/unsigned)
  (:translate %logbitp)
  (:conditional :c)
  (:arg-types unsigned-num (:constant (integer 0 63)))
  (:generator 5
    (inst bt x y)))

(macrolet ((define-conditional-vop (tran cond unsigned not-cond not-unsigned)
             (declare (ignore not-cond not-unsigned))
             `(progn
                ,@(mapcar
                   (lambda (suffix cost signed)
                     `(define-vop (,(symbolicate "FAST-IF-" tran suffix)
                                   ,(symbolicate "FAST-CONDITIONAL"  suffix))
                        (:translate ,tran)
                        (:conditional ,(if signed cond unsigned))
                        (:generator ,cost
                          (cond ((and (sc-is x any-reg signed-reg unsigned-reg)
                                      (eql y 0))
                                 (inst test x x))
                                (t
                                 (inst cmp x
                                       ,(case suffix
                                          (-c/fixnum
                                           `(constantize (fixnumize y)))
                                          ((-c/signed -c/unsigned)
                                           `(constantize y))
                                          (t 'y))))))))
                   '(/fixnum -c/fixnum /signed -c/signed /unsigned -c/unsigned)
;                  '(/fixnum  /signed  /unsigned)
                   '(4 3 6 5 6 5)
                   '(t t t t nil nil)))))

  (define-conditional-vop < :l :b :ge :ae)
  (define-conditional-vop > :g :a :le :be))

(define-vop (fast-if-eql/signed fast-conditional/signed)
  (:translate eql %eql/integer)
  (:generator 6
    (inst cmp x y)))

(define-vop (fast-if-eql-c/signed fast-conditional-c/signed)
  (:translate eql %eql/integer)
  (:generator 5
    (cond ((and (sc-is x signed-reg) (zerop y))
           (inst test x x))  ; smaller instruction
          (t
           (inst cmp x (constantize y))))))

(define-vop (fast-if-eql/unsigned fast-conditional/unsigned)
  (:translate eql %eql/integer)
  (:generator 6
    (inst cmp x y)))

(define-vop (fast-if-eql-c/unsigned fast-conditional-c/unsigned)
  (:translate eql %eql/integer)
  (:generator 5
    (cond ((and (sc-is x unsigned-reg) (zerop y))
           (inst test x x))  ; smaller instruction
          (t
           (inst cmp x (constantize y))))))

;;; EQL/FIXNUM is funny because the first arg can be of any type, not just a
;;; known fixnum.

;;; These versions specify a fixnum restriction on their first arg. We have
;;; also generic-eql/fixnum VOPs which are the same, but have no restriction on
;;; the first arg and a higher cost. The reason for doing this is to prevent
;;; fixnum specific operations from being used on word integers, spuriously
;;; consing the argument.

(define-vop (fast-eql/fixnum fast-conditional)
  (:args (x :scs (any-reg)
            :load-if (not (and (sc-is x control-stack)
                               (sc-is y any-reg))))
         (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql %eql/integer)
  (:generator 4
    (inst cmp x y)))

(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg)
            :load-if (not (and (sc-is x control-stack)
                               (sc-is y any-reg))))
         (y :scs (any-reg control-stack)))
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional-c/fixnum)
  (:args (x :scs (any-reg) :load-if t))
  (:arg-types tagged-num (:constant fixnum))
  (:info y)
  (:conditional :e)
  (:policy :fast-safe)
  (:translate eql %eql/integer)
  (:generator 2
    (cond ((and (sc-is x any-reg descriptor-reg) (zerop y))
           (inst test x x))  ; smaller instruction
          (t
           (inst cmp x (constantize (fixnumize y)))))))

(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:args (x :scs (any-reg descriptor-reg) :load-if t))
  (:arg-types * (:constant fixnum))
  (:variant-cost 6))

;;;; 64-bit logical operations

;;; Only the lower 6 bits of the shift amount are significant.
(define-vop (shift-towards-someplace)
  (:policy :fast-safe)
  (:args (num :scs (unsigned-reg) :target r)
         (amount :scs (signed-reg) :target ecx))
  (:arg-types unsigned-num tagged-num)
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (r :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num))

(define-vop (shift-towards-start shift-towards-someplace)
  (:translate shift-towards-start)
  (:note "SHIFT-TOWARDS-START")
  (:generator 1
    (move r num)
    (move ecx amount)
    (inst shr r :cl)))

(define-vop (shift-towards-end shift-towards-someplace)
  (:translate shift-towards-end)
  (:note "SHIFT-TOWARDS-END")
  (:generator 1
    (move r num)
    (move ecx amount)
    (inst shl r :cl)))

;;;; Modular functions

(defmacro define-mod-binop ((name prototype) function)
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
  `(define-vop (,name ,prototype)
       (:args (x :target r :scs (unsigned-reg signed-reg)
                 :load-if t))
     (:info y)
     (:arg-types untagged-num (:constant (or (unsigned-byte 64) (signed-byte 64))))
     (:results (r :scs (unsigned-reg signed-reg) :from (:argument 0)
                  :load-if t))
     (:result-types unsigned-num)
     (:translate ,function)))

(macrolet ((def (name -c-p)
             (let ((fun64 (intern (format nil "~S-MOD64" name)))
                   (vopu (intern (format nil "FAST-~S/UNSIGNED=>UNSIGNED" name)))
                   (vopcu (intern (format nil "FAST-~S-C/UNSIGNED=>UNSIGNED" name)))
                   (vopf (intern (format nil "FAST-~S/FIXNUM=>FIXNUM" name)))
                   (vopcf (intern (format nil "FAST-~S-C/FIXNUM=>FIXNUM" name)))
                   (vop64u (intern (format nil "FAST-~S-MOD64/WORD=>UNSIGNED" name)))
                   (vop64f (intern (format nil "FAST-~S-MOD64/FIXNUM=>FIXNUM" name)))
                   (vop64cu (intern (format nil "FAST-~S-MOD64-C/WORD=>UNSIGNED" name)))
                   (vop64cf (intern (format nil "FAST-~S-MOD64-C/FIXNUM=>FIXNUM" name)))
                   (funfx (intern (format nil "~S-MODFX" name)))
                   (vopfxf (intern (format nil "FAST-~S-MODFX/FIXNUM=>FIXNUM" name)))
                   (vopfxcf (intern (format nil "FAST-~S-MODFX-C/FIXNUM=>FIXNUM" name))))
               (declare (ignore vop64cf)) ; maybe someone will want it some day
               `(progn
                  (define-modular-fun ,fun64 (x y) ,name :untagged nil 64)
                  (define-modular-fun ,funfx (x y) ,name :tagged t
                                      #.(- n-word-bits n-fixnum-tag-bits))
                  (define-mod-binop (,vop64u ,vopu) ,fun64)
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

(define-modular-fun %negate-modfx (x) %negate :tagged t #.(- n-word-bits
                                                             n-fixnum-tag-bits))
(define-vop (%negate-modfx fast-negate/fixnum)
  (:translate %negate-modfx))

(define-vop (fast-ash-left-mod64-c/unsigned=>unsigned
             fast-ash-c/unsigned=>unsigned)
  (:translate ash-left-mod64))
(define-vop (fast-ash-left-mod64/unsigned=>unsigned
             fast-ash-left/unsigned=>unsigned))
(deftransform ash-left-mod64 ((integer count)
                              ((unsigned-byte 64) (unsigned-byte 6)))
  (when (sb!c::constant-lvar-p count)
    (sb!c::give-up-ir1-transform))
  '(%primitive fast-ash-left-mod64/unsigned=>unsigned integer count))

(define-vop (fast-ash-left-modfx-c/fixnum=>fixnum
             fast-ash-c/fixnum=>fixnum)
  (:variant :modular)
  (:translate ash-left-modfx))
(define-vop (fast-ash-left-modfx/fixnum=>fixnum
             fast-ash-left/fixnum=>fixnum))
(deftransform ash-left-modfx ((integer count)
                              (fixnum (unsigned-byte 6)))
  (when (sb!c::constant-lvar-p count)
    (sb!c::give-up-ir1-transform))
  '(%primitive fast-ash-left-modfx/fixnum=>fixnum integer count))

(in-package "SB!C")

(defknown sb!vm::%lea-mod64 (integer integer (member 1 2 4 8) (signed-byte 64))
  (unsigned-byte 64)
  (foldable flushable movable))
(defknown sb!vm::%lea-modfx (integer integer (member 1 2 4 8) (signed-byte 64))
  fixnum
  (foldable flushable movable))

(define-modular-fun-optimizer %lea ((base index scale disp) :untagged nil :width width)
  (when (and (<= width 64)
             (constant-lvar-p scale)
             (constant-lvar-p disp))
    (cut-to-width base :untagged width nil)
    (cut-to-width index :untagged width nil)
    'sb!vm::%lea-mod64))
(define-modular-fun-optimizer %lea ((base index scale disp) :tagged t :width width)
  (when (and (<= width (- sb!vm:n-word-bits sb!vm:n-fixnum-tag-bits))
             (constant-lvar-p scale)
             (constant-lvar-p disp))
    (cut-to-width base :tagged width t)
    (cut-to-width index :tagged width t)
    'sb!vm::%lea-modfx))

#+sb-xc-host
(progn
  (defun sb!vm::%lea-mod64 (base index scale disp)
    (ldb (byte 64 0) (%lea base index scale disp)))
  (defun sb!vm::%lea-modfx (base index scale disp)
    (mask-signed-field (- sb!vm:n-word-bits sb!vm:n-fixnum-tag-bits)
                       (%lea base index scale disp))))
#-sb-xc-host
(progn
  (defun sb!vm::%lea-mod64 (base index scale disp)
    (let ((base (logand base #xffffffffffffffff))
          (index (logand index #xffffffffffffffff)))
      ;; can't use modular version of %LEA, as we only have VOPs for
      ;; constant SCALE and DISP.
      (ldb (byte 64 0) (+ base (* index scale) disp))))
  (defun sb!vm::%lea-modfx (base index scale disp)
    (let* ((fixnum-width (- sb!vm:n-word-bits sb!vm:n-fixnum-tag-bits))
           (base (mask-signed-field fixnum-width base))
           (index (mask-signed-field fixnum-width index)))
      ;; can't use modular version of %LEA, as we only have VOPs for
      ;; constant SCALE and DISP.
      (mask-signed-field fixnum-width (+ base (* index scale) disp)))))

(in-package "SB!VM")

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
  (:translate sb!bignum:%bignum-length)
  (:policy :fast-safe))

(define-vop (bignum-set-length set-header-data)
  (:translate sb!bignum:%bignum-set-length)
  (:policy :fast-safe))

(define-full-reffer bignum-ref * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb!bignum:%bignum-ref)
(define-full-reffer+offset bignum-ref-with-offset * bignum-digits-offset
  other-pointer-lowtag (unsigned-reg) unsigned-num
  sb!bignum:%bignum-ref-with-offset)
(define-full-setter bignum-set * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb!bignum:%bignum-set)

(define-vop (digit-0-or-plus)
  (:translate sb!bignum:%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:conditional :ns)
  (:generator 3
    (inst test digit digit)))


;;; For add and sub with carry the sc of carry argument is any-reg so
;;; that it may be passed as a fixnum or word and thus may be 0, 1, or
;;; 8. This is easy to deal with and may save a fixnum-word
;;; conversion.
(define-vop (add-w/carry)
  (:translate sb!bignum:%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg) :target result)
         (b :scs (unsigned-reg unsigned-stack) :to :eval)
         (c :scs (any-reg) :target temp))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:temporary (:sc any-reg :from (:argument 2) :to :eval) temp)
  (:results (result :scs (unsigned-reg) :from (:argument 0))
            (carry :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (move result a)
    (move temp c)
    (inst neg temp) ; Set the carry flag to 0 if c=0 else to 1
    (inst adc result b)
    (inst mov carry 0)
    (inst adc carry carry)))

;;; Note: the borrow is 1 for no borrow and 0 for a borrow, the opposite
;;; of the x86-64 convention.
(define-vop (sub-w/borrow)
  (:translate sb!bignum:%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg) :to :eval :target result)
         (b :scs (unsigned-reg unsigned-stack) :to :result)
         (c :scs (any-reg control-stack)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg) :from :eval)
            (borrow :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:generator 5
    (inst cmp c 1) ; Set the carry flag to 1 if c=0 else to 0
    (move result a)
    (inst sbb result b)
    (inst mov borrow 1)
    (inst sbb borrow 0)))


(define-vop (bignum-mult-and-add-3-arg)
  (:translate sb!bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target eax)
         (y :scs (unsigned-reg unsigned-stack))
         (carry-in :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 0)
                   :to (:result 1) :target lo) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :from (:argument 1)
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
  (:translate sb!bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target eax)
         (y :scs (unsigned-reg unsigned-stack))
         (prev :scs (unsigned-reg unsigned-stack))
         (carry-in :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 0)
                   :to (:result 1) :target lo) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :from (:argument 1)
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
  (:translate sb!bignum:%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target eax)
         (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 0)
                   :to (:result 1) :target lo) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :from (:argument 1)
                   :to (:result 0) :target hi) edx)
  (:results (hi :scs (unsigned-reg))
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 20
    (move eax x)
    (inst mul eax y)
    (move hi edx)
    (move lo eax)))

#!+multiply-high-vops
(define-vop (mulhi)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target eax)
         (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 0))
              eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :from (:argument 1)
                   :to (:result 0) :target hi) edx)
  (:results (hi :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 20
    (move eax x)
    (inst mul eax y)
    (move hi edx)))

#!+multiply-high-vops
(define-vop (mulhi/fx)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (any-reg) :target eax)
         (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types positive-fixnum unsigned-num)
  (:temporary (:sc any-reg :offset eax-offset :from (:argument 0)) eax)
  (:temporary (:sc any-reg :offset edx-offset :from (:argument 1)
                   :to (:result 0) :target hi) edx)
  (:results (hi :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 15
    (move eax x)
    (inst mul eax y)
    (move hi edx)
    (inst and hi (lognot fixnum-tag-mask))))

(define-vop (bignum-lognot lognot-mod64/unsigned=>unsigned)
  (:translate sb!bignum:%lognot))

(define-vop (fixnum-to-digit)
  (:translate sb!bignum:%fixnum-to-digit)
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
  (:translate sb!bignum:%bigfloor)
  (:policy :fast-safe)
  (:args (div-high :scs (unsigned-reg) :target edx)
         (div-low :scs (unsigned-reg) :target eax)
         (divisor :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 1)
                   :to (:result 0) :target quo) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :from (:argument 0)
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
  (:translate sb!bignum:%fixnum-digit-with-correct-sign)
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
  (:translate sb!bignum:%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg unsigned-stack) :target result)
         (count :scs (unsigned-reg) :target ecx))
  (:arg-types unsigned-num positive-fixnum)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (result :scs (unsigned-reg) :from (:argument 0)
                    :load-if (not (and (sc-is result unsigned-stack)
                                       (location= digit result)))))
  (:result-types unsigned-num)
  (:generator 2
    (move result digit)
    (move ecx count)
    (inst sar result :cl)))

(define-vop (digit-ashr/c)
  (:translate sb!bignum:%ashr)
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
  (:translate sb!bignum:%digit-logical-shift-right)
  (:generator 1
    (move result digit)
    (move ecx count)
    (inst shr result :cl)))

(define-vop (digit-ashl digit-ashr)
  (:translate sb!bignum:%ashl)
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
     (cond ((or (immediate32-p mask)
                (location= x r))
            (loadw r x bignum-digits-offset other-pointer-lowtag)
            (unless (or (eql mask -1)
                        (eql mask (ldb (byte n-word-bits 0) -1)))
              (inst and r (constantize mask))))
           (t
            (inst mov r mask)
            (inst and r (make-ea-for-object-slot x
                                                 bignum-digits-offset
                                                 other-pointer-lowtag))))))

;; Specialised mask-signed-field VOPs.
(define-vop (mask-signed-field-word/c)
  (:translate sb!c::mask-signed-field)
  (:policy :fast-safe)
  (:args (x :scs (signed-reg unsigned-reg) :target r))
  (:arg-types (:constant (integer 0 64)) untagged-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:info width)
  (:generator 3
    (cond ((zerop width)
           (zeroize r))
          ((= width 64)
           (move r x))
          ((member width '(32 16 8))
           (inst movsx r (reg-in-size x (ecase width
                                             (32 :dword)
                                             (16 :word)
                                             (8  :byte)))))
          (t
           (move r x)
           (let ((delta (- n-word-bits width)))
             (inst shl r delta)
             (inst sar r delta))))))

(define-vop (mask-signed-field-bignum/c)
  (:translate sb!c::mask-signed-field)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target r))
  (:arg-types (:constant (integer 0 64)) bignum)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:info width)
  (:generator 4
    (cond ((zerop width)
           (zeroize r))
          ((member width '(8 16 32 64))
           (ecase width
             (64 (loadw r x bignum-digits-offset other-pointer-lowtag))
             ((32 16 8)
              (inst movsx r (make-ea (ecase width (32 :dword) (16 :word) (8 :byte))
                                     :base x
                                     :disp (- (* bignum-digits-offset n-word-bytes)
                                              other-pointer-lowtag))))))
          (t
           (loadw r x bignum-digits-offset other-pointer-lowtag)
           (let ((delta (- n-word-bits width)))
             (inst shl r delta)
             (inst sar r delta))))))

;;;; static functions

(define-static-fun two-arg-/ (x y) :translate /)

(define-static-fun two-arg-gcd (x y) :translate gcd)
(define-static-fun two-arg-lcm (x y) :translate lcm)

(define-static-fun two-arg-and (x y) :translate logand)
(define-static-fun two-arg-ior (x y) :translate logior)
(define-static-fun two-arg-xor (x y) :translate logxor)


(in-package "SB!C")

(defun *-transformer (y)
  (cond
    ((= y (ash 1 (integer-length y)))
     ;; there's a generic transform for y = 2^k
     (give-up-ir1-transform))
    ((member y '(3 5 9))
     ;; we can do these multiplications directly using LEA
     `(%lea x x ,(1- y) 0))
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
                 :important nil)
  "recode as leas, shifts and adds"
  (let ((y (lvar-value y)))
    (*-transformer y)))
(deftransform sb!vm::*-mod64
    ((x y) ((unsigned-byte 64) (constant-arg (unsigned-byte 64)))
     (unsigned-byte 64)
     :important nil)
  "recode as leas, shifts and adds"
  (let ((y (lvar-value y)))
    (*-transformer y)))

(deftransform * ((x y)
                 (fixnum (constant-arg (unsigned-byte 64)))
                 fixnum
                 :important nil)
  "recode as leas, shifts and adds"
  (let ((y (lvar-value y)))
    (*-transformer y)))
(deftransform sb!vm::*-modfx
    ((x y) (fixnum (constant-arg (unsigned-byte 64)))
     fixnum
     :important nil)
  "recode as leas, shifts and adds"
  (let ((y (lvar-value y)))
    (*-transformer y)))
