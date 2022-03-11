;;;; the VM definition of arithmetic VOPs for the x86

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; unary operations

(define-vop (fast-safe-arith-op)
  (:policy :fast-safe))

(define-vop (fixnum-unop fast-safe-arith-op)
  (:args (x :scs (any-reg) :target res))
  (:results (res :scs (any-reg)))
  (:note "inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num))

(define-vop (signed-unop fast-safe-arith-op)
  (:args (x :scs (signed-reg) :target res))
  (:results (res :scs (signed-reg)))
  (:note "inline (signed-byte 32) arithmetic")
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

(define-vop (fast-negate/signed-unsigned signed-unop)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
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
  (:note "inline (unsigned-byte 32) arithmetic"))

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
  (:note "inline (signed-byte 32) arithmetic"))

(define-vop (fast-fixnum-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg control-stack)))
  (:info y)
  (:arg-types tagged-num (:constant (signed-byte 30)))
  (:results (r :scs (any-reg)
               :load-if (not (location= x r))))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg unsigned-stack)))
  (:info y)
  (:arg-types unsigned-num (:constant (unsigned-byte 32)))
  (:results (r :scs (unsigned-reg)
               :load-if (not (location= x r))))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg signed-stack)))
  (:info y)
  (:arg-types signed-num (:constant (signed-byte 32)))
  (:results (r :scs (signed-reg)
               :load-if (not (location= x r))))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic"))

(macrolet ((define-binop (translate untagged-penalty op)
             `(progn
                (define-vop (,(symbolicate "FAST-" translate "/FIXNUM=>FIXNUM")
                             fast-fixnum-binop)
                  (:translate ,translate)
                  (:generator 2
                              (move r x)
                              (inst ,op r y)))
                (define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum)
                             fast-fixnum-binop-c)
                  (:translate ,translate)
                  (:generator 1
                  (move r x)
                  (inst ,op r (fixnumize y))))
                (define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
                             fast-signed-binop)
                  (:translate ,translate)
                  (:generator ,(1+ untagged-penalty)
                  (move r x)
                  (inst ,op r y)))
                (define-vop (,(symbolicate 'fast- translate '-c/signed=>signed)
                             fast-signed-binop-c)
                  (:translate ,translate)
                  (:generator ,untagged-penalty
                  (move r x)
                  (inst ,op r y)))
                (define-vop (,(symbolicate "FAST-"
                                           translate
                                           "/UNSIGNED=>UNSIGNED")
                fast-unsigned-binop)
                  (:translate ,translate)
                  (:generator ,(1+ untagged-penalty)
                  (move r x)
                  (inst ,op r y)))
                (define-vop (,(symbolicate 'fast-
                                           translate
                                           '-c/unsigned=>unsigned)
                             fast-unsigned-binop-c)
                  (:translate ,translate)
                  (:generator ,untagged-penalty
                  (move r x)
                  ,(if (eq translate 'logand)
                       ;; for the -C/UNSIGNED=>UNSIGNED VOP, this case
                       ;; is optimized away as an identity somewhere
                       ;; along the lines.  However, this VOP is used in
                       ;; -C/SIGNED=>UNSIGNED, below, when the
                       ;; higher-level lisp code can't optimize away the
                       ;; non-trivial identity.
                       `(unless (= y most-positive-word)
                          (inst ,op r y))
                       `(inst ,op r y)))))))
  (define-binop - 4 sub)
  (define-binop logand 2 and)
  (define-binop logior 2 or)
  (define-binop logxor 2 xor))

(define-vop (fast-logior-unsigned-signed=>signed fast-safe-arith-op)
  (:args (x :scs (unsigned-reg) :to (:result 1))
         (y :target r :scs (signed-reg)))
  (:arg-types unsigned-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (unsigned-byte 32) arithmetic")
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
  (:note "inline (unsigned-byte 32) arithmetic")
  (:translate logior)
  (:generator 3
    (move r x)
    (inst or r y)))

;;; Special handling of add on the x86; can use lea to avoid a
;;; register load, otherwise it uses add.
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
           (inst lea r (make-ea :dword :base x :index y)))
          (t
           (move r x)
           (inst add r y)))))

(define-vop (fast-+-c/fixnum=>fixnum fast-safe-arith-op)
  (:translate +)
  (:args (x :target r :scs (any-reg control-stack)))
  (:info y)
  (:arg-types tagged-num (:constant (signed-byte 30)))
  (:results (r :scs (any-reg)
               :load-if (not (location= x r))))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic")
  (:generator 1
    (cond ((and (sc-is x any-reg) (sc-is r any-reg) (not (location= x r)))
           (inst lea r (make-ea :dword :base x :disp (fixnumize y))))
          (t
           (move r x)
           (inst add r (fixnumize y))))))

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
  (:note "inline (signed-byte 32) arithmetic")
  (:generator 5
    (cond ((and (sc-is x signed-reg) (sc-is y signed-reg) (sc-is r signed-reg)
                (not (location= x r)))
           (inst lea r (make-ea :dword :base x :index y)))
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

(define-vop (fast-logand-c/signed-unsigned=>unsigned
             fast-logand-c/unsigned=>unsigned)
  (:args (x :target r :scs (signed-reg signed-stack)))
  (:arg-types signed-num (:constant (unsigned-byte 32))))

(define-vop (fast-logand/unsigned-signed=>unsigned
             fast-logand/unsigned=>unsigned)
  (:args (x :target r :scs (unsigned-reg)
            :load-if (not (and (sc-is x unsigned-stack)
                               (sc-is y signed-reg)
                               (sc-is r unsigned-stack)
                               (location= x r))))
         (y :scs (signed-reg signed-stack)))
  (:arg-types unsigned-num signed-num))

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
    (move r x)
    (generate-fixnum-test r)
    (inst jmp :nz BIGNUM)
    (inst sar r n-fixnum-tag-bits)
    (inst jmp DONE)
    BIGNUM
    (loadw r x bignum-digits-offset other-pointer-lowtag)
    DONE
    (unless (= mask most-positive-word)
      (inst and r mask))))


(define-vop (fast-+-c/signed=>signed fast-safe-arith-op)
  (:translate +)
  (:args (x :target r :scs (signed-reg signed-stack)))
  (:info y)
  (:arg-types signed-num (:constant (signed-byte 32)))
  (:results (r :scs (signed-reg)
               :load-if (not (location= x r))))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:generator 4
    (cond ((and (sc-is x signed-reg) (sc-is r signed-reg)
                (not (location= x r)))
           (inst lea r (make-ea :dword :base x :disp y)))
          (t
           (move r x)
           (if (= y 1)
               (inst inc r)
             (inst add r y))))))

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
  (:note "inline (unsigned-byte 32) arithmetic")
  (:generator 5
    (cond ((and (sc-is x unsigned-reg) (sc-is y unsigned-reg)
                (sc-is r unsigned-reg) (not (location= x r)))
           (inst lea r (make-ea :dword :base x :index y)))
          (t
           (move r x)
           (inst add r y)))))

(define-vop (fast-+-c/unsigned=>unsigned fast-safe-arith-op)
  (:translate +)
  (:args (x :target r :scs (unsigned-reg unsigned-stack)))
  (:info y)
  (:arg-types unsigned-num (:constant (unsigned-byte 32)))
  (:results (r :scs (unsigned-reg)
               :load-if (not (location= x r))))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:generator 4
    (cond ((and (sc-is x unsigned-reg) (sc-is r unsigned-reg)
                (not (location= x r)))
           (inst lea r (make-ea :dword :base x :disp y)))
          (t
           (move r x)
           (if (= y 1)
               (inst inc r)
             (inst add r y))))))

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
  (:args (x :scs (any-reg control-stack)))
  (:info y)
  (:arg-types tagged-num (:constant (signed-byte 30)))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic")
  (:generator 3
    (inst imul r x y)))

(define-vop (fast-*/signed=>signed fast-safe-arith-op)
  (:translate *)
  ;; We need different loading characteristics.
  (:args (x :scs (signed-reg) :target r)
         (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:generator 5
    (move r x)
    (inst imul r y)))

(define-vop (fast-*-c/signed=>signed fast-safe-arith-op)
  (:translate *)
  ;; We need different loading characteristics.
  (:args (x :scs (signed-reg signed-stack)))
  (:info y)
  (:arg-types signed-num (:constant (signed-byte 32)))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:generator 4
    (inst imul r x y)))

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
  (:note "inline (unsigned-byte 32) arithmetic")
  (:vop-var vop)
  (:generator 6
    (move eax x)
    (inst mul eax y)
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
    (inst cdq)
    (inst idiv eax y)
    (if (location= quo eax)
        (inst shl eax n-fixnum-tag-bits)
        (inst lea quo (make-ea :dword :index eax
                               :scale (ash 1 n-fixnum-tag-bits))))
    (move rem edx)))

(define-vop (fast-truncate-c/fixnum=>fixnum fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (any-reg) :target eax))
  (:info y)
  (:arg-types tagged-num (:constant (signed-byte 30)))
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
  (:generator 30
    (move eax x)
    (inst cdq)
    (inst mov y-arg (fixnumize y))
    (inst idiv eax y-arg)
    (if (location= quo eax)
        (inst shl eax n-fixnum-tag-bits)
        (inst lea quo (make-ea :dword :index eax
                               :scale (ash 1 n-fixnum-tag-bits))))
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
  (:note "inline (unsigned-byte 32) arithmetic")
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
  (:arg-types unsigned-num (:constant (unsigned-byte 32)))
  (:temporary (:sc unsigned-reg :offset eax-offset :target quo
                   :from :argument :to (:result 0)) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :target rem
                   :from :eval :to (:result 1)) edx)
  (:temporary (:sc unsigned-reg :from :eval :to :result) y-arg)
  (:results (quo :scs (unsigned-reg))
            (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:vop-var vop)
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
  (:note "inline (signed-byte 32) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 33
    (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
      (if (sc-is y signed-reg)
          (inst test y y)  ; smaller instruction
          (inst cmp y 0))
      (inst jmp :eq zero))
    (move eax x)
    (inst cdq)
    (inst idiv eax y)
    (move quo eax)
    (move rem edx)))

(define-vop (fast-truncate-c/signed=>signed fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg) :target eax))
  (:info y)
  (:arg-types signed-num (:constant (signed-byte 32)))
  (:temporary (:sc signed-reg :offset eax-offset :target quo
                   :from :argument :to (:result 0)) eax)
  (:temporary (:sc signed-reg :offset edx-offset :target rem
                   :from :eval :to (:result 1)) edx)
  (:temporary (:sc signed-reg :from :eval :to :result) y-arg)
  (:results (quo :scs (signed-reg))
            (rem :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:vop-var vop)
  (:generator 32
    (move eax x)
    (inst cdq)
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
           (inst lea result (make-ea :dword :base number :index number)))
          ((and (= amount 2) (not (location= number result)))
           (inst lea result (make-ea :dword :index number :scale 4)))
          ((and (= amount 3) (not (location= number result)))
           (inst lea result (make-ea :dword :index number :scale 8)))
          (t
           (move result number)
           (cond ((< -32 amount 32)
                  ;; this code is used both in ASH and ASH-MODFX, so
                  ;; be careful
                  (if (plusp amount)
                      (inst shl result amount)
                      (progn
                        (inst sar result (- amount))
                        (inst and result (lognot fixnum-tag-mask)))))
                 ((plusp amount)
                  (unless modularp
                    (aver (not "Impossible: fixnum ASH should not be called with
constant shift greater than word length")))
                  (if (sc-is result any-reg)
                      (inst xor result result)
                      (inst mov result 0)))
                 (t (inst sar result 31)
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
           (inst lea result (make-ea :dword :base number :index number)))
          ((and (= amount 2) (not (location= number result)))
           (inst lea result (make-ea :dword :index number :scale 4)))
          ((and (= amount 3) (not (location= number result)))
           (inst lea result (make-ea :dword :index number :scale 8)))
          (t
           (move result number)
           (cond ((plusp amount) (inst shl result amount))
                 (t (inst sar result (min 31 (- amount)))))))))

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
           (inst lea result (make-ea :dword :base number :index number)))
          ((and (= amount 2) (not (location= number result)))
           (inst lea result (make-ea :dword :index number :scale 4)))
          ((and (= amount 3) (not (location= number result)))
           (inst lea result (make-ea :dword :index number :scale 8)))
          (t
           (move result number)
           (cond ((< -32 amount 32)
                  ;; this code is used both in ASH and ASH-MOD32, so
                  ;; be careful
                  (if (plusp amount)
                      (inst shl result amount)
                      (inst shr result (- amount))))
                 (t (if (sc-is result unsigned-reg)
                        (inst xor result result)
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
    (inst jmp :ns positive)
    (inst neg ecx)
    (inst cmp ecx 31)
    (inst jmp :be okay)
    (inst mov ecx 31)
    OKAY
    (inst sar result :cl)
    (inst jmp done)

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
    (inst jmp :ns positive)
    (inst neg ecx)
    (inst cmp ecx 31)
    (inst jmp :be okay)
    (inst xor result result)
    (inst jmp done)
    OKAY
    (inst shr result :cl)
    (inst jmp done)

    POSITIVE
    ;; The result-type ensures us that this shift will not overflow.
    (inst shl result :cl)

    DONE))

(define-vop (fast-%ash/right/unsigned)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg) :target result)
         (amount :scs (unsigned-reg) :target ecx))
  (:arg-types unsigned-num unsigned-num)
  (:results (result :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:generator 4
    (move result number)
    (move ecx amount)
    (inst shr result :cl)))

(define-vop (fast-%ash/right/signed)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg) :target result)
         (amount :scs (unsigned-reg) :target ecx))
  (:arg-types signed-num unsigned-num)
  (:results (result :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:generator 4
    (move result number)
    (move ecx amount)
    (inst sar result :cl)))

(define-vop (fast-%ash/right/fixnum)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (any-reg) :target result)
         (amount :scs (unsigned-reg) :target ecx))
  (:arg-types tagged-num unsigned-num)
  (:results (result :scs (any-reg) :from (:argument 0)))
  (:result-types tagged-num)
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:generator 3
    (move result number)
    (move ecx amount)
    (inst sar result :cl)
    (inst and result (lognot fixnum-tag-mask))))

(in-package "SB-C")

(defknown %lea (integer integer (member 1 2 4 8) (signed-byte 32))
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

(in-package "SB-VM")

(define-vop (%lea/unsigned=>unsigned)
  (:translate %lea)
  (:policy :fast-safe)
  (:args (base :scs (unsigned-reg))
         (index :scs (unsigned-reg)))
  (:info scale disp)
  (:arg-types unsigned-num unsigned-num
              (:constant (member 1 2 4 8))
              (:constant (signed-byte 32)))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 5
    (inst lea r (make-ea :dword :base base :index index
                         :scale scale :disp disp))))

(define-vop (%lea/signed=>signed)
  (:translate %lea)
  (:policy :fast-safe)
  (:args (base :scs (signed-reg))
         (index :scs (signed-reg)))
  (:info scale disp)
  (:arg-types signed-num signed-num
              (:constant (member 1 2 4 8))
              (:constant (signed-byte 32)))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 4
    (inst lea r (make-ea :dword :base base :index index
                         :scale scale :disp disp))))

(define-vop (%lea/fixnum=>fixnum)
  (:translate %lea)
  (:policy :fast-safe)
  (:args (base :scs (any-reg))
         (index :scs (any-reg)))
  (:info scale disp)
  (:arg-types tagged-num tagged-num
              (:constant (member 1 2 4 8))
              (:constant (signed-byte 32)))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:generator 3
    (inst lea r (make-ea :dword :base base :index index
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
    (inst jmp :ns positive)
    (inst neg ecx)
    (inst xor zero zero)
    (inst shr result :cl)
    (inst cmp ecx 31)
    (inst cmov :nbe result zero)
    (inst jmp done)

    POSITIVE
    ;; The result-type ensures us that this shift will not overflow.
    (inst shl result :cl)

    DONE))

(define-vop (signed-byte-32-len)
  (:translate integer-length)
  (:note "inline (signed-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg) :target res))
  (:arg-types signed-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 28
    (move res arg)
    (if (sc-is res unsigned-reg)
        (inst test res res)
        (inst cmp res 0))
    (inst jmp :ge POS)
    (inst not res)
    POS
    (inst bsr res res)
    (inst jmp :z zero)
    (inst inc res)
    (inst jmp done)
    ZERO
    (inst xor res res)
    DONE))

(define-vop (unsigned-byte-32-len)
  (:translate integer-length)
  (:note "inline (unsigned-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 26
    (inst bsr res arg)
    (inst jmp :z zero)
    (inst inc res)
    (inst jmp done)
    ZERO
    (inst xor res res)
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

(define-vop (unsigned-byte-32-count)
  (:translate logcount)
  (:note "inline (unsigned-byte 32) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg) :target result))
  (:arg-types unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 14
    ;; See the comments below for how the algorithm works. The tricks
    ;; used can be found for example in AMD's software optimization
    ;; guide or at "http://www.hackersdelight.org/HDcode/pop.cc" in the
    ;; function "pop1".
    ;; Calculate 2-bit sums. Note that the value of a two-digit binary
    ;; number is the sum of the right digit and twice the left digit.
    ;; Thus we can calculate the sum of the two digits by shifting the
    ;; left digit to the right position and doing a two-bit subtraction.
    ;; This subtraction will never create a borrow and thus can be made
    ;; on all 16 2-digit numbers at once.
    (move result arg)
    (move temp arg)
    (inst shr result 1)
    (inst and result #x55555555)
    (inst sub temp result)
    ;; Calculate 4-bit sums by straightforward shift, mask and add.
    ;; Note that we shift the source operand of the MOV and not its
    ;; destination so that the SHR and the MOV can execute in the same
    ;; clock cycle.
    (inst mov result temp)
    (inst shr temp 2)
    (inst and result #x33333333)
    (inst and temp #x33333333)
    (inst add result temp)
    ;; Calculate 8-bit sums. Since each sum is at most 8, which fits
    ;; into 4 bits, we can apply the mask after the addition, saving one
    ;; instruction.
    (inst mov temp result)
    (inst shr result 4)
    (inst add result temp)
    (inst and result #x0f0f0f0f)
    ;; Calculate the two 16-bit sums and the 32-bit sum. No masking is
    ;; necessary inbetween since the final sum is at most 32 which fits
    ;; into 6 bits.
    (inst mov temp result)
    (inst shr result 8)
    (inst add result temp)
    (inst mov temp result)
    (inst shr result 16)
    (inst add result temp)
    (inst and result #xff)))

;;;; binary conditional VOPs

(define-vop (fast-conditional)
  (:conditional :e)
  (:policy :fast-safe))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg)
            :load-if (not (and (sc-is x control-stack)
                               (sc-is y any-reg))))
         (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg control-stack)))
  (:arg-types tagged-num (:constant (signed-byte 30)))
  (:info y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg)
            :load-if (not (and (sc-is x signed-stack)
                               (sc-is y signed-reg))))
         (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 32) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg signed-stack)))
  (:arg-types signed-num (:constant (signed-byte 32)))
  (:info y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg)
            :load-if (not (and (sc-is x unsigned-stack)
                               (sc-is y unsigned-reg))))
         (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num (:constant (unsigned-byte 32)))
  (:info y))

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
                                                    ,(if (eq suffix '-c/fixnum)
                                                         '(fixnumize y)
                                                         'y))))))))
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
  (:arg-types tagged-num (:constant (integer 0 29)))
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
  (:arg-types signed-num (:constant (integer 0 31)))
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
  (:arg-types unsigned-num (:constant (integer 0 31)))
  (:generator 5
    (inst bt x y)))

(macrolet ((define-conditional-vop (tran cond unsigned)
             `(progn
                ,@(mapcar
                   (lambda (suffix cost signed)
                     `(define-vop (;; FIXME: These could be done more
                                   ;; cleanly with SYMBOLICATE.
                                   ,(intern (format nil "~:@(FAST-IF-~A~A~)"
                                                    tran suffix))
                                   ,(intern
                                     (format nil "~:@(FAST-CONDITIONAL~A~)"
                                             suffix)))
                        (:translate ,tran)
                        (:conditional ,(if signed
                                           cond
                                           unsigned))
                        (:generator ,cost
                          (cond ((and (sc-is x any-reg signed-reg unsigned-reg)
                                      (eql y 0))
                                 (inst test x x))
                                (t
                                 (inst cmp x
                                       ,(if (eq suffix '-c/fixnum)
                                            '(fixnumize y)
                                            'y)))))))
                   '(/fixnum -c/fixnum /signed -c/signed /unsigned -c/unsigned)
                   '(4 3 6 5 6 5)
                   '(t t t t nil nil)))))

  (define-conditional-vop < :l :b)
  (define-conditional-vop > :g :a))

(define-vop (fast-if-eql/signed fast-conditional/signed)
  (:translate eql)
  (:generator 6
    (inst cmp x y)))

(define-vop (fast-if-eql-c/signed fast-conditional-c/signed)
  (:translate eql)
  (:generator 5
    (cond ((and (sc-is x signed-reg) (zerop y))
           (inst test x x))  ; smaller instruction
          (t
           (inst cmp x y)))))

(define-vop (fast-if-eql/unsigned fast-conditional/unsigned)
  (:translate eql)
  (:generator 6
    (inst cmp x y)))

(define-vop (fast-if-eql-c/unsigned fast-conditional-c/unsigned)
  (:translate eql)
  (:generator 5
    (cond ((and (sc-is x unsigned-reg) (zerop y))
           (inst test x x))  ; smaller instruction
          (t
           (inst cmp x y)))))

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
  (:translate eql)
  (:generator 4
    (inst cmp x y)))
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg)
            :load-if (not (and (sc-is x control-stack)
                               (sc-is y any-reg))))
         (y :scs (any-reg control-stack)))
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg control-stack)))
  (:arg-types tagged-num (:constant (signed-byte 30)))
  (:info y)
  (:translate eql)
  (:generator 2
    (cond ((and (sc-is x any-reg) (zerop y))
           (inst test x x))  ; smaller instruction
          (t
           (inst cmp x (fixnumize y))))))
(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:args (x :scs (any-reg descriptor-reg control-stack)))
  (:arg-types * (:constant (signed-byte 30)))
  (:variant-cost 6))

;;;; 32-bit logical operations

;;; Only the lower 5 bits of the shift amount are significant.
(macrolet ((define (translate operation)
             `(define-vop ()
                (:translate ,translate)
                (:note ,(string translate))
                (:policy :fast-safe)
                (:args (num :scs (unsigned-reg) :target r)
                       (amount :scs (signed-reg) :target ecx))
                (:arg-types unsigned-num tagged-num)
                (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
                (:results (r :scs (unsigned-reg) :from (:argument 0)))
                (:result-types unsigned-num)
                (:generator 1
                 (move r num)
                 (move ecx amount)
                 (inst ,operation r :cl)))))
  (define shift-towards-start shr)
  (define shift-towards-end   shl))

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
                 :load-if (not (and (or (sc-is x unsigned-stack)
                                        (sc-is x signed-stack))
                                    (or (sc-is r unsigned-stack)
                                        (sc-is r signed-stack))
                                    (location= x r)))))
     (:info y)
     (:arg-types untagged-num (:constant (or (unsigned-byte 32) (signed-byte 32))))
     (:results (r :scs (unsigned-reg signed-reg) :from (:argument 0)
                  :load-if (not (and (or (sc-is x unsigned-stack)
                                         (sc-is x signed-stack))
                                     (or (sc-is r unsigned-stack)
                                         (sc-is r unsigned-stack))
                                     (location= x r)))))
     (:result-types unsigned-num)
     (:translate ,function)))

(macrolet ((def (name -c-p)
             (let ((fun32   (symbolicate name "-MOD32"))
                   (funfx   (symbolicate name "-MODFX"))
                   (vopu    (symbolicate "FAST-" name "/UNSIGNED=>UNSIGNED"))
                   (vopcu   (symbolicate "FAST-" name "-C/UNSIGNED=>UNSIGNED"))
                   (vopf    (symbolicate "FAST-" name "/FIXNUM=>FIXNUM"))
                   (vopcf   (symbolicate "FAST-" name "-C/FIXNUM=>FIXNUM"))
                   (vop32u  (symbolicate "FAST-" name "-MOD32/WORD=>UNSIGNED"))
                   (vop32f  (symbolicate "FAST-" name "-MOD32/FIXNUM=>FIXNUM"))
                   (vop32cu (symbolicate "FAST-" name "-MOD32-C/WORD=>UNSIGNED"))
                   (vop32cf (symbolicate "FAST-" name "-MOD32-C/FIXNUM=>FIXNUM"))
                   (vopfxf  (symbolicate "FAST-" name "-MODFX/FIXNUM=>FIXNUM"))
                   (vopfxcf (symbolicate "FAST-" name "-MODFX-C/FIXNUM=>FIXNUM")))
               (declare (ignore vop32cf)) ; maybe someone will want it some day
               `(progn
                  (define-modular-fun ,fun32 (x y) ,name :untagged nil 32)
                  (define-modular-fun ,funfx (x y) ,name :tagged t
                                      #.(- n-word-bits n-fixnum-tag-bits))
                  (define-mod-binop (,vop32u ,vopu) ,fun32)
                  (define-vop (,vop32f ,vopf) (:translate ,fun32))
                  (define-vop (,vopfxf ,vopf) (:translate ,funfx))
                  ,@(when -c-p
                      `((define-mod-binop-c (,vop32cu ,vopcu) ,fun32)
                        (define-vop (,vopfxcf ,vopcf) (:translate ,funfx))))))))
  (def + t)
  (def - t)
  ;; (no -C variant as x86 MUL instruction doesn't take an immediate)
  (def * nil))

(define-modular-fun %negate-mod32 (x) %negate :untagged nil 32)
(define-vop (%negate-mod32)
  (:translate %negate-mod32)
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

(define-vop (fast-ash-left-mod32-c/unsigned=>unsigned
             fast-ash-c/unsigned=>unsigned)
  (:translate ash-left-mod32))

(define-vop (fast-ash-left-mod32/unsigned=>unsigned
             fast-ash-left/unsigned=>unsigned))
(deftransform ash-left-mod32 ((integer count)
                              ((unsigned-byte 32) (unsigned-byte 5)))
  (when (sb-c:constant-lvar-p count)
    (sb-c::give-up-ir1-transform))
  '(%primitive fast-ash-left-mod32/unsigned=>unsigned integer count))

(define-vop (fast-ash-left-modfx-c/fixnum=>fixnum
             fast-ash-c/fixnum=>fixnum)
  (:variant :modular)
  (:translate ash-left-modfx))

(define-vop (fast-ash-left-modfx/fixnum=>fixnum
             fast-ash-left/fixnum=>fixnum))
(deftransform ash-left-modfx ((integer count)
                              (fixnum (unsigned-byte 5)))
  (when (sb-c:constant-lvar-p count)
    (sb-c::give-up-ir1-transform))
  '(%primitive fast-ash-left-modfx/fixnum=>fixnum integer count))

(in-package "SB-C")

(defknown sb-vm::%lea-mod32 (integer integer (member 1 2 4 8) (signed-byte 32))
  (unsigned-byte 32)
  (foldable flushable movable))
(defknown sb-vm::%lea-modfx (integer integer (member 1 2 4 8) (signed-byte 32))
  fixnum
  (foldable flushable movable))

(define-modular-fun-optimizer %lea ((base index scale disp) :untagged nil :width width)
  (when (and (<= width 32)
             (constant-lvar-p scale)
             (constant-lvar-p disp))
    (cut-to-width base :untagged width nil)
    (cut-to-width index :untagged width nil)
    'sb-vm::%lea-mod32))
(define-modular-fun-optimizer %lea ((base index scale disp) :tagged t :width width)
  (when (and (<= width (- sb-vm:n-word-bits sb-vm:n-fixnum-tag-bits))
             (constant-lvar-p scale)
             (constant-lvar-p disp))
    (cut-to-width base :tagged width t)
    (cut-to-width index :tagged width t)
    'sb-vm::%lea-modfx))

#+sb-xc-host
(progn
  (defun sb-vm::%lea-mod32 (base index scale disp)
    (ldb (byte 32 0) (%lea base index scale disp)))
  (defun sb-vm::%lea-modfx (base index scale disp)
    (mask-signed-field (- sb-vm:n-word-bits sb-vm:n-fixnum-tag-bits)
                       (%lea base index scale disp))))
#-sb-xc-host
(progn
  (defun sb-vm::%lea-mod32 (base index scale disp)
    (let ((base (logand base #xffffffff))
          (index (logand index #xffffffff)))
      ;; can't use modular version of %LEA, as we only have VOPs for
      ;; constant SCALE and DISP.
      (ldb (byte 32 0) (+ base (* index scale) disp))))
  (defun sb-vm::%lea-modfx (base index scale disp)
    (let ((base (mask-signed-field sb-vm:n-fixnum-bits base))
          (index (mask-signed-field sb-vm:n-fixnum-bits index)))
      ;; can't use modular version of %LEA, as we only have VOPs for
      ;; constant SCALE and DISP.
      (mask-signed-field sb-vm:n-fixnum-bits (+ base (* index scale) disp)))))

(in-package "SB-VM")

(define-vop (%lea-mod32/unsigned=>unsigned
             %lea/unsigned=>unsigned)
  (:translate %lea-mod32))
(define-vop (%lea-modfx/fixnum=>fixnum
             %lea/fixnum=>fixnum)
  (:translate %lea-modfx))

;;; logical operations
(define-modular-fun lognot-mod32 (x) lognot :untagged nil 32)
(define-vop (lognot-mod32/word=>unsigned)
  (:translate lognot-mod32)
  (:args (x :scs (unsigned-reg signed-reg unsigned-stack signed-stack) :target r
            :load-if (not (and (or (sc-is x unsigned-stack)
                                   (sc-is x signed-stack))
                               (or (sc-is r unsigned-stack)
                                   (sc-is r signed-stack))
                               (location= x r)))))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)
               :load-if (not (and (or (sc-is x unsigned-stack)
                                      (sc-is x signed-stack))
                                  (or (sc-is r unsigned-stack)
                                      (sc-is r signed-stack))
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
(define-full-reffer+addend bignum-ref-with-offset *
  bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb-bignum:%bignum-ref-with-offset)
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
  (:temporary (:sc any-reg :from :eval :to :result :offset eax-offset :target carry) carry-temp)
  (:results (result :scs (unsigned-reg) :from (:argument 0))
            (carry :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (move result a)
    (move temp c)
    (inst neg temp) ; Set the carry flag to 0 if c=0 else to 1
    (inst adc result b)
    (inst set :c carry-temp)
    (inst and carry-temp 1)
    (move carry carry-temp)))

;;; Note: the borrow is 1 for no borrow and 0 for a borrow, the opposite
;;; of the x86 convention.
(define-vop (sub-w/borrow)
  (:translate sb-bignum:%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg) :to :eval :target result)
         (b :scs (unsigned-reg unsigned-stack) :to :result)
         (c :scs (any-reg unsigned-reg control-stack)))
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
  (:translate sb-bignum:%multiply-and-add)
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
  (:translate sb-bignum:%multiply-and-add)
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
  (:translate sb-bignum:%multiply)
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

(define-vop (bignum-lognot lognot-mod32/word=>unsigned)
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
  (:translate sb-bignum:%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg unsigned-stack) :target result))
  (:arg-types unsigned-num (:constant (integer 0 31)))
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

;;; Support for the Mersenne Twister, MT19937, random number generator
;;; due to Matsumoto and Nishimura.
;;;
;;; Makoto Matsumoto and T. Nishimura, "Mersenne twister: A
;;; 623-dimensionally equidistributed uniform pseudorandom number
;;; generator.", ACM Transactions on Modeling and Computer Simulation,
;;; 1997, to appear.
;;;
;;; State:
;;;  0-1:   Constant matrix A. [0, #x9908b0df] (not used here)
;;;  2:     Index; init. to 1.
;;;  3-626: State.
(defknown random-mt19937 ((simple-array (unsigned-byte 32) (*)))
  (unsigned-byte 32) ())
(define-vop (random-mt19937)
  (:policy :fast-safe)
  (:translate random-mt19937)
  (:args (state :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-unsigned-byte-32)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to :result) k)
  (:temporary (:sc unsigned-reg :offset eax-offset
                   :from (:eval 0) :to :result) tmp)
  (:results (y :scs (unsigned-reg) :from (:eval 0)))
  (:result-types unsigned-num)
  (:generator 50
    (loadw k state (+ 2 vector-data-offset) other-pointer-lowtag)
    (inst cmp k 624)
    (inst jmp :ne no-update)
    (inst mov tmp state)        ; The state is passed in EAX.
    (inst call (make-fixup 'random-mt19937-update :assembly-routine))
    ;; Restore k, and set to 0.
    (inst xor k k)
    NO-UPDATE
    ;; y = ptgfsr[k++];
    (inst mov y (make-ea-for-vector-data state :index k :offset 3))
    ;; y ^= (y >> 11);
    (inst shr y 11)
    (inst xor y (make-ea-for-vector-data state :index k :offset 3))
    ;; y ^= (y << 7) & #x9d2c5680
    (inst mov tmp y)
    (inst inc k)
    (inst shl tmp 7)
    (storew k state (+ 2 vector-data-offset) other-pointer-lowtag)
    (inst and tmp #x9d2c5680)
    (inst xor y tmp)
    ;; y ^= (y << 15) & #xefc60000
    (inst mov tmp y)
    (inst shl tmp 15)
    (inst and tmp #xefc60000)
    (inst xor y tmp)
    ;; y ^= (y >> 18);
    (inst mov tmp y)
    (inst shr tmp 18)
    (inst xor y tmp)))

(in-package "SB-C")

(defun mask-result (class width result)
  (ecase class
    (:unsigned
     `(logand ,result ,(1- (ash 1 width))))
    (:signed
     `(mask-signed-field ,width ,result))))

;;; This is essentially a straight implementation of the algorithm in
;;; "Strength Reduction of Multiplications by Integer Constants",
;;; Youfeng Wu, ACM SIGPLAN Notices, Vol. 30, No.2, February 1995.
(defun basic-decompose-multiplication (class width arg num n-bits condensed)
  (case (aref condensed 0)
    (0
     (let ((tmp (min 3 (aref condensed 1))))
       (decf (aref condensed 1) tmp)
       (mask-result class width
                    `(%lea ,arg
                           ,(decompose-multiplication class width
                             arg (ash (1- num) (- tmp)) (1- n-bits) (subseq condensed 1))
                           ,(ash 1 tmp) 0))))
    ((1 2 3)
     (let ((r0 (aref condensed 0)))
       (incf (aref condensed 1) r0)
       (mask-result class width
                    `(%lea ,(decompose-multiplication class width
                             arg (- num (ash 1 r0)) (1- n-bits) (subseq condensed 1))
                           ,arg
                           ,(ash 1 r0) 0))))
    (t (let ((r0 (aref condensed 0)))
         (setf (aref condensed 0) 0)
         (mask-result class width
                      `(ash ,(decompose-multiplication class width
                              arg (ash num (- r0)) n-bits condensed)
                            ,r0))))))

(defun decompose-multiplication (class width arg num n-bits condensed)
  (cond
    ((= n-bits 0) 0)
    ((= num 1) arg)
    ((= n-bits 1)
     (mask-result class width `(ash ,arg ,(1- (integer-length num)))))
    ((let ((max 0) (end 0))
       (loop for i from 2 to (length condensed)
             for j = (reduce #'+ (subseq condensed 0 i))
             when (and (> (- (* 2 i) 3 j) max)
                       (< (+ (ash 1 (1+ j))
                             (ash (ldb (byte (- 32 (1+ j)) (1+ j)) num)
                                  (1+ j)))
                          (ash 1 32)))
               do (setq max (- (* 2 i) 3 j)
                        end i))
       (when (> max 0)
         (let ((j (reduce #'+ (subseq condensed 0 end))))
           (let ((n2 (+ (ash 1 (1+ j))
                        (ash (ldb (byte (- 32 (1+ j)) (1+ j)) num) (1+ j))))
                 (n1 (1+ (ldb (byte (1+ j) 0) (lognot num)))))
           (mask-result class width
                        `(- ,(optimize-multiply class width arg n2)
                            ,(optimize-multiply  class width arg n1))))))))
    ((dolist (i '(9 5 3))
       (when (integerp (/ num i))
         (when (< (logcount (/ num i)) (logcount num))
           (let ((x (gensym)))
             (return `(let ((,x ,(optimize-multiply class width arg (/ num i))))
                       ,(mask-result class width
                                     `(%lea ,x ,x (1- ,i) 0)))))))))
    (t (basic-decompose-multiplication class width arg num n-bits condensed))))

(defun optimize-multiply (class width arg x)
  (let* ((n-bits (logcount x))
         (condensed (make-array n-bits)))
    (let ((count 0) (bit 0))
      (dotimes (i 32)
        (cond ((logbitp i x)
               (setf (aref condensed bit) count)
               (setf count 1)
               (incf bit))
              (t (incf count)))))
    (decompose-multiplication class width arg x n-bits condensed)))

(defun *-transformer (class width y &optional (fun '%lea))
  (cond
    ((= y (ash 1 (integer-length y)))
     ;; there's a generic transform for y = 2^k
     (give-up-ir1-transform))
    ((member y '(3 5 9))
     ;; we can do these multiplications directly using LEA
     `(,fun x x ,(1- y) 0))
    ((member :pentium4 *backend-subfeatures*)
     ;; the pentium4's multiply unit is reportedly very good
     (give-up-ir1-transform))
    ;; FIXME: should make this more fine-grained.  If nothing else,
    ;; there should probably be a cutoff of about 9 instructions on
    ;; pentium-class machines.
    (t (optimize-multiply class width 'x y))))

;; x86-64 considers these transforms to be totally unimportant
;; but the situation is not as clear with x86
(deftransform * ((x y)
                 ((unsigned-byte 32) (constant-arg (unsigned-byte 32)))
                 (unsigned-byte 32))
  "recode as leas, shifts and adds"
  (let ((y (lvar-value y)))
    (*-transformer :unsigned 32 y)))
(deftransform sb-vm::*-mod32
    ((x y) ((unsigned-byte 32) (constant-arg (unsigned-byte 32)))
     (unsigned-byte 32))
  "recode as leas, shifts and adds"
  (let ((y (lvar-value y)))
    (*-transformer :unsigned 32 y 'sb-vm::%lea-mod32)))

(deftransform * ((x y)
                 (fixnum (constant-arg (unsigned-byte 32)))
                 fixnum)
  "recode as leas, shifts and adds"
  (let ((y (lvar-value y)))
    (*-transformer :signed (- sb-vm:n-word-bits sb-vm:n-fixnum-tag-bits) y)))
(deftransform sb-vm::*-modfx
    ((x y) (fixnum (constant-arg (unsigned-byte 32)))
     fixnum)
  "recode as leas, shifts and adds"
  (let ((y (lvar-value y)))
    (*-transformer :signed (- sb-vm:n-word-bits sb-vm:n-fixnum-tag-bits) y
                   'sb-vm::%lea-modfx)))

;;; FIXME: we should also be able to write an optimizer or two to
;;; convert (+ (* x 2) 17), (- (* x 9) 5) to a %LEA.
