;;;; the VM definition arithmetic VOPs for the SPARC

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; unary operations.

(define-vop (fast-safe-arith-op)
  (:policy :fast-safe)
  (:effects)
  (:affected))

(define-vop (fixnum-unop fast-safe-arith-op)
  (:args (x :scs (any-reg)))
  (:results (res :scs (any-reg)))
  (:note "inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num))

(define-vop (signed-unop fast-safe-arith-op)
  (:args (x :scs (signed-reg)))
  (:results (res :scs (signed-reg)))
  (:note "inline (signed-byte 32) arithmetic")
  (:arg-types signed-num)
  (:result-types signed-num))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (inst neg res x)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (inst neg res x)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:translate lognot)
  (:generator 1
    (inst xor res x (fixnumize -1))))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 2
    (inst not res x)))

;;;; Binary fixnum operations.

;;; Assume that any constant operand is the second arg...

(define-vop (fast-fixnum-binop fast-safe-arith-op)
  (:args (x :target r :scs (any-reg zero))
         (y :target r :scs (any-reg zero)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg zero))
         (y :target r :scs (unsigned-reg zero)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-signed-binop fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg zero))
         (y :target r :scs (signed-reg zero)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic"))


(define-vop (fast-fixnum-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg zero)))
  (:info y)
  (:arg-types tagged-num
              (:constant (and (signed-byte 11) (not (integer 0 0)))))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg zero)))
  (:info y)
  (:arg-types unsigned-num
              (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg zero)))
  (:info y)
  (:arg-types signed-num
              (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic"))


(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro define-binop (translate untagged-penalty op
                        &optional arg-swap restore-fixnum-mask)
  `(progn
     (define-vop (,(symbolicate 'fast translate '/fixnum=>fixnum)
                  fast-fixnum-binop)
       ,@(when restore-fixnum-mask
           `((:temporary (:sc non-descriptor-reg) temp)))
       (:translate ,translate)
       (:generator 2
         ,(if arg-swap
              `(inst ,op ,(if restore-fixnum-mask 'temp 'r) y x)
              `(inst ,op ,(if restore-fixnum-mask 'temp 'r) x y))
         ,@(when restore-fixnum-mask
             `((inst andn r temp fixnum-tag-mask)))))
     ,@(unless arg-swap
         `((define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum)
                        fast-fixnum-binop-c)
             ,@(when restore-fixnum-mask
                 `((:temporary (:sc non-descriptor-reg) temp)))
             (:translate ,translate)
             (:generator 1
               (inst ,op ,(if restore-fixnum-mask 'temp 'r) x (fixnumize y))
               ,@(when restore-fixnum-mask
                   `((inst andn r temp fixnum-tag-mask)))))))
     (define-vop (,(symbolicate 'fast- translate '/signed=>signed)
                  fast-signed-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
         ,(if arg-swap
              `(inst ,op r y x)
              `(inst ,op r x y))))
     ,@(unless arg-swap
         `((define-vop (,(symbolicate 'fast- translate '-c/signed=>signed)
                        fast-signed-binop-c)
             (:translate ,translate)
             (:generator ,untagged-penalty
               (inst ,op r x y)))))
     (define-vop (,(symbolicate 'fast- translate '/unsigned=>unsigned)
                  fast-unsigned-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
         ,(if arg-swap
              `(inst ,op r y x)
              `(inst ,op r x y))))
     ,@(unless arg-swap
         `((define-vop (,(symbolicate 'fast- translate '-c/unsigned=>unsigned)
                        fast-unsigned-binop-c)
             (:translate ,translate)
             (:generator ,untagged-penalty
               (inst ,op r x y)))))))

); eval-when

(define-binop + 4 add)
(define-binop - 4 sub)
(define-binop logand 2 and)
(define-binop logandc1 2 andn t)
(define-binop logandc2 2 andn)
(define-binop logior 2 or)
(define-binop logorc1 2 orn t t)
(define-binop logorc2 2 orn nil t)
(define-binop logxor 2 xor)
(define-binop logeqv 2 xnor nil t)

(define-vop (fast-logand/signed-unsigned=>unsigned fast-logand/unsigned=>unsigned)
  (:args (x :scs (signed-reg) :target r)
         (y :scs (unsigned-reg) :target r))
  (:arg-types signed-num unsigned-num)
  (:translate logand))

;;; Truncate

;; This doesn't work for some reason.
#+nil
(define-vop (fast-v8-truncate/fixnum=>fixnum fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:results (quo :scs (any-reg))
            (rem :scs (any-reg)))
  (:result-types tagged-num tagged-num)
  (:note "inline fixnum arithmetic")
  (:temporary (:scs (any-reg) :target quo) q)
  (:temporary (:scs (any-reg)) r)
  (:temporary (:scs (signed-reg)) y-int)
  (:vop-var vop)
  (:save-p :compute-only)
  (:guard (or (member :sparc-v8 *backend-subfeatures*)
              (and (member :sparc-v9 *backend-subfeatures*)
                   (not (member :sparc-64 *backend-subfeatures*)))))
  (:generator 12
    (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
      (inst cmp y zero-tn)
      (inst b :eq zero)
      ;; Extend the sign of X into the Y register
        (inst sra r x 31)
      (inst wry r)
      ;; Remove tag bits so Q and R will be tagged correctly.
      (inst sra y-int y n-fixnum-tag-bits)
      (inst nop)
      (inst nop)

      (inst sdiv q x y-int)             ; Q is tagged.
      ;; We have the quotient so we need to compute the remainder
      (inst smul r q y-int)             ; R is tagged
      (inst sub rem x r)
      (unless (location= quo q)
        (move quo q)))))

(define-vop (fast-v8-truncate/signed=>signed fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (quo :scs (signed-reg))
            (rem :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:temporary (:scs (signed-reg) :target quo) q)
  (:temporary (:scs (signed-reg)) r)
  (:vop-var vop)
  (:save-p :compute-only)
  (:guard (or (member :sparc-v8 *backend-subfeatures*)
              (and (member :sparc-v9 *backend-subfeatures*)
                   (not (member :sparc-64 *backend-subfeatures*)))))
  (:generator 12
    (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
      (inst cmp y zero-tn)
      (if (member :sparc-v9 *backend-subfeatures*)
          (inst b :eq zero :pn)
          (inst b :eq zero))
      ;; Extend the sign of X into the Y register
      (inst sra r x 31)
      (inst wry r)
      (inst nop)
      (inst nop)
      (inst nop)

      (inst sdiv q x y)
      ;; We have the quotient so we need to compue the remainder
      (inst smul r q y)         ; rem
      (inst sub rem x r)
      (unless (location= quo q)
        (move quo q)))))

(define-vop (fast-v8-truncate/unsigned=>unsigned fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (quo :scs (unsigned-reg))
            (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:temporary (:scs (unsigned-reg) :target quo) q)
  (:temporary (:scs (unsigned-reg)) r)
  (:vop-var vop)
  (:save-p :compute-only)
  (:guard (or (member :sparc-v8 *backend-subfeatures*)
              (and (member :sparc-v9 *backend-subfeatures*)
                   (not (member :sparc-64 *backend-subfeatures*)))))
  (:generator 8
    (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
      (inst cmp y zero-tn)
      (if (member :sparc-v9 *backend-subfeatures*)
          (inst b :eq zero :pn)
          (inst b :eq zero))
      (inst wry zero-tn)                ; Clear out high part
      (inst nop)
      (inst nop)
      (inst nop)

      (inst udiv q x y)
      ;; Compute remainder
      (inst umul r q y)
      (inst sub rem x r)
      (unless (location= quo q)
        (inst move quo q)))))

(define-vop (fast-v9-truncate/signed=>signed fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (quo :scs (signed-reg))
            (rem :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:temporary (:scs (signed-reg) :target quo) q)
  (:temporary (:scs (signed-reg)) r)
  (:vop-var vop)
  (:save-p :compute-only)
  (:guard (member :sparc-64 *backend-subfeatures*))
  (:generator 8
    (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
      (inst cmp y zero-tn)
      (inst b :eq zero :pn)
      ;; Sign extend the numbers, just in case.
      (inst sra x 0)
      (inst sra y 0)
      (inst sdivx q x y)
      ;; Compute remainder
      (inst mulx r q y)
      (inst sub rem x r)
      (unless (location= quo q)
        (inst move quo q)))))

(define-vop (fast-v9-truncate/unsigned=>unsigned fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (quo :scs (unsigned-reg))
            (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:temporary (:scs (unsigned-reg) :target quo) q)
  (:temporary (:scs (unsigned-reg)) r)
  (:vop-var vop)
  (:save-p :compute-only)
  (:guard (member :sparc-64 *backend-subfeatures*))
  (:generator 8
    (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
      (inst cmp y zero-tn)
      (inst b :eq zero :pn)
      ;; Zap the higher 32 bits, just in case
      (inst srl x 0)
      (inst srl y 0)
      (inst udivx q x y)
      ;; Compute remainder
      (inst mulx r q y)
      (inst sub rem x r)
      (unless (location= quo q)
        (inst move quo q)))))

;;; Shifting

(define-vop (fast-ash/signed=>signed)
  (:note "inline ASH")
  (:args (number :scs (signed-reg) :to :save)
         (amount :scs (signed-reg) :to :save))
  (:arg-types signed-num signed-num)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) ndesc)
  (:generator 5
    (let ((done (gen-label)))
      (inst cmp amount)
      (inst b :ge done)
      ;; The result-type assures us that this shift will not
      ;; overflow.
      (inst sll result number amount)
      (inst neg ndesc amount)
      (inst cmp ndesc 31)
      (if (member :sparc-v9 *backend-subfeatures*)
          (progn
            (inst cmove :ge ndesc 31)
            (inst sra result number ndesc))
          (progn
            (inst b :le done)
            (inst sra result number ndesc)
            (inst sra result number 31)))
      (emit-label done))))

(define-vop (fast-ash-c/signed=>signed)
  (:note "inline constant ASH")
  (:args (number :scs (signed-reg)))
  (:info count)
  (:arg-types signed-num (:constant integer))
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:translate ash)
  (:policy :fast-safe)
  (:generator 4
    (cond
      ((< count 0) (inst sra result number (min (- count) 31)))
      ((> count 0) (inst sll result number (min count 31)))
      (t (bug "identity ASH not transformed away")))))

(define-vop (fast-ash/unsigned=>unsigned)
  (:note "inline ASH")
  (:args (number :scs (unsigned-reg) :to :save)
         (amount :scs (signed-reg) :to :save))
  (:arg-types unsigned-num signed-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) ndesc)
  (:generator 5
    (let ((done (gen-label)))
      (inst cmp amount)
      (inst b :ge done)
      ;; The result-type assures us that this shift will not
      ;; overflow.
      (inst sll result number amount)
      (inst neg ndesc amount)
      (inst cmp ndesc 32)
      (if (member :sparc-v9 *backend-subfeatures*)
          (progn
            (inst srl result number ndesc)
            (inst cmove :ge result zero-tn))
          (progn
            (inst b :lt done)
            (inst srl result number ndesc)
            (move result zero-tn)))
      (emit-label done))))

(define-vop (fast-ash-c/unsigned=>unsigned)
  (:note "inline constant ASH")
  (:args (number :scs (unsigned-reg)))
  (:info count)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash)
  (:policy :fast-safe)
  (:generator 4
    (cond
      ((< count -31) (move result zero-tn))
      ((< count 0) (inst srl result number (min (- count) 31)))
      ((> count 0) (inst sll result number (min count 31)))
      (t (bug "identity ASH not transformed away")))))

;; Some special cases where we know we want a left shift.  Just do the
;; shift, instead of checking for the sign of the shift.
(macrolet
    ((def (name sc-type type result-type cost)
       `(define-vop (,name)
         (:note "inline ASH")
         (:translate ash)
         (:args (number :scs (,sc-type))
                (amount :scs (signed-reg unsigned-reg immediate)))
         (:arg-types ,type positive-fixnum)
         (:results (result :scs (,result-type)))
         (:result-types ,type)
         (:policy :fast-safe)
         (:generator ,cost
          ;; The result-type assures us that this shift will not
          ;; overflow. And for fixnums, the zero bits that get
          ;; shifted in are just fine for the fixnum tag.
          (sc-case amount
           ((signed-reg unsigned-reg)
            (inst sll result number amount))
           (immediate
            (let ((amount (tn-value amount)))
              (aver (>= amount 0))
              (inst sll result number amount))))))))
  (def fast-ash-left/signed=>signed signed-reg signed-num signed-reg 3)
  (def fast-ash-left/fixnum=>fixnum any-reg tagged-num any-reg 2)
  (def fast-ash-left/unsigned=>unsigned unsigned-reg unsigned-num unsigned-reg 3))


(define-vop (signed-byte-32-len)
  (:translate integer-length)
  (:note "inline (signed-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg) :target shift))
  (:arg-types signed-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) shift)
  (:generator 30
    (let ((loop (gen-label))
          (test (gen-label)))
      (inst addcc shift zero-tn arg)
      (inst b :ge test)
      (move res zero-tn)
      (inst b test)
      (inst not shift)

      (emit-label loop)
      (inst add res (fixnumize 1))

      (emit-label test)
      (inst cmp shift)
      (inst b :ne loop)
      (inst srl shift 1))))

(define-vop (unsigned-byte-32-count)
  (:translate logcount)
  (:note "inline (unsigned-byte 32) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) mask temp)
  (:generator 35
      (move res arg)

      (dolist (stuff '((1 #x55555555) (2 #x33333333) (4 #x0f0f0f0f)
                       (8 #x00ff00ff) (16 #x0000ffff)))
        (destructuring-bind (shift bit-mask)
            stuff
          ;; Set mask
          (inst sethi mask (ldb (byte 22 10) bit-mask))
          (inst add mask (ldb (byte 10 0) bit-mask))

          (inst and temp res mask)
          (inst srl res shift)
          (inst and res mask)
          (inst add res temp)))))


;;; Multiply and Divide.

(define-vop (fast-v8-*/fixnum=>fixnum fast-fixnum-binop)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:translate *)
  (:guard (or (member :sparc-v8 *backend-subfeatures*)
              (and (member :sparc-v9 *backend-subfeatures*)
                   (not (member :sparc-64 *backend-subfeatures*)))))
  (:generator 2
    ;; The cost here should be less than the cost for
    ;; */signed=>signed.  Why?  A fixnum product using signed=>signed
    ;; has to convert both args to signed-nums.  But using this, we
    ;; don't have to and that saves an instruction.
    (inst sra temp y n-fixnum-tag-bits)
    (inst smul r x temp)))

(define-vop (fast-v8-*-c/fixnum=>fixnum fast-safe-arith-op)
  (:args (x :target r :scs (any-reg zero)))
  (:info y)
  (:arg-types tagged-num
              (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic")
  (:translate *)
  (:guard (or (member :sparc-v8 *backend-subfeatures*)
              (and (member :sparc-v9 *backend-subfeatures*)
                   (not (member :sparc-64 *backend-subfeatures*)))))
  (:generator 1
    (inst smul r x y)))

(define-vop (fast-v8-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:guard (or (member :sparc-v8 *backend-subfeatures*)
              (and (member :sparc-v9 *backend-subfeatures*)
                   (not (member :sparc-64 *backend-subfeatures*)))))
  (:generator 3
    (inst smul r x y)))

(define-vop (fast-v8-*-c/signed=>signed fast-signed-binop-c)
  (:translate *)
  (:guard (or (member :sparc-v8 *backend-subfeatures*)
              (and (member :sparc-v9 *backend-subfeatures*)
                   (not (member :sparc-64 *backend-subfeatures*)))))
  (:generator 2
    (inst smul r x y)))

(define-vop (fast-v8-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:guard (or (member :sparc-v8 *backend-subfeatures*)
              (and (member :sparc-v9 *backend-subfeatures*)
                   (not (member :sparc-64 *backend-subfeatures*)))))
  (:generator 3
    (inst umul r x y)))

(define-vop (fast-v8-*-c/unsigned=>unsigned fast-unsigned-binop-c)
  (:translate *)
  (:guard (or (member :sparc-v8 *backend-subfeatures*)
              (and (member :sparc-v9 *backend-subfeatures*)
                   (not (member :sparc-64 *backend-subfeatures*)))))
  (:generator 2
    (inst umul r x y)))

;; The smul and umul instructions are deprecated on the Sparc V9.  Use
;; mulx instead.
(define-vop (fast-v9-*/fixnum=>fixnum fast-fixnum-binop)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:translate *)
  (:guard (member :sparc-64 *backend-subfeatures*))
  (:generator 4
    (inst sra temp y n-fixnum-tag-bits)
    (inst mulx r x temp)))

(define-vop (fast-v9-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:guard (member :sparc-64 *backend-subfeatures*))
  (:generator 3
    (inst mulx r x y)))

(define-vop (fast-v9-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:guard (member :sparc-64 *backend-subfeatures*))
  (:generator 3
    (inst mulx r x y)))


;;;; Modular functions:
(define-modular-fun lognot-mod32 (x) lognot :untagged nil 32)
(define-vop (lognot-mod32/unsigned=>unsigned)
  (:translate lognot-mod32)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (inst not res x)))

(macrolet
    ((define-modular-backend (fun &optional constantp)
       (let ((mfun-name (symbolicate fun '-mod32))
             (modvop (symbolicate 'fast- fun '-mod32/unsigned=>unsigned))
             (modcvop (symbolicate 'fast- fun '-mod32-c/unsigned=>unsigned))
             (vop (symbolicate 'fast- fun '/unsigned=>unsigned))
             (cvop (symbolicate 'fast- fun '-c/unsigned=>unsigned)))
         `(progn
            (define-modular-fun ,mfun-name (x y) ,fun :untagged nil 32)
            (define-vop (,modvop ,vop)
              (:translate ,mfun-name))
            ,@(when constantp
                `((define-vop (,modcvop ,cvop)
                    (:translate ,mfun-name))))))))
  (define-modular-backend + t)
  (define-modular-backend - t)
  (define-modular-backend logeqv t)
  (define-modular-backend logandc1)
  (define-modular-backend logandc2 t)
  (define-modular-backend logorc1)
  (define-modular-backend logorc2 t))

(define-source-transform lognand (x y)
  `(lognot (logand ,x ,y)))
(define-source-transform lognor (x y)
  `(lognot (logior ,x ,y)))

(define-vop (fast-ash-left-mod32-c/unsigned=>unsigned
             fast-ash-c/unsigned=>unsigned)
  (:translate ash-left-mod32))

(define-vop (fast-ash-left-mod32/unsigned=>unsigned
             fast-ash-left/unsigned=>unsigned))
(deftransform ash-left-mod32 ((integer count)
                              ((unsigned-byte 32) (unsigned-byte 5)))
  (when (sb!c::constant-lvar-p count)
    (sb!c::give-up-ir1-transform))
  '(%primitive fast-ash-left-mod32/unsigned=>unsigned integer count))

;;;; Binary conditional VOPs:

(define-vop (fast-conditional)
  (:conditional)
  (:info target not-p)
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg zero))
         (y :scs (any-reg zero)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg zero)))
  (:arg-types tagged-num (:constant (signed-byte 11)))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg zero))
         (y :scs (signed-reg zero)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 32) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg zero)))
  (:arg-types signed-num (:constant (signed-byte 13)))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg zero))
         (y :scs (unsigned-reg zero)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg zero)))
  (:arg-types unsigned-num (:constant (unsigned-byte 12)))
  (:info target not-p y))


(defmacro define-conditional-vop (tran cond unsigned not-cond not-unsigned)
  `(progn
     ,@(mapcar (lambda (suffix cost signed)
                 (unless (and (member suffix '(/fixnum -c/fixnum))
                              (eq tran 'eql))
                   `(define-vop (,(intern (format nil "~:@(FAST-IF-~A~A~)"
                                                  tran suffix))
                                 ,(intern
                                   (format nil "~:@(FAST-CONDITIONAL~A~)"
                                           suffix)))
                     (:translate ,tran)
                     (:generator ,cost
                      (inst cmp x
                       ,(if (eq suffix '-c/fixnum) '(fixnumize y) 'y))
                      (inst b (if not-p
                                  ,(if signed not-cond not-unsigned)
                                  ,(if signed cond unsigned))
                       target)
                      (inst nop)))))
               '(/fixnum -c/fixnum /signed -c/signed /unsigned -c/unsigned)
               '(4 3 6 5 6 5)
               '(t t t t nil nil))))

(define-conditional-vop < :lt :ltu :ge :geu)

(define-conditional-vop > :gt :gtu :le :leu)

(define-conditional-vop eql :eq :eq :ne :ne)

;;; EQL/FIXNUM is funny because the first arg can be of any type, not just a
;;; known fixnum.

;;; These versions specify a fixnum restriction on their first arg.  We have
;;; also generic-eql/fixnum VOPs which are the same, but have no restriction on
;;; the first arg and a higher cost.  The reason for doing this is to prevent
;;; fixnum specific operations from being used on word integers, spuriously
;;; consing the argument.
;;;

(define-vop (fast-eql/fixnum fast-conditional)
  (:args (x :scs (any-reg zero))
         (y :scs (any-reg zero)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql)
  (:generator 4
    (inst cmp x y)
    (inst b (if not-p :ne :eq) target)
    (inst nop)))
;;;
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg)))
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg zero)))
  (:arg-types tagged-num (:constant (signed-byte 11)))
  (:info target not-p y)
  (:translate eql)
  (:generator 2
    (inst cmp x (fixnumize y))
    (inst b (if not-p :ne :eq) target)
    (inst nop)))
;;;
(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant (signed-byte 11)))
  (:variant-cost 6))


;;;; 32-bit logical operations

(define-vop (shift-towards-someplace)
  (:policy :fast-safe)
  (:args (num :scs (unsigned-reg))
         (amount :scs (signed-reg)))
  (:arg-types unsigned-num tagged-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (shift-towards-start shift-towards-someplace)
  (:translate shift-towards-start)
  (:note "shift-towards-start")
  (:generator 1
    (inst sll r num amount)))

(define-vop (shift-towards-end shift-towards-someplace)
  (:translate shift-towards-end)
  (:note "shift-towards-end")
  (:generator 1
    (inst srl r num amount)))

;;;; Bignum stuff.
(define-vop (bignum-length get-header-data)
  (:translate sb!bignum:%bignum-length)
  (:policy :fast-safe))

(define-vop (bignum-set-length set-header-data)
  (:translate sb!bignum:%bignum-set-length)
  (:policy :fast-safe))

(define-vop (bignum-ref word-index-ref)
  (:variant bignum-digits-offset other-pointer-lowtag)
  (:translate sb!bignum:%bignum-ref)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (bignum-set word-index-set)
  (:variant bignum-digits-offset other-pointer-lowtag)
  (:translate sb!bignum:%bignum-set)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate zero))
         (value :scs (unsigned-reg)))
  (:arg-types t positive-fixnum unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (digit-0-or-plus)
  (:translate sb!bignum:%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:guard (not (member :sparc-v9 *backend-subfeatures*)))
  (:generator 3
    (let ((done (gen-label)))
      (inst cmp digit)
      (inst b :lt done)
      (move result null-tn)
      (load-symbol result t)
      (emit-label done))))

(define-vop (v9-digit-0-or-plus-cmove)
  (:translate sb!bignum:%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:guard (member :sparc-v9 *backend-subfeatures*))
  (:generator 3
    (inst cmp digit)
    (load-symbol result t)
    (inst cmove :lt result null-tn)))

;; This doesn't work?
#+nil
(define-vop (v9-digit-0-or-plus-movr)
  (:translate sb!bignum:%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:guard #!+:sparc-v9 t #!-:sparc-v9 nil)
  (:generator 2
    (load-symbol temp t)
    (inst movr result null-tn digit :lz)
    (inst movr result temp digit :gez)))

(define-vop (add-w/carry)
  (:translate sb!bignum:%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg))
            (carry :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:generator 3
    (inst addcc zero-tn c -1)
    (inst addxcc result a b)
    (inst addx carry zero-tn zero-tn)))

(define-vop (sub-w/borrow)
  (:translate sb!bignum:%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg))
            (borrow :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (inst subcc zero-tn c 1)
    (inst subxcc result a b)
    (inst addx borrow zero-tn zero-tn)
    (inst xor borrow 1)))

;;; EMIT-MULTIPLY -- This is used both for bignum stuff and in assembly
;;; routines.
;;;
(defun emit-multiply (multiplier multiplicand result-high result-low)
  "Emit code to multiply MULTIPLIER with MULTIPLICAND, putting the result
  in RESULT-HIGH and RESULT-LOW.  KIND is either :signed or :unsigned.
  Note: the lifetimes of MULTIPLICAND and RESULT-HIGH overlap."
  (declare (type tn multiplier result-high result-low)
           (type (or tn (signed-byte 13)) multiplicand))
  ;; It seems that emit-multiply is only used to do an unsigned
  ;; multiply, so the code only does an unsigned multiply.
  (cond
    ((member :sparc-64 *backend-subfeatures*)
     ;; Take advantage of V9's 64-bit multiplier.
     ;;
     ;; Make sure the multiplier and multiplicand are really
     ;; unsigned 64-bit numbers.
     (inst srl multiplier 0)
     (inst srl multiplicand 0)

     ;; Multiply the two numbers and put the result in
     ;; result-high.  Copy the low 32-bits to result-low.  Then
     ;; shift result-high so the high 32-bits end up in the low
     ;; 32-bits.
     (inst mulx result-high multiplier multiplicand)
     (inst move result-low result-high)
     (inst srax result-high 32))
    ((or (member :sparc-v8 *backend-subfeatures*)
         (member :sparc-v9 *backend-subfeatures*))
     ;; V8 has a multiply instruction.  This should also work for
     ;; the V9, but umul and the Y register is deprecated on the
     ;; V9.
     (inst umul result-low multiplier multiplicand)
     (inst rdy result-high))
    (t
     (let ((label (gen-label)))
       (inst wry multiplier)
       (inst andcc result-high zero-tn)
       ;; Note: we can't use the Y register until three insts
       ;; after it's written.
       (inst nop)
       (inst nop)
       (dotimes (i 32)
         (inst mulscc result-high multiplicand))
       (inst mulscc result-high zero-tn)
       (inst cmp multiplicand)
       (inst b :ge label)
       (inst nop)
       (inst add result-high multiplier)
       (emit-label label)
       (inst rdy result-low)))))

(define-vop (bignum-mult-and-add-3-arg)
  (:translate sb!bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to (:eval 1))
         (y :scs (unsigned-reg) :to (:eval 1))
         (carry-in :scs (unsigned-reg) :to (:eval 2)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from (:eval 0))
            (lo :scs (unsigned-reg) :from (:eval 1)))
  (:result-types unsigned-num unsigned-num)
  (:generator 40
    (emit-multiply x y hi lo)
    (inst addcc lo carry-in)
    (inst addx hi zero-tn)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate sb!bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to (:eval 1))
         (y :scs (unsigned-reg) :to (:eval 1))
         (prev :scs (unsigned-reg) :to (:eval 2))
         (carry-in :scs (unsigned-reg) :to (:eval 2)))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from (:eval 0))
            (lo :scs (unsigned-reg) :from (:eval 1)))
  (:result-types unsigned-num unsigned-num)
  (:generator 40
    (emit-multiply x y hi lo)
    (inst addcc lo carry-in)
    (inst addx hi zero-tn)
    (inst addcc lo prev)
    (inst addx hi zero-tn)))

(define-vop (bignum-mult)
  (:translate sb!bignum:%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to (:result 1))
         (y :scs (unsigned-reg) :to (:result 1)))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg))
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 40
    (emit-multiply x y hi lo)))

(define-vop (bignum-lognot lognot-mod32/unsigned=>unsigned)
  (:translate sb!bignum:%lognot))

(define-vop (fixnum-to-digit)
  (:translate sb!bignum:%fixnum-to-digit)
  (:policy :fast-safe)
  (:args (fixnum :scs (any-reg)))
  (:arg-types tagged-num)
  (:results (digit :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst sra digit fixnum n-fixnum-tag-bits)))

(define-vop (bignum-floor)
  (:translate sb!bignum:%bigfloor)
  (:policy :fast-safe)
  (:args (div-high :scs (unsigned-reg) :target rem)
         (div-low :scs (unsigned-reg) :target quo)
         (divisor :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (quo :scs (unsigned-reg) :from (:argument 1))
            (rem :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num unsigned-num)
  (:generator 300
    (move rem div-high)
    (move quo div-low)
    (dotimes (i 33)
      (let ((label (gen-label)))
        (inst cmp rem divisor)
        (inst b :ltu label)
        (inst addxcc quo quo)
        (inst sub rem divisor)
        (emit-label label)
        (unless (= i 32)
          (inst addx rem rem))))
    (inst not quo)))

(define-vop (bignum-floor-v8)
  (:translate sb!bignum:%bigfloor)
  (:policy :fast-safe)
  (:args (div-high :scs (unsigned-reg) :target rem)
         (div-low :scs (unsigned-reg) :target quo)
         (divisor :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (quo :scs (unsigned-reg) :from (:argument 1))
            (rem :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :target quo) q)
  ;; This vop is for a v8 or v9, provided we're also not using
  ;; sparc-64, for which there a special sparc-64 vop.
  (:guard (or (member :sparc-v8 *backend-subfeatures*)
              (member :sparc-v9 *backend-subfeatures*)))
  (:generator 15
    (inst wry div-high)
    (inst nop)
    (inst nop)
    (inst nop)
    ;; Compute the quotient [Y, div-low] / divisor
    (inst udiv q div-low divisor)
    ;; Compute the remainder.  The high part of the result is in the Y
    ;; register.
    (inst umul rem q divisor)
    (inst sub rem div-low rem)
    (unless (location= quo q)
      (move quo q))))

(define-vop (bignum-floor-v9)
  (:translate sb!bignum:%bigfloor)
  (:policy :fast-safe)
  (:args (div-high :scs (unsigned-reg))
         (div-low :scs (unsigned-reg))
         (divisor :scs (unsigned-reg) :to (:result 1)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :from (:argument 0)) dividend)
  (:results (quo :scs (unsigned-reg))
            (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:guard (member :sparc-64 *backend-subfeatures*))
  (:generator 5
    ;; Set dividend to be div-high and div-low
    (inst sllx dividend div-high 32)
    (inst add dividend div-low)
    ;; Compute quotient
    (inst udivx quo dividend divisor)
    ;; Compute the remainder
    (inst mulx rem quo divisor)
    (inst sub rem dividend rem)))

(define-vop (signify-digit)
  (:translate sb!bignum:%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (sc-case res
      (any-reg
       (inst sll res digit n-fixnum-tag-bits))
      (signed-reg
       (move res digit)))))

(define-vop (digit-ashr)
  (:translate sb!bignum:%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg))
         (count :scs (unsigned-reg)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst sra result digit count)))

(define-vop (digit-lshr digit-ashr)
  (:translate sb!bignum:%digit-logical-shift-right)
  (:generator 1
    (inst srl result digit count)))

(define-vop (digit-ashl digit-ashr)
  (:translate sb!bignum:%ashl)
  (:generator 1
    (inst sll result digit count)))


;;;; Static functions.

(define-static-fun two-arg-gcd (x y) :translate gcd)
(define-static-fun two-arg-lcm (x y) :translate lcm)

(define-static-fun two-arg-+ (x y) :translate +)
(define-static-fun two-arg-- (x y) :translate -)
(define-static-fun two-arg-* (x y) :translate *)
(define-static-fun two-arg-/ (x y) :translate /)

(define-static-fun two-arg-< (x y) :translate <)
(define-static-fun two-arg-<= (x y) :translate <=)
(define-static-fun two-arg-> (x y) :translate >)
(define-static-fun two-arg->= (x y) :translate >=)
(define-static-fun two-arg-= (x y) :translate =)
(define-static-fun two-arg-/= (x y) :translate /=)

(define-static-fun %negate (x) :translate %negate)

(define-static-fun two-arg-and (x y) :translate logand)
(define-static-fun two-arg-ior (x y) :translate logior)
(define-static-fun two-arg-xor (x y) :translate logxor)
(define-static-fun two-arg-eqv (x y) :translate logeqv)


(in-package "SB!C")

(deftransform * ((x y)
                 ((unsigned-byte 32) (constant-arg (unsigned-byte 32)))
                 (unsigned-byte 32))
  "recode as shifts and adds"
  (let ((y (lvar-value y)))
    (multiple-value-bind (result adds shifts)
        (ub32-strength-reduce-constant-multiply 'x y)
      (cond
        ;; we assume, perhaps foolishly, that good SPARCs don't have an
        ;; issue with multiplications.  (Remember that there's a
        ;; different transform for converting x*2^k to a shift).
        ((member :sparc-64 *backend-subfeatures*) (give-up-ir1-transform))
        ((or (member :sparc-v9 *backend-subfeatures*)
             (member :sparc-v8 *backend-subfeatures*))
         ;; breakeven point as measured by Raymond Toy
         (when (> (+ adds shifts) 9)
           (give-up-ir1-transform))))
      (or result 0))))
