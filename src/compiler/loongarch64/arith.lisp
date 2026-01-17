;;;; the VM definition of arithmetic VOPs for the LoongArch

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(define-vop (fast-safe-arith-op)
  (:policy :fast-safe))

(define-vop (fixnum-unop fast-safe-arith-op)
  (:args (x :scs (any-reg)))
  (:results (res :scs (any-reg)))
  (:note "inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num))

(define-vop (signed-unop fast-safe-arith-op)
  (:args (x :scs (signed-reg)))
  (:results (res :scs (signed-reg)))
  (:note #.(format nil "inline (signed-byte ~a) arithmetic" n-machine-word-bits))
  (:arg-types signed-num)
  (:result-types signed-num))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (inst sub.d res zero-tn x)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (inst sub.d res zero-tn x)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:translate lognot)
  (:generator 1
    (inst s_xori res x (fixnumize -1))))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 2
    (inst s_xori res x -1)))

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
  (:note #.(format nil "inline (unsigned-byte ~a) arithmetic" n-machine-word-bits)))

(define-vop (fast-signed-binop fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg zero))
         (y :target r :scs (signed-reg zero)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note #.(format nil "inline (signed-byte ~a) arithmetic" n-machine-word-bits)))

(define-vop (fast-fixnum-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg zero)))
  (:info y)
  (:arg-types tagged-num (:constant short-immediate-fixnum))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline arithmetic"))

(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg zero)))
  (:info y)
  (:arg-types unsigned-num (:constant short-immediate))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline unsigned unboxed arithmetic"))

(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg zero)))
  (:info y)
  (:arg-types signed-num (:constant short-immediate))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline signed unboxed arithmetic"))

(defmacro define-binop (translate cost untagged-cost r-op &optional i-op)
  `(progn
     (define-vop (,(symbolicate "FAST-" translate "/FIXNUM=>FIXNUM")
                  fast-fixnum-binop)
       (:translate ,translate)
       (:generator ,(1+ cost)
         (inst ,r-op r x y)))
     (define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
                  fast-signed-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-cost)
         (inst ,r-op r x y)))
     (define-vop (,(symbolicate "FAST-" translate "/UNSIGNED=>UNSIGNED")
                  fast-unsigned-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-cost)
         (inst ,r-op r x y)))
     ,@(when i-op
         `((define-vop (,(symbolicate "FAST-" translate "-C/FIXNUM=>FIXNUM")
                        fast-fixnum-binop-c)
             (:translate ,translate)
             (:generator ,cost
               (inst ,i-op r x (fixnumize y))))
           (define-vop (,(symbolicate "FAST-" translate "-C/SIGNED=>SIGNED")
                        fast-signed-binop-c)
             (:translate ,translate)
             (:generator ,untagged-cost
               (inst ,i-op r x y)))
           (define-vop (,(symbolicate "FAST-" translate
                                      "-C/UNSIGNED=>UNSIGNED")
                        fast-unsigned-binop-c)
             (:translate ,translate)
             (:generator ,untagged-cost
               (inst ,i-op r x y)))))))
(define-binop + 1 5 add.d addi.d)
(define-binop - 1 5 sub.d subi)
(define-binop logior 1 3 or s_ori)
(define-binop logxor 1 3 xor s_xori)
(define-binop logand 1 3 and s_andi)

(define-vop (fast-logand/signed-unsigned=>unsigned fast-logand/unsigned=>unsigned)
  (:args (x :scs (signed-reg) :target r)
         (y :scs (unsigned-reg) :target r))
  (:arg-types signed-num unsigned-num))

(define-vop (fast-logand-c/signed-unsigned=>unsigned fast-logand-c/unsigned=>unsigned)
  (:args (x :scs (signed-reg) :target r))
  (:arg-types signed-num (:constant (eql #.most-positive-word)))
  (:ignore y)
  (:generator 1
    (move r x)))

(define-source-transform logeqv (&rest args)
  (if (oddp (length args))
      `(logxor ,@args)
      `(lognot (logxor ,@args))))
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

;;; Shifting
(define-vop (fast-ash-left-c/fixnum=>fixnum)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (any-reg) :target result))
  (:info amount)
  (:arg-types tagged-num (:constant unsigned-byte))
  (:results (result :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline ASH")
  (:generator 1
    (if (< amount n-word-bits)
        (inst slli.d result number amount)
        (inst li result 0))))

(define-vop (fast-ash-right-c/fixnum=>fixnum)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (any-reg) :target result))
  (:info amount)
  (:arg-types tagged-num (:constant (integer * -1)))
  (:results (result :scs (any-reg)))
  (:result-types tagged-num)
  (:temporary (:sc unsigned-reg :target result) temp)
  (:note "inline ASH")
  (:generator 1
    (inst srai.d temp number (min (- amount) (1- n-word-bits)))
    (inst s_andi result temp (lognot fixnum-tag-mask))))

(define-vop (fast-ash-c/unsigned=>unsigned)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg) :target result))
  (:info amount)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline ASH")
  (:generator 1
    (cond ((< (- n-word-bits) amount n-word-bits)
           (if (plusp amount)
               (inst slli.d result number amount)
               (inst srli.d result number (- amount))))
          (t
           (inst li result 0)))))

(define-vop (fast-ash-c/signed=>signed)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg) :target result))
  (:info amount)
  (:arg-types signed-num (:constant integer))
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline ASH")
  (:generator 1
    (cond ((< (- n-word-bits) amount n-word-bits)
           (if (plusp amount)
               (inst slli.d result number amount)
               (inst srai.d result number (- amount))))
          ((>= amount n-word-bits)
           (inst li result 0))
          (t
           (inst srai.d result number (1- n-word-bits))))))

(define-vop (fast-ash/signed/unsigned)
  (:note "inline ASH")
  (:args (number)
         (amount))
  (:results (result))
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) ndesc)
  (:variant-vars variant)
  (:generator 3
    (inst bge amount zero-tn positive)
    (inst li ndesc (- n-word-bits))
    (inst blt ndesc amount no-overflow)
    (ecase variant
      (:signed (inst srai.d result number (1- n-word-bits)))
      (:unsigned (move result zero-tn)))
    (inst j done)

    NO-OVERFLOW
    (inst sub.d ndesc zero-tn amount)
    (ecase variant
      (:signed (inst sra.d result number ndesc))
      (:unsigned (inst srl.d result number ndesc)))
    (inst j done)

    POSITIVE
    ;; The result-type assures us that this shift will not overflow.
    (inst sll.d result number amount)

    DONE))

(define-vop (fast-ash/unsigned=>unsigned fast-ash/signed/unsigned)
  (:args (number :scs (unsigned-reg) :to :save)
         (amount :scs (signed-reg) :to :save :target ndesc))
  (:arg-types unsigned-num signed-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash)
  (:variant :unsigned))

(define-vop (fast-ash/signed=>signed fast-ash/signed/unsigned)
  (:args (number :scs (signed-reg) :to :save)
         (amount :scs (signed-reg) :to :save :target ndesc))
  (:arg-types signed-num signed-num)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:translate ash)
  (:variant :signed))

(macrolet ((def (name sc-type type result-type cost)
             `(define-vop (,name)
                (:note "inline ASH")
                (:translate ash)
                (:args (number :scs (,sc-type))
                       (amount :scs (signed-reg unsigned-reg)))
                (:arg-types ,type positive-fixnum)
                (:results (result :scs (,result-type)))
                (:result-types ,type)
                (:policy :fast-safe)
                (:generator ,cost
                  (inst sll.d result number amount)))))
  ;; FIXME: There's the opportunity for a sneaky optimization here, I
  ;; think: a FAST-ASH-LEFT-C/FIXNUM=>SIGNED vop.  -- CSR, 2003-09-03
  (def fast-ash-left/fixnum=>fixnum any-reg tagged-num any-reg 2)
  (def fast-ash-left/signed=>signed signed-reg signed-num signed-reg 3)
  (def fast-ash-left/unsigned=>unsigned unsigned-reg unsigned-num unsigned-reg 3))

(define-vop (fast-%ash/right/unsigned)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg) :target result)
         (amount :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (result :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:generator 4
    (inst srl.d result number amount)))

(define-vop (fast-%ash/right/signed)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg) :target result)
         (amount :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:results (result :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:generator 4
    (inst sra.d result number amount)))

(define-vop (fast-%ash/right/fixnum)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (any-reg) :target result)
         (amount :scs (unsigned-reg) :target temp))
  (:arg-types tagged-num unsigned-num)
  (:results (result :scs (any-reg) :from (:argument 0)))
  (:result-types tagged-num)
  (:temporary (:sc unsigned-reg :target result) temp)
  (:generator 3
    (inst sra.d temp number amount)
    (inst s_andi result temp (lognot fixnum-tag-mask))))

(define-vop (signed-byte-64-len)
  (:translate integer-length)
  (:note #.(format nil "inline (signed-byte ~a) integer-length" n-machine-word-bits))
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg) :target shift))
  (:arg-types signed-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) shift)
  (:generator 30
    (let ((loop (gen-label))
          (test (gen-label)))
      (move shift arg)
      (move res zero-tn)
      (inst bge shift zero-tn test)

      (inst s_xori shift shift -1)
      (inst j test)

      (emit-label loop)
      (inst srli.d shift shift 1)
      (inst addi.d res res (fixnumize 1))

      (emit-label test)
      (inst bne shift zero-tn loop))))

(define-vop (unsigned-byte-64-count)
  (:translate logcount)
  (:note #.(format nil "inline (unsigned-byte ~a) logcount" n-machine-word-bits))
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg) :target num))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0) :to (:result 0)
                    :target res) num)
  (:temporary (:scs (non-descriptor-reg)) mask temp)
  (:generator 30
    (let* ((maskers
             '(#x5555555555555555 #x3333333333333333 #x0f0f0f0f0f0f0f0f #x00ff00ff00ff00ff #x0000ffff0000ffff #x00000000ffffffff))
           ;;; FIXME (first (last maskers)) can't transform this early
           ;;; in the build...
           (last #x00000000ffffffff))
      (loop for masker in maskers
            for shift = 1 then (ash shift 1) do
              (inst li mask masker)
              (let ((input (if (= shift 1) arg num)))
                (inst srli.d temp input shift)
                (inst and num input mask))
              (inst and temp temp mask)
              (inst add.d (if (= masker last) res num) num temp)))))

(define-vop (fast-*/fixnum=>fixnum fast-fixnum-binop)
  (:args (x :scs (signed-reg))
         (y :target r :scs (any-reg)))
  (:translate *)
  (:generator 2
    (inst mul.d r x y)))

(define-vop (fast-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:generator 3
    (inst mul.d r x y)))

(define-vop (fast-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:generator 3
    (inst mul.d r x y)))

(define-vop (fast-truncate/fixnum fast-fixnum-binop)
  (:translate truncate)
  (:args (x :scs (any-reg))
         (y :scs (any-reg) :to (:eval 1)))
  (:results (q :scs (any-reg))
            (r :scs (any-reg) :from (:eval 0)))
  (:result-types tagged-num tagged-num)
  (:temporary (:scs (non-descriptor-reg) :target q) temp)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 11
    (inst div.d temp x y)
    (inst mod.d r x y)
    ;; Division by zero check performed after division per the ISA
    ;; documentation recommendation.
    (let ((zero (generate-error-code vop 'division-by-zero-error x)))
      (inst beq y zero-tn zero))
    (inst slli.d q temp n-fixnum-tag-bits)))

(define-vop (fast-truncate/unsigned fast-unsigned-binop)
  (:translate truncate)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (unsigned-reg) :to :save))
  (:results (q :scs (unsigned-reg) :from :eval)
            (r :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 12
    (inst div.du q x y)
    (inst mod.du r x y)
    (let ((zero (generate-error-code vop 'division-by-zero-error x)))
      (inst beq y zero-tn zero))))

(define-vop (fast-truncate/signed fast-signed-binop)
  (:translate truncate)
  (:args (x :scs (signed-reg) :to :result)
         (y :scs (signed-reg) :to :save))
  (:results (q :scs (signed-reg) :from :eval)
            (r :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 12
    (inst div.d q x y)
    (inst mod.d r x y)
    (let ((zero (generate-error-code vop 'division-by-zero-error x)))
      (inst beq y zero-tn zero))))

(define-vop (fast-conditional)
  (:conditional)
  (:variant-vars condition)
  (:info target not-p)
  (:policy :fast-safe))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg zero))
         (y :scs (any-reg zero)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:generator 1
    (three-way-comparison x y condition :signed not-p target)))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg zero))
         (y :scs (signed-reg zero)))
  (:arg-types signed-num signed-num)
  (:note #.(format nil "inline (signed-byte ~a) comparison" n-word-bits))
  (:generator 1
    (three-way-comparison x y condition :signed not-p target)))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg zero))
         (y :scs (unsigned-reg zero)))
  (:arg-types unsigned-num unsigned-num)
  (:note #.(format nil "inline (unsigned-byte ~a) comparison" n-word-bits))
  (:generator 1
    (three-way-comparison x y condition :unsigned not-p target)))

(defmacro define-conditional-vop (translate op)
  `(progn
     ,@(mapcar (lambda (suffix)
                 (unless (and (eq suffix '/fixnum)
                              (eq translate 'eql))
                   `(define-vop (,(intern (format nil "~:@(FAST-IF-~A~A~)"
                                                  translate suffix))
                                 ,(intern
                                   (format nil "~:@(FAST-CONDITIONAL~A~)"
                                           suffix)))
                      (:translate ,translate)
                      (:variant ,op))))
               '(/fixnum /signed /unsigned))))
(define-conditional-vop < :lt)
(define-conditional-vop > :gt)
(define-conditional-vop eql :eq)

(define-vop (fast-eql/fixnum fast-conditional)
  (:args (x :scs (any-reg zero))
         (y :scs (any-reg zero)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql)
  (:ignore condition)
  (:generator 1
    (three-way-comparison x y :eq nil not-p target)))

(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg)))
  (:arg-types * tagged-num)
  (:variant-cost 7))

(macrolet ((define (translate operation)
             `(define-vop ()
                (:translate ,translate)
                (:note ,(string translate))
                (:policy :fast-safe)
                (:args (num :scs (unsigned-reg))
                       (amount :scs (signed-reg)))
                (:arg-types unsigned-num tagged-num)
                (:results (r :scs (unsigned-reg)))
                (:result-types unsigned-num)
                (:generator 1 (inst ,operation r num amount)))))
  (define shift-towards-start srl.d)
  (define shift-towards-end   sll.d))

;;;; Modular arithmetic
(defmacro define-mod-binop ((name prototype) function)
  `(define-vop (,name ,prototype)
     (:args (x :target r :scs (unsigned-reg signed-reg))
            (y :scs (unsigned-reg signed-reg)))
     (:arg-types untagged-num untagged-num)
     (:results (r :scs (unsigned-reg signed-reg) :from (:argument 0)))
     (:result-types unsigned-num)
     (:translate ,function)))

(defmacro define-mod-binop-c ((name prototype) function)
  `(define-vop (,name ,prototype)
     (:args (x :target r :scs (unsigned-reg signed-reg)))
     (:info y)
     (:arg-types untagged-num (:constant short-immediate))
     (:results (r :scs (unsigned-reg signed-reg) :from (:argument 0)))
     (:result-types unsigned-num)
     (:translate ,function)))

(macrolet ((def (name -c-p)
             (let ((funmod   (symbolicate name "-MODXLEN"))
                   (funfx   (symbolicate name "-MODFX"))
                   (vopu    (symbolicate "FAST-" name "/UNSIGNED=>UNSIGNED"))
                   (vopcu   (symbolicate "FAST-" name "-C/UNSIGNED=>UNSIGNED"))
                   (vopf    (symbolicate "FAST-" name "/FIXNUM=>FIXNUM"))
                   (vopcf   (symbolicate "FAST-" name "-C/FIXNUM=>FIXNUM"))
                   (vopmodu  (symbolicate "FAST-" name "-MODXLEN/WORD=>UNSIGNED"))
                   (vopmodf  (symbolicate "FAST-" name "-MODXLEN/FIXNUM=>FIXNUM"))
                   (vopmodcu (symbolicate "FAST-" name "-MODXLEN-C/WORD=>UNSIGNED"))
                   (vopfxf  (symbolicate "FAST-" name "-MODFX/FIXNUM=>FIXNUM"))
                   (vopfxcf (symbolicate "FAST-" name "-MODFX-C/FIXNUM=>FIXNUM")))
               `(progn
                  (define-modular-fun ,funmod (x y) ,name :untagged nil ,n-word-bits)
                  (define-modular-fun ,funfx (x y) ,name :tagged t
                                      ,(- n-word-bits n-fixnum-tag-bits))
                  (define-mod-binop (,vopmodu ,vopu) ,funmod)
                  (define-vop (,vopmodf ,vopf) (:translate ,funmod))
                  (define-vop (,vopfxf ,vopf) (:translate ,funfx))
                  ,@(when -c-p
                      `((define-mod-binop-c (,vopmodcu ,vopcu) ,funmod)
                        (define-vop (,vopfxcf ,vopcf) (:translate ,funfx))))))))
  (def + t)
  (def - t)
  (def * nil))

(progn
  (define-vop (fast-ash-left-mod64-c/unsigned=>unsigned
               fast-ash-c/unsigned=>unsigned)
    (:translate ash-left-mod64))

  (define-vop (fast-ash-left-mod64/unsigned=>unsigned
               fast-ash-left/unsigned=>unsigned))

  (deftransform ash-left-mod64 ((integer count)
                                ((unsigned-byte 64) (unsigned-byte 6)))
    (when (sb-c:constant-lvar-p count)
      (sb-c::give-up-ir1-transform))
    '(%primitive fast-ash-left-mod64/unsigned=>unsigned integer count)))

(define-modular-fun lognot-mod64 (x) lognot :untagged nil 64)

(define-vop (lognot-mod64/unsigned=>unsigned)
  (:translate lognot-mod64)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (inst s_xori r x -1)))

;;;; Bignum stuff.
(define-vop (bignum-length get-header-data)
  (:translate sb-bignum:%bignum-length)
  (:policy :fast-safe))

(define-vop (bignum-set-length set-header-data)
  (:translate sb-bignum:%bignum-set-length)
  (:policy :fast-safe))

(define-full-reffer bignum-ref * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb-bignum:%bignum-ref)

(define-full-setter bignum-set * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb-bignum:%bignum-set)

(define-vop (digit-0-or-plus)
  (:translate sb-bignum:%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:conditional)
  (:info target not-p)
  (:generator 1
    (if not-p
        (inst blt digit zero-tn target)
        (inst bge digit zero-tn target))))

(define-vop (add-w/carry)
  (:translate sb-bignum:%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target result) res)
  (:results (result :scs (unsigned-reg))
            (carry :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num positive-fixnum)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 5
    (let ((carry-in (gen-label))
          (done (gen-label)))
      (inst add.d res a b)
      (inst bne c zero-tn carry-in)

      (inst sltu carry res b)
      (inst j done)

      (emit-label carry-in)
      (inst addi.d res res 1)
      (inst s_xori temp a -1)
      (inst sltu carry b temp)
      (inst s_xori carry carry 1)

      (emit-label done)
      (move result res))))

(define-vop (sub-w/borrow)
  (:translate sb-bignum:%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target result) res)
  (:results (result :scs (unsigned-reg))
            (borrow :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (let ((no-borrow-in (gen-label))
          (done (gen-label)))
      (inst sub.d res a b)
      (inst bne c zero-tn no-borrow-in)

      (inst subi res res 1)
      (inst sltu borrow b a)
      (inst j done)

      (emit-label no-borrow-in)
      (inst sltu borrow a b)
      (inst s_xori borrow borrow 1)

      (emit-label done)
      (move result res))))

(define-vop (bignum-mult-and-add-3-arg)
  (:translate sb-bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (unsigned-reg) :to :result)
         (carry-in :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg)) temp)
  (:results (hi :scs (unsigned-reg) :from :load)
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 4
    (inst mulh.du hi x y)
    (inst mul.d lo x y)
    (inst add.d lo lo carry-in)
    (inst sltu temp lo carry-in)
    (inst add.d hi hi temp)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate sb-bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (unsigned-reg) :target lo)
         (prev :scs (unsigned-reg) :to (:eval 3))
         (carry-in :scs (unsigned-reg) :to (:eval 4)))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :from (:argument 2)) t1 t2)
  (:results (hi :scs (unsigned-reg) :from :eval)
            (lo :scs (unsigned-reg) :from (:eval 3)))
  (:result-types unsigned-num unsigned-num)
  (:generator 8
    (inst mulh.du hi x y)
    (inst mul.d t1 x y)
    (inst add.d lo prev carry-in)
    (inst sltu t2 lo carry-in)
    (inst add.d hi hi t2)
    (inst add.d lo lo t1)
    (inst sltu t1 lo t1)
    (inst add.d hi hi t1)))

(define-vop (bignum-mult)
  (:translate sb-bignum:%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (unsigned-reg) :to :result))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from :eval)
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 1
    (inst mulh.du hi x y)
    (inst mul.d lo x y)))

(define-vop (mulhi)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target hi)
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst mulh.du hi x y)))

(define-vop (mulhi/fx)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (any-reg) :target hi)
         (y :scs (unsigned-reg)))
  (:arg-types positive-fixnum unsigned-num)
  (:temporary (:sc unsigned-reg) temp)
  (:results (hi :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 15
    (inst mulh.du temp x y)
    (inst s_andi hi temp (lognot fixnum-tag-mask))))

(define-vop (bignum-floor)
  (:translate sb-bignum:%bigfloor)
  (:policy :fast-safe)
  (:args (num-high :scs (unsigned-reg) :target rem)
         (num-low :scs (unsigned-reg) :target rem-low)
         (denom :scs (unsigned-reg) :to (:eval 1)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :from (:argument 1)) rem-low)
  (:temporary (:scs (unsigned-reg) :from (:eval 0)) temp)
  (:results (quo :scs (unsigned-reg) :from (:eval 0))
            (rem :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num unsigned-num)
  (:generator 600
    (move rem num-high)
    (move rem-low num-low)
    (flet ((maybe-subtract (&optional (guess temp))
             (inst subi temp guess 1)
             (inst and temp temp denom)
             (inst sub.d rem rem temp)))
      (inst sltu quo rem denom)
      (maybe-subtract quo)
      (dotimes (i n-word-bits)
        (inst slli.d rem rem 1)
        (inst srli.d temp rem-low (1- n-word-bits))
        (inst or rem rem temp)
        (inst slli.d rem-low rem-low 1)
        (inst sltu temp rem denom)
        (inst slli.d quo quo 1)
        (inst or quo quo temp)
        (maybe-subtract)))
    (inst s_xori quo quo -1)))

(define-vop (signify-digit)
  (:translate sb-bignum:%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (sc-case res
      (any-reg
       (inst slli.d res digit n-fixnum-tag-bits))
      (signed-reg
       (move res digit)))))

(define-vop (digit-ashr)
  (:translate sb-bignum:%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg))
         (count :scs (unsigned-reg)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst sra.d result digit count)))

(define-vop (digit-lshr digit-ashr)
  (:translate sb-bignum:%digit-logical-shift-right)
  (:generator 1
    (inst srl.d result digit count)))

(define-vop (digit-ashl digit-ashr)
  (:translate sb-bignum:%ashl)
  (:generator 1
    (inst sll.d result digit count)))

(progn
(define-vop ()
  (:translate fastrem-32)
  (:policy :fast-safe)
  (:args (dividend :scs (unsigned-reg))
         (c :scs (unsigned-reg))
         (divisor :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (remainder :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 10
    (inst mul.d temp dividend c)
    (inst slli.d temp temp 32) ; drop the high 32 bits, keep the low 32 bits
    (inst srli.d temp temp 32)
    (inst mul.d temp temp divisor)
    (inst srli.d remainder temp 32))) ; take the high 32 bits
(define-vop ()
  (:translate fastrem-64)
  (:policy :fast-safe)
  (:args (dividend :scs (unsigned-reg))
         (c :scs (unsigned-reg))
         (divisor :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (remainder :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 10
    (inst mul.d temp dividend c) ; keep only the low 64 bits
    (inst mulh.du remainder temp divisor)))) ; only the high 64 bits
