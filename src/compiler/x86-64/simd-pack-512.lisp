;;;; AVX512 intrinsics support for x86-64

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")


;; should this be redefined as ea-for-avx512-stack ?
(defun ea-for-avx512-stack (tn &optional (base rbp-tn))
  (ea (frame-byte-offset (+ (tn-offset tn) 7)) base))

(defun float-avx512-p (tn)
  (sc-is tn single-avx512-reg single-avx512-stack fp-immediate
            double-avx512-reg double-avx512-stack fp-immediate))

(defun int-avx512-p (tn)
  (sc-is tn int-avx512-reg int-avx512-stack fp-immediate))

#+sb-xc-host
(progn ; the host compiler will complain about absence of these
  (defun %simd-pack-512-0 (x) (error "Called %SIMD-PACK-512-0 ~S" x))
  (defun %simd-pack-512-1 (x) (error "Called %SIMD-PACK-512-1 ~S" x))
  (defun %simd-pack-512-2 (x) (error "Called %SIMD-PACK-512-2 ~S" x))
  (defun %simd-pack-512-3 (x) (error "Called %SIMD-PACK-512-3 ~S" x))
  (defun %simd-pack-512-4 (x) (error "Called %SIMD-PACK-512-4 ~S" x))
  (defun %simd-pack-512-5 (x) (error "Called %SIMD-PACK-512-5 ~S" x))
  (defun %simd-pack-512-6 (x) (error "Called %SIMD-PACK-512-6 ~S" x))
  (defun %simd-pack-512-7 (x) (error "Called %SIMD-PACK-512-7 ~S" x))
  (defun %simd-pack-512-mask-value (x) (error "Called %SIMD-PACK-512-MASK-VALUE ~S" x)))

;; mask registers

;; Mask registers are 64-bit registers so we can reuse ea from scalar regs for
;; stack spilling, but the system has to use the specialized kmovq instruction
;; since they live in their own hardware registers, not shared with either
;; scalar nor zmm regs.

(define-move-fun (load-mask 2) (vop x y)
  ((kmask-stack) (mask-reg))
  (inst kmovq y x))

(define-move-fun (store-mask 2) (vop x y)
  ((mask-reg) (kmask-stack))
  (inst kmovq y x))

(define-move-fun (load-mask-immediate 1) (vop x y)
  ((fp-immediate) (mask-reg))
  (let ((val (%simd-pack-512-mask-value (tn-value x))))
    (cond ((= val 0) (inst kxorq y y y))
          ((= val (ldb (byte 64 0) -1)) (inst kxnorq y y y))
          (t (inst kmovq y (register-inline-constant :qword val))))))

(define-vop (mask-move)
  (:args (x :scs (mask-reg) :target y :load-if (not (location= x y))))
  (:arg-types simd-pack-512-mask-type)
  (:results (y :scs (mask-reg) :load-if (not (location= x y))))
  (:result-types simd-pack-512-mask-type)
  (:note "avx512 mask move")
  (:generator 3
    (unless (location= y x)
      (inst kmovq y x))))

(define-vop (move-mask-arg)
  (:args (x :scs (mask-reg) :target y)
         (fp :scs (any-reg)
             :load-if (not (sc-is y mask-reg))))
  (:results (y))
  (:note "avx512 mask argument move")
  (:generator 4
    (sc-case y
      (mask-reg
       (unless (location= x y)
         (inst kmovq y x)))
      (kmask-stack
       (inst kmovq (ea (frame-byte-offset (tn-offset y)) fp) x)))))

(define-vop (move-to-mask)
  (:args (x :scs (descriptor-reg)))
  (:arg-types simd-pack-512-mask-type)
  (:results (y :scs (mask-reg)))
  (:result-types simd-pack-512-mask-type)
  (:note "pointer to mask coercion")
  (:generator 2
    (let ((ea (object-slot-ea x simd-pack-512-mask-value-slot other-pointer-lowtag)))
      (inst kmovq y ea))))

(define-allocator (move-from-mask)
  (:args (x :scs (mask-reg)))
  (:arg-types simd-pack-512-mask-type)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (y :scs (descriptor-reg)))
  (:result-types simd-pack-512-mask-type)
  (:note "mask to pointer coercion")
  (:generator 10
    (inst kmovq tmp x) ;; save in a GPR for a potential call to alloc-tramp
    (alloc-other simd-pack-512-mask-widetag simd-pack-512-mask-size y)
    (let ((ea (object-slot-ea y simd-pack-512-mask-value-slot other-pointer-lowtag)))
      (inst mov ea tmp))))

(define-vop (move-from-mask-to-unsigned)
  (:args (x :scs (mask-reg)))
  (:arg-types simd-pack-512-mask-type)
  (:results (y :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "mask to unsigned move")
  (:generator 1
    (inst kmovq y x)))

(define-vop (move-from-unsigned-to-mask)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (y :scs (mask-reg)))
  (:result-types simd-pack-512-mask-type)
  (:note "unsigned to mask move")
  (:generator 1
    (inst kmovq y x)))

(define-vop (move-from-mask-to-signed)
  (:args (x :scs (mask-reg)))
  (:arg-types simd-pack-512-mask-type)
  (:results (y :scs (signed-reg)))
  (:result-types signed-num)
  (:note "mask to signed move")
  (:generator 1
    (inst kmovq y x)))

(define-vop (move-from-signed-to-mask)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num)
  (:results (y :scs (mask-reg)))
  (:result-types simd-pack-512-mask-type)
  (:note "signed to mask move")
  (:generator 1
    (inst kmovq y x)))

(define-vop (move-from-mask-to-any)
  (:args (x :scs (mask-reg)))
  (:arg-types simd-pack-512-mask-type)
  (:results (y :scs (any-reg)))
  (:result-types *)
  (:note "mask to any move")
  (:generator 1
    (inst kmovq y x)))

(define-vop (move-from-any-to-mask)
  (:args (x :scs (any-reg)))
  (:arg-types *)
  (:results (y :scs (mask-reg)))
  (:result-types simd-pack-512-mask-type)
  (:note "any to mask move")
  (:generator 1
    (inst kmovq y x)))

(define-move-vop move-from-mask-to-any :move (mask-reg) (any-reg))
(define-move-vop move-from-any-to-mask :move (any-reg) (mask-reg))
(define-move-vop move-from-mask-to-signed :move (mask-reg) (signed-reg))
(define-move-vop move-from-signed-to-mask :move (signed-reg) (mask-reg))
(define-move-vop move-from-mask-to-unsigned :move (mask-reg) (unsigned-reg))
(define-move-vop move-from-unsigned-to-mask :move (unsigned-reg) (mask-reg))
(define-move-vop move-to-mask :move (descriptor-reg) (mask-reg))
(define-move-vop move-from-mask :move (mask-reg) (descriptor-reg))
(define-move-vop mask-move :move (mask-reg) (mask-reg))
(define-move-vop move-mask-arg :move-arg (mask-reg) (mask-reg))
(define-move-vop move-arg :move-arg (mask-reg) (descriptor-reg))

(define-vop (%make-simd-pack-512-mask)
  (:translate sb-ext:%make-simd-pack-512-mask)
  (:policy :fast-safe)
  (:args (val :scs (unsigned-reg) :target dst))
  (:arg-types unsigned-num)
  (:results (dst :scs (mask-reg)))
  (:result-types simd-pack-512-mask-type)
  (:generator 1
    (inst kmovq dst val)))

(define-vop (%simd-pack-512-mask-value)
  (:translate sb-kernel:%simd-pack-512-mask-value)
  (:policy :fast-safe)
  (:args (val :scs (descriptor-reg)))
  (:arg-types simd-pack-512-mask-type)
  (:results (dst :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "extract simd-pack-512 mask")
  (:generator 3
    (loadw dst val simd-pack-512-mask-value-slot other-pointer-lowtag)))

;; simd-pack-512 related

(define-move-fun (load-int-avx512-immediate 1) (vop x y)
                 ((fp-immediate) (int-avx512-reg))
  (let* ((x  (tn-value x))
         (p0 (%simd-pack-512-0 x))
         (p1 (%simd-pack-512-1 x))
         (p2 (%simd-pack-512-2 x))
         (p3 (%simd-pack-512-3 x))
         (p4 (%simd-pack-512-4 x))
         (p5 (%simd-pack-512-5 x))
         (p6 (%simd-pack-512-6 x))
         (p7 (%simd-pack-512-7 x)))
    (cond ((= p0 p1 p2 p3 p4 p5 p6 p7 0)
           (inst vpxor y y y))
          ((= p0 p1 p2 p3 p4 p5 p6 p7 (ldb (byte 64 0) -1))
           ;; don't think this is recognized as dependency breaking...
           (inst vpcmpeqd y y y))
          (t
           (inst vmovdqu y (register-inline-constant x))))))

(define-move-fun (load-float-avx512-immediate 1) (vop x y)
  ((fp-immediate fp-immediate)
   (single-avx512-reg double-avx512-reg))
  (let* ((x  (tn-value x))
         (p0 (%simd-pack-512-0 x))
         (p1 (%simd-pack-512-1 x))
         (p2 (%simd-pack-512-2 x))
         (p3 (%simd-pack-512-3 x))
         (p4 (%simd-pack-512-4 x))
         (p5 (%simd-pack-512-5 x))
         (p6 (%simd-pack-512-6 x))
         (p7 (%simd-pack-512-7 x)))
    (cond ((= p0 p1 p2 p3 p4 p5 p6 p7 0)
           ;; in 512 it works on zmm regs; we good
           (inst vxorps y y y))
          ((= p0 p1 p2 p3 p4 p5 p6 p7 (ldb (byte 64 0) -1))
           (inst vpcmpeqd y y y))
          (t
           (inst vmovdqu64 y (register-inline-constant x))))))

(define-move-fun (load-int-avx512 2) (vop x y)
  ((int-avx512-stack) (int-avx512-reg))
  (inst vmovdqu64 y (ea-for-avx512-stack x)))

(define-move-fun (load-float-avx512 2) (vop x y)
  ((single-avx512-stack double-avx512-stack) (single-avx512-reg double-avx512-reg))
  (inst vmovups y (ea-for-avx512-stack x)))

(define-move-fun (store-int-avx512 2) (vop x y)
  ((int-avx512-reg) (int-avx512-stack))
  (inst vmovdqu64 (ea-for-avx512-stack y) x))

(define-move-fun (store-float-avx512 2) (vop x y)
  ((double-avx512-reg single-avx512-reg) (double-avx512-stack single-avx512-stack))
  (inst vmovups (ea-for-avx512-stack y) x))

(define-vop (avx512-move)
  (:args (x :scs (single-avx512-reg double-avx512-reg int-avx512-reg)
            :target y
            :load-if (not (location= x y))))
  (:results (y :scs (single-avx512-reg double-avx512-reg int-avx512-reg)
               :load-if (not (location= x y))))
  (:note "AVX512 move")
  (:generator 0
              (move y x)))

(define-move-vop avx512-move :move
  (int-avx512-reg single-avx512-reg double-avx512-reg)
  (int-avx512-reg single-avx512-reg double-avx512-reg))

(macrolet ((define-move-from-avx512 (type tag move sc stack-sc)
             (let ((name (symbolicate "MOVE-FROM-AVX512/" type)))
               `(progn
                  (define-allocator (,name)
                    (:args (x :scs (,sc)))
                    (:temporary (:sc ,sc :from (:argument 0)) tmp)
                    (:temporary (:sc ,stack-sc) stack)
                    (:results (y :scs (descriptor-reg)))
                    (:arg-types ,type)
                    (:note "AVX512 to pointer coercion")
                    (:generator 13
                      ;; Save on the stack for a potential call to alloc-tramp
                      (inst ,move (ea-for-avx512-stack stack) x)
                      (alloc-other simd-pack-512-widetag simd-pack-512-size y)
                      (inst ,move tmp (ea-for-avx512-stack stack))
                      (storew (fixnumize ,tag) y simd-pack-512-tag-slot other-pointer-lowtag)
                      (inst ,move (object-slot-ea y simd-pack-512-p0-slot other-pointer-lowtag)
                        tmp)))
                  (define-move-vop ,name :move (,sc) (descriptor-reg))))))
  ;; see +simd-pack-element-types+
  (define-move-from-avx512 simd-pack-512-single 0 vmovups single-avx512-reg single-avx512-stack)
  (define-move-from-avx512 simd-pack-512-double 1 vmovupd double-avx512-reg double-avx512-stack)
  (define-move-from-avx512 simd-pack-512-ub8    2 vmovdqu64 int-avx512-reg int-avx512-stack)
  (define-move-from-avx512 simd-pack-512-ub16   3 vmovdqu64 int-avx512-reg int-avx512-stack)
  (define-move-from-avx512 simd-pack-512-ub32   4 vmovdqu64 int-avx512-reg int-avx512-stack)
  (define-move-from-avx512 simd-pack-512-ub64   5 vmovdqu64 int-avx512-reg int-avx512-stack)
  (define-move-from-avx512 simd-pack-512-sb8    6 vmovdqu64 int-avx512-reg int-avx512-stack)
  (define-move-from-avx512 simd-pack-512-sb16   7 vmovdqu64 int-avx512-reg int-avx512-stack)
  (define-move-from-avx512 simd-pack-512-sb32   8 vmovdqu64 int-avx512-reg int-avx512-stack)
  (define-move-from-avx512 simd-pack-512-sb64   9 vmovdqu64 int-avx512-reg int-avx512-stack))

(define-vop (move-to-avx512)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (int-avx512-reg double-avx512-reg single-avx512-reg)))
  (:note "pointer to AVX512 coercion")
  (:generator 2
    (let ((ea (object-slot-ea x simd-pack-512-p0-slot other-pointer-lowtag)))
      (if (float-avx512-p y)
          (inst vmovups y ea)
          (inst vmovdqu64 y ea)))))

(define-move-vop move-to-avx512 :move
  (descriptor-reg)
  (int-avx512-reg double-avx512-reg single-avx512-reg))

(define-vop (move-avx512-arg)
  (:args (x :scs (int-avx512-reg double-avx512-reg single-avx512-reg) :target y)
         (fp :scs (any-reg)
             :load-if (not (sc-is y int-avx512-reg double-avx512-reg single-avx512-reg))))
  (:results (y))
  (:note "AVX512 argument move")
  (:generator 4
     (sc-case y
       ((int-avx512-reg double-avx512-reg single-avx512-reg)
        (unless (location= x y)
          (if (or (float-avx512-p x)
                  (float-avx512-p y))
              (inst vmovups y x)
              (inst vmovdqu64 y x))))
       ((int-avx512-stack double-avx512-stack single-avx512-stack)
        (if (float-avx512-p x)
            (inst vmovups (ea-for-avx512-stack y fp) x)
            (inst vmovdqu64 (ea-for-avx512-stack y fp) x))))))

(define-move-vop move-avx512-arg :move-arg
  (int-avx512-reg double-avx512-reg single-avx512-reg descriptor-reg)
  (int-avx512-reg double-avx512-reg single-avx512-reg))

(define-move-vop move-arg :move-arg
  (int-avx512-reg double-avx512-reg single-avx512-reg)
  (descriptor-reg))


(define-vop (%simd-pack-512-0)
  (:translate %simd-pack-512-0)
  (:args (x :scs (descriptor-reg)))
  (:arg-types simd-pack-512)
  (:results (dst :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 3
    (loadw dst x simd-pack-512-p0-slot other-pointer-lowtag)))

(define-vop (%simd-pack-512-1 %simd-pack-512-0)
  (:translate %simd-pack-512-1)
  (:generator 3
    (loadw dst x simd-pack-512-p1-slot other-pointer-lowtag)))

(define-vop (%simd-pack-512-2 %simd-pack-512-0)
  (:translate %simd-pack-512-2)
  (:generator 3
    (loadw dst x simd-pack-512-p2-slot other-pointer-lowtag)))

(define-vop (%simd-pack-512-3 %simd-pack-512-0)
  (:translate %simd-pack-512-3)
  (:generator 3
    (loadw dst x simd-pack-512-p3-slot other-pointer-lowtag)))

(define-vop (%simd-pack-512-4 %simd-pack-512-0)
  (:translate %simd-pack-512-4)
  (:generator 3
    (loadw dst x simd-pack-512-p4-slot other-pointer-lowtag)))

(define-vop (%simd-pack-512-5 %simd-pack-512-0)
  (:translate %simd-pack-512-5)
  (:generator 3
    (loadw dst x simd-pack-512-p5-slot other-pointer-lowtag)))

(define-vop (%simd-pack-512-6 %simd-pack-512-0)
  (:translate %simd-pack-512-6)
  (:generator 3
    (loadw dst x simd-pack-512-p6-slot other-pointer-lowtag)))

(define-vop (%simd-pack-512-7 %simd-pack-512-0)
  (:translate %simd-pack-512-7)
  (:generator 3
    (loadw dst x simd-pack-512-p7-slot other-pointer-lowtag)))

(define-allocator (%make-simd-pack-512)
  (:translate %make-simd-pack-512)
  (:policy :fast-safe)
  (:args (tag :scs (any-reg))
         (p0 :scs (unsigned-reg))
         (p1 :scs (unsigned-reg))
         (p2 :scs (unsigned-reg))
         (p3 :scs (unsigned-reg))
         (p4 :scs (unsigned-reg))
         (p5 :scs (unsigned-reg))
         (p6 :scs (unsigned-reg))
         (p7 :scs (unsigned-reg)))
  (:arg-types tagged-num
              unsigned-num unsigned-num unsigned-num unsigned-num
              unsigned-num unsigned-num unsigned-num unsigned-num)
  (:results (dst :scs (descriptor-reg) :from :load))
  (:result-types t)
  (:generator 13
    (alloc-other simd-pack-512-widetag simd-pack-512-size dst)
    ;; see +simd-pack-element-types+
    (storew tag dst simd-pack-512-tag-slot other-pointer-lowtag)
    (storew p0 dst simd-pack-512-p0-slot other-pointer-lowtag)
    (storew p1 dst simd-pack-512-p1-slot other-pointer-lowtag)
    (storew p2 dst simd-pack-512-p2-slot other-pointer-lowtag)
    (storew p3 dst simd-pack-512-p3-slot other-pointer-lowtag)
    (storew p4 dst simd-pack-512-p4-slot other-pointer-lowtag)
    (storew p5 dst simd-pack-512-p5-slot other-pointer-lowtag)
    (storew p6 dst simd-pack-512-p6-slot other-pointer-lowtag)
    (storew p7 dst simd-pack-512-p7-slot other-pointer-lowtag)))

(define-vop (%make-simd-pack-512-ub64)
  (:translate %make-simd-pack-512-ub64)
  (:policy :fast-safe)
  (:args (p0 :scs (unsigned-reg))
         (p1 :scs (unsigned-reg))
         (p2 :scs (unsigned-reg))
         (p3 :scs (unsigned-reg))
         (p4 :scs (unsigned-reg))
         (p5 :scs (unsigned-reg))
         (p6 :scs (unsigned-reg))
         (p7 :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num
              unsigned-num unsigned-num unsigned-num unsigned-num)
  (:results (dst :scs (int-avx512-reg)))
  (:result-types simd-pack-512-ub64)
  (:temporary (:scs (int-avx512-reg)) tmp1 tmp2 tmp3)
  (:generator 8
    ;; "xmm views" of zmm regs
    (let ((x0 (sb-c:make-random-tn (sb-c:sc-or-lose 'sb-vm::double-reg) (sb-c:tn-offset dst)))
          (x1 (sb-c:make-random-tn (sb-c:sc-or-lose 'sb-vm::double-reg) (sb-c:tn-offset tmp1)))
          (x2 (sb-c:make-random-tn (sb-c:sc-or-lose 'sb-vm::double-reg) (sb-c:tn-offset tmp2)))
          (x3 (sb-c:make-random-tn (sb-c:sc-or-lose 'sb-vm::double-reg) (sb-c:tn-offset tmp3))))

      (inst vmovq x0 p0)
      (inst vpinsrq x0 x0 p1 1)

      (inst vmovq x1 p2)
      (inst vpinsrq x1 x1 p3 1)

      (inst vmovq x2 p4)
      (inst vpinsrq x2 x2 p5 1)

      (inst vmovq x3 p6)
      (inst vpinsrq x3 x3 p7 1)

      (inst vinserti64x2 dst dst x1 1)
      (inst vinserti64x2 tmp2 tmp2 x3 1)

      (inst vinserti64x4 dst dst tmp2 1))))

(defmacro simd-pack-512-dispatch (pack &body body)
  (check-type pack symbol)
  `(let ((,pack ,pack))
     (etypecase ,pack
       ,@(map 'list (lambda (eltype)
                   `((simd-pack-512 ,eltype) ,@body))
          +simd-pack-element-types+))))

#-sb-xc-host
(progn
  (defun %make-simd-pack-512-ub32 (p0 p1 p2 p3 p4 p5 p6 p7 p8
                                   p9 p10 p11 p12 p13 p14 p15)
    (declare (type (unsigned-byte 32) p0 p1 p2 p3 p4 p5 p6 p7 p8
                                      p9 p10 p11 p12 p13 p14 p15))
    (%make-simd-pack-512
     #.(position '(unsigned-byte 32) +simd-pack-element-types+ :test #'equal)
     (logior p0 (ash p1 32))
     (logior p2 (ash p3 32))
     (logior p4 (ash p5 32))
     (logior p6 (ash p7 32))
     (logior p8 (ash p9 32))
     (logior p10 (ash p11 32))
     (logior p12 (ash p13 32))
     (logior p14 (ash p15 32)))))

(define-vop (%make-simd-pack-512-double)
  (:translate %make-simd-pack-512-double)
  (:policy :fast-safe)
  (:args (p0 :scs (double-reg) :target dst)
         (p1 :scs (double-reg))
         (p2 :scs (double-reg))
         (p3 :scs (double-reg))
         (p4 :scs (double-reg))
         (p5 :scs (double-reg))
         (p6 :scs (double-reg))
         (p7 :scs (double-reg)))
  (:arg-types double-float double-float double-float double-float
              double-float double-float double-float double-float)
  (:temporary (:scs (double-avx512-reg)) tmp1 tmp2 tmp3)
  (:results (dst :scs (double-avx512-reg) :from (:argument 0)))
  (:result-types simd-pack-512-double)
  (:generator 4
    (let ((x0  (sb-c:make-random-tn (sb-c:sc-or-lose 'sb-vm::double-reg) (sb-c:tn-offset dst)))
          (x1 (sb-c:make-random-tn (sb-c:sc-or-lose 'sb-vm::double-reg) (sb-c:tn-offset tmp1)))
          (x2 (sb-c:make-random-tn (sb-c:sc-or-lose 'sb-vm::double-reg) (sb-c:tn-offset tmp2)))
          (x3 (sb-c:make-random-tn (sb-c:sc-or-lose 'sb-vm::double-reg) (sb-c:tn-offset tmp3))))

      (inst vunpcklpd x0 p0 p1)
      (inst vunpcklpd x1 p2 p3)
      (inst vunpcklpd x2 p4 p5)
      (inst vunpcklpd x3 p6 p7)

      (inst vinsertf64x2 dst dst x1 1)
      (inst vinsertf64x2 tmp2 tmp2 x3 1)

      (inst vinsertf64x4 dst dst tmp2 1))))

(define-vop (%make-simd-pack-512-single)
  (:translate %make-simd-pack-512-single)
  (:policy :fast-safe)
  (:args (p0 :scs (single-reg) :target dst)
         (p1 :scs (single-reg))
         (p2 :scs (single-reg))
         (p3 :scs (single-reg))
         (p4 :scs (single-reg))
         (p5 :scs (single-reg))
         (p6 :scs (single-reg))
         (p7 :scs (single-reg))
         (p8 :scs (single-reg))
         (p9 :scs (single-reg))
         (p10 :scs (single-reg))
         (p11 :scs (single-reg))
         (p12 :scs (single-reg))
         (p13 :scs (single-reg))
         (p14 :scs (single-reg))
         (p15 :scs (single-reg)))
  (:arg-types single-float single-float single-float single-float
              single-float single-float single-float single-float
              single-float single-float single-float single-float
              single-float single-float single-float single-float)
  (:result-types simd-pack-512-single)
  (:results (dst :scs (single-avx512-reg) :from (:argument 4)))
  (:result-types simd-pack-512-single)
  (:temporary (:sc single-avx512-reg) t0 t1 t2 t3)
  (:generator 5
    (inst vunpcklps t0 p0 p1)
    (inst vunpcklps t1 p2 p3)
    (inst vshufps dst t0 t1 #x44)

    (inst vunpcklps t2 p4 p5)
    (inst vunpcklps t3 p6 p7)
    (inst vshufps t0 t2 t3 #x44)
    (inst vinsertf32x4 dst dst t0 1)

    (inst vunpcklps t2 p8 p9)
    (inst vunpcklps t3 p10 p11)
    (inst vshufps t0 t2 t3 #x44)
    (inst vinsertf32x4 dst dst t0 2)

    (inst vunpcklps t2 p12 p13)
    (inst vunpcklps t3 p14 p15)
    (inst vshufps t0 t2 t3 #x44)
    (inst vinsertf32x4 dst dst t0 3)))

(defknown %simd-pack-512-single-item
  (simd-pack-512 (integer 0 15)) single-float (flushable))

(define-vop (%simd-pack-512-single-item)
  (:translate %simd-pack-512-single-item)
  (:args (x :scs (int-avx512-reg double-avx512-reg single-avx512-reg)
            :target dst))
  (:info index)
  (:arg-types simd-pack-512 (:constant t))
  (:results (dst :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:sc single-reg :from (:argument 0)) tmp)
  (:policy :fast-safe)
  (:generator 3
    (multiple-value-bind (lane idx) (floor index 4)
      (inst vextractf32x4 tmp x lane)
      (if (zerop idx)
          (inst vmovss dst tmp)
          (inst vshufps dst tmp tmp idx)))))

(defknown %simd-pack-512-double-item
  (simd-pack-512 (integer 0 7)) double-float (flushable))

(define-vop (%simd-pack-512-double-item)
  (:translate %simd-pack-512-double-item)
  (:args (x :scs (int-avx512-reg double-avx512-reg single-avx512-reg)
            :target dst))
  (:info index)
  (:arg-types simd-pack-512 (:constant t))
  (:results (dst :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:sc double-reg :from (:argument 0)) tmp)
  (:policy :fast-safe)
  (:generator 3
    (multiple-value-bind (lane idx) (floor index 2)
      (inst vextractf64x2 tmp x lane)
      (if (zerop idx)
          (inst vmovsd dst tmp)
          (inst vpsrldq dst tmp 8)))))

#-sb-xc-host
(defun %simd-pack-512-inline-constant (pack)
  (list :avx512 (logior (%simd-pack-512-0 pack)
                        (ash (%simd-pack-512-1 pack) 64)
                        (ash (%simd-pack-512-2 pack) 128)
                        (ash (%simd-pack-512-3 pack) 192)
                        (ash (%simd-pack-512-4 pack) 256)
                        (ash (%simd-pack-512-5 pack) 320)
                        (ash (%simd-pack-512-6 pack) 384)
                        (ash (%simd-pack-512-7 pack) 448))))

(define-vop ()
  (:translate sap-ref-512)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
         (offset :scs (signed-reg immediate)))
  (:arg-types system-area-pointer signed-num)
  (:results (result :scs (int-avx512-reg)))
  (:result-types simd-pack-512-ub64)
  (:temporary
   (:sc unsigned-reg :unused-if (not (offset-needs-temp offset)))
   temp)
  (:generator 3
    (inst vmovdqu64 result (sap+offset-to-ea sap offset temp))))

(define-vop (set-sap-ref-512)
  (:translate (setf sap-ref-512))
  (:policy :fast-safe)
  (:args (value :scs (int-avx512-reg))
         (sap :scs (sap-reg))
         (offset :scs (signed-reg immediate)))
  (:arg-types simd-pack-512-ub64 system-area-pointer signed-num)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 5
    (inst vmovdqu64 (sap+offset-to-ea sap offset temp) value)))

(defknown %simd-pack-512-int-to-double
    ((simd-pack-512 (unsigned-byte 64))) (simd-pack-512 double-float) (flushable))
(defknown %simd-pack-512-int-to-single
    ((simd-pack-512 (unsigned-byte 64))) (simd-pack-512 single-float) (flushable))

(define-vop ()
  (:translate %simd-pack-512-int-to-double)
  (:args (x :scs (int-avx512-reg)))
  (:arg-types simd-pack-512-ub64)
  (:results (y :scs (double-avx512-reg)))
  (:result-types simd-pack-512-double)
  (:policy :fast-safe)
  (:generator 2
    (move x y)))

(define-vop ()
  (:translate %simd-pack-512-int-to-single)
  (:args (x :scs (int-avx512-reg)))
  (:arg-types simd-pack-512-ub64)
  (:results (y :scs (single-avx512-reg)))
  (:result-types simd-pack-512-single)
  (:policy :fast-safe)
  (:generator 2
    (move x y)))
