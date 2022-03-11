;;;; AVX2 intrinsics support for x86-64

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defun ea-for-avx-stack (tn &optional (base rbp-tn))
  (ea (frame-byte-offset (+ (tn-offset tn) 3)) base))

(defun float-avx2-p (tn)
  (sc-is tn single-avx2-reg single-avx2-stack single-avx2-immediate
            double-avx2-reg double-avx2-stack double-avx2-immediate))
(defun int-avx2-p (tn)
  (sc-is tn int-avx2-reg int-avx2-stack int-avx2-immediate))

#+sb-xc-host
(progn ; the host compiler will complain about absence of these
  (defun %simd-pack-256-0 (x) (error "Called %SIMD-PACK-256-0 ~S" x))
  (defun %simd-pack-256-1 (x) (error "Called %SIMD-PACK-256-1 ~S" x))
  (defun %simd-pack-256-2 (x) (error "Called %SIMD-PACK-256-2 ~S" x))
  (defun %simd-pack-256-3 (x) (error "Called %SIMD-PACK-256-3 ~S" x)))

(define-move-fun (load-int-avx2-immediate 1) (vop x y)
                 ((int-avx2-immediate) (int-avx2-reg))
  (let* ((x  (tn-value x))
         (p0 (%simd-pack-256-0 x))
         (p1 (%simd-pack-256-1 x))
         (p2 (%simd-pack-256-2 x))
         (p3 (%simd-pack-256-3 x)))
    (cond ((= p0 p1 p2 p3 0)
           (inst vpxor y y y))
          ((= p0 p1 p2 p3 (ldb (byte 64 0) -1))
           ;; don't think this is recognized as dependency breaking...
           (inst vpcmpeqd y y y))
          (t
           (inst vmovdqu y (register-inline-constant x))))))

(define-move-fun (load-float-avx2-immediate 1) (vop x y)
  ((single-avx2-immediate double-avx2-immediate)
   (single-avx2-reg double-avx2-reg))
  (let* ((x  (tn-value x))
         (p0 (%simd-pack-256-0 x))
         (p1 (%simd-pack-256-1 x))
         (p2 (%simd-pack-256-2 x))
         (p3 (%simd-pack-256-3 x)))
    (cond ((= p0 p1 p2 p3 0)
           (inst vxorps y y y))
          ((= p0 p1 p2 p3 (ldb (byte 64 0) -1))
           (inst vpcmpeqd y y y))
          (t
           (inst vmovups y (register-inline-constant x))))))

(define-move-fun (load-int-avx2 2) (vop x y)
  ((int-avx2-stack) (int-avx2-reg))
  (inst vmovdqu y (ea-for-avx-stack x)))

(define-move-fun (load-float-avx2 2) (vop x y)
  ((single-avx2-stack double-avx2-stack) (single-avx2-reg double-avx2-reg))
  (inst vmovups y (ea-for-avx-stack x)))

(define-move-fun (store-int-avx2 2) (vop x y)
  ((int-avx2-reg) (int-avx2-stack))
  (inst vmovdqu (ea-for-avx-stack y) x))

(define-move-fun (store-float-avx2 2) (vop x y)
  ((double-avx2-reg single-avx2-reg) (double-avx2-stack single-avx2-stack))
  (inst vmovups (ea-for-avx-stack y) x))

(define-vop (avx2-move)
  (:args (x :scs (single-avx2-reg double-avx2-reg int-avx2-reg)
            :target y
            :load-if (not (location= x y))))
  (:results (y :scs (single-avx2-reg double-avx2-reg int-avx2-reg)
               :load-if (not (location= x y))))
  (:note "AVX2 move")
  (:generator 0
     (move y x)))
(define-move-vop avx2-move :move
  (int-avx2-reg single-avx2-reg double-avx2-reg)
  (int-avx2-reg single-avx2-reg double-avx2-reg))

(define-vop (move-from-avx2)
  (:args (x :scs (single-avx2-reg double-avx2-reg int-avx2-reg)))
  (:results (y :scs (descriptor-reg)))
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:node-var node)
  (:note "AVX2 to pointer coercion")
  (:generator 13
     (alloc-other simd-pack-256-widetag simd-pack-256-size y node nil thread-tn)
       ;; see *simd-pack-element-types*
     (storew (fixnumize
              (sc-case x
                (single-avx2-reg 1)
                (double-avx2-reg 2)
                (int-avx2-reg 0)
                (t 0)))
         y simd-pack-256-tag-slot other-pointer-lowtag)
     (let ((ea (object-slot-ea
                y simd-pack-256-p0-slot other-pointer-lowtag)))
       (if (float-avx2-p x)
           (inst vmovups ea x)
           (inst vmovdqu ea x)))))
(define-move-vop move-from-avx2 :move
  (int-avx2-reg single-avx2-reg double-avx2-reg) (descriptor-reg))

(define-vop (move-to-avx2)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (int-avx2-reg double-avx2-reg single-avx2-reg)))
  (:note "pointer to AVX2 coercion")
  (:generator 2
    (let ((ea (object-slot-ea x simd-pack-256-p0-slot other-pointer-lowtag)))
      (if (float-avx2-p y)
          (inst vmovups y ea)
          (inst vmovdqu y ea)))))

(define-move-vop move-to-avx2 :move
  (descriptor-reg)
  (int-avx2-reg double-avx2-reg single-avx2-reg))

(define-vop (move-avx2-arg)
  (:args (x :scs (int-avx2-reg double-avx2-reg single-avx2-reg) :target y)
         (fp :scs (any-reg)
             :load-if (not (sc-is y int-avx2-reg double-avx2-reg single-avx2-reg))))
  (:results (y))
  (:note "AVX2 argument move")
  (:generator 4
     (sc-case y
       ((int-avx2-reg double-avx2-reg single-avx2-reg)
        (unless (location= x y)
          (if (or (float-avx2-p x)
                  (float-avx2-p y))
              (inst vmovups y x)
              (inst vmovdqu y x))))
       ((int-avx2-stack double-avx2-stack single-avx2-stack)
        (if (float-avx2-p x)
            (inst vmovups (ea-for-avx-stack y fp) x)
            (inst vmovdqu (ea-for-avx-stack y fp) x))))))
(define-move-vop move-avx2-arg :move-arg
  (int-avx2-reg double-avx2-reg single-avx2-reg descriptor-reg)
  (int-avx2-reg double-avx2-reg single-avx2-reg))

(define-move-vop move-arg :move-arg
  (int-avx2-reg double-avx2-reg single-avx2-reg)
  (descriptor-reg))


(define-vop (%simd-pack-256-0)
  (:translate %simd-pack-256-0)
  (:args (x :scs (int-avx2-reg double-avx2-reg single-avx2-reg)))
  (:arg-types simd-pack-256)
  (:results (dst :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 3
    (inst vmovq dst x)))

(define-vop (%simd-pack-256-1)
  (:translate %simd-pack-256-1)
  (:args (x :scs (int-avx2-reg double-avx2-reg single-avx2-reg)))
  (:arg-types simd-pack-256)
  (:results (dst :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 3
    (inst vpextrq dst x 1)))

(define-vop (%simd-pack-256-2)
  (:translate %simd-pack-256-2)
  (:args (x :scs (int-avx2-reg double-avx2-reg single-avx2-reg)))
  (:arg-types simd-pack-256)
  (:temporary (:sc ymm-reg :from (:argument 1)) tmp)
  (:results (dst :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 3
    (inst vextracti128 tmp x 1)
    (inst vmovq dst tmp)))

(define-vop (%simd-pack-256-3)
  (:translate %simd-pack-256-3)
  (:args (x :scs (int-avx2-reg double-avx2-reg single-avx2-reg)))
  (:arg-types simd-pack-256)
  (:temporary (:sc ymm-reg :from (:argument 1)) tmp)
  (:results (dst :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 3
    (inst vextracti128 tmp x 1)
    (inst vpextrq dst tmp 1)))

(define-vop (%make-simd-pack-256)
  (:translate %make-simd-pack-256)
  (:policy :fast-safe)
  (:args (tag :scs (any-reg))
         (p0 :scs (unsigned-reg))
         (p1 :scs (unsigned-reg))
         (p2 :scs (unsigned-reg))
         (p3 :scs (unsigned-reg)))
  (:arg-types tagged-num unsigned-num unsigned-num unsigned-num unsigned-num)
  (:results (dst :scs (descriptor-reg) :from :load))
  (:result-types t)
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:node-var node)
  (:generator 13
    (alloc-other simd-pack-256-widetag simd-pack-256-size dst node nil thread-tn)
    ;; see *simd-pack-element-types*
    (storew tag dst simd-pack-256-tag-slot other-pointer-lowtag)
    (storew p0 dst simd-pack-256-p0-slot other-pointer-lowtag)
    (storew p1 dst simd-pack-256-p1-slot other-pointer-lowtag)
    (storew p2 dst simd-pack-256-p2-slot other-pointer-lowtag)
    (storew p3 dst simd-pack-256-p3-slot other-pointer-lowtag)))

(define-vop (%make-simd-pack-256-ub64)
  (:translate %make-simd-pack-256-ub64)
  (:policy :fast-safe)
  (:args (p0 :scs (unsigned-reg))
         (p1 :scs (unsigned-reg))
         (p2 :scs (unsigned-reg))
         (p3 :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc int-avx2-reg) tmp)
  (:results (dst :scs (int-avx2-reg)))
  (:result-types simd-pack-256-int)
  (:generator 5
    (inst vmovq dst p0)
    (inst vpinsrq dst dst p1 1)
    (inst vmovq tmp p2)
    (inst vpinsrq tmp tmp p3 1)
    (inst vinserti128 dst dst tmp 1)))

(defmacro simd-pack-256-dispatch (pack &body body)
  (check-type pack symbol)
  `(let ((,pack ,pack))
     (etypecase ,pack
       ((simd-pack-256 double-float) ,@body)
       ((simd-pack-256 single-float) ,@body)
       ((simd-pack-256 integer) ,@body))))

#-sb-xc-host
(progn
  (declaim (inline %make-simd-pack-256-ub32))
  (defun %make-simd-pack-256-ub32 (p0 p1 p2 p3 p4 p5 p6 p7)
    (declare (type (unsigned-byte 32) p0 p1 p2 p3 p4 p5 p6 p7))
    (%make-simd-pack-256-ub64 (logior p0 (ash p1 32))
                              (logior p2 (ash p3 32))
                              (logior p4 (ash p5 32))
                              (logior p6 (ash p7 32))))

  (declaim (inline %simd-pack-256-ub32s %simd-pack-256-ub64s))
  (defun %simd-pack-256-ub32s (pack)
    (declare (type simd-pack-256 pack))
    (simd-pack-256-dispatch pack
      (let ((p0 (%simd-pack-256-0 pack))
            (p1 (%simd-pack-256-1 pack))
            (p2 (%simd-pack-256-2 pack))
            (p3 (%simd-pack-256-3 pack)))
        (values (ldb (byte 32 0) p0) (ash p0 -32)
                (ldb (byte 32 0) p1) (ash p1 -32)
                (ldb (byte 32 0) p2) (ash p2 -32)
                (ldb (byte 32 0) p3) (ash p3 -32)))))

  (defun %simd-pack-256-ub64s (pack)
    (declare (type simd-pack-256 pack))
    (simd-pack-256-dispatch pack
      (values (%simd-pack-256-0 pack)
              (%simd-pack-256-1 pack)
              (%simd-pack-256-2 pack)
              (%simd-pack-256-3 pack)))))

(define-vop (%make-simd-pack-256-double)
  (:translate %make-simd-pack-256-double)
  (:policy :fast-safe)
  (:args (p0 :scs (double-reg) :target dst)
         (p1 :scs (double-reg))
         (p2 :scs (double-reg))
         (p3 :scs (double-reg)))
  (:arg-types double-float double-float double-float double-float)
  (:temporary (:sc int-avx2-reg) tmp)
  (:results (dst :scs (double-avx2-reg) :from (:argument 0)))
  (:result-types simd-pack-256-double)
  (:generator 5
   (inst vunpcklpd dst p0 p1)
   (inst vunpcklpd tmp p2 p3)
   (inst vinsertf128 dst dst tmp 1)))

(define-vop (%make-simd-pack-256-single)
  (:translate %make-simd-pack-256-single)
  (:policy :fast-safe)
  (:args (p0 :scs (single-reg) :target dst)
         (p1 :scs (single-reg))
         (p2 :scs (single-reg))
         (p3 :scs (single-reg))
         (p4 :scs (single-reg) :target tmp)
         (p5 :scs (single-reg))
         (p6 :scs (single-reg))
         (p7 :scs (single-reg)))
  (:arg-types single-float single-float single-float single-float
              single-float single-float single-float single-float)
  (:temporary (:sc single-avx2-reg  :from (:argument 3)) tmp)
  (:results (dst :scs (single-avx2-reg) :from (:argument 0)))
  (:result-types simd-pack-256-single)
  (:generator 5
    (inst vinsertps dst p0 p1 (ash 1 4))
    (inst vinsertps dst dst p2 (ash 2 4))
    (inst vinsertps dst dst p3 (ash 3 4))
    (inst vinsertps tmp p4 p5 (ash 1 4))
    (inst vinsertps tmp tmp p6 (ash 2 4))
    (inst vinsertps tmp tmp p7 (ash 3 4))
    (inst vinsertf128 dst dst tmp 1)))

(defknown %simd-pack-256-single-item
  (simd-pack-256 (integer 0 7)) single-float (flushable))

(define-vop (%simd-pack-256-single-item)
  (:args (x :scs (int-avx2-reg double-avx2-reg single-avx2-reg)
            :target tmp))
  (:translate %simd-pack-256-single-item)
  (:arg-types simd-pack-256 (:constant t))
  (:info index)
  (:results (dst :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:sc single-avx2-reg :from (:argument 0)) tmp)
  (:policy :fast-safe)
  (:generator 3
    (cond ((>= index 4)
           (decf index 4)
           (inst vextractf128 tmp x 1))
          (t
           (move tmp x)))
    (when (plusp index)
      (inst vpsrldq tmp tmp (* 4 index)))
    (inst vxorps dst dst dst)
    (inst movss dst tmp)))

#-sb-xc-host
(progn
(declaim (inline %simd-pack-256-singles))
(defun %simd-pack-256-singles (pack)
  (declare (type simd-pack-256 pack))
  (simd-pack-256-dispatch pack
    (values (%simd-pack-256-single-item pack 0)
            (%simd-pack-256-single-item pack 1)
            (%simd-pack-256-single-item pack 2)
            (%simd-pack-256-single-item pack 3)
            (%simd-pack-256-single-item pack 4)
            (%simd-pack-256-single-item pack 5)
            (%simd-pack-256-single-item pack 6)
            (%simd-pack-256-single-item pack 7)))))

(defknown %simd-pack-256-double-item
  (simd-pack-256 (integer 0 3)) double-float (flushable))

(define-vop (%simd-pack-256-double-item)
  (:translate %simd-pack-256-double-item)
  (:args (x :scs (int-avx2-reg double-avx2-reg single-avx2-reg)
            :target tmp))
  (:info index)
  (:arg-types simd-pack-256 (:constant t))
  (:results (dst :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:sc double-avx2-reg :from (:argument 0)) tmp)
  (:policy :fast-safe)
  (:generator 3
    (cond ((>= index 2)
           (decf index 2)
           (inst vextractf128 tmp x 1))
          (t
           (move tmp x)))
    (when (plusp index)
      (inst vpsrldq tmp tmp (* 8 index)))
    (inst vxorpd dst dst dst)
    (inst movsd dst tmp)))

#-sb-xc-host
(progn
(declaim (inline %simd-pack-256-doubles))
(defun %simd-pack-256-doubles (pack)
  (declare (type simd-pack-256 pack))
  (simd-pack-256-dispatch pack
    (values (%simd-pack-256-double-item pack 0)
            (%simd-pack-256-double-item pack 1)
            (%simd-pack-256-double-item pack 2)
            (%simd-pack-256-double-item pack 3))))

(defun %simd-pack-256-inline-constant (pack)
  (list :avx2 (logior (%simd-pack-256-0 pack)
                      (ash (%simd-pack-256-1 pack) 64)
                      (ash (%simd-pack-256-2 pack) 128)
                      (ash (%simd-pack-256-3 pack) 192)))))
