(in-package #:sb-simd-sse)

(define-fake-vop f32-not (a)
  (%f32-andc1 a +f32-true+))

(define-fake-vop make-f32.4 (a b c d)
  (%f32.4-unpacklo
   (%f32.4-unpacklo
    (%f32.4!-from-f32 a)
    (%f32.4!-from-f32 c))
   (%f32.4-unpacklo
    (%f32.4!-from-f32 b)
    (%f32.4!-from-f32 d))))

(define-fake-vop f32.4-values (x)
  (let* ((zero (sb-ext:%make-simd-pack-single 0f0 0f0 0f0 0f0))
         (a0b0 (%f32.4-unpacklo x zero))
         (c0d0 (%f32.4-unpackhi x zero)))
    (values
     (%f32!-from-p128 (%f32.4-unpacklo a0b0 zero))
     (%f32!-from-p128 (%f32.4-unpackhi a0b0 zero))
     (%f32!-from-p128 (%f32.4-unpacklo c0d0 zero))
     (%f32!-from-p128 (%f32.4-unpackhi c0d0 zero)))))

(define-fake-vop f32.4-broadcast (x)
  (let ((v (%f32.4!-from-f32 x)))
    (%f32.4-shuffle v v 0)))

(define-fake-vop f32.4-not (a)
  (%f32.4-andc1
   a
   (%make-f32.4 +f32-true+ +f32-true+ +f32-true+ +f32-true+)))

(macrolet ((def (name op)
             `(define-fake-vop ,name (x)
                (let ((y (,op x (%f32.4-shuffle x x #4r2301))))
                  (%f32!-from-p128 (,op y (%f32.4-shuffle y y #4r1032)))))))
  (def f32.4-horizontal-and %two-arg-f32.4-and)
  (def f32.4-horizontal-or %two-arg-f32.4-or)
  (def f32.4-horizontal-xor %two-arg-f32.4-xor)
  (def f32.4-horizontal-max %two-arg-f32.4-max)
  (def f32.4-horizontal-min %two-arg-f32.4-min)
  (def f32.4-horizontal+ %two-arg-f32.4+)
  (def f32.4-horizontal* %two-arg-f32.4*))

(in-package #:sb-simd-sse2)

(define-fake-vop u8!-from-p128 (x)
  (logand #xff (%u64!-from-p128 x)))

(define-fake-vop u16!-from-p128 (x)
  (logand #xffff (%u64!-from-p128 x)))

(define-fake-vop u32!-from-p128 (x)
  (logand #xffffffff (%u64!-from-p128 x)))

(define-fake-vop f64-not (a)
  (%f64-andc1 a +f64-true+))

(define-fake-vop make-f64.2 (a b)
  (%f64.2-unpacklo
   (%f64.2!-from-f64 a)
   (%f64.2!-from-f64 b)))

(define-fake-vop f64.2-values (x)
  (values
   (%f64!-from-p128 x)
   (%f64!-from-p128 (%f64.2-shuffle x x 1))))

(define-fake-vop f64.2-broadcast (x)
  (let ((v (%f64.2!-from-f64 x)))
    (%f64.2-unpacklo v v)))

(define-fake-vop f64.2-not (a)
  (%f64.2-andc1
   a
   (%make-f64.2 +f64-true+ +f64-true+)))

(macrolet ((def (name op)
             `(define-fake-vop ,name (x)
                (multiple-value-bind (a b) (%f64.2-values x)
                  (,op a b)))))
  (def f64.2-horizontal-and %two-arg-f64-and)
  (def f64.2-horizontal-or %two-arg-f64-or)
  (def f64.2-horizontal-xor %two-arg-f64-xor)
  (def f64.2-horizontal-max %two-arg-f64-max)
  (def f64.2-horizontal-min %two-arg-f64-min)
  (def f64.2-horizontal+ %two-arg-f64+)
  (def f64.2-horizontal* %two-arg-f64*))

(define-fake-vop make-u8.16 (a b c d e f g h i j k l m n o p)
  (%u8.16-unpacklo
   (%u8.16!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-u8s a c e g i k m o)))
   (%u8.16!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-u8s b d f h j l n p)))))

(define-fake-vop u8.16-values (x)
  (multiple-value-call #'values
    (sb-simd::%u8s-from-u64 (%u64!-from-p128 x))
    (sb-simd::%u8s-from-u64 (%u64!-from-p128 (%u32.4-shuffle (%u32.4!-from-p128 x) #b00001110)))))

(define-fake-vop u8.16-broadcast (x)
  (let ((v (%u64.2!-from-u64 (sb-simd::%u64-from-u8s x x x x x x x x))))
    (%u8.16!-from-p128 (%u64.2-unpacklo v v))))

(define-fake-vop u8.16-not (a)
  (let* ((x +u8-true+)
         (v (%make-u8.16 x x x x x x x x x x x x x x x x)))
    (%u8.16-andc1 a v)))

(define-fake-vop two-arg-u8.16/= (a b)
  (%u8.16-not
   (%two-arg-u8.16= a b)))

(define-fake-vop two-arg-u8.16> (a b)
  (let* ((x (expt 2 7))
         (v (%make-u8.16 x x x x x x x x x x x x x x x x)))
    (%two-arg-u8.16>~ (%two-arg-u8.16- a v)
                      (%two-arg-u8.16- b v))))

(define-fake-vop two-arg-u8.16< (a b)
  (%two-arg-u8.16> b a))

(define-fake-vop two-arg-u8.16>= (a b)
  (%u8.16-not
   (%two-arg-u8.16< a b)))

(define-fake-vop two-arg-u8.16<= (a b)
  (%u8.16-not
   (%two-arg-u8.16> a b)))

(define-fake-vop make-u16.8 (a b c d e f g h)
  (%u16.8-unpacklo
   (%u16.8!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-u16s a c e g)))
   (%u16.8!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-u16s b d f h)))))

(define-fake-vop u16.8-values (x)
  (multiple-value-call #'values
    (sb-simd::%u16s-from-u64 (%u64!-from-p128 x))
    (sb-simd::%u16s-from-u64 (%u64!-from-p128 (%u32.4-shuffle (%u32.4!-from-p128 x) #b00001110)))))

(define-fake-vop u16.8-broadcast (x)
  (let ((v (%u64.2!-from-u64 (sb-simd::%u64-from-u16s x x x x))))
    (%u16.8!-from-p128 (%u64.2-unpacklo v v))))

(define-fake-vop u16.8-not (a)
  (%u16.8-andc1
   a
   (%make-u16.8 +u16-true+ +u16-true+ +u16-true+ +u16-true+
                +u16-true+ +u16-true+ +u16-true+ +u16-true+)))

(define-fake-vop two-arg-u16.8/= (a b)
  (%u16.8-not
   (%two-arg-u16.8= a b)))

(define-fake-vop two-arg-u16.8> (a b)
  (let* ((x (expt 2 15))
         (v (%make-u16.8 x x x x x x x x)))
    (%two-arg-u16.8>~ (%two-arg-u16.8- a v)
                      (%two-arg-u16.8- b v))))

(define-fake-vop two-arg-u16.8< (a b)
  (%two-arg-u16.8> b a))

(define-fake-vop two-arg-u16.8>= (a b)
  (%u16.8-not
   (%two-arg-u16.8< a b)))

(define-fake-vop two-arg-u16.8<= (a b)
  (%u16.8-not
   (%two-arg-u16.8> a b)))

(define-fake-vop u16.8-movemask (a)
  (u16-odd-bits
   (%u8.16-movemask
    (%u8.16!-from-p128 a))))

(define-fake-vop make-u32.4 (a b c d)
  (%u32.4-unpacklo
   (%u32.4!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-u32s a c)))
   (%u32.4!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-u32s b d)))))

(define-fake-vop u32.4-values (x)
  (multiple-value-call #'values
    (sb-simd::%u32s-from-u64 (%u64!-from-p128 x))
    (sb-simd::%u32s-from-u64 (%u64!-from-p128 (%u32.4-shuffle (%u32.4!-from-p128 x) #b00001110)))))

(define-fake-vop u32.4-broadcast (x)
  (let ((v (%u64.2!-from-u64 (sb-simd::%u64-from-u32s x x))))
    (%u32.4!-from-p128 (%u64.2-unpacklo v v))))

(define-fake-vop u32.4-not (a)
  (%u32.4-andc1
   a
   (%make-u32.4 +u32-true+ +u32-true+ +u32-true+ +u32-true+)))

(define-fake-vop two-arg-u32.4/= (a b)
  (%u32.4-not
   (%two-arg-u32.4= a b)))

(define-fake-vop two-arg-u32.4> (a b)
  (let* ((x (expt 2 31))
         (v (%make-u32.4 x x x x)))
    (%two-arg-u32.4>~ (%two-arg-u32.4- a v)
                      (%two-arg-u32.4- b v))))

(define-fake-vop two-arg-u32.4< (a b)
  (%two-arg-u32.4> b a))

(define-fake-vop two-arg-u32.4>= (a b)
  (%u32.4-not
   (%two-arg-u32.4< a b)))

(define-fake-vop two-arg-u32.4<= (a b)
  (%u32.4-not
   (%two-arg-u32.4> a b)))

(define-fake-vop make-u64.2 (a b)
  (%u64.2-unpacklo
   (%u64.2!-from-u64 a)
   (%u64.2!-from-u64 b)))

(define-fake-vop u64.2-values (x)
  (values
   (%u64!-from-p128 x)
   (%u64!-from-p128 (%u32.4-shuffle (%u32.4!-from-p128 x) #b00001110))))

(define-fake-vop u64.2-broadcast (x)
  (let ((v (%u64.2!-from-u64 x)))
    (%u64.2-unpacklo v v)))

(define-fake-vop u64.2-not (a)
  (%u64.2-andc1
   a
   (%make-u64.2 +u64-true+ +u64-true+)))

(define-fake-vop s8.16!-from-s8 (x)
  (%s8.16!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s8s x 0 0 0 0 0 0 0))))

(define-fake-vop make-s8.16 (a b c d e f g h i j k l m n o p)
  (%s8.16-unpacklo
   (%s8.16!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s8s a c e g i k m o)))
   (%s8.16!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s8s b d f h j l n p)))))

(define-fake-vop s8.16-values (x)
  (multiple-value-call #'values
    (sb-simd::%s8s-from-u64 (%u64!-from-p128 x))
    (sb-simd::%s8s-from-u64 (%u64!-from-p128 (%u32.4-shuffle (%u32.4!-from-p128 x) #b00001110)))))

(define-fake-vop s8.16-broadcast (x)
  (let ((v (%u64.2!-from-u64 (sb-simd::%u64-from-s8s x x x x x x x x))))
    (%s8.16!-from-p128 (%u64.2-unpacklo v v))))

(define-fake-vop s8.16-not (a)
  (%s8.16-andc1
   a
   (%make-s8.16 +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+)))

(define-fake-vop two-arg-s8.16/= (a b)
  (%u8.16-not
   (%two-arg-s8.16= a b)))

(define-fake-vop two-arg-s8.16< (a b)
  (%two-arg-s8.16> b a))

(define-fake-vop two-arg-s8.16>= (a b)
  (%u8.16-not
   (%two-arg-s8.16< a b)))

(define-fake-vop two-arg-s8.16<= (a b)
  (%u8.16-not
   (%two-arg-s8.16> a b)))

(define-fake-vop s16.8!-from-s16 (x)
  (%s16.8!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s16s x 0 0 0))))

(define-fake-vop make-s16.8 (a b c d e f g h)
  (%s16.8-unpacklo
   (%s16.8!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s16s a c e g)))
   (%s16.8!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s16s b d f h)))))

(define-fake-vop s16.8-values (x)
  (multiple-value-call #'values
    (sb-simd::%s16s-from-u64 (%u64!-from-p128 x))
    (sb-simd::%s16s-from-u64 (%u64!-from-p128 (%u32.4-shuffle (%u32.4!-from-p128 x) #b00001110)))))

(define-fake-vop s16.8-broadcast (x)
  (let ((v (%u64.2!-from-u64 (sb-simd::%u64-from-s16s x x x x))))
    (%s16.8!-from-p128 (%u64.2-unpacklo v v))))

(define-fake-vop s16.8-not (a)
  (%s16.8-andc1
   a
   (%make-s16.8 +s16-true+ +s16-true+ +s16-true+ +s16-true+
                +s16-true+ +s16-true+ +s16-true+ +s16-true+)))

(define-fake-vop two-arg-s16.8/= (a b)
  (%u16.8-not
   (%two-arg-s16.8= a b)))

(define-fake-vop two-arg-s16.8< (a b)
  (%two-arg-s16.8> b a))

(define-fake-vop two-arg-s16.8>= (a b)
  (%u16.8-not
   (%two-arg-s16.8< a b)))

(define-fake-vop two-arg-s16.8<= (a b)
  (%u16.8-not
   (%two-arg-s16.8> a b)))

(define-fake-vop s16.8-movemask (a)
  (u16-odd-bits
   (%u8.16-movemask
    (%u8.16!-from-p128 a))))

(define-fake-vop s32.4!-from-s32 (x)
  (%s32.4!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s32s x 0))))

(define-fake-vop make-s32.4 (a b c d)
  (%s32.4-unpacklo
   (%s32.4!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s32s a c)))
   (%s32.4!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s32s b d)))))

(define-fake-vop s32.4-values (x)
  (multiple-value-call #'values
    (sb-simd::%s32s-from-u64 (%u64!-from-p128 x))
    (sb-simd::%s32s-from-u64 (%u64!-from-p128 (%u32.4-shuffle (%u32.4!-from-p128 x) #b00001110)))))

(define-fake-vop s32.4-broadcast (x)
  (let ((v (%u64.2!-from-u64 (sb-simd::%u64-from-s32s x x))))
    (%s32.4!-from-p128 (%u64.2-unpacklo v v))))

(define-fake-vop s32.4-not (a)
  (%s32.4-andc1
   a
   (%make-s32.4 +s32-true+ +s32-true+ +s32-true+ +s32-true+)))

(define-fake-vop two-arg-s32.4/= (a b)
  (%u32.4-not
   (%two-arg-s32.4= a b)))

(define-fake-vop two-arg-s32.4< (a b)
  (%two-arg-s32.4> b a))

(define-fake-vop two-arg-s32.4>= (a b)
  (%u32.4-not
   (%two-arg-s32.4< a b)))

(define-fake-vop two-arg-s32.4<= (a b)
  (%u32.4-not
   (%two-arg-s32.4> a b)))

(define-fake-vop s64.2!-from-s64 (x)
  (%s64.2!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s64 x))))

(define-fake-vop make-s64.2 (a b)
  (%s64.2-unpacklo
   (%s64.2!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s64 a)))
   (%s64.2!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s64 b)))))

(define-fake-vop s64.2-values (x)
  (values
   (sb-simd::%s64-from-u64 (%u64!-from-p128 x))
   (sb-simd::%s64-from-u64 (%u64!-from-p128 (%u32.4-shuffle (%u32.4!-from-p128 x) #b00001110)))))

(define-fake-vop s64.2-broadcast (x)
  (let ((v (%u64.2!-from-u64 (sb-simd::%u64-from-s64 x))))
    (%s64.2!-from-p128 (%u64.2-unpacklo v v))))

(define-fake-vop s64.2-not (a)
  (%s64.2-andc1
   a
   (%make-s64.2 +s64-true+ +s64-true+)))

(in-package #:sb-simd-sse4.1)

(define-fake-vop two-arg-u64.2/= (a b)
  (sb-simd-sse2::%u64.2-not
   (%two-arg-u64.2= a b)))

(define-fake-vop two-arg-s64.2/= (a b)
  (sb-simd-sse2::%u64.2-not
   (%two-arg-s64.2= a b)))

(in-package #:sb-simd-sse4.2)

(define-fake-vop two-arg-u64.2> (a b)
  (let* ((x (expt 2 63))
         (v (sb-simd-sse2::%make-u64.2 x x)))
    (%two-arg-u64.2>~ (sb-simd-sse2::%two-arg-u64.2- a v)
                      (sb-simd-sse2::%two-arg-u64.2- b v))))

(define-fake-vop two-arg-u64.2< (a b)
  (%two-arg-u64.2> b a))

(define-fake-vop two-arg-u64.2>= (a b)
  (sb-simd-sse2::%u64.2-not
   (%two-arg-u64.2< a b)))

(define-fake-vop two-arg-u64.2<= (a b)
  (sb-simd-sse2::%u64.2-not
   (%two-arg-u64.2> a b)))

(define-fake-vop two-arg-s64.2< (a b)
  (%two-arg-s64.2> b a))

(define-fake-vop two-arg-s64.2>= (a b)
  (sb-simd-sse2::%u64.2-not
   (%two-arg-s64.2< a b)))

(define-fake-vop two-arg-s64.2<= (a b)
  (sb-simd-sse2::%u64.2-not
   (%two-arg-s64.2> a b)))

(in-package #:sb-simd-avx)

(define-fake-vop u8!-from-p128 (x)
  (logand #xff (%u64!-from-p128 x)))

(define-fake-vop u8!-from-p256 (x)
  (logand #xff (%u64!-from-p256 x)))

(define-fake-vop u16!-from-p128 (x)
  (logand #xffff (%u64!-from-p128 x)))

(define-fake-vop u16!-from-p256 (x)
  (logand #xffff (%u64!-from-p256 x)))

(define-fake-vop u32!-from-p128 (x)
  (logand #xffffffff (%u64!-from-p128 x)))

(define-fake-vop u32!-from-p256 (x)
  (logand #xffffffff (%u64!-from-p256 x)))

(define-fake-vop f32-not (a)
  (%f32-andc1 a +f32-true+))

(define-fake-vop f64-not (a)
  (%f64-andc1 a +f64-true+))

(define-fake-vop make-f32.4 (a b c d)
  (%f32.4-unpacklo
   (%f32.4-unpacklo
    (%f32.4!-from-f32 a)
    (%f32.4!-from-f32 c))
   (%f32.4-unpacklo
    (%f32.4!-from-f32 b)
    (%f32.4!-from-f32 d))))

(define-fake-vop f32.4-values (x)
  (values
   (%f32!-from-p128 x)
   (%f32!-from-p128 (%f32.4-permute x 1))
   (%f32!-from-p128 (%f32.4-permute x 2))
   (%f32!-from-p128 (%f32.4-permute x 3))))

(define-fake-vop f32.4-not (a)
  (%f32.4-andc1
   a
   (%make-f32.4 +f32-true+ +f32-true+ +f32-true+ +f32-true+)))

(macrolet ((def (name op)
             `(define-fake-vop ,name (x)
                (let ((y (,op x (%f32.4-shuffle x x #4r2301))))
                  (%f32!-from-p128 (,op y (%f32.4-shuffle y y #4r1032)))))))
  (def f32.4-horizontal-and %two-arg-f32.4-and)
  (def f32.4-horizontal-or %two-arg-f32.4-or)
  (def f32.4-horizontal-xor %two-arg-f32.4-xor)
  (def f32.4-horizontal-max %two-arg-f32.4-max)
  (def f32.4-horizontal-min %two-arg-f32.4-min)
  (def f32.4-horizontal+ %two-arg-f32.4+)
  (def f32.4-horizontal* %two-arg-f32.4*))

(define-fake-vop make-f64.2 (a b)
  (%f64.2-unpacklo
   (%f64.2!-from-f64 a)
   (%f64.2!-from-f64 b)))

(define-fake-vop f64.2-values (x)
  (values
   (%f64!-from-p128 x)
   (%f64!-from-p128 (%f64.2-permute x 1))))

(define-fake-vop f64.2-not (a)
  (%f64.2-andc1
   a
   (%make-f64.2 +f64-true+ +f64-true+)))

(macrolet ((def (name op)
             `(define-fake-vop ,name (x)
                (multiple-value-bind (a b) (%f64.2-values x)
                  (,op a b)))))
  (def f64.2-horizontal-and %two-arg-f64-and)
  (def f64.2-horizontal-or %two-arg-f64-or)
  (def f64.2-horizontal-xor %two-arg-f64-xor)
  (def f64.2-horizontal-max %two-arg-f64-max)
  (def f64.2-horizontal-min %two-arg-f64-min)
  (def f64.2-horizontal+ %two-arg-f64+)
  (def f64.2-horizontal* %two-arg-f64*))

(define-fake-vop make-f32.8 (a b c d e f g h)
  (let ((lo (%make-f32.4 a b c d))
        (hi (%make-f32.4 e f g h)))
    (%f32.8-insert-f32.4 (%f32.8!-from-p128 lo) hi 1)))

(define-fake-vop f32.8-values (x)
  (multiple-value-call #'values
    (%f32.4-values (%f32.4!-from-p256 x))
    (%f32.4-values (%f32.4-from-f32.8 x 1))))

(define-fake-vop f32.8-not (a)
  (%f32.8-andc1
   a
   (%make-f32.8 +f32-true+ +f32-true+ +f32-true+ +f32-true+
                +f32-true+ +f32-true+ +f32-true+ +f32-true+)))

(macrolet ((def (name op)
             `(define-fake-vop ,name (x)
                (let* ((x (,op x (%f32.8-dupodd x)))
                       (x (,op x (%f32.8-permute x #4r0202)))
                       (x (,op x (%f32.8-permute128 x x #4r01))))
                  (%f32!-from-p256 x)))))
  (def f32.8-horizontal-and %two-arg-f32.8-and)
  (def f32.8-horizontal-or %two-arg-f32.8-or)
  (def f32.8-horizontal-xor %two-arg-f32.8-xor)
  (def f32.8-horizontal-max %two-arg-f32.8-max)
  (def f32.8-horizontal-min %two-arg-f32.8-min)
  (def f32.8-horizontal+ %two-arg-f32.8+)
  (def f32.8-horizontal* %two-arg-f32.8*))

(define-fake-vop make-f64.4 (a b c d)
  (let ((lo (%make-f64.2 a b))
        (hi (%make-f64.2 c d)))
    (%f64.4-insert-f64.2 (%f64.4!-from-p128 lo) hi 1)))

(define-fake-vop f64.4-values (x)
  (multiple-value-call #'values
    (%f64.2-values (%f64.2!-from-p256 x))
    (%f64.2-values (%f64.2-from-f64.4 x 1))))

(define-fake-vop f64.4-not (a)
  (%f64.4-andc1
   a
   (%make-f64.4 +f64-true+ +f64-true+ +f64-true+ +f64-true+)))

(macrolet ((def (name op )
             `(define-fake-vop ,name (x)
                (let* ((y (,op x (%f64.4-permute x #b0101)))
                       (z (,op y (%f64.4-permute128 y y #4r01))))
                  (%f64!-from-p256 z)))))
  (def f64.4-horizontal-and %two-arg-f64.4-and)
  (def f64.4-horizontal-or  %two-arg-f64.4-or)
  (def f64.4-horizontal-xor %two-arg-f64.4-xor)
  (def f64.4-horizontal-max %two-arg-f64.4-max)
  (def f64.4-horizontal-min %two-arg-f64.4-min)
  (def f64.4-horizontal+    %two-arg-f64.4+)
  (def f64.4-horizontal*    %two-arg-f64.4*))

(define-fake-vop make-u8.16 (a b c d e f g h i j k l m n o p)
  (%u8.16-unpacklo
   (%u8.16!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-u8s a c e g i k m o)))
   (%u8.16!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-u8s b d f h j l n p)))))

(define-fake-vop u8.16-values (x)
  (multiple-value-call #'values
    (sb-simd::%u8s-from-u64 (%u64!-from-p128 x))
    (sb-simd::%u8s-from-u64 (%u64!-from-p128 (%u64.2-permute (%u64.2!-from-p128 x) 1)))))

(define-fake-vop u8.16-broadcast (x)
  (let ((v (%u64.2!-from-u64 (sb-simd::%u64-from-u8s x x x x x x x x))))
    (%u8.16!-from-p128 (%u64.2-unpacklo v v))))

(define-fake-vop u8.16-not (a)
  (%u8.16-andc1
   a
   (%make-u8.16 +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+)))

(define-fake-vop two-arg-u8.16/= (a b)
  (%u8.16-not
   (%two-arg-u8.16= a b)))

(define-fake-vop two-arg-u8.16> (a b)
  (let* ((x (expt 2 7))
         (v (%make-u8.16 x x x x x x x x x x x x x x x x)))
    (%two-arg-u8.16>~ (%two-arg-u8.16- a v)
                      (%two-arg-u8.16- b v))))

(define-fake-vop two-arg-u8.16< (a b)
  (%two-arg-u8.16> b a))

(define-fake-vop two-arg-u8.16>= (a b)
  (%u8.16-not
   (%two-arg-u8.16< a b)))

(define-fake-vop two-arg-u8.16<= (a b)
  (%u8.16-not
   (%two-arg-u8.16> a b)))

(define-fake-vop make-u16.8 (a b c d e f g h)
  (%u16.8-unpacklo
   (%u16.8!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-u16s a c e g)))
   (%u16.8!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-u16s b d f h)))))

(define-fake-vop u16.8-values (x)
  (multiple-value-call #'values
    (sb-simd::%u16s-from-u64 (%u64!-from-p128 x))
    (sb-simd::%u16s-from-u64 (%u64!-from-p128 (%u64.2-permute (%u64.2!-from-p128 x) 1)))))

(define-fake-vop u16.8-broadcast (x)
  (let ((v (%u64.2!-from-u64 (sb-simd::%u64-from-u16s x x x x))))
    (%u16.8!-from-p128 (%u64.2-unpacklo v v))))

(define-fake-vop u16.8-not (a)
  (%u16.8-andc1
   a
   (%make-u16.8 +u16-true+ +u16-true+ +u16-true+ +u16-true+
                +u16-true+ +u16-true+ +u16-true+ +u16-true+)))

(define-fake-vop two-arg-u16.8/= (a b)
  (%u16.8-not
   (%two-arg-u16.8= a b)))

(define-fake-vop two-arg-u16.8> (a b)
  (let* ((x (expt 2 15))
         (v (%make-u16.8 x x x x x x x x)))
    (%two-arg-u16.8>~ (%two-arg-u16.8- a v)
                      (%two-arg-u16.8- b v))))

(define-fake-vop two-arg-u16.8< (a b)
  (%two-arg-u16.8> b a))

(define-fake-vop two-arg-u16.8>= (a b)
  (%u16.8-not
   (%two-arg-u16.8< a b)))

(define-fake-vop two-arg-u16.8<= (a b)
  (%u16.8-not
   (%two-arg-u16.8> a b)))

(define-fake-vop u16.8-movemask (a)
  (u16-odd-bits
   (%u8.16-movemask
    (%u8.16!-from-p128 a))))

(define-fake-vop make-u32.4 (a b c d)
  (%u32.4-unpacklo
   (%u32.4!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-u32s a c)))
   (%u32.4!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-u32s b d)))))

(define-fake-vop u32.4-values (x)
  (multiple-value-call #'values
    (sb-simd::%u32s-from-u64 (%u64!-from-p128 x))
    (sb-simd::%u32s-from-u64 (%u64!-from-p128 (%u64.2-permute (%u64.2!-from-p128 x) 1)))))

(define-fake-vop u32.4-broadcast (x)
  (let ((v (%u64.2!-from-u64 (sb-simd::%u64-from-u32s x x))))
    (%u32.4!-from-p128 (%u64.2-unpacklo v v))))

(define-fake-vop u32.4-not (a)
  (%u32.4-andc1
   a
   (%make-u32.4 +u32-true+ +u32-true+ +u32-true+ +u32-true+)))

(define-fake-vop two-arg-u32.4/= (a b)
  (%u32.4-not
   (%two-arg-u32.4= a b)))

(define-fake-vop two-arg-u32.4> (a b)
  (let* ((x (expt 2 31))
         (v (%make-u32.4 x x x x)))
    (%two-arg-u32.4>~ (%two-arg-u32.4- a v)
                      (%two-arg-u32.4- b v))))

(define-fake-vop two-arg-u32.4< (a b)
  (%two-arg-u32.4> b a))

(define-fake-vop two-arg-u32.4>= (a b)
  (%u32.4-not
   (%two-arg-u32.4< a b)))

(define-fake-vop two-arg-u32.4<= (a b)
  (%u32.4-not
   (%two-arg-u32.4> a b)))

(define-fake-vop make-u64.2 (a b)
  (%u64.2-unpacklo
   (%u64.2!-from-u64 a)
   (%u64.2!-from-u64 b)))

(define-fake-vop u64.2-values (x)
  (multiple-value-call #'values
    (%u64!-from-p128 x)
    (%u64!-from-p128 (%u64.2-permute (%u64.2!-from-p128 x) 1))))

(define-fake-vop u64.2-broadcast (x)
  (let ((v (%u64.2!-from-u64 x)))
    (%u64.2-unpacklo v v)))

(define-fake-vop u64.2-not (a)
  (%u64.2-andc1
   a
   (%make-u64.2 +u64-true+ +u64-true+)))

(define-fake-vop two-arg-u64.2/= (a b)
  (%u64.2-not
   (%two-arg-u64.2= a b)))

(define-fake-vop two-arg-u64.2> (a b)
  (let* ((x (expt 2 63))
         (v (%make-u64.2 x x)))
    (%two-arg-u64.2>~ (%two-arg-u64.2- a v)
                      (%two-arg-u64.2- b v))))

(define-fake-vop two-arg-u64.2< (a b)
  (%two-arg-u64.2> b a))

(define-fake-vop two-arg-u64.2>= (a b)
  (sb-simd-avx::%u64.2-not
   (%two-arg-u64.2< a b)))

(define-fake-vop two-arg-u64.2<= (a b)
  (sb-simd-avx::%u64.2-not
   (%two-arg-u64.2> a b)))

(define-fake-vop make-u8.32
    (u01 u02 u03 u04 u05 u06 u07 u08 u09 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32)
  (let ((lo (%make-u8.16 u01 u02 u03 u04 u05 u06 u07 u08 u09 u10 u11 u12 u13 u14 u15 u16))
        (hi (%make-u8.16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32)))
    (%u8.32-insert-u8.16 (%u8.32!-from-p128 lo) hi 1)))

(define-fake-vop u8.32-values (x)
  (multiple-value-call #'values
    (%u8.16-values (%u8.16!-from-p256 x))
    (%u8.16-values (%u8.16-from-u8.32 x 1))))

(define-fake-vop u8.32-broadcast (x)
  (let ((v (%u8.16-broadcast x)))
    (%u8.32-insert-u8.16 (%u8.32!-from-p128 v) v 1)))

(define-fake-vop make-u16.16 (a b c d e f g h i j k l m n o p)
  (let ((lo (%make-u16.8 a b c d e f g h))
        (hi (%make-u16.8 i j k l m n o p)))
    (%u16.16-insert-u16.8 (%u16.16!-from-p128 lo) hi 1)))

(define-fake-vop u16.16-values (x)
  (multiple-value-call #'values
    (%u16.8-values (%u16.8!-from-p256 x))
    (%u16.8-values (%u16.8-from-u16.16 x 1))))

(define-fake-vop u16.16-broadcast (x)
  (let ((v (%u16.8-broadcast x)))
    (%u16.16-insert-u16.8 (%u16.16!-from-p128 v) v 1)))

(define-fake-vop make-u32.8 (a b c d e f g h)
  (let ((lo (%make-u32.4 a b c d))
        (hi (%make-u32.4 e f g h)))
    (%u32.8-insert-u32.4 (%u32.8!-from-p128 lo) hi 1)))

(define-fake-vop u32.8-values (x)
  (multiple-value-call #'values
    (%u32.4-values (%u32.4!-from-p256 x))
    (%u32.4-values (%u32.4-from-u32.8 x 1))))

(define-fake-vop u32.8-broadcast (x)
  (let ((v (%u32.4-broadcast x)))
    (%u32.8-insert-u32.4 (%u32.8!-from-p128 v) v 1)))

(define-fake-vop make-u64.4 (a b c d)
  (let ((lo (%make-u64.2 a b))
        (hi (%make-u64.2 c d)))
    (%u64.4-insert-u64.2 (%u64.4!-from-p128 lo) hi 1)))

(define-fake-vop u64.4-values (x)
  (multiple-value-call #'values
    (%u64.2-values (%u64.2!-from-p256 x))
    (%u64.2-values (%u64.2-from-u64.4 x 1))))

(define-fake-vop u64.4-broadcast (x)
  (let ((v (%u64.2-broadcast x)))
    (%u64.4-insert-u64.2 (%u64.4!-from-p128 v) v 1)))

(define-fake-vop s8.16!-from-s8 (x)
  (%s8.16!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s8s x 0 0 0 0 0 0 0))))

(define-fake-vop make-s8.16 (a b c d e f g h i j k l m n o p)
  (%s8.16-unpacklo
   (%s8.16!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s8s a c e g i k m o)))
   (%s8.16!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s8s b d f h j l n p)))))

(define-fake-vop s8.16-values (x)
  (multiple-value-call #'values
    (sb-simd::%s8s-from-u64 (%u64!-from-p128 x))
    (sb-simd::%s8s-from-u64 (%u64!-from-p128 (%u64.2-permute (%u64.2!-from-p128 x) 1)))))

(define-fake-vop s8.16-broadcast (x)
  (let ((v (%u64.2!-from-u64 (sb-simd::%u64-from-s8s x x x x x x x x))))
    (%s8.16!-from-p128 (%u64.2-unpacklo v v))))

(define-fake-vop s8.16-not (a)
  (%s8.16-andc1
   a
   (%make-s8.16 +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+)))

(define-fake-vop two-arg-s8.16/= (a b)
  (%u8.16-not
   (%two-arg-s8.16= a b)))

(define-fake-vop two-arg-s8.16< (a b)
  (%two-arg-s8.16> b a))

(define-fake-vop two-arg-s8.16>= (a b)
  (%u8.16-not
   (%two-arg-s8.16< a b)))

(define-fake-vop two-arg-s8.16<= (a b)
  (%u8.16-not
   (%two-arg-s8.16> a b)))

(define-fake-vop s16.8!-from-s16 (x)
  (%s16.8!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s16s x 0 0 0))))

(define-fake-vop make-s16.8 (a b c d e f g h)
  (%s16.8-unpacklo
   (%s16.8!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s16s a c e g)))
   (%s16.8!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s16s b d f h)))))

(define-fake-vop s16.8-values (x)
  (multiple-value-call #'values
    (sb-simd::%s16s-from-u64 (%u64!-from-p128 x))
    (sb-simd::%s16s-from-u64 (%u64!-from-p128 (%u64.2-permute (%u64.2!-from-p128 x) 1)))))

(define-fake-vop s16.8-broadcast (x)
  (let ((v (%u64.2!-from-u64 (sb-simd::%u64-from-s16s x x x x))))
    (%s16.8!-from-p128 (%u64.2-unpacklo v v))))

(define-fake-vop s16.8-not (a)
  (%s16.8-andc1
   a
   (%make-s16.8 +s16-true+ +s16-true+ +s16-true+ +s16-true+
                +s16-true+ +s16-true+ +s16-true+ +s16-true+)))

(define-fake-vop two-arg-s16.8/= (a b)
  (%u16.8-not
   (%two-arg-s16.8= a b)))

(define-fake-vop two-arg-s16.8< (a b)
  (%two-arg-s16.8> b a))

(define-fake-vop two-arg-s16.8>= (a b)
  (%u16.8-not
   (%two-arg-s16.8< a b)))

(define-fake-vop two-arg-s16.8<= (a b)
  (%u16.8-not
   (%two-arg-s16.8> a b)))

(define-fake-vop s16.8-movemask (a)
  (u16-odd-bits
   (%u8.16-movemask
    (%u8.16!-from-p128 a))))

(define-fake-vop s32.4!-from-s32 (x)
  (%s32.4!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s32s x 0))))

(define-fake-vop make-s32.4 (a b c d)
  (%s32.4-unpacklo
   (%s32.4!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s32s a c)))
   (%s32.4!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s32s b d)))))

(define-fake-vop s32.4-values (x)
  (multiple-value-call #'values
    (sb-simd::%s32s-from-u64 (%u64!-from-p128 x))
    (sb-simd::%s32s-from-u64 (%u64!-from-p128 (%u64.2-permute (%u64.2!-from-p128 x) 1)))))

(define-fake-vop s32.4-broadcast (x)
  (let ((v (%u64.2!-from-u64 (sb-simd::%u64-from-s32s x x))))
    (%s32.4!-from-p128 (%u64.2-unpacklo v v))))

(define-fake-vop s32.4-not (a)
  (%s32.4-andc1
   a
   (%make-s32.4 +s32-true+ +s32-true+ +s32-true+ +s32-true+)))

(define-fake-vop two-arg-s32.4/= (a b)
  (%u32.4-not
   (%two-arg-s32.4= a b)))

(define-fake-vop two-arg-s32.4< (a b)
  (%two-arg-s32.4> b a))

(define-fake-vop two-arg-s32.4>= (a b)
  (%u32.4-not
   (%two-arg-s32.4< a b)))

(define-fake-vop two-arg-s32.4<= (a b)
  (%u32.4-not
   (%two-arg-s32.4> a b)))

(define-fake-vop s64.2!-from-s64 (x)
  (%s64.2!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s64 x))))

(define-fake-vop make-s64.2 (a b)
  (%s64.2-unpacklo
   (%s64.2!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s64 a)))
   (%s64.2!-from-p128 (%u64.2!-from-u64 (sb-simd::%u64-from-s64 b)))))

(define-fake-vop s64.2-values (x)
  (multiple-value-call #'values
    (sb-simd::%s64-from-u64 (%u64!-from-p128 x))
    (sb-simd::%s64-from-u64 (%u64!-from-p128 (%u64.2-permute (%u64.2!-from-p128 x) 1)))))

(define-fake-vop s64.2-broadcast (x)
  (let ((v (%u64.2!-from-u64 (sb-simd::%u64-from-s64 x))))
    (%s64.2!-from-p128 (%u64.2-unpacklo v v))))

(define-fake-vop s64.2-not (a)
  (%s64.2-andc1
   a
   (%make-s64.2 +s64-true+ +s64-true+)))

(define-fake-vop two-arg-s64.2/= (a b)
  (%u64.2-not
   (%two-arg-s64.2= a b)))

(define-fake-vop two-arg-s64.2< (a b)
  (%two-arg-s64.2> b a))

(define-fake-vop two-arg-s64.2>= (a b)
  (sb-simd-avx::%u64.2-not
   (%two-arg-s64.2< a b)))

(define-fake-vop two-arg-s64.2<= (a b)
  (sb-simd-avx::%u64.2-not
   (%two-arg-s64.2> a b)))

(define-fake-vop s8.32!-from-s8 (x)
  (%s8.32!-from-p256 (%u64.4!-from-u64 (sb-simd::%u64-from-s8s x 0 0 0 0 0 0 0))))

(define-fake-vop make-s8.32
    (s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31 s32)
  (let ((lo (%make-s8.16 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15 s16))
        (hi (%make-s8.16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31 s32)))
    (%s8.32-insert-s8.16 (%s8.32!-from-p128 lo) hi 1)))

(define-fake-vop s8.32-values (x)
  (multiple-value-call #'values
    (%s8.16-values (%s8.16!-from-p256 x))
    (%s8.16-values (%s8.16-from-s8.32 x 1))))

(define-fake-vop s8.32-broadcast (x)
  (let ((v (%s8.16-broadcast x)))
    (%s8.32-insert-s8.16 (%s8.32!-from-p128 v) v 1)))

(define-fake-vop s16.16!-from-s16 (x)
  (%s16.16!-from-p256 (%u64.4!-from-u64 (sb-simd::%u64-from-s16s x 0 0 0))))

(define-fake-vop make-s16.16 (a b c d e f g h i j k l m n o p)
  (let ((lo (%make-s16.8 a b c d e f g h))
        (hi (%make-s16.8 i j k l m n o p)))
    (%s16.16-insert-s16.8 (%s16.16!-from-p128 lo) hi 1)))

(define-fake-vop s16.16-values (x)
  (multiple-value-call #'values
    (%s16.8-values (%s16.8!-from-p256 x))
    (%s16.8-values (%s16.8-from-s16.16 x 1))))

(define-fake-vop s16.16-broadcast (x)
  (let ((v (%s16.8-broadcast x)))
    (%s16.16-insert-s16.8 (%s16.16!-from-p128 v) v 1)))

(define-fake-vop s32.8!-from-s32 (x)
  (%s32.8!-from-p256 (%u64.4!-from-u64 (sb-simd::%u64-from-s32s x 0))))

(define-fake-vop make-s32.8 (a b c d e f g h)
  (let ((lo (%make-s32.4 a b c d))
        (hi (%make-s32.4 e f g h)))
    (%s32.8-insert-s32.4 (%s32.8!-from-p128 lo) hi 1)))

(define-fake-vop s32.8-values (x)
  (multiple-value-call #'values
    (%s32.4-values (%s32.4!-from-p256 x))
    (%s32.4-values (%s32.4-from-s32.8 x 1))))

(define-fake-vop s32.8-broadcast (x)
  (let ((v (%s32.4-broadcast x)))
    (%s32.8-insert-s32.4 (%s32.8!-from-p128 v) v 1)))

(define-fake-vop s64.4!-from-s64 (x)
  (%s64.4!-from-p256 (%u64.4!-from-u64 (sb-simd::%u64-from-s64 x))))

(define-fake-vop make-s64.4 (a b c d)
  (let ((lo (%make-s64.2 a b))
        (hi (%make-s64.2 c d)))
    (%s64.4-insert-s64.2 (%s64.4!-from-p128 lo) hi 1)))

(define-fake-vop s64.4-values (x)
  (multiple-value-call #'values
    (%s64.2-values (%s64.2!-from-p256 x))
    (%s64.2-values (%s64.2-from-s64.4 x 1))))

(define-fake-vop s64.4-broadcast (x)
  (let ((v (%s64.2-broadcast x)))
    (%s64.4-insert-s64.2 (%s64.4!-from-p128 v) v 1)))

(define-fake-vop f64.4-reverse (x)
  (%f64.4-permute (%f64.4-permute128 x x #4r01) #4r11))

(in-package #:sb-simd-avx2)

(define-fake-vop u8.16-broadcast (x)
  (%u8.16-broadcastvec (sb-simd-avx::%u8.16!-from-u8 x)))

(define-fake-vop u16.8-broadcast (x)
  (%u16.8-broadcastvec (sb-simd-avx::%u16.8!-from-u16 x)))

(define-fake-vop u32.4-broadcast (x)
  (%u32.4-broadcastvec (sb-simd-avx::%u32.4!-from-u32 x)))

(define-fake-vop u64.2-broadcast (x)
  (%u64.2-broadcastvec (sb-simd-avx::%u64.2!-from-u64 x)))

(define-fake-vop s8.16-broadcast (x)
  (%s8.16-broadcastvec (sb-simd-avx::%s8.16!-from-s8 x)))

(define-fake-vop s16.8-broadcast (x)
  (%s16.8-broadcastvec (sb-simd-avx::%s16.8!-from-s16 x)))

(define-fake-vop s32.4-broadcast (x)
  (%s32.4-broadcastvec (sb-simd-avx::%s32.4!-from-s32 x)))

(define-fake-vop s64.2-broadcast (x)
  (%s64.2-broadcastvec (sb-simd-avx::%s64.2!-from-s64 x)))

(define-fake-vop make-u8.32
    (u01 u02 u03 u04 u05 u06 u07 u08 u09 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32)
  (let ((lo (sb-simd-avx::%make-u8.16 u01 u02 u03 u04 u05 u06 u07 u08 u09 u10 u11 u12 u13 u14 u15 u16))
        (hi (sb-simd-avx::%make-u8.16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32)))
    (%u8.32-insert-u8.16 (sb-simd-avx::%u8.32!-from-p128 lo) hi 1)))

(define-fake-vop u8.32-values (x)
  (multiple-value-call #'values
    (sb-simd-avx::%u8.16-values (sb-simd-avx::%u8.16!-from-p256 x))
    (sb-simd-avx::%u8.16-values (%u8.16-from-u8.32 x 1))))

(define-fake-vop u8.32-broadcast (x)
  (%u8.32-broadcastvec (sb-simd-avx::%u8.32!-from-u8 x)))

(define-fake-vop u8.32-not (a)
  (%u8.32-andc1
   a
   (%make-u8.32 +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+)))

(define-fake-vop two-arg-u8.32/= (a b)
  (%u8.32-not
   (%two-arg-u8.32= a b)))

(define-fake-vop two-arg-u8.32> (a b)
  (let* ((x (expt 2 7))
         (v (%make-u8.32 x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)))
    (%two-arg-u8.32>~ (%two-arg-u8.32- a v)
                      (%two-arg-u8.32- b v))))

(define-fake-vop two-arg-u8.32< (a b)
  (%two-arg-u8.32> b a))

(define-fake-vop two-arg-u8.32>= (a b)
  (%u8.32-not
   (%two-arg-u8.32< a b)))

(define-fake-vop two-arg-u8.32<= (a b)
  (%u8.32-not
   (%two-arg-u8.32> a b)))

(define-fake-vop make-u16.16 (a b c d e f g h i j k l m n o p)
  (let ((lo (sb-simd-avx::%make-u16.8 a b c d e f g h))
        (hi (sb-simd-avx::%make-u16.8 i j k l m n o p)))
    (%u16.16-insert-u16.8 (sb-simd-avx::%u16.16!-from-p128 lo) hi 1)))

(define-fake-vop u16.16-values (x)
  (multiple-value-call #'values
    (sb-simd-avx::%u16.8-values (sb-simd-avx::%u16.8!-from-p256 x))
    (sb-simd-avx::%u16.8-values (%u16.8-from-u16.16 x 1))))

(define-fake-vop u16.16-broadcast (x)
  (%u16.16-broadcastvec (sb-simd-avx::%u16.16!-from-u16 x)))

(define-fake-vop u16.16-not (a)
  (%u16.16-andc1
   a
   (%make-u16.16 +u16-true+ +u16-true+ +u16-true+ +u16-true+
                 +u16-true+ +u16-true+ +u16-true+ +u16-true+
                 +u16-true+ +u16-true+ +u16-true+ +u16-true+
                 +u16-true+ +u16-true+ +u16-true+ +u16-true+)))

(define-fake-vop two-arg-u16.16/= (a b)
  (%u16.16-not
   (%two-arg-u16.16= a b)))

(define-fake-vop two-arg-u16.16> (a b)
  (let* ((x (expt 2 15))
         (v (%make-u16.16 x x x x x x x x x x x x x x x x)))
    (%two-arg-u16.16>~ (%two-arg-u16.16- a v)
                       (%two-arg-u16.16- b v))))

(define-fake-vop two-arg-u16.16< (a b)
  (%two-arg-u16.16> b a))

(define-fake-vop two-arg-u16.16>= (a b)
  (%u16.16-not
   (%two-arg-u16.16< a b)))

(define-fake-vop two-arg-u16.16<= (a b)
  (%u16.16-not
   (%two-arg-u16.16> a b)))

(define-fake-vop u16.16-movemask (a)
  (u32-odd-bits
   (%u8.32-movemask
    (sb-simd-avx::%u8.32!-from-p256 a))))

(define-fake-vop make-u32.8 (a b c d e f g h)
  (let ((lo (sb-simd-avx::%make-u32.4 a b c d))
        (hi (sb-simd-avx::%make-u32.4 e f g h)))
    (%u32.8-insert-u32.4 (sb-simd-avx::%u32.8!-from-p128 lo) hi 1)))

(define-fake-vop u32.8-values (x)
  (multiple-value-call #'values
    (sb-simd-avx::%u32.4-values (sb-simd-avx::%u32.4!-from-p256 x))
    (sb-simd-avx::%u32.4-values (%u32.4-from-u32.8 x 1))))

(define-fake-vop u32.8-broadcast (x)
  (%u32.8-broadcastvec (sb-simd-avx::%u32.8!-from-u32 x)))

(define-fake-vop u32.8-not (a)
  (%u32.8-andc1
   a
   (%make-u32.8 +u32-true+ +u32-true+ +u32-true+ +u32-true+
                +u32-true+ +u32-true+ +u32-true+ +u32-true+)))

(define-fake-vop two-arg-u32.8/= (a b)
  (%u32.8-not
   (%two-arg-u32.8= a b)))

(define-fake-vop two-arg-u32.8> (a b)
  (let* ((x (expt 2 31))
         (v (%make-u32.8 x x x x x x x x)))
    (%two-arg-u32.8>~ (%two-arg-u32.8- a v)
                      (%two-arg-u32.8- b v))))

(define-fake-vop two-arg-u32.8< (a b)
  (%two-arg-u32.8> b a))

(define-fake-vop two-arg-u32.8>= (a b)
  (%u32.8-not
   (%two-arg-u32.8< a b)))

(define-fake-vop two-arg-u32.8<= (a b)
  (%u32.8-not
   (%two-arg-u32.8> a b)))

(define-fake-vop make-u64.4 (a b c d)
  (let ((lo (sb-simd-avx::%make-u64.2 a b))
        (hi (sb-simd-avx::%make-u64.2 c d)))
    (%u64.4-insert-u64.2 (sb-simd-avx::%u64.4!-from-p128 lo) hi 1)))

(define-fake-vop u64.4-values (x)
  (multiple-value-call #'values
    (sb-simd-avx::%u64.2-values (sb-simd-avx::%u64.2!-from-p256 x))
    (sb-simd-avx::%u64.2-values (%u64.2-from-u64.4 x 1))))

(define-fake-vop u64.4-broadcast (x)
  (%u64.4-broadcastvec (sb-simd-avx::%u64.4!-from-u64 x)))

(define-fake-vop u64.4-not (a)
  (%u64.4-andc1
   a
   (%make-u64.4 +u64-true+ +u64-true+ +u64-true+ +u64-true+)))

(define-fake-vop two-arg-u64.4/= (a b)
  (%u64.4-not
   (%two-arg-u64.4= a b)))

(define-fake-vop two-arg-u64.4> (a b)
  (let* ((x (expt 2 63))
         (v (%make-u64.4 x x x x)))
    (%two-arg-u64.4>~ (%two-arg-u64.4- a v)
                      (%two-arg-u64.4- b v))))

(define-fake-vop two-arg-u64.4< (a b)
  (%two-arg-u64.4> b a))

(define-fake-vop two-arg-u64.4>= (a b)
  (%u64.4-not
   (%two-arg-u64.4< a b)))

(define-fake-vop two-arg-u64.4<= (a b)
  (%u64.4-not
   (%two-arg-u64.4> a b)))

(define-fake-vop make-s8.32
    (s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31 s32)
  (let ((lo (sb-simd-avx::%make-s8.16 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15 s16))
        (hi (sb-simd-avx::%make-s8.16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31 s32)))
    (%s8.32-insert-s8.16 (sb-simd-avx::%s8.32!-from-p128 lo) hi 1)))

(define-fake-vop s8.32-values (x)
  (multiple-value-call #'values
    (sb-simd-avx::%s8.16-values (sb-simd-avx::%s8.16!-from-p256 x))
    (sb-simd-avx::%s8.16-values (%s8.16-from-s8.32 x 1))))

(define-fake-vop s8.32-broadcast (x)
  (%s8.32-broadcastvec (sb-simd-avx::%s8.32!-from-s8 x)))

(define-fake-vop s8.32-not (a)
  (%s8.32-andc1
   a
   (%make-s8.32 +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+)))

(define-fake-vop two-arg-s8.32/= (a b)
  (%u8.32-not
   (%two-arg-s8.32= a b)))

(define-fake-vop two-arg-s8.32< (a b)
  (%two-arg-s8.32> b a))

(define-fake-vop two-arg-s8.32>= (a b)
  (%u8.32-not
   (%two-arg-s8.32< a b)))

(define-fake-vop two-arg-s8.32<= (a b)
  (%u8.32-not
   (%two-arg-s8.32> a b)))

(define-fake-vop make-s16.16 (a b c d e f g h i j k l m n o p)
  (let ((lo (sb-simd-avx::%make-s16.8 a b c d e f g h))
        (hi (sb-simd-avx::%make-s16.8 i j k l m n o p)))
    (%s16.16-insert-s16.8 (sb-simd-avx::%s16.16!-from-p128 lo) hi 1)))

(define-fake-vop s16.16-values (x)
  (multiple-value-call #'values
    (sb-simd-avx::%s16.8-values (sb-simd-avx::%s16.8!-from-p256 x))
    (sb-simd-avx::%s16.8-values (sb-simd-avx::%s16.8-from-s16.16 x 1))))

(define-fake-vop s16.16-broadcast (x)
  (%s16.16-broadcastvec (sb-simd-avx::%s16.16!-from-s16 x)))

(define-fake-vop s16.16-not (a)
  (%s16.16-andc1
   a
   (%make-s16.16 +s16-true+ +s16-true+ +s16-true+ +s16-true+
                 +s16-true+ +s16-true+ +s16-true+ +s16-true+
                 +s16-true+ +s16-true+ +s16-true+ +s16-true+
                 +s16-true+ +s16-true+ +s16-true+ +s16-true+)))

(define-fake-vop two-arg-s16.16/= (a b)
  (%u16.16-not
   (%two-arg-s16.16= a b)))

(define-fake-vop two-arg-s16.16< (a b)
  (%two-arg-s16.16> b a))

(define-fake-vop two-arg-s16.16>= (a b)
  (%u16.16-not
   (%two-arg-s16.16< a b)))

(define-fake-vop two-arg-s16.16<= (a b)
  (%u16.16-not
   (%two-arg-s16.16> a b)))

(define-fake-vop s16.16-movemask (a)
  (u32-odd-bits
   (%u8.32-movemask
    (sb-simd-avx::%u8.32!-from-p256 a))))

(define-fake-vop make-s32.8 (a b c d e f g h)
  (let ((lo (sb-simd-avx::%make-s32.4 a b c d))
        (hi (sb-simd-avx::%make-s32.4 e f g h)))
    (%s32.8-insert-s32.4 (sb-simd-avx::%s32.8!-from-p128 lo) hi 1)))

(define-fake-vop s32.8-values (x)
  (multiple-value-call #'values
    (sb-simd-avx::%s32.4-values (sb-simd-avx::%s32.4!-from-p256 x))
    (sb-simd-avx::%s32.4-values (sb-simd-avx::%s32.4-from-s32.8 x 1))))

(define-fake-vop s32.8-broadcast (x)
  (%s32.8-broadcastvec (sb-simd-avx::%s32.8!-from-s32 x)))

(define-fake-vop s32.8-not (a)
  (%s32.8-andc1
   a
   (%make-s32.8 +s32-true+ +s32-true+ +s32-true+ +s32-true+
                +s32-true+ +s32-true+ +s32-true+ +s32-true+)))

(define-fake-vop two-arg-s32.8/= (a b)
  (%u32.8-not
   (%two-arg-s32.8= a b)))

(define-fake-vop two-arg-s32.8< (a b)
  (%two-arg-s32.8> b a))

(define-fake-vop two-arg-s32.8>= (a b)
  (%u32.8-not
   (%two-arg-s32.8< a b)))

(define-fake-vop two-arg-s32.8<= (a b)
  (%u32.8-not
   (%two-arg-s32.8> a b)))

(define-fake-vop make-s64.4 (a b c d)
  (let ((lo (sb-simd-avx::%make-s64.2 a b))
        (hi (sb-simd-avx::%make-s64.2 c d)))
    (%s64.4-insert-s64.2 (sb-simd-avx::%s64.4!-from-p128 lo) hi 1)))

(define-fake-vop s64.4-values (x)
  (multiple-value-call #'values
    (sb-simd-avx::%s64.2-values (sb-simd-avx::%s64.2!-from-p256 x))
    (sb-simd-avx::%s64.2-values (%s64.2-from-s64.4 x 1))))

(define-fake-vop s64.4-broadcast (x)
  (%s64.4-broadcastvec (sb-simd-avx::%s64.4!-from-s64 x)))

(define-fake-vop s64.4-not (a)
  (%s64.4-andc1
   a
   (%make-s64.4 +s64-true+ +s64-true+ +s64-true+ +s64-true+)))

(define-fake-vop two-arg-s64.4/= (a b)
  (%u64.4-not
   (%two-arg-s64.4= a b)))

(define-fake-vop two-arg-s64.4< (a b)
  (%two-arg-s64.4> b a))

(define-fake-vop two-arg-s64.4>= (a b)
  (%u64.4-not
   (%two-arg-s64.4< a b)))

(define-fake-vop two-arg-s64.4<= (a b)
  (%u64.4-not
   (%two-arg-s64.4> a b)))

(define-fake-vop f64.4-reverse (a)
  (%f64.4-permute4x64 a #b00011011))
