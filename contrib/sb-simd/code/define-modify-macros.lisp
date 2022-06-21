(in-package #:sb-simd)

(define-modify-macro f32-incf (&optional (num 1f0)) two-arg-f32+)
(define-modify-macro f32-decf (&optional (num 1f0)) two-arg-f32-)

(define-modify-macro f64-incf (&optional (num 1d0)) two-arg-f64+)
(define-modify-macro f64-decf (&optional (num 1d0)) two-arg-f64-)

(define-modify-macro u8-incf (&optional (num 1)) two-arg-u8+)
(define-modify-macro u8-decf (&optional (num 1)) two-arg-u8-)

(define-modify-macro u16-incf (&optional (num 1)) two-arg-u16+)
(define-modify-macro u16-decf (&optional (num 1)) two-arg-u16-)

(define-modify-macro u32-incf (&optional (num 1)) two-arg-u32+)
(define-modify-macro u32-decf (&optional (num 1)) two-arg-u32-)

(define-modify-macro u64-incf (&optional (num 1)) two-arg-u64+)
(define-modify-macro u64-decf (&optional (num 1)) two-arg-u64-)

(define-modify-macro s8-incf (&optional (num 1)) two-arg-s8+)
(define-modify-macro s8-decf (&optional (num 1)) two-arg-s8-)

(define-modify-macro s16-incf (&optional (num 1)) two-arg-s16+)
(define-modify-macro s16-decf (&optional (num 1)) two-arg-s16-)

(define-modify-macro s32-incf (&optional (num 1)) two-arg-s32+)
(define-modify-macro s32-decf (&optional (num 1)) two-arg-s32-)

(define-modify-macro s64-incf (&optional (num 1)) two-arg-s64+)
(define-modify-macro s64-decf (&optional (num 1)) two-arg-s64-)

(in-package #:sb-simd-sse)

(define-modify-macro f32-incf (&optional (num 1f0)) two-arg-f32+)
(define-modify-macro f32-decf (&optional (num 1f0)) two-arg-f32-)

(define-modify-macro f32.4-incf (&optional (num 1f0)) two-arg-f32.4+)
(define-modify-macro f32.4-decf (&optional (num 1f0)) two-arg-f32.4-)

(in-package #:sb-simd-sse2)

(define-modify-macro f64-incf (&optional (num 1d0)) two-arg-f64+)
(define-modify-macro f64-decf (&optional (num 1d0)) two-arg-f64-)

(define-modify-macro f64.2-incf (&optional (num 1d0)) two-arg-f64.2+)
(define-modify-macro f64.2-decf (&optional (num 1d0)) two-arg-f64.2-)

(define-modify-macro u8.16-incf (&optional (num 1)) two-arg-u8.16+)
(define-modify-macro u8.16-decf (&optional (num 1)) two-arg-u8.16-)

(define-modify-macro u16.8-incf (&optional (num 1)) two-arg-u16.8+)
(define-modify-macro u16.8-decf (&optional (num 1)) two-arg-u16.8-)

(define-modify-macro u32.4-incf (&optional (num 1)) two-arg-u32.4+)
(define-modify-macro u32.4-decf (&optional (num 1)) two-arg-u32.4-)

(define-modify-macro u64.2-incf (&optional (num 1)) two-arg-u64.2+)
(define-modify-macro u64.2-decf (&optional (num 1)) two-arg-u64.2-)

(define-modify-macro s8.16-incf (&optional (num 1)) two-arg-s8.16+)
(define-modify-macro s8.16-decf (&optional (num 1)) two-arg-s8.16-)

(define-modify-macro s16.8-incf (&optional (num 1)) two-arg-s16.8+)
(define-modify-macro s16.8-decf (&optional (num 1)) two-arg-s16.8-)

(define-modify-macro s32.4-incf (&optional (num 1)) two-arg-s32.4+)
(define-modify-macro s32.4-decf (&optional (num 1)) two-arg-s32.4-)

(define-modify-macro s64.2-incf (&optional (num 1)) two-arg-s64.2+)
(define-modify-macro s64.2-decf (&optional (num 1)) two-arg-s64.2-)

(in-package #:sb-simd-avx)

(define-modify-macro f32-incf (&optional (num 1f0)) two-arg-f32+)
(define-modify-macro f32-decf (&optional (num 1f0)) two-arg-f32-)

(define-modify-macro f64-incf (&optional (num 1d0)) two-arg-f64+)
(define-modify-macro f64-decf (&optional (num 1d0)) two-arg-f64-)

(define-modify-macro f32.4-incf (&optional (num 1f0)) two-arg-f32.4+)
(define-modify-macro f32.4-decf (&optional (num 1f0)) two-arg-f32.4-)

(define-modify-macro f32.8-incf (&optional (num 1f0)) two-arg-f32.8+)
(define-modify-macro f32.8-decf (&optional (num 1f0)) two-arg-f32.8-)

(define-modify-macro f64.2-incf (&optional (num 1d0)) two-arg-f64.2+)
(define-modify-macro f64.2-decf (&optional (num 1d0)) two-arg-f64.2-)

(define-modify-macro f64.4-incf (&optional (num 1d0)) two-arg-f64.4+)
(define-modify-macro f64.4-decf (&optional (num 1d0)) two-arg-f64.4-)

(define-modify-macro u8.16-incf (&optional (num 1)) two-arg-u8.16+)
(define-modify-macro u8.16-decf (&optional (num 1)) two-arg-u8.16-)

(define-modify-macro u16.8-incf (&optional (num 1)) two-arg-u16.8+)
(define-modify-macro u16.8-decf (&optional (num 1)) two-arg-u16.8-)

(define-modify-macro u32.4-incf (&optional (num 1)) two-arg-u32.4+)
(define-modify-macro u32.4-decf (&optional (num 1)) two-arg-u32.4-)

(define-modify-macro u64.2-incf (&optional (num 1)) two-arg-u64.2+)
(define-modify-macro u64.2-decf (&optional (num 1)) two-arg-u64.2-)

(define-modify-macro s8.16-incf (&optional (num 1)) two-arg-s8.16+)
(define-modify-macro s8.16-decf (&optional (num 1)) two-arg-s8.16-)

(define-modify-macro s16.8-incf (&optional (num 1)) two-arg-s16.8+)
(define-modify-macro s16.8-decf (&optional (num 1)) two-arg-s16.8-)

(define-modify-macro s32.4-incf (&optional (num 1)) two-arg-s32.4+)
(define-modify-macro s32.4-decf (&optional (num 1)) two-arg-s32.4-)

(define-modify-macro s64.2-incf (&optional (num 1)) two-arg-s64.2+)
(define-modify-macro s64.2-decf (&optional (num 1)) two-arg-s64.2-)

(in-package #:sb-simd-avx2)

(define-modify-macro u8.32-incf (&optional (num 1)) two-arg-u8.32+)
(define-modify-macro u8.32-decf (&optional (num 1)) two-arg-u8.32-)

(define-modify-macro u16.16-incf (&optional (num 1)) two-arg-u16.16+)
(define-modify-macro u16.16-decf (&optional (num 1)) two-arg-u16.16-)

(define-modify-macro u32.8-incf (&optional (num 1)) two-arg-u32.8+)
(define-modify-macro u32.8-decf (&optional (num 1)) two-arg-u32.8-)

(define-modify-macro u64.4-incf (&optional (num 1)) two-arg-u64.4+)
(define-modify-macro u64.4-decf (&optional (num 1)) two-arg-u64.4-)

(define-modify-macro s8.32-incf (&optional (num 1)) two-arg-s8.32+)
(define-modify-macro s8.32-decf (&optional (num 1)) two-arg-s8.32-)

(define-modify-macro s16.16-incf (&optional (num 1)) two-arg-s16.16+)
(define-modify-macro s16.16-decf (&optional (num 1)) two-arg-s16.16-)

(define-modify-macro s32.8-incf (&optional (num 1)) two-arg-s32.8+)
(define-modify-macro s32.8-decf (&optional (num 1)) two-arg-s32.8-)

(define-modify-macro s64.4-incf (&optional (num 1)) two-arg-s64.4+)
(define-modify-macro s64.4-decf (&optional (num 1)) two-arg-s64.4-)

