
(in-package #:sb-simd-neon)

(sb-simd-test-suite:define-aref-test f32.4-aref single-float 4 f32.4-values)
(sb-simd-test-suite:define-aref-test f64.2-aref double-float       2 f64.2-values)
(sb-simd-test-suite:define-aref-test u8.16-aref (unsigned-byte 8) 16 u8.16-values)
(sb-simd-test-suite:define-aref-test u16.8-aref (unsigned-byte 16) 8 u16.8-values)
(sb-simd-test-suite:define-aref-test u32.4-aref (unsigned-byte 32) 4 u32.4-values)
(sb-simd-test-suite:define-aref-test u64.2-aref (unsigned-byte 64) 2 u64.2-values)
(sb-simd-test-suite:define-aref-test s8.16-aref (signed-byte 8)   16 s8.16-values)
(sb-simd-test-suite:define-aref-test s16.8-aref (signed-byte 16)   8 s16.8-values)
(sb-simd-test-suite:define-aref-test s32.4-aref (signed-byte 32)   4 s32.4-values)
(sb-simd-test-suite:define-aref-test s64.2-aref (signed-byte 64)   2 s64.2-values)
