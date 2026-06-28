(in-package #:sb-simd-test-suite)

(defmacro define-horizontal-test (horizontal-fn scalar-fn)
  (let* ((horizontal-record (find-function-record horizontal-fn))
         (scalar-record (find-function-record scalar-fn))
         (argument-record (first (function-record-required-argument-records horizontal-record)))
         (result-record (function-record-result-record horizontal-record))
         (width (value-record-simd-width argument-record))
         (args (loop repeat width collect (gensym)))
         (pack (third (simd-info (value-record-name argument-record)))))
    (assert (eq (function-record-result-record scalar-record) result-record))
    `(define-test ,horizontal-fn
       (let ((generator (find-generator ',(value-record-name result-record))))
         (loop repeat 99 do
           (let ,(loop for arg in args collect `(,arg (funcall generator)))
             (handler-case
                 (assert (bitwise= (,horizontal-fn (,pack ,@args))
                                   (,scalar-fn ,@args)))
               (floating-point-overflow ())
               (floating-point-overflow ()))))))))

(in-package #:sb-simd-sse)

(sb-simd-test-suite:define-horizontal-test f32.4-horizontal-and f32-and)
(sb-simd-test-suite:define-horizontal-test f32.4-horizontal-or  f32-or)
(sb-simd-test-suite:define-horizontal-test f32.4-horizontal-xor f32-xor)
(sb-simd-test-suite:define-horizontal-test f32.4-horizontal-max f32-max)
(sb-simd-test-suite:define-horizontal-test f32.4-horizontal-min f32-min)
(sb-simd-test-suite:define-horizontal-test f32.4-horizontal+ f32+)
(sb-simd-test-suite:define-horizontal-test f32.4-horizontal* f32*)

(in-package #:sb-simd-sse2)

(sb-simd-test-suite:define-horizontal-test f64.2-horizontal-and f64-and)
(sb-simd-test-suite:define-horizontal-test f64.2-horizontal-or  f64-or)
(sb-simd-test-suite:define-horizontal-test f64.2-horizontal-xor f64-xor)
(sb-simd-test-suite:define-horizontal-test f64.2-horizontal-max f64-max)
(sb-simd-test-suite:define-horizontal-test f64.2-horizontal-min f64-min)
(sb-simd-test-suite:define-horizontal-test f64.2-horizontal+ f64+)
(sb-simd-test-suite:define-horizontal-test f64.2-horizontal* f64*)

(in-package #:sb-simd-avx)

(sb-simd-test-suite:define-horizontal-test f32.4-horizontal-and f32-and)
(sb-simd-test-suite:define-horizontal-test f32.4-horizontal-or  f32-or)
(sb-simd-test-suite:define-horizontal-test f32.4-horizontal-xor f32-xor)
(sb-simd-test-suite:define-horizontal-test f32.4-horizontal-max f32-max)
(sb-simd-test-suite:define-horizontal-test f32.4-horizontal-min f32-min)
(sb-simd-test-suite:define-horizontal-test f32.4-horizontal+ f32+)
(sb-simd-test-suite:define-horizontal-test f32.4-horizontal* f32*)

(sb-simd-test-suite:define-horizontal-test f64.2-horizontal-and f64-and)
(sb-simd-test-suite:define-horizontal-test f64.2-horizontal-or  f64-or)
(sb-simd-test-suite:define-horizontal-test f64.2-horizontal-xor f64-xor)
(sb-simd-test-suite:define-horizontal-test f64.2-horizontal-max f64-max)
(sb-simd-test-suite:define-horizontal-test f64.2-horizontal-min f64-min)
(sb-simd-test-suite:define-horizontal-test f64.2-horizontal+ f64+)
(sb-simd-test-suite:define-horizontal-test f64.2-horizontal* f64*)

(sb-simd-test-suite:define-horizontal-test f32.8-horizontal-and f32-and)
(sb-simd-test-suite:define-horizontal-test f32.8-horizontal-or  f32-or)
(sb-simd-test-suite:define-horizontal-test f32.8-horizontal-xor f32-xor)
(sb-simd-test-suite:define-horizontal-test f32.8-horizontal-max f32-max)
(sb-simd-test-suite:define-horizontal-test f32.8-horizontal-min f32-min)
(sb-simd-test-suite:define-horizontal-test f32.8-horizontal+ f32+)
(sb-simd-test-suite:define-horizontal-test f32.8-horizontal* f32*)

(sb-simd-test-suite:define-horizontal-test f64.4-horizontal-and f64-and)
(sb-simd-test-suite:define-horizontal-test f64.4-horizontal-or  f64-or)
(sb-simd-test-suite:define-horizontal-test f64.4-horizontal-xor f64-xor)
(sb-simd-test-suite:define-horizontal-test f64.4-horizontal-max f64-max)
(sb-simd-test-suite:define-horizontal-test f64.4-horizontal-min f64-min)
(sb-simd-test-suite:define-horizontal-test f64.4-horizontal+ f64+)
(sb-simd-test-suite:define-horizontal-test f64.4-horizontal* f64*)

(in-package #:sb-simd-avx2)

(in-package #:sb-simd-neon)

(sb-simd-test-suite:define-horizontal-test f32.4-horizontal-max f32-max)
(sb-simd-test-suite:define-horizontal-test f32.4-horizontal-min f32-min)
(sb-simd-test-suite:define-horizontal-test f32.4-horizontal+ f32+)

(sb-simd-test-suite:define-horizontal-test f64.2-horizontal-max f64-max)
(sb-simd-test-suite:define-horizontal-test f64.2-horizontal-min f64-min)
(sb-simd-test-suite:define-horizontal-test f64.2-horizontal+ f64+)

(sb-simd-test-suite:define-horizontal-test u8.16-horizontal-max u8-max)
(sb-simd-test-suite:define-horizontal-test u8.16-horizontal-min u8-min)
(sb-simd-test-suite:define-horizontal-test u8.16-horizontal+ u8+)

(sb-simd-test-suite:define-horizontal-test u16.8-horizontal-max u16-max)
(sb-simd-test-suite:define-horizontal-test u16.8-horizontal-min u16-min)
(sb-simd-test-suite:define-horizontal-test u16.8-horizontal+ u16+)

(sb-simd-test-suite:define-horizontal-test u32.4-horizontal-max u32-max)
(sb-simd-test-suite:define-horizontal-test u32.4-horizontal-min u32-min)
(sb-simd-test-suite:define-horizontal-test u32.4-horizontal+ u32+)

(sb-simd-test-suite:define-horizontal-test u64.2-horizontal-max u64-max)
(sb-simd-test-suite:define-horizontal-test u64.2-horizontal-min u64-min)
(sb-simd-test-suite:define-horizontal-test u64.2-horizontal+ u64+)

(sb-simd-test-suite:define-horizontal-test s8.16-horizontal-max s8-max)
(sb-simd-test-suite:define-horizontal-test s8.16-horizontal-min s8-min)
(sb-simd-test-suite:define-horizontal-test s8.16-horizontal+ s8+)

(sb-simd-test-suite:define-horizontal-test s16.8-horizontal-max s16-max)
(sb-simd-test-suite:define-horizontal-test s16.8-horizontal-min s16-min)
(sb-simd-test-suite:define-horizontal-test s16.8-horizontal+ s16+)

(sb-simd-test-suite:define-horizontal-test s32.4-horizontal-max s32-max)
(sb-simd-test-suite:define-horizontal-test s32.4-horizontal-min s32-min)
(sb-simd-test-suite:define-horizontal-test s32.4-horizontal+ s32+)

(sb-simd-test-suite:define-horizontal-test s64.2-horizontal-max s64-max)
(sb-simd-test-suite:define-horizontal-test s64.2-horizontal-min s64-min)
(sb-simd-test-suite:define-horizontal-test s64.2-horizontal+ s64+)
