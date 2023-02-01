(in-package #:sb-simd-test-suite)

(defmacro define-aref-test (aref element-type simd-width &optional (unpacker 'identity))
  (let ((value-symbols (prefixed-symbols "V" simd-width))
        (zero (coerce 0 element-type))
        (one (coerce 1 element-type)))
    `(define-test ,aref
       ;; Create an array of zeros and successively replace zeros with
       ;; ones.  After each replacement, check whether a load still
       ;; produces the expected result.
       (let ((array (make-array '(,simd-width)
                                 :element-type ',element-type
                                 :initial-element ,zero)))
         (multiple-value-bind ,value-symbols (,unpacker (,aref array 0))
           ,@(loop for value-symbol in value-symbols
                   collect `(is (= ,value-symbol ,zero))))
         (loop for index below ,simd-width do
           (setf (aref array index) ,one)
           (loop for number in (multiple-value-list (,unpacker (,aref array 0)))
                 for position from 0
                 do (if (<= position index)
                        (is (= number ,one))
                        (is (= number ,zero))))))
       ;; Create an array with twice as many elements as the width of the
       ;; SIMD data type, and whose lower half consists of all zeros and
       ;; whose upper half consists of all ones.  Check that all valid
       ;; loads from this array have the expected state.
       (let ((array (make-array '(,(* 2 simd-width))
                                :element-type ',element-type
                                :initial-contents
                                (append (make-list ,simd-width :initial-element ,zero)
                                        (make-list ,simd-width :initial-element ,one)))))
         (loop for index below ,simd-width do
           (multiple-value-bind ,value-symbols
               (,unpacker (,aref array index))
             ,@(loop for value-symbol in value-symbols
                     for position from 0
                     collect `(if (< (+ ,position index) ,simd-width)
                                  (is (= ,value-symbol ,zero))
                                  (is (= ,value-symbol ,one))))))))))

(in-package #:sb-simd)

(sb-simd-test-suite:define-aref-test u8-aref  (unsigned-byte 8) 1)
(sb-simd-test-suite:define-aref-test u16-aref (unsigned-byte 16) 1)
(sb-simd-test-suite:define-aref-test u32-aref (unsigned-byte 32) 1)
(sb-simd-test-suite:define-aref-test u64-aref (unsigned-byte 64) 1)
(sb-simd-test-suite:define-aref-test s8-aref  (signed-byte 8) 1)
(sb-simd-test-suite:define-aref-test s16-aref (signed-byte 16) 1)
(sb-simd-test-suite:define-aref-test s32-aref (signed-byte 32) 1)
(sb-simd-test-suite:define-aref-test s64-aref (signed-byte 64) 1)
(sb-simd-test-suite:define-aref-test f32-aref single-float 1)
(sb-simd-test-suite:define-aref-test f64-aref double-float 1)

(in-package #:sb-simd-sse)

(sb-simd-test-suite:define-aref-test f32.4-aref single-float 4 f32.4-values)

(in-package #:sb-simd-sse2)

(sb-simd-test-suite:define-aref-test f64.2-aref double-float       2 f64.2-values)
(sb-simd-test-suite:define-aref-test u8.16-aref (unsigned-byte 8) 16 u8.16-values)
(sb-simd-test-suite:define-aref-test u16.8-aref (unsigned-byte 16) 8 u16.8-values)
(sb-simd-test-suite:define-aref-test u32.4-aref (unsigned-byte 32) 4 u32.4-values)
(sb-simd-test-suite:define-aref-test u64.2-aref (unsigned-byte 64) 2 u64.2-values)
(sb-simd-test-suite:define-aref-test s8.16-aref (signed-byte 8)   16 s8.16-values)
(sb-simd-test-suite:define-aref-test s16.8-aref (signed-byte 16)   8 s16.8-values)
(sb-simd-test-suite:define-aref-test s32.4-aref (signed-byte 32)   4 s32.4-values)
(sb-simd-test-suite:define-aref-test s64.2-aref (signed-byte 64)   2 s64.2-values)

(in-package #:sb-simd-avx)

(sb-simd-test-suite:define-aref-test f32.4-aref single-float 4 f32.4-values)
(sb-simd-test-suite:define-aref-test f32.8-aref single-float 8 f32.8-values)
(sb-simd-test-suite:define-aref-test f64.2-aref double-float 2 f64.2-values)
(sb-simd-test-suite:define-aref-test f64.4-aref double-float 4 f64.4-values)
(sb-simd-test-suite:define-aref-test u8.16-aref (unsigned-byte 8) 16 u8.16-values)
(sb-simd-test-suite:define-aref-test u16.8-aref (unsigned-byte 16) 8 u16.8-values)
(sb-simd-test-suite:define-aref-test u32.4-aref (unsigned-byte 32) 4 u32.4-values)
(sb-simd-test-suite:define-aref-test u64.2-aref (unsigned-byte 64) 2 u64.2-values)
(sb-simd-test-suite:define-aref-test s8.16-aref (signed-byte 8) 16 s8.16-values)
(sb-simd-test-suite:define-aref-test s16.8-aref (signed-byte 16) 8 s16.8-values)
(sb-simd-test-suite:define-aref-test s32.4-aref (signed-byte 32) 4 s32.4-values)
(sb-simd-test-suite:define-aref-test s64.2-aref (signed-byte 64) 2 s64.2-values)
(sb-simd-test-suite:define-aref-test u8.32-aref (unsigned-byte 8) 32 u8.32-values)
(sb-simd-test-suite:define-aref-test u16.16-aref (unsigned-byte 16) 16 u16.16-values)
(sb-simd-test-suite:define-aref-test u32.8-aref (unsigned-byte 32) 8 u32.8-values)
(sb-simd-test-suite:define-aref-test u64.4-aref (unsigned-byte 64) 4 u64.4-values)
(sb-simd-test-suite:define-aref-test s8.32-aref (signed-byte 8) 32 s8.32-values)
(sb-simd-test-suite:define-aref-test s16.16-aref (signed-byte 16) 16 s16.16-values)
(sb-simd-test-suite:define-aref-test s32.8-aref (signed-byte 32) 8 s32.8-values)
(sb-simd-test-suite:define-aref-test s64.4-aref (signed-byte 64) 4 s64.4-values)
