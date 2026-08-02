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
