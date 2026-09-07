(in-package #:sb-simd-avx10.1)

(define-instruction-set :avx10.1
  (:test (avx10.1-supported-p))
  (:include :avx512fp16))
