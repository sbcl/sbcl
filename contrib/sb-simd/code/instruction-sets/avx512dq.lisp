(in-package #:sb-simd-avx512dq)

(define-instruction-set :avx512dq
  (:test (avx512dq-supported-p))
  (:include :avx512bw)
  (:instructions
   (two-arg-u64.8*        #:vpmullq      (u64.8)  (u64.8 u64.8)   :cost 4 :associative t)
   (two-arg-s64.8*        #:vpmullq      (s64.8)  (s64.8 s64.8)   :cost 4 :associative t))
  (:associatives
   (u64.8* two-arg-u64.8* 1)
   (s64.8* two-arg-s64.8* 1)))
