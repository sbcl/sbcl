(in-package #:sb-simd-sse3)

(define-instruction-set :sse3
  (:include :sse2)
  (:test (sse3-supported-p))
  (:instructions
   (f32.4-hadd  #:haddps   (f32.4) (f32.4 f32.4) :cost 1 :encoding :sse)
   (f32.4-hdup  #:movshdup (f32.4) (f32.4) :cost 1)
   (f32.4-ldup  #:movsldup (f32.4) (f32.4) :cost 1)
   (f64.2-hadd  #:haddpd   (f64.2) (f64.2 f64.2) :cost 1 :encoding :sse)))

