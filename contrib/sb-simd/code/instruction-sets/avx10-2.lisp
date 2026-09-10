(in-package #:sb-simd-avx10.2)

(define-instruction-set :avx10.2
  (:test (avx10.2-supported-p))
  (:include :avx10.1)
  (:instructions
   ;; VMINMAXPS (imm8)
   (f32-minmax       #:vminmaxss    (f32)    (f32 f32 imm8)       :cost 3)
   (f32.4-minmax     #:vminmaxps    (f32.4)  (f32.4 f32.4 imm8)   :cost 3)
   (f32.8-minmax     #:vminmaxps    (f32.8)  (f32.8 f32.8 imm8)   :cost 3)
   (f32.16-minmax    #:vminmaxps    (f32.16) (f32.16 f32.16 imm8) :cost 3)

   ;; VMINMAXPD (imm8)
   (f64-minmax       #:vminmaxsd    (f64)    (f64 f64 imm8)       :cost 3)
   (f64.2-minmax     #:vminmaxpd    (f64.2)  (f64.2 f64.2 imm8)   :cost 3)
   (f64.4-minmax     #:vminmaxpd    (f64.4)  (f64.4 f64.4 imm8)   :cost 3)
   (f64.8-minmax     #:vminmaxpd    (f64.8)  (f64.8 f64.8 imm8)   :cost 3)

   ;; VMINMAXPH (imm8)
   (f16.8-minmax     #:vminmaxph    (f16.8)  (f16.8 f16.8 imm8)   :cost 3)
   (f16.16-minmax    #:vminmaxph    (f16.16) (f16.16 f16.16 imm8) :cost 3)
   (f16.32-minmax    #:vminmaxph    (f16.32) (f16.32 f16.32 imm8) :cost 3)))
