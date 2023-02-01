(in-package #:sb-simd-fma)

(define-instruction-set :fma
  (:test (fma-supported-p))
  (:include :avx2)
  (:instructions
   ;; f32
   (f32-fmadd       #:vfmadd213ss      (f32) (f32 f32 f32) :cost 1 :encoding :fma)
   (f32-fnmadd      #:vfnmadd213ss     (f32) (f32 f32 f32) :cost 1 :encoding :fma)
   (f32-fmsub       #:vfmsub213ss      (f32) (f32 f32 f32) :cost 1 :encoding :fma)
   ;; f64
   (f64-fmadd       #:vfmadd213sd     (f64) (f64 f64 f64) :cost 1 :encoding :fma)
   (f64-fnmadd      #:vfnmadd213sd    (f64) (f64 f64 f64) :cost 1 :encoding :fma)
   (f64-fmsub       #:vfmsub213sd     (f64) (f64 f64 f64) :cost 1 :encoding :fma)
   ;; f32.4
   (f32.4-fmadd     #:vfmadd213ps     (f32.4) (f32.4 f32.4 f32.4) :cost 1 :encoding :fma)
   (f32.4-fnmadd    #:vfnmadd213ps    (f32.4) (f32.4 f32.4 f32.4) :cost 1 :encoding :fma)
   (f32.4-fmsub     #:vfmsub213ps     (f32.4) (f32.4 f32.4 f32.4) :cost 1 :encoding :fma)
   (f32.4-fmaddsub  #:vfmaddsub213ps  (f32.4) (f32.4 f32.4 f32.4) :cost 1 :encoding :fma)
   (f32.4-fmsubadd  #:vfmsubadd213ps  (f32.4) (f32.4 f32.4 f32.4) :cost 1 :encoding :fma)
   ;; f32.8
   (f32.8-fmadd     #:vfmadd213ps     (f32.8) (f32.8 f32.8 f32.8) :cost 1 :encoding :fma)
   (f32.8-fnmadd    #:vfnmadd213ps    (f32.8) (f32.8 f32.8 f32.8) :cost 1 :encoding :fma)
   (f32.8-fmsub     #:vfmsub213ps     (f32.8) (f32.8 f32.8 f32.8) :cost 1 :encoding :fma)
   (f32.8-fmaddsub  #:vfmaddsub213ps  (f32.8) (f32.8 f32.8 f32.8) :cost 1 :encoding :fma)
   (f32.8-fmsubadd  #:vfmsubadd213ps  (f32.8) (f32.8 f32.8 f32.8) :cost 1 :encoding :fma)
   ;; f64.2
   (f64.2-fmadd     #:vfmadd213pd     (f64.2) (f64.2 f64.2 f64.2) :cost 1 :encoding :fma)
   (f64.2-fnmadd    #:vfnmadd213pd    (f64.2) (f64.2 f64.2 f64.2) :cost 1 :encoding :fma)
   (f64.2-fmsub     #:vfmsub213pd     (f64.2) (f64.2 f64.2 f64.2) :cost 1 :encoding :fma)
   (f64.2-fmaddsub  #:vfmaddsub213pd  (f64.2) (f64.2 f64.2 f64.2) :cost 1 :encoding :fma)
   (f64.2-fmsubadd  #:vfmsubadd213pd  (f64.2) (f64.2 f64.2 f64.2) :cost 1 :encoding :fma)
   ;; f64.4
   (f64.4-fmadd     #:vfmadd213pd     (f64.4) (f64.4 f64.4 f64.4) :cost 1 :encoding :fma)
   (f64.4-fnmadd    #:vfnmadd213pd    (f64.4) (f64.4 f64.4 f64.4) :cost 1 :encoding :fma)
   (f64.4-fmsub     #:vfmsub213pd     (f64.4) (f64.4 f64.4 f64.4) :cost 1 :encoding :fma)
   (f64.4-fmaddsub  #:vfmaddsub213pd  (f64.4) (f64.4 f64.4 f64.4) :cost 1 :encoding :fma)
   (f64.4-fmsubadd  #:vfmsubadd213pd  (f64.4) (f64.4 f64.4 f64.4) :cost 1 :encoding :fma)))
