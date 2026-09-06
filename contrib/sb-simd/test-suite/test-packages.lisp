(in-package #:sb-simd-test-suite)

(define-test packages)

(define-test packages
  (check-package '#:sb-simd-internals)
  (check-package '#:sb-simd)
  #+x86-64
  (progn
    (check-package '#:sb-simd-x86-64)
    (check-package '#:sb-simd-sse)
    (check-package '#:sb-simd-sse2)
    (check-package '#:sb-simd-sse3)
    (check-package '#:sb-simd-ssse3)
    (check-package '#:sb-simd-sse4.1)
    (check-package '#:sb-simd-sse4.2)
    (check-package '#:sb-simd-avx)
    (check-package '#:sb-simd-avx2)
    (check-package '#:sb-simd-avx512f :skip '(f32-fmaddsub f32-fmsubadd f64-fmaddsub f64-fmsubadd))
    (check-package '#:sb-simd-avx512bw :skip '(f32-fmaddsub f32-fmsubadd f64-fmaddsub f64-fmsubadd))
    (check-package '#:sb-simd-avx512dq :skip '(f32-fmaddsub f32-fmsubadd f64-fmaddsub f64-fmsubadd))
    (check-package '#:sb-simd-avx512fp16 :skip '(f32-fmaddsub f32-fmsubadd f64-fmaddsub f64-fmsubadd)))
  #+arm64
  (check-package '#:sb-simd-neon)
  ;; Ensure that every instruction has a corresponding VOP.
  (dolist (instruction-record (filter-available-function-records #'instruction-record-p))
    (is (fboundp (instruction-record-vop instruction-record)))))
