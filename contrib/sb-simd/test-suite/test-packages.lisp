(in-package #:sb-simd-test-suite)

(define-test packages)

(define-test packages
  (check-package '#:sb-simd-internals)
  (check-package '#:sb-simd)
  (check-package '#:sb-simd-x86-64)
  (check-package '#:sb-simd-sse)
  (check-package '#:sb-simd-sse2)
  (check-package '#:sb-simd-sse3)
  (check-package '#:sb-simd-ssse3)
  (check-package '#:sb-simd-sse4.1)
  (check-package '#:sb-simd-sse4.2)
  (check-package '#:sb-simd-avx)
  (check-package '#:sb-simd-avx2))
