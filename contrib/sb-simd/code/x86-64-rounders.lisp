(in-package #:sb-simd-sse4.1)

(define-inline f32.4-round (x)
  (f32.4-%round x #b00))

(define-inline f32.4-floor (x)
  (f32.4-%round x #b01))

(define-inline f32.4-ceiling (x)
  (f32.4-%round x #b10))

(define-inline f32.4-truncate (x)
  (f32.4-%round x #b11))

(define-inline f64.2-round (x)
  (f64.2-%round x #b00))

(define-inline f64.2-floor (x)
  (f64.2-%round x #b01))

(define-inline f64.2-ceiling (x)
  (f64.2-%round x #b10))

(define-inline f64.2-truncate (x)
  (f64.2-%round x #b11))

(in-package #:sb-simd-avx)

(define-inline f32.4-round (x)
  (f32.4-%round x #b00))

(define-inline f32.4-floor (x)
  (f32.4-%round x #b01))

(define-inline f32.4-ceiling (x)
  (f32.4-%round x #b10))

(define-inline f32.4-truncate (x)
  (f32.4-%round x #b11))

(define-inline f64.2-round (x)
  (f64.2-%round x #b00))

(define-inline f64.2-floor (x)
  (f64.2-%round x #b01))

(define-inline f64.2-ceiling (x)
  (f64.2-%round x #b10))

(define-inline f64.2-truncate (x)
  (f64.2-%round x #b11))

(define-inline f32.8-round (x)
  (f32.8-%round x #b00))

(define-inline f32.8-floor (x)
  (f32.8-%round x #b01))

(define-inline f32.8-ceiling (x)
  (f32.8-%round x #b10))

(define-inline f32.8-truncate (x)
  (f32.8-%round x #b11))

(define-inline f64.4-round (x)
  (f64.4-%round x #b00))

(define-inline f64.4-floor (x)
  (f64.4-%round x #b01))

(define-inline f64.4-ceiling (x)
  (f64.4-%round x #b10))

(define-inline f64.4-truncate (x)
  (f64.4-%round x #b11))

(in-package #:sb-simd-avx512f)

(define-inline f32.16-round (x)
  (f32.16-%round x #x00))

(define-inline f32.16-floor (x)
  (f32.16-%round x #x01))

(define-inline f32.16-ceiling (x)
  (f32.16-%round x #x02))

(define-inline f32.16-truncate (x)
  (f32.16-%round x #x03))

(define-inline f64.8-round (x)
  (f64.8-%round x #x00))

(define-inline f64.8-floor (x)
  (f64.8-%round x #x01))

(define-inline f64.8-ceiling (x)
  (f64.8-%round x #x02))

(define-inline f64.8-truncate (x)
  (f64.8-%round x #x03))

