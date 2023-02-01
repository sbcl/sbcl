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
