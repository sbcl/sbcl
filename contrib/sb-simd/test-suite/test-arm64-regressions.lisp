;;; Regression tests for AdvSIMD bugs found by auditing sb-simd against
;;; the A64 instruction set.  Each test targets a bug that once produced
;;; wrong results or illegal instructions rather than a clean error.

(in-package #:sb-simd-neon)

;;; Values that sit on both sides of the signed/unsigned wrap-around
;;; point.  Random sampling almost never generates pairs like
;;; (0, #xFFFFFFFFFFFFFFFF), so test them exhaustively.
(defparameter *u64-boundary-values*
  (list 0 1 2 3 5
        #x7FFFFFFFFFFFFFFE #x7FFFFFFFFFFFFFFF
        #x8000000000000000 #x8000000000000001
        #xFFFFFFFFFFFFFFFD #xFFFFFFFFFFFFFFFE #xFFFFFFFFFFFFFFFF))

(defparameter *s64-boundary-values*
  (list -9223372036854775808 -9223372036854775807 -1 0 1 42
        9223372036854775806 9223372036854775807))

;;; Issue: UMAX/UMIN/SMAX/SMIN have no .2d form; max/min are fake vops
;;; built from CMHI/CMGT plus BSL.
(sb-simd-test-suite:define-test u64.2-max-min-boundaries
  (dolist (a *u64-boundary-values*)
    (dolist (b *u64-boundary-values*)
      (let ((pa (make-u64.2 a a))
            (pb (make-u64.2 b b)))
        (sb-simd-test-suite:is (= (u64.2-lane-extract (u64.2-max pa pb) 0) (max a b)))
        (sb-simd-test-suite:is (= (u64.2-lane-extract (u64.2-min pa pb) 0) (min a b)))))))

(sb-simd-test-suite:define-test s64.2-max-min-boundaries
  (dolist (a *s64-boundary-values*)
    (dolist (b *s64-boundary-values*)
      (let ((pa (make-s64.2 a a))
            (pb (make-s64.2 b b)))
        (sb-simd-test-suite:is (= (s64.2-lane-extract (s64.2-max pa pb) 0) (max a b)))
        (sb-simd-test-suite:is (= (s64.2-lane-extract (s64.2-min pa pb) 0) (min a b)))))))

;;; Random cross-check of the same fake vops against the scalar
;;; reference implementation.
(sb-simd-test-suite:define-test u64.2-max-min-random
  (flet ((to-signed (x) (if (logbitp 63 x) (- x (ash 1 64)) x)))
    (dotimes (i 2000)
      (let* ((bits (lambda () (logior (ash (random (ash 1 32)) 32)
                                      (random (ash 1 32)))))
             (a0 (funcall bits)) (a1 (funcall bits))
             (b0 (funcall bits)) (b1 (funcall bits))
             (ua (make-u64.2 a0 a1))
             (ub (make-u64.2 b0 b1))
             (sa (make-s64.2 (to-signed a0) (to-signed a1)))
             (sb (make-s64.2 (to-signed b0) (to-signed b1))))
        (sb-simd-test-suite:is (= (u64.2-lane-extract (u64.2-max ua ub) 0) (max a0 b0)))
        (sb-simd-test-suite:is (= (u64.2-lane-extract (u64.2-min ua ub) 1) (min a1 b1)))
        (sb-simd-test-suite:is (= (s64.2-lane-extract (s64.2-max sa sb) 0)
               (max (to-signed a0) (to-signed b0))))
        (sb-simd-test-suite:is (= (s64.2-lane-extract (s64.2-min sa sb) 1)
               (min (to-signed a1) (to-signed b1))))))))

;;; Issue: the XTN-family narrowing records used .4h/.8h suffixes,
;;; silently computing a 32-to-16-bit narrowing on 64-bit lanes.
(sb-simd-test-suite:define-test u32.4-from-u64.2-narrowing
  ;; Plain narrowing takes the low half of each lane and zeroes the rest.
  (let ((r (u32.4-from-u64.2 (make-u64.2 #x1111111122222222
                                         #x3333333344444444))))
    (sb-simd-test-suite:is (= (u32.4-lane-extract r 0) #x22222222))
    (sb-simd-test-suite:is (= (u32.4-lane-extract r 1) #x44444444))
    (sb-simd-test-suite:is (= (u32.4-lane-extract r 2) 0))
    (sb-simd-test-suite:is (= (u32.4-lane-extract r 3) 0)))
  ;; Saturating narrowing clamps into the target range.
  (let ((r (u32.4-from-u64.2-saturating (make-u64.2 #xFFFFFFFFFFFFFFFF 3))))
    (sb-simd-test-suite:is (= (u32.4-lane-extract r 0) #xFFFFFFFF))
    (sb-simd-test-suite:is (= (u32.4-lane-extract r 1) 3)))
  ;; The -hi variants append to the high half and preserve the low half.
  (let ((r (u32.4-from-u64.2-hi (make-u32.4 10 20 30 40)
                                (make-u64.2 #x1111111122222222
                                            #x3333333344444444))))
    (sb-simd-test-suite:is (= (u32.4-lane-extract r 0) 10))
    (sb-simd-test-suite:is (= (u32.4-lane-extract r 1) 20))
    (sb-simd-test-suite:is (= (u32.4-lane-extract r 2) #x22222222))
    (sb-simd-test-suite:is (= (u32.4-lane-extract r 3) #x44444444)))
  (let ((r (u32.4-from-u64.2-saturating-hi
            (make-u32.4 10 20 30 40)
            (make-u64.2 #xFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF))))
    (sb-simd-test-suite:is (= (u32.4-lane-extract r 0) 10))
    (sb-simd-test-suite:is (= (u32.4-lane-extract r 1) 20))
    (sb-simd-test-suite:is (= (u32.4-lane-extract r 2) #xFFFFFFFF))
    (sb-simd-test-suite:is (= (u32.4-lane-extract r 3) #xFFFFFFFF))))

(sb-simd-test-suite:define-test s32.4-from-s64.2-narrowing
  (let ((r (s32.4-from-s64.2 (make-s64.2 -5 #x7000000011112222))))
    (sb-simd-test-suite:is (= (s32.4-lane-extract r 0) -5))
    (sb-simd-test-suite:is (= (s32.4-lane-extract r 1) #x11112222))
    (sb-simd-test-suite:is (= (s32.4-lane-extract r 2) 0))
    (sb-simd-test-suite:is (= (s32.4-lane-extract r 3) 0)))
  ;; Saturation clamps in both directions.
  (let ((r (s32.4-from-s64.2-saturating
            (make-s64.2 #x7FFFFFFFFFFFFFFF -99999999999999))))
    (sb-simd-test-suite:is (= (s32.4-lane-extract r 0) most-positive-s32))
    (sb-simd-test-suite:is (= (s32.4-lane-extract r 1) most-negative-s32)))
  (let ((r (s32.4-from-s64.2-hi (make-s32.4 10 20 30 40)
                                (make-s64.2 -5 #x7000000011112222))))
    (sb-simd-test-suite:is (= (s32.4-lane-extract r 0) 10))
    (sb-simd-test-suite:is (= (s32.4-lane-extract r 1) 20))
    (sb-simd-test-suite:is (= (s32.4-lane-extract r 2) -5))
    (sb-simd-test-suite:is (= (s32.4-lane-extract r 3) #x11112222))))

;;; Issue: signed lane extraction returned zero-extended values because
;;; the custom vops emitted UMOV instead of SMOV.
(sb-simd-test-suite:define-test signed-lane-extraction-sign-extends
  (let ((v (make-s8.16 -128 -1 127 42 0 -42 1 -127 2 -2 3 -3 4 -4 5 -5)))
    (loop for expected in '(-128 -1 127 42 0 -42 1 -127 2 -2 3 -3 4 -4 5 -5)
          for i from 0
          do (sb-simd-test-suite:is (= (s8.16-lane-extract v i) expected))))
  (let ((v (make-s16.8 -32768 -1 32767 12345 -5432 0 1 -1)))
    (loop for expected in '(-32768 -1 32767 12345 -5432 0 1 -1)
          for i from 0
          do (sb-simd-test-suite:is (= (s16.8-lane-extract v i) expected))))
  (let ((v (make-s32.4 -2147483648 -1 2147483647 42)))
    (loop for expected in '(-2147483648 -1 2147483647 42)
          for i from 0
          do (sb-simd-test-suite:is (= (s32.4-lane-extract v i) expected))))
  ;; Unsigned shapes must remain unaffected.
  (let ((v (make-u8.16 255 254 253 252 0 1 2 3 4 5 6 7 8 9 10 11)))
    (sb-simd-test-suite:is (= (u8.16-lane-extract v 0) 255))
    (sb-simd-test-suite:is (= (u8.16-lane-extract v 3) 252))))

;;; Issue: emit-movi-vector-imm passed replicated broadcast patterns to
;;; MOVI instead of lane immediates, aborting compilation with "bad
;;; immediate".  Every constant below is materialized through that code
;;; path; several exercise the shifted-cmode forms.  The tests are split
;;; into several functions because packing too many distinct vector
;;; constants into one function currently overflows the LDR spill
;;; offset range - see issue about large SIMD constant pools.
(sb-simd-test-suite:define-test integer-broadcast-constants-simple
  ;; The original reproducer: 1 does not fit MOVI's simple .8h form.
  (let ((r (u16.8-and (u16.8-broadcast 5) 1)))
    (sb-simd-test-suite:is (= (u16.8-lane-extract r 0) 1))
    (sb-simd-test-suite:is (= (u16.8-lane-extract r 7) 1)))
  ;; Shifted immediates (low byte zero).
  (let ((r (u16.8-and (u16.8-broadcast #x0100) #x0100)))
    (sb-simd-test-suite:is (= (u16.8-lane-extract r 3) #x0100)))
  ;; Byte-replicated patterns.
  (dolist (c (list 0 1 #x7F #x80 #xFF))
    (let ((r (u8.16-or (u8.16-broadcast c) (u8.16-broadcast c))))
      (sb-simd-test-suite:is (= (u8.16-lane-extract r 15) c)))))

(sb-simd-test-suite:define-test integer-broadcast-constants-word
  ;; 32-bit constants with shifted bytes.
  (dolist (c (list 0 1 255 256 #xFFFF #x10000 #xFF0000 #xFF000000))
    (let ((r (u32.4-and (u32.4-broadcast c) (u32.4-broadcast c))))
      (sb-simd-test-suite:is (= (u32.4-lane-extract r 0) c)))))

;;; Bit-replication patterns (.2d MOVI form): every byte is 0 or #xFF.
(sb-simd-test-suite:define-test integer-broadcast-constants-bit-replicated
  (dolist (c (list #xFF00FF00FF00FF00
                   #x8000000000000000
                   #xFFFFFFFFFFFFFFFF
                   #x7F7F7F7F7F7F7F7F
                   #x00FF00FF00FF00FF))
    (let ((r (u64.2-and (u64.2-broadcast c) (u64.2-broadcast c))))
      (sb-simd-test-suite:is (= (u64.2-lane-extract r 0) c))
      (sb-simd-test-suite:is (= (u64.2-lane-extract r 1) c)))))

;;; Patterns that fit no MOVI form fall back to register synthesis
;;; (MOVZ/DUP or logical-immediate ORR).
(sb-simd-test-suite:define-test integer-broadcast-constants-synthesized
  (dolist (c (list #x0102030405060708 #xDEADBEEFCAFEBABE))
    (let ((r (u64.2-and (u64.2-broadcast c) (u64.2-broadcast c))))
      (sb-simd-test-suite:is (= (u64.2-lane-extract r 1) c)))))

;;; Float constants take a different path (movi-immediate-p guarded);
;;; keep them covered too.
(sb-simd-test-suite:define-test float-broadcast-constants
  (let ((r (f32.4-max (make-f32.4 0.5 -1.0 2.0 3.0) 1.0f0)))
    (sb-simd-test-suite:is (= (f32.4-lane-extract r 0) 1.0f0))
    (sb-simd-test-suite:is (= (f32.4-lane-extract r 1) 1.0f0))
    (sb-simd-test-suite:is (= (f32.4-lane-extract r 2) 2.0f0)))
  (let ((r (f64.2-min (make-f64.2 0.5d0 -5d0) -1d0)))
    (sb-simd-test-suite:is (= (f64.2-lane-extract r 0) -1d0))
    (sb-simd-test-suite:is (= (f64.2-lane-extract r 1) -5d0))))
