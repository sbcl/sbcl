(in-package #:sb-simd-neon)

(define-fake-vop f32.4-values (x)
  (values (%f32.4-lane-extract x 0)
          (%f32.4-lane-extract x 1)
          (%f32.4-lane-extract x 2)
          (%f32.4-lane-extract x 3)))

(define-fake-vop make-f32.4 (x y z w)
  (let* ((t1 (%f32.4!-from-f32 x))
         (t2 (%f32.4-lane-insert t1 y 1))
         (t3 (%f32.4-lane-insert t2 z 2)))
    (%f32.4-lane-insert t3 w 3)))

(define-fake-vop two-arg-f32.4/= (a b)
  (%u32.4-not
   (%two-arg-f32.4= a b)))

(define-fake-vop two-arg-f32.4< (a b)
  (%two-arg-f32.4> b a))

(define-fake-vop two-arg-f32.4<= (a b)
  (%two-arg-f32.4>= b a))

(define-fake-vop f32.4-horizontal+ (x)
  (let ((tmp (%f32.4-pair+ x x)))
    ;; TODO: This could use the scalar faddp instead, avoiding the extraction.
    (%f32.4-lane-extract (%f32.4-pair+ tmp tmp) 0)))

(define-fake-vop f32.4-andc1 (a b)
  (%f32.4-andc2 b a))

(define-fake-vop f64.2-values (x)
  (values (%f64.2-lane-extract x 0)
          (%f64.2-lane-extract x 1)))

(define-fake-vop make-f64.2 (x y)
  (%f64.2-lane-insert (%f64.2!-from-f64 x) y 1))

(define-fake-vop two-arg-f64.2/= (a b)
  (%u64.2-not
   (%two-arg-f64.2= a b)))

(define-fake-vop two-arg-f64.2< (a b)
  (%two-arg-f64.2> b a))

(define-fake-vop two-arg-f64.2<= (a b)
  (%two-arg-f64.2>= b a))

(define-fake-vop f64.2-andc1 (a b)
  (%f64.2-andc2 b a))

(macrolet
    ((def (sign width count)
       (flet ((name (format-control &rest format-arguments)
                (intern (apply #'format nil format-control format-arguments)
                        (find-package :sb-simd-neon))))
         (let ((make-args (loop for i from 0 below count
                                collect (make-symbol (format nil "V~a" i))))
               (from (name "%~a~d.~d!-FROM-~a~d" sign width count sign width))
               (insert (name "%~a~d.~d-LANE-INSERT" sign width count))
               (extract (name "%~a~d.~d-LANE-EXTRACT" sign width count)))
           `(progn
              (define-fake-vop ,(name "MAKE-~a~d.~d" sign width count) ,make-args
                ,(loop with result = (list from (first make-args))
                       for arg in (rest make-args)
                       for i from 1
                       do (setf result (list insert result arg i))
                       finally (return result)))
              (define-fake-vop ,(name "~a~d.~d-VALUES" sign width count) (x)
                (values ,@(loop for i from 0 below count
                                collect `(,extract x ,i))))
              (define-fake-vop ,(name "~a~d.~d-ANDC1" sign width count) (a b)
                (,(name "%~a~d.~d-ANDC2" sign width count) b a))
              (define-fake-vop ,(name "TWO-ARG-~a~d.~d/=" sign width count) (a b)
                (,(name "%U~d.~d-NOT" width count)
                 (,(name "%TWO-ARG-~a~d.~d=" sign width count) a b)))
              (define-fake-vop ,(name "TWO-ARG-~a~d.~d<" sign width count) (a b)
                (,(name "%TWO-ARG-~a~d.~d>" sign width count) b a))
              (define-fake-vop ,(name "TWO-ARG-~a~d.~d<=" sign width count) (a b)
                (,(name "%TWO-ARG-~a~d.~d>=" sign width count) b a)))))))
  (def :u 8 16)
  (def :u 16 8)
  (def :u 32 4)
  (def :u 64 2)
  (def :s 8 16)
  (def :s 16 8)
  (def :s 32 4)
  (def :s 64 2))

(define-fake-vop u64.2-pair-min (a b)
  (%make-u64.2 (sb-simd::%two-arg-u64-min (%u64.2-lane-extract a 0) (%u64.2-lane-extract a 1))
               (sb-simd::%two-arg-u64-min (%u64.2-lane-extract b 0) (%u64.2-lane-extract b 1))))

(define-fake-vop u64.2-horizontal-min (a)
  (sb-simd::%two-arg-u64-min (%u64.2-lane-extract a 0) (%u64.2-lane-extract a 1)))

(define-fake-vop u64.2-pair-max (a b)
  (%make-u64.2 (sb-simd::%two-arg-u64-max (%u64.2-lane-extract a 0) (%u64.2-lane-extract a 1))
               (sb-simd::%two-arg-u64-max (%u64.2-lane-extract b 0) (%u64.2-lane-extract b 1))))

(define-fake-vop u64.2-horizontal-max (a)
  (sb-simd::%two-arg-u64-max (%u64.2-lane-extract a 0) (%u64.2-lane-extract a 1)))

(define-fake-vop u64.2-pair+ (a b)
  (%make-u64.2 (sb-simd::%two-arg-u64+ (%u64.2-lane-extract a 0) (%u64.2-lane-extract a 1))
               (sb-simd::%two-arg-u64+ (%u64.2-lane-extract b 0) (%u64.2-lane-extract b 1))))

(define-fake-vop u64.2-horizontal+ (a)
  (sb-simd::%two-arg-u64+ (%u64.2-lane-extract a 0) (%u64.2-lane-extract a 1)))

(define-fake-vop s64.2-pair-min (a b)
  (%make-s64.2 (sb-simd::%two-arg-s64-min (%s64.2-lane-extract a 0) (%s64.2-lane-extract a 1))
               (sb-simd::%two-arg-s64-min (%s64.2-lane-extract b 0) (%s64.2-lane-extract b 1))))

(define-fake-vop s64.2-horizontal-min (a)
  (sb-simd::%two-arg-s64-min (%s64.2-lane-extract a 0) (%s64.2-lane-extract a 1)))

(define-fake-vop s64.2-pair-max (a b)
  (%make-s64.2 (sb-simd::%two-arg-s64-max (%s64.2-lane-extract a 0) (%s64.2-lane-extract a 1))
               (sb-simd::%two-arg-s64-max (%s64.2-lane-extract b 0) (%s64.2-lane-extract b 1))))

(define-fake-vop s64.2-horizontal-max (a)
  (sb-simd::%two-arg-s64-max (%s64.2-lane-extract a 0) (%s64.2-lane-extract a 1)))

(define-fake-vop s64.2-pair+ (a b)
  (%make-s64.2 (sb-simd::%two-arg-s64+ (%s64.2-lane-extract a 0) (%s64.2-lane-extract a 1))
               (sb-simd::%two-arg-s64+ (%s64.2-lane-extract b 0) (%s64.2-lane-extract b 1))))

(define-fake-vop s64.2-horizontal+ (a)
  (sb-simd::%two-arg-s64+ (%s64.2-lane-extract a 0) (%s64.2-lane-extract a 1)))
