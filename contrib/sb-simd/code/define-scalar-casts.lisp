(in-package #:sb-simd-internals)

;;; For each value record we define a function of the same name that will
;;; either suitably convert its argument to that value record's type, or
;;; signal an error.

(macrolet
    ((define-scalar-cast (scalar-cast-record-name)
       (with-accessors ((name scalar-cast-record-name))
           (find-function-record scalar-cast-record-name)
         (let ((err (mksym (symbol-package name) "CANNOT-CONVERT-TO-" name)))
           `(progn
              (define-notinline ,err (x)
                (error "Cannot convert ~S to ~S." x ',name))
              (sb-c:defknown ,name (t) (values ,name &optional)
                  (sb-c:foldable)
                :overwrite-fndb-silently t)
              (sb-c:deftransform ,name ((x) (,name) *)
                'x)
              ,@(case name
                  (sb-simd:f32
                   `((sb-c:deftransform ,name ((x) (double-float) *)
                       '(coerce x 'single-float))))
                  (sb-simd:f64
                   `((sb-c:deftransform ,name ((x) (single-float) *)
                       '(coerce x 'double-float))))
                  (sb-simd-sse:f32
                   `((sb-c:deftransform ,name ((x) (double-float) *)
                       '(sb-kernel:%single-float x))
                     (sb-c:deftransform ,name ((x) ((signed-byte 64)) *)
                       '(sb-simd-sse::f32-from-s64 x))))
                  (sb-simd-sse2:f64
                   `((sb-c:deftransform ,name ((x) (single-float) *)
                       '(sb-simd-sse2::f64-from-f32 x))
                     (sb-c:deftransform ,name ((x) ((signed-byte 64)) *)
                       '(sb-simd-sse2::f64-from-s64 x))))
                  (sb-simd-avx:f32
                   `((sb-c:deftransform ,name ((x) (double-float) *)
                       '(sb-simd-avx::f32-from-f64 x))
                     (sb-c:deftransform ,name ((x) ((signed-byte 64)) *)
                       '(sb-simd-avx::f32-from-s64 x))))
                  (sb-simd-avx:f64
                   `((sb-c:deftransform ,name ((x) (single-float) *)
                       '(sb-simd-avx::f64-from-f32 x))
                     (sb-c:deftransform ,name ((x) ((signed-byte 64)) *)
                       '(sb-simd-avx::f64-from-s64 x)))))
              (defun ,name (x)
                (typecase x
                  (,name x)
                  ,@(case name
                      (sb-simd:f32
                       `((double-float (coerce x 'single-float))
                         (real (coerce x ',name))))
                      (sb-simd:f64
                       `((sb-simd-sse2:f32 (coerce x 'double-float))
                         (real (coerce x ',name))))
                      (sb-simd-sse:f32
                       `((double-float (sb-kernel:%single-float x))
                         (sb-simd-sse:s64 (sb-simd-sse::%f32-from-s64 x))
                         (real (coerce x ',name))))
                      (sb-simd-sse2:f64
                       `((sb-simd-sse2:f32 (sb-simd-sse2::%f64-from-f32 x))
                         (sb-simd-sse2:s64 (sb-simd-sse2::%f64-from-s64 x))
                         (real (coerce x ',name))))
                      (sb-simd-avx:f32
                       `((sb-simd-avx:f64 (sb-simd-avx::%f32-from-f64 x))
                         (sb-simd-avx:s64 (sb-simd-avx::%f32-from-s64 x))
                         (real (coerce x ',name))))
                      (sb-simd-avx:f64
                       `((sb-simd-avx:f32 (sb-simd-avx::%f64-from-f32 x))
                         (sb-simd-avx:s64 (sb-simd-avx::%f64-from-s64 x))
                         (real (coerce x ',name)))))
                  (otherwise (,err x))))))))
     (define-scalar-casts ()
       `(progn
          ,@(loop for scalar-cast-record in (filter-function-records #'scalar-cast-record-p)
                  collect `(define-scalar-cast ,(function-record-name scalar-cast-record))))))
  (define-scalar-casts))
