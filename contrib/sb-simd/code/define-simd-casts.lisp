(in-package #:sb-simd-internals)

;;; For each SIMD data type X.Y, define two functions:
;;;
;;; 1. A function named X.Y that ensures that an object is either of the
;;;    type X.Y, or a scalar that can be broadcast to the type X.Y.
;;;
;;; 2. A function named X.Y! that reinterprets the bits of another SIMD
;;;    pack or suitable scalar as an X.Y.  If the supplied argument has
;;;    more bits than the target data type, the excess bits are discarded.
;;;    If the supplied argument has less bits than the target data types,
;;;    the remaining bits are set to zero.

;;; The pXXX SIMD types are special - we define their 'cast' function
;;; manually.
(define-inline sb-simd-sse:p128 (x) (the sb-simd-sse:p128 x))
(define-inline sb-simd-avx:p128 (x) (the sb-simd-avx:p128 x))
(define-inline sb-simd-avx:p256 (x) (the sb-simd-avx:p256 x))

(macrolet
    (;; We cannot call known functions directly in the definition of a
     ;; cast, only their VOPs.  The reason is that each known function
     ;; definition uses casts to automatically upgrade its arguments, and
     ;; we don't want to end up with a circular dependency.
     (call-vop (instruction-record-name &rest arguments)
       (with-accessors ((instruction-set instruction-record-instruction-set)
                        (vop instruction-record-vop))
           (find-function-record instruction-record-name)
         (if (instruction-set-available-p instruction-set)
             `(,vop ,@arguments)
             `(progn
                (missing-instruction
                 (load-time-value
                  (find-function-record ',instruction-record-name)))
                (touch ,@arguments)))))
     (define-simd-cast (simd-cast-record-name)
       (with-accessors ((name simd-cast-record-name)
                        (instruction-set simd-cast-record-instruction-set)
                        (broadcast-record simd-cast-record-broadcast))
           (find-function-record simd-cast-record-name)
         (let* ((broadcast (function-record-name broadcast-record))
                (simd-record (function-record-result-record broadcast-record))
                (real-record (simd-record-scalar-record simd-record))
                (simd-type (value-record-name simd-record))
                (real-type (value-record-name real-record))
                (package (instruction-set-package instruction-set))
                (err (mksym package "CANNOT-CONVERT-TO-" name)))
           `(progn
              (define-notinline ,err (x)
                (error "Cannot convert ~S to ~S." x ',name))
              (sb-c:defknown ,name (t) (values ,name &optional)
                  (sb-c:foldable)
                :overwrite-fndb-silently t)
              (sb-c:deftransform ,name ((x) (,simd-type) *)
                'x)
              (sb-c:deftransform ,name ((x) (real) *)
                '(,broadcast (,real-type x)))
              (defun ,name (x)
                (typecase x
                  (,simd-type x)
                  (real (call-vop ,broadcast (,real-type x)))
                  (otherwise (,err x))))))))
     (define-reinterpret-cast (reinterpret-cast-record)
       (with-accessors ((name reinterpret-cast-record-name)
                        (instruction-set reinterpret-cast-record-instruction-set)
                        (reinterpreters reinterpret-cast-record-reinterpreters))
           (find-function-record reinterpret-cast-record)
         (let* ((package (instruction-set-package instruction-set))
                (err (mksym package "CANNOT-REINTERPRET-AS-" name)))
           `(progn
              (define-notinline ,err (x)
                (error "Cannot reinterpret ~S as ~S." x ',name))
              (declaim (inline ,name))
              (defun ,name (x)
                (typecase x
                  ,@(loop for reinterpreter in reinterpreters
                          for argument-record = (first (function-record-required-argument-records reinterpreter))
                          collect
                          `(,(value-record-name argument-record)
                            (call-vop ,(function-record-name reinterpreter) x)))
                  (otherwise (,err x))))))))
     (define-simd-casts ()
       `(progn
          ,@(loop for simd-cast-record in (filter-function-records #'simd-cast-record-p)
                  collect
                  `(define-simd-cast ,(simd-cast-record-name simd-cast-record)))))
     (define-reinterpret-casts ()
       `(progn
          ,@(loop for reinterpret-cast-record in (filter-function-records #'reinterpret-cast-record-p)
                  collect
                  `(define-reinterpret-cast ,(reinterpret-cast-record-name reinterpret-cast-record))))))
  (define-simd-casts)
  (define-reinterpret-casts))
