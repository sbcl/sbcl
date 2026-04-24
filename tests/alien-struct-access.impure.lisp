;;;; Over-read tests for struct-by-value argument passing.
;;;;
;;;; Allocate shapes whose size is not a whole number of 8-byte
;;;; eightbytes next to a guarded page, then pass to an alien
;;;; function by value: reading past the declared size will fault.

#-(or x86-64 arm64) (invoke-restart 'run-tests::skip-file)

;;; POSIX-only: Windows would need VirtualAlloc

#+win32             (invoke-restart 'run-tests::skip-file)

;;; Guard page allocation and shapre fixtures

(compile-so "alien-struct-access.c" "alien-struct-access.so")

(define-alien-routine guarded-alloc system-area-pointer
  (size unsigned-long))

(define-alien-routine guarded-free void
  (p system-area-pointer) (size unsigned-long))

(define-alien-type nil (struct gp-i8 (m0 (signed 8))))
(define-alien-routine gp-i8-sum (signed 64) (s (struct gp-i8)))
(define-alien-routine gp-i8-identity (struct gp-i8) (s (struct gp-i8)))

(define-alien-type nil (struct gp-i16 (m0 (signed 16))))
(define-alien-routine gp-i16-sum (signed 64) (s (struct gp-i16)))
(define-alien-routine gp-i16-identity (struct gp-i16) (s (struct gp-i16)))

(define-alien-type nil (struct gp-i32 (m0 (signed 32))))
(define-alien-routine gp-i32-sum (signed 64) (s (struct gp-i32)))
(define-alien-routine gp-i32-identity (struct gp-i32) (s (struct gp-i32)))

(define-alien-type nil (struct gp-1f (m0 single-float)))
(define-alien-routine gp-1f-sum double (s (struct gp-1f)))
(define-alien-routine gp-1f-identity (struct gp-1f) (s (struct gp-1f)))

(define-alien-type nil (struct gp-i8x7 (m (array (signed 8) 7))))
(define-alien-routine gp-i8x7-sum (signed 64) (s (struct gp-i8x7)))
(define-alien-routine gp-i8x7-identity (struct gp-i8x7) (s (struct gp-i8x7)))

(define-alien-type nil (struct gp-i8x9 (m (array (signed 8) 9))))
(define-alien-routine gp-i8x9-sum (signed 64) (s (struct gp-i8x9)))
(define-alien-routine gp-i8x9-identity (struct gp-i8x9) (s (struct gp-i8x9)))

(define-alien-type nil (struct gp-i8x15 (m (array (signed 8) 15))))
(define-alien-routine gp-i8x15-sum (signed 64) (s (struct gp-i8x15)))
(define-alien-routine gp-i8x15-identity (struct gp-i8x15) (s (struct gp-i8x15)))

(define-alien-type nil (struct gp-3f (a single-float) (b single-float) (c single-float)))
(define-alien-routine gp-3f-sum double (s (struct gp-3f)))
(define-alien-routine gp-3f-identity (struct gp-3f) (s (struct gp-3f)))

;;; The test harness runs with --lose-on-corruption, which causes
;;; SIGSEGV to lose(): flip the runtime flag while inside a probe.

(defmacro with-memory-faults (&body body)
  `(sb-alien:with-alien ((flag sb-alien:int :extern "lose_on_corruption_p"))
     (let ((saved flag))
       (unwind-protect
            (progn
              (setf flag 0)
              ,@body)
         (setf flag saved)))))

(defmacro with-guarded-struct ((var size type-form) &body body)
  (let ((sap (gensym "SAP-"))
        (size-sym (gensym "SIZE-")))
    `(let* ((,size-sym ,size)
            (,sap (guarded-alloc ,size-sym)))
       (when (sb-sys:sap= ,sap (sb-sys:int-sap 0))
         (error "guarded_struct_alloc(~A) failed" ,size-sym))
       (unwind-protect
            (let ((,var (sb-alien:deref
                         (sb-alien:sap-alien ,sap (* ,type-form)))))
              ,@body)
         (guarded-free ,sap ,size-sym)))))

(defmacro probe-overread (size type-form init-form sum-fn expected-sum
                          &key (tolerance 0))
  `(with-guarded-struct (s ,size ,type-form)
     ,init-form
     (let ((result (with-memory-faults
                       (handler-case (,sum-fn s)
                         (sb-sys:memory-fault-error (c)
                           (error "OVER-READ: ~A" c))))))
       ,(if (zerop tolerance)
            `(assert (= result ,expected-sum))
            `(assert (< (abs (- result ,expected-sum)) ,tolerance))))))

(with-test (:name :overread-i8)
  (probe-overread 1 (struct gp-i8)
                  (setf (slot s 'm0) -42)
                  gp-i8-sum -42))

(with-test (:name :overread-i16)
  (probe-overread 2 (struct gp-i16)
                  (setf (slot s 'm0) -12345)
                  gp-i16-sum -12345))

(with-test (:name :overread-i32)
  (probe-overread 4 (struct gp-i32)
                  (setf (slot s 'm0) #x7abcdef0)
                  gp-i32-sum #x7abcdef0))

(with-test (:name :overread-1f)
  (probe-overread 4 (struct gp-1f)
                  (setf (slot s 'm0) 42.5f0)
                  gp-1f-sum 42.5d0
                  :tolerance 1d-6))

(with-test (:name :overread-i8x7)
  (probe-overread 7 (struct gp-i8x7)
                  (dotimes (i 7)
                    (setf (deref (slot s 'm) i) (- i 3)))
                  gp-i8x7-sum (loop for i below 7 sum (- i 3))))

(with-test (:name :overread-i8x9)
  (probe-overread 9 (struct gp-i8x9)
                  (dotimes (i 9)
                    (setf (deref (slot s 'm) i) (- i 4)))
                  gp-i8x9-sum (loop for i below 9 sum (- i 4))))

(with-test (:name :overread-i8x15)
  (probe-overread 15 (struct gp-i8x15)
                  (dotimes (i 15)
                    (setf (deref (slot s 'm) i) (- i 7)))
                  gp-i8x15-sum (loop for i below 15 sum (- i 7))))

(with-test (:name :overread-3f)
  (probe-overread 12 (struct gp-3f)
                  (setf (slot s 'a) 1.0f0
                        (slot s 'b) 2.0f0
                        (slot s 'c) 3.0f0)
                  gp-3f-sum 6.0d0
                  :tolerance 1d-6))

;;; Round-trip probes: exercise both the input arg-read and the
;;; return-value side.

(with-test (:name :overread-i8-identity)
  (with-guarded-struct (s 1 (struct gp-i8))
    (setf (slot s 'm0) 99)
    (let ((r (with-memory-faults
               (handler-case (gp-i8-identity s)
                 (sb-sys:memory-fault-error (c)
                   (error "OVER-READ in identity: ~A" c))))))
      (assert (= (slot r 'm0) 99)))))

(with-test (:name :overread-3f-identity)
  (with-guarded-struct (s 12 (struct gp-3f))
    (setf (slot s 'a) 7.5f0 (slot s 'b) -1.25f0 (slot s 'c) 0.5f0)
    (let ((r (with-memory-faults
               (handler-case (gp-3f-identity s)
                 (sb-sys:memory-fault-error (c)
                   (error "OVER-READ in identity: ~A" c))))))
      (assert (= (slot r 'a) 7.5f0))
      (assert (= (slot r 'b) -1.25f0))
      (assert (= (slot r 'c) 0.5f0)))))
