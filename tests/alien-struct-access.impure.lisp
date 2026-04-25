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

(defmacro with-fault-handler (label &body body)
  `(with-memory-faults
     (handler-case (progn ,@body)
       (sb-sys:memory-fault-error (c)
         (error "~A: ~A" ,label c)))))

;;; VAR is bound to a deref'd struct view of a freshly-allocated
;;; guarded buffer.
;;;
;;; Optional SAP-VAR names the raw system-area-pointer to that same
;;; buffer, for probes that need to pass it to ALIEN-FUNCALL-INTO.

(defmacro with-guarded-struct ((var size type-form &optional sap-var) &body body)
  (let ((sap-sym (or sap-var (gensym "SAP-")))
        (size-sym (gensym "SIZE-")))
    `(let* ((,size-sym ,size)
            (,sap-sym (guarded-alloc ,size-sym)))
       (when (sb-sys:sap= ,sap-sym (sb-sys:int-sap 0))
         (error "guarded_alloc(~A) failed" ,size-sym))
       (unwind-protect
            (let ((,var (sb-alien:deref
                         (sb-alien:sap-alien ,sap-sym (* ,type-form)))))
              ,@body)
         (guarded-free ,sap-sym ,size-sym)))))

(defmacro probe-overread (size type-form init-form value-fn expected-value
                          &key (tolerance 0))
  `(with-guarded-struct (s ,size ,type-form)
     ,init-form
     (let ((result (with-fault-handler "OVER-READ" (,value-fn s))))
       ,(if (zerop tolerance)
            `(assert (= result ,expected-value))
            `(assert (< (abs (- result ,expected-value)) ,tolerance))))))

(defmacro probe-overwrite (size type-form make-name fn-type
                           call-args check-body)
  `(with-guarded-struct (s ,size ,type-form sap)
     (declare (ignorable s))
     (with-fault-handler "OVER-WRITE"
       (alien-funcall-into (extern-alien ,make-name ,fn-type)
                           sap ,@call-args))
     ,check-body))

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
    (let ((r (with-fault-handler "OVER-READ in identity"
               (gp-i8-identity s))))
      (assert (= (slot r 'm0) 99)))))

(with-test (:name :overread-3f-identity)
  (with-guarded-struct (s 12 (struct gp-3f))
    (setf (slot s 'a) 7.5f0 (slot s 'b) -1.25f0 (slot s 'c) 0.5f0)
    (let ((r (with-fault-handler "OVER-READ in identity"
               (gp-3f-identity s))))
      (assert (= (slot r 'a) 7.5f0))
      (assert (= (slot r 'b) -1.25f0))
      (assert (= (slot r 'c) 0.5f0)))))

;;; Over-write probes (return-buffer side).

(with-test (:name :overwrite-i8 :broken-on :sbcl)
  (probe-overwrite 1 (struct gp-i8)
                   "gp_i8_make"
                   (function (struct gp-i8) (signed 8))
                   (-42)
                   (assert (= (slot s 'm0) -42))))

(with-test (:name :overwrite-i16 :broken-on :sbcl)
  (probe-overwrite 2 (struct gp-i16)
                   "gp_i16_make"
                   (function (struct gp-i16) (signed 16))
                   (12345)
                   (assert (= (slot s 'm0) 12345))))

(with-test (:name :overwrite-i32 :broken-on :sbcl)
  (probe-overwrite 4 (struct gp-i32)
                   "gp_i32_make"
                   (function (struct gp-i32) (signed 32))
                   (#x7abcdef0)
                   (assert (= (slot s 'm0) #x7abcdef0))))

(with-test (:name :overwrite-1f :broken-on :sbcl)
  (probe-overwrite 4 (struct gp-1f)
                   "gp_1f_make"
                   (function (struct gp-1f) single-float)
                   (42.5f0)
                   (assert (= (slot s 'm0) 42.5f0))))

(with-test (:name :overwrite-3f :broken-on :sbcl)
  (probe-overwrite 12 (struct gp-3f)
                   "gp_3f_make"
                   (function (struct gp-3f)
                             single-float single-float single-float)
                   (1.0f0 2.0f0 3.0f0)
                   (progn
                     (assert (= (slot s 'a) 1.0f0))
                     (assert (= (slot s 'b) 2.0f0))
                     (assert (= (slot s 'c) 3.0f0)))))
