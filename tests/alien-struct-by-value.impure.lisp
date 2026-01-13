;;;; This file is for compiler tests of passing structs by value to/from
;;;; foreign functions.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

#-(or (and x86-64 (not win32)) arm64) (invoke-restart 'run-tests::skip-file)

;;; Compile and load shared library

(defvar *soname*)
#+win32
(with-scratch-file (dll "dll")
  (sb-ext:run-program "gcc" `("-shared" "-o" ,dll "alien-struct-by-value.c")
                      :search t)
  (load-shared-object dll))
#-win32
(progn
  (unless (probe-file "alien-struct-by-value.so")
    (sb-ext:run-program "/bin/sh" '("run-compiler.sh" "-sbcl-pic" "-sbcl-shared"
                                    "-o" "alien-struct-by-value.so"
                                    "alien-struct-by-value.c")))
  (setq *soname* (truename "alien-struct-by-value.so"))
  (load-shared-object *soname*))

;;; Tiny struct, alignment 8 (fits in one register)
(define-alien-type nil (struct tiny-align-8 (m0 (integer 64))))

(define-alien-routine tiny-align-8-get-m0 (integer 64) (m (struct tiny-align-8)))
(define-alien-routine tiny-align-8-mutate void (m (struct tiny-align-8)))

(define-alien-routine tiny-align-8-return (struct tiny-align-8) (val (integer 64)))
(define-alien-routine tiny-align-8-identity (struct tiny-align-8) (m (struct tiny-align-8)))

;;; Runtime tests for tiny struct
(with-test (:name :struct-by-value-tiny-align-8-runtime)
  ;; Test passing struct as argument
  (with-alien ((s (struct tiny-align-8)))
    (setf (slot s 'm0) 42)
    (assert (= (tiny-align-8-get-m0 s) 42)))
  ;; Test passing different values
  (with-alien ((s (struct tiny-align-8)))
    (setf (slot s 'm0) -123456789)
    (assert (= (tiny-align-8-get-m0 s) -123456789))))

;;; Runtime tests for tiny struct return
(with-test (:name :struct-by-value-tiny-align-8-return-runtime)
  ;; Test return from C function
  (let ((result (tiny-align-8-return 42)))
    (assert (= (slot result 'm0) 42)))
  ;; Test with negative value
  (let ((result (tiny-align-8-return -987654321)))
    (assert (= (slot result 'm0) -987654321)))
  ;; Test with zero
  (let ((result (tiny-align-8-return 0)))
    (assert (= (slot result 'm0) 0)))
  ;; Test with max positive value
  (let ((result (tiny-align-8-return (1- (ash 1 63)))))
    (assert (= (slot result 'm0) (1- (ash 1 63)))))
  ;; Test identity (pass struct, get same struct back)
  (with-alien ((s (struct tiny-align-8)))
    (setf (slot s 'm0) 12345)
    (let ((result (tiny-align-8-identity s)))
      (assert (= (slot result 'm0) 12345)))))

;;; Small struct, alignment 8 (fits in two registers)
(define-alien-type nil (struct small-align-8 (m0 (integer 64)) (m1 (integer 64))))

(define-alien-routine small-align-8-get-m0 (integer 64) (m (struct small-align-8)))
(define-alien-routine small-align-8-get-m1 (integer 64) (m (struct small-align-8)))
(define-alien-routine small-align-8-mutate void (m (struct small-align-8)))

(define-alien-routine small-align-8-return (struct small-align-8)
  (v0 (integer 64)) (v1 (integer 64)))
(define-alien-routine small-align-8-identity (struct small-align-8) (m (struct small-align-8)))

;;; Runtime tests for small struct (2 registers)
(with-test (:name :struct-by-value-small-align-8-runtime)
  ;; Test passing struct as argument
  (with-alien ((s (struct small-align-8)))
    (setf (slot s 'm0) 100)
    (setf (slot s 'm1) 200)
    (assert (= (small-align-8-get-m0 s) 100))
    (assert (= (small-align-8-get-m1 s) 200)))
  ;; Test with negative values
  (with-alien ((s (struct small-align-8)))
    (setf (slot s 'm0) -999)
    (setf (slot s 'm1) 888)
    (assert (= (small-align-8-get-m0 s) -999))
    (assert (= (small-align-8-get-m1 s) 888))))

;;; Runtime tests for small struct return
(with-test (:name :struct-by-value-small-align-8-return-runtime)
  ;; Test return from C function
  (let ((result (small-align-8-return 100 200)))
    (assert (= (slot result 'm0) 100))
    (assert (= (slot result 'm1) 200)))
  ;; Test with negative values
  (let ((result (small-align-8-return -111 222)))
    (assert (= (slot result 'm0) -111))
    (assert (= (slot result 'm1) 222)))
  ;; Test with zeros
  (let ((result (small-align-8-return 0 0)))
    (assert (= (slot result 'm0) 0))
    (assert (= (slot result 'm1) 0)))
  ;; Test identity (pass struct, get same struct back)
  (with-alien ((s (struct small-align-8)))
    (setf (slot s 'm0) 11111)
    (setf (slot s 'm1) 22222)
    (let ((result (small-align-8-identity s)))
      (assert (= (slot result 'm0) 11111))
      (assert (= (slot result 'm1) 22222)))))

;;; Large struct, alignment 8 (too big for registers, uses hidden pointer)
(define-alien-type nil
    (struct large-align-8
            (m0 (integer 64)) (m4 (integer 64)) (m8 (integer 64)) (m12 (integer 64))
            (m1 (integer 64)) (m5 (integer 64)) (m9 (integer 64)) (m13 (integer 64))
            (m2 (integer 64)) (m6 (integer 64)) (m10 (integer 64)) (m14 (integer 64))
            (m3 (integer 64)) (m7 (integer 64)) (m11 (integer 64)) (m15 (integer 64))))

(with-test (:name :struct-by-value-large-align-8-args)
    (macrolet
      ((def-large-align-8-get (i)
         (let ((lisp-name (sb-int:symbolicate "LARGE-ALIGN-8-GET-M" i)))
           `(define-alien-routine ,lisp-name (integer 64) (m (struct large-align-8)))))
       (defs-large-align-8-get ()
         "Test functions for each member"
         (let ((defs (loop for i upto 15 collect `(def-large-align-8-get ,i))))
           `(progn ,@defs))))
    (defs-large-align-8-get)
    (define-alien-routine large-align-8-mutate void (m (struct large-align-8))))
  #-(or x86-64 arm64)
  (macrolet
      ((def-large-align-8-get (i)
         (let ((lisp-name (sb-int:symbolicate "LARGE-ALIGN-8-GET-M" i)))
           `(assert-unimplemented
             (define-alien-routine ,lisp-name (integer 64) (m (struct large-align-8))))))
       (defs-large-align-8-get ()
         (let ((defs (loop for i upto 15 collect `(def-large-align-8-get ,i))))
           `(progn ,@defs))))
    (defs-large-align-8-get)
    (assert-unimplemented
     (define-alien-routine large-align-8-mutate void (m (struct large-align-8))))))


(define-alien-routine large-align-8-return (struct large-align-8)
  (v0 (integer 64)) (v1 (integer 64)))
(define-alien-routine large-align-8-identity (struct large-align-8) (m (struct large-align-8)))

;;; Runtime tests for large struct (uses hidden pointer for return)
;;; Large structs (>16 bytes) are returned via hidden pointer in x8 (ARM64) or
;;; implicit first arg (x86-64).
(with-test (:name :struct-by-value-large-align-8-return-runtime)
  ;; Test return from C function
  (let ((result (large-align-8-return 1000 2000)))
    (assert (= (slot result 'm0) 1000))
    (assert (= (slot result 'm1) 2000)))
  ;; Test identity (pass struct, get same struct back)
  (with-alien ((s (struct large-align-8)))
    (setf (slot s 'm0) 111)
    (setf (slot s 'm1) 222)
    (setf (slot s 'm2) 333)
    (setf (slot s 'm3) 444)
    (let ((result (large-align-8-identity s)))
      (assert (= (slot result 'm0) 111))
      (assert (= (slot result 'm1) 222))
      (assert (= (slot result 'm2) 333))
      (assert (= (slot result 'm3) 444)))))

;;; Floating-point struct tests (for SSE register handling on x86-64 and HFA on ARM64)
(define-alien-type nil (struct two-doubles (d0 double) (d1 double)))
(define-alien-type nil (struct two-floats (f0 single-float) (f1 single-float)))
(define-alien-type nil (struct int-double (i (integer 64)) (d double)))


(define-alien-routine two-doubles-return (struct two-doubles)
  (d0 double) (d1 double))
(define-alien-routine two-doubles-sum double (m (struct two-doubles)))
(define-alien-routine two-doubles-identity (struct two-doubles) (m (struct two-doubles)))

;;; Runtime tests for floating-point struct (passing as argument)
(with-test (:name :struct-by-value-two-doubles-runtime)
  (with-alien ((s (struct two-doubles)))
    (setf (slot s 'd0) 1.5d0)
    (setf (slot s 'd1) 2.5d0)
    (assert (= (two-doubles-sum s) 4.0d0))))

;;; Runtime tests for floating-point struct return
(with-test (:name :struct-by-value-two-doubles-return-runtime)
  ;; Test return from C function
  (let ((result (two-doubles-return 1.5d0 2.5d0)))
    (assert (= (slot result 'd0) 1.5d0))
    (assert (= (slot result 'd1) 2.5d0)))
  ;; Test with negative values
  (let ((result (two-doubles-return -3.14159d0 2.71828d0)))
    (assert (< (abs (- (slot result 'd0) -3.14159d0)) 1d-10))
    (assert (< (abs (- (slot result 'd1) 2.71828d0)) 1d-10)))
  ;; Test with zeros
  (let ((result (two-doubles-return 0.0d0 0.0d0)))
    (assert (= (slot result 'd0) 0.0d0))
    (assert (= (slot result 'd1) 0.0d0)))
  ;; Test identity (pass struct, get same struct back)
  (with-alien ((s (struct two-doubles)))
    (setf (slot s 'd0) 123.456d0)
    (setf (slot s 'd1) 789.012d0)
    (let ((result (two-doubles-identity s)))
      (assert (= (slot result 'd0) 123.456d0))
      (assert (= (slot result 'd1) 789.012d0)))))

(define-alien-routine two-floats-return (struct two-floats)
  (f0 single-float) (f1 single-float))
(define-alien-routine two-floats-sum single-float (m (struct two-floats)))
(define-alien-routine two-floats-identity (struct two-floats) (m (struct two-floats)))

;;; Runtime tests for single-float struct return
(with-test (:name :struct-by-value-two-floats-return-runtime)
  ;; Test return from C function
  (let ((result (two-floats-return 1.5 2.5)))
    (assert (= (slot result 'f0) 1.5))
    (assert (= (slot result 'f1) 2.5)))
  ;; Test with negative values
  (let ((result (two-floats-return -3.5 4.5)))
    (assert (= (slot result 'f0) -3.5))
    (assert (= (slot result 'f1) 4.5)))
  ;; Test identity (pass struct, get same struct back)
  (with-alien ((s (struct two-floats)))
    (setf (slot s 'f0) 11.11)
    (setf (slot s 'f1) 22.22)
    (let ((result (two-floats-identity s)))
      (assert (< (abs (- (slot result 'f0) 11.11)) 0.001))
      (assert (< (abs (- (slot result 'f1) 22.22)) 0.001)))))


(define-alien-routine int-double-return (struct int-double)
  (i (integer 64)) (d double))
(define-alien-routine int-double-get-int (integer 64) (m (struct int-double)))
(define-alien-routine int-double-get-double double (m (struct int-double)))
(define-alien-routine int-double-identity (struct int-double) (m (struct int-double)))

;;; Runtime tests for mixed int-double struct return
(with-test (:name :struct-by-value-int-double-return-runtime)
  ;; Test return from C function
  (let ((result (int-double-return 42 3.14159d0)))
    (assert (= (slot result 'i) 42))
    (assert (< (abs (- (slot result 'd) 3.14159d0)) 1d-10)))
  ;; Test with negative values
  (let ((result (int-double-return -999 -2.71828d0)))
    (assert (= (slot result 'i) -999))
    (assert (< (abs (- (slot result 'd) -2.71828d0)) 1d-10)))
  ;; Test identity (pass struct, get same struct back)
  (with-alien ((s (struct int-double)))
    (setf (slot s 'i) 12345)
    (setf (slot s 'd) 67.89d0)
    (let ((result (int-double-identity s)))
      (assert (= (slot result 'i) 12345))
      (assert (= (slot result 'd) 67.89d0)))))

;;; Medium struct (24 bytes) - tests boundary case (>16 bytes, uses hidden pointer)
(define-alien-type nil (struct medium-align-8 (m0 (integer 64)) (m1 (integer 64)) (m2 (integer 64))))

(define-alien-routine medium-align-8-return (struct medium-align-8)
  (v0 (integer 64)) (v1 (integer 64)) (v2 (integer 64)))
(define-alien-routine medium-align-8-get-m0 (integer 64) (m (struct medium-align-8)))
(define-alien-routine medium-align-8-get-m1 (integer 64) (m (struct medium-align-8)))
(define-alien-routine medium-align-8-get-m2 (integer 64) (m (struct medium-align-8)))
(define-alien-routine medium-align-8-identity (struct medium-align-8) (m (struct medium-align-8)))

;;; Runtime tests for medium struct (24 bytes - uses hidden pointer)
(with-test (:name :struct-by-value-medium-align-8-return-runtime)
  ;; Test return from C function
  (let ((result (medium-align-8-return 100 200 300)))
    (assert (= (slot result 'm0) 100))
    (assert (= (slot result 'm1) 200))
    (assert (= (slot result 'm2) 300)))
  ;; Test identity
  (with-alien ((s (struct medium-align-8)))
    (setf (slot s 'm0) 111)
    (setf (slot s 'm1) 222)
    (setf (slot s 'm2) 333)
    (let ((result (medium-align-8-identity s)))
      (assert (= (slot result 'm0) 111))
      (assert (= (slot result 'm1) 222))
      (assert (= (slot result 'm2) 333)))))

;;; Four floats struct - tests HFA (Homogeneous Floating-point Aggregate) on ARM64
;;; 16 bytes total, fits in 4 single-precision FP registers on ARM64
(define-alien-type nil (struct four-floats (f0 single-float) (f1 single-float)
                                           (f2 single-float) (f3 single-float)))

(define-alien-routine four-floats-return (struct four-floats)
  (f0 single-float) (f1 single-float) (f2 single-float) (f3 single-float))
(define-alien-routine four-floats-sum single-float (m (struct four-floats)))
(define-alien-routine four-floats-identity (struct four-floats) (m (struct four-floats)))

(with-test (:name :struct-by-value-four-floats-return-runtime)
  ;; Test return from C function
  (let ((result (four-floats-return 1.0 2.0 3.0 4.0)))
    (assert (= (slot result 'f0) 1.0))
    (assert (= (slot result 'f1) 2.0))
    (assert (= (slot result 'f2) 3.0))
    (assert (= (slot result 'f3) 4.0)))
  ;; Test sum (passing as argument)
  (with-alien ((s (struct four-floats)))
    (setf (slot s 'f0) 1.0)
    (setf (slot s 'f1) 2.0)
    (setf (slot s 'f2) 3.0)
    (setf (slot s 'f3) 4.0)
    (assert (= (four-floats-sum s) 10.0)))
  ;; Test identity
  (with-alien ((s (struct four-floats)))
    (setf (slot s 'f0) 1.5)
    (setf (slot s 'f1) 2.5)
    (setf (slot s 'f2) 3.5)
    (setf (slot s 'f3) 4.5)
    (let ((result (four-floats-identity s)))
      (assert (= (slot result 'f0) 1.5))
      (assert (= (slot result 'f1) 2.5))
      (assert (= (slot result 'f2) 3.5))
      (assert (= (slot result 'f3) 4.5)))))

;;; Three doubles struct - 24 bytes, HFA on ARM64 (fits in 3 double FP registers)
;;; But exceeds 16 bytes so may use memory return depending on ABI interpretation
(define-alien-type nil (struct three-doubles (d0 double) (d1 double) (d2 double)))

(define-alien-routine three-doubles-return (struct three-doubles)
  (d0 double) (d1 double) (d2 double))
(define-alien-routine three-doubles-sum double (m (struct three-doubles)))
(define-alien-routine three-doubles-identity (struct three-doubles) (m (struct three-doubles)))

;;; Runtime tests for three doubles (24 bytes - uses hidden pointer)
(with-test (:name :struct-by-value-three-doubles-return-runtime)
  ;; Test return from C function
  (let ((result (three-doubles-return 1.1d0 2.2d0 3.3d0)))
    (assert (< (abs (- (slot result 'd0) 1.1d0)) 1d-10))
    (assert (< (abs (- (slot result 'd1) 2.2d0)) 1d-10))
    (assert (< (abs (- (slot result 'd2) 3.3d0)) 1d-10)))
  ;; Test sum (passing as argument)
  (with-alien ((s (struct three-doubles)))
    (setf (slot s 'd0) 1.0d0)
    (setf (slot s 'd1) 2.0d0)
    (setf (slot s 'd2) 3.0d0)
    (assert (= (three-doubles-sum s) 6.0d0)))
  ;; Test identity
  (with-alien ((s (struct three-doubles)))
    (setf (slot s 'd0) 10.0d0)
    (setf (slot s 'd1) 20.0d0)
    (setf (slot s 'd2) 30.0d0)
    (let ((result (three-doubles-identity s)))
      (assert (= (slot result 'd0) 10.0d0))
      (assert (= (slot result 'd1) 20.0d0))
      (assert (= (slot result 'd2) 30.0d0)))))

;;; HFA with array of 4 floats - tests array-based HFA detection on ARM64
;;; This struct has a single field which is an array of 4 floats (16 bytes total)
;;; On ARM64, this should be detected as an HFA with 4 single-float members
(define-alien-type nil (struct float-array-4 (arr (array single-float 4))))

(define-alien-routine float-array-4-return (struct float-array-4)
  (f0 single-float) (f1 single-float) (f2 single-float) (f3 single-float))
(define-alien-routine float-array-4-sum single-float (m (struct float-array-4)))
(define-alien-routine float-array-4-identity (struct float-array-4) (m (struct float-array-4)))

(with-test (:name :struct-by-value-float-array-4-return-runtime)
  ;; Test return from C function
  (let ((result (float-array-4-return 1.0 2.0 3.0 4.0)))
    (assert (= (deref (slot result 'arr) 0) 1.0))
    (assert (= (deref (slot result 'arr) 1) 2.0))
    (assert (= (deref (slot result 'arr) 2) 3.0))
    (assert (= (deref (slot result 'arr) 3) 4.0)))
  ;; Test sum (passing as argument)
  (with-alien ((s (struct float-array-4)))
    (setf (deref (slot s 'arr) 0) 1.0)
    (setf (deref (slot s 'arr) 1) 2.0)
    (setf (deref (slot s 'arr) 2) 3.0)
    (setf (deref (slot s 'arr) 3) 4.0)
    (assert (= (float-array-4-sum s) 10.0)))
  ;; Test identity
  (with-alien ((s (struct float-array-4)))
    (setf (deref (slot s 'arr) 0) 1.5)
    (setf (deref (slot s 'arr) 1) 2.5)
    (setf (deref (slot s 'arr) 2) 3.5)
    (setf (deref (slot s 'arr) 3) 4.5)
    (let ((result (float-array-4-identity s)))
      (assert (= (deref (slot result 'arr) 0) 1.5))
      (assert (= (deref (slot result 'arr) 1) 2.5))
      (assert (= (deref (slot result 'arr) 2) 3.5))
      (assert (= (deref (slot result 'arr) 3) 4.5)))))

;;; HFA with array of 2 doubles - tests array-based HFA with doubles
;;; This struct has a single field which is an array of 2 doubles (16 bytes total)
(define-alien-type nil (struct double-array-2 (arr (array double 2))))

(define-alien-routine double-array-2-return (struct double-array-2)
  (d0 double) (d1 double))
(define-alien-routine double-array-2-sum double (m (struct double-array-2)))
(define-alien-routine double-array-2-identity (struct double-array-2) (m (struct double-array-2)))

(with-test (:name :struct-by-value-double-array-2-return-runtime)
  ;; Test return from C function
  (let ((result (double-array-2-return 1.5d0 2.5d0)))
    (assert (= (deref (slot result 'arr) 0) 1.5d0))
    (assert (= (deref (slot result 'arr) 1) 2.5d0)))
  ;; Test sum (passing as argument)
  (with-alien ((s (struct double-array-2)))
    (setf (deref (slot s 'arr) 0) 10.0d0)
    (setf (deref (slot s 'arr) 1) 20.0d0)
    (assert (= (double-array-2-sum s) 30.0d0)))
  ;; Test identity
  (with-alien ((s (struct double-array-2)))
    (setf (deref (slot s 'arr) 0) 100.0d0)
    (setf (deref (slot s 'arr) 1) 200.0d0)
    (let ((result (double-array-2-identity s)))
      (assert (= (deref (slot result 'arr) 0) 100.0d0))
      (assert (= (deref (slot result 'arr) 1) 200.0d0)))))

;;; HFA with array of 3 floats - tests odd-sized array HFA (12 bytes)
(define-alien-type nil (struct float-array-3 (arr (array single-float 3))))

(define-alien-routine float-array-3-return (struct float-array-3)
  (f0 single-float) (f1 single-float) (f2 single-float))
(define-alien-routine float-array-3-sum single-float (m (struct float-array-3)))
(define-alien-routine float-array-3-identity (struct float-array-3) (m (struct float-array-3)))

(with-test (:name :struct-by-value-float-array-3-return-runtime)
  ;; Test return from C function
  (let ((result (float-array-3-return 1.0 2.0 3.0)))
    (assert (= (deref (slot result 'arr) 0) 1.0))
    (assert (= (deref (slot result 'arr) 1) 2.0))
    (assert (= (deref (slot result 'arr) 2) 3.0)))
  ;; Test sum (passing as argument)
  (with-alien ((s (struct float-array-3)))
    (setf (deref (slot s 'arr) 0) 1.0)
    (setf (deref (slot s 'arr) 1) 2.0)
    (setf (deref (slot s 'arr) 2) 3.0)
    (assert (= (float-array-3-sum s) 6.0)))
  ;; Test identity
  (with-alien ((s (struct float-array-3)))
    (setf (deref (slot s 'arr) 0) 10.0)
    (setf (deref (slot s 'arr) 1) 20.0)
    (setf (deref (slot s 'arr) 2) 30.0)
    (let ((result (float-array-3-identity s)))
      (assert (= (deref (slot result 'arr) 0) 10.0))
      (assert (= (deref (slot result 'arr) 1) 20.0))
      (assert (= (deref (slot result 'arr) 2) 30.0)))))

;;;; Callback tests for struct-by-value parameters
;;;; These test receiving structs by value in Lisp callbacks called from C

;;; Define alien routines that call callbacks with struct parameters
(define-alien-routine call-with-small-struct (integer 64)
  (cb system-area-pointer) (v0 (integer 64)) (v1 (integer 64)))
(define-alien-routine call-with-large-struct (integer 64)
  (cb system-area-pointer)
  (v0 (integer 64)) (v1 (integer 64)) (v2 (integer 64)) (v3 (integer 64)))
(define-alien-routine call-with-two-structs (integer 64)
  (cb system-area-pointer)
  (a0 (integer 64)) (a1 (integer 64)) (b0 (integer 64)) (b1 (integer 64)))
(define-alien-routine call-with-float-struct double
  (cb system-area-pointer) (d0 double) (d1 double))

;;; Test callback with small struct parameter (16 bytes, passed in registers)
(with-test (:name :callback-struct-small)
  (with-alien-callable
      ((cb (integer 64) ((s (struct small-align-8)))
         (+ (slot s 'm0) (slot s 'm1))))
    (assert (= (call-with-small-struct (alien-sap cb) 10 20) 30))
    (assert (= (call-with-small-struct (alien-sap cb) -100 200) 100))
    (assert (= (call-with-small-struct (alien-sap cb) 0 0) 0))))

;;; Test callback with large struct parameter (128 bytes, passed on stack)
(with-test (:name :callback-struct-large)
  (with-alien-callable
      ((cb (integer 64) ((s (struct large-align-8)))
         (+ (slot s 'm0) (slot s 'm1) (slot s 'm2) (slot s 'm3))))
    (assert (= (call-with-large-struct (alien-sap cb) 1 2 3 4) 10))
    (assert (= (call-with-large-struct (alien-sap cb) 100 200 300 400) 1000))
    (assert (= (call-with-large-struct (alien-sap cb) -1 -2 -3 -4) -10))))

;;; Test callback with two struct parameters (like clang_visitChildren pattern)
(with-test (:name :callback-struct-two-structs)
  (with-alien-callable
      ((cb (integer 64) ((s1 (struct small-align-8))
                         (s2 (struct small-align-8)))
         (+ (slot s1 'm0) (slot s1 'm1)
            (slot s2 'm0) (slot s2 'm1))))
    (assert (= (call-with-two-structs (alien-sap cb) 1 2 3 4) 10))
    (assert (= (call-with-two-structs (alien-sap cb) 10 20 30 40) 100))))

;;; Test callback with float struct parameter (SSE registers)
(with-test (:name :callback-struct-floats)
  (with-alien-callable
      ((cb double ((s (struct two-doubles)))
         (+ (slot s 'd0) (slot s 'd1))))
    (assert (= (call-with-float-struct (alien-sap cb) 1.5d0 2.5d0) 4.0d0))
    (assert (= (call-with-float-struct (alien-sap cb) 100.0d0 200.0d0) 300.0d0))))

;;; Define alien routines that call callbacks returning structs
(define-alien-routine call-returning-small-struct (struct small-align-8)
  (cb system-area-pointer) (v0 (integer 64)) (v1 (integer 64)))
(define-alien-routine call-returning-double-struct (struct two-doubles)
  (cb system-area-pointer) (d0 double) (d1 double))
(define-alien-routine call-returning-medium-struct (struct medium-align-8)
  (cb system-area-pointer) (v0 (integer 64)) (v1 (integer 64)) (v2 (integer 64)))

;;; Test callback returning small struct (16 bytes, in registers)
(with-test (:name :callback-struct-return-small)
  (with-alien-callable
      ((cb (struct small-align-8) ((v0 (integer 64)) (v1 (integer 64)))
         (with-alien ((s (struct small-align-8)))
           (setf (slot s 'm0) v0)
           (setf (slot s 'm1) v1)
           s)))
    (let ((result (call-returning-small-struct (alien-sap cb) 100 200)))
      (assert (= (slot result 'm0) 100))
      (assert (= (slot result 'm1) 200)))
    (let ((result (call-returning-small-struct (alien-sap cb) -42 42)))
      (assert (= (slot result 'm0) -42))
      (assert (= (slot result 'm1) 42)))))

;;; Test callback returning struct with doubles (SSE registers)
(with-test (:name :callback-struct-return-doubles)
  (with-alien-callable
      ((cb (struct two-doubles) ((d0 double) (d1 double))
         (with-alien ((s (struct two-doubles)))
           (setf (slot s 'd0) d0)
           (setf (slot s 'd1) d1)
           s)))
    (let ((result (call-returning-double-struct (alien-sap cb) 1.5d0 2.5d0)))
      (assert (= (slot result 'd0) 1.5d0))
      (assert (= (slot result 'd1) 2.5d0)))
    (let ((result (call-returning-double-struct (alien-sap cb) -3.14d0 2.71d0)))
      (assert (< (abs (- (slot result 'd0) -3.14d0)) 1d-10))
      (assert (< (abs (- (slot result 'd1) 2.71d0)) 1d-10)))))

;;; Test callback returning large struct (24 bytes, via hidden pointer)
(with-test (:name :callback-struct-return-large
            :broken-on :x86-64)
  (with-alien-callable
      ((cb (struct medium-align-8) ((v0 (integer 64)) (v1 (integer 64)) (v2 (integer 64)))
         (with-alien ((s (struct medium-align-8)))
           (setf (slot s 'm0) v0)
           (setf (slot s 'm1) v1)
           (setf (slot s 'm2) v2)
           s)))
    (let ((result (call-returning-medium-struct (alien-sap cb) 111 222 333)))
      (assert (= (slot result 'm0) 111))
      (assert (= (slot result 'm1) 222))
      (assert (= (slot result 'm2) 333)))
    (let ((result (call-returning-medium-struct (alien-sap cb) -1 0 1)))
      (assert (= (slot result 'm0) -1))
      (assert (= (slot result 'm1) 0))
      (assert (= (slot result 'm2) 1)))))

;;;; Union-by-value tests

;;; Small union (8 bytes) - fits in one register
(define-alien-type nil (union small-union
                               (as-int (integer 64))
                               (as-double double)))

(define-alien-routine small-union-from-int (union small-union) (val (integer 64)))
(define-alien-routine small-union-from-double (union small-union) (val double))
(define-alien-routine small-union-get-int (integer 64) (u (union small-union)))
(define-alien-routine small-union-get-double double (u (union small-union)))
(define-alien-routine small-union-identity (union small-union) (u (union small-union)))

;;; Runtime tests for small union
(with-test (:name :union-by-value-small-runtime)
  ;; Test creating union from int and reading back
  (let ((result (small-union-from-int 42)))
    (assert (= (slot result 'as-int) 42)))
  ;; Test creating union from double and reading back
  (let ((result (small-union-from-double 3.14159d0)))
    (assert (< (abs (- (slot result 'as-double) 3.14159d0)) 1d-10)))
  ;; Test passing union as argument (as int)
  (with-alien ((u (union small-union)))
    (setf (slot u 'as-int) 12345)
    (assert (= (small-union-get-int u) 12345)))
  ;; Test passing union as argument (as double)
  (with-alien ((u (union small-union)))
    (setf (slot u 'as-double) 2.71828d0)
    (assert (< (abs (- (small-union-get-double u) 2.71828d0)) 1d-10)))
  ;; Test identity
  (with-alien ((u (union small-union)))
    (setf (slot u 'as-int) 999)
    (let ((result (small-union-identity u)))
      (assert (= (slot result 'as-int) 999)))))

;;; Medium union (16 bytes) - fits in two registers
(define-alien-type nil (union medium-union
                               (as-pair (struct medium-union-pair
                                                (lo (integer 64))
                                                (hi (integer 64))))
                               (as-doubles (struct medium-union-doubles
                                                   (d0 double)
                                                   (d1 double)))))

(define-alien-routine medium-union-from-pair (union medium-union)
  (lo (integer 64)) (hi (integer 64)))
(define-alien-routine medium-union-from-doubles (union medium-union)
  (d0 double) (d1 double))
(define-alien-routine medium-union-get-lo (integer 64) (u (union medium-union)))
(define-alien-routine medium-union-get-hi (integer 64) (u (union medium-union)))
(define-alien-routine medium-union-get-d0 double (u (union medium-union)))
(define-alien-routine medium-union-get-d1 double (u (union medium-union)))
(define-alien-routine medium-union-identity (union medium-union) (u (union medium-union)))

;;; Runtime tests for medium union
(with-test (:name :union-by-value-medium-runtime)
  ;; Test creating union from pair of ints
  (let ((result (medium-union-from-pair 100 200)))
    (assert (= (slot (slot result 'as-pair) 'lo) 100))
    (assert (= (slot (slot result 'as-pair) 'hi) 200)))
  ;; Test creating union from pair of doubles
  (let ((result (medium-union-from-doubles 1.5d0 2.5d0)))
    (assert (= (slot (slot result 'as-doubles) 'd0) 1.5d0))
    (assert (= (slot (slot result 'as-doubles) 'd1) 2.5d0)))
  ;; Test passing union as argument
  (with-alien ((u (union medium-union)))
    (setf (slot (slot u 'as-pair) 'lo) 111)
    (setf (slot (slot u 'as-pair) 'hi) 222)
    (assert (= (medium-union-get-lo u) 111))
    (assert (= (medium-union-get-hi u) 222))
    (let ((result (medium-union-identity u)))
      (assert (= (slot (slot result 'as-pair) 'lo) 111))
      (assert (= (slot (slot result 'as-pair) 'hi) 222)))))

;;; Large union (32 bytes) - uses hidden pointer for return
(define-alien-type nil (union large-union
                               (arr-int (array (integer 64) 4))
                               (arr-double (array double 4))))

(define-alien-routine large-union-from-ints (union large-union)
  (v0 (integer 64)) (v1 (integer 64)) (v2 (integer 64)) (v3 (integer 64)))
(define-alien-routine large-union-get-int (integer 64)
  (u (union large-union)) (index int))
(define-alien-routine large-union-get-double double
  (u (union large-union)) (index int))
(define-alien-routine large-union-identity (union large-union) (u (union large-union)))

;;; Runtime tests for large union
(with-test (:name :union-by-value-large-runtime)
  ;; Test creating union from ints
  (let ((result (large-union-from-ints 10 20 30 40)))
    (assert (= (deref (slot result 'arr-int) 0) 10))
    (assert (= (deref (slot result 'arr-int) 1) 20))
    (assert (= (deref (slot result 'arr-int) 2) 30))
    (assert (= (deref (slot result 'arr-int) 3) 40)))
  ;; Test passing union as argument
  (with-alien ((u (union large-union)))
    (setf (deref (slot u 'arr-int) 0) 100)
    (setf (deref (slot u 'arr-int) 1) 200)
    (setf (deref (slot u 'arr-int) 2) 300)
    (setf (deref (slot u 'arr-int) 3) 400)
    (assert (= (large-union-get-int u 0) 100))
    (assert (= (large-union-get-int u 1) 200))
    (assert (= (large-union-get-int u 2) 300))
    (assert (= (large-union-get-int u 3) 400)))
  ;; Test identity
  (with-alien ((u (union large-union)))
    (setf (deref (slot u 'arr-int) 0) 1)
    (setf (deref (slot u 'arr-int) 1) 2)
    (setf (deref (slot u 'arr-int) 2) 3)
    (setf (deref (slot u 'arr-int) 3) 4)
    (let ((result (large-union-identity u)))
      (assert (= (deref (slot result 'arr-int) 0) 1))
      (assert (= (deref (slot result 'arr-int) 1) 2))
      (assert (= (deref (slot result 'arr-int) 2) 3))
      (assert (= (deref (slot result 'arr-int) 3) 4)))))

;;; Callback tests for unions

(define-alien-routine call-with-small-union (integer 64)
  (cb system-area-pointer) (val (integer 64)))
(define-alien-routine call-returning-small-union (union small-union)
  (cb system-area-pointer) (val (integer 64)))

;;; Test callback with union parameter
(with-test (:name :callback-union-parameter)
  (with-alien-callable
      ((cb (integer 64) ((u (union small-union)))
         (slot u 'as-int)))
    (assert (= (call-with-small-union (alien-sap cb) 42) 42))
    (assert (= (call-with-small-union (alien-sap cb) -999) -999))))

;;; Test callback returning union
(with-test (:name :callback-union-return)
  (with-alien-callable
      ((cb (union small-union) ((val (integer 64)))
         (with-alien ((u (union small-union)))
           (setf (slot u 'as-int) val)
           u)))
    (let ((result (call-returning-small-union (alien-sap cb) 12345)))
      (assert (= (slot result 'as-int) 12345)))
    (let ((result (call-returning-small-union (alien-sap cb) -54321)))
      (assert (= (slot result 'as-int) -54321)))))

;;; Clean up
#-win32 (ignore-errors (delete-file *soname*))
