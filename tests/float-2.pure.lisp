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

;;; Hannu Rummukainen reported a CMU CL bug on cmucl-imp@cons.org 26
;;; Jun 2000. This is the test case for it.
;;;
;;; The bug was listed as "39: .. Probably the same bug exists in
;;; SBCL" for a while until Martin Atzmueller showed that it's not
;;; present after all, presumably because the bug was introduced into
;;; CMU CL after the fork. But we'll test for it anyway, in case
;;; e.g. someone inadvertently ports the bad code.
(defun point39 (x y)
  (make-array 2
              :element-type 'double-float
              :initial-contents (list x y)))

(declaim (inline point39-x point39-y))
(defun point39-x (p)
  (declare (type (simple-array double-float (2)) p))
  (aref p 0))
(defun point39-y (p)
  (declare (type (simple-array double-float (2)) p))
  (aref p 1))
(defun order39 (points)
  (sort points  (lambda (p1 p2)
                  (let* ((y1 (point39-y p1))
                         (y2 (point39-y p2)))
                    (if (= y1 y2)
                        (< (point39-x p1)
                           (point39-x p2))
                        (< y1 y2))))))
(defun test39 ()
  (order39 (make-array 4
                       :initial-contents (list (point39 0.0d0 0.0d0)
                                               (point39 1.0d0 1.0d0)
                                               (point39 2.0d0 2.0d0)
                                               (point39 3.0d0 3.0d0)))))
(assert (equalp (test39)
                #(#(0.0d0 0.0d0)
                  #(1.0d0 1.0d0)
                  #(2.0d0 2.0d0)
                  #(3.0d0 3.0d0))))

(defun complex-double-float-ppc (x y)
  (declare (type (complex double-float) x y))
  (declare (optimize speed))
  (+ x y))
(compile 'complex-double-float-ppc)
(assert (= (complex-double-float-ppc #c(0.0d0 1.0d0) #c(2.0d0 3.0d0))
           #c(2.0d0 4.0d0)))

(defun single-float-ppc (x)
  (declare (type (signed-byte 32) x) (optimize speed))
  (float x 1f0))
(compile 'single-float-ppc)
(assert (= (single-float-ppc -30) -30f0))

;;; constant-folding irrational functions
(declaim (inline df))
(defun df (x)
  ;; do not remove the ECASE here: the bug this checks for indeed
  ;; depended on this configuration
  (ecase x (1 least-positive-double-float)))
(macrolet ((test (fun)
             (let ((name (intern (format nil "TEST-CONSTANT-~A" fun))))
               `(progn
                  (defun ,name () (,fun (df 1)))
                  (,name)))))
  (test sqrt)
  (test log)
  (test sin)
  (test cos)
  (test tan)
  (test asin)
  (test acos)
  (test atan)
  (test sinh)
  (test cosh)
  (test tanh)
  (test asinh)
  (test acosh)
  (test atanh)
  (test exp))

;;; Broken move-arg-double-float for non-rsp frame pointers on x86-64
(defun test (y)
  (declare (optimize speed))
  (multiple-value-bind (x)
      (labels ((aux (x)
                 (declare (double-float x))
                 (etypecase y
                   (double-float
                    nil)
                   (fixnum
                    (aux x))
                   (complex
                    (format t "y=~s~%" y)))
                 (values x)))
        (aux 2.0d0))
    x))

(assert (= (test 1.0d0) 2.0d0))

(deftype myarraytype (&optional (length '*))
  `(simple-array double-float (,length)))
(defun new-pu-label-from-pu-labels (array)
  (setf (aref (the myarraytype array) 0)
        sb-ext:double-float-positive-infinity))

(with-test (:name :bug-407a
            :fails-on :no-float-traps)
  (assert-error
   (loop for n from (expt 2 1024) upto (+ 10 (expt 2 1024))
         do (coerce n 'single-float))
   floating-point-overflow))

(with-test (:name :bug-407b
            :fails-on :no-float-traps)
  (assert-error
      (loop for n from (expt 2 1024) upto (+ 10 (expt 2 1024))
            do (format nil "~E~%" n))
      floating-point-overflow))

(with-test (:name :bignum-double-float-overflow
            :fails-on :no-float-traps)
  (loop for n from 1024 to 1030
        do (assert-error (coerce (opaque-identity (expt 2 n)) 'double-float) floating-point-overflow)
           (assert-error (coerce (opaque-identity (- (expt 2 n))) 'double-float) floating-point-overflow)))

;; 1.0.29.44 introduces a ton of changes for complex floats
;; on x86-64. Huge test of doom to help catch weird corner
;; cases.
;; Abuse the framework to also test some float arithmetic
;; changes wrt constant arguments in 1.0.29.54.
(defmacro def-compute (name real-type
                       &optional (complex-type `(complex ,real-type)))
  `(defun ,name (x y r)
     (declare (type ,complex-type x y)
              (type ,real-type r))
     (flet ((reflections (x)
              (values x
                      (conjugate x)
                      (complex (- (realpart x)) (imagpart x))
                      (- x)))
            (compute (x y r)
              (declare (type ,complex-type x y)
                       (type ,real-type r))
              (list (1+ x) (* 2 x) (/ x 2) (= 1 x)
                    (+ x y) (+ r x) (+ x r)
                    (- x y) (- r x) (- x r)
                    (* x y) (* x r) (* r x)
                    (unless (zerop y)
                      (/ x y))
                    (unless (zerop r)
                      (/ x r))
                    (unless (zerop x)
                      (/ r x))
                    (conjugate x) (conjugate r)
                    (abs r) (- r) (= 1 r)
                    (- x) (1+ r) (* 2 r) (/ r 2)
                    (complex r) (complex r r) (complex 0 r)
                    (= x y) (= r x) (= y r) (= x (complex 0 r))
                    (= r (realpart x)) (= (realpart x) r)
                    (> r (realpart x)) (< r (realpart x))
                    (> (realpart x) r) (< (realpart x) r)
                    (eql x y) (eql x (complex r)) (eql y (complex r))
                    (eql x (complex r r)) (eql y (complex 0 r))
                    (eql r (realpart x)) (eql (realpart x) r))))
       (declare (inline reflections))
       (multiple-value-bind (x1 x2 x3 x4) (reflections x)
         (multiple-value-bind (y1 y2 y3 y4) (reflections y)
           #.(let ((form '(list)))
               (dolist (x '(x1 x2 x3 x4) (reverse form))
                 (dolist (y '(y1 y2 y3 y4))
                   (push `(list ,x ,y r
                                (append (compute ,x ,y r)
                                        (compute ,x ,y (- r))))
                         form)))))))))

(def-compute compute-number real number)
(def-compute compute-single single-float)
(def-compute compute-double double-float)


(with-test (:name :complex-float)
  (labels ((equal-enough (x y)
             (cond ((eql x y))
                   ((or (complexp x)
                        (complexp y))
                    (or (eql (coerce x '(complex double-float))
                             (coerce y '(complex double-float)))
                        (and (equal-enough (realpart x) (realpart y))
                             (equal-enough (imagpart x) (imagpart y)))))
                   ((numberp x)
                    (or (eql (coerce x 'double-float) (coerce y 'double-float))
                        (< (abs (- x y))  1d-5))))))
    (let* ((reals     '(0 1 2))
           (complexes '#.(let ((reals '(0 1 2))
                               (cpx   '()))
                           (dolist (x reals (nreverse cpx))
                             (dolist (y reals)
                               (push (complex x y) cpx))))))
      (declare (notinline every))
      (dolist (r reals)
        (dolist (x complexes)
          (dolist (y complexes)
            (let ((value  (compute-number x y r))
                  (single (compute-single (coerce x '(complex single-float))
                                          (coerce y '(complex single-float))
                                          (coerce r 'single-float)))
                  (double (compute-double (coerce x '(complex double-float))
                                          (coerce y '(complex double-float))
                                          (coerce r 'double-float))))

              (mapc (lambda (pos ref single double)
                      (declare (ignorable pos))
                      (mapc (lambda (ref single double)
                              (unless
                                  (or (and (equal-enough ref single)
                                           (equal-enough ref double))
                                      (and (not (numberp single)) ;; -ve 0s
                                           (equal-enough single double)))
                                (error "r: ~a, x: ~a, y: ~a~%ref: ~a, single: ~a, double ~a"
                                       r x y
                                       ref single double)))
                            (fourth ref) (fourth single) (fourth double)))
                    '((0 0) (0 1) (0 2) (0 3)
                      (1 0) (1 1) (1 2) (1 3)
                      (2 0) (2 1) (2 2) (2 3)
                      (3 0) (3 1) (3 2) (3 3))
                    value single double))))))))

;; To test the range reduction of trigonometric functions we need a much
;; more accurate approximation of pi than CL:PI is. Calculating this is
;; more fun than copy-pasting a constant and Gauss-Legendre converges
;; extremely fast.
(defun pi-gauss-legendre (n-bits)
  "Return a rational approximation to pi using the Gauss-Legendre
algorithm. The calculations are done with integers, representing
multiples of (expt 2 (- N-BITS)), and the result is an integral multiple
of this number. The result is accurate to a few less than N-BITS many
fractional bits."
  (let ((a (ash 1 n-bits))                     ; scaled 1
        (b (isqrt (expt 2 (1- (* n-bits 2))))) ; scaled (sqrt 1/2)
        (c (ash 1 (- n-bits 2)))               ; scaled 1/4
        (d 0))
    (loop
      (when (<= (- a b) 1)
        (return))
      (let ((a1 (ash (+ a b) -1)))
        (psetf a a1
               b (isqrt (* a b))
               c (- c (ash (expt (- a a1) 2) (- d n-bits)))
               d (1+ d))))
    (/ (round (expt (+ a b) 2) (* 4 c))
       (ash 1 n-bits))))

;; Test that the range reduction of trigonometric functions is done
;; with a sufficiently accurate value of pi that the reduced argument
;; is correct to nearly double-float precision even for arguments of
;; very large absolute value.
(with-test (:name (:range-reduction :precise-pi))
  (let ((rational-pi-half (/ (pi-gauss-legendre 2200) 2)))
    (labels ((round-pi-half (x)
               "Return two values as if (ROUND X (/ PI 2)) was called
                but where PI is precise enough that for all possible
                double-float arguments the quotient is exact and the
                remainder is exact to double-float precision."
               (declare (type double-float x))
               (multiple-value-bind (q r)
                   (round (rational x) rational-pi-half)
                 (values q (coerce r 'double-float))))
             (expected-val (op x)
               "Calculate (OP X) precisely by shifting the argument by
                an integral multiple of (/ PI 2) into the range from
                (- (/ PI 4)) to (/ PI 4) and applying the phase-shift
                formulas for the trigonometric functions. PI here is
                precise enough that the result is exact to double-float
                precision."
               (labels ((precise-val (op q r)
                          (ecase op
                            (sin (let ((x (if (zerop (mod q 2))
                                              (sin r)
                                              (cos r))))
                                   (if (<= (mod q 4) 1)
                                       x
                                       (- x))))
                            (cos (precise-val 'sin (1+ q) r))
                            (tan (if (zerop (mod q 2))
                                     (tan r)
                                     (/ (- (tan r))))))))
                 (multiple-value-bind (q r)
                     (round-pi-half x)
                   (precise-val op q r))))
             (test (op x)
               (let ((actual (funcall op x))
                     (expected (expected-val op x)))
                 ;; Some of the test values are chosen to lie very near
                 ;; to an integral multiple of pi/2 (within a distance of
                 ;; between 1d-11 and 1d-8), making the absolute value of
                 ;; their sine or cosine this small, too. The absolute
                 ;; value of the tangent is then either similarly small or
                 ;; as large as the reciprocal of this value. Therefore we
                 ;; measure relative instead of absolute error.
                 (unless (or (= actual expected 0)
                             (and (= (signum actual) (signum expected))
                                  (< (abs (/ (- actual expected)
                                             (+ actual expected)))
                                     (* 8 double-float-epsilon))))
                   (error "Inaccurate result for ~a: expected ~a, got ~a"
                          (list op x) expected actual)))))
      (dolist (op '(sin cos tan))
        (dolist (val `(,(coerce most-positive-fixnum 'double-float)
                       ,@(loop for v = most-positive-double-float
                               ;; EXPT calls %POW which directly calls the libm pow() function.
                               ;; Some libm's might internally cause an *expected* overflow on
                               ;; certain inputs. So instead of computing an answer for
                               ;; (EXPT 1.7976931348623157d308 4/5) it would trap.
                               then (sb-int:with-float-traps-masked (:overflow) (expt v 4/5))
                               while (> v (expt 2 50))
                               collect v)
                       ;; The following values cover all eight combinations
                       ;; of values slightly below or above integral
                       ;; multiples of pi/2 with the integral factor
                       ;; congruent to 0, 1, 2 or 3 modulo 4.
                       5.526916451564098d71
                       4.913896894631919d229
                       7.60175752894437d69
                       3.8335637324151093d42
                       1.8178427396473695d155
                       9.41634760758887d89
                       4.2766818550391727d188
                       1.635888515419299d28))
          (test op val))))))

(with-test (:name :truncate-bignum-type-derivation)
  (checked-compile `(lambda (x)
                      (declare ((or (integer 1208925819614629174706175)
                                    (integer * -1208925819614629174706175))
                                x))
                      (truncate (float x 4d0))))
  (checked-compile `(lambda (x)
                      (declare ((or (integer 1208925819614629174706175)
                                    (integer * -1208925819614629174706175))
                                x))
                      (truncate (float x 1d0) 4d0))))

(with-test (:name :bignum-float-compare)
  (flet ((test (integer)
           (assert (= (float integer 1d0)
                      (truncate (float integer 1d0))))
           (assert (= (float integer)
                      (truncate (float integer))))))
    (loop for i from 80 to 100 by 4
          do (test (expt 2 i))
             (test (- (expt 2 i)))
             (test (1+ (expt 2 i)))
             (test (- (expt 2 i))))))

(with-test (:name :scale-float-unboxed)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (declare (single-float x))
         (locally (declare (optimize (space 0)))
           (scale-float x 1)))
    ((1.0) 2.0)))

(with-test (:name :complex-division)
  (checked-compile-and-assert
   ()
   `(lambda (a b c d)
      (declare ((complex double-float) b)
               (double-float a))
      (= (/ a b)
         (/ c d)))
   ((1.0d0 #C(10000.1d0 1.3d0) 1.0d0 #C(10000.1d0 1.3d0)) t))
  (checked-compile-and-assert
   ()
   `(lambda (a b c d)
      (declare ((complex double-float) a b))
      (= (/ a b)
         (/ c d)))
   ((#C(1.0d0 0.0d0) #C(10000.1d0 1.3d0) #C(1.0d0 0.0d0) #C(10000.1d0 1.3d0)) t))
  #-x86
  (checked-compile-and-assert
   ()
   `(lambda (a b c d)
      (declare ((complex single-float) b)
               (single-float a))
      (= (/ a b)
         (/ c d)))
   ((1.0f0 #C(10000.1f0 1.3f0) 1.0f0 #C(10000.1f0 1.3f0)) t))
  #-x86
  (checked-compile-and-assert
   ()
   `(lambda (a b c d)
      (declare ((complex single-float) a b))
      (= (/ a b)
         (/ c d)))
   ((#C(1.0f0 0.0f0) #C(10000.1f0 1.3f0) #C(1.0f0 0.0f0) #C(10000.1f0 1.3f0)) t)))

(with-test (:name :rational-derive-type-nan)
  (checked-compile `(lambda () (rationalize #.sb-ext:single-float-positive-infinity))
                   :allow-style-warnings t)
  (checked-compile `(lambda () (rational #.sb-ext:single-float-positive-infinity))
                   :allow-style-warnings t))

;; Not all libm implementations of log() raise SIGFPE, which is actually valid,
;; because when a math function specification speaks of "exceptions" it is
;; technically referring to a _language's_ exception mechanism, and not POSIX
;; signals necessarily. SBCL hooks into the signal which might not be delivered.
;; And also note that "man log" on Linux has two different ways
;; of expressing what could happen at the edge case:
;; 1) If x is zero, then a pole error occurs, and the functions return
;;    -HUGE_VAL, -HUGE_VALF, or -HUGE_VALL, respectively.
;; 2) The following errors *can* occur: [not "SHALL" occur]
;;    Pole error: x is zero.  errno is set to ERANGE.  A divide-by-zero
;;    floating-point exception (FE_DIVBYZERO) is raised
;; So those are both valid, and they have different effects on type derivation
;; (which is perhaps unfortunate)
(with-test (:name :log-derive-type-at-pole)
  (unless (eql (ignore-errors (sb-kernel:%log2 0d0))
               #.double-float-negative-infinity)
    (assert-type (lambda (y)
                   (declare (integer y))
                   (log y 2.0d0))
                 (or (double-float 0d0) (complex double-float)))))

(with-test (:name :log-derive-type)
  (assert-type (lambda (y)
                 (declare (double-float y))
                 (log y 2.0d0))
               (or double-float (complex double-float)))
  (assert-type (lambda (d)
                 (declare ((double-float 0d0) d))
                 (when (< d 1.0d0)
                   (log d)))
               (or null (double-float * 0.0d0)))
  (assert-type (lambda (d)
                 (declare ((double-float 0d0) d))
                 (when (< d 1.0d0)
                   (log d 2.0d0)))
               (or null (double-float * 0.0d0)))
  (assert-type (lambda (d)
                 (log -5 d))
               (or (complex single-float) (complex double-float) (member 0.0 0.0d0)))
  (assert-type (lambda (d)
                 (declare (double-float d))
                 (log -5 d))
               (or (complex double-float) (member 0.0d0)))
  (assert-type (lambda (d)
                 (declare ((double-float 10d0) d))
                 (log -5 d))
               (complex double-float))
  (assert-type (lambda (d)
                 (declare (complex d))
                 (log -5 d))
               (or (member 0.0d0 0.0) (complex double-float) (complex single-float)))
  (assert-type (lambda (d)
                 (declare ((complex double-float) d))
                 (log -5 d))
               (or (member 0.0d0) (complex double-float)))
  (assert-type (lambda (d)
                 (declare ((complex single-float) d))
                 (log -5 d))
               (or (member 0.0) (complex single-float)))
  (assert-type (lambda (x y)
                 (declare ((integer * -1) y))
                 (log 96 (if x
                             y
                             (expt 3434484076828220102 -16))))
               (or (complex single-float)
                   (eql #.(log 96 (expt 3434484076828220102 -16)))))
  (assert-type (lambda (v)
                 (log 3532504320689493448
                      (if v
                          7384646045169235520
                          51)))
               (or (eql #.(log 3532504320689493448 7384646045169235520))
                   (eql #.(log 3532504320689493448 51))))
  (assert-type (lambda (v)
                 (LOG 1.9775647e18
                      (IF v
                          12
                          -881536083005615600)))
               (or (eql #.(log 1.9775647e18 12))
                   (complex single-float))))

(with-test (:name :log-base-zero)
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (declare (single-float a b))
         (log a b))
    ((-1.0 0.0) 0.0)
    ((-1.0 -0.0) 0.0))
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (declare (double-float a b))
         (log a b))
    ((-1d0 0d0) 0d0)
    ((-1d0 -0d0) 0d0))
  (checked-compile-and-assert
      ()
      `(lambda (f)
         (declare (single-float f))
         (the real (log -3.7739912e17 f)))
    ((0.0) 0.0))
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (declare (single-float a b))
         (the real (log a b)))
    ((-3.0 0.0) 0.0)))

(with-test (:name (ftruncate :minus-zeros :one-arg))
  (flet ((f (x) (declare (notinline ftruncate)) (ftruncate x)))
    (declare (notinline f))
    (assert (equal (multiple-value-list (f 0)) '(0.0 0)))
    (assert (equal (multiple-value-list (f 0.0)) '(0.0 0.0)))
    (assert (equal (multiple-value-list (f -0.0)) '(-0.0 0.0)))
    (assert (equal (multiple-value-list (f 0.5)) '(0.0 0.5)))
    (assert (equal (multiple-value-list (f 1/2)) '(0.0 1/2)))
    (assert (equal (multiple-value-list (f -0.5)) '(-0.0 -0.5)))
    (assert (equal (multiple-value-list (f -1/2)) '(-0.0 -1/2)))))

(with-test (:name (ffloor :minus-zeros :one-arg))
  (flet ((f (x) (declare (notinline ffloor)) (ffloor x)))
    (declare (notinline f))
    (assert (equal (multiple-value-list (f 0)) '(0.0 0)))
    (assert (equal (multiple-value-list (f 0.0)) '(0.0 0.0)))
    (assert (equal (multiple-value-list (f -0.0)) '(-0.0 0.0)))
    (assert (equal (multiple-value-list (f 0.5)) '(0.0 0.5)))
    (assert (equal (multiple-value-list (f 1/2)) '(0.0 1/2)))
    (assert (equal (multiple-value-list (f -0.5)) '(-1.0 0.5)))
    (assert (equal (multiple-value-list (f -1/2)) '(-1.0 1/2)))))

(with-test (:name (fceiling :minus-zeros :one-arg))
  (flet ((f (x) (declare (notinline fceiling)) (fceiling x)))
    (declare (notinline f))
    (assert (equal (multiple-value-list (f 0)) '(0.0 0)))
    (assert (equal (multiple-value-list (f 0.0)) '(0.0 0.0)))
    (assert (equal (multiple-value-list (f -0.0)) '(-0.0 0.0)))
    (assert (equal (multiple-value-list (f 0.5)) '(1.0 -0.5)))
    (assert (equal (multiple-value-list (f 1/2)) '(1.0 -1/2)))
    (assert (equal (multiple-value-list (f -0.5)) '(-0.0 -0.5)))
    (assert (equal (multiple-value-list (f -1/2)) '(-0.0 -1/2)))))

(with-test (:name (fround :minus-zeros :one-arg))
  (flet ((f (x) (declare (notinline fround)) (fround x)))
    (declare (notinline f))
    (assert (equal (multiple-value-list (f 0)) '(0.0 0)))
    (assert (equal (multiple-value-list (f 0.0)) '(0.0 0.0)))
    (assert (equal (multiple-value-list (f -0.0)) '(-0.0 0.0)))
    (assert (equal (multiple-value-list (f 0.5)) '(0.0 0.5)))
    (assert (equal (multiple-value-list (f 1/2)) '(0.0 1/2)))
    (assert (equal (multiple-value-list (f -0.5)) '(-0.0 -0.5)))
    (assert (equal (multiple-value-list (f -1/2)) '(-0.0 -1/2)))))

(with-test (:name (ftruncate :minus-zeros :two-arg))
  (flet ((f (x y) (declare (notinline ftruncate)) (ftruncate x y)))
    (declare (notinline f))
    (assert (equal (multiple-value-list (f 0 1)) '(0.0 0)))
    (assert (equal (multiple-value-list (f 0 -1)) '(-0.0 0)))
    (assert (equal (multiple-value-list (f 0.0 1)) '(0.0 0.0)))
    (assert (equal (multiple-value-list (f 0.0 -1)) '(-0.0 0.0)))
    (assert (equal (multiple-value-list (f -0.0 1)) '(-0.0 0.0)))
    (assert (equal (multiple-value-list (f -0.0 -1)) '(0.0 0.0)))
    (assert (equal (multiple-value-list (f 0.5 1)) '(0.0 0.5)))
    (assert (equal (multiple-value-list (f 0.5 -1)) '(-0.0 0.5)))
    (assert (equal (multiple-value-list (f 1/2 1)) '(0.0 1/2)))
    (assert (equal (multiple-value-list (f 1/2 -1)) '(-0.0 1/2)))
    (assert (equal (multiple-value-list (f -0.5 1)) '(-0.0 -0.5)))
    (assert (equal (multiple-value-list (f -0.5 -1)) '(0.0 -0.5)))
    (assert (equal (multiple-value-list (f -1/2 1)) '(-0.0 -1/2)))
    (assert (equal (multiple-value-list (f -1/2 -1)) '(0.0 -1/2)))))

(with-test (:name (ffloor :minus-zeros :two-arg))
  (flet ((f (x y) (declare (notinline ffloor)) (ffloor x y)))
    (declare (notinline f))
    (assert (equal (multiple-value-list (f 0 1)) '(0.0 0)))
    (assert (equal (multiple-value-list (f 0 -1)) '(-0.0 0)))
    (assert (equal (multiple-value-list (f 0.0 1)) '(0.0 0.0)))
    (assert (equal (multiple-value-list (f 0.0 -1)) '(-0.0 0.0)))
    (assert (equal (multiple-value-list (f -0.0 1)) '(-0.0 0.0)))
    (assert (equal (multiple-value-list (f -0.0 -1)) '(0.0 0.0)))
    (assert (equal (multiple-value-list (f 0.5 1)) '(0.0 0.5)))
    (assert (equal (multiple-value-list (f 0.5 -1)) '(-1.0 -0.5)))
    (assert (equal (multiple-value-list (f 1/2 1)) '(0.0 1/2)))
    (assert (equal (multiple-value-list (f 1/2 -1)) '(-1.0 -1/2)))
    (assert (equal (multiple-value-list (f -0.5 1)) '(-1.0 0.5)))
    (assert (equal (multiple-value-list (f -0.5 -1)) '(0.0 -0.5)))
    (assert (equal (multiple-value-list (f -1/2 1)) '(-1.0 1/2)))
    (assert (equal (multiple-value-list (f -1/2 -1)) '(0.0 -1/2)))))

(with-test (:name (fceiling :minus-zeros :two-arg))
  (flet ((f (x y) (declare (notinline fceiling)) (fceiling x y)))
    (declare (notinline f))
    (assert (equal (multiple-value-list (f 0 1)) '(0.0 0)))
    (assert (equal (multiple-value-list (f 0 -1)) '(-0.0 0)))
    (assert (equal (multiple-value-list (f 0.0 1)) '(0.0 0.0)))
    (assert (equal (multiple-value-list (f 0.0 -1)) '(-0.0 0.0)))
    (assert (equal (multiple-value-list (f -0.0 1)) '(-0.0 0.0)))
    (assert (equal (multiple-value-list (f -0.0 -1)) '(0.0 0.0)))
    (assert (equal (multiple-value-list (f 0.5 1)) '(1.0 -0.5)))
    (assert (equal (multiple-value-list (f 0.5 -1)) '(-0.0 0.5)))
    (assert (equal (multiple-value-list (f 1/2 1)) '(1.0 -1/2)))
    (assert (equal (multiple-value-list (f 1/2 -1)) '(-0.0 1/2)))
    (assert (equal (multiple-value-list (f -0.5 1)) '(-0.0 -0.5)))
    (assert (equal (multiple-value-list (f -0.5 -1)) '(1.0 0.5)))
    (assert (equal (multiple-value-list (f -1/2 1)) '(-0.0 -1/2)))
    (assert (equal (multiple-value-list (f -1/2 -1)) '(1.0 1/2)))))

(with-test (:name (fround :minus-zeros :two-arg))
  (flet ((f (x y) (declare (notinline fround)) (fround x y)))
    (declare (notinline f))
    (assert (equal (multiple-value-list (f 0 1)) '(0.0 0)))
    (assert (equal (multiple-value-list (f 0 -1)) '(-0.0 0)))
    (assert (equal (multiple-value-list (f 0.0 1)) '(0.0 0.0)))
    (assert (equal (multiple-value-list (f 0.0 -1)) '(-0.0 0.0)))
    (assert (equal (multiple-value-list (f -0.0 1)) '(-0.0 0.0)))
    (assert (equal (multiple-value-list (f -0.0 -1)) '(0.0 0.0)))
    (assert (equal (multiple-value-list (f 0.5 1)) '(0.0 0.5)))
    (assert (equal (multiple-value-list (f 0.5 -1)) '(-0.0 0.5)))
    (assert (equal (multiple-value-list (f 1/2 1)) '(0.0 1/2)))
    (assert (equal (multiple-value-list (f 1/2 -1)) '(-0.0 1/2)))
    (assert (equal (multiple-value-list (f -0.5 1)) '(-0.0 -0.5)))
    (assert (equal (multiple-value-list (f -0.5 -1)) '(0.0 -0.5)))
    (assert (equal (multiple-value-list (f -1/2 1)) '(-0.0 -1/2)))
    (assert (equal (multiple-value-list (f -1/2 -1)) '(0.0 -1/2)))))

(with-test (:name (ftruncate double-float))
  (flet ((f (x y)
           (declare (type double-float x))
           (let ((ev (eval `(ftruncate ,x)))
                 (co (ftruncate x)))
             (assert (eql ev y))
             (assert (eql co y)))))
     (f 0d0 0d0)
     (f -0d0 -0d0)
     (f 0.25d0 0d0)
     (f 0.49d0 0d0)
     (f 0.5d0 0d0)
     (f 0.51d0 0d0)
     (f 0.75d0 0d0)
     (f -0.25d0 -0d0)
     (f -0.49d0 -0d0)
     (f -0.5d0 -0d0)
     (f -0.51d0 -0d0)
     (f -0.75d0 -0d0)
     (f 1d0 1d0)
     (f 1.5d0 1d0)
     (f -1d0 -1d0)
     (f -1.5d0 -1d0)
     (f 1.0000000000000001d15 1d15)
     (f -1.0000000000000001d15 -1d15)
     (f 1d16 1d16)
     (f -1d16 -1d16)))

(with-test (:name (fround double-float))
  (flet ((f (x y)
           (declare (type double-float x))
           (let ((ev (eval `(fround ,x)))
                 (co (fround x)))
             (assert (eql ev y))
             (assert (eql co y)))))
    (f 0d0 0d0)
    (f -0d0 -0d0)
    (f 0.25d0 0d0)
    (f 0.49d0 0d0)
    (f 0.5d0 0d0)
    (f 0.51d0 1d0)
    (f 0.75d0 1d0)
    (f -0.25d0 -0d0)
    (f -0.49d0 -0d0)
    (f -0.5d0 -0d0)
    (f -0.51d0 -1d0)
    (f -0.75d0 -1d0)
    (f 1d0 1d0)
    (f 1.49d0 1d0)
    (f 1.5d0 2d0)
    (f 1.51d0 2d0)
    (f -1d0 -1d0)
    (f -1.49d0 -1d0)
    (f -1.5d0 -2d0)
    (f -1.51d0 -2d0)
    (f 2d0 2d0)
    (f 2.49d0 2d0)
    (f 2.5d0 2d0)
    (f 2.51d0 3d0)
    (f -2d0 -2d0)
    (f -2.49d0 -2d0)
    (f -2.5d0 -2d0)
    (f -2.51d0 -3d0)
    (f 1.0000000000000001d15 1d15)
    (f -1.0000000000000001d15 -1d15)
    (f 1d16 1d16)
    (f -1d16 -1d16)))

(macrolet ((test-rounders ()
             (let (forms)
               (dolist (type '(single-float double-float))
                 (dolist (fun '(truncate floor ceiling round))
                   (push
                    `(with-test (:name (,fun ,type :zeros))
                       (let* ((form `(lambda (x)
                                       (declare (type (,',type ,(coerce -10 ',type) ,(coerce 10 ',type)) x))
                                       (,',fun x ,(coerce 2 ',type))))
                              (cfun (compile nil form)))
                         (multiple-value-bind (q r)
                             (funcall (opaque-identity ',fun) (coerce 0.0 ',type))
                           (assert (eql q 0))
                           (assert (eql r (coerce 0.0 ',type))))
                         (multiple-value-bind (q r)
                             (funcall (opaque-identity ',fun) (coerce 0.0 ',type) (coerce 2 ',type))
                           (assert (eql q 0))
                           (assert (eql r (coerce 0.0 ',type))))
                         (multiple-value-bind (q r)
                             (funcall (opaque-identity ',fun) (coerce -0.0 ',type))
                           (assert (eql q 0))
                           (assert (eql r (coerce -0.0 ',type))))
                         (multiple-value-bind (q r)
                             (funcall (opaque-identity ',fun) (coerce -0.0 ',type) (coerce 2 ',type))
                           (assert (eql q 0))
                           (assert (eql r (coerce -0.0 ',type))))
                         (multiple-value-bind (q r)
                             (funcall cfun (coerce 0.0 ',type))
                           (assert (eql q 0))
                           (assert (eql r (coerce 0.0 ',type))))
                         (multiple-value-bind (q r)
                             (funcall cfun (coerce -0.0 ',type))
                           (assert (eql q 0))
                           (assert (eql r (coerce -0.0 ',type))))))
                    forms)))
               `(progn
                  ,@forms))))
  (test-rounders))

(with-test (:name :scale-float-denormals)
  (assert (= (scale-float (scale-float (opaque-identity (expt 2.0d0 -1021))
                                       (opaque-identity -5))
                          (opaque-identity 5))
             (expt 2.0d0 -1021)))
  (assert (= (scale-float (opaque-identity least-positive-single-float)
                          (opaque-identity 0))
             least-positive-single-float)))

(with-test (:name :truncate-by-zero-error)
  (assert-error (truncate 1 (opaque-identity 0d0)) division-by-zero)
  (assert-error (truncate 1f0 (opaque-identity 0f0)) division-by-zero)
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (assert-error (truncate 1 (opaque-identity 0d0)) division-by-zero)
    (assert-error (truncate 1f0 (opaque-identity 0f0)) division-by-zero)))

(with-test (:name :+negative-zero)
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (+ a (- b)))
    ((-0.0 0) 0.0))
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (+ (- a) b))
    ((0 -0.0) 0.0))
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (- a (- b)))
    ((-0.0 0) -0.0)))

(with-test (:name :expt-to-sqrt)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (expt a 1/2))
    ((-6) (expt (opaque-identity -6) (opaque-identity 1/2))))
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (expt a -3))
    ((-392.42026693446144d0) (expt (opaque-identity -392.42026693446144d0)
                                   (opaque-identity -3))))
  (assert (= (expt (opaque-identity -0.0)
                   (opaque-identity 0.5))
             0.0)))

(with-test (:name :truncate-bignum-remainder)
  (assert (equal (multiple-value-list (truncate (opaque-identity 4503599627370495.5d0)
                                                (opaque-identity 1d0)))
                 (opaque-identity '(4503599627370495 0.5d0)))))

(with-test (:name :phase--0-derive-type)
  (assert-type (lambda (p1)
                 (declare ((single-float 0.0) p1))
                 (phase p1))
               (or (member 0.0) (single-float 3.1415927 3.1415927)))
  (assert (= (phase (opaque-identity -0.0))
             (opaque-identity 3.1415927)))
  (assert-type (lambda (p1)
                 (declare ((double-float 0d0) p1))
                 (phase p1))
               (or (member 0d0) (eql 3.141592653589793d0)))
  (assert (= (phase (opaque-identity -0d0))
             (opaque-identity pi))))

(with-test (:name :log-minus-zero-rational)
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (assert (= (log (opaque-identity -0d0) (opaque-identity 2d0))
               (opaque-identity double-float-negative-infinity)))
    (assert (= (log (opaque-identity -0f0) (opaque-identity 2f0))
               (opaque-identity single-float-negative-infinity)))
    (assert (= (log (opaque-identity -0d0) (opaque-identity 2))
               (opaque-identity double-float-negative-infinity)))
    (assert (= (log (opaque-identity -0f0) (opaque-identity 2))
               (opaque-identity single-float-negative-infinity)))))

(with-test (:name :ffloor-minus-zero-derive-type)
  (assert-type (lambda (x)
                 (declare ((float (-1) (0)) x))
                 (values (ftruncate x)))
               (float 0.0 0.0))
  (assert-type (lambda (x)
                 (declare ((single-float (0.0) (1.0)) x))
                 (values (ftruncate x -1.0)))
               (single-float 0.0 0.0)))

(defun floats-around (float)
  (multiple-value-bind (sig exp) (integer-decode-float float)
    (let* ((prev-float (if (= sig (ash 1 (1- (float-digits float))))
                           (scale-float (float (1- (ash 1 (float-digits float))) float) (1- exp))
                           (scale-float (float (1- sig) float) exp)))
           (next-float (scale-float (float (1+ sig) float) exp)))
      (values prev-float next-float sig))))

(defun check-ratio-to-float (ratio type)
  (declare (ratio ratio))
  (let* ((result (float ratio type))
         (new-ratio (rational result)))
    (multiple-value-bind (prev-float next-float sig) (floats-around result)
      (let* ((error (abs (- ratio new-ratio)))
             (error-prev (abs (- ratio (rational prev-float))))
             (error-next (abs (- ratio (rational next-float)))))
        (cond
          ((< error-next error)
           (error "(float ~a ~a) = ~a, while ~a is closer" ratio type result next-float))
          ((< error-prev error)
           (error "(float ~a ~a) = ~a, while ~a is closer" ratio type result prev-float))
          ((or (= error error-prev) (= error error-next))
           (unless (evenp sig)
             (error "(float ~a ~a) = ~a, not rounded to even" ratio type result))))))))

(with-test (:name :ratio-to-float)
  (let ((*random-state* (make-random-state t)))
    (loop repeat 20000
          do
          (let* ((n-bits (random 1075))
                 (d-bits (random 1075))
                 (num (random (ash 1 n-bits)))
                 (den (max 1 (random (ash 1 d-bits))))
                 (ratio (/ num den)))
            (when (typep ratio 'ratio)
              (handler-case (progn
                              (check-ratio-to-float ratio 1f0)
                              (check-ratio-to-float ratio 1d0))
                (arithmetic-error ())))))))

(with-test (:name :scale-float-rounding)
  (assert (= (integer-decode-float (scale-float (opaque-identity 0.7836354097202904d0) -1032))
             (opaque-identity 3446464979698))))

(with-test (:name :rationalize-denormals)
  (assert (= (rationalize (opaque-identity 1.4012985e-45))
             (opaque-identity 1/713623803817686610712884392543280002697789472))))
