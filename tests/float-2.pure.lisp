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

(with-test (:name :bug-407a)
  (assert-error
   (loop for n from (expt 2 1024) upto (+ 10 (expt 2 1024))
         do (coerce n 'single-float))
   floating-point-overflow))

(with-test (:name :bug-407b)
  (assert-error
   (loop for n from (expt 2 1024) upto (+ 10 (expt 2 1024))
         do (format nil "~E~%" n))
   floating-point-overflow))

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

;; The x86 port used not to reduce the arguments of transcendentals
;; correctly.
;; This test is valid only for x86: The x86 port uses the builtin x87
;; FPU instructions to implement the trigonometric functions; other
;; ports rely on the system's math library. These two differ in the
;; precision of pi used for the range reduction and so yield results
;; that can differ by arbitrarily large amounts for large inputs.
;; The test expects the x87 results.
(with-test (:name (:range-reduction :x87)
            :skipped-on (not :x86))
  (flet ((almost= (x y)
           (< (abs (- x y)) 1d-5)))
    (macrolet ((foo (op value)
                 `(let ((actual (,op ,value))
                        (expected (,op (mod ,value (* 2 pi)))))
                    (unless (almost= actual expected)
                      (error "Inaccurate result for ~a: expected ~a, got ~a"
                             (list ',op ,value) expected actual)))))
      (let ((big (* pi (expt 2d0 70)))
            (mid (coerce most-positive-fixnum 'double-float))
            (odd (* pi most-positive-fixnum)))
        (foo sin big)
        (foo sin mid)
        (foo sin odd)
        (foo sin (/ odd 2d0))

        (foo cos big)
        (foo cos mid)
        (foo cos odd)
        (foo cos (/ odd 2d0))

        (foo tan big)
        (foo tan mid)
        (foo tan odd)))))

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
;; This test is skipped on x86; as to why see the comment at the test
;; (:range-reduction :x87) above.
(with-test (:name (:range-reduction :precise-pi)
            :skipped-on :x86
            :fails-on (and :openbsd :x86-64))
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
                               ;; I'll bet that's why we disabled the test on openbsd,
                               ;; but I don't care to find out.
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
