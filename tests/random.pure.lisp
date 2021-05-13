;;;; various RANDOM tests without side effects

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

(enable-test-parallelism)

;;; Tests in this file that rely on properties of the distribution of
;;; the random numbers are designed to be fast and have a very low
;;; probability of false positives, generally of the order of (expt 10 -60).
;;; These tests are not intended to assure the statistical qualities of the
;;; pseudo random number generator but to help find bugs in its and RANDOM's
;;; implementation.

;; When the type of the argument of RANDOM is a set of integers, a
;; DEFTRANSFORM triggered that simply generated (REM (RANDOM-CHUNK) NUM),
;; which has two severe problems: The resulting distribution is very uneven
;; for most arguments of RANDOM near the size of a random chunk and the
;; RANDOM-CHUNK used was always 32 bits, even under 64 bit wordsize which
;; yields even more disastrous distributions.
(with-test (:name (random integer :set-of-integers :distribution))
  (let* ((high (floor (expt 2 33) 3))
         (mid (floor high 2))
         (fun (checked-compile `(lambda (x)
                                  (random (if x ,high 10)))))
         (n1 0)
         (n 10000))
    (dotimes (i n)
      (when (>= (funcall fun t) mid)
        (incf n1)))
    ;; Half of the values of (RANDOM HIGH) should be >= MID, so we expect
    ;; N1 to be binomially distributed such that this distribution can be
    ;; approximated by a normal distribution with mean (/ N 2) and standard
    ;; deviation (* (sqrt N) 1/2). The broken RANDOM we are testing here for
    ;; yields (/ N 3) and (* (sqrt N) (sqrt 2/9)), respectively. We test if
    ;; N1 is below the average of (/ N 3) and (/ N 2). With a value of N of
    ;; 10000 this is more than 16 standard deviations away from the expected
    ;; mean, which has a probability of occurring by chance of below
    ;; (expt 10 -60).
    (when (< n1 (* n 5/12))
      (error "bad RANDOM distribution: expected ~d, got ~d" (/ n 2) n1))))

(with-test (:name (random integer :set-of-integers :chunk-size))
  (let* ((high (expt 2 64))
         (fun (checked-compile `(lambda (x)
                                  (random (if x ,high 10)))))
         (n 200)
         (x 0))
    (dotimes (i n)
      (setf x (logior x (funcall fun t))))
    ;; If RANDOM works correctly, x should be #b111...111 (64 ones)
    ;; with a probability of 1 minus approximately (expt 2 -194).
    (unless (= x (1- high))
      (error "bad RANDOM distribution: ~16,16,'0r" x))))

;;; Some tests for basic integer RANDOM functionality.

(with-test (:name (random integer :error-if-invalid-random-state))
  (map-optimize-declarations
   (lambda (optimize)
     (dolist (expr `((lambda (x state)
                       (declare (optimize ,@optimize))
                       (random x state))
                     (lambda (x state)
                       (declare (optimize ,@optimize))
                       (declare (type integer x))
                       (random x state))
                     (lambda (x state)
                       (declare (optimize ,@optimize))
                       (declare (type (integer 100 200) x))
                       (random x state))
                     (lambda (x state)
                       (declare (optimize ,@optimize))
                       (random (if x 10 20) state))))
       (let ((fun (checked-compile expr)))
         (assert-error (funcall fun 150 nil) type-error))))
   :speed '(0 3) :safety nil :space '(nil 0) :compilation-speed '(0 3)))

(with-test (:name (random integer :distribution))
  (let ((generic-random (checked-compile '(lambda (x)
                                            (random x)))))
    ;; Check powers of two: Every bit in the output should be sometimes
    ;; 0, sometimes 1.
    (dotimes (e 200)
      (let* ((number (expt 2 e))
             (foo (lambda ()
                    (funcall generic-random number)))
             (bar (checked-compile `(lambda ()
                                      (declare (optimize speed))
                                      (random ,number)))))
        (flet ((test (fun)
                 (let ((x-and (funcall fun))
                       (x-ior (funcall fun)))
                   (dotimes (i 199)
                     (setf x-and (logand x-and (funcall fun))
                           x-ior (logior x-ior (funcall fun))))
                   (assert (= x-and 0))
                   (assert (= x-ior (1- number))))))
          (test foo)
          (test bar))))
    ;; Test a collection of fixnums and bignums, powers of two and
    ;; numbers just below and above powers of two, numbers needing one,
    ;; two or more random chunks etc.
    (dolist (number (remove-duplicates
                     `(,@(loop for i from 2 to 11 collect i)
                       ,@(loop for i in '(29 30 31 32 33 60 61 62 63 64 65)
                               nconc (list (1- (expt 2 i))
                                           (expt 2 i)
                                           (1+ (expt 2 i))))
                       ,@(loop for i from (1- sb-kernel::n-random-chunk-bits)
                               to (* sb-kernel::n-random-chunk-bits 4)
                               collect (* 3 (expt 2 i)))
                       ,@(loop for i from 2 to sb-vm:n-word-bits
                               for n = (expt 16 i)
                               for r = (+ n (random n))
                               collect r))))
      (let ((foo (lambda ()
                   (funcall generic-random number)))
            (bar (checked-compile `(lambda ()
                                     (declare (optimize speed))
                                     (random ,number)))))
        (flet ((test (fun)
                 (let* ((min (funcall fun))
                        (max min))
                   (dotimes (i 9999)
                     (let ((r (funcall fun)))
                       (when (< r min)
                         (setf min r))
                       (when (> r max)
                         (setf max r))))
                   ;; With 10000 trials and an argument of RANDOM below
                   ;; 70 the probability of the minimum not being 0 is
                   ;; less than (expt 10 -60), so we can test for that;
                   ;; correspondingly with the maximum. For larger
                   ;; arguments we can only test that all results are
                   ;; in range.
                   (if (< number 70)
                       (progn
                         (assert (= min 0))
                         (assert (= max (1- number))))
                       (progn
                         (assert (>= min 0))
                         (assert (< max number)))))))
          (test foo)
          (test bar))))))

;;; RANDOM with a float argument used to produce that argument as a
;;; return value with a probability depending on the magnitude of the
;;; argument. That behavior was wrong since the argument is an
;;; /exclusive/ upper bound for the range of produced values. The
;;; cases below increase the failure possibility as much as possible:
;;; an exclusive upper bound of LEAST-POSITIVE-*-FLOAT means that the
;;; respective 0 is the only valid return value and that the old code
;;; produced a wrong answer with probably 0.5 (assuming proper
;;; distribution of the produced values).
(with-test (:name (random float :upper-bound-exclusive))
  ;; SINGLE-FLOAT, possibly inlined
  (let ((values (loop :repeat 10000
                      :collect (random least-positive-single-float))))
    (assert (not (find 0.0f0 values :test-not #'=))))
  ;; DOUBLE-FLOAT, possibly inlined
  (let ((values (loop :repeat 10000
                      :collect (random least-positive-double-float))))
    (assert (not (find 0.0d0 values :test-not #'=))))
  (locally (declare (notinline random))
    ;; SINGLE-FLOAT, not inlined
    (let ((values (loop :repeat 10000
                        :collect (random least-positive-single-float))))
      (assert (not (find 0.0f0 values :test-not #'=))))
    ;; DOUBLE-FLOAT, not inlined
    (let ((values (loop :repeat 10000
                        :collect (random least-positive-double-float))))
      (assert (not (find 0.0d0 values :test-not #'=))))))

(with-test (:name :float-no-consing
            :fails-on :ppc
            :skipped-on :interpreter)
  (let ((fun (checked-compile `(lambda ()
                                 (declare (optimize speed))
                                 (> (random 40d0) 1d0))
                              :allow-notes nil)))
    (ctu:assert-no-consing (funcall fun))))
