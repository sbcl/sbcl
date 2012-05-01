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

(in-package :cl-user)

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
(with-test (:name (:random :integer :set-of-integers :distribution))
  (let* ((high (floor (expt 2 33) 3))
         (mid (floor high 2))
         (fun (compile nil `(lambda (x)
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

(with-test (:name (:random :integer :set-of-integers :chunk-size))
  (let* ((high (expt 2 64))
         (fun (compile nil `(lambda (x)
                              (random (if x ,high 10)))))
         (n 200)
         (x 0))
    (dotimes (i n)
      (setf x (logior x (funcall fun t))))
    ;; If RANDOM works correctly, x should be #b111...111 (64 ones)
    ;; with a probability of 1 minus approximately (expt 2 -194).
    (unless (= x (1- high))
      (error "bad RANDOM distribution: ~16,16,'0r" x))))
