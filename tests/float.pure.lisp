;;;; floating-point-related tests with no side effects

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

(cl:in-package :cl-user)

(dolist (ifnis (list (cons single-float-positive-infinity
                           single-float-negative-infinity)
                     (cons double-float-positive-infinity
                           double-float-negative-infinity)))
  (destructuring-bind (+ifni . -ifni) ifnis
    (assert (= (* +ifni 1) +ifni))
    (assert (= (* +ifni -0.1) -ifni))
    (assert (= (+ +ifni -0.1) +ifni))
    (assert (= (- +ifni -0.1) +ifni))
    (assert (= (sqrt +ifni) +ifni))
    (assert (= (* -ifni -14) +ifni))
    (assert (= (/ -ifni 0.1) -ifni))
    (assert (= (/ -ifni 100/3) -ifni))
    (assert (not (= +ifni -ifni)))
    (assert (= -ifni -ifni))
    (assert (not (= +ifni 100/3)))
    (assert (not (= -ifni -1.0 -ifni)))
    (assert (not (= -ifni -17/02 -ifni)))
    (assert (< -ifni +ifni))
    (assert (not (< +ifni 100)))
    (assert (not (< +ifni 100.0)))
    (assert (not (< +ifni -ifni)))
    (assert (< 100 +ifni))
    (assert (< 100.0 +ifni))
    (assert (>= 100 -ifni))
    (assert (not (<= 6/7 (* 3 -ifni))))
    (assert (not (> +ifni +ifni)))))

;;; ANSI: FLOAT-RADIX should signal an error if its argument is not a
;;; float.
;;;
;;; (Peter Van Eynde's ansi-test suite caught this, and Eric Marsden
;;; reported a fix for CMU CL, which was ported to sbcl-0.6.12.35.)
(assert (typep (nth-value 1 (ignore-errors (float-radix "notfloat")))
               'type-error))

(assert (typep (nth-value 1 (ignore-errors
                              (funcall (fdefinition 'float-radix) "notfloat")))
               'type-error))

;;; Before 0.8.2.14 the cross compiler failed to work with
;;; denormalized numbers
(when (subtypep 'single-float 'short-float)
  (assert (eql least-positive-single-float least-positive-short-float)))

;;; bug found by Paul Dietz: FFLOOR and similar did not work for integers
(let ((tests '(((ffloor -8 3) (-3.0 1))
               ((fround -8 3) (-3.0 1))
               ((ftruncate -8 3) (-2.0 -2))
               ((fceiling -8 3) (-2.0 -2)))))
  (loop for (exp res) in tests
        for real-res = (multiple-value-list (eval exp))
        do (assert (equal real-res res))))

;;; bug 45b reported by PVE
(dolist (type '(short single double long))
  (dolist (sign '(positive negative))
    (let* ((name (find-symbol (format nil "LEAST-~A-~A-FLOAT"
                                      sign type)
                              :cl))
           (value (symbol-value name)))
      (assert (zerop (/ value 2))))))

;;; bug found by Paul Dietz: bad rounding on small floats
(assert (= (fround least-positive-short-float least-positive-short-float) 1.0))

;;; bug found by Peter Seibel: scale-float was only accepting float
;;; exponents, when it should accept all integers.  (also bug #269)
(assert (= (multiple-value-bind (significand expt sign)
               (integer-decode-float least-positive-double-float)
             (* (scale-float (float significand 0.0d0) expt) sign))
           least-positive-double-float))
(assert (= (multiple-value-bind (significand expt sign)
               (decode-float least-positive-double-float)
             (* (scale-float significand expt) sign))
           least-positive-double-float))
(assert (= 0.0 (scale-float 1.0 most-negative-fixnum)))
(assert (= 0.0d0 (scale-float 1.0d0 (1- most-negative-fixnum))))

(with-test (:name (:scale-float-overflow :bug-372)
            :fails-on '(or :ppc :darwin)) ;; bug 372
  (progn
    (assert (raises-error? (scale-float 1.0 most-positive-fixnum)
                           floating-point-overflow))
    (assert (raises-error? (scale-float 1.0d0 (1+ most-positive-fixnum))
                           floating-point-overflow))))

;;; bug found by jsnell when nfroyd tried to implement better LOGAND
;;; type derivation.
(assert (= (integer-decode-float (coerce -1756510900000000000
                                         'single-float))
           12780299))

;;; MISC.564: no out-of-line %ATAN2 for constant folding
(assert (typep
  (funcall
   (compile
    nil
    '(lambda (p1)
      (declare (optimize (speed 3) (safety 2) (debug 3) (space 0))
       (type complex p1))
      (phase (the (eql #c(1.0d0 2.0d0)) p1))))
   #c(1.0d0 2.0d0))
    'double-float))

;;; More out of line functions (%COS, %SIN, %TAN) for constant folding,
;;; reported by Mika Pihlajamäki
(funcall (compile nil '(lambda () (cos (tan (round 0))))))
(funcall (compile nil '(lambda () (sin (tan (round 0))))))
(funcall (compile nil '(lambda () (tan (tan (round 0))))))

(with-test (:name (:addition-overflow :bug-372)
            :fails-on '(or :ppc :darwin :mips (and :x86 :netbsd)))
  (assert (typep (nth-value
                  1
                  (ignore-errors
                    (sb-sys:without-interrupts
                     (loop repeat 2 summing most-positive-double-float)
                     (sleep 2))))
                 'floating-point-overflow)))

;;; On x86-64 generating complex floats on the stack failed an aver in
;;; the compiler if the stack slot was the same as the one containing
;;; the real part of the complex. The following expression was able to
;;; trigger this in 0.9.5.62.
(with-test (:name :complex-float-stack)
  (dolist (type '((complex double-float)
                  (complex single-float)))
    (compile nil
             `(lambda (x0 x1 x2 x3 x4 x5 x6 x7)
                (declare (type ,type x0 x1 x2 x3 x4 x5 x6 x7))
                (let ((x0 (+ x0 x0))
                      (x1 (+ x1 x1))
                      (x2 (+ x2 x2))
                      (x3 (+ x3 x3))
                      (x4 (+ x4 x4))
                      (x5 (+ x5 x5))
                      (x6 (+ x6 x6))
                      (x7 (+ x7 x7)))
                  (* (+ x0 x1 x2 x3) (+ x4 x5 x6 x7)
                     (+ x0 x2 x4 x6) (+ x1 x3 x5 x7)
                     (+ x0 x3 x4 x7) (+ x1 x2 x5 x6)
                     (+ x0 x1 x6 x7) (+ x2 x3 x4 x5)))))))


(with-test (:name :nan-comparisons
            :fails-on (or :x86-64 :sparc :mips))
  (sb-int:with-float-traps-masked (:invalid)
    (macrolet ((test (form)
                 (let ((nform (subst '(/ 0.0 0.0) 'nan form)))
                   `(progn
                      (assert (eval ',nform))
                      (assert (eval `(let ((nan (/ 0.0 0.0)))
                                       ,',form)))
                      (assert (funcall
                               (compile nil `(lambda () ,',nform))))
                      (assert (funcall
                               (compile nil `(lambda (nan) ,',form))
                               (/ 0.0 0.0)))))))
      (test (/= nan nan))
      (test (/= nan nan nan))
      (test (/= 1.0 nan 2.0 nan))
      (test (/= nan 1.0 2.0 nan))
      (test (not (= nan 1.0)))
      (test (not (= nan nan)))
      (test (not (= nan nan nan)))
      (test (not (= 1.0 nan)))
      (test (not (= nan 1.0)))
      (test (not (= 1.0 1.0 nan)))
      (test (not (= 1.0 nan 1.0)))
      (test (not (= nan 1.0 1.0)))
      (test (not (>= nan nan)))
      (test (not (>= nan 1.0)))
      (test (not (>= 1.0 nan)))
      (test (not (>= 1.0 nan 0.0)))
      (test (not (>= 1.0 0.0 nan)))
      (test (not (>= nan 1.0 0.0)))
      (test (not (<= nan nan)))
      (test (not (<= nan 1.0)))
      (test (not (<= 1.0 nan)))
      (test (not (<= 1.0 nan 2.0)))
      (test (not (<= 1.0 2.0 nan)))
      (test (not (<= nan 1.0 2.0)))
      (test (not (< nan nan)))
      (test (not (< -1.0 nan)))
      (test (not (< nan 1.0)))
      (test (not (> nan nan)))
      (test (not (> -1.0 nan)))
      (test (not (> nan 1.0))))))
