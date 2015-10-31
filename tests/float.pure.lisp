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
            :fails-on '(and :darwin :ppc)) ;; bug 372
  (assert-error (scale-float 1.0 most-positive-fixnum)
                floating-point-overflow)
  (assert-error (scale-float 1.0d0 (1+ most-positive-fixnum))
                floating-point-overflow))

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
            :fails-on '(or (and :ppc :openbsd)
                           (and :ppc :darwin)
                           (and :x86 :netbsd)))
  (assert (typep (nth-value
                  1
                  (ignore-errors
                    (sb-sys:without-interrupts
                     (sb-int:set-floating-point-modes :current-exceptions nil
                                                      :accrued-exceptions nil)
                     (loop repeat 2 summing most-positive-double-float)
                     (sleep 2))))
                 'floating-point-overflow)))

;; This is the same test as above.  Even if the above copy passes,
;; this copy will fail if SIGFPE handling ends up clearing the FPU
;; control word, which can happen if the kernel clears the FPU control
;; (a reasonable thing for it to do) and the runtime fails to
;; compensate for this (see RESTORE_FP_CONTROL_WORD in interrupt.c).
;; Note that this only works when running float.pure.lisp alone, as
;; the preceeding "pure" test files aren't as free of side effects as
;; we might like.
(with-test (:name (:addition-overflow :bug-372 :take-2)
            :fails-on '(or (and :ppc :openbsd)
                           (and :ppc :darwin)
                           (and :x86 :netbsd)))
  (assert (typep (nth-value
                  1
                  (ignore-errors
                    (sb-sys:without-interrupts
                     (sb-int:set-floating-point-modes :current-exceptions nil
                                                      :accrued-exceptions nil)
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
            :fails-on '(or :sparc))
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

(with-test (:name :log-int/double-accuracy)
  ;; we used to use single precision for intermediate results
  (assert (eql 2567.6046442221327d0
               (log (loop for n from 1 to 1000 for f = 1 then (* f n)
                          finally (return f))
                    10d0)))
  ;; both ways
  (assert (eql (log 123123123.0d0 10) (log 123123123 10.0d0))))

(with-test (:name :log-base-zero-return-type)
  (assert (eql 0.0f0 (log 123 (eval 0))))
  (assert (eql 0.0d0 (log 123.0d0 (eval 0))))
  (assert (eql 0.0d0 (log 123 (eval 0.0d0))))
  (let ((f (compile nil '(lambda (x y)
                          (declare (optimize speed))
                          (etypecase x
                            (single-float
                             (etypecase y
                               (single-float (log x y))
                               (double-float (log x y))))
                            (double-float
                             (etypecase y
                               (single-float (log x y))
                               (double-float (log x y)))))))))
    (assert (eql 0.0f0 (funcall f 123.0 0.0)))
    (assert (eql 0.0d0 (funcall f 123.0d0 0.0)))
    (assert (eql 0.0d0 (funcall f 123.0d0 0.0d0)))
    (assert (eql 0.0d0 (funcall f 123.0 0.0d0)))))

;; Bug reported by Eric Marsden on July 15 2009. The compiler
;; used not to constant fold calls with arguments of type
;; (EQL foo).
(with-test (:name :eql-type-constant-fold)
  (assert (equal '(FUNCTION (T) (VALUES (MEMBER T) &OPTIONAL))
                 (sb-kernel:%simple-fun-type
                  (compile nil `(lambda (x)
                                  (eql #c(1.0 2.0)
                                       (the (eql #c(1.0 2.0))
                                         x))))))))

;; Leakage from the host could result in wrong values for truncation.
(with-test (:name :truncate)
  (assert (plusp (sb-kernel:%unary-truncate/single-float (expt 2f0 33))))
  (assert (plusp (sb-kernel:%unary-truncate/double-float (expt 2d0 33))))
  ;; That'd be one strange host, but just in case
  (assert (plusp (sb-kernel:%unary-truncate/single-float (expt 2f0 65))))
  (assert (plusp (sb-kernel:%unary-truncate/double-float (expt 2d0 65)))))

;; On x86-64, we sometimes forgot to clear the higher order bits of the
;; destination register before using it with an instruction that doesn't
;; clear the (unused) high order bits. Suspect instructions are operations
;; with only one operand: for everything else, the destination has already
;; been loaded with a value, making it safe (by induction).
;;
;; The tests are extremely brittle and could be broken by any number of
;; back- or front-end optimisations. We should just keep the issue above
;; in mind at all times when working with SSE or similar instruction sets.
;;
;; Run only on x86/x86-64m as no other platforms have SB-VM::TOUCH-OBJECT.
#-interpreter
(macrolet ((with-pinned-floats ((count type &rest names) &body body)
             "Force COUNT float values to be kept live (and hopefully in registers),
              fill a temporary register with noise, and execute BODY."
             ;; KLUDGE: SB-VM is locked, and non-x86oids don't have
             ;; SB-VM::TOUCH-OBJECT.  Don't even READ this body on
             ;; other platforms.
             #-(or x86 x86-64)
             (declare (ignore count type names body))
             #+(or x86 x86-64)
             (let ((dummy (loop repeat count
                                collect (or (pop names)
                                            (gensym "TEMP")))))
               `(let ,(loop for i downfrom -1
                            for var in dummy
                            for j = (coerce i type)
                            collect
                            `(,var ,(complex j j))) ; we don't actually need that, but
                  (declare (type (complex ,type) ,@dummy)) ; future-proofing can't hurt
                  ,@(loop for var in dummy
                          for i upfrom 0
                          collect `(setf ,var ,(complex i (coerce i type))))
                  (multiple-value-prog1
                      (progn
                        (let ((x ,(complex 1d0 1d0)))
                          (declare (type (complex double-float) x))
                          (setf x ,(complex most-positive-fixnum (float most-positive-fixnum 1d0)))
                          (sb-vm::touch-object x))
                        (locally ,@body))
                    ,@(loop for var in dummy
                            collect `(sb-vm::touch-object ,var)))))))
  (with-test (:name :clear-sqrtsd :skipped-on '(not (or :x86 :x86-64)))
    (flet ((test-sqrtsd (float)
             (declare (optimize speed (safety 1))
                      (type (double-float (0d0)) float))
             (with-pinned-floats (14 double-float x0)
               (let ((x (sqrt float)))
                 (values (+ x x0) float)))))
      (declare (notinline test-sqrtsd))
      (assert (zerop (imagpart (test-sqrtsd 4d0))))))

  (with-test (:name :clear-sqrtsd-single :skipped-on '(not (or :x86 :x86-64)))
    (flet ((test-sqrtsd-float (float)
             (declare (optimize speed (safety 1))
                      (type (single-float (0f0)) float))
             (with-pinned-floats (14 single-float x0)
               (let ((x (sqrt float)))
                 (values (+ x x0) float)))))
      (declare (notinline test-sqrtsd-float))
      (assert (zerop (imagpart (test-sqrtsd-float 4f0))))))

  (with-test (:name :clear-cvtss2sd :skipped-on '(not (or :x86 :x86-64)))
    (flet ((test-cvtss2sd (float)
             (declare (optimize speed (safety 1))
                      (type single-float float))
             (with-pinned-floats (14 double-float x0)
               (let ((x (float float 0d0)))
                 (values (+ x x0) (+ 1e0 float))))))
      (declare (notinline test-cvtss2sd))
      (assert (zerop (imagpart (test-cvtss2sd 1f0))))))

  (with-test (:name :clear-cvtsd2ss :skipped-on '(not (or :x86 :x86-64)))
    (flet ((test-cvtsd2ss (float)
             (declare (optimize speed (safety 1))
                      (type double-float float))
             (with-pinned-floats (14 single-float x0)
               (let ((x (float float 1e0)))
                 (values (+ x x0) (+ 1d0 float))))))
      (declare (notinline test-cvtsd2ss))
      (assert (zerop (imagpart (test-cvtsd2ss 4d0))))))

  (with-test (:name :clear-cvtsi2sd :skipped-on '(not (or :x86 :x86-64)))
    (flet ((test-cvtsi2sd (int)
             (declare (optimize speed (safety 0))
                      (type (unsigned-byte 10) int))
             (with-pinned-floats (15 double-float x0)
               (+ (float int 0d0) x0))))
      (declare (notinline test-cvtsi2sd))
      (assert (zerop (imagpart (test-cvtsi2sd 4))))))

  (with-test (:name :clear-cvtsi2ss :skipped-on '(not (or :x86 :x86-64)))
    (flet ((test-cvtsi2ss (int)
             (declare (optimize speed (safety 0))
                      (type (unsigned-byte 10) int))
             (with-pinned-floats (15 single-float x0)
               (+ (float int 0e0) x0))))
      (declare (notinline test-cvtsi2ss))
      (assert (zerop (imagpart (test-cvtsi2ss 4)))))))

(with-test (:name :round-to-bignum)
  (assert (= (round 1073741822.3d0) 1073741822))
  (assert (= (round 1073741822.5d0) 1073741822))
  (assert (= (round 1073741822.7d0) 1073741823))
  (assert (= (round 1073741823.3d0) 1073741823))
  (assert (= (round 1073741823.5d0) 1073741824))
  (assert (= (round 1073741823.7d0) 1073741824)))

(with-test (:name :round-single-to-bignum)
  (assert (= (round 1e14) 100000000376832))
  (assert (= (round 1e19) 9999999980506447872)))

(with-test (:name :scaled-%hypot)
  (assert (<= (abs (complex most-positive-double-float 1d0))
              (1+ most-positive-double-float))))

;; On x86-64, MAKE-SINGLE-FLOAT with a negative argument used to set
;; bits 32-63 of the XMM register to 1, breaking the invariant that
;; unused parts of XMM registers are always zero. This could become
;; visible as a QNaN in the imaginary part when next using the register
;; in a (COMPLEX SINGLE-FLOAT) operation.
(with-test (:name :make-single-float-clear-imagpart)
  (let ((f (compile nil
                    '(lambda (x)
                       (declare (optimize speed))
                       (= #c(1.0f0 2.0f0)
                          (+ #c(3.0f0 2.0f0)
                             (sb-kernel:make-single-float x))))))
        (bits (sb-kernel:single-float-bits -2.0f0)))
    (assert (< bits 0))         ; Make sure the test is fit for purpose.
    (assert (funcall f bits))))
