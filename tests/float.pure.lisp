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

(with-test (:name (:infinities :comparison))
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
      (assert (not (> +ifni +ifni))))))

;;; ANSI: FLOAT-RADIX should signal an error if its argument is not a
;;; float.
;;;
;;; (Peter Van Eynde's ansi-test suite caught this, and Eric Marsden
;;; reported a fix for CMU CL, which was ported to sbcl-0.6.12.35.)
(with-test (:name (float-radix simple-type-error))
  (multiple-value-bind  (fun failure-p warnings)
      (checked-compile '(lambda () (float-radix "notfloat")) :allow-warnings t)
    (assert failure-p)
    (assert (= 1 (length warnings)))
    (assert-error (funcall fun) type-error))
  (assert-error (funcall (fdefinition 'float-radix) "notfloat") type-error))

;;; Before 0.8.2.14 the cross compiler failed to work with
;;; denormalized numbers
(with-test (:name (:denormalized float))
  (when (subtypep 'single-float 'short-float)
    (assert (eql least-positive-single-float least-positive-short-float))))

;;; bug found by Paul Dietz: FFLOOR and similar did not work for integers
(with-test (:name (ffloor integer))
  (let ((tests '(((ffloor -8 3) (-3.0 1))
                 ((fround -8 3) (-3.0 1))
                 ((ftruncate -8 3) (-2.0 -2))
                 ((fceiling -8 3) (-2.0 -2)))))
    (loop for (exp res) in tests
       for real-res = (multiple-value-list (eval exp))
       do (assert (equal real-res res)))))

;;; bug 45b reported by PVE
(with-test (:name (:least-*-*-float :bug-45b))
  (dolist (type '(short single double long))
    (dolist (sign '(positive negative))
      (let* ((name (find-symbol (format nil "LEAST-~A-~A-FLOAT"
                                        sign type)
                                :cl))
             (value (symbol-value name)))
        (assert (zerop (/ value 2)))))))

;;; bug found by Paul Dietz: bad rounding on small floats
(with-test (:name (fround least-positive-short-float))
  (assert (= (fround least-positive-short-float least-positive-short-float) 1.0)))

;;; bug found by Peter Seibel: scale-float was only accepting float
;;; exponents, when it should accept all integers.  (also bug #269)
(with-test (:name (scale-float :bug-269))
  (assert (= (multiple-value-bind (significand expt sign)
                 (integer-decode-float least-positive-double-float)
               (* (scale-float (float significand 0.0d0) expt) sign))
             least-positive-double-float))
  (assert (= (multiple-value-bind (significand expt sign)
                 (decode-float least-positive-double-float)
               (* (scale-float significand expt) sign))
             least-positive-double-float))
  (assert (= 0.0 (scale-float 1.0 most-negative-fixnum)))
  (assert (= 0.0d0 (scale-float 1.0d0 (1- most-negative-fixnum)))))

(with-test (:name (:scale-float-overflow :bug-372)
            :fails-on (or :arm64 (and :darwin :ppc)))
  (flet ((test (form)
           (assert-error (funcall (checked-compile `(lambda () ,form)
                                                   :allow-style-warnings t))
                         floating-point-overflow)))
    (test '(scale-float 1.0 most-positive-fixnum))
    (test '(scale-float 1.0d0 (1+ most-positive-fixnum)))))

;;; bug found by jsnell when nfroyd tried to implement better LOGAND
;;; type derivation.
(assert (= (integer-decode-float (coerce -1756510900000000000
                                         'single-float))
           12780299))

;;; MISC.564: no out-of-line %ATAN2 for constant folding
(with-test (:name (:%atan2 :constant-folding))
  (assert (typep
           (funcall
            (checked-compile
             '(lambda (p1)
               (declare (optimize (speed 3) (safety 2) (debug 3) (space 0))
                        (type complex p1))
               (phase (the (eql #c(1.0d0 2.0d0)) p1))))
            #c(1.0d0 2.0d0))
           'double-float)))

;;; More out of line functions (%COS, %SIN, %TAN) for constant folding,
;;; reported by Mika Pihlajamäki
(with-test (:name (sin cos tan :constant-folding))
  (flet ((test (function)
           (funcall (checked-compile
                     `(lambda () (,function (tan (round 0))))))))
    (mapc #'test '(sin cos tan))))

(with-test (:name (:addition-overflow :bug-372)
            :fails-on (or :arm64
                        (and :ppc :openbsd)
                        (and :ppc :darwin)
                        (and :x86 :netbsd)))
  (assert-error
   (sb-sys:without-interrupts
     (sb-int:set-floating-point-modes :current-exceptions nil
                                      :accrued-exceptions nil)
     (loop repeat 2 summing most-positive-double-float)
     (sleep 2))
   floating-point-overflow))

;; This is the same test as above.  Even if the above copy passes,
;; this copy will fail if SIGFPE handling ends up clearing the FPU
;; control word, which can happen if the kernel clears the FPU control
;; (a reasonable thing for it to do) and the runtime fails to
;; compensate for this (see RESTORE_FP_CONTROL_WORD in interrupt.c).
;; Note that this only works when running float.pure.lisp alone, as
;; the preceeding "pure" test files aren't as free of side effects as
;; we might like.
(with-test (:name (:addition-overflow :bug-372 :take-2)
            :fails-on (or :arm64
                        (and :ppc :openbsd)
                        (and :ppc :darwin)
                        (and :x86 :netbsd)))
  (assert-error
   (sb-sys:without-interrupts
     (sb-int:set-floating-point-modes :current-exceptions nil
                                      :accrued-exceptions nil)
     (loop repeat 2 summing most-positive-double-float)
     (sleep 2))
   floating-point-overflow))

;;; On x86-64 generating complex floats on the stack failed an aver in
;;; the compiler if the stack slot was the same as the one containing
;;; the real part of the complex. The following expression was able to
;;; trigger this in 0.9.5.62.
(with-test (:name :complex-float-stack)
  (dolist (type '((complex double-float)
                  (complex single-float)))
    (checked-compile `(lambda (x0 x1 x2 x3 x4 x5 x6 x7)
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

(with-test (:name (:nan :comparison)
            :fails-on (or :sparc))
  (sb-int:with-float-traps-masked (:invalid)
    (macrolet ((test (form)
                 (let ((nform (subst '(/ 0.0 0.0) 'nan form)))
                   `(progn
                      (assert (eval ',nform))
                      (assert (eval `(let ((nan (/ 0.0 0.0)))
                                       ,',form)))
                      (assert (funcall
                               (checked-compile `(lambda () ,',nform))))
                      (assert (funcall
                               (checked-compile `(lambda (nan) ,',form))
                               (locally
                                   (declare (muffle-conditions style-warning))
                                 (/ 0.0 0.0))))))))
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
  (let ((f (checked-compile '(lambda (x y)
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
  (with-test (:name :clear-sqrtsd :skipped-on (not (or :x86 :x86-64)))
    (flet ((test-sqrtsd (float)
             (declare (optimize speed (safety 1))
                      (type (double-float (0d0)) float))
             (with-pinned-floats (14 double-float x0)
               (let ((x (sqrt float)))
                 (values (+ x x0) float)))))
      (declare (notinline test-sqrtsd))
      (assert (zerop (imagpart (test-sqrtsd 4d0))))))

  (with-test (:name :clear-sqrtsd-single :skipped-on (not (or :x86 :x86-64)))
    (flet ((test-sqrtsd-float (float)
             (declare (optimize speed (safety 1))
                      (type (single-float (0f0)) float))
             (with-pinned-floats (14 single-float x0)
               (let ((x (sqrt float)))
                 (values (+ x x0) float)))))
      (declare (notinline test-sqrtsd-float))
      (assert (zerop (imagpart (test-sqrtsd-float 4f0))))))

  (with-test (:name :clear-cvtss2sd :skipped-on (not (or :x86 :x86-64)))
    (flet ((test-cvtss2sd (float)
             (declare (optimize speed (safety 1))
                      (type single-float float))
             (with-pinned-floats (14 double-float x0)
               (let ((x (float float 0d0)))
                 (values (+ x x0) (+ 1e0 float))))))
      (declare (notinline test-cvtss2sd))
      (assert (zerop (imagpart (test-cvtss2sd 1f0))))))

  (with-test (:name :clear-cvtsd2ss :skipped-on (not (or :x86 :x86-64)))
    (flet ((test-cvtsd2ss (float)
             (declare (optimize speed (safety 1))
                      (type double-float float))
             (with-pinned-floats (14 single-float x0)
               (let ((x (float float 1e0)))
                 (values (+ x x0) (+ 1d0 float))))))
      (declare (notinline test-cvtsd2ss))
      (assert (zerop (imagpart (test-cvtsd2ss 4d0))))))

  (with-test (:name :clear-cvtsi2sd :skipped-on (not (or :x86 :x86-64)))
    (flet ((test-cvtsi2sd (int)
             (declare (optimize speed (safety 0))
                      (type (unsigned-byte 10) int))
             (with-pinned-floats (15 double-float x0)
               (+ (float int 0d0) x0))))
      (declare (notinline test-cvtsi2sd))
      (assert (zerop (imagpart (test-cvtsi2sd 4))))))

  (with-test (:name :clear-cvtsi2ss :skipped-on (not (or :x86 :x86-64)))
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
  (let ((f (checked-compile
            '(lambda (x)
              (declare (optimize speed))
              (= #c(1.0f0 2.0f0)
               (+ #c(3.0f0 2.0f0)
                (sb-kernel:make-single-float x))))))
        (bits (sb-kernel:single-float-bits -2.0f0)))
    (assert (< bits 0))         ; Make sure the test is fit for purpose.
    (assert (funcall f bits))))

(with-test (:name :negative-zero-derivation)
  (assert (not
           (funcall (checked-compile
                     '(lambda (exponent)
                       (declare ((integer 0 1) exponent))
                       (eql 0d0 (scale-float -0.0d0 exponent))))
                    0))))

(with-test (:name :complex-eql-all-constants)
  (assert (funcall (checked-compile
                    '(lambda ()
                      (declare (optimize (debug 2)))
                      (typep #c(1.0 1.0) '(member #c(1.0 1.0))))))))

(with-test (:name (truncate float :no-consing)
                  :skipped-on :interpreter)
  (let ((f (checked-compile
            '(lambda (x)
              (values (truncate (the double-float x)))))))
    (ctu:assert-no-consing (funcall f 1d0))
    (ctu:assert-no-consing (funcall f (float most-negative-fixnum 1d0))))
  (let ((f (checked-compile
            '(lambda (x)
              (values (truncate (the single-float x)))))))
    (ctu:assert-no-consing (funcall f 1f0))
    (ctu:assert-no-consing (funcall f (float most-negative-fixnum 1f0)))))

(with-test (:name :trig-derive-type-complex-rational)
  (macrolet ((test (fun type)
               `(checked-compile-and-assert
                 ()
                 '(lambda (a)
                   (declare ((complex ,type) a))
                   (,fun a))
                 ((#C(1 2)) (eval '(,fun #C(1 2)))))))
    (test sin integer)
    (test cos integer)
    (test tan integer)
    (test sin rational)
    (test cos rational)
    (test tan rational)))

(defun exercise-float-decoder (type exponent-bits mantissa-bits &optional print)
  (let* ((exp-max (1- (ash 1 (1- exponent-bits))))
         (exp-min (- (1- exp-max)))
         (exp-bias exp-max)
         ;; mantissa-bits excludes the hidden bit
         (total-bits (+ mantissa-bits exponent-bits 1)))
    (labels ((try (sign-bit exponent mantissa)
               (let* ((bit-pattern
                       (logior (ash sign-bit (+ exponent-bits mantissa-bits))
                               (ash (+ exp-bias exponent) mantissa-bits)
                               mantissa))
                      (signed-bits
                       (sb-disassem:sign-extend bit-pattern total-bits))
                      (x (ecase type
                          (single-float
                           (sb-kernel:make-single-float signed-bits))
                          (double-float
                           (sb-kernel:make-double-float (ash signed-bits -32)
                                                        (ldb (byte 32 0) signed-bits))))))
                 (when print
                   (format t "~v,'0b -> ~f~%" total-bits bit-pattern x))
                 (multiple-value-bind (significand exponent sign) (decode-float x)
                   (let ((reconstructed (* significand (expt 2 exponent) sign)))
                     (unless (= reconstructed x)
                       (error "DF -> ~s ~s ~s -> ~f~%" significand exponent sign
                              reconstructed))))
                 (multiple-value-bind (significand exponent sign) (integer-decode-float x)
                   (let ((reconstructed (* significand (expt 2 exponent) sign)))
                     (unless (= reconstructed x)
                       (error "IDF -> ~s ~s ~s -> ~f~%" significand exponent sign
                              reconstructed)))))))
      ;; walking 1 bit
      (loop for exp from exp-min to (1- exp-max)
            do (let ((bit (ash 1 mantissa-bits)))
                 (loop while (/= bit 0)
                       do (try 0 exp (ldb (byte mantissa-bits 0) bit))
                          (setq bit (ash bit -1))))))))

(with-test (:name :test-float-decoders)
  (flet ((test-df (input expect-sig expect-exp expect-sign)
           (multiple-value-bind (significand exponent sign)
               (decode-float input)
             (assert (and (= significand expect-sig)
                          (= exponent expect-exp)
                          (= sign expect-sign)))))
         (test-idf (input expect-sig expect-exp expect-sign)
           (multiple-value-bind (significand exponent sign)
               (integer-decode-float input)
             (assert (and (= significand expect-sig)
                          (= exponent expect-exp)
                          (= sign expect-sign))))))
    (test-df +0s0 0.0s0 0 1.0)
    (test-df -0s0 0.0s0 0 -1.0)
    (test-df +0d0 0.0d0 0 1.0d0)
    (test-df -0d0 0.0d0 0 -1.0d0)
    (test-idf +0s0 0 0 1)
    (test-idf -0s0 0 0 -1)
    (test-idf +0d0 0 0 1)
    (test-idf -0d0 0 0 -1)
    (test-idf least-positive-normalized-single-float 8388608 -149 1)
    (test-idf least-negative-normalized-single-float 8388608 -149 -1)
    (test-idf least-positive-normalized-double-float 4503599627370496 -1074 1)
    (test-idf least-negative-normalized-double-float 4503599627370496 -1074 -1))
  (exercise-float-decoder 'single-float  8 23)
  (exercise-float-decoder 'double-float 11 52)
  ;; TODO: test denormals
  )


(with-test (:name :conservative-floor-bounds)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            `(lambda (x)
               (declare (unsigned-byte x))
               (values (truncate 1.0 x)))))
          '(function (unsigned-byte) (values unsigned-byte &optional)))))

(with-test (:name :single-float-sign-stubs)
  (checked-compile-and-assert
   ()
   '(lambda (p1)
     (declare (type (eql -96088.234) p1))
     (float-sign
      (the single-float
       (labels ((%f () (the real p1))) (%f)))))
   ((-96088.234) -1.0)))

(with-test (:name :inline-signum)
  (assert (ctu:find-named-callees ; should be a full call
           (compile nil '(lambda (x)
                           (signum (truly-the number x))))))
  ;; should not be a full call
  (dolist (type '(integer
                  (or (integer 1 10) (integer 50 90))
                  rational
                  single-float
                  (or (single-float -10f0 0f0) (single-float 1f0 20f0))
                  double-float
                  (or (double-float -10d0 0d0) (double-float 1d0 20d0))))
    (assert (null (ctu:find-named-callees
                   (compile nil `(lambda (x)
                                   (signum (truly-the ,type x))))))))
  ;; check signed zero
  (let ((f (compile nil '(lambda (x) (signum (the single-float x))))))
    (assert (eql (funcall f -0f0) -0f0))
    (assert (eql (funcall f +0f0) +0f0)))
  (let ((f (compile nil '(lambda (x) (signum (the double-float x))))))
    (assert (eql (funcall f -0d0) -0d0))
    (assert (eql (funcall f +0d0) +0d0))))


(with-test (:name :expt-double-no-complex)
  (checked-compile-and-assert
      (:allow-notes nil)
      `(lambda (x y)
         (> (expt (the double-float x) 4d0)
            (the double-float y)))
    ((1d0 0d0) t))
  (checked-compile-and-assert
      (:allow-notes nil)
      `(lambda (x y)
         (> (expt (the (double-float 0d0) x) (the double-float y))
            y))
    ((1d0 0d0) t)))

(with-test (:name :ftruncate-inline
            :skipped-on (not :64-bit))
  (checked-compile
   `(lambda (v d)
      (declare (optimize speed)
               (double-float d)
               ((simple-array double-float (2)) v))
      (setf (aref v 0) (ffloor (aref v 0) d))
      v)
   :allow-notes nil))

(with-test (:name :ctype-of-nan)
  (checked-compile '(lambda () #.(sb-kernel:make-single-float -1))))

;; bug #1914094
(with-test (:name :float-type-derivation :skipped-on (not :64-bit))
  (labels ((car-type-equal (x y)
             (and (subtypep (car x) (car y))
                  (subtypep (car y) (car x)))))
    (let ((long #+long-float 'long-float
                #-long-float 'double-float))
      (checked-compile-and-assert () '(lambda (x) (ctu:compiler-derived-type (* 3d0 x)))
        ((1) (values `(or ,long (complex ,long)) t) :test #'car-type-equal))
      (checked-compile-and-assert () '(lambda (x) (ctu:compiler-derived-type (* 3f0 x)))
        ((1) (values `(or single-float ,long (complex single-float) (complex ,long)) t)
         :test #'car-type-equal))
      (checked-compile-and-assert () '(lambda (x) (ctu:compiler-derived-type (* 3f0 x)))
        ((1) (values `(or single-float ,long (complex single-float) (complex ,long)) t)
         :test #'car-type-equal))
      (checked-compile-and-assert () '(lambda (x y) (ctu:compiler-derived-type (atan x y)))
        ((1 2) (values `(or ,long single-float (complex ,long) (complex single-float)) t) :test #'car-type-equal)))))

(with-test (:name :comparison-transform-overflow)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare (float a))
      (= a 1854150818890592943838975159000134470424763027560))
   ((1d0) nil)
   ((1f0) nil)))

(with-test (:name :comparison-merging)
  (checked-compile-and-assert
   ()
   `(lambda (a b)
      (declare (double-float a b))
      (cond ((= a b) 0)
            ((< a b) 1)
            (t 2)))
   ((1d0 1d0) 0)
   ((1d0 3d0) 1)
   ((3d0 1d0) 2)))

;; Based on example in lp#1926383
(defun idf (x) (multiple-value-list (cl:integer-decode-float x)))
(defun testfloat (k)
  (let* ((kidf (idf k))
         (kff (float (* (car kidf) (expt 2 (cadr kidf))) k))
         (kss (scale-float (float (car kidf) k) (cadr kidf))))
    (format t "Input k(~a): ~,15e, IDF ~{~b ~d ~d~}~%" (type-of k) k kidf)
    (format t "float k(~a): ~,15e, IDF ~{~b ~d ~d~}, diff ~,5e~%" (type-of k) kff (idf kff) (- k kff))
    (format t "scale k(~a): ~,15e, IDF ~{~b ~d ~d~}, diff ~,5e~%" (type-of k) kff (idf kss) (- k kss))))

;;; (time (exhaustive-test-single-floats))
;;; Evaluation took:
;;;   12.873 seconds of real time
;;;   12.666938 seconds of total run time (12.629706 user, 0.037232 system)
;;;   [ Run times consist of 0.055 seconds GC time, and 12.612 seconds non-GC time. ]
;;;   98.40% CPU
;;;   36,149,296,946 processor cycles
;;;   5,033,148,304 bytes consed
;;;
#+nil ; This is too slow to be a regression test. And why does it cons?
(defun exhaustive-test-single-floats ()
  (loop for i from 1 to (1- (ash 1 23))
     do (let ((k (sb-kernel:make-lisp-obj (logior (ash i 32) sb-vm:single-float-widetag))))
          (multiple-value-bind (mant exp sign) (integer-decode-float k)
            (declare (ignore sign))
            (let ((way1 (float (* mant (expt 2 exp)) k))
                  (way2 (scale-float (float mant k) exp)))
              ;; Do bitwise comparison
              (assert (= (sb-kernel:single-float-bits k)
                         (sb-kernel:single-float-bits way1)))
              (assert (= (sb-kernel:single-float-bits k)
                         (sb-kernel:single-float-bits way2))))))))

;;; For #+64-bit we could eradicate the legacy interface
;;; to MAKE-DOUBLE-FLOAT, and just take the bits.
(defun mdf (x)
  (let ((f (sb-sys:%primitive sb-vm::fixed-alloc
                              'make-double-float 2 sb-vm:double-float-widetag
                              sb-vm:other-pointer-lowtag nil)))
    (setf (sb-sys:sap-ref-word (sb-sys:int-sap (sb-kernel:get-lisp-obj-address f))
                               (- 8 sb-vm:other-pointer-lowtag))
          (the sb-vm:word x))
    f))
(compile 'mdf)

#+64-bit
(progn
(defun test-single-floats (n)
  (dotimes (i n)
    (let* ((bits (random (ash 1 23)))
           ;; This isn't a valid call to MAKE-LISP-OBJ for 32 bit words
           (k (sb-kernel:make-lisp-obj (logior (ash i 32) sb-vm:single-float-widetag))))
      (when (zerop bits) (incf bits))
      (multiple-value-bind (mant exp sign) (integer-decode-float k)
        (declare (ignore sign))
        (let ((way1 (float (* mant (expt 2 exp)) k))
              (way2 (scale-float (float mant k) exp)))
          ;; Do bitwise comparison
          (assert (= (sb-kernel:single-float-bits k)
                     (sb-kernel:single-float-bits way1)))
          (assert (= (sb-kernel:single-float-bits k)
                     (sb-kernel:single-float-bits way2))))))))

(defun test-double-floats (n)
  (dotimes (i n)
    (let ((bits (random (ash 1 52))))
      (when (zerop bits) (incf bits))
      (let ((k (mdf bits)))
        (multiple-value-bind (mant exp sign) (integer-decode-float k)
          (declare (ignore sign))
          (let ((way1 (float (* mant (expt 2 exp)) k))
                (way2 (scale-float (float mant k) exp)))
            ;; Do bitwise comparison
            (assert (= (sb-kernel:double-float-bits k)
                       (sb-kernel:double-float-bits way1)))
            (assert (= (sb-kernel:double-float-bits k)
                       (sb-kernel:double-float-bits way2)))))))))

(with-test (:name :round-trip-decode-recompose)
  (test-single-floats 10000)
  (test-double-floats 10000))
)

;; lp#1920931
(with-test (:name :coerce-to-float-no-warning)
  (let ((f (checked-compile '(lambda (y) (coerce (sqrt y) 'float)))))
    (assert (floatp (funcall f 3)))
    (assert-error (funcall f #c(1 2)))))

(with-test (:name :imagpart-real-negative-zero-derived-type)
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (eql (imagpart (the real x)) -0.0))
   ((-1.0) t)))

(with-test (:name :negative-zero-in-ranges)
  (checked-compile-and-assert
      ()
      `(lambda (x y)
         (declare ((OR (INTEGER 0 0) (DOUBLE-FLOAT 0.0d0 0.0d0)) x)
                  ((OR (RATIONAL -10 0) (DOUBLE-FLOAT -10.0d0 -0.0d0)) y))
         (= x y))
  ((0 0) t)
  ((0 0d0) t)
  ((0 -0d0) t)
  ((0d0 -0d0) t)
  ((0 -1d0) nil)))
