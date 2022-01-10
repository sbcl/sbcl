;;;; arithmetic tests with no side effects

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

;;; Once upon a time, in the process of porting CMUCL's SPARC backend
;;; to SBCL, multiplications were excitingly broken.  While it's
;;; unlikely that anything with such fundamental arithmetic errors as
;;; these are going to get this far, it's probably worth checking.
(with-test (:name (:fundamental-arithmetic :smoke))
  (macrolet ((test (op res1 res2)
               `(progn
                  (assert (= (,op 4 2) ,res1))
                  (assert (= (,op 2 4) ,res2))
                  (assert (= (funcall (checked-compile '(lambda (x y) (,op x y)))
                                      4 2)
                             ,res1))
                  (assert (= (funcall (checked-compile '(lambda (x y) (,op x y)))
                                      2 4)
                             ,res2)))))
    (test + 6 6)
    (test - 2 -2)
    (test * 8 8)
    (test / 2 1/2)
    (test expt 16 16)))

;;; In a bug reported by Wolfhard Buss on cmucl-imp 2002-06-18 (BUG
;;; 184), sbcl didn't catch all divisions by zero, notably divisions
;;; of bignums and ratios by 0.  Fixed in sbcl-0.7.6.13.
(with-test (:name (/ :division-by-zero ratio))
  (checked-compile-and-assert (:allow-style-warnings t)
      '(lambda () (/ 2/3 0))
    (() (condition 'division-by-zero))))

(with-test (:name (/ :division-by-zero bignum))
  (checked-compile-and-assert (:allow-style-warnings t)
      '(lambda () (/ (1+ most-positive-fixnum) 0))
    (() (condition 'division-by-zero))))

;;; In a bug reported by Raymond Toy on cmucl-imp 2002-07-18, (COERCE
;;; <RATIONAL> '(COMPLEX FLOAT)) was failing to return a complex
;;; float; a patch was given by Wolfhard Buss cmucl-imp 2002-07-19.
(with-test (:name (coerce complex float 1))
  (assert (= (coerce 1 '(complex float)) #c(1.0 0.0)))
  (assert (= (coerce 1/2 '(complex float)) #c(0.5 0.0)))
  (assert (= (coerce 1.0d0 '(complex float)) #c(1.0d0 0.0d0))))

;;; (COERCE #c(<RATIONAL> <RATIONAL>) '(complex float)) resulted in
;;; an error up to 0.8.17.31
(with-test (:name (coerce complex float 2))
  (assert (= (coerce #c(1 2) '(complex float)) #c(1.0 2.0))))

;;; COERCE also sometimes failed to verify that a particular coercion
;;; was possible (in particular coercing rationals to bounded float
;;; types.
(with-test (:name (coerce :to float :outside-bounds))
  (checked-compile-and-assert (:allow-style-warnings t)
      '(lambda () (coerce 1 '(float 2.0 3.0)))
    (() (condition 'type-error)))
  (checked-compile-and-assert (:allow-style-warnings t)
      '(lambda () (coerce 1 '(single-float -1.0 0.0)))
    (() (condition 'type-error)))
  (assert (eql (coerce 1 '(single-float -1.0 2.0)) 1.0)))

;;; ANSI says MIN and MAX should signal TYPE-ERROR if any argument
;;; isn't REAL. SBCL 0.7.7 didn't in the 1-arg case. (reported as a
;;; bug in CMU CL on #lisp IRC by lrasinen 2002-09-01)
(with-test (:name (min max type-error))
  (checked-compile-and-assert (:allow-warnings t :allow-style-warnings t)
      '(lambda () (min '(1 2 3)))
    (() (condition 'type-error)))
  (assert (= (min -1) -1))
  (checked-compile-and-assert (:allow-warnings t :allow-style-warnings t)
      '(lambda () (min 1 #(1 2 3)))
    (() (condition 'type-error)))
  (assert (= (min 10 11) 10))
  (checked-compile-and-assert (:allow-warnings t :allow-style-warnings t)
      '(lambda () (min (find-package "CL") -5.0))
    (() (condition 'type-error)))
  (assert (= (min 5.0 -3) -3))
  (checked-compile-and-assert (:allow-warnings t :allow-style-warnings t)
      '(lambda () (max #c(4 3)))
    (() (condition 'type-error)))
  (assert (= (max 0) 0))
  (checked-compile-and-assert (:allow-warnings t :allow-style-warnings t)
      '(lambda () (max "MIX" 3))
    (() (condition 'type-error)))
  (assert (= (max -1 10.0) 10.0))
  (checked-compile-and-assert (:allow-warnings t :allow-style-warnings t)
      '(lambda () (max 3 #'max))
    (() (condition 'type-error)))
  (assert (= (max -3 0) 0)))

(with-test (:name :numeric-inequality-&rest-arguments)
  (dolist (f '(= < <= > >=))
    ;; 1 arg
    (assert-error (funcall f 'feep) type-error)
    (unless (eq f '=)
      ;; = accepts complex numbers
      (assert-error (funcall f #c(0s0 1s0)) type-error))
    ;; 2 arg
    (assert-error (funcall f 3 'feep) type-error)
    (assert-error (funcall f 'feep 3) type-error)
    ;; 3 arg
    (assert-error (funcall f 0 0 'feep) type-error)
    (assert-error (funcall f 0 1 'feep) type-error)
    (assert-error (funcall f 1 0 'feep) type-error)
    ;; 4 arg
    (assert-error (funcall f 0 0 0 'feep) type-error))
  ;; Also MIN,MAX operate only on REAL
  (dolist (f '(min max))
    (assert-error (funcall f #c(1s0 -2s0)) type-error)))

;;; (CEILING x 2^k) was optimized incorrectly
(with-test (:name (ceiling :power-of-two))
  (loop for divisor in '(-4 4)
        for ceiler = (checked-compile `(lambda (x)
                                         (declare (fixnum x))
                                         (declare (optimize (speed 3)))
                                         (ceiling x ,divisor)))
        do (loop for i from -5 to 5
                 for exact-q = (/ i divisor)
                 do (multiple-value-bind (q r)
                        (funcall ceiler i)
                      (assert (= (+ (* q divisor) r) i))
                      (assert (<= exact-q q))
                      (assert (< q (1+ exact-q)))))))

;;; (TRUNCATE x 2^k) was optimized incorrectly
(with-test (:name (truncate :power-of-two))
  (loop for divisor in '(-4 4)
        for truncater = (checked-compile `(lambda (x)
                                            (declare (fixnum x))
                                            (declare (optimize (speed 3)))
                                            (truncate x ,divisor)))
        do (loop for i from -9 to 9
                 for exact-q = (/ i divisor)
                 do (multiple-value-bind (q r)
                        (funcall truncater i)
                      (assert (= (+ (* q divisor) r) i))
                      (assert (<= (abs q) (abs exact-q)))
                      (assert (< (abs exact-q) (1+ (abs q))))))))

;;; CEILING had a corner case, spotted by Paul Dietz
(with-test  (:name (ceiling :corner-case))
  (assert (= (ceiling most-negative-fixnum (1+ most-positive-fixnum)) -1)))

;;; give any optimizers of constant multiplication a light testing.
;;; 100 may seem low, but (a) it caught CSR's initial errors, and (b)
;;; before checking in, CSR tested with 10000.  So one hundred
;;; checkins later, we'll have doubled the coverage.
(with-test (:name (* :multiplication :constant :optimization))
  (dotimes (i 100)
    (let* ((x (random most-positive-fixnum))
           (x2 (* x 2))
           (x3 (* x 3))
           (fn (checked-compile
                `(lambda (y)
                   (declare (optimize speed) (type (integer 0 3) y))
                   (* y ,x))
                :allow-notes (> x3 most-positive-fixnum))))
      (assert (= (funcall fn 0) 0))
      (assert (= (funcall fn 1) x))
      (assert (= (funcall fn 2) x2))
      (assert (= (funcall fn 3) x3)))))

;;; Bugs reported by Paul Dietz:

;;; (GCD 0 x) must return (abs x)
(with-test (:name (gcd 0))
  (dolist (x (list -10 (* 3 most-negative-fixnum)))
    (assert (= (gcd 0 x) (abs x)))))

;;; LCM returns a non-negative number
(with-test (:name (lcm :non-negative))
  (assert (= (lcm 4 -10) 20))
  (assert (= (lcm 0 0) 0)))

;;; PPC bignum arithmetic bug:
(with-test (:name (truncate bignum :ppc))
  (multiple-value-bind (quo rem)
      (truncate 291351647815394962053040658028983955 10000000000000000000000000)
    (assert (= quo 29135164781))
    (assert (= rem 5394962053040658028983955))))

;;; x86 LEA bug:
(with-test (:name (:x86 :lea))
  (checked-compile-and-assert ()
      '(lambda (x) (declare (bit x)) (+ x #xf0000000))
    ((1) #xf0000001)))

(with-test (:name (logbitp bignum))
  (dolist (x '(((1+ most-positive-fixnum) 1 nil)
               ((1+ most-positive-fixnum) -1 t)
               ((1+ most-positive-fixnum) (1+ most-positive-fixnum) nil)
               ((1+ most-positive-fixnum) (1- most-negative-fixnum) t)
               (1 (ash most-negative-fixnum 1) nil)
               (#.(- sb-vm:n-word-bits sb-vm:n-fixnum-tag-bits 1) most-negative-fixnum t)
               (#.(1+ (- sb-vm:n-word-bits sb-vm:n-fixnum-tag-bits 1)) (ash most-negative-fixnum 1) t)
               (#.(+ 2 (- sb-vm:n-word-bits sb-vm:n-fixnum-tag-bits 1)) (ash most-negative-fixnum 1) t)
               (#.(+ sb-vm:n-word-bits 32) (ash most-negative-fixnum #.(+ 32 sb-vm:n-fixnum-tag-bits 2)) nil)
               (#.(+ sb-vm:n-word-bits 33) (ash most-negative-fixnum #.(+ 32 sb-vm:n-fixnum-tag-bits 2)) t)))
    (destructuring-bind (index int result) x
      (assert (eq (eval `(logbitp ,index ,int)) result)))))

;;; off-by-1 type inference error for %DPB and %DEPOSIT-FIELD:
(with-test (:name (dpb deposit-field :off-by-one))
  (checked-compile-and-assert ()
      '(lambda (b)
         (integer-length (dpb b (byte 4 28) -1005)))
    ((1230070) 32))
  (checked-compile-and-assert ()
      '(lambda (b)
         (integer-length (deposit-field b (byte 4 28) -1005)))
    ((1230070) 32)))

;;; type inference leading to an internal compiler error:
(with-test (:name (ldb byte 0 :type-inference))
  (checked-compile-and-assert ()
      '(lambda (x)
         (declare (type fixnum x))
         (ldb (byte 0 0) x))
    ((1)                    0)
    ((most-positive-fixnum) 0)
    ((-1)                   0)))

;;; Alpha bignum arithmetic bug:
(with-test (:name (bignum :arithmetic :alpha))
  (assert (= (* 966082078641 419216044685) 404997107848943140073085)))

;;; Alpha smallnum arithmetic bug:
(with-test (:name (fixnum :arithmetc :alpha))
  (assert (= (ash -129876 -1026) -1)))

;;; Alpha middlenum (yes, really! Affecting numbers between 2^32 and
;;; 2^64 :) arithmetic bug
(with-test (:name (truncate :middlenum))
  (checked-compile-and-assert ()
      '(lambda (a b c d)
         (declare (type (integer -1621 -513) a)
                  (type (integer -3 34163) b)
                  (type (integer -9485132993 81272960) c)
                  (type (integer -255340814 519943) d)
                  (ignorable a b c d))
         (truncate c (min -100 4149605)))
    ((-1332 5864 -6963328729 -43789079) (values 69633287 -29))))

;;; Here's another fantastic Alpha backend bug: the code to load
;;; immediate 64-bit constants into a register was wrong.
(with-test (:name (dpb :constants))
  (checked-compile-and-assert ()
      '(lambda (a b c d)
         (declare (type (integer -3563 2733564) a)
                  (type (integer -548947 7159) b)
                  (type (integer -19 0) c)
                  (type (integer -2546009 0) d)
                  (ignorable a b c d))
         (case a
           ((89 125 16) (ash a (min 18 -706)))
           (t (dpb -3 (byte 30 30) -1))))
    ((1227072 -529823 -18 -792831) -2147483649)))

;;; ASH of a negative bignum by a bignum count would erroneously
;;; return 0 prior to sbcl-0.8.4.4
(with-test (:name (ash :negative bignum))
  (assert (= (ash (1- most-negative-fixnum) (1- most-negative-fixnum)) -1)))

;;; Whoops.  Too much optimization in division operators for 0
;;; divisor.
(with-test (:name (mod truncate rem / floor ceiling :division-by-zero fixnum))
  (flet ((frob (name)
           (let ((fn (checked-compile
                      `(lambda (x)
                         (declare (optimize speed) (fixnum x))
                         (,name x 0)))))
             (assert-error (funcall fn 1) division-by-zero))))
    (mapc #'frob '(mod truncate rem / floor ceiling))))

;; Check that the logic in SB-KERNEL::BASIC-COMPARE for doing fixnum/float
;; comparisons without rationalizing the floats still gives the right anwers
;; in the edge cases (had a fencepost error).
(macrolet ((test (range type sign)
             `(let (ints
                    floats
                    (start (- ,(find-symbol (format nil
                                                    "MOST-~A-EXACTLY-~A-FIXNUM"
                                                    sign type)
                                            :sb-kernel)
                              ,range)))
                (dotimes (i (1+ (* ,range 2)))
                  (let* ((x (+ start i))
                         (y (coerce x ',type)))
                    (push x ints)
                    (push y floats)))
                (dolist (i ints)
                  (dolist (f floats)
                    (dolist (op '(< <= = >= >))
                      (unless (eq (funcall op i f)
                                  (funcall op i (rationalize f)))
                        (error "(not (eq (~a ~a ~f) (~a ~a ~a)))~%"
                               op i f
                               op i (rationalize f)))
                      (unless (eq (funcall op f i)
                                  (funcall op (rationalize f) i))
                        (error "(not (eq (~a ~f ~a) (~a ~a ~a)))~%"
                               op f i
                               op (rationalize f) i))))))))
  (test 32 double-float negative)
  (test 32 double-float positive)
  (test 32 single-float negative)
  (test 32 single-float positive))

;; x86-64 sign-extension bug found using pfdietz's random tester.
(with-test (:name (:x86-64 :sign-extension))
  (assert (= 286142502
             (funcall (lambda ()
                        (declare (notinline logxor))
                        (min (logxor 0 0 0 286142502)))))))

;; Small bugs in LOGCOUNT can still allow SBCL to be built and thus go
;; unnoticed, so check more thoroughly here.
(with-test (:name logcount)
  (flet ((test (x n)
           (unless (= (logcount x) n)
             (error "logcount failure for ~a" x))))
    ;; Test with some patterns with well known number of ones/zeroes ...
    (dotimes (i 128)
      (let ((x (ash 1 i)))
        (test x 1)
        (test (- x) i)
        (test (1- x) i)))
    ;; ... and with some random integers of varying length.
    (flet ((test-logcount (x)
             (declare (type integer x))
             (do ((result 0 (1+ result))
                  (x (if (minusp x)
                         (lognot x)
                         x)
                     (logand x (1- x))))
                 ((zerop x) result))))
      (dotimes (i 200)
        (let ((x (random (ash 1 i))))
          (test x (test-logcount x))
          (test (- x) (test-logcount (- x))))))))

;; 1.0 had a broken ATANH on win32
(with-test (:name atanh)
  (assert (= (atanh 0.9d0) 1.4722194895832204d0)))

;; Test some cases of integer operations with constant arguments
(with-test (:name :constant-integers)
  (labels ((test-forms (op x y header &rest forms)
             (let ((val (funcall op x y)))
               (dolist (form forms)
                 (let ((new-val (funcall (checked-compile (append header form)) x y)))
                   (unless (eql val new-val)
                     (error "~S /= ~S: ~S ~S ~S~%" val new-val (append header form) x y))))))
           (test-case (op x y type)
             (test-forms op x y `(lambda (x y &aux z)
                                   (declare (type ,type x y)
                                            (ignorable x y z)
                                            (notinline identity)
                                            (optimize speed (safety 0))))
                         `((,op x ,y))
                         `((setf z (,op x ,y))
                           (identity x)
                           z)
                         `((values (,op x ,y) x))
                         `((,op ,x y))
                         `((setf z (,op ,x y))
                           (identity y)
                           z)
                         `((values (,op ,x y) y))

                         `((identity x)
                           (,op x ,y))
                         `((identity x)
                           (setf z (,op x ,y))
                           (identity x)
                           z)
                         `((identity x)
                           (values (,op x ,y) x))
                         `((identity y)
                           (,op ,x y))
                         `((identity y)
                           (setf z (,op ,x y))
                           (identity y)
                           z)
                         `((identity y)
                           (values (,op ,x y) y))))
           (test-op (op)
             (let ((ub `(unsigned-byte ,sb-vm:n-word-bits))
                   (sb `(signed-byte ,sb-vm:n-word-bits)))
               (loop for (x y type)
                     in `((2 1 fixnum)
                          (2 1 ,ub)
                          (2 1 ,sb)
                          (,(1+ (ash 1 28)) ,(1- (ash 1 28)) fixnum)
                          (,(+ 3 (ash 1 30)) ,(+ 2 (ash 1 30)) ,ub)
                          (,(- -2 (ash 1 29)) ,(- 3 (ash 1 29)) ,sb)
                          ,@(when (> sb-vm:n-word-bits 32)
                              `((,(1+ (ash 1 29)) ,(1- (ash 1 29)) fixnum)
                                (,(1+ (ash 1 31)) ,(1- (ash 1 31)) ,ub)
                                (,(- -2 (ash 1 31)) ,(- 3 (ash 1 30)) ,sb)
                                (,(ash 1 40) ,(ash 1 39) fixnum)
                                (,(ash 1 40) ,(ash 1 39) ,ub)
                                (,(ash 1 40) ,(ash 1 39) ,sb)))
                          ;; fixnums that can be represented as 32-bit
                          ;; sign-extended immediates on x86-64
                          ,@(when (and (> sb-vm:n-word-bits 32)
                                       (< sb-vm:n-fixnum-tag-bits 3))
                              `((,(1+ (ash 1 (- 31 sb-vm:n-fixnum-tag-bits)))
                                 ,(1- (ash 1 (- 32 sb-vm:n-fixnum-tag-bits)))
                                 fixnum))))
                     do
                  (test-case op x y type)
                  (test-case op x x type)))))
    (mapc #'test-op '(+ - * truncate
                      < <= = >= >
                      eql
                      eq))))

;; GCD used to sometimes return negative values. The following did, on 32 bit
;; builds.
(with-test (:name gcd)
  ;; from lp#413680
  (assert (plusp (gcd 20286123923750474264166990598656
                      680564733841876926926749214863536422912)))
  ;; from lp#516750
  (assert (plusp (gcd 2596102012663483082521318626691873
                      2596148429267413814265248164610048))))

(with-test (:name (expt 0 0))
  ;; Check that (expt 0.0 0.0) and (expt 0 0.0) signal error, but
  ;; (expt 0.0 0) returns 1.0
  (flet ((error-case (expr)
           (checked-compile-and-assert (:allow-style-warnings t)
               `(lambda () ,expr)
             (() (condition 'sb-int:arguments-out-of-domain-error)))))
    (error-case '(expt 0.0 0.0))
    (error-case '(expt 0 0.0)))
  (checked-compile-and-assert (:allow-style-warnings t)
      `(lambda () (expt 0.0 0))
    (() 1.0)))

(with-test (:name :multiple-constant-folding)
  (let ((*random-state* (make-random-state t)))
    (flet ((make-args ()
             (let (args vars)
               (loop repeat (1+ (random 12))
                     do (if (zerop (random 2))
                            (let ((var (gensym)))
                              (push var args)
                              (push var vars))
                            (push (- (random 21) 10) args)))
               (values args vars))))
      (dolist (op '(+ * logior logxor logand logeqv gcd lcm - /))
        (loop repeat 10
              do (multiple-value-bind (args vars) (make-args)
                   (let ((fast (checked-compile
                                `(lambda ,vars
                                   (,op ,@args))
                                :allow-style-warnings (eq op '/)))
                         (slow (checked-compile
                                `(lambda ,vars
                                   (declare (notinline ,op))
                                   (,op ,@args))
                                :allow-style-warnings (eq op '/))))
                     (loop repeat 3
                           do (let* ((call-args (loop repeat (length vars)
                                                      collect (- (random 21) 10)))
                                     (fast-result (handler-case
                                                      (apply fast call-args)
                                                    (division-by-zero () :div0)))
                                     (slow-result (handler-case
                                                      (apply slow call-args)
                                                    (division-by-zero () :div0))))
                                (if (not (eql fast-result slow-result))
                                    (error "oops: ~S, ~S" args call-args)
                                    #+nil (print (list :ok `(,op ,@args) :=> fast-result))
                                    ))))))))))

;;; (TRUNCATE <unsigned-word> <constant unsigned-word>) is optimized
;;; to use multiplication instead of division. This propagates to FLOOR,
;;; MOD and REM. Test that the transform is indeed triggered and test
;;; several cases for correct results.
(with-test (:name (:integer-division-using-multiplication :used)
                  :skipped-on (not (or :x86-64 :x86)))
  (dolist (fun '(truncate floor ceiling mod rem))
    (let* ((foo (checked-compile
                 `(lambda (x)
                    (declare (optimize (speed 3)
                                       (space 1)
                                       (compilation-speed 0))
                             (type (unsigned-byte ,sb-vm:n-word-bits) x))
                    (,fun x 9))))
           (disassembly (with-output-to-string (s)
                          (disassemble foo :stream s))))
      ;; KLUDGE copied from test :float-division-using-exact-reciprocal
      ;; in compiler.pure.lisp.
      (assert (and (not (search "DIV" disassembly))
                   (search "MUL" disassembly))))))

(with-test (:name (:integer-division-using-multiplication :correctness))
  (let ((*random-state* (make-random-state t)))
    (dolist (dividend-type `((unsigned-byte ,sb-vm:n-word-bits)
                             (signed-byte ,sb-vm:n-word-bits)
                             (and fixnum unsigned-byte)
                             (integer 10000 10100)
                             fixnum))
      (dolist (divisor `(;; Some special cases from the paper
                         7 10 14 641 274177
                         ;; Range extremes
                         3
                         ,most-positive-fixnum
                         ,(1- (expt 2 sb-vm:n-word-bits))
                         ;; Some random values
                         ,@(loop for i from 8 to sb-vm:n-word-bits
                                 for r = (random (expt 2 i))
                                 ;; We don't want 0, 1 and powers of 2.
                                 when (not (zerop (logand r (1- r))))
                                 collect r
                                 and
                                 collect (- r))))
        (dolist (fun '(truncate ceiling floor mod rem))
          (let ((foo (checked-compile
                      `(lambda (x)
                         (declare (optimize (speed 3)
                                            (space 1)
                                            (compilation-speed 0))
                                  (type ,dividend-type x))
                         (,fun x ,divisor)))))
            (dolist (dividend `(0 1 ,most-positive-fixnum
                                ,(1- divisor) ,divisor
                                ,(1- (* divisor 2)) ,(* divisor 2)
                                ,@(loop repeat 4
                                        collect (+ 10000 (random 101)))
                                ,@(loop for i from 4 to sb-vm:n-word-bits
                                        for pow = (expt 2 (1- i))
                                        for r = (+ pow (random pow))
                                        collect r
                                        collect (- r))))
              (when (typep dividend dividend-type)
                (multiple-value-bind (q1 r1)
                    (funcall foo dividend)
                  (multiple-value-bind (q2 r2)
                      (funcall fun dividend divisor)
                    (unless (and (= q1 q2)
                                 (eql r1 r2))
                      (error "bad results for ~s with dividend type ~s"
                             (list fun dividend divisor)
                             dividend-type))))))))))))

;; The fast path for logbitp underestimated sb-vm:n-positive-fixnum-bits
;; for > 61 bit fixnums.
(with-test (:name (logbitp :wide fixnum))
  (assert (not (logbitp (1- (integer-length most-positive-fixnum))
                        most-negative-fixnum))))

;; EXPT dispatches in a complicated way on the types of its arguments.
;; Check that all possible combinations are covered.
(with-test (:name (expt :argument-type-combinations))
  (let ((numbers '(2                 ; fixnum
                   3/5               ; ratio
                   1.2f0             ; single-float
                   2.0d0             ; double-float
                   #c(3/5 1/7)       ; complex rational
                   #c(1.2f0 1.3f0)   ; complex single-float
                   #c(2.0d0 3.0d0))) ; complex double-float
        (bignum (expt 2 64))
        results)
    (dolist (base (cons bignum numbers))
      (dolist (power numbers)
        #+nil (format t "(expt ~s ~s) => " base power)
        (let ((result (expt base power)))
          #+nil (format t "~s~%" result)
          (push result results))))
    (assert (every #'numberp results))))

(with-test (:name :bug-741564)
  ;; The bug was that in (expt <fixnum> <(complex double-float)>) the
  ;; calculation was partially done only to single-float precision,
  ;; making the complex double-float result too unprecise. Some other
  ;; combinations of argument types were affected, too; test that all
  ;; of them are good to double-float precision.
  (labels ((nearly-equal-p (x y)
             "Are the arguments equal to nearly double-float precision?"
             (declare (type double-float x y))
             (< (/ (abs (- x y)) (abs y))
                (* double-float-epsilon 4))) ; Differences in the two least
                                             ; significant mantissa bits
                                             ; are OK.
           (test-complex (x y)
             (and (nearly-equal-p (realpart x) (realpart y))
                  (nearly-equal-p (imagpart x) (imagpart y))))
           (print-result (msg base power got expected)
             (declare (ignorable msg base power got expected))
             #+nil
             (format t "~a (expt ~s ~s)~%got      ~s~%expected ~s~%"
                     msg base power got expected)))
    (let ((n-broken 0))
      (flet ((test (base power coerce-to-type)
               (let* ((got (expt base power))
                      (expected (expt (coerce base coerce-to-type) power))
                      (result (test-complex got expected)))
                 (print-result (if result "Good:" "Bad:")
                               base power got expected)
                 (unless result
                   (incf n-broken)))))
        (dolist (base (list 2                    ; fixnum
                            (expt 2 64)          ; bignum
                            3/5                  ; ratio
                            2.0f0))              ; single-float
          (let ((power #c(-2.5d0 -4.5d0)))       ; complex double-float
            (test base power 'double-float)))
        (dolist (base (list #c(2.0f0 3.0f0)      ; complex single-float
                            #c(2 3)              ; complex fixnum
                            (complex (expt 2 64) (expt 2 65))
                                                 ; complex bignum
                            #c(3/5 1/7)))        ; complex ratio
          (dolist (power (list #c(-2.5d0 -4.5d0) ; complex double-float
                               -2.5d0))          ; double-float
            (test base power '(complex double-float)))))
      (when (> n-broken 0)
        (error "Number of broken combinations: ~a" n-broken)))))

(with-test (:name (ldb :rlwinm :ppc))
  (let ((one (checked-compile '(lambda (a) (ldb (byte 9 27) a))))
        (two (checked-compile '(lambda (a)
                                 (declare (type (integer -3 57216651) a))
                                 (ldb (byte 9 27) a)))))
    (assert (= 0 (- (funcall one 10) (funcall two 10))))))

;; The ISQRT implementation is sufficiently complicated that it should
;; be tested.
(with-test (:name isqrt)
  (labels ((test (x)
             (let* ((r (isqrt x))
                    (r2 (expt r 2))
                    (s2 (expt (1+ r) 2)))
               (unless (and (<= r2 x)
                            (> s2 x))
                 (error "isqrt failure for ~a" x))))
           (tests (x)
             (test x)
             (let ((x2 (expt x 2)))
               (test x2)
               (test (1+ x2))
               (test (1- x2)))))
    (test most-positive-fixnum)
    (test (1+ most-positive-fixnum))
    (loop for i from 1 to 200
          for pow = (expt 2 (1- i))
          for j = (+ pow (random pow))
          do
          (tests i)
          (tests j))
    (dotimes (i 10)
      (tests (random (expt 2 (+ 1000 (random 10000))))))))

;; bug 1026634 (reported by Eric Marsden on sbcl-devel)
(with-test (:name :recursive-cut-to-width)
  (checked-compile-and-assert (:optimize '(:speed t :safety t :debug t :space t))
      '(lambda (x)
         (declare (type (integer 12417236377505266230
                                 12417274239874990070)
                        x))
         (logand 8459622733968096971 x))
    ((12417237222845306758) 2612793697039849090)))

;; Also reported by Eric Marsden on sbcl-devel (2013-06-06)
(with-test (:name (:recursive-cut-to-width :more))
  (checked-compile-and-assert ()
      '(lambda (a b)
         (logand (the (eql 16779072918521075607) a)
                 (the (integer 21371810342718833225 21371810343571293860) b)))
    ((16779072918521075607 21371810342718833263) 2923729245085762055)))

(with-test (:name (ldb :negative-index-no-error))
  (checked-compile-and-assert ()
      '(lambda (x y) (ldb (byte x y) 100))
    ((-1 -2) (condition 'error)))
  (checked-compile-and-assert ()
      '(lambda (x y) (mask-field (byte x y) 100))
    ((-1 -2) (condition 'error)))
  (checked-compile-and-assert ()
      '(lambda (x y) (dpb 0 (byte x y) 100))
    ((-1 -2) (condition 'error)))
  (checked-compile-and-assert ()
      '(lambda (x y) (deposit-field 0 (byte x y) 100))
    ((-1 -2) (condition 'error))))

(with-test (:name (mask-field setf))
  (checked-compile-and-assert ()
      '(lambda (a)
         (setf (mask-field (byte 2 0) a) 1) a)
    ((15) 13)))

(with-test (:name :complex-multiply)
  (checked-compile-and-assert ()
      '(lambda ()
         (let (z)
           (expt (setf z (complex -0.123 -0.789)) 2)))
    (() #C(-0.60739195 0.194094))))

(with-test (:name (sqrt complex))
  (assert (= (expt (sqrt least-negative-double-float) 2)
             least-negative-double-float)))

(with-test (:name (ldb :sign))
  (checked-compile-and-assert ()
      `(lambda (x)
         (ldb (byte ,(1- sb-vm:n-word-bits) 0) x))
    ((12) 12)))

(with-test (:name :mod-arith-large-constant)
  (checked-compile-and-assert ()
      '(lambda (x)
         (declare (sb-ext:word x))
         (logand sb-ext:most-positive-word
                 (+ x 2312423423)))
    ((12) 2312423435)))

(with-test (:name (bignum :ash :left fixnum))
  (assert (= (eval '(ash most-negative-fixnum (1- sb-vm:n-word-bits)))
             (eval '(* most-negative-fixnum (expt 2 (1- sb-vm:n-word-bits)))))))

(with-test (:name (ldb fixnum :sign-bits))
  (checked-compile-and-assert ()
      '(lambda (x)
         (declare (fixnum x))
         (ldb (byte (/ sb-vm:n-word-bits 2)
                    (/ sb-vm:n-word-bits 2))
              x))
    ((most-positive-fixnum) (ash most-positive-fixnum (- (/ sb-vm:n-word-bits 2))))
    ((-1)                   (1- (expt 2 (/ sb-vm:n-word-bits 2))))))

(with-test (:name (dpb :sign-bits))
  (checked-compile-and-assert ()
      '(lambda (x)
         (declare (fixnum x))
         (dpb 1 (byte (/ sb-vm:n-word-bits 2)
                      (/ sb-vm:n-word-bits 2))
              x))
    ((-1)
     (logior (ash 1 (/ sb-vm:n-word-bits 2))
             (logandc2 -1
                       (mask-field (byte (/ sb-vm:n-word-bits 2)
                                         (/ sb-vm:n-word-bits 2))
                                   -1))))
    ((most-positive-fixnum)
     (logior (ash 1 (/ sb-vm:n-word-bits 2))
             (logandc2 most-positive-fixnum
                       (mask-field (byte (/ sb-vm:n-word-bits 2)
                                         (/ sb-vm:n-word-bits 2))
                                   -1))))))

(with-test (:name (dpb :position-zero))
  (checked-compile-and-assert ()
      '(lambda (x)
         (declare (sb-vm:word x))
         (dpb 0 (byte (/ sb-vm:n-word-bits 2) 0) x))
    ((1) 0)
    ((sb-ext:most-positive-word)
     (logxor sb-ext:most-positive-word
             (1- (expt 2 (/ sb-vm:n-word-bits 2)))))))

(with-test (:name (logand :mask-word))
  (checked-compile-and-assert ()
      '(lambda (x)
         (logand x (ash sb-ext:most-positive-word -1)))
    ((-1) (ash most-positive-word -1))))

(with-test (:name (/ complex real single-float))
  (checked-compile-and-assert ()
      '(lambda (b)
         (declare (type single-float b))
         (/ #c(1.0 2.0) b))
    ((1.0) #c(1.0 2.0))))

(with-test (:name (ash :unsigned))
  (checked-compile-and-assert ()
      '(lambda (x)
         (declare (sb-vm:signed-word x))
         (ash x -64))
    (( 123)  0)
    ((-321) -1)))

(with-test (:name :64-bit-logcount)
  (checked-compile-and-assert
      ()
      '(lambda (x)
        (declare (type (unsigned-byte 54) x))
        (logcount x))
    (((1- (ash 1 24))) 24)
    (((1- (ash 1 32))) 32)
    (((1- (ash 1 48))) 48)
    (((1- (ash 1 54))) 54)))

(with-test (:name :complex-rational-eql)
  ;; ensure no funny business with constant sharing so that we're forced
  ;; to do the most general form or EQL on two (complex rational)s
  (eql (read-from-string "#c(1/2 1/3)") (read-from-string "#c(1/2 1/3)")))

(with-test (:name :gcd-derive-type)
  (checked-compile-and-assert
   ()
   '(lambda (a)
     (declare (type (integer -142 -29) a))
     (eql (gcd (setq a -29) a) 1))
   ((-33) nil)))

;;; Test that LOGIOR and LOGXOR on PPC64 can correctly perform the constant 2nd-arg
;;; vop variant on a 32-bit immediate using op + op-shifted instructions.
;;; (Probably we should refactor large parts of the test suite by the CPU
;;; architecture so that you can really see clearly the areas of concern
;;; for each backend, but I'm not about to embark on that now)
(with-test (:name :ppc64-logxor-32-bit-const)
  (let ((f (compile nil
                    '(lambda (x)
                      (declare (fixnum x))
                      ;; The asm code went wrong only if the result was not in the same
                      ;; register as the source, so make sure that X is live throughout
                      ;; the test so that each result is in a different register.
                      (let ((a (logxor x #x9516A7))
                            (b (logior x #x2531b4))
                            (c (ash x -1)))
                        (list a b c x))))))
    (let ((result (funcall f 0)))
      (assert (equal result
                     '(#x9516A7 #x2531b4 0 0))))))

(with-test (:name :truncate-by-zero-derivation)
  (assert
   (not (equal (cadr
                (cdaddr (sb-kernel:%simple-fun-type
                         (checked-compile
                          `(lambda ()
                             (truncate 5 0))
                          :allow-style-warnings t))))
               '(integer 0 0)))))

(with-test (:name :truncate-by-zero-derivation.2)
  (checked-compile
   `(lambda (x y)
      (declare (type (unsigned-byte 8) x y)
               (optimize speed (safety 0)))
      (mod x y))
   :allow-notes nil))

(with-test (:name :ash-fixnum)
  (checked-compile-and-assert
   ()
   `(lambda (b)
     (declare (type (integer -2 2) b))
     (ash b (min 13 b)))
   ((-2) -1)
   ((-1) -1)
   ((0) 0)
   ((1) 2)
   ((2) 8)))

(with-test (:name :mod-ash-cut)
  (checked-compile-and-assert
      ()
      `(lambda (b)
         (logand #xFF (ash 1 (the (integer -1000 1000) b))))
    ((1) 2)
    ((500) 0))
  (checked-compile-and-assert
      ()
      `(lambda (x b)
         (logand #xFF (ash (the (unsigned-byte 64) x) (the (integer -1000 1000) b))))
    (((1- (expt 2 64)) -63) 1)
    (((1- (expt 2 64)) -64) 0)))

(with-test (:name :bogus-modular-fun-widths)
  (checked-compile-and-assert
      ()
      `(lambda (b)
         (logorc2 0 (- (isqrt (abs (logand (if b -1 2) 2))))))
    ((t) 0)
    ((nil) 0)))

(with-test (:name :lognot-type-derive)
  (assert
   (equal (caddr (sb-kernel:%simple-fun-type
                  (checked-compile
                   `(lambda (b)
                      (lognot (if b -1 2))))))
          '(values (or (integer -3 -3) (integer 0 0)) &optional))))

(with-test (:name :logand-minus-1-type-derive)
  (assert
   (equal (caddr (sb-kernel:%simple-fun-type
                   (checked-compile
                    `(lambda (b)
                       (logand #xf (if b -1 2))))))
          '(values (or (integer 2 2) (integer 15 15)) &optional))))

(with-test (:name :ash-vop-liftimes)
  (checked-compile-and-assert
      ()
      `(lambda (A C)
         (declare ((integer 18512171785636 25543390924355) a)
                  ((integer -20485927966480856 54446023204744213) c))
         (dpb a (byte 20 25) (ash a (min 2 c))))
    ((23906959691249 -15632482499364879) 15512918556672)
    ((23906959691249 1) 50697318833122)))


(with-test (:name :ash-modarith-transform-loop)
  (checked-compile-and-assert
      ()
      `(lambda (p1 p2 p3)
         (declare (type (integer * 53) p1)
                  (type number p2)
                  (type
                   (member 21006398744832 16437837094852630852 2251799813685252
                           -1597729350241882 466525164)
                   p3))
         (ldb (byte (the (integer -3642545987372 *) p1) p2) p3))
    ((53 2 21006398744832) 5251599686208)))

(with-test (:name :logcount-negative-fixnum)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (logcount (the fixnum x)))
    ((54) 4)
    ((-54) 4)))

(with-test (:name :mod-ash64-signed)
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (declare (fixnum a b))
         (logand (ash a b) (1+ most-positive-fixnum)))
    ((-1 -3) (1+ most-positive-fixnum))
    ((1 3) 0)))

(with-test (:name :zero-shift-flags)
  (checked-compile-and-assert
      ()
      `(lambda (a m)
         (declare ((integer 0 5000000000) a)
                  (bit m))
         (zerop (ash a m)))
    ((1 0) nil)
    ((0 1) t)))

(with-test (:name :signum-merged-branch-if)
  (checked-compile-and-assert
   ()
   `(lambda (a y)
      (declare (fixnum a y))
      (when (< y 10)
        (signum a)))
   ((1 5) 1)
   ((0 5) 0)
   ((-1 5) -1)))

(with-test (:name :cmov-merged-branch-if)
  (checked-compile-and-assert
   ()
   `(lambda (b c)
      (declare (type (integer 0 7711851432375361987) b))
      (declare (type (integer -2 0) c))
      (let ((v9 c))
        (if (< c v9)
            c
            (if (= v9 c)
                v9
                b))))
   ((0 -1) -1)))

(with-test (:name :ash-amount-unsigned-comparison)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare (type (integer -581 900) a))
      (ash 3 (ash (ash a -50) (1- sb-vm:n-word-bits))))
   ((-1) 0)
   ((1) 3)))
