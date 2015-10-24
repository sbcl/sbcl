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

(cl:in-package :cl-user)

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
  (assert-error (funcall (checked-compile
                          `(lambda () (/ 2/3 0))
                          :allow-style-warnings t))
                division-by-zero))

(with-test (:name (/ :division-by-zero bignum))
  (assert-error (funcall (checked-compile
                          `(lambda () (/ (1+ most-positive-fixnum) 0))
                          :allow-style-warnings t))
                division-by-zero))

;;; In a bug reported by Raymond Toy on cmucl-imp 2002-07-18, (COERCE
;;; <RATIONAL> '(COMPLEX FLOAT)) was failing to return a complex
;;; float; a patch was given by Wolfhard Buss cmucl-imp 2002-07-19.
(assert (= (coerce 1 '(complex float)) #c(1.0 0.0)))
(assert (= (coerce 1/2 '(complex float)) #c(0.5 0.0)))
(assert (= (coerce 1.0d0 '(complex float)) #c(1.0d0 0.0d0)))

;;; (COERCE #c(<RATIONAL> <RATIONAL>) '(complex float)) resulted in
;;; an error up to 0.8.17.31
(assert (= (coerce #c(1 2) '(complex float)) #c(1.0 2.0)))

;;; COERCE also sometimes failed to verify that a particular coercion
;;; was possible (in particular coercing rationals to bounded float
;;; types.
(with-test (:name (coerce :to float :outside-bounds))
  (assert-error (funcall (checked-compile
                          `(lambda () (coerce 1 '(float 2.0 3.0)))
                          :allow-style-warnings t))
                type-error)
  (assert-error (funcall (checked-compile
                          `(lambda () (coerce 1 '(single-float -1.0 0.0)))
                          :allow-style-warnings t))
                type-error)
  (assert (eql (coerce 1 '(single-float -1.0 2.0)) 1.0)))

;;; ANSI says MIN and MAX should signal TYPE-ERROR if any argument
;;; isn't REAL. SBCL 0.7.7 didn't in the 1-arg case. (reported as a
;;; bug in CMU CL on #lisp IRC by lrasinen 2002-09-01)
(with-test (:name (min max type-error))
  (assert (null (ignore-errors (funcall
                                (checked-compile `(lambda () (min '(1 2 3)))
                                                 :allow-style-warnings t)))))
  (assert (= (min -1) -1))
  (assert (null (ignore-errors (funcall
                                (checked-compile `(lambda () (min 1 #(1 2 3)))
                                                 :allow-style-warnings t)))))
  (assert (= (min 10 11) 10))
  (assert (null (ignore-errors (funcall
                                (checked-compile
                                 `(lambda () (min (find-package "CL") -5.0))
                                 :allow-style-warnings t)))))
  (assert (= (min 5.0 -3) -3))
  (assert (null (ignore-errors (checked-compile `(lambda () (max #c(4 3)))
                                                :allow-style-warnings t))))
  (assert (= (max 0) 0))
  (assert (null (ignore-errors (funcall
                                (checked-compile `(lambda () (max "MIX" 3))
                                                 :allow-style-warnings t)))))
  (assert (= (max -1 10.0) 10.0))
  (assert (null (ignore-errors (funcall
                                (checked-compile `(lambda () (max 3 #'max))
                                                 :allow-style-warnings t)))))
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
(loop for divisor in '(-4 4)
      for ceiler = (compile nil `(lambda (x)
                                   (declare (fixnum x))
                                   (declare (optimize (speed 3)))
                                   (ceiling x ,divisor)))
      do (loop for i from -5 to 5
               for exact-q = (/ i divisor)
               do (multiple-value-bind (q r)
                      (funcall ceiler i)
                    (assert (= (+ (* q divisor) r) i))
                    (assert (<= exact-q q))
                    (assert (< q (1+ exact-q))))))

;;; (TRUNCATE x 2^k) was optimized incorrectly
(loop for divisor in '(-4 4)
      for truncater = (compile nil `(lambda (x)
                                      (declare (fixnum x))
                                      (declare (optimize (speed 3)))
                                      (truncate x ,divisor)))
      do (loop for i from -9 to 9
               for exact-q = (/ i divisor)
               do (multiple-value-bind (q r)
                      (funcall truncater i)
                    (assert (= (+ (* q divisor) r) i))
                    (assert (<= (abs q) (abs exact-q)))
                    (assert (< (abs exact-q) (1+ (abs q)))))))

;;; CEILING had a corner case, spotted by Paul Dietz
(assert (= (ceiling most-negative-fixnum (1+ most-positive-fixnum)) -1))

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
(dolist (x (list -10 (* 3 most-negative-fixnum)))
  (assert (= (gcd 0 x) (abs x))))
;;; LCM returns a non-negative number
(assert (= (lcm 4 -10) 20))
(assert (= (lcm 0 0) 0))

;;; PPC bignum arithmetic bug:
(multiple-value-bind (quo rem)
    (truncate 291351647815394962053040658028983955 10000000000000000000000000)
  (assert (= quo 29135164781))
  (assert (= rem 5394962053040658028983955)))

;;; x86 LEA bug:
(assert (= (funcall
            (compile nil '(lambda (x) (declare (bit x)) (+ x #xf0000000)))
            1)
           #xf0000001))

;;; LOGBITP on bignums:
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
    (assert (eq (eval `(logbitp ,index ,int)) result))))

;;; off-by-1 type inference error for %DPB and %DEPOSIT-FIELD:
(let ((f (compile nil '(lambda (b)
                        (integer-length (dpb b (byte 4 28) -1005))))))
  (assert (= (funcall f 1230070) 32)))
(let ((f (compile nil '(lambda (b)
                        (integer-length (deposit-field b (byte 4 28) -1005))))))
  (assert (= (funcall f 1230070) 32)))

;;; type inference leading to an internal compiler error:
(let ((f (compile nil '(lambda (x)
                        (declare (type fixnum x))
                        (ldb (byte 0 0) x)))))
  (assert (= (funcall f 1) 0))
  (assert (= (funcall f most-positive-fixnum) 0))
  (assert (= (funcall f -1) 0)))

;;; Alpha bignum arithmetic bug:
(assert (= (* 966082078641 419216044685) 404997107848943140073085))

;;; Alpha smallnum arithmetic bug:
(assert (= (ash -129876 -1026) -1))

;;; Alpha middlenum (yes, really! Affecting numbers between 2^32 and
;;; 2^64 :) arithmetic bug
(with-test (:name (truncate :middlenum))
  (let ((fn (checked-compile
             `(lambda (a b c d)
                (declare (type (integer -1621 -513) a)
                         (type (integer -3 34163) b)
                         (type (integer -9485132993 81272960) c)
                         (type (integer -255340814 519943) d)
                         (ignorable a b c d)
                         (optimize (speed 3) (safety 1) (debug 1)))
                (truncate c (min -100 4149605))))))
    (assert (= (funcall fn -1332 5864 -6963328729 -43789079) 69633287))))

;;; Here's another fantastic Alpha backend bug: the code to load
;;; immediate 64-bit constants into a register was wrong.
(with-test (:name (dpb :constants))
  (let ((fn (checked-compile `(lambda (a b c d)
                                (declare (type (integer -3563 2733564) a)
                                         (type (integer -548947 7159) b)
                                         (type (integer -19 0) c)
                                         (type (integer -2546009 0) d)
                                         (ignorable a b c d)
                                         (optimize (speed 3) (safety 1) (debug 1)))
                                (case a
                                  ((89 125 16) (ash a (min 18 -706)))
                                  (t (dpb -3 (byte 30 30) -1)))))))
    (assert (= (funcall fn 1227072 -529823 -18 -792831) -2147483649))))

;;; ASH of a negative bignum by a bignum count would erroneously
;;; return 0 prior to sbcl-0.8.4.4
(assert (= (ash (1- most-negative-fixnum) (1- most-negative-fixnum)) -1))

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
(assert (= 286142502
           (funcall (lambda ()
                      (declare (notinline logxor))
                      (min (logxor 0 0 0 286142502))))))

;; Small bugs in LOGCOUNT can still allow SBCL to be built and thus go
;; unnoticed, so check more thoroughly here.
(with-test (:name :logcount)
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
(with-test (:name :atanh)
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
(with-test (:name :gcd)
  ;; from lp#413680
  (assert (plusp (gcd 20286123923750474264166990598656
                      680564733841876926926749214863536422912)))
  ;; from lp#516750
  (assert (plusp (gcd 2596102012663483082521318626691873
                      2596148429267413814265248164610048))))

(with-test (:name :expt-zero-zero)
  ;; Check that (expt 0.0 0.0) and (expt 0 0.0) signal error, but
  ;; (expt 0.0 0) returns 1.0
  (flet ((error-case (expr)
           (assert-error (funcall (checked-compile `(lambda () ,expr)
                                                   :allow-style-warnings t))
                         sb-int:arguments-out-of-domain-error)))
    (error-case '(expt 0.0 0.0))
    (error-case '(expt 0 0.0)))
  (assert (eql (expt 0.0 0) 1.0)))

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
                                (if (eql fast-result slow-result)
                                    (print (list :ok `(,op ,@args) :=> fast-result))
                                    (error "oops: ~S, ~S" args call-args)))))))))))

;;; (TRUNCATE <unsigned-word> <constant unsigned-word>) is optimized
;;; to use multiplication instead of division. This propagates to FLOOR,
;;; MOD and REM. Test that the transform is indeed triggered and test
;;; several cases for correct results.
(with-test (:name (:integer-division-using-multiplication :used)
                  :skipped-on '(not (or :x86-64 :x86)))
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
                             (and fixnum unsigned-byte)
                             (integer 10000 10100)))
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
                                 collect r)))
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
                                        collect r)))
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

;; The fast path for logbitp underestimated sb!vm:n-positive-fixnum-bits
;; for > 61 bit fixnums.
(with-test (:name :logbitp-wide-fixnum)
  (assert (not (logbitp (1- (integer-length most-positive-fixnum))
                        most-negative-fixnum))))

;; EXPT dispatches in a complicated way on the types of its arguments.
;; Check that all possible combinations are covered.
(with-test (:name (:expt :argument-type-combinations))
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
        (format t "(expt ~s ~s) => " base power)
        (let ((result (expt base power)))
          (format t "~s~%" result)
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

(with-test (:name (:ldb :rlwinm :ppc))
  (let ((one (checked-compile `(lambda (a) (ldb (byte 9 27) a))))
        (two (checked-compile `(lambda (a)
                                 (declare (type (integer -3 57216651) a))
                                 (ldb (byte 9 27) a)))))
    (assert (= 0 (- (funcall one 10) (funcall two 10))))))

;; The ISQRT implementation is sufficiently complicated that it should
;; be tested.
(with-test (:name :isqrt)
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
  (assert (eql (funcall
                (checked-compile
                 `(lambda (x)
                    (declare (optimize (space 3))
                             (type (integer 12417236377505266230
                                            12417274239874990070)
                                   x))
                    (logand 8459622733968096971 x)))
                12417237222845306758)
               2612793697039849090)))

;; Also reported by Eric Marsden on sbcl-devel (2013-06-06)
(with-test (:name :more-recursive-cut-to-width)
  (assert (eql (funcall
                (checked-compile
                 `(lambda (a b)
                    (declare (optimize (speed 2) (safety 0)))
                    (logand (the (eql 16779072918521075607) a)
                            (the (integer 21371810342718833225 21371810343571293860) b))))
                16779072918521075607 21371810342718833263)
               2923729245085762055)))

(with-test (:name :complicated-logand-identity)
  (loop for k from -8 upto 8 do
    (loop for min from -16 upto 16 do
      (loop for max from min upto 16 do
        (let ((f (checked-compile `(lambda (x)
                                     (declare (type (integer ,min ,max) x))
                                     (logand x ,k)))))
          (loop for x from min upto max do
            (assert (eql (logand x k) (funcall f x)))))))))

(with-test (:name :complicated-logior-identity)
  (loop for k from -8 upto 8 do
    (loop for min from -16 upto 16 do
      (loop for max from min upto 16 do
        (let ((f (checked-compile `(lambda (x)
                                     (declare (type (integer ,min ,max) x))
                                     (logior x ,k)))))
          (loop for x from min upto max do
            (assert (eql (logior x k) (funcall f x)))))))))

(with-test (:name :ldb-negative-index-no-error)
  (assert-error
   (funcall (checked-compile `(lambda (x y)
                                (ldb (byte x y) 100)))
            -1 -2))
  (assert-error
   (funcall (checked-compile `(lambda (x y)
                                (mask-field (byte x y) 100)))
            -1 -2))
  (assert-error
   (funcall (checked-compile `(lambda (x y)
                                (dpb 0 (byte x y) 100)))
            -1 -2))
  (assert-error
   (funcall (checked-compile `(lambda (x y)
                                (deposit-field 0 (byte x y) 100)))
            -1 -2)))

(with-test (:name :setf-mask-field)
  (assert (= (funcall
              (checked-compile `(lambda (a)
                                  (setf (mask-field (byte 2 0) a) 1) a))
              15))))

(with-test (:name :complex-multiply)
  (assert (= (funcall
              (checked-compile
               `(lambda ()
                  (declare (optimize speed))
                  (let (z)
                    (expt (setf z (complex -0.123 -0.789)) 2)))))
             #C(-0.60739195 0.194094))))
