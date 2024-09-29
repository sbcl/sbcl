;;;; arithmetic tests without side effects

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

(defmacro define-compiled-fun (fun name)
  `(progn
    (declaim (notinline ,name))
    (defun ,name (&rest args)
     (declare (optimize safety))
     (case (length args)
       (1 (,fun (car args)))
       (2 (,fun (car args) (cadr args)))
       (t (apply #',fun args))))))

(define-compiled-fun min compiled-min)
(define-compiled-fun max compiled-max)
(define-compiled-fun + compiled-+)
(define-compiled-fun * compiled-*)
(define-compiled-fun logand compiled-logand)
(define-compiled-fun logior compiled-logior)
(define-compiled-fun logxor compiled-logxor)

(assert (null (ignore-errors (compiled-min '(1 2 3)))))
(assert (= (compiled-min -1) -1))
(assert (null (ignore-errors (compiled-min 1 #(1 2 3)))))
(assert (= (compiled-min 10 11) 10))
(assert (null (ignore-errors (compiled-min (find-package "CL") -5.0))))
(assert (= (compiled-min 5.0 -3) -3))
(assert (null (ignore-errors (compiled-max #c(4 3)))))
(assert (= (compiled-max 0) 0))
(assert (null (ignore-errors (compiled-max "MIX" 3))))
(assert (= (compiled-max -1 10.0) 10.0))
(assert (null (ignore-errors (compiled-max 3 #'max))))
(assert (= (compiled-max -3 0) 0))

(assert (null (ignore-errors (compiled-+ "foo"))))
(assert (= (compiled-+ 3f0) 3f0))
(assert (null (ignore-errors (compiled-+ 1 #p"tmp"))))
(assert (= (compiled-+ 1 2) 3))
(assert (null (ignore-errors (compiled-+ '(1 2 3) 3))))
(assert (= (compiled-+ 3f0 4f0) 7f0))
(assert (null (ignore-errors (compiled-* "foo"))))
(assert (= (compiled-* 3f0) 3f0))
(assert (null (ignore-errors (compiled-* 1 #p"tmp"))))
(assert (= (compiled-* 1 2) 2))
(assert (null (ignore-errors (compiled-* '(1 2 3) 3))))
(assert (= (compiled-* 3f0 4f0) 12f0))

(assert (null (ignore-errors (compiled-logand #(1)))))
(assert (= (compiled-logand 1) 1))
(assert (null (ignore-errors (compiled-logior 3f0))))
(assert (= (compiled-logior 4) 4))
(assert (null (ignore-errors (compiled-logxor #c(2 3)))))
(assert (= (compiled-logxor -6) -6))

(with-test (:name (coerce :overflow))
  (checked-compile-and-assert
      ()
      '(lambda (n) (coerce n 'single-float))
    (((expt 10 1000)) (condition 'floating-point-overflow))))

(defun are-we-getting-ash-right (x y)
  (declare (optimize speed)
           (type (unsigned-byte 32) x)
           (type (integer -40 0) y))
  (ash x y))
(defun what-about-with-constants (x)
  (declare (optimize speed) (type (unsigned-byte 32) x))
  (ash x -32))

(dotimes (i 41)
  (assert (= (are-we-getting-ash-right (1- (ash 1 32)) (- i))
             (if (< i 32)
                 (1- (ash 1 (- 32 i)))
                 0))))
(assert (= (what-about-with-constants (1- (ash 1 32))) 0))

(defun one-more-test-case-to-catch-sparc (x y)
  (declare (optimize speed (safety 0))
           (type (unsigned-byte 32) x) (type (integer -40 2) y))
  (the (unsigned-byte 32) (ash x y)))
(assert (= (one-more-test-case-to-catch-sparc (1- (ash 1 32)) -40) 0))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *n-fixnum-bits* (- sb-vm:n-word-bits sb-vm::n-fixnum-tag-bits))
  (defvar *shifts* (let ((list (list 0
                                     1
                                     (1- sb-vm:n-word-bits)
                                     sb-vm:n-word-bits
                                     (1+ sb-vm:n-word-bits))))
                     (append list (mapcar #'- list)))))

(macrolet ((nc-list ()
             `(list ,@(loop for i from 0 below (length *shifts*)
                         collect `(frob (nth ,i *shifts*)))))
           (c-list ()
             `(list ,@(loop for i from 0 below (length *shifts*)
                         collect `(frob ,(nth i *shifts*))))))
  (defun nc-ash (x)
    (macrolet ((frob (y)
                 `(list x ,y (ash x ,y))))
      (nc-list)))
  (defun c-ash (x)
    (macrolet ((frob (y)
                 `(list x ,y (ash x ,y))))
      (c-list)))
  (defun nc-modular-ash-ub (x)
    (macrolet ((frob (y)
                 `(list x ,y (logand most-positive-fixnum (ash x ,y)))))
      (nc-list)))
  (defun c-modular-ash-ub (x)
    (declare (type (and fixnum unsigned-byte) x)
             (optimize speed))
    (macrolet ((frob (y)
                 `(list x ,y (logand most-positive-fixnum (ash x ,y)))))
      (c-list))))

(let* ((values (list 0 1 most-positive-fixnum))
       (neg-values (cons most-negative-fixnum
                         (mapcar #'- values))))
  (labels ((test (value fun1 fun2)
             (let ((res1 (funcall fun1 value))
                   (res2 (funcall fun2 value)))
               (mapcar (lambda (a b)
                         (unless (equalp a b)
                           (error "ash failure for ~A vs ~A: ~A not EQUALP ~A"
                                  fun1 fun2
                                  a b)))
                       res1 res2))))
    (loop for x in values do
         (test x 'nc-ash 'c-ash)
         (test x 'nc-modular-ash-ub 'c-modular-ash-ub))
    (loop for x in neg-values do
         (test x 'nc-ash 'c-ash))))


(declaim (inline ppc-ldb-2))

(defun ppc-ldb-2 (fun value)
  (declare (type (signed-byte 32) value)
           (optimize (speed 3) (safety 0) (space 1) (debug 1)
                     (compilation-speed 0)))
  (funcall fun (ldb (byte 8 24) value))
  (funcall fun (ldb (byte 8 16) value))
  (funcall fun (ldb (byte 8 8) value))
  (funcall fun (ldb (byte 8 0) value))
  (values))

(defun ppc-ldb-1 (fun)
  (declare (optimize (speed 3) (safety 0) (space 1) (debug 1)
                     (compilation-speed 0)))
  (loop
     for param :across (make-array 1 :initial-element nil)
     for size :across (make-array 1 :element-type 'fixnum :initial-element 3)
     do (ppc-ldb-2 fun (if param size -1))))

(with-test (:name :ppc-ldb)
 (let ((acc '()))
   (ppc-ldb-1 (lambda (x)
                (push x acc)))
   (assert (equal acc '(#xff #xff #xff #xff)))))

(with-test (:name :ldb-word-cast)
  (checked-compile-and-assert
      ()
      `(lambda (x y)
         (truly-the fixnum (ldb (byte x y) 100)))
    ((100 0) 100)))

(with-test (:name :logbitp-negative-error)
  (checked-compile-and-assert
      (:optimize :safe)
      `(lambda (x y)
         (logbitp x y))
    ((-1 0) (condition 'type-error))
    ((-2 (1+ most-positive-fixnum)) (condition 'type-error))
    (((1- most-negative-fixnum) 1) (condition 'type-error))))

(with-test (:name :*-overflow-ratio)
  (checked-compile-and-assert
      (:optimize :safe)
      `(lambda (a)
         (the fixnum (* 8 a)))
    ((1/8) 1)))

#+64-bit
(with-test (:name :bignum-float)
  (checked-compile-and-assert
      ()
      `(lambda (d)
         (sb-sys:without-gcing
           (let ((res (sb-bignum:%allocate-bignum 2)))
             (setf (sb-bignum:%bignum-ref res 1) 529
                   (sb-bignum:%bignum-ref res 0) 9223372036854775807)
             (sb-kernel:set-header-data res 1)
             (unwind-protect
                  (< res d)
               (sb-kernel:set-header-data res 2)))))
    ((-9.223372036854776d18) nil)
    ((9.223372036854776d18) t)))

(with-test (:name :overflow-transform-nil)
  (checked-compile-and-assert
      (:allow-warnings t)
      `(lambda (v)
         (let ((i 0))
           (flet ((f (i)
                    (the fixnum i)
                    (svref v (+ i 26387449082611642302))))
             (f i)
             (incf i)
             (f i)
             (incf i)))))
  (checked-compile-and-assert
      (:allow-style-warnings t)
      `(lambda (s e)
         (subseq s 0 (when e
                       (- (length s) 12129535698721845515))))))

(with-test (:name :integer-length-union-derivation)
  (checked-compile-and-assert
      ()
      `(lambda (b)
         (integer-length
          (if (>= b 0)
              b
              -2)))
    ((-1) 1)
    ((0) 0)
    ((15) 4)))

(with-test (:name :isqrt-union)
  (assert-type
   (lambda (x)
     (declare ((or (integer 1 5) (integer 9 10)) x))
     (isqrt x))
   (integer 1 3)))

(with-test (:name :integer-length-union)
  (assert-type
   (lambda (x)
     (declare ((or (integer 1 5) (integer 9 10)) x))
     (integer-length x))
   (integer 1 4)))

(with-test (:name :rem-transform-erase-types)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare ((integer * 0) a))
      (zerop (rem a 2)))
   ((-1) nil)
   ((-2) t))
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare ((member 7 -9) a))
      (zerop (rem a 8)))
   ((7) nil)
   ((-9) nil)))

(with-test (:name :unexpected-immediates-in-vops)
  (checked-compile
   `(lambda (n)
      (declare (fixnum n))
      (loop for i below 2
            do (print (logbitp i n))
               (the (satisfies minusp) i))))
  (checked-compile
   `(lambda ()
      (loop for i below 2
            do (print (lognot i))
               (the (satisfies minusp) i))))
  (checked-compile
   `(lambda ()
      (loop for i below 2
            do (print (- i))
               (the (satisfies minusp) i))))
  (checked-compile
   `(lambda ()
      (loop for i below 2
            do (print (* i 3))
               (the (satisfies minusp) i))))
  (checked-compile
   `(lambda ()
      (loop for i below 2
            do (print (* i 3))
               (the (satisfies minusp) i))))
  (checked-compile
   `(lambda ()
      (loop for i of-type fixnum below 2
            do (print (logand most-positive-word (* i 4)))
               (the (satisfies minusp) i)))))

(with-test (:name :/-by-integer-type)
  (assert-type
   (lambda (x y)
     (declare ((integer 1 9) x)
              (integer y))
     (/ x y))
   (or (rational -9 (0)) (rational (0) 9)))
  (assert-type
   (lambda (x y)
     (declare ((integer 1 9) x)
              ((integer 0) y))
     (/ x y))
   (rational (0) 9))
  (assert-type
   (lambda (x y)
     (declare ((rational 0 9) x)
              ((integer 0) y))
     (/ x y))
   (rational 0 9)))

(with-test (:name :truncate-unused-q)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare (fixnum a))
      (rem a 4))
   ((3) 3)
   ((-3) -3)
   ((4) 0)
   ((-4) 0)))

(with-test (:name :*-by-integer-type)
  (assert-type
   (lambda (x)
     (declare (integer x))
     (* x 5))
   (or (integer 5) (integer * -5) (integer 0 0))))

(with-test (:name :truncate-transform-unused-result)
  (assert-type
   (lambda (c)
     (declare ((integer -1000 0) c)
              (optimize speed))
     (values
      (truncate (truncate (rem c -89) -16) 20)))
   (or (integer 0 0))))

(with-test (:name :rem^2)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare (fixnum a))
      (rem a 2))
   ((-2) 0)
   ((-3) -1)
   ((2) 0)
   ((3) 1)))

(with-test (:name :deposit-field-derive-type)
  (assert-type
   (lambda (s)
     (declare ((member 8 10) s))
     (deposit-field -21031455 (byte s 9) 1565832649825))
   (or (integer 1565832320097 1565832320097) (integer 1565832713313 1565832713313))))

(with-test (:name :logior-negative-bound)
  (checked-compile-and-assert
   ()
   `(lambda (b c)
      (declare ((integer 7703 1903468060) c))
      (logandc1 (/ (logorc2 c b) -1) c))
   ((-1 7703) 7702)))

(with-test (:name :set-numeric-contagion)
  (assert-type
   (lambda (n)
     (loop for i below n
           sum (coerce n 'single-float)))
   (or (integer 0 0) single-float)))

(with-test (:name :overflow-transform-order)
  (checked-compile-and-assert
      (:optimize :safe)
      `(lambda (a m)
         (declare (fixnum a))
         (let ((j (* 44 a)))
           (when m
             (the fixnum j))))
    ((most-positive-fixnum nil) nil)
    ((most-positive-fixnum t) (condition 'type-error))))

(with-test (:name :logtest-memref-boxed)
  (checked-compile-and-assert
      ()
      `(lambda (b)
         (declare (sb-vm:word b))
         (when (oddp b)
           (lambda (m)
             (when m
               (setf b 1))
             b)))
    (((expt 2 (1- sb-vm:n-word-bits))) nil)
    (((1+ (expt 2 (1- sb-vm:n-word-bits)))) t :test (lambda (x y)
                                                      y
                                                      (functionp (car x))))))

(with-test (:name :range-unsigned)
  (assert-type
   (lambda (d)
     (declare (type (integer 1 109) d))
     (typep (- d) '(integer -47727025476642942 -2593702250735)))
   null))

(with-test (:name :signed-byte-8-p-unsigned
                  ;; these lack the necessary RANGE<= vop
                  :fails-on (:or :mips :ppc :ppc64 :sparc :riscv))
  (checked-compile
   `(lambda (a)
      (declare (type (simple-array sb-vm:word (*)) a)
               (optimize speed))
      (the (signed-byte 8) (aref a 0)))
   :allow-notes nil))

(with-test (:name :or-chain)
  (checked-compile-and-assert
   ()
   `(lambda (b)
      (declare (fixnum b))
      (case b ((0 -3) 1) (t 2)))
   ((0) 1)
   ((-3) 1)
   ((3) 2)
   ((1) 2)))

(with-test (:name :or-chain-types)
  (checked-compile-and-assert
   ()
   `(lambda (b)
      (declare ((integer -1 1) b))
      (case b
        ((-1 0) 0)
        (t 1)))
   ((-1) 0)
   ((0) 0)
   ((1) 1)))

(with-test (:name :or-chain-tagging)
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (or (eq x -6)
          (eq x -2)))
   ((-6) t)
   ((-2) t)
   ((6) nil)
   ((2) nil)
   ((-12) nil)
   ((-4) nil))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (or (eq x 0)
          (eq x -4)))
   ((0) t)
   ((-4) t)
   ((4) nil)
   ((-8) nil))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (or (eq x 97)
          (eq x 65)))
   ((-4611686018427387807) nil)
   ((97) t)
   ((65) t))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (or (eq x -65)
          (eq x -97)))
   ((-97) t)
   ((-65) t))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (case x ((-3 -2 17) t)))
   ((4611686018427387902) nil)
   ((-3) t)
   ((-2) t)
   ((17) t)))

(with-test (:name :range<=-same)
  (checked-compile-and-assert
   ()
   `(lambda (a c)
      (declare (type fixnum a))
      (let ((v7 (if c
                    4611686018427387904
                    -6)))
        (if (> v7 a)
            a
            (if (<= a v7)
                0
                a))))
    ((-7 nil) -7)
    ((-7 t) -7)
    ((-6 nil) 0)
    ((-6 t) -6)
    ((-3 nil) -3)))

(with-test (:name :/-folding)
  (checked-compile-and-assert
      (:optimize :safe)
      `(lambda (a)
         (declare (bit a))
         (/ 1 a))
    ((1) 1)
    ((0) (condition 'division-by-zero)))
  (checked-compile-and-assert
      (:optimize :safe)
      `(lambda (a)
         (declare (bit a))
         (= (/ 5 a) 5))
    ((1) t)
    ((0) (condition 'division-by-zero))))

(with-test (:name :dpb-size-overflow)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare ((unsigned-byte 8) a))
      (dpb a (byte 63 8)
           81))
   ((90) 23121))
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare ((unsigned-byte 8) a))
      (dpb a (byte 32 32)
           1))
   ((1) 4294967297)))

(with-test (:name :mask-field-size-overflow)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (truly-the fixnum
                 (mask-field (byte 78 0) a)))
   ((35) 35)))
(with-test (:name :ash-count-integr)
  (checked-compile-and-assert
      (:optimize :safe)
      `(lambda (a b)
         (ash a b))
    ((1 -1.0) (condition 'type-error))
    (((expt 2 74) -1.0) (condition 'type-error))
    ((0 1.0) (condition 'type-error))
    (((expt 2 74) 1.0) (condition 'type-error))
    ((1 1d0) (condition 'type-error))
    (((expt 2 74) 1d0) (condition 'type-error))
    ((0 -3d0) (condition 'type-error))
    (((expt 2 74) -2d0) (condition 'type-error))))

(with-test (:name :log-integer-derive-type)
  (assert-type
   (lambda (x)
     (declare ((integer 0) x))
     (log x))
   (single-float 0.0))
  (assert-type
   (lambda (x)
     (declare (integer x))
     (log x))
   (or (complex single-float) (single-float 0.0))))

(with-test (:name :floor-derive-type)
  (assert-type
   (lambda (a b)
     (declare ((integer -10 0) b)
              ((unsigned-byte 8) a))
     (floor a b))
   (values (integer -255 0) (integer -9 0) &optional)))

(with-test (:name :logbitp-on-integers)
  (assert (not (ctu:ir1-named-calls `(lambda (x)
                                       (logbitp 20 x))))))
(with-test (:name :bt-negative-bit)
  (checked-compile-and-assert
   ()
   `(lambda (c)
     (declare ((signed-byte 64) c))
     (logtest c -2199023255553))
   ((-2049990302793354782) t)
   ((0) nil)
   (((ash 1 41)) nil))
  (checked-compile-and-assert
   ()
   `(lambda (b)
      (declare (fixnum b))
      (logior b -4611686018427387905))
   ((-6) -1)))

(with-test (:name :float-cmp)
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (declare ((unsigned-byte 20) a)
                  (float b))
         (< a b))
    ((6 4.0) nil)
    ((1 1.1) t)))

(with-test (:name :complex+non-complex-type)
  (assert-type
   (lambda (a)
     (+ a #c(1.0 3.0)))
   (or (complex single-float) (complex double-float)))
  (assert-type
   (lambda (a)
     (* a #c(1d0 0d0)))
    (complex double-float)))
