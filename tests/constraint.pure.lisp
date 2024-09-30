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

;;;; This file of tests was added because the tests in 'compiler.pure.lisp'
;;;; are a total hodgepodge- there is often no hugely compelling reason for
;;;; their being tests of the compiler per se, such as whether
;;;; INPUT-ERROR-IN-COMPILED-FILE is a subclass of SERIOUS-CONDITION;
;;;; in addition to which it is near impossible to wade through the
;;;; ton of nameless, slow, and noisy tests.

#+sb-unicode
(with-test (:name :base-char-p)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (x)
              (if (sb-kernel:base-char-p x)
                  (characterp x)
                  t))))
          '(function (t) (values (member t) &optional)))))

(with-test (:name :setq-eql)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (x) (let (y) (setq y x) (eql y x)))))
          '(function (t) (values (member t) &optional)))))

(with-test (:name :setq-lvar-substition)
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (declare ((integer 0 10) a)
                  (fixnum b))
         (let ((c b))
           (setq b a)
           (eql c b)))
    ((0 2) nil)
    ((0 0) t)))

(with-test (:name :number-comparisons)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (a)
              (if (< a 0)
                  (typep a '(integer 0 10))
                  nil))))
          '(function (t) (values null &optional))))
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (a)
              (if (= a 30)
                  (typep a '(integer 0 10))
                  nil))))
          '(function (t) (values null &optional)))))

(with-test (:name :=-constraint-complex-no-bounds)
  (checked-compile-and-assert
      ()
      `(lambda (p)
        (let ((x #c(1 2)))
          (when (= x p)
            x)))
    ((#c(1 2)) #c(1 2))
    ((#c(2 1)) nil)))

(with-test (:name :compare-both-operands)
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (declare (type real a b))
         (if (>= a a)
             (if (= b a)
                 1
                 2)
             t))
    ((0 1) 2)
    ((1 1) 1)))

(with-test (:name :eql-constant)
  (assert
   (equal (third (sb-kernel:%simple-fun-type
                  (checked-compile
                   '(lambda (i)
                     (declare ((integer 0) i))
                     (cond
                       ((= i 0) 3)
                       ((= i 1) 3)
                       (t i))))))
          '(values (integer 2) &optional))))

(with-test (:name :ir1-phases-delay)
  (assert
   (equal (third (sb-kernel:%simple-fun-type
                  (checked-compile
                   '(lambda (n z)
                     (when (typep n 'fixnum)
                       (let ((ar (if (integerp n)
                                     (make-array n)
                                     z)))
                         (declare (type vector ar))
                         (print ar)
                         (array-has-fill-pointer-p ar)))))))
          '(values null &optional))))

(with-test (:name :--sign)
  (assert
   (equal (third (sb-kernel:%simple-fun-type
                  (checked-compile
                   '(lambda (x y)
                     (declare (integer x y))
                     (if (<= x y)
                         (- x y)
                         -10)))))
          '(values (integer * 0) &optional))))

(with-test (:name :--type)
  (assert
   (equal (third (sb-kernel:%simple-fun-type
                  (checked-compile
                   '(lambda (x y)
                     (if (> x y)
                         (- x y)
                         1)))))
          '(values real &optional))))

(with-test (:name :remove-equivalent-blocks-clear-constraints)
  (checked-compile-and-assert
      ()
      `(lambda (a c)
         (declare ((and fixnum unsigned-byte) a)
                  (fixnum c))
         (eql c
              (if (eql a c)
                  c
                  a)))
    ((3 1) nil)
    ((3 3) t)))

(with-test (:name :type-constraint-joining)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda ()
          (let ((x 'foo))
            (if (read)
                (setq x 3)
                (setq x 5))
            x)))))
    '(values (or (integer 5 5) (integer 3 3)) &optional))))

(with-test (:name :type-constraint-joining.2)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x)
          (etypecase x
            (integer (read))
            (float (read)))
          x))))
    '(values (or float integer) &optional))))

(with-test (:name :type-constraint-joining.3)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x)
          (if (read)
              (setq x (random 10))
              (setq x (random 10.0)))
          x))))
    '(values (or (single-float 0.0 (10.0)) (mod 10)) &optional))))

(with-test (:name :type-constraint-joining-terminates)
  (checked-compile
   `(lambda (name vop)
      (block foo
        (do* ((block (sb-c::vop-block vop) (sb-c::ir2-block-prev block))
              (last vop (sb-c::ir2-block-last-vop block)))
             (nil)
          (sb-c::aver (eq (sb-c::ir2-block-block block) (sb-c::ir2-block-block (sb-c::vop-block vop))))
          (do ((current last (sb-c::vop-prev current)))
              ((null current))
            (when (eq (sb-c::vop-name current) name)
              (return-from foo current))))))))

(with-test (:name :type-constraint-joining-conflicts)
  (assert (nth-value
           1
           (checked-compile
            '(lambda (y)
               (let ((x 'foo))
                 (ecase y
                   (1 (setq x (random 10)))
                   (2 (setq x (make-array 10)))
                   (3 (setq x (make-hash-table))))
                 (symbol-name x)))
            :allow-warnings t))))

(with-test (:name :type-constraint-joining.eql)
  (assert
   (equal (caddr
           (sb-kernel:%simple-fun-type
            (checked-compile
             `(lambda (x)
                (ecase x
                  (1 (read))
                  (2 (read)))
                x))))
          '(values (integer 1 2) &optional))))

(with-test (:name :type-constraint-joining.</=)
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (x)
                 (declare (integer x))
                 (cond ((= x 20))
                       ((< x 5))
                       ((< x 10))
                       (t (error ""))) x))))
           '(values (or
                     (integer * 9)
                     (integer 20 20))
             &optional))))

(with-test (:name :type-constraint-joining.</=.2)
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (x)
                 (if (typep x 'rational)
                     (cond ((= x 20))
                           ((< x 5))
                           ((< x 10))
                           (t (error "")))
                     (setf x :foo))
                 x))))
           '(values (or
                     (integer 20 20)
                     (rational * (10))
                     (member :foo))
             &optional))))

(with-test (:name :type-constraint-joining.</=.3)
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (x)
                 (cond ((< x 5))
                       ((< x 10))
                       (t (error "")))
                 x))))
           '(values (or
                     (double-float * (10.0d0))
                     (single-float * (10.0))
                     (rational * (10)))
             &optional))))

(with-test (:name :type-constraint-joining.>/=)
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (x)
                 (declare (integer x))
                 (cond ((= x 5))
                       ((> x 20))
                       ((> x 10))
                       (t (error ""))) x))))
           '(values (or
                     (integer 11)
                     (integer 5 5))
             &optional))))

(with-test (:name :type-constraint-joining.complement)
  (assert
   (equal (caddr
           (sb-kernel:%simple-fun-type
            (checked-compile
             `(lambda (x)
                (if (read)
                    (cond ((typep x 'integer)
                           (error ""))
                          (t (print "")))
                    (cond ((typep x 'float)
                           (print ""))
                          (t (error ""))))
                x))))
          '(values (not integer) &optional))))

(with-test (:name (:type-constraint-joining :infinities 1))
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (number)
                 (declare (type (or number null) number))
                 (cond ((null number) nil)
                       ((sb-ext:float-nan-p number) :nan)
                       ((= number sb-ext:double-float-positive-infinity) :inf)
                       ((= number sb-ext:double-float-negative-infinity) :-inf)
                       (number))))))
           '(values (or (member nil :nan :inf :-inf) float) &optional))))

(with-test (:name (:type-constraint-joining :infinities 2))
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (number)
                 (declare (type (or number null) number))
                 (cond ((null number) nil)
                       ((sb-ext:float-nan-p number) :nan)
                       ((= number sb-ext:double-float-positive-infinity) number)
                       ((= number sb-ext:double-float-negative-infinity) number)
                       (t :number))))))
           `(values (or (single-float ,sb-ext:single-float-negative-infinity
                                      ,sb-ext:single-float-negative-infinity)
                        (double-float ,sb-ext:double-float-negative-infinity
                                      ,sb-ext:double-float-negative-infinity)
                        (single-float ,sb-ext:single-float-positive-infinity
                                      ,sb-ext:single-float-positive-infinity)
                        (double-float ,sb-ext:double-float-positive-infinity
                                      ,sb-ext:double-float-positive-infinity)
                        (member nil :nan :number))
                    &optional))))

(with-test (:name (:type-constraint-joining :infinities 3))
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (number)
                 (declare (type (or number null) number))
                 (cond ((null number) nil)
                       ((sb-ext:float-nan-p number) :nan)
                       ((eql number sb-ext:double-float-positive-infinity) number)
                       ((eql number sb-ext:double-float-negative-infinity) number)
                       (t :number))))))
           `(values (or (double-float ,sb-ext:double-float-negative-infinity
                                      ,sb-ext:double-float-negative-infinity)
                        (double-float ,sb-ext:double-float-positive-infinity
                                      ,sb-ext:double-float-positive-infinity)
                        (member nil :nan :number))
                    &optional))))

(with-test (:name :debug-vars)
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (a)
                 (declare (optimize (debug 2)))
                 (let ((x (eq a 30)))
                   (if x
                       a
                       (error "")))))))
           `(values (integer 30 30) &optional))))

(with-test (:name :vector-length-constraints)
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (x y)
                 (declare (simple-vector x))
                 (when (< (length x) y)
                   (> (length x) y))))))
           `(values null &optional)))
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (x y)
                 (declare (vector x))
                 (when (< (length x) y)
                   (> (length x) y))))))
           `(values boolean &optional))))

(with-test (:name :non-commutative-invert)
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (x y)
                 (if (< x y)
                     (> y x)
                     (error ""))))))
            `(values (member t) &optional)))
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (x y)
                 (if (< x y)
                     (< y x)
                     (error ""))))))
            `(values null &optional))))

(with-test (:name :=-real)
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (a b)
                 (declare ((real 10 20) b))
                 (if (= a b)
                     a
                     (error ""))))))
           `(values (or (real 10 20) (complex (rational 10 20)) (complex (single-float 10.0 20.0))
                        (complex (double-float 10.0d0 20.0d0))) &optional)))
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (a b)
                 (declare (integer a)
                          ((real 10 20) b))
                 (if (= a b)
                     a
                     (error ""))))))
           `(values (integer 10 20) &optional)))
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (a b)
                 (declare ((real 10 20) b))
                 (if (> a b)
                     a
                     (error ""))))))
           `(values (real (10)) &optional))))

(with-test (:name :<=-constraints)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (if (/= a 0)
             (if (>= a 0)
                 t)))
    ((1) t)
    ((0) nil)))

(with-test (:name :vector-length-derive-type)
  (let ((s "X"))
    (checked-compile-and-assert
        ()
        `(lambda (x)
           (declare ((or (vector t 2) (member #\@ ,s)) x))
           (typep x '(string 1)))
      ((s) t))))

(with-test (:name :constant-refs)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (let ((m 2301453501242805283))
           (if (< m a)
               (< m a)
               (error ""))))
    (((expt 2 80)) t)
    (:return-type (values (member t) &optional))))

(with-test (:name :constant-refs.2)
  (checked-compile-and-assert
      ()
      `(lambda (v)
         (if (<= -3483114449144072 v)
             0
             (if (>= -3483114449144072 v)
                 1
                 40)))
    ((9) 0)
    ((-3483114449144073) 1)
    (:return-type (values bit &optional))))

(with-test (:name :array-has-fill-pointer-p-constraint)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (s)
          (if (array-has-fill-pointer-p s)
              (error "")
              s)))))
    '(values array &optional))))

(with-test (:name :+integer-argument-constraint.var)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (a x y)
          (declare (fixnum x))
          (let ((m (+ x y)))
            (aref a (+ m (aref a m)))
            y)))))
    `(values (integer ,(1+ most-negative-fixnum)
                      ,(+ most-positive-fixnum array-dimension-limit))
             &optional))))

(with-test (:name :+integer-argument-constraint.typep)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x)
          (let ((m (+ 1 x)))
            (if (integerp m)
                (+ m x)
                2))))))
    '(values integer &optional))))

(with-test (:name :truncate-zero-remainder)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x y)
          (declare (integer x y))
          (multiple-value-bind (q r) (truncate x y)
            (if (zerop r)
                (error "~a" q)
                x))))))
    '(values (or (integer * -1) (integer 1)) &optional))))

(with-test (:name :vector-length-var)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x)
          (declare (simple-vector x))
          (if (< 3 (length x) 5)
              (length x)
              (error ""))))))
    '(values (integer 4 4) &optional))))

(with-test (:name :ash)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x)
          (declare ((integer 1) x))
          (let ((d (ash x -1)))
            (< d x))))))
    '(values (member t) &optional)))
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x)
          (declare ((integer 1) x))
          (let ((d (ash x -1)))
            (print d)
            (< d x))))))
    '(values (member t) &optional)))
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x y)
          (declare (unsigned-byte x)
                   ((integer * 0) y))
          (let ((d (ash x y)))
            (> d x))))))
    '(values null &optional))))

(with-test (:name :/)
  (assert-type
   (lambda (x)
     (declare ((integer 1) x))
     (let ((d (truncate x 4)))
       (< d x)))
   (member t))
  (assert-type
   (lambda (x y)
     (declare ((integer 0) x y))
     (let ((d (/ x y)))
       (> d x)))
   null)
  (assert-type
   (lambda (a b)
     (declare ((integer 1) a b))
     (< (/ a b) a))
   boolean)
  (assert-type
   (lambda (x y)
     (declare (integer x y))
     (if (plusp (rem x y))
         x
         1))
   (integer 1)))

(with-test (:name :negate-<)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (a b)
          (declare (type integer a)
                   ((integer 2 3) b))
          (if (< (- a) b)
              a
              (loop))))))
    '(values (integer -2) &optional))))

(with-test (:name :eq-vector-lengths)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (a b j)
          (declare (simple-vector a b)
                   ((integer 10 20) j)
                   (optimize (debug 1)))
          (when (and (= (length a) (length b))
                     (= j (length a)))
            (length b))))))
    '(values (or null (integer 10 20)) &optional))))

(with-test (:name :+>)
  (assert-type
   (lambda (a j)
     (declare (integer a)
              ((integer 1) j))
     (> (+ a j) a))
   (member t))
  (assert-type
   (lambda (a j)
     (declare (integer a)
              ((integer 0) j))
     (> (+ j a) a))
   boolean)
  (assert-type
   (lambda (a b j)
     (declare (integer a b)
              ((integer 1) j))
     (if (= b a)
         (let ((d (+ b j)))
           (> a d))
         (loop)))
   null)
  (assert-type
   (lambda (a b j)
     (declare (integer a b)
              ((integer 0) j))
     (if (<= a b)
         (let ((d (+ b j)))
           (>= a d))
         (loop)))
   boolean)
  (assert-type
   (lambda (a b j)
     (declare (integer a b)
              ((integer 1) j))
     (if (<= a b)
         (let ((d (+ b j)))
           (>= a d))
         (loop)))
   null))

(with-test (:name :->)
  (assert-type
   (lambda (v)
     (declare (integer v)
              (optimize (debug 2)))
     (let ((l (1- v)))
       (< l v)))
   (member t))
  (assert-type
   (lambda (v)
     (declare (simple-vector v)
              (optimize (debug 2)))
     (let ((l (1- (length v))))
       (< l (length v))))
   (member t))
  (assert-type
   (lambda (v)
     (declare (integer v)
              (optimize (debug 1)))
     (let ((l (1- v)))
       (< l v)))
   (member t))
  (assert-type
   (lambda (v)
     (declare (simple-vector v)
              (optimize (debug 1)))
     (let ((l (1- (length v))))
       (< l (length v))))
   (member t)))

(with-test (:name :sub-sign)
  (assert-type
   (lambda (x)
     (declare (unsigned-byte x))
     (- x (truncate x 2)))
   unsigned-byte))

(with-test (:name :equal)
  (assert-type
   (lambda (x y)
     (if (eql x y)
         (equalp x y)
         t))
   (member t))
  (assert-type
   (lambda (x y)
     (if (eq x y)
         (equal x y)
         t))
   (member t))
  (assert-type
   (lambda (x y)
     (if (eql x y)
         nil
         (equal x y)))
   boolean)
  (assert-type
   (lambda (x y)
     (if (equalp x y)
         (eql x y)
         t))
   boolean)
  (assert-type
   (lambda (y)
     (let ((ht #1=#.(make-hash-table)))
       (if (equalp (the (eql #1#) ht) y)
           (eql y ht)
           t)))
   boolean))

(with-test (:name :bounds-check-constants)
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (v)
                        (declare (simple-vector v))
                        (setf (aref v 0) (aref v 1)))
                     nil))
             1)))

(with-test (:name :bounds-check-constants-svref)
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (v)
                        (values (svref v 1)
                                (svref v 0)))
                     nil))
             1)))

(with-test (:name :bounds-check-variable-svref)
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (x i)
                        (values (svref x i)
                                (svref x i)))
                     nil))
             1)))

(with-test (:name :bounds-check-length)
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (x y)
                        (when (< x (length y))
                          (svref y x)))
                     nil))
             0)))

(with-test (:name :bounds-check-length)
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (v)
                        (declare (simple-vector v)
                                 (optimize (debug 2)))
                        (loop for i below (1- (length v))
                              sum (aref v i)))
                     nil))
             0))
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (v)
                        (declare (simple-vector v)
                                 (optimize (debug 1)))
                        (loop for i below (1- (length v))
                              sum (aref v i)))
                     nil))
             0)))


(with-test (:name :bounds-check-min-length)
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (x v)
                        (declare (integer x)
                                 (simple-vector v)
                                 (optimize (debug 2)))
                        (loop for i below (min x (1- (length v)))
                              sum (aref v i)))
                     nil))
             0))
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (x v)
                        (declare (integer x)
                                 (simple-vector v))
                        (loop for i below (min x (length v))
                              sum (aref v i)))
                     nil))
             0))
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (x v)
                        (declare (simple-vector v))
                        (loop for i below (min x (length v))
                              sum (aref v i)))
                     nil))
             0)))

(with-test (:name :bounds-check-make-array)
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (length)
                        (declare (integer length))
                        (let ((array (make-array length)))
                          (loop for i below length
                                do (setf (aref array i) i))))
                     nil))
             0))
  #-sbcl
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (length)
                        (let ((array (make-array length)))
                          (loop for i below length
                                do (setf (aref array i) i))))
                     nil))
             0))
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (type length)
                        (let ((array (make-sequence type length)))
                          (loop for i below length
                                do (setf (aref array i) i))))
                     nil))
             0)))

(with-test (:name :bounds-check-down)
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (v)
                        (let ((end (1- (length v))))
                          (decf end)
                          (when (>= end 0)
                            (svref v end))))
                     nil))
             0))
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (v)
                        (declare (optimize (debug 2)))
                        (let ((end (1- (length v))))
                          (decf end)
                          (when (>= end 0)
                            (svref v end))))
                     nil))
             0))
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (v)
                        (loop for i from (1- (length v)) downto 0
                              collect (svref v i)))
                     nil))
             0))
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (v)
                        (declare (optimize (debug 2)))
                        (loop for i from (1- (length v)) downto 0
                              collect (svref v i)))
                     nil))
             0)))

(with-test (:name :reoptimize)
  (assert-type
   (lambda ()
     (let ((m 0))
       (incf m)
       (incf m)
       m))
   (eql 2))
  (assert-type
   (lambda (n j)
     (declare (integer j)
              (optimize (debug 1)))
     (the (integer 5) n)
     (when (> j n)
       j))
   (or (integer 6) null)))

(with-test (:name :+back)
  (assert-type
   (lambda (x y)
     (when (> (+ x 1) y)
       x))
   (or real null))
  (assert-type
   (lambda (x y)
     (when (= (+ x 1) y)
       x))
   (or number null))
  (assert-type
   (lambda (x y)
     (when (integerp (+ x y))
       x))
   (or rational (complex rational) null))
  (assert-type
   (lambda (x y)
     (declare (real y))
     (when (integerp (+ x y))
       x))
   (or rational null))
  (assert-type
   (lambda (x)
     (when (integerp (+ x 1))
       x))
   (or integer null))
  (assert-type
   (lambda (x)
     (when (typep (+ x 3) '(integer 0 20))
       x))
   (or (integer -3 17) null))
  (assert-type
   (lambda (x m)
     (declare ((real 2 4) m)
              (fixnum x))
     (when (typep (+ m x) '(integer 2 4))
       x))
   (or (integer -2 2) null)))

(with-test (:name :-back)
  (assert-type
   (lambda (x)
     (when (typep (- x 4) '(integer 0 10))
       x))
   (or (integer 4 14) null))
  (assert-type
   (lambda (x)
     (when (typep (- 3 x) '(integer 0 10))
       x))
   (or (integer -7 3) null))
  (assert-type
   (lambda (x)
     (when (> (- 3 x) 10)
       x))
   (or real null))
  (assert-type
   (lambda (x)
     (when (floatp (- x))
       x))
   (or float null))
  (assert-type
   (lambda (x)
     (when (typep (- x) '(integer 1 2))
       x))
   (or (integer -2 -1) null)))

(with-test (:name :*back)
  (assert-type
   (lambda (x)
     (when (typep (* x 4) '(integer 0 10))
       x))
   (or (rational 0 5/2) null))
  (assert-type
   (lambda (x m)
     (declare ((real 0 3) m))
     (when (typep (* x m) '(integer 0 10))
       x))
   (or (or rational (complex rational) null) null))
  (assert-type
   (lambda (x)
     (when (> (* x 3) 10)
       x))
   (or real null))
  (assert-type
   (lambda (x m)
     (declare (real x)
              (integer m))
     (when (typep (* x m) 'integer)
       x))
   (or rational null)))

(with-test (:name :ignore-hairy-types)
  (checked-compile
   '(lambda (a)
     (declare (optimize (debug 2)))
     (let ((v (the (satisfies eval) a)))
       (dotimes (i 3)
         (print (/= a 1))
         (the real a))
       v)))
  (checked-compile
   '(lambda (d)
     (values
      (let ((v9 d)
            (v1 (the (satisfies eval) (setf d 1))))
        (if (= v1 v9) 0 1))
      (loop sum (if (> (if (<= 220 d)
                           (if (and (< 220 d)
                                    (>= 1024 d))

                               7)
                           220) 0)
                    1 2))))))

(with-test (:name :set-constraint)
  (assert-type
   (lambda (x)
     (when (> (decf x 10) 10)
       x))
   (or (real (10)) null)))

(with-test (:name :set-constraint-inherit)
  (checked-compile-and-assert
   ()
   `(lambda (b c)
      (declare (type (integer 1 1625159256) b))
      ((lambda (v2 v6)
         (logorc1 (setf c 1448416540835)
                  (gcd
                   (if (>= b c)
                       (if (< b c)
                           0
                           -197521449755177454)
                       0)
                   (decf v2 (incf v2 (incf v6 (setq v2 (setq b 1044756040))))))))
       0 0))
   ((572837281 0) -1448416540836)))

(with-test (:name :float-eql-bits)
  (assert-type
   (lambda (number)
     (declare (type double-float number))
     (cond ((eql number 1d0) number)))
   (or (eql 1.0d0) null)))

(with-test (:name :constraint-amount)
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (v)
                        (loop for i below (- (length v) 2) by 2
                              sum (svref v (1+ i))))
                     nil))
             0))
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (v)
                        (loop for i from (- (length v) 2) downto 0
                              collect (svref v (1+ i))))
                     nil))
             0))
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (declare (type fixnum a))
         (let ((j (+ a most-positive-fixnum)))
           (let ((bbb a))
             (dotimes (i a)
               (incf bbb))
             (< bbb j))))
    ((3) t)))

(with-test (:name :constraint-multiple-eql-variables)
  (assert-type
   (lambda (x y)
     (declare (integer x y)
              (optimize (debug 2)))
     (assert (< x y))
     (< x y))
   (eql t)))

(with-test (:name :dead-blocks)
  (assert-type
   (lambda (c d)
     (declare ((integer -31307831194 26651266057) c))
     (let ((v5 (if d
                   -206316434836409151
                   17179869177)))
       (if (/= v5 c)
           1
           (if (> v5 c)
               (logior
                c
                (loop for lv2 below 1
                      sum
                      (rem lv2 128)))
               1))))
   (eql 1)))

(with-test (:name :dead-blocks.2)
  (checked-compile `(lambda (a b)
                      (declare (type (integer 0 171951449) a)
                               ((member -1965785401190741689 18014398509481983) b)
                               (optimize (debug 2)))
                      (let ((v3 b)
                            (v6 116510974941639))
                        (if (> v6 v3)
                            (if (> v3 v6)
                                (let ((v3 (logand a v3)))
                                  (loop for lv1 below 3
                                        count (let ((v8 (min lv1 v3)))
                                                (<= v8  576460752303423490))))))))))

(with-test (:name :multiple-equality-constraints)
  (checked-compile
   `(lambda (v)
      (declare (simple-vector v))
      (let* ((i (1- (length v))))
        (declare (fixnum i))
        (loop
         (decf i)
         (print (aref v i)))))))

(with-test (:name :min/max-constraints-set)
  (checked-compile-and-assert
   ()
   `(lambda (a b c)
      (let ((x
              (if (< a c)
                  c
                  a))
            (y (setq a (min most-positive-fixnum b))))
        (> y x)))
   ((1 5 3) t)
   ((2 4 6) nil)))

(with-test (:name :mod)
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (v i)
                        (svref v (mod i (length v))))
                     nil))
             0)))

(with-test (:name :index-ranges)
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (a)
                        (when (= (length a) 2)
                          (svref a 1)))
                     nil))
             0))
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (a l i)
                        (declare ((integer 2 3) l)
                                 ((integer 0 1) i))
                        (when (= (length a) l)
                          (svref a i)))
                     nil))
             0)))

(with-test (:name :constant-vector)
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (x)
                        (declare (fixnum x))
                        (and (<= x 2)
                             (aref #(1 2 3) x)))
                     nil))
             0)))

(with-test (:name :read-sequence-bounds)
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (string stream)
                        (declare (simple-string string))
                        (loop for i below (read-sequence string stream)
                              count (char= (char string i) #\a)))
                     nil))
             0))
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (string stream)
                        (declare (simple-string string))
                        (loop for p = (read-sequence string stream)
                              while (plusp p)
                              sum
                              (loop for i below p
                                    count (char= (char string i) #\a))))
                     nil))
             0)))

(with-test (:name :map-equality-constraints)
  (checked-compile-and-assert
   ()
   `(lambda (c d)
      (declare (notinline identity)
               (bit d))
      (dotimes (e 3 (identity (logior c 10 (- d -6 e))))))
   ((2 1) 14)
   ((9 0) 11)))

(with-test (:name :local-call)
  (assert-type
   (lambda (a)
     (flet ((b ()
              a))
       (declare (notinline b))
       (the integer a)
       (b)
       (b)))
   integer))

(with-test (:name :set-vars-equal)
  (assert-type
   (lambda (a)
     (incf a 10)
     (eq a (progn (loop repeat a do (print 20)) a)))
   (eql t))
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (logxor (unwind-protect a (setq a 7)) a))
  ((0) 7)
  ((7) 0)))

(with-test (:name :set-vars-equal-leaf-change)
  (checked-compile-and-assert
   ()
   `(lambda (b)
      (declare (integer b))
      (let ((c 1))
        (decf c (shiftf b b))))
   ((3) -2)
   ((-4) 5)))

(with-test (:name :all-var-values)
  (checked-compile
   `(lambda (c)
      (declare (optimize (safety 0)))
      (loop for lv3 below 1
            do
            (loop for lv4 below 3
                  sum (progv nil nil
                        (setq c 1000)))
            (unless (eq  lv3 -1)
              (the integer (catch 'ct4 16385)))))))

(with-test (:name :cons-cells)
  (assert-type
   (lambda (x)
     (if (car x)
         (consp x)
         t))
   (eql t))
  (assert-type
   (lambda (x)
     (if (car x)
         nil
         (consp x)))
   boolean)
  (assert-type
   (lambda (x)
     (if (> (car x) 1)
         (consp x)
         t))
   (eql t))
  (assert-type
   (lambda (x)
     (if (> (car x) 1)
         t
         (consp x)))
   (eql t)))

(with-test (:name :char-code)
  (assert-type
   (lambda (x)
     (let ((n (char-code x)))
       (when (or (< 31 n 127) (= n 10))
         x)))
   (or null standard-char))
  (assert-type
   (lambda (x)
     (declare (optimize (debug 2)))
     (when (standard-char-p x)
       x))
   (or null standard-char)))

(with-test (:name :vector-length-alias)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (or (typep x '(integer 3 3))
             (typep x '(simple-array character (3)))))
    ((3) t)
    (("000") t)
    ((6) nil)
    (("") nil)))

(with-test (:name :eql-var-replacement)
  (checked-compile-and-assert
      (:allow-notes nil)
      `(lambda (x y)
         (declare (float x)
                  ((simple-array double-float (*)) y))
         (let ((d (aref y 0)))
           (when (and (/= d 0)
                      (eql d x))
             x)))
    ((1d0 (make-array 1 :element-type 'double-float :initial-element 1d0)) 1d0 :test #'equal)
    ((1d0 (make-array 1 :element-type 'double-float :initial-element 0d0)) nil)
    ((0d0 (make-array 1 :element-type 'double-float :initial-element 1d0)) nil)
    ((0d0 (make-array 1 :element-type 'double-float :initial-element 0d0)) nil)))

(with-test (:name :eql-var-initial-unused)
  (checked-compile-and-assert
      ()
      `(lambda (p1 p2 p3)
         (declare (type (integer -5) p3))
         (let ((n (the (integer 0) p3)))
           (setq n (dpb p1 (byte p2 35) n))
           n))
    ((1 2 3) 34359738371)))

(with-test (:name :%in-bounds-constraint-not-eq)
  (checked-compile-and-assert
      ()
      `(lambda (sv key n)
         (declare (fixnum n) (simple-vector sv))
         (let ((found (find key sv :end n)))
           (if (< n (length sv))
               -1
               found)))
    ((#(1 2 3) 2 2) -1)
    ((#(1 2 3) 2 3) 2)
    ((#(1 2 3) 4 3) nil)))

(with-test (:name :characters)
  (assert-type
   (lambda (c)
     (cond ((eql c #\a) 1)
           ((eql c #\a) 2)
           (t 1)))
   (eql 1))
  (assert-type
   (lambda (c)
     (cond ((eql c #\a) 1)
           ((eql c #\b) 1)
           ((eql c #\a) 2)
           (t 1)))
   (eql 1))
  (assert-type
   (lambda (x)
     (and (char= x #\a) x))
   (or (eql #\a) null)))

(with-test (:name :multiply-by-one)
  (assert-type
   (lambda (x)
     (the (unsigned-byte 8) (* x 1))
     x)
   (unsigned-byte 8)))

(with-test (:name :multiply-by-zero)
  (assert-type
   (lambda (b c)
     (declare ((integer 0 2) b)
              ((integer 1 10) c))
     (if (typep (* b c) '(integer * 0))
         (values b c)
         (values nil nil)))
   (values (or null (integer 0 0))
           (or null (integer 1 10)) &optional))
  (assert-type
   (lambda (b c)
     (declare ((integer 0 2) b)
              ((integer -10 -2) c))
     (if (typep (* b c) '(integer 0 1))
         (values b c)
         (values nil nil)))
   (values (or null (integer 0 0))
           (or null (integer -10 -2)) &optional)))

(with-test (:name :length-back)
  (assert-type
   (lambda (x)
     (if (= (length x) 0)
         x
         (error "")))
   (and sequence (not cons)))
  (assert-type
   (lambda (x)
     (if (= (length x) 0)
         (error "")
         x))
   (and sequence (not null)))
  (assert-type
   (lambda (x)
     (if (> (length x) 0)
         x
         (error "")))
   (and sequence (not null)))
  (assert-type
   (lambda (x)
     (if (> (length x) 0)
         (error "")
         x))
   (and sequence (not cons)))
  (assert-type
   (lambda (x)
     (if (< (length x) 1)
         x
         (error "")))
   (and sequence (not cons)))
  (assert-type
   (lambda (x)
     (if (< (length x) 1)
         (error "")
         x))
   (and sequence (not null)))
  (assert-type
   (lambda (x)
     (if (< (length x) 10)
         x
         (error "")))
   sequence)
  (assert-type
   (lambda (x)
     (if (< (length x) 10)
         (error "")
         x))
   (and sequence (not null))))

(with-test (:name :find-item-type)
  (assert-type
   (lambda (x y)
     (declare (string y))
     (if (find x y)
         x
         (error "")))
   character)
  (assert-type
   (lambda (x y)
     (declare (string y))
     (if (position x y)
         x
         (error "")))
   character)
  (assert-type
   (lambda (x y)
     (if (position x y :test #'=)
         x
         (error "")))
   number)
  (assert-type
   (lambda (x y)
     (if (position x y :key #'1+)
         x
         (error "")))
   number)
  (assert-type
   (lambda (b v)
     (declare ((vector (unsigned-byte 8)) v))
     (if (find b v :test #'>)
         b
         (error "")))
   real)
  (assert-type
   (lambda (a v)
     (declare (fixnum a)
              ((vector (unsigned-byte 8)) v))
     (if (find a v :test #'=)
         a
         (error "")))
   (unsigned-byte 8))
  (assert-type
   (lambda (x y)
     (if (member x y :key #'1+)
         (values x y)
         (error "")))
   (values number cons &optional))
  (assert-type
   (lambda (x y)
     (if (member x y :test #'string=)
         (values x y)
         (error "")))
   (values (or string symbol character) cons &optional))
  (assert-type
   (lambda (x y)
     (if (position x y :key #'1+ :test-not #'eql)
         x
         (error "")))
   t)
  (assert-type
   (lambda (x y)
     (if (position x y :test-not #'=)
         x
         (error "")))
   number)
  (assert-type
   (lambda (x y)
     (if (find x y)
         x
         (error "")))
   (not null))
  (assert-type
   (lambda (x y)
     (declare (vector y))
     (if (find x y)
         x
         (error "")))
   (not null)))

(with-test (:name :member-list-type)
  (assert-type
   (lambda (x y)
     (if (member x y)
         y
         (error "")))
   cons))

(with-test (:name :string=)
  (assert-type
   (lambda (y end)
     (when (string= "ab" y :end1 end)
       y))
   (or string symbol character))
  (assert-type
   (lambda (y)
     (when (string= "ab" y)
       y))
   (or string symbol))
  (assert-type
   (lambda (x y)
     (when (string= x y :end1 2)
       y))
   (or string symbol))
  (assert-type
   (lambda (y)
     (when (string-equal "ab" y)
       y))
   (or string symbol))
  (assert-type
   (lambda (y)
     (when (string-equal y "ab")
       y))
   (or string symbol)))

(with-test (:name :find-sequence-type)
  (assert-type
   (lambda (y)
     (declare (simple-string y))
     (when (position #\a y)
       (length y)))
   (or null (integer 1 (#.array-dimension-limit))))
  (assert-type
   (lambda (y)
     (if (find 1 y)
         y
         (error "x")))
   (and sequence (not null)))
  (assert-type
   (lambda (y)
     (declare (array y))
     (if (find t y)
         y
         (error "")))
   (vector t))
  (assert-type
   (lambda (y)
     (if (position t y)
         y
         (error "")))
   (and sequence (not null) (or (not array) (vector t))))
  (assert-type
   (lambda (x y)
     (declare (simple-array y))
     (if (find x y :key #'car)
         y
         (error "")))
   simple-vector))

(with-test (:name :equal-length)
  (assert-type
   (lambda (y x)
     (declare (simple-vector x y))
     (if (equalp y x)
         (= (length y) (length x))
         t))
   (eql t))
  (assert-type
   (lambda (x y)
     (declare (simple-vector y x)
              ((simple-vector 10) x))
     (if (equalp x y)
         (length y)
         (error "")))
   (eql 10))
  (assert-type
   (lambda (x y)
     (declare (simple-vector y x))
     (if (and (eql (length x) 9)
              (equalp x y))
         (length y)
         (error "")))
   (eql 9))
  (assert-type
   (lambda (x y)
     (declare (simple-vector y x))
     (if (and (> (length x) 5)
              (equalp x y))
         (length y)
         (error "")))
   (integer 6 (#.array-dimension-limit)))
  (assert-type
   (lambda (x)
     (declare (simple-string x))
     (if (string= x "abc")
         (length x)
         (error "")))
   (eql 3)))

(with-test (:name :search-length)
  (assert-type
   (lambda (x)
     (declare (simple-string x))
     (if (search "abc" x)
         (length x)
         (error "")))
   (integer 3 (#.array-dimension-limit)))
  (assert-type
   (lambda (a b)
     (declare (simple-array a b))
     (when (and (> (length a) 3)
                (search a b))
       (length b)))
   (or null (integer 4 (#.array-dimension-limit)))))

(with-test (:name :subseq)
  (assert-type
   (lambda (y s e)
     (declare (simple-vector y)
              ((integer 1 2) s)
              ((integer 7 8) e)
              (optimize (debug 2)))
     (let ((s (subseq y s e)))
       (length s)))
   (integer 5 7))
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (y n)
                        (declare (simple-vector y)
                                 (integer n))
                        (let ((s (subseq y 0 n)))
                          (aref s (1- n))))
                     nil))
             0))
  (assert-type
   (lambda (y)
     (declare (simple-vector y)
              (optimize (debug 2)))
     (when (< (length y) 6)
       (let ((s (subseq y 1 nil)))
         (length s))))
   (or null (mod 5)))
  (assert-type
   (lambda (y)
          (declare ((simple-array * (*)) y)
                   (optimize (debug 2)))
          (let ((copy (copy-seq y)))
            (eq (length copy) (length y))))
   (eql t)))

(with-test (:name :remove)
  (assert-type
   (lambda (x)
     (declare ((simple-vector 9) x)
              (optimize (debug 2)))
     (let ((j (delete 5 x)))
       (length j)))
   (mod 10))
  (assert-type
   (lambda (x)
     (declare (simple-vector x)
              (optimize (debug 2)))
     (when (< (length x) 5)
       (let ((j (remove-if #'evenp x)))
         (length j))))
   (or null (mod 5))))

(with-test (:name :zero-length-check-bound)
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (x)
                        (declare (simple-string x))
                        (unless (string= x "")
                          (aref x 0)))
                     nil))
             0)))

(with-test (:name :concatenate-length)
  (assert-type
   (lambda (x y)
     (declare ((simple-vector 10) x)
              (optimize (debug 2)))
     (let ((x (concatenate 'vector x y)))
       (length x)))
   (integer 10 (#.array-dimension-limit)))
  (assert-type
   (lambda (x)
     (declare (optimize (debug 2)))
     (let ((x (concatenate 'vector '(1 2 3) (subseq x 1))))
       (length x)))
   (integer 3 (#.array-dimension-limit)))
  (assert-type
   (lambda (x)
     (declare ((simple-vector 3) x)
              (optimize (debug 2)))
     (let ((x (concatenate 'vector x (if * "abc" "abcd"))))
       (length x)))
   (integer 6 7))
  (assert-type
   (lambda (x)
     (declare (optimize (debug 2)))
     (let ((x (concatenate 'vector "00" (subseq x 0 2))))
       (length x)))
   (integer 4 4)))

(with-test (:name :inherit-length-var)
  (assert (not (ctu:ir1-named-calls
                `(lambda (x y n)
                   (declare (optimize (space 0))
                            (fixnum n)
                            (simple-base-string y))
                   (when (< n (length y))
                     (find x y :start n))))))
  (assert-type
   (lambda (x y)
     (declare (simple-vector x))
     (if (< y (length x))
         (let ((m (length x)))
           (if (< y m)
               t
               m))
         t))
   (eql t)))

(with-test (:name :multiple-back)
  (assert-type
   (lambda (m)
     (the (integer 0 30) (+ (* m 2) 2))
     m)
   (rational -1 14)))
