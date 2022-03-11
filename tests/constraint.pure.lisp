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
