;;;; various compiler tests without side effects

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

;;;; This file strives to do better on all fronts:
;;;; the tests should be fast, named, and not noisy.

(enable-test-parallelism)

(with-test (:name :return-constant-reuse)
  (checked-compile-and-assert
      ()
      `(lambda ()
         #1=(values 0 1 2 3 :c1 :c2 :c3 :c4 :c5 :c6 :c6 :c7 :c7 :c8 :c5 :c4 :c3 :c2 :c1))
    (() #1#)))

(with-test (:name :reuse-move-coercion-scs)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (declare (fixnum a))
         (+ (let ((v
                    (if (< 0 a)
                        ,(ldb (byte sb-vm:n-word-bits 0) 13174327107650979063)
                        a)))
              (if (< v a)
                  a
                  v))
            3))
    (((logand most-positive-fixnum 12917690363219115))
     (ldb (byte sb-vm:n-word-bits 0) 13174327107650979066))
    ((-1) 2)))

(with-test (:name :node-conservative-type-list-uses)
  (assert-type
   (lambda (x y)
     (declare ((vector * 10) x)
              ((vector * 20) y))
     (length (the string (if *
                             x
                             y))))
   (mod 21)))

(with-test (:name :unknown-keys-type-derivation)
  (assert-type
   (lambda (a)
     (make-array 1 a t))
   (vector))
  (assert-type
   (lambda (a)
     (make-array 1 :element-type t a 0))
   (vector t)))

(with-test (:name :ir1-final-unreachable-cast-uses)
  (checked-compile-and-assert
      ()
      `(lambda (a y)
         (declare ((or null (mod 5)) y))
         (let ((x (if y (1+ y))))
           (if a (+ x 3))))
    ((nil nil) nil)
    ((t 2) 6))
  (checked-compile-and-assert
      ()
      `(lambda (a y)
         (declare ((or null fixnum) y))
         (let ((x (if y
                      (+ y 2)
                      nil)))
           (when a
             (+ x 3))))
    ((nil nil) nil)
    ((t 2) 7))
  (checked-compile '(lambda ()
                     (declare (optimize (safety 0)))
                     (the integer (flet ((f () (catch 'x)))
                                    (f)))))
  (checked-compile '(lambda ()
                     (declare (optimize (safety 0)))
                     (throw 'x
                       (the integer
                            (flet ((f () (round 0)))
                              (block a
                                (values nil
                                        (block b
                                          (let ((* (lambda () (return-from b))))
                                            (return-from a (f)))))))))))
  (checked-compile '(lambda (v)
                     (declare (optimize (safety 0)))
                     (the integer (if v (funcall v))))))

(with-test (:name :throw-any-reg)
  (checked-compile `(lambda () (throw (the fixnum *) 1))
                   :allow-style-warnings t))

(with-test (:name :local-call-compute-old-nfp)
  (checked-compile-and-assert
   ()
   `(lambda (in)
      (declare (fixnum in))
      (flet ((f (im)
               (opaque-identity 1)
               (values (logand most-positive-word (* im 3))
                       (logand most-positive-word (* im 4))
                       (logand most-positive-word (* im 5))
                       (logand most-positive-word (* im 6))
                       (logand most-positive-word (* im 7))
                       (logand most-positive-word (* im 8))
                       (logand most-positive-word (* im 9))
                       (logand most-positive-word (* im 10))
                       (logand most-positive-word (* im 11))
                       (logand most-positive-word (* im 12))
                       (logand most-positive-word (* im 13))
                       (logand most-positive-word (* im 14)))))
        (declare (notinline f))
        (with-alien ((buf (array int 10)))
          (multiple-value-bind (a b c d e f g h i j k l) (f in)
            (values (logand #xFFFFFFF a)
                    (logand #xFFFFFFF b)
                    (logand #xFFFFFFF c)
                    (logand #xFFFFFFF d)
                    (logand #xFFFFFFF e)
                    (logand #xFFFFFFF f)
                    (logand #xFFFFFFF g)
                    (logand #xFFFFFFF h)
                    (logand #xFFFFFFF i)
                    (logand #xFFFFFFF j)
                    (logand #xFFFFFFF k)
                    (logand #xFFFFFFF l))))))
   ((1000) (values 3000 4000 5000 6000 7000 8000 9000 10000 11000 12000 13000 14000))))

(with-test (:name :recursive-calls-inherit-constraints)
  (checked-compile-and-assert
   ()
   `(lambda ()
      (labels ((m (i j)
                 (if (> i 5)
                     (values i j)
                     ((lambda (&optional jj)
                        (m (+ i 1) jj) )
                      i))))

        (m 0 0)))
   (() (values 6 5))))
