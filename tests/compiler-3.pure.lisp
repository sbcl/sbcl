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
   (mod 21))
  (checked-compile `(lambda (a)
                      (declare (optimize debug))
                      (nth 0
                           (handler-case 1
                             (error ()
                               (typecase a
                                 ((member -3) (list 3))
                                 (integer (list 1)))))))
                   :allow-style-warnings t))

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

(with-test (:name :local-call-compute-old-nfp
            :fails-on (and :c-stack-is-control-stack
                           (not (or :arm :arm64 :ppc64))))
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

(with-test (:name :eq-integer-word)
  (checked-compile
   `(lambda (n m)
      (declare (optimize speed)
               (integer m)
               (fixnum n))
      (eql m (logior (1+ n) 1)))
   :allow-notes nil))

(with-test (:name :select-tagging
            :implemented-on (or :arm64 :x86-64))
  (checked-compile
   `(lambda (x)
      (declare (optimize speed))
      (let ((digit (sb-bignum:%bignum-ref x 1)))
        (= (sb-c::mask-signed-field 16 digit)
           digit)))
   :allow-notes nil)
  (checked-compile
   `(lambda (x)
      (declare (optimize speed))
      (let ((digit (sb-c::mask-signed-field sb-vm:n-word-bits (sb-bignum:%bignum-ref x 1))))
        (= (sb-c::mask-signed-field 16 digit)
           digit)))
   :allow-notes nil)
  (checked-compile
   `(lambda (a n)
      (let ((v (- (ldb (byte 32 0) n))))
        (setq v 32768)
        (ldb (byte 32 0) (+ a v))))))

(with-test (:name :round-transform-too-early)
  (checked-compile
   `(lambda (f)
      (declare (optimize speed))
      (when (typep f 'single-float)
        (values (round f))))
   :allow-notes nil))

(with-test (:name (:local-function-result-type :through-cast))
  (checked-compile-and-assert
   ()
   `(lambda (n v)
      (declare (type (integer 0 10) n) (simple-vector v))
      (labels ((f (n)
                 (if (zerop n)
                     0
                     (multiple-value-prog1 (f (1- n))
                       (setf (svref v n) n)))))
        (f n)))
   (:return-type (eql 0))
   ((3 (make-array 4 :initial-element nil)) 0))
  ;; F is in the tail set of the lambda.
  (checked-compile-and-assert
   ()
   `(lambda (x v)
      (declare (simple-vector v))
      (flet ((f (x) (if (consp x) 1 2)))
        (if (symbolp x)
            (f x)
            (multiple-value-prog1 (f x)
              (setf (svref v 0) x)))))
   (:return-type (integer 1 2))
   (('a (make-array 1)) 2)
   (('(a) (make-array 1)) 1)))

(with-test (:name (:recursive-local-function-result-type :count))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (declare (type (integer 0 100) x))
      (labels ((f (n) (if (zerop n) 0 (1+ (f (1- n))))))
        (f x)))
   (:return-type unsigned-byte)
   ((0) 0)
   ((100) 100)))

(with-test (:name (:recursive-local-function-result-type :fib))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (declare (type (integer 0 30) x))
      (labels ((fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))
        (fib x)))
   (:return-type unsigned-byte)
   ((0) 0)
   ((1) 1)
   ((20) 6765)))

(with-test (:name (:recursive-local-function-result-type :tree-depth))
  (checked-compile-and-assert
   ()
   `(lambda (tree)
      (labels ((depth (x)
                 (if (consp x)
                     (1+ (max (depth (car x)) (depth (cdr x))))
                     0)))
        (depth tree)))
   (:return-type unsigned-byte)
   ((nil) 0)
   (('(1 (2 (3)))) 5)))

(with-test (:name (:recursive-local-function-result-type :count-leaves))
  (checked-compile-and-assert
   ()
   `(lambda (tree)
      (labels ((c (x)
                 (cond ((null x) 0)
                       ((atom x) 1)
                       (t (+ (c (car x)) (c (cdr x)))))))
        (c tree)))
   (:return-type unsigned-byte)
   ((nil) 0)
   (('(a (b c) d)) 4)))

(with-test (:name (:recursive-local-function-result-type :max))
  (checked-compile-and-assert
   ()
   `(lambda (l)
      (declare (list l))
      (labels ((f (l)
                 (if l
                     (max (the fixnum (car l)) (f (cdr l)))
                     0)))
        (f l)))
   ;; FIXME: Ideally this would be (AND FIXNUM UNSIGNED-BYTE).
   ;; MAX becomes (IF (> R A) R A), and knowing that A is not negative
   ;; in the second branch would take the relation between A and R.
   (:return-type fixnum)
   ((nil) 0)
   (('(-5 3 -2)) 3)))

(with-test (:name (:recursive-local-function-result-type :mutual))
  (assert-type
   (lambda (x)
     (declare (type (integer 0 100) x))
     (labels ((ev (n) (if (zerop n) t (od (1- n))))
              (od (n) (if (zerop n) nil (ev (1- n)))))
       (ev x)))
   boolean))

(with-test (:name (:recursive-local-function-result-type :list))
  (assert-type
   (lambda (x)
     (declare (type (integer 0 100) x))
     (labels ((f (n) (if (zerop n) nil (cons n (f (1- n))))))
       (f x)))
   list))

;;; Result types of recursive functions and optimistic parameter types
;;; depending on each other.
(with-test (:name (:recursive-local-function-result-type :parameter-from-result))
  (checked-compile-and-assert
   ()
   `(lambda (n)
      (declare (type (integer 0 100) n))
      (labels ((f (n) (if (zerop n) 0 (1+ (f (1- n)))))
               (g (x) (if (< x 10) (g (1+ x)) x)))
        (g (f n))))
   (:return-type (integer 10))
   ((3) 10)
   ((20) 20)))

(with-test (:name (:recursive-local-function-result-type :result-from-parameter))
  (checked-compile-and-assert
   ()
   `(lambda (n)
      (declare (type (integer 0 100) n))
      (labels ((f (n acc) (if (zerop n) acc (1+ (f (1- n) (1+ acc))))))
        (f n 0)))
   (:return-type unsigned-byte)
   ((0) 0)
   ((5) 10)))

(with-test (:name (:recursive-local-function-result-type :result-to-own-parameter))
  (checked-compile-and-assert
   ()
   `(lambda (n)
      (declare (type (integer 0 100) n))
      (labels ((f (n) (if (zerop n) 0 (1+ (f (min 100 (f (1- n))))))))
        (f n)))
   (:return-type unsigned-byte)
   ((0) 0)
   ((1) 1)
   ((2) 2)))

(with-test (:name (:recursive-local-function-result-type :parameter-result-cycle))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (declare (type (integer 0 5) x))
      (labels ((f (p n) (if (zerop n) p (1+ (f (f p (1- n)) (1- n))))))
        (f 0 x)))
   (:return-type unsigned-byte)
   ((0) 0)
   ((2) 3)))

;;; F and G are in different tail sets.
(with-test (:name (:recursive-local-function-result-type :mutual-tail-sets))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (declare (type (integer 0 100) x))
      (labels ((f (n) (if (zerop n) 0 (1+ (g (1- n)))))
               (g (n) (if (zerop n) 0 (* 2 (f (1- n))))))
        (+ (f x) (g x))))
   (:return-type unsigned-byte)
   ((0) 0)
   ((3) 5)))

(with-test (:name (:recursive-local-function-result-type :stored-into-array))
  (checked-compile-and-assert
   ()
   `(lambda (n v)
      (declare (type (integer 0 10) n) (type (vector fixnum) v))
      (labels ((f (n) (if (zerop n) 0 (setf (aref v 0) (1+ (f (1- n)))))))
        (f n)))
   ((3 (make-array 1 :element-type 'fixnum :adjustable t)) 3))
  (checked-compile-and-assert
   ()
   `(lambda (n v)
      (declare (type (integer 0 10) n) (type (simple-array fixnum (1)) v))
      (labels ((f (n) (if (zerop n) 0 (setf (aref v 0) (1+ (f (1- n)))))))
        (f n)))
   (:return-type (and fixnum unsigned-byte))
   ((3 (make-array 1 :element-type 'fixnum)) 3)))

(with-test (:name (:recursive-local-function-result-type :multiple-values))
  (checked-compile-and-assert
   ()
   `(lambda (n)
      (declare (type (integer 0 10) n))
      (labels ((f (n)
                 (if (zerop n)
                     (values 0 :x)
                     (let ((r (f (1- n))))
                       (values (1+ r) :y)))))
        (f n)))
   (:return-type (values number (member :x :y) &optional))
   ((0) (values 0 :x))
   ((3) (values 3 :y)))
  ;; BOTH is in the tail set of the lambda, and returns what SUBTYPEP does.
  (assert-type
   (lambda (x y)
     (flet ((both (type) (and (typep x type) (subtypep y type))))
       (or (both 'number) (both 'cons))))
   (values boolean &optional boolean)))

(with-test (:name (:recursive-local-function-result-type :stored-into-cons))
  (assert-type
   (lambda (n c)
     (declare (type (integer 0 10) n))
     (labels ((f (n) (if (zerop n) 0 (setf (car c) (1+ (f (1- n)))))))
       (f n)))
   unsigned-byte))

(with-test (:name (:optimistic-type :step-narrower-than-initial-value))
  (checked-compile-and-assert
   ()
   `(lambda (c n)
      (declare (fixnum n))
      (labels ((f (x n) (if (<= n 0) x (f (ash x -8) (1- n)))))
        (integerp (f (cdr c) n))))
   (((cons 1 :x) 0) nil)
   (((cons 1 256) 1) t)))

;;; FOUND is set to what G returns, which is only solved for once
;;; FOUND's type is first assumed.
(with-test (:name (:optimistic-type :set-to-recursive-result))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (let ((found nil))
        (labels ((g (y) (if (consp y) (list (g (car y))) y)))
          (setq found (g x)))
        (if found :yes :no)))
   ((5) :yes)
   ((nil) :no)))

(with-test (:name :ir1-optimize-return-unused-recursive-result-type)
  (checked-compile-and-assert
      (:allow-style-warnings t)
      `(lambda (a b c)
         (labels ((%f8 (f8-1) (declare (ignore f8-1)) 0))
           (flet ((%f14 (f14-1 &optional (f14-2 a) (f14-3 b) (f14-4 0))
                    (declare (ignore f14-1 f14-4))
                    (case f14-3
                      ((-992)
                       (multiple-value-bind (v3 v5)
                           (if (%f8 0) (values 0 (%f8 f14-3)) (values 0 f14-2))
                         (declare (ignore v3 v5))
                         0))
                      (t 1))))
             (case (multiple-value-bind (v5 v6)
                       (if (%f14 0 0 0) (values a (%f14 0 b 0 a)) (values c 0))
                     (declare (ignore v5))
                     (case (identity c) ((12) 0) ((12) (%f14 v6)) (t a)))
               ((-5024) (%f8 0))
               (t (multiple-value-call #'%f8 (values 0)))))))
    ((1 2 3) 0))
  (checked-compile-and-assert
      ()
      `(lambda (c)
         (labels ((f (n) n)
                  (g (&optional k l)
                    (declare (ignore l))
                    (if k
                        (values 0 (f k))
                        t)))
           (unless c
             (if c
                 (the integer (g))))
           (g 0)
           (g 0 0)
           (f 0)))
    ((t) 0)
    ((nil) 0)))

(with-test (:name :*-rational-by-zero-type)
  (assert-type
   (lambda (n)
     (declare (rational n))
     (* n 0))
   (eql 0))
  (assert-type
   (lambda (n)
     (declare ((rational 4 9) n))
     (* n 0))
   (eql 0))
  (assert-type
   (lambda (n)
     (declare (integer n))
     (* n 8))
   (or (integer * -8) (integer 0 0) (integer 8)))
  (assert-type
   (lambda (n)
     (declare ((or float integer) n))
     (* n 7))
   (or float (integer * -7) (integer 0 0) (integer 7)))
  (assert-type
   (lambda (n)
     (declare (real n))
     (* n 0))
   (or float (eql 0)))
  (assert-type
   (lambda (n)
     (* n 0))
   (or (eql 0) float (complex float))))

(with-test (:name :/zero-type)
  (assert-type
   (lambda (n)
     (declare (rational n))
     (/ 0 n))
   (eql 0))
  (assert-type
   (lambda (n)
     (/ 0 n))
   (or (eql 0) float (complex float)))
  (assert-type
   (lambda (n)
     (truncate 0 n))
   (values (eql 0) (member 0.0d0 0.0 0) &optional))
  (assert-type
   (lambda (n)
     (truncate 0.0 n))
   (values (eql 0) (member 0.0d0 0.0) &optional))
  (assert-type
   (lambda (m n)
     (declare ((real 0 0) m))
     (truncate m n))
   (values (eql 0) (real 0 0) &optional))
  (assert-type
   (lambda (n)
     (floor 0 n))
   (values (eql 0) (member 0.0d0 0.0 0) &optional))
  (assert-type
   (lambda (n)
     (ceiling 0 n))
   (values (eql 0) (member 0.0d0 0.0 0) &optional))
  (assert-type
   (lambda (n)
     (ffloor 0 n))
   (values float (or float (integer 0 0)) &optional))
  (assert-type
   (lambda (n)
     (ffloor 0 (the rational n)))
   (values (single-float 0.0 0.0) (eql 0) &optional))
  (assert-type
   (lambda (n)
     (/ n 0))
   (or float (complex float)))
  (assert-type
   (lambda (n)
     (ftruncate 0 n))
   (values float (or float (eql 0)) &optional)))

(with-test (:name :-zero-type)
  (assert-type
   (lambda (n)
     (declare ((real 10 20) n))
     (- n n))
   (member 0.0d0 0.0 0))
  (assert-type
   (lambda (n)
     (declare (rational n))
     (- n n))
   (eql 0))
  (assert-type
   (lambda (n)
     (- n n))
   (or (eql 0) float (complex float))))

(with-test (:name :/-same-type)
  (assert-type
   (lambda (n)
     (declare ((real 1 20) n))
     (/ n n))
   (real 1 1))
  (assert-type
   (lambda (n)
     (declare ((real 1) n))
     (/ n n))
   (or (eql 1) float))
  (assert-type
   (lambda (n)
     (/ n n))
   (or (eql 1) float (complex float)))
  (assert-type
   (lambda (n)
     (truncate n n))
   (values (eql 1) (member 0 0d0 0f0) &optional))
  (assert-type
   (lambda (n)
     (declare (rational n))
     (truncate n n))
   (values (eql 1) (eql 0) &optional))
  (assert-type
   (lambda (n)
     (declare (rational n))
     (ffloor n n))
   (values (single-float 1.0 1.0) (integer 0 0) &optional))
  (assert-type
   (lambda (n)
     (ftruncate n n))
   (values float (or float (eql 0)) &optional))
  (assert (not (ctu:ir1-named-calls `(lambda (n)
                                       (declare ((integer 10 300) n))
                                       (ffloor n n))))))

(with-test (:name (:recursive-local-function-result-type :parameter-from-own-result :lp2168790))
  (checked-compile-and-assert
   ()
   `(lambda ()
      (flet ((f (n)
               (1+ n)))
        (f (f 0))))
   (() 2)))

(with-test (:name (:recursive-local-function-result-type :parameter-from-own-result :shrinking))
  (checked-compile-and-assert
   ()
   `(lambda ()
      (flet ((f (n)
               (* n n)))
        (f (f 2))))
   (() 16)))

(with-test (:name (:recursive-local-function-result-type :parameter-from-own-result :mixed-numbers))
  (checked-compile-and-assert
   ()
   `(lambda ()
      (flet ((f (n)
               (+ n 0.5)))
        (f (f 0))))
   (() 1.0)))

(with-test (:name (:recursive-local-function-result-type :parameter-from-own-result :characters))
  (checked-compile-and-assert
   ()
   `(lambda ()
      (flet ((f (c)
               (code-char (1+ (char-code c)))))
        (f (f #\a))))
   (() #\c)))

(with-test (:name (:setq-derive-type :declared-counter))
  (assert-type
   (lambda (l)
     (let ((c 0))
       (declare (fixnum c))
       (dolist (x l c)
         (when x
           (incf c)))))
   (and unsigned-byte fixnum)))

(macrolet ((def (name init step type)
             `(with-test (:name (:setq-derive-type ,name))
                (assert-type
                 (lambda (n)
                   (declare (fixnum n))
                   (let ((x ,init))
                     (dotimes (i n x)
                       (setq x ,step))))
                 ,type))))
  (def :guarded 0 (if (< x 50) (1+ x) x) (mod 51))
  (def :either-way 50 (cond ((< x 40) (+ x 3)) ((> x 60) (- x 3)) (t x))
    (integer 50 50))
  (def :min 0 (min 100 (+ x 3)) (or (integer 0 0) (integer 3 100)))
  (def :max 100 (max 0 (- x 3)) (or (mod 98) (integer 100 100)))
  (def :abs -5 (abs (1- x)) (or (integer -5 -5) (mod 7)))
  (def :logior 1 (logior x (ash x 1)) (integer 1))
  (def :square 2 (* x x) (or (integer 2 2) (integer 4)))
  (def :integer-to-float 0 (+ x 0.5)
    (or (integer 0 0) (single-float 0.5 0.5) (single-float 1.0)))
  (def :mixed 0 (if (> x 10) (/ x 2.0) (1+ x)) (or (mod 12) (single-float 5.0))))

(with-test (:name (:setq-derive-type :negated))
  (assert-type
   (lambda (n)
     (declare (fixnum n))
     (let ((x 1))
       (dotimes (i n x)
         (setq x (- x)))))
   (or (integer -1 -1) (integer 1 1))))

(with-test (:name (:setq-derive-type :value-of-each-set))
  (assert-type
   (lambda (y z)
     (let ((x nil))
       (if (and (typep y 'fixnum) (typep z 'fixnum))
           (values (setq x y) (setq x z) x)
           (values 0 0 0))))
   (values fixnum fixnum fixnum &optional)))

(macrolet ((def (name init step type)
             `(with-test (:name (:setq-derive-type :narrowed-later ,name))
                (assert-type
                 (lambda (notes)
                   (let ((x ,init))
                     (dolist (n notes x)
                       (let ((old (car n))
                             (new (cdr n)))
                         (declare ((unsigned-byte 8) old new))
                         (when (> new old)
                           (error "~S grew." n))
                         (let ((d (- old new)))
                           (setq x ,step))))))
                 ,type))))
  (def :+ 0 (+ x d) unsigned-byte)
  (def :- 0 (- x d) (integer * 0))
  (def :if 0 (if (evenp d) (+ x d) x) unsigned-byte)
  (def :nested 0 (+ (* 2 x) d) unsigned-byte)
  (def :1+ 0 (1+ (+ x d)) unsigned-byte)
  (def :* 1 (* x (1+ d)) (integer 1))
  (def :logior 0 (logior x d) (unsigned-byte 8))
  (def :ash 1 (ash x d) (integer 1)))

(with-test (:name (:recursive-local-function-result-type :merged-tail-set :lp2169497))
  (checked-compile-and-assert
   (:allow-style-warnings t)
   `(lambda (f)
      (labels ((a (&rest r)
                 (let ((x -33))
                   x))
               (b ()
                 (a (a))))
        (funcall f (apply #'a (b) nil))
        (+ (let ((min (let ((arg 5))
                        (let ((max (a)))
                          (if (< max arg)
                              max
                              arg)))))
             min)
           (if t 8 0))))
   ((#'identity) -25)))

(with-test (:name (:recursive-local-function-result-type :merged-into-bounded-tail-set :lp2169657))
  (let ((fun (checked-compile
              `(lambda (a f)
                 (declare (optimize (speed 0)))
                 (flet ((g (d)
                          (the double-float
                               (if d
                                   (let ((e 864.0d0))
                                     e)
                                   (labels ((i (n x)
                                              (declare (ignore x))
                                              (if *
                                                  (let ((r (j (1- n))))
                                                    (x)
                                                    r)
                                                  (labels ((k (p)
                                                             (l p 986567.0d0)
                                                             -973.0d0)
                                                           (l (u w)
                                                             (declare (ignore u w))
                                                             d))
                                                    (k 0))))
                                            (j (m)
                                              (declare ((integer 0 5) m))
                                              (if (plusp m)
                                                  (let ((r (i (1- m) 1)))
                                                    r)
                                                  651222.0d0)))
                                     (i 2 *))))))
                   (funcall f (g a))))
              :allow-style-warnings t)))
    (assert (= (funcall fun 255 #'identity) 864.0d0))))

(with-test (:name (:recursive-local-function-result-type :parameter-from-own-result :oscillating))
  (assert-type
   (lambda ()
     (flet ((f (n)
              (- 1 n)))
       (f (f (f 5)))))
   (or (integer -4 -4) (integer 5 5))))

(with-test (:name :the-lambda)
  (checked-compile
   `(lambda ()
      (declare (optimize speed))
      (the (function (fixnum))
           (lambda (n) (> n 1))))
   :allow-notes nil)
  (assert (nth-value 2
                     (checked-compile
                      `(lambda ()
                         (the (function * (values integer &optional))
                              (lambda (n) (+ n 1.0))))
                      :allow-warnings t)))
  (assert (nth-value 2
                     (checked-compile
                      `(lambda ()
                         (the (function * (values integer &optional))
                              (lambda (&optional (n 0)) (+ n 1.0))))
                      :allow-warnings t)))
  (assert (nth-value 2
                     (checked-compile
                      `(lambda ()
                         (the (function * (values integer &optional))
                              (lambda (&key (n 0)) (+ n 1.0))))
                      :allow-warnings t)))
  (assert (nth-value 2
                     (checked-compile
                      `(lambda ()
                         (let ((n (lambda (&optional (n 0)) (+ n 1.0))))
                           (values (the (function * (values integer &optional)) n)
                                   n)))
                      :allow-warnings t)))
  (checked-compile `(lambda () (the (function * list) (lambda (&rest r) r)))))

(with-test (:name :lvar-ref-recursion)
  (checked-compile `(lambda ()
                      (declare (optimize debug))
                      (labels ((m (z vars)
                                 (m z vars)))
                        (m 1 (error "x")))))
  (checked-compile `(lambda ()
                      (labels ((f (a)
                                 (if (plusp a)
                                     (f a)
                                     (catch 'a))))
                        (+ (f) (f *))))
                   :allow-warnings t)
  (checked-compile `(lambda (a)
                      (labels ((f (a b)
                                 (if (plusp a)
                                     (f (1- a) b)
                                     a)))
                        (+ (lognot a)
                           (f 1
                              (if (>= 5 a)
                                  8
                                  (mod a 8))))))))

(with-test (:name :mv-call-lambda-type)
  (assert-type
   (lambda (f)
     (multiple-value-call
         (lambda (&rest r) (1+ (car r)))
       (funcall f)))
   number))

(with-test (:name :local-call-coerce-tns)
  `(lambda ()
     (declare (optimize speed (safety 0)))
     (labels ((f0 ()
                (ldb (byte 32 0)
                     (let ((l (funcall *)))
                       (do () ((> * l) *)
                         (return *)))))
              (f1 (a)
                (funcall * (+ a (f0))))
              (f2 (a)
                (cond (*
                       (ldb (byte 11 1) a))
                      ((f1 a)
                       a)
                      (t (f0)))
                ))
       (values (f1 1)
               (f2 (f0))
               (f2 2))))
  (checked-compile
   `(lambda (n m)
      (declare (optimize (speed 0) (debug 2)))
      (labels ((f1 (a)
                 a
                 32)
               (f2 (a)
                 (block nil
                   (return
                     ((lambda (p)
                        (values
                         (ceiling p
                                  (let ((d a))
                                    (if (<= -1 d 5)
                                        d)))))
                      (let ((o (ash 1 (logand a 7))))
                        (loop while (funcall n (- a))
                              do (setq o (ldb (byte 32 0) (f1 a))))
                        o)))
                   (f1 1))))
        (or * * 4)
        (values (f2 (f2 (- (ldb (byte 32 0) m) 2))))))))

(with-test (:name :let-lvar-dest-single-use)
  (checked-compile-and-assert
      ()
      `(lambda (n)
         (declare (fixnum n))
         (flet ((f (a)
                  (eql a 0)))
           (values (f (rem n 3))
                   (f n))))
    ((0) (values t t))
    ((1) (values nil nil))
    ((3) (values t nil))))

(with-test (:name :bypass-if)
  (checked-compile-and-assert
      ()
      `(lambda (x n j)
         (let ((n (car (if x n nil))))
           (incf j)
           (if n
               1
               j)))
    ((1 nil 3) 4)
    ((1 '(a) 3) 1)
    ((nil '(a) 3) 4)))

(with-test (:name :ir1-optimize-mv-call-dead)
  (checked-compile
   `(lambda (n)
      (labels ((f1 (&key)
                 1)
               (f2 ()
                 (apply #'f1 nil)))
        (declare (inline f2))
        (+ (apply #'f2 n)
           (apply #'f2 nil)))))
  (checked-compile
   `(lambda ()
      (labels ((f0 (a))
               (f1 (b c))
               (f3 ()
                 (f1 (f1 (f0 127) 2))))
        (declare
         (inline f0)
         (inline f3))
        (+ #'f3 (apply #'f3 nil))))
   :allow-warnings t
   :allow-style-warnings t))

(with-test (:name :number-stack-unread-arg)
  (checked-compile-and-assert
      ()
      `(lambda (i n)
         (declare ((signed-byte 64) i))
         (labels ((f (a b)
                    (if (plusp a)
                        (f (1- a) b)
                        (catch 'a))))
           (values
            (if (< i -17592186044417)
                i
                1)
            (f n i))))
    ((5 3) (values 1 nil)))
  (checked-compile
   `(lambda ()
      (labels ((f (a)
                 (if (f 1.0d0)
                     (f a)
                     1)))
        (if (f 4.0d0)
            0)))))

(with-test (:name :default-values-constantp)
  (checked-compile
   `(lambda ()
      (flet ((f (b &key c (d (if nil b)))
               (values c d)))
        (f 1 :c 4))
      (flet ((f (b &key c (d (if 1 2 b)))
               (values c d)))
        (f 1 :c 4))
      (flet ((f (&key c (d (if nil c)))
               d))
        (f :c 4)))))

(with-test (:name :propagate-to-args-deleted-args)
  (checked-compile
   `(lambda ()
      (labels ((f (a b &key c (d (if nil b)))
                 (declare (ignore c a))
                 (f 1 2 :c (f 3 d :c 3))))
        (f 1 2 :c 4)))))

(with-test (:name :accumulate-optimistic-arg-types-delete-fun)
 (checked-compile
  `(lambda ()
     (declare (optimize speed))
     (labels ((f1 ()
                (if nil
                    (labels ((g12 (n14 x16)
                               (print (g13 2 (<= n14 x16))))
                             (g13 (n15 x17)
                               (let ((r19 (g12 1 2)))
                                 (error "~a" r19))))
                      (g12 0 1)))))
       (f1)))))

(with-test (:name :nlx-restore-nsp)
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (-
          (block nil
            (unwind-protect 9
              (return (- (ldb (byte 63 0) a) 4611686018427387905))))
          (- (ldb (byte 63 0) b) 4611686018427387904)))
    ((4611686018427387903 0) 4611686018427387902)))

(with-test (:name :erase-lvar-type-lambda-var-types)
  (checked-compile-and-assert
   ()
   `(lambda (l)
      (labels ((f (l)
                 (loop for e in (cdr l)
                       for i14 below 8
                       sum -12)))
        (f (cons 0 l))))
   ((nil) 0)
   (('(1)) -12))
  (checked-compile-and-assert
      ()
      `(lambda (i)
         (flet ((f1 (a &rest r)
                  (declare (integer a)
                           (ignore r))
                  (if (plusp a)
                      (progn
                        (check-type a integer)
                        -9))))
           (f1
            (mod
             (multiple-value-call #'min 0 (lognot i))
             6))))
    ((4) -9)))

(with-test (:name :same-ref-recompute-without-sets)
  (assert (nth-value 2
                     (checked-compile
                      `(lambda (a)
                         (declare ((integer -3 3) a))
                         (+
                          (let (b)
                            (if (eq a a)
                                (setq b 1d0)
                                (setq b a))
                            (the integer b))
                          (progn (check-type a integer) 0)))
                      :allow-warnings t))))

(with-test (:name :optional-type-check-supplied-p)
  (checked-compile-and-assert
      (:optimize :default)
      `(lambda (&optional (a 1 p) (b 2) (c 3))
         (declare (type (integer 0 5) b))
         (values a b c p))
    ((1 2 9) (values 1 2 9 t))
    ((1 8 3) (condition 'type-error))))

(with-test (:name :check-type-faster-type-derivation)
  (assert-type
   (lambda (i)
     (declare (optimize (speed 0)))
     (check-type i cons)
     i)
   cons))

(with-test (:name :ppc64-environment-conflicts)
  (checked-compile-and-assert
      ()
      `(lambda (i f)
         (declare (fixnum i)
                  (single-float f))
         (+ (catch t (throw t 4))
            (+ f i)))
    ((1 1.0) 6.0)))

(with-test (:name :reuse-move-coercion-loop)
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (labels ((f0 (q)
                    (let ((m -170141183460469231731687303715884105600))
                      (loop for i from 0 below 7
                            do (setq m
                                     (let ((w
                                             (if (>= m i)
                                                 q
                                                 m)))
                                       (- (ldb (byte 128 0) (+ (ldb (byte 64 0) w)
                                                               170141183460469231731687303715884105728))
                                          170141183460469231731687303715884105728))))
                      m)))
           (declare (ftype (function ((signed-byte 128))) f0))
           (f0 (logeqv a b))))
    ((-170141183460469231731687303715884105728 170141183460469231731687303715884105727) 0)
    ((1 2) 18446744073709551612)))
