;;;; tests that dynamic-extent functionality works.

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

(when (eq sb-ext:*evaluator-mode* :interpret)
  (sb-ext:quit :unix-status 104))

(load "compiler-test-util.lisp")
(use-package :ctu)

(setq sb-c::*check-consistency* t
      sb-ext:*stack-allocate-dynamic-extent* t)

(defmacro defun-with-dx (name arglist &body body)
  `(defun ,name ,arglist
     ,@body))

(declaim (notinline opaque-identity))
(defun opaque-identity (x)
  x)

;;; &REST lists

(defun-with-dx dxlength (&rest rest)
  (declare (dynamic-extent rest))
  (length rest))

(with-test (:name (:dx-&rest :basics))
  (assert (= (dxlength 1 2 3) 3))
  (assert (= (dxlength t t t t t t) 6))
  (assert (= (dxlength) 0)))

(defun callee (list)
  (destructuring-bind (a b c d e f &rest g) list
    (+ a b c d e f (length g))))

(defun-with-dx dxcaller (&rest rest)
  (declare (dynamic-extent rest))
  (callee rest))

(with-test (:name (:dx-&rest :pass-down-to-callee :tail-call))
  (assert (= (dxcaller 1 2 3 4 5 6 7) 22)))

(defun-with-dx dxcaller-align-1 (x &rest rest)
  (declare (dynamic-extent rest))
  (+ x (callee rest)))

(with-test (:name (:dx-&rest :pass-down-to-callee :non-tail-call))
  (assert (= (dxcaller-align-1 17 1 2 3 4 5 6 7) 39))
  (assert (= (dxcaller-align-1 17 1 2 3 4 5 6 7 8) 40)))

;;; %NIP-VALUES

(defun-with-dx test-nip-values ()
  (flet ((bar (x &rest y)
           (declare (dynamic-extent y))
           (if (> x 0)
               (values x (length y))
               (values (car y)))))
    (multiple-value-call #'values
      (bar 1 2 3 4 5 6)
      (bar -1 'a 'b))))

(with-test (:name (:nip-values))
  (assert (equal (multiple-value-list (test-nip-values)) '(1 5 a))))

;;; LET-variable substitution

(defun-with-dx test-let-var-subst1 (x)
  (let ((y (list x (1- x))))
    (opaque-identity :foo)
    (let ((z (the list y)))
      (declare (dynamic-extent z))
      (length z))))

(with-test (:name (:let-variable-substitution))
  (assert (eql (test-let-var-subst1 17) 2)))

(defun-with-dx test-let-var-subst2 (x)
  (let ((y (list x (1- x))))
    (declare (dynamic-extent y))
    (opaque-identity :foo)
    (let ((z (the list y)))
      (length z))))

(with-test (:name (:let-variable-substitution-2))
  (assert (eql (test-let-var-subst2 17) 2)))


;;; DX propagation through LET-return.

(defun-with-dx test-lvar-subst (x)
  (let ((y (list x (1- x))))
    (declare (dynamic-extent y))
    (second (let ((z (the list y)))
              (opaque-identity :foo)
              z))))

(with-test (:name (:dx-propagation-through-let-return))
  (assert (eql (test-lvar-subst 11) 10)))

;;; this code is incorrect, but the compiler should not fail
(defun-with-dx test-let-var-subst-incorrect (x)
  (let ((y (list x (1- x))))
    (opaque-identity :foo)
    (let ((z (the list y)))
      (declare (dynamic-extent z))
      (opaque-identity :bar)
      z)))

;;; alignment

(defvar *x*)
(defun-with-dx test-alignment-dx-list (form)
  (multiple-value-prog1 (eval form)
    (let ((l (list 1 2 3 4)))
      (declare (dynamic-extent l))
      (setq *x* (copy-list l)))))

(with-test (:name (:dx-list :alignment))
  (dotimes (n 64)
    (let* ((res (loop for i below n collect i))
           (form `(values ,@res)))
      (assert (equal (multiple-value-list (test-alignment-dx-list form)) res))
      (assert (equal *x* '(1 2 3 4))))))

;;; closure

(declaim (notinline true))
(defun true (x)
  (declare (ignore x))
  t)

(defun-with-dx dxclosure (x)
  (flet ((f (y)
           (+ y x)))
    (declare (dynamic-extent #'f))
    (true #'f)))

(with-test (:name (:dx-closure))
  (assert (eq t (dxclosure 13))))

;;; value-cells

(defun-with-dx dx-value-cell (x)
  ;; Not implemented everywhere, yet.
  #+(or x86 x86-64 mips hppa)
  (let ((cell x))
    (declare (sb-int:truly-dynamic-extent cell))
    (flet ((f ()
             (incf cell)))
      (declare (dynamic-extent #'f))
      (true #'f))))

;;; CONS

(defun-with-dx cons-on-stack (x)
  (let ((cons (cons x x)))
    (declare (dynamic-extent cons))
    (true cons)
    nil))

;;; MAKE-ARRAY

(defun force-make-array-on-stack (n)
  (declare (optimize safety))
  (let ((v (make-array (min n 1))))
    (declare (sb-int:truly-dynamic-extent v))
    (true v)
    nil))

(defun-with-dx make-array-on-stack-1 ()
  (let ((v (make-array '(42) :element-type 'single-float)))
    (declare (dynamic-extent v))
    (true v)
    nil))

(defun-with-dx make-array-on-stack-2 (n x)
  (declare (integer n))
  (let ((v (make-array n :initial-contents x)))
    (declare (sb-int:truly-dynamic-extent v))
    (true v)
    nil))

(defun-with-dx make-array-on-stack-3 (x y z)
  (let ((v (make-array 3
                       :element-type 'fixnum :initial-contents (list x y z)
                       :element-type t :initial-contents x)))
    (declare (sb-int:truly-dynamic-extent v))
    (true v)
    nil))

(defun-with-dx make-array-on-stack-4 ()
  (let ((v (make-array 3 :initial-contents '(1 2 3))))
    (declare (sb-int:truly-dynamic-extent v))
    (true v)
    nil))

(defun-with-dx make-array-on-stack-5 ()
  (let ((v (make-array 3 :initial-element 12 :element-type t)))
    (declare (sb-int:truly-dynamic-extent v))
    (true v)
    nil))

(defun-with-dx vector-on-stack (x y)
  (let ((v (vector 1 x 2 y 3)))
    (declare (sb-int:truly-dynamic-extent v))
    (true v)
    nil))

;;; MAKE-STRUCTURE

(declaim (inline make-fp-struct-1))
(defstruct fp-struct-1
  (s 0.0 :type single-float)
  (d 0.0d0 :type double-float))

(defun-with-dx test-fp-struct-1.1 (s d)
  (let ((fp (make-fp-struct-1 :s s)))
    (declare (dynamic-extent fp))
    (assert (eql s (fp-struct-1-s fp)))
    (assert (eql 0.0d0 (fp-struct-1-d fp)))))

(defun-with-dx test-fp-struct-1.2 (s d)
  (let ((fp (make-fp-struct-1 :d d)))
    (declare (dynamic-extent fp))
    (assert (eql 0.0 (fp-struct-1-s fp)))
    (assert (eql d (fp-struct-1-d fp)))))

(defun-with-dx test-fp-struct-1.3 (s d)
  (let ((fp (make-fp-struct-1 :d d :s s)))
    (declare (dynamic-extent fp))
    (assert (eql s (fp-struct-1-s fp)))
    (assert (eql d (fp-struct-1-d fp)))))

(defun-with-dx test-fp-struct-1.4 (s d)
  (let ((fp (make-fp-struct-1 :s s :d d)))
    (declare (dynamic-extent fp))
    (assert (eql s (fp-struct-1-s fp)))
    (assert (eql d (fp-struct-1-d fp)))))

(with-test (:name (:test-fp-struct-1.1))
  (test-fp-struct-1.1 123.456 876.243d0))
(with-test (:name (:test-fp-struct-1.2))
  (test-fp-struct-1.2 123.456 876.243d0))
(with-test (:name (:test-fp-struct-1.3))
  (test-fp-struct-1.3 123.456 876.243d0))
(with-test (:name (:test-fp-struct-1.4))
  (test-fp-struct-1.4 123.456 876.243d0))

(declaim (inline make-fp-struct-2))
(defstruct fp-struct-2
  (d 0.0d0 :type double-float)
  (s 0.0 :type single-float))

(defun-with-dx test-fp-struct-2.1 (s d)
  (let ((fp (make-fp-struct-2 :s s)))
    (declare (dynamic-extent fp))
    (assert (eql s (fp-struct-2-s fp)))
    (assert (eql 0.0d0 (fp-struct-2-d fp)))))

(defun-with-dx test-fp-struct-2.2 (s d)
  (let ((fp (make-fp-struct-2 :d d)))
    (declare (dynamic-extent fp))
    (assert (eql 0.0 (fp-struct-2-s fp)))
    (assert (eql d (fp-struct-2-d fp)))))

(defun-with-dx test-fp-struct-2.3 (s d)
  (let ((fp (make-fp-struct-2 :d d :s s)))
    (declare (dynamic-extent fp))
    (assert (eql s (fp-struct-2-s fp)))
    (assert (eql d (fp-struct-2-d fp)))))

(defun-with-dx test-fp-struct-2.4 (s d)
  (let ((fp (make-fp-struct-2 :s s :d d)))
    (declare (dynamic-extent fp))
    (assert (eql s (fp-struct-2-s fp)))
    (assert (eql d (fp-struct-2-d fp)))))

(with-test (:name (:test-fp-struct-2.1))
  (test-fp-struct-2.1 123.456 876.243d0))
(with-test (:name (:test-fp-struct-2.2))
  (test-fp-struct-2.2 123.456 876.243d0))
(with-test (:name (:test-fp-struct-2.3))
  (test-fp-struct-2.3 123.456 876.243d0))
(with-test (:name (:test-fp-struct-2.4))
  (test-fp-struct-2.4 123.456 876.243d0))

(declaim (inline make-cfp-struct-1))
(defstruct cfp-struct-1
  (s (complex 0.0) :type (complex single-float))
  (d (complex 0.0d0) :type (complex double-float)))

(defun-with-dx test-cfp-struct-1.1 (s d)
  (let ((cfp (make-cfp-struct-1 :s s)))
    (declare (dynamic-extent cfp))
    (assert (eql s (cfp-struct-1-s cfp)))
    (assert (eql (complex 0.0d0) (cfp-struct-1-d cfp)))))

(defun-with-dx test-cfp-struct-1.2 (s d)
  (let ((cfp (make-cfp-struct-1 :d d)))
    (declare (dynamic-extent cfp))
    (assert (eql (complex 0.0) (cfp-struct-1-s cfp)))
    (assert (eql d (cfp-struct-1-d cfp)))))

(defun-with-dx test-cfp-struct-1.3 (s d)
  (let ((cfp (make-cfp-struct-1 :d d :s s)))
    (declare (dynamic-extent cfp))
    (assert (eql s (cfp-struct-1-s cfp)))
    (assert (eql d (cfp-struct-1-d cfp)))))

(defun-with-dx test-cfp-struct-1.4 (s d)
  (let ((cfp (make-cfp-struct-1 :s s :d d)))
    (declare (dynamic-extent cfp))
    (assert (eql s (cfp-struct-1-s cfp)))
    (assert (eql d (cfp-struct-1-d cfp)))))

(with-test (:name (:test-cfp-struct-1.1))
  (test-cfp-struct-1.1 (complex 0.123 123.456) (complex 908132.41d0 876.243d0)))
(with-test (:name (:test-cfp-struct-1.2))
  (test-cfp-struct-1.2 (complex 0.123 123.456) (complex 908132.41d0 876.243d0)))
(with-test (:name (:test-cfp-struct-1.3))
  (test-cfp-struct-1.3 (complex 0.123 123.456) (complex 908132.41d0 876.243d0)))
(with-test (:name (:test-cfp-struct-1.4))
  (test-cfp-struct-1.4 (complex 0.123 123.456) (complex 908132.41d0 876.243d0)))

(declaim (inline make-cfp-struct-2))
(defstruct cfp-struct-2
  (d (complex 0.0d0) :type (complex double-float))
  (s (complex 0.0) :type (complex single-float)))

(defun-with-dx test-cfp-struct-2.1 (s d)
  (let ((cfp (make-cfp-struct-2 :s s)))
    (declare (dynamic-extent cfp))
    (assert (eql s (cfp-struct-2-s cfp)))
    (assert (eql (complex 0.0d0) (cfp-struct-2-d cfp)))))

(defun-with-dx test-cfp-struct-2.2 (s d)
  (let ((cfp (make-cfp-struct-2 :d d)))
    (declare (dynamic-extent cfp))
    (assert (eql (complex 0.0) (cfp-struct-2-s cfp)))
    (assert (eql d (cfp-struct-2-d cfp)))))

(defun-with-dx test-cfp-struct-2.3 (s d)
  (let ((cfp (make-cfp-struct-2 :d d :s s)))
    (declare (dynamic-extent cfp))
    (assert (eql s (cfp-struct-2-s cfp)))
    (assert (eql d (cfp-struct-2-d cfp)))))

(defun-with-dx test-cfp-struct-2.4 (s d)
  (let ((cfp (make-cfp-struct-2 :s s :d d)))
    (declare (dynamic-extent cfp))
    (assert (eql s (cfp-struct-2-s cfp)))
    (assert (eql d (cfp-struct-2-d cfp)))))

(with-test (:name (:test-cfp-struct-2.1))
  (test-cfp-struct-2.1 (complex 0.123 123.456) (complex 908132.41d0 876.243d0)))
(with-test (:name (:test-cfp-struct-2.2))
  (test-cfp-struct-2.2 (complex 0.123 123.456) (complex 908132.41d0 876.243d0)))
(with-test (:name (:test-cfp-struct-2.3))
  (test-cfp-struct-2.3 (complex 0.123 123.456) (complex 908132.41d0 876.243d0)))
(with-test (:name (:test-cfp-struct-2.4))
  (test-cfp-struct-2.4 (complex 0.123 123.456) (complex 908132.41d0 876.243d0)))

(declaim (inline make-foo1 make-foo2 make-foo3))
(defstruct foo1 x)

(defun-with-dx make-foo1-on-stack (x)
  (let ((foo (make-foo1 :x x)))
    (declare (dynamic-extent foo))
    (assert (eql x (foo1-x foo)))))

(defstruct foo2
  (x 0.0 :type single-float)
  (y 0.0d0 :type double-float)
  a
  b
  c)

(defun-with-dx make-foo2-on-stack (x y)
  (let ((foo (make-foo2 :y y :c 'c)))
    (declare (dynamic-extent foo))
    (assert (eql 0.0 (foo2-x foo)))
    (assert (eql y (foo2-y foo)))
    (assert (eql 'c (foo2-c foo)))
    (assert (eql nil (foo2-b foo)))))

;;; Check that constants work out as argument for all relevant
;;; slot types.
(defstruct foo3
  (a 0 :type t)
  (b 1 :type fixnum)
  (c 2 :type sb-vm:word)
  (d 3.0 :type single-float)
  (e 4.0d0 :type double-float))

(defun-with-dx make-foo3-on-stack ()
  (let ((foo (make-foo3)))
    (declare (dynamic-extent foo))
    (assert (eql 0 (foo3-a foo)))
    (assert (eql 1 (foo3-b foo)))
    (assert (eql 2 (foo3-c foo)))
    (assert (eql 3.0 (foo3-d foo)))
    (assert (eql 4.0d0 (foo3-e foo)))))

;;; Nested DX

(defun-with-dx nested-dx-lists ()
  (let ((dx (list (list 1 2) (list 3 4))))
    (declare (dynamic-extent dx))
    (true dx)
    nil))

(defun-with-dx nested-dx-conses ()
  (let ((dx (cons 1 (cons 2 (cons 3 (cons (cons t t) nil))))))
    (declare (dynamic-extent dx))
    (true dx)
    nil))

(defun-with-dx nested-dx-not-used (x)
  (declare (list x))
  (let ((l (setf (car x) (list x x x))))
    (declare (dynamic-extent l))
    (true l)
    (true (length l))
    nil))

(defun-with-dx nested-evil-dx-used (x)
  (declare (list x))
  (let ((l (list x x x)))
    (declare (dynamic-extent l))
    (unwind-protect
         (progn
           (setf (car x) l)
           (true l))
      (setf (car x) nil))
    nil))

(defparameter *bar* nil)
(declaim (inline make-nested-bad make-nested-good))
(defstruct (nested (:constructor make-nested-bad (&key bar &aux (bar (setf *bar* bar))))
                   (:constructor make-nested-good (&key bar)))
  bar)

(defun-with-dx nested-good (y)
  (let ((x (list (list (make-nested-good :bar (list (list (make-nested-good :bar (list y)))))))))
    (declare (dynamic-extent x))
    (true x)))

(defun-with-dx nested-bad (y)
  (let ((x (list (list (make-nested-bad :bar (list (list (make-nested-bad :bar (list y)))))))))
    (declare (dynamic-extent x))
    (unless (equalp (caar x) (make-nested-good :bar *bar*))
      (error "got ~S, wanted ~S" (caar x) (make-nested-good :bar *bar*)))
    (caar x)))

(with-test (:name :conservative-nested-dx)
  ;; NESTED-BAD should not stack-allocate :BAR due to the SETF.
  (assert (equalp (nested-bad 42) (make-nested-good :bar *bar*)))
  (assert (equalp *bar* (list (list (make-nested-bad :bar (list 42)))))))

;;; multiple uses for dx lvar

(defun-with-dx multiple-dx-uses ()
  (let ((dx (if (true t)
                (list 1 2 3)
                (list 2 3 4))))
    (declare (dynamic-extent dx))
    (true dx)
    nil))

;;; handler-case and handler-bind should use DX internally

(defun dx-handler-bind (x)
  (handler-bind ((error
                  #'(lambda (c)
                      (break "OOPS: ~S caused ~S" x c)))
                 ((and serious-condition (not error))
                  #'(lambda (c)
                      (break "OOPS2: ~S did ~S" x c))))
    (/ 2 x)))

(defun dx-handler-case (x)
  (assert (zerop (handler-case (/ 2 x)
                   (error (c)
                     (break "OOPS: ~S caused ~S" x c)
                     -1)
                   (:no-error (res)
                     (1- res))))))

(defvar *a-cons* (cons nil nil))

#+stack-allocatable-closures
(with-test (:name (:no-consing :dx-closures))
  (assert-no-consing (dxclosure 42)))

#+stack-allocatable-lists
(with-test (:name (:no-consing :dx-lists))
  (assert-no-consing (dxlength 1 2 3))
  (assert-no-consing (dxlength t t t t t t))
  (assert-no-consing (dxlength))
  (assert-no-consing (dxcaller 1 2 3 4 5 6 7))
  (assert-no-consing (test-nip-values))
  (assert-no-consing (test-let-var-subst2 17))
  (assert-no-consing (test-lvar-subst 11))
  (assert-no-consing (nested-dx-lists))
  (assert-consing (nested-dx-not-used *a-cons*))
  (assert-no-consing (nested-evil-dx-used *a-cons*))
  (assert-no-consing (multiple-dx-uses)))

(with-test (:name (:no-consing :dx-value-cell))
  (assert-no-consing (dx-value-cell 13)))

#+stack-allocatable-fixed-objects
(with-test (:name (:no-consing :dx-fixed-objects))
  (assert-no-consing (cons-on-stack 42))
  (assert-no-consing (make-foo1-on-stack 123))
  (assert-no-consing (nested-good 42))
  (assert-no-consing (nested-dx-conses))
  (assert-no-consing (dx-handler-bind 2))
  (assert-no-consing (dx-handler-case 2)))

#+stack-allocatable-vectors
(with-test (:name (:no-consing :dx-vectors))
  (assert-no-consing (force-make-array-on-stack 128))
  (assert-no-consing (make-array-on-stack-1))
  (assert-no-consing (make-array-on-stack-2 5 '(1 2.0 3 4.0 5)))
  (assert-no-consing (make-array-on-stack-3 9 8 7))
  (assert-no-consing (make-array-on-stack-4))
  (assert-no-consing (make-array-on-stack-5))
  (assert-no-consing (vector-on-stack :x :y)))

#+raw-instance-init-vops
(with-test (:name (:no-consing :dx-raw-instances) :fails-on :ppc)
  (let (a b)
    (setf a 1.24 b 1.23d0)
    (assert-no-consing (make-foo2-on-stack a b)))
    (assert-no-consing (make-foo3-on-stack)))

;;; not really DX, but GETHASH and (SETF GETHASH) should not cons

(defvar *table* (make-hash-table))

(defun test-hash-table ()
  (setf (gethash 5 *table*) 13)
  (gethash 5 *table*))

;; This fails on threaded PPC because the hash-table implementation
;; uses recursive system spinlocks, which cons (see below for test
;; (:no-consing :spinlock), which also fails on threaded PPC).
(with-test (:name (:no-consing :hash-tables) :fails-on '(and :ppc :sb-thread))
  (assert-no-consing (test-hash-table)))

;;; with-spinlock and with-mutex should use DX and not cons

(defvar *slock* (sb-thread::make-spinlock :name "slocklock"))

(defun test-spinlock ()
  (sb-thread::with-spinlock (*slock*)
    (true *slock*)))

(defvar *mutex* (sb-thread::make-mutex :name "mutexlock"))

(defun test-mutex ()
  (sb-thread:with-mutex (*mutex*)
    (true *mutex*)))

#+sb-thread
(with-test (:name (:no-consing :mutex) :fails-on :ppc)
  (assert-no-consing (test-mutex)))

#+sb-thread
(with-test (:name (:no-consing :spinlock) :fails-on :ppc)
  (assert-no-consing (test-spinlock)))



;;; Bugs found by Paul F. Dietz

(with-test (:name (:dx-bug-misc :pfdietz))
  (assert
   (eq
    (funcall
     (compile
      nil
      '(lambda (a b)
        (declare (optimize (speed 2) (space 0) (safety 0)
                  (debug 1) (compilation-speed 3)))
        (let* ((v5 (cons b b)))
          (declare (dynamic-extent v5))
          a)))
     'x 'y)
    'x)))

;;; bug reported by Svein Ove Aas
(defun svein-2005-ii-07 (x y)
  (declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (let ((args (list* y 1 2 x)))
    (declare (dynamic-extent args))
    (apply #'aref args)))

(with-test (:name (:dx-bugs-misc :svein-2005-ii-07))
  (assert (eql
           (svein-2005-ii-07
            '(0)
            #3A(((1 1 1) (1 1 1) (1 1 1))
                ((1 1 1) (1 1 1) (4 1 1))
                ((1 1 1) (1 1 1) (1 1 1))))
           4)))

;;; bug reported by Brian Downing: stack-allocated arrays were not
;;; filled with zeroes.
(defun-with-dx bdowning-2005-iv-16 ()
  (let ((a (make-array 11 :initial-element 0)))
    (declare (dynamic-extent a))
    (assert (every (lambda (x) (eql x 0)) a))))

(with-test (:name (:dx-bug-misc :bdowning-2005-iv-16))
  #+(or hppa mips x86 x86-64)
  (assert-no-consing (bdowning-2005-iv-16))
  (bdowning-2005-iv-16))

(declaim (inline my-nconc))
(defun-with-dx my-nconc (&rest lists)
  (declare (dynamic-extent lists))
  (apply #'nconc lists))
(defun-with-dx my-nconc-caller (a b c)
  (let ((l1 (list a b c))
        (l2 (list a b c)))
    (my-nconc l1 l2)))
(with-test (:name :rest-stops-the-buck)
  (let ((list1 (my-nconc-caller 1 2 3))
        (list2 (my-nconc-caller 9 8 7)))
    (assert (equal list1 '(1 2 3 1 2 3)))
    (assert (equal list2 '(9 8 7 9 8 7)))))

(defun-with-dx let-converted-vars-dx-allocated-bug (x y z)
  (let* ((a (list x y z))
         (b (list x y z))
         (c (list a b)))
    (declare (dynamic-extent c))
    (values (first c) (second c))))
(with-test (:name :let-converted-vars-dx-allocated-bug)
  (multiple-value-bind (i j) (let-converted-vars-dx-allocated-bug 1 2 3)
    (assert (and (equal i j)
                 (equal i (list 1 2 3))))))

;;; workaround for bug 419 -- real issue remains, but check that the
;;; bandaid holds.
(defun-with-dx bug419 (x)
  (multiple-value-call #'list
    (eval '(values 1 2 3))
    (let ((x x))
      (declare (dynamic-extent x))
      (flet ((mget (y)
               (+ x y))
             (mset (z)
               (incf x z)))
        (declare (dynamic-extent #'mget #'mset))
        ((lambda (f g) (eval `(progn ,f ,g (values 4 5 6)))) #'mget #'mset)))))

(with-test (:name (:dx-bug-misc :bug419))
  (assert (equal (bug419 42) '(1 2 3 4 5 6))))

;;; Multiple DX arguments in a local function call
(defun test-dx-flet-test (fun n f1 f2 f3)
  (let ((res (with-output-to-string (s)
               (assert (eql n (ignore-errors (funcall fun s)))))))
    (multiple-value-bind (x pos) (read-from-string res nil)
      (assert (equalp f1 x))
      (multiple-value-bind (y pos2) (read-from-string res nil nil :start pos)
        (assert (equalp f2 y))
        (assert (equalp f3 (read-from-string res nil nil :start pos2))))))
  #+(or hppa mips x86 x86-64)
  (assert-no-consing (assert (eql n (funcall fun nil))))
  (assert (eql n (funcall fun nil))))

(macrolet ((def (n f1 f2 f3)
             (let ((name (sb-pcl::format-symbol :cl-user "DX-FLET-TEST.~A" n)))
               `(progn
                  (defun-with-dx ,name (s)
                    (flet ((f (x)
                             (declare (dynamic-extent x))
                             (when s
                               (print x s)
                               (finish-output s))
                             nil))
                      (f ,f1)
                      (f ,f2)
                      (f ,f3)
                      ,n))
                  (with-test (:name (:dx-flet-test ,n))
                    (test-dx-flet-test #',name ,n ,f1 ,f2 ,f3))))))
  (def 0 (list :one) (list :two) (list :three))
  (def 1 (make-array 128) (list 1 2 3 4 5 6 7 8) (list 'list))
  (def 2 (list 1) (list 2 3) (list 4 5 6 7)))

;;; Test that unknown-values coming after a DX value won't mess up the
;;; stack analysis
(defun test-update-uvl-live-sets (x y z)
 (declare (optimize speed (safety 0)))
 (flet ((bar (a b)
          (declare (dynamic-extent a))
          (eval `(list (length ',a) ',b))))
   (list (bar x y)
         (bar (list x y z)                  ; dx push
              (list
               (multiple-value-call 'list
                 (eval '(values 1 2 3))     ; uv push
                 (max y z)
               )                            ; uv pop
               14)
         ))))

(with-test (:name (:update-uvl-live-sets))
  (assert (equal '((0 4) (3 ((1 2 3 5) 14)))
                 (test-update-uvl-live-sets #() 4 5))))

(with-test (:name :regression-1.0.23.38)
  (compile nil '(lambda ()
                 (flet ((make (x y)
                          (let ((res (cons x x)))
                            (setf (cdr res) y)
                            res)))
                   (declaim (inline make))
                   (let ((z (make 1 2)))
                     (declare (dynamic-extent z))
                     (print z)
                     t))))
  (compile nil '(lambda ()
                 (flet ((make (x y)
                          (let ((res (cons x x)))
                            (setf (cdr res) y)
                            (if x res y))))
                   (declaim (inline make))
                   (let ((z (make 1 2)))
                     (declare (dynamic-extent z))
                     (print z)
                     t)))))

;;; On x86 and x86-64 upto 1.0.28.16 LENGTH and WORDS argument
;;; tns to ALLOCATE-VECTOR-ON-STACK could be packed in the same
;;; location, leading to all manner of badness. ...reproducing this
;;; reliably is hard, but this it at least used to break on x86-64.
(defun length-and-words-packed-in-same-tn (m)
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  (let ((array (make-array (max 1 m) :element-type 'fixnum)))
    (declare (dynamic-extent array))
    (array-total-size array)))
(with-test (:name :length-and-words-packed-in-same-tn)
  (assert (= 1 (length-and-words-packed-in-same-tn -3))))

(with-test (:name :handler-case-bogus-compiler-note :fails-on :ppc)
  (handler-bind
      ((compiler-note (lambda (note)
                        (error "compiler issued note ~S during test" note))))
    ;; Taken from SWANK, used to signal a bogus stack allocation
    ;; failure note.
    (compile nil
             `(lambda (files fasl-dir load)
                (let ((needs-recompile nil))
                  (dolist (src files)
                    (let ((dest (binary-pathname src fasl-dir)))
                      (handler-case
                          (progn
                            (when (or needs-recompile
                                      (not (probe-file dest))
                                      (file-newer-p src dest))
                              (setq needs-recompile t)
                              (ensure-directories-exist dest)
                              (compile-file src :output-file dest :print nil :verbose t))
                            (when load
                              (load dest :verbose t)))
                        (serious-condition (c)
                          (handle-loadtime-error c dest))))))))))

(declaim (inline foovector barvector))
(defun foovector (x y z)
  (let ((v (make-array 3)))
    (setf (aref v 0) x
          (aref v 1) y
          (aref v 2) z)
    v))
(defun barvector (x y z)
  (make-array 3 :initial-contents (list x y z)))
(with-test (:name :dx-compiler-notes :fails-on :ppc)
  (flet ((assert-notes (j lambda)
           (let ((n 0))
             (handler-bind ((compiler-note (lambda (c)
                                             (declare (ignore c))
                                             (incf n))))
               (compile nil lambda)
               (unless (= j n)
                 (error "Wanted ~S notes, got ~S for~%   ~S"
                        j n lambda))))))
    ;; These ones should complain.
    (assert-notes 1 `(lambda (x)
                       (let ((v (make-array x)))
                         (declare (dynamic-extent v))
                         (length v))))
    (assert-notes 2 `(lambda (x)
                       (let ((y (if (plusp x)
                                    (true x)
                                    (true (- x)))))
                         (declare (dynamic-extent y))
                         (print y)
                         nil)))
    (assert-notes 1 `(lambda (x)
                       (let ((y (foovector x x x)))
                         (declare (sb-int:truly-dynamic-extent y))
                         (print y)
                         nil)))
    ;; These ones should not complain.
    (assert-notes 0 `(lambda (name)
                       (with-alien
                           ((posix-getenv (function c-string c-string)
                                          :EXTERN "getenv"))
                         (values
                          (alien-funcall posix-getenv name)))))
    (assert-notes 0 `(lambda (x)
                       (let ((y (barvector x x x)))
                         (declare (dynamic-extent y))
                         (print y)
                         nil)))
    (assert-notes 0 `(lambda (list)
                       (declare (optimize (space 0)))
                       (sort list #'<)))
    (assert-notes 0 `(lambda (other)
                       #'(lambda (s c n)
                           (ignore-errors (funcall other s c n)))))))

;;; Stack allocating a value cell in HANDLER-CASE would blow up stack
;;; in an unfortunate loop.
(defun handler-case-eating-stack ()
  (let ((sp nil))
    (do ((n 0 (logand most-positive-fixnum (1+ n))))
        ((>= n 1024))
     (multiple-value-bind (value error) (ignore-errors)
       (when (and value error) nil))
      (if sp
          (assert (= sp (sb-c::%primitive sb-c:current-stack-pointer)))
          (setf sp (sb-c::%primitive sb-c:current-stack-pointer))))))
(with-test (:name :handler-case-eating-stack :fails-on :ppc)
  (assert-no-consing (handler-case-eating-stack)))

;;; A nasty bug where RECHECK-DYNAMIC-EXTENT-LVARS thought something was going
;;; to be stack allocated when it was not, leading to a bogus %NIP-VALUES.
;;; Fixed by making RECHECK-DYNAMIC-EXTENT-LVARS deal properly with nested DX.
(deftype vec ()
  `(simple-array single-float (3)))
(declaim (ftype (function (t t t) vec) vec))
(declaim (inline vec))
(defun vec (a b c)
  (make-array 3 :element-type 'single-float :initial-contents (list a b c)))
(defun bad-boy (vec)
  (declare (type vec vec))
  (lambda (fun)
    (let ((vec (vec (aref vec 0) (aref vec 1) (aref vec 2))))
      (declare (dynamic-extent vec))
      (funcall fun vec))))
(with-test (:name :recheck-nested-dx-bug :fails-on :ppc)
  (assert (funcall (bad-boy (vec 1.0 2.0 3.3))
                   (lambda (vec) (equalp vec (vec 1.0 2.0 3.3)))))
  (flet ((foo (x) (declare (ignore x))))
    (let ((bad-boy (bad-boy (vec 2.0 3.0 4.0))))
      (assert-no-consing (funcall bad-boy #'foo)))))

(with-test (:name :bug-497321)
  (flet ((test (lambda type)
           (let ((n 0))
             (handler-bind ((condition (lambda (c)
                                         (incf n)
                                         (unless (typep c type)
                                           (error "wanted ~S for~%  ~S~%got ~S"
                                                  type lambda (type-of c))))))
               (compile nil lambda))
             (assert (= n 1)))))
    (test `(lambda () (declare (dynamic-extent #'bar)))
          'style-warning)
    (test `(lambda () (declare (dynamic-extent bar)))
          'style-warning)
    (test `(lambda (bar) (cons bar (lambda () (declare (dynamic-extent bar)))))
          'sb-ext:compiler-note)
    (test `(lambda ()
             (flet ((bar () t))
               (cons #'bar (lambda () (declare (dynamic-extent #'bar))))))
          'sb-ext:compiler-note)))

(with-test (:name :bug-586105 :fails-on '(not (and :stack-allocatable-vectors
                                                   :stack-allocatable-lists)))
  (flet ((test (x)
           (let ((vec (make-array 1 :initial-contents (list (list x)))))
             (declare (dynamic-extent vec))
             (assert (eql x (car (aref vec 0)))))))
    (assert-no-consing (test 42))))

