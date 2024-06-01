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
  (invoke-restart 'run-tests::skip-file))

(use-package :ctu)

(setq sb-c::*check-consistency* t
      sb-ext:*stack-allocate-dynamic-extent* t)

(defun crashme (a)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (declare (muffle-conditions style-warning)) ; re F1 and F2
  (restart-case
      (progn (ignore-errors (error "Foo")) (write-char #\.))
    (retry () (f1 a))
    (use-value (new) (f2 new))))

;; lp#1530390
(with-test (:name :do-not-dxify-restarts)
  (let ((a (make-array 100))) (unwind-protect (crashme 'bork) (fill a 0)))
  (let ((a (make-array 100))) (unwind-protect (crashme 'bork) (fill a 0)))
  (let ((a (make-array 100))) (unwind-protect (crashme 'bork) (fill a 0)))
  (let ((a (make-array 100))) (unwind-protect (crashme 'bork) (fill a 0)))
  )

(defmacro defun-with-dx (name arglist &body body)
  (let ((debug-name (sb-int:symbolicate name "-HIGH-DEBUG"))
        (default-name (sb-int:symbolicate name "-DEFAULT")))
    `(progn
       (defun ,debug-name ,arglist
         (declare (optimize debug))
         ,@body)
       (defun ,default-name ,arglist
        ,@body)
       (defun ,name (&rest args)
         (apply #',debug-name args)
         (apply #',default-name args)))))

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
  (let ((cell x))
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
    (declare (dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-1 ()
  (let ((v (make-array '(42) :element-type 'single-float)))
    (declare (dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-2 (n x)
  (declare (integer n))
  (let ((v (make-array n :initial-contents x)))
    (declare (dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-3 (x y z)
  (let ((v (make-array 3
                       :element-type 'fixnum :initial-contents (list x y z)
                       :element-type t :initial-contents x)))
    (declare (dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-4 ()
  (let ((v (make-array 3 :initial-contents '(1 2 3))))
    (declare (dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-5 ()
  (let ((v (make-array 3 :initial-element 12 :element-type t)))
    (declare (dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-6 ()
  (let ((v (make-array 3 :initial-element 12 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-7 ()
  (let ((v (make-array 3 :initial-element 12 :element-type '(signed-byte 8))))
    (declare (dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-8 ()
  (let ((v (make-array 3 :initial-element 12 :element-type 'word)))
    (declare (dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-9 ()
  (let ((v (make-array 3 :initial-element 12.0 :element-type 'single-float)))
    (declare (dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-10 ()
  (let ((v (make-array 3 :initial-element 12.0d0 :element-type 'double-float)))
    (declare (dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-11 ()
  (let ((v (make-array (the integer (opaque-identity 3)) :initial-element 12.0d0 :element-type 'double-float)))
    (declare (dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx vector-on-stack (x y)
  (let ((v (vector 1 x 2 y 3)))
    (declare (dynamic-extent v))
    (true v)
    nil))

(defun-with-dx make-3d-fixed-array-on-stack-1 ()
  (let ((a (make-array '(4 8 3) :initial-element 12 :element-type t)))
    (declare (dynamic-extent a))
    (true a)
    (true a)
    nil))
(defun-with-dx make-3d-fixed-array-on-stack-2 (a b c d)
  (sb-int:dx-let ((a (make-array '(2 2 1)
                                 :element-type 'bit
                                 :initial-contents `(#((,a) (,b)) (#(,c) (,d))))))
    (true a)
    (true a)
    nil))

(defun-with-dx make-2d-variable-array-on-stack ()
  (let* ((n (opaque-identity 5))
         (a (make-array `(,n 2) :initial-element 12 :element-type t)))
    (declare (dynamic-extent a))
    (true a)
    (true a)
    nil))

(defun 2d-array-initializer (n)
  (ecase n
    (1 '((a)))
    (2 '((a b) (c d)))
    (3 '((a b c) (c d e) (f g h)))))

(defun-with-dx make-2d-array-function-initializer (n)
  (let* ((x (opaque-identity n))
         (y (opaque-identity x))
         (a (make-array `(,x ,y) :initial-contents (2d-array-initializer x))))
    (declare (dynamic-extent a))
    (true a)
    (true a)
    nil))

;;; MAKE-LIST

(declaim (inline make-list-container))
(defstruct list-container listy-slot)
(defun make-var-length-dx-list (n thunk)
  (sb-int:dx-let ((s (make-list-container :listy-slot (make-list n))))
    (values (funcall (the function thunk) s))))
;; stack-allocatable lists are necessary but not sufficient
(with-test (:name (:dx-list :make-list) :fails-on (not :x86-64)
                  :skipped-on :debug-gc-barriers)
  (let ((calls (ctu:asm-search "CALL" #'make-var-length-dx-list)))
    ;; Call nothing but the funarg
    (assert (eql (length calls) 1)))
  (assert-no-consing (make-var-length-dx-list
                      50 (lambda (x) (declare (ignore x))))))

;;; MAKE-STRUCTURE

;; stack-allocatable fixed-size objects are necessary but not sufficient
(with-test (:name :copy-structure-dx :fails-on (not (or :x86 :x86-64)))
  (let ((thing sb-c::*backend-parsed-vops*))
    ;; check some preconditions
    (assert (typep thing 'hash-table))
    (assert (/= (sb-kernel:layout-bitmap (sb-kernel:%instance-layout thing))
                sb-kernel:+layout-all-tagged+))
    (assert-no-consing
     (sb-int:dx-let ((x (copy-structure thing)))
       (opaque-identity x)
       0))))

(declaim (inline make-fp-struct-1))
(defstruct fp-struct-1
  (s 0.0 :type single-float)
  (d 0.0d0 :type double-float))

(defun-with-dx test-fp-struct-1.1 (s d)
  d
  (let ((fp (make-fp-struct-1 :s s)))
    (declare (dynamic-extent fp))
    (assert (eql s (fp-struct-1-s fp)))
    (assert (eql 0.0d0 (fp-struct-1-d fp)))))

(defun-with-dx test-fp-struct-1.2 (s d)
  s
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
  d
  (let ((fp (make-fp-struct-2 :s s)))
    (declare (dynamic-extent fp))
    (assert (eql s (fp-struct-2-s fp)))
    (assert (eql 0.0d0 (fp-struct-2-d fp)))))

(defun-with-dx test-fp-struct-2.2 (s d)
  s
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
  d
  (let ((cfp (make-cfp-struct-1 :s s)))
    (declare (dynamic-extent cfp))
    (assert (eql s (cfp-struct-1-s cfp)))
    (assert (eql (complex 0.0d0) (cfp-struct-1-d cfp)))))

(defun-with-dx test-cfp-struct-1.2 (s d)
  s
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
  d
  (let ((cfp (make-cfp-struct-2 :s s)))
    (declare (dynamic-extent cfp))
    (assert (eql s (cfp-struct-2-s cfp)))
    (assert (eql (complex 0.0d0) (cfp-struct-2-d cfp)))))

(defun-with-dx test-cfp-struct-2.2 (s d)
  s
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

;; It works to declare a structure constructor INLINE after the DEFSTRUCT
;; was processed, as long as it was in the null lexical environment.
;; In a perfect world, we'd figure out that a variable declared DX which
;; receives the value of a structure constructor defined in the same file
;; and neither expressly INLINE nor NOTINLINE should be locally inlined.
;; But at least this shows that a declaration at the intended use point
;; is sufficient, without also a bracketing INLINE/NOTINLINE at the DEFSTRUCT.

;; Verify the precondition for the assertions that claim that DXifying works
;; even though the MAKE- function was not expressly INLINE when its DEFSTRUCT
;; was compiled.
(dolist (s '(make-foo1 make-foo2 make-foo3))
  (assert (null (sb-int:info :function :inlining-data s))))

(defstruct foo1 x)

(defun-with-dx make-foo1-on-stack (x)
  (declare (inline make-foo1))
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
  (declare (inline make-foo2))
  x
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
  (declare (inline make-foo3))
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
    ;; the NESTED instance itself *should* be DX!
    (copy-nested (caar x))))

(with-test (:name :conservative-nested-dx)
  ;; NESTED-BAD should not stack-allocate :BAR due to the SETF.
  (assert (equalp (nested-bad 42) (make-nested-good :bar *bar*)))
  (assert (equalp *bar* (list (list (make-nested-bad :bar (list 42)))))))

;;; Conditional nested DX

;; These two test cases prompted a substantial redesign of the STACK
;; phase of the compiler to handle their particular permutation of
;; nested DX.

(with-test (:name (:bug-1044465 :reduced))
  ;; Test case from Stas Boukarev
  (checked-compile '(lambda (x)
                      (let ((a (if x
                                   (list (list x))
                                   (list (list x)))))
                        (declare (dynamic-extent a))
                        (prin1 a)
                        1))))

(with-test (:name (:bug-1044465 :nasty))
  ;; Test case from Alastair Bridgewater
  (checked-compile '(lambda (x y)
                      (dotimes (i 2)
                        (block bar
                          (let ((a (if x
                                       (list (list x))
                                       (list (list x))))
                                (b (if y x (return-from bar x))))
                            (declare (dynamic-extent a))
                            (prin1 a)
                            b))))))

;;; multiple uses for dx lvar

(defun-with-dx multiple-dx-uses ()
  (let ((dx (if (true t)
                (list 1 2 3)
                (list 2 3 4))))
    (declare (dynamic-extent dx))
    (true dx)
    nil))

;;; mapfoo should make the initial cons dx

(defun loop-collect-negate (x) (loop for item in x collect (- x)))
(defun mapcar-negate (x) (mapcar #'- x))
(defun mapcan-reverse (x) (mapcan #'reverse x))

;;; handler-case and handler-bind should use DX internally

(defun dx-handler-bind (x)
  (let ((y 3))
    (macrolet ((fool () `(lambda (c) (print (list c (incf y))))))
      (handler-bind ((error
                      #'(lambda (c)
                          (break "OOPS: ~S caused ~S" x c)))
                     (warning (fool))
                     ((and serious-condition (not error))
                      #'(lambda (c)
                          (break "OOPS2: ~S did ~S" x c))))
        (/ 2 x)))))

(defun dx-handler-case (x)
  (assert (zerop (handler-case (/ 2 x)
                   (error (c)
                     (break "OOPS: ~S caused ~S" x c)
                     -1)
                   (:no-error (res)
                     (1- res))))))

(defun list-delete-some-stuff ()
  ;; opaque-identity hides the fact that we are calling a destructive function
  ;; on a constant, which is technically illegal. But no deletion occurs,
  ;; so it's innocuous. Also these aren't tests of DX, but oh well...
  (declare (muffle-conditions style-warning))
  (delete 'a (opaque-identity '(x y)))
  (delete 'a (opaque-identity '(x y)) :from-end t)
  (delete-duplicates (opaque-identity '(x y))))

(defvar *a-cons* (cons nil nil))

(with-test (:name (:no-consing :dx-closures))
  (assert-no-consing (dxclosure 42)))

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
  (assert-no-consing (loop-collect-negate nil))
  (assert-no-consing (mapcar-negate nil))
  (assert-no-consing (mapcan-reverse nil))
  (assert-no-consing (list-delete-some-stuff))
  (assert-no-consing (multiple-dx-uses)))

(with-test (:name (:no-consing :dx-value-cell))
  (assert-no-consing (dx-value-cell 13)))

(with-test (:name (:no-consing :dx-fixed-objects))
  (assert-no-consing (cons-on-stack 42))
  (assert-no-consing (make-foo1-on-stack 123))
  (assert-no-consing (nested-good 42))
  (assert-no-consing (nested-dx-conses))
  (assert-no-consing (dx-handler-bind 2))
  (assert-no-consing (dx-handler-case 2)))

(with-test (:name (:no-consing :dx-vectors))
  (assert-no-consing (force-make-array-on-stack 128))
  (assert-no-consing (make-array-on-stack-2 5 '(1 2.0 3 4.0 5)))
  (assert-no-consing (make-array-on-stack-3 9 8 7))
  (assert-no-consing (make-array-on-stack-4))
  (assert-no-consing (make-array-on-stack-5))
  (assert-no-consing (vector-on-stack :x :y)))

(with-test (:name (:no-consing :dx-arrays))
  (assert-no-consing (make-3d-fixed-array-on-stack-1))
  (assert-no-consing (make-2d-variable-array-on-stack))
  (assert-no-consing (make-2d-array-function-initializer 1))
  (assert-no-consing (make-2d-array-function-initializer 2))
  (assert-no-consing (make-2d-array-function-initializer 3)))

(with-test (:name (:no-consing :dx-specialized-arrays)
            :skipped-on (not :c-stack-is-control-stack))
  (assert-no-consing (make-3d-fixed-array-on-stack-2 0 0 1 1)))

(with-test (:name (:no-consing :specialized-dx-vectors)
            :skipped-on (not :c-stack-is-control-stack))
  (assert-no-consing (make-array-on-stack-1))
  (assert-no-consing (make-array-on-stack-6))
  (assert-no-consing (make-array-on-stack-7))
  (assert-no-consing (make-array-on-stack-8))
  (assert-no-consing (make-array-on-stack-9))
  (assert-no-consing (make-array-on-stack-10))
  (assert-no-consing (make-array-on-stack-11)))

(with-test (:name (:no-consing :dx-raw-instances)
            :skipped-on (not (and :generational :c-stack-is-control-stack)))
  (let (a b)
    (setf a 1.24 b 1.23d0)
    (assert-no-consing (make-foo2-on-stack a b)))
    (assert-no-consing (make-foo3-on-stack)))

;;; not really DX, but GETHASH and (SETF GETHASH) should not cons

(defvar *table* (make-hash-table))

(defun test-hash-table ()
  (setf (gethash 5 *table*) 13)
  (setf (gethash 6 *table*) 14)
  (values (gethash 5 *table*)
          (gethash 6 *table*)))

(with-test (:name (:no-consing :hash-tables))
  (test-hash-table) ;; initialize all the vectors first
  (assert-no-consing (test-hash-table)))

;;; Both with-pinned-objects and without-gcing should not cons

(defun call-without-gcing (fun)
  (sb-sys:without-gcing (funcall fun)))

(defun call-with-pinned-object (fun obj)
  (sb-sys:with-pinned-objects (obj)
    (funcall fun obj)))

(with-test (:name (:no-consing :without-gcing))
  (assert-no-consing (call-without-gcing (lambda ()))))

(with-test (:name (:no-consing :with-pinned-objects))
  (assert-no-consing (call-with-pinned-object #'identity 42)))

;;; with-mutex should use DX and not cons

(defvar *mutex* (sb-thread::make-mutex :name "mutexlock"))

(defun test-mutex ()
  (sb-thread:with-mutex (*mutex*)
    (true *mutex*)))

(with-test (:name (:no-consing :mutex) :skipped-on (not :sb-thread))
  (assert-no-consing (test-mutex)))


;;; Bugs found by Paul F. Dietz

(with-test (:name (:dx-bug-misc :pfdietz))
  (checked-compile-and-assert (:optimize :all)
      '(lambda (a b)
         (let* ((v5 (cons b b)))
           (declare (dynamic-extent v5))
           v5
           a))
    (('x 'y) 'x)))

(with-test (:name :bug-1738095)
  ;; STACK analysis wasn't marking UVL or DX LVARs that are held live
  ;; by a DX allocation as being live in blocks along the path from
  ;; the allocation to the ENTRY node, causing problems when there is
  ;; a block that is outside the lexical environment of the "earlier"
  ;; DX allocation (in the case below, such a block would be the first
  ;; (outermost) use of LIST).
  (checked-compile '(lambda ()
                     (let ((* (list (let ((* (list nil)))
                                      (declare (dynamic-extent *))
                                      (list 1)))))
                       (declare (dynamic-extent *))))))

(with-test (:name :bug-1739308)
  ;; STACK analysis wasn't propagating DX LVARs back from ENTRY to
  ;; allocation through non-local-entry environments (below, the CATCH
  ;; entry point), causing problems when such is the ONLY live path
  ;; back to the allocation.
  (checked-compile '(lambda (x y)
                     (let ((s
                            (multiple-value-prog1
                                (list x)
                              (catch 'ct1
                                (throw 'ct8 30)))))
                       (declare (dynamic-extent s))
                       (funcall (the function y) s)))))

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
  (assert-no-consing (bdowning-2005-iv-16))
  (bdowning-2005-iv-16))

(declaim (inline my-nconc))
(defun my-nconc (&rest lists)
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
    (values a b)))
(with-test (:name :let-converted-vars-dx-allocated-bug)
  (multiple-value-bind (i j) (let-converted-vars-dx-allocated-bug-default 1 2 3)
    (assert (equal i j))
    (assert (equal i (list 1 2 3)))))

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
  (assert-no-consing (assert (eql n (funcall fun nil))))
  (assert (eql n (funcall fun nil))))

(macrolet ((def (n f1 f2 f3)
             (let ((name (sb-int:package-symbolicate "DX-FLET-TEST." n)))
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
  (def 1 (make-array 128 :initial-element nil) (list 1 2 3 4 5 6 7 8) (list 'list))
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
  (checked-compile '(lambda ()
                      (declare (muffle-conditions compiler-note))
                      (flet ((make (x y)
                               (let ((res (cons x x)))
                                 (setf (cdr res) y)
                                 res)))
                        (declare (inline make))
                        (let ((z (make 1 2)))
                          (declare (dynamic-extent z))
                          (print z)
                          t))))
  (checked-compile '(lambda ()
                      (declare (muffle-conditions compiler-note))
                      (flet ((make (x y)
                               (let ((res (cons x x)))
                                 (setf (cdr res) y)
                                 (if x res y))))
                        (declare (inline make))
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

(with-test (:name :handler-case-bogus-compiler-note)
  ;; Taken from SWANK, used to signal a bogus stack allocation
  ;; failure note.
  (checked-compile
   `(lambda (files fasl-dir load)
      (declare (muffle-conditions style-warning))
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
                (handle-loadtime-error c dest)))))))
   :allow-notes nil))

(declaim (inline barvector))
(defun barvector (x y z)
  (make-array 3 :initial-contents (list x y z)))
(with-test (:name :dx-compiler-notes)
  (flet ((assert-notes (j lambda)
           (let ((notes (nth 4 (multiple-value-list (checked-compile lambda))))) ; TODO
             (unless (= (length notes) j)
               (error "Wanted ~S notes, got ~S for~%   ~S"
                      j (length notes) lambda)))))
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
                       (sort list (lambda (x y) ; shut unrelated notes up
                                    (< (truly-the fixnum x)
                                       (truly-the fixnum y))))))
    (assert-notes 0 `(lambda (other)
                       #'(lambda (s c n)
                           (ignore-errors (funcall other s c n)))))))

;;; Stack allocating a value cell in HANDLER-CASE would blow up stack
;;; in an unfortunate loop.
(defun handler-case-eating-stack ()
  (declare (muffle-conditions warning)) ; "dead code detected ... IR1-PHASES"
  (let ((sp nil))
    (do ((n 0 (logand most-positive-fixnum (1+ n))))
        ((>= n 1024))
     (multiple-value-bind (value error) (ignore-errors)
       (when (and value error) nil))
      (if sp
          (assert (= sp (sb-c::%primitive sb-c:current-stack-pointer)))
          (setf sp (sb-c::%primitive sb-c:current-stack-pointer))))))
(with-test (:name :handler-case-eating-stack)
  (assert-no-consing (handler-case-eating-stack)))

;;; A nasty bug where RECHECK-DYNAMIC-EXTENT-LVARS thought something was going
;;; to be stack allocated when it was not, leading to a bogus %NIP-VALUES.
;;; Fixed by making RECHECK-DYNAMIC-EXTENT-LVARS deal properly with nested DX.
(deftype vec ()
  `(simple-array t (3)))
(declaim (ftype (function (t t t) vec) vec))
(declaim (inline vec))
(defun vec (a b c)
  (make-array 3 :initial-contents (list a b c)))
(defun bad-boy (vec)
  (declare (type vec vec))
  (lambda (fun)
    (let ((vec (vec (aref vec 0) (aref vec 1) (aref vec 2))))
      (declare (dynamic-extent vec))
      (funcall fun vec))))
(with-test (:name :recheck-nested-dx-bug)
  (assert (funcall (bad-boy (vec 1.0 2.0 3.3))
                   (lambda (vec) (equalp vec (vec 1.0 2.0 3.3)))))
  (flet ((foo (x) (declare (ignore x))))
    (let ((bad-boy (bad-boy (vec 2.0 3.0 4.0))))
      (assert-no-consing (funcall bad-boy #'foo)))))

(with-test (:name :bug-497321)
  (flet ((test (lambda &rest args)
           (multiple-value-bind (fun failure-p warnings style-warnings notes)
               (apply #'checked-compile lambda args)
             (declare (ignore fun failure-p))
             (assert (= (length (append warnings style-warnings notes)) 1)))))
    (test `(lambda () (declare (dynamic-extent #'bar)))
          :allow-style-warnings 'style-warning)
    (test `(lambda () (declare (dynamic-extent bar)))
          :allow-warnings 'warning)
    (test `(lambda (bar) (cons bar (lambda () (declare (dynamic-extent bar)))))
          :allow-notes 'sb-ext:compiler-note)
    (test `(lambda ()
             (flet ((bar () t))
               (cons #'bar (lambda () (declare (dynamic-extent #'bar))))))
          :allow-notes 'sb-ext:compiler-note)))

(with-test (:name :bug-586105)
  (flet ((test (x)
           (let ((vec1 (make-array 1 :initial-contents (list (list x))))
                 (vec2 (make-array 1 :initial-contents `((,x))))
                 (vec3 (make-array 1 :initial-contents `#((,x))))
                 (vec4 (make-array 1 :initial-contents `(#(,x)))))
             (declare (dynamic-extent vec1 vec2 vec3 vec4))
             (assert (eql x (car (aref vec1 0))))
             (assert (eql x (car (aref vec2 0))))
             (assert (eql x (car (aref vec3 0))))
             (assert (eql x (elt (aref vec4 0) 0))))))
    (assert-no-consing (test 42))))

(defun bug-681092 ()
  (declare (optimize speed))
  (let ((c 0))
    (flet ((bar () c))
      (declare (dynamic-extent #'bar))
      (do () ((list) (bar))
        (setf c 10)
        (return (bar))))))
(with-test (:name :bug-681092)
  (assert (= 10 (bug-681092))))

;;;; Including a loop in the flow graph between a DX-allocation and
;;;; the start of its environment would, for a while, cause executing
;;;; any of the code in the loop to discard the value.  Found via
;;;; attempting to DX-allocate an array, which triggered the FILL
;;;; transform, which inserted such a loop.
(defun bug-1472785 (x)
  (let ((y (let ((z (cons nil nil)))
             (let ((i 0))
               (tagbody
                b1
                  (when (= x i) (go b2))
                  (incf i)
                  (go b1)
                b2))
             z))
        (w (cons t t)))
    (declare (dynamic-extent y w))
    (eq y w)))
(with-test (:name :bug-1472785)
  (assert (null (bug-1472785 1))))

;;;; &REST lists should stop DX propagation -- not required by ANSI,
;;;; but required by sanity.

(declaim (inline rest-stops-dx))
(defun-with-dx rest-stops-dx (&rest args)
  (declare (dynamic-extent args))
  (apply #'opaque-identity args))

(defun-with-dx rest-stops-dx-ok ()
  (equal '(:foo) (rest-stops-dx (list :foo))))

(with-test (:name :rest-stops-dynamic-extent)
  (assert (rest-stops-dx-ok)))

;;;; These tests aren't strictly speaking DX, but rather &REST -> &MORE
;;;; conversion.
(with-test (:name :rest-to-more-conversion)
  (let ((f1 (checked-compile `(lambda (f &rest args)
                                (apply f args)))))
    (assert-no-consing (assert (eql f1 (funcall f1 #'identity f1)))))
  (let ((f2 (checked-compile `(lambda (f1 f2 &rest args)
                                (values (apply f1 args) (apply f2 args))))))
    (assert-no-consing (multiple-value-bind (a b)
                           (funcall f2 (lambda (x y z) (+ x y z)) (lambda (x y z) (- x y z))
                                    1 2 3)
                         (assert (and (eql 6 a) (eql -4 b))))))
  (let ((f3 (checked-compile `(lambda (f &optional x &rest args)
                                (when x
                                  (apply f x args))))))
    (assert-no-consing (assert (eql 42 (funcall f3
                                                (lambda (a b c) (+ a b c))
                                                11
                                                10
                                                21)))))
  (let ((f4
         (checked-compile `(lambda (f &optional x &rest args
                                    &key y &allow-other-keys)
                             (apply f y x args))
                          :allow-style-warnings t)))
    (assert-no-consing (funcall f4 (lambda (y x yk y2 b c)
                                     (assert (eq y 'y))
                                     (assert (= x 2))
                                     (assert (eq :y yk))
                                     (assert (eq y2 'y))
                                     (assert (eq b 'b))
                                     (assert (eq c 'c)))
                                2 :y 'y 'b 'c)))
  (checked-compile-and-assert ()
      `(lambda (a b c &rest args)
         (apply #'list* a b c args))
    ((1 2 3 4 5 6 '(7)) '(1 2 3 4 5 6 7)))
  (checked-compile-and-assert ()
      `(lambda (x y)
         (concatenate 'string x y))
    (("foo" "bar") "foobar"))
  (checked-compile-and-assert ()
      `(lambda (&rest args)
         (lambda (f)
           (apply f args)))
    (('a 'b 'c 'd 'e 'f) '(a b c d e f)
     :test (lambda (values expected)
             (equal (multiple-value-list
                     (funcall (first values) 'list))
                    expected))))
  (checked-compile-and-assert ()
      `(lambda (&rest args)
         (flet ((foo (f)
                  (apply f args)))
           #'foo))
    (('a 'b 'c 'd 'e 'f) '(a b c d e f)
     :test (lambda (values expected)
             (equal (multiple-value-list
                     (funcall (first values) 'list))
                    expected))))
  (checked-compile-and-assert ()
      `(lambda (f &rest args)
         (flet ((foo (g)
                  (apply g args)))
           (declare (dynamic-extent #'foo))
           (funcall f #'foo)))
    (((lambda (f) (funcall f 'list)) 'a 'b 'c 'd 'e 'f)
     '(a b c d e f)))
  (checked-compile-and-assert ()
      `(lambda (f &rest args)
         (flet ((foo (g)
                  (apply g args)))
           (funcall f #'foo)))
    (((lambda (f) (funcall f 'list)) 'a 'b 'c 'd 'e 'f)
     '(a b c d e f)))
  (checked-compile-and-assert ()
      `(lambda (x y z)
         (block out
           (labels ((foo (x &rest rest)
                      (apply (lambda (&rest rest2)
                               (return-from out (values-list rest2)))
                             x rest)))
             (if x
                 (foo x y z)
                 (foo y z x)))))
    ((1 2 3) (values 1 2 3))))

(defun opaque-funcall (function &rest arguments)
  (apply function arguments))

(with-test (:name :implicit-value-cells)
  (flet ((test-it (type input output)
           (checked-compile-and-assert ()
               `(lambda (x)
                  (declare (type ,type x))
                  (flet ((inc ()
                           (incf x)))
                    (declare (dynamic-extent #'inc))
                    (list (opaque-funcall #'inc) x)))
             ((input) (list output output)))))
    (let ((width sb-vm:n-word-bits))
      (test-it t (1- most-positive-fixnum) most-positive-fixnum)
      (test-it `(unsigned-byte ,(1- width)) (ash 1 (- width 2)) (1+ (ash 1 (- width 2))))
      (test-it `(signed-byte ,width) (ash -1 (- width 2)) (1+ (ash -1 (- width 2))))
      (test-it `(unsigned-byte ,width) (ash 1 (1- width)) (1+ (ash 1 (1- width))))
      (test-it 'single-float 3f0 4f0)
      (test-it 'double-float 3d0 4d0)
      (test-it '(complex single-float) #c(3f0 4f0) #c(4f0 4f0))
      (test-it '(complex double-float) #c(3d0 4d0) #c(4d0 4d0)))))

(with-test (:name :sap-implicit-value-cells)
  (let ((f (checked-compile `(lambda (x)
                               (declare (type system-area-pointer x))
                               (flet ((inc ()
                                        (setf x (sb-sys:sap+ x 16))))
                                 (declare (dynamic-extent #'inc))
                                 (list (opaque-funcall #'inc) x)))))
        (width sb-vm:n-machine-word-bits))
    (assert (every (lambda (x)
                     (sb-sys:sap= x (sb-sys:int-sap (+ 16 (ash 1 (1- width))))))
                   (funcall f (sb-sys:int-sap (ash 1 (1- width))))))))

(with-test (:name (:&more-bounds :lp-1154946))
  (checked-compile-and-assert () '(lambda (&rest args) (car args)) (() nil))
  (checked-compile-and-assert () '(lambda (&rest args) (nth 6 args)) (() nil))
  (checked-compile-and-assert () '(lambda (&rest args) (cadr args)) (() nil))
  (checked-compile-and-assert () '(lambda (&rest args) (third args)) (() nil)))

(with-test (:name :local-notinline-functions)
  (multiple-value-bind (start result end)
      (funcall (checked-compile
                `(lambda ()
                   (values (sb-kernel:current-sp)
                           (flet ((x () (cons 1 2)))
                             (declare (notinline x))
                             (let ((x (x)))
                               (declare (dynamic-extent x))
                               (true x))
                             (true 10))
                           (sb-kernel:current-sp)))))
    (assert (sb-sys:sap= start end))
    (assert result))
  (multiple-value-bind (start result end)
      (funcall (checked-compile
                `(lambda ()
                   (declare (optimize speed))
                   (values (sb-kernel:current-sp)
                           (flet ((x () (cons 1 2)))
                             (let ((x (x))
                                   (y (x)))
                               (declare (dynamic-extent x y))
                               (true x)
                               (true y))
                             (true 10))
                           (sb-kernel:current-sp)))))
    (assert (sb-sys:sap= start end))
    (assert result)))

(with-test (:name :unused-paremeters-of-an-inlined-function)
  (let ((name (gensym "fun")))
    (proclaim `(inline ,name))
    (eval `(defun ,name (a b &optional c d)
             (declare (ignore c d))
             (cons a b)))
    (checked-compile-and-assert ()
        `(lambda ()
           (let ((x (cons
                     (,name 1 2)
                     (,name 2 3))))
             (declare (dynamic-extent x))
             (true x))
           #',name)
      (() '(2 . 3) :test (lambda (values expected)
                           (equal (multiple-value-list
                                   (funcall (first values) 2 3))
                                  expected))))))

(with-test (:name :nested-multiple-use-vars)
  (let ((fun (checked-compile
              `(lambda ()
                 (sb-int:dx-let ((x (let ((x (make-array 3)))
                                      (setf (aref x 0) 22)
                                      x)))
                   (opaque-identity x)
                   10)))))
    (assert-no-consing (funcall fun))))

(with-test (:name :nested-multiple-use-vars-vector-fill)
  (let ((fun (checked-compile
              `(lambda ()
                 (declare (optimize speed))
                 (sb-int:dx-let ((x (make-array 3 :initial-element 123)))
                   (opaque-identity x)
                   10)))))
    (assert-no-consing (funcall fun))))

(defun trythisfun (test arg &key key)
  (declare (dynamic-extent test key))
  (funcall test (funcall key arg)))
(declaim (maybe-inline sortasort))
(defun sortasort (seq pred)
  (declare (dynamic-extent pred))
  (funcall pred (elt seq 0) (elt seq 1))
  seq)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'sb-c::fun-name-dx-args))
(with-test (:name :store-dx-arglist)
  ;; Positional argument 0 and keyword argument :KEY
  (assert (equal (fun-name-dx-args 'trythisfun) '(0 :key)))
  ;; Positional argument 1
  (assert (equal (fun-name-dx-args 'sortasort) '(1)))
  ;; And also an inline expansion
  (assert (sb-c::fun-name-inline-expansion 'sortasort)))
(with-test (:name :store-dx-arglist-std-functions)
  ;; You might think this would go in the SB-C::FUN-INFO,
  ;; but we want user-defined functions to have this bit of info
  ;; as well, potentially.
  (assert (equal (fun-name-dx-args 'remove) '(:test :test-not :key)))
  (assert (equal (fun-name-dx-args 'remove-if) '(0 :key))))

(defun trivial-hof (fun arg)
  (declare (dynamic-extent fun))
  (funcall fun 3 arg))

(declaim (ftype (function (function t &key (:key function)) *)
                fancy-hof))
(defun fancy-hof (pred arg &key key)
  (declare (dynamic-extent pred key))
  (funcall pred (funcall key arg)))

(defun autodxclosure1 (&optional (x 4))
  ;; Calling a higher-order function will only implicitly DXify a funarg
  ;; if the callee is trusted (a CL: function) or the caller is unsafe.
  (declare (optimize speed (safety 0) (debug 0)))
  (trivial-hof (lambda (a b) (+ a b x)) 92))

(defun autodxclosure2 (&aux (i 0) (j 0))
  (declare (optimize speed (safety 0) (debug 0)))
  (assert (eq (fancy-hof (lambda (x) (incf i) (symbolp x))
                         '(a b)
                         :key (lambda (x) (incf j) (car x)))
              t))
  (assert (and (= i 1) (= j 1))))

(with-test (:name (:no-consing :auto-dx-closures))
  (assert-no-consing (autodxclosure1 42))
  (assert-no-consing (autodxclosure2)))

(with-test (:name (:no-consing :more-auto-dx-closures))
  (assert-no-consing
   (let ((ct 0))
     (sb-vm:map-allocated-objects
      (lambda (obj type size)
        (declare (ignore obj type size))
        (incf ct))
      :static)
     ;; Static-space has some small number of objects
     (assert (<= 1 ct 100)))))

(with-test (:name :cast-dx-funarg-no-spurious-warn)
  (checked-compile
   '(lambda (&key (test #'eql) key)
     (declare (function test key))
     (declare (dynamic-extent test key))
     test key
     1)
   :allow-notes nil))

(with-test (:name :stack-alloc-p :skipped-on (not :sb-thread))
  (let* ((sem1 (sb-thread:make-semaphore))
         (sem2 (sb-thread:make-semaphore))
         (blah nil)
         (thread
          (sb-thread:make-thread
           (lambda ()
             (sb-int:dx-let ((a (cons 1 2)))
               (setq blah a)
               (sb-thread:signal-semaphore sem1)
               (sb-thread:wait-on-semaphore sem2))
             'yay))))
    ;; thread1 will assign something on its stack into BLAH
    (sb-thread:wait-on-semaphore sem1)
    (assert blah)
    ;; not on my stack
    (assert (not (sb-ext:stack-allocated-p blah)))
    ;; but on their stack
    (assert (eq (sb-ext:stack-allocated-p blah t) thread))
    (setq blah nil) ; be safe, don't look at other stacks
    (sb-thread:signal-semaphore sem2)
    (sb-thread:join-thread thread)))

(with-test (:name :back-propagation-losing-blocks)
  (checked-compile-and-assert ()
   `(lambda ()
      (let ((v (list (list :good)
                     (labels ((r (v)
                                (or v
                                    (r (not v)))))
                       (r nil)))))
        (declare (dynamic-extent v))
        (caar v)))
   (() :good)))

(with-test (:name :back-propagation-losing-blocks.2)
  (checked-compile-and-assert
   ()
   `(lambda (b c)
      (let ((v
              (vector
               (list 10)
               (BLOCK NIL
                 (let ((loop-repeat-550 1)
                       (loop-sum-551 0))
                   (tagbody
                    next-loop
                      (if (<= loop-repeat-550 0)
                          (go end-loop)
                          (decf loop-repeat-550))
                      (setq loop-sum-551
                            (if (not c)
                                (return-from nil 0)
                                (block nil
                                  (let ((loop-repeat-552 1)
                                        (loop-sum-553 0))
                                    (tagbody
                                     next-loop
                                       (if (<= loop-repeat-552 0)
                                           (go end-loop)
                                           (decf loop-repeat-552))
                                       (setq loop-sum-553
                                             (if b
                                                 1
                                                 2))
                                       (go next-loop)
                                     end-loop
                                       (return-from nil loop-sum-553))))))
                      (go next-loop)
                    end-loop)
                   loop-sum-551)))))
        (declare (dynamic-extent v))
        (car (elt v 0))))
   ((t t) 10)))

(with-test (:name :back-propagate-one-dx-lvar-nlx)
  (checked-compile-and-assert
   ()
   `(lambda (c)
      (catch 'c
        (let ((v (list (vector 0 c 0 0) (catch 'ct5 (throw 'ct5 0)) 0)))
          (declare (dynamic-extent v))
          (elt (elt v 0) 1))))
    ((33) 33)))

(with-test (:name :dominators-recomputation)
  (let (sb-c::*check-consistency*)
    (checked-compile-and-assert
     ()
     `(lambda (x)
        (let ((m (if x
                     (make-array 2 :initial-element 1)
                     (make-array 2 :initial-element 2))))
          (declare (dynamic-extent m))
          (elt m 0)))
     ((t) 1)
     ((nil) 2))))

(with-test (:name :notes-in-deleted-code)
  (checked-compile
   '(lambda ()
     (labels ((z ()
                (list 1))
              (fn (&key)
                (let ((x (z)))
                  (declare (dynamic-extent x))
                  (print x)
                  1)))
       (declare (ignorable #'fn))))
   :allow-notes nil))

(with-test (:name :list+fill+make-array)
  (let ((fun (checked-compile
              `(lambda ()
                 (declare (optimize speed))
                 (let* ((v (make-array 8 :initial-element nil))
                        (node (let ((m v))
                                (list m m))))
                   (declare (simple-vector v)
                            (dynamic-extent v node))
                   (opaque-identity node)
                   0))
              :allow-notes nil)))
    (assert-no-consing (funcall fun))))

(with-test (:name :with-output-to-string)
  (let ((s (make-array 20000 :fill-pointer 0 :element-type 'character)))
    (assert-no-consing
     (with-output-to-string (*standard-output* s)
       (write-char #\x)))))

(with-test (:name :cycles-without-dx-lvars)
  (checked-compile-and-assert
   ()
   `(lambda (f x z)
      (let ((l (if x
                   (loop while z)
                   (list (list 1)))))
        (declare (dynamic-extent l))
        (funcall f l)))
   (((lambda (l) (equal l '((1)))) nil nil) t)
   (((lambda (l) (equal l nil)) t nil) t)))

;;; Test that we don't preserve UVLs too long because of DX in stack
;;; analysis.
(with-test (:name :uvl-preserved-by-dx-too-long)
  (checked-compile
   '(lambda (f1 test args)
     (flet ((f3 (&rest m) (apply test m)))
       (declare (dynamic-extent #'f3))
       (apply f1 #'f3 args)))))

;;; Test that we don't preserve LVARs that just happen to be in the
;;; same block.
(with-test (:name :uvl-preserved-incidentally)
  (checked-compile
   '(lambda (b)
     (catch 'ct2
       (multiple-value-prog1 (multiple-value-prog1 (restart-case 0))
         (restart-case 0)
         b
         0)))))

(with-test (:name :uvl-preserved-incidentally.2)
  (checked-compile
   '(lambda ()
     (multiple-value-prog1
         (multiple-value-prog1 (catch 'ct2 0))
       (restart-bind nil
         (flet ((%f () 0))
           (declare (dynamic-extent (function %f)))
           (%f)))))))

;;; After dx entries no longer started delimiting their blocks, there
;;; was no reason for propagate-dx to end its own blocks either. In
;;; fact propagate-dx started causing problems instead.
(with-test (:name :propagate-dx-ended-block)
  (checked-compile
   '(lambda (c)
     (declare (notinline - svref))
     (let* ((v1 (svref #(1 3 4 6) 0)))
       (declare (dynamic-extent v1))
       (- c v1)))))

(with-test (:name :dx-for-optional-entries)
  (checked-compile-and-assert
   ()
   `(lambda (a b c)
      (declare (notinline funcall max))
      (labels ((%f14
                   (f14-1 f14-2
                    &optional (f14-3 (setq a (min 13 (max 1 0))))
                              (f14-4 (setq c 0)))
                 (declare (ignore f14-1 f14-2 f14-3 f14-4))
                 0))
        (declare (dynamic-extent (function %f14)))
        (funcall #'%f14
                 (%f14
                  (min 11 (max 0 c))
                  (funcall #'%f14 a b 1)
                  1
                  b)
                 0)))
   ((1 2 3) 0)))

(with-test (:name :dx-for-optional-entries.2)
  (checked-compile-and-assert
   (:optimize '(:speed 3))
   `(lambda (b)
      (declare (notinline funcall))
      (labels ((%f1 (&optional f1-1)
                 (declare (ignore f1-1))
                 (shiftf b 0)))
        (declare (dynamic-extent (function %f1)))
        (funcall #'%f1 :bad)))
   ((10) 10)))

(with-test (:name :no-stack-cleanup-before-return)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (let ((identity (eval '#'identity))
               (continuation (eval '(lambda (&rest args)
                                     args))))
           (flet ((f (n fn)
                    (funcall fn n (vector))))
             (f 1
                (lambda (j &rest points)
                  (apply continuation  j (mapcar identity points)))))))
    (() '(1 #()) :test #'equalp)))

(with-test (:name :special-bind-removal)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (multiple-value-prog1 (eval a)
           (let ((* (let ((x (list 1)))
                      (setf (car x) 1)
                      x)))
             (declare (dynamic-extent *)))))
    ((1) 1)))

(with-test (:name :let-setf-aref)
  (checked-compile
   `(lambda ()
      (declare (optimize debug))
      (let ((x (let ((z (make-array 3)))
                 (setf (aref z 0) 10)
                 (setf (aref z 0) 20)
                 z)))
        (declare (dynamic-extent x))
        (aref x 0)))
   :allow-notes nil))

(with-test (:name :dx-multi-use-with-cast)
  (checked-compile '(lambda (bar)
                     (declare (inline make-foo1))
                     (let ((a (if bar
                                  (make-foo1 :x 2)
                                  (cons 1 2))))
                       (declare (dynamic-extent a))
                       (print a)))))

(with-test (:name :dx-propagation-dx-already-exists)
  (checked-compile '(lambda ()
                     (let ((x (let ((m (make-array 3)))
                                (declare (dynamic-extent m))
                                (fill m 0)
                                m)))
                       (declare (dynamic-extent x))
                       (print x)))))

(with-test (:name :nested-var)
  (checked-compile-and-assert
   ()
   `(lambda ()
      (multiple-value-bind (v4 c)
          (values (vector 1) 2)
        (let ((m v4))
          (declare (dynamic-extent m))
          (eval m))
        (values (sb-ext:heap-allocated-p v4) c)))
   (() (values :dynamic 2)))
  (checked-compile-and-assert
   ()
   `(lambda ()
      (flet ((m (&key v4 c)
               (let ((m v4))
                 (declare (dynamic-extent m))
                 (eval m))
               (values v4 c)))
        (let ((m (vector 1)))
          (m :v4 m)
          (sb-ext:heap-allocated-p m))))
   (() :dynamic)))

;;; The point of PRINTing seems to be to force a use of the DX value
;;; and also show that it's not garbage. But assuming it works,
;;; we don't need to actually see the output
(defun print-nothing (x) (print x (make-broadcast-stream)))

(with-test (:name :dx-do-propagate-let-var)
  (checked-compile-and-assert
   ()
   '(lambda (z)
     (let ((x (cons nil (let ((y (list z z z)))
                          (declare (optimize debug))
                          y))))
       (declare (dynamic-extent x))
       (print-nothing x)
       (print-nothing x)
       (assert (sb-ext:stack-allocated-p x))
       (assert (sb-ext:stack-allocated-p (cdr x))))
     nil)
   ((3) NIL)))

(with-test (:name :dx-anonymous-closure)
  (checked-compile-and-assert
   ()
   '(lambda (z)
     (let ((x (lambda () (print-nothing z))))
       (declare (dynamic-extent x))
       (funcall x)
       (funcall x)
       (assert (sb-ext:stack-allocated-p x))
       (funcall x)))
   ((3) 3)))

(with-test (:name :dx-anonymous-closure-otherwise-inaccessible)
  (checked-compile-and-assert
   ()
   '(lambda (x)
     (let ((y (cons (lambda () (print-nothing x))
                    (lambda () (print-nothing x)))))
       (declare (dynamic-extent y))
       (print-nothing y)
       (assert (sb-ext:stack-allocated-p y))
       (assert (sb-ext:stack-allocated-p (car y)))
       (assert (sb-ext:stack-allocated-p (cdr y)))
       (print-nothing y)
       (funcall (car y))))
   ((3) 3)))

(with-test (:name :dx-anonymous-closure-otherwise-inaccessible.flet)
  (checked-compile-and-assert
   ()
   '(lambda (x)
     (let ((y (cons (flet ((g () (print-nothing x)))
                      #'g)
                    (lambda () (print-nothing x)))))
       (declare (dynamic-extent y))
       (print-nothing y)
       (assert (sb-ext:stack-allocated-p y))
       (assert (sb-ext:stack-allocated-p (car y)))
       (assert (sb-ext:stack-allocated-p (cdr y)))
       (print-nothing y)
       (funcall (car y))))
   ((3) 3)))

(defun known-function-autodx (x thing list)
  (subst-if x (lambda (y) (eq y thing)) list))

(with-test (:name :auto-dx-known-functions-too)
  (assert (equal (known-function-autodx 3 4 (list 1 2 3 4)) '(1 2 3 3)))
  (assert-no-consing (known-function-autodx 3 4 nil)))

(defun known-function-autodx-transform (x list)
  (map nil (lambda (y) (+ x y)) list))

(with-test (:name :auto-dx-known-functions-too.transform)
  (assert-no-consing (known-function-autodx-transform 3 '(1 2 3 4))))

(defun known-function-autodx-transform-2 (array x list)
  (map-into array (lambda (y) (+ x y)) list)
  array)

(with-test (:name :auto-dx-known-functions-too.transform-2)
  (let ((array (make-array 4)))
    (declare (dynamic-extent array))
    (assert (equalp (known-function-autodx-transform-2 array 3 '(1 2 3 4)) #(4 5 6 7)))
    #+(or) ; appears to not work on some platforms for some reason?
    (assert-no-consing (known-function-autodx-transform-2 array 3 '(1 2 3 4)))))

(defun auto-dx-cleaned-up-too-many-times (off array)
  (let ((acc 0))
    (flet ((positivep (num) (plusp (+ num off))))
      (dotimes (i 10)
        (incf acc (position-if #'positivep array))))
    acc))

(with-test (:name :auto-dx-cleaned-up-too-many-times)
  (assert (= (auto-dx-cleaned-up-too-many-times 1 #(-1 2 3)) 10)))

(with-test (:name :auto-dx-correct-mess-up)
  (checked-compile-and-assert
   (:allow-notes nil
    :optimize '(:speed 1))
   '(lambda (x y z)
     (map 'list
      (lambda (a b)
        (+ z a b))
      x
      y))
   (('(1 2 3 4) '(1 2 3 4) 1) '(3 5 7 9) :test #'equal)))

(defun dxf (s i)
  (elt s i))
(declaim (notinline dxf))

(with-test (:name :dynamic-extent-lp2031224)
  (let ((sb-c::*check-consistency* t))
    (checked-compile
     '(lambda (a b)
       (let ((v
               (list
                (list (vector 0 0) a
                      (restart-bind nil a))
                (block b2 b))))
         (declare (dynamic-extent v))
         (dxf (dxf v 0) 2))))))

(with-test (:name :dynamic-extent-lp2031399)
  (let ((sb-c::*check-consistency* t))
    (checked-compile
     '(lambda ()
       (declare (notinline funcall not))
       (labels ((%f1 ()
                  (labels ((%f2 (&key (key1
                                       (if (not t)
                                           (ignore-errors 0)
                                           0)))
                             (declare (ignore key1))
                             0))
                    (funcall #'%f2))))
         (%f1))))))

(with-test (:name :dynamic-extent-conditional-allocation.partial)
  (let ((sb-c::*check-consistency* t))
    (checked-compile-and-assert
     ()
     '(lambda (test y z)
       (let ((x (if test
                    (cons (cons y y) z)
                    nil)))
         (declare (dynamic-extent x))
         (when test
           (assert (sb-ext:stack-allocated-p x)))
         (and x
              (* (car (car x)) (cdr x)))))
     ((t 4 5) 20)
     ((nil 4 5) nil))))

(with-test (:name :dynamic-extent-conditional-allocation.partial.2)
  (let ((sb-c::*check-consistency* t))
    (checked-compile-and-assert
     ()
     '(lambda (test y z)
       (let (res)
         (dotimes (i 3)
           (let ((x (if test
                        (cons (cons y y) z)
                        nil)))
             (declare (dynamic-extent x))
             (when test
               (assert (sb-ext:stack-allocated-p x)))
             (setq res (and x
                            (* (car (car x)) (cdr x))))))
         res))
     ((t 4 5) 20)
     ((nil 4 5) nil))))

(with-test (:name :dynamic-extent-mess-up)
  (let ((sb-c::*check-consistency* t))
    (checked-compile-and-assert
     ()
     '(lambda (x)
       (let ((y (list (cons 1 2)
                      (progn
                        (if x
                            (print-nothing 2)
                            (print-nothing 3))
                        (cons 3 4))
                      (progn
                        (if x
                            (print-nothing 2)
                            (print-nothing 3))
                        (cons 3 4)))))
         (declare (dynamic-extent y))
         (assert (sb-ext:stack-allocated-p y))
         (copy-tree y)))
     ((t) '((1 . 2) (3 . 4) (3 . 4)) :test #'tree-equal))))

(with-test (:name :stack-analysis-graph-walk-nlx)
  (let ((sb-c::*check-consistency* t))
    (checked-compile
     '(lambda ()
       (let ((+++ 3))
         (multiple-value-bind (result error)
             (ignore-errors (eval 1))
           (declare (ignore result error)))
         (catch 'foo
           (error "bar"))
         (multiple-value-bind (result error)
             (ignore-errors (eval 1))
           (declare (ignore result error))))))))

(with-test (:name :stack-analysis-graph-walk-nlx.2)
  (let ((sb-c::*check-consistency* t))
    (checked-compile
     '(lambda ()
       (declare (optimize (speed 0) (debug 1)))
       (tagbody
        a
          (flet ((f ()
                   (go tag)))
            (f))
        tag
          (let ((m (list 1)))
            (declare (dynamic-extent m))
            (eval m)
            (let ((m (list 2)))
              (declare (dynamic-extent m))
              (eval m))))))))

(defstruct thing
  (times-v nil :type simple-vector :read-only t)
  (fringe nil))

(with-test (:name :dx-propagation-existing-dynamic-extent)
  (let ((sb-c::*check-consistency* t))
    (checked-compile-and-assert
     ()
     '(lambda ()
       (declare (inline make-thing))
       (let ((times-v (make-array 2))
             (result nil))
         (declare (dynamic-extent times-v))
         (dolist (portion '(1 2 3))
           (let ((scratchpad (make-thing :times-v times-v)))
             (declare (dynamic-extent scratchpad))
             (setq result (nconc result (thing-fringe scratchpad)))))
         result))
     (() nil))))

(with-test (:name :dynamic-extent-setq-let)
  (let ((sb-c::*check-consistency* t))
    (checked-compile-and-assert
     ()
     '(lambda (x)
       (dotimes (i 2)
         (let ((y (cons x x)))
           (declare (dynamic-extent y))
           (assert (sb-ext:stack-allocated-p y))
           (setq y (list x x x))
           (assert (equal y (list x x x)))
           (assert (sb-ext:stack-allocated-p y))
           (setq y (make-array 20 :initial-element nil))
           (assert (equalp y (make-array 20 :initial-element nil)))
           (assert (sb-ext:stack-allocated-p y))
           (setq y (list (cons x i) (cons x i)))
           (assert (tree-equal y (list (cons x i) (cons x i))))
           (assert (sb-ext:stack-allocated-p y))
           (assert (sb-ext:stack-allocated-p (first y) (second y))))))
     ((6) nil))))

(with-test (:name :dynamic-extent-setq-local-calls)
  (let ((sb-c::*check-consistency* t))
    (checked-compile-and-assert
     ()
     '(lambda (x y)
       (labels ((f (a b)
                  (declare (dynamic-extent a b))
                  (setq a (cons x y))
                  (assert (sb-ext:stack-allocated-p a))
                  (setq b (vector x y))
                  (assert (sb-ext:stack-allocated-p b))
                  (assert (equal a (cons x y)))
                  (assert (equalp b (vector x y)))))
         (dotimes (i 4)
           (cond (x
                  (f nil nil)
                  (print-nothing x))
                 (t
                  (f t t)
                  (print-nothing y))))))
     ((nil 2) nil)
     ((t 2) nil))))

(with-test (:name :dynamic-extent-setq-different-environments)
  (let ((sb-c::*check-consistency* t))
    (checked-compile-and-assert
     ()
     '(lambda (test)
       (let ((list nil))
         (declare (dynamic-extent list))
         (flet ((body ()
                  (setq list (cons nil list))))
           (if test
               (progn
                 (body)
                 (print-nothing 'foo))
               (body)))))
     ((t) 'foo)
     ((nil) '(nil)))))

(with-test (:name :dynamic-extent-nested)
  (let ((sb-c::*check-consistency* t))
    (checked-compile-and-assert
     ()
     '(lambda (a)
       (let ((v (list (vector 0 0)
                      (let ((x (cons 1 2)))
                        (declare (dynamic-extent x))
                        (print-nothing x)
                        (list 1 2)))))
         (declare (dynamic-extent v))
         (copy-list (elt v a))))
     ((1) '(1 2)))))

(with-test (:name :dynamic-extent-setq-nested)
  (let ((sb-c::*check-consistency* t))
    (checked-compile-and-assert
     ()
     '(lambda ()
       (dotimes (i 2)
         (let ((y (cons 1 2)))
           (declare (dynamic-extent y))
           (assert (equal y (cons 1 2)))
           (assert (sb-ext:stack-allocated-p y))
           (let ((z (cons 3 4)))
             (declare (dynamic-extent z))
             (assert (equal z (cons 3 4)))
             (assert (sb-ext:stack-allocated-p z))
             (setq y (cons 2 1))
             (assert (equal y (cons 2 1)))
             (assert (sb-ext:stack-allocated-p y)))
           (let ((z (list 9 9 9 9 9)))
             (declare (dynamic-extent z))
             (assert (equal z (list 9 9 9 9 9)))
             (assert (sb-ext:stack-allocated-p z))
             (assert (equal y (cons 2 1)))
             (assert (sb-ext:stack-allocated-p y))))))
     (() nil))))

(with-test (:name :dynamic-extent-start-later)
  (let ((sb-c::*check-consistency* t))
    (checked-compile-and-assert
     ()
     '(lambda (x)
       (let ((bar))
         (declare (dynamic-extent bar))
         (print-nothing (if x 1 2))
         (push 12 bar)
         (assert (sb-ext:stack-allocated-p bar))
         (assert (equal bar '(12)))
         nil))
     ((t) nil)
     ((nil) nil))))

(with-test (:name :dynamic-extent-preserve.unreferenced-tn)
  (let ((sb-c::*check-consistency* t))
    (checked-compile-and-assert
     ()
     '(lambda (b)
       (let ((v nil))
         (declare (dynamic-extent v))
         (case b
           (0 (setq v (vector 3)))
           (1 (setq v (vector 0 (let ((x (cons 1 2)))
                                  (declare (dynamic-extent x))
                                  (print-nothing x)
                                  2)))))
         (elt v 0)))
     ((0) 3)
     ((1) 0))))

(with-test (:name :dynamic-extent-preserve.heap-exhaustion)
  (let ((sb-c::*check-consistency* t))
    (checked-compile-and-assert
     ()
     '(lambda (a b c)
       (let ((v nil))
         (declare (dynamic-extent v))
         (case a
           (0 (setq v (list (prog2 b (restart-bind nil -17607) c) 17632849286127)))
           (1 (setq v (list -17532189700714087747 b c))))
         (case a (0 (elt v 1)) (1 (elt v 2)))))
     ((1 2 3) 3))))

(with-test (:name :dynamic-extent-setq-already-existing)
  (let ((sb-c::*check-consistency* t))
    (checked-compile-and-assert
     ()
     '(lambda (a)
       (let ((x (eval a)))
         (declare (dynamic-extent x))
         (let ((y 0))
           (declare (dynamic-extent y))
           (let ((z y))
             (setq y x)
             (values z)))))
     ((1) 0))))

(with-test (:name :stack-allocated-vector-checks-overflow
            :broken-on (not (and :x86-64 :linux)))
  (checked-compile-and-assert
   (:optimize :safe)
   '(lambda ()
     (let ((x (make-array
               ;; guaranteed to overflow the stack.
               (abs (- (sb-sys:sap-int
                        (sb-vm::current-thread-offset-sap sb-vm::thread-control-stack-start-slot))
                       (sb-sys:sap-int
                        (sb-int:descriptor-sap sb-vm:*control-stack-end*)))))))
       (declare (dynamic-extent x))
       (dotimes (i (length x))
         (setf (aref x i) i))
       123))
   ;; This condition will be different depending on whether the
   ;; explicit stack check signals or the guard page gets hit.
   (() (condition 'sb-kernel::storage-condition))))

(with-test (:name :stack-allocated-vector-integer-size-arg)
  (checked-compile-and-assert
   (:optimize :safe)
   '(lambda ()
      (let ((buffer (make-array (* 128 1024) :element-type '(unsigned-byte 8))))
        (declare (dynamic-extent buffer))
        (dotimes (i (length buffer))
          (setf (aref buffer i) (mod i 32)))
        (print-nothing (aref buffer 0))
        nil))
   (() nil)))

(with-test (:name :stack-analysis-preserve.setq-loop)
  (let ((sb-c::*check-consistency* t))
    (checked-compile-and-assert
     ()
     '(lambda ()
       (let ((digits '()))
         (declare (dynamic-extent digits))
         (let ((result (make-array 128 :element-type 'base-char)))
           (declare (dynamic-extent result))
           (dotimes (i 2)
             (setq digits (cons 0 digits)))
           (dotimes (i 3)
             (setf (aref result i) #\0)))
         ;; can't actually deallocate RESULT yet because we have to keep
         ;; DIGITS on the stack.
         (let ((another-result (make-array 128 :element-type 'base-char))
               (another (list 1 2 3 4 5 6 7 8 9 10)))
           (declare (dynamic-extent another another-result))
           (dotimes (i 3)
             (setf (aref another-result i) #\0))
           (print-nothing (car another)))
         (car digits)))
     (() 0))))

(with-test (:name :encode-error-break-large-immediate)
  (disassemble '(lambda ()
                 (sb-int:dx-let ((v (make-array 65536
                                                :element-type '(unsigned-byte 8))))
                  (opaque-identity v)))
               :stream (make-broadcast-stream)))

(with-test (:name :make-list-large-immediate)
  (checked-compile
   '(lambda ()
     (let ((x (make-list (1- (ash most-positive-fixnum (- (+ sb-vm:word-shift 1)))))))
       (declare (dynamic-extent x))
       (car x))))
  (checked-compile
   '(lambda ()
     (let ((x (make-list 100000 :initial-element 1)))
       (declare (dynamic-extent x))
       (reduce #'+ x)))))
