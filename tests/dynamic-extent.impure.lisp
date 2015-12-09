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
  (sb-ext:exit :code 104))

(load "compiler-test-util.lisp")
(use-package :ctu)

(setq sb-c::*check-consistency* t
      sb-ext:*stack-allocate-dynamic-extent* t)

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
    (declare (sb-int:truly-dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-3 (x y z)
  (let ((v (make-array 3
                       :element-type 'fixnum :initial-contents (list x y z)
                       :element-type t :initial-contents x)))
    (declare (sb-int:truly-dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-4 ()
  (let ((v (make-array 3 :initial-contents '(1 2 3))))
    (declare (sb-int:truly-dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-5 ()
  (let ((v (make-array 3 :initial-element 12 :element-type t)))
    (declare (sb-int:truly-dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-6 ()
  (let ((v (make-array 3 :initial-element 12 :element-type '(unsigned-byte 8))))
    (declare (sb-int:truly-dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-7 ()
  (let ((v (make-array 3 :initial-element 12 :element-type '(signed-byte 8))))
    (declare (sb-int:truly-dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-8 ()
  (let ((v (make-array 3 :initial-element 12 :element-type 'word)))
    (declare (sb-int:truly-dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-9 ()
  (let ((v (make-array 3 :initial-element 12.0 :element-type 'single-float)))
    (declare (sb-int:truly-dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-10 ()
  (let ((v (make-array 3 :initial-element 12.0d0 :element-type 'double-float)))
    (declare (sb-int:truly-dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx make-array-on-stack-11 ()
  (let ((v (make-array (the integer (opaque-identity 3)) :initial-element 12.0d0 :element-type 'double-float)))
    (declare (sb-int:truly-dynamic-extent v))
    (true v)
    (true v)
    nil))

(defun-with-dx vector-on-stack (x y)
  (let ((v (vector 1 x 2 y 3)))
    (declare (sb-int:truly-dynamic-extent v))
    (true v)
    nil))

;;; MAKE-LIST

(declaim (inline make-list-container))
(defstruct list-container listy-slot)
(defun make-var-length-dx-list (n thunk)
  (sb-int:dx-let ((s (make-list-container :listy-slot (make-list n))))
    (funcall thunk s)))
;; :stack-allocatable-lists is necessary but not sufficient
(with-test (:name (:dx-list :make-list) :skipped-on '(not :x86-64))
  (assert (null (ctu:find-named-callees #'make-var-length-dx-list)))
  (assert-no-consing (make-var-length-dx-list
                      50 (lambda (x) (declare (ignore x))))))

;;; MAKE-STRUCTURE

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
  (assert (null (sb-int:info :function :inline-expansion-designator s))))

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

;;; multiple uses for dx lvar

(defun-with-dx multiple-dx-uses ()
  (let ((dx (if (true t)
                (list 1 2 3)
                (list 2 3 4))))
    (declare (dynamic-extent dx))
    (true dx)
    nil))

;;; mapfoo should make the initial cons dx

(defun mapcar-negate (x) (mapcar #'- x))
(defun mapcan-reverse (x) (mapcan #'reverse x))

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

(defun list-delete-some-stuff ()
  ;; opaque-identity hides the fact that we are calling a destructive function
  ;; on a constant, which is technically illegal. But no deletion occurs,
  ;; so it's innocuous. Also these aren't tests of DX, but oh well...
  (declare (muffle-conditions style-warning))
  (delete 'a (opaque-identity '(x y)))
  (delete 'a (opaque-identity '(x y)) :from-end t)
  (delete-duplicates (opaque-identity '(x y))))

(defvar *a-cons* (cons nil nil))

(with-test (:name (:no-consing :dx-closures) :skipped-on '(not :stack-allocatable-closures))
  (assert-no-consing (dxclosure 42)))

(with-test (:name (:no-consing :dx-lists)
            :skipped-on '(not (and :stack-allocatable-lists
                               :stack-allocatable-fixed-objects)))
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
  (assert-no-consing (mapcar-negate nil))
  (assert-no-consing (mapcan-reverse nil))
  (assert-no-consing (list-delete-some-stuff))
  (assert-no-consing (multiple-dx-uses)))

(with-test (:name (:no-consing :dx-value-cell)
                  :skipped-on '(not :stack-allocatable-closures))
  (assert-no-consing (dx-value-cell 13)))

(with-test (:name (:no-consing :dx-fixed-objects)
                  :skipped-on '(not (and :stack-allocatable-fixed-objects
                                         :stack-allocatable-closures)))
  (assert-no-consing (cons-on-stack 42))
  (assert-no-consing (make-foo1-on-stack 123))
  (assert-no-consing (nested-good 42))
  (assert-no-consing (nested-dx-conses))
  (assert-no-consing (dx-handler-bind 2))
  (assert-no-consing (dx-handler-case 2)))

(with-test (:name (:no-consing :dx-vectors) :skipped-on '(not :stack-allocatable-vectors))
  (assert-no-consing (force-make-array-on-stack 128))
  (assert-no-consing (make-array-on-stack-2 5 '(1 2.0 3 4.0 5)))
  (assert-no-consing (make-array-on-stack-3 9 8 7))
  (assert-no-consing (make-array-on-stack-4))
  (assert-no-consing (make-array-on-stack-5))
  (assert-no-consing (vector-on-stack :x :y)))

(with-test (:name (:no-consing :specialized-dx-vectors)
            :skipped-on `(not (and :stack-allocatable-vectors
                                   :c-stack-is-control-stack)))
  (assert-no-consing (make-array-on-stack-1))
  (assert-no-consing (make-array-on-stack-6))
  (assert-no-consing (make-array-on-stack-7))
  (assert-no-consing (make-array-on-stack-8))
  (assert-no-consing (make-array-on-stack-9))
  (assert-no-consing (make-array-on-stack-10))
  (assert-no-consing (make-array-on-stack-11)))

(with-test (:name (:no-consing :dx-raw-instances) :skipped-on '(or (not :raw-instance-init-vops)
                                                                   (not (and :gencgc :c-stack-is-control-stack))))
  (let (a b)
    (setf a 1.24 b 1.23d0)
    (assert-no-consing (make-foo2-on-stack a b)))
    (assert-no-consing (make-foo3-on-stack)))

;;; not really DX, but GETHASH and (SETF GETHASH) should not cons

(defvar *table* (make-hash-table))

(defun test-hash-table ()
  (setf (gethash 5 *table*) 13)
  (gethash 5 *table*))

(with-test (:name (:no-consing :hash-tables))
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

(with-test (:name (:no-consing :mutex) :skipped-on '(not :sb-thread))
  (assert-no-consing (test-mutex)))


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
          v5
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
  (compile nil '(lambda ()
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

(with-test (:name :handler-case-bogus-compiler-note
            :skipped-on '(not (and :stack-allocatable-fixed-objects
                                   :stack-allocatable-closures)))
  (handler-bind
      ((compiler-note (lambda (note)
                        (error "compiler issued note ~S during test" note))))
    ;; Taken from SWANK, used to signal a bogus stack allocation
    ;; failure note.
    (compile nil
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
(with-test (:name :dx-compiler-notes
            :skipped-on '(not (and :stack-allocatable-vectors
                                   :stack-allocatable-closures)))
  (flet ((assert-notes (j lambda)
           (let ((n 0))
             (handler-bind ((compiler-note (lambda (c)
                                             (declare (ignore c))
                                             (incf n))))
               (let ((*error-output* (make-broadcast-stream)))
                 (compile nil lambda))
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
(with-test (:name :handler-case-eating-stack
            :skipped-on '(not (and :stack-allocatable-fixed-objects
                                   :stack-allocatable-closures)))
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
(with-test (:name :recheck-nested-dx-bug
            :skipped-on '(not :stack-allocatable-vectors))
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
               (let ((*error-output* (make-broadcast-stream)))
                 (compile nil lambda)))
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

(with-test (:name :bug-586105
            :skipped-on '(not (and :stack-allocatable-vectors
                               :stack-allocatable-lists)))
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
  (let ((f1 (compile nil `(lambda (f &rest args)
                            (apply f args)))))
    (assert-no-consing (assert (eql f1 (funcall f1 #'identity f1)))))
  (let ((f2 (compile nil `(lambda (f1 f2 &rest args)
                            (values (apply f1 args) (apply f2 args))))))
    (assert-no-consing (multiple-value-bind (a b)
                           (funcall f2 (lambda (x y z) (+ x y z)) (lambda (x y z) (- x y z))
                                    1 2 3)
                         (assert (and (eql 6 a) (eql -4 b))))))
  (let ((f3 (compile nil `(lambda (f &optional x &rest args)
                            (when x
                              (apply f x args))))))
    (assert-no-consing (assert (eql 42 (funcall f3
                                                (lambda (a b c) (+ a b c))
                                                11
                                                10
                                                21)))))
  (let ((f4
          (let ((*error-output* (make-broadcast-stream)))
            (compile nil `(lambda (f &optional x &rest args
                                     &key y &allow-other-keys)
                            (apply f y x args))))))
    (assert-no-consing (funcall f4 (lambda (y x yk y2 b c)
                                     (assert (eq y 'y))
                                     (assert (= x 2))
                                     (assert (eq :y yk))
                                     (assert (eq y2 'y))
                                     (assert (eq b 'b))
                                     (assert (eq c 'c)))
                                2 :y 'y 'b 'c)))
  (let ((f5 (compile nil `(lambda (a b c &rest args)
                            (apply #'list* a b c args)))))
    (assert (equal '(1 2 3 4 5 6 7) (funcall f5 1 2 3 4 5 6 '(7)))))
  (let ((f6 (compile nil `(lambda (x y)
                            (declare (optimize speed))
                            (concatenate 'string x y)))))
    (assert (equal "foobar" (funcall f6 "foo" "bar"))))
  (let ((f7 (compile nil `(lambda (&rest args)
                            (lambda (f)
                              (apply f args))))))
    (assert (equal '(a b c d e f) (funcall (funcall f7 'a 'b 'c 'd 'e 'f) 'list))))
  (let ((f8 (compile nil `(lambda (&rest args)
                            (flet ((foo (f)
                                     (apply f args)))
                              #'foo)))))
    (assert (equal '(a b c d e f) (funcall (funcall f8 'a 'b 'c 'd 'e 'f) 'list))))
  (let ((f9 (compile nil `(lambda (f &rest args)
                            (flet ((foo (g)
                                     (apply g args)))
                              (declare (dynamic-extent #'foo))
                              (funcall f #'foo))))))
    (assert (equal '(a b c d e f)
                   (funcall f9 (lambda (f) (funcall f 'list)) 'a 'b 'c 'd 'e 'f))))
  (let ((f10 (compile nil `(lambda (f &rest args)
                            (flet ((foo (g)
                                     (apply g args)))
                              (funcall f #'foo))))))
    (assert (equal '(a b c d e f)
                   (funcall f10 (lambda (f) (funcall f 'list)) 'a 'b 'c 'd 'e 'f))))
  (let ((f11 (compile nil `(lambda (x y z)
                             (block out
                               (labels ((foo (x &rest rest)
                                          (apply (lambda (&rest rest2)
                                                   (return-from out (values-list rest2)))
                                                 x rest)))
                                (if x
                                    (foo x y z)
                                    (foo y z x))))))))
    (multiple-value-bind (a b c) (funcall f11 1 2 3)
      (assert (eql a 1))
      (assert (eql b 2))
      (assert (eql c 3)))))

(defun opaque-funcall (function &rest arguments)
  (apply function arguments))

(with-test (:name :implicit-value-cells)
  (flet ((test-it (type input output)
           (let ((f (compile nil `(lambda (x)
                                    (declare (type ,type x))
                                    (flet ((inc ()
                                             (incf x)))
                                      (declare (dynamic-extent #'inc))
                                      (list (opaque-funcall #'inc) x))))))
             (assert (equal (funcall f input)
                            (list output output))))))
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
  (let ((f (compile nil `(lambda (x)
                           (declare (type system-area-pointer x))
                           (flet ((inc ()
                                    (setf x (sb-sys:sap+ x 16))))
                             (declare (dynamic-extent #'inc))
                             (list (opaque-funcall #'inc) x)))))
        (width sb-vm:n-machine-word-bits))
    (assert (every (lambda (x)
                     (sb-sys:sap= x (sb-sys:int-sap (+ 16 (ash 1 (1- width))))))
                   (funcall f (sb-sys:int-sap (ash 1 (1- width))))))))

(with-test (:name :&more-bounds)
  ;; lp#1154946
  (assert (not (funcall (compile nil '(lambda (&rest args) (car args))))))
  (assert (not (funcall (compile nil '(lambda (&rest args) (nth 6 args))))))
  (assert (not (funcall (compile nil '(lambda (&rest args) (elt args 10))))))
  (assert (not (funcall (compile nil '(lambda (&rest args) (cadr args))))))
  (assert (not (funcall (compile nil '(lambda (&rest args) (third args)))))))
