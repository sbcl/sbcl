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

(setq sb-c::*check-consistency* t)

(defmacro defun-with-dx (name arglist &body body)
  `(locally
     (declare (optimize sb-c::stack-allocate-dynamic-extent))
     (defun ,name ,arglist
       ,@body)))

(declaim (notinline opaque-identity))
(defun opaque-identity (x)
  x)

;;; &REST lists
(defun-with-dx dxlength (&rest rest)
  (declare (dynamic-extent rest))
  (length rest))

(assert (= (dxlength 1 2 3) 3))
(assert (= (dxlength t t t t t t) 6))
(assert (= (dxlength) 0))

(defun callee (list)
  (destructuring-bind (a b c d e f &rest g) list
    (+ a b c d e f (length g))))

(defun-with-dx dxcaller (&rest rest)
  (declare (dynamic-extent rest))
  (callee rest))
(assert (= (dxcaller 1 2 3 4 5 6 7) 22))

(defun-with-dx dxcaller-align-1 (x &rest rest)
  (declare (dynamic-extent rest))
  (+ x (callee rest)))
(assert (= (dxcaller-align-1 17 1 2 3 4 5 6 7) 39))
(assert (= (dxcaller-align-1 17 1 2 3 4 5 6 7 8) 40))

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

(assert (equal (multiple-value-list (test-nip-values)) '(1 5 a)))

;;; LET-variable substitution
(defun-with-dx test-let-var-subst1 (x)
  (let ((y (list x (1- x))))
    (opaque-identity :foo)
    (let ((z (the list y)))
      (declare (dynamic-extent z))
      (length z))))
(assert (eql (test-let-var-subst1 17) 2))

(defun-with-dx test-let-var-subst2 (x)
  (let ((y (list x (1- x))))
    (declare (dynamic-extent y))
    (opaque-identity :foo)
    (let ((z (the list y)))
      (length z))))
(assert (eql (test-let-var-subst2 17) 2))

;;; DX propagation through LET-return.
(defun-with-dx test-lvar-subst (x)
  (let ((y (list x (1- x))))
    (declare (dynamic-extent y))
    (second (let ((z (the list y)))
              (opaque-identity :foo)
              z))))
(assert (eql (test-lvar-subst 11) 10))

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
(dotimes (n 64)
  (let* ((res (loop for i below n collect i))
         (form `(values ,@res)))
    (assert (equal (multiple-value-list (test-alignment-dx-list form)) res))
    (assert (equal *x* '(1 2 3 4)))))

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

(assert (eq t (dxclosure 13)))


(defmacro assert-no-consing (form &optional times)
  `(%assert-no-consing (lambda () ,form) ,times))
(defun %assert-no-consing (thunk &optional times)
  (let ((before (get-bytes-consed))
        (times (or times 10000)))
    (declare (type (integer 1 *) times))
    (dotimes (i times)
      (funcall thunk))
    (assert (< (- (get-bytes-consed) before) times))))

#+(or x86 x86-64 alpha ppc sparc mips)
(progn
  (assert-no-consing (dxclosure 42))
  (assert-no-consing (dxlength 1 2 3))
  (assert-no-consing (dxlength t t t t t t))
  (assert-no-consing (dxlength))
  (assert-no-consing (dxcaller 1 2 3 4 5 6 7))
  (assert-no-consing (test-nip-values))
  (assert-no-consing (test-let-var-subst1 17))
  (assert-no-consing (test-let-var-subst2 17))
  (assert-no-consing (test-lvar-subst 11)))


;;; Bugs found by Paul F. Dietz
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
  'x))


;;; other bugs

;;; bug reported by Svein Ove Aas
(defun svein-2005-ii-07 (x y)
  (declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (let ((args (list* y 1 2 x)))
    (declare (dynamic-extent args))
    (apply #'aref args)))
(assert (eql
         (svein-2005-ii-07
          '(0)
          #3A(((1 1 1) (1 1 1) (1 1 1))
              ((1 1 1) (1 1 1) (4 1 1))
              ((1 1 1) (1 1 1) (1 1 1))))
         4))

;;; bug reported by Brian Downing: stack-allocated arrays were not
;;; filled with zeroes.
(defun-with-dx bdowning-2005-iv-16 ()
  (let ((a (make-array 11 :initial-element 0)))
    (declare (dynamic-extent a))
    (assert (every (lambda (x) (eql x 0)) a))))
(bdowning-2005-iv-16)


