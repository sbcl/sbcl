;;;; This file is for compiler tests which are not about correctness
;;;; of the compiler, but are "nice to have" features in a robust
;;;; implementation of a compiler, such as detection of various style
;;;; issues, with the nuanced meaning that it is SBCL's notion of poor style,
;;;; such as things that don't emit as efficient code as possible
;;;; because of <blah>.

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

;; These tests don't work unless compiling
#+interpreter (sb-ext:exit :code 104)

(defun f-with-macro (arg) (list arg))
(defun f2-with-macro (a b) (list a b))
(defun map-f-with-macro (l) (mapcar #'f-with-macro l))
(defun use-f2-with-macro ()
  (list (f2-with-macro 1 2) (f2-with-macro 3 4)))

(locally
    ;; ignore that we don't know what F is
    (declare (muffle-conditions style-warning))
  (defun just-call-f (x) (declare (notinline f)) (f x)))

(defun g-with-macro (arg) (list arg))
(defun map-g-with-macro (l)
  (declare (notinline g-with-macro))
  (mapcar #'g-with-macro l))

(declaim (notinline h-with-macro))
(defun h-with-macro (arg) (list arg))
(defun map-h-with-macro (l) (mapcar #'h-with-macro l))

(test-util:with-test (:name :compiler-macro-order-bug)
  ;; There is one explicit NOTINLINE, but we still get a warning.
  (assert-signal
   (define-compiler-macro f-with-macro (arg) `(list ,arg))
   sb-c:compiler-macro-application-missed-warning)
  ;; To exercise both cases of the ~:P directive in the warning message.
  (assert-signal
   (define-compiler-macro f2-with-macro (a b) `(list ,a ,b))
   sb-c:compiler-macro-application-missed-warning)

  ;; There is a local notinline decl, so no warning about a compiler-macro.
  (assert-no-signal
    (define-compiler-macro g-with-macro (arg) `(list ,arg)))
  ;; There is a global notinline proclamation.
  (assert-no-signal
    (define-compiler-macro h-with-macro (arg) `(list ,arg))))

(defun g (x) (1- x))
(defun h (x) (1+ x))
(defun use-g (x) (g x))
(defun use-h (x) (list (h x) (h x)))
(with-test (:name :inline-failure-1)
  (assert-signal (declaim (inline g h))
                 sb-c:inlining-dependency-failure 2))

(declaim (inline fast-guy))
(with-test (:name :inline-failure-2a)
  (assert-signal (compile nil '(lambda (x) (fast-guy x)))
                 sb-c:inlining-dependency-failure))

(defun zippy (y) y)
(with-test (:name :inline-failure-2b)
  (assert-signal
   (eval '(defun baz (arg) (declare (inline zippy)) (zippy arg)))
   sb-c:inlining-dependency-failure))

(locally (declare (muffle-conditions style-warning))
  (defun foofy1 (x) (and (somestruct-p x) 'hi)))

(test-util:with-test (:name :structure-pred-inline-failure)
 (assert-signal (defstruct somestruct a b)
                sb-c:inlining-dependency-failure))

(test-util:with-test (:name :redef-macro-same-file)
  (let* ((lisp (scratch-file-name "lisp"))
         (fasl (compile-file-pathname lisp)))
    (unwind-protect
         (let ((redef-count 0))
           (with-open-file (f lisp :direction :output)
             (dolist (form '((defmacro glork (x) `(car ,x))
                             (define-compiler-macro glorpy (x) `(+ ,x 1))
                             (defmacro glork (x) `(first ,x))
                             (define-compiler-macro glorpy (x) `(+ ,x 2))))
               (print form f)))
           (multiple-value-bind (fasl warn fail)
               (handler-bind ((sb-int:same-file-redefinition-warning
                               (lambda (c) c (incf redef-count))))
                 (let ((*error-output* (make-broadcast-stream)))
                   (compile-file lisp :print nil :verbose nil)))
             (declare (ignore fasl))
             (assert (and warn (not fail) (= redef-count 2)))))
      (ignore-errors (delete-file lisp))
      (ignore-errors (delete-file fasl)))))
