;;;; tests of the fop compiler

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

(in-package "CL-USER")

;; Can't use normal ASSERT, since it is not fopcompilable...
(defun assert* (value)
  (unless value
    (error "assert failed")))

;;; Test that the forms that are supposed to be fopcompilable are, and
;;; the ones that aren't aren't. The body might contain further tests to
;;; ensure that the fopcompiled code works as intended.
(defmacro fopcompile-test (fopcompilable-p &body body)
  (assert (eql (sb-c::fopcompilable-p `(progn ,@body))
               fopcompilable-p))
  `(progn ,@body))

(fopcompile-test t
 (let ((a 1))
   (assert* (eql a 1))))

(fopcompile-test t
 (let ((a 3))
   (let ((a 4))
     (assert* (eql a 4)))))

(fopcompile-test t
 (let* ((a 5))
   (let* ((a 6))
     (assert* (eql a 6)))))

(fopcompile-test nil
 (let ((a 7))
   (assert* (eql (funcall (lambda () a)) 7))))

(fopcompile-test nil
  (let* ((a 8))
    (assert* (eql (funcall (lambda () a)) 8))))

(fopcompile-test t
  (let ((a 8)
        (b (lambda () 1)))
    nil))

(fopcompile-test t
  (let* ((a (lambda () 1)))
    nil))

(fopcompile-test nil
  (let* ((a 8)
         (b (lambda () 1)))
    nil))

(fopcompile-test nil
  (let* ((a 9)
         (b (funcall (lambda () a))))
    (assert* (eql b 9))))

(fopcompile-test t
  (let ((a 10))
    (let ((a 11)
          (b a))
      (assert* (eql b 10)))))

(fopcompile-test t
  (let ((a 12))
    (let* ((a 13)
           (b a))
      (assert* (eql b 13)))))

(setf (symbol-value 'fopcompile-test-foo) 1)
(assert* (eql fopcompile-test-foo 1))

;;; Ensure that we're passing sensible environments to macros during
;;; fopcompilation. Reported by Samium Gromoff.

(defmacro bar (vars &environment env)
  (assert (equal vars
                 (mapcar #'car (sb-c::lexenv-vars env)))))

(symbol-macrolet ((foo 1))
  (let* ((x (bar (foo)))
         (y (bar (x foo))))
    (bar (y x foo))))

;;; Some tests involving compiler-macros.

(defvar *cmacro-result* nil)

(defun baz (x) (declare (ignore x)))

;; functional foo - a function with a compiler-macro
(defun ffoo (x) (push `(regular-ffoo ,x) *cmacro-result*))
(define-compiler-macro ffoo (x)
  `(push `(cmacro-ffoo ,,x) *cmacro-result*))

;; macro foo - a macro with a compiler-macro
(defmacro mfoo (x) `(push `(regular-mfoo ,,x) *cmacro-result*))
(define-compiler-macro mfoo (x)
  `(push `(cmacro-mfoo ,,x) *cmacro-result*))

(defun get-s () (declare (special s)) s)

;; Verify some assumptions that the tests will test what was intended.
(eval-when (:compile-toplevel)
  (let ((sb-c::*lexenv* (sb-kernel:make-null-lexenv)))
    (assert (sb-c::fopcompilable-p '(baz (ffoo 3))))
    (assert (sb-c::fopcompilable-p '(baz (mfoo 3))))
    ;; The special binding of S makes these forms not fopcompilable.
    (assert (not (sb-c::fopcompilable-p
                  '(ffoo (let ((s 3)) (declare (special s)) (get-s))))))
    (assert (not (sb-c::fopcompilable-p
                  '(mfoo (let ((s 3)) (declare (special s)) (get-s))))))))

;; fopcompilable toplevel form should execute the compiler macro
(ffoo 1)
(mfoo 1)
;; fopcompilable form expands embedded compiler-macro
(baz (ffoo 2))
(baz (mfoo 2))
;; not-fopcompilable toplevel form should execute the compiler macro.
;; This was ok if the toplevel call was a function with a compiler-macro,
;; but was not working for a toplevel macro having a compiler-macro.
(ffoo (let ((s 3)) (declare (special s)) (get-s)))
(mfoo (let ((s 3)) (declare (special s)) (get-s)))

(with-test (:name :compiler-macros-at-toplevel)
  ;; Now assert about the macroexpansions that happened.
  (assert (equal *cmacro-result*
                 '((CMACRO-MFOO 3) (CMACRO-FFOO 3)
                   (CMACRO-MFOO 2) (CMACRO-FFOO 2)
                   (CMACRO-MFOO 1) (CMACRO-FFOO 1)))))

(when (eval nil)
  (lambda () #.(find-package "CL")))

(with-test (:name :skip-load-form)
  (assert (eq #.(find-package "CL")
              (eval '(find-package "CL")))))
