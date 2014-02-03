;;;; tests related to setf

;;;; This file is impure because we want to be able to use DEFUN.

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

(in-package :cl-user)

(defvar *foo* nil)
(defun (setf foo) (bar)
    (setf *foo* bar))

;;; Regression test for get-setf-expansion without explicit
;;; environment object.
(assert (multiple-value-list (get-setf-expansion '(foo))))

;;; Regression test for SHIFTF of values.
(let ((x (list 1))
      (y (list 2)))
  (shiftf (values (car x) (car y)) (values (car y) (car x)))
  (assert (equal (list x y) '((2) (1)))))

;;; SETF of values with multiple-value place forms
(let ((a t) (b t) (c t) (d t))
  (let ((list (multiple-value-list
               (setf (values (values a b) (values c d)) (values 1 2 3 4)))))
    (assert (equal list '(1 2)))
    (assert (eql a 1))
    (assert (eql c 2))
    (assert (null b))
    (assert (null d))))

;;; SETF of THE with VALUES.
(let (x y)
  (setf (the (values fixnum fixnum) (values x y))
        (values 1 2))
  (assert (= x 1))
  (assert (= y 2)))

;;; SETF of MACRO-FUNCTION must accept a NIL environment
(let ((fun (constantly 'ok)))
  (setf (macro-function 'nothing-at-all nil) fun)
  (assert (eq fun (macro-function 'nothing-at-all nil))))


;;; DEFSETF accepts &ENVIRONMENT but not &AUX
(defsetf test-defsetf-env-1  (&environment env) (new)
  (declare (ignore new))
  (if (macro-function 'defsetf-env-trick env)
      :local
      :global))

(defsetf test-defsetf-env-2  (local global &environment env) (new)
  (declare (ignore new))
  (if (macro-function 'defsetf-env-trick env)
      local
      global))

(assert (eq :local (macrolet ((defsetf-env-trick ()))
                     (setf (test-defsetf-env-1) 13))))

(assert (eq :global (setf (test-defsetf-env-1) 13)))

(assert (eq :local (macrolet ((defsetf-env-trick ()))
                     (setf (test-defsetf-env-2 :local :oops) 13))))

(assert (eq :global (setf (test-defsetf-env-2 :oops :global) 13)))

(assert (eq :error
            (handler-case
                (eval '(defsetf test-defsetf-aux (&aux aux) (new) nil))
              (error ()
                :error))))

(handler-bind ((style-warning #'error))
  (compile nil '(lambda ()
                 (defsetf test-defsetf-no-env (foo) (new)
                   `(set-foo ,foo ,new))))
  (compile nil '(lambda ()
                 (defsetf test-defsetf-ignore-env (foo &environment env) (new)
                   (declare (ignore env))
                   `(set-foo ,foo ,new)))))

;;; Not required by the spec, but allowes compiler-macros for SETF-functiosn
;;; to see their constant argument forms.
(with-test (:name :constantp-aware-get-setf-expansion)
  (multiple-value-bind (temps values stores set get)
      (get-setf-expansion '(foo 1 2 3))
    (assert (not temps))
    (assert (not values))
    (assert (equal `(funcall #'(setf foo) ,@stores 1 2 3) set))
    (assert (equal '(foo 1 2 3) get))))

(with-test (:name :update-fn-should-be-a-symbol-in-defsetf)
  (assert (eq :error
            (handler-case
                (eval '(defsetf access-fn 5))
              (error ()
                :error)))))

(with-test (:name :getf-unused-default-variable)
  (handler-bind ((style-warning #'error))
    (compile nil `(lambda (x y)
                    (setf (gethash :x x 0) 4)
                    (setf (getf y :y 0) 4)
                    (setf (get 'z :z 0) 4)))))

(with-test (:name :setf-fun-and-macro-full-warn)
  (multiple-value-bind (fun warnings-p failure-p)
      (compile nil '(lambda (x) (setf (shoe-color x) 'cordovan)))
    (assert (and fun warnings-p (not failure-p))))
  (assert (typep (handler-case (eval '(defsetf shoe-color set-shoe-color))
                   (warning (c) c))
                 '(and warning (not style-warning)))))

(with-test (:name :setf-fun-and-macro-style-1)
  (eval '(defun (setf shoe-size) (new x) x new))
  (assert (typep (handler-case (eval '(defsetf shoe-size set-shoe-size))
                   (warning (c) c))
                 'style-warning)))

;; This is a test of the compiler, but it belongs with the above.
(defvar *tmpfile* "setf-tmp.lisp")
(with-test (:name :setf-fun-and-macro-style-2)
  (unwind-protect
       (progn
         ;; verify the test's precondition, for sanity
         (assert (not (fboundp '(setf shoe-count))))
         (with-open-file (f *tmpfile* :direction :output
                                      :if-exists :supersede)
           (prin1 '(defun (setf shoe-count) (new x) (print x) new) f)
           (prin1 '(defsetf shoe-count set-shoe-count) f))
         ;; Expect a warning because the compiler knows about
         ;; (SETF SHOE-COUNT), which isn't yet FBOUNDP,
         ;; and then we also compile a SETF inverse.
         (multiple-value-bind (output warnings-p failure-p)
             (compile-file *tmpfile*)
           (ignore-errors (delete-file output))
           (assert (and output warnings-p (not failure-p)))))
      (ignore-errors (delete-file *tmpfile*))))

;;; success
