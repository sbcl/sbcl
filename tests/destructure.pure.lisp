;;;; tests of DESTRUCTURING-BIND-ish functionality

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

;;; In sbcl-1.0.7.8, the printer for the ERROR condition signalled from
;;;   (DESTRUCTURING-BIND (...) 1 ...)
;;; contained the implicit assumption that the bad datum was a list,
;;; so that attempting to print the condition caused a new error.
(with-test (:name (destructuring-bind :dotted-list error :printable 1))
  (let ((error (nth-value 1 (ignore-errors
                             (funcall
                              (checked-compile
                               `(lambda (x) (destructuring-bind (y . z) x (list y z))))
                              1)))))
    ;; Printing ERROR shouldn't cause an error.
    (assert (search "(Y . Z)" (princ-to-string error)))))

(with-test (:name (destructuring-bind :dotted-list error :printable 2))
  (let ((c (make-condition 'sb-kernel::arg-count-error
                           :args 'x
                           :lambda-list '(a . b)
                           :minimum 1
                           :maximum nil
                           :name 'foo
                           :kind 'macro)))
    (assert (search "(A . B)" (write-to-string c :escape nil))))
  (let ((c (make-condition 'sb-kernel::arg-count-error
                           :args '(x . y)
                           :lambda-list '(a b . c)
                           :minimum 1
                           :maximum nil
                           :name 'foo
                           :kind 'macro)))
    (assert (search "(A B . C)" (write-to-string c :escape nil)))))

(with-test (:name :supplied-p-multiple-values)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (destructuring-bind
             (&key (x (funcall x) x-supplied)) ()
           (list x x-supplied)))
    (((lambda () (values 1 2))) '(1 nil) :test #'equal)))
