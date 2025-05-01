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

#-soft-card-marks
(invoke-restart 'run-tests::skip-file)

(defun assert-barriers (potential actual fun &rest compile-args)
  (let* ((old-potential sb-vm::*store-barriers-potentially-emitted*)
         (old-actual sb-vm::*store-barriers-emitted*))
    (apply #'checked-compile fun compile-args)
    (assert (= potential (- sb-vm::*store-barriers-potentially-emitted* old-potential)))
    (assert (= actual (- sb-vm::*store-barriers-emitted* old-actual)))))

(with-test (:name :rplaca-union-types)
  (assert-barriers 1 0
                   `(lambda (x y)
                      (when (typep y '(or fixnum null))
                        (rplaca x y)))))

(with-test (:name :move-from-fixnum)
  (assert-barriers 1 0
                   `(lambda (a b)
                      (declare (fixnum a))
                      (setf (car b) (1+ a))))
  (assert-barriers 1 0
                   `(lambda (a b)
                      (declare (fixnum a))
                      (setf (car b) (- a)))))

(with-test (:name :old-slot-set-no-barrier)
  (assert-barriers 1 0
                   '(lambda (y)
                     (let ((x (cons 0 0)))
                       (setf (car x) y)
                       x))))

(with-test (:name :consequent)
  (assert-barriers 2 2
                   `(lambda (x m j)
                      (setf (car x) m)
                      (print x)
                      (setf (cdr x) j)))
  (assert-barriers 2 2
                   `(lambda (x m j)
                      (setf (car x) m)
                      (setf x *)
                      (setf (cdr x) j)))
  #+(or arm64 x86-64)
  (progn
    (assert-barriers 1 1
                     `(lambda (x m j)
                        (setf (car x) m)
                        (setf (cdr x) j)))
    (assert-barriers 2 1
                     `(lambda (x m j)
                        (declare (fixnum m))
                        (setf (car x) m)
                        (setf (cdr x) j)))
    #+nil
    (assert-barriers 1 1
                     `(lambda (x m j)
                        (setf (car x) m)
                        (setf (cdr x) (list j))))))
