;;;; Interaction of SB-MOP:FUNCALLABLE-STANDARD-OBJECT with COERCE

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

(defun unoptimized/symbol ()
  (lambda (x)
    (coerce x (opaque-identity 'sb-mop:funcallable-standard-object))))
(defun unoptimized/class ()
  (lambda (x)
    (coerce x (opaque-identity (find-class 'sb-mop:funcallable-standard-object)))))
(defun optimized/symbol ()
  (compile nil `(lambda (x) (coerce x 'sb-mop:funcallable-standard-object))))
(defun optimized/class ()
  (compile nil `(lambda (x) (coerce x ',(find-class 'sb-mop:funcallable-standard-object)))))

(with-test (:name (coerce standard-object))
  (let ((o (make-instance 'standard-object)))
    (assert-error (funcall (unoptimized/symbol) o) type-error)
    (assert-error (funcall (unoptimized/class) o) type-error)
    (assert-error (funcall (optimized/symbol) o) type-error)
    (assert-error (funcall (optimized/class) o) type-error)))

(with-test (:name (coerce :funcallable-standard-object))
  (let ((o (make-instance 'sb-mop:funcallable-standard-object)))
    (assert (eql (funcall (unoptimized/symbol) o) o))
    (assert (eql (funcall (unoptimized/class) o) o))
    (assert (eql (funcall (optimized/symbol) o) o))
    (assert (eql(funcall (optimized/class) o) o))))

(with-test (:name (coerce symbol))
  (let ((o 'identity))
    (assert-error (funcall (unoptimized/symbol) o) type-error)
    (assert-error (funcall (unoptimized/class) o) type-error)
    (assert-error (funcall (optimized/symbol) o) type-error)
    (assert-error (funcall (optimized/class) o) type-error)))

(with-test (:name (coerce lambda))
  (let ((o '(lambda (x) (1+ x))))
    (assert-error (funcall (unoptimized/symbol) o) type-error)
    (assert-error (funcall (unoptimized/class) o) type-error)
    (assert-error (funcall (optimized/symbol) o) type-error)
    (assert-error (funcall (optimized/class) o) type-error)))
