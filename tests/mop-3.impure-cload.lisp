;;;; miscellaneous side-effectful tests of the MOP

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

;;; This file contains two tests for COMPUTE-APPLICABLE-METHODS on
;;; subclasses of generic functions.

;;; tests from Bruno Haible (sbcl-devel 2004-08-02)

(defclass msl-generic-function (standard-generic-function)
  ()
  (:metaclass sb-mop:funcallable-standard-class))

(defun reverse-method-list (methods)
  (let ((result '()))
    (dolist (method methods)
      (if (and (consp result)
               (equal (sb-mop:method-qualifiers method)
                      (sb-mop:method-qualifiers (caar result))))
          (push method (car result))
          (push (list method) result)))
    (reduce #'append result)))

(defmethod sb-mop:compute-applicable-methods ((gf msl-generic-function) arguments)
  (reverse-method-list (call-next-method)))
(defmethod sb-mop:compute-applicable-methods-using-classes
    ((gf msl-generic-function) classes)
  (reverse-method-list (call-next-method)))

(defgeneric testgf07 (x)
  (:generic-function-class msl-generic-function)
  (:method ((x integer))
    (cons 'integer (if (next-method-p) (call-next-method))))
  (:method ((x real))
    (cons 'real (if (next-method-p) (call-next-method))))
  (:method ((x number))
    (cons 'number (if (next-method-p) (call-next-method))))
  (:method :around ((x integer))
    (coerce (call-next-method) 'vector)))

(with-test (:name (:mop-3 1))
  (assert (equalp (list (testgf07 5.0) (testgf07 17))
                  '((number real) #(number real integer)))))

(defclass nonumber-generic-function (standard-generic-function)
  ()
  (:metaclass sb-mop:funcallable-standard-class))

(defun nonumber-method-list (methods)
  (remove-if #'(lambda (method)
                 (member (find-class 'number)
                         (sb-mop:method-specializers method)))
             methods))

(defmethod sb-mop:compute-applicable-methods
    ((gf nonumber-generic-function) arguments)
  (nonumber-method-list (call-next-method)))
(defmethod sb-mop:compute-applicable-methods-using-classes
    ((gf nonumber-generic-function) classes)
  (nonumber-method-list (call-next-method)))

(defgeneric testgf08 (x)
  (:generic-function-class nonumber-generic-function)
  (:method ((x integer))
    (cons 'integer (if (next-method-p) (call-next-method))))
  (:method ((x real))
    (cons 'real (if (next-method-p) (call-next-method))))
  (:method ((x number))
    (cons 'number (if (next-method-p) (call-next-method))))
  (:method :around ((x integer))
    (coerce (call-next-method) 'vector)))

(with-test (:name (:mop-3 2))
  (assert (equalp (list (testgf08 5.0) (testgf08 17))
                  '((real) #(integer real)))))
