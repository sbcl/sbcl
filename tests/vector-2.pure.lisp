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

;;; test case from Utz-Uwe Haus
(defstruct some-struct
  (a 0 :type integer))
(defun foo (m)
  (declare (type (vector some-struct) m))
  m)
(defun bar (m)
  (declare (type (vector some-struct) m))
  (let* ((subarray (make-array (- (length m) 1)
                               :element-type 'some-struct
                               :displaced-to m :displaced-index-offset 1)))
    (foo subarray)))
(defvar *a-foo* (make-some-struct))
(defvar *a-foo-vec*
  (make-array 2 :element-type 'some-struct :adjustable t
              :initial-contents (list *a-foo* *a-foo*)))
(assert (typep (bar *a-foo-vec*) '(vector some-struct)))

;;; some extra sanity checks
(compile (defun compiled-vector-t-p (x) (typep x '(vector t))))
(compile (defun compiled-simple-vector-p (x) (typep x 'simple-vector)))
(defun evaluated-vector-t-p (x) (typep x (opaque-identity '(vector t))))
(defun evaluated-simple-vector-p (x)
  (typep x (opaque-identity 'simple-vector)))

(defvar *simple-vector* (vector 1 2))
(defvar *adjustable-vector-t* (make-array 2 :adjustable t))
(defvar *adjustable-array* (make-array '(2 2) :adjustable t))
(defvar *vector-with-fill-pointer* (make-array 2 :fill-pointer t))
(defvar *vector-displaced-to-simple-vector*
  (make-array 1 :displaced-to *simple-vector* :displaced-index-offset 1))
(defvar *vector-displaced-to-adjustable-vector-t*
  (make-array 1 :displaced-to *adjustable-vector-t* :displaced-index-offset 1))
(defvar *vector-displaced-to-adjustable-array*
  (make-array 1 :displaced-to *adjustable-array* :displaced-index-offset 3))
(defvar *vector-displaced-to-vector-with-fill-pointer*
  (make-array 1 :displaced-to *vector-with-fill-pointer*
              :displaced-index-offset 1))
(defvar *array-displaced-to-simple-vector*
  (make-array '(1 1) :displaced-to *simple-vector*
              :displaced-index-offset 0))
(defvar *array-displaced-to-adjustable-vector-t*
  (make-array '(1 1) :displaced-to *adjustable-vector-t*
              :displaced-index-offset 1))
(defvar *simple-array* (make-array '(1 1)))

(macrolet
    ((frob (object simple-vector-p vector-t-p)
       `(progn
         (assert (eq (compiled-vector-t-p ,object) ,vector-t-p))
         (assert (eq (compiled-simple-vector-p ,object) ,simple-vector-p))
         (assert (eq (evaluated-vector-t-p ,object) ,vector-t-p))
         (assert (eq (evaluated-simple-vector-p ,object) ,simple-vector-p)))))
  (frob *simple-vector* t t)
  (frob *adjustable-vector-t* nil t)
  (frob *adjustable-array* nil nil)
  (frob *vector-with-fill-pointer* nil t)
  (frob *vector-displaced-to-simple-vector* nil t)
  (frob *vector-displaced-to-adjustable-vector-t* nil t)
  (frob *vector-displaced-to-adjustable-array* nil t)
  (frob *vector-displaced-to-vector-with-fill-pointer* nil t)
  (frob *array-displaced-to-simple-vector* nil nil)
  (frob *array-displaced-to-adjustable-vector-t* nil nil)
  (frob *simple-array* nil nil))

;;; While it's true that we might actually want NOT to zero-fill
;;; all dx-vectors, the zero-fill code for x86-64 was broken with ':msan'
;;; in features because it reused 'rcx' as the count after rcx was
;;; already decremented to 0 by the shadow unpoisoning loop.
(with-test (:name :dx-char-vector-zeroized)
  (checked-compile-and-assert
      ()
      `(lambda (n)
         (sb-int:dx-let ((v (make-array (the (mod 200) n)
                                        :initial-element #\null
                                        :element-type 'base-char)))
           (find #\null v :test #'char/=)))
    ((40) nil)))
