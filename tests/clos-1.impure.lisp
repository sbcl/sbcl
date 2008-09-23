;;;; miscellaneous side-effectful tests of CLOS

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

;;; clos.impure.lisp was getting too big and confusing

(load "assertoid.lisp")

(defpackage "CLOS-1"
  (:use "CL" "ASSERTOID" "TEST-UTIL"))

;;; tests that various optimization paths for slot-valuish things
;;; respect class redefinitions.
(defclass foo ()
  ((a :initarg :a)))

(defvar *foo* (make-instance 'foo :a 1))

(defmethod a-of ((x foo))
  (slot-value x 'a))
(defmethod b-of ((x foo))
  (slot-value x 'b))
(defmethod c-of ((x foo))
  (slot-value x 'c))

(let ((fun (compile nil '(lambda (x) (slot-value x 'a)))))
  (dotimes (i 4) ; KLUDGE: get caches warm
    (assert (= 1 (slot-value *foo* 'a)))
    (assert (= 1 (a-of *foo*)))
    (assert (= 1 (funcall fun *foo*)))
    (assert (raises-error? (b-of *foo*)))
    (assert (raises-error? (c-of *foo*)))))

(defclass foo ()
  ((b :initarg :b :initform 3) (a :initarg :a)))

(let ((fun (compile nil '(lambda (x) (slot-value x 'a)))))
  (dotimes (i 4) ; KLUDGE: get caches warm
    (assert (= 1 (slot-value *foo* 'a)))
    (assert (= 1 (a-of *foo*)))
    (assert (= 1 (funcall fun *foo*)))
    (assert (= 3 (b-of *foo*)))
    (assert (raises-error? (c-of *foo*)))))

(defclass foo ()
  ((c :initarg :c :initform t :allocation :class)
   (b :initarg :b :initform 3)
   (a :initarg :a)))

(let ((fun (compile nil '(lambda (x) (slot-value x 'a)))))
  (dotimes (i 4) ; KLUDGE: get caches warm
    (assert (= 1 (slot-value *foo* 'a)))
    (assert (= 1 (a-of *foo*)))
    (assert (= 1 (funcall fun *foo*)))
    (assert (= 3 (b-of *foo*)))
    (assert (eq t (c-of *foo*)))))

(defclass foo ()
  ((a :initarg :a)
   (b :initarg :b :initform 3)
   (c :initarg :c :initform t)))

(let ((fun (compile nil '(lambda (x) (slot-value x 'a)))))
  (dotimes (i 4) ; KLUDGE: get caches warm
    (assert (= 1 (slot-value *foo* 'a)))
    (assert (= 1 (a-of *foo*)))
    (assert (= 1 (funcall fun *foo*)))
    (assert (= 3 (b-of *foo*)))
    (assert (eq t (c-of *foo*)))))

(defclass foo ()
  ((b :initarg :b :initform 3)))

(let ((fun (compile nil '(lambda (x) (slot-value x 'a)))))
  (dotimes (i 4) ; KLUDGE: get caches warm
    (assert (raises-error? (slot-value *foo* 'a)))
    (assert (raises-error? (a-of *foo*)))
    (assert (raises-error? (funcall fun *foo*)))
    (assert (= 3 (b-of *foo*)))
    (assert (raises-error? (c-of *foo*)))))

;;; test that :documentation argument to slot specifiers are used as
;;; the docstrings of accessor methods.
(defclass foo ()
  ((a :reader a-of :documentation "docstring for A")
   (b :writer set-b-of :documentation "docstring for B")
   (c :accessor c :documentation  "docstring for C")))

(flet ((doc (fun)
         (documentation fun t)))
  (assert (string= (doc (find-method #'a-of nil '(foo))) "docstring for A"))
  (assert (string= (doc (find-method #'set-b-of nil '(t foo))) "docstring for B"))
  (assert (string= (doc (find-method #'c nil '(foo))) "docstring for C"))
  (assert (string= (doc (find-method #'(setf c) nil '(t foo))) "docstring for C")))

;;; some nasty tests of NO-NEXT-METHOD.
(defvar *method-with-no-next-method*)
(defvar *nnm-count* 0)
(defun make-nnm-tester (x)
  (setq *method-with-no-next-method* (defmethod nnm-tester ((y (eql x))) (call-next-method))))
(make-nnm-tester 1)
(defmethod no-next-method ((gf (eql #'nnm-tester)) method &rest args)
  (assert (eql method *method-with-no-next-method*))
  (incf *nnm-count*))
(with-test (:name (no-next-method :unknown-specializer))
  (nnm-tester 1)
  (assert (= *nnm-count* 1)))
(let ((gf #'nnm-tester))
  (reinitialize-instance gf :name 'new-nnm-tester)
  (setf (fdefinition 'new-nnm-tester) gf))
(with-test (:name (no-next-method :gf-name-changed))
  (new-nnm-tester 1)
  (assert (= *nnm-count* 2)))

;;; Tests the compiler's incremental rejiggering of GF types.
(fmakunbound 'foo)
(with-test (:name keywords-supplied-in-methods-ok-1)
  (assert
   (null
    (nth-value
     1
     (progn
       (defgeneric foo (x &key))
       (defmethod foo ((x integer) &key bar) (list x bar))
       (compile nil '(lambda () (foo (read) :bar 10))))))))

(fmakunbound 'foo)
(with-test (:name keywords-supplied-in-methods-ok-2)
  (assert
   (nth-value
    1
    (progn
      (defgeneric foo (x &key))
      (defmethod foo ((x integer) &key bar) (list x bar))
      ;; On second thought...
      (remove-method #'foo (find-method #'foo () '(integer)))
      (compile nil '(lambda () (foo (read) :bar 10)))))))
