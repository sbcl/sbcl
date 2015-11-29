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
  (dotimes (i 4)                        ; KLUDGE: get caches warm
    (assert (= 1 (slot-value *foo* 'a)))
    (assert (= 1 (a-of *foo*)))
    (assert (= 1 (funcall fun *foo*)))
    (assert-error (b-of *foo*))
    (assert-error (c-of *foo*))))

(defclass foo ()
  ((b :initarg :b :initform 3) (a :initarg :a)))

(let ((fun (compile nil '(lambda (x) (slot-value x 'a)))))
  (dotimes (i 4)                        ; KLUDGE: get caches warm
    (assert (= 1 (slot-value *foo* 'a)))
    (assert (= 1 (a-of *foo*)))
    (assert (= 1 (funcall fun *foo*)))
    (assert (= 3 (b-of *foo*)))
    (assert-error (c-of *foo*))))

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
  (dotimes (i 4)                        ; KLUDGE: get caches warm
    (assert-error (slot-value *foo* 'a))
    (assert-error (a-of *foo*))
    (assert-error (funcall fun *foo*))
    (assert (= 3 (b-of *foo*)))
    (assert-error (c-of *foo*))))

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
  (declare (ignore args))
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
(with-test (:name :keywords-supplied-in-methods-ok-1)
  (assert
   (null
    (nth-value
     1
     (progn
       (defgeneric foo (x &key))
       (defmethod foo ((x integer) &key bar) (list x bar))
       (compile nil '(lambda () (foo (read) :bar 10))))))))

(fmakunbound 'foo)
(with-test (:name :keywords-supplied-in-methods-ok-2)
  (assert
   (nth-value
    1
    (progn
      (defgeneric foo (x &key))
      (defmethod foo ((x integer) &key bar) (list x bar))
      ;; On second thought...
      (remove-method #'foo (find-method #'foo () '(integer)))
      (compile nil '(lambda () (foo (read) :bar 10)))))))

;; If the GF has &REST with no &KEY, not all methods are required to
;; parse the tail of the arglist as keywords, so we don't treat the
;; function type as having &KEY in it.
(fmakunbound 'foo)
(with-test (:name :gf-rest-method-key)
  (defgeneric foo (x &rest y))
  (defmethod foo ((i integer) &key w) (list i w))
  ;; 1.0.20.30 failed here.
  (assert
   (null (nth-value 1 (compile nil '(lambda () (foo 5 :w 10 :foo 15))))))
  (assert
   (not (sb-kernel::args-type-keyp (sb-int:proclaimed-ftype 'foo)))))

;; If the GF has &KEY and &ALLOW-OTHER-KEYS, the methods' keys can be
;; anything, and we don't warn about unrecognized keys.
(fmakunbound 'foo)
(with-test (:name :gf-allow-other-keys)
  (defgeneric foo (x &key &allow-other-keys))
  (defmethod foo ((i integer) &key y z) (list i y z))
  ;; Correctness of a GF's ftype was previously ensured by the compiler,
  ;; and only if a lambda was compiled that referenced the GF, in a way
  ;; that was just barely non-broken enough to make the compiler happy.
  ;; Now the FTYPE is computed the instant anyone asks for it.
  (assert (equal (mapcar 'sb-kernel:key-info-name
                         (sb-kernel:fun-type-keywords
                          (sb-int:proclaimed-ftype 'foo)))
                 '(:y :z)))
  (assert
   (null (nth-value 1 (compile nil '(lambda () (foo 5 :z 10 :y 15))))))
  (assert
   (null (nth-value 1 (compile nil '(lambda () (foo 5 :z 10 :foo 15))))))
  (assert
   (sb-kernel::args-type-keyp (sb-int:proclaimed-ftype 'foo)))
  (assert
   (sb-kernel::args-type-allowp (sb-int:proclaimed-ftype 'foo))))

;; If any method has &ALLOW-OTHER-KEYS, 7.6.4 point 5 seems to say the
;; GF should be construed to have &ALLOW-OTHER-KEYS.
(fmakunbound 'foo)
(with-test (:name :method-allow-other-keys)
  (defgeneric foo (x &key))
  (defmethod foo ((x integer) &rest y &key &allow-other-keys) (list x y))
  (assert (null (nth-value 1 (compile nil '(lambda () (foo 10 :foo 20))))))
  (assert (sb-kernel::args-type-keyp (sb-int:proclaimed-ftype 'foo)))
  (assert (sb-kernel::args-type-allowp (sb-int:proclaimed-ftype 'foo))))

(fmakunbound 'foo)
(with-test (:name (defmethod symbol-macrolet))
  (symbol-macrolet ((cnm (call-next-method)))
    (defmethod foo ((x number)) (1+ cnm)))
  (defmethod foo ((x t)) 3)
  (assert (= (foo t) 3))
  (assert (= (foo 3) 4)))

(fmakunbound 'foo)
(define-symbol-macro magic-cnm (call-next-method))
(with-test (:name (defmethod define-symbol-macro))
  (defmethod foo ((x number)) (1- magic-cnm))
  (defmethod foo ((x t)) 3)
  (assert (= (foo t) 3))
  (assert (= (foo 3) 2)))
