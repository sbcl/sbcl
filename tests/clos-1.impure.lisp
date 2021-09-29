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

(let ((fun (checked-compile '(lambda (x) (slot-value x 'a)))))
  (dotimes (i 4)                        ; KLUDGE: get caches warm
    (assert (= 1 (slot-value *foo* 'a)))
    (assert (= 1 (a-of *foo*)))
    (assert (= 1 (funcall fun *foo*)))
    (assert-error (b-of *foo*))
    (assert-error (c-of *foo*))))

(defclass foo ()
  ((b :initarg :b :initform 3) (a :initarg :a)))

(let ((fun (checked-compile '(lambda (x) (slot-value x 'a)))))
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

(let ((fun (checked-compile '(lambda (x) (slot-value x 'a)))))
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

(let ((fun (checked-compile '(lambda (x) (slot-value x 'a)))))
  (dotimes (i 4) ; KLUDGE: get caches warm
    (assert (= 1 (slot-value *foo* 'a)))
    (assert (= 1 (a-of *foo*)))
    (assert (= 1 (funcall fun *foo*)))
    (assert (= 3 (b-of *foo*)))
    (assert (eq t (c-of *foo*)))))

(defclass foo ()
  ((b :initarg :b :initform 3)))

(let ((fun (checked-compile '(lambda (x) (slot-value x 'a)))))
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
  (defgeneric foo (x &key))
  (defmethod foo ((x integer) &key bar) (list x bar))
  (checked-compile '(lambda () (foo (read) :bar 10))))

(fmakunbound 'foo)
(with-test (:name :keywords-supplied-in-methods-ok-2)
  (defgeneric foo (x &key))
  (defmethod foo ((x integer) &key bar) (list x bar))
  ;; On second thought...
  (remove-method #'foo (find-method #'foo () '(integer)))
  (multiple-value-bind  (fun failure-p warnings style-warnings)
      (checked-compile '(lambda () (foo (read) :bar 10))
                       :allow-style-warnings t)
    (declare (ignore fun failure-p warnings))
    (assert (= (length style-warnings) 1))))

;; If the GF has &REST with no &KEY, not all methods are required to
;; parse the tail of the arglist as keywords, so we don't treat the
;; function type as having &KEY in it.
(fmakunbound 'foo)
(with-test (:name :gf-rest-method-key)
  (defgeneric foo (x &rest y))
  (defmethod foo ((i integer) &key w) (list i w))
  ;; 1.0.20.30 failed here.
  (checked-compile '(lambda () (foo 5 :w 10 :foo 15)))
  (assert
   (not (sb-kernel::args-type-keyp (sb-int:global-ftype 'foo)))))

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
                          (sb-int:global-ftype 'foo)))
                 '(:y :z)))
  (checked-compile '(lambda () (foo 5 :z 10 :y 15)))
  (checked-compile '(lambda () (foo 5 :z 10 :foo 15)))
  (assert
   (sb-kernel::args-type-keyp (sb-int:global-ftype 'foo)))
  (assert
   (sb-kernel::args-type-allowp (sb-int:global-ftype 'foo))))

;; If any method has &ALLOW-OTHER-KEYS, 7.6.4 point 5 seems to say the
;; GF should be construed to have &ALLOW-OTHER-KEYS.
(fmakunbound 'foo)
(with-test (:name :method-allow-other-keys)
  (defgeneric foo (x &key))
  (defmethod foo ((x integer) &rest y &key &allow-other-keys) (list x y))
  (checked-compile '(lambda () (foo 10 :foo 20)))
  (assert (sb-kernel::args-type-keyp (sb-int:global-ftype 'foo)))
  (assert (sb-kernel::args-type-allowp (sb-int:global-ftype 'foo))))

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

(with-test (:name :bug-309084-a-i)
  (assert-error (eval '(define-method-combination bug-309084-a-i :documentation :operator))
                program-error))
(with-test (:name :bug-309084-a-ii)
  (assert-error (eval '(define-method-combination bug-309084-a-ii :documentation nil))
                program-error))
(with-test (:name :bug-309084-a-iii)
  (assert-error (eval '(define-method-combination bug-309084-a-iii nil))
                program-error))
(with-test (:name :bug-309084-a-vi)
  (assert-error (eval '(define-method-combination bug-309084-a-vi nil nil
                        (:generic-function)))
                program-error))
(with-test (:name :bug-309084-a-vii)
  (assert-error (eval '(define-method-combination bug-309084-a-vii nil nil
                        (:generic-function bar baz)))
                program-error))
(with-test (:name :bug-309084-a-viii)
  (assert-error (eval '(define-method-combination bug-309084-a-viii nil nil
                        (:generic-function (bar))))
                program-error))
(with-test (:name :bug-309084-a-ix)
  (assert-error (eval '(define-method-combination bug-309084-a-ix nil ((3))))
                program-error))
(with-test (:name :bug-309084-a-x)
  (assert-error (eval '(define-method-combination bug-309084-a-x nil ((a))))
                program-error))
(with-test (:name :bug-309084-a-iv)
  (assert-error (eval '(define-method-combination bug-309084-a-iv nil nil
                        (:arguments order &aux &key)))
                program-error))
(with-test (:name :bug-309084-a-v)
  (assert-error (eval '(define-method-combination bug-309084-a-v nil nil
                        (:arguments &whole)))
                program-error))

(let (warnings)
  (handler-bind ((warning (lambda (c) (push c warnings))))
    (eval '(define-method-combination bug-309084-b/mc nil
             ((all *))
             (:arguments x &optional (y 'a yp) &key (z 'b zp) &aux (w (list y z)))
             `(list ,x ,y ,yp ,z ,zp ,w)))
    ;; Should not get any "assigned but never read" warnings.
    (assert (= (length warnings) 1))
    (assert (search "&OPTIONAL and &KEY" (princ-to-string (car warnings))))))

(defgeneric bug-309084-b/gf (a &optional b &key &allow-other-keys)
  (:method-combination bug-309084-b/mc)
  (:method (m &optional n &key) (list m n)))

(with-test (:name :bug-309084-b)
  (assert (equal (bug-309084-b/gf 1) '(1 a nil b nil (a b))))
  (assert (equal (bug-309084-b/gf 1 2) '(1 2 t b nil (2 b))))
  (assert (equal (bug-309084-b/gf 1 2 :z 3) '(1 2 t 3 t (2 3)))))

(defgeneric bug-309084-b/gf2 (a b &optional c d &key &allow-other-keys)
  (:method-combination bug-309084-b/mc)
  (:method (m n &optional o p &key) (list m n o p)))

(with-test (:name :bug-309084-b2)
  (assert (equal (bug-309084-b/gf2 1 2) '(1 a nil b nil (a b))))
  (assert (equal (bug-309084-b/gf2 1 2 3) '(1 3 t b nil (3 b))))
  (assert (equal (bug-309084-b/gf2 1 2 3 4) '(1 3 t b nil (3 b))))
  (assert (equal (bug-309084-b/gf2 1 2 :z t) '(1 :z t b nil (:z b))))
  (assert (equal (bug-309084-b/gf2 1 2 3 4 :z 5) '(1 3 t 5 t (3 5)))))

(defmethod bug-1840595-a (x y))
(defmethod bug-1840595-z (x))

(with-test (:name :bug-1840595/reader)
  (eval '(defclass bug-1840595r () ()))
  (assert-error (eval '(defclass bug-1840595r () ((a :reader bug-1840595-a)))))
  (eval '(defclass bug-1840595r () ())))

(with-test (:name :bug-1840595/writer)
  (eval '(defclass bug-1840595w () ()))
  (assert-error (eval '(defclass bug-1840595w () ((z :writer bug-1840595-z)))))
  (eval '(defclass bug-1840595w () ())))

(with-test (:name :bug-1909659/reader)
  (eval '(defclass bug-1909659r () ((name :initarg :name :reader bug-1909659r-name))))
  (let ((one (make-instance 'bug-1909659r :name 1))
        (two (make-instance 'bug-1909659r :name 2)))
    (assert-error (bug-1909659r-name one two) program-error)
    (assert (eql (bug-1909659r-name one) 1))
    (assert (eql (bug-1909659r-name two) 2))))

(with-test (:name :bug-1909659/writer)
  (eval '(defclass bug-1909659w () ((name :initarg :name :writer bug-1909659w-set-name))))
  (let ((one (make-instance 'bug-1909659w :name 1))
        (two (make-instance 'bug-1909659w :name 2)))
    (assert-error (bug-1909659w-set-name one) program-error)
    (assert-error (bug-1909659w-set-name two) program-error)
    (bug-1909659w-set-name one two)
    (assert (eql (slot-value one 'name) 1))
    (assert (eql (slot-value two 'name) one))))

(with-test (:name :defmethod-self-call-arg-mismatch
            :skipped-on :interpreter)
  (assert-signal (eval '(defmethod method-self-call (a b &key)
                         b
                         (method-self-call a)))
      (and warning
           (not sb-kernel:redefinition-warning)))
  (assert-no-signal (eval '(defmethod method-self-call (a b &key z)
                            (method-self-call a b :z z)))
      (and warning
           (not sb-kernel:redefinition-warning)))
  (assert-signal (eval '(defmethod method-self-call (a b &key j)
                         j
                         (method-self-call a b :z j)))
      (and warning
           (not sb-kernel:redefinition-warning)))
  (eval '(defmethod method-self-call (a (b list) &key z)
          (list a b z)))

  (assert-no-signal (eval '(defmethod method-self-call (a b &key j)
                            j
                            (method-self-call a b :z j :j 10)))
      (and warning
           (not sb-kernel:redefinition-warning))))
