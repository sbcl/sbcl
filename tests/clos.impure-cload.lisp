;;;; miscellaneous side-effectful tests of CLOS and file-compiler
;;;; optimizations

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

;;; Fix due to pmai, ported from CMUCL, regarding
;;; MAKE-INSTANCES-OBSOLETE:
(defclass mio-test ()
  ((test :initarg :test)))

(defun mio-demo ()
  (let ((x (make-instance 'mio-test :test 42)))
    (incf (slot-value x 'test))))

(defun mio-test ()
  (mio-demo)
  (make-instances-obsolete 'mio-test)
  (mio-demo))

(mio-test)

;;; Some tests of bits of optimized MAKE-INSTANCE that were hopelessly
;;; wrong until Gerd's ctor MAKE-INSTANCE optimization was ported.
(defvar *d-i-s-e-count* 0)
(defclass default-initargs-side-effect ()
  ((x :initarg :x))
  (:default-initargs :x (incf *d-i-s-e-count*)))
(defun default-initargs-side-effect ()
  (make-instance 'default-initargs-side-effect))
(assert (= *d-i-s-e-count* 0))
(default-initargs-side-effect)
(assert (= *d-i-s-e-count* 1))
(make-instance 'default-initargs-side-effect)
(assert (= *d-i-s-e-count* 2))
(make-instance 'default-initargs-side-effect :x 3)
(assert (= *d-i-s-e-count* 2))

(defclass class-allocation ()
  ((x :allocation :class :initarg :x :initform 3)))
(defun class-allocation-reader ()
  (slot-value (make-instance 'class-allocation) 'x))
(defun class-allocation-writer (value)
  (setf (slot-value (make-instance 'class-allocation) 'x) value))
(assert (= (class-allocation-reader) 3))
(class-allocation-writer 4)
(assert (= (class-allocation-reader) 4))

;;; from James Anderson via Gerd Moellmann: defining methods with
;;; specializers with forward-referenced superclasses used not to
;;; work.
(defclass specializer1 () ())
(defclass specializer2 (forward-ref1) ())
(defmethod baz ((x specializer2)) x)
(defmethod baz ((x specializer1)) x)
(assert (typep (baz (make-instance 'specializer1)) 'specializer1))

;;; ... and from McCLIM, another test case:
(defclass specializer1a (specializer2a specializer2b) ())
(defclass specializer2a () ())
(defmethod initialize-instance :after
    ((obj specializer2a) &key &allow-other-keys)
  (print obj))

;;; in a similar vein, we should be able to define methods on classes
;;; that are effectively unknown to the type system:
(sb-mop:ensure-class 'unknown-type)
(defmethod method-on-unknown ((x unknown-type)) x)
;;; (we can't call it without defining methods on allocate-instance
;;; etc., but we should be able to define it).

;;; the ctor MAKE-INSTANCE optimizer used not to handle duplicate
;;; initargs...
(defclass dinitargs-class1 ()
  ((a :initarg :a)))
(assert (= (slot-value (make-instance 'dinitargs-class1 :a 1 :a 2) 'a) 1))

(defclass dinitargs-class2 ()
  ((b :initarg :b1 :initarg :b2)))
(assert (= (slot-value (make-instance 'dinitargs-class2 :b2 3 :b1 4) 'b) 3))
;;; ... or default-initargs when the location was already initialized
(defvar *definitargs-counter* 0)
(defclass definitargs-class ()
  ((a :initarg :a :initarg :a2))
  (:default-initargs :a2 (incf *definitargs-counter*)))
(assert (= (slot-value (make-instance 'definitargs-class) 'a) 1))
(assert (= (slot-value (make-instance 'definitargs-class :a 0) 'a) 0))
(assert (= *definitargs-counter* 2))

;;; inherited local -> shared slot initforms
;;  (adapted from Paul F. Dietz's test suite DEFCLASS-0211.1)
(defclass shared-to-local-initform-super ()
  ((redefined :allocation :instance :initform 'orig-initform)))
(defclass shared-to-local-initform-sub (shared-to-local-initform-super)
  ((redefined :allocation :class)))
(assert (slot-boundp (make-instance 'shared-to-local-initform-sub) 'redefined))
(assert (eq 'orig-initform
            (slot-value (make-instance 'shared-to-local-initform-sub) 'redefined)))

(defgeneric no-ignored-warnings (x y))
(handler-case
    (eval '(defmethod no-ignored-warnings ((x t) (y t))
            (declare (ignore x y)) nil))
  (style-warning (c) (error c)))
(handler-case
    (eval '(defmethod no-ignored-warnings ((x number) (y t))
            (declare (ignore x y)) (setq *print-level* nil)))
  (style-warning (c) (error c)))
(handler-case
    (eval '(defmethod no-ignored-warnings ((x fixnum) (y t))
            (declare (ignore x)) (setq y 'foo)))
  (style-warning (c) (error c)))

;;; ctor optimization bugs:
;;;
;;; :DEFAULT-INITARGS not checked for validity
(defclass invalid-default-initargs ()
  ((foo :initarg :foo))
  (:default-initargs :invalid-initarg 2))
(multiple-value-bind (result condition)
    (ignore-errors (make-instance 'invalid-default-initargs :foo 1))
  (assert (null result))
  (assert (typep condition 'program-error)))
;;; :DEFAULT-INITARGS not passed to INITIALIZE-INSTANCE or
;;; SHARED-INITIALIZE :BEFORE methods.
(defclass default-initargs-with-method ()
  ((foo :initarg :valid-initarg))
  (:default-initargs :valid-initarg 2))
(defmethod shared-initialize :before ((thing default-initargs-with-method)
                                      slot-names &key valid-initarg)
  (assert (= valid-initarg 2)))
(make-instance 'default-initargs-with-method)
;;; and a test with a non-constant initarg
(defvar *d-i-w-m-2* 0)
(defclass default-initargs-with-method2 ()
  ((foo :initarg :valid-initarg))
  (:default-initargs :valid-initarg (incf *d-i-w-m-2*)))
(defmethod shared-initialize :before ((thing default-initargs-with-method2)
                                      slot-names &key valid-initarg)
  (assert (= valid-initarg 1)))
(make-instance 'default-initargs-with-method2)
(assert (= *d-i-w-m-2* 1))

;;; from Axel Schairer on cmucl-imp 2004-08-05
(defclass class-with-symbol-initarg ()
  ((slot :initarg slot)))
(defmethod initialize-instance :after
    ((x class-with-symbol-initarg) &rest initargs &key &allow-other-keys)
  (unless (or (null initargs)
              (eql (getf initargs 'slot)
                   (slot-value x 'slot)))
    (error "bad bad bad")))
(defun make-thing (arg)
  (make-instance 'class-with-symbol-initarg 'slot arg))
(defun make-other-thing (slot arg)
  (make-instance 'class-with-symbol-initarg slot arg))
(assert (eql (slot-value (make-thing 1) 'slot) 1))
(assert (eql (slot-value (make-other-thing 'slot 2) 'slot) 2))

;;; test that ctors can be used with the literal class
(eval-when (:compile-toplevel)
  (defclass ctor-literal-class () ())
  (defclass ctor-literal-class2 () ()))
(defun ctor-literal-class ()
  (make-instance #.(find-class 'ctor-literal-class)))
(defun ctor-literal-class2 ()
  (make-instance '#.(find-class 'ctor-literal-class2)))
(with-test (:name (:ctor :literal-class-unquoted))
  (assert (typep (ctor-literal-class) 'ctor-literal-class)))
(with-test (:name (:ctor :literal-class-quoted))
  (assert (typep (ctor-literal-class2) 'ctor-literal-class2)))

;;; test that call-next-method and eval-when doesn't cause an
;;; undumpable method object to arise in the effective source code.
;;; (from Sascha Wilde sbcl-devel 2007-07-15)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod just-call-next-method (thing)
    (call-next-method)))

(defvar *compile-count* 0)
(sb-int:encapsulate 'compile 'call-counter
                    (lambda (f name thing)
                      (incf *compile-count*)
                      (funcall f name thing)))

(defstruct mystruct-r/w (some-slot))
(defstruct mystruct-r/o (allegedly-immutable-slot 3 :read-only t))

(with-test (:name :allocate-instance-of-struct)
  (let ((old-count *compile-count*))
    (make-instance 'mystruct-r/w)
    (assert (= *compile-count* (+ old-count 2)))
    (make-instance 'mystruct-r/w)
    (assert (= *compile-count* (+ old-count 2))))) ; same as before

(with-test (:name (:setf-slot-value-on-readonly-struct-slot))
  (let ((myobj (make-mystruct-r/o))
        (old-count *compile-count*)
        ;; hide the slot name from the compiler so it doesn't optimize
        (slot-name (eval ''allegedly-immutable-slot)))
    (setf (slot-value myobj slot-name) :newval1)
    (assert (= *compile-count* (1+ old-count)))
    (setf (slot-value myobj slot-name) :newval2)
    (assert (= *compile-count* (1+ old-count))))) ; same as before
