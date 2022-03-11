;;;; gray-box testing of the constructor optimization machinery

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

(load "compiler-test-util.lisp")

(defpackage "CTOR-TEST"
  (:use "CL" "TEST-UTIL" "COMPILER-TEST-UTIL"))

(in-package "CTOR-TEST")

(defclass no-slots () ())

(defun make-no-slots ()
  (make-instance 'no-slots))
(compile 'make-no-slots)

(defmethod update-instance-for-redefined-class
    ((object no-slots) added discarded plist &rest initargs)
  (declare (ignore initargs))
  (error "Called U-I-F-R-C on ~A" object))

(assert (typep (make-no-slots) 'no-slots))

(make-instances-obsolete 'no-slots)

(assert (typep (make-no-slots) 'no-slots))
(assert (typep (funcall (gethash '(sb-pcl::ctor no-slots nil) sb-pcl::*all-ctors*)) 'no-slots))

(defclass one-slot ()
  ((a :initarg :a)))

(defun make-one-slot-a (a)
  (make-instance 'one-slot :a a))
(compile 'make-one-slot-a)
(defun make-one-slot-noa ()
  (make-instance 'one-slot))
(compile 'make-one-slot-noa)

(defmethod update-instance-for-redefined-class
    ((object one-slot) added discarded plist &rest initargs)
  (declare (ignore initargs))
  (error "Called U-I-F-R-C on ~A" object))

(assert (= (slot-value (make-one-slot-a 3) 'a) 3))
(assert (not (slot-boundp (make-one-slot-noa) 'a)))

(make-instances-obsolete 'one-slot)

(assert (= (slot-value (make-one-slot-a 3) 'a) 3))
(assert (= (slot-value (funcall (gethash '(sb-pcl::ctor one-slot nil :a sb-pcl::\.p0.) sb-pcl::*all-ctors*)
                                4) 'a) 4))
(assert (not (slot-boundp (make-one-slot-noa) 'a)))
(assert (not (slot-boundp (funcall (gethash '(sb-pcl::ctor one-slot nil) sb-pcl::*all-ctors*)) 'a)))

(defclass one-slot-superclass ()
  ((b :initarg :b)))
(defclass one-slot-subclass (one-slot-superclass)
  ())

(defun make-one-slot-subclass (b)
  (make-instance 'one-slot-subclass :b b))
(compile 'make-one-slot-subclass)

(defmethod update-instance-for-redefined-class
    ((object one-slot-superclass) added discarded plist &rest initargs)
  (declare (ignore initargs))
  (error "Called U-I-F-R-C on ~A" object))

(assert (= (slot-value (make-one-slot-subclass 2) 'b) 2))

(make-instances-obsolete 'one-slot-subclass)

(assert (= (slot-value (make-one-slot-subclass 2) 'b) 2))
(assert (= (slot-value (funcall (gethash '(sb-pcl::ctor one-slot-subclass nil :b sb-pcl::\.p0.) sb-pcl::*all-ctors*)
                                3) 'b) 3))
(make-instances-obsolete 'one-slot-superclass)

(assert (= (slot-value (make-one-slot-subclass 2) 'b) 2))
(assert (= (slot-value (funcall (gethash '(sb-pcl::ctor one-slot-subclass nil :b sb-pcl::\.p0.) sb-pcl::*all-ctors*)
                                4) 'b) 4))

;;; Tests for CTOR optimization of non-constant class args and constant class object args
(defun find-ctor-caches (fun)
  (remove-if-not (lambda (value)
                   (and (consp value) (eq 'sb-pcl::ctor-cache (car value))))
                 (find-code-constants fun)))

(let* ((transform (sb-int:info :function :source-transform 'make-instance))
        (opt 0)
        (wrapper (lambda (form env)
                   (let ((res (funcall transform form env)))
                     (unless (eq form res)
                       (incf opt))
                     res))))
   (sb-ext:without-package-locks
     (unwind-protect
          (progn
            (setf (sb-int:info :function :source-transform 'make-instance) wrapper)
            (with-test (:name (make-instance :non-constant-class))
              (assert (= 0 opt))
              (let ((f (checked-compile `(lambda (class)
                                           (make-instance class :b t)))))
                (assert (= 1 (length (find-ctor-caches f))))
                (assert (= 1 opt))
                (assert (typep (funcall f 'one-slot-subclass) 'one-slot-subclass))))
            (with-test (:name (make-instance :constant-class-object))
              (let ((f (checked-compile `(lambda ()
                                           (make-instance ,(find-class 'one-slot-subclass) :b t)))))
                (assert (not (find-ctor-caches f)))
                (assert (= 2 opt))
                (assert (typep (funcall f) 'one-slot-subclass))))
            (with-test (:name (make-instance :constant-non-std-class-object))
              (let ((f (checked-compile `(lambda ()
                                           (make-instance ,(find-class 'structure-object))))))
                (assert (not (find-ctor-caches f)))
                (assert (= 3 opt))
                (assert (typep (funcall f) 'structure-object))))
            (with-test (:name (make-instance :constant-non-std-class-name))
              (let ((f (checked-compile `(lambda ()
                                           (make-instance 'structure-object)))))
                (assert (not (find-ctor-caches f)))
                (assert (= 4 opt))
                (assert (typep (funcall f) 'structure-object)))))
       (setf (sb-int:info :function :source-transform 'make-instance) transform))))

(with-test (:name (make-instance :ctor-inline-cache-resize))
  (let* ((f (checked-compile `(lambda (name) (make-instance name))))
         (classes (loop repeat (* 2 sb-pcl:+ctor-table-max-size+)
                        collect (class-name (eval `(defclass ,(gentemp) () ())))))
         (count 0)
         (caches (find-ctor-caches f))
         (cache (pop caches)))
    (assert cache)
    (assert (not caches))
    (assert (not (cdr cache)))
    (dolist (class classes)
      (assert (typep (funcall f (if (oddp count) class (find-class class))) class))
      (incf count)
      (cond ((<= count sb-pcl:+ctor-list-max-size+)
             (unless (consp (cdr cache))
               (error "oops, wanted list cache, got: ~S" cache))
             (unless (= count (length (cdr cache)))
               (error "oops, wanted ~S elts in cache, got: ~S" count cache)))
            (t
             (assert (simple-vector-p (cdr cache))))))
    (dolist (class classes)
      (assert (typep (funcall f (if (oddp count) class (find-class class))) class))
      (incf count))))

;;; Make sure we get default initargs right with on the FAST-MAKE-INSTANCE path CTORs
(defclass some-class ()
  ((aroundp :initform nil :reader aroundp))
  (:default-initargs :x :success1))

(defmethod shared-initialize :around ((some-class some-class) slots &key (x :fail?))
  (unless (eq x :success1)
    (error "Default initarg lossage"))
  (setf (slot-value some-class 'aroundp) t)
  (when (next-method-p)
    (call-next-method)))

(with-test (:name (make-instance :ctor-default-initargs-1))
  (assert (aroundp (eval `(make-instance 'some-class))))
  (let ((fun (checked-compile `(lambda () (make-instance 'some-class)))))
    (assert (aroundp (funcall fun)))
    ;; make sure we tested what we think we tested...
    (let ((ctors (find-anonymous-callees fun :type 'sb-pcl::ctor)))
      (assert ctors)
      (assert (not (cdr ctors)))
      (assert (find-named-callees (car ctors) :name 'sb-pcl::fast-make-instance)))))

;;; Make sure we get default initargs right with on the FAST-MAKE-INSTANCE path CTORs
;;; in more interesting cases as well...
(defparameter *some-counter* 0)
(let* ((x 'success2))
  (defclass some-class2 ()
    ((aroundp :initform nil :reader aroundp))
    (:default-initargs :x (progn (incf *some-counter*) x))))

(defmethod shared-initialize :around ((some-class some-class2) slots &key (x :fail2?))
  (unless (eq x 'success2)
    (error "Default initarg lossage"))
  (setf (slot-value some-class 'aroundp) t)
  (when (next-method-p)
    (call-next-method)))

(with-test (:name (make-instance :ctor-default-initargs-2))
  (assert (= 0 *some-counter*))
  (assert (aroundp (eval `(make-instance 'some-class2))))
  (assert (= 1 *some-counter*))
  (let ((fun (checked-compile `(lambda () (make-instance 'some-class2)))))
    (assert (= 1 *some-counter*))
    (assert (aroundp (funcall fun)))
    (assert (= 2 *some-counter*))
    ;; make sure we tested what we think we tested...
    (let ((ctors (find-anonymous-callees fun :type 'sb-pcl::ctor)))
      (assert ctors)
      (assert (not (cdr ctors)))
      (assert (find-named-callees (car ctors) :name 'sb-pcl::fast-make-instance)))))

;;; No compiler notes, please
(locally (declare (optimize safety))
  (defclass type-check-thing ()
    ((slot :type (integer 0) :initarg :slot))))
(with-test (:name (make-instance :no-compile-note-at-runtime))
  (let ((fun (checked-compile `(lambda (x)
                                 (declare (optimize safety))
                                 (make-instance 'type-check-thing :slot x)))))
    (handler-bind ((sb-ext:compiler-note #'error))
      (funcall fun 41)
      (funcall fun 13))))

;;; NO-APPLICABLE-METHOD called
(defmethod no-applicable-method ((gf (eql #'make-instance)) &rest args)
  (cons :no-applicable-method args))
(with-test (:name :constant-invalid-class-arg)
  (checked-compile-and-assert ()
      `(lambda (x) (make-instance "FOO" :quux x))
    ((14) '(:no-applicable-method "FOO" :quux 14)))
  (checked-compile-and-assert ()
      `(lambda (x y) (make-instance ''abc 'zot x 'bar y))
    ((1 2) '(:no-applicable-method 'abc zot 1 bar 2))))

(with-test (:name :variable-invalid-class-arg)
  (checked-compile-and-assert ()
      `(lambda (c x) (make-instance c :quux x))
    (("FOO" 14) '(:no-applicable-method "FOO" :quux 14)))
  (checked-compile-and-assert ()
      `(lambda (c x y) (make-instance c 'zot x 'bar y))
    ((''abc 1 2) '(:no-applicable-method 'abc zot 1 bar 2))))

(defclass sneaky-class (standard-class)
  ())

(defmethod sb-mop:validate-superclass ((class sneaky-class) (super standard-class))
  t)

(defclass sneaky ()
  ((dirty :initform nil :accessor dirty-slots)
   (a :initarg :a :reader sneaky-a)
   (b :initform "b" :reader sneaky-b)
   (c :accessor sneaky-c))
  (:metaclass sneaky-class))

(defvar *supervising* nil)

(defmethod (setf sb-mop:slot-value-using-class)
    :before (value (class sneaky-class) (instance sneaky) slotd)
  (unless *supervising*
    (let ((name (sb-mop:slot-definition-name slotd))
          (*supervising* t))
      (when (slot-boundp instance 'dirty)
        (pushnew name (dirty-slots instance))))))

(with-test (:name (make-instance :setf-slot-value-using-class-hits-other-slots))
  (let ((fun (checked-compile `(lambda (a c)
                                 (let ((i (make-instance 'sneaky :a a)))
                                   (setf (sneaky-c i) c)
                                   i)))))
    (loop repeat 3
          do (let ((i (funcall fun "a" "c")))
               (assert (equal '(c b a) (dirty-slots i)))
               (assert (equal "a" (sneaky-a i)))
               (assert (equal "b" (sneaky-b i)))
               (assert (equal "c" (sneaky-c i)))))))

(defclass bug-728650-base ()
  ((value
    :initarg :value
    :initform nil)))

(defmethod initialize-instance :after ((instance bug-728650-base) &key)
  (with-slots (value) instance
    (unless value
      (error "Impossible! Value slot not initialized in ~S" instance))))

(defclass bug-728650-child-1 (bug-728650-base)
  ())

(defmethod initialize-instance :around ((instance bug-728650-child-1) &rest initargs &key)
  (apply #'call-next-method instance :value 'provided-by-child-1 initargs))

(defclass bug-728650-child-2 (bug-728650-base)
  ())

(defmethod initialize-instance :around ((instance bug-728650-child-2) &rest initargs &key)
  (let ((foo (make-instance 'bug-728650-child-1)))
    (apply #'call-next-method instance :value foo initargs)))

(with-test (:name :bug-728650)
  (let ((child1 (slot-value (make-instance 'bug-728650-child-2) 'value)))
    (assert (typep child1 'bug-728650-child-1))
    (assert (eq 'provided-by-child-1 (slot-value child1 'value)))))

(defclass test-fancy-cnm () ((a :initarg :a)))
(defmethod initialize-instance :around ((self test-fancy-cnm) &rest args)
  ;; WALK-METHOD-LAMBDA would get to the second form of CALL-NEXT-METHOD
  ;; and set the CALL-NEXT-METHOD-P flag to :SIMPLE
  ;; even though it had already been set to T by the earlier call.
  (if t
      (call-next-method self :a `(expect-this ,(getf args :a)))
      (call-next-method)))
(defun fancy-cnm-in-ii-test (x) (make-instance 'test-fancy-cnm :a x))
(with-test (:name :bug-1397454)
  (assert (equal (slot-value (fancy-cnm-in-ii-test 'hi) 'a)
                 '(expect-this hi))))

(with-test (:name (make-instance :ctor
                   :constant-initarg :constant-redefinition
                   :bug-1644944))
  (let ((class-name (gensym))
        (slot-name (gensym))
        (all-specs '()))
    (eval `(defclass ,class-name () ((,slot-name :initarg :s
                                                 :reader ,slot-name))))
    (flet ((define-constant (name value)
             (handler-bind ((sb-ext:defconstant-uneql #'continue))
               (eval `(defconstant ,name ',value))))
           (make (value)
             (checked-compile
              `(lambda ()
                 (make-instance ',class-name :s ,value))))
           (check (&rest specs)
             (setf all-specs (append all-specs specs))
             (loop :for (fun expected) :on all-specs :by #'cddr
                :do (assert (eql (funcall (symbol-function slot-name)
                                          (funcall fun))
                                 expected)))))
      ;; Test constructors using the constant symbol and the relevant
      ;; constant values.
      (let ((constant-name (gensym)))
        (define-constant constant-name 1)
        (destructuring-bind (f-1-c f-1-1 f-1-2)
            (mapcar #'make `(,constant-name 1 2))
          (check f-1-c 1 f-1-1 1 f-1-2 2))

        ;; Redefining the constant must not affect the existing
        ;; constructors. New constructors must use the new value.
        (define-constant constant-name 2)
        (destructuring-bind (f-2-c f-2-1 f-2-2)
            (mapcar #'make `(,constant-name 1 2))
          (check f-2-c 2 f-2-1 1 f-2-2 2))

        ;; Same for non-atom values, with the additional complication of
        ;; preserving (non-)same-ness.
        (let ((a1 '(:a)) (a2 '(:a)) (b '(:b)))
          (define-constant constant-name a1)
          (destructuring-bind (f-3-c f-3-a1 f-3-a2 f-3-b)
              (mapcar #'make (list constant-name `',a1 `',a2 `',b))
            (check f-3-c a1 f-3-a1 a1 f-3-a2 a2 f-3-b b))
          (define-constant constant-name b)
          (destructuring-bind (f-4-c f-4-a1 f-4-a2 f-4-b)
              (mapcar #'make (list constant-name `',a1 `',a2 `',b))
            (check f-4-c b f-4-a1 a1 f-4-a2 a2 f-4-b b))))

      ;; A different constant with the same value must not cause
      ;; aliasing.
      (let ((constant-name-1 (gensym))
            (constant-name-2 (gensym)))
        (define-constant constant-name-1 1)
        (define-constant constant-name-2 1)
        (destructuring-bind (f-5-d-c f-5-d-1 f-5-d-2)
            (mapcar #'make `(,constant-name-1 1 2))
          (check f-5-d-c 1 f-5-d-1 1 f-5-d-2 2))
        (destructuring-bind (f-5-e-c f-5-e-1 f-5-e-2)
            (mapcar #'make `(,constant-name-2 1 2))
          (check f-5-e-c 1 f-5-e-1 1 f-5-e-2 2))
        (define-constant constant-name-1 2)
        (destructuring-bind (f-6-d-c f-6-d-1 f-6-d-2)
            (mapcar #'make `(,constant-name-1 1 2))
          (check f-6-d-c 2 f-6-d-1 1 f-6-d-2 2))
        (destructuring-bind (f-6-e-c f-6-e-1 f-6-e-2)
            (mapcar #'make `(,constant-name-2 1 2))
          (check f-6-e-c 1 f-6-e-1 1 f-6-e-2 2))))))
