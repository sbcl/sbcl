;;;; This file is for testing typechecking of writes to CLOS object slots
;;;; for code compiled with a (SAFETY 3) optimization policy.

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

;;; Typechecking should be working, but it isn't.
#+interpreter (sb-ext:exit :code 104)

(shadow 'slot)

(declaim (optimize safety))

(defvar *t* t)
(defvar *one* 1)

;;;; Slot type checking for standard instances

(defclass foo ()
  ((slot :initarg :slot :type fixnum :accessor slot)))
(defmethod succeed/sv ((x foo))
  (setf (slot-value x 'slot) 1))
(defmethod fail/sv ((x foo))
  (setf (slot-value x 'slot) t))
(defmethod succeed/acc ((x foo))
  (setf (slot x) 1))
(defmethod fail/acc ((x foo))
  (setf (slot x) t))

;;; Test slot type checking for standard instances in EVALed code.
(with-test (:name (standard-object slot-value setf :initarg type-error eval))
  (eval '(setf (slot-value (make-instance 'foo) 'slot) 1))
  (assert-error (eval '(setf (slot-value (make-instance 'foo) 'slot) t))
                type-error)
  (eval '(setf (slot (make-instance 'foo)) 1))
  (assert-error (eval '(setf (slot (make-instance 'foo)) t))
                type-error)
  (eval '(succeed/sv (make-instance 'foo)))
  (assert-error (eval '(fail/sv (make-instance 'foo)))
                type-error)
  (eval '(succeed/acc (make-instance 'foo)))
  (assert-error (eval '(fail/acc (make-instance 'foo)))
                type-error)
  (eval '(make-instance 'foo :slot 1))
  (assert-error (eval '(make-instance 'foo :slot t))
                type-error)
  (eval '(make-instance 'foo :slot *one*))
  (assert-error (eval '(make-instance 'foo :slot *t*))
                type-error))

;;; Test slot type checking for standard instances in compiled code.
(with-test (:name (standard-object slot-value setf :initarg type-error
                   compile))
  (checked-compile-and-assert (:optimize :safe)
      '(lambda ()
         (setf (slot-value (make-instance 'foo) 'slot) 1))
    (() 1))
  (checked-compile-and-assert ()
      '(lambda ()
         (setf (slot-value (make-instance 'foo) 'slot) t))
    (() (condition 'type-error)))

  (checked-compile-and-assert (:optimize :safe)
      '(lambda () (setf (slot (make-instance 'foo)) 1))
    (() 1))
  (checked-compile-and-assert (:optimize :safe)
      '(lambda () (setf (slot (make-instance 'foo)) t))
    (() (condition 'type-error)))

  (checked-compile-and-assert (:optimize :safe)
      '(lambda () (succeed/sv (make-instance 'foo)))
    (() 1))
  (checked-compile-and-assert (:optimize :safe)
      '(lambda () (fail/sv (make-instance 'foo)))
    (() (condition 'type-error)))

  (checked-compile-and-assert (:optimize :safe)
      '(lambda () (succeed/acc (make-instance 'foo)))
    (() 1))
  (checked-compile-and-assert (:optimize :safe)
      '(lambda () (fail/acc (make-instance 'foo)))
    (() (condition 'type-error)))

  ;; These four cases trigger PCL's constructor mechanism and
  ;;
  ;; FIXME: the type mismatch is handled poorly:
  ;; When the function is *called*, an entry is added to the
  ;; constructor cache, this process signals a TYPE-WARNING, usually
  ;; meant for compile-time. When the call proceeds and executes the
  ;; newly cached constructor, the expected runtime TYPE-ERROR is
  ;; signaled.
  ;;
  ;; FIXME: also note that constructor type checks are only inserted
  ;; when SAFETY is 3, which is different from other slot type checks.
  (checked-compile-and-assert (:optimize :maximally-safe)
      '(lambda () (make-instance 'foo :slot 1))
    (() '(cons foo null) :test (lambda (values expected)
                                 (typep values (first expected)))))
  (checked-compile-and-assert (:optimize :maximally-safe)
      '(lambda () (make-instance 'foo :slot t))
    (() (condition '(or sb-int:type-warning type-error))))

  (checked-compile-and-assert (:optimize :maximally-safe)
      '(lambda () (make-instance 'foo :slot *one*))
    (() '(cons foo null) :test (lambda (values expected)
                                 (typep values (first expected)))))
  (checked-compile-and-assert (:optimize :maximally-safe)
      '(lambda () (make-instance 'foo :slot *t*))
    (() (condition 'type-error))))

;;;; Slot type checking for funcallable instances

(defclass foo/gf (sb-mop:standard-generic-function)
  ((slot/gf :initarg :slot/gf :type fixnum :accessor slot/gf))
  (:metaclass sb-mop:funcallable-standard-class))
(defmethod succeed/sv/gf ((x foo/gf))
  (setf (slot-value x 'slot/gf) 1))
(defmethod fail/sv/gf ((x foo/gf))
  (setf (slot-value x 'slot/gf) t))
(defmethod succeed/acc/gf ((x foo/gf))
  (setf (slot/gf x) 1))
(defmethod fail/acc/gf ((x foo/gf))
  (setf (slot/gf x) t))

;;; Test slot type checking for funcallable instances in EVALed code.
(with-test (:name (sb-mop:funcallable-standard-object slot-value setf
                   :initarg type-error eval))
  (eval '(setf (slot-value (make-instance 'foo/gf) 'slot/gf) 1))
  (assert-error
   (eval '(setf (slot-value (make-instance 'foo/gf) 'slot/gf) t))
   type-error)
  (eval '(setf (slot/gf (make-instance 'foo/gf)) 1))
  (assert-error (eval '(setf (slot/gf (make-instance 'foo/gf)) t))
                type-error)
  (eval '(succeed/sv/gf (make-instance 'foo/gf)))
  (assert-error (eval '(fail/sv/gf (make-instance 'foo/gf)))
                type-error)
  (eval '(succeed/acc/gf (make-instance 'foo/gf)))
  (assert-error (eval '(fail/acc/gf (make-instance 'foo/gf)))
                type-error)
  (eval '(make-instance 'foo/gf :slot/gf 1))
  (assert-error (eval '(make-instance 'foo/gf :slot/gf t))
                type-error)
  (eval '(make-instance 'foo/gf :slot/gf *one*))
  (assert-error (eval '(make-instance 'foo/gf :slot/gf *t*))
                type-error))

;;; Test slot type checking for funcallable instances in compiled
;;; code.
(with-test (:name (sb-mop:funcallable-standard-object slot-value setf
                   :initarg type-error compile))
  (checked-compile-and-assert (:optimize :safe)
      '(lambda ()
         (setf (slot-value (make-instance 'foo/gf) 'slot/gf) 1))
    (() 1))
  (checked-compile-and-assert (:optimize :safe)
      '(lambda ()
         (setf (slot-value (make-instance 'foo/gf) 'slot/gf) t))
    (() (condition 'type-error)))

  (checked-compile-and-assert (:optimize :safe)
      '(lambda () (setf (slot/gf (make-instance 'foo/gf)) 1))
    (() 1))
  (checked-compile-and-assert (:optimize :safe)
      '(lambda () (setf (slot/gf (make-instance 'foo/gf)) t))
    (() (condition 'type-error)))

  (checked-compile-and-assert (:optimize :safe)
      '(lambda () (succeed/sv/gf (make-instance 'foo/gf)))
    (() 1))
  (checked-compile-and-assert (:optimize :safe)
      '(lambda ()
         (fail/sv/gf (make-instance 'foo/gf)))
    (() (condition 'type-error)))

  (checked-compile-and-assert (:optimize :safe)
      '(lambda () (succeed/acc/gf (make-instance 'foo/gf)))
    (() 1))
  (checked-compile-and-assert (:optimize :safe)
      '(lambda () (fail/acc/gf (make-instance 'foo/gf)))
    (() (condition 'type-error)))

  ;; FIXME: Comments for corresponding cases in standard instance test
  ;; apply here as well.
  (checked-compile-and-assert (:optimize :maximally-safe)
      '(lambda () (make-instance 'foo/gf :slot/gf 1))
    (() '(cons foo/gf null) :test (lambda (actual expected)
                                    (typep actual (first expected)))))
  (checked-compile-and-assert (:optimize :maximally-safe)
      '(lambda () (make-instance 'foo/gf :slot/gf t))
    (() (condition '(or sb-int:type-warning type-error))))

  (checked-compile-and-assert (:optimize :maximally-safe)
      '(lambda () (make-instance 'foo/gf :slot/gf *one*))
    (() '(cons foo/gf null) :test (lambda (actual expected)
                                    (typep actual (first expected)))))
  (checked-compile-and-assert (:optimize :maximally-safe)
      '(lambda () (make-instance 'foo/gf :slot/gf *t*))
    (() (condition 'type-error))))

;;;; Type checking for inherited slots

(defclass inheritance-a/slot-value/float ()
  ((slot1 :initform 0.0 :type float)))
(defclass inheritance-b/slot-value/float (inheritance-a/slot-value/float)
  ((slot1 :initform 0.0 :type single-float)))
(defmethod inheritance/slot-value/float ((a inheritance-a/slot-value/float))
  (setf (slot-value a 'slot1) 1d0))

(with-test (:name (:slot-inheritance slot-value float single-float))
  (inheritance/slot-value/float
   (make-instance 'inheritance-a/slot-value/float))
  (assert-error (inheritance/slot-value/float
                 (make-instance 'inheritance-b/slot-value/float))
                type-error))

(defclass inheritance-a/slot-value/t ()
  ((slot1 :initform 0.0)))
(defclass inheritance-b/slot-value/t (inheritance-a/slot-value/t)
  ((slot1 :initform 0.0 :type single-float)))
(defmethod inheritance/slot-value/t ((a inheritance-a/slot-value/t))
  (setf (slot-value a 'slot1) 1d0))

(with-test (:name (:slot-inheritance slot-value t single-float))
  (inheritance/slot-value/t (make-instance 'inheritance-a/slot-value/t))
  (assert-error (inheritance/slot-value/t
                 (make-instance 'inheritance-b/slot-value/t))
                type-error))

(defclass inheritance-a/accessor/float ()
  ((slot1 :initform 0.0 :type float :accessor slot1-of)))
(defclass inheritance-b/accessor/float (inheritance-a/accessor/float)
  ((slot1 :initform 0.0 :type single-float)))
(defmethod inheritance/accessor/float ((a inheritance-a/accessor/float))
  (setf (slot1-of a) 1d0))

(with-test (:name (:slot-inheritance :writer float single-float))
  (inheritance/accessor/float
   (make-instance 'inheritance-a/accessor/float))
  (assert-error (inheritance/accessor/float
                 (make-instance 'inheritance-b/accessor/float))
                type-error))

(defclass inheritance-a/accessor/t ()
  ((slot1 :initform 0.0 :accessor slot1-of)))
(defclass inheritance-b/accessor/t (inheritance-a/accessor/t)
  ((slot1 :initform 0.0 :type single-float)))
(defmethod inheritance/accessor/t ((a inheritance-a/accessor/t))
  (setf (slot1-of a) 1d0))

(with-test (:name (:slot-inheritance :writer t single-float))
  (inheritance/accessor/t (make-instance 'inheritance-a/accessor/t))
  (assert-error (inheritance/accessor/t
                 (make-instance 'inheritance-b/accessor/t))
                type-error))

(defclass inheritance-intersection-a* ()
  ((slot1 :initform 1
          :initarg :slot1
          :accessor slot1-of
          :type fixnum)))
(defclass inheritance-intersection-b* ()
  ((slot1 :initform 1
          :initarg :slot1
          :accessor slot1-of
          :type unsigned-byte)))
(defclass inheritance-intersection-c* (inheritance-intersection-a*
                                       inheritance-intersection-b*)
  ())

(with-test (:name (:slot-inheritance :type-intersection))
  (setf (slot1-of (make-instance 'inheritance-intersection-a*)) -1)
  (setf (slot1-of (make-instance 'inheritance-intersection-b*))
        (1+ most-positive-fixnum))
  (setf (slot1-of (make-instance 'inheritance-intersection-c*)) 1)
  (assert-error (setf (slot1-of (make-instance 'inheritance-intersection-c*))
                      -1)
                type-error)
  (assert-error (setf (slot1-of (make-instance 'inheritance-intersection-c*))
                      (1+ most-positive-fixnum))
                type-error)
  (assert-error (make-instance 'inheritance-intersection-c* :slot1 -1)
                type-error)
  (assert-error (make-instance 'inheritance-intersection-c*
                               :slot1 (1+ most-positive-fixnum))
                type-error))

(defclass slot-type-function-a ()
  ((slot1 :initform nil
          :initarg :slot1
          :accessor slot1-of
          :type (or null function))))
(defclass slot-type-function-b (slot-type-function-a)
  ((slot1 :initform nil
          :initarg :slot1
          :accessor slot1-of
          :type (or null (function (fixnum) fixnum)))))

(with-test (:name (:type function type-error))
  (setf (slot1-of (make-instance 'slot-type-function-a)) (lambda () 1))
  (setf (slot1-of (make-instance 'slot-type-function-b)) (lambda () 1))
  (assert-error (setf (slot1-of (make-instance 'slot-type-function-a)) 1)
                type-error)
  (assert-error (setf (slot1-of (make-instance 'slot-type-function-b)) 1)
                type-error)
  (make-instance 'slot-type-function-a :slot1 (lambda () 1))
  (make-instance 'slot-type-function-b :slot1 (lambda () 1)))

(defclass my-alt-metaclass (standard-class) ())
(defmethod sb-mop:validate-superclass ((class my-alt-metaclass) superclass)
  t)
(defclass my-alt-metaclass-instance-class ()
  ((slot :type fixnum :initarg :slot))
  (:metaclass my-alt-metaclass))
(defun make-my-instance (class)
  (make-instance class :slot :not-a-fixnum))

(with-test (:name :alternate-metaclass/standard-instance-structure-protocol)
  (assert-error (make-my-instance 'my-alt-metaclass-instance-class)
                type-error))

(with-test (:name (:typecheck :allocation :class))
  ;; :CLASS slot :INITFORMs are executed at class definition time
  (assert-error
   (eval `(locally (declare (optimize safety))
            (defclass class-allocation-test-bad ()
              ((slot :initform "slot"
                     :initarg :slot
                     :type fixnum
                     :allocation :class)))))
   type-error)
  (let ((name (gensym "CLASS-ALLOCATION-TEST-GOOD")))
    (eval `(locally (declare (optimize safety))
             (defclass ,name ()
               ((slot :initarg :slot
                      :type (integer 100 200)
                      :allocation :class)))))
    (eval
     `(macrolet ((check (form)
                   `(assert (multiple-value-bind (ok err)
                                (ignore-errors ,form)
                              (and (not ok)
                                   (typep err 'type-error)
                                   (equal '(integer 100 200)
                                          (type-error-expected-type err)))))))
        (macrolet ((test (form)
                     `(progn
                        (check (eval '(locally (declare (optimize safety))
                                       ,form)))
                        (check (funcall (checked-compile
                                         '(lambda ()
                                           (declare (optimize safety))
                                           ,form))))))
                   (test-slot (value form)
                     `(progn
                        (assert (eql ,value (slot-value (eval ',form) 'slot)))
                        (assert (eql ,value (slot-value (funcall (checked-compile
                                                                  '(lambda () ,form)))
                                                        'slot))))))
          (test (make-instance ',name :slot :bad))
          (assert (not (slot-boundp (make-instance ',name) 'slot)))
          (let ((* (make-instance ',name :slot 101)))
            (test-slot 101 *)
            (test (setf (slot-value * 'slot) (list 1 2 3)))
            (setf (slot-value * 'slot) 110)
            (test-slot 110 *))
          (test-slot 110 (make-instance ',name))
          (test-slot 111 (make-instance ',name :slot 111)))))))
