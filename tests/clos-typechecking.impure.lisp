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

(defclass foo ()
  ((slot :initarg :slot :type fixnum :accessor slot)))
(defclass foo/gf (sb-mop:standard-generic-function)
  ((slot/gf :initarg :slot/gf :type fixnum :accessor slot/gf))
  (:metaclass sb-mop:funcallable-standard-class))
(defmethod succeed/sv ((x foo))
  (setf (slot-value x 'slot) 1))
(defmethod fail/sv ((x foo))
  (setf (slot-value x 'slot) t))
(defmethod succeed/acc ((x foo))
  (setf (slot x) 1))
(defmethod fail/acc ((x foo))
  (setf (slot x) t))
(defmethod succeed/sv/gf ((x foo/gf))
  (setf (slot-value x 'slot/gf) 1))
(defmethod fail/sv/gf ((x foo/gf))
  (setf (slot-value x 'slot/gf) t))
(defmethod succeed/acc/gf ((x foo/gf))
  (setf (slot/gf x) 1))
(defmethod fail/acc/gf ((x foo/gf))
  (setf (slot/gf x) t))
(defvar *t* t)
(defvar *one* 1)

;; evaluator
(with-test (:name (:evaluator))
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
;; evaluator/gf
(with-test (:name (:evaluator/gf))
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

;; compiler
(with-test (:name (:compiler))
  (funcall (compile nil '(lambda ()
                          (setf (slot-value (make-instance 'foo) 'slot) 1))))
  (funcall (compile nil '(lambda () (setf (slot (make-instance 'foo)) 1))))
  (assert-error
   (funcall
    (compile nil '(lambda () (setf (slot (make-instance 'foo)) t))))
   type-error)
  (funcall (compile nil '(lambda () (succeed/sv (make-instance 'foo)))))
  (assert-error
   (funcall (compile nil '(lambda () (fail/sv (make-instance 'foo)))))
   type-error)
  (funcall (compile nil '(lambda () (succeed/acc (make-instance 'foo)))))
  (assert-error
   (funcall (compile nil '(lambda () (fail/acc (make-instance 'foo)))))
   type-error)
  (funcall (compile nil '(lambda () (make-instance 'foo :slot 1))))
  (assert-error
   (funcall (compile nil '(lambda () (make-instance 'foo :slot t))))
   type-error)
  (funcall (compile nil '(lambda () (make-instance 'foo :slot *one*))))
  (assert-error
   (funcall (compile nil '(lambda () (make-instance 'foo :slot *t*))))
   type-error))

(with-test (:name (:compiler :setf :slot-value))
  (assert-error
   (funcall
    (compile nil '(lambda ()
                   (setf (slot-value (make-instance 'foo) 'slot) t))))
   type-error))

; compiler/gf
(with-test (:name (:compiler/gf))
  (funcall (compile nil
                    '(lambda ()
                      (setf (slot-value (make-instance 'foo/gf) 'slot/gf) 1))))
  (funcall (compile nil '(lambda () (setf (slot/gf (make-instance 'foo/gf)) 1))))
  (assert-error
   (funcall
    (compile nil
             '(lambda () (setf (slot/gf (make-instance 'foo/gf)) t))))
   type-error)
  (funcall (compile nil '(lambda () (succeed/sv/gf (make-instance 'foo/gf)))))
  (assert-error
   (funcall (compile nil '(lambda ()
                           (fail/sv/gf (make-instance 'foo/gf)))))
   type-error)
  (funcall (compile nil '(lambda () (succeed/acc/gf (make-instance 'foo/gf)))))
  (assert-error
   (funcall (compile nil '(lambda ()
                           (fail/acc/gf (make-instance 'foo/gf)))))
   type-error)
  (funcall (compile nil '(lambda () (make-instance 'foo/gf :slot/gf 1))))
  (assert-error
   (funcall (compile nil '(lambda ()
                           (make-instance 'foo/gf :slot/gf t))))
   type-error)
  (funcall (compile nil '(lambda () (make-instance 'foo/gf :slot/gf *one*))))
  (assert-error
   (funcall (compile nil '(lambda ()
                           (make-instance 'foo/gf :slot/gf *t*))))
   type-error))

(with-test (:name (:compiler/gf :setf :slot-value))
  (assert-error
   (funcall
    (compile nil
             '(lambda ()
               (setf (slot-value (make-instance 'foo/gf) 'slot/gf) t))))
   type-error))


(with-test (:name (:slot-inheritance :slot-value :float/single-float))
  (defclass a () ((slot1 :initform 0.0 :type float)))
  (defclass b (a) ((slot1 :initform 0.0 :type single-float)))
  (defmethod inheritance-test ((a a)) (setf (slot-value a 'slot1) 1d0))
  (inheritance-test (make-instance 'a))
  (assert-error (inheritance-test (make-instance 'b)) type-error))

(with-test (:name (:slot-inheritance :slot-value :t/single-float))
  (defclass a () ((slot1 :initform 0.0)))
  (defclass b (a) ((slot1 :initform 0.0 :type single-float)))
  (defmethod inheritance-test ((a a)) (setf (slot-value a 'slot1) 1d0))
  (inheritance-test (make-instance 'a))
  (assert-error (inheritance-test (make-instance 'b)) type-error))

(with-test (:name (:slot-inheritance :writer :float/single-float))
  (defclass a () ((slot1 :initform 0.0 :type float :accessor slot1-of)))
  (defclass b (a) ((slot1 :initform 0.0 :type single-float)))
  (defmethod inheritance-test ((a a)) (setf (slot1-of a) 1d0))
  (inheritance-test (make-instance 'a))
  (assert-error (inheritance-test (make-instance 'b)) type-error))

(with-test (:name (:slot-inheritance :writer :float/single-float))
  (defclass a () ((slot1 :initform 0.0 :accessor slot1-of)))
  (defclass b (a) ((slot1 :initform 0.0 :type single-float)))
  (defmethod inheritance-test ((a a)) (setf (slot1-of a) 1d0))
  (inheritance-test (make-instance 'a))
  (assert-error (inheritance-test (make-instance 'b)) type-error))

(with-test (:name (:slot-inheritance :type-intersection))
  (defclass a* ()
    ((slot1 :initform 1
            :initarg :slot1
            :accessor slot1-of
            :type fixnum)))
  (defclass b* ()
    ((slot1 :initform 1
            :initarg :slot1
            :accessor slot1-of
            :type unsigned-byte)))
  (defclass c* (a* b*)
    ())
  (setf (slot1-of (make-instance 'a*)) -1)
  (setf (slot1-of (make-instance 'b*)) (1+ most-positive-fixnum))
  (setf (slot1-of (make-instance 'c*)) 1)
  (assert-error (setf (slot1-of (make-instance 'c*)) -1)
                type-error)
  (assert-error (setf (slot1-of (make-instance 'c*))
                      (1+ most-positive-fixnum))
                type-error)
  (assert-error (make-instance 'c* :slot1 -1)
                type-error)
  (assert-error (make-instance 'c* :slot1 (1+ most-positive-fixnum))
                type-error))

(defclass a ()
  ((slot1 :initform nil
          :initarg :slot1
          :accessor slot1-of
          :type (or null function))))
(defclass b (a)
  ((slot1 :initform nil
          :initarg :slot1
          :accessor slot1-of
          :type (or null (function (fixnum) fixnum)))))

(with-test (:name (:type :function))
  (setf (slot1-of (make-instance 'a)) (lambda () 1))
  (setf (slot1-of (make-instance 'b)) (lambda () 1))
  (assert-error (setf (slot1-of (make-instance 'a)) 1)
                type-error)
  (assert-error (setf (slot1-of (make-instance 'b)) 1)
                type-error)
  (make-instance 'a :slot1 (lambda () 1))
  (make-instance 'b :slot1 (lambda () 1)))

(with-test (:name :alternate-metaclass/standard-instance-structure-protocol)
  (defclass my-alt-metaclass (standard-class) ())
  (defmethod sb-mop:validate-superclass ((class my-alt-metaclass) superclass)
    t)
  (defclass my-alt-metaclass-instance-class ()
    ((slot :type fixnum :initarg :slot))
    (:metaclass my-alt-metaclass))
  (defun make-my-instance (class)
    (make-instance class :slot :not-a-fixnum))
  (assert-error (make-my-instance 'my-alt-metaclass-instance-class)
                type-error))

(with-test (:name :typecheck-class-allocation)
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
                        (check (funcall (compile nil '(lambda ()
                                                       (declare (optimize safety))
                                                       ,form))))))
                   (test-slot (value form)
                     `(progn
                        (assert (eql ,value (slot-value (eval ',form) 'slot)))
                        (assert (eql ,value (slot-value (funcall (compile nil '(lambda () ,form)))
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
