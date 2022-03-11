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

;;;; Note that the MOP is not in an entirely supported state.
;;;; However, this seems a good a way as any of ensuring that we have
;;;; no regressions.

(load "compiler-test-util.lisp")
(defpackage "MOP-TEST"
  (:use "CL" "SB-MOP" "ASSERTOID" "TEST-UTIL"))

(in-package "MOP-TEST")

;;; AMOP says these are the defaults
(with-test (:name :standard-direct-superclasses)
  (assert (equal (list (find-class 'standard-object))
                 (sb-mop:class-direct-superclasses (make-instance 'standard-class))))
  (assert (equal (list (find-class 'sb-mop:funcallable-standard-object))
                 (sb-mop:class-direct-superclasses (make-instance 'sb-mop:funcallable-standard-class)))))

;;; Readers for Class Metaobjects (pp. 212--214 of AMOP)
(defclass red-herring (forward-ref) ())

(assert (null (class-direct-slots (find-class 'forward-ref))))
(assert (null (class-direct-default-initargs
               (find-class 'forward-ref))))

;;; Readers for Generic Function Metaobjects (pp. 216--218 of AMOP)
(defgeneric fn-with-odd-arg-precedence (a b c)
  (:argument-precedence-order b c a))

(assert (equal
         (generic-function-lambda-list #'fn-with-odd-arg-precedence)
         '(a b c)))
(assert (equal
         (generic-function-argument-precedence-order #'fn-with-odd-arg-precedence)
         '(b c a)))
;;; Test for DOCUMENTATION's order, which was wrong until sbcl-0.7.8.39
(assert (equal
         (generic-function-argument-precedence-order #'documentation)
         (let ((ll (generic-function-lambda-list #'documentation)))
           (list (nth 1 ll) (nth 0 ll)))))

(assert (null
         (generic-function-declarations #'fn-with-odd-arg-precedence)))
(defgeneric gf-with-declarations (x)
  (declare (optimize (speed 3)))
  (declare (optimize (safety 0))))
(let ((decls (generic-function-declarations #'gf-with-declarations)))
  (assert (= (length decls) 2))
  (assert (member '(optimize (speed 3)) decls :test #'equal))
  (assert (member '(optimize (safety 0)) decls :test #'equal)))

;;; Readers for Slot Definition Metaobjects (pp. 221--224 of AMOP)

;;; Ensure that SLOT-DEFINITION-ALLOCATION returns :INSTANCE/:CLASS as
;;; appropriate.
(defclass sdm-test-class ()
  ((an-instance-slot :accessor an-instance-slot)
   (a-class-slot :allocation :class :accessor a-class-slot)))
(dolist (m (list (list #'an-instance-slot :instance)
                 (list #'a-class-slot :class)))
  (let ((methods (generic-function-methods (car m))))
    (assert (= (length methods) 1))
    (assert (eq (slot-definition-allocation
                 (accessor-method-slot-definition
                  (car methods)))
                (cadr m)))))

;;; Class Finalization Protocol (see section 5.5.2 of AMOP)
(let ((finalized-count 0))
  (defmethod finalize-inheritance :after ((x standard-class))
    (incf finalized-count))
  (defun get-count () finalized-count))
(defclass finalization-test-1 () ())
(make-instance 'finalization-test-1)
(assert (= (get-count) 1))
(defclass finalization-test-2 (finalization-test-3) ())
(assert (= (get-count) 1))
(defclass finalization-test-3 () ())
(make-instance 'finalization-test-3)
(assert (or (= (get-count) 2) (= (get-count) 3)))
(make-instance 'finalization-test-2)
(assert (= (get-count) 3))

;;; Bits of FUNCALLABLE-STANDARD-CLASS are easy to break; make sure
;;; that it is at least possible to define classes with that as a
;;; metaclass.
(defclass gf-class (standard-generic-function) ()
  (:metaclass funcallable-standard-class))
(defgeneric g (a b c)
  (:generic-function-class gf-class))

;;; until sbcl-0.7.12.47, PCL wasn't aware of some direct class
;;; relationships.  These aren't necessarily true, but are probably
;;; not going to change often.
(dolist (x '(number array sequence character symbol))
  (assert (eq (car (class-direct-superclasses (find-class x)))
              (find-class t)))
  (assert (member (find-class x)
                  (class-direct-subclasses (find-class t)))))

;;; the class-prototype of the NULL class used to be some weird
;;; standard-instance-like thing.  Make sure it's actually NIL.
;;;
;;; (and FIXME: eventually turn this into asserting that the prototype
;;; of all built-in-classes is of the relevant type)
(assert (null (class-prototype (find-class 'null))))

;;; simple consistency checks for the SB-MOP package: all of the
;;; functionality specified in AMOP is in functions and classes:
(assert (null (loop for x being each external-symbol in "SB-MOP"
                    unless (or (fboundp x) (find-class x)) collect x)))
;;; and all generic functions in SB-MOP have at least one specified
;;; method, except for UPDATE-DEPENDENT
(assert (null (loop for x being each external-symbol in "SB-MOP"
                    unless (or (not (fboundp x))
                               (eq x 'update-dependent)
                               (not (typep (fdefinition x) 'generic-function))
                               (> (length (generic-function-methods
                                           (fdefinition x)))
                                  0))
                    collect x)))

;;; make sure that ENSURE-CLASS-USING-CLASS's arguments are the right
;;; way round (!)
(defvar *e-c-u-c-arg-order* nil)
(defmethod ensure-class-using-class :after
    (class (name (eql 'e-c-u-c-arg-order)) &key &allow-other-keys)
  (setf *e-c-u-c-arg-order* t))
(defclass e-c-u-c-arg-orderoid () ())
(assert (null *e-c-u-c-arg-order*))
(defclass e-c-u-c-arg-order () ())
(assert (eq *e-c-u-c-arg-order* t))

;;; verify that FIND-CLASS works after FINALIZE-INHERITANCE
(defclass automethod-class (standard-class) ())
(defmethod validate-superclass ((c1 automethod-class) (c2 standard-class))
  t)
(defmethod finalize-inheritance :after ((x automethod-class))
  (format t "~&~S ~S~%" x (find-class (class-name x))))
(defclass automethod-object () ()
  (:metaclass automethod-class))
(defvar *automethod-object* (make-instance 'automethod-object))
(assert (typep *automethod-object* 'automethod-object))

;;; COMPUTE-EFFECTIVE-SLOT-DEFINITION should take three arguments, one
;;; of which is the name of the slot.
(defvar *compute-effective-slot-definition-count* 0)
(defmethod compute-effective-slot-definition :before
    (class (name (eql 'foo)) dsds)
  (incf *compute-effective-slot-definition-count*))
(defclass cesd-test-class ()
  ((foo :initarg :foo)))
(make-instance 'cesd-test-class :foo 3)
;;; FIXME: this assertion seems a little weak.  I don't know why
;;; COMPUTE-EFFECTIVE-SLOT-DEFINITION gets called twice in this
;;; sequence, nor whether that's compliant with AMOP.  -- CSR,
;;; 2003-04-17
(assert (> *compute-effective-slot-definition-count* 0))

;;; this used to cause a nasty uncaught metacircularity in PCL.
(defclass substandard-method (standard-method) ())
(defgeneric substandard-defgeneric (x y)
  (:method-class substandard-method)
  (:method ((x number) (y number)) (+ x y))
  (:method ((x string) (y string)) (concatenate 'string x y)))
(assert (= (substandard-defgeneric 1 2) 3))
(assert (string= (substandard-defgeneric "1" "2") "12"))

(let* ((x (find-class 'pathname))
       (xs (class-direct-subclasses x)))
  (assert (>= (length xs) 1))
  (assert (member (find-class 'logical-pathname) xs)))

;;; BUG 338: "MOP specializers as type specifiers"
;;;  (reported by Bruno Haible sbcl-devel 2004-06-11)
(with-test (:name :eql-specializer-as-type)
  (let* ((m (defmethod eql-specialized-method ((x (eql 4.0))) 3.0))
         (spec (first (sb-mop:method-specializers m))))
    (declare (notinline typep)) ; in case of SSC (sufficiently/super smart compiler)
    (assert (not (typep 1 spec)))
    (assert (typep 4.0 spec))
    ;; TYPEP on spec should not cons. It used to cons 14 words:
    ;;   6 words for %MAKE-MEMBER-TYPE
    ;;   4 words for ALLOC-XSET
    ;;   1 cons in MAKE-EQL-TYPE
    ;;   1 cons in ADD-TO-XSET
    #-interpreter
    (ctu:assert-no-consing (typep 4.0 spec))))

;;; BUG #334, relating to programmatic addition of slots to a class
;;; with COMPUTE-SLOTS.
;;;
;;; FIXME: the DUMMY classes here are to prevent class finalization
;;; before the compute-slots method is around.  This should probably
;;; be done by defining the COMPUTE-SLOTS methods on a metaclass,
;;; which can be defined before.
;;;
;;; a. adding an :allocation :instance slot
(defclass class-to-add-instance-slot (dummy-ctais) ())
(defmethod compute-slots ((c (eql (find-class 'class-to-add-instance-slot))))
  (append (call-next-method)
          (list (make-instance 'standard-effective-slot-definition
                               :name 'y
                               :allocation :instance))))
(defclass dummy-ctais () ((x :allocation :class)))
(finalize-inheritance (find-class 'class-to-add-instance-slot))
(assert (equal (mapcar #'slot-definition-allocation
                       (class-slots (find-class 'class-to-add-instance-slot)))
               ;; FIXME: is the order really guaranteed?
               '(:class :instance)))
(assert (typep (slot-definition-location
                (cadr (class-slots (find-class 'class-to-add-instance-slot))))
               'unsigned-byte))
#| (assert (typep (slot-definition-location (car ...)) '???)) |#
(let ((x (make-instance 'class-to-add-instance-slot)))
  (assert (not (slot-boundp x 'x)))
  (setf (slot-value x 'x) t)
  (assert (not (slot-boundp x 'y)))
  (setf (slot-value x 'y) 1)
  (assert (= 1 (slot-value x 'y))))
(let ((x (make-instance 'class-to-add-instance-slot)))
  (assert (slot-boundp x 'x))
  (assert (eq t (slot-value x 'x)))
  (assert (not (slot-boundp x 'y))))

;;; b. adding an :allocation :class slot
(defclass class-to-add-class-slot (dummy-ctacs) ())
(defmethod compute-slots ((c (eql (find-class 'class-to-add-class-slot))))
  (append (call-next-method)
          (list (make-instance 'standard-effective-slot-definition
                               :name 'y
                               :allocation :class))))
(defclass dummy-ctacs () ((x :allocation :class)))
(finalize-inheritance (find-class 'class-to-add-class-slot))
(assert (equal (mapcar #'slot-definition-allocation
                       (class-slots (find-class 'class-to-add-class-slot)))
               '(:class :class)))
(let ((x (make-instance 'class-to-add-class-slot)))
  (assert (not (slot-boundp x 'x)))
  (setf (slot-value x 'x) nil)
  (assert (not (slot-boundp x 'y)))
  (setf (slot-value x 'y) 1)
  (assert (= 1 (slot-value x 'y))))
(let ((x (make-instance 'class-to-add-class-slot)))
  (assert (slot-boundp x 'x))
  (assert (eq nil (slot-value x 'x)))
  (assert (slot-boundp x 'y))
  (assert (= 1 (slot-value x 'y))))
;;; extra paranoia: check that we haven't broken the instance-slot class
(let ((x (make-instance 'class-to-add-instance-slot)))
  (assert (slot-boundp x 'x))
  (assert (eq t (slot-value x 'x)))
  (assert (not (slot-boundp x 'y))))

;;;; the CTOR optimization was insufficiently careful about its
;;;; assumptions: firstly, it failed with a failed AVER for
;;;; non-standard-allocation slots:
(defclass class-with-frob-slot ()
  ((frob-slot :initarg :frob-slot :allocation :frob)))
(handler-case
    (funcall (compile nil '(lambda ()
                            (make-instance 'class-with-frob-slot
                             :frob-slot 1))))
  (sb-int:bug (c) (error c))
  (error () "Probably OK: haven't implemented SLOT-BOUNDP-USING-CLASS"))
;;; secondly, it failed to take account of the fact that we might wish
;;; to customize (setf slot-value-using-class)
(defclass class-with-special-ssvuc ()
  ((some-slot :initarg :some-slot)))
(defvar *special-ssvuc-counter* 0)
(defmethod (setf slot-value-using-class) :before
    (new-value class (instance class-with-special-ssvuc) slotd)
  (incf *special-ssvuc-counter*))
(let ((fun (compile nil '(lambda () (make-instance 'class-with-special-ssvuc
                                     :some-slot 1)))))
  (assert (= *special-ssvuc-counter* 0))
  (funcall fun)
  (assert (= *special-ssvuc-counter* 1))
  (funcall fun)
  (assert (= *special-ssvuc-counter* 2)))
;;; and now with the customization after running the function once
(defclass class-with-special-ssvuc-2 ()
  ((some-slot :initarg :some-slot)))
(defvar *special-ssvuc-counter-2* 0)
(let ((fun (compile nil '(lambda () (make-instance 'class-with-special-ssvuc-2
                                     :some-slot 1)))))
  (assert (= *special-ssvuc-counter-2* 0))
  (funcall fun)
  (assert (= *special-ssvuc-counter-2* 0))
  (defmethod (setf slot-value-using-class) :before
      (new-value class (instance class-with-special-ssvuc-2) slotd)
    (incf *special-ssvuc-counter-2*))
  (funcall fun)
  (assert (= *special-ssvuc-counter-2* 1)))

;;; vicious metacycle detection and resolution wasn't good enough: it
;;; didn't take account that the slots (and hence the slot readers)
;;; might be inherited from superclasses.  This example, due to Bruno
;;; Haible, also tests programmatic addition of accessors.
(defclass auto-accessors-direct-slot-definition-class (standard-class)
  ((containing-class-name :initarg :containing-class-name)))
(defmethod validate-superclass
    ((c1 auto-accessors-direct-slot-definition-class) (c2 standard-class))
  t)
(defclass auto-accessors-class (standard-class)
  ())
(defmethod direct-slot-definition-class ((class auto-accessors-class)
                                         &rest initargs)
  (declare (ignore initargs))
  (let ((dsd-class-name (gensym)))
    (sb-mop:ensure-class
     dsd-class-name
     :metaclass 'auto-accessors-direct-slot-definition-class
     :direct-superclasses (list (find-class 'standard-direct-slot-definition))
     :containing-class-name (class-name class))
    (eval `(defmethod initialize-instance :after ((dsd ,dsd-class-name)
                                                  &rest args)
            (declare (ignore args))
            (when (and (null (slot-definition-readers dsd))
                       (null (slot-definition-writers dsd)))
              (let* ((containing-class-name
                      (slot-value (class-of dsd) 'containing-class-name))
                     (accessor-name
                      (intern
                       (concatenate 'string
                                    (symbol-name containing-class-name)
                                    "-"
                                    (symbol-name (slot-definition-name dsd)))
                       (symbol-package containing-class-name))))
                (setf (slot-definition-readers dsd) (list accessor-name))
                (setf (slot-definition-writers dsd)
                      (list (list 'setf accessor-name)))))))
    (find-class dsd-class-name)))
(defmethod validate-superclass ((c1 auto-accessors-class) (c2 standard-class))
  t)
(defclass testclass15 ()
  ((x :initarg :x) (y))
  (:metaclass auto-accessors-class))
(let ((inst (make-instance 'testclass15 :x 12)))
  (assert (equal (list (testclass15-x inst) (setf (testclass15-y inst) 13))
                 '(12 13))))

;;; bug reported by Bruno Haible on sbcl-devel 2004-11-17: incorrect
;;; handling of multiple values for non-standard slot-options
(progn
  (defclass option-slot-definition (sb-mop:standard-direct-slot-definition)
    ((option :accessor sl-option :initarg :my-option)))
  (defclass option-slot-class (standard-class)
    ())
  (defmethod sb-mop:direct-slot-definition-class
      ((c option-slot-class) &rest args)
    (declare (ignore args))
    (find-class 'option-slot-definition))
  (defmethod sb-mop:validate-superclass
      ((c1 option-slot-class) (c2 standard-class))
    t)
  (eval '(defclass test-multiple-slot-option-bug ()
          ((x :my-option bar :my-option baz))
          (:metaclass option-slot-class)))
  (assert (null (set-difference
                 '(bar baz)
                 (sl-option (first (sb-mop:class-direct-slots
                                    (find-class 'test-multiple-slot-option-bug))))))))

;;; bug reported by Bruno Haible on sbcl-devel 2004-11-19: AMOP requires
;;; that CLASS-PROTOYPE signals an error if the class is not yet finalized
(defclass prototype-not-finalized-sub (prototype-not-finalized-super) ())
(multiple-value-bind (val err)
    (ignore-errors (sb-mop:class-prototype (find-class 'prototype-not-finalized-super)))
  (assert (null val))
  (assert (typep err 'error)))

;;; AMOP says so
(with-test (:name (allocate-instance built-in-class error))
  (dolist (class-name '(fixnum bignum symbol t))
    (let ((class (find-class class-name)))
      ;; actually T can't be a built-in-class
      (when (typep class 'built-in-class)
        (multiple-value-bind (value error)
            (ignore-errors (allocate-instance class))
          (assert (null value))
          (assert (typep error 'error)))))))

;;; bug reported by David Morse: direct-subclass update protocol was broken
(defclass vegetable () ())
(defclass tomato (vegetable) ())
(assert (equal (list (find-class 'tomato)) (sb-mop:class-direct-subclasses (find-class 'vegetable))))
(defclass tomato () ())
(assert (null (sb-mop:class-direct-subclasses (find-class 'vegetable))))

;;; bug 331: lazy creation of clos classes for defstructs
(defstruct bug-331-super)
(defstruct (bug-331-sub (:include bug-331-super)))
(let ((subs (sb-mop:class-direct-subclasses (find-class 'bug-331-super))))
  (assert (= 1 (length subs)))
  (assert (eq (car subs) (find-class 'bug-331-sub))))
;;; (addendum to test for #331: conditions suffered the same problem)
(define-condition condition-bug-331-super () ())
(define-condition condition-bug-331-sub (condition-bug-331-super) ())
(let ((subs (sb-mop:class-direct-subclasses
             (find-class 'condition-bug-331-super))))
  (assert (= 1 (length subs)))
  (assert (eq (car subs) (find-class 'condition-bug-331-sub))))
;;; (addendum to the addendum: the fix for this revealed breakage in
;;; REINITIALIZE-INSTANCE)
(define-condition condition-bug-331a () ((slot331a :reader slot331a)))
(reinitialize-instance (find-class 'condition-bug-331a))
(let* ((gf #'slot331a)
       (methods (sb-mop:generic-function-methods gf)))
  (assert (= (length methods) 1))
  (assert (eq (car methods)
              (find-method #'slot331a nil
                           (list (find-class 'condition-bug-331a))))))

;;; detection of multiple class options in defclass, reported by Bruno Haible
(defclass option-class (standard-class)
  ((option :accessor cl-option :initarg :my-option)))
(defmethod sb-mop:validate-superclass ((c1 option-class) (c2 standard-class))
  t)
(multiple-value-bind (result error)
    (ignore-errors (eval '(defclass option-class-instance ()
                           ()
                           (:my-option bar)
                           (:my-option baz)
                           (:metaclass option-class))))
  (assert (not result))
  (assert error))

;;; class as :metaclass
(assert (typep
         (sb-mop:ensure-class-using-class
          nil 'class-as-metaclass-test
          :metaclass (find-class 'standard-class)
          :name 'class-as-metaclass-test
          :direct-superclasses (list (find-class 'standard-object)))
         'class))

;;; COMPUTE-DEFAULT-INITARGS protocol mismatch reported by Bruno
;;; Haible
(defparameter *extra-initarg-value* 'extra)
(defclass custom-default-initargs-class (standard-class)
  ())
(defmethod compute-default-initargs ((class custom-default-initargs-class))
  (let ((original-default-initargs
         (remove-duplicates
          (reduce #'append
                  (mapcar #'class-direct-default-initargs
                          (class-precedence-list class)))
          :key #'car
          :from-end t)))
    (cons (list ':extra '*extra-initarg-value* #'(lambda () *extra-initarg-value*))
          (remove ':extra original-default-initargs :key #'car))))
(defmethod validate-superclass ((c1 custom-default-initargs-class)
                                (c2 standard-class))
  t)
(defclass extra-initarg ()
  ((slot :initarg :extra))
  (:metaclass custom-default-initargs-class))
(assert (eq (slot-value (make-instance 'extra-initarg) 'slot) 'extra))

;;; STANDARD-CLASS valid as a superclass for FUNCALLABLE-STANDARD-CLASS
(defclass standard-class-for-fsc ()
  ((scforfsc-slot :initarg :scforfsc-slot :accessor scforfsc-slot)))
(defvar *standard-class-for-fsc*
  (make-instance 'standard-class-for-fsc :scforfsc-slot 1))
(defclass fsc-with-standard-class-superclass
    (standard-class-for-fsc funcallable-standard-object)
  ((fsc-slot :initarg :fsc-slot :accessor fsc-slot))
  (:metaclass funcallable-standard-class))
(defvar *fsc/scs*
  (make-instance 'fsc-with-standard-class-superclass
                 :scforfsc-slot 2
                 :fsc-slot 3))
(assert (= (scforfsc-slot *standard-class-for-fsc*) 1))
(assert (= (scforfsc-slot *fsc/scs*) 2))
(assert (= (fsc-slot *fsc/scs*) 3))
(assert (subtypep 'fsc-with-standard-class-superclass 'function))
(assert (not (subtypep 'standard-class-for-fsc 'function)))

;;; also check that our sanity check for functionness is good
(assert-error
 (progn
   (defclass bad-standard-class (funcallable-standard-object)
     ()
     (:metaclass standard-class))
   (make-instance 'bad-standard-class)))
(assert-error
 (progn
   (defclass bad-funcallable-standard-class (standard-object)
     ()
     (:metaclass funcallable-standard-class))
   (make-instance 'bad-funcallable-standard-class)))

;;; we should be able to make classes with silly names
(make-instance 'standard-class :name 3)
(defclass foo () ())
(reinitialize-instance (find-class 'foo) :name '(a b))

;;; classes (including anonymous ones) and eql-specializers should be
;;; allowed to be specializers.
(defvar *anonymous-class*
  (make-instance 'standard-class
                 :direct-superclasses (list (find-class 'standard-object))))
(defvar *object-of-anonymous-class*
  (make-instance *anonymous-class*))
(eval `(defmethod method-on-anonymous-class ((obj ,*anonymous-class*)) 41))
(assert (eql (method-on-anonymous-class *object-of-anonymous-class*) 41))
(eval `(defmethod method-on-anonymous-class
        ((obj ,(intern-eql-specializer *object-of-anonymous-class*)))
        42))
(assert (eql (method-on-anonymous-class *object-of-anonymous-class*) 42))

;;; accessors can cause early finalization, which caused confusion in
;;; the system, leading to uncompileable TYPEP problems.
(defclass funcallable-class-for-typep ()
  ((some-slot-with-accessor :accessor some-slot-with-accessor))
  (:metaclass funcallable-standard-class))
(compile nil '(lambda (x) (typep x 'funcallable-class-for-typep)))

;;; even anonymous classes should be valid types
(let* ((class1 (make-instance 'standard-class :direct-superclasses (list (find-class 'standard-object))))
       (class2 (make-instance 'standard-class :direct-superclasses (list class1))))
  (assert (subtypep class2 class1))
  (assert (typep (make-instance class2) class1)))

;;; ensure-class got its treatment of :metaclass wrong.
(ensure-class 'better-be-standard-class :direct-superclasses '(standard-object)
              :metaclass 'standard-class
              :metaclass 'funcallable-standard-class)
(assert (eq (class-of (find-class 'better-be-standard-class))
            (find-class 'standard-class)))

;;; CLASS-SLOTS should signal an error for classes that are not yet
;;; finalized. Reported by Levente Meszaros on sbcl-devel.
(defclass has-slots-but-isnt-finalized () (a b c))
(let ((class (find-class 'has-slots-but-isnt-finalized)))
  (assert (not (sb-mop:class-finalized-p class)))
  (assert-error (sb-mop:class-slots class) sb-kernel::reference-condition))

;;; Check that MAKE-METHOD-LAMBDA which wraps the original body doesn't
;;; break RETURN-FROM.
(defclass wrapped-generic (standard-generic-function)
  ()
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod sb-mop:make-method-lambda ((gf wrapped-generic) method lambda env)
  (call-next-method gf method
                    `(lambda ,(second lambda)
                       (flet ((default () :default))
                         ,@(cddr lambda)))
                    env))

(defgeneric wrapped (x)
  (:generic-function-class wrapped-generic))

(defmethod wrapped ((x cons))
  (return-from wrapped (default)))

(with-test (:name :make-method-lambda-wrapping+return-from)
  (assert (eq :default (wrapped (cons t t)))))

;; This test tests something a little shady - that a method
;; are accessible by way of FDEFNs. They aren't all.
;; See the comment in src/pcl/low.lisp at SET-FUN-NAME.
(with-test (:name :slow-method-is-fboundp)
  (assert (fboundp '(sb-pcl::slow-method wrapped (cons))))
  (assert (eq :default (funcall #'(sb-pcl::slow-method wrapped (cons)) (list (cons t t)) nil))))

;;; Check that SLOT-BOUNDP-USING-CLASS doesn't confuse MAKE-INSTANCE
;;; optimizations.
(defclass sbuc-mio-test-class (standard-class)
  ())
(defmethod validate-superclass ((class sbuc-mio-test-class)
                                (superclass standard-class))
  t)
(defvar *sbuc-counter* 0)
(defmethod slot-boundp-using-class ((class sbuc-mio-test-class)
                                    (object t)
                                    (slot standard-effective-slot-definition))
  (incf *sbuc-counter*)
  (call-next-method))
(defclass sbuc-mio-test-object ()
  ((slot :initform 5 :accessor a-slot))
  (:metaclass sbuc-mio-test-class))
(with-test (:name :sbuc-mio-test)
  (assert (= 5 (funcall
                (compile
                 nil
                 `(lambda ()
                    (let ((object (make-instance 'sbuc-mio-test-object)))
                      (slot-value object 'slot)))))))
  (assert (= 1 *sbuc-counter*)))

;;; Redefining classes so that slot definition class changes.
(defclass func-slot-class (standard-class)
  ())

(defmethod sb-mop:validate-superclass ((class func-slot-class) (super standard-class))
  t)

(defclass func-slot-definition ()
  ((function :initform nil :initarg :function :reader slotd-function)))

(defclass effective-func-slot-definition (sb-mop:standard-effective-slot-definition
                                          func-slot-definition)
  ())

(defclass direct-func-slot-definition (sb-mop:standard-direct-slot-definition
                                       func-slot-definition)
  ())

(defmethod sb-mop:slot-value-using-class ((class func-slot-class)
                                          instance
                                          (slotd effective-func-slot-definition))
  (funcall (slotd-function slotd) (call-next-method)))

(defvar *func-slot*)

(defmethod sb-mop:effective-slot-definition-class ((class func-slot-class) &key)
  (if *func-slot*
      (find-class 'effective-func-slot-definition)
      (call-next-method)))

(defmethod sb-mop:direct-slot-definition-class ((class func-slot-class) &key)
  (find-class 'direct-func-slot-definition))

(defmethod sb-mop:compute-effective-slot-definition ((class func-slot-class) name dslotds)
  (let* ((*func-slot* (some #'slotd-function dslotds))
         (slotd (call-next-method)))
    (when *func-slot*
      (setf (slot-value slotd 'function) (fdefinition *func-slot*)))
    slotd))

(with-test (:name :class-redefinition-changes-custom-slot-type)
  (eval `(defclass func-slot-object ()
           ((foo :initarg :foo :reader foofoo))
           (:metaclass func-slot-class)))
  (let ((x (cons t t)))
    (assert (eq x (foofoo (make-instance 'func-slot-object :foo x)))))
  (eval `(defclass func-slot-object ()
           ((foo :initarg :foo :reader foofoo :function car))
           (:metaclass func-slot-class)))
  (let* ((x (cons t t))
         (y (list x)))
    (assert (eq x (foofoo (make-instance 'func-slot-object :foo y))))))

(with-test (:name :class-redefinition-changes-custom-slot-type-mio)
  (eval `(defclass func-slot-object2 ()
           ((foo :initarg :foo :reader foofoo))
           (:metaclass func-slot-class)))
  (let* ((x (cons t t))
         (y (cons x x))
         (o (make-instance 'func-slot-object2 :foo y)))
    (assert (eq y (foofoo o)))
    (eval `(defclass func-slot-object2 ()
           ((foo :initarg :foo :reader foofoo :function car))
           (:metaclass func-slot-class)))
    (assert (eq x (foofoo o)))))

(defclass class-slot-removal-test ()
  ((instance :initform 1)
   (class :allocation :class :initform :ok)))

(defmethod update-instance-for-redefined-class ((x class-slot-removal-test) added removed plist &rest inits)
  (throw 'update-instance
    (list added removed plist inits)))

(with-test (:name :class-redefinition-removes-class-slot)
  (let ((o (make-instance 'class-slot-removal-test)))
    (assert (equal '(nil nil nil nil)
                   (catch 'update-instance
                     (eval `(defclass class-slot-removal-test ()
                              ((instance :initform 2))))
                     (slot-value o 'instance))))))

(defclass class-slot-add-test ()
  ((instance :initform 1)))

(defmethod update-instance-for-redefined-class ((x class-slot-add-test) added removed plist &rest inits)
  (throw 'update-instance
    (list added removed plist inits)))

(with-test (:name :class-redefinition-adds-class-slot)
  (let ((o (make-instance 'class-slot-add-test)))
    (assert (equal '(nil nil nil nil)
                   (catch 'update-instance
                     (eval `(defclass class-slot-add-test ()
                              ((instance :initform 2)
                               (class :allocation :class :initform :ok))))
                     (slot-value o 'instance))))))

(defgeneric definitely-a-funcallable-instance (x))
(with-test (:name (set-funcallable-instance-function :typechecking))
  (assert-error (set-funcallable-instance-function
                  (lambda (y) (declare (ignore y)) nil)
                  #'definitely-a-funcallable-instance)
                 type-error))

(with-test (:name (defstruct :nil-slot-name :bug-633911))
  (defstruct nil-slot-name nil)
  (let ((fun (compile nil '(lambda (x) (slot-value x 'nil)))))
    (assert (= 3 (funcall fun (make-nil-slot-name :nil 3))))))

;;; Duplicated initargs in effective slot definition

(defclass duplicated-intiargs.a () ((a :initarg :a)))
(defclass duplicated-intiargs.b () ((a :initarg :a)))
(defclass duplicated-intiargs.c (duplicated-intiargs.a duplicated-intiargs.b) ())

(with-test (:name (compute-effective-slot-definition :duplicated-intiargs))
  (let* ((class (sb-pcl:ensure-class-finalized (find-class 'duplicated-intiargs.c)))
         (slot (first (class-slots class))))
    (assert (equal (slot-definition-initargs slot) '(:a)))))


(defclass change-class-test-m (standard-class) ())
(defmethod validate-superclass ((c1 change-class-test-m) (c2 standard-class))
  t)

(defmethod slot-value-using-class ((class change-class-test-m) object slot)
  (call-next-method))

(defclass change-class-test ()
  ((a :initarg :a)
   (b :initarg :b
      :initform nil))
  (:metaclass change-class-test-m))

(defclass change-class-test-2 ()
  ((a :initarg :a
      :initform nil)
   (b :initarg :b
      :initform nil))
  (:metaclass change-class-test-m))

(with-test (:name :change-class-svuc)
  (let ((new (change-class (make-instance 'change-class-test :b 20)
                           'change-class-test-2)))
    (assert (eql (slot-value new 'b) 20))
    (assert (not (slot-boundp new 'a)))))

(defclass chgclass-dx-test-1 () ())
(defclass chgclass-dx-test-2 () ())
(defclass chgclass-dx-test-fin-1 (funcallable-standard-object) ()
  (:metaclass funcallable-standard-class))
(defclass chgclass-dx-test-fin-2 (funcallable-standard-object
                                  chgclass-dx-test-2)
  ()
  (:metaclass funcallable-standard-class))
(defvar *uifdc-called*)
(defmethod update-instance-for-different-class :before (old (new chgclass-dx-test-2)
                                                        &rest initargs)
  (declare (ignore initargs))
  (setq *uifdc-called* t)
  (assert (sb-ext:stack-allocated-p old))
  (when (functionp new)
    (assert (sb-ext:stack-allocated-p (sb-kernel:%funcallable-instance-fun old)))))
(with-test (:name :change-class-temp-on-stack)
  (let ((i (make-instance 'chgclass-dx-test-1))
        (*uifdc-called* nil))
    (change-class i 'chgclass-dx-test-2)
    (assert *uifdc-called*))
  (let ((i (make-instance 'chgclass-dx-test-fin-1))
        (*uifdc-called* nil))
    (change-class i 'chgclass-dx-test-fin-2)
    (assert *uifdc-called*)))
