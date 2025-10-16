;;;; Tests of SLOT-VALUE

(load "compiler-test-util.lisp")
(defpackage "SLOT-VALUE-TEST"
  (:use "CL" "SB-MOP" "ASSERTOID" "TEST-UTIL"))

(in-package "SLOT-VALUE-TEST")

(defclass a-class ()
  ((x :initform 123)))

(defun a-class-x-0 (a)
  (slot-value a 'x))

(defmethod a-class-x-1 ((a a-class))
  (slot-value a 'x))

(defmethod a-class-x-2 ((a a-class))
  (let ((%a a))
    (slot-value %a 'x)))

(defmethod a-class-x-3 ((a t))
  (slot-value a 'x))

(with-test (:name (slot-value defun))
  (assert (= (a-class-x-0 (make-instance 'a-class)) 123)))

(with-test (:name (slot-value defmethod))
  (assert (= (a-class-x-1 (make-instance 'a-class)) 123)))

(with-test (:name (slot-value defmethod let))
  (assert (= (a-class-x-2 (make-instance 'a-class)) 123)))

(with-test (:name (slot-value defmethod t))
  (assert (= (a-class-x-3 (make-instance 'a-class)) 123)))

(defun read-slot-way1 (instance slot)
  (slot-value instance slot))
(defun read-slot-way2 (instance slot)
  (slot-value (the structure-object instance) slot))
(compile 'read-slot-way1)
(compile 'read-slot-way2)

;;; Collect up a bunch of instances that are subtypes of STRUCTURE-OBJECT
;;; I've seen mismatch occur in the SLOT-VALUE calls, so I've inserted logic
;;; to exclude expected faillures.
;;; (I guess the finalizer thread manipulates hash-tables while this thread runs?)
(with-test (:name :fast-structure-slot-value)
  (let ((instances
         (sb-vm:list-allocated-objects
          :all :type sb-vm:instance-widetag
          :test (lambda (x)
                  (and (typep x 'structure-object)
                       ;; want to ensure the instances under test are mostly immutable.
                       ;; This is no guarantee, but it works.
                       (eq (sb-kernel:generation-of x)
                           sb-vm:+pseudo-static-generation+)))
          :count 10000)))
    (dolist (x instances)
      (let* ((layout (sb-kernel:%instance-layout x))
             (dd (sb-kernel:layout-dd layout)))
        (dolist (dsd (sb-kernel:dd-slots dd))
          (let ((slot-name (sb-kernel:dsd-name dsd)))
            ;; some slots may be raw, don't worry about being EQ
            (unless (eql (read-slot-way1 x slot-name)
                         (read-slot-way2 x slot-name))
              (unless (and (hash-table-p x) (eq slot-name 'sb-impl::cache))
                (error "Failed on ~s ~s" x slot-name)))))))))

(defstruct a-struct x)
(defstruct (another-struct (:include a-struct)) y)

(defmethod read-a-struct-x ((o a-struct))
  (slot-value o 'x))
(defmethod read-a-struct-y ((o a-struct))
  (slot-value o 'y))

(with-test (:name :fast-structure-slot-value-in-method)
  (assert (eql (read-a-struct-x (make-a-struct :x 42)) 42))
  (assert (eql (read-a-struct-x (make-another-struct :x 43 :y 44)) 43))
  (assert-error (read-a-struct-y (make-a-struct :x 45)) sb-pcl::missing-slot)
  (assert (eql (read-a-struct-y (make-another-struct :x 46 :y 47)) 47)))

(with-test (:name :structure-slot-value-in-method-actually-fast
            :skipped-on (not (or :x86 :x86-64)))
  (let* ((method (find-method #'read-a-struct-x nil (list (find-class 'a-struct))))
         (lines (ctu:disassembly-lines (sb-mop:method-function method))))
    (assert (notany (lambda (x) (search "CALL" x)) lines))
    (assert (notany (lambda (x) (search "JMP" x)) lines))))

(with-test (:name :structure-slot-value-in-method-no-callees)
  (let* ((method (find-method #'read-a-struct-x nil (list (find-class 'a-struct))))
         (mf (sb-mop:method-function method))
         (fmf (sb-pcl::%method-function-fast-function mf))
         (callees (ctu:find-named-callees fmf))
         (pcl-callees '(sb-pcl::method-function-from-fast-function sb-pcl::%make-method-function)))
    (ecase sb-ext:*evaluator-mode*
      (:interpret)
      (:compile
       (assert (null (set-difference pcl-callees callees)))
       (assert (null (set-difference callees pcl-callees)))))))

(with-test (:name :structure-missing-slot-value-in-method-calls-global
            :skipped-on (not (or :x86 :x86-64)))
  (let* ((method (find-method #'read-a-struct-y nil (list (find-class 'a-struct))))
         (lines (ctu:disassembly-lines (sb-mop:method-function method))))
    (assert (some (lambda (x) (search "SB-PCL::STRUCTURE-SLOT-VALUE" x)) lines))))

(with-test (:name :structure-missing-slot-value-in-method-single-callee)
  (let* ((method (find-method #'read-a-struct-y nil (list (find-class 'a-struct))))
         (mf (sb-mop:method-function method))
         (fmf (sb-pcl::%method-function-fast-function mf))
         (callees (ctu:find-named-callees fmf))
         (pcl-callees '(sb-pcl::method-function-from-fast-function sb-pcl::%make-method-function)))
    (ecase sb-ext:*evaluator-mode*
      (:interpret)
      (:compile
       (assert (null (set-difference pcl-callees callees)))
       (assert (equal (set-difference callees pcl-callees) '(sb-pcl::structure-slot-value)))))))

(define-condition a-condition () ((a :initarg :a)))
(defmethod sb-mop:slot-value-using-class :around (class (c a-condition) slotd)
  (list :a-slot-value (call-next-method)))
(with-test (:name :condition-slot-value-using-class-method)
  (assert (equal (slot-value (make-condition 'a-condition :a 4) 'a) '(:a-slot-value 4))))

(defstruct cfg-struct-type1 name)
(defstruct cfg-struct-type2 identifier name)
(defstruct cfg-struct-type3 name)
(defun configuration-structure-name-good (x)
  (slot-value (the (or cfg-struct-type1 cfg-struct-type2 cfg-struct-type3) x) 'name))
(defun configuration-structure-name-best (x)
  (slot-value (the (or cfg-struct-type1 cfg-struct-type2) x) 'name))
(compile 'configuration-structure-name-good)
(compile 'configuration-structure-name-best)
(with-test (:name :structure-slot-value-2-possible-instance-types)
  (let ((callees (ctu:find-named-callees #'configuration-structure-name-good)))
    (assert (find 'sb-pcl::structure-slot-value callees)))
  (let ((callees (ctu:find-named-callees #'configuration-structure-name-best)))
    (assert (not (find 'sb-pcl::structure-slot-value callees)))))

(defun read-slot-of-maybe-struct (x)
  (declare (type (or null cfg-struct-type3) x))
  (with-slots (name) x name))

(defmethod slot-missing ((class (eql (find-class 'null))) the-object slot op &optional new)
  (declare (ignore new))
  (if (and (eq slot 'name) (eq op 'slot-value)) :i-was-called))

(with-test (:name :slot-value-nullable-instance)
  (let ((callees (ctu:find-named-callees #'read-slot-of-maybe-struct)))
    (assert (and (sb-int:singleton-p callees)
                 (eq (car callees) 'sb-pcl::nil-not-slot-object)))
    (assert (eq 'steve (read-slot-of-maybe-struct (make-cfg-struct-type3 :name 'steve))))
    (assert (eq :i-was-called (read-slot-of-maybe-struct nil)))))
