(in-package :cl-user)

(load "assertoid.lisp")

(defmacro assert-nil-nil (expr)
  `(assert (equal '(nil nil) (multiple-value-list ,expr))))
(defmacro assert-nil-t (expr)
  `(assert (equal '(nil t) (multiple-value-list ,expr))))
(defmacro assert-t-t (expr)
  `(assert (equal '(t t) (multiple-value-list ,expr))))

(let ((types '(character
	       integer fixnum (integer 0 10)
	       single-float (single-float -1.0 1.0) (single-float 0.1)
	       (real 4 8) (real -1 7) (real 2 11)
	       (member #\a #\b #\c) (member 1 #\a) (member 3.0 3.3)
	       ;; FIXME: When bug 91 is fixed, add these to the list:
	       ;;   (INTEGER -1 1)
	       ;;   UNSIGNED-BYTE
	       ;;   (RATIONAL -1 7) (RATIONAL -2 4)
	       ;;   RATIO
	       )))
  (dolist (i types)
    (format t "type I=~S~%" i)
    (dolist (j types)
      (format t "  type J=~S~%" j)
      (assert (subtypep i `(or ,i ,j)))
      (assert (subtypep i `(or ,j ,i)))
      (assert (subtypep i `(or ,i ,i ,j)))
      (assert (subtypep i `(or ,j ,i)))
      (dolist (k types)
	(format t "    type K=~S~%" k)
	(assert (subtypep `(or ,i ,j) `(or ,i ,j ,k)))
	(assert (subtypep `(or ,i ,j) `(or ,k ,j ,i)))))))

;;; gotchas that can come up in handling subtypeness as "X is a
;;; subtype of Y if each of the elements of X is a subtype of Y"
(let ((subtypep-values (multiple-value-list
			(subtypep '(single-float -1.0 1.0)
				  '(or (real -100.0 0.0)
				       (single-float 0.0 100.0))))))
  (assert (member subtypep-values
		  '(;; The system isn't expected to
		    ;; understand the subtype relationship.
		    (nil nil)
		    ;; But if it does, that'd be neat.
		    (t t)
		    ;; (And any other return would be wrong.)
		    )
		  :test #'equal)))

(defun type-evidently-= (x y)
  (and (subtypep x y)
       (subtypep y x)))

(assert (subtypep 'single-float 'float))

(assert (type-evidently-= '(integer 0 10) '(or (integer 0 5) (integer 4 10))))

;;; sbcl-0.6.10 did (UPGRADED-ARRAY-ELEMENT-TYPE 'SOME-UNDEF-TYPE)=>T
;;; and (UPGRADED-COMPLEX-PART-TYPE 'SOME-UNDEF-TYPE)=>T.
(assert (raises-error? (upgraded-array-element-type 'some-undef-type)))
(assert (eql (upgraded-array-element-type t) t))
(assert (raises-error? (upgraded-complex-part-type 'some-undef-type)))
(assert (subtypep (upgraded-complex-part-type 'fixnum) 'real))

;;; Do reasonable things with undefined types, and with compound types
;;; built from undefined types.
;;;
;;; part I: TYPEP
(assert (typep #(11) '(simple-array t 1)))
(assert (typep #(11) '(simple-array (or integer symbol) 1)))
(assert (raises-error? (typep #(11) '(simple-array undef-type 1))))
(assert (not (typep 11 '(simple-array undef-type 1))))
;;; part II: SUBTYPEP
(assert (subtypep '(vector some-undef-type) 'vector))
(assert (not (subtypep '(vector some-undef-type) 'integer)))
(assert-nil-nil (subtypep 'utype-1 'utype-2))
(assert-nil-nil (subtypep '(vector utype-1) '(vector utype-2)))
(assert-nil-nil (subtypep '(vector utype-1) '(vector t)))
(assert-nil-nil (subtypep '(vector t) '(vector utype-2)))

;;; ANSI specifically disallows bare AND and OR symbols as type specs.
#| ; Alas, this is part of bug 10, still unfixed as of sbcl-0.6.11.10.
(assert (raises-error? (typep 11 'and)))
(assert (raises-error? (typep 11 'or)))
|#
;;; Of course empty lists of subtypes are still OK.
(assert (typep 11 '(and)))
(assert (not (typep 11 '(or))))

;;; bug 12: type system didn't grok nontrivial intersections
(assert (subtypep '(and symbol (satisfies keywordp)) 'symbol))
(assert (not (subtypep '(and symbol (satisfies keywordp)) 'null)))
(assert (subtypep 'keyword 'symbol))
(assert (not (subtypep 'symbol 'keyword)))
(assert (subtypep 'ratio 'real))
(assert (subtypep 'ratio 'number))

;;;; Douglas Thomas Crosher rewrote the CMU CL type test system to allow
;;;; inline type tests for CONDITIONs and STANDARD-OBJECTs, and generally
;;;; be nicer, and Martin Atzmueller ported the patches.
;;;; They look nice but they're nontrivial enough that it's not obvious
;;;; from inspection that everything is OK. Let's make sure that things
;;;; still basically work.

;; structure type tests setup
(defstruct structure-foo1)
(defstruct (structure-foo2 (:include structure-foo1))
  x)
(defstruct (structure-foo3 (:include structure-foo2)))
(defstruct (structure-foo4 (:include structure-foo3))
  y z)

;; structure-class tests setup
(defclass structure-class-foo1 () () (:metaclass cl:structure-class))
(defclass structure-class-foo2 (structure-class-foo1)
  () (:metaclass cl:structure-class))
(defclass structure-class-foo3 (structure-class-foo2)
  () (:metaclass cl:structure-class))
(defclass structure-class-foo4 (structure-class-foo3)
  () (:metaclass cl:structure-class))

;; standard-class tests setup
(defclass standard-class-foo1 () () (:metaclass cl:standard-class))
(defclass standard-class-foo2 (standard-class-foo1)
  () (:metaclass cl:standard-class))
(defclass standard-class-foo3 (standard-class-foo2)
  () (:metaclass cl:standard-class))
(defclass standard-class-foo4 (standard-class-foo3)
  () (:metaclass cl:standard-class))

;; condition tests setup
(define-condition condition-foo1 (condition) ())
(define-condition condition-foo2 (condition-foo1) ())
(define-condition condition-foo3 (condition-foo2) ())
(define-condition condition-foo4 (condition-foo3) ())

(fmakunbound 'test-inline-type-tests)
(defun test-inline-type-tests ()
  ;; structure type tests
  (assert (typep (make-structure-foo3) 'structure-foo2))
  (assert (not (typep (make-structure-foo1) 'structure-foo4)))
  (assert (null (ignore-errors
                  (setf (structure-foo2-x (make-structure-foo1)) 11))))

  ;; structure-class tests
  (assert (typep (make-instance 'structure-class-foo3)
                 'structure-class-foo2))
  (assert (not (typep (make-instance 'structure-class-foo1)
                      'structure-class-foo4)))
  (assert (null (ignore-errors
                  (setf (slot-value (make-instance 'structure-class-foo1) 'x)
			11))))

  ;; standard-class tests
  (assert (typep (make-instance 'standard-class-foo3)
                 'standard-class-foo2))
  (assert (not (typep (make-instance 'standard-class-foo1)
                      'standard-class-foo4)))
  (assert (null (ignore-errors
                  (setf (slot-value (make-instance 'standard-class-foo1) 'x)
                          11))))

  ;; condition tests
  (assert (typep (make-condition 'condition-foo3)
                 'condition-foo2))
  (assert (not (typep (make-condition 'condition-foo1)
                      'condition-foo4)))
  (assert (null (ignore-errors
                  (setf (slot-value (make-condition 'condition-foo1) 'x)
                          11))))
  (assert (subtypep 'error 't))
  (assert (subtypep 'simple-condition 'condition))
  (assert (subtypep 'simple-error 'simple-condition))
  (assert (subtypep 'simple-error 'error))
  (assert (not (subtypep 'condition 'simple-condition)))
  (assert (not (subtypep 'error 'simple-error)))
  (assert (eq (car (sb-kernel:class-direct-superclasses (find-class
                                                         'simple-condition)))
              (find-class 'condition)))

  (assert (eq (car (sb-pcl:class-direct-superclasses (sb-pcl:find-class
                                                      'simple-condition)))
              (sb-pcl:find-class 'condition)))
  (assert (null (set-difference
                 (sb-pcl:class-direct-subclasses (sb-pcl:find-class
                                                  'simple-condition))
                 (mapcar #'sb-pcl:find-class '(simple-type-error simple-error
                                               sb-int:simple-style-warning)))))

  ;; precedence lists
  (assert (equal (sb-pcl:class-precedence-list
                  (sb-pcl:find-class 'simple-condition))
                 (mapcar #'sb-pcl:find-class '(simple-condition condition
                                               sb-kernel:instance t))))

  ;; stream classes
  (assert (null (sb-kernel:class-direct-superclasses (find-class
                                                      'fundamental-stream))))
  (assert (equal (sb-pcl:class-direct-superclasses (sb-pcl:find-class
                                                    'fundamental-stream))
                 (mapcar #'sb-pcl:find-class '(standard-object stream))))
  (assert (null (set-difference
                 (sb-pcl:class-direct-subclasses (sb-pcl:find-class
                                                  'fundamental-stream))
                 (mapcar #'sb-pcl:find-class '(fundamental-binary-stream
                                               fundamental-character-stream
                                               fundamental-output-stream
                                               fundamental-input-stream)))))
  (assert (equal (sb-pcl:class-precedence-list (sb-pcl:find-class
                                                'fundamental-stream))
                 (mapcar #'sb-pcl:find-class '(fundamental-stream
                                               standard-object
                                               sb-pcl::std-object
                                               sb-pcl::slot-object
                                               stream
                                               sb-kernel:instance
                                               t))))
  (assert (equal (sb-pcl:class-precedence-list (sb-pcl:find-class
                                                'fundamental-stream))
                 (mapcar #'sb-pcl:find-class '(fundamental-stream
					       standard-object
                                               sb-pcl::std-object
                                               sb-pcl::slot-object stream
                                               sb-kernel:instance t))))
  (assert (subtypep (find-class 'stream) (find-class t)))
  (assert (subtypep (find-class 'fundamental-stream) 'stream))
  (assert (not (subtypep 'stream 'fundamental-stream))))

;;; inline-type tests:
;;; Test the interpreted version.
(test-inline-type-tests)
;;; Test the compiled version.
(compile nil #'test-inline-type-tests)
(test-inline-type-tests)

;;; success
(quit :unix-status 104)
