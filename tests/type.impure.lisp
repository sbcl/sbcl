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
;;; FIXME: This is broken because of compiler bug 123: the compiler
;;; optimizes the type test to T, so it never gets a chance to raise a
;;; runtime error. (It used to work under the IR1 interpreter just
;;; because the IR1 interpreter doesn't try to optimize TYPEP as hard
;;; as the byte compiler does.)
#+nil (assert (raises-error? (typep #(11) '(simple-array undef-type 1))))
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

;;; bug 50.g: Smarten up hairy type specifiers slightly. We may wish
;;; to revisit this, perhaps by implementing a COMPLEMENT type
;;; (analogous to UNION and INTERSECTION) to take the logic out of the
;;; HAIRY domain.
(assert-nil-t (subtypep 'atom 'cons))
(assert-nil-t (subtypep 'cons 'atom))
(assert-nil-t (subtypep '(not list) 'cons))
(assert-nil-t (subtypep '(not float) 'single-float))
(assert-t-t (subtypep '(not atom) 'cons))
(assert-t-t (subtypep 'cons '(not atom)))
;;; FIXME: Another thing to revisit is %INVOKE-TYPE-METHOD.
;;; Essentially, the problem is that when the two arguments to
;;; subtypep are of different specifier-type types (e.g. HAIRY and
;;; UNION), there are two applicable type methods -- in this case
;;; HAIRY-COMPLEX-SUBTYPEP-ARG1-TYPE-METHOD and
;;; UNION-COMPLEX-SUBTYPEP-ARG2-TYPE-METHOD.  Both of these exist, but
;;; [!%]INVOKE-TYPE-METHOD aren't smart enough to know that if one of
;;; them returns NIL, NIL (indicating uncertainty) it should try the
;;; other; this is complicated by the presence of other TYPE-METHODS
;;; (e.g. INTERSECTION and UNION) whose return convention may or may
;;; not follow the same standard.
#||
(assert-nil-t (subtypep '(not cons) 'list))
(assert-nil-t (subtypep '(not single-float) 'float))
||#
;;; If we fix the above FIXME, we should for free have fixed bug 58.
#||
(assert-t-t (subtypep '(and zilch integer) 'zilch))
||#

;;;; Douglas Thomas Crosher rewrote the CMU CL type test system to
;;;; allow inline type tests for CONDITIONs and STANDARD-OBJECTs, and
;;;; generally be nicer, and Martin Atzmueller ported the patches.
;;;; They look nice but they're nontrivial enough that it's not
;;;; obvious from inspection that everything is OK. Let's make sure
;;;; that things still basically work.

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

;;; inline type tests
(format t "~&/setting up *TESTS-OF-INLINE-TYPE-TESTS*~%")
(defparameter *tests-of-inline-type-tests*
  '(progn

     ;; structure type tests
     (assert (typep (make-structure-foo3) 'structure-foo2))
     (assert (not (typep (make-structure-foo1) 'structure-foo4)))
     (assert (typep (nth-value 1
			       (ignore-errors (structure-foo2-x
					       (make-structure-foo1))))
		    'type-error))
     (assert (null (ignore-errors
		     (setf (structure-foo2-x (make-structure-foo1)) 11))))

     ;; structure-class tests
     (assert (typep (make-instance 'structure-class-foo3)
		    'structure-class-foo2))
     (assert (not (typep (make-instance 'structure-class-foo1)
			 'structure-class-foo4)))
     (assert (null (ignore-errors
		     (setf (slot-value (make-instance 'structure-class-foo1)
				       'x)
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
     (assert (eq (car (sb-kernel:class-direct-superclasses
		       (find-class 'simple-condition)))
		 (find-class 'condition)))

     (assert (eq (car (sb-pcl:class-direct-superclasses (sb-pcl:find-class
							 'simple-condition)))
		 (sb-pcl:find-class 'condition)))

    (let ((subclasses (mapcar #'sb-pcl:find-class
                              '(simple-type-error
                                simple-error
                                simple-warning
                                sb-int:simple-file-error
                                sb-int:simple-style-warning))))
      (assert (null (set-difference
                     (sb-pcl:class-direct-subclasses (sb-pcl:find-class
                                                      'simple-condition))
                     subclasses))))

     ;; precedence lists
     (assert (equal (sb-pcl:class-precedence-list
		     (sb-pcl:find-class 'simple-condition))
		    (mapcar #'sb-pcl:find-class '(simple-condition
						  condition
						  sb-kernel:instance
						  t))))

     ;; stream classes
     (assert (null (sb-kernel:class-direct-superclasses
		    (find-class 'fundamental-stream))))
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
     (assert (not (subtypep 'stream 'fundamental-stream)))))
;;; Test under the interpreter.
(eval *tests-of-inline-type-tests*)
(format t "~&/done with interpreted *TESTS-OF-INLINE-TYPE-TESTS*~%")
;;; Test under the compiler.
(defun tests-of-inline-type-tests ()
  #.*tests-of-inline-type-tests*)
(tests-of-inline-type-tests)
(format t "~&/done with compiled (TESTS-OF-INLINE-TYPE-TESTS)~%")

;;; success
(quit :unix-status 104)
