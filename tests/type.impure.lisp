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

(load "assertoid.lisp")
(use-package "ASSERTOID")
(use-package "TEST-UTIL")

(defmacro assert-nil-nil (expr)
  `(assert (equal '(nil nil) (multiple-value-list ,expr))))
(defmacro assert-nil-t (expr)
  `(assert (equal '(nil t) (multiple-value-list ,expr))))
(defmacro assert-t-t (expr)
  `(assert (equal '(t t) (multiple-value-list ,expr))))

(defmacro assert-t-t-or-uncertain (expr)
  `(assert (let ((list (multiple-value-list ,expr)))
             (or (equal '(nil nil) list)
                 (equal '(t t) list)))))

(let ((types '(character
               integer fixnum (integer 0 10)
               single-float (single-float -1.0 1.0) (single-float 0.1)
               (real 4 8) (real -1 7) (real 2 11)
               null symbol keyword
               (member #\a #\b #\c) (member 1 #\a) (member 3.0 3.3)
              (member #\a #\c #\d #\f) (integer -1 1)
               unsigned-byte
               (rational -1 7) (rational -2 4)
               ratio
               )))
  (dolist (i types)
    ;(format t "type I=~S~%" i)
    (dolist (j types)
      ;(format t "  type J=~S~%" j)
      (assert (subtypep i `(or ,i ,j)))
      (assert (subtypep i `(or ,j ,i)))
      (assert (subtypep i `(or ,i ,i ,j)))
      (assert (subtypep i `(or ,j ,i)))
      (dolist (k types)
        ;(format t "    type K=~S~%" k)
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

;;; Bug 50(c,d): numeric types with empty ranges should be NIL
(assert (type-evidently-= 'nil '(integer (0) (0))))
(assert (type-evidently-= 'nil '(rational (0) (0))))
(assert (type-evidently-= 'nil '(float (0.0) (0.0))))

;;; sbcl-0.6.10 did (UPGRADED-ARRAY-ELEMENT-TYPE 'SOME-UNDEF-TYPE)=>T
;;; and (UPGRADED-COMPLEX-PART-TYPE 'SOME-UNDEF-TYPE)=>T.
(assert-error (upgraded-array-element-type 'some-undef-type))
(assert (eql (upgraded-array-element-type t) t))
(assert-error (upgraded-complex-part-type 'some-undef-type))
(assert (subtypep (upgraded-complex-part-type 'fixnum) 'real))

;;; Do reasonable things with undefined types, and with compound types
;;; built from undefined types.
;;;
;;; part I: TYPEP
(assert (typep #(11) '(simple-array t 1)))
(assert (typep #(11) '(simple-array (or integer symbol) 1)))
(assert-error (typep #(11) '(simple-array undef-type 1)))
(assert (not (typep 11 '(simple-array undef-type 1))))
;;; part II: SUBTYPEP

(assert (subtypep '(vector some-undef-type) 'vector))
(assert (not (subtypep '(vector some-undef-type) 'integer)))
(assert-nil-nil (subtypep 'utype-1 'utype-2))
(assert-nil-nil (subtypep '(vector utype-1) '(vector utype-2)))
(assert-nil-nil (subtypep '(vector utype-1) '(vector t)))
(assert-nil-nil (subtypep '(vector t) '(vector utype-2)))

;;; ANSI specifically disallows bare AND and OR symbols as type specs.
(assert-error (typep 11 'and))
(assert-error (typep 11 'or))
(assert-error (typep 11 'member))
(assert-error (typep 11 'values))
(assert-error (typep 11 'eql))
(assert-error (typep 11 'satisfies))
(assert-error (typep 11 'not))
;;; and while it doesn't specifically disallow illegal compound
;;; specifiers from the CL package, we don't have any.
(assert-error (subtypep 'fixnum '(fixnum 1)))
(assert-error (subtypep 'class '(list)))
(assert-error (subtypep 'foo '(ratio 1/2 3/2)))
(assert-error (subtypep 'character '(character 10)))
#+nil ; doesn't yet work on PCL-derived internal types
(assert-error (subtypep 'lisp '(class)))
#+nil
(assert-error (subtypep 'bar '(method number number)))

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
;;; These two are desireable but not necessary for ANSI conformance;
;;; maintenance work on other parts of the system broke them in
;;; sbcl-0.7.13.11 -- CSR
#+nil
(assert-nil-t (subtypep '(not list) 'cons))
#+nil
(assert-nil-t (subtypep '(not float) 'single-float))
(assert-t-t (subtypep '(not atom) 'cons))
(assert-t-t (subtypep 'cons '(not atom)))
;;; ANSI requires that SUBTYPEP relationships among built-in primitive
;;; types never be uncertain, i.e. never return NIL as second value.
;;; Prior to about sbcl-0.7.2.6, ATOM caused a lot of problems here
;;; (because it's a negation type, implemented as a HAIRY-TYPE, and
;;; CMU CL's HAIRY-TYPE logic punted a lot).
(assert-t-t (subtypep 'integer 'atom))
(assert-t-t (subtypep 'function 'atom))
(assert-nil-t (subtypep 'list 'atom))
(assert-nil-t (subtypep 'atom 'integer))
(assert-nil-t (subtypep 'atom 'function))
(assert-nil-t (subtypep 'atom 'list))
;;; ATOM is equivalent to (NOT CONS):
(assert-t-t (subtypep 'integer '(not cons)))
(assert-nil-t (subtypep 'list '(not cons)))
(assert-nil-t (subtypep '(not cons) 'integer))
(assert-nil-t (subtypep '(not cons) 'list))
;;; And we'd better check that all the named types are right. (We also
;;; do some more tests on ATOM here, since once CSR experimented with
;;; making it a named type.)
(assert-t-t (subtypep 'nil 'nil))
(assert-t-t (subtypep 'nil 'atom))
(assert-t-t (subtypep 'nil 't))
(assert-nil-t (subtypep 'atom 'nil))
(assert-t-t (subtypep 'atom 'atom))
(assert-t-t (subtypep 'atom 't))
(assert-nil-t (subtypep 't 'nil))
(assert-nil-t (subtypep 't 'atom))
(assert-t-t (subtypep 't 't))
;;; Also, LIST is now somewhat special, in that (NOT LIST) should be
;;; recognized as a subtype of ATOM:
(assert-t-t (subtypep '(not list) 'atom))
(assert-nil-t (subtypep 'atom '(not list)))
;;; These used to fail, because when the two arguments to subtypep are
;;; of different specifier-type types (e.g. HAIRY and UNION), there
;;; are two applicable type methods -- in this case
;;; HAIRY-COMPLEX-SUBTYPEP-ARG1-TYPE-METHOD and
;;; UNION-COMPLEX-SUBTYPEP-ARG2-TYPE-METHOD. Both of these exist, but
;;; [!%]INVOKE-TYPE-METHOD aren't smart enough to know that if one of
;;; them returns NIL, NIL (indicating uncertainty) it should try the
;;; other. However, as of sbcl-0.7.2.6 or so, CALL-NEXT-METHOD-ish
;;; logic in those type methods fixed it.
(assert-nil-t (subtypep '(not cons) 'list))
(assert-nil-t (subtypep '(not single-float) 'float))
;;; Somewhere along the line (probably when adding CALL-NEXT-METHOD-ish
;;; logic in SUBTYPEP type methods) we fixed bug 58 too:
(assert-t-t (subtypep '(and zilch integer) 'zilch))
(assert-t-t (subtypep '(and integer zilch) 'zilch))

;;; Bug 84: SB-KERNEL:CSUBTYPEP was a bit enthusiastic at
;;; special-casing calls to subtypep involving *EMPTY-TYPE*,
;;; corresponding to the NIL type-specifier; we were bogusly returning
;;; NIL, T (indicating surety) for the following:
(assert-nil-nil (subtypep '(satisfies some-undefined-fun) 'nil))

;;; It turns out that, as of sbcl-0.7.2, we require to be able to
;;; detect this to compile src/compiler/node.lisp (and in particular,
;;; the definition of the component structure). Since it's a sensible
;;; thing to want anyway, let's test for it here:
(assert-t-t (subtypep '(or some-undefined-type (member :no-ir2-yet :dead))
                      '(or some-undefined-type (member :no-ir2-yet :dead))))
;;; BUG 158 (failure to compile loops with vector references and
;;; increments of greater than 1) was a symptom of type system
;;; uncertainty, to wit:
(assert-t-t (subtypep '(and (mod 536870911) (or (integer 0 0) (integer 2 536870912)))
                      '(mod 536870911))) ; aka SB-INT:INDEX.
;;; floating point types can be tricky.
(assert-t-t (subtypep '(member 0.0) '(single-float 0.0 0.0)))
(assert-t-t (subtypep '(member -0.0) '(single-float 0.0 0.0)))
(assert-t-t (subtypep '(member 0.0) '(single-float -0.0 0.0)))
(assert-t-t (subtypep '(member -0.0) '(single-float 0.0 -0.0)))
(assert-t-t (subtypep '(member 0.0d0) '(double-float 0.0d0 0.0d0)))
(assert-t-t (subtypep '(member -0.0d0) '(double-float 0.0d0 0.0d0)))
(assert-t-t (subtypep '(member 0.0d0) '(double-float -0.0d0 0.0d0)))
(assert-t-t (subtypep '(member -0.0d0) '(double-float 0.0d0 -0.0d0)))

(assert-nil-t (subtypep '(single-float 0.0 0.0) '(member 0.0)))
(assert-nil-t (subtypep '(single-float 0.0 0.0) '(member -0.0)))
(assert-nil-t (subtypep '(single-float -0.0 0.0) '(member 0.0)))
(assert-nil-t (subtypep '(single-float 0.0 -0.0) '(member -0.0)))
(assert-nil-t (subtypep '(double-float 0.0d0 0.0d0) '(member 0.0d0)))
(assert-nil-t (subtypep '(double-float 0.0d0 0.0d0) '(member -0.0d0)))
(assert-nil-t (subtypep '(double-float -0.0d0 0.0d0) '(member 0.0d0)))
(assert-nil-t (subtypep '(double-float 0.0d0 -0.0d0) '(member -0.0d0)))

(assert-t-t (subtypep '(member 0.0 -0.0) '(single-float 0.0 0.0)))
(assert-t-t (subtypep '(single-float 0.0 0.0) '(member 0.0 -0.0)))
(assert-t-t (subtypep '(member 0.0d0 -0.0d0) '(double-float 0.0d0 0.0d0)))
(assert-t-t (subtypep '(double-float 0.0d0 0.0d0) '(member 0.0d0 -0.0d0)))

(assert-t-t (subtypep '(not (single-float 0.0 0.0)) '(not (member 0.0))))
(assert-t-t (subtypep '(not (double-float 0.0d0 0.0d0)) '(not (member 0.0d0))))

(assert-t-t (subtypep '(float -0.0) '(float 0.0)))
(assert-t-t (subtypep '(float 0.0) '(float -0.0)))
(assert-t-t (subtypep '(float (0.0)) '(float (-0.0))))
(assert-t-t (subtypep '(float (-0.0)) '(float (0.0))))

(with-test (:name :member-type-and-numeric)
  ;; (MEMBER 0s0 -s0) used to appear to parse correctly,
  ;; but it didn't because MAKE-MEMBER-TYPE returned a union type
  ;; (OR (MEMBER 0.0) (SINGLE-FLOAT 0.0 0.0)) which was further reduced
  ;; to just the numeric type, being a supertype of the singleton.
  ;; The parsing problem became evident when any other member was added in,
  ;; because in that case the member type is not a subtype of the numeric.
  (let* ((x (sb-kernel:specifier-type '(member 0s0 foo -0s0)))
         (m (find-if #'sb-kernel:member-type-p (sb-kernel:union-type-types x))))
    (assert (equal (sb-kernel:member-type-members m) '(foo)))))


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
     (assert (eq (car (sb-pcl:class-direct-superclasses
                       (find-class 'simple-condition)))
                 (find-class 'condition)))

     #+nil ; doesn't look like a good test
     (let ((subclasses (mapcar #'find-class
                               '(simple-type-error
                                 simple-error
                                 simple-warning
                                 sb-int:simple-file-error
                                 sb-int:simple-style-warning))))
       (assert (null (set-difference
                      (sb-pcl:class-direct-subclasses (find-class
                                                       'simple-condition))
                      subclasses))))

     ;; precedence lists
     (assert (equal (sb-pcl:class-precedence-list
                     (find-class 'simple-condition))
                    (mapcar #'find-class '(simple-condition
                                           condition
                                           sb-pcl::slot-object
                                           t))))

     ;; stream classes
     (assert (equal (sb-pcl:class-direct-superclasses (find-class
                                                       'fundamental-stream))
                    (mapcar #'find-class '(standard-object stream))))
     (assert (null (set-difference
                    (sb-pcl:class-direct-subclasses (find-class
                                                     'fundamental-stream))
                    (mapcar #'find-class '(fundamental-binary-stream
                                           fundamental-character-stream
                                           fundamental-output-stream
                                           fundamental-input-stream)))))
     (assert (equal (sb-pcl:class-precedence-list (find-class
                                                   'fundamental-stream))
                    (mapcar #'find-class '(fundamental-stream
                                           standard-object
                                           sb-pcl::slot-object
                                           stream
                                           t))))
     (assert (equal (sb-pcl:class-precedence-list (find-class
                                                   'fundamental-stream))
                    (mapcar #'find-class '(fundamental-stream
                                           standard-object
                                           sb-pcl::slot-object stream
                                           t))))
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

;;; Redefinition of classes should alter the type hierarchy (BUG 140):
(defclass superclass () ())
(defclass maybe-subclass () ())
(assert-nil-t (subtypep 'maybe-subclass 'superclass))
(defclass maybe-subclass (superclass) ())
(assert-t-t (subtypep 'maybe-subclass 'superclass))
(defclass maybe-subclass () ())
(assert-nil-t (subtypep 'maybe-subclass 'superclass))

;;; Prior to sbcl-0.7.6.27, there was some confusion in ARRAY types
;;; specialized on some as-yet-undefined type which would cause this
;;; program to fail (bugs #123 and #165). Verify that it doesn't.
(defun foo (x)
  (declare (type (vector bar) x))
  (aref x 1))
(deftype bar () 'single-float)
(assert (eql (foo (make-array 3 :element-type 'bar :initial-element 0.0f0))
             0.0f0))

;;; bug 260a
(assert-t-t
 (let* ((s (gensym))
        (t1 (sb-kernel:specifier-type s)))
   (eval `(defstruct ,s))
   (sb-kernel:type= t1 (sb-kernel:specifier-type s))))

;;; bug found by PFD's random subtypep tester
(let ((t1 '(cons rational (cons (not rational) (cons integer t))))
      (t2 '(not (cons (integer 0 1) (cons single-float long-float)))))
  (assert-t-t (subtypep t1 t2))
  (assert-nil-t (subtypep t2 t1))
  (assert-t-t (subtypep `(not ,t2) `(not ,t1)))
  (assert-nil-t (subtypep `(not ,t1) `(not ,t2))))

;;; not easily visible to user code, but this used to be very
;;; confusing.
(with-test (:name (:ctor :typep-function))
  (assert (eval '(typep (sb-pcl::ensure-ctor
                         (list 'sb-pcl::ctor (gensym)) nil nil nil)
                        'function))))
(with-test (:name (:ctor :functionp))
  (assert (functionp (sb-pcl::ensure-ctor
                      (list 'sb-pcl::ctor (gensym)) nil nil nil))))
;;; some new (2008-10-03) ways of going wrong...
(with-test (:name (:ctor-allocate-instance :typep-function))
  (assert (eval '(typep (allocate-instance (find-class 'sb-pcl::ctor))
                        'function))))
(with-test (:name (:ctor-allocate-instance :functionp))
  (assert (functionp (allocate-instance (find-class 'sb-pcl::ctor)))))

;;; from PFD ansi-tests
(let ((t1 '(cons (cons (cons (real -744833699 -744833699) cons)
                       (integer -234496 215373))
                 integer))
      (t2 '(cons (cons (cons integer integer)
                       (integer -234496 215373))
                 t)))
  (assert (null (values (subtypep `(not ,t2) `(not ,t1))))))

(defstruct misc-629a)
(defclass misc-629b () ())
(defclass misc-629c () () (:metaclass sb-mop:funcallable-standard-class))

(assert (typep (make-misc-629a) 'sb-kernel:instance))
(assert-t-t (subtypep `(member ,(make-misc-629a)) 'sb-kernel:instance))
(assert-nil-t (subtypep `(and (member ,(make-misc-629a)) sb-kernel:instance)
                        nil))
(let ((misc-629a (make-misc-629a)))
  (assert-t-t (subtypep `(member ,misc-629a)
                        `(and (member ,misc-629a) sb-kernel:instance)))
  (assert-t-t (subtypep `(and (member ,misc-629a)
                          sb-kernel:funcallable-instance)
                        nil)))

(assert (typep (make-instance 'misc-629b) 'sb-kernel:instance))
(assert-t-t (subtypep `(member ,(make-instance 'misc-629b))
                      'sb-kernel:instance))
(assert-nil-t (subtypep `(and (member ,(make-instance 'misc-629b))
                          sb-kernel:instance)
                        nil))
(let ((misc-629b (make-instance 'misc-629b)))
  (assert-t-t (subtypep `(member ,misc-629b)
                        `(and (member ,misc-629b) sb-kernel:instance)))
  (assert-t-t (subtypep `(and (member ,misc-629b)
                          sb-kernel:funcallable-instance)
                        nil)))

(assert (typep (make-instance 'misc-629c) 'sb-kernel:funcallable-instance))
(assert-t-t (subtypep `(member ,(make-instance 'misc-629c))
                      'sb-kernel:funcallable-instance))
(assert-nil-t (subtypep `(and (member ,(make-instance 'misc-629c))
                          sb-kernel:funcallable-instance)
                        nil))
(let ((misc-629c (make-instance 'misc-629c)))
  (assert-t-t (subtypep `(member ,misc-629c)
                        `(and (member ,misc-629c)
                          sb-kernel:funcallable-instance)))
  (assert-t-t (subtypep `(and (member ,misc-629c)
                          sb-kernel:instance)
                        nil)))

;;; this was broken during the FINALIZE-INHERITANCE rearrangement; the
;;; MAKE-INSTANCE finalizes the superclass, thus invalidating the
;;; subclass, so SUBTYPEP must be prepared to deal with
(defclass ansi-tests-defclass1 () ())
(defclass ansi-tests-defclass3 (ansi-tests-defclass1) ())
(make-instance 'ansi-tests-defclass1)
(assert-t-t (subtypep 'ansi-tests-defclass3 'standard-object))

;;; so was this
(let ((class (eval '(defclass to-be-type-ofed () ()))))
  (setf (find-class 'to-be-type-ofed) nil)
  (assert (eq (type-of (make-instance class)) class)))

;;; accuracy of CONS :SIMPLE-TYPE-=
(deftype goldbach-1 () '(satisfies even-and-greater-then-two-p))
(deftype goldbach-2 () ' (satisfies sum-of-two-primes-p))

(multiple-value-bind (ok win)
    (sb-kernel:type= (sb-kernel:specifier-type '(cons goldbach1 integer))
                     (sb-kernel:specifier-type '(cons goldbach1 integer)))
  (assert ok)
  (assert win))

;; See FIXME in type method for CONS :SIMPLE-TYPE-=
#+nil
(multiple-value-bind (ok win)
    (sb-kernel:type= (sb-kernel:specifier-type '(cons goldbach1 integer))
                     (sb-kernel:specifier-type '(cons goldbach1 single-float)))
  (assert (not ok))
  (assert win))

(multiple-value-bind (ok win)
    (sb-kernel:type= (sb-kernel:specifier-type '(cons goldbach1 integer))
                     (sb-kernel:specifier-type '(cons goldbach2 single-float)))
  (assert (not ok))
  (assert (not win)))

;;; precise unions of array types (was bug 306a)
(defun bug-306-a (x)
  (declare (optimize speed)
           (type (or (array cons) (array vector)) x))
  (elt (aref x 0) 0))
(assert (= 0 (bug-306-a #((0)))))

;;; FUNCALLABLE-INSTANCE is a subtype of function.
(assert-t-t (subtypep '(and pathname function) nil))
(assert-t-t (subtypep '(and pathname sb-kernel:funcallable-instance) nil))
(assert (not (subtypep '(and stream function) nil)))
(assert (not (subtypep '(and stream sb-kernel:funcallable-instance) nil)))
(assert (not (subtypep '(and function standard-object) nil)))
(assert (not (subtypep '(and sb-kernel:funcallable-instance standard-object) nil)))

;;; also, intersections of classes with INSTANCE should not be too
;;; general
(assert (not (typep #'print-object '(and standard-object sb-kernel:instance))))
(assert (not (subtypep 'standard-object '(and standard-object sb-kernel:instance))))

(assert-t-t
 (subtypep '(or simple-array simple-string) '(or simple-string simple-array)))
(assert-t-t
 (subtypep '(or simple-string simple-array) '(or simple-array simple-string)))
(assert-t-t
 (subtypep '(or fixnum simple-string end-of-file parse-error fixnum vector)
           '(or fixnum vector end-of-file parse-error fixnum simple-string)))

#+sb-eval
(assert-t-t
 (subtypep '(and function (not compiled-function)
             (not sb-eval:interpreted-function))
           nil))
#+sb-fasteval
(assert-t-t
 (subtypep '(and function (not compiled-function)
             (not sb-interpreter:interpreted-function))
           nil))

;;; weakening of union type checks
(defun weaken-union-1 (x)
  (declare (optimize speed))
  (car x))
(multiple-value-bind (res err)
    (ignore-errors (weaken-union-1 "askdjhasdkj"))
  (assert (not res))
  (assert (typep err 'type-error)))
(defun weaken-union-2 (x)
  (declare (optimize speed)
           (type (or cons fixnum) x))
  (etypecase x
    (fixnum x)
    (cons
     (setf (car x) 3)
     x)))
(multiple-value-bind (res err)
    (ignore-errors (weaken-union-2 "asdkahsdkhj"))
  (assert (not res))
  (assert (typep err 'type-error))
  (assert (or (equal '(or cons fixnum) (type-error-expected-type err))
              (equal '(or fixnum cons) (type-error-expected-type err)))))

;;; TYPEXPAND & Co

(deftype a-deftype (arg)
  `(cons (eql ,arg) *))

(deftype another-deftype (arg)
  `(a-deftype ,arg))

(deftype list-of-length (length &optional element-type)
  (assert (not (minusp length)))
  (if (zerop length)
      'null
      `(cons ,element-type (list-of-length ,(1- length) ,element-type))))

(with-test (:name :typexpand-1)
  (multiple-value-bind (expansion-1 expandedp-1)
      (sb-ext:typexpand-1 '(another-deftype symbol))
    (assert expandedp-1)
    (assert (equal expansion-1 '(a-deftype symbol)))
    (multiple-value-bind (expansion-2 expandedp-2)
        (sb-ext:typexpand-1 expansion-1)
      (assert expandedp-2)
      (assert (equal expansion-2 '(cons (eql symbol) *)))
      (multiple-value-bind (expansion-3 expandedp-3)
          (sb-ext:typexpand-1 expansion-2)
        (assert (not expandedp-3))
        (assert (eq expansion-2 expansion-3))))))

(with-test (:name :typexpand.1)
  (multiple-value-bind (expansion-1 expandedp-1)
      (sb-ext:typexpand '(another-deftype symbol))
    (assert expandedp-1)
    (assert (equal expansion-1 '(cons (eql symbol) *)))
    (multiple-value-bind (expansion-2 expandedp-2)
        (sb-ext:typexpand expansion-1)
      (assert (not expandedp-2))
      (assert (eq expansion-1 expansion-2)))))

(with-test (:name :typexpand.2)
  (assert (equal (sb-ext:typexpand '(list-of-length 3 fixnum))
                 '(cons fixnum (list-of-length 2 fixnum)))))

(with-test (:name :typexpand-all)
  (assert (equal (sb-ext:typexpand-all '(list-of-length 3))
                 '(cons t (cons t (cons t null)))))
  (assert (equal (sb-ext:typexpand-all '(list-of-length 3 fixnum))
                 '(cons fixnum (cons fixnum (cons fixnum null))))))

(defclass a-deftype () ())

(with-test (:name (:typexpand-1 :after-type-redefinition-to-class))
  (multiple-value-bind (expansion expandedp)
      (sb-ext:typexpand-1 '#1=(a-deftype symbol))
    (assert (not expandedp))
    (assert (eq expansion '#1#))))


(with-test (:name :defined-type-name-p)
  (assert (not (sb-ext:defined-type-name-p '#:foo)))
  (assert (sb-ext:defined-type-name-p 'a-deftype))
  (assert (sb-ext:defined-type-name-p 'structure-foo1))
  (assert (sb-ext:defined-type-name-p 'structure-class-foo1))
  (assert (sb-ext:defined-type-name-p 'standard-class-foo1))
  (assert (sb-ext:defined-type-name-p 'condition-foo1))
  (dolist (prim-type '(t nil fixnum cons atom))
    (assert (sb-ext:defined-type-name-p prim-type))))


(with-test (:name :valid-type-specifier-p)
  (macrolet ((yes (form) `(assert ,form))
             (no  (form) `(assert (not ,form))))
    (no  (sb-ext:valid-type-specifier-p '(cons #(frob) *)))
    (no  (sb-ext:valid-type-specifier-p 'list-of-length))
    (no  (sb-ext:valid-type-specifier-p '(list-of-length 5 #(x))))
    (yes (sb-ext:valid-type-specifier-p '(list-of-length 5 fixnum)))

    (yes (sb-ext:valid-type-specifier-p 'structure-foo1))
    (no  (sb-ext:valid-type-specifier-p '(structure-foo1 x)))
    (yes (sb-ext:valid-type-specifier-p 'condition-foo1))
    (yes (sb-ext:valid-type-specifier-p 'standard-class-foo1))
    (yes (sb-ext:valid-type-specifier-p 'structure-class-foo1))

    (yes (sb-ext:valid-type-specifier-p 'readtable))
    (no  (sb-ext:valid-type-specifier-p '(readtable)))
    (no  (sb-ext:valid-type-specifier-p '(readtable x)))

    (yes (sb-ext:valid-type-specifier-p '(values)))
    (no  (sb-ext:valid-type-specifier-p 'values))
    (yes (sb-ext:valid-type-specifier-p '(and)))
    (no  (sb-ext:valid-type-specifier-p 'and))))

(with-test (:name (:valid-type-specifier-p :introspection-test))
  (flet ((map-functions (fn)
           (do-all-symbols (s)
             (when (and (fboundp s)
                        (not (macro-function s))
                        (not (special-operator-p s)))
               (funcall fn s)))))
    (map-functions
     #'(lambda (s)
         (let* ((fun   (sb-kernel:%fun-fun (fdefinition s)))
                (ftype (sb-kernel:%simple-fun-type fun)))
           (unless (sb-ext:valid-type-specifier-p ftype)
             (format *error-output*
                     "~@<~S returned NIL on ~S's FTYPE: ~2I~_~S~@:>"
                     'sb-ext:valid-type-specifier-p
                     s
                     ftype )
             (error "FAILURE")))))))

(with-test (:name (:bug-309128 1))
  (let* ((s (gensym))
         (t1 (sb-kernel:specifier-type s)))
    (eval `(defstruct ,s))
    (multiple-value-bind (ok sure)
        (sb-kernel:csubtypep t1 (sb-kernel:specifier-type s))
      (assert (and ok sure)))))

(with-test (:name (:bug-309128 2))
  (let* ((s (gensym))
         (t1 (sb-kernel:specifier-type s)))
    (eval `(defstruct ,s))
    (multiple-value-bind (ok sure)
        (sb-kernel:csubtypep (sb-kernel:specifier-type s) t1)
      (assert (and ok sure)))))

(with-test (:name (:bug-309128 3))
  (let* ((s (gensym))
         (t1 (sb-kernel:specifier-type s))
         (s2 (gensym))
         (t2 (sb-kernel:specifier-type s2)))
    (eval `(deftype ,s2 () ',s))
    (eval `(defstruct ,s))
    (multiple-value-bind (ok sure) (sb-kernel:csubtypep t1 t2)
      (assert (and ok sure)))))

(with-test (:name :unknown-type-not=-for-sure)
  (let* ((type (gensym "FOO"))
         (spec1 (sb-kernel:specifier-type `(vector ,type)))
         (spec2 (sb-kernel:specifier-type `(vector single-float))))
    (eval `(deftype ,type () 'double-float))
    (multiple-value-bind (ok sure) (sb-kernel:type= spec1 spec2)
      (assert (not ok))
      (assert sure))))

(defclass subtypep-fwd-test1 (subtypep-fwd-test-unknown1) ())
(defclass subtypep-fwd-test2 (subtypep-fwd-test-unknown2) ())
(defclass subtypep-fwd-testb1 (subtypep-fwd-testb-unknown1) ())
(defclass subtypep-fwd-testb2 (subtypep-fwd-testb-unknown2 subtypep-fwd-testb1) ())
(with-test (:name (:subtypep :forward-referenced-classes))
  (flet ((test (c1 c2 b1 b2)
           (multiple-value-bind (x1 x2) (subtypep c1 c2)
             (unless (and (eq b1 x1) (eq b2 x2))
               (error "(subtypep ~S ~S) => ~S, ~S but wanted ~S, ~S"
                      c1 c2 x1 x2 b1 b2)))))
    (test 'subtypep-fwd-test1 'subtypep-fwd-test1 t t)
    (test 'subtypep-fwd-test2 'subtypep-fwd-test2 t t)
    (test 'subtypep-fwd-test1 'subtypep-fwd-test2 nil nil)
    (test 'subtypep-fwd-test2 'subtypep-fwd-test1 nil nil)

    (test 'subtypep-fwd-test1 'subtypep-fwd-test-unknown1 t t)
    (test 'subtypep-fwd-test2 'subtypep-fwd-test-unknown2 t t)
    (test 'subtypep-fwd-test1 'subtypep-fwd-test-unknown2 nil nil)
    (test 'subtypep-fwd-test2 'subtypep-fwd-test-unknown1 nil nil)

    (test 'subtypep-fwd-test-unknown2 'subtypep-fwd-test-unknown2 t t)
    (test 'subtypep-fwd-test-unknown1 'subtypep-fwd-test-unknown1 t t)
    (test 'subtypep-fwd-test-unknown1 'subtypep-fwd-test-unknown2 nil nil)
    (test 'subtypep-fwd-test-unknown2 'subtypep-fwd-test-unknown1 nil nil)

    (test 'subtypep-fwd-testb1 'subtypep-fwd-testb2 nil nil)
    (test 'subtypep-fwd-testb2 'subtypep-fwd-testb1 t t)))

;;; Array type unions have some tricky semantics.

;; borrowed from 'info.impure.lisp' - FIXME: put in test-util
;; and make it accept with either lists or vectors.
(defun shuffle (list)
  (let ((vector (coerce list 'vector)))
    (loop for lim from (1- (length vector)) downto 0
          for chosen = (random (1+ lim))
          do (rotatef (aref vector chosen) (aref vector lim)))
    (coerce vector 'list)))

(macrolet
    ((disunity-test (name type-specifier-1 type-specifier-2)
       `(with-test (:name ,name)
          (let ((type1 (sb-kernel:specifier-type ',type-specifier-1))
                (type2 (sb-kernel:specifier-type ',type-specifier-2)))
            (assert (null (sb-kernel::%type-union2 type1 type2)))
            (assert (null (sb-kernel::%type-union2 type2 type1))))))
     (unity-test (name type-specifier-1 type-specifier-2 result-type-specifier)
       `(with-test (:name ,name)
          (let* ((type1 (sb-kernel:specifier-type ',type-specifier-1))
                 (type2 (sb-kernel:specifier-type ',type-specifier-2))
                 (rtype (sb-kernel:specifier-type ',result-type-specifier))
                 (res1 (sb-kernel::%type-union2 type1 type2))
                 (res2 (sb-kernel::%type-union2 type2 type1)))
            ;; The (ARRAY :SIMPLE-=) type method doesn't always (or
            ;; even usually) check the element type, preferring
            ;; instead to check the upgraded element type.  Therefore,
            ;; check the element types ourselves.
            (assert (and (sb-kernel:type= res1 rtype)
                         (sb-kernel:type=
                          (sb-kernel:array-type-element-type res1)
                          (sb-kernel:array-type-element-type rtype))))
            (assert (and (sb-kernel:type= res2 rtype)
                         (sb-kernel:type=
                          (sb-kernel:array-type-element-type res2)
                          (sb-kernel:array-type-element-type rtype))))))))

  (unity-test (:array-type-union :basic)
              array
              array
              array)

  (unity-test (:array-type-union :dimensional-compatability :wild)
              (array * *)
              (array * (* * *))
              (array * *))

  (disunity-test (:array-type-union :dimensional-compatability :incompatible 1)
                 (array * (* *))
                 (array * (* * *)))

  (disunity-test (:array-type-union :dimensional-compatability :incompatible 2)
                 (array * (* 1 *))
                 (array * (* 2 *)))

  (disunity-test (:array-type-union :dimensional-unity :only-one-dimension-per-union)
              (array * (2 *))
              (array * (* 3)))

  (unity-test (:array-type-union :complexp-unity :moves-towards-maybe)
              simple-array
              (and array (not simple-array))
              array)

  (disunity-test (:array-type-union :complexp-and-dimensions-dont-unite 1)
                 (simple-array * (* *))
                 (and (array * (* 3)) (not simple-array)))

  (disunity-test (:array-type-union :complexp-and-dimensions-dont-unite 2)
              (simple-array * (* *))
              (array * (* 3)))

  (unity-test (:array-type-union :element-subtypes :unite-within-saetp)
              (array (integer 15 27))
              (array (integer 15 26))
              (array (integer 15 27)))

  (disunity-test (:array-type-union :element-subtypes :dont-unite-across-saetp)
                 (array (unsigned-byte 7))
                 (array (unsigned-byte 3)))

  (disunity-test (:array-type-union :disjoint-element-types :dont-unite)
                 (array (integer 15 27))
                 (array (integer 17 30)))

  (unity-test (:array-type-union :wild-element-type :unites)
              array
              (array (unsigned-byte 8))
              array)

  (disunity-test (:array-type-union :element-type-and-dimensions-dont-unite)
                 (array (unsigned-byte 8))
                 (array * (* *)))

  (disunity-test (:array-type-union :element-type-and-complexp-dont-unite)
                 (simple-array (unsigned-byte 8))
                 (and array (not simple-array)))

  ;; These tests aren't really impure once the SHUFFLE function is provided.
  ;; Logically they belong with the above, so here they are.
  (with-test (:name :union-of-all-arrays-is-array-of-wild)
    (flet ((huge-union (fn)
             (mapcar fn sb-kernel::*specialized-array-element-types*)))

      (let ((answers '(VECTOR
                       (SIMPLE-ARRAY * (*))
                       (AND VECTOR (NOT SIMPLE-ARRAY))
                       (VECTOR * 400)
                       (SIMPLE-ARRAY * (400))
                       (AND (VECTOR * 400) (NOT SIMPLE-ARRAY)))))
        (dolist (dim '(() (400)))
          (dolist (simpleness '(() (simple-array) ((not simple-array))))
            (assert
             (equal
              (sb-kernel:type-specifier
               (sb-kernel:specifier-type
                `(or ,@(huge-union
                        (lambda (x) `(and ,@simpleness (vector ,x ,@dim)))))))
              (pop answers))))))

      ;; The algorithm is indifferent to non-array types.
      (assert
       (equal (sb-kernel:type-specifier
               (sb-kernel:specifier-type
                `(or list ,@(huge-union (lambda (x) `(array ,x (1 1 1)))))))
              '(or cons null (array * (1 1 1)))))

      ;; And unions of unions of distinct array types should reduce.
      (assert
       (equal
        (sb-kernel:type-specifier
         (sb-kernel:specifier-type
          `(or (simple-array bletch (3 2 8))
               ,@(huge-union
                  (lambda (x) `(and (not simple-array) (array ,x (2 2)))))
               function
               ,@(huge-union (lambda (x) `(simple-array ,x (10)))))))
        '(or (simple-array bletch (3 2 8))
             (and (array * (2 2)) (not simple-array))
             function
             (simple-array * (10)))))

      ;; After uniting all simple and non-simple arrays of every specializer
      ;; the result is just ARRAY.
      (flet ((u (rank)
               (shuffle ; should be insensitive to ordering
                (nconc (huge-union
                        (lambda (x) `(and (not simple-array) (array ,x ,rank))))
                       (huge-union
                        (lambda (x) `(and (simple-array) (array ,x ,rank))))))))
        (assert
         (equal (sb-kernel:type-specifier
                 (sb-kernel:specifier-type `(or bit-vector ,@(u 2))))
                '(or bit-vector (array * (* *)))))
        (assert
         (equal (sb-kernel:type-specifier
                 (sb-kernel:specifier-type `(or bit-vector ,@(u 1))))
                'vector))))))

(with-test (:name :source-transform-union-of-arrays-typep)
  ;; Ensure we don't pessimize rank 1 specialized array.
  ;; (SIMPLE unboxed vector is done differently)
  (let* ((hair (sb-kernel:specifier-type '(sb-kernel:unboxed-array 1)))
         (xform (sb-c::source-transform-union-typep 'myobj hair))
         (g (caar (cadr xform)))) ; (LET ((#:g MYOBJ)) ...)
    (assert (equal xform
                   `(let ((,g myobj))
                      (or (typep ,g '(and vector (not (array t)))))))))

  ;; Exclude one subtype at a time and make sure they all work.
  (dotimes (i (length sb-vm:*specialized-array-element-type-properties*))
    (let* ((excluded-type
            (sb-vm:saetp-specifier
             (aref sb-vm:*specialized-array-element-type-properties* i)))
           (hair
            (loop for x across sb-vm:*specialized-array-element-type-properties*
                  for j from 0
                  unless (eql i j)
                  collect `(array ,(sb-vm:saetp-specifier x))))
           (xform
            (sb-c::source-transform-union-typep 'myobj
             (sb-kernel:specifier-type `(or ,@(shuffle hair) fixnum))))
           (g (caar (cadr xform)))) ; (LET ((#:g MYOBJ)) ...)
      (assert (equal xform
                     `(let ((,g myobj))
                        (or (typep ,g '(and array (not (array ,excluded-type))))
                            (typep ,g 'fixnum))))))))

(with-test (:name :interned-type-specifiers)
  ;; In general specifiers can repeatedly parse the same due to
  ;; the caching in VALUES-SPECIFIER-TYPE, provided that the entry
  ;; was not evicted. Here we want to check a stronger condition,
  ;; that they really always parse to the identical object.
  (flet ((try (specifier)
           (let ((parse1 (sb-kernel:specifier-type specifier)))
             (sb-int:drop-all-hash-caches)
             (let ((parse2 (sb-kernel:specifier-type specifier)))
               (assert (eq parse1 parse2))))))
    (mapc #'try
          '((satisfies keywordp) ; not the same as KEYWORD
            boolean
            cons
            null
            character
            integer
            bit
            ;; one-dimensional arrays of unknown type
            (array * (*))
            (simple-array * (*))
            (and (array * (*)) (not simple-array))
            ;; floating-point
            single-float
            double-float
            (complex single-float)
            (complex double-float)))
    ;; and check all specialized arrays
    (dolist (spec sb-kernel::*specialized-array-element-types*)
      (try `(array ,spec (*)))
      (try `(simple-array ,spec (*)))
      (try `(and (not simple-array) (array ,spec (*)))))))

;; The expansion of FRUITBAT is a class itself, not its name.
(deftype fruitbat () (find-class 'hash-table))
(with-test (:name :typexpand-into-classoid)
  (assert (eq (sb-kernel:specifier-type 'fruitbat)
              (sb-kernel:find-classoid 'hash-table))))

;;; success
