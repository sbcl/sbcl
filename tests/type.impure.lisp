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

(with-test (:name (subtypep or))
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
      (dolist (j types)
        (assert (subtypep i `(or ,i ,j)))
        (assert (subtypep i `(or ,j ,i)))
        (assert (subtypep i `(or ,i ,i ,j)))
        (assert (subtypep i `(or ,j ,i)))
        (dolist (k types)
          (assert (subtypep `(or ,i ,j) `(or ,i ,j ,k)))
          (assert (subtypep `(or ,i ,j) `(or ,k ,j ,i))))))))

;;; gotchas that can come up in handling subtypeness as "X is a
;;; subtype of Y if each of the elements of X is a subtype of Y"
(with-test (:name (subtypep single-float real or))
  ;; The system isn't expected to understand the subtype
  ;; relationship. But if it does, that'd be neat.
  (multiple-value-bind (subtypep certainp)
      (subtypep '(single-float -1.0 1.0)
                '(or (real -100.0 0.0)
                  (single-float 0.0 100.0)))
    (assert (or (and subtypep certainp)
                (and (not subtypep) (not certainp))))))

(with-test (:name (subtypep single-float float))
  (assert (subtypep 'single-float 'float)))

(with-test (:name (:type= integer :ranges or))
  (assert (type-evidently-= '(integer 0 10)
                            '(or (integer 0 5) (integer 4 10)))))

;;; Bug 50(c,d): numeric types with empty ranges should be NIL
(with-test (:name (:type= integer rational float :empty :bug-50c :bug-50d))
  (assert (type-evidently-= 'nil '(integer (0) (0))))
  (assert (type-evidently-= 'nil '(rational (0) (0))))
  (assert (type-evidently-= 'nil '(float (0.0) (0.0)))))

;;; sbcl-0.6.10 did (UPGRADED-ARRAY-ELEMENT-TYPE 'SOME-UNDEF-TYPE)=>T
;;; and (UPGRADED-COMPLEX-PART-TYPE 'SOME-UNDEF-TYPE)=>T.
(with-test (:name (upgraded-array-element-type :undefined))
  (assert-error (upgraded-array-element-type 'some-undef-type))
  (assert (eql (upgraded-array-element-type t) t)))

(with-test (:name (upgraded-complex-part-type :undefined))
  (assert-error (upgraded-complex-part-type 'some-undef-type))
  (assert (subtypep (upgraded-complex-part-type 'fixnum) 'real)))

;;; Do reasonable things with undefined types, and with compound types
;;; built from undefined types.
(with-test (:name (typep :undefined :compound))
  (assert (typep #(11) '(simple-array t 1)))
  (assert (typep #(11) '(simple-array (or integer symbol) 1)))
  (assert-error (typep #(11) '(simple-array undef-type 1)))
  (assert (not (typep 11 '(simple-array undef-type 1)))))
(with-test (:name (subtypep :undefined :compound))
  (assert (subtypep '(vector some-undef-type) 'vector))
  (assert (not (subtypep '(vector some-undef-type) 'integer)))
  (assert-tri-eq nil nil (subtypep 'utype-1 'utype-2))
  (assert-tri-eq nil nil (subtypep '(vector utype-1) '(vector utype-2)))
  (assert-tri-eq nil nil (subtypep '(vector utype-1) '(vector t)))
  (assert-tri-eq nil nil (subtypep '(vector t) '(vector utype-2))))

;;; ANSI specifically disallows bare AND and OR symbols as type specs.
(with-test (:name (typep :bare :compound error))
  (assert-error (typep 11 'and))
  (assert-error (typep 11 'or))
  (assert-error (typep 11 'member))
  (assert-error (typep 11 'values))
  (assert-error (typep 11 'eql))
  (assert-error (typep 11 'satisfies))
  (assert-error (typep 11 'not)))
;;; and while it doesn't specifically disallow illegal compound
;;; specifiers from the CL package, we don't have any.
(with-test (:name (subtypep :illegal :compound error))
  (assert-error (subtypep 'fixnum '(fixnum 1)))
  (assert-error (subtypep 'class '(list)))
  (assert-error (subtypep 'foo '(ratio 1/2 3/2)))
  (assert-error (subtypep 'character '(character 10))))
#+nil ; doesn't yet work on PCL-derived internal types
(assert-error (subtypep 'lisp '(class)))
#+nil
(assert-error (subtypep 'bar '(method number number)))

;;; Of course empty lists of subtypes are still OK.
(with-test (:name (typep :empty and or))
  (assert (typep 11 '(and)))
  (assert (not (typep 11 '(or)))))

;;; bug 12: type system didn't grok nontrivial intersections
(with-test (:name (subtypep and :bug-12))
  (assert-tri-eq t   t (subtypep '(and symbol (satisfies keywordp)) 'symbol))
  ;; I'm not sure this next test was saying what it thinks it's saying.
  (assert-tri-eq nil t (subtypep '(and symbol (satisfies keywordp)) 'null))
  (assert-tri-eq nil t (subtypep '(and symbol (satisfies keywordp)) 'nil))
  ;; would be nice if this one could say T
  (assert-tri-eq nil nil (subtypep '(satisfies keywordp) 'nil))
  (assert-tri-eq t   t (subtypep 'keyword 'symbol))
  (assert-tri-eq nil t (subtypep 'symbol 'keyword))
  (assert-tri-eq t   t (subtypep 'ratio 'real))
  (assert-tri-eq t   t (subtypep 'ratio 'number)))

;;; bug 50.g: Smarten up hairy type specifiers slightly. We may wish
;;; to revisit this, perhaps by implementing a COMPLEMENT type
;;; (analogous to UNION and INTERSECTION) to take the logic out of the
;;; HAIRY domain.
(with-test (:name (subtypep atom cons :bug-50g))
  (assert-tri-eq nil t (subtypep 'atom 'cons))
  (assert-tri-eq nil t (subtypep 'cons 'atom)))

;;; These two are desireable but not necessary for ANSI conformance;
;;; maintenance work on other parts of the system broke them in
;;; sbcl-0.7.13.11 -- CSR
(with-test (:name (subtypep not atom list cons))
  #+nil
  (assert-tri-eq nil t (subtypep '(not list) 'cons))
  #+nil
  (assert-tri-eq nil t (subtypep '(not float) 'single-float))
  (assert-tri-eq t   t (subtypep '(not atom) 'cons))
  (assert-tri-eq t   t (subtypep 'cons '(not atom))))

;;; ANSI requires that SUBTYPEP relationships among built-in primitive
;;; types never be uncertain, i.e. never return NIL as second value.
;;; Prior to about sbcl-0.7.2.6, ATOM caused a lot of problems here
;;; (because it's a negation type, implemented as a HAIRY-TYPE, and
;;; CMU CL's HAIRY-TYPE logic punted a lot).
(with-test (:name (subtypep integer function atom list))
  (assert-tri-eq t   t (subtypep 'integer 'atom))
  (assert-tri-eq t   t (subtypep 'function 'atom))
  (assert-tri-eq nil t (subtypep 'list 'atom))
  (assert-tri-eq nil t (subtypep 'atom 'integer))
  (assert-tri-eq nil t (subtypep 'atom 'function))
  (assert-tri-eq nil t (subtypep 'atom 'list)))

;;; ATOM is equivalent to (NOT CONS):
(with-test (:name (subtypep atom cons cons))
  (assert-tri-eq t   t (subtypep 'integer '(not cons)))
  (assert-tri-eq nil t (subtypep 'list '(not cons)))
  (assert-tri-eq nil t (subtypep '(not cons) 'integer))
  (assert-tri-eq nil t (subtypep '(not cons) 'list)))

;;; And we'd better check that all the named types are right. (We also
;;; do some more tests on ATOM here, since once CSR experimented with
;;; making it a named type.)
(with-test (:name (subtypep nil atom t))
  (assert-tri-eq t   t (subtypep 'nil 'nil))
  (assert-tri-eq t   t (subtypep 'nil 'atom))
  (assert-tri-eq t   t (subtypep 'nil 't))
  (assert-tri-eq nil t (subtypep 'atom 'nil))
  (assert-tri-eq t   t (subtypep 'atom 'atom))
  (assert-tri-eq t   t (subtypep 'atom 't))
  (assert-tri-eq nil t (subtypep 't 'nil))
  (assert-tri-eq nil t (subtypep 't 'atom))
  (assert-tri-eq t   t (subtypep 't 't)))

;;; Also, LIST is now somewhat special, in that (NOT LIST) should be
;;; recognized as a subtype of ATOM:
(with-test (:name (subtypep not list atom))
  (assert-tri-eq t   t (subtypep '(not list) 'atom))
  (assert-tri-eq nil t (subtypep 'atom '(not list))))

;;; These used to fail, because when the two arguments to subtypep are
;;; of different specifier-type types (e.g. HAIRY and UNION), there
;;; are two applicable type methods -- in this case
;;; HAIRY-COMPLEX-SUBTYPEP-ARG1-TYPE-METHOD and
;;; UNION-COMPLEX-SUBTYPEP-ARG2-TYPE-METHOD. Both of these exist, but
;;; [!%]INVOKE-TYPE-METHOD aren't smart enough to know that if one of
;;; them returns NIL, NIL (indicating uncertainty) it should try the
;;; other. However, as of sbcl-0.7.2.6 or so, CALL-NEXT-METHOD-ish
;;; logic in those type methods fixed it.
(with-test (:name (subtypep not list float))
  (assert-tri-eq nil t (subtypep '(not cons) 'list))
  (assert-tri-eq nil t (subtypep '(not single-float) 'float)))

;;; Somewhere along the line (probably when adding CALL-NEXT-METHOD-ish
;;; logic in SUBTYPEP type methods) we fixed bug 58 too:
(with-test (:name (subtypep and integer :unknown))
  (assert-tri-eq t t (subtypep '(and zilch integer) 'zilch))
  (assert-tri-eq t t (subtypep '(and integer zilch) 'zilch)))

;;; Bug 84: SB-KERNEL:CSUBTYPEP was a bit enthusiastic at
;;; special-casing calls to subtypep involving *EMPTY-TYPE*,
;;; corresponding to the NIL type-specifier; we were bogusly returning
;;; NIL, T (indicating surety) for the following:
(with-test (:name (subtypep satisfies :undefined-function nil :bug-84))
  (assert-tri-eq nil nil (subtypep '(satisfies some-undefined-fun) 'nil)))

;;; It turns out that, as of sbcl-0.7.2, we require to be able to
;;; detect this to compile src/compiler/node.lisp (and in particular,
;;; the definition of the component structure). Since it's a sensible
;;; thing to want anyway, let's test for it here:
(with-test (:name (subtypep or :unknown member))
  (assert-tri-eq t t (subtypep '(or some-undefined-type (member :no-ir2-yet :dead))
                               '(or some-undefined-type (member :no-ir2-yet :dead)))))

;;; BUG 158 (failure to compile loops with vector references and
;;; increments of greater than 1) was a symptom of type system
;;; uncertainty, to wit:
(with-test (:name (subtypep and or mod integer :bug-158))
  (assert-tri-eq t t (subtypep '(and (mod 536870911) (or (integer 0 0) (integer 2 536870912)))
                               '(mod 536870911)))) ; aka SB-INT:INDEX.

;;; floating point types can be tricky.
(with-test (:name (subtypep float single-float double-float member not))
  (assert-tri-eq t t (subtypep '(member 0.0) '(single-float 0.0 0.0)))
  (assert-tri-eq t t (subtypep '(member -0.0) '(single-float 0.0 0.0)))
  (assert-tri-eq t t (subtypep '(member 0.0) '(single-float -0.0 0.0)))
  (assert-tri-eq t t (subtypep '(member -0.0) '(single-float 0.0 -0.0)))
  (assert-tri-eq t t (subtypep '(member 0.0d0) '(double-float 0.0d0 0.0d0)))
  (assert-tri-eq t t (subtypep '(member -0.0d0) '(double-float 0.0d0 0.0d0)))
  (assert-tri-eq t t (subtypep '(member 0.0d0) '(double-float -0.0d0 0.0d0)))
  (assert-tri-eq t t (subtypep '(member -0.0d0) '(double-float 0.0d0 -0.0d0)))

  (assert-tri-eq nil t (subtypep '(single-float 0.0 0.0) '(member 0.0)))
  (assert-tri-eq nil t (subtypep '(single-float 0.0 0.0) '(member -0.0)))
  (assert-tri-eq nil t (subtypep '(single-float -0.0 0.0) '(member 0.0)))
  (assert-tri-eq nil t (subtypep '(single-float 0.0 -0.0) '(member -0.0)))
  (assert-tri-eq nil t (subtypep '(double-float 0.0d0 0.0d0) '(member 0.0d0)))
  (assert-tri-eq nil t (subtypep '(double-float 0.0d0 0.0d0) '(member -0.0d0)))
  (assert-tri-eq nil t (subtypep '(double-float -0.0d0 0.0d0) '(member 0.0d0)))
  (assert-tri-eq nil t (subtypep '(double-float 0.0d0 -0.0d0) '(member -0.0d0)))

  (assert-tri-eq t t (subtypep '(member 0.0 -0.0) '(single-float 0.0 0.0)))
  (assert-tri-eq t t (subtypep '(single-float 0.0 0.0) '(member 0.0 -0.0)))
  (assert-tri-eq t t (subtypep '(member 0.0d0 -0.0d0) '(double-float 0.0d0 0.0d0)))
  (assert-tri-eq t t (subtypep '(double-float 0.0d0 0.0d0) '(member 0.0d0 -0.0d0)))

  (assert-tri-eq t t (subtypep '(not (single-float 0.0 0.0)) '(not (member 0.0))))
  (assert-tri-eq t t (subtypep '(not (double-float 0.0d0 0.0d0)) '(not (member 0.0d0))))

  (assert-tri-eq t t (subtypep '(float -0.0) '(float 0.0)))
  (assert-tri-eq t t (subtypep '(float 0.0) '(float -0.0)))
  (assert-tri-eq t t (subtypep '(float (0.0)) '(float (-0.0))))
  (assert-tri-eq t t (subtypep '(float (-0.0)) '(float (0.0)))))

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
(with-test (:name :add-subclassoid)
  (flet ((has-subs (name n)
           (= n (length (sb-kernel:classoid-subclasses
                         (sb-kernel:find-classoid name))))))
  (assert (has-subs 'condition-foo1 3)) ; has foo{2,3,4}
  (assert (has-subs 'condition-foo2 2)) ; has foo{3,4}
  (assert (has-subs 'condition-foo3 1)) ; has foo4
  (assert (has-subs 'condition-foo4 0))))

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
     (assert (eq (car (sb-mop:class-direct-superclasses
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
                      (sb-mop:class-direct-subclasses (find-class
                                                       'simple-condition))
                      subclasses))))

     ;; precedence lists
     (assert (equal (sb-mop:class-precedence-list
                     (find-class 'simple-condition))
                    (mapcar #'find-class '(simple-condition
                                           condition
                                           sb-pcl::slot-object
                                           t))))
    (sb-mop:finalize-inheritance (find-class 'fundamental-stream))
     ;; stream classes
     (assert (equal (sb-mop:class-direct-superclasses (find-class
                                                       'fundamental-stream))
                    (mapcar #'find-class '(standard-object stream))))
     (assert (null (set-difference
                    (sb-mop:class-direct-subclasses (find-class
                                                     'fundamental-stream))
                    (mapcar #'find-class '(fundamental-binary-stream
                                           fundamental-character-stream
                                           fundamental-output-stream
                                           fundamental-input-stream)))))
     (assert (equal (sb-mop:class-precedence-list (find-class
                                                   'fundamental-stream))
                    (mapcar #'find-class '(fundamental-stream
                                           standard-object
                                           sb-pcl::slot-object
                                           stream
                                           t))))
     (assert (equal (sb-mop:class-precedence-list (find-class
                                                   'fundamental-stream))
                    (mapcar #'find-class '(fundamental-stream
                                           standard-object
                                           sb-pcl::slot-object stream
                                           t))))
     (assert (subtypep (find-class 'stream) (find-class t)))
     (assert (subtypep (find-class 'fundamental-stream) 'stream))
     (assert (not (subtypep 'stream 'fundamental-stream)))))

;;; Test under the interpreter.
(with-test (:name (:inline-type-tests :interpreted))
  (eval *tests-of-inline-type-tests*)
  (format t "~&/done with interpreted *TESTS-OF-INLINE-TYPE-TESTS*~%"))

;;; Test under the compiler.
(defun tests-of-inline-type-tests ()
  #.*tests-of-inline-type-tests*)
(with-test (:name (:inline-type-tests :compiled))
  (tests-of-inline-type-tests)
  (format t "~&/done with compiled (TESTS-OF-INLINE-TYPE-TESTS)~%"))

;;; Redefinition of classes should alter the type hierarchy (BUG 140):
(defclass superclass () ())
(defclass maybe-subclass () ())
(assert-tri-eq nil t (subtypep 'maybe-subclass 'superclass))
(defclass maybe-subclass (superclass) ())
(assert-tri-eq t t (subtypep 'maybe-subclass 'superclass))
(defclass maybe-subclass () ())
(assert-tri-eq nil t (subtypep 'maybe-subclass 'superclass))

;;; Prior to sbcl-0.7.6.27, there was some confusion in ARRAY types
;;; specialized on some as-yet-undefined type which would cause this
;;; program to fail (bugs #123 and #165). Verify that it doesn't.
(defun foo (x)
  (declare (type (vector bar) x))
  (aref x 1))
(deftype bar () 'single-float)
(with-test (:name (array :unknown :element-type))
  (assert (eql (foo (make-array 3 :element-type 'bar :initial-element 0.0f0))
               0.0f0)))

(with-test (:name (sb-kernel:type= :bug-260a))
  (let* ((s (gensym))
         (t1 (sb-kernel:specifier-type s)))
    (eval `(defstruct ,s))
    (assert-tri-eq t t (sb-kernel:type= t1 (sb-kernel:specifier-type s)))))

;;; bug found by PFD's random subtypep tester
(with-test (:name (subtypep cons rational integer single-float))
  (let ((t1 '(cons rational (cons (not rational) (cons integer t))))
        (t2 '(not (cons (integer 0 1) (cons single-float long-float)))))
    (assert-tri-eq t   t (subtypep t1 t2))
    (assert-tri-eq nil t (subtypep t2 t1))
    (assert-tri-eq t   t (subtypep `(not ,t2) `(not ,t1)))
    (assert-tri-eq nil t (subtypep `(not ,t1) `(not ,t2)))))

;;; not easily visible to user code, but this used to be very
;;; confusing.
(with-test (:name (:ctor typep function))
  (assert (eval '(typep (sb-pcl::ensure-ctor
                         (list 'sb-pcl::ctor (gensym)) nil nil nil)
                        'function))))
(with-test (:name (:ctor functionp))
  (assert (functionp (sb-pcl::ensure-ctor
                      (list 'sb-pcl::ctor (gensym)) nil nil nil))))
;;; some new (2008-10-03) ways of going wrong...
(with-test (:name (:ctor allocate-instance typep function))
  (assert (eval '(typep (allocate-instance (find-class 'sb-pcl::ctor))
                        'function))))
(with-test (:name (:ctor allocate-instance functionp))
  (assert (functionp (allocate-instance (find-class 'sb-pcl::ctor)))))

;;; from PFD ansi-tests
(with-test (:name (subtypep :complex-cons-type))
  (let ((t1 '(cons (cons (cons (real -744833699 -744833699) cons)
                    (integer -234496 215373))
              integer))
        (t2 '(cons (cons (cons integer integer)
                    (integer -234496 215373))
              t)))
    (assert-tri-eq nil t (subtypep `(not ,t2) `(not ,t1)))))

(defstruct misc-629a)
(defclass misc-629b () ())
(defclass misc-629c () () (:metaclass sb-mop:funcallable-standard-class))

(with-test (:name (typep subtypep defstruct defclass))
  (assert (typep (make-misc-629a) 'sb-kernel:instance))
  (assert-tri-eq t t (subtypep `(member ,(make-misc-629a)) 'sb-kernel:instance))
  (assert-tri-eq nil t (subtypep `(and (member ,(make-misc-629a)) sb-kernel:instance)
                                 nil))
  (let ((misc-629a (make-misc-629a)))
    (assert-tri-eq t t (subtypep `(member ,misc-629a)
                                 `(and (member ,misc-629a) sb-kernel:instance)))
    (assert-tri-eq t t (subtypep `(and (member ,misc-629a)
                                       sb-kernel:funcallable-instance)
                                 nil)))

  (assert (typep (make-instance 'misc-629b) 'sb-kernel:instance))
  (assert-tri-eq t t (subtypep `(member ,(make-instance 'misc-629b))
                               'sb-kernel:instance))
  (assert-tri-eq nil t (subtypep `(and (member ,(make-instance 'misc-629b))
                                       sb-kernel:instance)
                                 nil))
  (let ((misc-629b (make-instance 'misc-629b)))
    (assert-tri-eq t t (subtypep `(member ,misc-629b)
                                 `(and (member ,misc-629b) sb-kernel:instance)))
    (assert-tri-eq t t (subtypep `(and (member ,misc-629b)
                                       sb-kernel:funcallable-instance)
                                 nil)))

  (assert (typep (make-instance 'misc-629c) 'sb-kernel:funcallable-instance))
  (assert-tri-eq t t (subtypep `(member ,(make-instance 'misc-629c))
                               'sb-kernel:funcallable-instance))
  (assert-tri-eq nil t (subtypep `(and (member ,(make-instance 'misc-629c))
                                       sb-kernel:funcallable-instance)
                                 nil))
  (let ((misc-629c (make-instance 'misc-629c)))
    (assert-tri-eq t t (subtypep `(member ,misc-629c)
                                 `(and (member ,misc-629c)
                                       sb-kernel:funcallable-instance)))
    (assert-tri-eq t t (subtypep `(and (member ,misc-629c)
                                       sb-kernel:instance)
                                 nil))))

;;; this was broken during the FINALIZE-INHERITANCE rearrangement; the
;;; MAKE-INSTANCE finalizes the superclass, thus invalidating the
;;; subclass, so SUBTYPEP must be prepared to deal with
(defclass ansi-tests-defclass1 () ())
(defclass ansi-tests-defclass3 (ansi-tests-defclass1) ())
(with-test (:name (subtypep defclass make-instance))
  (make-instance 'ansi-tests-defclass1)
  (assert-tri-eq t t (subtypep 'ansi-tests-defclass3 'standard-object)))

;;; so was this
(with-test (:name (type-of defclass :undefine))
  (let ((class (eval '(defclass to-be-type-ofed () ()))))
    (setf (find-class 'to-be-type-ofed) nil)
    (assert (eq (type-of (make-instance class)) class))))

;;; accuracy of CONS :SIMPLE-TYPE-=
(deftype goldbach-1 () '(satisfies even-and-greater-then-two-p))
(deftype goldbach-2 () '(satisfies sum-of-two-primes-p))

(with-test (:name (sb-kernel:type= cons satisfies integer))
  (assert-tri-eq t t
   (sb-kernel:type= (sb-kernel:specifier-type '(cons goldbach-1 integer))
                    (sb-kernel:specifier-type '(cons goldbach-1 integer))))

  ;; See FIXME in type method for CONS :SIMPLE-TYPE-=
  #+nil
  (assert-tri-eq nil t
   (sb-kernel:type= (sb-kernel:specifier-type '(cons goldbach-1 integer))
                    (sb-kernel:specifier-type '(cons goldbach-1 single-float))))

  (assert-tri-eq nil nil
   (sb-kernel:type= (sb-kernel:specifier-type '(cons goldbach-1 integer))
                    (sb-kernel:specifier-type '(cons goldbach-2 single-float)))))

;;; precise unions of array types (was bug 306a)
(defun bug-306-a (x)
  (declare (optimize speed)
           (type (or (array cons) (array vector)) x))
  (elt (aref x 0) 0))
(with-test (:name (array :element-type aref optimize speed :bug-306-a))
  (assert (= 0 (bug-306-a #((0))))))

;;; FUNCALLABLE-INSTANCE is a subtype of function.
(with-test (:name (subtypep function sb-kernel:funcallable-instance))
  (assert-tri-eq t t (subtypep '(and pathname function) nil))
  (assert-tri-eq t t (subtypep '(and pathname sb-kernel:funcallable-instance) nil))
  (assert (not (subtypep '(and stream function) nil)))
  (assert (not (subtypep '(and stream sb-kernel:funcallable-instance) nil)))
  (assert (not (subtypep '(and function standard-object) nil)))
  (assert (not (subtypep '(and sb-kernel:funcallable-instance standard-object) nil))))

;;; also, intersections of classes with INSTANCE should not be too
;;; general
(with-test (:name (subtypep standard-object sb-kernel:instance))
  (assert (not (typep #'print-object '(and standard-object sb-kernel:instance))))
  (assert (not (subtypep 'standard-object '(and standard-object sb-kernel:instance)))))

(with-test (:name (subtypep simple-array simple-string condition or))
  (assert-tri-eq t t
                 (subtypep '(or simple-array simple-string) '(or simple-string simple-array)))
  (assert-tri-eq t t
                 (subtypep '(or simple-string simple-array) '(or simple-array simple-string)))
  (assert-tri-eq t t
                 (subtypep '(or fixnum simple-string end-of-file parse-error fixnum vector)
                           '(or fixnum vector end-of-file parse-error fixnum simple-string))))

(with-test (:name (subtypep function compiled-function :interpreted-function))
  (assert-tri-eq nil t (subtypep 'compiled-function nil)) ; lp#1537003
  ;; It is no longer the case that COMPILED-FUNCTION and INTERPRETED-FUNCTION
  ;; form an exhaustive partition of FUNCTION.
  ;; CLHS: "Implementations are free to define other subtypes of FUNCTION"
  (assert-tri-eq nil nil (subtypep '(and function (not compiled-function)
                                 (not sb-kernel:interpreted-function))
                               nil)))

;;; weakening of union type checks
(defun weaken-union-1 (x)
  (declare (optimize speed))
  (car x))
(with-test (:name (:weaken-union type-error 1))
  (assert-error (weaken-union-1 "askdjhasdkj") type-error))

(defun weaken-union-2 (x)
  (declare (optimize speed)
           (type (or cons fixnum) x))
  (etypecase x
    (fixnum x)
    (cons
     (setf (car x) 3)
     x)))
(with-test (:name (:weaken-union type-error 2))
  (multiple-value-bind (res err)
      (ignore-errors (weaken-union-2 "asdkahsdkhj"))
    (assert (not res))
    (assert (typep err 'type-error))
    (assert (or (equal '(or cons fixnum) (type-error-expected-type err))
                (equal '(or fixnum cons) (type-error-expected-type err))))))

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

(with-test (:name sb-ext:typexpand-1)
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

(with-test (:name (sb-ext:typexpand 1))
  (multiple-value-bind (expansion-1 expandedp-1)
      (sb-ext:typexpand '(another-deftype symbol))
    (assert expandedp-1)
    (assert (equal expansion-1 '(cons (eql symbol) *)))
    (multiple-value-bind (expansion-2 expandedp-2)
        (sb-ext:typexpand expansion-1)
      (assert (not expandedp-2))
      (assert (eq expansion-1 expansion-2)))))

(with-test (:name (sb-ext:typexpand 2))
  (assert (equal (sb-ext:typexpand '(list-of-length 3 fixnum))
                 '(cons fixnum (list-of-length 2 fixnum)))))

(with-test (:name sb-ext:typexpand-all)
  (assert (equal (sb-ext:typexpand-all '(list-of-length 3))
                 '(cons t (cons t (cons t null)))))
  (assert (equal (sb-ext:typexpand-all '(list-of-length 3 fixnum))
                 '(cons fixnum (cons fixnum (cons fixnum null))))))

(defclass a-deftype () ())

(with-test (:name (sb-ext:typexpand-1 :after-type-redefinition-to-class))
  (multiple-value-bind (expansion expandedp)
      (sb-ext:typexpand-1 '#1=(a-deftype symbol))
    (assert (not expandedp))
    (assert (eq expansion '#1#))))

(with-test (:name sb-ext:defined-type-name-p)
  (assert (not (sb-ext:defined-type-name-p '#:foo)))
  (assert (sb-ext:defined-type-name-p 'a-deftype))
  (assert (sb-ext:defined-type-name-p 'structure-foo1))
  (assert (sb-ext:defined-type-name-p 'structure-class-foo1))
  (assert (sb-ext:defined-type-name-p 'standard-class-foo1))
  (assert (sb-ext:defined-type-name-p 'condition-foo1))
  (dolist (prim-type '(t nil fixnum cons atom))
    (assert (sb-ext:defined-type-name-p prim-type))))

(with-test (:name (sb-ext:valid-type-specifier-p))
  (macrolet ((yes (spec) `(assert (sb-ext:valid-type-specifier-p ',spec)))
             (no  (spec) `(assert (not (sb-ext:valid-type-specifier-p ',spec)))))
    (no  (cons #(frob) *))
    (no  list-of-length)
    (no  (list-of-length 5 #(x)))
    (yes (list-of-length 5 fixnum))

    (yes structure-foo1)
    (no  (structure-foo1 x))
    (yes condition-foo1)
    (yes standard-class-foo1)
    (yes structure-class-foo1)

    (yes readtable)
    (no  (readtable))
    (no  (readtable x))

    (yes (values))
    (no  values)
    (yes (and))
    (no  and)))

(with-test (:name (sb-ext:valid-type-specifier-p :introspection-test))
  (flet ((map-functions (fn)
           (do-all-symbols (s)
             (when (and (fboundp s)
                        (not (macro-function s))
                        (not (special-operator-p s)))
               (funcall fn s)))))
    (map-functions
     (lambda (name)
       (let* ((fun   (sb-kernel:%fun-fun (fdefinition name)))
              (ftype (sb-kernel:%simple-fun-type fun)))
         (unless (sb-ext:valid-type-specifier-p ftype)
           (error "~@<~S returned NIL on ~S's FTYPE: ~2I~_~S~@:>"
                  'sb-ext:valid-type-specifier-p name ftype)))))))

(with-test (:name (:bug-309128 1))
  (let* ((s (gensym))
         (t1 (sb-kernel:specifier-type s)))
    (eval `(defstruct ,s))
    (assert-tri-eq t t (sb-kernel:csubtypep t1 (sb-kernel:specifier-type s)))))

(with-test (:name (:bug-309128 2))
  (let* ((s (gensym))
         (t1 (sb-kernel:specifier-type s)))
    (eval `(defstruct ,s))
    (assert-tri-eq t t (sb-kernel:csubtypep (sb-kernel:specifier-type s) t1))))

(with-test (:name (:bug-309128 3))
  (let* ((s (gensym))
         (t1 (sb-kernel:specifier-type s))
         (s2 (gensym))
         (t2 (sb-kernel:specifier-type s2)))
    (eval `(deftype ,s2 () ',s))
    (eval `(defstruct ,s))
    (assert-tri-eq t t (sb-kernel:csubtypep t1 t2))))

(with-test (:name (sb-kernel:type= :unknown-type :not-equal))
  (let* ((type (gensym "FOO"))
         (spec1 (sb-kernel:specifier-type `(vector ,type)))
         (spec2 (sb-kernel:specifier-type `(vector single-float))))
    (eval `(deftype ,type () 'double-float))
    (assert-tri-eq nil t (sb-kernel:type= spec1 spec2))))

(defclass subtypep-fwd-test1 (subtypep-fwd-test-unknown1) ())
(defclass subtypep-fwd-test2 (subtypep-fwd-test-unknown2) ())
(defclass subtypep-fwd-testb1 (subtypep-fwd-testb-unknown1) ())
(defclass subtypep-fwd-testb2 (subtypep-fwd-testb-unknown2 subtypep-fwd-testb1) ())
(with-test (:name (subtypep :forward-referenced-classes))
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
                 (and array (not simple-array))))

;;; These tests aren't really impure once the SHUFFLE function is provided.
;;; Logically they belong with the above, so here they are.
(with-test (:name :union-of-all-arrays-is-array-of-wild)
  (flet ((huge-union (fn)
           (map 'list (lambda (x) (funcall fn (sb-vm:saetp-specifier x)))
                sb-vm:*specialized-array-element-type-properties*)))

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
    (let ((result (sb-kernel:type-specifier
                   (sb-kernel:specifier-type
                    `(or list ,@(huge-union (lambda (x) `(array ,x (1 1 1)))))))))
      (assert (or (equal result '(or list (array * (1 1 1))))
                  (equal result '(or (array * (1 1 1)) list)))))

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
              'vector)))))

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
    (sb-int:dovector (saetp sb-vm:*specialized-array-element-type-properties*)
      (let ((spec (sb-vm:saetp-specifier saetp)))
        (try `(array ,spec (*)))
        (try `(simple-array ,spec (*)))
        (try `(and (not simple-array) (array ,spec (*))))))))

;; The expansion of FRUITBAT is a class itself, not its name.
(deftype fruitbat () (find-class 'hash-table))
(with-test (:name :typexpand-into-classoid)
  (assert (eq (sb-kernel:specifier-type 'fruitbat)
              (sb-kernel:find-classoid 'hash-table))))

(deftype foofa () 'single-float)
(with-test (:name :redefine-deftype-to-defstruct)
  (defstruct foofa (a nil :type foofa)))

(with-test (:name :undefine-class)
  (let ((class (gensym "CLASS")))
    (eval `(progn (defclass ,class () ())
                  (lambda (x) (typep x ',class))
                  (setf (find-class ',class) nil)))
    (checked-compile-and-assert (:allow-style-warnings t)
        `(lambda (x) (typep x ',class))
      ((10) (condition '(or error sb-kernel:parse-unknown-type))))
    (assert (handler-case (not (sb-kernel:specifier-type class))
              (sb-kernel:parse-unknown-type ()
                t)))))

;;; Try depthoid in excess of sb-kernel::layout-id-vector-fixed-capacity
(defstruct d2)                ; depthoid = 2
(defstruct (d3(:include d2))) ; = 3
(defstruct (d4(:include d3))) ; and so on
(defstruct (d5(:include d4)))
(defstruct (d6(:include d5)))
(defstruct (d7(:include d6)))
(defstruct (d8(:include d7)))
(compile 'd8-p)
(with-test (:name :deep-structure-is-a)
  (assert (d8-p (opaque-identity (make-d8)))))

(with-test (:name :intersection-complex-=)
  (let ((unk (sb-kernel:specifier-type '(and unknown unknown2))))
    (assert-tri-eq nil nil (sb-kernel:type= (sb-kernel:specifier-type t) unk))
    (assert-tri-eq nil nil (sb-kernel:type= (sb-kernel:specifier-type 'integer) unk))
    (assert-tri-eq nil nil (sb-kernel:type= (sb-kernel:specifier-type 'float) unk))
    (assert-tri-eq nil nil (sb-kernel:type= (sb-kernel:specifier-type 'pathname) unk))
    (assert-tri-eq nil nil (sb-kernel:type= (sb-kernel:specifier-type 'sequence) unk))))

(with-test (:name :lp-308938) ; got silently fixed in git rev ef8c95377a55
  (multiple-value-bind (answer certain)
      (subtypep '(or (satisfies x) string)
                '(or (satisfies x) integer))
    (assert (and (not answer) (not certain))))
  (multiple-value-bind (answer certain)
      (subtypep 'string '(or (satisfies x) integer))
    (assert (and (not answer) (not certain)))))

(deftype jn-even () '(and integer (or (eql 0) (satisfies f))))
(deftype jn-odd () '(and integer (or (eql 1) (satisfies g))))
(with-test (:name :lp-1528837) ; probably the same as the preceding fix
  (multiple-value-bind (answer certain) (subtypep 'jn-odd 'jn-even)
    (assert (and (not answer) (not certain))))
  (multiple-value-bind (answer certain) (subtypep 'jn-even 'jn-odd)
    (assert (and (not answer) (not certain)))))
