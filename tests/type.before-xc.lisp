;;;; tests of the type system, intended to be executed as soon as
;;;; the cross-compiler is built

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

(unless (find-package "BEFORE-XC-TESTS")
  (make-package "BEFORE-XC-TESTS" :use '("SB-XC" "SB-KERNEL" "SB-INT")))
(do-external-symbols (s "SB-XC") ; Import all symbols from SB-XC, then use CL
  (shadowing-import s "BEFORE-XC-TESTS"))
(import '(sb-kernel::type-union2) "BEFORE-XC-TESTS")
(cl:use-package '("COMMON-LISP") "BEFORE-XC-TESTS")

(in-package "BEFORE-XC-TESTS")

;;; Assert that some of the type specifiers which we claim have unique internal
;;; representations do, and that parsing does not rely critically on
;;; memoization performed in SPECIFIER-TYPE, which is only a best effort
;;; to produce the EQ model object given an EQUAL specifier.
(dolist (type sb-kernel::*special-union-types*)
  (dolist (constituent-type (union-type-types (specifier-type type)))
    (let ((specifier (type-specifier constituent-type)))
      (drop-all-hash-caches)
      (let ((parse (specifier-type specifier)))
        (drop-all-hash-caches)
        (let ((reparse (specifier-type specifier)))
          (aver (eq parse reparse)))))))

(assert (type= (specifier-type '(and fixnum (satisfies foo)))
               (specifier-type '(and (satisfies foo) fixnum))))
(assert (type= (specifier-type '(member 1 2 3))
               (specifier-type '(member 2 3 1))))
(assert (type= (specifier-type '(and (member 1.0 2 3) single-float))
               (specifier-type '(member 1.0))))

(assert (typep "hello" '(and array (not (array t)))))
(assert (typep "hello" 'string))
(assert (typep "hello" 'simple-string))
(assert (typep "hello" 'unboxed-array))
(assert (typep "hello" 'simple-unboxed-array))

(assert (typep #*101 '(and array (not (array t)))))
(assert (typep #*101 'bit-vector))
(assert (typep #*101 'simple-bit-vector))
(assert (typep #*101 'unboxed-array))
(assert (typep #*101 'simple-unboxed-array))

;;; When the host does not have (UNSIGNED-BYTE n), this makes an excellent test.
;;; When the host *does* have it, this test is "suspicious" (in the sense that
;;; it would not necessarily detect a bug in our portable array logic),
;;; but is nonetheless valid, and especially since most other lisps don't
;;; have (UNSIGNED-BYTE 2), it's a pretty reasonable thing to check.
(dovector (x sb-vm:*specialized-array-element-type-properties*)
  (let ((et (sb-vm:saetp-specifier x)))
    ;; Test the numeric array specializations.
    (unless (member et '(nil t base-char character))
      (let ((a (make-array 11 :element-type et)))
        (assert (type= (ctype-of a) (specifier-type `(simple-array ,et (11)))))
        (assert (typep a '(and array (not (array t)))))
        (assert (typep a `(simple-array ,et (11))))
        (assert (typep a `(array ,et (11))))
        (dolist (type-atom '(unboxed-array simple-unboxed-array))
          (assert (typep a type-atom))
          (assert (typep a `(,type-atom *)))
          (assert (typep a `(,type-atom (*))))
          (assert (typep a `(,type-atom (11)))))))))

;;; Here it doesn't matter what we specify as element-type to MAKE-ARRAY
;;; because it introspects as if it were SIMPLE-VECTOR due to
;;; non-use of make-specialized-array.
;;; (Note use of CL:MAKE-ARRAY. This file of tests causes symbol lookup
;;; to default to using the SB-XC symbol otherwise)
(assert (type= (ctype-of (cl:make-array 11 :element-type '(signed-byte 8)))
               (specifier-type '(simple-vector 11))))

(assert (typep #(1 2 3) 'simple-vector))
(assert (typep #(1 2 3) 'vector))
(assert (not (typep '(1 2 3) 'vector)))
(assert (not (typep 1 'vector)))

(assert (typep '(1 2 3) 'list))
(assert (typep '(1 2 3) 'cons))
(assert (not (typep '(1 2 3) 'null)))
(assert (not (typep "1 2 3" 'list)))
(assert (not (typep 1 'list)))

(assert (typep nil 'null))
(assert (typep nil '(member nil)))
(assert (typep nil '(member 1 2 nil 3)))
(assert (not (typep nil '(member 1 2 3))))

(assert (type= *empty-type*
               (type-intersection (specifier-type 'list)
                                  (specifier-type 'vector))))
(assert (type= *empty-type*
               (type-intersection (specifier-type 'list)
                                  (specifier-type 'vector))))
(assert (type= (specifier-type 'null)
               (type-intersection (specifier-type 'list)
                                  (specifier-type '(or vector null)))))
(assert (type= (specifier-type 'null)
               (type-intersection (specifier-type 'sequence)
                                  (specifier-type 'symbol))))
(assert (type= (specifier-type 'cons)
               (type-intersection (specifier-type 'sequence)
                                  (specifier-type '(or cons number)))))
(assert (type= (specifier-type '(simple-array character (*)))
               (type-intersection (specifier-type 'sequence)
                                  (specifier-type '(simple-array character)))))
(assert (type= (specifier-type 'list)
               (type-intersection (specifier-type 'sequence)
                                  (specifier-type 'list))))
(assert (type= *empty-type*
               (type-intersection (specifier-type '(satisfies keywordp))
                                  *empty-type*)))

(assert (type= (specifier-type 'list)
               (type-union (specifier-type 'cons) (specifier-type 'null))))
(assert (type= (specifier-type 'list)
               (type-union (specifier-type 'null) (specifier-type 'cons))))
#+nil ; not any more
(assert (type= (specifier-type 'sequence)
               (type-union (specifier-type 'list) (specifier-type 'vector))))
#+nil ; not any more
(assert (type= (specifier-type 'sequence)
               (type-union (specifier-type 'vector) (specifier-type 'list))))
(assert (type= (specifier-type 'list)
               (type-union (specifier-type 'cons) (specifier-type 'list))))
(let ((sb-kernel::*xtypep-uncertainty-action* nil))
  (assert (not (csubtypep (type-union (specifier-type 'list)
                                      (specifier-type '(satisfies foo)))
                          (specifier-type 'list))))
  (assert (csubtypep (specifier-type 'list)
                     (type-union (specifier-type 'list)
                                 (specifier-type '(satisfies foo))))))

;;; Identities should be identities.
(dolist (type-specifier '(nil
                          t
                          null
                          (satisfies keywordp)
                          (satisfies foo)
                          (not fixnum)
                          (not null)
                          (and symbol (satisfies foo))
                          (and (satisfies foo) string)
                          (or symbol sequence)
                          (or single-float character)
                          (or float (satisfies bar))
                          integer (integer 0 1)
                          character standard-char
                          (member 1 2 3)))
  (let ((ctype (specifier-type type-specifier)))

    (assert (type= *empty-type* (type-intersection ctype *empty-type*)))
    (assert (type= *empty-type* (type-intersection *empty-type* ctype)))
    (assert (type= *empty-type* (type-intersection2 ctype *empty-type*)))
    (assert (type= *empty-type* (type-intersection2 *empty-type* ctype)))

    (assert (type= ctype (type-intersection ctype *universal-type*)))
    (assert (type= ctype (type-intersection *universal-type* ctype)))
    (assert (type= ctype (type-intersection2 ctype *universal-type*)))
    (assert (type= ctype (type-intersection2 *universal-type* ctype)))

    (assert (type= *universal-type* (type-union ctype *universal-type*)))
    (assert (type= *universal-type* (type-union *universal-type* ctype)))
    (assert (type= *universal-type* (type-union2 ctype *universal-type*)))
    (assert (type= *universal-type* (type-union2 *universal-type* ctype)))

    (assert (type= ctype (type-union ctype *empty-type*)))
    (assert (type= ctype (type-union *empty-type* ctype)))
    (assert (type= ctype (type-union2 ctype *empty-type*)))
    (assert (type= ctype (type-union2 *empty-type* ctype)))

    (assert (csubtypep *empty-type* ctype))
    (assert (csubtypep ctype *universal-type*))))

(assert (subtypep 'simple-vector 'vector))
(assert (subtypep 'simple-vector 'simple-array))
(assert (subtypep 'vector 'array))
(assert (not (subtypep 'vector 'simple-vector)))
(assert (not (subtypep 'vector 'simple-array)))

(macrolet ((assert-secondnil (expr) `(assert (null (nth-value 1 ,expr)))))
  (assert-secondnil (subtypep t '(satisfies foo)))
  (assert-secondnil (subtypep t '(and (satisfies foo) (satisfies bar))))
  (assert-secondnil (subtypep t '(or (satisfies foo) (satisfies bar))))
  (assert-secondnil (subtypep '(satisfies foo) nil))
  (assert-secondnil (subtypep '(and (satisfies foo) (satisfies bar))
                                    nil))
  (assert-secondnil (subtypep '(or (satisfies foo) (satisfies bar))
                                    nil)))

;;; tests of 2-value quantifieroids FOO/TYPE
(macrolet ((2= (v1 v2 expr2)
             (let ((x1 (gensym))
                   (x2 (gensym)))
               `(multiple-value-bind (,x1 ,x2) ,expr2
                  (unless (and (eql ,x1 ,v1) (eql ,x2 ,v2))
                    (error "mismatch for EXPR2=~S" ',expr2))))))
  (flet (;; SUBTYPEP running in the cross-compiler
         (xsubtypep (x y)
           (csubtypep (specifier-type x)
                      (specifier-type y))))
    (2=   t   t (any/type   #'xsubtypep 'fixnum '(real integer)))
    (2=   t   t (any/type   #'xsubtypep 'fixnum '(real cons)))
    (2= nil   t (any/type   #'xsubtypep 'fixnum '(cons vector)))
    (2= nil nil (any/type   #'xsubtypep 'fixnum '(cons some-unknown-type-foo)))
    (2= nil nil (any/type   #'xsubtypep 'fixnum '(some-unknown-type-foo cons)))
    (2=   t   t (any/type   #'xsubtypep 'fixnum '(some-unknown-type-foo real)))
    (2=   t   t (any/type   #'xsubtypep 'fixnum '(real some-unknown-type-foo)))
    (2= nil   t (any/type   #'xsubtypep 'fixnum '()))
    (2=   t   t (every/type #'xsubtypep 'fixnum '()))
    (2= nil nil (every/type #'xsubtypep 'fixnum '(real some-unknown-type-foo)))
    (2= nil nil (every/type #'xsubtypep 'fixnum '(some-unknown-type-foo real)))
    (2= nil   t (every/type #'xsubtypep 'fixnum '(some-unknown-type-foo cons)))
    (2= nil   t (every/type #'xsubtypep 'fixnum '(cons some-unknown-type-foo)))
    (2=   t   t (every/type #'xsubtypep 'fixnum '(real integer)))
    (2= nil   t (every/type #'xsubtypep 'fixnum '(real cons)))
    (2= nil   t (every/type #'xsubtypep 'fixnum '(cons vector)))))

;;; various dead bugs
(assert (union-type-p (type-intersection (specifier-type 'list)
                                         (specifier-type '(or list vector)))))
(assert (type= (type-intersection (specifier-type 'list)
                                  (specifier-type '(or list vector)))
               (specifier-type 'list)))
(assert (array-type-p (type-intersection (specifier-type 'vector)
                                         (specifier-type '(or list vector)))))
(assert (type= (type-intersection (specifier-type 'vector)
                                  (specifier-type '(or list vector)))
               (specifier-type 'vector)))
(assert (type= (type-intersection (specifier-type 'number)
                                  (specifier-type 'integer))
               (specifier-type 'integer)))
(let ((sb-kernel::*xtypep-uncertainty-action* nil))
  (assert (null (type-intersection2 (specifier-type 'symbol)
                                    (specifier-type '(satisfies foo)))))
  (assert (intersection-type-p (specifier-type '(and symbol (satisfies foo))))))
(assert (ctypep :x86 (specifier-type '(satisfies keywordp))))
(assert (not (ctypep 'cons (specifier-type '(satisfies keywordp)))))
(assert (type= (specifier-type '(member :x86))
               (specifier-type '(and (member :x86) (satisfies keywordp)))))
#+nil
(let* ((type1 (specifier-type '(member :x86)))
       (type2 (specifier-type '(or keyword null)))
       (isect (type-intersection type1 type2)))
  (assert (type= isect type1))
  (assert (type= isect (type-intersection type2 type1)))
  (assert (type= isect (type-intersection type2 type1 type2)))
  (assert (type= isect (type-intersection type1 type1 type2 type1)))
  (assert (type= isect (type-intersection type1 type2 type1 type2))))
(let* ((type1 (specifier-type 'keyword))
       (type2 (specifier-type '(or keyword null)))
       (isect (type-intersection type1 type2)))
  (assert (type= isect type1))
  (assert (type= isect (type-intersection type2 type1)))
  (assert (type= isect (type-intersection type2 type1 type2)))
  (assert (type= isect (type-intersection type1 type1 type2 type1)))
  (assert (type= isect (type-intersection type1 type2 type1 type2))))
(assert (csubtypep (specifier-type '(or (single-float -1.0 1.0)
                                        (single-float 0.1)))
                   (specifier-type '(or (real -1 7)
                                        (single-float 0.1)
                                        (single-float -1.0 1.0)))))
(assert (not (csubtypep (specifier-type '(or (real -1 7)
                                             (single-float 0.1)
                                             (single-float -1.0 1.0)))
                        (specifier-type '(or (single-float -1.0 1.0)
                                             (single-float 0.1))))))

(assert (typep #\, 'character))
(assert (typep #\@ 'character))

(assert (type= (type-intersection (specifier-type '(member #\a #\c #\e))
                                 (specifier-type '(member #\b #\c #\f)))
              (specifier-type '(member #\c))))

(multiple-value-bind (yes win)
    (subtypep 'package 'instance)
  (assert yes)
  (assert win))
(multiple-value-bind (yes win)
    (subtypep 'symbol 'instance)
  (assert (not yes))
  (assert win))
(multiple-value-bind (yes win)
    (subtypep 'package 'funcallable-instance)
  (assert (not yes))
  (assert win))
(multiple-value-bind (yes win)
    (subtypep 'symbol 'funcallable-instance)
  (assert (not yes))
  (assert win))
(multiple-value-bind (yes win)
    (subtypep 'funcallable-instance 'function)
  (assert yes)
  (assert win))
(multiple-value-bind (yes win)
    (subtypep 'array 'instance)
  (assert (not yes))
  (assert win))
(multiple-value-bind (yes win)
    (subtypep 'character 'instance)
  (assert (not yes))
  (assert win))
(multiple-value-bind (yes win)
    (subtypep 'number 'instance)
  (assert (not yes))
  (assert win))
(multiple-value-bind (yes win)
    (subtypep 'package '(and (or symbol package) instance))
  (assert yes)
  (assert win))
(multiple-value-bind (yes win)
    (subtypep '(and (or double-float integer) instance) 'nil)
  (assert yes)
  (assert win))
(multiple-value-bind (yes win)
    (subtypep '(and (or double-float integer) funcallable-instance) 'nil)
  (assert yes)
  (assert win))
(multiple-value-bind (yes win) (subtypep 'instance 'type-specifier)
  (assert (not yes))
  (assert (not win)))
(multiple-value-bind (yes win) (subtypep 'type-specifier 'instance)
  (assert (not yes))
  (assert win))
(multiple-value-bind (yes win) (subtypep 'class 'type-specifier)
  (assert yes)
  (assert win))
(multiple-value-bind (yes win) (subtypep 'classoid 'type-specifier)
  (assert yes)
  (assert win))
(multiple-value-bind (yes win)
    (subtypep '(and (function (t)) funcallable-instance) 'nil)
  (assert (not win))
  (assert (not yes)))
(multiple-value-bind (yes win)
    (subtypep '(and fixnum function) 'nil)
  (assert yes)
  (assert win))
(multiple-value-bind (yes win)
    (subtypep '(and fixnum hash-table) 'nil)
  (assert yes)
  (assert win))
(multiple-value-bind (yes win)
    (subtypep '(function) '(function (t &rest t)))
  (assert (not yes))
  (assert win))
;; Used to run out of stack.
(let ((sb-kernel::*xtypep-uncertainty-action* nil))
  (multiple-value-bind (yes win)
      (handler-bind ((sb-kernel::cross-type-giving-up #'muffle-warning))
        (subtypep 'null '(or unk0 unk1)))
    (assert (not yes))
    (assert (not win))))

(multiple-value-bind (yes win)
    (subtypep '(and function instance) nil)
  (assert yes)
  (assert win))
(multiple-value-bind (yes win)
    (subtypep nil '(and function instance))
  (assert yes)
  (assert win))
(multiple-value-bind (yes win)
    (subtypep '(and function funcallable-instance) 'funcallable-instance)
  (assert yes)
  (assert win))
(multiple-value-bind (yes win)
    (subtypep 'funcallable-instance '(and function funcallable-instance))
  (assert yes)
  (assert win))
(multiple-value-bind (yes win)
    (subtypep 'stream 'instance)
  (assert (not win))
  (assert (not yes)))
(multiple-value-bind (yes win)
    (subtypep 'stream 'funcallable-instance)
  (assert (not yes))
  (assert win))
(multiple-value-bind (yes win)
    (subtypep '(and stream instance) 'instance)
  (assert yes)
  (assert win))
(multiple-value-bind (yes win)
    (subtypep '(and stream funcallable-instance) 'funcallable-instance)
  (assert yes)
  (assert win))
(multiple-value-bind (yes win)
    (subtypep '(and stream instance) 'stream)
  (assert yes)
  (assert win))
(multiple-value-bind (yes win)
    (subtypep '(and stream funcallable-instance) 'stream)
  (assert yes)
  (assert win))

(assert (type= (specifier-type 'nil)
               (specifier-type '(and symbol funcallable-instance))))

(assert (not (type= (specifier-type '(function (t) (values &optional)))
                    (specifier-type '(function (t) (values))))))

;; Assert that these types are interned by parsing each twice,
;; dropping the specifier-type cache in between.
(dolist (spec '(index cons null boolean character base-char extended-char))
  (let ((a (specifier-type spec)))
    (drop-all-hash-caches)
    (let ((b (specifier-type spec)))
      (assert (eq a b)))))
(drop-all-hash-caches)
;; BOOLEAN's deftype lists the members as (T NIL),
;; but it should also be EQ to (MEMBER NIL T)
(assert (eq (specifier-type '(member nil t)) (specifier-type 'boolean)))

#+x86-64
(progn
  (assert (= (sb-vm::immediate-constant-sc #c(0.0f0 0.0f0))
             sb-vm::fp-complex-single-zero-sc-number))
  (assert (= (sb-vm::immediate-constant-sc #c(0.0d0 0.0d0))
             sb-vm::fp-complex-double-zero-sc-number)))

;;; Unparse a union of (up to) 3 things depending on :sb-unicode as 2 things.
(assert (sb-kernel::brute-force-type-specifier-equalp
         (type-specifier (specifier-type '(or string null)))
         '(or #+sb-unicode string #-sb-unicode base-string null)))

(multiple-value-bind (result exactp)
    (sb-vm::primitive-type (specifier-type 'list))
  (assert (and (eq result (sb-vm::primitive-type-or-lose 'list))
               exactp)))
(multiple-value-bind (result exactp)
    (sb-vm::primitive-type (specifier-type 'cons))
  (assert (and (eq result (sb-vm::primitive-type-or-lose 'list))
               (not exactp))))

(let ((bs (specifier-type 'base-string))
      (not-sbs (specifier-type '(not simple-base-string)))
      (not-ss (specifier-type '(not simple-string))))
  (let ((intersect (type-intersection bs not-sbs)))
    (assert (array-type-p intersect))
    (assert (type= intersect (specifier-type '(and base-string (not simple-array))))))
  ;; should be commutative
  (let ((intersect (type-intersection not-sbs bs)))
    (assert (array-type-p intersect))
    (assert (type= intersect (specifier-type '(and base-string (not simple-array))))))
  ;; test when the righthand side is a larger negation type
  (let ((intersect (type-intersection bs not-ss)))
    (assert (array-type-p intersect))
    (assert (type= intersect (specifier-type '(and base-string (not simple-array))))))
  (let ((intersect (type-intersection not-sbs bs)))
    (assert (array-type-p intersect))
    (assert (type= intersect (specifier-type '(and base-string (not simple-array)))))))

#+sb-unicode
(let ((cs (specifier-type 'sb-kernel::character-string))
      (not-scs (specifier-type '(not sb-kernel:simple-character-string)))
      (not-ss (specifier-type '(not simple-string))))
  (let ((intersect (type-intersection cs not-scs)))
    (assert (array-type-p intersect))
    (assert (type= intersect (specifier-type '(and sb-kernel::character-string (not simple-array))))))
  (let ((intersect (type-intersection not-scs cs)))
    (assert (array-type-p intersect))
    (assert (type= intersect (specifier-type '(and sb-kernel::character-string (not simple-array))))))
  ;; test when the righthand side is a larger negation type
  (let ((intersect (type-intersection cs not-ss)))
    (assert (array-type-p intersect))
    (assert (type= intersect (specifier-type '(and sb-kernel::character-string (not simple-array))))))
  (let ((intersect (type-intersection not-ss cs)))
    (assert (array-type-p intersect))
    (assert (type= intersect (specifier-type '(and sb-kernel::character-string (not simple-array)))))))

#+sb-unicode
(let ((s (specifier-type 'string))
      (not-ss (specifier-type '(not simple-string))))
  (let ((intersect (type-intersection s not-ss)))
    (assert (union-type-p intersect))
    (assert (type= intersect (specifier-type '(and string (not simple-array))))))
  (let ((intersect (type-intersection not-ss s)))
    (assert (union-type-p intersect))
    (assert (type= intersect (specifier-type '(and string (not simple-array)))))))

(let ((left (specifier-type '(array bit (2 2))))
      (right (specifier-type '(not (simple-array bit)))))
  (let ((intersect (type-intersection left right)))
    (assert (array-type-p intersect))
    (assert (type= intersect (specifier-type
                              '(and (array bit (2 2))
                                    (not simple-array)))))))

;;; The instance-typep transform shouldn't need two different lowtag tests
;;; on instance types other than [funcallable-]standard-object.
;;; And for some reason we suppose that STREAM may be either funcallable or not.
(dolist (type '(pathname logical-pathname condition))
  (multiple-value-bind (answer certain)
      (csubtypep (find-classoid type) (specifier-type 'funcallable-instance))
    (assert (and (not answer) certain)))
  (aver (csubtypep (find-classoid type) (specifier-type 'instance))))

(assert (sb-int:list-elts-eq '(a b 1) '(a b 1)))
(assert (not (sb-int:list-elts-eq '(foo) '(foo bar))))
(assert (not (sb-int:list-elts-eq '(foo bar) '(foo))))

(assert (sb-int:list-elements-eql '(a b 1) '(a b 1)))
(assert (sb-int:list-elements-eql '(1.0d0 x y) '(1.0d0 x y)))
(assert (not (sb-int:list-elements-eql '(foo) '(foo bar))))
(assert (not (sb-int:list-elements-eql '(foo bar) '(foo))))

;;; I frankly have no idea whether we really care about the enumerable bit any more,
;;; because while what it's supposed to mean is "could _this_ type which is not a MEMBER
;;; type be internally represented as a MEMBER type?" Wwhat makes it questionable
;;; is that we seldom or never represent numerics as MEMBER. I do not know if this is a
;;; relic of days long past.
(assert (sb-kernel::type-enumerable
         (sb-kernel:specifier-type '(and integer (integer 1 5)))))
(assert (sb-kernel::type-enumerable
         (sb-kernel:specifier-type '(single-float 1.0 1.0))))
