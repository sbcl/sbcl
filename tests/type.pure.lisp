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

(in-package "CL-USER")

(test-util:with-test (:name :typexpand-check-lexenv)
  (flet ((try (f) (assert-error (funcall f 'hash-table 3))))
    (mapc #'try '(typexpand-1 typexpand typexpand-all))))

(locally
  (declare (notinline mapcar))
  (mapcar (lambda (args)
            (destructuring-bind (obj type-spec result) args
              (flet ((matches-result? (x)
                       (eq (if x t nil) result)))
                (assert (matches-result? (typep obj type-spec)))
                (assert (matches-result? (sb-kernel:ctypep
                                          obj
                                          (sb-kernel:specifier-type
                                           type-spec)))))))
          '((nil (or null vector)              t)
            (nil (or number vector)            nil)
            (12  (or null vector)              nil)
            (12  (and (or number vector) real) t))))


;;; This test is motivated by bug #195, which previously had (THE REAL
;;; #(1 2 3)) give an error which prints as "This is not a (OR
;;; SINGLE-FLOAT DOUBLE-FLOAT RATIONAL)".  We ideally want all of the
;;; defined-by-ANSI types to unparse as themselves or at least
;;; something similar (e.g. CHARACTER can unparse to BASE-CHAR, since
;;; the types are equivalent in current SBCL, and EXTENDED-CHAR can
;;; unparse to NIL, since there are no EXTENDED-CHARs currently).
(let ((standard-types '(;; from table 4-2 in section 4.2.3 in the
                        ;; CLHS.
                        arithmetic-error
                        function
                        simple-condition
                        array
                        generic-function
                        simple-error
                        atom
                        hash-table
                        simple-string
                        base-char
                        integer
                        simple-type-error
                        base-string
                        keyword
                        simple-vector
                        bignum
                        list
                        simple-warning
                        bit
                        logical-pathname
                        single-float
                        bit-vector
                        long-float
                        standard-char
                        broadcast-stream
                        method
                        standard-class
                        built-in-class
                        method-combination
                        standard-generic-function
                        cell-error
                        nil
                        standard-method
                        character
                        null
                        standard-object
                        class
                        number
                        storage-condition
                        compiled-function
                        package
                        stream
                        complex
                        package-error
                        stream-error
                        concatenated-stream
                        parse-error
                        string
                        condition
                        pathname
                        string-stream
                        cons
                        print-not-readable
                        structure-class
                        control-error
                        program-error
                        structure-object
                        division-by-zero
                        random-state
                        style-warning
                        double-float
                        ratio
                        symbol
                        echo-stream
                        rational
                        synonym-stream
                        end-of-file
                        reader-error
                        t
                        error
                        readtable
                        two-way-stream
                        extended-char
                        real
                        type-error
                        file-error
                        restart
                        unbound-slot
                        file-stream
                        sequence
                        unbound-variable
                        fixnum
                        serious-condition
                        undefined-function
                        float
                        short-float
                        unsigned-byte
                        floating-point-inexact
                        signed-byte
                        vector
                        floating-point-invalid-operation
                        simple-array
                        warning
                        floating-point-overflow
                        simple-base-string
                        floating-point-underflow
                        simple-bit-vector)))
  (dolist (type standard-types)
    (format t "~&~S~%" type)
    (assert (not (sb-kernel:unknown-type-p (sb-kernel:specifier-type type))))
    (assert (atom (sb-kernel:type-specifier (sb-kernel:specifier-type type))))))

;;; a bug underlying the reported bug #221: The SB-KERNEL type code
;;; signalled an error on this expression.
(subtypep '(function (fixnum) (values package boolean))
          '(function (t) (values package boolean)))

;;; bug reported by Valtteri Vuorik
(compile nil '(lambda () (member (char "foo" 0) '(#\. #\/) :test #'char=)))
(assert (not (equal (multiple-value-list
                     (subtypep '(function ()) '(function (&rest t))))
                    '(nil t))))

(assert (not (equal (multiple-value-list
                     (subtypep '(function (&rest t)) '(function ())))
                    '(t t))))

(assert (subtypep '(function)
                  '(function (&optional * &rest t))))
(assert (equal (multiple-value-list
                (subtypep '(function)
                          '(function (t &rest t))))
               '(nil t)))
(assert (and (subtypep 'function '(function))
             (subtypep '(function) 'function)))

;;; Absent any exciting generalizations of |R, the type RATIONAL is
;;; partitioned by RATIO and INTEGER.  Ensure that the type system
;;; knows about this.  [ the type system is permitted to return NIL,
;;; NIL for these, so if future maintenance breaks these tests that
;;; way, that's fine.  What the SUBTYPEP calls are _not_ allowed to
;;; return is NIL, T, because that's completely wrong. ]
(assert (subtypep '(or integer ratio) 'rational))
(assert (subtypep 'rational '(or integer ratio)))
;;; Likewise, these are allowed to return NIL, NIL, but shouldn't
;;; return NIL, T:
(assert (subtypep t '(or real (not real))))
(assert (subtypep t '(or keyword (not keyword))))
(assert (subtypep '(and cons (not (cons symbol integer)))
                  '(or (cons (not symbol) *) (cons * (not integer)))))
(assert (subtypep '(or (cons (not symbol) *) (cons * (not integer)))
                  '(and cons (not (cons symbol integer)))))
(assert (subtypep '(or (eql 0) (rational (0) 10))
                  '(rational 0 10)))
(assert (subtypep '(rational 0 10)
                  '(or (eql 0) (rational (0) 10))))
;;; Until sbcl-0.7.13.7, union of CONS types when the CDRs were the
;;; same type gave exceedingly wrong results
(assert (null (subtypep '(or (cons fixnum single-float)
                             (cons bignum single-float))
                        '(cons single-float single-float))))
(assert (subtypep '(cons integer single-float)
                  '(or (cons fixnum single-float) (cons bignum single-float))))

(assert (not (nth-value 1 (subtypep '(and null some-unknown-type)
                                    'another-unknown-type))))

;;; bug 46c
(with-test (:name :coerce-function-on-macro)
  (dolist (fun '(and if))
    (assert-error (coerce fun 'function))))

(dotimes (i 100)
  (let ((x (make-array 0 :element-type `(unsigned-byte ,(1+ i)))))
    (eval `(typep ,x (class-of ,x)))))

(assert (not (typep #c(1 2) '(member #c(2 1)))))
(assert (typep #c(1 2) '(member #c(1 2))))
(assert (subtypep 'nil '(complex nil)))
(assert (subtypep '(complex nil) 'nil))
(assert (subtypep 'nil '(complex (eql 0))))
(assert (subtypep '(complex (eql 0)) 'nil))
(assert (subtypep 'nil '(complex (integer 0 0))))
(assert (subtypep '(complex (integer 0 0)) 'nil))
(assert (subtypep 'nil '(complex (rational 0 0))))
(assert (subtypep '(complex (rational 0 0)) 'nil))
(assert (subtypep 'complex '(complex real)))
(assert (subtypep '(complex real) 'complex))
(assert (subtypep '(complex (eql 1)) '(complex (member 1 2))))
(assert (subtypep '(complex ratio) '(complex rational)))
(assert (subtypep '(complex ratio) 'complex))
(assert (equal (multiple-value-list
                (subtypep '(complex (integer 1 2))
                          '(member #c(1 1) #c(1 2) #c(2 1) #c(2 2))))
               '(nil t)))

(assert (typep 0 '(real #.(ash -1 10000) #.(ash 1 10000))))
(assert (subtypep '(real #.(ash -1 1000) #.(ash 1 1000))
                  '(real #.(ash -1 10000) #.(ash 1 10000))))
(assert (subtypep '(real (#.(ash -1 1000)) (#.(ash 1 1000)))
                  '(real #.(ash -1 1000) #.(ash 1 1000))))

;;; Bug, found by Paul F. Dietz
(let* ((x (eval #c(-1 1/2)))
       (type (type-of x)))
  (assert (subtypep type '(complex rational)))
  (assert (typep x type)))

;;; Test derivation of LOG{AND,IOR,XOR} bounds for unsigned arguments.
;;;
;;; Fear the Loop of Doom!
;;;
;;; (In fact, this is such a fearsome loop that executing it with the
;;; evaluator would take ages... Disable it under those circumstances.)
#+#.(cl:if (cl:eq sb-ext:*evaluator-mode* :compile) '(and) '(or))
(with-test (:name (:type-derivation :logical-operations :correctness))
  (let* ((n-bits 5)
         (size (ash 1 n-bits)))
    (labels ((brute-force (a b c d op)
               (loop with min = (ash 1 n-bits)
                     with max = 0
                     for i from a upto b do
                     (loop for j from c upto d do
                           (let ((x (funcall op i j)))
                             (setf min (min min x)
                                   max (max max x))))
                     finally (return (values min max))))
             (test (a b c d op deriver)
               (multiple-value-bind (brute-low brute-high)
                   (brute-force a b c d op)
                 (multiple-value-bind (test-low test-high)
                     (funcall deriver
                              (sb-c::specifier-type `(integer ,a ,b))
                              (sb-c::specifier-type `(integer ,c ,d)))
                   (unless (and (= brute-low test-low)
                                (= brute-high test-high))
                     (format t "FAIL: ~A [~D, ~D] [~D, ~D]~%EXPECTED [~D, ~D] GOT [~D, ~D]~%"
                             op a b c d
                             brute-low brute-high test-low test-high)
                     (assert (and (= brute-low test-low)
                                  (= brute-high test-high))))))))
      (dolist (op '(logand logior logxor))
        (let ((deriver (intern (format nil "~A-DERIVE-UNSIGNED-BOUNDS" op)
                               (find-package :sb-c))))
          (format t "testing type derivation: ~A~%" deriver)
          (loop for a from 0 below size do
                (loop for b from a below size do
                      (loop for c from 0 below size do
                            (loop for d from c below size do
                                  (test a b c d op deriver))))))))))

(with-test (:name (:type-derivation :logical-operations :scaling))
  (let ((type-x1 (sb-c::specifier-type `(integer ,(expt 2 10000)
                                                 ,(expt 2 10000))))
        (type-x2 (sb-c::specifier-type `(integer ,(expt 2 100000)
                                                 ,(expt 2 100000))))
        (type-y (sb-c::specifier-type '(integer 0 1))))
    (dolist (op '(logand logior logxor))
      (let* ((deriver (intern (format nil "~A-DERIVE-TYPE-AUX" op)
                              (find-package :sb-c)))
             (scale (/ (runtime (funcall deriver type-x2 type-y))
                       (runtime (funcall deriver type-x1 type-y)))))
        ;; Linear scaling is good, quadratical bad. Draw the line
        ;; near the geometric mean of the corresponding SCALEs.
        (when (> scale 32)
          (error "Bad scaling of ~a: input 10 times but runtime ~a times as large."
                 deriver scale))))))

;;; subtypep on CONS types wasn't taking account of the fact that a
;;; CONS type could be the empty type (but no other non-CONS type) in
;;; disguise.
(multiple-value-bind (yes win)
    (subtypep '(and function stream) 'nil)
  (multiple-value-bind (cyes cwin)
      (subtypep '(cons (and function stream) t)
                '(cons nil t))
    (assert (eq yes cyes))
    (assert (eq win cwin))))

;;; CONS type subtypep could be too enthusiastic about thinking it was
;;; certain
(multiple-value-bind (yes win)
    (subtypep '(satisfies foo) '(satisfies bar))
  (assert (null yes))
  (assert (null win))
  (multiple-value-bind (cyes cwin)
      (subtypep '(cons (satisfies foo) t)
                '(cons (satisfies bar) t))
    (assert (null cyes))
    (assert (null cwin))))

(multiple-value-bind (yes win)
    (subtypep 'generic-function 'function)
  (assert yes)
  (assert win))
;;; this would be in some internal test suite like type.before-xc.lisp
;;; except that generic functions don't exist at that stage.
(multiple-value-bind (yes win)
    (subtypep 'generic-function 'sb-kernel:funcallable-instance)
  (assert yes)
  (assert win))

;;; all sorts of answers are right for this one, but it used to
;;; trigger an AVER instead.
(subtypep '(function ()) '(and (function ()) (satisfies identity)))

(assert (sb-kernel:unknown-type-p (sb-kernel:specifier-type 'an-unkown-type)))

(assert
 (sb-kernel:type=
  (sb-kernel:specifier-type '(or (simple-array an-unkown-type (*))
                              (simple-array an-unkown-type)))
  (sb-kernel:specifier-type '(or (simple-array an-unkown-type (*))
                              (simple-array an-unkown-type)))))

(assert
 (sb-kernel:type=
  (sb-kernel:specifier-type '(simple-array an-unkown-type (*)))
  (sb-kernel:specifier-type '(simple-array an-unkown-type (*)))))

(assert
 (not
  (sb-kernel:type=
   (sb-kernel:specifier-type '(simple-array an-unkown-type (*)))
   (sb-kernel:specifier-type '(array an-unkown-type (*))))))

(assert
 (not
  (sb-kernel:type=
   (sb-kernel:specifier-type '(simple-array an-unkown-type (7)))
   (sb-kernel:specifier-type '(simple-array an-unkown-type (8))))))

(assert
 (sb-kernel:type/= (sb-kernel:specifier-type 'cons)
                   (sb-kernel:specifier-type '(cons single-float single-float))))

(multiple-value-bind (match win)
    (sb-kernel:type= (sb-kernel:specifier-type '(cons integer))
                     (sb-kernel:specifier-type '(cons)))
  (assert (and (not match) win)))

(assert (typep #p"" 'sb-kernel:instance))
(assert (subtypep '(member #p"") 'sb-kernel:instance))

(with-test (:name (:typep :character-set :negation))
  (flet ((generate-chars ()
           (loop repeat 100
                 collect (code-char (random char-code-limit)))))
    (dotimes (i 1000)
      (let* ((chars (generate-chars))
             (type `(member ,@chars))
             (not-type `(not ,type)))
        (dolist (char chars)
          (assert (typep char type))
          (assert (not (typep char not-type))))
        (let ((other-chars (generate-chars)))
          (dolist (char other-chars)
            (unless (member char chars)
              (assert (not (typep char type)))
              (assert (typep char not-type)))))))))

(with-test (:name (:check-type :store-value :complex-place))
  (let ((a (cons 0.0 2))
        (handler-invoked nil))
    (handler-bind ((error
                    (lambda (c)
                      (declare (ignore c))
                      (assert (not handler-invoked))
                      (setf handler-invoked t)
                      (invoke-restart 'store-value 1))))
      (check-type (car a) integer))
    (assert (eql (car a) 1))))

;;; The VOP FIXNUMP/UNSIGNED-BYTE-64 was broken on x86-64, failing
;;; the first ASSERT below. The second ASSERT takes care that the fix
;;; doesn't overshoot the mark.
(with-test (:name (:typep :fixnum-if-unsigned-byte))
  (let ((f (compile nil
                    '(lambda (x)
                      (declare (type (unsigned-byte #.sb-vm:n-word-bits) x))
                      (typep x (quote fixnum))))))
    (assert (not (funcall f (1+ most-positive-fixnum))))
    (assert (funcall f most-positive-fixnum))))

(with-test (:name (:typep :member-uses-eql))
  (assert (eval '(typep 1/3 '(member 1/3 nil))))
  (assert (eval '(typep 1.0 '(member 1.0 t))))
  (assert (eval '(typep #c(1.1 1.2) '(member #c(1.1 1.2)))))
  (assert (eval '(typep #c(1 1) '(member #c(1 1)))))
  (let ((bignum1 (+ 12 most-positive-fixnum))
        (bignum2 (- (+ 15 most-positive-fixnum) 3)))
    (assert (eval `(typep ,bignum1 '(member ,bignum2))))))

(with-test (:name :opt+rest+key-canonicalization)
  (let ((type '(function (&optional t &rest t &key (:x t) (:y t)) *)))
    (assert (equal type (sb-kernel:type-specifier (sb-kernel:specifier-type type))))))

(with-test (:name :bug-369)
  (let ((types (mapcar #'sb-c::values-specifier-type
                       '((values (vector package) &optional)
                         (values (vector package) &rest t)
                         (values (vector hash-table) &rest t)
                         (values (vector hash-table) &optional)
                         (values t &optional)
                         (values t &rest t)
                         (values nil &optional)
                         (values nil &rest t)
                         (values sequence &optional)
                         (values sequence &rest t)
                         (values list &optional)
                         (values list &rest t)))))
    (dolist (x types)
      (dolist (y types)
        (let ((i (sb-c::values-type-intersection x y)))
          (assert (sb-c::type= i (sb-c::values-type-intersection i x)))
          (assert (sb-c::type= i (sb-c::values-type-intersection i y))))))))

(with-test (:name :bug-485972)
  (assert (equal (multiple-value-list (subtypep 'symbol 'keyword)) '(nil t)))
  (assert (equal (multiple-value-list (subtypep 'keyword 'symbol)) '(t t))))

;; WARNING: this test case would fail by recursing into the stack's guard page.
(with-test (:name :bug-883498)
  (sb-kernel:specifier-type
   `(or (INTEGER -2 -2)
        (AND (SATISFIES FOO) (RATIONAL -3/2 -3/2)))))

;; The infinite recursion mentioned in the previous test was caused by an
;; attempt to get the following right.
(with-test (:name :quirky-integer-rational-union)
  (assert (subtypep `(or (integer * -1)
                         (and (rational * -1/2) (not integer)))
                    `(rational * -1/2)))
  (assert (subtypep `(rational * -1/2)
                    `(or (integer * -1)
                         (and (rational * -1/2) (not integer))))))

;; for the longest time (at least 05525d3a), single-value-type would
;; return CHARACTER on this.
(with-test (:name :single-value-&optional-type)
  (assert (sb-c::type= (sb-c::single-value-type
                        (sb-c::values-specifier-type '(values &optional character)))
                       (sb-c::specifier-type '(or null character)))))

;; lp#1317308 - TYPE-OF must not return a type specifier
;; involving AND,EQL,MEMBER,NOT,OR,SATISFIES,or VALUES.
(with-test (:name :ANSIly-report-hairy-array-type)
  (let ((simp-t (make-array 9))
        (simp-bit (make-array 16 :element-type 'bit)))
    ;; TYPE-OF doesn't have an optimization that returns a constant specifier
    ;; from a non-constant array of known type. If it did, we'd probably
    ;; want to check that these results are all equal:
    ;;  - the runtime-determined type
    ;;  - the compile-time-determined constant type
    ;;  - the compile-time-determined type of an equivalent object
    ;;    that is in fact a compile-time constant
    (flet ((our-type-of (x) (sb-kernel:type-specifier (sb-kernel:ctype-of x))))
      (let ((hairy-t (make-array 3 :displaced-to simp-t)))
        (assert (equal (our-type-of hairy-t)
                       '(and (vector t 3) (not simple-array))))
        (assert (equal (type-of hairy-t) '(vector t 3))))
      (let ((hairy-t (make-array '(3 2) :displaced-to simp-t)))
        (assert (equal (our-type-of hairy-t)
                       '(and (array t (3 2)) (not simple-array))))
        (assert (equal (type-of hairy-t) '(array t (3 2)))))
      (let ((hairy-bit
             (make-array 5 :displaced-to simp-bit :element-type 'bit)))
        (assert (equal (our-type-of hairy-bit)
                       '(and (bit-vector 5) (not simple-array))))
        (assert (equal (type-of hairy-bit) '(bit-vector 5)))))))

(with-test (:name :bug-309098)
  (let ((u `(or ,@(map 'list (lambda (x) `(array ,(sb-vm:saetp-specifier x)))
                       sb-vm:*specialized-array-element-type-properties*))))
    (assert (equal (multiple-value-list (subtypep 'array u)) '(t t)))))

(with-test (:name :bug-1258716)
  (let ((intersection (sb-kernel:type-intersection
                       (sb-kernel:specifier-type 'simple-vector)
                       (sb-kernel:specifier-type `(vector #:unknown)))))
    (assert (sb-kernel:array-type-p intersection))
    ;; and not *wild-type*
    (assert (sb-kernel:type= (sb-kernel:array-type-specialized-element-type intersection)
                             sb-kernel:*universal-type*))))

(with-test (:name :parse-safely)
  (dolist (x '(array integer cons))
    (assert (handler-case (sb-kernel:specifier-type `(,x . 0))
              (sb-kernel::arg-count-error () t)
              (error (c) (print c) nil)))))

(with-test (:name :unparse-safely)
  (let* ((intersection (sb-kernel:type-intersection
                        (sb-kernel:specifier-type '(vector (or bit character)))
                        (sb-kernel:specifier-type `(vector (or bit symbol)))))
         (round-trip (sb-kernel:specifier-type
                     (sb-kernel:type-specifier intersection))))
    (assert (sb-kernel:type= intersection round-trip))
    (assert (sb-kernel:array-type-p intersection))
    ;; and not *wild-type*
    (assert (sb-kernel:type/= (sb-kernel:array-type-specialized-element-type intersection)
                              (sb-kernel:specifier-type 'bit)))))

;; lp#1333731
(with-test (:name :adjust-array-changes-type-of)
  (let ((a (make-array 10 :adjustable t)))
    (assert (equal (type-of a) '(vector t 10)))
    (adjust-array a 20)
    (assert (equal (type-of a) '(vector t 20)))))


(with-test (:name :unknown-type-strongly-uncacheable)
  ;; VALUES-SPECIFIER-TYPE should not cache a specifier any part of which
  ;; is unknown. This leads to consistent results when parsing unknown
  ;; types. Previously it was indeterminate whether a condition would
  ;; be signaled for (OR UNKNOWN KNOWN) depending on whether that expression
  ;; had ever been parsed and whether it had been evicted from the cache.
  (assert-signal (progn (sb-kernel:specifier-type '(or weeble ratio))
                        (sb-kernel:specifier-type '(or weeble ratio)))
                 sb-kernel:parse-unknown-type 2) ; expect 2 signals
  (assert-signal (progn (sb-kernel:specifier-type '(and potrzebie real))
                        (sb-kernel:specifier-type '(and potrzebie real)))
                 sb-kernel:parse-unknown-type 2) ; expect 2 signals
  (assert-signal (progn (sb-kernel:specifier-type '(array strudel))
                        (sb-kernel:specifier-type '(array strudel)))
                 sb-kernel:parse-unknown-type 2) ; expect 2 signals
  (assert-signal (progn (sb-kernel:specifier-type '(not bad))
                        (sb-kernel:specifier-type '(not bad)))
                 sb-kernel:parse-unknown-type 2)) ; expect 2 signals

(in-package "SB-KERNEL")
(test-util:with-test (:name :partition-array-into-simple/hairy)
  ;; Some tests that (simple-array | hairy-array) = array
  ;; At present this works only for wild element-type.
  (multiple-value-bind (eq winp)
      (type= (specifier-type '(not (and array (not simple-array))))
             (specifier-type '(or (not array) simple-array)))
    (assert (and eq winp)))

  ;; if X is neither simple-array nor hairy-array, it is not an array
  (assert (type= (specifier-type '(and (not simple-array)
                                       (not (and array (not simple-array)))))
                 (specifier-type '(not array))))

  ;; (simple-array * (*)) = (AND (NOT <hairy-array>) VECTOR) etc
  (flet ((try (unrestricted simple)
           (assert (type= (specifier-type simple)
                          (type-intersection
                           (specifier-type
                            '(not (and array (not simple-array))))
                           (specifier-type unrestricted))))))
    (try 'vector '(simple-array * (*)))
    (try '(vector t) 'simple-vector)
    (try 'bit-vector 'simple-bit-vector)
    (try 'string 'simple-string)
    #+sb-unicode(try 'character-string 'simple-character-string)
    (try 'base-string 'simple-base-string))

  ;; if X is a known string and not an array-header
  ;; it must be a SIMPLE-STRING
  (assert (type= (type-intersection
                  (specifier-type 'string)
                  (specifier-type
                   '(not (or (and simple-array (not vector))
                             (and array (not simple-array))))))
                 (specifier-type 'simple-string))))

(test-util:with-test (:name :classoids-as-type-specifiers)
  (dolist (classoid (list (find-classoid 'integer)
                          (find-class 'integer)))
    ;; Classoids and classes should work as type specifiers
    ;; in the atom form, not as lists.
    ;; Their legality or lack thereof is equivalent in all cases.
    (flet ((expect-win (type)
             (multiple-value-bind (f warn err)
                 (compile nil `(lambda (x) (declare (,type x)) x))
               (assert (and f (not warn) (not err))))
             (multiple-value-bind (f warn err)
                 (compile nil `(lambda (x) (declare (type ,type x)) x))
               (assert (and f (not warn) (not err))))))
      (expect-win classoid))
    ;; Negative tests come in two flavors:
    ;; In the case of (DECLARE (TYPE ...)), parsing the following thing
    ;; as a type should fail. But when 'TYPE is implied, "canonization"
    ;; should do nothing, because the following form is not a type,
    ;; so we get an error about an unrecognized declaration instead.
    (flet ((expect-lose (type)
             (multiple-value-bind (f warn err)
                 (let ((*error-output* (make-broadcast-stream)))
                   (compile nil `(lambda (x) (declare (,type x)) x)))
               (declare (ignore f warn))
               (assert err))
             (multiple-value-bind (f warn err)
                 (let ((*error-output* (make-broadcast-stream)))
                   (compile nil `(lambda (x) (declare (type ,type x)) x)))
               (declare (ignore f warn))
               (assert err))))
      (expect-lose `(,classoid))
      (expect-lose `(,classoid 1 100)))))

(test-util:with-test (:name :classoid-type-kind)
  (do-all-symbols (s)
    (let ((c (sb-kernel:find-classoid s nil)))
      ;; No classoid can have a :TYPE :KIND that is :DEFINED.
      (when c
        (if (typep c 'sb-kernel:built-in-classoid)
            (assert (eq (sb-int:info :type :kind s) :primitive))
            (assert (eq (sb-int:info :type :kind s) :instance)))))))
