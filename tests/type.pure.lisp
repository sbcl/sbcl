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

(enable-test-parallelism)

(assert (not (sb-kernel:member-type-p (sb-kernel:make-eql-type #\z))))
(assert (not (sb-kernel:member-type-p (sb-kernel:make-eql-type 1.0))))
(assert (sb-kernel:member-type-p (sb-kernel:make-eql-type -0.0s0)))

(with-test (:name (typexpand-1 typexpand typexpand-all :check-lexenv))
  (flet ((try (f) (assert-error (funcall f 'hash-table 3))))
    (mapc #'try '(typexpand-1 typexpand typexpand-all))))

(with-test (:name :no-*-as-t) ; lp#1860919
  (assert-signal (sb-kernel:specifier-type '(function (*) t)) warning)
  (dolist (f '((lambda (x) (the * x))
               (lambda (x) (declare (* x)) x)
               (lambda (x) (declare (type * x)) x)))
    (multiple-value-bind (f warn err)
        (let ((*error-output* (make-broadcast-stream))) (compile nil f))
      (declare (ignore f))
      (assert (and warn err)))))

(with-test (:name (typep sb-kernel:ctypep))
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
              (12  (and (or number vector) real) t)))))


;;; This test is motivated by bug #195, which previously had (THE REAL
;;; #(1 2 3)) give an error which prints as "This is not a (OR
;;; SINGLE-FLOAT DOUBLE-FLOAT RATIONAL)".  We ideally want all of the
;;; defined-by-ANSI types to unparse as themselves or at least
;;; something similar (e.g. CHARACTER can unparse to BASE-CHAR, since
;;; the types are equivalent in current SBCL, and EXTENDED-CHAR can
;;; unparse to NIL, since there are no EXTENDED-CHARs currently).
(with-test (:name :standard-types)
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
      #+nil (format t "~&~S~%" type)
      (assert (not (sb-kernel:unknown-type-p (sb-kernel:specifier-type type))))
      (assert (atom (sb-kernel:type-specifier (sb-kernel:specifier-type type)))))))

;;; a bug underlying the reported bug #221: The SB-KERNEL type code
;;; signalled an error on this expression.
(with-test (:name (subtypep function values :bug-221))
  (subtypep '(function (fixnum) (values package boolean))
            '(function (t) (values package boolean))))

;;; bug reported by Valtteri Vuorik
(with-test (:name (subtypep function &rest))
  (checked-compile '(lambda () (member (char "foo" 0) '(#\. #\/) :test #'char=)))
  (assert-tri-eq t   t (subtypep '(function ()) '(function (&rest t))))
  (assert-tri-eq nil t (subtypep '(function (&rest t)) '(function ())))
  (assert-tri-eq t   t (subtypep '(function)
                                 '(function (&optional t &rest t))))
  (assert-tri-eq nil t (subtypep '(function) '(function (t &rest t))))
  (assert-tri-eq t   t (subtypep 'function '(function)))
  (assert-tri-eq t   t (subtypep '(function) 'function)))

;;; Absent any exciting generalizations of |R, the type RATIONAL is
;;; partitioned by RATIO and INTEGER.  Ensure that the type system
;;; knows about this.  [ the type system is permitted to return NIL,
;;; NIL for these, so if future maintenance breaks these tests that
;;; way, that's fine.  What the SUBTYPEP calls are _not_ allowed to
;;; return is NIL, T, because that's completely wrong. ]
(with-test (:name (subtypep integer ratio rational))
  (assert-tri-eq t t (subtypep '(or integer ratio) 'rational))
  (assert-tri-eq t t (subtypep 'rational '(or integer ratio))))

;;; Likewise, these are allowed to return NIL, NIL, but shouldn't
;;; return NIL, T:
(with-test (:name (subtypep or and not))
  (assert-tri-eq t t (subtypep t '(or real (not real))))
  (assert-tri-eq t t (subtypep t '(or keyword (not keyword))))
  (assert-tri-eq t t (subtypep '(and cons (not (cons symbol integer)))
                               '(or (cons (not symbol) *) (cons * (not integer)))))
  (assert-tri-eq t t (subtypep '(or (cons (not symbol) *) (cons * (not integer)))
                               '(and cons (not (cons symbol integer)))))
  (assert-tri-eq t t (subtypep '(or (eql 0) (rational (0) 10))
                               '(rational 0 10)))
  (assert-tri-eq t t (subtypep '(rational 0 10)
                               '(or (eql 0) (rational (0) 10)))))

;;; Until sbcl-0.7.13.7, union of CONS types when the CDRs were the
;;; same type gave exceedingly wrong results
(with-test (:name (subtypep cons :same-cdr))
  (let ((a '(or (cons fixnum single-float) (cons bignum single-float)))
        (b '(cons single-float single-float))
        (c '(cons integer single-float)))
    (assert-tri-eq nil t (subtypep a b))
    (assert-tri-eq t   t (subtypep c a))))

(with-test (:name (subtypep :unknown-type))
  (checked-compile-and-assert (:allow-style-warnings t)
      '(lambda ()
         (subtypep '(and null some-unknown-type) 'another-unknown-type))
    (() (values nil nil) :allow-conditions 'sb-kernel:parse-unknown-type)))

;;; bug 46c
(with-test (:name (coerce function :on :macro))
  (dolist (fun '(and if))
    (assert-error (coerce fun 'function))))

(with-test (:name (typep array class-of))
  (dotimes (i 100)
    (let ((x (make-array 0 :element-type `(unsigned-byte ,(1+ i)))))
      (eval `(typep ,x (class-of ,x))))))

(with-test (:name (typep complex member))
  (assert (not (typep #c(1 2) '(member #c(2 1)))))
  (assert (typep #c(1 2) '(member #c(1 2)))))

(with-test (:name (subtypep complex))
  (assert-tri-eq t   t (subtypep 'nil '(complex nil)))
  (assert-tri-eq t   t (subtypep '(complex nil) 'nil))
  (assert-tri-eq t   t (subtypep 'nil '(complex (eql 0))))
  (assert-tri-eq t   t (subtypep '(complex (eql 0)) 'nil))
  (assert-tri-eq t   t (subtypep 'nil '(complex (integer 0 0))))
  (assert-tri-eq t   t (subtypep '(complex (integer 0 0)) 'nil))
  (assert-tri-eq t   t (subtypep 'nil '(complex (rational 0 0))))
  (assert-tri-eq t   t (subtypep '(complex (rational 0 0)) 'nil))
  (assert-tri-eq t   t (subtypep 'complex '(complex real)))
  (assert-tri-eq t   t (subtypep '(complex real) 'complex))
  (assert-tri-eq t   t (subtypep '(complex (eql 1)) '(complex (member 1 2))))
  (assert-tri-eq t   t (subtypep '(complex ratio) '(complex rational)))
  (assert-tri-eq t   t (subtypep '(complex ratio) 'complex))
  (assert-tri-eq nil t (subtypep '(complex (integer 1 2))
                                 '(member #c(1 1) #c(1 2) #c(2 1) #c(2 2)))))

(with-test (:name (typep real))
  (assert (typep 0 `(real ,(ash -1 10000) ,(ash 1 10000)))))

(with-test (:name (subtypep real))
  (assert-tri-eq t t (subtypep `(real ,(ash -1 1000) ,(ash 1 1000))
                               `(real ,(ash -1 10000) ,(ash 1 10000))))
  (assert-tri-eq t t (subtypep `(real (,(ash -1 1000)) (,(ash 1 1000)))
                               `(real ,(ash -1 1000) ,(ash 1 1000)))))

;;; Bug, found by Paul F. Dietz
(with-test (:name (typep subtypep complex rational))
  (let* ((x (eval #c(-1 1/2)))
         (type (type-of x)))
    (assert (subtypep type '(complex rational)))
    (assert (typep x type))))

;;; Test derivation of LOG{AND,IOR,XOR} bounds for unsigned arguments.
;;;
;;; Fear the Loop of Doom!
;;;
;;; (In fact, this is such a fearsome loop that executing it with the
;;; evaluator would take ages... Disable it under those circumstances.)
#+#.(cl:if (cl:eq sb-ext:*evaluator-mode* :compile) '(and) '(or))
(with-test (:name (:type-derivation :logical-operations :correctness) :slow t)
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
          #+(or) (format t "testing type derivation: ~A~%" deriver)
          (loop for a from 0 below size do
                (loop for b from a below size do
                      (loop for c from 0 below size do
                            (loop for d from c below size do
                                  (test a b c d op deriver))))))))))

(with-test (:name (:type-derivation :logical-operations :scaling)
            :broken-on :mark-region-gc
            :slow t)
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
        (when (> scale 40)
          (error "Bad scaling of ~a: input 10 times but runtime ~a times as large."
                 deriver scale))))))

;;; SUBTYPEP on CONS types wasn't taking account of the fact that a
;;; CONS type could be the empty type (but no other non-CONS type) in
;;; disguise
(with-test (:name (subtypep cons :empty))
  (multiple-value-bind (yes win)
      (subtypep '(and function stream) 'nil)
    (multiple-value-bind (cyes cwin)
        (subtypep '(cons (and function stream) t)
                  '(cons nil t))
      (assert (eq yes cyes))
      (assert (eq win cwin)))))

(with-test (:name :intelligent-satisfies)
  (assert (sb-kernel:type= (sb-kernel:specifier-type '(satisfies realp))
                           (sb-kernel:specifier-type 'real)))
  ;; Part of an example in https://bugs.launchpad.net/sbcl/+bug/309455
  (multiple-value-bind (answer certain)
      (subtypep 'complex '(and number (satisfies realp)))
    (assert (not answer))
    (assert certain)))

;;; CONS type SUBTYPEP could be too enthusiastic about thinking it was
;;; certain
(with-test (:name (subtypep cons satisfies))
  (assert-tri-eq nil nil (subtypep '(satisfies foo) '(satisfies bar)))
  (assert-tri-eq nil nil (subtypep '(cons (satisfies foo) t)
                                   '(cons (satisfies bar) t))))

(with-test (:name (subtypep generic-function function))
  (assert-tri-eq t t (subtypep 'generic-function 'function)))

;;; this would be in some internal test suite like type.before-xc.lisp
;;; except that generic functions don't exist at that stage.
(with-test (:name (subtypep generic-function sb-kernel:funcallable-instance))
  (assert-tri-eq t t (subtypep 'generic-function
                               'sb-kernel:funcallable-instance)))

;;; all sorts of answers are right for this one, but it used to
;;; trigger an AVER instead.
(with-test (:name (subtypep function satisfies :smoke))
  (subtypep '(function ()) '(and (function ()) (satisfies identity))))

(with-test (:name (sb-kernel:specifier-type :unknown-type))
  (assert (sb-kernel:unknown-type-p (sb-kernel:specifier-type 'an-unkown-type))))

(with-test (:name (sb-kernel:type= array))
  (assert-tri-eq t   t (ctype= '(or (simple-array an-unkown-type (*))
                                    (simple-array an-unkown-type))
                               '(or (simple-array an-unkown-type (*))
                                    (simple-array an-unkown-type))))
  (assert-tri-eq t   t (ctype= '(simple-array an-unkown-type (*))
                               '(simple-array an-unkown-type (*))))
  (assert-tri-eq nil t (ctype= '(simple-array an-unkown-type (*))
                               '(array an-unkown-type (*))))
  (assert-tri-eq nil t (ctype= '(simple-array an-unkown-type (7))
                               '(simple-array an-unkown-type (8)))))

(with-test (:name (sb-kernel:type= cons))
  (assert-tri-eq nil t (ctype= 'cons '(cons single-float single-float)))
  (assert-tri-eq nil t (ctype= '(cons integer) '(cons))))

(with-test (:name (typep subtypep sb-kernel:instance))
  (assert (typep #p"" 'sb-kernel:instance))
  (assert-tri-eq t t (subtypep '(member #p"") 'sb-kernel:instance)))

(with-test (:name (sb-kernel:type= :simd-pack))
  (dolist (x '(single-float double-float))
    (let ((spec `(simd-pack ,x)))
      (assert (equal (multiple-value-list (ctype= spec spec)) '(t t))))))

(with-test (:name (typep :character-set :negation))
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

(with-test (:name (check-type :store-value :complex-place))
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
(with-test (:name (typep :fixnum-if-unsigned-byte))
  (checked-compile-and-assert ()
      '(lambda (x)
        (declare (type (unsigned-byte #.sb-vm:n-word-bits) x))
        (typep x (quote fixnum)))
    (((1+ most-positive-fixnum)) nil)
    ((most-positive-fixnum)      t)))

(with-test (:name (typep :member :uses eql))
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

(with-test (:name (subtypep keyword symbol :bug-485972))
  (assert-tri-eq nil t (subtypep 'symbol 'keyword))
  (assert-tri-eq t   t (subtypep 'keyword 'symbol)))

;; WARNING: this test case would fail by recursing into the stack's guard page.
(with-test (:name (sb-kernel:specifier-type or and satisfies :bug-883498))
  (sb-kernel:specifier-type
   '(or (integer -2 -2)
        (and (satisfies foo) (rational -3/2 -3/2)))))

;; The infinite recursion mentioned in the previous test was caused by an
;; attempt to get the following right.
(with-test (:name :quirky-integer-rational-union)
  (assert-tri-eq t t (subtypep '(or (integer * -1)
                                    (and (rational * -1/2) (not integer)))
                               '(rational * -1/2)))
  (assert-tri-eq t t (subtypep '(rational * -1/2)
                               '(or (integer * -1)
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
                       '(vector t)))
        (assert (equal (type-of hairy-t) '(vector t 3))))
      (let ((hairy-t (make-array '(3 2) :displaced-to simp-t)))
        (assert (equal (our-type-of hairy-t)
                       '(array t (* *))))
        (assert (equal (type-of hairy-t) '(array t (3 2)))))
      (let ((hairy-bit
              (make-array 5 :displaced-to simp-bit :element-type 'bit)))
        (assert (equal (our-type-of hairy-bit)
                       'bit-vector))
        (assert (equal (type-of hairy-bit) '(bit-vector 5)))))))

(with-test (:name (subtypep array :bug-309098))
  (let ((u `(or ,@(map 'list (lambda (x) `(array ,(sb-vm:saetp-specifier x)))
                       sb-vm:*specialized-array-element-type-properties*))))
    (assert-tri-eq t t (subtypep 'array u))))

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
(with-test (:name (adjust-array :changes type-of))
  ;; I think adjusting an array to enlarge it must read all the old data,
  ;; which would be undefined behavior if you hadn't initialized the array.
  (let ((a (make-array 10 :adjustable t :initial-element 0)))
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

(with-test (:name (typep :complex-integer))
  (assert (not (eval '(typep #c(0 1/2) '(complex integer))))))

(with-test (:name :typep-satisfies-boolean)
  (assert (eq (eval '(typep 1 '(satisfies eval))) t)))

(import '(sb-kernel:specifier-type
          sb-kernel:type-specifier
          sb-kernel:type-intersection
          #+sb-unicode sb-kernel::character-string
          sb-kernel:simple-character-string
          sb-kernel:type=
          sb-kernel:find-classoid
          sb-kernel:make-numeric-type
          sb-kernel::numeric-types-adjacent
          sb-kernel::numeric-types-intersect
          sb-kernel:*empty-type*))

(with-test (:name :partition-array-into-simple/hairy)
  ;; Some tests that (simple-array | hairy-array) = array
  ;; At present this works only for wild element-type.
  (assert-tri-eq
   t t (type= (specifier-type '(not (and array (not simple-array))))
              (specifier-type '(or (not array) simple-array))))

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

(with-test (:name :values-*-illegal)
  (dolist (x '((values *)
               (values *)
               (values * bit)
               (values bit *)
               (values bit &optional *)
               (values bit &rest *)
               (values bit &rest *)))
    (assert-signal (sb-kernel:values-specifier-type x) warning)))

(with-test (:name :classoids-as-type-specifiers)
  (dolist (classoid (list (find-classoid 'integer)
                          (find-class 'integer)))
    ;; Classoids and classes should work as type specifiers
    ;; in the atom form, not as lists.
    ;; Their legality or lack thereof is equivalent in all cases.
    (checked-compile `(lambda (x) (declare (,classoid x)) x))
    (checked-compile `(lambda (x) (declare (type ,classoid x)) x))
    ;; Negative tests come in two flavors:
    ;; In the case of (DECLARE (TYPE ...)), parsing the following thing
    ;; as a type should fail. But when 'TYPE is implied, "canonization"
    ;; should do nothing, because the following form is not a type,
    ;; so we get an error about an unrecognized declaration instead.
    (flet ((expect-lose (type)
             (assert (nth-value 1 (checked-compile
                                   `(lambda (x) (declare (,type x)) x)
                                   :allow-warnings t)))
             (assert (nth-value 1 (checked-compile
                                   `(lambda (x) (declare (,type x)) x)
                                   :allow-warnings t)))))
      (expect-lose `(,classoid))
      (expect-lose `(,classoid 1 100)))))

(with-test (:name :classoid-type-kind)
  (do-all-symbols (s)
    (let ((c (sb-kernel:find-classoid s nil)))
      ;; No classoid can have a :TYPE :KIND that is :DEFINED.
      (when c
        (if (typep c 'sb-kernel:built-in-classoid)
            (assert (eq (sb-int:info :type :kind s) :primitive))
            (assert (eq (sb-int:info :type :kind s) :instance)))))))

(with-test (:name (make-numeric-type :smoke))
  (assert (eq (make-numeric-type :class 'integer :low '(4) :high '(5))
              *empty-type*)))

(with-test (:name (make-numeric-type :union))
  (assert (equal (type-specifier (make-numeric-type :low '(-79106810381456307)))
                 `(or (rational (-79106810381456307))
                      (single-float (-7.910681e16))
                      (double-float (-7.91068103814563d16))))))

(with-test (:name (make-numeric-type :infinities))
  ;; Without class
  (assert (equal (type-specifier
                  (make-numeric-type :low sb-ext:single-float-negative-infinity
                                     :high sb-ext:single-float-negative-infinity))
                 `(or (single-float ,sb-ext:single-float-negative-infinity
                                    ,sb-ext:single-float-negative-infinity)
                      (double-float ,sb-ext:double-float-negative-infinity
                                    ,sb-ext:double-float-negative-infinity))))
  (assert (equal (type-specifier
                  (make-numeric-type :low sb-ext:single-float-negative-infinity))
                 'real))
  ;; With FLOAT class
  (assert (equal (type-specifier
                  (make-numeric-type :class 'float
                                     :low sb-ext:single-float-negative-infinity
                                     :high sb-ext:single-float-negative-infinity))
                 `(or (single-float ,sb-ext:single-float-negative-infinity
                                    ,sb-ext:single-float-negative-infinity)
                      (double-float ,sb-ext:double-float-negative-infinity
                                    ,sb-ext:double-float-negative-infinity))))
  (assert (equal (type-specifier
                  (make-numeric-type :class 'float
                                     :low sb-ext:single-float-negative-infinity))
                 `float)))

(with-test (:name :prettier-union-types :skipped-on (not :sb-unicode))
  ;; (OR STRING BIGNUM) used to unparse as
  ;; (OR (VECTOR CHARACTER) BASE-STRING (INTEGER * -4611686018427387905)
  ;;     (INTEGER 4611686018427387904)) etc
  (dolist (other '(float real bignum))
    (let* ((spec `(or string ,other))
           (parse (sb-kernel:specifier-type spec))
           (unparse (sb-kernel:type-specifier parse)))
      (assert (or (equal unparse `(or string ,other))
                  (equal unparse `(or ,other string)))))))

(with-test (:name :unparse-string)
  (assert (equal (type-specifier (specifier-type '(string 10)))
                 '(#+sb-unicode string #-sb-unicode base-string 10)))
  (assert (equal (type-specifier (specifier-type '(simple-string 10)))
                 '(#+sb-unicode simple-string #-sb-unicode simple-base-string 10))))

(with-test (:name :numeric-types-adjacent)
  (dolist (x '(-0s0 0s0))
    (dolist (y '(-0s0 0s0))
      (let ((a (specifier-type `(single-float -10s0 ,x)))
            (b (specifier-type `(single-float ,y 20s0))))
        (assert (numeric-types-intersect a b)))
      (let ((a (specifier-type `(single-float -10s0 (,x))))
            (b (specifier-type `(single-float ,y 20s0))))
        (assert (not (numeric-types-intersect a b)))
        (assert (numeric-types-adjacent a b)))
      (let ((a (specifier-type `(single-float -10s0 ,x)))
            (b (specifier-type `(single-float (,y) 20s0))))
        (assert (not (numeric-types-intersect a b)))
        (assert (numeric-types-adjacent a b))))))

(with-test (:name :ctypep-function)
  (assert (not (sb-kernel:ctypep #'+ (eval '(sb-kernel:specifier-type '(function (list))))))))

(with-test (:name :cons-union-loop)
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (typep x '(or (cons (or fixnum vector (member a "b")))
                 (cons (or (and (not vector) array) (and (not integer) number)) number))))
   ((10) nil)
   (((cons 1 2)) t)))

(with-test (:name :pathnamep-flag-bit)
  (let ((f (compile nil '(lambda (x) (pathnamep x)))))
    (assert (not (ctu:find-code-constants f)))))

(with-test (:name :structure-is-a)
  (dolist (what '(sb-int:sset-element sb-c::leaf sb-c::functional
                  sb-c::optional-dispatch))
    (assert (eval `(sb-c::%structure-is-a ,(sb-kernel:find-layout 'sb-c::optional-dispatch)
                                          ,(sb-kernel:find-layout what))))))

(with-test (:name :type-of-empty-instance)
  (assert (eq (type-of (test-util::make-funcallable-instance 6))
              'sb-kernel:funcallable-instance))
  (assert (eq (type-of (eval '(sb-kernel:%make-instance 12)))
              'sb-kernel:instance)))

(with-test (:name (:cons-union :lp1912863))
  (let ((c (cons 2 4)))
    (assert (not (typep c '(or (cons (integer 0 8) (integer 5 15))
                            (cons (integer 3 15) (integer 4 14))))))))

(with-test (:name (:rational-union :equivalent-to-t))
  (let ((type '(or (integer * -1) (rational -1/2 1/2) (integer 1) (not integer))))
    (assert-tri-eq t t (subtypep t type))))

(with-test (:name (:rational-union :wider-equivalent-to-t))
  (let ((type '(or (integer * -2) (rational -3/2 3/2) (integer 2) (not integer))))
    (assert-tri-eq t t (subtypep t type))))

(with-test (:name (:rational-union :no-integers-in-rational))
  (let ((type '(or (integer 1 1) (rational 1/2 1/2))))
    (assert-tri-eq t t (subtypep type 'rational))
    (assert-tri-eq nil t (subtypep 'rational type))
    (assert-tri-eq nil t (subtypep type 'integer))
    (assert-tri-eq nil t (subtypep 'integer type))
    (assert (typep 1 type))
    (assert (typep 1/2 type))
    (assert (not (typep 3/4 type)))))

(with-test (:name (:rational-union :open-bounds-closed))
  (let ((t1 '(rational -1 1))
        (t2 '(or (integer -1 1) (rational (-1) (1)))))
    (assert-tri-eq t t (subtypep t1 t2))
    (assert-tri-eq t t (subtypep t2 t1))))

(with-test (:name (:rational-union :lp1912863 :bug039))
  (flet ((bug039 ()
           (let ((t1 'cons)
                 (t2 '(or (not (cons t (real -1 1)))
                       (not (cons sequence (eql 2))))))
             (assert-tri-eq t t (subtypep t1 t2))
             (assert-tri-eq t t (subtypep `(not ,t2) `(not ,t1))))))
    (bug039)))

(with-test (:name (:rational-union :lp1912863 :bug041))
  (flet ((bug041 ()
           (let ((t1 '(not (cons t integer)))
                 (t2 '(not (cons (array nil) (eql 0))))
                 (t3 '(cons simple-array t)))
             (assert-tri-eq t t (subtypep t1 t2))
             (assert-tri-eq t t (subtypep `(not (or ,t2 ,t3)) `(not ,t1)))
             (assert-tri-eq t t (subtypep `(and (not ,t2) (not ,t3)) `(not ,t1))))))
    (bug041)))

(with-test (:name (:lp1916040 :answer))
  (let* ((t1 '(cons sequence short-float))
         (t2 '(or (cons t atom) (cons function t)))
         (answer (multiple-value-list (subtypep t1 t2))))
    (assert (member answer '((nil nil) (t t)) :test 'equal))))

(with-test (:name (:lp1916233))
  (assert-tri-eq t t (subtypep '(cons (or (simple-array ratio) simple-array) nil) nil))
  (assert-tri-eq t t (subtypep '(or (array ratio) sequence) t)))

(defun my-widetag-of (x)
  (sb-sys:sap-ref-8 (sb-sys:int-sap (sb-kernel:get-lisp-obj-address x))
                    (- sb-vm:other-pointer-lowtag)))
;;; I'll bet that nothing anywhere tests this
(with-test (:name :nil-has-symbol-widetag
            :skipped-on (:or :ppc64 :big-endian))
  (assert (= (my-widetag-of nil) (my-widetag-of t))))

(with-test (:name :array-rank-deriver-negation)
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (a) (array-rank (the (not (array t)) a))))))
           `(values (mod 129) &optional))))

(with-test (:name (:rational-intersection :lp1998008))
  (flet ((bug101 ()
           (let ((t1 '(or (not (real 1 3)) (eql 2))))
             (assert-tri-eq t t (subtypep `(not (not ,t1)) t1))
             (assert-tri-eq t t (subtypep t1 `(not (not ,t1)))))))
    (bug101)))

(with-test (:name (:rational-intersection :integer-bounds))
  (let ((t1 '(and (not integer) (rational 3 5)))
        (t2 '(and (not integer) (rational (3) (5)))))
    (assert-tri-eq t t (subtypep t1 t2))
    (assert-tri-eq t t (subtypep t2 t1))
    (assert-tri-eq t t (subtypep `(not ,t1) `(not ,t2)))
    (assert-tri-eq t t (subtypep `(not ,t2) `(not ,t1)))))

(with-test (:name (:cons-union :lp1999352))
  (let* ((v (list :a))
         (type1 `(cons (or atom (eql ,v))))
         (type2 `(cons (or (member :a 2) cons) list)))
    (let ((bug103 (compile nil
                           `(lambda (val)
                              (declare (type ,type1 val))
                              (the ,type2 val)))))
      (assert (equal (funcall bug103 (list v)) '((:a)))))))

(with-test (:name :union-type-checks)
  (assert (not (find 'integerp
                     (ctu:ir1-named-calls `(lambda (x)
                                             (declare ((or list fixnum) x))
                                             (typep x 'integer))
                                          nil)))))

(with-test (:name :union-intersection-simplification)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (typep a '(or
                 (and symbol (not null))
                 (and array (not string)))))
   ((#()) t)
   (("") nil)
   ((t) t)
   ((nil) nil)))

(with-test (:name :union-integer-complex)
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (typep x '(or (integer 36757953510256822604)
                 (complex fixnum))))
   ((-1) nil)
   ((36757953510256822603) nil)
   ((36757953510256822604) t)
   ((36757953510256822605) t)
   ((#C(1d0 1d0)) nil)
   ((#C(1 1)) t)
   ((#C(1 #.(expt 2 300))) nil)))

#+(or arm64 x86-64)
(with-test (:name :structure-typep-fold)
  (assert-type
   (lambda (a b)
     (declare (character a))
       (sb-c::structure-typep a b))
   null)
  (assert-type
   (lambda (a)
     (declare (hash-table a))
     (sb-c::structure-typep a #.(sb-kernel:find-layout 'condition)))
   null)
  (assert-type
   (lambda (a)
     (declare (pathname a))
     (sb-c::structure-typep a #.(sb-kernel:find-layout 'pathname)))
   (eql t)))

(with-test (:name :typep-vector-folding)
  (assert-type
   (lambda (p)
     (declare (integer p))
     (typep p '(vector t 1)))
   null))

(with-test (:name :non-null-symbol-load-widetag)
  (checked-compile-and-assert
   ()
   `(lambda (p)
     (declare ((or symbol array) p))
     (typecase  p
       ((and symbol (not null)) 1)
       (simple-array 2)))
   ((nil) nil)
   ((t) 1)
   ((:a) 1)
   (("") 2)
   (((make-array 10 :adjustable t)) nil)))

(with-test (:name :other-pointer-subtypes)
  (assert-type
   (lambda (j)
     (sb-kernel:%other-pointer-p (the (and sequence (not vector)) j)))
   null))

(with-test (:name :non-simple-arrays)
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (typep x '(and (vector t) (not simple-array))))
   ((#()) nil)
   (((make-array 10 :adjustable t)) t))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (typep x '(and (array t) (not simple-array))))
   ((#()) nil)
   ((#2A()) nil)
   (((make-array '(10 10) :adjustable t)) t))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (typep x '(and (vector t 10) (not simple-array))))
   ((#10(t)) nil)
   (((make-array 10 :adjustable t)) t)
   (((make-array '(2 5) :adjustable t)) nil))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (typep x '(and (array t 2) (not simple-array))))
   ((#2A()) nil)
   (((make-array '(2 2) :adjustable t)) t)
   (((make-array 2 :adjustable t)) nil)))
