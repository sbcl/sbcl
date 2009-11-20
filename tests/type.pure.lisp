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
(dolist (fun '(and if))
  (assert (raises-error? (coerce fun 'function) type-error)))

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
(let* ((bits 5)
       (size (ash 1 bits)))
  (flet ((brute-force (a b c d op minimize)
           (loop with extreme = (if minimize (ash 1 bits) 0)
                 with collector = (if minimize #'min #'max)
                 for i from a upto b do
                 (loop for j from c upto d do
                       (setf extreme (funcall collector
                                              extreme
                                              (funcall op i j))))
                 finally (return extreme))))
    (dolist (op '(logand logior logxor))
      (dolist (minimize '(t nil))
        (let ((deriver (intern (format nil "~A-DERIVE-UNSIGNED-~:[HIGH~;LOW~]-BOUND"
                                       op minimize)
                               (find-package :sb-c))))
          (format t "testing type derivation: ~A~%" deriver)
          (loop for a from 0 below size do
                (loop for b from a below size do
                      (loop for c from 0 below size do
                            (loop for d from c below size do
                                  (let* ((brute (brute-force a b c d op minimize))
                                         (x-type (sb-c::specifier-type `(integer ,a ,b)))
                                         (y-type (sb-c::specifier-type `(integer ,c ,d)))
                                         (derived (funcall deriver x-type y-type)))
                                    (unless (= brute derived)
                                      (format t "FAIL: ~A [~D,~D] [~D,~D] ~A~%
ACTUAL ~D DERIVED ~D~%"
                                              op a b c d minimize brute derived)
                                      (assert (= brute derived)))))))))))))

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
                    (lambda (x)
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
