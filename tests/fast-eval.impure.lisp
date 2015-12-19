;;;; various tests of the new interpreter

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

#-sb-fasteval
(sb-ext:exit :code 104)

(setf sb-ext:*evaluator-mode* :interpret)

(in-package sb-interpreter)

(test-util:with-test (:name :type-checker-for-function)
  ;; The test for (FUNCTION (HAIR) (MORE-HAIR)) is just FUNCTIONP.
  ;; The test for not that is (NOT FUNCTION).
  (assert (eq (type-checker (specifier-type '(function (cons) cons)))
              #'functionp))
  (assert (type-checker (specifier-type
                         '(not (function (integer) integer))))))

(defvar *invocation-count* 0)

(defmacro what-cell (x)
  (incf *invocation-count*)
  `(car ,x))

(defun foo (x &optional (howmuch 1)) (incf (what-cell x) howmuch))

(test-util:with-test (:name :interpreter-macro-cache-flush)
  (let ((cell (cons 0 0)))
    (foo cell)
    (foo cell)
    (foo cell)
    (assert (= *invocation-count* 1)) ; once only
    (assert (= (car cell) 3))
    (defmacro what-cell (x)
      (incf *invocation-count*)
      `(cdr ,x))
    ;; Even though INCF's definition is unchanged, the expansion of INCF
    ;; is invalidated by the change to the definition of WHAT-CELL.
    (foo cell 10)
    (foo cell 10)
    (foo cell 10)
    (assert (= *invocation-count* 2)) ; once more
    (assert (= (cdr cell) 30))))

(test-util:with-test (:name :interpreter-eval-if-cond-nil-nil)
  (let ((x 0))
    (flet ((foo () (setq x 1)))
      ;; this was accidentally optimizing out the call to FOO
      (if (foo) nil nil)
      (assert (= x 1)))))

(defmacro expect-type-error (form)
  `(handler-case ,form
     (type-error ())
     (:no-error () (error "Should have gotten a TYPE-ERROR"))))

(defmacro expect-bad-key-error (form)
  `(handler-case ,form
    (error (c)
      (assert (search "not in the allowed set"
                      (write-to-string c :escape nil))))
    (:no-error () (error "Expected an error"))))

(defmacro expect-odd-keys-error (form)
  `(handler-case ,form
     (error (c)
       (assert (search "odd number" (simple-condition-format-control c))))
     (:no-error () (error "Expected an error"))))

(test-util:with-test (:name :interpreter-keyword-parsing)
  ;; Keyword parsing
  ;; No error - &allow-other-keys was specified.
  (validate-keywords '(:foo 3 :bar 2 :baz 3) 1 #(:foo))

  (expect-odd-keys-error (validate-keywords '(:bar) 1 #(:foo)))

  ;; :ALLOW-OTHER-KEYS key is always allowed even if its value is nil.
  (validate-keywords '(:a 1 :allow-other-keys nil :b 2)
                     (ash 3 3) #(:a :b :c))

  ;; :X is not allowed
  (expect-bad-key-error
   (validate-keywords '(:a 1 :x nil :b 2) (ash 3 3) #(:a :b :c)))

  ;; As with all keywords, only the first value matters.
  (expect-bad-key-error
   (validate-keywords '(:a 1 :allow-other-keys nil :allow-other-keys t :x 2)
                      (ash 3 3) #(:a :b :c)))

  ;; here it's T
  (validate-keywords '(:a 1 :allow-other-keys t :allow-other-keys nil :x 2)
                     (ash 3 3) #(:a :b :c))
  ;; here we short-cicuit after seeing T but still have to check for ODDP
  (expect-odd-keys-error
   (validate-keywords '(:a 1 :allow-other-keys t :allow-other-keys nil :x)
                      (ash 3 3) #(:a :b :c)))

  (defun foo (x &optional b c &key akey) (list x b c akey))
  ;; The ODDP check occurs only on actual args that remain after
  ;; processing optional arguments.
  ;; No errors should result from these calls.
  (foo 1 2)
  (foo 1 2 3)
  (foo 1 2 3 :akey 'hi))

(test-util:with-test (:name :interpreter-type-checking)

  (expect-type-error (the integer (values 'a 1 2)))

  ;; "Just do it"
  (locally (declare (optimize (safety 0))) (the integer (values 'a 1 2)))

  ;; THE returns multiple values even if not a VALUES type-specifier.
  (let ((l (multiple-value-call #'list
             (the integer (values 1 'foo 'bar)))))
    (assert (= (length l) 3)))

  ;; Too many values in a "strict" THE form are not permitted.
  (expect-type-error (the (values integer &optional) (values 1 2)))

  ;; A trailing type of which NIL is a member (so LIST,SYMBOL,T at least)
  ;; causes (THE VALUES ...) to accept absence of a value. By definition the
  ;; missing values are NIL. While this seems liberal, so far as VALUES
  ;; expressing a shape similar to DESTRUCTURING-BIND, CLHS draws attention
  ;; to it specifically:
  ;;
  ;;  "It is permissible for _form_ to yield a different number of values than
  ;;  are specified by value-type, provided that the values for which types are
  ;;  declared are indeed of those types. Missing values are treated as nil
  ;;  for the purpose of checking their types"
  (dolist (trailing-type '(symbol t))
    (eval `(the (values integer ,trailing-type) 4))
    (eval `(the (values integer ,trailing-type) (values 4 'foo)))
    (eval `(the (values integer ,trailing-type) (values 4 'foo 5))))

  ;; But a strict THE form does not allow this liberty.
  (expect-type-error (the (values integer symbol &optional) 4))

  (defun f () 'hi)
  (defun g (x) (length (string x)))
  (defun h ()
    (let* ((x (f))
           (x (g x)))
      (declare (fixnum x))
      x))

  ;; The first binding of X in h is to a symbol, which is fine.
  ;; The FIXNUM declaration applies to the _second_ binding named X.
  (h)

  ;; The SETQ is not valid because X is restricted to fixnum.
  (expect-type-error
   (let* ((x 3)
          (y (setq x 'fred)))
     (declare (fixnum x))))

  (defun foo1 ()
    (the (values integer symbol &optional string) (bar)))
  (defun foo2 ()
    (the (values integer symbol string) (bar)))
  (defun foo3 ()
    (the (values integer &optional) (bar)))

  (defun bar () (values 1 'hi))

  (foo1) ; ok
  (expect-type-error (foo2)) ; didn't get a string as 3rd value
  (expect-type-error (foo3)) ; got too many values

  (expect-type-error
   (the (values integer symbol string) (bar)))

  (defun no-vals () (values))

  (handler-case (let ((x (the integer (no-vals)))) x)
    (simple-error ())
    (:no-error () (error "Should have gotten an ERROR")))

  (defmacro nice-macro (a b)
    (declare (type (member :first :second) a))
    (case a
      (:first `(car ,b))
      (:second `(cadr ,b))))

  (assert (equal (macroexpand-1 '(nice-macro :first (x)))
                 '(car (x))))
  ;; macro should fail.
  (expect-type-error (macroexpand '(nice-macro :third (x))))

  ;; SETQ of MUMBLE which is a "free" (not bound) typed special variable
  (expect-type-error
   (let* ((foo 3) (baz foo))
      (declare (special foo mumble) (real mumble))
      (setq mumble 'a)))

  (expect-type-error
   (let ((x 3))
      (declare (special x) (integer x))
      ; (print x)
      (let ((x 'a))
        (declare (symbol x))
        ; (print x)
        (locally (declare (special x))
          ; (print x)
          ;; this references the special X, not the lexical X
          (setq x 'foo)))))

  ;; This works due to short-circuiting within TYPEP
  (let ((x 3)) (declare (type (or integer blurf) x)) x)

  ;; This fails because the unknown type is tested first
  (handler-case (let ((x 3)) (declare (type (or blurf integer) x)) x)
    (simple-error ()) ; "unknown type"
    (:no-error () "Expected an ERROR")))

(test-util:with-test (:name :tagbody-if-optimizer)
  (assert
   (string= "ABC"
            (with-output-to-string (*standard-output*)
             (tagbody
              (go :a)
              :b
              (princ :b)
              (if nil (go :b))
              (go :c)
              :a
              (princ :a)
              (go :b)
              :c
              (princ :c))))))

(macrolet ((foo= (x y) `(= (the fixnum ,x) (the fixnum ,y))))
  (declare (optimize speed))
  (declaim (inline foo-compare))
  (defun foo-compare (a b) (foo= a b)))

(test-util:with-test (:name :inline-lexenv-not-too-hairy)
  (assert (sb-c::fun-name-inline-expansion 'foo-compare)))
