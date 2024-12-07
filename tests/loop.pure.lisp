;;;; miscellaneous tests of LOOP-related stuff

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

;;; The bug reported by Alexei Dejneka on sbcl-devel 2001-09-03
;;; is fixed now.
(assert (equal (let ((hash (make-hash-table)))
                 (setf (gethash 'key1 hash) 'val1)
                 (setf (gethash 'key2 hash) 'val2)
                 (sort (loop for key being each hash-key in hash
                             collect key)
                       #'string<))
               '(key1 key2)))

;;; Bug 81, reported by Wolfhard Buss on cmucl-help 2001-02-14, was
;;; fixed by Alexey Dejneka's patch on sbcl-devel 2001-09-30.
(with-test (:name :loop-destructuring-bind)
  ;; FIXME: should this produce a warning? I don't see why.
  ;; caught STYLE-WARNING:
  ;;   This is not a FLOAT:
  ;;    NIL
  (declare (muffle-conditions style-warning))
  (assert (equal '(0.0 1.0 2.0 3.0)
                 (loop with (a . b) of-type float = '(0.0 . 1.0)
                       and (c . d) of-type float = '(2.0 . 3.0)
                       return (list a b c d)))))

;;; a bug reported and fixed by Alexey Dejneka sbcl-devel 2001-10-05:
;;; The type declarations should apply, hence under Python's
;;; declarations-are-assertions rule, the code should signal a type
;;; error. (Except when running interpreted code)
#+#.(cl:if (cl:eq sb-ext:*evaluator-mode* :compile) '(and) '(or))
(with-test (:name :loop-type-decl)
  (declare (muffle-conditions style-warning))
  (assert (typep (nth-value 1
                            (ignore-errors
                              (funcall (lambda ()
                                         (loop with (a . b)
                                               of-type float = '(5 . 5)
                                               return (list a b))))))
                 'type-error)))

;;; bug 103, reported by Arthur Lemmens sbcl-devel 2001-05-05,
;;; fixed by Alexey Dejneka patch sbcl-devel 2001-10-05:
;;; LOOP syntax requires that forms after INITIALLY, FINALLY, and DO
;;; must be compound forms.
(with-test (:name :loop-syntax-err)
  (multiple-value-bind (function warnings-p failure-p)
      (compile nil
               '(lambda ()
                  (loop while t do
                        *print-level*
                        (print t))))
    (declare (ignore function warnings-p))
    (assert failure-p)))

;;; a bug reported by Paul F. Dietz (in his ANSI test suite):
;;; duplicate bindings in LOOP must signal errors of type
;;; PROGRAM-ERROR.
(with-test (:name :loop-duplicate-binding)
  (assert (typep (nth-value 1
                            (ignore-errors
                              (funcall (lambda ()
                                         (loop for (a . a) in '((1 . 2) (3 . 4))
                                               return a)))))
                 'program-error)))

;;; similar to gcl/ansi-test LOOP.1.27, and fixed at the same time:
(assert (equal (loop for x downto 7 by 2 from 13 collect x) '(13 11 9 7)))

;;; some more from gcl/ansi-test:
(let ((table (make-hash-table)))
  (setf (gethash 'foo table) '(bar baz))
  (assert (= (loop for nil being the hash-keys of table count t) 1))
  (assert (equal (loop for nil being the hash-keys of table
                               using (hash-value (v1 . v2))
                       when v1
                         return v2)
                 '(baz))))

(assert (= (loop for nil being the external-symbols of :cl count t) 978))
(assert (= (loop for x being the external-symbols of :cl count x) 977))

(let ((cl:*package* (find-package :cl)))
  (assert (= (loop for x being each external-symbol count t) 978)))

(assert (eq (loop for a = (return t) return nil) t))

(multiple-value-bind (result error)
    (ignore-errors
      (loop for nil being the external-symbols of :nonexistent-package
            count t))
  (assert (null result))
  (assert (typep error 'package-error)))

(assert (equal (loop for i from 1 repeat (the (integer 7 7) 7) collect i)
               '(1 2 3 4 5 6 7)))

(multiple-value-bind (result error)
    (ignore-errors
      (eval '(loop for i from 1 repeat 7 of-type fixnum collect i)))
  (assert (null result))
  (assert (typep error 'program-error)))

(assert (equal
         (ignore-errors (loop for i from 1 repeat 6.5 collect i))
         (ignore-errors (loop for i from 1 repeat (eval '6.5) collect i))))

(assert (eq (block nil
              (loop named foo do (loop-finish) finally (return :good))
              :bad)
            :good))

(assert (= (loop with (a nil) = '(1 2) return a) 1))
(assert (= (loop with (nil a) = '(1 2) return a) 2))
(assert (= (loop with (a . nil) = '(1 2) return a) 1))
(assert (equal (loop with (nil . a) = '(1 2) return a) '(2)))

(with-test (:name :loop-invalid-collector-1)
  (multiple-value-bind (result error)
      (ignore-errors
        (loop for i in '(1 2 3) collect i always (< i 4)))
    (assert (null result))
    (assert (typep error 'program-error))))
(with-test (:name :loop-invalid-collector-2)
  (assert (equal
           (loop for i in '(1 2 3) collect i into foo always (< i 4)
                 finally (return foo))
           '(1 2 3))))
(with-test (:name :loop-invalid-collector-3)
  (assert (equal
           (loop for i in '(1 2 3) collect i into foo always (= i 4)
                 finally (return foo))
           nil)))
(with-test (:name :loop-invalid-collector-4)
  (multiple-value-bind (result error)
      (ignore-errors
        (loop for i in '(1 2 3) always (< i 4) collect i))
    (assert (null result))
    (assert (typep error 'program-error))))
(assert (equal
         (loop for i in '(1 2 3) always (< i 4) collect i into foo
               finally (return foo))
         '(1 2 3)))
(assert (equal
         (loop for i in '(1 2 3) always (= i 4) collect i into foo
               finally (return foo))
         nil))
(with-test (:name :loop-invalid-collector-5)
  (multiple-value-bind (result error)
      (ignore-errors
        (loop for i in '(1 2 3) thereis (= i 3) collect i))
    (assert (null result))
    (assert (typep error 'program-error))))

(with-test (:name :loop-invalid-collector-6)
    (multiple-value-bind (result error)
      (ignore-errors
        (loop with i = 1 for x from 1 to 3 collect x into i))
    (assert (null result))
    (assert (typep error 'program-error))))
(with-test (:name :loop-invalid-collector-7)
  (multiple-value-bind (result error)
      ;; this one has a plausible interpretation in terms of LET*, but
      ;; ANSI seems specifically to disallow it
      (ignore-errors
        (loop with i = 1 with i = (1+ i)
              for x from 1 to 3
              collect (+ x i)))
    (assert (null result))
    (assert (typep error 'program-error))))

(let ((it 'z))
  (assert (equal
           ;; this one just seems weird.  Nevertheless...
           (loop for i in '(a b c d)
                 when i
                   collect it
                   and collect it)
           '(a z b z c z d z))))

(let ((ht (make-hash-table)))
  (setf (gethash 1 ht) 3)
  (setf (gethash 7 ht) 15)
  (assert (= (loop for v fixnum being each hash-key in ht sum v) 8))
  (assert (= (loop for v fixnum being each hash-value in ht sum v) 18))
  #+#.(cl:if (cl:eq sb-ext:*evaluator-mode* :compile) '(and) '(or))
  (assert-error (loop for v float being each hash-value in ht sum v)
                type-error))

;; arithmetic indexes can be NIL or symbols.
(with-test (:name :loop-anonymous-arithmetic-index)
  ;; FIXME: these produce style-warnings. If they're acceptable, they should not warn.
  (declare (muffle-conditions style-warning))
  (assert (equal (loop for nil from 0 to 2 collect nil)
                 '(nil nil nil)))
  (assert (equal (loop for nil to 2 collect nil)
                 '(nil nil nil))))

;; although allowed by the loop syntax definition in 6.2/LOOP,
;; 6.1.2.1.1 says: "The variable var is bound to the value of form1 in
;; the first iteration[...]"; since we can't bind (i j) to anything,
;; we give a program error.
(with-test (:name :statically-observable-destructuring-problem-1)
  (multiple-value-bind (function warnings-p failure-p)
      (compile nil
               `(lambda ()
                  (loop for (i j) from 4 to 6 collect nil)))
    (declare (ignore function warnings-p))
    (assert failure-p)))

;; ...and another for indexes without FROM forms (these are treated
;; differently by the loop code right now
(with-test (:name :statically-observable-destructuring-problem-2)
  (multiple-value-bind (function warnings-p failure-p)
      (compile nil
               `(lambda ()
                  (loop for (i j) to 6 collect nil)))
    (declare (ignore function warnings-p))
    (assert failure-p)))

(assert
 (equal
  (let ((x 2d0))
    (loop for d of-type double-float from 0d0 to 10d0 by x collect d))
  '(0d0 2d0 4d0 6d0 8d0 10d0)))
(assert
 (equal
  (let ((x 2d0))
    (loop for d of-type double-float downfrom 10d0 to 0d0 by x collect d))
  '(10d0 8d0 6d0 4d0 2d0 0d0)))

(let ((fn (handler-case
              (compile nil '(lambda ()
                             (declare (special x y))
                             (loop thereis (pop x) thereis (pop y))))
            (warning (c) (error "Warned: ~S" c)))))
  (let ((x (list nil nil 1))
        (y (list nil 2 nil)))
    (declare (special x y))
    (assert (= (funcall fn) 2))))

;;; Incorrect LIST type declaration, reported and patched by Teemu
;;; Kalvas: end testing is done "as if by atom" so this is supposed
;;; to work.
(assert (equal '(1 2) (loop for (a . b) on '(1 2 . 3)  collect a)))

;;; Detection of duplicate bindings, reported by Bruno Haible for CMUCL.
(multiple-value-bind (_ condition)
    (ignore-errors
      (macroexpand '(LOOP WITH A = 0 FOR A DOWNFROM 10 TO 0 DO (PRINT A))))
  (declare (ignore _))
  (assert (typep condition 'program-error)))

;;; Loop variable with a range excluding 0, reported by Andras Simon.
;;; (Used to signal an error during macroexpansion.)
(with-test (:name :loop-var-range-excludes-zero)
  (assert (not (loop with foo of-type (single-float 1.0 2.0) = 1.5
                     do (progn foo (return))))))

;;; 1.0.26.12 used to signal a bogus type error for this.
(loop with x of-type (simple-vector 1) = (make-array '(1))
      repeat 1
      return x)

(with-test (:name :bug-540186)
  (let ((fun (compile nil `(lambda (x)
                             (loop for i from 0 below (length x)
                                   for vec of-type vector = (aref x i)
                                   collect vec)))))
    (assert (equal '("foo" "bar")
             (funcall fun
                      (vector "foo" "bar"))))))

(with-test (:name :bug-lp613871)
  (multiple-value-bind (function warnings-p failure-p)
      (compile nil '(lambda () (loop with nil = 1 repeat 2 collect t)))
    (assert (null warnings-p))
    (assert (null failure-p))
    (assert (equal '(t t) (funcall function))))
  (multiple-value-bind (function warnings-p failure-p)
      (compile nil '(lambda () (loop with nil repeat 2 collect t)))
    (assert (null warnings-p))
    (assert (null failure-p))
    (assert (equal '(t t) (funcall function)))))

(with-test (:name :bug-654220-regression)
  (assert (= 32640 (loop for i to 255
                         sum i into sum of-type fixnum
                         finally (return sum)))))

(with-test (:name :of-type-character-init)
  ;; The intention here is to if we initialize C to NIL before iteration start
  ;; by looking for tell-tale types such as (OR NULL CHARACTER). ...not the
  ;; most robust test ever, no.
  (let* ((fun (compile nil `(lambda (x)
                              (loop for c of-type character in x
                                    collect (char-code c)))))
         (consts (ctu:find-code-constants fun :type '(or symbol list))))
    (assert (or (null consts) (equal 'character consts)))))

(with-test (:name :type-of-nilled-vars)
  (assert (equal (loop for (a b) float = '(1.0 2.0)
                       return (list a b))
                 '(1.0 2.0)))
  (assert (equal (loop for (a nil b) float = '(1.0 3.0 2.0)
                       return (list a b))
                 '(1.0 2.0))))

(with-test (:name :misplaced-declarations)
  (assert-no-signal
   (compile nil `(lambda ()
                   (loop with (a) = '(1.0)
                         and (nil f)
                         return (list a f))))
   warning))

(with-test (:name :duplicate-bindings)
  (assert-error
   (funcall (compile nil `(lambda ()
                            (loop with (a b) = '(1.0 2.0)
                                  and (c a) = '(3.0 4.0)
                                  return (list a b c))))))
  (assert-error
   (funcall (compile nil `(lambda ()
                            (loop with a = 10
                                  with ((a) b) = '((1.0) 2.0)
                                  return (list a b))))))
  (assert-error
   (funcall (compile nil `(lambda ()
                            (loop with (b) = '(10)
                                  with (a) = '(3)
                                  for b to 10
                                  collect a)))))
  (assert-error
   (funcall (compile nil `(lambda ()
                            (loop with (a) = '(3)
                                  for b to 10
                                  collect a into b))))))

(with-test (:name :multiple-maximize)
  (assert-no-signal
   (compile nil `(lambda ()
                   (loop for x to 10 maximize x minimize x)))
   warning)
  (assert-no-signal
   (compile nil `(lambda ()
                   (loop for x to 10 minimize x minimize x)))
   warning)
  (assert-no-signal
   (compile nil `(lambda ()
                   (loop for x to 10 minimize x into z minimize x into z finally (return z))))
   warning))

(with-test (:name :destructuring-less)
  (assert (equal (loop with (a b) = '() repeat 1 collect (list a b))
                 '((NIL NIL)))))

(with-test (:name :count-with-sum)
  (assert (= (loop repeat 1 count 1 sum #c(1 2))
             #c(2 2)))
  (assert (= (loop repeat 1 sum 1 count 1)
             2)))

(with-test (:name :iterate-over-complex)
  (assert
   (equal
    (loop for c from #c(0 1) repeat 5 collect c)
    '(#C(0 1) #C(1 1) #C(2 1) #C(3 1) #C(4 1)))))

(with-test (:name :side-effecting-start-form)
  (assert (equal (let ((n 0))
                   (loop for x from (incf n) to (+ n 5) collect x))
                 '(1 2 3 4 5 6))))

(with-test (:name :summing-complex)
  (assert (equal (loop for i from 1 to 4
                       sum (complex i (1+ i)) of-type complex)
                 #c(10 14))))

(with-test (:name :negative-repeat)
  (assert (zerop (let ((z 0))
                   (loop repeat 0 do (incf z))
                   z)))
  (assert (zerop (let ((z 0))
                   (loop repeat -1.5 do (incf z))
                   z)))
  (assert (zerop (let ((z 0))
                   (loop repeat -1.5 do (incf z))
                   z)))
  (assert (zerop (let ((z 0))
                   (loop repeat -1000000 do (incf z))
                   z))))

(with-test (:name :of-type-character)
  (assert (null (loop with a t return a)))
  #+sb-unicode
  (assert (typep (loop with a of-type extended-char return a) 'extended-char))
  (assert (typep (loop with a of-type character return a) 'character))
  (assert (typep (loop with a of-type base-char return a) 'base-char))
  (assert (typep (loop with a of-type standard-char return a) 'standard-char)))

(with-test (:name :empty-type)
  (assert-signal
   (compile nil `(lambda ()
                   (loop with a of-type (and fixnum string) return a)))
   warning)
  (assert-signal
   (compile nil `(lambda ()
                   (loop for i to 10 sum i of-type (and fixnum string))))
   warning))

(with-test (:name :loop-repeat-const)
  ;; without explicit constant-folding in LOOP-DO-REPEAT, the type of this loop's
  ;; counter is INTEGER which unfortunately resulted in generic math throughout,
  ;; since a FOR/THEN clause interacts badly with type inference.
  ;; [if there is no FOR/THEN, the compiler understands the code better,
  ;; and is able to infer a lower bound on decrementing from (+ 1 5)]
  (assert-no-signal
   (compile nil '(lambda ()
                   (declare (optimize speed))
                   (loop for baz = 'this then 'that repeat (+ 1 5)
                         do (print baz))))))

(with-test (:name :loop-default-init-type)
  (assert-no-signal (compile nil
                             '(lambda (list)
                               (declare (optimize speed))
                               (loop for a of-type (simple-vector 4) in list
                                     collect (aref a 2))))
                    sb-ext:compiler-note))

(with-test (:name :with-destructuring)
  (declare (muffle-conditions style-warning)) ; why?
  (assert (= (loop with ((a . b)) = '((1 . 2))
                   return (+ a b))
             3))
  (assert (= (loop with (((a) b)) = '(((1) 3))
                   return (+ a b))
             4)))

(with-test (:name :destructuring-m-v-list :skipped-on :interpreter)
  (flet ((f (n-iter)
           (loop for i from 0 below n-iter
                 for (a b) = (multiple-value-list (floor i 5))
                 sum (+ a b))))
    (ctu:assert-no-consing (f 1000))))

(with-test (:name :destructuring-m-v-list-with-nil)
  (assert (equal-mod-gensyms
           (macroexpand-1 '(sb-loop::loop-desetq (x nil z) (multiple-value-list (foo))))
           '(multiple-value-bind (g1 g2 g3) (foo)
             (declare (ignore g2))
             (sb-loop::loop-desetq x g1)
             (sb-loop::loop-desetq z g3)))))

(with-test (:name :collect-list-type)
  (assert
   (equal (third (sb-kernel:%simple-fun-type
                  (checked-compile
                   '(lambda (l)
                     (loop for x in l
                           collect x into m
                           finally (return m))))))
          '(values list &optional)))
  (assert
   (equal (third (sb-kernel:%simple-fun-type
                  (checked-compile
                   '(lambda (l)
                     (loop for x in l
                           collect x)))))
          '(values list &optional))))


(with-test (:name :loop-in-reverse-by-cddr)
  (checked-compile-and-assert
   (:optimize nil)
   `(lambda (l)
      (loop for x in (reverse l) by #'cddr collect x))
   (('(1)) '(1) :test #'equal)
   (('(1 2)) '(2) :test #'equal)
   (('(1 2 3)) '(3 1) :test #'equal)
   (('(1 2 3 4)) '(4 2) :test #'equal)))
