;;;; tests related to setf

;;;; This file is impure because we want to be able to use DEFUN.

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

(load "compiler-test-util.lisp")

(defvar *foo* nil)
(defun (setf foo) (bar)
    (setf *foo* bar))

;;; Regression test for get-setf-expansion without explicit
;;; environment object.
(assert (multiple-value-list (get-setf-expansion '(foo))))

;;; Regression test for SHIFTF of values.
(let ((x (list 1))
      (y (list 2)))
  (shiftf (values (car x) (car y)) (values (car y) (car x)))
  (assert (equal (list x y) '((2) (1)))))

;;; SETF of values with multiple-value place forms
(let ((a t) (b t) (c t) (d t))
  (let ((list (multiple-value-list
               (setf (values (values a b) (values c d)) (values 1 2 3 4)))))
    (assert (equal list '(1 2)))
    (assert (eql a 1))
    (assert (eql c 2))
    (assert (null b))
    (assert (null d))))

;;; SETF of THE with VALUES.
(let (x y)
  (setf (the (values fixnum fixnum) (values x y))
        (values 1 2))
  (assert (= x 1))
  (assert (= y 2)))

;;; SETF of MACRO-FUNCTION must accept a NIL environment
(let ((fun (constantly 'ok)))
  (setf (macro-function 'nothing-at-all nil) fun)
  (assert (eq fun (macro-function 'nothing-at-all nil))))


;;; DEFSETF accepts &ENVIRONMENT but not &AUX
(defsetf test-defsetf-env-1  (&environment env) (new)
  ;; Note: we're not trying to ignore NEW, we're trying to ignore
  ;; the variable that SETF binds whose name is in NEW.
  (if (macro-function 'defsetf-env-trick env)
      `(progn ,new :local)
      `(progn ,new :global)))

(defsetf test-defsetf-env-2  (local global &environment env) (new)
  ;; As above, NEW values are generally not supposed to be ignored.
  (if (macro-function 'defsetf-env-trick env)
      `(progn ,new ,local)
      `(progn ,new ,global)))

;; Returning an atom is not illegal, though is strange.
(defsetf test-defsetf-trick-3 () (new) new 'bork)
(with-test (:name :setf-expander-returns-atom)
  ;; Simply don't crash in SETF and we're good.
  (macroexpand-1 '(setf (test-defsetf-trick-3) 'a)))

(assert (eq :local (macrolet ((defsetf-env-trick ()))
                     (setf (test-defsetf-env-1) 13))))

(assert (eq :global (setf (test-defsetf-env-1) 13)))

(assert (eq :local (macrolet ((defsetf-env-trick ()))
                     (setf (test-defsetf-env-2 :local :oops) 13))))

(assert (eq :global (setf (test-defsetf-env-2 :oops :global) 13)))

(assert (eq :error
            (handler-case
                (eval '(defsetf test-defsetf-aux (&aux aux) (new) nil))
              (error ()
                :error))))

(handler-bind ((style-warning #'error))
  (compile nil '(lambda ()
                 (defsetf test-defsetf-no-env (foo) (new)
                   `(set-foo ,foo ,new))))
  (compile nil '(lambda ()
                 (defsetf test-defsetf-ignore-env (foo &environment env) (new)
                   (declare (ignore env))
                   `(set-foo ,foo ,new)))))

;;; Not required by the spec, but allowes compiler-macros for SETF-functiosn
;;; to see their constant argument forms.
(with-test (:name :constantp-aware-get-setf-expansion)
  (multiple-value-bind (temps values stores set get)
      (get-setf-expansion '(foo 1 2 3))
    (assert (not temps))
    (assert (not values))
    (assert (equal `(funcall #'(setf foo) ,@stores 1 2 3) set))
    (assert (equal '(foo 1 2 3) get))))

(with-test (:name :update-fn-should-be-a-symbol-in-defsetf)
  (assert (eq :error
            (handler-case
                (eval '(defsetf access-fn 5))
              (error ()
                :error)))))

(with-test (:name :getf-unused-default-variable)
  (handler-bind ((style-warning #'error))
    (compile nil `(lambda (x y)
                    (setf (gethash :x x 0) 4)
                    (setf (getf y :y 0) 4)
                    (setf (get 'z :z 0) 4)))))

(with-test (:name :setf-fun-and-macro-full-warn)
  ;; make the compiler assume existence of #'(setf shoe-color)
  (handler-bind ((warning #'muffle-warning))
      (compile nil '(lambda (x) (setf (shoe-color x) 'cordovan))))
  ;; now we get a full warning because the macro was seen too late.
  (assert (typep (handler-case (eval '(defsetf shoe-color set-shoe-color))
                   (warning (c) c))
                 '(and warning (not style-warning)))))

(with-test (:name :setf-fun-and-macro-style-1)
  (eval '(defun (setf shoe-size) (new x) x new))
  ;; unlike above, this is merely a style-warning
  (assert (typep (handler-case (eval '(defsetf shoe-size set-shoe-size))
                   (warning (c) c))
                 'style-warning)))

;; This is a test of the compiler, but it belongs with the above.
;; FIXME: does this need to go through COMPILE-FILE, or will COMPILE work?
(defvar *tmpfile* (scratch-file-name "lisp"))
(with-test (:name :setf-fun-and-macro-style-2)
  (unwind-protect
       (progn
         ;; verify the test's precondition, for sanity
         (assert (not (fboundp '(setf shoe-count))))
         (with-open-file (f *tmpfile* :direction :output
                                      :if-exists :supersede)
           (prin1 '(defun (setf shoe-count) (new x) (print x) new) f)
           (prin1 '(defsetf shoe-count set-shoe-count) f))
         ;; Expect a warning because the compiler knows about
         ;; (SETF SHOE-COUNT), which isn't yet FBOUNDP,
         ;; and then we also compile a SETF inverse.
         (multiple-value-bind (output warnings-p failure-p)
             (compile-file *tmpfile*)
           (ignore-errors (delete-file output))
           (assert (and output warnings-p (not failure-p)))))
      (ignore-errors (delete-file *tmpfile*))))

;; The expander for (CADAR x) should behave as (CAR (CDAR x)) etc.
;; This mainly affects read/modify/write semantics.
(with-test (:name :car+cdr-compositions-lp1450968)
  (flet ((maketree (n &aux (count -1))
           (labels ((recurse (n)
                      (if (zerop n)
                          (incf count)
                          (cons (recurse (1- n)) (recurse (1- n))))))
             (recurse n))))
    (loop
     for n-ops from 2 to 4
     do (dotimes (bitmask (ash 1 n-ops))
          (let* ((ops (coerce (loop for i below n-ops
                                    collect (if (logbitp i bitmask) #\D #\A))
                              'string))
                 (accessor (sb-int:symbolicate "C" ops "R"))
                 (tree (maketree n-ops))
                 (left (car tree))
                 (right (cdr tree)))
            (assert (eql (funcall accessor tree) bitmask))
            (let ((f (compile nil
                              `(lambda (obj)
                                 (incf (,accessor obj)
                                       (progn (rplaca obj nil)
                                              (rplacd obj nil)
                                              1000))))))
              (funcall f tree)
              (let ((tree* (cons left right)))
                (assert (eql (funcall accessor tree*)
                             (+ bitmask 1000))))))))))

(define-symbol-macro %foofy1% (values a b c))
(define-symbol-macro %foofy2% (values x y z))
;; PSETF and PSETQ eliminate vacuous LET* forms.
(with-test (:name :psetf-expansion-maximally-concise)
  (dolist (op '(psetq psetf))
    (let* ((form `(,op %foofy1% (f) %foofy2% (g)))
           (expansion (let ((*gensym-counter* 1)) (macroexpand-1 form)))
           (expect '(multiple-value-bind (new1 new2 new3) (f)
                     (multiple-value-bind (new4 new5 new6) (g)
                       (setq a new1) (setq b new2) (setq c new3)
                       (setq x new4) (setq y new5) (setq z new6)
                       nil))))
      (assert (equal-mod-gensyms expansion expect)))))

(with-test (:name :defsetf-syntax-errors)
  (dolist (test '((defsetf foo set-foo junk other-junk) ; would accept
                  (defsetf foo set-foo . junk))) ; would crash
    (assert (search "Ill-formed DEFSETF"
                    (simple-condition-format-control
                     (nth-value 1 (ignore-errors (macroexpand-1 test)))))))
  ;; no (SETF (SETF f)) methods
  (assert-error (macroexpand-1 '(defsetf (setf foo) set-setf-foo))))

(defmacro mymacro () '*x*)
(define-symbol-macro foox (car *x*))
(with-test (:name :setf-of-symbol-macro)
  (assert (equal (macroexpand-1 '(setf foox 3)) '(sb-kernel:%rplaca *x* 3))))
(with-test (:name :setf-of-macro)
  (assert (equal (macroexpand-1 '(setf (mymacro) 3)) '(setq *x* 3))))

(defvar *x* (list 1))
(defun set-foox (x)
  (declare (type (integer 1 20) foox))
  (setq foox x))
(with-test (:name :setf-of-symbol-macro-typecheck)
  ;; this was not broken, but since I've deleted the comment
  ;;  "FIXME: [Free] type declaration. -- APD, 2002-01-26"
  ;; from ir1-translators, it's probably worth a test
  ;; since at some point it must not have worked as intended.
  (assert-error (set-foox 99)))

(declaim (special *foo-array*))
;; When dealing with symbol-macros, compiled SETQ would locate the leaf
;; for the symbol and then covertly stuff in the expansion to a SETF form.
;; *MACROEXPAND-HOOK* would see the SETF but not the expansion
;; of the symbol, except those expansions occurring with GET-SETF-EXPANSION.
;; Now it can see the first-round expansion too.
;; The macroexpand hook for this test needs to be compiled, but you can't
;; pass a quoted lambda (as a sexpr) to COMPILE because it needs to
;; capture EXPANSIONS, but you can't pass an function-quoted lambda
;; because WITH-TEST creates a too-complex environment for conversion
;; from an interpreted lambda.
(with-test (:name :compiled-setq-macroexpand-hook :skipped-on :interpreter)
  (sb-int:collect ((expansions))
    (let ((*macroexpand-hook*
           (lambda (expander form env)
             (let ((new (funcall expander form env)))
               (when (or (symbolp form) (eq (car form) 'setf))
                 (expansions (list form new)))
               new))))
      (compile nil '(lambda (x)
                     (symbol-macrolet ((ref-it (aref a 0))
                                       (a *foo-array*)
                                       (thing ref-it))
                       (setq thing x)))))
    (assert (equal-mod-gensyms
             (expansions)
             '((thing ref-it)
               (ref-it (aref a 0))
               (a *foo-array*)
               ((setf thing x)
                (let* ((a0 a) (new1 x))
                  (funcall #'(setf aref) new1 a0 0))))))))

(with-test (:name :remf-basic-correctness)
  (flet ((try (indicator input expect)
           (handler-case (sb-impl::%remf indicator (copy-list input))
             (error () (assert (eq expect :error)))
             (:no-error (newval flag)
               (assert (and (equal newval expect)
                            (eq flag (not (equal newval input)))))
               (let* ((foo (vector (copy-list input)))
                      (removedp (remf (aref foo 0) indicator)))
                 (assert (equal (aref foo 0) expect))
                 (assert (eq removedp (not (equal input expect)))))))))
    (try 'x '() '())
    (try 'x 'a :error)
    (try 'x '(a) :error)
    (try 'x '(a . b) :error)
    (try 'x '(a b . c) :error)
    ;; indicator not found
    (try 'weazel '(a b :foo :goo) '(a b :foo :goo))
    (try 'weazel '(a b :foo :goo . 3) :error) ; improper
    (try 'weazel '(a b :foo :goo baz) :error) ; odd length
    ;; pair deleted from head
    (try 'a '(a b :foo :goo a 3) '(:foo :goo a 3))
    (try 'a '(a b :foo :goo) '(:foo :goo))
    (try 'a '(a b :foo) '(:foo)) ; odd length unnoticed
    (try 'a '(a b . :foo) :error) ; but won't return a non-list
    ;; pair deleted from interior
    (try :foo '(a b :foo :goo) '(a b))
    (try :foo '(a b :foo :goo :boo) '(a b :boo)) ; odd length unnoticed
    (try :foo '(a b :foo :goo :foo) '(a b :foo)) ; other :FOO is irrelevant
    (try :foo '(a b :foo :goo . bad) :error)
    ))

(with-test (:name :incf-argument-eval-order)
  (let ((testvar 1))
    (flet ((double-it () (setq testvar (* 2 testvar))))
      (incf testvar (double-it)))
    ;; testvar should be 4, not 3, because the read for INCF
    ;; occurs after doubling.
    (assert (eql testvar 4))))

;; Simple DEFSETF test
(with-test (:name :defsetf-subseq-constant-indices)
  (assert (equal-mod-gensyms
           (macroexpand-1 '(setf (subseq (foo) 4 6) "Hi"))
           '(let* ((subform (foo)) (newval "Hi"))
              (replace subform newval :start1 4 :end1 6)
              newval))))

(with-test (:name :defsetf-gethash)
  (assert (equal-mod-gensyms
           (macroexpand-1 '(push 1 (gethash :k tbl '(none))))
           ;; the only temp var should be for TBL
           '(let* ((#1=#:hashtable tbl))
             (sb-kernel:%puthash :k #1# (cons 1 (gethash :k #1# '(none))))))))

;; Setup for CLHS hairy example (not working)
(defvar *xy* (make-array '(10 10)))
(defun xy (&key ((x x) 0) ((y y) 0)) (aref *xy* x y))
(defun set-xy (new-value &key ((x x) 0) ((y y) 0))
  (setf (aref *xy* x y) new-value))
(defsetf xy (&key ((x x) 0) ((y y) 0)) (store)
   `(set-xy ,store 'x ,x 'y ,y))
;; FIXME: add tests

(with-test (:name :setf-of-apply-aref)
  (let ((n 0)
        (array (make-array '(3 2 7)))
        (indices (list 0 0 0)))
    (flet ((bump-index ()
             (let ((i (1- (length indices))))
               (loop (cond ((< (nth i indices) (1- (array-dimension array i)))
                            (return (incf (nth i indices))))
                           ((= i 0) (return nil))
                           (t (setf (nth i indices) 0) (decf i)))))))
      (loop (setf (apply #'aref array indices) (incf n))
            (unless (bump-index) (return)))
      (assert (equalp
               #3A(((1 2 3 4 5 6 7) (8 9 10 11 12 13 14))
                   ((15 16 17 18 19 20 21) (22 23 24 25 26 27 28))
                   ((29 30 31 32 33 34 35) (36 37 38 39 40 41 42)))
               array)))))

(define-modify-macro append2+ (a b &optional c &rest more) append
                     "append at least two more lists onto the place")
(define-modify-macro other-incf (&optional (delta 1)) +)

(with-test (:name :define-modify-macro-arg-eval-order)
  ;; Uses a bunch of temps
  (assert (equal-mod-gensyms
           (macroexpand-1 '(append2+ (car x) (f) (g) (h) (i) (j)))
           '(let* ((x1 x) (a (f)) (b (g)) (c (h)) (g3 (i)) (g4 (j)))
             (sb-kernel:%rplaca x1 (append (car x1) a b c g3 g4)))))

  ;; Calling OTHER-INCF with the default delta of 1 uses no temps.
  (assert (equal (macroexpand '(other-incf *foo-base*))
                 '(setq *foo-base* (+ *foo-base* 1))))
  ;; Otherwise, it uses a temp because it "doesn't know" that + commutes.
  (assert (equal-mod-gensyms (macroexpand '(other-incf b (ff)))
                             '(let* ((delta (ff))) (setq b (+ b delta)))))
  ;; And the following result should be identical to that of ordinary INCF.
  (let ((testvar 1))
    (flet ((double-it () (setq testvar (* 2 testvar))))
      (other-incf testvar (double-it)))
    (assert (eql testvar 4))))

(with-test (:name :incf-avoid-temp-vars)
  (assert (equal (macrolet ((x () 'y)
                            (try (&environment env)
                              (list 'quote (macroexpand-1 '(incf (x)) env))))
                   (try))
                 '(setq y (+ 1 y)))))

(with-test (:name :push-getf-avoid-temp-vars)
  ;; Not only should subforms of PLACE avoid binding temp vars for constants,
  ;; so should the arguments to GETF and %PUTF.
  ;; This reads (AREF A) twice but I think that's unavoidable
  (assert (equal-mod-gensyms
           (macroexpand-1 '(push 'foo (getf (aref a (x) 1) :my-indicator '(t))))
           '(let* ((a642 a)
                   (g643 (x))
                   (new645
                    (cons 'foo (getf (aref a642 g643 1) :my-indicator '(t)))))
             (let ((new644
                    (sb-impl::%putf (aref a642 g643 1) :my-indicator new645)))
               (funcall #'(setf aref) new644 a642 g643 1)
               new645)))))

(defparameter *foobar-list* (list 1 2 3))
(defun my-foobar-list () *foobar-list*)
(defun (setf my-foobar-list) (newval)
  (incf (car *foobar-list*))
  (setq *foobar-list* newval))
(with-test (:name :pop-eval-order-bug-1454021)
  ;; Assert that POP reads CAR of the list before invoking the setter
  (assert (eq (pop (my-foobar-list)) 1)))

;; lp#1460360
(with-test (:name :pushnew-evals-keyword-args)
  ;; Though not directly supported by an example in CLHS,
  ;; convention seems to dictate that :KEY, :TEST, :TEST-NOT are not
  ;; parsed by DS-BIND, but instead are blindly forwarded to ADJOIN.
  (let ((k :test) (v #'equal) (list nil))
    (pushnew '(hi) list k v)
    (assert (equal list '((hi))))))

(with-test (:name :setf-ldb-syntax)
  ;; this gets both a warning and an error.
  (assert-error (let ((x 0)) (setf (ldb (byte 4 2 3) x) 1))))

(with-test (:name :setf-ldb-recognize-local-macros)
  ;; This lambda should call neither %LDB nor %DPB
  (assert (not (ctu:find-named-callees
                (compile nil
                         '(lambda (x)
                           (declare (type (cons) x))
                           (symbol-macrolet ((b (byte 4 3)))
                             (incf (ldb b (truly-the fixnum (car x)))))))))))

;; There's aren't a lot of reasonable uses of the setf "getter" for LOGBITP.
;; It might come in handy for SHIFTF or ROTATEF, or this:
(define-modify-macro negatef () not)
(with-test (:name :modify-macro-logbitp)
  (dotimes (i 11)
    (let ((foo (list 0)))
      ;; To be extra tricky, flip the Ith bit in a 9-bit subfield
      ;; starting at 2. This should have no effect for I >= 9.
      (negatef (logbitp i (ldb (byte 9 2) (car foo))))
      (if (< i 9)
          (assert (= (car foo) (ash 1 (+ i 2))))
          (assert (= (car foo) 0))))))

;; a DEFSETF lambda list is not a macro lambda-list
(with-test (:name :defsetf-lambda-list-strictness)
  (assert-error
   ;; All implementations agree that this is malformed.
   (macroexpand-1 '(defsetf baz (a . other-stuff) (v) ''who-cares))))


(with-test (:name :shiftf-let*)
  (define-setf-expander shiftf-let* ()
    (let ((a (gensym "A"))
          (b (gensym "B"))
          (store (gensym "STORE")))
      (values
       (list a b)
       `(10 (1+ ,a))
       (list store)
       (list 'list a b store)
       b)))
  (assert (eql (funcall (compile nil `(lambda () (shiftf (shiftf-let*) 21)))) 11)))

(locally
    (declare (optimize (debug 0)))
  (defsetf ffff (x) (y) `(list ,x ,y)))

(with-test (:name :unknown-lambda-var-names)
  (checked-compile '(lambda (x) (setf (ffff x) nil))))
