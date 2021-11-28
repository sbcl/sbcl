;;;; This file is for compiler tests which have side effects (e.g.
;;;; executing DEFUN) but which don't need any special side-effecting
;;;; environmental stuff (e.g. DECLAIM of particular optimization
;;;; settings). Similar tests which *do* expect special settings may
;;;; be in files compiler-1.impure.lisp, compiler-2.impure.lisp, etc.

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

;; The tests in this file do not work under the legacy interpreter.
;; They mostly do work in the fast interpreter, and are either harmless
;; or actually reasonable things to test.
(when (and (eq sb-ext:*evaluator-mode* :interpret)
           (not (member :sb-fasteval *features*)))
  (sb-ext:exit :code 104))

(load "compiler-test-util.lisp")

;;; Old CMU CL code assumed that the names of "keyword" arguments are
;;; necessarily self-evaluating symbols, but ANSI Common Lisp allows
;;; them to be any symbols, not necessarily keywords, and thus not
;;; necessarily self-evaluating. Make sure that this works.
(defun newfangled-cons (&key ((left-thing x)) ((right-thing y)))
  (cons x y))
(with-test (:name (:lambda-list &key :self-evaluating))
  (assert (equal (cons 1 2) (newfangled-cons 'right-thing 2 'left-thing 1))))

;;; ANSI specifically says that duplicate keys are OK in lambda lists,
;;; with no special exception for macro lambda lists. (As reported by
;;; Pierre Mai on cmucl-imp 2001-03-30, Python didn't think so. The
;;; rest of the thread had some entertainment value, at least for me
;;; (WHN). The unbelievers were besmote and now even CMU CL will
;;; conform to the spec in this regard. Who needs diplomacy when you
;;; have brimstone?:-)
(defmacro ayup-duplicate-keys-are-ok-i-see-the-lite (&key k)
  k)
(with-test (:name (:macro :lambda-list :duplicate &key :arguments))
  (checked-compile-and-assert ()
      '(lambda ()
         (ayup-duplicate-keys-are-ok-i-see-the-lite :k 112))
    (() 112))
  (checked-compile-and-assert ()
      '(lambda ()
         (ayup-duplicate-keys-are-ok-i-see-the-lite :k 'x :k 'y))
    (() 'x)))

;;; Lexically binding a name that is 1) bound to a global symbol macro
;;; 2) at home in a locked package

(defpackage #:package-for-global-symbol-macro (:lock t))
(cl:in-package #:package-for-global-symbol-macro)
(cl:define-symbol-macro global-symbol-macro 1)
(cl:in-package #:cl-user)

(with-test (:name (let define-symbol-macro :locked package))
  (multiple-value-bind (fun failure-p warnings)
      (checked-compile
       '(lambda () (let ((package-for-global-symbol-macro::global-symbol-macro 1))
                     package-for-global-symbol-macro::global-symbol-macro))
       :allow-failure t :allow-warnings t)
    (declare (ignore fun))
    (assert failure-p)
    (assert warnings)))

;;; As reported by Alexey Dejneka (sbcl-devel 2002-01-30), in
;;; sbcl-0.7.1 plus his patch (i.e. essentially sbcl-0.7.1.2), the
;;; compiler barfed on this, blowing up in FIND-IN-ENVIRONMENT looking for
;;; the LAMBDA-VAR named NUM. That was fixed in sbcl-0.7.1.3.
(defun parse-num (index)
  (let (num x)
    (flet ((digs ()
             (setq num index))
           (z ()
             (let ()
               (setq x nil))))
      (when (and (digs) (digs)) x))))

;;; Bug 132: The compiler used to fail to compile INTEGER-valued CATCH
;;; tags. This was fixed by Alexey Dejneka in sbcl-0.7.1.14. (INTEGER
;;; catch tags are still a bad idea because EQ is used to compare
;;; tags, and EQ comparison on INTEGERs is unportable; but now it's a
;;; compiler warning instead of a failure to compile.)
(with-test (:name (compile catch :integer-valued-tag :bug-132))
  (multiple-value-bind (fun failure-p warnings style-warnings)
      (checked-compile '(lambda () (catch 0 (print 1331 (make-broadcast-stream))))
                       :allow-style-warnings t)
    (declare (ignore failure-p warnings))
    (funcall fun)
    (assert style-warnings)))

;;; Bug 150: In sbcl-0.7.1.15, compiling this code caused a failure in
;;; SB-C::ADD-TEST-CONSTRAINTS:
;;;    The value NIL is not of type SB-C::CONTINUATION.
;;; This bug was fixed by APD in sbcl-0.7.1.30.
(defun bug150-test1 ()
  (let* ()
    (flet ((wufn () (glorp table1 4.9)))
      (gleep *uustk* #'wufn "#1" (list)))
    (if (eql (lo foomax 3.2))
        (values)
        (error "not ~S" '(eql (lo foomax 3.2))))
    (values)))
;;; A simpler test case for bug 150: The compiler died with the
;;; same type error when trying to compile this.
(defun bug150-test2 ()
  (let ()
    (<)))

;;; bug 147, fixed by APD 2002-04-28
;;;
;;; This test case used to crash the compiler, e.g. with
;;;   failed AVER: "(= (LENGTH (BLOCK-SUCC CALL-BLOCK)) 1)"
(defun bug147 (string ind)
  (flet ((digs ()
           (let (old-index)
             (if (and (< ind ind)
                      (typep (char string ind) '(member #\1)))
                 nil))))))

;;; bug reported and fixed by Matthias Hoelzl sbcl-devel 2002-05-13
(defmacro foo-2002-05-13 () ''x)
(eval '(foo-2002-05-13))
(compile 'foo-2002-05-13)
(foo-2002-05-13) ; (The bug caused UNDEFINED-FUNCTION to be signalled here.)

;;; floating point pain on the PPC.
;;;
;;; This test case used to fail to compile on most powerpcs prior to
;;; sbcl-0.7.4.2x, as floating point traps were being incorrectly
;;; masked.
(defun floating-point-pain (x)
  (declare (single-float x))
  (log x))

;;; bug found and fixed ca. sbcl-0.7.5.12: The INTERSECTION-TYPE
;;; here satisfies "is a subtype of ARRAY-TYPE", but can't be
;;; accessed with ARRAY-TYPE accessors like
;;; ARRAY-TYPE-SPECIALIZED-ELEMENT-TYPE, so ARRAY-related
;;; DEFTRANSFORMs died with TYPE-ERROR at compile time when
;;; compiling the DEFUN here.
(defun stupid-input-to-smart-array-deftransforms-0-7-5-12 (v)
  (declare (type (and simple-vector fwd-type-ref) v))
  (aref v 0))

;;; Ca. sbcl-0.7.5.15 the compiler would fail an internal consistency
;;; check on this code because it expected all calls to %INSTANCE-REF
;;; to be transformed away, but its expectations were dashed by perverse
;;; code containing app programmer errors like this.
(defstruct something-known-to-be-a-struct x y)
(with-test (:name (compile defstruct :instance-ref))
  (assert (nth-value
           1 (checked-compile
              ;; Python sees the structure accessor on the
              ;; known-not-to-be-a-struct A5 value and is very, very
              ;; disappointed in you. (But it doesn't signal BUG any
              ;; more.)
              `(lambda ()
                 (labels ((a1 (a2 a3)
                            (cond (t (a4 a2 a3))))
                          (a4 (a2 a3 a5 a6)
                            (declare (type (or simple-vector null) a5 a6))
                            (something-known-to-be-a-struct-x a5))
                          (a8 (a2 a3)
                            (a9 #'a1 a10 a2 a3))
                          (a11 (a2 a3)
                            (cond ((and (funcall a12 a2)
                                        (funcall a12 a3))
                                   (funcall a13 a2 a3))
                                  (t
                                   (when a14
                                     (let ((a15 (a1 a2 a3)))
                                       ))
                                   a16))))
                   (values #'a17 #'a11)))
              :allow-failure t :allow-warnings t :allow-style-warnings t))))

;;; On the SPARC, there was an erroneous definition of some VOPs used
;;; to compile LOGANDs, which would lead to compilation of the
;;; following function giving rise to a compile-time error (bug
;;; spotted and fixed by Raymond Toy for CMUCL)
(defun logand-sparc-bogons (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
  (declare (type (unsigned-byte 32) a0)
           (type (signed-byte 32) a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
           ;; to ensure that the call is a candidate for
           ;; transformation
           (optimize (speed 3) (safety 0) (compilation-speed 0) (debug 0)))
  (values
   ;; the call that fails compilation
   (logand a0 a10)
   ;; a call to prevent the other arguments from being optimized away
   (logand a1 a2 a3 a4 a5 a6 a7 a8 a9)))

;;; bug 192, reported by Einar Floystad Dorum sbcl-devel 2002-08-14,
;;; fixed in sbcl-0.7.6.26: Compiling this function in 0.7.6 caused
;;; the compiler to try to constant-fold DATA-VECTOR-REF, which is OK,
;;; except that there was no non-VOP definition of DATA-VECTOR-REF, so
;;; it would fail.
(defun bug192 ()
      (funcall
       (LAMBDA (TEXT I L )
         (LABELS ((G908 (I)
                    (LET ((INDEX
                           (OR
                            (IF (= I L)
                                NIL
                                (LET ((S TEXT)
                                      (E (ELT TEXT I)))
                                  (DECLARE (IGNORABLE S E))
                                  (WHEN (EQL #\a E)
                                    (G909 (1+ I))))))))
                      INDEX))
                  (G909 (I)
                    (OR
                     (IF (= I L)
                         NIL
                         (LET ((S TEXT)
                               (E (ELT TEXT I)))
                           (DECLARE (IGNORABLE S E))
                           (WHEN (EQL #\b E) (G910 (1+ I)))))))
                  (G910 (I)
                    (LET ((INDEX
                           (OR
                            (IF NIL
                                NIL
                                (LET ((S TEXT))
                                  (DECLARE (IGNORABLE S))
                                  (WHEN T I))))))
                      INDEX)))
           (G908 I))) "abcdefg" 0 (length "abcdefg")))

;;; bugs #65, #70, and #109, closed by APD's patch sbcl-devel 2002-08-17
;;;
;;; This was "YA code deletion bug" whose symptom was the failure of
;;; the assertion
;;;   (EQ (C::LAMBDA-TAIL-SET C::CALLER)
;;;       (C::LAMBDA-TAIL-SET (C::LAMBDA-HOME C::CALLEE)))
;;; at compile time.
(defun bug65-1 (termx termy) ; from Carl Witty on submit bugs list, debian.org
  (labels
    ((alpha-equal-bound-term-lists (listx listy)
       (or (and (null listx) (null listy))
           (and listx listy
                (let ((bindings-x (bindings-of-bound-term (car listx)))
                      (bindings-y (bindings-of-bound-term (car listy))))
                  (if (and (null bindings-x) (null bindings-y))
                      (alpha-equal-terms (term-of-bound-term (car listx))
                                         (term-of-bound-term (car listy)))
                      (and (= (length bindings-x) (length bindings-y))
                           (prog2
                               (enter-binding-pairs (bindings-of-bound-term (car listx))
                                                    (bindings-of-bound-term (car listy)))
                               (alpha-equal-terms (term-of-bound-term (car listx))
                                                  (term-of-bound-term (car listy)))
                             (exit-binding-pairs (bindings-of-bound-term (car listx))
                                                 (bindings-of-bound-term (car listy)))))))
                (alpha-equal-bound-term-lists (cdr listx) (cdr listy)))))

     (alpha-equal-terms (termx termy)
       (if (and (variable-p termx)
                (variable-p termy))
           (equal-bindings (id-of-variable-term termx)
                           (id-of-variable-term termy))
           (and (equal-operators-p (operator-of-term termx) (operator-of-term termy))
                (alpha-equal-bound-term-lists (bound-terms-of-term termx)
                                              (bound-terms-of-term termy))))))

    (or (eq termx termy)
        (and termx termy
             (with-variable-invocation (alpha-equal-terms termx termy))))))
(defun bug65-2 () ; from Bob Rogers cmucl-imp 1999-07-28
  ;; Given an FSSP alignment file named by the argument . . .
  (labels ((get-fssp-char ()
             (get-fssp-char))
           (read-fssp-char ()
             (get-fssp-char)))
    ;; Stub body, enough to tickle the bug.
    (list (read-fssp-char)
          (read-fssp-char))))
(defun bug70 ; from David Young cmucl-help 30 Nov 2000
    (item sequence &key (test #'eql))
  (labels ((find-item (obj seq test &optional (val nil))
                      (let ((item (first seq)))
                        (cond ((null seq)
                               (values nil nil))
                              ((funcall test obj item)
                               (values val seq))
                              (t
                               (find-item obj
                                          (rest seq)
                                          test
                                          (nconc val `(,item))))))))
    (find-item item sequence test)))
(defun bug109 () ; originally from CMU CL bugs collection, reported as
                 ; SBCL bug by MNA 2001-06-25
  (labels
      ((eff (&key trouble)
            (eff)
            ;; nil
            ;; Uncomment and it works
            ))
    (eff)))

;;; bug 192a, fixed by APD "more strict type checking" patch
;;; (sbcl-devel 2002-08-07)
(defun bug192a (x)
  (declare (optimize (speed 0) (safety 3)))
  ;; Even with bug 192a, this declaration was checked as an assertion.
  (declare (real x))
  (+ x
     (locally
       ;; Because of bug 192a, this declaration was trusted without checking.
       (declare (single-float x))
       (sin x))))
(with-test (:name (declare type :bug-192a))
  (assert-error (bug192a nil) type-error)
  (multiple-value-bind (result error) (ignore-errors (bug192a 100))
    (assert (null result))
    (assert (equal (type-error-expected-type error) 'single-float))))

;;; bug 194, fixed in part by APD "more strict type checking" patch
;;; (sbcl-devel 2002-08-07)
(with-test (:name (the :bug-194))
  (assert-error (multiple-value-prog1 (progn (the real '(1 2 3)))) type-error)
  (assert-error (the real '(1 2 3)) type-error))

(defun bug194d ()
  (null (ignore-errors
          (let ((arg1 1)
                (arg2 (identity (the real #(1 2 3)))))
            (if (< arg1 arg2) arg1 arg2)))))
(with-test (:name (the :bug-194d))
  (assert (eq (bug194d) t)))


;;; BUG 48a. and b. (symbol-macrolet handling), fixed by Eric Marsden
;;; and Raymond Toy for CMUCL, fix ported for sbcl-0.7.6.18.
(flet ((test (form)
         (multiple-value-bind (function failure-p)
             (checked-compile form :allow-failure t)
           (assert failure-p)
           (assert-error (funcall function) program-error))))

  (with-test (:name (compile symbol-macrolet :bug-48a))
    (test `(lambda ()
             ;; not interested in the package lock violation here
             (declare (sb-ext:disable-package-locks t))
             (symbol-macrolet ((t nil)) t))))

  (with-test (:name (compile symbol-macrolet :bug-48b))
    (test `(lambda ()
             ;; not interested in the package lock violation here
             (declare (sb-ext:disable-package-locks *standard-input*))
             (symbol-macrolet ((*standard-input* nil))
               *standard-input*))))

  (with-test (:name (compile symbol-macrolet :bug-48c))
    (test `(lambda ()
             (symbol-macrolet ((s nil)) (declare (special s)) s)))))


;;; bug 120a: Turned out to be constraining code looking like (if foo
;;; <X> <X>) where <X> was optimized by the compiler to be the exact
;;; same block in both cases, but not turned into (PROGN FOO <X>).
;;; Fixed by APD in sbcl-0.7.7.2, who provided this test:
(declaim (inline dont-constrain-if-too-much))
(defun dont-constrain-if-too-much (frame up-frame)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (if (or (not frame) t)
      frame
      "bar"))
(defun dont-constrain-if-too-much-aux (x y)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (if x t (if y t (dont-constrain-if-too-much x y))))

(with-test (:name (if :constraint-propagation :bug-120a))
  (assert (null (dont-constrain-if-too-much-aux nil nil))))

;;; TYPE-ERROR confusion ca. sbcl-0.7.7.24, reported and fixed by
;;; APD sbcl-devel 2002-09-14
(defun exercise-0-7-7-24-bug (x)
  (declare (integer x))
  (let (y)
    (setf y (the single-float (if (> x 0) x 3f0)))
    (list y y)))
(with-test (:name (type-error :0.7.7.24))
  (assert-error (exercise-0-7-7-24-bug 4) type-error)
  (assert (equal (exercise-0-7-7-24-bug -4) '(3f0 3f0))))

;;; non-intersecting type declarations were DWIMing in a confusing
;;; fashion until sbcl-0.7.7.28, when APD reported and fixed the
;;; problem.
(defun non-intersecting-the (x)
  (let (y)
    (setf y (the single-float (the integer x)))
    (list y y)))

(with-test (:name :non-intersecting-the)
  (assert-error (non-intersecting-the 3) type-error)
  (assert-error (non-intersecting-the 3f0) type-error))

;;; until 0.8.2 SBCL did not check THEs in arguments
(defun the-in-arguments-aux (x)
  x)
(defun the-in-arguments-1 (x)
  (list x (the-in-arguments-aux (the (single-float 0s0) x))))
(defun the-in-arguments-2 (x)
  (list x (the-in-arguments-aux (the single-float x))))

(with-test (:name (the :argument))
  (assert-error (the-in-arguments-1 1) type-error)
  (assert-error (the-in-arguments-2 1) type-error))

;;; bug 153: a hole in a structure slot type checking
(declaim (optimize safety))
(defstruct foo153
  (bla 0 :type fixnum))
(defun bug153-1 ()
  (let ((foo (make-foo153)))
    (setf (foo153-bla foo) '(1 . 1))
    (format t "Is ~a of type ~a a cons? => ~a~%"
            (foo153-bla foo)
            (type-of (foo153-bla foo))
            (consp (foo153-bla foo)))))
(defun bug153-2 (x)
  (let ((foo (make-foo153)))
    (setf (foo153-bla foo) x)
    (format t "Is ~a of type ~a a cons? => ~a~%"
            (foo153-bla foo)
            (type-of (foo153-bla foo))
            (consp (foo153-bla foo)))))

(with-test (:name (:name defstruct :slot :type :bug-153))
  (assert-error (bug153-1) type-error)
  (assert-error (bug153-2 '(1 . 1)) type-error))

;;;; bug 110: the compiler flushed the argument type test and the default
;;;; case in the cond.
;
;(locally (declare (optimize (safety 3) (speed 2)))
;  (defun bug110 (x)
;    (declare (optimize (safety 2) (speed 3)))
;    (declare (type (or string stream) x))
;    (cond ((typep x 'string) 'string)
;          ((typep x 'stream) 'stream)
;          (t
;           'none))))
;
;(multiple-value-bind (result condition)
;    (ignore-errors (bug110 0))
;  (declare (ignore result))
;  (assert (typep condition 'type-error)))

;;; bug 202: the compiler failed to compile a function, which derived
;;; type contradicted declared.
(declaim (ftype (function () null) bug202))
(defun bug202 ()
  t)

;;; bugs 178, 199: compiler failed to compile a call of a function
;;; with a hairy type
(defun bug178 (x)
  (funcall (the function (the standard-object x))))

(defun bug199-aux (f)
  (eq nil (funcall f)))

(defun bug199 (f x)
  (declare (type (and function (satisfies bug199-aux)) f))
  (funcall f x))

(with-test (:name (declaim &optional &rest :bogus style-warning))
  (assert-no-signal
   (ctu:file-compile
    "(declaim (ftype (function (symbol &optional t &rest t)) foo))
     (defun foo (x &optional y &rest z)
       (declare (ignore x y z)))"
    :load nil)
   style-warning))

;;; check non-toplevel DEFMACRO
(defvar *defmacro-test-status* nil)

(with-test (:name (compile-file defmacro))
  (fmakunbound 'defmacro-test-aux)
  (let* ((src "defmacro-test.lisp")
         (obj (scratch-file-name "fasl")))
    (unwind-protect
         (progn
           (compile-file src :output-file obj)
           (assert (equal *defmacro-test-status* '(function a)))
           (setq *defmacro-test-status* nil)
           (load obj)
           (assert (equal *defmacro-test-status* nil))
           (macroexpand '(defmacro-test-aux 'a))
           (assert (equal *defmacro-test-status* '(macro 'a z-value)))
           (eval '(defmacro-test-aux 'a))
           (assert (equal *defmacro-test-status* '(expanded 'a z-value))))
      (ignore-errors (delete-file obj)))))

;;; bug 204: EVAL-WHEN inside a local environment
(defvar *bug204-test-status*)

(with-test (:name (compile-file eval-when :local-environment :bug-204))
  (let* ((src "bug204-test.lisp")
         (obj (scratch-file-name "fasl")))
    (unwind-protect
         (progn
           (setq *bug204-test-status* nil)
           (compile-file src :output-file obj :verbose nil :print nil)
           (assert (equal *bug204-test-status* '((:expanded :load-toplevel)
                                                 (:called :compile-toplevel)
                                                 (:expanded :compile-toplevel))))
           (setq *bug204-test-status* nil)
           (load obj)
           (assert (equal *bug204-test-status* '((:called :load-toplevel)))))
      (ignore-errors (delete-file obj)))))

;;; toplevel SYMBOL-MACROLET
;;; FIXME: I didn't look into why this fails in the interpreter, but it does.
(defvar *symbol-macrolet-test-status*)

(with-test (:name (compile-file symbol-macrolet) :skipped-on :interpreter)
  (let* ((src "symbol-macrolet-test.lisp")
         (obj (scratch-file-name "fasl")))
    (unwind-protect
         (progn
           (setq *symbol-macrolet-test-status* nil)
           (compile-file src :output-file obj :verbose nil :print nil)
           (assert (equal *symbol-macrolet-test-status*
                          '(2 1)))
           (setq *symbol-macrolet-test-status* nil)
           (load obj)
           (assert (equal *symbol-macrolet-test-status* '(2))))
      (ignore-errors (delete-file obj)))))

;;; On the x86, this code failed to compile until sbcl-0.7.8.37:
(defun x86-assembler-failure (x)
  (declare (optimize (speed 3) (safety 0)))
  (eq (setf (car x) 'a) nil))

;;; bug 211: :ALLOW-OTHER-KEYS
(defun bug211d (&key (x :x x-p) ((:allow-other-keys y) :y y-p))
  (list x x-p y y-p))

(with-test (:name (:allow-other-keys :bug-211d))
  (assert (equal (bug211d) '(:x nil :y nil)))
  (assert (equal (bug211d :x 1) '(1 t :y nil)))
  (assert-error (bug211d :y 2) program-error)
  (assert (equal (bug211d :y 2 :allow-other-keys t :allow-other-keys nil)
                 '(:x nil t t)))
  (assert-error (bug211d :y 2 :allow-other-keys nil) program-error))

(checked-compile
 '(lambda ()
    (flet ((test (&key (x :x x-p) ((:allow-other-keys y) :y y-p))
             (list x x-p y y-p)))
      (assert (equal (test) '(:x nil :y nil)))
      (assert (equal (test :x 1) '(1 t :y nil)))
      (assert (equal (test :y 2 :allow-other-keys 11 :allow-other-keys nil)
                     '(:x nil 11 t)))))
 :name 'bug211b)
(with-test (:name (compile :lambda-list :bug-211b))
  (bug211b))

(with-test (:name (compile :lambda-list :bug-211c))
  (checked-compile-and-assert ()
      '(lambda ()
         (flet ((test (&key (x :x x-p))
                  (list x x-p)))
           (assert (equal (test) '(:x nil)))
           (assert (equal (test :x 1) '(1 t)))
           (assert (equal (test :y 2 :allow-other-keys 11 :allow-other-keys nil)
                          '(:x nil)))
           nil))
      (() nil)))

(with-test (:name (compile :lambda-list :allow-other-keys
                           :bug-211 :do-not-allow))
  (dolist (form '((test :y 2)
                  (test :y 2 :allow-other-keys nil)
                  (test :y 2 :allow-other-keys nil :allow-other-keys t)))
    (multiple-value-bind (result failure-p)
        (checked-compile
         `(lambda ()
            (flet ((test (&key (x :x x-p) ((:allow-other-keys y) :y y-p))
                     (list x x-p y y-p)))
              ,form))
         :allow-failure t :allow-warnings t)
      (assert failure-p)
      (assert-error (funcall result) program-error))))

;;; bug 217: wrong type inference
(defun bug217-1 (x s)
  (let ((f (etypecase x
             (character #'write-char)
             (integer #'write-byte))))
    (funcall f x s)
    (etypecase x
      (character (write-char x s))
      (integer (write-byte x s)))))
(with-test (:name (:bug-217))
  (bug217-1 #\1 (make-broadcast-stream)))

;;; bug 221: tried and died on CSUBTYPEP (not VALUES-SUBTYPEP) of the
;;; function return types when inferring the type of the IF expression
(declaim (ftype (function (fixnum) (values package boolean)) bug221f1))
(declaim (ftype (function (t) (values package boolean)) bug221f2))
(defun bug221 (b x)
  (funcall (if b #'bug221f1 #'bug221f2) x))

;;; bug 172: macro lambda lists were too permissive until 0.7.9.28
;;; (fix provided by Matthew Danish) on sbcl-devel
(with-test (:name (defmacro :lambda-list :bug-172))
  (assert-error (defmacro bug172 (&rest rest foo) `(list ,rest ,foo))))

(with-test (:name (compile :embedded the))
  (flet ((check-embedded-thes (policy1 policy2 x y)
           (handler-case
               (funcall (checked-compile
                         `(lambda (f)
                            (declare (optimize (speed 2) (safety ,policy1)))
                            (multiple-value-list
                             (the (values (integer 2 3) t &optional)
                                  (locally (declare (optimize (safety ,policy2)))
                                    (the (values t (single-float 2f0 3f0) &optional)
                                         (funcall f)))))))
                        (lambda () (values x y)))
             (type-error (error)
               error))))

    (assert (equal (check-embedded-thes 0 0  :a :b) '(:a :b)))

    (assert (equal (check-embedded-thes 0 3  :a 2.5f0) '(:a 2.5f0)))
    (assert (typep (check-embedded-thes 0 3  2 3.5f0) 'type-error))

    (assert (equal (check-embedded-thes 0 1  :a 3f0) '(:a 3f0)))
    (assert (typep (check-embedded-thes 0 1  2 2.5d0) 'type-error))

    (assert (equal (check-embedded-thes 3 0  2 :a) '(2 :a)))
    (assert (typep (check-embedded-thes 3 0  4 2.5f0) 'type-error))

    (assert (equal (check-embedded-thes 1 0  3 :b) '(3 :b)))
    (assert (typep (check-embedded-thes 1 0  1.0 2.5f0) 'type-error))


    (assert (equal (check-embedded-thes 3 3  2 2.5f0) '(2 2.5f0)))
    (assert (typep (check-embedded-thes 3 3  0 2.5f0) 'type-error))
    (assert (typep (check-embedded-thes 3 3  2 3.5f0) 'type-error))))


;;; INLINE inside MACROLET
(declaim (inline to-be-inlined))
(macrolet ((def (x) `(defun ,x (y) (+ y 1))))
  (def to-be-inlined))
(defun call-inlined (z)
  (to-be-inlined z))
(assert (= (call-inlined 3) 4))
(macrolet ((frob (x) `(+ ,x 3)))
  (defun to-be-inlined (y)
    (frob y)))
#-interpreter
(assert (= (call-inlined 3)
           ;; we should have inlined the previous definition, so the
           ;; new one won't show up yet.
           4))
(defun call-inlined (z)
  (to-be-inlined z))
#-interpreter (assert (= (call-inlined 3) 6))
(defun to-be-inlined (y)
  (+ y 5))
#-interpreter (assert (= (call-inlined 3) 6))

;;; DEFINE-COMPILER-MACRO to work as expected, not via weird magical
;;; IR1 pseudo-:COMPILE-TOPLEVEL handling
(defvar *bug219-a-expanded-p* nil)
(defun bug219-a (x)
  (+ x 1))
(define-compiler-macro bug219-a (&whole form y)
  (setf *bug219-a-expanded-p* t)
  (if (constantp y)
      (+ (eval y) 2)
      form))
(defun bug219-a-aux ()
  (bug219-a 2))
(defvar *bug219-a-temp* 3)
(with-test (:name (define-compiler-macro :bug-219a))
  (assert (= (bug219-a-aux) (if *bug219-a-expanded-p* 4 3)))
  (assert (= (bug219-a *bug219-a-temp*) 4)))

(defvar *bug219-b-expanded-p* nil)
(defun bug219-b-aux1 (x)
  (when x
    (define-compiler-macro bug219-b (y)
      (setf *bug219-b-expanded-p* t)
      `(+ ,y 2))))
(defun bug219-b-aux2 (z)
  (bug219-b z))
(assert (not *bug219-b-expanded-p*))
(assert-error (bug219-b-aux2 1) undefined-function)
(bug219-b-aux1 t)
(defun bug219-b-aux2 (z)
  (bug219-b z))
(defun bug219-b (x)
  x)
(assert (= (bug219-b-aux2 1)
           (if *bug219-b-expanded-p* 3 1)))

(with-test (:name (:unreachable-code locally declare :bug-224))
  (map-optimize-declarations
   (lambda (declaration)
     (multiple-value-bind (fun failure-p warnings style-warnings)
         (checked-compile
          `(lambda ()
             (declare (optimize ,@declaration))
             (,(gensym "LOCALY") (declare (optimize (safety 3)))
              (ignore-errors (progn (values-list (car (list '(1 . 2)))) t))))
          :allow-failure t :allow-style-warnings t)
       (declare (ignore fun warnings))
       (assert failure-p)
       (assert (= (length style-warnings) 1))))
   :safety nil))

(with-test (:name (:unreachable-code error labels :bug-224))
  (map-optimize-declarations
   (lambda (declaration)
     (checked-compile `(lambda ()
                         (declare (optimize ,@declaration))
                         (labels ((ext ()
                                    (tagbody
                                       (labels ((i1 () (list (i2) (i2)))
                                                (i2 () (list (int) (i1)))
                                                (int () (go :exit)))
                                         (list (i1) (i1) (i1)))
                                     :exit (return-from ext))))
                           (list (error "nih") (ext) (ext))))))
   :safety nil))

(with-test (:name (:unreachable-code error let :bug-224))
  (map-optimize-declarations
    (lambda (declaration)
      (checked-compile `(lambda (x)
                          (declare (optimize ,@declaration))
                          (let ((y (error ""))) (list x y)))))
    :safety nil))

;;; bug 223: invalid moving of global function name referencing
(defun bug223-int (n)
  `(int ,n))

(defun bug223-wrap ()
  (let ((old #'bug223-int))
    (setf (fdefinition 'bug223-int)
          (lambda (n)
            (assert (> n 0))
            `(ext ,@(funcall old (1- n)))))))

(with-test (:name (compile setf fdefinition :bug-223))
  (compile 'bug223-wrap)
  (assert (equal (bug223-int 4) '(int 4)))
  (bug223-wrap)
  (assert (equal (bug223-int 4) '(ext int 3)))
  (bug223-wrap)
  (assert (equal (bug223-int 4) '(ext ext int 2))))

;;; COERCE got its own DEFOPTIMIZER which has to reimplement most of
;;; SPECIFIER-TYPE-NTH-ARG.  For a while, an illegal type would throw
;;; you into the debugger on compilation.
(defun coerce-defopt1 (x)
  ;; illegal, but should be compilable.
  (coerce x '(values t)))
(defun coerce-defopt2 (x)
  ;; illegal, but should be compilable.
  (coerce x '(values t &optional)))
(with-test (:name (coerce :optimizer))
  (assert-error (coerce-defopt1 3))
  (assert-error (coerce-defopt2 3)))

;;; Oops.  In part of the (CATCH ..) implementation of DEBUG-RETURN,
;;; it was possible to confuse the type deriver of the compiler
;;; sufficiently that compiler invariants were broken (explained by
;;; APD sbcl-devel 2003-01-11).

;;; WHN's original report
(defun debug-return-catch-break1 ()
  (with-open-file (s "/tmp/foo"
                     :direction :output
                     :element-type (list
                                    'signed-byte
                                    (1+
                                     (integer-length most-positive-fixnum))))
    (read-byte s)
    (read-byte s)
    (read-byte s)
    (read-byte s)))

;;; APD's simplified test case
(defun debug-return-catch-break2 (x)
  (declare (type (vector (unsigned-byte 8)) x))
  (setq *y* (the (unsigned-byte 8) (aref x 4))))

;;; FUNCTION-LAMBDA-EXPRESSION should return something that COMPILE
;;; can understand.  Here's a simple test for that on a function
;;; that's likely to return a hairier list than just a lambda:
(macrolet ((def (fn) `(progn
                       (declaim (inline ,fn))
                       (defun ,fn (x) (1+ x)))))
  (def bug228))
(with-test (:name (function-lambda-expression compile :bug-228))
  (let ((x (function-lambda-expression #'bug228)))
    (when x
      (checked-compile-and-assert (:optimize nil) x
        ((1) 2)))))

;;;
(defun bug192b (i)
  (dotimes (j i)
    (declare (type (mod 4) i))
    (unless (< i 5)
      (print j))))
(with-test (:name :bug-192b)
  (assert-error (bug192b 6) type-error))

(defun bug192c (x y)
  (locally (declare (type fixnum x y))
    (+ x (* 2 y))))
(with-test (:name :bug-192c)
  (assert-error (bug192c 1.1 2) type-error)
  (assert-error (progn (the real (list 1)) t) type-error))

(defun bug236 (a f)
  (declare (optimize (speed 2) (safety 0)))
  (+ 1d0
     (the double-float
       (multiple-value-prog1
           (svref a 0)
         (unless f (return-from bug236 0))))))
(with-test (:name :bug-236)
  (assert (eql (bug236 #(4) nil) 0)))

;;; Bug reported by reported by rif on c.l.l 2003-03-05
(defun test-type-of-special-1 (x)
  (declare (special x)
           (fixnum x)
           (optimize (safety 3)))
  (list x))
(defun test-type-of-special-2 (x)
  (declare (special x)
           (fixnum x)
           (optimize (safety 3)))
  (list x (setq x (/ x 2)) x))
(with-test (:name (declare special type))
  (assert-error (test-type-of-special-1 3/2) type-error)
  (assert-error (test-type-of-special-2 3) type-error)
  (assert (equal (test-type-of-special-2 8) '(8 4 4))))

;;; bug which existed in 0.8alpha.0.4 for several milliseconds before
;;; APD fixed it in 0.8alpha.0.5
(defun frob8alpha04 (x y)
  (+ x y))
(defun baz8alpha04 (this kids)
  (flet ((n-i (&rest rest)
           ;; Removing the #+NIL here makes the bug go away.
           #+nil (format t "~&in N-I REST=~S~%" rest)
           (apply #'frob8alpha04 this rest)))
    (n-i kids)))
;;; failed in 0.8alpha.0.4 with "The value 13 is not of type LIST."
(with-test (:name (&rest type-error))
  (assert (= (baz8alpha04 12 13) 25)))

;;; evaluation order in structure slot writers
(defstruct sswo
  a b)
(with-test (:name (:evaluation-order structure :slot-writer))
  (let* ((i 0)
         (s (make-sswo :a (incf i) :b (incf i)))
         (l (list s :v)))
    (assert (= (sswo-a s) 1))
    (assert (= (sswo-b s) 2))
    (setf (sswo-a (pop l)) (pop l))
    (assert (eq l nil))
    (assert (eq (sswo-a s) :v))))

(defun bug249 (x)
  (flet ((bar (y)
           (declare (fixnum y))
           (incf x)))
    (list (bar x) (bar x) (bar x))))

(with-test (:name (flet declare type :bug-249))
  (assert-error (bug249 1.0) type-error))

;;; bug reported by ohler on #lisp 2003-07-10
(defun bug-ohler-2003-07-10 (a b)
  (declare (optimize (speed 0) (safety 3) (space 0)
                     (debug 1) (compilation-speed 0)))
  (adjoin a b))

;;; bug reported by Doug McNaught on sbcl-devel 2003-09-14:
;;; COMPILE-FILE did not bind *READTABLE*
(with-test (:name (compile-file *readtable*))
  (let* ((source "bug-doug-mcnaught-20030914.lisp")
         (fasl (scratch-file-name "fasl")))
    (labels ((check ()
               (assert (null (get-macro-character #\]))))
             (full-check ()
               (check)
               (assert (typep *bug-doug-mcnaught-20030914*
                              '(simple-array (unsigned-byte 4) (*))))
               (assert (equalp *bug-doug-mcnaught-20030914* #(1 2 3)))
               (makunbound '*bug-doug-mcnaught-20030914*)))
      (compile-file source :output-file fasl)
      (check)
      (load fasl)
      (full-check)
      (load source)
      (full-check)
      (delete-file fasl))))

(defun expt-derive-type-bug (a b)
  (unless (< a b)
    (truncate (expt a b))))
(with-test (:name (expt :derive-type))
  (assert (equal (multiple-value-list (expt-derive-type-bug 1 1))
                 '(1 0))))

;;; Problems with type checking in functions with EXPLICIT-CHECK
;;; attribute (reported by Peter Graves)
(with-test (:name (:type-checking :explicit-check))
  (loop for (fun . args) in '((= a) (/= a)
                              (< a) (<= a) (> a) (>= a))
     do (assert-error (apply fun args) type-error)))

(defclass broken-input-stream (sb-gray:fundamental-input-stream) ())
(defmethod sb-gray:stream-read-char ((stream broken-input-stream))
  (throw 'break :broken))
(with-test (:name :explicit-check)
  (assert (eql (block return
                 (handler-case
                     (catch 'break
                       (funcall (eval ''peek-char)
                                1 (make-instance 'broken-input-stream))
                       :test-broken)
                   (type-error (c)
                     c
                     (return-from return :good))))
               :good)))

;;;; MUFFLE-CONDITIONS test (corresponds to the test in the manual)
; FIXME: make a better test!
(with-test (:name muffle-conditions :skipped-on (or :ppc64 :x86-64))
  (multiple-value-bind (fun failure-p warnings style-warnings notes)
      (checked-compile
       '(lambda (x)
         (declare (optimize speed) (fixnum x))
         (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
         (values (* x 5)                ; no compiler note from this
          (locally
              (declare (sb-ext:unmuffle-conditions sb-ext:compiler-note))
            ;; this one gives a compiler note
            (* x -5)))))
    (declare (ignore failure-p warnings style-warnings))
    (assert (= (length notes) 1))
    (assert (equal (multiple-value-list (funcall fun 1)) '(5 -5)))))

(with-test (:name (flet labels &key))
  (assert-error (eval '(flet ((%f (&key) nil)) (%f nil nil))))
  (assert-error (eval '(labels ((%f (&key x) x)) (%f nil nil)))))

;;; PROGV must not bind constants, or violate declared types -- ditto for SET.
(assert-error (set pi 3))
(assert-error (progv '(pi s) '(3 pi) (symbol-value x)))
(declaim (cons *special-cons*))
(assert-error (set '*special-cons* "nope") type-error)
(assert-error (progv '(*special-cons*) '("no hope") (car *special-cons*)) type-error)

;;; No bogus warnings for calls to functions with complex lambda-lists.
(defun complex-function-signature (&optional x &rest y &key z1 z2)
  (declare (ignore z1 z2))
  (cons x y))
(with-test (:name :complex-call-doesnt-warn)
  (checked-compile '(lambda (x) (complex-function-signature x :z1 1 :z2 2))))

(with-test (:name :non-required-args-update-info)
  (let ((name (gensym "NON-REQUIRE-ARGS-TEST"))
        (*evaluator-mode* :compile))
    (eval `(defun ,name (x) x))
    (assert (equal '(function (t) (values t &optional))
                   (sb-kernel:type-specifier (sb-int:global-ftype name))))
    (eval `(defun ,name (x &optional y) (or x y)))
    (assert (equal '(function (t &optional t) (values t &optional))
                   (sb-kernel:type-specifier (sb-int:global-ftype name))))))

;;;; inline & maybe inline nested calls

(defun quux-marker (x) x)
(declaim (inline foo-inline))
(defun foo-inline (x) (quux-marker x))
(declaim (maybe-inline foo-maybe-inline))
(defun foo-maybe-inline (x) (quux-marker x))

(with-test (:name :nested-inline-calls)
  (let ((fun (checked-compile `(lambda (x)
                               (foo-inline (foo-inline (foo-inline x)))))))
    (assert (= 0 (ctu:count-full-calls "FOO-INLINE" fun)))
    (assert (= 3 (ctu:count-full-calls "QUUX-MARKER" fun)))))

(with-test (:name :nested-maybe-inline-calls)
  (let ((fun (checked-compile
              `(lambda (x)
                 (declare (optimize (space 0)))
                 (foo-maybe-inline (foo-maybe-inline (foo-maybe-inline x)))))))
    (assert (= 0 (ctu:count-full-calls "FOO-MAYBE-INLINE" fun)))
    (assert (= 1 (ctu:count-full-calls "QUUX-MARKER" fun)))))

(with-test (:name :inline-calls)
  (let ((fun (checked-compile `(lambda (x)
                                 (list (foo-inline x)
                                       (foo-inline x)
                                       (foo-inline x))))))
    (assert (= 0 (ctu:count-full-calls "FOO-INLINE" fun)))
    (assert (= 3 (ctu:count-full-calls "QUUX-MARKER" fun)))))

(with-test (:name :maybe-inline-calls)
  (let ((fun (checked-compile `(lambda (x)
                                 (declare (optimize (space 0)))
                                 (list (foo-maybe-inline x)
                                       (foo-maybe-inline x)
                                       (foo-maybe-inline x))))))
    (assert (= 0 (ctu:count-full-calls "FOO-MAYBE-INLINE" fun)))
    (assert (= 1 (ctu:count-full-calls "QUUX-MARKER" fun)))))

(with-test (:name :maybe-inline-let-calls)
  (checked-compile `(lambda (x)
                      (declare (optimize (space 0)))
                      (foo-maybe-inline (let ((z #'foo-maybe-inline))
                                          (funcall z x))))))

(with-test (:name :bug-405)
  ;; These used to break with a TYPE-ERROR
  ;;     The value NIL is not of type SB-C::ENVIRONMENT.
  ;; in MERGE-LETS.
  (ctu:file-compile
   '((LET (outer-let-var)
       (lambda ()
         (print outer-let-var)
         (MULTIPLE-VALUE-CALL 'some-function
           (MULTIPLE-VALUE-CALL (LAMBDA (a) 'foo)
             1))))))
  (ctu:file-compile
   '((declaim (optimize (debug 3)))
     (defstruct bug-405-foo bar)
     (let ()
       (flet ((i (x) (frob x (bug-405-foo-bar foo))))
         (i :five))))))

;;; bug 235a
(declaim (ftype (function (cons) number) bug-235a-aux))
(declaim (inline bug-235a-aux))
(defun bug-235a-aux (c)
  (the number (car c)))
(with-test (:name :bug-235a)
  (let ((fun (checked-compile
              `(lambda (x y)
                 (values (locally (declare (optimize (safety 0)))
                           (bug-235a-aux x))
                         (locally (declare (optimize (safety 3)))
                           (bug-235a-aux y)))))))
    (assert
     (eq :error
         (handler-case
             (funcall fun '(:one) '(:two))
           (type-error (e)
             (assert (eq :two (type-error-datum e)))
             (assert (eq 'number (type-error-expected-type e)))
             :error))))))

;;; This tested something which is no longer a thing. There was a hash-table
;;; named *COMPILED-DEBUG-FUNS* which held on to an arbitrary number of code blobs,
;;; and it was made weak in git rev 9abfd1a2b2 and then removed in rev a2aa5eceb5
;;; (except for cheneygc - and I'm not entirely sure that there isn't a way to
;;; remove it from cheneygc as well, but it wasn't interesting).
;;; So really it's testing nothing, except that maybe it is.
;;; But the test doesn't pass with parallel-exec because profiling the code causes
;;; potentially any and every lambda to be retained when a stack sample is taken.
;;; So I'm not sure whether to delete or disable this. I guess disable.
(with-test (:name :compiled-debug-funs-leak :fails-on :parallel-test-runner)
  (sb-ext:gc :full t)
  (let ((usage-before (sb-kernel::dynamic-usage)))
    (dotimes (x 10000)
      (let ((f (checked-compile '(lambda () (error "X")))))
        (handler-case
            (funcall f)
          (error () nil))))
    (sb-ext:gc :full t)
    (let ((usage-after (sb-kernel::dynamic-usage)))
      (when (< (+ usage-before 2000000) usage-after)
        (error "Leak")))))

;;; PROGV compilation and type checking when the declared type
;;; includes a FUNCTION subtype.
(declaim (type (or (function (t) (values boolean &optional)) string)
               *hairy-progv-var*))
(defvar *hairy-progv-var* #'null)
(with-test (:name :hairy-progv-type-checking)
  (assert (eq :error
              (handler-case
                  (progv '(*hairy-progv-var*) (list (eval 42))
                    *hairy-progv-var*)
                (type-error () :error))))
  (assert (equal "GOOD!"
                 (progv '(*hairy-progv-var*) (list (eval "GOOD!"))
                    *hairy-progv-var*))))

(with-test (:name :fill-complex-single-float)
  (assert (every (lambda (x) (eql x #c(-1.0 -2.0)))
                 (funcall
                  (lambda ()
                    (make-array 2
                                :element-type '(complex single-float)
                                :initial-element #c(-1.0 -2.0)))))))

(with-test (:name :make-array-symbol-as-initial-element)
  (checked-compile-and-assert ()
      `(lambda ()
         (make-array 12 :initial-element 'a))
    (() #(a a a a a a a a a a a a) :test 'equalp)))

;;; This non-minimal test-case catches a nasty error when loading
;;; inline constants.
(deftype matrix ()
  `(simple-array single-float (16)))
(declaim (ftype (sb-int:sfunction (single-float single-float single-float single-float
                                   single-float single-float single-float single-float
                                   single-float single-float single-float single-float
                                   single-float single-float single-float single-float)
                                  matrix)
                matrix)
         (inline matrix))
(defun matrix (m11 m12 m13 m14
               m21 m22 m23 m24
               m31 m32 m33 m34
               m41 m42 m43 m44)
  (make-array 16
              :element-type 'single-float
              :initial-contents (list m11 m21 m31 m41
                                      m12 m22 m32 m42
                                      m13 m23 m33 m43
                                      m14 m24 m34 m44)))
(declaim (ftype (sb-int:sfunction ((simple-array single-float (3)) single-float) matrix)
                rotate-around))
(defun rotate-around (a radians)
  (let ((c (cos radians))
        (s (sin radians))
        ;; The 1.0 here was misloaded on x86-64.
        (g (- 1.0 (cos radians))))
    (let* ((x (aref a 0))
           (y (aref a 1))
           (z (aref a 2))
           (gxx (* g x x)) (gxy (* g x y)) (gxz (* g x z))
           (gyy (* g y y)) (gyz (* g y z)) (gzz (* g z z)))
      (matrix
       (+ gxx c)        (- gxy (* s z))  (+ gxz (* s y)) 0.0
       (+ gxy (* s z))  (+ gyy c)        (- gyz (* s x)) 0.0
       (- gxz (* s y))  (+ gyz (* s x))  (+ gzz c)       0.0
       0.0              0.0              0.0             1.0))))
(with-test (:name :regression-1.0.29.54)
  (assert (every #'=
                 '(-1.0 0.0 0.0 0.0 0.0 -1.0 0.0 0.0 0.0 0.0 -1.0 0.0 0.0 0.0 0.0 1.0)
                 (rotate-around
                  (make-array 3 :element-type 'single-float :initial-element 0f0)
                  (coerce pi 'single-float))))
  ;; Same bug manifests in COMPLEX-ATANH as well.
  (assert (= (atanh #C(-0.7d0 1.1d0)) #C(-0.28715567731069275d0 0.9394245539093365d0))))

(with-test (:name :slot-value-on-structure)
  (let ((f (checked-compile `(lambda (x a b)
                               (declare (something-known-to-be-a-struct x))
                               (setf (slot-value x 'x) a
                                     (slot-value x 'y) b)
                               (list (slot-value x 'x)
                                     (slot-value x 'y))))))
    (assert (equal '(#\x #\y)
                   (funcall f
                            (make-something-known-to-be-a-struct :x "X" :y "Y")
                            #\x #\y)))
    (assert (not (ctu:find-named-callees f)))))

(defclass some-slot-thing ()
  ((slot :initarg :slot)))
(with-test (:name :with-slots-the)
  (let ((x (make-instance 'some-slot-thing :slot "foo")))
    (with-slots (slot) (the some-slot-thing x)
      (assert (equal "foo" slot)))))

;;; Missing &REST type in proclamation causing a miscompile.
(declaim (ftype
          (function
           (sequence unsigned-byte
                     &key (:initial-element t) (:initial-contents sequence))
           (values sequence &optional))
          bug-458354))
(defun bug-458354
    (sequence length
     &rest keys
     &key (initial-element nil iep) (initial-contents nil icp))
  (declare (sb-ext:unmuffle-conditions style-warning))
  (declare (ignorable keys initial-element iep initial-contents icp))
  (apply #'sb-sequence:make-sequence-like sequence length keys))
(with-test (:name :bug-458354)
  (assert (equalp #((a b) (a b)) (bug-458354 #(1 2) 2 :initial-element '(a b)))))

(with-test (:name :bug-542807)
  (handler-bind ((style-warning #'error))
    (eval '(defstruct bug-542807 slot)))
  (let (conds)
    (handler-bind ((style-warning (lambda (c)
                                    (push c conds))))
      (eval '(defstruct bug-542807 slot)))
    (assert (and conds
                 (every (lambda (x) (typep x 'sb-kernel:redefinition-with-defun))
                        conds)))))

(with-test (:name :defmacro-not-list-lambda-list)
  (assert-error (eval `(defmacro ,(gensym) "foo"))
                type-error))

(with-test (:name :bug-308951)
  (let ((x 1))
    (dotimes (y 10)
      (let ((y y))
        (when (funcall (eval #'(lambda (x) (eql x 2))) y)
          (defun bug-308951-foo (z)
            (incf x (incf y z))))))
    (defun bug-308951-bar (z)
      (bug-308951-foo z)
      (values x)))
  (assert (= 4 (bug-308951-bar 1))))

(declaim (inline bug-308914-storage))
(defun bug-308914-storage (x)
  (the (simple-array flt (*)) (bug-308914-unknown x)))

(with-test (:name :bug-308914-workaround)
  ;; This used to hang in ORDER-UVL-SETS.
  (handler-case
      (with-timeout 10
        (checked-compile
         `(lambda (lumps &key cg)
            (let ((nodes (map 'list (lambda (lump)
                                      (bug-308914-storage lump))
                              lumps)))
              (setf (aref nodes 0) 2)
              (assert (every #'~= (apply #'concatenate 'list nodes) '(2 3 6 9)))))
         :allow-warnings t :allow-style-warnings t))
    (sb-ext:timeout ()
      (error "Hang in ORDER-UVL-SETS?"))))

(declaim (inline inlined-function-in-source-path))
(defun inlined-function-in-source-path (x)
  (+ x x))

(with-test (:name :inlined-function-in-source-path)
  (let ((output
         (with-output-to-string (*error-output*)
           (compile nil `(lambda (x)
                           (declare (optimize speed))
                           (funcall #'inlined-function-in-source-path x))))))
    ;; We want the name
    (assert (search "INLINED-FUNCTION-IN-SOURCE-PATH" output))
    ;; ...not the leaf.
    (assert (not (search "DEFINED-FUN" output)))))

(defmacro bug-795705 ()
  t)

(with-test (:name :bug-795705)
  (assert (macro-function 'bug-795705))
  (fmakunbound 'bug-795705)
  (assert (not (macro-function 'bug-795705))))

(with-test (:name (load-time-value :type-derivation))
  (let ((name 'load-time-value-type-derivation-test))
    (labels ((funtype (fun)
               (sb-kernel:type-specifier
                (sb-kernel:single-value-type
                 (sb-kernel:fun-type-returns
                  (sb-kernel:specifier-type
                   (sb-kernel:%simple-fun-type fun))))))
             (test (type1 type2 form value-cell-p)
               (declare (ignore value-cell-p))
               (let* ((lambda-form `(lambda ()
                                      (load-time-value ,form)))
                      (core-fun (checked-compile lambda-form))
                      (core-type (funtype core-fun))
                      (defun-form `(defun ,name ()
                                     (load-time-value ,form)))
                      (file-fun (let ((*error-output* (make-broadcast-stream)))
                                  (ctu:file-compile (list defun-form) :load t)
                                  (symbol-function name)))
                      (file-type (funtype file-fun)))
                 (unless (subtypep core-type type1)
                   (error "core: wanted ~S, got ~S" type1 core-type))
                 (unless (subtypep file-type type2)
                   (error "file: wanted ~S, got ~S" type2 file-type)))))
      (let ((* 10))
        (test '(integer 11 11) 'number
              '(+ * 1) nil))
      (let ((* "fooo"))
        (test '(integer 4 4) 'unsigned-byte
              '(length *) nil))
      (test '(integer 10 10) '(integer 10 10) 10 nil)
      (test 'cons 'cons '(cons t t) t))))

(with-test (:name (load-time-value :errors))
  (multiple-value-bind (warn fail)
      (ctu:file-compile
       `((defvar *load-time-value-error-value* 10)
         (declaim (fixnum *load-time-value-error-value*))
         (defun load-time-value-error-test-1 ()
           (the list (load-time-value *load-time-value-error-value*))))
       :load t)
    (assert warn)
    (assert fail))
  (handler-case (load-time-value-error-test-1)
    (type-error (e)
      (and (eql 10 (type-error-datum e))
           (eql 'list (type-error-expected-type e)))))
  (multiple-value-bind (warn2 fail2)
      (ctu:file-compile
       `((defun load-time-value-error-test-2 ()
           (the list (load-time-value 10))))
       :load t)
    (assert warn2)
    (assert fail2))
  (handler-case (load-time-value-error-test-2)
    (type-error (e)
      (and (eql 10 (type-error-datum e))
           (eql 'list (type-error-expected-type e))))))

;;;; tests for compiler output
(with-test (:name :unexpected-compiler-output)
  (let* ((*error-output* (make-string-output-stream))
         (output (with-output-to-string (*standard-output*)
                   (compile-file "compiler-output-test.lisp"
                                 :output-file (scratch-file-name)
                                 :print nil :verbose nil))))
    (unless (zerop (length output))
      (error "Unexpected output: ~S" output))))

(with-test (:name :bug-493380)
  (flet ((test (forms)
           (catch 'debug
             (let ((*debugger-hook* (lambda (condition if)
                                      (declare (ignore if))
                                      (throw 'debug
                                        (if (typep condition 'serious-condition)
                                            :debug
                                            :oops)))))
               (multiple-value-bind (warned failed) (ctu:file-compile forms)
                 (when (and warned failed)
                   :failed))))))
    (assert (eq :failed (test "(defun")))
    (assert (eq :failed (test "(defun no-pkg::foo ())")))
    (assert (eq :failed (test "(cl:no-such-sym)")))
    (assert (eq :failed (test "...")))))

(defun cmacro-signals-error () :fun)
(define-compiler-macro cmacro-signals-error () (error "oops"))

(with-test (:name :cmacro-signals-error)
  (multiple-value-bind (fun failure-p warnings)
      (checked-compile `(lambda () (cmacro-signals-error))
                       :allow-failure t :allow-warnings t)
    (assert (and fun failure-p warnings))
    (assert (eq :fun (funcall fun)))))

(defun cmacro-with-simple-key (&key a)
  (format nil "fun=~A" a))
(define-compiler-macro cmacro-with-simple-key (&whole form &key a)
  (if (constantp a)
      (format nil "cmacro=~A" (eval a))
      form))

(with-test (:name (:cmacro-with-simple-key :no-key))
  (checked-compile-and-assert ()
      `(lambda () (cmacro-with-simple-key))
    (() "cmacro=NIL")))

(with-test (:name (:cmacro-with-simple-key :constant-key))
  (checked-compile-and-assert ()
      `(lambda () (cmacro-with-simple-key :a 42))
    (() "cmacro=42")))

(with-test (:name (:cmacro-with-simple-key :variable-key))
  (checked-compile-and-assert ()
      `(lambda (x) (cmacro-with-simple-key x 42))
    ((:a) "fun=42")))

(defun cmacro-with-nasty-key (&key ((nasty-key var)))
  (format nil "fun=~A" var))
(define-compiler-macro cmacro-with-nasty-key (&whole form &key ((nasty-key var)))
  (if (constantp var)
      (format nil "cmacro=~A" (eval var))
      form))

(with-test (:name (:cmacro-with-nasty-key :no-key))
  (checked-compile-and-assert ()
      `(lambda () (cmacro-with-nasty-key))
    (() "cmacro=NIL")))

(with-test (:name (:cmacro-with-nasty-key :constant-key))
  ;; This bogosity is thanks to cmacro lambda lists being /macro/ lambda
  ;; lists.
  (checked-compile-and-assert ()
      `(lambda () (cmacro-with-nasty-key 'nasty-key 42))
    (() "fun=42")))

(with-test (:name (:cmacro-with-nasty-key :variable-key))
  (checked-compile-and-assert ()
      `(lambda (nasty-key) (cmacro-with-nasty-key nasty-key 42))
    (('nasty-key) "fun=42")))

(defconstant tricky-key 'tricky-key)
(defun cmacro-with-tricky-key (&key ((tricky-key var)))
  (format nil "fun=~A" var))
(define-compiler-macro cmacro-with-tricky-key (&whole form &key ((tricky-key var)))
  (if (constantp var)
      (format nil "cmacro=~A" (eval var))
      form))

(with-test (:name (:cmacro-with-tricky-key :no-key))
  (let ((fun (checked-compile
              `(lambda () (cmacro-with-tricky-key)))))
    (assert (string= "cmacro=NIL" (funcall fun)))))

(with-test (:name (:cmacro-with-tricky-key :constant-quoted-key))
  ;; This bogosity is thanks to cmacro lambda lists being /macro/ lambda
  ;; lists.
  (let ((fun (checked-compile
              `(lambda () (cmacro-with-tricky-key 'tricky-key 42)))))
    (assert (string= "fun=42" (funcall fun)))))

(with-test (:name (:cmacro-with-tricky-key :constant-unquoted-key))
  (let ((fun (checked-compile
              `(lambda () (cmacro-with-tricky-key tricky-key 42)))))
    (assert (string= "cmacro=42" (funcall fun)))))

(with-test (:name (:cmacro-with-tricky-key :variable-key))
  (let ((fun (checked-compile
              `(lambda (x) (cmacro-with-tricky-key x 42)))))
    (assert (string= "fun=42" (funcall fun 'tricky-key)))))

(defun test-function-983 (x) x)
(define-compiler-macro test-function-983 (x) x)

(with-test (:name :funcall-compiler-macro)
  (assert
   (handler-case
       (and (compile nil
                     `(lambda ()
                        (funcall (function test-function-983 junk) 1)))
            nil)
     (sb-c:compiler-error () t))))

(defsetf test-984 %test-984)

(with-test (:name :setf-function-with-setf-expander)
  (assert
   (handler-case
       (and (defun (setf test-984) ())
            nil)
     (style-warning () t)))
  (checked-compile `(lambda () #'(setf test-984))
                   :allow-style-warnings t))

(with-test (:name :compile-setf-function)
  (defun (setf compile-setf) ())
  (assert (equal (compile '(setf compile-setf))
                 '(setf compile-setf))))

(declaim (inline cut-test))
(defun cut-test (b)
  (cond ((integerp b) b)
        (b 469)
        (t 2)))

(with-test (:name :cut-to-width-bad-constant)
  (assert (= (funcall (checked-compile
                       `(lambda ()
                          (multiple-value-bind (a b) (values t t)
                            (declare (ignore b))
                            (mask-field (byte 10 0) (cut-test a))))))
             469)))

;; META: there's a test in compiler.pure.lisp that also tests
;; interaction of PROGV with (debug 3). These tests should be together.
(with-test (:name :progv-debug-3)
  (unwind-protect
       (sb-ext:restrict-compiler-policy 'debug 3)
    (assert (funcall (checked-compile
                      `(lambda (*v*)
                         (declare (special *v*))
                         (progv '(*v*) '())
                         (boundp '*v*)))
                     1))
    (sb-ext:restrict-compiler-policy 'debug 0)))

(with-test (:name :restrict-compiler-policy-result)
  (let ((sb-c::*policy-min* sb-c::*policy-min*)
        (sb-c::*policy-max* sb-c::*policy-max*))
    (sb-ext:restrict-compiler-policy 'safety 2)
    (checked-compile '(lambda () (declare (optimize (safety 0))))))
  (let ((sb-c::*policy-min* sb-c::*policy-min*)
        (sb-c::*policy-max* sb-c::*policy-max*))
    ;; Passing no arguments returns the current quality/value pairs.
    (assert (null (sb-ext:restrict-compiler-policy)))
    (let ((res (sb-ext:restrict-compiler-policy 'safety 2)))
      ;; returns an alist
      (assert (equal res '((safety . 2)))))
    (let ((res (sb-ext:restrict-compiler-policy 'debug 3)))
      ;; returns an alist, indeterminate order
      (assert (or (equal res '((safety . 2) (debug . 3)))
                  (equal res '((debug . 3) (safety . 2))))))
    ;; remove the SAFETY restriction
    (let ((res (sb-ext:restrict-compiler-policy 'safety 0)))
      (assert (equal res '((debug . 3)))))
    ;; remove the DEBUG restriction
    (let ((res (sb-ext:restrict-compiler-policy 'debug 0)))
      (assert (null res)))))

;;;; tests not in the problem domain, but of the consistency of the
;;;; compiler machinery itself

(in-package "SB-C")

;;; Hunt for wrong-looking things in fundamental compiler definitions,
;;; and gripe about them.
;;;
;;; FIXME: It should be possible to (1) repair the things that this
;;; code gripes about, and then (2) make the code signal errors
;;; instead of just printing complaints to standard output, in order
;;; to prevent the code from later falling back into disrepair.
(defun grovel-results (function)
  (dolist (template (fun-info-templates (info :function :info function)))
    (when (template-more-results-type template)
      (format t "~&Template ~A has :MORE results, and translates ~A.~%"
              (template-name template)
              function)
      (return nil))
    (when (eq (template-result-types template) :conditional)
      ;; dunno.
      (return t))
    (let ((types (template-result-types template))
          (result-type (fun-type-returns (global-ftype function))))
      (cond
        ((values-type-p result-type)
         (do ((ltypes (append (args-type-required result-type)
                              (args-type-optional result-type))
                      (rest ltypes))
              (types types (rest types)))
             ((null ltypes)
              (unless (null types)
                #+nil
                (format t "~&More types than ltypes in ~A, translating ~A.~%"
                        (template-name template)
                        function)
                (return nil)))
           (when (null types)
             (unless (null ltypes)
               #+nil
               (format t "~&More ltypes than types in ~A, translating ~A.~%"
                       (template-name template)
                       function)
               (return nil)))))
        ((eq result-type (specifier-type nil))
         (unless (null types)
           (format t "~&Template ~A returns values for function ~A with RESULT-TYPE NIL.~%"
                   (template-name template)
                   function)
           (return nil)))
        ((/= (length types) 1)
         (format t "~&Template ~A isn't returning 1 value for ~A.~%"
                 (template-name template)
                 function)
         (return nil))
        (t t)))))
(test-util:with-test (:name :identify-suspect-vops)
  (sb-int:call-with-each-globaldb-name
   (lambda (name)
     ;; LEGAL-FUN-NAME-P test is necessary, since (INFO :FUNCTION :TYPE)
     ;; has a defaulting expression that involves calling FDEFINITION.
     (when (and (legal-fun-name-p name) (global-ftype name))
       ;; OK, so we have an entry in the INFO database. Now, if ...
       (binding* ((info (info :function :info name) :exit-if-null)
                  (nil (fun-info-templates info) :exit-if-null))
         ;; ... it has translators
         (grovel-results name))))))

;;;; bug 305: INLINE/NOTINLINE causing local ftype to be lost

(test-util:with-test (:name (compile inline notinline))
  (labels ((compile-lambda (type sense allow-notes)
             (nth-value
              4 (test-util:checked-compile
                 `(lambda ()
                    (declare
                     ,@(when type '((ftype (function () (integer 0 10)) bug-305)))
                     (,sense bug-305)
                     (optimize speed))
                    (1+ (bug-305)))
                 :allow-style-warnings t :allow-notes allow-notes))))
    (assert (compile-lambda nil 'inline t))
    (assert (compile-lambda nil 'notinline t))
    (compile-lambda t 'inline nil)
    (compile-lambda t 'notinline nil)))

;;; bug 211e: bogus style warning from duplicated keyword argument to
;;; a local function.
(test-util:with-test (:name (compile flet :duplicate &key :bug-211e))
  (let ((f (test-util:checked-compile
            '(lambda ()
              (flet ((foo (&key y) (list y)))
                (list (foo :y 1 :y 2)))))))
    (assert (equal '((1)) (funcall f)))))

;;; check that EQL is optimized when other argument is (OR SYMBOL FIXNUM).
(test-util:with-test (:name (compile :optimize eql))
  (let ((f1 (test-util:checked-compile
             '(lambda (x1 y1)
               (declare (type (or symbol fixnum) x1)
                (optimize speed))
               (eql x1 y1))
             :allow-notes nil))
        (f2 (test-util:checked-compile
             '(lambda (x2 y2)
               (declare (type (or symbol fixnum) y2)
                (optimize speed))
               (eql x2 y2))
             :allow-notes nil)))
    (let ((fix (random most-positive-fixnum))
          (sym (gensym))
          (e-count 0))
      (assert (funcall f1 fix fix))
      (assert (funcall f2 fix fix))
      (assert (funcall f1 sym sym))
      (assert (funcall f2 sym sym))
      (handler-bind ((type-error (lambda (c)
                                   (incf e-count)
                                   (continue c))))
        (flet ((test (f x y)
                 (with-simple-restart (continue "continue with next test")
                   (funcall f x y)
                   (error "fell through with (~S ~S ~S)" f x y))))
          (test f1 "oops" 42)
          (test f1 (1+ most-positive-fixnum) 42)
          (test f2 42 "oops")
          (test f2 42 (1+ most-positive-fixnum))))
      (assert (= e-count 4)))))

;;; bug #389 (Rick Taube sbcl-devel)
(declaim (ftype function bes-j0 bes-j1))
(defun bes-jn (unn ux)
   (let ((nn unn) (x ux))
     (let* ((n (floor (abs nn)))
            (besn
             (if (= n 0)
                 (bes-j0 x)
                 (if (= n 1)
                     (bes-j1 x)
                     (if (zerop x)
                         0.0
                         (let ((iacc 40)
                               (ans 0.0)
                               (bigno 1.0e+10)
                               (bigni 1.0e-10))
                           (if (> (abs x) n)
                               (do ((tox (/ 2.0 (abs x)))
                                    (bjm (bes-j0 (abs x)))
                                    (bj (bes-j1 (abs x)))
                                    (j 1 (+ j 1))
                                    (bjp 0.0))
                                   ((= j n) (setf ans bj))
                                 (setf bjp (- (* j tox bj) bjm))
                                 (setf bjm bj)
                                 (setf bj bjp))
                               (let ((tox (/ 2.0 (abs x)))
                                     (m
                                      (* 2
                                         (floor
                                          (/ (+ n (sqrt (* iacc n)))
                                             2))))
                                     (jsum 0.0)
                                     (bjm 0.0)
                                     (sum 0.0)
                                     (bjp 0.0)
                                     (bj 1.0))
                                 (do ((j m (- j 1)))
                                     ((= j 0))
                                   (setf bjm (- (* j tox bj) bjp))
                                   (setf bjp bj)
                                   (setf bj bjm)
                                   (when (> (abs bj) bigno)
                                     (setf bj (* bj bigni))
                                     (setf bjp (* bjp bigni))
                                     (setf ans (* ans bigni))
                                     (setf sum (* sum bigni)))
                                   (if (not (= 0 jsum)) (incf sum bj))
                                   (setf jsum (- 1 jsum))
                                   (if (= j n) (setf ans bjp)))
                                 (setf sum (- (* 2.0 sum) bj))
                                 (setf ans (/ ans sum))))
                           (if (and (minusp x) (oddp n))
                               (- ans)
                               ans)))))))
       (if (and (minusp nn) (oddp nn)) (- besn) besn))))


;;; bug 233b: lvar lambda-var equality in constraint propagation

;; Put this in a separate function.
(defun test-constraint-propagation/ref ()
  (let ((x nil))
    (if (multiple-value-prog1 x (setq x t))
        1
        x)))

(test-util:with-test (:name (:compiler :constraint-propagation :ref))
  (assert (eq t (test-constraint-propagation/ref))))

;; Put this in a separate function.
(defun test-constraint-propagation/typep (x y)
  (if (typep (multiple-value-prog1 x (setq x y))
             'double-float)
      (+ x 1d0)
      (+ x 2)))

(test-util:with-test (:name (:compiler :constraint-propagation :typep))
  (assert (= 6.0d0 (test-constraint-propagation/typep 1d0 5))))

(test-util:with-test (:name (:compiler :constraint-propagation :eq/eql))
  (assert (eq :right (let ((c :wrong))
                       (if (eq (let ((x c))
                                 (setq c :right)
                                 x)
                               :wrong)
                           c
                           0)))))

;;; Put this in a separate function.
(defun test-constraint-propagation/cast (x)
  (when (the double-float (multiple-value-prog1
                              x
                            (setq x (1+ x))))
    x))

(test-util:with-test (:name (:compiler :constraint-propagation :cast))
  (assertoid:assert-error
   (test-constraint-propagation/cast 1) type-error))

;;; bug #399
(let ((result (make-array 50000 :fill-pointer 0 :adjustable t)))
  (defun string->html (string &optional (max-length nil))
    (when (and (numberp max-length)
               (> max-length (array-dimension result 0)))
      (setf result (make-array max-length :fill-pointer 0 :adjustable t)))
    (let ((index 0)
          (left-quote? t))
      (labels ((add-char (it)
                 (setf (aref result index) it)
                 (incf index))
               (add-string (it)
                 (loop for ch across it do
                       (add-char ch))))
        (loop for char across string do
              (cond ((char= char #\<)
                     (add-string "&lt;"))
                    ((char= char #\>)
                     (add-string "&gt;"))
                    ((char= char #\&)
                     (add-string "&amp;"))
                    ((char= char #\')
                     (add-string "&#39;"))
                    ((char= char #\newline)
                     (add-string "<br>"))
                    ((char= char #\")
                     (if left-quote? (add-string "&#147;") (add-string "&#148;"))
                     (setf left-quote? (not left-quote?)))
                    (t
                     (add-char char))))
        (setf (fill-pointer result) index)
        (coerce result 'string)))))

;;; Calling thru constant symbols

(declaim (inline target-fun))
(defun target-fun (arg0 arg1)
  (+ arg0 arg1))
(declaim (notinline target-fun))

;; That issue aside, neither sb-introspect nor ctu:find-named-callees
;; can examine an interpreted function for its callees,
;; so we can't actually use this function.
(defun test-target-fun-called (fun res)
  (assert (member #'target-fun
                  (ctu:find-named-callees #'caller-fun-1)))
  (assert (equal (funcall fun) res)))

(defun caller-fun-1 ()
  (funcall 'target-fun 1 2))
#-interpreter(test-target-fun-called #'caller-fun-1 3)

(defun caller-fun-2 ()
  (declare (inline target-fun))
  (apply 'target-fun 1 '(3)))
#-interpreter(test-target-fun-called #'caller-fun-2 4)

(defun caller-fun-3 ()
  (flet ((target-fun (a b)
           (- a b)))
    (list (funcall #'target-fun 1 4) (funcall 'target-fun 1 4))))
#-interpreter(test-target-fun-called #'caller-fun-3 (list -3 5))

;;; Reported by NIIMI Satoshi
;;; Subject: [Sbcl-devel] compilation error with optimization
;;; Date: Sun, 09 Apr 2006 17:36:05 +0900
(defun test-minimal-debug-info-for-unstored-but-used-parameter (n a)
  (declare (optimize (speed 3)
                     (debug 1)))
  (if (= n 0)
      0
      (test-minimal-debug-info-for-unstored-but-used-parameter (1- n) a)))

;;; &KEY arguments with non-constant defaults.
(defstruct tricky-defaults
  (fun #'identity :type function)
  (num (test-util:opaque-identity 3) :type fixnum))

(test-util:with-test (:name :defstruct-tricky-defaults)
  (macrolet ((frob (form expected-expected-type)
               `(handler-case ,form
                  (type-error (c) (assert (eq (type-error-expected-type c)
                                              ',expected-expected-type)))
                  (:no-error (&rest vals) (error "~S returned values: ~S" ',form vals)))))
    (frob (make-tricky-defaults :fun 3) function)
    (frob (make-tricky-defaults :num #'identity) fixnum)))

(test-util:with-test (:name (compile &key :non-constant :default))
  (let ((fun (test-util:checked-compile
              '(lambda (&key (key (test-util:opaque-identity 3)))
                 (declare (optimize safety) (type integer key))
                 key))))
    (assert (= (funcall fun) 3))
    (assert (= (funcall fun :key 17) 17))
    (handler-case (funcall fun :key t)
      (type-error (c) (assert (eq (type-error-expected-type c) 'integer)))
      (:no-error (&rest vals) (declare (ignore vals)) (error "no error")))))

;;; Basic compiler-macro expansion
(define-compiler-macro test-cmacro-0 () ''expanded)

;; The interpreter is not required to expand compiler-macros.
;; (Actually neither is the compiler!)
#-interpreter(assert (eq 'expanded (funcall (lambda () (test-cmacro-0)))))

;;; FUNCALL forms in compiler macros, lambda-list parsing
(define-compiler-macro test-cmacro-1
    (&whole whole a (a2) &optional b &rest c &key d)
  (list whole a a2 b c d))

(macrolet ((test (form a a2 b c d)
             `(let ((form ',form))
                (destructuring-bind (whole a a2 b c d)
                    (funcall (compiler-macro-function 'test-cmacro-1) form nil)
                  (assert (equal whole form))
                  (assert (eql a ,a))
                  (assert (eql a2 ,a2))
                  (assert (eql b ,b))
                  (assert (equal c ,c))
                  (assert (eql d ,d))))) )
  (test (funcall 'test-cmacro-1 1 (x) 2 :d 3) 1 'x 2 '(:d 3) 3)
  (test (test-cmacro-1 11 (y) 12 :d 13) 11 'y 12 '(:d 13) 13))

;;; FUNCALL forms in compiler macros, expansions
(define-compiler-macro test-cmacro-2 () ''ok)

#-interpreter(assert (eq 'ok (funcall (lambda () (funcall 'test-cmacro-2)))))
#-interpreter(assert (eq 'ok (funcall (lambda () (funcall #'test-cmacro-2)))))

;;; Shadowing of compiler-macros by local functions
(define-compiler-macro test-cmacro-3 () ''global)

(defmacro find-cmacro-3 (&environment env)
  (compiler-macro-function 'test-cmacro-3 env))

(assert (funcall (lambda () (find-cmacro-3))))
(assert (not (funcall (lambda () (flet ((test-cmacro-3 ()))
                                   (find-cmacro-3))))))
(assert (eq 'local (funcall (lambda () (flet ((test-cmacro-3 () 'local))
                                         (test-cmacro-3))))))
(assert (eq 'local (funcall (lambda () (flet ((test-cmacro-3 () 'local))
                                         (funcall #'test-cmacro-3))))))
#-interpreter
(assert (eq 'global (funcall (lambda () (flet ((test-cmacro-3 () 'local))
                                          (funcall 'test-cmacro-3))))))

;;; Local NOTINLINE & INLINE
(defun test-cmacro-4 () 'fun)
(define-compiler-macro test-cmacro-4 () ''macro)

(assert (eq 'fun (funcall (lambda ()
                            (declare (notinline test-cmacro-4))
                            (test-cmacro-4)))))

#-interpreter
(assert (eq 'macro (funcall (lambda ()
                              (declare (inline test-cmacro-4))
                              (test-cmacro-4)))))

;;; SETF function compiler macros
(define-compiler-macro (setf test-cmacro-4) (&whole form value) ''ok)

#-interpreter
(assert (eq 'ok (funcall (lambda () (setf (test-cmacro-4) 'zot)))))
#-interpreter
(assert (eq 'ok (funcall (lambda () (funcall #'(setf test-cmacro-4) 'zot)))))

;;; Step instrumentation breaking type-inference
(test-util:with-test (:name (compile :insert-step-conditions :type-inference))
  (assert (= 42 (funcall (test-util:checked-compile
                          '(lambda (v x)
                            (declare (optimize sb-c:insert-step-conditions))
                            (if (typep (the function x) 'fixnum)
                                (svref v (the function x))
                                (funcall x))))
                         nil (constantly 42)))))

;;; bug 368: array type intersections in the compiler
(defstruct e368)
(defstruct i368)
(defstruct g368
  (i368s (make-array 0 :fill-pointer t) :type (or (vector i368) null)))
(defstruct s368
  (g368 (error "missing :G368") :type g368 :read-only t))
(declaim (ftype (function (fixnum (vector i368) e368) t) r368))
(declaim (ftype (function (fixnum (vector e368)) t) h368))
(defparameter *h368-was-called-p* nil)
(defun nsu (vertices e368)
  (let ((i368s (g368-i368s (make-g368))))
    (let ((fuis (r368 0 i368s e368)))
      (format t "~&FUIS=~S~%" fuis)
      (or fuis (h368 0 i368s)))))
(defun r368 (w x y)
  (declare (ignore w x y))
  nil)
(defun h368 (w x)
  (declare (ignore w x))
  (setf *h368-was-called-p* t)
  (make-s368 :g368 (make-g368)))
(let ((nsu (nsu #() (make-e368))))
  (format t "~&NSU returned ~S~%" nsu)
  (format t "~&*H368-WAS-CALLED-P*=~S~%" *h368-was-called-p*)
  (assert (s368-p nsu))
  (assert *h368-was-called-p*))

;;; bug 367: array type intersections in the compiler
(defstruct e367)
(defstruct i367)
(defstruct g367
  (i367s (make-array 0 :fill-pointer t) :type (or (vector i367) null)))
(defstruct s367
  (g367 (error "missing :G367") :type g367 :read-only t))
(declaim (ftype (function ((vector i367) e367) (or s367 null)) r367))
(declaim (ftype (function ((vector e367)) (values)) h367))
(defun frob-367 (v w)
  (let ((x (g367-i367s (make-g367))))
    (let* ((y (or (r367 x w)
                  (h367 x)))
           (z (s367-g367 y)))
      (format t "~&Y=~S Z=~S~%" y z)
      (g367-i367s z))))
(defun r367 (x y) (declare (ignore x y)) nil)
(defun h367 (x) (declare (ignore x)) (values))
(assertoid:assert-error (frob-367 0 (make-e367)) type-error)

(handler-case
    (delete-file (compile-file "circ-tree-test.lisp"
                               :output-file (test-util:scratch-file-name)))
  (storage-condition (e)
    (error e)))

;;; warnings due to step-instrumentation
(defclass debug-test-class () ())
(test-util:checked-compile
 '(lambda ()
   (declare (optimize (debug 3)))
   (defmethod print-object ((x debug-test-class) s)
     (call-next-method))))

;;; program-error from bad lambda-list keyword
(test-util:with-test (:name (compile :bad lambda-list keyword program-error))
  (multiple-value-bind (fun failure-p)
      (test-util:checked-compile '(lambda (&whole x)
                                    (list &whole x))
                                 :allow-failure t)
    (assert failure-p)
    (assertoid:assert-error (funcall fun) program-error)))

(test-util:with-test (:name (eval :bad lambda-list keyword program-error)
                      :skipped-on (not :sb-eval))
  (assertoid:assert-error (let ((*evaluator-mode* :interpret))
                            (funcall (eval '(lambda (&whole x)
                                             (list &whole x)))))
                          program-error))

;;; ignore &environment
(test-util:with-test (:name (defmacro ignore &environment))
  (test-util:checked-compile
   '(lambda ()
     (defmacro macro-ignore-env (&environment env)
       (declare (ignore env))
       :foo))))

(test-util:with-test (:name (defmacro :no &environment))
  (test-util:checked-compile
   '(lambda ()
     (defmacro macro-no-env ()
       :foo))))

(test-util:with-test (:name (disassemble :source-form))
  (dolist (*evaluator-mode* '(#+sb-eval :interpret :compile))
    (disassemble (eval '(defun disassemble-source-form-bug (x y z)
                         (declare (optimize debug))
                         (list x y z)))
                 :stream (make-broadcast-stream))))

;;; long-standing bug in defaulting unknown values on the x86-64,
;;; since changing the calling convention (test case by Christopher
;;; Laux sbcl-help 30-06-2007)

(defun default-values-bug-demo-sub ()
  (format t "test")
  nil)
(compile 'default-values-bug-demo-sub)

(defun default-values-bug-demo-main ()
  (multiple-value-bind (a b c d e f g h)
      (default-values-bug-demo-sub)
    (if a (+ a b c d e f g h) t)))
(compile 'default-values-bug-demo-main)

(test-util:with-test (:name (multiple-value-bind :default-values))
  (assert (default-values-bug-demo-main)))

;;; copy propagation bug reported by Paul Khuong

(defun local-copy-prop-bug-with-move-arg (x)
  (labels ((inner ()
             (values 1 0)))
    (if x
        (inner)
        (multiple-value-bind (a b)
            (inner)
          (values b a)))))

(test-util:with-test  (:name (labels multiple-value-bind :copy-propagation))
  (assert (equal '(0 1) (multiple-value-list
                         (local-copy-prop-bug-with-move-arg nil))))
  (assert (equal '(1 0) (multiple-value-list
                         (local-copy-prop-bug-with-move-arg t)))))

;;;; with-pinned-objects & unwind-protect, using all non-tail conventions

(defun wpo-quux () (list 1 2 3))
(defvar *wpo-quux* #'wpo-quux)

(defun wpo-call ()
  (unwind-protect
       (sb-sys:with-pinned-objects (*wpo-quux*)
         (values (funcall *wpo-quux*)))))
(assert (equal '(1 2 3) (wpo-call)))

(defun wpo-multiple-call ()
  (unwind-protect
       (sb-sys:with-pinned-objects (*wpo-quux*)
         (funcall *wpo-quux*))))
(assert (equal '(1 2 3) (wpo-multiple-call)))

(defun wpo-call-named ()
  (unwind-protect
       (sb-sys:with-pinned-objects (*wpo-quux*)
         (values (wpo-quux)))))
(assert (equal '(1 2 3) (wpo-call-named)))

(defun wpo-multiple-call-named ()
  (unwind-protect
       (sb-sys:with-pinned-objects (*wpo-quux*)
         (wpo-quux))))
(assert (equal '(1 2 3) (wpo-multiple-call-named)))

(defun wpo-call-variable (&rest args)
  (unwind-protect
       (sb-sys:with-pinned-objects (*wpo-quux*)
         (values (apply *wpo-quux* args)))))
(assert (equal '(1 2 3) (wpo-call-variable)))

(defun wpo-multiple-call-variable (&rest args)
  (unwind-protect
       (sb-sys:with-pinned-objects (*wpo-quux*)
         (apply #'wpo-quux args))))
(assert (equal '(1 2 3) (wpo-multiple-call-named)))

(defun wpo-multiple-call-local ()
  (flet ((quux ()
           (wpo-quux)))
    (unwind-protect
         (sb-sys:with-pinned-objects (*wpo-quux*)
           (quux)))))
(assert (equal '(1 2 3) (wpo-multiple-call-local)))

;;; bug 417: toplevel NIL confusing source path logic
(test-util:with-test (:name :bug-417)
  (handler-case
      (delete-file (let ((*error-output* (make-broadcast-stream)))
                     (compile-file "bug-417.lisp"
                                   :output-file (test-util:scratch-file-name)
                                   :verbose nil :print nil)))
    (sb-ext:code-deletion-note (e)
      (error e))))

;;; unknown values return convention getting disproportionate
;;; amounts of values.
(declaim (notinline one-value two-values))
(defun one-value (x)
  (not x))
(defun two-values (x y)
  (values y x))
(defun wants-many-values (x y)
  (multiple-value-bind (a b c d e f)
      (one-value y)
    (assert (and (eql (not y) a)
                 (not (or b c d e f)))))
  (multiple-value-bind (a b c d e f)
      (two-values y x)
    (assert (and (eql a x) (eql b y)
                 (not (or c d e f)))))
  (multiple-value-bind (a b c d e f g h i)
      (one-value y)
    (assert (and (eql (not y) a)
                 (not (or b c d e f g h i)))))
  (multiple-value-bind (a b c d e f g h i)
      (two-values y x)
    (assert (and (eql a x) (eql b y)
                 (not (or c d e f g h i)))))
  (multiple-value-bind (a b c d e f g h i j k l m n o p q r s)
      (one-value y)
    (assert (and (eql (not y) a)
                 (not (or b c d e f g h i j k l m n o p q r s)))))
  (multiple-value-bind (a b c d e f g h i j k l m n o p q r s)
      (two-values y x)
    (assert (and (eql a x) (eql b y)
                 (not (or c d e f g h i j k l m n o p q r s))))))
(test-util:with-test (:name (multiple-value-bind :unknown values))
  (wants-many-values 1 42))

;;; constant coalescing

(defun count-code-constants (x f)
  (let ((code (sb-kernel:fun-code-header f))
        (n 0))
    (loop for i from sb-vm:code-constants-offset below (sb-kernel:code-header-words code)
          do (when (equal x (sb-kernel:code-header-ref code i))
               (incf n)))
    n))

(defvar *lambda*)

(defun compile2 (lambda)
  (let* ((lisp (test-util:scratch-file-name "lisp"))
         (fasl (test-util:scratch-file-name "fasl")))
    (unwind-protect
         (progn
           (with-open-file (f lisp :direction :output)
             (prin1 `(setf *lambda* ,lambda) f))
           (multiple-value-bind (fasl warn fail)
               (compile-file lisp :output-file fasl :verbose nil :print nil)
             (declare (ignore warn))
             (when fail
               (error "File-compiling ~S failed." lambda))
             (let ((*lambda* nil))
               (load fasl)
               (values *lambda* (test-util:checked-compile lambda)))))
      (ignore-errors (delete-file lisp))
      (ignore-errors (delete-file fasl)))))

;; named and unnamed
(defconstant +born-to-coalesce+ '.born-to-coalesce.)
(test-util:with-test (:name (compile compile-file :coalescing symbol))
  (multiple-value-bind (file-fun core-fun)
      (compile2 '(lambda ()
                  (let ((x (cons +born-to-coalesce+ nil))
                        (y (cons '.born-to-coalesce. nil)))
                    (list x y))))
    (assert (= 1 (count-code-constants '.born-to-coalesce. file-fun)))
    (assert (= 1 (count-code-constants '.born-to-coalesce. core-fun)))))

;; some things must retain identity under COMPILE, but we want to coalesce them under COMPILE-FILE
(defun assert-coalescing (constant)
  (let ((value (copy-seq (symbol-value constant))))
    (multiple-value-bind (file-fun core-fun)
        (compile2 `(lambda ()
                     (let ((x (cons ,constant nil))
                           (y (cons ',value nil)))
                       (list x y))))
      (assert (= 1 (count-code-constants value file-fun)))
      (assert (= 2 (count-code-constants value core-fun)))
      (let* ((l (funcall file-fun))
             (a (car (first l)))
             (b (car (second l))))
        (assert (and (equal value a)
                     (equal a b)
                     (eq a b))))
      (let* ((l (funcall core-fun))
             (a (car (first l)))
             (b (car (second l))))
        (assert (and (equal value a)
                     (equal a b)
                     (not (eq a b))))))))

(defconstant +born-to-coalesce2+ "maybe coalesce me!")
(test-util:with-test (:name (compile compile-file :coalescing string))
  (assert-coalescing '+born-to-coalesce2+))

(defconstant +born-to-coalesce3+ #*01101001011101110100011)
(test-util:with-test (:name (compile compile-file :coalescing bit-vector))
  (assert-coalescing '+born-to-coalesce3+))

(defconstant +born-to-coalesce4+ '(foo bar "zot" 123 (nested "quux") #*0101110010))
(test-util:with-test (:name (compile compile-file :coalescing :mixed))
  (assert-coalescing '+born-to-coalesce4+))

(defclass some-constant-thing () ())

;;; correct handling of nested things loaded via SYMBOL-VALUE
(defvar *sneaky-nested-thing* (list (make-instance 'some-constant-thing)))
(defconstant +sneaky-nested-thing+ *sneaky-nested-thing*)
(multiple-value-bind (file-fun core-fun) (compile2 '(lambda () +sneaky-nested-thing+))
  (assert (equal *sneaky-nested-thing* (funcall file-fun)))
  (assert (equal *sneaky-nested-thing* (funcall core-fun))))

;;; catch constant modifications thru undefined variables
(defun sneak-set-dont-set-me (x)
  (ignore-errors (setq dont-set-me x)))
(defconstant dont-set-me 42)
(assert (not (sneak-set-dont-set-me 13)))
(assert (= 42 dont-set-me))
(defun sneak-set-dont-set-me2 (x)
  (ignore-errors (setq dont-set-me2 x)))
(defconstant dont-set-me2 (make-instance 'some-constant-thing))
(assert (not (sneak-set-dont-set-me2 13)))
(assert (typep dont-set-me2 'some-constant-thing))

;;; check that non-trivial constants are EQ across different files: this is
;;; not something ANSI either guarantees or requires, but we want to do it
;;; anyways.
(when (find-symbol "SORT-INLINE-CONSTANTS" "SB-VM")
  (push :inline-constants *features*))
(defconstant +share-me-1+ #-inline-constants 123.456d0 #+inline-constants nil)
(defconstant +share-me-2+ "a string to share")
(defconstant +share-me-3+ (vector 1 2 3))
(defconstant +share-me-4+ (* 2 most-positive-fixnum))
(multiple-value-bind (f1 c1) (compile2 '(lambda () (values +share-me-1+
                                                           +share-me-2+
                                                           +share-me-3+
                                                           +share-me-4+
                                                           #-inline-constants pi)))
  (multiple-value-bind (f2 c2) (compile2 '(lambda () (values +share-me-1+
                                                             +share-me-2+
                                                             +share-me-3+
                                                             +share-me-4+
                                                             #-inline-constants pi)))
    (flet ((test (fa fb)
             (mapc (lambda (a b)
                     (assert (eq a b)))
                   (multiple-value-list (funcall fa))
                   (multiple-value-list (funcall fb)))))
      (test f1 c1)
      (test f1 f2)
      (test f1 c2))))

;;; user-defined satisfies-types cannot be folded
(deftype mystery () '(satisfies mysteryp))
(defvar *mystery* nil)
(defun mysteryp (x) (eq x *mystery*))
(defstruct thing (slot (error "missing") :type mystery))
(defun test-mystery (m) (when (eq :mystery (thing-slot m)) :ok))
(setf *mystery* :mystery)
(assert (eq :ok (test-mystery (make-thing :slot :mystery))))

;;; Singleton types can also be constant.
(test-util:with-test (:name :propagate-singleton-types-to-eql)
  (macrolet ((test (type value &aux (fun (gensym "FUN")))
               `(progn
                  (declaim (ftype (function () (values ,type &optional)) ,fun))
                  (defun ,fun ()
                    ',value)
                  (lambda (x)
                    (if (eql x (,fun))
                        nil
                        (eql x (,fun)))))))
    (values
      (test (eql foo) foo)
      (test (integer 0 0) 0)
      (test (double-float 0d0 0d0) 0d0)
      (test (eql #\c) #\c))))

(declaim (ftype (function () (integer 42 42)) bug-655581))
(defun bug-655581 ()
  42)
(declaim (notinline bug-655581))
(test-util:with-test (:name :bug-655581)
  (test-util:checked-compile-and-assert ()
      `(lambda ()
         (ctu:compiler-derived-type (bug-655581)))
    (() (values '(integer 42 42) t))))

(test-util:with-test (:name :clear-derived-types-on-set-fdefn)
  (let ((*evaluator-mode* :compile)
        (*derive-function-types* t))
    (eval `(progn
             (defun clear-derived-types-on-set-fdefn-1 ()
               "foo")
             (setf (symbol-function 'clear-derived-types-on-set-fdefn-1)
                   (constantly "foobar"))
             (defun clear-derived-types-on-set-fdefn-2 ()
               (length (clear-derived-types-on-set-fdefn-1)))))
    (assert (= 6 (clear-derived-types-on-set-fdefn-2)))))

(test-util:with-test (:name (:bug-655126 :derive-function-types t))
  (let ((*evaluator-mode* :compile)
        (*derive-function-types* t))
    (eval `(defun bug-655126 (x) x))
    ;; Full warnings are ok due to *derive-function-types* = T.
    (assert (eq :full-warning
                (handler-case
                    (eval `(defun bug-655126-2 ()
                             (bug-655126)))
                  ((and warning (not style-warning)) ()
                    :full-warning))))
    (assert (eq 'bug-655126
                (handler-case
                    (eval `(defun bug-655126 (x y)
                             (cons x y)))
                  ((and warning (not sb-kernel:redefinition-warning)) ()
                    :oops))))
    (assert (eq :full-warning
                (handler-case
                    (eval `(defun bug-655126 (x)
                             (bug-655126 x y)))
                  ((and warning
                    (not style-warning)
                    (not sb-kernel:redefinition-warning)) ()
                    :full-warning))))))

(test-util:with-test (:name (:bug-655126 :derive-function-types nil))
  (let ((*evaluator-mode* :compile))
    (eval `(defun bug-655126/b (x) x))
    ;; Just style-warning here.
    (assert (eq :style-warning
                (handler-case
                    (eval `(defun bug-655126-2/b ()
                             (bug-655126/b)))
                  (style-warning ()
                    :style-warning))))
    (assert (eq 'bug-655126/b
                (handler-case
                    (eval `(defun bug-655126/b (x y)
                             (cons x y)))
                  ((and warning (not sb-kernel:redefinition-warning)) ()
                    :oops))))
    ;; Bogus self-call is always worth a full one.
    (assert (eq :full-warning
                (handler-case
                    (eval `(defun bug-655126/b (x)
                             (bug-655126/b x y)))
                  ((and warning
                    (not style-warning)
                    (not sb-kernel:redefinition-warning)) ()
                    :full-warning))))))

(test-util:with-test (:name :bug-657499)
  ;; Don't trust derived types within the compilation unit.
  (ctu:file-compile
   `((declaim (optimize safety))
     (defun bug-657499-foo ()
       (cons t t))
     (defun bug-657499-bar ()
       (let ((cons (bug-657499-foo)))
         (setf (car cons) 3)
         cons)))
   :load t)
  (locally (declare (optimize safety))
    (setf (symbol-function 'bug-657499-foo) (constantly "foobar"))
    (assert (eq :type-error
                (handler-case
                    (funcall 'bug-657499-bar)
                  (type-error (e)
                    (assert (eq 'cons (type-error-expected-type e)))
                    (assert (equal "foobar" (type-error-datum e)))
                    :type-error))))))

(declaim (unsigned-byte *symbol-value-test-var*))
(defvar *symbol-value-test-var*)

(declaim (unsigned-byte **global-symbol-value-test-var**))
(defglobal **global-symbol-value-test-var** 0)

(test-util:with-test (:name :symbol-value-type-derivation)
  (let ((fun (compile nil `(lambda () *symbol-value-test-var*))))
    (assert (equal '(function () (values unsigned-byte &optional))
                   (%simple-fun-type fun))))
  (let ((fun (compile nil `(lambda () **global-symbol-value-test-var**))))
    (assert (equal '(function () (values unsigned-byte &optional))
                   (%simple-fun-type fun))))
  (let ((ufix (type-specifier (specifier-type `(and unsigned-byte fixnum)))))
    ;; Test a free type decl
    (let ((fun (compile nil `(lambda ()
                               (declare (fixnum *symbol-value-test-var*))
                               (symbol-value '*symbol-value-test-var*)))))
      (assert (equal `(function () (values ,ufix &optional))
                     (%simple-fun-type fun))))
    ;; Test a bound type decl
    (let ((fun (compile nil `(lambda (*symbol-value-test-var*)
                               (declare (fixnum *symbol-value-test-var*))
                               (symbol-value '*symbol-value-test-var*)))))
      (assert (equal `(function (,ufix) (values ,ufix &optional))
                     (%simple-fun-type fun)))))
  (let ((fun (compile
              nil
              `(lambda ()
                 (declare (fixnum **global-symbol-value-test-var**))
                 (symbol-global-value '**global-symbol-value-test-var**))))
        (ufix (type-specifier (specifier-type `(and unsigned-byte fixnum)))))
    (assert (equal `(function () (values ,ufix &optional))
                   (%simple-fun-type fun)))))

(test-util:with-test (:name :mv-bind-to-let-type-propagation)
  (let ((f (test-util:checked-compile
            `(lambda (x)
               (declare (optimize speed)
                        (type (integer 20 50) x))
               (< (truncate x 10) 1))))
        (g (test-util:checked-compile
            `(lambda (x)
               (declare (optimize speed)
                        (type (integer 20 50) x))
               (< (nth-value 1 (truncate x 10)) 10))))
        (h (test-util:checked-compile
            `(lambda (x)
               (declare (optimize speed)
                        (type (integer 20 50) x))
               (multiple-value-bind (q r)
                   (truncate x 10)
                 (declare (ignore r))
                 (< q 1)))))
        (type0 '(function ((integer 20 50)) (values null &optional)))
        (type1 '(function ((integer 20 50)) (values (member t) &optional))))
    (assert (equal type0 (sb-kernel:%simple-fun-type f)))
    (assert (equal type1 (sb-kernel:%simple-fun-type g)))
    (assert (equal type0 (sb-kernel:%simple-fun-type h)))))

;;; I think *CHECK-CONSISTENCY* is confused in the presence of dynamic-extent.
;;; A simpler example not involving package iteration also fails in COMPILE-FILE:
;;;   (let ((l (mapcar #'string *features*))) (defun g () l))
(test-util:with-test (:name :bug-308921 :fails-on :sbcl)
  (let ((*check-consistency* t))
    (ctu:file-compile
     `((let ((exported-symbols-alist
               (loop for symbol being the external-symbols of :cl
                     collect (cons symbol
                                   (concatenate 'string
                                                "#"
                                                (string-downcase symbol))))))
         (defun hyperdoc-lookup (symbol)
           (cdr (assoc symbol exported-symbols-alist)))))
     :load nil)))

(test-util:with-test (:name :bug-308941)
  (multiple-value-bind (warn fail)
      (let ((*check-consistency* t))
        (ctu:file-compile
         "(eval-when (:compile-toplevel :load-toplevel :execute)
            (defstruct foo3))
          (defstruct bar
            (foo #.(make-foo3)))"
         :load nil))
    ;; ...but the compiler should not break.
    (assert (and warn fail))))

(test-util:with-test (:name :bug-903821)
  (let* ((fun (test-util:checked-compile
               '(lambda (x n)
                 (declare (sb-ext:word x)
                  (type (integer 0 #.(1- sb-vm:n-machine-word-bits)) n)
                  (optimize speed))
                 (logandc2 x (ash -1 n)))))
         (thing-not-to-call
          (intern (format nil "ASH-LEFT-MOD~D" sb-vm:n-machine-word-bits) "SB-VM")))
    (assert (not (member (symbol-function thing-not-to-call)
                         (ctu:find-named-callees fun))))
    (assert (= 7 (funcall fun 15 3)))))

(test-util:with-test (:name :bug-997528)
  (let ((fun (test-util:checked-compile
              '(lambda (x)
                (declare (optimize (speed 0) (space 0))
                 (type (integer -228645653448155482 -228645653447928749) x))
                (floor 1.0 (the (integer -228645653448151677 -228645653448150900) x))))))
    (multiple-value-bind (quo rem)
        (funcall fun -228645653448151381)
      (assert (= quo -1))
      (assert (= rem (float -228645653448151381))))))

(defmacro def-many-code-constants ()
  `(defun many-code-constants ()
     ,@(loop for i from 0 below 1000
          collect `(print ,(format nil "hi-~d" i)))))

(test-util:with-test (:name :many-code-constants)
  (def-many-code-constants)
  (assert (search "hi-999"
                  (with-output-to-string (*standard-output*)
                    (many-code-constants)))))

(test-util:with-test (:name :bug-943953)
  ;; we sometimes splice compiler structures like clambda in
  ;; source, and our error reporting would happily use that
  ;; as source forms.
  (let* ((src "bug-943953.lisp")
         (obj (test-util:scratch-file-name "fasl")))
    (unwind-protect (let ((*error-output* (make-broadcast-stream)))
                      (compile-file src
                                    :output-file obj
                                    :verbose nil :print nil))
      (ignore-errors (delete-file obj)))))

(declaim (inline vec-1177703))
(defstruct (vec-1177703 (:constructor vec-1177703 (&optional x)))
  (x 0.0d0 :type double-float))

(declaim (inline norm-1177703))
(defun norm-1177703 (v)
  (vec-1177703 (sqrt (vec-1177703-x v))))

(test-util:with-test (:name :bug-1177703)
  (test-util:checked-compile `(lambda (x)
                                (norm-1177703 (vec-1177703 x)))))

(declaim (inline call-1035721))
(defun call-1035721 (function)
  (lambda (x)
    (funcall function x)))

(declaim (inline identity-1035721))
(defun identity-1035721 (x)
  x)

(test-util:with-test (:name :bug-1035721)
  (test-util:checked-compile
   `(lambda ()
      (list
       (call-1035721 #'identity-1035721)
       (lambda (x)
         (identity-1035721 x))))))

(test-util:with-test (:name :expt-type-derivation-and-method-redefinition)
  (defmethod expt-type-derivation ((x list) &optional (y 0.0))
    (declare (type float y))
    (expt 2 y))
  ;; the redefinition triggers a type lookup of the old
  ;; fast-method-function's type, which had a bogus type specifier of
  ;; the form (double-float 0) from EXPT type derivation
  (defmethod expt-type-derivation ((x list) &optional (y 0.0))
    (declare (type float y))
    (expt 2 y)))

;; Lp# 1066451 - declarations were either misplaced or dropped
;; depending on whether the :policy argument was used in DEFTRANSFORM.
;; That was a bit random. :policy doesn't affect whether decls
;; are accepted now.
(defun foo (blah)
  (declare (special randomness-factor))
  (if (constant-lvar-p randomness-factor)
      (format nil "Weird transform answer is ~D"
              (+ (lvar-value randomness-factor) blah))))
(defknown weird-fn (integer symbol &key (:magic real)) t)
(deftransform weird-fn ((x s &key ((:magic randomness-factor)))
                        (fixnum t &key (:magic fixnum)))
  ;; I can't see much use for declarations other than SPECIAL here,
  ;; but we shouldn't supposedly allow them and then not handle them right.
  (declare (special fred) (special randomness-factor) (lvar x s))
  (foo fred))
(test-util:with-test (:name :deftransform-bug-1066451)
  (let ((f (let ((fred 3))
             (declare (special fred))
             (test-util:checked-compile
              '(lambda () (weird-fn 2 'foo :magic 11))))))
    (assert (string= (funcall f)
                     "Weird transform answer is 14"))))

(test-util:with-test (:name :macro-policy)
  (flet ((count-notes ()
          (let ((count 0))
            (handler-bind ((compiler-note
                            (lambda (c)
                              c
                              (incf count)
                              (muffle-warning))))
              (multiple-value-bind (fasl warnings errors)
                  (compile-file "macro-policy-test.lisp"
                                :output-file (test-util:scratch-file-name)
                                :print nil :verbose nil)
                (ignore-errors (delete-file fasl))
                (assert (and (not warnings) (not errors)))
                count)))))
    (let* ((baseline (count-notes))
           (test (progv '(*frob-macro-policy*) '(t) (count-notes)))
           (baseline-again (count-notes)))
      (assert (/= 0 baseline))
      (assert (= 0 test))
      ;; macro-policy is rebound inside compile-file
      (assert (= baseline-again baseline)))))

(in-package :cl-user)

(with-test (:name :merge-lambdas-dead-return)
  (let ((fasl (let ((*error-output* (make-broadcast-stream)))
                (compile-file "merge-lambdas.lisp"
                              :output-file (scratch-file-name)
                              :print nil :verbose nil))))
    (ignore-errors (delete-file fasl))))

(declaim (inline ensure-a-fun))
(defun ensure-a-fun (f) (coerce f 'function))
(defmacro compose2 (f g)
  `(let ((f (ensure-a-fun ,f))
         (g (ensure-a-fun ,g)))
     (lambda (arg) (funcall f (funcall g arg)))))

(with-test (:name :coerce-to-function-smarter)
  (let ((f (checked-compile
            '(lambda (x)
              (funcall (compose2 #'integerp #'car) x)))))
    ;; should be completely inlined
    (assert (null (ctu:find-named-callees f)))))

(with-test (:name :derived-function-type-casts)
  (let ((fasl (compile-file "derived-function-type-casts.lisp"
                            :output-file (scratch-file-name)
                            :print nil :verbose nil)))
    (load fasl)
    (ignore-errors (delete-file fasl))
    (assert (null (funcall 'derived-function-type-casts)))))

(with-test (:name (compile function :invalid-function-name))
  (flet ((test-case (nameoid)
           (assert (nth-value
                    1 (checked-compile `(lambda () (function ,nameoid))
                                       :allow-failure t)))))
    (test-case 1)
    (test-case "foo")
    (test-case '(foo bar))))

(defun catch-compiled-program-error (form &rest args)
  (multiple-value-bind (function failure-p)
      (checked-compile form :allow-failure t :allow-warnings t)
    (assert failure-p)
    (assert-error (apply function args))))

(with-test (:name :duplicate-&key-no-error)
  (catch-compiled-program-error
   '(lambda () (defun duplicate-&key-no-error (&key a a) a))))

(with-test (:name :bad-type-specifiers)
  (catch-compiled-program-error
   '(lambda (x) (typep x '(values 10)))
   1)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda () (declare (sb-ext:muffle-conditions 10)))
                      :allow-warnings t))))

(with-test (:name :coverage-and-errors)
  (ctu:file-compile
   '((locally (declare (optimize sb-c:store-coverage-data))
       (1)))))

(symbol-macrolet ((x 30))
  (macrolet ((foo (y) (+ x y)))
    (declaim (inline environment-around-inline))
    (defun environment-around-inline (z)
      (* z (foo 4)))))

(with-test (:name :environment-around-inline)
  (defun environment-around-inline.2 (z)
    (environment-around-inline z))
  (assert (= (environment-around-inline.2 10) 340)))

(with-test (:name :defun-setf-return-value)
  (let ((name `(setf ,(gensym))))
    (assert (equal (eval `(defun ,name ()))
                   name))))

(with-test (:name (make-sequence :unknown type))
  (let ((fun (checked-compile
              `(lambda (x)
                 (let ((vector (make-sequence '(simple-array make-sequence-unknown (*)) 10)))
                   (setf (aref vector 0) x)
                   vector))
              :allow-style-warnings t)))
    (deftype make-sequence-unknown () 'fixnum)
    (assert-error (funcall fun 'abc) type-error)))

(with-test (:name (:compiler-messages function :type-specifier))
  ;; Previously, function types were often printed confusingly, e.g.:
  ;;
  ;;   (function ())           => #'NIL
  ;;   (function *)            => #'*
  ;;   (function (function a)) => #'#'A
  ;;
  (flet ((test-case (spec)
           (destructuring-bind (type expected) spec
             (unwind-protect
                  (let ((report))
                    (proclaim '(ftype (function () boolean) my-function))
                    (handler-bind ((warning
                                    (lambda (condition)
                                      (setf report (princ-to-string condition))
                                      (muffle-warning))))
                      (proclaim `(ftype ,type my-function)))
                    (assert (search expected report)))
               (fmakunbound 'my-function)))))
    (mapc
     #'test-case
     `(((function ())                 "(FUNCTION NIL)")
       ((function *)                  "(FUNCTION *)")
       ((function (function t))       "(FUNCTION (FUNCTION T))")
       ((function (function (eql 1))) "(FUNCTION (FUNCTION (EQL 1))")))))

(with-test (:name (:compiler-messages function :lambda-list))
  ;; Previously, function lambda lists were sometimes printed
  ;; confusingly, e.g.:
  ;;
  ;;   (function collection) => #'COLLECTION
  ;;   ((function t))        => (#'T)
  ;;
  (handler-case
      (sb-c::parse-lambda-list '((function t) . a) :context 'defmethod)
    (error (condition)
      (assert (search "illegal dotted lambda list: ((FUNCTION T) . A)"
                      (princ-to-string condition))))))

(with-test (:name :boxed-ref-setf-special
            :skipped-on :interpreter)
  (let* ((var (gensym))
         (fun (checked-compile `(lambda ()
                                  (declare (special ,var))
                                  (setf ,var 10d0)))))
    (ctu:assert-no-consing (funcall fun))))

(with-test (:name :boxed-ref-bind-special
            :skipped-on :interpreter)
  (let* ((var (gensym))
         (fun (checked-compile `(lambda ()
                                  (let ((,var 10d0))
                                    (declare (special ,var)))))))
    (ctu:assert-no-consing (funcall fun))))

(with-test (:name :boxed-ref-svref
            :skipped-on :interpreter)
  (let ((fun (checked-compile `(lambda (x)
                                 (setf (svref x 0) 10d0))))
        (vector (vector nil)))
    (ctu:assert-no-consing (funcall fun vector))))

(with-test (:name :boxed-ref-instance-set
            :skipped-on :interpreter)
  (let* ((name (gensym "STRUCT"))
         (fun (progn
                (eval `(defstruct ,name x))
                (checked-compile `(lambda (x)
                                    (setf (,(sb-int:symbolicate name '-x) x)
                                          10d0)))))
         (instance (funcall (sb-int:symbolicate 'make- name))))
    (ctu:assert-no-consing (funcall fun instance))))

(with-test (:name :boxed-ref-car
            :skipped-on :interpreter)
  (let ((fun (checked-compile `(lambda (x)
                                 (setf (car x) 10d0)
                                 (setf (cdr x) 10d0))))
        (list (list nil)))
    (ctu:assert-no-consing (funcall fun list))))


(with-test (:name :ftype-return-type-conflict
            ;; Not having UNWIND-TO-FRAME-AND-CALL-VOP changes
            ;; the condition type here?
            :fails-on (not :unwind-to-frame-and-call-vop))
  (proclaim '(ftype (function () fixnum) ftype-return-type-conflict))
  (checked-compile-and-assert (:optimize :safe :allow-warnings t)
      `(sb-int:named-lambda ftype-return-type-conflict () nil)
    (() (condition 'type-error))))


(declaim (inline bug-1728074-to-boolean bug-1728074-foo))
(defun bug-1728074-to-boolean (x) (/= x 0))
(defun bug-1728074-foo (storage key converter)
  (labels ((value (entry)
             (funcall converter (ash entry -17)))
           (insert (start key new)
             (when *
               (return-from insert (value (aref storage start))))
             (let ((entry (aref storage start)))
               (let* ((okey (logand entry #xf00))
                      (ostart (logand okey #xf)))
                 (unless (= ostart start)
                   (insert ostart okey entry)))))
           (probe (index)
             (let ((entry (aref storage index)))
               (when (= key (logand #xf00 entry))
                 (value entry)))))
    (declare (inline value))
    (probe (logand key #xf))))

(with-test (:name :defined-fun-in-a-deleted-home-lambda)
  (checked-compile `(lambda (cache key) (bug-1728074-foo cache key #'bug-1728074-to-boolean))))

(defstruct (lambda-var-ref-lvar-null (:type list))
  (x nil)
  (y 0))

(with-test (:name :lambda-var-ref-lvar-null)
  (assert (nth-value 1
                     (checked-compile
                      `(lambda (a)
                         (setf (lambda-var-ref-lvar-null-x :b) nil
                               (lambda-var-ref-lvar-null-y a) 0))
                      :allow-warnings t))))

(sb-c:defknown blah * * (sb-c:foldable))
(defun blah (x) (- x))
(with-test (:name :foldable-wild-args-fun)
  (assert (eql (funcall (checked-compile '(lambda () (blah (+ 1 2))))) -3)))


(declaim (ftype (function * (values cons &optional)) safety-zero-return-checking)
         (inline safety-zero-return-checking))
(defun safety-zero-return-checking (x)
  x)

(with-test (:name :safety-zero-return-checking)
  (assert (nth-value 1 (checked-compile `(lambda ()
                                           (declare (optimize (safety 0)))
                                           (safety-zero-return-checking 1))
                                        :allow-warnings t))))

(with-test (:name :inline-macrolet-debug-0)
  (let* ((fun (gensym "FUN"))
         (fun (eval
               `(locally (declare (optimize (debug 0)))
                  (macrolet ((macro () 10))
                    (proclaim '(inline ,fun))
                    (defun ,fun () (macro)))
                  (lambda () (,fun))))))
    (assert (= (funcall fun) 10))))

(with-test (:name :substitute-lvar-updating-lvar-dependenceis)
  (let ((name (gensym)))
    (proclaim `(ftype (function * (values number &optional))
                      ,name))
    (checked-compile-and-assert
     ()
     `(sb-int:named-lambda ,name (x fun)
        (block nil
          (labels
              ((snoop ()
                 (return
                   (funcall fun))))
            (declare (inline snoop))
            (when (plusp x)
              (snoop))
            (when (plusp x)
              (snoop))
            33)))
     ((10 (constantly 131)) 131))))

(defvar *symbol-value-bound-here-escaping* 10)

(with-test (:name :symbol-value-bound-here-escaping)
  (assert (eql (funcall
                (funcall
                 (checked-compile
                  `(lambda ()
                     (let (*symbol-value-bound-here-escaping*)
                       (lambda ()
                         (let (*symbol-value-bound-here-escaping*))
                         *symbol-value-bound-here-escaping*))))))
               10)))

(with-test (:name :+constant-boundp+)
  (let* ((package-name (gensym "CONSTANT-BOUNDP-TEST"))
         (*package* (make-package package-name :use '(cl))))
    (ctu:file-compile
     "(defconstant +constant-boundp+
        (if (boundp '+constant-boundp+)
            (symbol-value '+constant-boundp+)
            (let (*) (make-hash-table))))"
     :load t
     :before-load (lambda ()
                    (delete-package *package*)
                    (setf *package* (make-package package-name :use '(cl)))))))

(with-test (:name :bug-1804796)
  (let ((f337
         (checked-compile
          `(lambda (a)
             (flet ((%f9 (f9-1 &optional (f9-2 0) (f9-3 (complex a 0)))
                      (declare (ignore f9-1 f9-3))
                      (/
                       (count
                        (flet ((%f14 (&optional (f14-1 0) (f14-2 (complex f9-2 0)))
                                 (declare (ignore f14-1 f14-2))
                                 0))
                          (multiple-value-call #'%f14 (values f9-2)))
                        '(1 2 3))
                       -1)))
               (apply #'%f9
                      (list 0
                            (complex (complex
                                      (multiple-value-call #'%f9 (values a))
                                      0)
                                     0))))))))
    (assert (eql (funcall f337 17) 0))))

(defstruct dump-me)
(defmethod make-load-form ((self dump-me) &optional e)
  (declare (notinline typep) (ignore e))
  ;; This error in bad usage of TYPEP would cause SB-C::COMPUTE-ENTRY-INFO
  ;; to encounter another error:
  ;; The value
  ;;   #<SB-FORMAT::FMT-CONTROL "bad thing to be a type specifier: ~/SB-IMPL:PRINT-TYPE-SPECIFIER/">
  ;; is not of type SEQUENCE
  ;;
  (if (typep 'foo (cons 1 2))
      (make-load-form-saving-slots self)
      ''i-cant-even))

(with-test (:name :failed-dump-fun-source)
  (with-scratch-file (fasl "fasl")
    (let ((error-stream (make-string-output-stream)))
      (multiple-value-bind (fasl warnings errors)
          (let ((*error-output* error-stream))
            (compile-file "bad-mlf-test.lisp" :output-file fasl))
        (assert (and fasl warnings (not errors)))
        (assert (search "bad thing to be" (get-output-stream-string error-stream)))
        (load fasl)
        (assert (eq (zook) 'hi))))))

(with-test (:name :block-compile)
  (with-scratch-file (fasl "fasl")
    (compile-file "block-compile-test.lisp" :output-file fasl :block-compile t)
    (load fasl)
    ;; Make sure the defuns all get compiled into the same code
    ;; component.
    (assert (and (eq (sb-kernel::fun-code-header #'baz2)
                     (sb-kernel::fun-code-header #'bar2))
                 (eq (sb-kernel::fun-code-header #'baz2)
                     (sb-kernel::fun-code-header #'foo2))))))

(with-test (:name (:block-compile :entry-point))
  (with-scratch-file (fasl "fasl")
    (compile-file "block-compile-test-2.lisp" :output-file fasl :block-compile t :entry-points '(foo1))
    (load fasl)
    ;; Ensure bar1 gets let-converted into FOO1 and disappears.
    (assert (not (fboundp 'bar1)))
    (assert (eql (foo1 10 1) 3628800))
    (let ((code (sb-kernel::fun-code-header #'foo1)))
      (assert (= (sb-kernel::code-n-entries code) 1))
      (assert (eq (sb-kernel:%code-entry-point code 0)
                  #'foo1)))))

(with-test (:name (:block-compile :inline))
  (with-scratch-file (fasl "fasl")
    (compile-file "block-compile-test-3.lisp" :output-file fasl :block-compile t)
    (load fasl)
    ;; Ensure all functions get inlined properly, so that there are no
    ;; local calls between them. The result will be that NONE of the
    ;; functions will share a code component.
    (let ((components '()))
      (flet ((frob (fun)
               (pushnew (sb-kernel::fun-code-header fun) components)))
        (frob #'inl)
        (frob #'a)
        (frob #'foo2)
        (frob #'bar2))
      (assert (= (length components) 4)))))

(with-test (:name (:block-compile :start-block/end-block))
  (with-scratch-file (fasl "fasl")
    (compile-file "block-compile-test-4.lisp" :output-file fasl :block-compile :specified)
    (load fasl)
    (assert (eq (sb-kernel::fun-code-header #'fun1)
                (sb-kernel::fun-code-header #'fun3)))
    (assert (not (eq (sb-kernel::fun-code-header #'fun1)
                     (sb-kernel::fun-code-header #'fun4))))
    (assert (not (fboundp 'fun2)))))

(with-test (:name (:block-compile :inlined-functions-local-call))
  (with-scratch-file (fasl "fasl")
    (compile-file "block-compile-test-5.lisp" :output-file fasl :block-compile :specified)
    (load fasl)
    (assert (not (fboundp 'zeta1)))
    (assert (not (fboundp 'zeta2)))
    (assert (= (gamma1 4) 10))))

(with-test (:name :symbol-value-constant)
  (let* ((package-name (gensym "SYMBOL-VALUE-CONSTANT-TEST"))
         (*package* (make-package package-name :use '(cl))))
    (ctu:file-compile
     "(defconstant +constant+
       (let (*)
         (if (boundp '+constant+)
             (symbol-value '+constant+)
             (complex 0.0 0.0))))"
     :load t
     :before-load (lambda ()
                    (delete-package *package*)
                    (setf *package* (make-package package-name :use '(cl)))))))

(with-test (:name :block-compile-merge-lambdas)
  (ctu:file-compile
   "(let (f) (when f (funcall f)))
      (let ())"
   :block-compile t))


(with-test (:name :fopcompile-specials)
  (ctu:file-compile
   "(locally (declare (special foo)) (print foo))"))

(with-test (:name :local-call-context)
  (ctu:file-compile
   "(lambda (&optional b) (declare (type integer b)) b)"
   :load t))

(defstruct entry-info-type-struct)

(defvar *entry-info-type-struct* (make-entry-info-type-struct))

(defmethod make-load-form ((struct entry-info-type-struct) &optional env)
  (declare (ignore env))
  '*entry-info-type-struct*)

(declaim (ftype (function () (member #.*entry-info-type-struct*)) entry-info-type-func))


(with-test (:name :dump-entry-info-type)
  (ctu:file-compile
   "(lambda () (entry-info-type-func))"
   :load t))
