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

(load "assertoid.lisp")
(use-package "ASSERTOID")

;;; Old CMU CL code assumed that the names of "keyword" arguments are
;;; necessarily self-evaluating symbols, but ANSI Common Lisp allows
;;; them to be any symbols, not necessarily keywords, and thus not
;;; necessarily self-evaluating. Make sure that this works.
(defun newfangled-cons (&key ((left-thing x)) ((right-thing y)))
  (cons x y))
(assert (equal (cons 1 2) (newfangled-cons 'right-thing 2 'left-thing 1)))

;;; ANSI specifically says that duplicate keys are OK in lambda lists,
;;; with no special exception for macro lambda lists. (As reported by
;;; Pierre Mai on cmucl-imp 2001-03-30, Python didn't think so. The
;;; rest of the thread had some entertainment value, at least for me
;;; (WHN). The unbelievers were besmote and now even CMU CL will
;;; conform to the spec in this regard. Who needs diplomacy when you
;;; have brimstone?:-)
(defmacro ayup-duplicate-keys-are-ok-i-see-the-lite (&key k)
  k)
(assert (equal (ayup-duplicate-keys-are-ok-i-see-the-lite :k 112) 112))
(assert (equal (ayup-duplicate-keys-are-ok-i-see-the-lite :k 'x :k 'y) 'x))

;;; As reported by Alexey Dejneka (sbcl-devel 2002-01-30), in
;;; sbcl-0.7.1 plus his patch (i.e. essentially sbcl-0.7.1.2), the
;;; compiler barfed on this, blowing up in FIND-IN-PHYSENV looking for
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
(defun foo ()
  (catch 0 (print 1331)))

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
(multiple-value-bind (fun warnings-p failure-p)
    (compile nil
	     '(lambda ()
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
		  (values #'a17 #'a11))))
  ;; Python sees the structure accessor on the known-not-to-be-a-struct
  ;; A5 value and is very, very disappointed in you. (But it doesn't
  ;; signal BUG any more.)
  (assert failure-p))

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
(assert (null (ignore-errors (bug192a nil))))
(multiple-value-bind (result error) (ignore-errors (bug192a 100))
  (assert (null result))
  (assert (equal (type-error-expected-type error) 'single-float)))

;;; bug 194, fixed in part by APD "more strict type checking" patch
;;; (sbcl-devel 2002-08-07)
(progn
  (multiple-value-bind (result error)
      (ignore-errors (multiple-value-prog1 (progn (the real '(1 2 3)))))
    (assert (null result))
    (assert (typep error 'type-error)))
  (multiple-value-bind (result error)
      (ignore-errors (the real '(1 2 3)))
    (assert (null result))
    (assert (typep error 'type-error))))

(defun bug194d ()
  (null (ignore-errors
          (let ((arg1 1)
                (arg2 (identity (the real #(1 2 3)))))
            (if (< arg1 arg2) arg1 arg2)))))
(assert (eq (bug194d) t))


;;; BUG 48a. and b. (symbol-macrolet handling), fixed by Eric Marsden
;;; and Raymond Toy for CMUCL, fix ported for sbcl-0.7.6.18.
(multiple-value-bind (function warnings-p failure-p)
    (compile nil '(lambda ()
                   ;; not interested in the package lock violation here
                   (declare (sb-ext:disable-package-locks t))
                   (symbol-macrolet ((t nil)) t)))
  (assert failure-p)
  (assert (raises-error? (funcall function) program-error)))
(multiple-value-bind (function warnings-p failure-p)
    (compile nil
	     '(lambda ()
               ;; not interested in the package lock violation here
               (declare (sb-ext:disable-package-locks *standard-input*))
		(symbol-macrolet ((*standard-input* nil))
		  *standard-input*)))
  (assert failure-p)
  (assert (raises-error? (funcall function) program-error)))
(multiple-value-bind (function warnings-p failure-p)
    (compile nil '(lambda () (symbol-macrolet ((s nil)) (declare (special s)) s)))
  (assert failure-p)
  (assert (raises-error? (funcall function) program-error)))

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

(assert (null (dont-constrain-if-too-much-aux nil nil)))  

;;; TYPE-ERROR confusion ca. sbcl-0.7.7.24, reported and fixed by
;;; APD sbcl-devel 2002-09-14
(defun exercise-0-7-7-24-bug (x)
  (declare (integer x))
  (let (y)
    (setf y (the single-float (if (> x 0) x 3f0)))
    (list y y)))
(multiple-value-bind (v e) (ignore-errors (exercise-0-7-7-24-bug 4))
  (assert (null v))
  (assert (typep e 'type-error)))
(assert (equal (exercise-0-7-7-24-bug -4) '(3f0 3f0)))

;;; non-intersecting type declarations were DWIMing in a confusing
;;; fashion until sbcl-0.7.7.28, when APD reported and fixed the
;;; problem.
(defun non-intersecting-the (x)
  (let (y)
    (setf y (the single-float (the integer x)))
    (list y y)))

(raises-error? (foo 3) type-error)
(raises-error? (foo 3f0) type-error)

;;; until 0.8.2 SBCL did not check THEs in arguments
(defun the-in-arguments-aux (x)
  x)
(defun the-in-arguments-1 (x)
  (list x (the-in-arguments-aux (the (single-float 0s0) x))))
(defun the-in-arguments-2 (x)
  (list x (the-in-arguments-aux (the single-float x))))

(multiple-value-bind (result condition)
    (ignore-errors (the-in-arguments-1 1))
  (assert (null result))
  (assert (typep condition 'type-error)))
(multiple-value-bind (result condition)
    (ignore-errors (the-in-arguments-2 1))
  (assert (null result))
  (assert (typep condition 'type-error)))

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

(multiple-value-bind (result condition)
    (ignore-errors (bug153-1))
  (declare (ignore result))
  (assert (typep condition 'type-error)))
(multiple-value-bind (result condition)
    (ignore-errors (bug153-2 '(1 . 1)))
  (declare (ignore result))
  (assert (typep condition 'type-error)))

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

;;; check non-toplevel DEFMACRO
(defvar *defmacro-test-status* nil)

(defun defmacro-test ()
  (fmakunbound 'defmacro-test-aux)
  (let* ((src "defmacro-test.lisp")
         (obj (compile-file-pathname src)))
    (unwind-protect
         (progn
           (compile-file src)
           (assert (equal *defmacro-test-status* '(function a)))
           (setq *defmacro-test-status* nil)
           (load obj)
           (assert (equal *defmacro-test-status* nil))
           (macroexpand '(defmacro-test-aux 'a))
           (assert (equal *defmacro-test-status* '(macro 'a z-value)))
           (eval '(defmacro-test-aux 'a))
           (assert (equal *defmacro-test-status* '(expanded 'a z-value))))
      (ignore-errors (delete-file obj)))))

(defmacro-test)

;;; bug 204: EVAL-WHEN inside a local environment
(defvar *bug204-test-status*)

(defun bug204-test ()
  (let* ((src "bug204-test.lisp")
         (obj (compile-file-pathname src)))
    (unwind-protect
         (progn
           (setq *bug204-test-status* nil)
           (compile-file src)
           (assert (equal *bug204-test-status* '((:expanded :load-toplevel)
                                                 (:called :compile-toplevel)
                                                 (:expanded :compile-toplevel))))
           (setq *bug204-test-status* nil)
           (load obj)
           (assert (equal *bug204-test-status* '((:called :load-toplevel)))))
      (ignore-errors (delete-file obj)))))

(bug204-test)

;;; toplevel SYMBOL-MACROLET
(defvar *symbol-macrolet-test-status*)

(defun symbol-macrolet-test ()
  (let* ((src "symbol-macrolet-test.lisp")
         (obj (compile-file-pathname src)))
    (unwind-protect
         (progn
           (setq *symbol-macrolet-test-status* nil)
           (compile-file src)
           (assert (equal *symbol-macrolet-test-status*
                          '(2 1)))
           (setq *symbol-macrolet-test-status* nil)
           (load obj)
           (assert (equal *symbol-macrolet-test-status* '(2))))
      (ignore-errors (delete-file obj)))))

(symbol-macrolet-test)

;;; On the x86, this code failed to compile until sbcl-0.7.8.37:
(defun x86-assembler-failure (x)
  (declare (optimize (speed 3) (safety 0)))
  (eq (setf (car x) 'a) nil))

;;; bug 211: :ALLOW-OTHER-KEYS
(defun bug211d (&key (x :x x-p) ((:allow-other-keys y) :y y-p))
  (list x x-p y y-p))

(assert (equal (bug211d) '(:x nil :y nil)))
(assert (equal (bug211d :x 1) '(1 t :y nil)))
(assert (raises-error? (bug211d :y 2) program-error))
(assert (equal (bug211d :y 2 :allow-other-keys t :allow-other-keys nil)
               '(:x nil t t)))
(assert (raises-error? (bug211d :y 2 :allow-other-keys nil) program-error))

(let ((failure-p
       (nth-value
        3
        (compile 'bug211b
                 '(lambda ()
                   (flet ((test (&key (x :x x-p) ((:allow-other-keys y) :y y-p))
                            (list x x-p y y-p)))
                     (assert (equal (test) '(:x nil :y nil)))
                     (assert (equal (test :x 1) '(1 t :y nil)))
                     (assert (equal (test :y 2 :allow-other-keys 11 :allow-other-keys nil)
                                    '(:x nil 11 t)))))))))
  (assert (not failure-p))
  (bug211b))

(let ((failure-p
       (nth-value
        3
        (compile 'bug211c
                 '(lambda ()
                   (flet ((test (&key (x :x x-p))
                            (list x x-p)))
                     (assert (equal (test) '(:x nil)))
                     (assert (equal (test :x 1) '(1 t)))
                     (assert (equal (test :y 2 :allow-other-keys 11 :allow-other-keys nil)
                                    '(:x nil)))))))))
  (assert (not failure-p))
  (bug211c))

(dolist (form '((test :y 2)
                (test :y 2 :allow-other-keys nil)
                (test :y 2 :allow-other-keys nil :allow-other-keys t)))
  (multiple-value-bind (result warnings-p failure-p)
      (compile nil `(lambda ()
                     (flet ((test (&key (x :x x-p) ((:allow-other-keys y) :y y-p))
                              (list x x-p y y-p)))
                       ,form)))
    (assert failure-p)
    (assert (raises-error? (funcall result) program-error))))

;;; bug 217: wrong type inference
(defun bug217-1 (x s)
  (let ((f (etypecase x
             (character #'write-char)
             (integer #'write-byte))))
    (funcall f x s)
    (etypecase x
      (character (write-char x s))
      (integer (write-byte x s)))))
(bug217-1 #\1 *standard-output*)


;;; bug 221: tried and died on CSUBTYPEP (not VALUES-SUBTYPEP) of the
;;; function return types when inferring the type of the IF expression
(declaim (ftype (function (fixnum) (values package boolean)) bug221f1))
(declaim (ftype (function (t) (values package boolean)) bug221f2))
(defun bug221 (b x)
  (funcall (if b #'bug221f1 #'bug221f2) x))

;;; bug 172: macro lambda lists were too permissive until 0.7.9.28
;;; (fix provided by Matthew Danish) on sbcl-devel
(assert (null (ignore-errors
		(defmacro bug172 (&rest rest foo) `(list ,rest ,foo)))))

;;; embedded THEs
(defun check-embedded-thes (policy1 policy2 x y)
  (handler-case
      (funcall (compile nil
                        `(lambda (f)
                           (declare (optimize (speed 2) (safety ,policy1)))
                           (multiple-value-list
                            (the (values (integer 2 3) t &optional)
                              (locally (declare (optimize (safety ,policy2)))
                                (the (values t (single-float 2f0 3f0) &optional)
                                  (funcall f)))))))
               (lambda () (values x y)))
    (type-error (error)
      error)))

(assert (equal (check-embedded-thes 0 0  :a :b) '(:a :b)))

(assert (equal (check-embedded-thes 0 3  :a 2.5f0) '(:a 2.5f0)))
(assert (typep (check-embedded-thes 0 3  2 3.5f0) 'type-error))

(assert (equal (check-embedded-thes 0 1  :a 3.5f0) '(:a 3.5f0)))
(assert (typep (check-embedded-thes 0 1  2 2.5d0) 'type-error))

(assert (equal (check-embedded-thes 3 0  2 :a) '(2 :a)))
(assert (typep (check-embedded-thes 3 0  4 2.5f0) 'type-error))

(assert (equal (check-embedded-thes 1 0  4 :b) '(4 :b)))
(assert (typep (check-embedded-thes 1 0  1.0 2.5f0) 'type-error))


(assert (equal (check-embedded-thes 3 3  2 2.5f0) '(2 2.5f0)))
(assert (typep (check-embedded-thes 3 3  0 2.5f0) 'type-error))
(assert (typep (check-embedded-thes 3 3  2 3.5f0) 'type-error))

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
(assert (= (call-inlined 3)
	   ;; we should have inlined the previous definition, so the
	   ;; new one won't show up yet.
	   4))
(defun call-inlined (z)
  (to-be-inlined z))
(assert (= (call-inlined 3) 6))
(defun to-be-inlined (y)
  (+ y 5))
(assert (= (call-inlined 3) 6))

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
(assert (= (bug219-a-aux)
	   (if *bug219-a-expanded-p* 4 3)))
(defvar *bug219-a-temp* 3)
(assert (= (bug219-a *bug219-a-temp*) 4))

(defvar *bug219-b-expanded-p* nil)
(defun bug219-b-aux1 (x)
  (when x
    (define-compiler-macro bug219-b (y)
      (setf *bug219-b-expanded-p* t)
      `(+ ,y 2))))
(defun bug219-b-aux2 (z)
  (bug219-b z))
(assert (not *bug219-b-expanded-p*))
(assert (raises-error? (bug219-b-aux2 1) undefined-function))
(bug219-b-aux1 t)
(defun bug219-b-aux2 (z)
  (bug219-b z))
(defun bug219-b (x)
  x)
(assert (= (bug219-b-aux2 1)
	   (if *bug219-b-expanded-p* 3 1)))

;;; bug 224: failure in unreachable code deletion
(defmacro do-optimizations (&body body)
  `(dotimes (.speed. 4)
     (dotimes (.space. 4)
       (dotimes (.debug. 4)
         (dotimes (.compilation-speed. 4)
           (proclaim `(optimize (speed , .speed.) (space , .space.)
                                (debug , .debug.)
                                (compilation-speed , .compilation-speed.)))
           ,@body)))))

(do-optimizations
    (compile nil
             (read-from-string
              "(lambda () (#:localy (declare (optimize (safety 3)))
                                    (ignore-errors (progn (values-list (car (list '(1 . 2)))) t))))")))

(do-optimizations
    (compile nil '(lambda ()
                   (labels ((ext ()
                              (tagbody
                                 (labels ((i1 () (list (i2) (i2)))
                                          (i2 () (list (int) (i1)))
                                          (int () (go :exit)))
                                   (list (i1) (i1) (i1)))
                               :exit (return-from ext)
                                 )))
                     (list (error "nih") (ext) (ext))))))

(do-optimizations
  (compile nil '(lambda (x) (let ((y (error ""))) (list x y)))))

;;; bug 223: invalid moving of global function name referencing
(defun bug223-int (n)
  `(int ,n))

(defun bug223-wrap ()
  (let ((old #'bug223-int))
    (setf (fdefinition 'bug223-int)
          (lambda (n)
            (assert (> n 0))
            `(ext ,@(funcall old (1- n)))))))
(compile 'bug223-wrap)

(assert (equal (bug223-int 4) '(int 4)))
(bug223-wrap)
(assert (equal (bug223-int 4) '(ext int 3)))
(bug223-wrap)
(assert (equal (bug223-int 4) '(ext ext int 2)))

;;; COERCE got its own DEFOPTIMIZER which has to reimplement most of
;;; SPECIFIER-TYPE-NTH-ARG.  For a while, an illegal type would throw
;;; you into the debugger on compilation.
(defun coerce-defopt1 (x)
  ;; illegal, but should be compilable.
  (coerce x '(values t)))
(defun coerce-defopt2 (x)
  ;; illegal, but should be compilable.
  (coerce x '(values t &optional)))
(assert (null (ignore-errors (coerce-defopt1 3))))
(assert (null (ignore-errors (coerce-defopt2 3))))

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

;;; FUNCTION-LAMBDA-EXPRESSION should return something that's COMPILE
;;; can understand.  Here's a simple test for that on a function
;;; that's likely to return a hairier list than just a lambda:
(macrolet ((def (fn) `(progn
		       (declaim (inline ,fn))
		       (defun ,fn (x) (1+ x)))))
  (def bug228))
(let ((x (function-lambda-expression #'bug228)))
  (when x
    (assert (= (funcall (compile nil x) 1) 2))))

;;;
(defun bug192b (i)
  (dotimes (j i)
    (declare (type (mod 4) i))
    (unless (< i 5)
      (print j))))
(assert (raises-error? (bug192b 6) type-error))

(defun bug192c (x y)
  (locally (declare (type fixnum x y))
    (+ x (* 2 y))))
(assert (raises-error? (bug192c 1.1 2) type-error))

(assert (raises-error? (progn (the real (list 1)) t) type-error))

(defun bug236 (a f)
  (declare (optimize (speed 2) (safety 0)))
  (+ 1d0
     (the double-float
       (multiple-value-prog1
           (svref a 0)
         (unless f (return-from bug236 0))))))
(assert (eql (bug236 #(4) nil) 0))

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
(assert (raises-error? (test-type-of-special-1 3/2) type-error))
(assert (raises-error? (test-type-of-special-2 3) type-error))
(assert (equal (test-type-of-special-2 8) '(8 4 4)))

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
(assert (= (baz8alpha04 12 13) 25))

;;; evaluation order in structure slot writers
(defstruct sswo
  a b)
(let* ((i 0)
       (s (make-sswo :a (incf i) :b (incf i)))
       (l (list s :v)))
  (assert (= (sswo-a s) 1))
  (assert (= (sswo-b s) 2))
  (setf (sswo-a (pop l)) (pop l))
  (assert (eq l nil))
  (assert (eq (sswo-a s) :v)))

(defun bug249 (x)
  (flet ((bar (y)
           (declare (fixnum y))
           (incf x)))
    (list (bar x) (bar x) (bar x))))

(assert (raises-error? (bug249 1.0) type-error))

;;; bug reported by ohler on #lisp 2003-07-10
(defun bug-ohler-2003-07-10 (a b)
  (declare (optimize (speed 0) (safety 3) (space 0)
                     (debug 1) (compilation-speed 0)))
  (adjoin a b))

;;; bug reported by Doug McNaught on sbcl-devel 2003-09-14:
;;; COMPILE-FILE did not bind *READTABLE*
(let* ((source "bug-doug-mcnaught-20030914.lisp")
       (fasl (compile-file-pathname source)))
  (labels ((check ()
             (assert (null (get-macro-character #\]))))
           (full-check ()
             (check)
             (assert (typep *bug-doug-mcnaught-20030914*
                            '(simple-array (unsigned-byte 4) (*))))
             (assert (equalp *bug-doug-mcnaught-20030914* #(1 2 3)))
             (makunbound '*bug-doug-mcnaught-20030914*)))
    (compile-file source)
    (check)
    (load fasl)
    (full-check)
    (load source)
    (full-check)
    (delete-file fasl)))

(defun expt-derive-type-bug (a b)
  (unless (< a b)
    (truncate (expt a b))))
(assert (equal (multiple-value-list (expt-derive-type-bug 1 1))
	       '(1 0)))

;;; Problems with type checking in functions with EXPLICIT-CHECK
;;; attribute (reported by Peter Graves)
(loop for (fun . args) in '((= a) (/= a)
                            (< a) (<= a) (> a) (>= a))
      do (assert (raises-error? (apply fun args) type-error)))

(defclass broken-input-stream (sb-gray:fundamental-input-stream) ())
(defmethod sb-gray:stream-read-char ((stream broken-input-stream))
  (throw 'break :broken))
(assert (eql (block return
               (handler-case
                   (catch 'break
                     (funcall (eval ''peek-char)
                              1 (make-instance 'broken-input-stream))
                     :test-broken)
                 (type-error (c)
                   (return-from return :good))))
             :good))

;;;; MUFFLE-CONDITIONS test (corresponds to the test in the manual)
(defvar *compiler-note-count* 0)
#-(or alpha x86-64) ; FIXME: make a better test!
(handler-bind ((sb-ext:compiler-note (lambda (c)
				       (declare (ignore c))
				       (incf *compiler-note-count*))))
  (let ((fun
	 (compile nil
		  '(lambda (x)
		    (declare (optimize speed) (fixnum x))
		    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
		    (values (* x 5) ; no compiler note from this
		     (locally
		       (declare (sb-ext:unmuffle-conditions sb-ext:compiler-note))
		       ;; this one gives a compiler note
		       (* x -5)))))))
    (assert (= *compiler-note-count* 1))
    (assert (equal (multiple-value-list (funcall fun 1)) '(5 -5)))))

(handler-case
    (eval '(flet ((%f (&key) nil)) (%f nil nil)))
  (error (c) :good)
  (:no-error (val) (error "no error: ~S" val)))
(handler-case
    (eval '(labels ((%f (&key x) x)) (%f nil nil)))
  (error (c) :good)
  (:no-error (val) (error "no error: ~S" val)))

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
	  (result-type (fun-type-returns (info :function :type function))))
      (cond
	((values-type-p result-type)
	 (do ((ltypes (append (args-type-required result-type)
			      (args-type-optional result-type))
		      (rest ltypes))
	      (types types (rest types)))
	     ((null ltypes)
	      (unless (null types)
		(format t "~&More types than ltypes in ~A, translating ~A.~%"
			(template-name template)
			function)
		(return nil)))
	   (when (null types)
	     (unless (null ltypes)
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
(defun identify-suspect-vops (&optional (env (first
					      (last *info-environment*))))
  (do-info (env :class class :type type :name name :value value)
    (when (and (eq class :function) (eq type :type))
      ;; OK, so we have an entry in the INFO database. Now, if ...
      (let* ((info (info :function :info name))
	     (templates (and info (fun-info-templates info))))
	(when templates
	  ;; ... it has translators
	  (grovel-results name))))))
(identify-suspect-vops)

;;; success
(quit :unix-status 104)
