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

(cl:in-package :cl-user)

(load "assertoid.lisp")

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
  #+nil ; FIXME: still broken in 0.7.7.19 (after patch)
  (multiple-value-bind (result error)
      (ignore-errors (multiple-value-prog1 (progn (the real '(1 2 3)))))
    (assert (null result))
    (assert (typep error 'type-error)))
  #+nil ; FIXME: still broken in 0.7.7.19 (after patch)
  (multiple-value-bind (result error)
      (ignore-errors (the real '(1 2 3)))
    (assert (null result))
    (assert (typep error 'type-error))))

;;; BUG 48a. and b. (symbol-macrolet handling), fixed by Eric Marsden
;;; and Raymond Toy for CMUCL, fix ported for sbcl-0.7.6.18.
(multiple-value-bind (function warnings-p failure-p)
    (compile nil '(lambda () (symbol-macrolet ((t nil)) t)))
  (assert failure-p)
  (assert (raises-error? (funcall function) program-error)))
(multiple-value-bind (function warnings-p failure-p)
    (compile nil
	     '(lambda ()
		(symbol-macrolet ((*standard-input* nil))
		  *standard-input*)))
  (assert failure-p)
  (assert (raises-error? (funcall function) program-error)))
#||
BUG 48c, not yet fixed:
(multiple-value-bind (function warnings-p failure-p)
    (compile nil '(lambda () (symbol-macrolet ((s nil)) (declare (special s)) s)))
  (assert failure-p)
  (assert (raises-error? (funcall function) program-error)))
||#

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

;;; bug 110: the compiler flushed the argument type test and the default
;;; case in the cond.

(defun bug110 (x)
  (declare (optimize (safety 2) (speed 3)))
  (declare (type (or string stream) x))
  (cond ((typep x 'string) 'string)
        ((typep x 'stream) 'stream)
        (t
         'none)))

(multiple-value-bind (result condition)
    (ignore-errors (bug110 0))
  (declare (ignore result))
  (assert (typep condition 'type-error)))

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
