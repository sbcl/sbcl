;;;; the LOOP iteration macro

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This code was modified by William Harold Newman beginning
;;;; 19981106, originally to conform to the new SBCL bootstrap package
;;;; system and then subsequently to address other cross-compiling
;;;; bootstrap issues. Whether or not it then supported all the
;;;; environments implied by the reader conditionals in the source
;;;; code (e.g. #!+CLOE-RUNTIME) before that modification, it sure
;;;; doesn't now: it might be appropriate for CMU-CL-derived systems
;;;; in general but only claims to be appropriate for the particular
;;;; branch I was working on.

;;;; This software is derived from software originally released by the
;;;; Massachusetts Institute of Technology and Symbolics, Inc. Copyright and
;;;; release statements follow. Later modifications to the software are in
;;;; the public domain and are provided with absolutely no warranty. See the
;;;; COPYING and CREDITS files for more information.

;;;; Portions of LOOP are Copyright (c) 1986 by the Massachusetts Institute
;;;; of Technology. All Rights Reserved.
;;;;
;;;; Permission to use, copy, modify and distribute this software and its
;;;; documentation for any purpose and without fee is hereby granted,
;;;; provided that the M.I.T. copyright notice appear in all copies and that
;;;; both that copyright notice and this permission notice appear in
;;;; supporting documentation. The names "M.I.T." and "Massachusetts
;;;; Institute of Technology" may not be used in advertising or publicity
;;;; pertaining to distribution of the software without specific, written
;;;; prior permission. Notice must be given in supporting documentation that
;;;; copying distribution is by permission of M.I.T. M.I.T. makes no
;;;; representations about the suitability of this software for any purpose.
;;;; It is provided "as is" without express or implied warranty.
;;;;
;;;;      Massachusetts Institute of Technology
;;;;      77 Massachusetts Avenue
;;;;      Cambridge, Massachusetts  02139
;;;;      United States of America
;;;;      +1-617-253-1000

;;;; Portions of LOOP are Copyright (c) 1989, 1990, 1991, 1992 by Symbolics,
;;;; Inc. All Rights Reserved.
;;;;
;;;; Permission to use, copy, modify and distribute this software and its
;;;; documentation for any purpose and without fee is hereby granted,
;;;; provided that the Symbolics copyright notice appear in all copies and
;;;; that both that copyright notice and this permission notice appear in
;;;; supporting documentation. The name "Symbolics" may not be used in
;;;; advertising or publicity pertaining to distribution of the software
;;;; without specific, written prior permission. Notice must be given in
;;;; supporting documentation that copying distribution is by permission of
;;;; Symbolics. Symbolics makes no representations about the suitability of
;;;; this software for any purpose. It is provided "as is" without express
;;;; or implied warranty.
;;;;
;;;; Symbolics, CLOE Runtime, and Minima are trademarks, and CLOE, Genera,
;;;; and Zetalisp are registered trademarks of Symbolics, Inc.
;;;;
;;;;      Symbolics, Inc.
;;;;      8 New England Executive Park, East
;;;;      Burlington, Massachusetts  01803
;;;;      United States of America
;;;;      +1-617-221-1000

(in-package "SB!LOOP")

;;;; The design of this LOOP is intended to permit, using mostly the same
;;;; kernel of code, up to three different "loop" macros:
;;;;
;;;; (1) The unextended, unextensible ANSI standard LOOP;
;;;;
;;;; (2) A clean "superset" extension of the ANSI LOOP which provides
;;;; functionality similar to that of the old LOOP, but "in the style of"
;;;; the ANSI LOOP. For instance, user-definable iteration paths, with a
;;;; somewhat cleaned-up interface.
;;;;
;;;; (3) Extensions provided in another file which can make this LOOP
;;;; kernel behave largely compatibly with the Genera-vintage LOOP macro,
;;;; with only a small addition of code (instead of two whole, separate,
;;;; LOOP macros).
;;;;
;;;; Each of the above three LOOP variations can coexist in the same LISP
;;;; environment.
;;;;
;;;; KLUDGE: In SBCL, we only really use variant (1), and any generality
;;;; for the other variants is wasted. -- WHN 20000121

;;;; FIXME: the STEP-FUNCTION stuff in the code seems to've been
;;;; intended to support code which was conditionalized with
;;;; LOOP-PREFER-POP (not true on CMU CL) and which has since been
;;;; removed. Thus, STEP-FUNCTION stuff could probably be removed too.

;;;; miscellaneous environment things

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *loop-real-data-type* 'real))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *loop-gentemp* nil)
  (defun loop-gentemp (&optional (pref 'loopvar-))
    (if *loop-gentemp*
      (gentemp (string pref))
      (gensym))))

;;; @@@@ The following form takes a list of variables and a form which
;;; presumably references those variables, and wraps it somehow so that the
;;; compiler does not consider those variables have been referenced. The intent
;;; of this is that iteration variables can be flagged as unused by the
;;; compiler, e.g. I in (loop for i from 1 to 10 do (print t)), since we will
;;; tell it when a usage of it is "invisible" or "not to be considered".
;;;
;;; We implicitly assume that a setq does not count as a reference. That is,
;;; the kind of form generated for the above loop construct to step I,
;;; simplified, is
;;;   `(SETQ I ,(HIDE-VARIABLE-REFERENCES '(I) '(1+ I))).
;;;
;;; FIXME: This is a no-op except for Genera, now obsolete, so it
;;; can be removed.
(defun hide-variable-references (variable-list form)
  (declare (ignore variable-list))
  form)

;;; @@@@ The following function takes a flag, a variable, and a form which
;;; presumably references that variable, and wraps it somehow so that the
;;; compiler does not consider that variable to have been referenced. The
;;; intent of this is that iteration variables can be flagged as unused by the
;;; compiler, e.g. I in (loop for i from 1 to 10 do (print t)), since we will
;;; tell it when a usage of it is "invisible" or "not to be considered".
;;;
;;; We implicitly assume that a setq does not count as a reference. That is,
;;; the kind of form generated for the above loop construct to step I,
;;; simplified, is
;;;   `(SETQ I ,(HIDE-VARIABLE-REFERENCES T 'I '(1+ I))).
;;;
;;; Certain cases require that the "invisibility" of the reference be
;;; conditional upon something. This occurs in cases of "named" variables (the
;;; USING clause). For instance, we want IDX in (LOOP FOR E BEING THE
;;; VECTOR-ELEMENTS OF V USING (INDEX IDX) ...) to be "invisible" when it is
;;; stepped, so that the user gets informed if IDX is not referenced. However,
;;; if no USING clause is present, we definitely do not want to be informed
;;; that some gensym or other is not used.
;;;
;;; It is easier for the caller to do this conditionally by passing a flag
;;; (which happens to be the second value of NAMED-VARIABLE, q.v.) to this
;;; function than for all callers to contain the conditional invisibility
;;; construction.
;;;
;;; FIXME: This is a no-op except for Genera, now obsolete, so it
;;; can be removed.
(defun hide-variable-reference (really-hide variable form)
  (declare (ignore really-hide variable))
  form)

;;;; list collection macrology

(sb!kernel:defmacro-mundanely with-loop-list-collection-head
    ((head-var tail-var &optional user-head-var) &body body)
  (let ((l (and user-head-var (list (list user-head-var nil)))))
    `(let* ((,head-var (list nil)) (,tail-var ,head-var) ,@l)
       ,@body)))

(sb!kernel:defmacro-mundanely loop-collect-rplacd
    (&environment env (head-var tail-var &optional user-head-var) form)
  (setq form (sb!xc:macroexpand form env))
  (flet ((cdr-wrap (form n)
	   (declare (fixnum n))
	   (do () ((<= n 4) (setq form `(,(case n
					    (1 'cdr)
					    (2 'cddr)
					    (3 'cdddr)
					    (4 'cddddr))
					 ,form)))
	     (setq form `(cddddr ,form) n (- n 4)))))
    (let ((tail-form form) (ncdrs nil))
      ;; Determine whether the form being constructed is a list of known
      ;; length.
      (when (consp form)
	(cond ((eq (car form) 'list)
	       (setq ncdrs (1- (length (cdr form)))))
	      ((member (car form) '(list* cons))
	       (when (and (cddr form) (member (car (last form)) '(nil 'nil)))
		 (setq ncdrs (- (length (cdr form)) 2))))))
      (let ((answer
	      (cond ((null ncdrs)
		     `(when (setf (cdr ,tail-var) ,tail-form)
			(setq ,tail-var (last (cdr ,tail-var)))))
		    ((< ncdrs 0) (return-from loop-collect-rplacd nil))
		    ((= ncdrs 0)
		     ;; @@@@ Here we have a choice of two idioms:
		     ;;   (RPLACD TAIL (SETQ TAIL TAIL-FORM))
		     ;;   (SETQ TAIL (SETF (CDR TAIL) TAIL-FORM)).
		     ;; Genera and most others I have seen do better with the
		     ;; former.
		     `(rplacd ,tail-var (setq ,tail-var ,tail-form)))
		    (t `(setq ,tail-var ,(cdr-wrap `(setf (cdr ,tail-var)
							  ,tail-form)
						   ncdrs))))))
	;; If not using locatives or something similar to update the
	;; user's head variable, we've got to set it... It's harmless
	;; to repeatedly set it unconditionally, and probably faster
	;; than checking.
	(when user-head-var
	  (setq answer
		`(progn ,answer
			(setq ,user-head-var (cdr ,head-var)))))
	answer))))

(sb!kernel:defmacro-mundanely loop-collect-answer (head-var
						   &optional user-head-var)
  (or user-head-var
      `(cdr ,head-var)))

;;;; maximization technology

#|
The basic idea of all this minimax randomness here is that we have to
have constructed all uses of maximize and minimize to a particular
"destination" before we can decide how to code them. The goal is to not
have to have any kinds of flags, by knowing both that (1) the type is
something which we can provide an initial minimum or maximum value for
and (2) know that a MAXIMIZE and MINIMIZE are not being combined.

SO, we have a datastructure which we annotate with all sorts of things,
incrementally updating it as we generate loop body code, and then use
a wrapper and internal macros to do the coding when the loop has been
constructed.
|#

(defstruct (loop-minimax
	     (:constructor make-loop-minimax-internal)
	     (:copier nil)
	     (:predicate nil))
  answer-variable
  type
  temp-variable
  flag-variable
  operations
  infinity-data)

(defvar *loop-minimax-type-infinities-alist*
  ;; Note: In the portable loop.lisp, this had various
  ;; conditional-on-*FEATURES* cases to support machines which had true
  ;; floating infinity. Now that we're limited to CMU CL, this is irrelevant.
  ;; FIXME: Or is it? What if we ever support infinity? Perhaps we should
  ;; put in something conditional on SB-INFINITY or something?
  '((fixnum most-positive-fixnum most-negative-fixnum)))

(defun make-loop-minimax (answer-variable type)
  (let ((infinity-data (cdr (assoc type
				   *loop-minimax-type-infinities-alist*
				   :test #'subtypep))))
    (make-loop-minimax-internal
      :answer-variable answer-variable
      :type type
      :temp-variable (loop-gentemp 'loop-maxmin-temp-)
      :flag-variable (and (not infinity-data)
			  (loop-gentemp 'loop-maxmin-flag-))
      :operations nil
      :infinity-data infinity-data)))

(defun loop-note-minimax-operation (operation minimax)
  (pushnew (the symbol operation) (loop-minimax-operations minimax))
  (when (and (cdr (loop-minimax-operations minimax))
	     (not (loop-minimax-flag-variable minimax)))
    (setf (loop-minimax-flag-variable minimax)
	  (loop-gentemp 'loop-maxmin-flag-)))
  operation)

(sb!kernel:defmacro-mundanely with-minimax-value (lm &body body)
  (let ((init (loop-typed-init (loop-minimax-type lm)))
	(which (car (loop-minimax-operations lm)))
	(infinity-data (loop-minimax-infinity-data lm))
	(answer-var (loop-minimax-answer-variable lm))
	(temp-var (loop-minimax-temp-variable lm))
	(flag-var (loop-minimax-flag-variable lm))
	(type (loop-minimax-type lm)))
    (if flag-var
	`(let ((,answer-var ,init) (,temp-var ,init) (,flag-var nil))
	   (declare (type ,type ,answer-var ,temp-var))
	   ,@body)
	`(let ((,answer-var ,(if (eq which 'min)
				 (first infinity-data)
				 (second infinity-data)))
	       (,temp-var ,init))
	   (declare (type ,type ,answer-var ,temp-var))
	   ,@body))))

(sb!kernel:defmacro-mundanely loop-accumulate-minimax-value (lm
							     operation
							     form)
  (let* ((answer-var (loop-minimax-answer-variable lm))
	 (temp-var (loop-minimax-temp-variable lm))
	 (flag-var (loop-minimax-flag-variable lm))
	 (test
	   (hide-variable-reference
	     t (loop-minimax-answer-variable lm)
	     `(,(ecase operation
		  (min '<)
		  (max '>))
	       ,temp-var ,answer-var))))
    `(progn
       (setq ,temp-var ,form)
       (when ,(if flag-var `(or (not ,flag-var) ,test) test)
	 (setq ,@(and flag-var `(,flag-var t))
	       ,answer-var ,temp-var)))))

;;;; LOOP keyword tables

#|
LOOP keyword tables are hash tables string keys and a test of EQUAL.

The actual descriptive/dispatch structure used by LOOP is called a "loop
universe" contains a few tables and parameterizations. The basic idea is
that we can provide a non-extensible ANSI-compatible loop environment,
an extensible ANSI-superset loop environment, and (for such environments
as CLOE) one which is "sufficiently close" to the old Genera-vintage
LOOP for use by old user programs without requiring all of the old LOOP
code to be loaded.
|#

;;;; token hackery

;;; Compare two "tokens". The first is the frob out of *LOOP-SOURCE-CODE*,
;;; the second a symbol to check against.
(defun loop-tequal (x1 x2)
  (and (symbolp x1) (string= x1 x2)))

(defun loop-tassoc (kwd alist)
  (and (symbolp kwd) (assoc kwd alist :test #'string=)))

(defun loop-tmember (kwd list)
  (and (symbolp kwd) (member kwd list :test #'string=)))

(defun loop-lookup-keyword (loop-token table)
  (and (symbolp loop-token)
       (values (gethash (symbol-name loop-token) table))))

(sb!kernel:defmacro-mundanely loop-store-table-data (symbol table datum)
  `(setf (gethash (symbol-name ,symbol) ,table) ,datum))

(defstruct (loop-universe
	     (:copier nil)
	     (:predicate nil))
  keywords	       ; hash table, value = (fn-name . extra-data)
  iteration-keywords     ; hash table, value = (fn-name . extra-data)
  for-keywords	   ; hash table, value = (fn-name . extra-data)
  path-keywords	  ; hash table, value = (fn-name . extra-data)
  type-symbols	   ; hash table of type SYMBOLS, test EQ,
			 ; value = CL type specifier
  type-keywords	  ; hash table of type STRINGS, test EQUAL,
			 ; value = CL type spec
  ansi		   ; NIL, T, or :EXTENDED
  implicit-for-required) ; see loop-hack-iteration
(sb!int:def!method print-object ((u loop-universe) stream)
  (let ((string (case (loop-universe-ansi u)
		  ((nil) "Non-ANSI")
		  ((t) "ANSI")
		  (:extended "Extended-ANSI")
		  (t (loop-universe-ansi u)))))
    (print-unreadable-object (u stream :type t)
      (write-string string stream))))

;;; This is the "current" loop context in use when we are expanding a
;;; loop. It gets bound on each invocation of LOOP.
(defvar *loop-universe*)

(defun make-standard-loop-universe (&key keywords for-keywords
					 iteration-keywords path-keywords
					 type-keywords type-symbols ansi)
  (check-type ansi (member nil t :extended))
  (flet ((maketable (entries)
	   (let* ((size (length entries))
		  (ht (make-hash-table :size (if (< size 10) 10 size)
				       :test 'equal)))
	     (dolist (x entries)
	       (setf (gethash (symbol-name (car x)) ht) (cadr x)))
	     ht)))
    (make-loop-universe
      :keywords (maketable keywords)
      :for-keywords (maketable for-keywords)
      :iteration-keywords (maketable iteration-keywords)
      :path-keywords (maketable path-keywords)
      :ansi ansi
      :implicit-for-required (not (null ansi))
      :type-keywords (maketable type-keywords)
      :type-symbols (let* ((size (length type-symbols))
			   (ht (make-hash-table :size (if (< size 10) 10 size)
						:test 'eq)))
		      (dolist (x type-symbols)
			(if (atom x)
			    (setf (gethash x ht) x)
			    (setf (gethash (car x) ht) (cadr x))))
		      ht))))

;;;; SETQ hackery

(defvar *loop-destructuring-hooks*
	nil
  #!+sb-doc
  "If not NIL, this must be a list of two things:
a LET-like macro, and a SETQ-like macro, which perform LOOP-style destructuring.")

(defun loop-make-psetq (frobs)
  (and frobs
       (loop-make-desetq
	 (list (car frobs)
	       (if (null (cddr frobs)) (cadr frobs)
		   `(prog1 ,(cadr frobs)
			   ,(loop-make-psetq (cddr frobs))))))))

(defun loop-make-desetq (var-val-pairs)
  (if (null var-val-pairs)
      nil
      (cons (if *loop-destructuring-hooks*
		(cadr *loop-destructuring-hooks*)
		'loop-really-desetq)
	    var-val-pairs)))

(defvar *loop-desetq-temporary*
	(make-symbol "LOOP-DESETQ-TEMP"))

(sb!kernel:defmacro-mundanely loop-really-desetq (&environment env
						  &rest var-val-pairs)
  (labels ((find-non-null (var)
	     ;; see whether there's any non-null thing here
	     ;; recurse if the list element is itself a list
	     (do ((tail var)) ((not (consp tail)) tail)
	       (when (find-non-null (pop tail)) (return t))))
	   (loop-desetq-internal (var val &optional temp)
	     ;; returns a list of actions to be performed
	     (typecase var
	       (null
		 (when (consp val)
		   ;; don't lose possible side-effects
		   (if (eq (car val) 'prog1)
		       ;; these can come from psetq or desetq below.
		       ;; throw away the value, keep the side-effects.
		       ;;Special case is for handling an expanded POP.
		       (mapcan #'(lambda (x)
				   (and (consp x)
					(or (not (eq (car x) 'car))
					    (not (symbolp (cadr x)))
					    (not (symbolp (setq x (sb!xc:macroexpand x env)))))
					(cons x nil)))
			       (cdr val))
		       `(,val))))
	       (cons
		 (let* ((car (car var))
			(cdr (cdr var))
			(car-non-null (find-non-null car))
			(cdr-non-null (find-non-null cdr)))
		   (when (or car-non-null cdr-non-null)
		     (if cdr-non-null
			 (let* ((temp-p temp)
				(temp (or temp *loop-desetq-temporary*))
				(body `(,@(loop-desetq-internal car
								`(car ,temp))
					  (setq ,temp (cdr ,temp))
					  ,@(loop-desetq-internal cdr
								  temp
								  temp))))
			   (if temp-p
			       `(,@(unless (eq temp val)
				     `((setq ,temp ,val)))
				 ,@body)
			       `((let ((,temp ,val))
				   ,@body))))
			 ;; no cdring to do
			 (loop-desetq-internal car `(car ,val) temp)))))
	       (otherwise
		 (unless (eq var val)
		   `((setq ,var ,val)))))))
    (do ((actions))
	((null var-val-pairs)
	 (if (null (cdr actions)) (car actions) `(progn ,@(nreverse actions))))
      (setq actions (revappend
		      (loop-desetq-internal (pop var-val-pairs)
					    (pop var-val-pairs))
		      actions)))))

;;;; LOOP-local variables

;;;This is the "current" pointer into the LOOP source code.
(defvar *loop-source-code*)

;;;This is the pointer to the original, for things like NAMED that
;;;insist on being in a particular position
(defvar *loop-original-source-code*)

;;;This is *loop-source-code* as of the "last" clause. It is used
;;;primarily for generating error messages (see loop-error, loop-warn).
(defvar *loop-source-context*)

;;;List of names for the LOOP, supplied by the NAMED clause.
(defvar *loop-names*)

;;;The macroexpansion environment given to the macro.
(defvar *loop-macro-environment*)

;;;This holds variable names specified with the USING clause.
;;; See LOOP-NAMED-VARIABLE.
(defvar *loop-named-variables*)

;;; LETlist-like list being accumulated for one group of parallel bindings.
(defvar *loop-variables*)

;;;List of declarations being accumulated in parallel with
;;;*loop-variables*.
(defvar *loop-declarations*)

;;;Used by LOOP for destructuring binding, if it is doing that itself.
;;; See loop-make-variable.
(defvar *loop-desetq-crocks*)

;;; List of wrapping forms, innermost first, which go immediately inside
;;; the current set of parallel bindings being accumulated in
;;; *loop-variables*. The wrappers are appended onto a body. E.g.,
;;; this list could conceivably has as its value ((with-open-file (g0001
;;; g0002 ...))), with g0002 being one of the bindings in
;;; *loop-variables* (this is why the wrappers go inside of the variable
;;; bindings).
(defvar *loop-wrappers*)

;;;This accumulates lists of previous values of *loop-variables* and the
;;;other lists  above, for each new nesting of bindings. See
;;;loop-bind-block.
(defvar *loop-bind-stack*)

;;;This is a LOOP-global variable for the (obsolete) NODECLARE clause
;;;which inhibits  LOOP from actually outputting a type declaration for
;;;an iteration (or any) variable.
(defvar *loop-nodeclare*)

;;;This is simply a list of LOOP iteration variables, used for checking
;;;for duplications.
(defvar *loop-iteration-variables*)

;;;List of prologue forms of the loop, accumulated in reverse order.
(defvar *loop-prologue*)

(defvar *loop-before-loop*)
(defvar *loop-body*)
(defvar *loop-after-body*)

;;;This is T if we have emitted any body code, so that iteration driving
;;;clauses can be disallowed. This is not strictly the same as
;;;checking *loop-body*, because we permit some clauses  such as RETURN
;;;to not be considered "real" body (so as to permit the user to "code"
;;;an  abnormal return value "in loop").
(defvar *loop-emitted-body*)

;;;List of epilogue forms (supplied by FINALLY generally), accumulated
;;; in reverse order.
(defvar *loop-epilogue*)

;;;List of epilogue forms which are supplied after the above "user"
;;;epilogue. "normal" termination return values are provide by putting
;;;the return form in here. Normally this is done using
;;;loop-emit-final-value, q.v.
(defvar *loop-after-epilogue*)

;;;The "culprit" responsible for supplying a final value from the loop.
;;;This  is so loop-emit-final-value can moan about multiple return
;;;values being supplied.
(defvar *loop-final-value-culprit*)

;;;If not NIL, we are in some branch of a conditional. Some clauses may
;;;be disallowed.
(defvar *loop-inside-conditional*)

;;;If not NIL, this is a temporary bound around the loop for holding the
;;;temporary  value for "it" in things like "when (f) collect it". It
;;;may be used as a supertemporary by some other things.
(defvar *loop-when-it-variable*)

;;;Sometimes we decide we need to fold together parts of the loop, but
;;;some part of the generated iteration  code is different for the first
;;;and remaining iterations. This variable will be the temporary which
;;;is the flag used in the loop to tell whether we are in the first or
;;;remaining iterations.
(defvar *loop-never-stepped-variable*)

;;;List of all the value-accumulation descriptor structures in the loop.
;;; See loop-get-collection-info.
(defvar *loop-collection-cruft*)		; for multiple COLLECTs (etc)

;;;; code analysis stuff

(defun loop-constant-fold-if-possible (form &optional expected-type)
  (let ((new-form form) (constantp nil) (constant-value nil))
    (when (setq constantp (constantp new-form))
      (setq constant-value (eval new-form)))
    (when (and constantp expected-type)
      (unless (typep constant-value expected-type)
	(loop-warn "The form ~S evaluated to ~S, which was not of the anticipated type ~S."
		   form constant-value expected-type)
	(setq constantp nil constant-value nil)))
    (values new-form constantp constant-value)))

(defun loop-constantp (form)
  (constantp form))

;;;; LOOP iteration optimization

(defvar *loop-duplicate-code*
	nil)

(defvar *loop-iteration-flag-variable*
	(make-symbol "LOOP-NOT-FIRST-TIME"))

(defun loop-code-duplication-threshold (env)
  (declare (ignore env))
  (let (;; If we could read optimization declaration information (as with
	;; the DECLARATION-INFORMATION function (present in CLTL2, removed
	;; from ANSI standard) we could set these values flexibly. Without
	;; DECLARATION-INFORMATION, we have to set them to constants.
	(speed 1)
	(space 1))
    (+ 40 (* (- speed space) 10))))

(sb!kernel:defmacro-mundanely loop-body (&environment env
					 prologue
					 before-loop
					 main-body
					 after-loop
					 epilogue
					 &aux rbefore rafter flagvar)
  (unless (= (length before-loop) (length after-loop))
    (error "LOOP-BODY called with non-synched before- and after-loop lists"))
  ;;All our work is done from these copies, working backwards from the end:
  (setq rbefore (reverse before-loop) rafter (reverse after-loop))
  (labels ((psimp (l)
	     (let ((ans nil))
	       (dolist (x l)
		 (when x
		   (push x ans)
		   (when (and (consp x)
			      (member (car x) '(go return return-from)))
		     (return nil))))
	       (nreverse ans)))
	   (pify (l) (if (null (cdr l)) (car l) `(progn ,@l)))
	   (makebody ()
	     (let ((form `(tagbody
			    ,@(psimp (append prologue (nreverse rbefore)))
			 next-loop
			    ,@(psimp (append main-body
					     (nreconc rafter
						      `((go next-loop)))))
			 end-loop
			    ,@(psimp epilogue))))
	       (if flagvar `(let ((,flagvar nil)) ,form) form))))
    (when (or *loop-duplicate-code* (not rbefore))
      (return-from loop-body (makebody)))
    ;; This outer loop iterates once for each not-first-time flag test
    ;; generated plus once more for the forms that don't need a flag test
    (do ((threshold (loop-code-duplication-threshold env))) (nil)
      (declare (fixnum threshold))
      ;; Go backwards from the ends of before-loop and after-loop merging all
      ;; the equivalent forms into the body.
      (do () ((or (null rbefore) (not (equal (car rbefore) (car rafter)))))
	(push (pop rbefore) main-body)
	(pop rafter))
      (unless rbefore (return (makebody)))
      ;; The first forms in rbefore & rafter (which are the chronologically
      ;; last forms in the list) differ, therefore they cannot be moved
      ;; into the main body. If everything that chronologically precedes
      ;; them either differs or is equal but is okay to duplicate, we can
      ;; just put all of rbefore in the prologue and all of rafter after
      ;; the body. Otherwise, there is something that is not okay to
      ;; duplicate, so it and everything chronologically after it in
      ;; rbefore and rafter must go into the body, with a flag test to
      ;; distinguish the first time around the loop from later times.
      ;; What chronologically precedes the non-duplicatable form will
      ;; be handled the next time around the outer loop.
      (do ((bb rbefore (cdr bb))
	   (aa rafter (cdr aa))
	   (lastdiff nil)
	   (count 0)
	   (inc nil))
	  ((null bb) (return-from loop-body (makebody)))	; Did it.
	(cond ((not (equal (car bb) (car aa))) (setq lastdiff bb count 0))
	      ((or (not (setq inc (estimate-code-size (car bb) env)))
		   (> (incf count inc) threshold))
	       ;; Ok, we have found a non-duplicatable piece of code.
	       ;; Everything chronologically after it must be in the central
	       ;; body. Everything chronologically at and after lastdiff goes
	       ;; into the central body under a flag test.
	       (let ((then nil) (else nil))
		 (do () (nil)
		   (push (pop rbefore) else)
		   (push (pop rafter) then)
		   (when (eq rbefore (cdr lastdiff)) (return)))
		 (unless flagvar
		   (push `(setq ,(setq flagvar *loop-iteration-flag-variable*)
				t)
			 else))
		 (push `(if ,flagvar ,(pify (psimp then)) ,(pify (psimp else)))
		       main-body))
	       ;; Everything chronologically before lastdiff until the
	       ;; non-duplicatable form (car bb) is the same in rbefore and
	       ;; rafter so just copy it into the body
	       (do () (nil)
		 (pop rafter)
		 (push (pop rbefore) main-body)
		 (when (eq rbefore (cdr bb)) (return)))
	       (return)))))))

(defun duplicatable-code-p (expr env)
  (if (null expr) 0
      (let ((ans (estimate-code-size expr env)))
	(declare (fixnum ans))
	;; @@@@ Use (DECLARATION-INFORMATION 'OPTIMIZE ENV) here to get an
	;; alist of optimize quantities back to help quantify how much code we
	;; are willing to duplicate.
	ans)))

(defvar *special-code-sizes*
	'((return 0) (progn 0)
	  (null 1) (not 1) (eq 1) (car 1) (cdr 1)
	  (when 1) (unless 1) (if 1)
	  (caar 2) (cadr 2) (cdar 2) (cddr 2)
	  (caaar 3) (caadr 3) (cadar 3) (caddr 3)
	  (cdaar 3) (cdadr 3) (cddar 3) (cdddr 3)
	  (caaaar 4) (caaadr 4) (caadar 4) (caaddr 4)
	  (cadaar 4) (cadadr 4) (caddar 4) (cadddr 4)
	  (cdaaar 4) (cdaadr 4) (cdadar 4) (cdaddr 4)
	  (cddaar 4) (cddadr 4) (cdddar 4) (cddddr 4)))

(defvar *estimate-code-size-punt*
	'(block
	   do do* dolist
	   flet
	   labels lambda let let* locally
	   macrolet multiple-value-bind
	   prog prog*
	   symbol-macrolet
	   tagbody
	   unwind-protect
	   with-open-file))

(defun destructuring-size (x)
  (do ((x x (cdr x)) (n 0 (+ (destructuring-size (car x)) n)))
      ((atom x) (+ n (if (null x) 0 1)))))

(defun estimate-code-size (x env)
  (catch 'estimate-code-size
    (estimate-code-size-1 x env)))

(defun estimate-code-size-1 (x env)
  (flet ((list-size (l)
	   (let ((n 0))
	     (declare (fixnum n))
	     (dolist (x l n) (incf n (estimate-code-size-1 x env))))))
    ;;@@@@ ???? (declare (function list-size (list) fixnum))
    (cond ((constantp x) 1)
	  ((symbolp x) (multiple-value-bind (new-form expanded-p)
			   (sb!xc:macroexpand-1 x env)
			 (if expanded-p
			     (estimate-code-size-1 new-form env)
			     1)))
	  ((atom x) 1) ;; ??? self-evaluating???
	  ((symbolp (car x))
	   (let ((fn (car x)) (tem nil) (n 0))
	     (declare (symbol fn) (fixnum n))
	     (macrolet ((f (overhead &optional (args nil args-p))
			  `(the fixnum (+ (the fixnum ,overhead)
					  (the fixnum
					       (list-size ,(if args-p
							       args
							     '(cdr x))))))))
	       (cond ((setq tem (get fn 'estimate-code-size))
		      (typecase tem
			(fixnum (f tem))
			(t (funcall tem x env))))
		     ((setq tem (assoc fn *special-code-sizes*))
		      (f (second tem)))
		     ((eq fn 'cond)
		      (dolist (clause (cdr x) n)
			(incf n (list-size clause)) (incf n)))
		     ((eq fn 'desetq)
		      (do ((l (cdr x) (cdr l))) ((null l) n)
			(setq n (+ n
				   (destructuring-size (car l))
				   (estimate-code-size-1 (cadr l) env)))))
		     ((member fn '(setq psetq))
		      (do ((l (cdr x) (cdr l))) ((null l) n)
			(setq n (+ n (estimate-code-size-1 (cadr l) env) 1))))
		     ((eq fn 'go) 1)
		     ((eq fn 'function)
		      ;; This skirts the issue of implementationally-defined
		      ;; lambda macros by recognizing CL function names and
		      ;; nothing else.
		      (if (or (symbolp (cadr x))
			      (and (consp (cadr x)) (eq (caadr x) 'setf)))
			  1
			  (throw 'duplicatable-code-p nil)))
		     ((eq fn 'multiple-value-setq)
		      (f (length (second x)) (cddr x)))
		     ((eq fn 'return-from)
		      (1+ (estimate-code-size-1 (third x) env)))
		     ((or (special-operator-p fn)
			  (member fn *estimate-code-size-punt*))
		      (throw 'estimate-code-size nil))
		     (t (multiple-value-bind (new-form expanded-p)
			    (sb!xc:macroexpand-1 x env)
			  (if expanded-p
			      (estimate-code-size-1 new-form env)
			      (f 3))))))))
	  (t (throw 'estimate-code-size nil)))))

;;;; loop errors

(defun loop-context ()
  (do ((l *loop-source-context* (cdr l)) (new nil (cons (car l) new)))
      ((eq l (cdr *loop-source-code*)) (nreverse new))))

(defun loop-error (format-string &rest format-args)
  (error "~?~%current LOOP context:~{ ~S~}."
	 format-string
	 format-args
	 (loop-context)))

(defun loop-warn (format-string &rest format-args)
  (warn "~?~%current LOOP context:~{ ~S~}."
	format-string
	format-args
	(loop-context)))

(defun loop-check-data-type (specified-type required-type
			     &optional (default-type required-type))
  (if (null specified-type)
      default-type
      (multiple-value-bind (a b) (subtypep specified-type required-type)
	(cond ((not b)
	       (loop-warn "LOOP couldn't verify that ~S is a subtype of the required type ~S."
			  specified-type required-type))
	      ((not a)
	       (loop-error "The specified data type ~S is not a subtype of ~S."
			   specified-type required-type)))
	specified-type)))

(defun loop-translate (*loop-source-code*
		       *loop-macro-environment*
		       *loop-universe*)
  (let ((*loop-original-source-code* *loop-source-code*)
	(*loop-source-context* nil)
	(*loop-iteration-variables* nil)
	(*loop-variables* nil)
	(*loop-nodeclare* nil)
	(*loop-named-variables* nil)
	(*loop-declarations* nil)
	(*loop-desetq-crocks* nil)
	(*loop-bind-stack* nil)
	(*loop-prologue* nil)
	(*loop-wrappers* nil)
	(*loop-before-loop* nil)
	(*loop-body* nil)
	(*loop-emitted-body* nil)
	(*loop-after-body* nil)
	(*loop-epilogue* nil)
	(*loop-after-epilogue* nil)
	(*loop-final-value-culprit* nil)
	(*loop-inside-conditional* nil)
	(*loop-when-it-variable* nil)
	(*loop-never-stepped-variable* nil)
	(*loop-names* nil)
	(*loop-collection-cruft* nil))
    (loop-iteration-driver)
    (loop-bind-block)
    (let ((answer `(loop-body
		     ,(nreverse *loop-prologue*)
		     ,(nreverse *loop-before-loop*)
		     ,(nreverse *loop-body*)
		     ,(nreverse *loop-after-body*)
		     ,(nreconc *loop-epilogue*
			       (nreverse *loop-after-epilogue*)))))
      (do () (nil)
	(setq answer `(block ,(pop *loop-names*) ,answer))
	(unless *loop-names* (return nil)))
      (dolist (entry *loop-bind-stack*)
	(let ((vars (first entry))
	      (dcls (second entry))
	      (crocks (third entry))
	      (wrappers (fourth entry)))
	  (dolist (w wrappers)
	    (setq answer (append w (list answer))))
	  (when (or vars dcls crocks)
	    (let ((forms (list answer)))
	      ;;(when crocks (push crocks forms))
	      (when dcls (push `(declare ,@dcls) forms))
	      (setq answer `(,(cond ((not vars) 'locally)
				    (*loop-destructuring-hooks*
				     (first *loop-destructuring-hooks*))
				    (t
				     'let))
			     ,vars
			     ,@(if crocks
				   `((destructuring-bind ,@crocks
					 ,@forms))
				 forms)))))))
      answer)))

(defun loop-iteration-driver ()
  (do () ((null *loop-source-code*))
    (let ((keyword (car *loop-source-code*)) (tem nil))
      (cond ((not (symbolp keyword))
	     (loop-error "~S found where LOOP keyword expected" keyword))
	    (t (setq *loop-source-context* *loop-source-code*)
	       (loop-pop-source)
	       (cond ((setq tem
			    (loop-lookup-keyword keyword
						 (loop-universe-keywords
						  *loop-universe*)))
		      ;; It's a "miscellaneous" toplevel LOOP keyword (DO,
		      ;; COLLECT, NAMED, etc.)
		      (apply (symbol-function (first tem)) (rest tem)))
		     ((setq tem
			    (loop-lookup-keyword keyword
						 (loop-universe-iteration-keywords *loop-universe*)))
		      (loop-hack-iteration tem))
		     ((loop-tmember keyword '(and else))
		      ;; The alternative is to ignore it, i.e. let it go
		      ;; around to the next keyword...
		      (loop-error "secondary clause misplaced at top level in LOOP macro: ~S ~S ~S ..."
				  keyword
				  (car *loop-source-code*)
				  (cadr *loop-source-code*)))
		     (t (loop-error "unknown LOOP keyword: ~S" keyword))))))))

(defun loop-pop-source ()
  (if *loop-source-code*
      (pop *loop-source-code*)
      (loop-error "LOOP source code ran out when another token was expected.")))

(defun loop-get-progn ()
  (do ((forms (list (loop-pop-source)) (cons (loop-pop-source) forms))
       (nextform (car *loop-source-code*) (car *loop-source-code*)))
      ((atom nextform)
       (if (null (cdr forms)) (car forms) (cons 'progn (nreverse forms))))))

(defun loop-get-form ()
  (if *loop-source-code*
      (loop-pop-source)
      (loop-error "LOOP code ran out where a form was expected.")))

(defun loop-construct-return (form)
  `(return-from ,(car *loop-names*) ,form))

(defun loop-pseudo-body (form)
  (cond ((or *loop-emitted-body* *loop-inside-conditional*)
	 (push form *loop-body*))
	(t (push form *loop-before-loop*) (push form *loop-after-body*))))

(defun loop-emit-body (form)
  (setq *loop-emitted-body* t)
  (loop-pseudo-body form))

(defun loop-emit-final-value (form)
  (push (loop-construct-return form) *loop-after-epilogue*)
  (when *loop-final-value-culprit*
    (loop-warn "The LOOP clause is providing a value for the iteration,~@
		however one was already established by a ~S clause."
	       *loop-final-value-culprit*))
  (setq *loop-final-value-culprit* (car *loop-source-context*)))

(defun loop-disallow-conditional (&optional kwd)
  (when *loop-inside-conditional*
    (loop-error "~:[This LOOP~;The LOOP ~:*~S~] clause is not permitted inside a conditional." kwd)))

;;;; loop types

(defun loop-typed-init (data-type)
  (when (and data-type (subtypep data-type 'number))
    (if (or (subtypep data-type 'float) (subtypep data-type '(complex float)))
	(coerce 0 data-type)
	0)))

(defun loop-optional-type (&optional variable)
  ;; No variable specified implies that no destructuring is permissible.
  (and *loop-source-code* ; Don't get confused by NILs..
       (let ((z (car *loop-source-code*)))
	 (cond ((loop-tequal z 'of-type)
		;; This is the syntactically unambigous form in that the form
		;; of the type specifier does not matter. Also, it is assumed
		;; that the type specifier is unambiguously, and without need
		;; of translation, a common lisp type specifier or pattern
		;; (matching the variable) thereof.
		(loop-pop-source)
		(loop-pop-source))

	       ((symbolp z)
		;; This is the (sort of) "old" syntax, even though we didn't
		;; used to support all of these type symbols.
		(let ((type-spec (or (gethash z
					      (loop-universe-type-symbols
					       *loop-universe*))
				     (gethash (symbol-name z)
					      (loop-universe-type-keywords
					       *loop-universe*)))))
		  (when type-spec
		    (loop-pop-source)
		    type-spec)))
	       (t
		;; This is our sort-of old syntax. But this is only valid for
		;; when we are destructuring, so we will be compulsive (should
		;; we really be?) and require that we in fact be doing variable
		;; destructuring here. We must translate the old keyword
		;; pattern typespec into a fully-specified pattern of real type
		;; specifiers here.
		(if (consp variable)
		    (unless (consp z)
		     (loop-error
			"~S found where a LOOP keyword, LOOP type keyword, or LOOP type pattern expected"
			z))
		    (loop-error "~S found where a LOOP keyword or LOOP type keyword expected" z))
		(loop-pop-source)
		(labels ((translate (k v)
			   (cond ((null k) nil)
				 ((atom k)
				  (replicate
				    (or (gethash k
						 (loop-universe-type-symbols
						  *loop-universe*))
					(gethash (symbol-name k)
						 (loop-universe-type-keywords
						  *loop-universe*))
					(loop-error
					  "The destructuring type pattern ~S contains the unrecognized type keyword ~S."
					  z k))
				    v))
				 ((atom v)
				  (loop-error
				    "The destructuring type pattern ~S doesn't match the variable pattern ~S."
				    z variable))
				 (t (cons (translate (car k) (car v))
					  (translate (cdr k) (cdr v))))))
			 (replicate (typ v)
			   (if (atom v)
			       typ
			       (cons (replicate typ (car v))
				     (replicate typ (cdr v))))))
		  (translate z variable)))))))

;;;; loop variables

(defun loop-bind-block ()
  (when (or *loop-variables* *loop-declarations* *loop-wrappers*)
    (push (list (nreverse *loop-variables*)
		*loop-declarations*
		*loop-desetq-crocks*
		*loop-wrappers*)
	  *loop-bind-stack*)
    (setq *loop-variables* nil
	  *loop-declarations* nil
	  *loop-desetq-crocks* nil
	  *loop-wrappers* nil)))

(defun loop-make-variable (name initialization dtype
			   &optional iteration-variable-p)
  (cond ((null name)
	 (cond ((not (null initialization))
		(push (list (setq name (loop-gentemp 'loop-ignore-))
			    initialization)
		      *loop-variables*)
		(push `(ignore ,name) *loop-declarations*))))
	((atom name)
	 (cond (iteration-variable-p
		(if (member name *loop-iteration-variables*)
		    (loop-error "duplicated LOOP iteration variable ~S" name)
		    (push name *loop-iteration-variables*)))
	       ((assoc name *loop-variables*)
		(loop-error "duplicated variable ~S in LOOP parallel binding"
			    name)))
	 (unless (symbolp name)
	   (loop-error "bad variable ~S somewhere in LOOP" name))
	 (loop-declare-variable name dtype)
	 ;; We use ASSOC on this list to check for duplications (above),
	 ;; so don't optimize out this list:
	 (push (list name (or initialization (loop-typed-init dtype)))
	       *loop-variables*))
	(initialization
	 (cond (*loop-destructuring-hooks*
		(loop-declare-variable name dtype)
		(push (list name initialization) *loop-variables*))
	       (t (let ((newvar (loop-gentemp 'loop-destructure-)))
		    (push (list newvar initialization) *loop-variables*)
		    ;; *LOOP-DESETQ-CROCKS* gathered in reverse order.
		    (setq *loop-desetq-crocks*
		      (list* name newvar *loop-desetq-crocks*))
		    ;; FIXME: We can delete this, right?
		    #+ignore
		    (loop-make-variable name
					nil
					dtype
					iteration-variable-p)))))
	(t (let ((tcar nil) (tcdr nil))
	     (if (atom dtype) (setq tcar (setq tcdr dtype))
		 (setq tcar (car dtype) tcdr (cdr dtype)))
	     (loop-make-variable (car name) nil tcar iteration-variable-p)
	     (loop-make-variable (cdr name) nil tcdr iteration-variable-p))))
  name)

(defun loop-make-iteration-variable (name initialization dtype)
  (loop-make-variable name initialization dtype t))

(defun loop-declare-variable (name dtype)
  (cond ((or (null name) (null dtype) (eq dtype t)) nil)
	((symbolp name)
	 (unless (or (eq dtype t) (member (the symbol name) *loop-nodeclare*))
	   (let ((dtype (let ((init (loop-typed-init dtype)))
			  (if (typep init dtype)
			    dtype
			    `(or (member ,init) ,dtype)))))
	     (push `(type ,dtype ,name) *loop-declarations*))))
	((consp name)
	 (cond ((consp dtype)
		(loop-declare-variable (car name) (car dtype))
		(loop-declare-variable (cdr name) (cdr dtype)))
	       (t (loop-declare-variable (car name) dtype)
		  (loop-declare-variable (cdr name) dtype))))
	(t (error "invalid LOOP variable passed in: ~S" name))))

(defun loop-maybe-bind-form (form data-type)
  (if (loop-constantp form)
      form
      (loop-make-variable (loop-gentemp 'loop-bind-) form data-type)))

(defun loop-do-if (for negatep)
  (let ((form (loop-get-form)) (*loop-inside-conditional* t) (it-p nil))
    (flet ((get-clause (for)
	     (do ((body nil)) (nil)
	       (let ((key (car *loop-source-code*)) (*loop-body* nil) data)
		 (cond ((not (symbolp key))
			(loop-error
			  "~S found where keyword expected getting LOOP clause after ~S"
			  key for))
		       (t (setq *loop-source-context* *loop-source-code*)
			  (loop-pop-source)
			  (when (loop-tequal (car *loop-source-code*) 'it)
			    (setq *loop-source-code*
				  (cons (or it-p
					    (setq it-p
						  (loop-when-it-variable)))
					(cdr *loop-source-code*))))
			  (cond ((or (not (setq data (loop-lookup-keyword
						       key (loop-universe-keywords *loop-universe*))))
				     (progn (apply (symbol-function (car data))
						   (cdr data))
					    (null *loop-body*)))
				 (loop-error
				   "~S does not introduce a LOOP clause that can follow ~S."
				   key for))
				(t (setq body (nreconc *loop-body* body)))))))
	       (if (loop-tequal (car *loop-source-code*) :and)
		   (loop-pop-source)
		   (return (if (cdr body)
			       `(progn ,@(nreverse body))
			       (car body)))))))
      (let ((then (get-clause for))
	    (else (when (loop-tequal (car *loop-source-code*) :else)
		    (loop-pop-source)
		    (list (get-clause :else)))))
	(when (loop-tequal (car *loop-source-code*) :end)
	  (loop-pop-source))
	(when it-p (setq form `(setq ,it-p ,form)))
	(loop-pseudo-body
	  `(if ,(if negatep `(not ,form) form)
	       ,then
	       ,@else))))))

(defun loop-do-initially ()
  (loop-disallow-conditional :initially)
  (push (loop-get-progn) *loop-prologue*))

(defun loop-do-finally ()
  (loop-disallow-conditional :finally)
  (push (loop-get-progn) *loop-epilogue*))

(defun loop-do-do ()
  (loop-emit-body (loop-get-progn)))

(defun loop-do-named ()
  (let ((name (loop-pop-source)))
    (unless (symbolp name)
      (loop-error "~S is an invalid name for your LOOP" name))
    (when (or *loop-before-loop* *loop-body* *loop-after-epilogue* *loop-inside-conditional*)
      (loop-error "The NAMED ~S clause occurs too late." name))
    (when *loop-names*
      (loop-error "You may only use one NAMED clause in your loop: NAMED ~S ... NAMED ~S."
		  (car *loop-names*) name))
    (setq *loop-names* (list name nil))))

(defun loop-do-return ()
  (loop-pseudo-body (loop-construct-return (loop-get-form))))

;;;; value accumulation: LIST

(defstruct (loop-collector
	    (:copier nil)
	    (:predicate nil))
  name
  class
  (history nil)
  (tempvars nil)
  dtype
  (data nil)) ;collector-specific data

(defun loop-get-collection-info (collector class default-type)
  (let ((form (loop-get-form))
	(dtype (and (not (loop-universe-ansi *loop-universe*)) (loop-optional-type)))
	(name (when (loop-tequal (car *loop-source-code*) 'into)
		(loop-pop-source)
		(loop-pop-source))))
    (when (not (symbolp name))
      (loop-error "The value accumulation recipient name, ~S, is not a symbol." name))
    (unless dtype
      (setq dtype (or (loop-optional-type) default-type)))
    (let ((cruft (find (the symbol name) *loop-collection-cruft*
		       :key #'loop-collector-name)))
      (cond ((not cruft)
	     (push (setq cruft (make-loop-collector
				 :name name :class class
				 :history (list collector) :dtype dtype))
		   *loop-collection-cruft*))
	    (t (unless (eq (loop-collector-class cruft) class)
		 (loop-error
		   "incompatible kinds of LOOP value accumulation specified for collecting~@
		    ~:[as the value of the LOOP~;~:*INTO ~S~]: ~S and ~S"
		   name (car (loop-collector-history cruft)) collector))
	       (unless (equal dtype (loop-collector-dtype cruft))
		 (loop-warn
		   "unequal datatypes specified in different LOOP value accumulations~@
		   into ~S: ~S and ~S"
		   name dtype (loop-collector-dtype cruft))
		 (when (eq (loop-collector-dtype cruft) t)
		   (setf (loop-collector-dtype cruft) dtype)))
	       (push collector (loop-collector-history cruft))))
      (values cruft form))))

(defun loop-list-collection (specifically)	; NCONC, LIST, or APPEND
  (multiple-value-bind (lc form)
      (loop-get-collection-info specifically 'list 'list)
    (let ((tempvars (loop-collector-tempvars lc)))
      (unless tempvars
	(setf (loop-collector-tempvars lc)
	      (setq tempvars (list* (loop-gentemp 'loop-list-head-)
				    (loop-gentemp 'loop-list-tail-)
				    (and (loop-collector-name lc)
					 (list (loop-collector-name lc))))))
	(push `(with-loop-list-collection-head ,tempvars) *loop-wrappers*)
	(unless (loop-collector-name lc)
	  (loop-emit-final-value `(loop-collect-answer ,(car tempvars)
						       ,@(cddr tempvars)))))
      (ecase specifically
	(list (setq form `(list ,form)))
	(nconc nil)
	(append (unless (and (consp form) (eq (car form) 'list))
		  (setq form `(copy-list ,form)))))
      (loop-emit-body `(loop-collect-rplacd ,tempvars ,form)))))

;;;; value accumulation: MAX, MIN, SUM, COUNT

(defun loop-sum-collection (specifically required-type default-type);SUM, COUNT
  (multiple-value-bind (lc form)
      (loop-get-collection-info specifically 'sum default-type)
    (loop-check-data-type (loop-collector-dtype lc) required-type)
    (let ((tempvars (loop-collector-tempvars lc)))
      (unless tempvars
	(setf (loop-collector-tempvars lc)
	      (setq tempvars (list (loop-make-variable
				     (or (loop-collector-name lc)
					 (loop-gentemp 'loop-sum-))
				     nil (loop-collector-dtype lc)))))
	(unless (loop-collector-name lc)
	  (loop-emit-final-value (car (loop-collector-tempvars lc)))))
      (loop-emit-body
	(if (eq specifically 'count)
	    `(when ,form
	       (setq ,(car tempvars)
		     ,(hide-variable-reference t
					       (car tempvars)
					       `(1+ ,(car tempvars)))))
	    `(setq ,(car tempvars)
		   (+ ,(hide-variable-reference t
						(car tempvars)
						(car tempvars))
		      ,form)))))))

(defun loop-maxmin-collection (specifically)
  (multiple-value-bind (lc form)
      (loop-get-collection-info specifically 'maxmin *loop-real-data-type*)
    (loop-check-data-type (loop-collector-dtype lc) *loop-real-data-type*)
    (let ((data (loop-collector-data lc)))
      (unless data
	(setf (loop-collector-data lc)
	      (setq data (make-loop-minimax
			   (or (loop-collector-name lc)
			       (loop-gentemp 'loop-maxmin-))
			   (loop-collector-dtype lc))))
	(unless (loop-collector-name lc)
	  (loop-emit-final-value (loop-minimax-answer-variable data))))
      (loop-note-minimax-operation specifically data)
      (push `(with-minimax-value ,data) *loop-wrappers*)
      (loop-emit-body `(loop-accumulate-minimax-value ,data
						      ,specifically
						      ,form)))))

;;;; value accumulation:  aggregate booleans

;;; ALWAYS and NEVER
;;;
;;; Under ANSI these are not permitted to appear under conditionalization.
(defun loop-do-always (restrictive negate)
  (let ((form (loop-get-form)))
    (when restrictive (loop-disallow-conditional))
    (loop-emit-body `(,(if negate 'when 'unless) ,form
		      ,(loop-construct-return nil)))
    (loop-emit-final-value t)))

;;; THEREIS
;;;
;;; Under ANSI this is not permitted to appear under conditionalization.
(defun loop-do-thereis (restrictive)
  (when restrictive (loop-disallow-conditional))
  (loop-emit-body `(when (setq ,(loop-when-it-variable) ,(loop-get-form))
		     ,(loop-construct-return *loop-when-it-variable*))))

(defun loop-do-while (negate kwd &aux (form (loop-get-form)))
  (loop-disallow-conditional kwd)
  (loop-pseudo-body `(,(if negate 'when 'unless) ,form (go end-loop))))

(defun loop-do-with ()
  (loop-disallow-conditional :with)
  (do ((var) (val) (dtype)) (nil)
    (setq var (loop-pop-source)
	  dtype (loop-optional-type var)
	  val (cond ((loop-tequal (car *loop-source-code*) :=)
		     (loop-pop-source)
		     (loop-get-form))
		    (t nil)))
    (loop-make-variable var val dtype)
    (if (loop-tequal (car *loop-source-code*) :and)
	(loop-pop-source)
	(return (loop-bind-block)))))

;;;; the iteration driver

(defun loop-hack-iteration (entry)
  (flet ((make-endtest (list-of-forms)
	   (cond ((null list-of-forms) nil)
		 ((member t list-of-forms) '(go end-loop))
		 (t `(when ,(if (null (cdr (setq list-of-forms
						 (nreverse list-of-forms))))
				(car list-of-forms)
				(cons 'or list-of-forms))
		       (go end-loop))))))
    (do ((pre-step-tests nil)
	 (steps nil)
	 (post-step-tests nil)
	 (pseudo-steps nil)
	 (pre-loop-pre-step-tests nil)
	 (pre-loop-steps nil)
	 (pre-loop-post-step-tests nil)
	 (pre-loop-pseudo-steps nil)
	 (tem) (data))
	(nil)
      ;; Note that we collect endtests in reverse order, but steps in correct
      ;; order. MAKE-ENDTEST does the nreverse for us.
      (setq tem (setq data
		      (apply (symbol-function (first entry)) (rest entry))))
      (and (car tem) (push (car tem) pre-step-tests))
      (setq steps (nconc steps (copy-list (car (setq tem (cdr tem))))))
      (and (car (setq tem (cdr tem))) (push (car tem) post-step-tests))
      (setq pseudo-steps
	    (nconc pseudo-steps (copy-list (car (setq tem (cdr tem))))))
      (setq tem (cdr tem))
      (when *loop-emitted-body*
	(loop-error "iteration in LOOP follows body code"))
      (unless tem (setq tem data))
      (when (car tem) (push (car tem) pre-loop-pre-step-tests))
      ;; FIXME: This (SETF FOO (NCONC FOO BAR)) idiom appears often enough
      ;; that it might be worth making it into an NCONCF macro.
      (setq pre-loop-steps
	    (nconc pre-loop-steps (copy-list (car (setq tem (cdr tem))))))
      (when (car (setq tem (cdr tem)))
	(push (car tem) pre-loop-post-step-tests))
      (setq pre-loop-pseudo-steps
	    (nconc pre-loop-pseudo-steps (copy-list (cadr tem))))
      (unless (loop-tequal (car *loop-source-code*) :and)
	(setq *loop-before-loop*
	      (list* (loop-make-desetq pre-loop-pseudo-steps)
		     (make-endtest pre-loop-post-step-tests)
		     (loop-make-psetq pre-loop-steps)
		     (make-endtest pre-loop-pre-step-tests)
		     *loop-before-loop*))
	(setq *loop-after-body*
	      (list* (loop-make-desetq pseudo-steps)
		     (make-endtest post-step-tests)
		     (loop-make-psetq steps)
		     (make-endtest pre-step-tests)
		     *loop-after-body*))
	(loop-bind-block)
	(return nil))
      (loop-pop-source)				; Flush the "AND".
      (when (and (not (loop-universe-implicit-for-required *loop-universe*))
		 (setq tem
		       (loop-lookup-keyword
			(car *loop-source-code*)
			(loop-universe-iteration-keywords *loop-universe*))))
	;; The latest ANSI clarification is that the FOR/AS after the AND must
	;; NOT be supplied.
	(loop-pop-source)
	(setq entry tem)))))

;;;; main iteration drivers

;;; FOR variable keyword ..args..
(defun loop-do-for ()
  (let* ((var (loop-pop-source))
	 (data-type (loop-optional-type var))
	 (keyword (loop-pop-source))
	 (first-arg nil)
	 (tem nil))
    (setq first-arg (loop-get-form))
    (unless (and (symbolp keyword)
		 (setq tem (loop-lookup-keyword
			     keyword
			     (loop-universe-for-keywords *loop-universe*))))
      (loop-error "~S is an unknown keyword in FOR or AS clause in LOOP."
		  keyword))
    (apply (car tem) var first-arg data-type (cdr tem))))

(defun loop-do-repeat ()
  (let ((form (loop-get-form))
	(type (loop-check-data-type (loop-optional-type)
				    *loop-real-data-type*)))
    (when (and (consp form) (eq (car form) 'the) (subtypep (second form) type))
      (setq type (second form)))
    (multiple-value-bind (number constantp value)
	(loop-constant-fold-if-possible form type)
      (cond ((and constantp (<= value 1)) `(t () () () ,(<= value 0) () () ()))
	    (t (let ((var (loop-make-variable (loop-gentemp 'loop-repeat-)
					      number
					      type)))
		 (if constantp
		     `((not (plusp (setq ,var (1- ,var))))
		       () () () () () () ())
		     `((minusp (setq ,var (1- ,var)))
		       () () ()))))))))

(defun loop-when-it-variable ()
  (or *loop-when-it-variable*
      (setq *loop-when-it-variable*
	    (loop-make-variable (loop-gentemp 'loop-it-) nil nil))))

;;;; various FOR/AS subdispatches

;;; ANSI "FOR x = y [THEN z]" is sort of like the old Genera one when the THEN
;;; is omitted (other than being more stringent in its placement), and like the
;;; old "FOR x FIRST y THEN z" when the THEN is present. I.e., the first
;;; initialization occurs in the loop body (first-step), not in the variable
;;; binding phase.
(defun loop-ansi-for-equals (var val data-type)
  (loop-make-iteration-variable var nil data-type)
  (cond ((loop-tequal (car *loop-source-code*) :then)
	 ;; Then we are the same as "FOR x FIRST y THEN z".
	 (loop-pop-source)
	 `(() (,var ,(loop-get-form)) () ()
	   () (,var ,val) () ()))
	(t ;; We are the same as "FOR x = y".
	 `(() (,var ,val) () ()))))

(defun loop-for-across (var val data-type)
  (loop-make-iteration-variable var nil data-type)
  (let ((vector-var (loop-gentemp 'loop-across-vector-))
	(index-var (loop-gentemp 'loop-across-index-)))
    (multiple-value-bind (vector-form constantp vector-value)
	(loop-constant-fold-if-possible val 'vector)
      (loop-make-variable
	vector-var vector-form
	(if (and (consp vector-form) (eq (car vector-form) 'the))
	    (cadr vector-form)
	    'vector))
      (loop-make-variable index-var 0 'fixnum)
      (let* ((length 0)
	     (length-form (cond ((not constantp)
				 (let ((v (loop-gentemp 'loop-across-limit-)))
				   (push `(setq ,v (length ,vector-var))
					 *loop-prologue*)
				   (loop-make-variable v 0 'fixnum)))
				(t (setq length (length vector-value)))))
	     (first-test `(>= ,index-var ,length-form))
	     (other-test first-test)
	     (step `(,var (aref ,vector-var ,index-var)))
	     (pstep `(,index-var (1+ ,index-var))))
	(declare (fixnum length))
	(when constantp
	  (setq first-test (= length 0))
	  (when (<= length 1)
	    (setq other-test t)))
	`(,other-test ,step () ,pstep
	  ,@(and (not (eq first-test other-test))
		 `(,first-test ,step () ,pstep)))))))

;;;; list iteration

(defun loop-list-step (listvar)
  ;; We are not equipped to analyze whether 'FOO is the same as #'FOO here in
  ;; any sensible fashion, so let's give an obnoxious warning whenever 'FOO is
  ;; used as the stepping function.
  ;;
  ;; While a Discerning Compiler may deal intelligently with
  ;; (FUNCALL 'FOO ...), not recognizing FOO may defeat some LOOP
  ;; optimizations.
  (let ((stepper (cond ((loop-tequal (car *loop-source-code*) :by)
			(loop-pop-source)
			(loop-get-form))
		       (t '(function cdr)))))
    (cond ((and (consp stepper) (eq (car stepper) 'quote))
	   (loop-warn "Use of QUOTE around stepping function in LOOP will be left verbatim.")
	   `(funcall ,stepper ,listvar))
	  ((and (consp stepper) (eq (car stepper) 'function))
	   (list (cadr stepper) listvar))
	  (t
	   `(funcall ,(loop-make-variable (loop-gentemp 'loop-fn-)
					  stepper
					  'function)
		     ,listvar)))))

(defun loop-for-on (var val data-type)
  (multiple-value-bind (list constantp list-value)
      (loop-constant-fold-if-possible val)
    (let ((listvar var))
      (cond ((and var (symbolp var))
	     (loop-make-iteration-variable var list data-type))
	    (t (loop-make-variable (setq listvar (loop-gentemp)) list 'list)
	       (loop-make-iteration-variable var nil data-type)))
      (let ((list-step (loop-list-step listvar)))
	(let* ((first-endtest
		(hide-variable-reference
		 (eq var listvar)
		 listvar
		 ;; the following should use `atom' instead of `endp', per
		 ;; [bug2428]
		 `(atom ,listvar)))
	       (other-endtest first-endtest))
	  (when (and constantp (listp list-value))
	    (setq first-endtest (null list-value)))
	  (cond ((eq var listvar)
		 ;; Contour of the loop is different because we use the user's
		 ;; variable...
		 `(() (,listvar ,(hide-variable-reference t listvar list-step))
		   ,other-endtest () () () ,first-endtest ()))
		(t (let ((step `(,var ,listvar))
			 (pseudo `(,listvar ,list-step)))
		     `(,other-endtest ,step () ,pseudo
		       ,@(and (not (eq first-endtest other-endtest))
			      `(,first-endtest ,step () ,pseudo)))))))))))

(defun loop-for-in (var val data-type)
  (multiple-value-bind (list constantp list-value)
      (loop-constant-fold-if-possible val)
    (let ((listvar (loop-gentemp 'loop-list-)))
      (loop-make-iteration-variable var nil data-type)
      (loop-make-variable listvar list 'list)
      (let ((list-step (loop-list-step listvar)))
	(let* ((first-endtest `(endp ,listvar))
	       (other-endtest first-endtest)
	       (step `(,var (car ,listvar)))
	       (pseudo-step `(,listvar ,list-step)))
	  (when (and constantp (listp list-value))
	    (setq first-endtest (null list-value)))
	  `(,other-endtest ,step () ,pseudo-step
	    ,@(and (not (eq first-endtest other-endtest))
		   `(,first-endtest ,step () ,pseudo-step))))))))

;;;; iteration paths

(defstruct (loop-path
	    (:copier nil)
	    (:predicate nil))
  names
  preposition-groups
  inclusive-permitted
  function
  user-data)

(defun add-loop-path (names function universe
		      &key preposition-groups inclusive-permitted user-data)
  (unless (listp names) (setq names (list names)))
  (check-type universe loop-universe)
  (let ((ht (loop-universe-path-keywords universe))
	(lp (make-loop-path
	      :names (mapcar #'symbol-name names)
	      :function function
	      :user-data user-data
	      :preposition-groups (mapcar (lambda (x)
					    (if (listp x) x (list x)))
					  preposition-groups)
	      :inclusive-permitted inclusive-permitted)))
    (dolist (name names)
      (setf (gethash (symbol-name name) ht) lp))
    lp))

;;; Note:  path functions are allowed to use loop-make-variable, hack
;;; the prologue, etc.
(defun loop-for-being (var val data-type)
  ;; FOR var BEING each/the pathname prep-phrases using-stuff... each/the =
  ;; EACH or THE. Not clear if it is optional, so I guess we'll warn.
  (let ((path nil)
	(data nil)
	(inclusive nil)
	(stuff nil)
	(initial-prepositions nil))
    (cond ((loop-tmember val '(:each :the)) (setq path (loop-pop-source)))
	  ((loop-tequal (car *loop-source-code*) :and)
	   (loop-pop-source)
	   (setq inclusive t)
	   (unless (loop-tmember (car *loop-source-code*)
				 '(:its :each :his :her))
	     (loop-error "~S was found where ITS or EACH expected in LOOP iteration path syntax."
			 (car *loop-source-code*)))
	   (loop-pop-source)
	   (setq path (loop-pop-source))
	   (setq initial-prepositions `((:in ,val))))
	  (t (loop-error "unrecognizable LOOP iteration path syntax: missing EACH or THE?")))
    (cond ((not (symbolp path))
	   (loop-error
	    "~S was found where a LOOP iteration path name was expected."
	    path))
	  ((not (setq data (loop-lookup-keyword path (loop-universe-path-keywords *loop-universe*))))
	   (loop-error "~S is not the name of a LOOP iteration path." path))
	  ((and inclusive (not (loop-path-inclusive-permitted data)))
	   (loop-error "\"Inclusive\" iteration is not possible with the ~S LOOP iteration path." path)))
    (let ((fun (loop-path-function data))
	  (preps (nconc initial-prepositions
			(loop-collect-prepositional-phrases
			 (loop-path-preposition-groups data)
			 t)))
	  (user-data (loop-path-user-data data)))
      (when (symbolp fun) (setq fun (symbol-function fun)))
      (setq stuff (if inclusive
		      (apply fun var data-type preps :inclusive t user-data)
		      (apply fun var data-type preps user-data))))
    (when *loop-named-variables*
      (loop-error "Unused USING variables: ~S." *loop-named-variables*))
    ;; STUFF is now (bindings prologue-forms . stuff-to-pass-back). Protect the
    ;; system from the user and the user from himself.
    (unless (member (length stuff) '(6 10))
      (loop-error "Value passed back by LOOP iteration path function for path ~S has invalid length."
		  path))
    (do ((l (car stuff) (cdr l)) (x)) ((null l))
      (if (atom (setq x (car l)))
	  (loop-make-iteration-variable x nil nil)
	  (loop-make-iteration-variable (car x) (cadr x) (caddr x))))
    (setq *loop-prologue* (nconc (reverse (cadr stuff)) *loop-prologue*))
    (cddr stuff)))

(defun named-variable (name)
  (let ((tem (loop-tassoc name *loop-named-variables*)))
    (declare (list tem))
    (cond ((null tem) (values (loop-gentemp) nil))
	  (t (setq *loop-named-variables* (delete tem *loop-named-variables*))
	     (values (cdr tem) t)))))

(defun loop-collect-prepositional-phrases (preposition-groups
					   &optional
					   USING-allowed
					   initial-phrases)
  (flet ((in-group-p (x group) (car (loop-tmember x group))))
    (do ((token nil)
	 (prepositional-phrases initial-phrases)
	 (this-group nil nil)
	 (this-prep nil nil)
	 (disallowed-prepositions
	   (mapcan #'(lambda (x)
		       (copy-list
			 (find (car x) preposition-groups :test #'in-group-p)))
		   initial-phrases))
	 (used-prepositions (mapcar #'car initial-phrases)))
	((null *loop-source-code*) (nreverse prepositional-phrases))
      (declare (symbol this-prep))
      (setq token (car *loop-source-code*))
      (dolist (group preposition-groups)
	(when (setq this-prep (in-group-p token group))
	  (return (setq this-group group))))
      (cond (this-group
	     (when (member this-prep disallowed-prepositions)
	       (loop-error
		 (if (member this-prep used-prepositions)
		     "A ~S prepositional phrase occurs multiply for some LOOP clause."
		     "Preposition ~S was used when some other preposition has subsumed it.")
		 token))
	     (setq used-prepositions (if (listp this-group)
					 (append this-group used-prepositions)
					 (cons this-group used-prepositions)))
	     (loop-pop-source)
	     (push (list this-prep (loop-get-form)) prepositional-phrases))
	    ((and USING-allowed (loop-tequal token 'using))
	     (loop-pop-source)
	     (do ((z (loop-pop-source) (loop-pop-source)) (tem)) (nil)
	       (when (or (atom z)
			 (atom (cdr z))
			 (not (null (cddr z)))
			 (not (symbolp (car z)))
			 (and (cadr z) (not (symbolp (cadr z)))))
		 (loop-error "~S bad variable pair in path USING phrase" z))
	       (when (cadr z)
		 (if (setq tem (loop-tassoc (car z) *loop-named-variables*))
		     (loop-error
		       "The variable substitution for ~S occurs twice in a USING phrase,~@
			with ~S and ~S."
		       (car z) (cadr z) (cadr tem))
		     (push (cons (car z) (cadr z)) *loop-named-variables*)))
	       (when (or (null *loop-source-code*)
			 (symbolp (car *loop-source-code*)))
		 (return nil))))
	    (t (return (nreverse prepositional-phrases)))))))

;;;; master sequencer function

(defun loop-sequencer (indexv indexv-type indexv-user-specified-p
			  variable variable-type
			  sequence-variable sequence-type
			  step-hack default-top
			  prep-phrases)
   (let ((endform nil) ; Form (constant or variable) with limit value
	 (sequencep nil) ; T if sequence arg has been provided
	 (testfn nil) ; endtest function
	 (test nil) ; endtest form
	 (stepby (1+ (or (loop-typed-init indexv-type) 0))) ; our increment
	 (stepby-constantp t)
	 (step nil) ; step form
	 (dir nil) ; direction of stepping: NIL, :UP, :DOWN
	 (inclusive-iteration nil) ; T if include last index
	 (start-given nil) ; T when prep phrase has specified start
	 (start-value nil)
	 (start-constantp nil)
	 (limit-given nil) ; T when prep phrase has specified end
	 (limit-constantp nil)
	 (limit-value nil)
	 )
     (when variable (loop-make-iteration-variable variable nil variable-type))
     (do ((l prep-phrases (cdr l)) (prep) (form) (odir)) ((null l))
       (setq prep (caar l) form (cadar l))
       (case prep
	 ((:of :in)
	  (setq sequencep t)
	  (loop-make-variable sequence-variable form sequence-type))
	 ((:from :downfrom :upfrom)
	  (setq start-given t)
	  (cond ((eq prep :downfrom) (setq dir ':down))
		((eq prep :upfrom) (setq dir ':up)))
	  (multiple-value-setq (form start-constantp start-value)
	    (loop-constant-fold-if-possible form indexv-type))
	  (loop-make-iteration-variable indexv form indexv-type))
	 ((:upto :to :downto :above :below)
	  (cond ((loop-tequal prep :upto) (setq inclusive-iteration
						(setq dir ':up)))
		((loop-tequal prep :to) (setq inclusive-iteration t))
		((loop-tequal prep :downto) (setq inclusive-iteration
						  (setq dir ':down)))
		((loop-tequal prep :above) (setq dir ':down))
		((loop-tequal prep :below) (setq dir ':up)))
	  (setq limit-given t)
	  (multiple-value-setq (form limit-constantp limit-value)
	    (loop-constant-fold-if-possible form indexv-type))
	  (setq endform (if limit-constantp
			    `',limit-value
			    (loop-make-variable
			      (loop-gentemp 'loop-limit-) form indexv-type))))
	 (:by
	   (multiple-value-setq (form stepby-constantp stepby)
	     (loop-constant-fold-if-possible form indexv-type))
	   (unless stepby-constantp
	     (loop-make-variable (setq stepby (loop-gentemp 'loop-step-by-))
				 form
				 indexv-type)))
	 (t (loop-error
	      "~S invalid preposition in sequencing or sequence path;~@
	       maybe invalid prepositions were specified in iteration path descriptor?"
	      prep)))
       (when (and odir dir (not (eq dir odir)))
	 (loop-error "conflicting stepping directions in LOOP sequencing path"))
       (setq odir dir))
     (when (and sequence-variable (not sequencep))
       (loop-error "missing OF or IN phrase in sequence path"))
     ;; Now fill in the defaults.
     (unless start-given
       (loop-make-iteration-variable
	 indexv
	 (setq start-constantp t
	       start-value (or (loop-typed-init indexv-type) 0))
	 indexv-type))
     (cond ((member dir '(nil :up))
	    (when (or limit-given default-top)
	      (unless limit-given
		(loop-make-variable (setq endform
					  (loop-gentemp 'loop-seq-limit-))
				    nil indexv-type)
		(push `(setq ,endform ,default-top) *loop-prologue*))
	      (setq testfn (if inclusive-iteration '> '>=)))
	    (setq step (if (eql stepby 1) `(1+ ,indexv) `(+ ,indexv ,stepby))))
	   (t (unless start-given
		(unless default-top
		  (loop-error "don't know where to start stepping"))
		(push `(setq ,indexv (1- ,default-top)) *loop-prologue*))
	      (when (and default-top (not endform))
		(setq endform (loop-typed-init indexv-type)
		      inclusive-iteration t))
	      (when endform (setq testfn (if inclusive-iteration  '< '<=)))
	      (setq step
		    (if (eql stepby 1) `(1- ,indexv) `(- ,indexv ,stepby)))))
     (when testfn
       (setq test
	     (hide-variable-reference t indexv `(,testfn ,indexv ,endform))))
     (when step-hack
       (setq step-hack
	     `(,variable ,(hide-variable-reference indexv-user-specified-p
						   indexv
						   step-hack))))
     (let ((first-test test) (remaining-tests test))
       (when (and stepby-constantp start-constantp limit-constantp)
	 (when (setq first-test
		     (funcall (symbol-function testfn)
			      start-value
			      limit-value))
	   (setq remaining-tests t)))
       `(() (,indexv ,(hide-variable-reference t indexv step))
	 ,remaining-tests ,step-hack () () ,first-test ,step-hack))))

;;;; interfaces to the master sequencer

(defun loop-for-arithmetic (var val data-type kwd)
  (loop-sequencer
    var (loop-check-data-type data-type *loop-real-data-type*) t
    nil nil nil nil nil nil
    (loop-collect-prepositional-phrases
      '((:from :upfrom :downfrom) (:to :upto :downto :above :below) (:by))
      nil (list (list kwd val)))))

(defun loop-sequence-elements-path (variable data-type prep-phrases
				    &key
				    fetch-function
				    size-function
				    sequence-type
				    element-type)
  (multiple-value-bind (indexv indexv-user-specified-p) (named-variable 'index)
    (let ((sequencev (named-variable 'sequence)))
      (list* nil nil				; dummy bindings and prologue
	     (loop-sequencer
	       indexv 'fixnum indexv-user-specified-p
	       variable (or data-type element-type)
	       sequencev sequence-type
	       `(,fetch-function ,sequencev ,indexv)
	       `(,size-function ,sequencev)
	       prep-phrases)))))

;;;; builtin LOOP iteration paths

#||
(loop for v being the hash-values of ht do (print v))
(loop for k being the hash-keys of ht do (print k))
(loop for v being the hash-values of ht using (hash-key k) do (print (list k v)))
(loop for k being the hash-keys of ht using (hash-value v) do (print (list k v)))
||#

(defun loop-hash-table-iteration-path (variable data-type prep-phrases
				       &key which)
  (check-type which (member hash-key hash-value))
  (cond ((or (cdr prep-phrases) (not (member (caar prep-phrases) '(:in :of))))
	 (loop-error "Too many prepositions!"))
	((null prep-phrases)
	 (loop-error "missing OF or IN in ~S iteration path")))
  (let ((ht-var (loop-gentemp 'loop-hashtab-))
	(next-fn (loop-gentemp 'loop-hashtab-next-))
	(dummy-predicate-var nil)
	(post-steps nil))
    (multiple-value-bind (other-var other-p)
	(named-variable (if (eq which 'hash-key) 'hash-value 'hash-key))
      ;; @@@@ named-variable returns a second value of T if the name was
      ;; actually specified, so clever code can throw away the gensym'ed up
      ;; variable if it isn't really needed. The following is for those
      ;; implementations in which we cannot put dummy NILs into
      ;; multiple-value-setq variable lists.
      (setq other-p t
	    dummy-predicate-var (loop-when-it-variable))
      (let ((key-var nil)
	    (val-var nil)
	    (bindings `((,variable nil ,data-type)
			(,ht-var ,(cadar prep-phrases))
			,@(and other-p other-var `((,other-var nil))))))
	(if (eq which 'hash-key)
	    (setq key-var variable val-var (and other-p other-var))
	    (setq key-var (and other-p other-var) val-var variable))
	(push `(with-hash-table-iterator (,next-fn ,ht-var)) *loop-wrappers*)
	(when (consp key-var)
	  (setq post-steps
		`(,key-var ,(setq key-var (loop-gentemp 'loop-hash-key-temp-))
			   ,@post-steps))
	  (push `(,key-var nil) bindings))
	(when (consp val-var)
	  (setq post-steps
		`(,val-var ,(setq val-var (loop-gentemp 'loop-hash-val-temp-))
			   ,@post-steps))
	  (push `(,val-var nil) bindings))
	`(,bindings				;bindings
	  ()					;prologue
	  ()					;pre-test
	  ()					;parallel steps
	  (not (multiple-value-setq (,dummy-predicate-var ,key-var ,val-var)
		 (,next-fn)))	;post-test
	  ,post-steps)))))

(defun loop-package-symbols-iteration-path (variable data-type prep-phrases
					    &key symbol-types)
  (cond ((or (cdr prep-phrases) (not (member (caar prep-phrases) '(:in :of))))
	 (loop-error "Too many prepositions!"))
	((null prep-phrases)
	 (loop-error "missing OF or IN in ~S iteration path")))
  (unless (symbolp variable)
    (loop-error "Destructuring is not valid for package symbol iteration."))
  (let ((pkg-var (loop-gentemp 'loop-pkgsym-))
	(next-fn (loop-gentemp 'loop-pkgsym-next-)))
    (push `(with-package-iterator (,next-fn ,pkg-var ,@symbol-types))
	  *loop-wrappers*)
    `(((,variable nil ,data-type) (,pkg-var ,(cadar prep-phrases)))
      ()
      ()
      ()
      (not (multiple-value-setq (,(loop-when-it-variable)
				 ,variable)
	     (,next-fn)))
      ())))

;;;; ANSI LOOP

(defun make-ansi-loop-universe (extended-p)
  (let ((w (make-standard-loop-universe
	     :keywords `((named (loop-do-named))
			 (initially (loop-do-initially))
			 (finally (loop-do-finally))
			 (do (loop-do-do))
			 (doing (loop-do-do))
			 (return (loop-do-return))
			 (collect (loop-list-collection list))
			 (collecting (loop-list-collection list))
			 (append (loop-list-collection append))
			 (appending (loop-list-collection append))
			 (nconc (loop-list-collection nconc))
			 (nconcing (loop-list-collection nconc))
			 (count (loop-sum-collection count
						     ,*loop-real-data-type*
						     fixnum))
			 (counting (loop-sum-collection count
							,*loop-real-data-type*
							fixnum))
			 (sum (loop-sum-collection sum number number))
			 (summing (loop-sum-collection sum number number))
			 (maximize (loop-maxmin-collection max))
			 (minimize (loop-maxmin-collection min))
			 (maximizing (loop-maxmin-collection max))
			 (minimizing (loop-maxmin-collection min))
			 (always (loop-do-always t nil)) ; Normal, do always
			 (never (loop-do-always t t)) ; Negate test on always.
			 (thereis (loop-do-thereis t))
			 (while (loop-do-while nil :while)) ; Normal, do while
			 (until (loop-do-while t :until)) ;Negate test on while
			 (when (loop-do-if when nil))	; Normal, do when
			 (if (loop-do-if if nil))	; synonymous
			 (unless (loop-do-if unless t))	; Negate test on when
			 (with (loop-do-with)))
	     :for-keywords '((= (loop-ansi-for-equals))
			     (across (loop-for-across))
			     (in (loop-for-in))
			     (on (loop-for-on))
			     (from (loop-for-arithmetic :from))
			     (downfrom (loop-for-arithmetic :downfrom))
			     (upfrom (loop-for-arithmetic :upfrom))
			     (below (loop-for-arithmetic :below))
			     (to (loop-for-arithmetic :to))
			     (upto (loop-for-arithmetic :upto))
			     (being (loop-for-being)))
	     :iteration-keywords '((for (loop-do-for))
				   (as (loop-do-for))
				   (repeat (loop-do-repeat)))
	     :type-symbols '(array atom bignum bit bit-vector character
			     compiled-function complex cons double-float
			     fixnum float function hash-table integer
			     keyword list long-float nil null number
			     package pathname random-state ratio rational
			     readtable sequence short-float simple-array
			     simple-bit-vector simple-string simple-vector
			     single-float standard-char stream string
			     base-char symbol t vector)
	     :type-keywords nil
	     :ansi (if extended-p :extended t))))
    (add-loop-path '(hash-key hash-keys) 'loop-hash-table-iteration-path w
		   :preposition-groups '((:of :in))
		   :inclusive-permitted nil
		   :user-data '(:which hash-key))
    (add-loop-path '(hash-value hash-values) 'loop-hash-table-iteration-path w
		   :preposition-groups '((:of :in))
		   :inclusive-permitted nil
		   :user-data '(:which hash-value))
    (add-loop-path '(symbol symbols) 'loop-package-symbols-iteration-path w
		   :preposition-groups '((:of :in))
		   :inclusive-permitted nil
		   :user-data '(:symbol-types (:internal
					       :external
					       :inherited)))
    (add-loop-path '(external-symbol external-symbols)
		   'loop-package-symbols-iteration-path w
		   :preposition-groups '((:of :in))
		   :inclusive-permitted nil
		   :user-data '(:symbol-types (:external)))
    (add-loop-path '(present-symbol present-symbols)
		   'loop-package-symbols-iteration-path w
		   :preposition-groups '((:of :in))
		   :inclusive-permitted nil
		   :user-data '(:symbol-types (:internal)))
    w))

(defparameter *loop-ansi-universe*
  (make-ansi-loop-universe nil))

(defun loop-standard-expansion (keywords-and-forms environment universe)
  (if (and keywords-and-forms (symbolp (car keywords-and-forms)))
    (loop-translate keywords-and-forms environment universe)
    (let ((tag (gensym)))
      `(block nil (tagbody ,tag (progn ,@keywords-and-forms) (go ,tag))))))

(sb!kernel:defmacro-mundanely loop (&environment env &rest keywords-and-forms)
  (loop-standard-expansion keywords-and-forms env *loop-ansi-universe*))

(sb!kernel:defmacro-mundanely loop-finish ()
  #!+sb-doc
  "Causes the iteration to terminate \"normally\", the same as implicit
termination by an iteration driving clause, or by use of WHILE or
UNTIL -- the epilogue code (if any) will be run, and any implicitly
collected result will be returned as the value of the LOOP."
  '(go end-loop))
