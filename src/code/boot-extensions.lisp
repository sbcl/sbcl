;;;; extensions which are needed in order to (cross-)compile target-only code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!EXT")

;;; Lots of code wants to get to the KEYWORD package or the
;;; COMMON-LISP package without a lot of fuss, so we cache them in
;;; variables. TO DO: How much does this actually buy us? It sounds
;;; sensible, but I don't know for sure that it saves space or time..
;;; -- WHN 19990521
;;;
;;; (The initialization forms here only matter on the cross-compilation
;;; host; In the target SBCL, these variables are set in cold init.)
(declaim (type package *cl-package* *keyword-package*))
(defvar *cl-package*      (find-package "COMMON-LISP"))
(defvar *keyword-package* (find-package "KEYWORD"))

;;; a helper function for various macros which expect clauses of a
;;; given length, etc.
;;;
;;; FIXME: This implementation will hang on circular list structure.
;;; Since this is an error-checking utility, i.e. its job is to deal
;;; with screwed-up input, it'd be good style to fix it so that it can
;;; deal with circular list structure.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Return true if X is a proper list whose length is between MIN and
  ;; MAX (inclusive).
  (defun proper-list-of-length-p (x min &optional (max min))
    (cond ((minusp max)
	   nil)
	  ((null x)
	   (zerop min))
	  ((consp x)
	   (and (plusp max)
		(proper-list-of-length-p (cdr x)
					 (if (plusp (1- min))
					   (1- min)
					   0)
					 (1- max))))
	  (t nil))))

;;;; the COLLECT macro

;;; helper functions for COLLECT, which become the expanders of the
;;; MACROLET definitions created by COLLECT
;;;
;;; COLLECT-NORMAL-EXPANDER handles normal collection macros.
;;;
;;; COLLECT-LIST-EXPANDER handles the list collection case. N-TAIL
;;; is the pointer to the current tail of the list, or NIL if the list
;;; is empty.
(defun collect-normal-expander (n-value fun forms)
  `(progn
    ,@(mapcar #'(lambda (form) `(setq ,n-value (,fun ,form ,n-value))) forms)
    ,n-value))
(defun collect-list-expander (n-value n-tail forms)
  (let ((n-res (gensym)))
    `(progn
      ,@(mapcar #'(lambda (form)
		    `(let ((,n-res (cons ,form nil)))
		       (cond (,n-tail
			      (setf (cdr ,n-tail) ,n-res)
			      (setq ,n-tail ,n-res))
			     (t
			      (setq ,n-tail ,n-res  ,n-value ,n-res)))))
		forms)
      ,n-value)))

;;; the ultimate collection macro...
(defmacro collect (collections &body body)
  #!+sb-doc
  "Collect ({(Name [Initial-Value] [Function])}*) {Form}*
  Collect some values somehow. Each of the collections specifies a bunch of
  things which collected during the evaluation of the body of the form. The
  name of the collection is used to define a local macro, a la MACROLET.
  Within the body, this macro will evaluate each of its arguments and collect
  the result, returning the current value after the collection is done. The
  body is evaluated as a PROGN; to get the final values when you are done, just
  call the collection macro with no arguments.

  INITIAL-VALUE is the value that the collection starts out with, which
  defaults to NIL. FUNCTION is the function which does the collection. It is
  a function which will accept two arguments: the value to be collected and the
  current collection. The result of the function is made the new value for the
  collection. As a totally magical special-case, FUNCTION may be COLLECT,
  which tells us to build a list in forward order; this is the default. If an
  INITIAL-VALUE is supplied for Collect, the stuff will be RPLACD'd onto the
  end. Note that FUNCTION may be anything that can appear in the functional
  position, including macros and lambdas."

  (let ((macros ())
	(binds ()))
    (dolist (spec collections)
      (unless (proper-list-of-length-p spec 1 3)
	(error "Malformed collection specifier: ~S." spec))
      (let* ((name (first spec))
	     (default (second spec))
	     (kind (or (third spec) 'collect))
	     (n-value (gensym (concatenate 'string
					   (symbol-name name)
					   "-N-VALUE-"))))
	(push `(,n-value ,default) binds)
	(if (eq kind 'collect)
	  (let ((n-tail (gensym (concatenate 'string
					     (symbol-name name)
					     "-N-TAIL-"))))
	    (if default
	      (push `(,n-tail (last ,n-value)) binds)
	      (push n-tail binds))
	    (push `(,name (&rest args)
		     (collect-list-expander ',n-value ',n-tail args))
		  macros))
	  (push `(,name (&rest args)
		   (collect-normal-expander ',n-value ',kind args))
		macros))))
    `(macrolet ,macros (let* ,(nreverse binds) ,@body))))

(declaim (ftype (function () nil) required-argument))
(defun required-argument ()
  #!+sb-doc
  "This function can be used as the default value for keyword arguments that
  must be always be supplied. Since it is known by the compiler to never
  return, it will avoid any compile-time type warnings that would result from a
  default value inconsistent with the declared type. When this function is
  called, it signals an error indicating that a required keyword argument was
  not supplied. This function is also useful for DEFSTRUCT slot defaults
  corresponding to required arguments."
  (/show0 "entering REQUIRED-ARGUMENT")
  (error "A required keyword argument was not supplied."))

;;; "the ultimate iteration macro"
;;;
;;; note for Schemers: This seems to be identical to Scheme's "named LET".
(defmacro iterate (name binds &body body)
  #!+sb-doc
  "Iterate Name ({(Var Initial-Value)}*) Declaration* Form*
  This is syntactic sugar for Labels. It creates a local function Name with
  the specified Vars as its arguments and the Declarations and Forms as its
  body. This function is then called with the Initial-Values, and the result
  of the call is returned from the macro."
  (dolist (x binds)
    (unless (proper-list-of-length-p x 2)
      (error "Malformed ITERATE variable spec: ~S." x)))
  `(labels ((,name ,(mapcar #'first binds) ,@body))
     (,name ,@(mapcar #'second binds))))

;;; ONCE-ONLY is a utility useful in writing source transforms and
;;; macros. It provides a concise way to wrap a LET around some code
;;; to ensure that some forms are only evaluated once.
(defmacro once-only (specs &body body)
  #!+sb-doc
  "Once-Only ({(Var Value-Expression)}*) Form*
  Create a Let* which evaluates each Value-Expression, binding a temporary
  variable to the result, and wrapping the Let* around the result of the
  evaluation of Body. Within the body, each Var is bound to the corresponding
  temporary variable."
  (iterate frob
	   ((specs specs)
	    (body body))
    (if (null specs)
	`(progn ,@body)
	(let ((spec (first specs)))
	  ;; FIXME: should just be DESTRUCTURING-BIND of SPEC
	  (unless (proper-list-of-length-p spec 2)
	    (error "malformed ONCE-ONLY binding spec: ~S" spec))
	  (let* ((name (first spec))
		 (exp-temp (gensym (symbol-name name))))
	    `(let ((,exp-temp ,(second spec))
		   (,name (gensym "OO-")))
	       `(let ((,,name ,,exp-temp))
		  ,,(frob (rest specs) body))))))))

;;;; some old-fashioned functions. (They're not just for old-fashioned
;;;; code, they're also used as optimized forms of the corresponding
;;;; general functions when the compiler can prove that they're
;;;; equivalent.)

;;; like (MEMBER ITEM LIST :TEST #'EQ)
(defun memq (item list)
  #!+sb-doc
  "Returns tail of LIST beginning with first element EQ to ITEM."
  ;; KLUDGE: These could be and probably should be defined as
  ;;   (MEMBER ITEM LIST :TEST #'EQ)),
  ;; but when I try to cross-compile that, I get an error from
  ;; LTN-ANALYZE-KNOWN-CALL, "Recursive known function definition". The
  ;; comments for that error say it "is probably a botched interpreter stub".
  ;; Rather than try to figure that out, I just rewrote this function from
  ;; scratch. -- WHN 19990512
  (do ((i list (cdr i)))
      ((null i))
    (when (eq (car i) item)
      (return i))))

;;; like (ASSOC ITEM ALIST :TEST #'EQ)
(defun assq (item alist)
  #!+sb-doc
  "Return the first pair of ALIST where ITEM is EQ to the key of the pair."
  ;; KLUDGE: CMU CL defined this with
  ;;   (DECLARE (INLINE ASSOC))
  ;;   (ASSOC ITEM ALIST :TEST #'EQ))
  ;; which is pretty, but which would have required adding awkward
  ;; build order constraints on SBCL (or figuring out some way to make
  ;; inline definitions installable at build-the-cross-compiler time,
  ;; which was too ambitious for now). Rather than mess with that, we
  ;; just define ASSQ explicitly in terms of more primitive
  ;; operations:
  (dolist (pair alist)
    (when (eq (car pair) item)
      (return pair))))

(defun delq (item list)
  #!+sb-doc
  "Delete all LIST entries EQ to ITEM (destructively modifying LIST), and
  return the modified LIST."
  (let ((list list))
    (do ((x list (cdr x))
	 (splice '()))
	((endp x) list)
      (cond ((eq item (car x))
	     (if (null splice)
	       (setq list (cdr x))
	       (rplacd splice (cdr x))))
	    (t (setq splice x)))))) ; Move splice along to include element.
