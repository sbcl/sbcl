;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

#|

The CommonLoops evaluator is meta-circular.

Most of the code in PCL is methods on generic functions, including
most of the code that actually implements generic functions and method
lookup.

So, we have a classic bootstrapping problem. The solution to this is
to first get a cheap implementation of generic functions running,
these are called early generic functions. These early generic
functions and the corresponding early methods and early method lookup
are used to get enough of the system running that it is possible to
create real generic functions and methods and implement real method
lookup. At that point (done in the file FIXUP) the function
!FIX-EARLY-GENERIC-FUNCTIONS is called to convert all the early generic
functions to real generic functions.

The cheap generic functions are built using the same
FUNCALLABLE-INSTANCE objects that real generic functions are made out of.
This means that as PCL is being bootstrapped, the cheap generic
function objects which are being created are the same objects which
will later be real generic functions. This is good because:
  - we don't cons garbage structure, and
  - we can keep pointers to the cheap generic function objects
    during booting because those pointers will still point to
    the right object after the generic functions are all fixed up.

This file defines the DEFMETHOD macro and the mechanism used to expand
it. This includes the mechanism for processing the body of a method.
DEFMETHOD basically expands into a call to LOAD-DEFMETHOD, which
basically calls ADD-METHOD to add the method to the generic function.
These expansions can be loaded either during bootstrapping or when PCL
is fully up and running.

An important effect of this arrangement is it means we can compile
files with DEFMETHOD forms in them in a completely running PCL, but
then load those files back in during bootstrapping. This makes
development easier. It also means there is only one set of code for
processing DEFMETHOD. Bootstrapping works by being sure to have
LOAD-METHOD be careful to call only primitives which work during
bootstrapping.

|#

;;; FIXME: As of sbcl-0.6.9.10, PCL still uses this nonstandard type
;;; of declaration internally. It would be good to figure out how to
;;; get rid of it, or failing that, (1) document why it's needed and
;;; (2) use a private symbol with a forbidding name which suggests
;;; it's not to be messed with by the user (e.g. SB-PCL:%CLASS)
;;; instead of the too-inviting CLASS. (I tried just deleting the
;;; declarations in MAKE-METHOD-LAMBDA-INTERNAL ca. sbcl-0.6.9.10, but
;;; then things break.)
(declaim (declaration class))

(declaim (notinline make-a-method
		    add-named-method
		    ensure-generic-function-using-class
		    add-method
		    remove-method))

(defvar *!early-functions*
	'((make-a-method early-make-a-method
			 real-make-a-method)
	  (add-named-method early-add-named-method
			    real-add-named-method)
	  ))

;;; For each of the early functions, arrange to have it point to its
;;; early definition. Do this in a way that makes sure that if we
;;; redefine one of the early definitions the redefinition will take
;;; effect. This makes development easier.
(dolist (fns *!early-functions*)
  (let ((name (car fns))
	(early-name (cadr fns)))
    (setf (gdefinition name)
            (set-fun-name
             (lambda (&rest args)
	       (apply (fdefinition early-name) args))
             name))))

;;; *!GENERIC-FUNCTION-FIXUPS* is used by !FIX-EARLY-GENERIC-FUNCTIONS
;;; to convert the few functions in the bootstrap which are supposed
;;; to be generic functions but can't be early on.
(defvar *!generic-function-fixups*
  '((add-method
     ((generic-function method)	 ;lambda-list
      (standard-generic-function method) ;specializers
      real-add-method))		 ;method-function
    (remove-method
     ((generic-function method)
      (standard-generic-function method)
      real-remove-method))
    (get-method
     ((generic-function qualifiers specializers &optional (errorp t))
      (standard-generic-function t t)
      real-get-method))
    (ensure-generic-function-using-class
     ((generic-function fun-name
			&key generic-function-class environment
			&allow-other-keys)
      (generic-function t)
      real-ensure-gf-using-class--generic-function)
     ((generic-function fun-name
			&key generic-function-class environment
			&allow-other-keys)
      (null t)
      real-ensure-gf-using-class--null))
    (make-method-lambda
     ((proto-generic-function proto-method lambda-expression environment)
      (standard-generic-function standard-method t t)
      real-make-method-lambda))
    (make-method-initargs-form
     ((proto-generic-function proto-method
			      lambda-expression
			      lambda-list environment)
      (standard-generic-function standard-method t t t)
      real-make-method-initargs-form))
    (compute-effective-method
     ((generic-function combin applicable-methods)
      (generic-function standard-method-combination t)
      standard-compute-effective-method))))

(defmacro defgeneric (fun-name lambda-list &body options)
  (declare (type list lambda-list))
  (unless (legal-fun-name-p fun-name)
    (error 'simple-program-error
	   :format-control "illegal generic function name ~S"
	   :format-arguments (list fun-name)))
  (check-gf-lambda-list lambda-list)
  (let ((initargs ())
	(methods ()))
    (flet ((duplicate-option (name)
	     (error 'simple-program-error
		    :format-control "The option ~S appears more than once."
		    :format-arguments (list name)))
	   (expand-method-definition (qab) ; QAB = qualifiers, arglist, body
	     (let* ((arglist-pos (position-if #'listp qab))
		    (arglist (elt qab arglist-pos))
		    (qualifiers (subseq qab 0 arglist-pos))
		    (body (nthcdr (1+ arglist-pos) qab)))
	       `(push (defmethod ,fun-name ,@qualifiers ,arglist ,@body)
                      (generic-function-initial-methods #',fun-name)))))
      (macrolet ((initarg (key) `(getf initargs ,key)))
	(dolist (option options)
	  (let ((car-option (car option)))
	    (case car-option
	      (declare
	       (when (and
		      (consp (cadr option))
		      (member (first (cadr option))
			      ;; FIXME: this list is slightly weird.
			      ;; ANSI (on the DEFGENERIC page) in one
			      ;; place allows only OPTIMIZE; in
			      ;; another place gives this list of
			      ;; disallowed declaration specifiers.
			      ;; This seems to be the only place where
			      ;; the FUNCTION declaration is
			      ;; mentioned; TYPE seems to be missing.
			      ;; Very strange.  -- CSR, 2002-10-21
			      '(declaration ftype function
				inline notinline special)))
		 (error 'simple-program-error
			:format-control "The declaration specifier ~S ~
                                         is not allowed inside DEFGENERIC."
			:format-arguments (list (cadr option))))
	       (push (cadr option) (initarg :declarations)))
	      (:method-combination
	       (when (initarg car-option)
		 (duplicate-option car-option))
	       (unless (symbolp (cadr option))
		 (error 'simple-program-error
			:format-control "METHOD-COMBINATION name not a ~
                                         symbol: ~S"
			:format-arguments (list (cadr option))))
	       (setf (initarg car-option)
		     `',(cdr option)))
	      (:argument-precedence-order
	       (let* ((required (parse-lambda-list lambda-list))
		      (supplied (cdr option)))
		 (unless (= (length required) (length supplied))
		   (error 'simple-program-error
			  :format-control "argument count discrepancy in ~
                                           :ARGUMENT-PRECEDENCE-ORDER clause."
			  :format-arguments nil))
		 (when (set-difference required supplied)
		   (error 'simple-program-error
			  :format-control "unequal sets for ~
                                           :ARGUMENT-PRECEDENCE-ORDER clause: ~
                                           ~S and ~S"
			  :format-arguments (list required supplied)))
		 (setf (initarg car-option)
		       `',(cdr option))))
	      ((:documentation :generic-function-class :method-class)
	       (unless (proper-list-of-length-p option 2)
		 (error "bad list length for ~S" option))
	       (if (initarg car-option)
		   (duplicate-option car-option)
		   (setf (initarg car-option) `',(cadr option))))
	      (:method
	       (push (cdr option) methods))
	      (t
	       ;; ANSI requires that unsupported things must get a
	       ;; PROGRAM-ERROR.
	       (error 'simple-program-error
		      :format-control "unsupported option ~S"
		      :format-arguments (list option))))))

	(when (initarg :declarations)
	  (setf (initarg :declarations)
		`',(initarg :declarations))))
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (compile-or-load-defgeneric ',fun-name))
         (load-defgeneric ',fun-name ',lambda-list ,@initargs)
        ,@(mapcar #'expand-method-definition methods)
        (fdefinition ',fun-name)))))

(defun compile-or-load-defgeneric (fun-name)
  (proclaim-as-fun-name fun-name)
  (note-name-defined fun-name :function)
  (unless (eq (info :function :where-from fun-name) :declared)
    (setf (info :function :where-from fun-name) :defined)
    (setf (info :function :type fun-name)
	  (specifier-type 'function))))

(defun load-defgeneric (fun-name lambda-list &rest initargs)
  (when (fboundp fun-name)
    (style-warn "redefining ~S in DEFGENERIC" fun-name)
    (let ((fun (fdefinition fun-name)))
      (when (generic-function-p fun)
        (loop for method in (generic-function-initial-methods fun)
              do (remove-method fun method))
        (setf (generic-function-initial-methods fun) '()))))
  (apply #'ensure-generic-function
         fun-name
         :lambda-list lambda-list
         :definition-source `((defgeneric ,fun-name) ,*load-pathname*)
         initargs))

(define-condition generic-function-lambda-list-error
    (reference-condition simple-program-error)
  ()
  (:default-initargs :references (list '(:ansi-cl :section (3 4 2)))))

(defun check-gf-lambda-list (lambda-list)
  (flet ((ensure (arg ok)
           (unless ok
	     (error 'generic-function-lambda-list-error
		    :format-control
		    "~@<invalid ~S ~_in the generic function lambda list ~S~:>"
		    :format-arguments (list arg lambda-list)))))
    (multiple-value-bind (required optional restp rest keyp keys allowp
                          auxp aux morep more-context more-count)
	(parse-lambda-list lambda-list)
      (declare (ignore required)) ; since they're no different in a gf ll
      (declare (ignore restp rest)) ; since they're no different in a gf ll
      (declare (ignore allowp)) ; since &ALLOW-OTHER-KEYS is fine either way
      (declare (ignore aux)) ; since we require AUXP=NIL
      (declare (ignore more-context more-count)) ; safely ignored unless MOREP
      ;; no defaults allowed for &OPTIONAL arguments
      (dolist (i optional)
	(ensure i (or (symbolp i)
		      (and (consp i) (symbolp (car i)) (null (cdr i))))))
      ;; no defaults allowed for &KEY arguments
      (when keyp
	(dolist (i keys)
	  (ensure i (or (symbolp i)
			(and (consp i)
			     (or (symbolp (car i))
				 (and (consp (car i))
				      (symbolp (caar i))
				      (symbolp (cadar i))
				      (null (cddar i))))
			     (null (cdr i)))))))
      ;; no &AUX allowed
      (when auxp
	(error "&AUX is not allowed in a generic function lambda list: ~S"
	       lambda-list))
      ;; Oh, *puhlease*... not specifically as per section 3.4.2 of
      ;; the ANSI spec, but the CMU CL &MORE extension does not
      ;; belong here!
      (aver (not morep)))))

(defmacro defmethod (&rest args &environment env)
  (multiple-value-bind (name qualifiers lambda-list body)
      (parse-defmethod args)
    (multiple-value-bind (proto-gf proto-method)
	(prototypes-for-make-method-lambda name)
      (expand-defmethod name
			proto-gf
			proto-method
			qualifiers
			lambda-list
			body
			env))))

(defun prototypes-for-make-method-lambda (name)
  (if (not (eq *boot-state* 'complete))
      (values nil nil)
      (let ((gf? (and (gboundp name)
		      (gdefinition name))))
	(if (or (null gf?)
		(not (generic-function-p gf?)))
	    (values (class-prototype (find-class 'standard-generic-function))
		    (class-prototype (find-class 'standard-method)))
	    (values gf?
		    (class-prototype (or (generic-function-method-class gf?)
					 (find-class 'standard-method))))))))

;;; Take a name which is either a generic function name or a list specifying
;;; a SETF generic function (like: (SETF <generic-function-name>)). Return
;;; the prototype instance of the method-class for that generic function.
;;;
;;; If there is no generic function by that name, this returns the
;;; default value, the prototype instance of the class
;;; STANDARD-METHOD. This default value is also returned if the spec
;;; names an ordinary function or even a macro. In effect, this leaves
;;; the signalling of the appropriate error until load time.
;;;
;;; Note: During bootstrapping, this function is allowed to return NIL.
(defun method-prototype-for-gf (name)
  (let ((gf? (and (gboundp name)
		  (gdefinition name))))
    (cond ((neq *boot-state* 'complete) nil)
	  ((or (null gf?)
	       (not (generic-function-p gf?)))		; Someone else MIGHT
							; error at load time.
	   (class-prototype (find-class 'standard-method)))
	  (t
	    (class-prototype (or (generic-function-method-class gf?)
				 (find-class 'standard-method)))))))

(defun expand-defmethod (name
			 proto-gf
			 proto-method
			 qualifiers
			 lambda-list
			 body
			 env)
  (multiple-value-bind (method-lambda unspecialized-lambda-list specializers)
      (add-method-declarations name qualifiers lambda-list body env)
    (multiple-value-bind (method-function-lambda initargs)
	(make-method-lambda proto-gf proto-method method-lambda env)
      (let ((initargs-form (make-method-initargs-form proto-gf
						      proto-method
						      method-function-lambda
						      initargs
						      env)))
	`(progn
	  ;; Note: We could DECLAIM the ftype of the generic function
	  ;; here, since ANSI specifies that we create it if it does
	  ;; not exist. However, I chose not to, because I think it's
	  ;; more useful to support a style of programming where every
	  ;; generic function has an explicit DEFGENERIC and any typos
	  ;; in DEFMETHODs are warned about. Otherwise
	  ;;
	  ;;   (DEFGENERIC FOO-BAR-BLETCH ((X T)))
	  ;;   (DEFMETHOD FOO-BAR-BLETCH ((X HASH-TABLE)) ..)
	  ;;   (DEFMETHOD FOO-BRA-BLETCH ((X SIMPLE-VECTOR)) ..)
	  ;;   (DEFMETHOD FOO-BAR-BLETCH ((X VECTOR)) ..)
	  ;;   (DEFMETHOD FOO-BAR-BLETCH ((X ARRAY)) ..)
	  ;;   (DEFMETHOD FOO-BAR-BLETCH ((X LIST)) ..)
	  ;;
	  ;; compiles without raising an error and runs without
	  ;; raising an error (since SIMPLE-VECTOR cases fall through
	  ;; to VECTOR) but still doesn't do what was intended. I hate
	  ;; that kind of bug (code which silently gives the wrong
	  ;; answer), so we don't do a DECLAIM here. -- WHN 20000229
	  ,(make-defmethod-form name qualifiers specializers
				unspecialized-lambda-list
				(if proto-method
				    (class-name (class-of proto-method))
				    'standard-method)
				initargs-form
				(getf (getf initargs :plist)
				      :pv-table-symbol)))))))

(defun interned-symbol-p (x)
  (and (symbolp x) (symbol-package x)))

(defun make-defmethod-form (name qualifiers specializers
				 unspecialized-lambda-list method-class-name
				 initargs-form &optional pv-table-symbol)
  (let (fn
	fn-lambda)
    (if (and (interned-symbol-p (fun-name-block-name name))
	     (every #'interned-symbol-p qualifiers)
	     (every (lambda (s)
		      (if (consp s)
			  (and (eq (car s) 'eql)
			       (constantp (cadr s))
			       (let ((sv (eval (cadr s))))
				 (or (interned-symbol-p sv)
				     (integerp sv)
				     (and (characterp sv)
					  (standard-char-p sv)))))
			  (interned-symbol-p s)))
		    specializers)
	     (consp initargs-form)
	     (eq (car initargs-form) 'list*)
	     (memq (cadr initargs-form) '(:function :fast-function))
	     (consp (setq fn (caddr initargs-form)))
	     (eq (car fn) 'function)
	     (consp (setq fn-lambda (cadr fn)))
	     (eq (car fn-lambda) 'lambda))
	(let* ((specls (mapcar (lambda (specl)
				 (if (consp specl)
				     `(,(car specl) ,(eval (cadr specl)))
				   specl))
			       specializers))
	       (mname `(,(if (eq (cadr initargs-form) :function)
			     'method 'fast-method)
			,name ,@qualifiers ,specls))
	       (mname-sym (let ((*print-pretty* nil)
				;; (We bind *PACKAGE* to KEYWORD here
				;; as a way to force symbols to be
				;; printed with explicit package
				;; prefixes.)
				(target *package*)
				(*package* *keyword-package*))
			    (format-symbol target "~S" mname))))
	  `(progn
	     (defun ,mname-sym ,(cadr fn-lambda)
	       ,@(cddr fn-lambda))
	     ,(make-defmethod-form-internal
	       name qualifiers `',specls
	       unspecialized-lambda-list method-class-name
	       `(list* ,(cadr initargs-form)
		       #',mname-sym
		       ,@(cdddr initargs-form))
	       pv-table-symbol)))
	(make-defmethod-form-internal
	 name qualifiers
	 `(list ,@(mapcar (lambda (specializer)
			    (if (consp specializer)
				``(,',(car specializer)
				      ,,(cadr specializer))
				`',specializer))
			  specializers))
	 unspecialized-lambda-list
	 method-class-name
	 initargs-form
	 pv-table-symbol))))

(defun make-defmethod-form-internal
    (name qualifiers specializers-form unspecialized-lambda-list
     method-class-name initargs-form &optional pv-table-symbol)
  `(load-defmethod
    ',method-class-name
    ',name
    ',qualifiers
    ,specializers-form
    ',unspecialized-lambda-list
    ,initargs-form
    ;; Paper over a bug in KCL by passing the cache-symbol here in
    ;; addition to in the list. FIXME: We should no longer need to do
    ;; this, since the CLOS code is now SBCL-specific, and doesn't
    ;; need to be ported to every buggy compiler in existence.
    ',pv-table-symbol))

(defmacro make-method-function (method-lambda &environment env)
  (make-method-function-internal method-lambda env))

(defun make-method-function-internal (method-lambda &optional env)
  (multiple-value-bind (proto-gf proto-method)
      (prototypes-for-make-method-lambda nil)
    (multiple-value-bind (method-function-lambda initargs)
	(make-method-lambda proto-gf proto-method method-lambda env)
      (make-method-initargs-form proto-gf
				 proto-method
				 method-function-lambda
				 initargs
				 env))))

(defun add-method-declarations (name qualifiers lambda-list body env)
  (declare (ignore env))
  (multiple-value-bind (parameters unspecialized-lambda-list specializers)
      (parse-specialized-lambda-list lambda-list)
    (multiple-value-bind (real-body declarations documentation)
	(parse-body body)
      (values `(lambda ,unspecialized-lambda-list
		 ,@(when documentation `(,documentation))
		 ;; (Old PCL code used a somewhat different style of
		 ;; list for %METHOD-NAME values. Our names use
		 ;; ,@QUALIFIERS instead of ,QUALIFIERS so that the
		 ;; method names look more like what you see in a
		 ;; DEFMETHOD form.)
		 ;;
		 ;; FIXME: As of sbcl-0.7.0.6, code elsewhere, at
		 ;; least the code to set up named BLOCKs around the
		 ;; bodies of methods, depends on the function's base
		 ;; name being the first element of the %METHOD-NAME
		 ;; list. It would be good to remove this dependency,
		 ;; perhaps by building the BLOCK here, or by using
		 ;; another declaration (e.g. %BLOCK-NAME), so that
		 ;; our method debug names are free to have any format,
		 ;; e.g. (:METHOD PRINT-OBJECT :AROUND (CLOWN T)).
		 ;;
		 ;; Further, as of sbcl-0.7.9.10, the code to
		 ;; implement NO-NEXT-METHOD is coupled to the form of
		 ;; this declaration; see the definition of
		 ;; CALL-NO-NEXT-METHOD (and the passing of
		 ;; METHOD-NAME-DECLARATION arguments around the
		 ;; various CALL-NEXT-METHOD logic).
		 (declare (%method-name (,name
					 ,@qualifiers
					 ,specializers)))
		 (declare (%method-lambda-list ,@lambda-list))
		 ,@declarations
		 ,@real-body)
	      unspecialized-lambda-list specializers))))

(defun real-make-method-initargs-form (proto-gf proto-method
				       method-lambda initargs env)
  (declare (ignore proto-gf proto-method))
  (unless (and (consp method-lambda)
	       (eq (car method-lambda) 'lambda))
    (error "The METHOD-LAMBDA argument to MAKE-METHOD-FUNCTION, ~S, ~
	    is not a lambda form."
	   method-lambda))
  (make-method-initargs-form-internal method-lambda initargs env))

(unless (fboundp 'make-method-initargs-form)
  (setf (gdefinition 'make-method-initargs-form)
	(symbol-function 'real-make-method-initargs-form)))

(defun real-make-method-lambda (proto-gf proto-method method-lambda env)
  (declare (ignore proto-gf proto-method))
  (make-method-lambda-internal method-lambda env))

;;; a helper function for creating Python-friendly type declarations
;;; in DEFMETHOD forms
(defun parameter-specializer-declaration-in-defmethod (parameter specializer)
  (cond ((and (consp specializer)
	      (eq (car specializer) 'eql))
	 ;; KLUDGE: ANSI, in its wisdom, says that
	 ;; EQL-SPECIALIZER-FORMs in EQL specializers are evaluated at
	 ;; DEFMETHOD expansion time. Thus, although one might think
	 ;; that in
	 ;;   (DEFMETHOD FOO ((X PACKAGE)
	 ;;                   (Y (EQL 12))
	 ;;      ..))
	 ;; the PACKAGE and (EQL 12) forms are both parallel type
	 ;; names, they're not, as is made clear when you do
	 ;;   (DEFMETHOD FOO ((X PACKAGE)
	 ;;                   (Y (EQL 'BAR)))
	 ;;     ..)
	 ;; where Y needs to be a symbol named "BAR", not some cons
	 ;; made by (CONS 'QUOTE 'BAR). I.e. when the
	 ;; EQL-SPECIALIZER-FORM is (EQL 'X), it requires an argument
	 ;; to be of type (EQL X). It'd be easy to transform one to
	 ;; the other, but it'd be somewhat messier to do so while
	 ;; ensuring that the EQL-SPECIALIZER-FORM is only EVAL'd
	 ;; once. (The new code wouldn't be messy, but it'd require a
	 ;; big transformation of the old code.) So instead we punt.
	 ;; -- WHN 20000610
	 '(ignorable))
	((member specializer
		 ;; KLUDGE: For some low-level implementation
		 ;; classes, perhaps because of some problems related
		 ;; to the incomplete integration of PCL into SBCL's
		 ;; type system, some specializer classes can't be
		 ;; declared as argument types. E.g.
		 ;;   (DEFMETHOD FOO ((X SLOT-OBJECT))
		 ;;     (DECLARE (TYPE SLOT-OBJECT X))
		 ;;     ..)
		 ;; loses when
		 ;;   (DEFSTRUCT BAR A B)
		 ;;   (FOO (MAKE-BAR))
		 ;; perhaps because of the way that STRUCTURE-OBJECT
		 ;; inherits both from SLOT-OBJECT and from
		 ;; SB-KERNEL:INSTANCE. In an effort to sweep such
		 ;; problems under the rug, we exclude these problem
		 ;; cases by blacklisting them here. -- WHN 2001-01-19
		 '(slot-object))
	 '(ignorable))
	((not (eq *boot-state* 'complete))
	 ;; KLUDGE: PCL, in its wisdom, sometimes calls methods with
	 ;; types which don't match their specializers. (Specifically,
	 ;; it calls ENSURE-CLASS-USING-CLASS (T NULL) with a non-NULL
	 ;; second argument.) Hopefully it only does this kind of
	 ;; weirdness when bootstrapping.. -- WHN 20000610
	 '(ignorable))
	(t
	 ;; Otherwise, we can usually make Python very happy.
	 (let ((type (info :type :kind specializer)))
	   (ecase type
	     ((:primitive :defined :instance :forthcoming-defclass-type)
	      `(type ,specializer ,parameter))
	     ((nil)
	      (let ((class (find-class specializer nil)))
		(if class
		    `(type ,(class-name class) ,parameter)
		    (progn
		      ;; we can get here, and still not have a failure
		      ;; case, by doing MOP programming like (PROGN
		      ;; (ENSURE-CLASS 'FOO) (DEFMETHOD BAR ((X FOO))
		      ;; ...)).  Best to let the user know we haven't
		      ;; been able to extract enough information:
		      (style-warn
		       "~@<can't find type for presumed class ~S in ~S.~@:>"
		       specializer
		       'parameter-specializer-declaration-in-defmethod)
		      '(ignorable))))))))))

(defun make-method-lambda-internal (method-lambda &optional env)
  (unless (and (consp method-lambda) (eq (car method-lambda) 'lambda))
    (error "The METHOD-LAMBDA argument to MAKE-METHOD-LAMBDA, ~S, ~
	    is not a lambda form."
	   method-lambda))
  (multiple-value-bind (real-body declarations documentation)
      (parse-body (cddr method-lambda))
    (let* ((name-decl (get-declaration '%method-name declarations))
	   (sll-decl (get-declaration '%method-lambda-list declarations))
	   (method-name (when (consp name-decl) (car name-decl)))
	   (generic-function-name (when method-name (car method-name)))
	   (specialized-lambda-list (or sll-decl (cadr method-lambda))))
      (multiple-value-bind (parameters lambda-list specializers)
	  (parse-specialized-lambda-list specialized-lambda-list)
	(let* ((required-parameters
		(mapcar (lambda (r s) (declare (ignore s)) r)
			parameters
			specializers))
	       (slots (mapcar #'list required-parameters))
	       (calls (list nil))
	       (class-declarations
		`(declare
		  ;; These declarations seem to be used by PCL to pass
		  ;; information to itself; when I tried to delete 'em
		  ;; ca. 0.6.10 it didn't work. I'm not sure how
		  ;; they work, but note the (VAR-DECLARATION '%CLASS ..)
		  ;; expression in CAN-OPTIMIZE-ACCESS1. -- WHN 2000-12-30
		  ,@(remove nil
			    (mapcar (lambda (a s) (and (symbolp s)
						       (neq s t)
						       `(%class ,a ,s)))
				    parameters
				    specializers))
		  ;; These TYPE declarations weren't in the original
		  ;; PCL code, but the Python compiler likes them a
		  ;; lot. (We're telling the compiler about our
		  ;; knowledge of specialized argument types so that
		  ;; it can avoid run-time type dispatch overhead,
		  ;; which can be a huge win for Python.)
		  ;;
		  ;; KLUDGE: when I tried moving these to
		  ;; ADD-METHOD-DECLARATIONS, things broke.  No idea
		  ;; why.  -- CSR, 2004-06-16
		  ,@(mapcar #'parameter-specializer-declaration-in-defmethod
			    parameters
			    specializers)))
	       (method-lambda
		;; Remove the documentation string and insert the
		;; appropriate class declarations. The documentation
		;; string is removed to make it easy for us to insert
		;; new declarations later, they will just go after the
		;; CADR of the method lambda. The class declarations
		;; are inserted to communicate the class of the method's
		;; arguments to the code walk.
		`(lambda ,lambda-list
		   ;; The default ignorability of method parameters
		   ;; doesn't seem to be specified by ANSI. PCL had
		   ;; them basically ignorable but was a little
		   ;; inconsistent. E.g. even though the two
		   ;; method definitions 
		   ;;   (DEFMETHOD FOO ((X T) (Y T)) "Z")
		   ;;   (DEFMETHOD FOO ((X T) Y) "Z")
		   ;; are otherwise equivalent, PCL treated Y as
		   ;; ignorable in the first definition but not in the
		   ;; second definition. We make all required
		   ;; parameters ignorable as a way of systematizing
		   ;; the old PCL behavior. -- WHN 2000-11-24
		   (declare (ignorable ,@required-parameters))
		   ,class-declarations
		   ,@declarations
		   (block ,(fun-name-block-name generic-function-name)
		     ,@real-body)))
	       (constant-value-p (and (null (cdr real-body))
				      (constantp (car real-body))))
	       (constant-value (and constant-value-p
				    (eval (car real-body))))
	       (plist (and constant-value-p
                           (or (typep constant-value
                                      '(or number character))
                               (and (symbolp constant-value)
                                    (symbol-package constant-value)))
                           (list :constant-value constant-value)))
	       (applyp (dolist (p lambda-list nil)
			 (cond ((memq p '(&optional &rest &key))
				(return t))
			       ((eq p '&aux)
				(return nil))))))
	  (multiple-value-bind
		(walked-lambda call-next-method-p closurep
			       next-method-p-p setq-p)
	      (walk-method-lambda method-lambda
				  required-parameters
				  env
				  slots
				  calls)
	    (multiple-value-bind (walked-lambda-body
				  walked-declarations
				  walked-documentation)
		(parse-body (cddr walked-lambda))
	      (declare (ignore walked-documentation))
	      (when (or next-method-p-p call-next-method-p)
		(setq plist (list* :needs-next-methods-p t plist)))
	      (when (some #'cdr slots)
		(multiple-value-bind (slot-name-lists call-list)
		    (slot-name-lists-from-slots slots calls)
		  (let ((pv-table-symbol (make-symbol "pv-table")))
		    (setq plist
			  `(,@(when slot-name-lists
				`(:slot-name-lists ,slot-name-lists))
			      ,@(when call-list
				  `(:call-list ,call-list))
			      :pv-table-symbol ,pv-table-symbol
			      ,@plist))
		    (setq walked-lambda-body
			  `((pv-binding (,required-parameters
					 ,slot-name-lists
					 ,pv-table-symbol)
					,@walked-lambda-body))))))
	      (when (and (memq '&key lambda-list)
			 (not (memq '&allow-other-keys lambda-list)))
		(let ((aux (memq '&aux lambda-list)))
		(setq lambda-list (nconc (ldiff lambda-list aux)
					 (list '&allow-other-keys)
					 aux))))
	      (values `(lambda (.method-args. .next-methods.)
			 (simple-lexical-method-functions
			  (,lambda-list .method-args. .next-methods.
					:call-next-method-p
					,call-next-method-p
					:next-method-p-p ,next-method-p-p
			                :setq-p ,setq-p
			                ;; we need to pass this along
			                ;; so that NO-NEXT-METHOD can
			                ;; be given a suitable METHOD
			                ;; argument; we need the
			                ;; QUALIFIERS and SPECIALIZERS
			                ;; inside the declaration to
			                ;; give to FIND-METHOD.
			                :method-name-declaration ,name-decl
					:closurep ,closurep
					:applyp ,applyp)
			  ,@walked-declarations
			  ,@walked-lambda-body))
		      `(,@(when plist
		      `(:plist ,plist))
			  ,@(when documentation
			  `(:documentation ,documentation)))))))))))

(unless (fboundp 'make-method-lambda)
  (setf (gdefinition 'make-method-lambda)
	(symbol-function 'real-make-method-lambda)))

(defmacro simple-lexical-method-functions ((lambda-list
					    method-args
					    next-methods
					    &rest lmf-options)
					   &body body)
  `(progn
     ,method-args ,next-methods
     (bind-simple-lexical-method-macros (,method-args ,next-methods)
       (bind-lexical-method-functions (,@lmf-options)
	 (bind-args (,lambda-list ,method-args)
	   ,@body)))))

(defmacro fast-lexical-method-functions ((lambda-list
					  next-method-call
					  args
					  rest-arg
					  &rest lmf-options)
					 &body body)
  `(bind-fast-lexical-method-macros (,args ,rest-arg ,next-method-call)
     (bind-lexical-method-functions (,@lmf-options)
       (bind-args (,(nthcdr (length args) lambda-list) ,rest-arg)
	 ,@body))))

(defmacro bind-simple-lexical-method-macros ((method-args next-methods)
					     &body body)
  `(macrolet ((call-next-method-bind (&body body)
	       `(let ((.next-method. (car ,',next-methods))
		      (,',next-methods (cdr ,',next-methods)))
		 .next-method. ,',next-methods
		 ,@body))
	      (call-next-method-body (method-name-declaration cnm-args)
	       `(if .next-method.
		    (funcall (if (std-instance-p .next-method.)
				 (method-function .next-method.)
			     .next-method.) ; for early methods
		             (or ,cnm-args ,',method-args)
		             ,',next-methods)
		    (apply #'call-no-next-method ',method-name-declaration
		            (or ,cnm-args ,',method-args))))
	      (next-method-p-body ()
	       `(not (null .next-method.)))
	      (with-rebound-original-args ((call-next-method-p setq-p)
					   &body body)
		(declare (ignore call-next-method-p setq-p))
		`(let () ,@body)))
    ,@body))

(defun call-no-next-method (method-name-declaration &rest args)
  (destructuring-bind (name) method-name-declaration
    (destructuring-bind (name &rest qualifiers-and-specializers) name
      ;; KLUDGE: inefficient traversal, but hey.  This should only
      ;; happen on the slow error path anyway.
      (let* ((qualifiers (butlast qualifiers-and-specializers))
	     (specializers (car (last qualifiers-and-specializers)))
	     (method (find-method (gdefinition name) qualifiers specializers)))
	(apply #'no-next-method
	       (method-generic-function method)
	       method
	       args)))))

(defstruct (method-call (:copier nil))
  (function #'identity :type function)
  call-method-args)

#-sb-fluid (declaim (sb-ext:freeze-type method-call))

(defmacro invoke-method-call1 (function args cm-args)
  `(let ((.function. ,function)
	 (.args. ,args)
	 (.cm-args. ,cm-args))
     (if (and .cm-args. (null (cdr .cm-args.)))
	 (funcall .function. .args. (car .cm-args.))
	 (apply .function. .args. .cm-args.))))

(defmacro invoke-method-call (method-call restp &rest required-args+rest-arg)
  `(invoke-method-call1 (method-call-function ,method-call)
			,(if restp
			     `(list* ,@required-args+rest-arg)
			     `(list ,@required-args+rest-arg))
			(method-call-call-method-args ,method-call)))

(defstruct (fast-method-call (:copier nil))
  (function #'identity :type function)
  pv-cell
  next-method-call
  arg-info)

#-sb-fluid (declaim (sb-ext:freeze-type fast-method-call))

(defmacro fmc-funcall (fn pv-cell next-method-call &rest args)
  `(funcall ,fn ,pv-cell ,next-method-call ,@args))

(defmacro invoke-fast-method-call (method-call &rest required-args+rest-arg)
  `(fmc-funcall (fast-method-call-function ,method-call)
		(fast-method-call-pv-cell ,method-call)
		(fast-method-call-next-method-call ,method-call)
		,@required-args+rest-arg))

(defstruct (fast-instance-boundp (:copier nil))
  (index 0 :type fixnum))

#-sb-fluid (declaim (sb-ext:freeze-type fast-instance-boundp))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *allow-emf-call-tracing-p* nil)
  (defvar *enable-emf-call-tracing-p* #-sb-show nil #+sb-show t))

;;;; effective method functions

(defvar *emf-call-trace-size* 200)
(defvar *emf-call-trace* nil)
(defvar *emf-call-trace-index* 0)

;;; This function was in the CMU CL version of PCL (ca Debian 2.4.8)
;;; without explanation. It appears to be intended for debugging, so
;;; it might be useful someday, so I haven't deleted it.
;;; But it isn't documented and isn't used for anything now, so
;;; I've conditionalized it out of the base system. -- WHN 19991213
#+sb-show
(defun show-emf-call-trace ()
  (when *emf-call-trace*
    (let ((j *emf-call-trace-index*)
	  (*enable-emf-call-tracing-p* nil))
      (format t "~&(The oldest entries are printed first)~%")
      (dotimes-fixnum (i *emf-call-trace-size*)
	(let ((ct (aref *emf-call-trace* j)))
	  (when ct (print ct)))
	(incf j)
	(when (= j *emf-call-trace-size*)
	  (setq j 0))))))

(defun trace-emf-call-internal (emf format args)
  (unless *emf-call-trace*
    (setq *emf-call-trace* (make-array *emf-call-trace-size*)))
  (setf (aref *emf-call-trace* *emf-call-trace-index*)
	(list* emf format args))
  (incf *emf-call-trace-index*)
  (when (= *emf-call-trace-index* *emf-call-trace-size*)
    (setq *emf-call-trace-index* 0)))

(defmacro trace-emf-call (emf format args)
  (when *allow-emf-call-tracing-p*
    `(when *enable-emf-call-tracing-p*
       (trace-emf-call-internal ,emf ,format ,args))))

(defmacro invoke-effective-method-function-fast
    (emf restp &rest required-args+rest-arg)
  `(progn
     (trace-emf-call ,emf ,restp (list ,@required-args+rest-arg))
     (invoke-fast-method-call ,emf ,@required-args+rest-arg)))

(defmacro invoke-effective-method-function (emf restp
						&rest required-args+rest-arg)
  (unless (constantp restp)
    (error "The RESTP argument is not constant."))
  ;; FIXME: The RESTP handling here is confusing and maybe slightly
  ;; broken if RESTP evaluates to a non-self-evaluating form. E.g. if
  ;;   (INVOKE-EFFECTIVE-METHOD-FUNCTION EMF '(ERROR "gotcha") ...)
  ;; then TRACE-EMF-CALL-CALL-INTERNAL might die on a gotcha error.
  (setq restp (eval restp))
  `(progn
     (trace-emf-call ,emf ,restp (list ,@required-args+rest-arg))
     (cond ((typep ,emf 'fast-method-call)
	    (invoke-fast-method-call ,emf ,@required-args+rest-arg))
	   ;; "What," you may wonder, "do these next two clauses do?"
	   ;; In that case, you are not a PCL implementor, for they
	   ;; considered this to be self-documenting.:-| Or CSR, for
	   ;; that matter, since he can also figure it out by looking
	   ;; at it without breaking stride. For the rest of us,
	   ;; though: From what the code is doing with .SLOTS. and
	   ;; whatnot, evidently it's implementing SLOT-VALUEish and
	   ;; GET-SLOT-VALUEish things. Then we can reason backwards
	   ;; and conclude that setting EMF to a FIXNUM is an
	   ;; optimized way to represent these slot access operations.
	   ,@(when (and (null restp) (= 1 (length required-args+rest-arg)))
	       `(((typep ,emf 'fixnum)
		  (let* ((.slots. (get-slots-or-nil
				   ,(car required-args+rest-arg)))
			 (value (when .slots. (clos-slots-ref .slots. ,emf))))
		    (if (eq value +slot-unbound+)
			(slot-unbound-internal ,(car required-args+rest-arg)
					       ,emf)
			value)))))
	   ,@(when (and (null restp) (= 2 (length required-args+rest-arg)))
	       `(((typep ,emf 'fixnum)
		  (let ((.new-value. ,(car required-args+rest-arg))
			(.slots. (get-slots-or-nil
				  ,(cadr required-args+rest-arg))))
		    (when .slots.
		      (setf (clos-slots-ref .slots. ,emf) .new-value.))))))
	   ;; (In cmucl-2.4.8 there was a commented-out third ,@(WHEN
	   ;; ...) clause here to handle SLOT-BOUNDish stuff. Since
	   ;; there was no explanation and presumably the code is 10+
	   ;; years stale, I simply deleted it. -- WHN)
	   (t
	    (etypecase ,emf
	      (method-call
	       (invoke-method-call ,emf ,restp ,@required-args+rest-arg))
	      (function
	       ,(if restp
		    `(apply (the function ,emf) ,@required-args+rest-arg)
		    `(funcall (the function ,emf)
			      ,@required-args+rest-arg))))))))

(defun invoke-emf (emf args)
  (trace-emf-call emf t args)
  (etypecase emf
    (fast-method-call
     (let* ((arg-info (fast-method-call-arg-info emf))
	    (restp (cdr arg-info))
	    (nreq (car arg-info)))
       (if restp
	   (let* ((rest-args (nthcdr nreq args))
		  (req-args (ldiff args rest-args)))
	     (apply (fast-method-call-function emf)
		    (fast-method-call-pv-cell emf)
		    (fast-method-call-next-method-call emf)
		    (nconc req-args (list rest-args))))
	   (cond ((null args)
		  (if (eql nreq 0)
		      (invoke-fast-method-call emf)
		      (error "wrong number of args")))
		 ((null (cdr args))
		  (if (eql nreq 1)
		      (invoke-fast-method-call emf (car args))
		      (error "wrong number of args")))
		 ((null (cddr args))
		  (if (eql nreq 2)
		      (invoke-fast-method-call emf (car args) (cadr args))
		      (error "wrong number of args")))
		 (t
		  (apply (fast-method-call-function emf)
			 (fast-method-call-pv-cell emf)
			 (fast-method-call-next-method-call emf)
			 args))))))
    (method-call
     (apply (method-call-function emf)
	    args
	    (method-call-call-method-args emf)))
    (fixnum
     (cond ((null args) (error "1 or 2 args were expected."))
	   ((null (cdr args))
	    (let* ((slots (get-slots (car args)))
                   (value (clos-slots-ref slots emf)))
	      (if (eq value +slot-unbound+)
		  (slot-unbound-internal (car args) emf)
		  value)))
	   ((null (cddr args))
             (setf (clos-slots-ref (get-slots (cadr args)) emf)
		   (car args)))
	   (t (error "1 or 2 args were expected."))))
    (fast-instance-boundp
     (if (or (null args) (cdr args))
	 (error "1 arg was expected.")
       (let ((slots (get-slots (car args))))
	 (not (eq (clos-slots-ref slots
				  (fast-instance-boundp-index emf))
		  +slot-unbound+)))))
    (function
     (apply emf args))))

(defmacro bind-fast-lexical-method-macros ((args rest-arg next-method-call)
					   &body body)
  (let* ((all-params (append args (when rest-arg (list rest-arg))))
	 (rebindings (mapcar (lambda (x) (list x x)) all-params)))
    `(macrolet ((narrowed-emf (emf)
		 ;; INVOKE-EFFECTIVE-METHOD-FUNCTION has code in it to
		 ;; dispatch on the possibility that EMF might be of
		 ;; type FIXNUM (as an optimized representation of a
		 ;; slot accessor). But as far as I (WHN 2002-06-11)
		 ;; can tell, it's impossible for such a representation
		 ;; to end up as .NEXT-METHOD-CALL. By reassuring
		 ;; INVOKE-E-M-F that when called from this context
		 ;; it needn't worry about the FIXNUM case, we can
		 ;; keep those cases from being compiled, which is
		 ;; good both because it saves bytes and because it
		 ;; avoids annoying type mismatch compiler warnings.
		 ;;
		 ;; KLUDGE: In sbcl-0.7.4.29, the compiler's type
		 ;; system isn't smart enough about NOT and
		 ;; intersection types to benefit from a (NOT FIXNUM)
		 ;; declaration here. -- WHN 2002-06-12 (FIXME: maybe
		 ;; it is now... -- CSR, 2003-06-07)
		 ;;
		 ;; FIXME: Might the FUNCTION type be omittable here,
		 ;; leaving only METHOD-CALLs? Failing that, could this
		 ;; be documented somehow? (It'd be nice if the types
		 ;; involved could be understood without solving the
		 ;; halting problem.)
		 `(the (or function method-call fast-method-call)
		   ,emf))
		(call-next-method-bind (&body body)
		 `(let () ,@body))
		(call-next-method-body (method-name-declaration cnm-args)
		 `(if ,',next-method-call
		      ,(locally
			;; This declaration suppresses a "deleting
			;; unreachable code" note for the following IF
			;; when REST-ARG is NIL. It is not nice for
			;; debugging SBCL itself, but at least it
			;; keeps us from annoying users.
			(declare (optimize (inhibit-warnings 3)))
			(if (and (null ',rest-arg)
				 (consp cnm-args)
				 (eq (car cnm-args) 'list))
			    `(invoke-effective-method-function
			      (narrowed-emf ,',next-method-call)
			      nil
			      ,@(cdr cnm-args))
			    (let ((call `(invoke-effective-method-function
					  (narrowed-emf ,',next-method-call)
					  ,',(not (null rest-arg))
					  ,@',args
					  ,@',(when rest-arg `(,rest-arg)))))
			      `(if ,cnm-args
				(bind-args ((,@',args
					     ,@',(when rest-arg
						       `(&rest ,rest-arg)))
					    ,cnm-args)
				 ,call)
				,call))))
		      ,(locally
			;; As above, this declaration suppresses code
			;; deletion notes.
			(declare (optimize (inhibit-warnings 3)))
			(if (and (null ',rest-arg)
				 (consp cnm-args)
				 (eq (car cnm-args) 'list))
			    `(call-no-next-method ',method-name-declaration
			      ,@(cdr cnm-args))
			    `(call-no-next-method ',method-name-declaration
			      ,@',args
			      ,@',(when rest-arg
					`(,rest-arg)))))))
		(next-method-p-body ()
		 `(not (null ,',next-method-call)))
		(with-rebound-original-args ((cnm-p setq-p) &body body)
		  (if (or cnm-p setq-p)
		      `(let ,',rebindings
			(declare (ignorable ,@',all-params))
			,@body)
		      `(let () ,@body))))
      ,@body)))

(defmacro bind-lexical-method-functions
    ((&key call-next-method-p next-method-p-p setq-p
	   closurep applyp method-name-declaration)
     &body body)
  (cond ((and (null call-next-method-p) (null next-method-p-p)
	      (null closurep) (null applyp) (null setq-p))
	 `(let () ,@body))
	(t
	 `(call-next-method-bind
	    (flet (,@(and call-next-method-p
			  `((call-next-method (&rest cnm-args)
			     (call-next-method-body
			      ,method-name-declaration
			      cnm-args))))
		   ,@(and next-method-p-p
			  '((next-method-p ()
			     (next-method-p-body)))))
	      (with-rebound-original-args (,call-next-method-p ,setq-p)
		,@body))))))

(defmacro bind-args ((lambda-list args) &body body)
  (let ((args-tail '.args-tail.)
	(key '.key.)
	(state 'required))
    (flet ((process-var (var)
	     (if (memq var lambda-list-keywords)
		 (progn
		   (case var
		     (&optional	      (setq state 'optional))
		     (&key	      (setq state 'key))
		     (&allow-other-keys)
		     (&rest	      (setq state 'rest))
		     (&aux	      (setq state 'aux))
		     (otherwise
		      (error
		       "encountered the non-standard lambda list keyword ~S"
		       var)))
		   nil)
		 (case state
		   (required `((,var (pop ,args-tail))))
		   (optional (cond ((not (consp var))
				    `((,var (when ,args-tail
					      (pop ,args-tail)))))
				   ((null (cddr var))
				    `((,(car var) (if ,args-tail
						      (pop ,args-tail)
						      ,(cadr var)))))
				   (t
				    `((,(caddr var) ,args-tail)
				      (,(car var) (if ,args-tail
						      (pop ,args-tail)
						      ,(cadr var)))))))
		   (rest `((,var ,args-tail)))
		   (key (cond ((not (consp var))
			       `((,var (car
					(get-key-arg-tail ,(keywordicate var)
					                  ,args-tail)))))
			      ((null (cddr var))
			       (multiple-value-bind (keyword variable)
				   (if (consp (car var))
				       (values (caar var)
					       (cadar var))
				       (values (keywordicate (car var))
					       (car var)))
				 `((,key (get-key-arg-tail ',keyword
					                   ,args-tail))
				   (,variable (if ,key
						  (car ,key)
						  ,(cadr var))))))
			      (t
			       (multiple-value-bind (keyword variable)
				   (if (consp (car var))
				       (values (caar var)
					       (cadar var))
				       (values (keywordicate (car var))
					       (car var)))
				 `((,key (get-key-arg-tail ',keyword
					                   ,args-tail))
				   (,(caddr var) ,key)
				   (,variable (if ,key
						  (car ,key)
						  ,(cadr var))))))))
		   (aux `(,var))))))
      (let ((bindings (mapcan #'process-var lambda-list)))
	`(let* ((,args-tail ,args)
		,@bindings
		(.dummy0.
		 ,@(when (eq state 'optional)
		     `((unless (null ,args-tail)
			 (error 'simple-program-error
				:format-control "surplus arguments: ~S"
				:format-arguments (list ,args-tail)))))))
	   (declare (ignorable ,args-tail .dummy0.))
	   ,@body)))))

(defun get-key-arg-tail (keyword list)
  (loop for (key . tail) on list by #'cddr
	when (null tail) do
	  ;; FIXME: Do we want to export this symbol? Or maybe use an
	  ;; (ERROR 'SIMPLE-PROGRAM-ERROR) form?
	  (sb-c::%odd-key-args-error)
	when (eq key keyword)
	  return tail))

(defun walk-method-lambda (method-lambda required-parameters env slots calls)
  (let ((call-next-method-p nil)   ; flag indicating that CALL-NEXT-METHOD
				   ; should be in the method definition
	(closurep nil)		   ; flag indicating that #'CALL-NEXT-METHOD
				   ; was seen in the body of a method
	(next-method-p-p nil)      ; flag indicating that NEXT-METHOD-P
				   ; should be in the method definition
	(setq-p nil))
    (flet ((walk-function (form context env)
	     (cond ((not (eq context :eval)) form)
		   ;; FIXME: Jumping to a conclusion from the way it's used
		   ;; above, perhaps CONTEXT should be called SITUATION
		   ;; (after the term used in the ANSI specification of
		   ;; EVAL-WHEN) and given modern ANSI keyword values
		   ;; like :LOAD-TOPLEVEL.
		   ((not (listp form)) form)
		   ((eq (car form) 'call-next-method)
		    (setq call-next-method-p t)
		    form)
		   ((eq (car form) 'next-method-p)
		    (setq next-method-p-p t)
		    form)
		   ((eq (car form) 'setq)
		    ;; FIXME: this is possibly a little strong as
		    ;; conditions go.  Ideally we would want to detect
		    ;; which, if any, of the method parameters are
		    ;; being set, and communicate that information to
		    ;; e.g. SPLIT-DECLARATIONS.  However, the brute
		    ;; force method doesn't really cost much; a little
		    ;; loss of discrimination over IGNORED variables
		    ;; should be all.  -- CSR, 2004-07-01
		    (setq setq-p t)
		    form)
		   ((and (eq (car form) 'function)
			 (cond ((eq (cadr form) 'call-next-method)
				(setq call-next-method-p t)
				(setq closurep t)
				form)
			       ((eq (cadr form) 'next-method-p)
				(setq next-method-p-p t)
				(setq closurep t)
				form)
			       (t nil))))
		   ((and (memq (car form)
                               '(slot-value set-slot-value slot-boundp))
			 (constantp (caddr form)))
                     (let ((parameter (can-optimize-access form
							   required-parameters
							   env)))
                      (let ((fun (ecase (car form)
                                   (slot-value #'optimize-slot-value)
                                   (set-slot-value #'optimize-set-slot-value)
                                   (slot-boundp #'optimize-slot-boundp))))
                        (funcall fun slots parameter form))))
		   ((and (eq (car form) 'apply)
			 (consp (cadr form))
			 (eq (car (cadr form)) 'function)
			 (generic-function-name-p (cadr (cadr form))))
		    (optimize-generic-function-call
		     form required-parameters env slots calls))
		   ((generic-function-name-p (car form))
		    (optimize-generic-function-call
		     form required-parameters env slots calls))
		   (t form))))

      (let ((walked-lambda (walk-form method-lambda env #'walk-function)))
	(values walked-lambda
		call-next-method-p
		closurep
		next-method-p-p
		setq-p)))))

(defun generic-function-name-p (name)
  (and (legal-fun-name-p name)
       (gboundp name)
       (if (eq *boot-state* 'complete)
	   (standard-generic-function-p (gdefinition name))
	   (funcallable-instance-p (gdefinition name)))))

(defvar *method-function-plist* (make-hash-table :test 'eq))
(defvar *mf1* nil)
(defvar *mf1p* nil)
(defvar *mf1cp* nil)
(defvar *mf2* nil)
(defvar *mf2p* nil)
(defvar *mf2cp* nil)

(defun method-function-plist (method-function)
  (unless (eq method-function *mf1*)
    (rotatef *mf1* *mf2*)
    (rotatef *mf1p* *mf2p*)
    (rotatef *mf1cp* *mf2cp*))
  (unless (or (eq method-function *mf1*) (null *mf1cp*))
    (setf (gethash *mf1* *method-function-plist*) *mf1p*))
  (unless (eq method-function *mf1*)
    (setf *mf1* method-function
	  *mf1cp* nil
	  *mf1p* (gethash method-function *method-function-plist*)))
  *mf1p*)

(defun (setf method-function-plist)
    (val method-function)
  (unless (eq method-function *mf1*)
    (rotatef *mf1* *mf2*)
    (rotatef *mf1cp* *mf2cp*)
    (rotatef *mf1p* *mf2p*))
  (unless (or (eq method-function *mf1*) (null *mf1cp*))
    (setf (gethash *mf1* *method-function-plist*) *mf1p*))
  (setf *mf1* method-function
	*mf1cp* t
	*mf1p* val))

(defun method-function-get (method-function key &optional default)
  (getf (method-function-plist method-function) key default))

(defun (setf method-function-get)
    (val method-function key)
  (setf (getf (method-function-plist method-function) key) val))

(defun method-function-pv-table (method-function)
  (method-function-get method-function :pv-table))

(defun method-function-method (method-function)
  (method-function-get method-function :method))

(defun method-function-needs-next-methods-p (method-function)
  (method-function-get method-function :needs-next-methods-p t))

(defmacro method-function-closure-generator (method-function)
  `(method-function-get ,method-function 'closure-generator))

(defun load-defmethod
    (class name quals specls ll initargs &optional pv-table-symbol)
  (setq initargs (copy-tree initargs))
  (let ((method-spec (or (getf initargs :method-spec)
			 (make-method-spec name quals specls))))
    (setf (getf initargs :method-spec) method-spec)
    (load-defmethod-internal class name quals specls
			     ll initargs pv-table-symbol)))

(defun load-defmethod-internal
    (method-class gf-spec qualifiers specializers lambda-list
		  initargs pv-table-symbol)
  (when pv-table-symbol
    (setf (getf (getf initargs :plist) :pv-table-symbol)
	  pv-table-symbol))
  (when (and (eq *boot-state* 'complete)
	     (fboundp gf-spec))
    (let* ((gf (fdefinition gf-spec))
	   (method (and (generic-function-p gf)
                        (generic-function-methods gf)
			(find-method gf
				     qualifiers
                                     (parse-specializers specializers)
				     nil))))
      (when method
	(style-warn "redefining ~S~{ ~S~} ~S in DEFMETHOD"
		    gf-spec qualifiers specializers))))
  (let ((method (apply #'add-named-method
		       gf-spec qualifiers specializers lambda-list
		       :definition-source `((defmethod ,gf-spec
						,@qualifiers
					      ,specializers)
					    ,*load-pathname*)
		       initargs)))
    (unless (or (eq method-class 'standard-method)
		(eq (find-class method-class nil) (class-of method)))
      ;; FIXME: should be STYLE-WARNING?
      (format *error-output*
	      "~&At the time the method with qualifiers ~:S and~%~
	       specializers ~:S on the generic function ~S~%~
	       was compiled, the method-class for that generic function was~%~
	       ~S. But, the method class is now ~S, this~%~
	       may mean that this method was compiled improperly.~%"
	      qualifiers specializers gf-spec
	      method-class (class-name (class-of method))))
    method))

(defun make-method-spec (gf-spec qualifiers unparsed-specializers)
  `(method ,gf-spec ,@qualifiers ,unparsed-specializers))

(defun initialize-method-function (initargs &optional return-function-p method)
  (let* ((mf (getf initargs :function))
	 (method-spec (getf initargs :method-spec))
	 (plist (getf initargs :plist))
	 (pv-table-symbol (getf plist :pv-table-symbol))
	 (pv-table nil)
	 (mff (getf initargs :fast-function)))
    (flet ((set-mf-property (p v)
	     (when mf
	       (setf (method-function-get mf p) v))
	     (when mff
	       (setf (method-function-get mff p) v))))
      (when method-spec
	(when mf
	  (setq mf (set-fun-name mf method-spec)))
	(when mff
	  (let ((name `(,(or (get (car method-spec) 'fast-sym)
			     (setf (get (car method-spec) 'fast-sym)
				   ;; KLUDGE: If we're going to be
				   ;; interning private symbols in our
				   ;; a this way, it would be cleanest
				   ;; to use a separate package
				   ;; %PCL-PRIVATE or something, and
				   ;; failing that, to use a special
				   ;; symbol prefix denoting privateness.
				   ;; -- WHN 19991201
				   (format-symbol *pcl-package*
						  "FAST-~A" 
						  (car method-spec))))
			,@(cdr method-spec))))
	    (set-fun-name mff name)
	    (unless mf
	      (set-mf-property :name name)))))
      (when plist
	(let ((snl (getf plist :slot-name-lists))
	      (cl (getf plist :call-list)))
	  (when (or snl cl)
	    (setq pv-table (intern-pv-table :slot-name-lists snl
					    :call-list cl))
	    (when pv-table (set pv-table-symbol pv-table))
	    (set-mf-property :pv-table pv-table)))
	(loop (when (null plist) (return nil))
	      (set-mf-property (pop plist) (pop plist)))
	(when method
	  (set-mf-property :method method))
	(when return-function-p
	  (or mf (method-function-from-fast-function mff)))))))

(defun analyze-lambda-list (lambda-list)
  (flet (;; FIXME: Is this redundant with SB-C::MAKE-KEYWORD-FOR-ARG?
	 (parse-key-arg (arg)
	   (if (listp arg)
	       (if (listp (car arg))
		   (caar arg)
		   (keywordicate (car arg)))
	       (keywordicate arg))))
    (let ((nrequired 0)
	  (noptional 0)
	  (keysp nil)
	  (restp nil)
          (nrest 0)
	  (allow-other-keys-p nil)
	  (keywords ())
	  (keyword-parameters ())
	  (state 'required))
      (dolist (x lambda-list)
	(if (memq x lambda-list-keywords)
	    (case x
	      (&optional	 (setq state 'optional))
	      (&key	         (setq keysp t
				       state 'key))
	      (&allow-other-keys (setq allow-other-keys-p t))
	      (&rest             (setq restp t
				       state 'rest))
	      (&aux	      (return t))
	      (otherwise
		(error "encountered the non-standard lambda list keyword ~S"
		       x)))
	    (ecase state
	      (required  (incf nrequired))
	      (optional  (incf noptional))
	      (key       (push (parse-key-arg x) keywords)
			 (push x keyword-parameters))
	      (rest      (incf nrest)))))
      (when (and restp (zerop nrest))
        (error "Error in lambda-list:~%~
                After &REST, a DEFGENERIC lambda-list ~
                must be followed by at least one variable."))
      (values nrequired noptional keysp restp allow-other-keys-p
	      (reverse keywords)
	      (reverse keyword-parameters)))))

(defun keyword-spec-name (x)
  (let ((key (if (atom x) x (car x))))
    (if (atom key)
	(keywordicate key)
	(car key))))

(defun ftype-declaration-from-lambda-list (lambda-list name)
  (multiple-value-bind (nrequired noptional keysp restp allow-other-keys-p
				  keywords keyword-parameters)
      (analyze-lambda-list lambda-list)
    (declare (ignore keyword-parameters))
    (let* ((old (info :function :type name)) ;FIXME:FDOCUMENTATION instead?
	   (old-ftype (if (fun-type-p old) old nil))
	   (old-restp (and old-ftype (fun-type-rest old-ftype)))
	   (old-keys (and old-ftype
			  (mapcar #'key-info-name
				  (fun-type-keywords
				   old-ftype))))
	   (old-keysp (and old-ftype (fun-type-keyp old-ftype)))
	   (old-allowp (and old-ftype
			    (fun-type-allowp old-ftype)))
	   (keywords (union old-keys (mapcar #'keyword-spec-name keywords))))
      `(function ,(append (make-list nrequired :initial-element t)
			  (when (plusp noptional)
			    (append '(&optional)
				    (make-list noptional :initial-element t)))
			  (when (or restp old-restp)
			    '(&rest t))
			  (when (or keysp old-keysp)
			    (append '(&key)
				    (mapcar (lambda (key)
					      `(,key t))
					    keywords)
				    (when (or allow-other-keys-p old-allowp)
				      '(&allow-other-keys)))))
		 *))))

(defun defgeneric-declaration (spec lambda-list)
  `(ftype ,(ftype-declaration-from-lambda-list lambda-list spec) ,spec))

;;;; early generic function support

(defvar *!early-generic-functions* ())

(defun ensure-generic-function (fun-name
				&rest all-keys
				&key environment
				&allow-other-keys)
  (declare (ignore environment))
  (let ((existing (and (gboundp fun-name)
		       (gdefinition fun-name))))
    (if (and existing
	     (eq *boot-state* 'complete)
	     (null (generic-function-p existing)))
	(generic-clobbers-function fun-name)
	(apply #'ensure-generic-function-using-class
	       existing fun-name all-keys))))

(defun generic-clobbers-function (fun-name)
  (error 'simple-program-error
	 :format-control "~S already names an ordinary function or a macro."
	 :format-arguments (list fun-name)))

(defvar *sgf-wrapper*
  (boot-make-wrapper (early-class-size 'standard-generic-function)
		     'standard-generic-function))

(defvar *sgf-slots-init*
  (mapcar (lambda (canonical-slot)
	    (if (memq (getf canonical-slot :name) '(arg-info source))
		+slot-unbound+
		(let ((initfunction (getf canonical-slot :initfunction)))
		  (if initfunction
		      (funcall initfunction)
		      +slot-unbound+))))
	  (early-collect-inheritance 'standard-generic-function)))

(defvar *sgf-method-class-index*
  (!bootstrap-slot-index 'standard-generic-function 'method-class))

(defun early-gf-p (x)
  (and (fsc-instance-p x)
       (eq (clos-slots-ref (get-slots x) *sgf-method-class-index*)
	   +slot-unbound+)))

(defvar *sgf-methods-index*
  (!bootstrap-slot-index 'standard-generic-function 'methods))

(defmacro early-gf-methods (gf)
  `(clos-slots-ref (get-slots ,gf) *sgf-methods-index*))

(defvar *sgf-arg-info-index*
  (!bootstrap-slot-index 'standard-generic-function 'arg-info))

(defmacro early-gf-arg-info (gf)
  `(clos-slots-ref (get-slots ,gf) *sgf-arg-info-index*))

(defvar *sgf-dfun-state-index*
  (!bootstrap-slot-index 'standard-generic-function 'dfun-state))

(defstruct (arg-info
	    (:conc-name nil)
	    (:constructor make-arg-info ())
	    (:copier nil))
  (arg-info-lambda-list :no-lambda-list)
  arg-info-precedence
  arg-info-metatypes
  arg-info-number-optional
  arg-info-key/rest-p
  arg-info-keys   ;nil        no &KEY or &REST allowed
		  ;(k1 k2 ..) Each method must accept these &KEY arguments.
		  ;T	      must have &KEY or &REST

  gf-info-simple-accessor-type ; nil, reader, writer, boundp
  (gf-precompute-dfun-and-emf-p nil) ; set by set-arg-info

  gf-info-static-c-a-m-emf
  (gf-info-c-a-m-emf-std-p t)
  gf-info-fast-mf-p)

#-sb-fluid (declaim (sb-ext:freeze-type arg-info))

(defun arg-info-valid-p (arg-info)
  (not (null (arg-info-number-optional arg-info))))

(defun arg-info-applyp (arg-info)
  (or (plusp (arg-info-number-optional arg-info))
      (arg-info-key/rest-p arg-info)))

(defun arg-info-number-required (arg-info)
  (length (arg-info-metatypes arg-info)))

(defun arg-info-nkeys (arg-info)
  (count-if (lambda (x) (neq x t)) (arg-info-metatypes arg-info)))

;;; Keep pages clean by not setting if the value is already the same.
(defmacro esetf (pos val)
  (with-unique-names (valsym)
    `(let ((,valsym ,val))
       (unless (equal ,pos ,valsym)
	 (setf ,pos ,valsym)))))

(defun create-gf-lambda-list (lambda-list)
  ;;; Create a gf lambda list from a method lambda list
  (loop for x in lambda-list
        collect (if (consp x) (list (car x)) x)
        if (eq x '&key) do (loop-finish)))

(defun set-arg-info (gf &key new-method (lambda-list nil lambda-list-p)
			argument-precedence-order)
  (let* ((arg-info (if (eq *boot-state* 'complete)
		       (gf-arg-info gf)
		       (early-gf-arg-info gf)))
	 (methods (if (eq *boot-state* 'complete)
		      (generic-function-methods gf)
		      (early-gf-methods gf)))
	 (was-valid-p (integerp (arg-info-number-optional arg-info)))
	 (first-p (and new-method (null (cdr methods)))))
    (when (and (not lambda-list-p) methods)
      (setq lambda-list (gf-lambda-list gf)))
    (when (or lambda-list-p
	      (and first-p
		   (eq (arg-info-lambda-list arg-info) :no-lambda-list)))
      (multiple-value-bind (nreq nopt keysp restp allow-other-keys-p keywords)
	  (analyze-lambda-list lambda-list)
	(when (and methods (not first-p))
	  (let ((gf-nreq (arg-info-number-required arg-info))
		(gf-nopt (arg-info-number-optional arg-info))
		(gf-key/rest-p (arg-info-key/rest-p arg-info)))
	    (unless (and (= nreq gf-nreq)
			 (= nopt gf-nopt)
			 (eq (or keysp restp) gf-key/rest-p))
	      (error "The lambda-list ~S is incompatible with ~
		     existing methods of ~S."
		     lambda-list gf))))
        (esetf (arg-info-lambda-list arg-info)
               (if lambda-list-p
                   lambda-list
                   (create-gf-lambda-list lambda-list)))
	(when (or lambda-list-p argument-precedence-order
		  (null (arg-info-precedence arg-info)))
	  (esetf (arg-info-precedence arg-info)
		 (compute-precedence lambda-list nreq
				     argument-precedence-order)))
	(esetf (arg-info-metatypes arg-info) (make-list nreq))
	(esetf (arg-info-number-optional arg-info) nopt)
	(esetf (arg-info-key/rest-p arg-info) (not (null (or keysp restp))))
	(esetf (arg-info-keys arg-info)
	       (if lambda-list-p
		   (if allow-other-keys-p t keywords)
		   (arg-info-key/rest-p arg-info)))))
    (when new-method
      (check-method-arg-info gf arg-info new-method))
    (set-arg-info1 gf arg-info new-method methods was-valid-p first-p)
    arg-info))

(defun check-method-arg-info (gf arg-info method)
  (multiple-value-bind (nreq nopt keysp restp allow-other-keys-p keywords)
      (analyze-lambda-list (if (consp method)
			       (early-method-lambda-list method)
			       (method-lambda-list method)))
    (flet ((lose (string &rest args)
	     (error 'simple-program-error
		    :format-control "~@<attempt to add the method~2I~_~S~I~_~
                                     to the generic function~2I~_~S;~I~_~
                                     but ~?~:>"
		    :format-arguments (list method gf string args)))
	   (comparison-description (x y)
	     (if (> x y) "more" "fewer")))
      (let ((gf-nreq (arg-info-number-required arg-info))
	    (gf-nopt (arg-info-number-optional arg-info))
	    (gf-key/rest-p (arg-info-key/rest-p arg-info))
	    (gf-keywords (arg-info-keys arg-info)))
	(unless (= nreq gf-nreq)
	  (lose
	   "the method has ~A required arguments than the generic function."
	   (comparison-description nreq gf-nreq)))
	(unless (= nopt gf-nopt)
	  (lose
	   "the method has ~A optional arguments than the generic function."
	   (comparison-description nopt gf-nopt)))
	(unless (eq (or keysp restp) gf-key/rest-p)
	  (lose
	   "the method and generic function differ in whether they accept~_~
	    &REST or &KEY arguments."))
	(when (consp gf-keywords)
	  (unless (or (and restp (not keysp))
		      allow-other-keys-p
		      (every (lambda (k) (memq k keywords)) gf-keywords))
	    (lose "the method does not accept each of the &KEY arguments~2I~_~
		   ~S."
		  gf-keywords)))))))

(defun set-arg-info1 (gf arg-info new-method methods was-valid-p first-p)
  (let* ((existing-p (and methods (cdr methods) new-method))
	 (nreq (length (arg-info-metatypes arg-info)))
	 (metatypes (if existing-p
			(arg-info-metatypes arg-info)
			(make-list nreq)))
	 (type (if existing-p
		   (gf-info-simple-accessor-type arg-info)
		   nil)))
    (when (arg-info-valid-p arg-info)
      (dolist (method (if new-method (list new-method) methods))
	(let* ((specializers (if (or (eq *boot-state* 'complete)
				     (not (consp method)))
				 (method-specializers method)
				 (early-method-specializers method t)))
	       (class (if (or (eq *boot-state* 'complete) (not (consp method)))
			  (class-of method)
			  (early-method-class method)))
	       (new-type (when (and class
				    (or (not (eq *boot-state* 'complete))
					(eq (generic-function-method-combination gf)
					    *standard-method-combination*)))
			   (cond ((eq class *the-class-standard-reader-method*)
				  'reader)
				 ((eq class *the-class-standard-writer-method*)
				  'writer)
				 ((eq class *the-class-standard-boundp-method*)
				  'boundp)))))
	  (setq metatypes (mapcar #'raise-metatype metatypes specializers))
	  (setq type (cond ((null type) new-type)
			   ((eq type new-type) type)
			   (t nil)))))
      (esetf (arg-info-metatypes arg-info) metatypes)
      (esetf (gf-info-simple-accessor-type arg-info) type)))
  (when (or (not was-valid-p) first-p)
    (multiple-value-bind (c-a-m-emf std-p)
	(if (early-gf-p gf)
	    (values t t)
	    (compute-applicable-methods-emf gf))
      (esetf (gf-info-static-c-a-m-emf arg-info) c-a-m-emf)
      (esetf (gf-info-c-a-m-emf-std-p arg-info) std-p)
      (unless (gf-info-c-a-m-emf-std-p arg-info)
	(esetf (gf-info-simple-accessor-type arg-info) t))))
  (unless was-valid-p
    (let ((name (if (eq *boot-state* 'complete)
		    (generic-function-name gf)
		    (!early-gf-name gf))))
      (esetf (gf-precompute-dfun-and-emf-p arg-info)
	     (cond
	       ((and (consp name)
		     (member (car name)
			     *internal-pcl-generalized-fun-name-symbols*))
		nil)
	       (t (let* ((symbol (fun-name-block-name name))
			 (package (symbol-package symbol)))
		    (and (or (eq package *pcl-package*)
			     (memq package (package-use-list *pcl-package*)))
			 ;; FIXME: this test will eventually be
			 ;; superseded by the *internal-pcl...* test,
			 ;; above.  While we are in a process of
			 ;; transition, however, it should probably
			 ;; remain.
			 (not (find #\Space (symbol-name symbol))))))))))
  (esetf (gf-info-fast-mf-p arg-info)
	 (or (not (eq *boot-state* 'complete))
	     (let* ((method-class (generic-function-method-class gf))
		    (methods (compute-applicable-methods
			      #'make-method-lambda
			      (list gf (class-prototype method-class)
				    '(lambda) nil))))
	       (and methods (null (cdr methods))
		    (let ((specls (method-specializers (car methods))))
		      (and (classp (car specls))
			   (eq 'standard-generic-function
			       (class-name (car specls)))
			   (classp (cadr specls))
			   (eq 'standard-method
			       (class-name (cadr specls)))))))))
  arg-info)

;;; This is the early definition of ENSURE-GENERIC-FUNCTION-USING-CLASS.
;;;
;;; The STATIC-SLOTS field of the funcallable instances used as early
;;; generic functions is used to store the early methods and early
;;; discriminator code for the early generic function. The static
;;; slots field of the fins contains a list whose:
;;;    CAR    -   a list of the early methods on this early gf
;;;    CADR   -   the early discriminator code for this method
(defun ensure-generic-function-using-class (existing spec &rest keys
					    &key (lambda-list nil
							      lambda-list-p)
					    argument-precedence-order
					    &allow-other-keys)
  (declare (ignore keys))
  (cond ((and existing (early-gf-p existing))
	 (when lambda-list-p
	   (set-arg-info existing :lambda-list lambda-list))
	 existing)
	((assoc spec *!generic-function-fixups* :test #'equal)
	 (if existing
	     (make-early-gf spec lambda-list lambda-list-p existing
			    argument-precedence-order)
	     (error "The function ~S is not already defined." spec)))
	(existing
	 (error "~S should be on the list ~S."
		spec
		'*!generic-function-fixups*))
	(t
	 (pushnew spec *!early-generic-functions* :test #'equal)
	 (make-early-gf spec lambda-list lambda-list-p nil
			argument-precedence-order))))

(defun make-early-gf (spec &optional lambda-list lambda-list-p
		      function argument-precedence-order)
  (let ((fin (allocate-funcallable-instance *sgf-wrapper* *sgf-slots-init*)))
    (set-funcallable-instance-function
     fin
     (or function
	 (if (eq spec 'print-object)
	     #'(instance-lambda (instance stream)
		 (print-unreadable-object (instance stream :identity t)
		   (format stream "std-instance")))
	     #'(instance-lambda (&rest args)
		 (declare (ignore args))
		 (error "The function of the funcallable-instance ~S~
			 has not been set." fin)))))
    (setf (gdefinition spec) fin)
    (!bootstrap-set-slot 'standard-generic-function fin 'name spec)
    (!bootstrap-set-slot 'standard-generic-function
			 fin
			 'source
			 *load-pathname*)
    (set-fun-name fin spec)
    (let ((arg-info (make-arg-info)))
      (setf (early-gf-arg-info fin) arg-info)
      (when lambda-list-p
	(proclaim (defgeneric-declaration spec lambda-list))
	(if argument-precedence-order
	    (set-arg-info fin
			  :lambda-list lambda-list
			  :argument-precedence-order argument-precedence-order)
	    (set-arg-info fin :lambda-list lambda-list))))
    fin))

(defun set-dfun (gf &optional dfun cache info)
  (when cache
    (setf (cache-owner cache) gf))
  (let ((new-state (if (and dfun (or cache info))
		       (list* dfun cache info)
		       dfun)))
    (if (eq *boot-state* 'complete)
	(setf (gf-dfun-state gf) new-state)
	(setf (clos-slots-ref (get-slots gf) *sgf-dfun-state-index*)
	      new-state)))
  dfun)

(defun gf-dfun-cache (gf)
  (let ((state (if (eq *boot-state* 'complete)
		   (gf-dfun-state gf)
		   (clos-slots-ref (get-slots gf) *sgf-dfun-state-index*))))
    (typecase state
      (function nil)
      (cons (cadr state)))))

(defun gf-dfun-info (gf)
  (let ((state (if (eq *boot-state* 'complete)
		   (gf-dfun-state gf)
		   (clos-slots-ref (get-slots gf) *sgf-dfun-state-index*))))
    (typecase state
      (function nil)
      (cons (cddr state)))))

(defvar *sgf-name-index*
  (!bootstrap-slot-index 'standard-generic-function 'name))

(defun !early-gf-name (gf)
  (clos-slots-ref (get-slots gf) *sgf-name-index*))

(defun gf-lambda-list (gf)
  (let ((arg-info (if (eq *boot-state* 'complete)
		      (gf-arg-info gf)
		      (early-gf-arg-info gf))))
    (if (eq :no-lambda-list (arg-info-lambda-list arg-info))
	(let ((methods (if (eq *boot-state* 'complete)
			   (generic-function-methods gf)
			   (early-gf-methods gf))))
	  (if (null methods)
	      (progn
		(warn "no way to determine the lambda list for ~S" gf)
		nil)
	      (let* ((method (car (last methods)))
		     (ll (if (consp method)
			     (early-method-lambda-list method)
			     (method-lambda-list method))))
                (create-gf-lambda-list ll))))
	(arg-info-lambda-list arg-info))))

(defmacro real-ensure-gf-internal (gf-class all-keys env)
  `(progn
     (cond ((symbolp ,gf-class)
	    (setq ,gf-class (find-class ,gf-class t ,env)))
	   ((classp ,gf-class))
	   (t
	    (error "The :GENERIC-FUNCTION-CLASS argument (~S) was neither a~%~
		    class nor a symbol that names a class."
		   ,gf-class)))
     (remf ,all-keys :generic-function-class)
     (remf ,all-keys :environment)
     (let ((combin (getf ,all-keys :method-combination '.shes-not-there.)))
       (unless (eq combin '.shes-not-there.)
	 (setf (getf ,all-keys :method-combination)
	       (find-method-combination (class-prototype ,gf-class)
					(car combin)
					(cdr combin)))))
    (let ((method-class (getf ,all-keys :method-class '.shes-not-there.)))
      (unless (eq method-class '.shes-not-there.)
        (setf (getf ,all-keys :method-class)
	      (find-class method-class t ,env))))))

(defun real-ensure-gf-using-class--generic-function
       (existing
	fun-name
	&rest all-keys
	&key environment (lambda-list nil lambda-list-p)
	     (generic-function-class 'standard-generic-function gf-class-p)
	&allow-other-keys)
  (real-ensure-gf-internal generic-function-class all-keys environment)
  (unless (or (null gf-class-p)
	      (eq (class-of existing) generic-function-class))
    (change-class existing generic-function-class))
  (prog1
      (apply #'reinitialize-instance existing all-keys)
    (when lambda-list-p
      (proclaim (defgeneric-declaration fun-name lambda-list)))))

(defun real-ensure-gf-using-class--null
       (existing
	fun-name
	&rest all-keys
	&key environment (lambda-list nil lambda-list-p)
	     (generic-function-class 'standard-generic-function)
	&allow-other-keys)
  (declare (ignore existing))
  (real-ensure-gf-internal generic-function-class all-keys environment)
  (prog1
      (setf (gdefinition fun-name)
	    (apply #'make-instance generic-function-class
		   :name fun-name all-keys))
    (when lambda-list-p
      (proclaim (defgeneric-declaration fun-name lambda-list)))))

(defun get-generic-fun-info (gf)
  ;; values   nreq applyp metatypes nkeys arg-info
  (multiple-value-bind (applyp metatypes arg-info)
      (let* ((arg-info (if (early-gf-p gf)
			   (early-gf-arg-info gf)
			   (gf-arg-info gf)))
	     (metatypes (arg-info-metatypes arg-info)))
	(values (arg-info-applyp arg-info)
		metatypes
		arg-info))
    (values (length metatypes) applyp metatypes
	    (count-if (lambda (x) (neq x t)) metatypes)
	    arg-info)))

(defun early-make-a-method (class qualifiers arglist specializers initargs doc
			    &optional slot-name)
  (initialize-method-function initargs)
  (let ((parsed ())
	(unparsed ()))
    ;; Figure out whether we got class objects or class names as the
    ;; specializers and set parsed and unparsed appropriately. If we
    ;; got class objects, then we can compute unparsed, but if we got
    ;; class names we don't try to compute parsed.
    ;;
    ;; Note that the use of not symbolp in this call to every should be
    ;; read as 'classp' we can't use classp itself because it doesn't
    ;; exist yet.
    (if (every (lambda (s) (not (symbolp s))) specializers)
	(setq parsed specializers
	      unparsed (mapcar (lambda (s)
				 (if (eq s t) t (class-name s)))
			       specializers))
	(setq unparsed specializers
	      parsed ()))
    (list :early-method		  ;This is an early method dammit!

	  (getf initargs :function)
	  (getf initargs :fast-function)

	  parsed		  ;The parsed specializers. This is used
				  ;by early-method-specializers to cache
				  ;the parse. Note that this only comes
				  ;into play when there is more than one
				  ;early method on an early gf.

	  (list class	     ;A list to which real-make-a-method
		qualifiers	;can be applied to make a real method
		arglist	   ;corresponding to this early one.
		unparsed
		initargs
		doc
		slot-name))))

(defun real-make-a-method
       (class qualifiers lambda-list specializers initargs doc
	&optional slot-name)
  (setq specializers (parse-specializers specializers))
  (apply #'make-instance class
	 :qualifiers qualifiers
	 :lambda-list lambda-list
	 :specializers specializers
	 :documentation doc
	 :slot-name slot-name
	 :allow-other-keys t
	 initargs))

(defun early-method-function (early-method)
  (values (cadr early-method) (caddr early-method)))

(defun early-method-class (early-method)
  (find-class (car (fifth early-method))))

(defun early-method-standard-accessor-p (early-method)
  (let ((class (first (fifth early-method))))
    (or (eq class 'standard-reader-method)
	(eq class 'standard-writer-method)
	(eq class 'standard-boundp-method))))

(defun early-method-standard-accessor-slot-name (early-method)
  (seventh (fifth early-method)))

;;; Fetch the specializers of an early method. This is basically just
;;; a simple accessor except that when the second argument is t, this
;;; converts the specializers from symbols into class objects. The
;;; class objects are cached in the early method, this makes
;;; bootstrapping faster because the class objects only have to be
;;; computed once.
;;;
;;; NOTE:
;;;  The second argument should only be passed as T by
;;;  early-lookup-method. This is to implement the rule that only when
;;;  there is more than one early method on a generic function is the
;;;  conversion from class names to class objects done. This
;;;  corresponds to the fact that we are only allowed to have one
;;;  method on any generic function up until the time classes exist.
(defun early-method-specializers (early-method &optional objectsp)
  (if (and (listp early-method)
	   (eq (car early-method) :early-method))
      (cond ((eq objectsp t)
	     (or (fourth early-method)
		 (setf (fourth early-method)
		       (mapcar #'find-class (cadddr (fifth early-method))))))
	    (t
	     (cadddr (fifth early-method))))
      (error "~S is not an early-method." early-method)))

(defun early-method-qualifiers (early-method)
  (cadr (fifth early-method)))

(defun early-method-lambda-list (early-method)
  (caddr (fifth early-method)))

(defun early-add-named-method (generic-function-name
			       qualifiers
			       specializers
			       arglist
			       &rest initargs)
  (let* ((gf (ensure-generic-function generic-function-name))
	 (existing
	   (dolist (m (early-gf-methods gf))
	     (when (and (equal (early-method-specializers m) specializers)
			(equal (early-method-qualifiers m) qualifiers))
	       (return m))))
	 (new (make-a-method 'standard-method
			     qualifiers
			     arglist
			     specializers
			     initargs
			     ())))
    (when existing (remove-method gf existing))
    (add-method gf new)))

;;; This is the early version of ADD-METHOD. Later this will become a
;;; generic function. See !FIX-EARLY-GENERIC-FUNCTIONS which has
;;; special knowledge about ADD-METHOD.
(defun add-method (generic-function method)
  (when (not (fsc-instance-p generic-function))
    (error "Early ADD-METHOD didn't get a funcallable instance."))
  (when (not (and (listp method) (eq (car method) :early-method)))
    (error "Early ADD-METHOD didn't get an early method."))
  (push method (early-gf-methods generic-function))
  (set-arg-info generic-function :new-method method)
  (unless (assoc (!early-gf-name generic-function)
		 *!generic-function-fixups*
		 :test #'equal)
    (update-dfun generic-function)))

;;; This is the early version of REMOVE-METHOD. See comments on
;;; the early version of ADD-METHOD.
(defun remove-method (generic-function method)
  (when (not (fsc-instance-p generic-function))
    (error "An early remove-method didn't get a funcallable instance."))
  (when (not (and (listp method) (eq (car method) :early-method)))
    (error "An early remove-method didn't get an early method."))
  (setf (early-gf-methods generic-function)
	(remove method (early-gf-methods generic-function)))
  (set-arg-info generic-function)
  (unless (assoc (!early-gf-name generic-function)
		 *!generic-function-fixups*
		 :test #'equal)
    (update-dfun generic-function)))

;;; This is the early version of GET-METHOD. See comments on the early
;;; version of ADD-METHOD.
(defun get-method (generic-function qualifiers specializers
				    &optional (errorp t))
  (if (early-gf-p generic-function)
      (or (dolist (m (early-gf-methods generic-function))
	    (when (and (or (equal (early-method-specializers m nil)
				  specializers)
			   (equal (early-method-specializers m t)
				  specializers))
		       (equal (early-method-qualifiers m) qualifiers))
	      (return m)))
	  (if errorp
	      (error "can't get early method")
	      nil))
      (real-get-method generic-function qualifiers specializers errorp)))

(defun !fix-early-generic-functions ()
  (let ((accessors nil))
    ;; Rearrange *!EARLY-GENERIC-FUNCTIONS* to speed up
    ;; FIX-EARLY-GENERIC-FUNCTIONS.
    (dolist (early-gf-spec *!early-generic-functions*)
      (when (every #'early-method-standard-accessor-p
		   (early-gf-methods (gdefinition early-gf-spec)))
	(push early-gf-spec accessors)))
    (dolist (spec (nconc accessors
			 '(accessor-method-slot-name
			   generic-function-methods
			   method-specializers
			   specializerp
			   specializer-type
			   specializer-class
			   slot-definition-location
			   slot-definition-name
			   class-slots
			   gf-arg-info
			   class-precedence-list
			   slot-boundp-using-class
			   (setf slot-value-using-class)
			   slot-value-using-class
			   structure-class-p
			   standard-class-p
			   funcallable-standard-class-p
			   specializerp)))
      (/show spec)
      (setq *!early-generic-functions*
	    (cons spec
		  (delete spec *!early-generic-functions* :test #'equal))))

    (dolist (early-gf-spec *!early-generic-functions*)
      (/show early-gf-spec)
      (let* ((gf (gdefinition early-gf-spec))
	     (methods (mapcar (lambda (early-method)
				(let ((args (copy-list (fifth
							early-method))))
				  (setf (fourth args)
					(early-method-specializers
					 early-method t))
				  (apply #'real-make-a-method args)))
			      (early-gf-methods gf))))
	(setf (generic-function-method-class gf) *the-class-standard-method*)
	(setf (generic-function-method-combination gf)
	      *standard-method-combination*)
	(set-methods gf methods)))

    (dolist (fn *!early-functions*)
      (/show fn)
      (setf (gdefinition (car fn)) (fdefinition (caddr fn))))

    (dolist (fixup *!generic-function-fixups*)
      (/show fixup)
      (let* ((fspec (car fixup))
	     (gf (gdefinition fspec))
	     (methods (mapcar (lambda (method)
				(let* ((lambda-list (first method))
				       (specializers (second method))
				       (method-fn-name (third method))
				       (fn-name (or method-fn-name fspec))
				       (fn (fdefinition fn-name))
				       (initargs
					(list :function
					      (set-fun-name
					       (lambda (args next-methods)
						 (declare (ignore
							   next-methods))
						 (apply fn args))
					       `(call ,fn-name)))))
				  (declare (type function fn))
				  (make-a-method 'standard-method
						 ()
						 lambda-list
						 specializers
						 initargs
						 nil)))
			      (cdr fixup))))
	(setf (generic-function-method-class gf) *the-class-standard-method*)
	(setf (generic-function-method-combination gf)
	      *standard-method-combination*)
	(set-methods gf methods))))
  (/show "leaving !FIX-EARLY-GENERIC-FUNCTIONS"))

;;; PARSE-DEFMETHOD is used by DEFMETHOD to parse the &REST argument
;;; into the 'real' arguments. This is where the syntax of DEFMETHOD
;;; is really implemented.
(defun parse-defmethod (cdr-of-form)
  (declare (list cdr-of-form))
  (let ((name (pop cdr-of-form))
	(qualifiers ())
	(spec-ll ()))
    (loop (if (and (car cdr-of-form) (atom (car cdr-of-form)))
	      (push (pop cdr-of-form) qualifiers)
	      (return (setq qualifiers (nreverse qualifiers)))))
    (setq spec-ll (pop cdr-of-form))
    (values name qualifiers spec-ll cdr-of-form)))

(defun parse-specializers (specializers)
  (declare (list specializers))
  (flet ((parse (spec)
	   (let ((result (specializer-from-type spec)))
	     (if (specializerp result)
		 result
		 (if (symbolp spec)
		     (error "~S was used as a specializer,~%~
			     but is not the name of a class."
			    spec)
		     (error "~S is not a legal specializer." spec))))))
    (mapcar #'parse specializers)))

(defun unparse-specializers (specializers-or-method)
  (if (listp specializers-or-method)
      (flet ((unparse (spec)
	       (if (specializerp spec)
		   (let ((type (specializer-type spec)))
		     (if (and (consp type)
			      (eq (car type) 'class))
			 (let* ((class (cadr type))
				(class-name (class-name class)))
			   (if (eq class (find-class class-name nil))
			       class-name
			       type))
			 type))
		   (error "~S is not a legal specializer." spec))))
	(mapcar #'unparse specializers-or-method))
      (unparse-specializers (method-specializers specializers-or-method))))

(defun parse-method-or-spec (spec &optional (errorp t))
  (let (gf method name temp)
    (if (method-p spec)	
	(setq method spec
	      gf (method-generic-function method)
	      temp (and gf (generic-function-name gf))
	      name (if temp
		       (intern-fun-name
			 (make-method-spec temp
					   (method-qualifiers method)
					   (unparse-specializers
					     (method-specializers method))))
		       (make-symbol (format nil "~S" method))))
	(multiple-value-bind (gf-spec quals specls)
	    (parse-defmethod spec)
	  (and (setq gf (and (or errorp (gboundp gf-spec))
			     (gdefinition gf-spec)))
	       (let ((nreq (compute-discriminating-function-arglist-info gf)))
		 (setq specls (append (parse-specializers specls)
				      (make-list (- nreq (length specls))
						 :initial-element
						 *the-class-t*)))
		 (and
		   (setq method (get-method gf quals specls errorp))
		   (setq name
			 (intern-fun-name (make-method-spec gf-spec
							    quals
							    specls))))))))
    (values gf method name)))

(defun extract-parameters (specialized-lambda-list)
  (multiple-value-bind (parameters ignore1 ignore2)
      (parse-specialized-lambda-list specialized-lambda-list)
    (declare (ignore ignore1 ignore2))
    parameters))

(defun extract-lambda-list (specialized-lambda-list)
  (multiple-value-bind (ignore1 lambda-list ignore2)
      (parse-specialized-lambda-list specialized-lambda-list)
    (declare (ignore ignore1 ignore2))
    lambda-list))

(defun extract-specializer-names (specialized-lambda-list)
  (multiple-value-bind (ignore1 ignore2 specializers)
      (parse-specialized-lambda-list specialized-lambda-list)
    (declare (ignore ignore1 ignore2))
    specializers))

(defun extract-required-parameters (specialized-lambda-list)
  (multiple-value-bind (ignore1 ignore2 ignore3 required-parameters)
      (parse-specialized-lambda-list specialized-lambda-list)
    (declare (ignore ignore1 ignore2 ignore3))
    required-parameters))

(define-condition specialized-lambda-list-error
    (reference-condition simple-program-error)
  ()
  (:default-initargs :references (list '(:ansi-cl :section (3 4 3)))))

(defun parse-specialized-lambda-list
    (arglist
     &optional supplied-keywords (allowed-keywords '(&optional &rest &key &aux))
     &aux (specialized-lambda-list-keywords
	   '(&optional &rest &key &allow-other-keys &aux)))
  (let ((arg (car arglist)))
    (cond ((null arglist) (values nil nil nil nil))
	  ((eq arg '&aux)
	   (values nil arglist nil nil))
	  ((memq arg lambda-list-keywords)
	   ;; non-standard lambda-list-keywords are errors.
	   (unless (memq arg specialized-lambda-list-keywords)
	     (error 'specialized-lambda-list-error
		    :format-control "unknown specialized-lambda-list ~
                                     keyword ~S~%"
		    :format-arguments (list arg)))
	   ;; no multiple &rest x &rest bla specifying
	   (when (memq arg supplied-keywords)
	     (error 'specialized-lambda-list-error
		    :format-control "multiple occurrence of ~
                                     specialized-lambda-list keyword ~S~%"
		    :format-arguments (list arg)))
	   ;; And no placing &key in front of &optional, either.
	   (unless (memq arg allowed-keywords)
	     (error 'specialized-lambda-list-error
		    :format-control "misplaced specialized-lambda-list ~
                                     keyword ~S~%"
		    :format-arguments (list arg)))
	   ;; When we are at a lambda-list keyword, the parameters
	   ;; don't include the lambda-list keyword; the lambda-list
	   ;; does include the lambda-list keyword; and no
	   ;; specializers are allowed to follow the lambda-list
	   ;; keywords (at least for now).
	   (multiple-value-bind (parameters lambda-list)
	       (parse-specialized-lambda-list (cdr arglist)
					      (cons arg supplied-keywords)
					      (if (eq arg '&key)
						  (cons '&allow-other-keys
							(cdr (member arg allowed-keywords)))
						(cdr (member arg allowed-keywords))))
	     (when (and (eq arg '&rest)
			(or (null lambda-list)
			    (memq (car lambda-list)
				  specialized-lambda-list-keywords)
			    (not (or (null (cadr lambda-list))
				     (memq (cadr lambda-list)
					   specialized-lambda-list-keywords)))))
	       (error 'specialized-lambda-list-error
		      :format-control
		      "in a specialized-lambda-list, excactly one ~
                       variable must follow &REST.~%"
		      :format-arguments nil))
	     (values parameters
		     (cons arg lambda-list)
		     ()
		     ())))
	  (supplied-keywords
	   ;; After a lambda-list keyword there can be no specializers.
	   (multiple-value-bind (parameters lambda-list)
	       (parse-specialized-lambda-list (cdr arglist)
					      supplied-keywords
					      allowed-keywords)
	     (values (cons (if (listp arg) (car arg) arg) parameters)
		     (cons arg lambda-list)
		     ()
		     ())))
	  (t
	   (multiple-value-bind (parameters lambda-list specializers required)
	       (parse-specialized-lambda-list (cdr arglist))
	     (values (cons (if (listp arg) (car arg) arg) parameters)
		     (cons (if (listp arg) (car arg) arg) lambda-list)
		     (cons (if (listp arg) (cadr arg) t) specializers)
		     (cons (if (listp arg) (car arg) arg) required)))))))

(setq *boot-state* 'early)

;;; FIXME: In here there was a #-CMU definition of SYMBOL-MACROLET
;;; which used %WALKER stuff. That suggests to me that maybe the code
;;; walker stuff was only used for implementing stuff like that; maybe
;;; it's not needed any more? Hunt down what it was used for and see.

(defmacro with-slots (slots instance &body body)
  (let ((in (gensym)))
    `(let ((,in ,instance))
       (declare (ignorable ,in))
       ,@(let ((instance (if (and (consp instance) (eq (car instance) 'the))
			     (third instance)
			     instance)))
	   (and (symbolp instance)
		`((declare (%variable-rebinding ,in ,instance)))))
       ,in
       (symbol-macrolet ,(mapcar (lambda (slot-entry)
				   (let ((var-name
					  (if (symbolp slot-entry)
					      slot-entry
					      (car slot-entry)))
					 (slot-name
					  (if (symbolp slot-entry)
					      slot-entry
					      (cadr slot-entry))))
				     `(,var-name
				       (slot-value ,in ',slot-name))))
				 slots)
			,@body))))

(defmacro with-accessors (slots instance &body body)
  (let ((in (gensym)))
    `(let ((,in ,instance))
       (declare (ignorable ,in))
       ,@(let ((instance (if (and (consp instance) (eq (car instance) 'the))
			     (third instance)
			     instance)))
	   (and (symbolp instance)
		`((declare (%variable-rebinding ,in ,instance)))))
       ,in
       (symbol-macrolet ,(mapcar (lambda (slot-entry)
				   (let ((var-name (car slot-entry))
					 (accessor-name (cadr slot-entry)))
				     `(,var-name (,accessor-name ,in))))
				 slots)
	  ,@body))))
