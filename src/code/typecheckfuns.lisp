;;;; Out-of-line structure slot accessor functions need to do type
;;;; tests. These accessor functions aren't called very often, so it's
;;;; unreasonable to implement them all as different compiled
;;;; functions. But when they are called, it's not reasonable to just
;;;; punt to interpreted TYPEP. The system implemented here is 
;;;; a solution to this problem.
;;;;
;;;; Structure accessor functions are still implemented as closures,
;;;; but now one of the closed-over variables is a function which does
;;;; the type test. When a type can be expanded fully into known
;;;; types at compile time, we compile a LAMBDA which does TYPEP on it, and
;;;; use that. If the function can't be expanded at compile time,
;;;; then it can't be compiled efficiently anyway, so we just emit a note.
;;;;
;;;; As a further wrinkle on this, we reuse the type-test functions,
;;;; so that the dozens of slot accessors which have e.g. :TYPE SYMBOL
;;;; can all share the same code instead of having to keep dozens of
;;;; copies of the same function floating around. We can also pull a few
;;;; other tricks to reduce bloat, like implementing tests for structure
;;;; classes as a closure over structure LAYOUTs.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; setting up to precompile code for common types once and for all
(declaim (type simple-vector *typecheckfun-standard-typespecs*))
(declaim (type simple-vector *typecheckfun-standard-typespecs*))
(eval-when (:compile-toplevel)
  ;; When we generate collections of standard specialized array types,
  ;; what should their element types be?
  (defvar *typecheckfun-standard-element-typespecs*
    ;; Note: This table is pretty arbitrary, just things I use a lot
    ;; or see used a lot. If someone has ideas for better values,
    ;; lemme know. -- WHN 2001-10-15
    #(t
      character
      bit fixnum (unsigned-byte 32) (signed-byte 32)
      single-float double-float))
  ;; What are the standard testable types? (If a slot accessor looks
  ;; up one of these types, it doesn't need to supply a compiled TYPEP
  ;; function to initialize the possibly-empty entry: instead it's
  ;; guaranteed that the entry is there. This should save some compile
  ;; time and object file bloat.)
  (defvar *typecheckfun-standard-typespecs*
    (coerce (remove-duplicates
	     (mapcar (lambda (typespec)
		       (type-specifier (specifier-type typespec)))
		     ;; Note: This collection of input values is
		     ;; pretty arbitrary, just inspired by things I
		     ;; use a lot or see being used a lot in the
		     ;; system. If someone has ideas for better
		     ;; values, lemme know. -- WHN 2001-10-15
		     (concatenate
		      'list
		      ;; non-array types
		      '(bit
			boolean
			character
			cons
			double-float
			fixnum
			hash-table
			index
			integer
			list
			package
			signed-byte
			(signed-byte 8)
			single-float
			structure-object
			symbol
			unsigned-byte
			(unsigned-byte 8)
			(unsigned-byte 32))
		      ;; systematic names for array types
		      (map 'list
			   (lambda (element-type)
			     `(simple-array ,element-type 1))
			   *typecheckfun-standard-element-typespecs*)
		      (map 'list
			   (lambda (element-type)
			     `(vector ,element-type))
			   *typecheckfun-standard-element-typespecs*)
		      ;; idiosyncratic names for array types
		      '(simple-vector
			bit-vector simple-bit-vector
			string simple-string)))
	     :test #'equal)
	    'simple-vector)))

(defun ctype-is-standard-typecheckfun-type-p (ctype)
  (position (type-specifier ctype) *typecheckfun-standard-typespecs*
	    :test #'equal))

(defun typecheck-failure (arg typespec)
  (error 'type-error :datum arg :expected-type typespec))

;;; memoization cache for typecheckfuns: a map from fully-expanded type
;;; specifiers to functions which test the type of their argument
(defvar *typecheckfuns*
  (make-hash-table :test 'equal))

;;; Memoize the FORM which returns a typecheckfun for TYPESPEC.
(defmacro memoized-typecheckfun-form (form typespec)
  (let ((n-typespec (gensym "TYPESPEC")))
    `(let ((,n-typespec ,typespec))
       (or (gethash ,n-typespec *typecheckfuns*)
	   (setf (gethash ,n-typespec *typecheckfuns*)
		 ,form)))))

;;; Initialize the memoization cache with typecheckfuns for
;;; *TYPECHECKFUN-STANDARD-TYPESPECS*.
(macrolet ((macro ()
             `(progn
		,@(map 'list
		       (lambda (typespec)
			 `(setf (gethash ',typespec *typecheckfuns*)
				(lambda (arg)
				  (unless (typep arg ',typespec)
				    (typecheck-failure arg ',typespec))
				  (values))))
		       *typecheckfun-standard-typespecs*))))
  (macro)) 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (warn "FIXME: Init *TYPECHECKFUN-STANDARD-TYPESPECS* at cold init time?")
  (warn "FIXME: Don't forget to clear the cache when a structure type is undefined."))

;;; Return a trivial best-you-can-expect-when-you-don't-predefine-the-type
;;; implementation of a function which checks the type of its argument.
(defun interpreted-typecheckfun (typespec)
  ;; Note that we don't and shouldn't memoize this, since otherwise the
  ;; user could do 
  ;;   (DEFSTRUCT FOO (X NIL :TYPE XYTYPE))
  ;;   (DEFTYPE XYTYPE () (OR SYMBOL CHARACTER))
  ;;   (DEFSTRUCT BAR (Y NIL :TYPE XYTYPE))
  ;; and be unpleasantly surprised when the memoized old interpreted
  ;; type check from the FOO-X slot setter interfered with the
  ;; construction of a shiny new compiled type check for the BAR-Y
  ;; slot setter.
  (lambda (arg)
    (unless (typep arg typespec)
      (typecheck-failure arg typespec))
    (values)))

;;; Type checks for structure objects are all implemented the same
;;; way, with only the LAYOUT varying, so they're practically begging
;;; to be implemented as closures over the layout.
(defun %structure-object-typecheckfun (typespec)
  (let ((layout (compiler-layout-or-lose typespec)))
    (lambda (arg)
      (unless (typep-to-layout arg layout)
	(typecheck-failure arg typespec))
      (values))))
(defun structure-object-typecheckfun (typespec)
  (memoized-typecheckfun-form (%structure-object-typecheckfun typespec)
			      typespec))

;;; General type checks need the full compiler, not just stereotyped
;;; closures. We arrange for UNMEMOIZED-TYPECHECKFUN to be produced
;;; for us at compile time (or it can be skipped if the compiler knows
;;; that the memoization lookup can't fail).
(defun general-typecheckfun (typespec &optional unmemoized-typecheckfun)
  (or (gethash typespec *typecheckfuns*)
      (setf (gethash typespec *typecheckfuns*) unmemoized-typecheckfun)
      ;; UNMEMOIZED-TYPECHECKFUN shouldn't be NIL unless the compiler
      ;; knew that the memo would exist, so we shouldn't be here.
      (error "internal error: no typecheckfun memo for ~%  ~S" typespec)))

(defun ctype-needs-to-be-interpreted-p (ctype)
  ;; What we should really do is factor out the code in
  ;; (DEF-SOURCE-TRANSFORM TYPEP ..) so that it can be shared here.
  ;; Until then this toy version should be good enough for some testing.
  (warn "FIXME: This is just a toy stub CTYPE-NEEDS-TO-BE-INTERPRETED-P.")
  (not (or (position (type-specifier ctype)
		     *typecheckfun-standard-typespecs*
		     :test #'equal)
	   (member-type-p ctype)
	   (numeric-type-p ctype)
	   (array-type-p ctype)
	   (cons-type-p ctype))))

;;; Evaluate (at load/execute time) to a function which checks that
;;; its argument is of the specified type.
;;;
;;; The name is slightly misleading, since some cases are memoized, so
;;; we might reuse a value which was made earlier instead of creating
;;; a new one from scratch.
(declaim (ftype (function (t) function) make-typecheckfun))
(defun make-typecheckfun (typespec)
  ;; a general-purpose default case, hopefully overridden by the
  ;; DEFINE-COMPILER-MACRO implementation
  (interpreted-typecheckfun typespec))

;;; If we know the value of the typespec at compile time, we might
;;; well be able to avoid interpreting it at runtime.
(define-compiler-macro make-typecheckfun (&whole whole typespec-form)
  (if (and (consp typespec-form)
	   (eql (first typespec-form) 'quote))
      (let* ((typespec (second typespec-form))
	     (ctype (specifier-type typespec)))
	(aver (= 2 (length typespec-form)))
	(cond ((structure-class-p ctype)
	       `(structure-object-typecheckfun ,typespec-form))
	      ((ctype-needs-to-be-interpreted-p ctype)
	       whole) ; i.e. give up compiler macro
	      (t
	       `(let ((typespec ,typespec-form))
		  (general-typecheckfun
		   typespec
		   ;; Unless we know that the function is already in the
		   ;; memoization cache
		   ,@(unless (ctype-is-standard-typecheckfun-type-p ctype)
		       ;; Note that we're arranging for the
		       ;; UNMEMOIZED-TYPECHECKFUN argument value to be
		       ;; constructed at compile time. This means the
		       ;; compiler does the work of compiling the function,
		       ;; and the loader does the work of loading the
		       ;; function, regardless of whether the runtime check
		       ;; for "is it in the memoization cache?" succeeds.
		       ;; (Then if the memoization check succeeds, the
		       ;; compiled and loaded function is eventually GCed.)
		       ;; The wasted motion in the case of a successful
		       ;; memoization check is unfortunate, but it avoids
		       ;; having to invoke the compiler at load time when
		       ;; memoization fails, which is probably more
		       ;; important.
		       `((lambda (arg)
			   (unless (typep arg typespec)
			     (typecheck-failure arg typespec))))))))))
      whole)) ; i.e. give up compiler macro
