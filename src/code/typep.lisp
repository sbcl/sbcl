;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; (Note that when cross-compiling, SB!XC:TYPEP is interpreted as a
;;; test that the host Lisp object OBJECT translates to a target SBCL
;;; type TYPE. This behavior is needed e.g. to test for the validity
;;; of numeric subtype bounds read when cross-compiling.)
(defun typep (object type &optional environment)
  #!+sb-doc
  "Is OBJECT of type TYPE?"
  (declare (ignore environment))
  ;; Actually interpreting types at runtime is done by %TYPEP. The
  ;; cost of the extra function call here should be negligible
  ;; compared to the cost of interpreting types. (And the compiler
  ;; tries hard to optimize away the interpretation of types at
  ;; runtime, and when it succeeds, we never get here anyway.)
  (%typep object type))

;;; the actual TYPEP engine. The compiler only generates calls to this
;;; function when it can't figure out anything more intelligent to do.
(defun %typep (object specifier)
  (%%typep object
	   (if (ctype-p specifier)
	       specifier
	       (specifier-type specifier))))
(defun %%typep (object type)
  (declare (type ctype type))
  (etypecase type
    (named-type
     (ecase (named-type-name type)
       ((* t) t)
       ((nil) nil)))
    (numeric-type
     (and (numberp object)
	  (let (;; I think this works because of an invariant of the
		;; two components of a COMPLEX are always coerced to
		;; be the same, e.g. (COMPLEX 1.0 3/2) => #C(1.0 1.5).
		;; Dunno why that holds, though -- ANSI? Python
		;; tradition? marsh faerie spirits? -- WHN 2001-10-27
		(num (if (complexp object)
			 (realpart object)
			 object)))
	    (ecase (numeric-type-class type)
	      (integer (integerp num))
	      (rational (rationalp num))
	      (float
	       (ecase (numeric-type-format type)
		 (short-float (typep num 'short-float))
		 (single-float (typep num 'single-float))
		 (double-float (typep num 'double-float))
		 (long-float (typep num 'long-float))
		 ((nil) (floatp num))))
	      ((nil) t)))
	  #!-negative-zero-is-not-zero
	  (flet ((bound-test (val)
		   (let ((low (numeric-type-low type))
			 (high (numeric-type-high type)))
		     (and (cond ((null low) t)
				((listp low) (> val (car low)))
				(t (>= val low)))
			  (cond ((null high) t)
				((listp high) (< val (car high)))
				(t (<= val high)))))))
	    (ecase (numeric-type-complexp type)
	      ((nil) t)
	      (:complex
	       (and (complexp object)
		    (bound-test (realpart object))
		    (bound-test (imagpart object))))
	      (:real
	       (and (not (complexp object))
		    (bound-test object)))))
	  #!+negative-zero-is-not-zero
	  (labels ((signed-> (x y)
		     (if (and (zerop x) (zerop y) (floatp x) (floatp y))
			 (> (float-sign x) (float-sign y))
			 (> x y)))
		   (signed->= (x y)
		     (if (and (zerop x) (zerop y) (floatp x) (floatp y))
			 (>= (float-sign x) (float-sign y))
			 (>= x y)))
		   (bound-test (val)
		     (let ((low (numeric-type-low type))
			   (high (numeric-type-high type)))
		       (and (cond ((null low) t)
				  ((listp low)
				   (signed-> val (car low)))
				  (t
				   (signed->= val low)))
			    (cond ((null high) t)
				  ((listp high)
				   (signed-> (car high) val))
				  (t
				   (signed->= high val)))))))
	    (ecase (numeric-type-complexp type)
	      ((nil) t)
	      (:complex
	       (and (complexp object)
		    (bound-test (realpart object))
		    (bound-test (imagpart object))))
	      (:real
	       (and (not (complexp object))
		    (bound-test object)))))))
    (array-type
     (and (arrayp object)
	  (ecase (array-type-complexp type)
	    ((t) (not (typep object 'simple-array)))
	    ((nil) (typep object 'simple-array))
	    ((:maybe) t))
	  (or (eq (array-type-dimensions type) '*)
	      (do ((want (array-type-dimensions type) (cdr want))
		   (got (array-dimensions object) (cdr got)))
		  ((and (null want) (null got)) t)
		(unless (and want got
			     (or (eq (car want) '*)
				 (= (car want) (car got))))
		  (return nil))))
	  (if (unknown-type-p (array-type-element-type type))
	      ;; better to fail this way than to get bogosities like
	      ;;   (TYPEP (MAKE-ARRAY 11) '(ARRAY SOME-UNDEFINED-TYPE)) => T
	      (error "~@<unknown element type in array type: ~2I~_~S~:>"
		     (type-specifier type))
	      t)
	  (or (eq (array-type-element-type type) *wild-type*)
	      (values (type= (array-type-specialized-element-type type)
			     (specifier-type (array-element-type
					      object)))))))
    (member-type
     (if (member object (member-type-members type)) t))
    (sb!xc:class
     #+sb-xc-host (ctypep object type)
     #-sb-xc-host (class-typep (layout-of object) type object))
    (union-type
     (some (lambda (union-type-type) (%%typep object union-type-type))
	   (union-type-types type)))
    (intersection-type
     (every (lambda (intersection-type-type)
	      (%%typep object intersection-type-type))
	    (intersection-type-types type)))
    (cons-type
     (and (consp object)
	  (%%typep (car object) (cons-type-car-type type))
	  (%%typep (cdr object) (cons-type-cdr-type type))))
    (unknown-type
     ;; dunno how to do this ANSIly -- WHN 19990413
     #+sb-xc-host (error "stub: %%TYPEP UNKNOWN-TYPE in xcompilation host")
     ;; Parse it again to make sure it's really undefined.
     (let ((reparse (specifier-type (unknown-type-specifier type))))
       (if (typep reparse 'unknown-type)
	   (error "unknown type specifier: ~S"
		  (unknown-type-specifier reparse))
	   (%%typep object reparse))))
    (negation-type
     (not (%%typep object (negation-type-type type))))
    (hairy-type
     ;; Now the tricky stuff.
     (let* ((hairy-spec (hairy-type-specifier type))
	    (symbol (car hairy-spec)))
       (ecase symbol
	 (and
	  (every (lambda (spec) (%%typep object (specifier-type spec)))
		 (rest hairy-spec)))
	 ;; Note: it should be safe to skip OR here, because union
	 ;; types can always be represented as UNION-TYPE in general
	 ;; or other CTYPEs in special cases; we never need to use
	 ;; HAIRY-TYPE for them.
	 (not
	  (unless (proper-list-of-length-p hairy-spec 2)
	    (error "invalid type specifier: ~S" hairy-spec))
	  (not (%%typep object (specifier-type (cadr hairy-spec)))))
	 (satisfies
	  (unless (proper-list-of-length-p hairy-spec 2)
	    (error "invalid type specifier: ~S" hairy-spec))
	  (values (funcall (symbol-function (cadr hairy-spec)) object))))))
    (alien-type-type
     (sb!alien-internals:alien-typep object (alien-type-type-alien-type type)))
    (fun-type
     (error "Function types are not a legal argument to TYPEP:~%  ~S"
	    (type-specifier type)))))

;;; Do a type test from a class cell, allowing forward reference and
;;; redefinition.
(defun class-cell-typep (obj-layout cell object)
  (let ((class (class-cell-class cell)))
    (unless class
      (error "The class ~S has not yet been defined." (class-cell-name cell)))
    (class-typep obj-layout class object)))

;;; Test whether OBJ-LAYOUT is from an instance of CLASS.
(defun class-typep (obj-layout class object)
  (declare (optimize speed))
  (when (layout-invalid obj-layout)
    (if (and (typep (sb!xc:class-of object) 'sb!xc:standard-class) object)
	(setq obj-layout (sb!pcl::check-wrapper-validity object))
	(error "TYPEP was called on an obsolete object (was class ~S)."
	       (class-proper-name (layout-class obj-layout)))))
  (let ((layout (class-layout class))
	(obj-inherits (layout-inherits obj-layout)))
    (when (layout-invalid layout)
      (error "The class ~S is currently invalid." class))
    (or (eq obj-layout layout)
	(dotimes (i (length obj-inherits) nil)
	  (when (eq (svref obj-inherits i) layout)
	    (return t))))))
