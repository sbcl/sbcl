;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; The actual TYPEP engine. The compiler only generates calls to this
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
	  (let ((num (if (complexp object) (realpart object) object)))
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
     (dolist (type (union-type-types type))
       (when (%%typep object type)
	 (return t))))
    (unknown-type
     ;; dunno how to do this ANSIly -- WHN 19990413
     #+sb-xc-host (error "stub: %%TYPEP UNKNOWN-TYPE in xcompilation host")
     ;; Parse it again to make sure it's really undefined.
     (let ((reparse (specifier-type (unknown-type-specifier type))))
       (if (typep reparse 'unknown-type)
	   (error "unknown type specifier: ~S"
		  (unknown-type-specifier reparse))
	   (%%typep object reparse))))
    (hairy-type
     ;; Now the tricky stuff.
     (let* ((hairy-spec (hairy-type-specifier type))
	    (symbol (if (consp hairy-spec) (car hairy-spec) hairy-spec)))
       (ecase symbol
	 (and
	  (or (atom hairy-spec)
	      (dolist (spec (cdr hairy-spec) t)
		(unless (%%typep object (specifier-type spec))
		  (return nil)))))
	 (not
	  (unless (proper-list-of-length-p hairy-spec 2)
	    (error "invalid type specifier: ~S" hairy-spec))
	  (not (%%typep object (specifier-type (cadr hairy-spec)))))
	 (satisfies
	  (unless (proper-list-of-length-p hairy-spec 2)
	    (error "invalid type specifier: ~S" hairy-spec))
	  (let ((fn (cadr hairy-spec)))
	    (if (funcall (typecase fn
			   (function fn)
			   (symbol (symbol-function fn))
			   (t
			    (coerce fn 'function)))
			 object)
		t
		nil))))))
    (alien-type-type
     (sb!alien-internals:alien-typep object (alien-type-type-alien-type type)))
    (function-type
     (error "Function types are not a legal argument to TYPEP:~%  ~S"
	    (type-specifier type)))))

;;; Do type test from a class cell, allowing forward reference and
;;; redefinition.
(defun class-cell-typep (obj-layout cell object)
  (let ((class (class-cell-class cell)))
    (unless class
      (error "The class ~S has not yet been defined." (class-cell-name cell)))
    (class-typep obj-layout class object)))

;;; Test whether Obj-Layout is from an instance of Class.
(defun class-typep (obj-layout class object)
  (declare (optimize speed))
  (when (layout-invalid obj-layout)
    (if (and (typep (sb!xc:class-of object) 'sb!xc:standard-class) object)
	(setq obj-layout (pcl-check-wrapper-validity-hook object))
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

;;; to be redefined as PCL::CHECK-WRAPPER-VALIDITY when PCL is loaded
;;;
;;; FIXME: should probably be renamed SB!PCL:CHECK-WRAPPER-VALIDITY
(defun pcl-check-wrapper-validity-hook (object)
  object)
