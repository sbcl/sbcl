;;;; machine-independent aspects of the object representation and
;;;; primitive types

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; primitive type definitions

(/show0 "primtype.lisp 17")

(!def-primitive-type t (descriptor-reg))
(/show0 "primtype.lisp 20")
(setf *backend-t-primitive-type* (primitive-type-or-lose t))

;;; primitive integer types that fit in registers
(/show0 "primtype.lisp 24")
(!def-primitive-type positive-fixnum (any-reg signed-reg unsigned-reg)
  :type (unsigned-byte 29))
(/show0 "primtype.lisp 27")
#!-alpha
(!def-primitive-type unsigned-byte-31 (signed-reg unsigned-reg descriptor-reg)
  :type (unsigned-byte 31))
(/show0 "primtype.lisp 31")
#!-alpha
(!def-primitive-type unsigned-byte-32 (unsigned-reg descriptor-reg)
  :type (unsigned-byte 32))
(/show0 "primtype.lisp 35")
#!+alpha
(!def-primitive-type unsigned-byte-63 (signed-reg unsigned-reg descriptor-reg)
  :type (unsigned-byte 63))
#!+alpha
(!def-primitive-type unsigned-byte-64 (unsigned-reg descriptor-reg)
  :type (unsigned-byte 64))
(!def-primitive-type fixnum (any-reg signed-reg)
  :type (signed-byte 30))
#!-alpha
(!def-primitive-type signed-byte-32 (signed-reg descriptor-reg)
  :type (signed-byte 32))
#!+alpha
(!def-primitive-type signed-byte-64 (signed-reg descriptor-reg)
  :type (signed-byte 64))

(defvar *fixnum-primitive-type* (primitive-type-or-lose 'fixnum))

(/show0 "primtype.lisp 53")
(!def-primitive-type-alias tagged-num (:or positive-fixnum fixnum))
(!def-primitive-type-alias unsigned-num (:or #!-alpha unsigned-byte-32
					     #!-alpha unsigned-byte-31
					     #!+alpha unsigned-byte-64
					     #!+alpha unsigned-byte-63
					     positive-fixnum))
(!def-primitive-type-alias signed-num (:or #!-alpha signed-byte-32
					   #!+alpha signed-byte-64
					   fixnum
					   #!-alpha unsigned-byte-31
					   #!+alpha unsigned-byte-63
					   positive-fixnum))

;;; other primitive immediate types
(/show0 "primtype.lisp 68")
(!def-primitive-type base-char (base-char-reg any-reg))

;;; primitive pointer types
(/show0 "primtype.lisp 73")
(!def-primitive-type function (descriptor-reg))
(!def-primitive-type list (descriptor-reg))
(!def-primitive-type instance (descriptor-reg))

(/show0 "primtype.lisp 77")
(!def-primitive-type funcallable-instance (descriptor-reg))

;;; primitive other-pointer number types
(/show0 "primtype.lisp 81")
(!def-primitive-type bignum (descriptor-reg))
(!def-primitive-type ratio (descriptor-reg))
(!def-primitive-type complex (descriptor-reg))
(/show0 "about to !DEF-PRIMITIVE-TYPE SINGLE-FLOAT")
(!def-primitive-type single-float (single-reg descriptor-reg))
(/show0 "about to !DEF-PRIMITIVE-TYPE DOUBLE-FLOAT")
(!def-primitive-type double-float (double-reg descriptor-reg))

(/show0 "about to !DEF-PRIMITIVE-TYPE COMPLEX-SINGLE-FLOAT")
(!def-primitive-type complex-single-float (complex-single-reg descriptor-reg)
  :type (complex single-float))
(/show0 "about to !DEF-PRIMITIVE-TYPE COMPLEX-DOUBLE-FLOAT")
(!def-primitive-type complex-double-float (complex-double-reg descriptor-reg)
  :type (complex double-float))


;;; primitive other-pointer array types
(/show0 "primtype.lisp 96")
(macrolet ((define-simple-array-primitive-types ()
	       `(progn
		 ,@(map 'list
			(lambda (saetp)
			  `(!def-primitive-type
			    ,(saetp-primitive-type-name saetp)
			    (descriptor-reg)
			    :type (simple-array ,(saetp-specifier saetp) (*))))
			*specialized-array-element-type-properties*))))
  (define-simple-array-primitive-types))
;;; Note: The complex array types are not included, 'cause it is
;;; pointless to restrict VOPs to them.

;;; other primitive other-pointer types
(!def-primitive-type system-area-pointer (sap-reg descriptor-reg))
(!def-primitive-type weak-pointer (descriptor-reg))

;;; miscellaneous primitive types that don't exist at the LISP level
(!def-primitive-type catch-block (catch-block) :type nil)

;;;; PRIMITIVE-TYPE-OF and friends

;;; Return the most restrictive primitive type that contains OBJECT.
(/show0 "primtype.lisp 147")
(!def-vm-support-routine primitive-type-of (object)
  (let ((type (ctype-of object)))
    (cond ((not (member-type-p type)) (primitive-type type))
	  ((equal (member-type-members type) '(nil))
	   (primitive-type-or-lose 'list))
	  (t
	   *backend-t-primitive-type*))))

;;; Return the primitive type corresponding to a type descriptor
;;; structure. The second value is true when the primitive type is
;;; exactly equivalent to the argument Lisp type.
;;;
;;; In a bootstrapping situation, we should be careful to use the
;;; correct values for the system parameters.
;;;
;;; We need an aux function because we need to use both
;;; !DEF-VM-SUPPORT-ROUTINE and DEFUN-CACHED.
(/show0 "primtype.lisp 188")
(!def-vm-support-routine primitive-type (type)
  (primitive-type-aux type))
(/show0 "primtype.lisp 191")
(defun-cached (primitive-type-aux
	       :hash-function (lambda (x)
				(logand (type-hash-value x) #x1FF))
	       :hash-bits 9
	       :values 2
	       :default (values nil :empty))
	      ((type eq))
  (declare (type ctype type))
  (macrolet ((any () '(values *backend-t-primitive-type* nil))
	     (exactly (type)
	       `(values (primitive-type-or-lose ',type) t))
	     (part-of (type)
	       `(values (primitive-type-or-lose ',type) nil)))
    (flet ((maybe-numeric-type-union (t1 t2)
	     (let ((t1-name (primitive-type-name t1))
		   (t2-name (primitive-type-name t2)))
	       (case t1-name
		 (positive-fixnum
		  (if (or (eq t2-name 'fixnum)
			  (eq t2-name #!-alpha 'signed-byte-32
				      #!+alpha 'signed-byte-64)
			  (eq t2-name #!-alpha 'unsigned-byte-31
				      #!+alpha 'unsigned-byte-63)
			  (eq t2-name #!-alpha 'unsigned-byte-32
				      #!+alpha 'unsigned-byte-64))
		      t2))
		 (fixnum
		  (case t2-name
		    (#!-alpha signed-byte-32
		     #!+alpha signed-byte-64 t2)
		    (#!-alpha unsigned-byte-31
		     #!+alpha unsigned-byte-63
		     (primitive-type-or-lose
		      #!-alpha 'signed-byte-32
		      #!+alpha 'signed-byte-64))))
		 (#!-alpha signed-byte-32
		  #!+alpha signed-byte-64
		  (if (eq t2-name #!-alpha 'unsigned-byte-31
				  #!+alpha 'unsigned-byte-63)
		      t1))
		 (#!-alpha unsigned-byte-31
		  #!+alpha unsigned-byte-63
		  (if (eq t2-name #!-alpha 'unsigned-byte-32
				  #!+alpha 'unsigned-byte-64)
		      t2))))))
      (etypecase type
	(numeric-type
	 (let ((lo (numeric-type-low type))
	       (hi (numeric-type-high type)))
	   (case (numeric-type-complexp type)
	     (:real
	      (case (numeric-type-class type)
		(integer
		 (cond ((and hi lo)
			(dolist (spec
				  `((positive-fixnum 0 ,(1- (ash 1 29)))
				    #!-alpha
				    (unsigned-byte-31 0 ,(1- (ash 1 31)))
				    #!-alpha
				    (unsigned-byte-32 0 ,(1- (ash 1 32)))
				    #!+alpha
				    (unsigned-byte-63 0 ,(1- (ash 1 63)))
				    #!+alpha
				    (unsigned-byte-64 0 ,(1- (ash 1 64)))
				    (fixnum ,(ash -1 29)
					    ,(1- (ash 1 29)))
				    #!-alpha
				    (signed-byte-32 ,(ash -1 31)
							  ,(1- (ash 1 31)))
				    #!+alpha
				    (signed-byte-64 ,(ash -1 63)
						    ,(1- (ash 1 63))))
				 (if (or (< hi (ash -1 29))
					 (> lo (1- (ash 1 29))))
				     (part-of bignum)
				     (any)))
			  (let ((type (car spec))
				(min (cadr spec))
				(max (caddr spec)))
			    (when (<= min lo hi max)
			      (return (values
				       (primitive-type-or-lose type)
				       (and (= lo min) (= hi max))))))))
		       ((or (and hi (< hi sb!xc:most-negative-fixnum))
			    (and lo (> lo sb!xc:most-positive-fixnum)))
			(part-of bignum))
		       (t
			(any))))
		(float
		 (let ((exact (and (null lo) (null hi))))
		   (case (numeric-type-format type)
		     ((short-float single-float)
		      (values (primitive-type-or-lose 'single-float)
			      exact))
		     ((double-float)
		      (values (primitive-type-or-lose 'double-float)
			      exact))
		     (t
		      (any)))))
		(t
		 (any))))
	     (:complex
	      (if (eq (numeric-type-class type) 'float)
		  (let ((exact (and (null lo) (null hi))))
		    (case (numeric-type-format type)
		      ((short-float single-float)
		       (values (primitive-type-or-lose 'complex-single-float)
			       exact))
		      ((double-float long-float)
		       (values (primitive-type-or-lose 'complex-double-float)
			       exact))
		      (t
		       (part-of complex))))
		  (part-of complex)))
	     (t
	      (any)))))
	(array-type
	 (if (array-type-complexp type)
	     (any)
	     (let* ((dims (array-type-dimensions type))
		    (etype (array-type-specialized-element-type type))
		    (type-spec (type-specifier etype))
		    ;; FIXME: We're _WHAT_?  Testing for type equality
		    ;; with a specifier and #'EQUAL?  *BOGGLE*.  --
		    ;; CSR, 2003-06-24
		    (ptype (cdr (assoc type-spec *simple-array-primitive-types*
				       :test #'equal))))
	       (if (and (consp dims) (null (rest dims)) ptype)
		   (values (primitive-type-or-lose ptype)
			   (eq (first dims) '*))
		   (any)))))
	(union-type
	 (if (type= type (specifier-type 'list))
	     (exactly list)
	     (let ((types (union-type-types type)))
	       (multiple-value-bind (res exact) (primitive-type (first types))
		 (dolist (type (rest types) (values res exact))
		   (multiple-value-bind (ptype ptype-exact)
		       (primitive-type type)
		     (unless ptype-exact (setq exact nil))
		     (unless (eq ptype res)
		       (let ((new-ptype
			      (or (maybe-numeric-type-union res ptype)
				  (maybe-numeric-type-union ptype res))))
			 (if new-ptype
			     (setq res new-ptype)
			     (return (any)))))))))))
	(member-type
	 (let* ((members (member-type-members type))
		(res (primitive-type-of (first members))))
	   (dolist (mem (rest members) (values res nil))
	     (let ((ptype (primitive-type-of mem)))
	       (unless (eq ptype res)
		 (let ((new-ptype (or (maybe-numeric-type-union res ptype)
				      (maybe-numeric-type-union ptype res))))
		   (if new-ptype
		       (setq res new-ptype)
		       (return (any)))))))))
	(named-type
	 (ecase (named-type-name type)
	   ((t *) (values *backend-t-primitive-type* t))
	   ((nil) (any))))
	(built-in-classoid
	 (case (classoid-name type)
	   ((complex function instance
	     system-area-pointer weak-pointer)
	    (values (primitive-type-or-lose (classoid-name type)) t))
	   (funcallable-instance
	    (part-of function))
	   (base-char
	    (exactly base-char))
	   (cons-type
	    (part-of list))
	   (t
	    (any))))
	(fun-type
	 (exactly function))
	(classoid
	 (if (csubtypep type (specifier-type 'function))
	     (part-of function)
	     (part-of instance)))
	(ctype
         (if (csubtypep type (specifier-type 'function))
	     (part-of function)
             (any)))))))

(/show0 "primtype.lisp end of file")
