;;;; This file contains parts of the ALIEN implementation that
;;;; are not part of the compiler.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!ALIEN")

(file-comment
  "$Header$")

;;;; alien variables

;;; Make a string out of the symbol, converting all uppercase letters to
;;; lower case and hyphens into underscores.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun guess-alien-name-from-lisp-name (lisp-name)
    (declare (type symbol lisp-name))
    (nsubstitute #\_ #\- (string-downcase (symbol-name lisp-name)))))

;;; The opposite of GUESS-ALIEN-NAME-FROM-LISP-NAME. Make a symbol out
;;; of the string, converting all lowercase letters to uppercase and
;;; underscores into hyphens.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun guess-lisp-name-from-alien-name (alien-name)
    (declare (type simple-string alien-name))
    (intern (nsubstitute #\- #\_ (string-upcase alien-name)))))

;;; Extract the Lisp and alien names from NAME. If only one is given,
;;; guess the other.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pick-lisp-and-alien-names (name)
    (etypecase name
      (string
       (values (guess-lisp-name-from-alien-name name) name))
      (symbol
       (values name (guess-alien-name-from-lisp-name name)))
      (list
       (unless (proper-list-of-length-p name 2)
	 (error "badly formed alien name"))
       (values (cadr name) (car name))))))

(defmacro def-alien-variable (name type &environment env)
  #!+sb-doc
  "Define NAME as an external alien variable of type TYPE. NAME should be
   a list of a string holding the alien name and a symbol to use as the Lisp
   name. If NAME is just a symbol or string, then the other name is guessed
   from the one supplied."
  (multiple-value-bind (lisp-name alien-name) (pick-lisp-and-alien-names name)
    (with-auxiliary-alien-types env
      (let ((alien-type (parse-alien-type type env)))
	`(eval-when (:compile-toplevel :load-toplevel :execute)
	   ,@(when *new-auxiliary-types*
	       `((%def-auxiliary-alien-types ',*new-auxiliary-types*)))
	   (%def-alien-variable ',lisp-name
				',alien-name
				',alien-type))))))

;;; Do the actual work of DEF-ALIEN-VARIABLE.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %def-alien-variable (lisp-name alien-name type)
    (setf (info :variable :kind lisp-name) :alien)
    (setf (info :variable :where-from lisp-name) :defined)
    (clear-info :variable :constant-value lisp-name)
    (setf (info :variable :alien-info lisp-name)
	  (make-heap-alien-info :type type
				:sap-form `(foreign-symbol-address
					    ',alien-name)))))

(defmacro extern-alien (name type &environment env)
  #!+sb-doc
  "Access the alien variable named NAME, assuming it is of type TYPE. This
   is SETFable."
  (let ((alien-name (etypecase name
		      (symbol (guess-alien-name-from-lisp-name name))
		      (string name))))
    `(%heap-alien ',(make-heap-alien-info
		     :type (parse-alien-type type env)
		     :sap-form `(foreign-symbol-address ',alien-name)))))

(defmacro with-alien (bindings &body body &environment env)
  #!+sb-doc
  "Establish some local alien variables. Each BINDING is of the form:
     VAR TYPE [ ALLOCATION ] [ INITIAL-VALUE | EXTERNAL-NAME ]
   ALLOCATION should be one of:
     :LOCAL (the default)
       The alien is allocated on the stack, and has dynamic extent.
     :STATIC
       The alien is allocated on the heap, and has infinite extent. The alien
       is allocated at load time, so the same piece of memory is used each time
       this form executes.
     :EXTERN
       No alien is allocated, but VAR is established as a local name for
       the external alien given by EXTERNAL-NAME."
  (with-auxiliary-alien-types env
    (dolist (binding (reverse bindings))
      (destructuring-bind
	  (symbol type &optional (opt1 nil opt1p) (opt2 nil opt2p))
	  binding
	(let ((alien-type (parse-alien-type type env)))
	  (multiple-value-bind (allocation initial-value)
	      (if opt2p
		  (values opt1 opt2)
		  (case opt1
		    (:extern
		     (values opt1 (guess-alien-name-from-lisp-name symbol)))
		    (:static
		     (values opt1 nil))
		    (t
		     (values :local opt1))))
	    (setf body
		  (ecase allocation
		    #+nil
		    (:static
		     (let ((sap
			    (make-symbol (concatenate 'string "SAP-FOR-"
						      (symbol-name symbol)))))
		       `((let ((,sap (load-time-value (%make-alien ...))))
			   (declare (type system-area-pointer ,sap))
			   (symbol-macrolet
			    ((,symbol (sap-alien ,sap ,type)))
			    ,@(when initial-value
				`((setq ,symbol ,initial-value)))
			    ,@body)))))
		    (:extern
		     (let ((info (make-heap-alien-info
				  :type alien-type
				  :sap-form `(foreign-symbol-address
					      ',initial-value))))
		       `((symbol-macrolet
			  ((,symbol (%heap-alien ',info)))
			  ,@body))))
		    (:local
		     (let ((var (gensym))
			   (initval (if initial-value (gensym)))
			   (info (make-local-alien-info :type alien-type)))
		       `((let ((,var (make-local-alien ',info))
			       ,@(when initial-value
				   `((,initval ,initial-value))))
			   (note-local-alien-type ',info ,var)
			   (multiple-value-prog1
			       (symbol-macrolet
				((,symbol (local-alien ',info ,var)))
				,@(when initial-value
				    `((setq ,symbol ,initval)))
				,@body)
			       (dispose-local-alien ',info ,var))))))))))))
    (verify-local-auxiliaries-okay)
    `(symbol-macrolet ((&auxiliary-type-definitions&
			,(append *new-auxiliary-types*
				 (auxiliary-type-definitions env))))
       ,@body)))

;;;; runtime C values that don't correspond directly to Lisp types

;;; ALIEN-VALUE
;;;
;;; Note: The DEFSTRUCT for ALIEN-VALUE lives in a separate file
;;; 'cause it has to be real early in the cold-load order.
#!-sb-fluid (declaim (freeze-type alien-value))
(def!method print-object ((value alien-value) stream)
  (print-unreadable-object (value stream)
    (format stream
	    "~S :SAP #X~8,'0X"
	    'alien-value
	    (sap-int (alien-value-sap value)))))

#!-sb-fluid (declaim (inline null-alien))
(defun null-alien (x)
  #!+sb-doc
  "Return true if X (which must be an ALIEN pointer) is null, false otherwise."
  (zerop (sap-int (alien-sap x))))

(defmacro sap-alien (sap type &environment env)
  #!+sb-doc
  "Convert the system area pointer SAP to an ALIEN of the specified TYPE (not
   evaluated.) TYPE must be pointer-like."
  (let ((alien-type (parse-alien-type type env)))
    (if (eq (compute-alien-rep-type alien-type) 'system-area-pointer)
	`(%sap-alien ,sap ',alien-type)
	(error "cannot make aliens of type ~S out of SAPs" type))))

(defun %sap-alien (sap type)
  (declare (type system-area-pointer sap)
	   (type alien-type type))
  (make-alien-value :sap sap :type type))

(defun alien-sap (alien)
  #!+sb-doc
  "Return a System-Area-Pointer pointing to Alien's data."
  (declare (type alien-value alien))
  (alien-value-sap alien))

;;;; allocation/deallocation of heap aliens

(defmacro make-alien (type &optional size &environment env)
  #!+sb-doc
  "Allocate an alien of type TYPE and return an alien pointer to it. If SIZE
   is supplied, how it is interpreted depends on TYPE. If TYPE is an array
   type, SIZE is used as the first dimension for the allocated array. If TYPE
   is not an array, then SIZE is the number of elements to allocate. The
   memory is allocated using ``malloc'', so it can be passed to foreign
   functions which use ``free''."
  (let ((alien-type (if (alien-type-p type)
			type
			(parse-alien-type type env))))
    (multiple-value-bind (size-expr element-type)
	(if (alien-array-type-p alien-type)
	    (let ((dims (alien-array-type-dimensions alien-type)))
	      (cond
	       (size
		(unless dims
		  (error
		   "cannot override the size of zero-dimensional arrays"))
		(when (constantp size)
		  (setf alien-type (copy-alien-array-type alien-type))
		  (setf (alien-array-type-dimensions alien-type)
			(cons (eval size) (cdr dims)))))
	       (dims
		(setf size (car dims)))
	       (t
		(setf size 1)))
	      (values `(* ,size ,@(cdr dims))
		      (alien-array-type-element-type alien-type)))
	    (values (or size 1) alien-type))
      (let ((bits (alien-type-bits element-type))
	    (alignment (alien-type-alignment element-type)))
	(unless bits
	  (error "The size of ~S is unknown."
		 (unparse-alien-type element-type)))
	(unless alignment
	  (error "The alignment of ~S is unknown."
		 (unparse-alien-type element-type)))
	`(%sap-alien (%make-alien (* ,(align-offset bits alignment)
				     ,size-expr))
		     ',(make-alien-pointer-type :to alien-type))))))

;;; Allocate a block of memory at least BITS bits long and return a
;;; system area pointer to it.
#!-sb-fluid (declaim (inline %make-alien))
(defun %make-alien (bits)
  (declare (type index bits) (optimize-interface (safety 2)))
  (alien-funcall (extern-alien "malloc" (function system-area-pointer unsigned))
		 (ash (the index (+ bits 7)) -3)))

#!-sb-fluid (declaim (inline free-alien))
(defun free-alien (alien)
  #!+sb-doc
  "Dispose of the storage pointed to by ALIEN. ALIEN must have been allocated
   by MAKE-ALIEN or ``malloc''."
  (alien-funcall (extern-alien "free" (function (values) system-area-pointer))
		 (alien-sap alien))
  nil)

;;;; the SLOT operator

;;; Find the field named SLOT, or die trying.
(defun slot-or-lose (type slot)
  (declare (type alien-record-type type)
	   (type symbol slot))
  (or (find slot (alien-record-type-fields type)
	    :key #'alien-record-field-name)
      (error "There is no slot named ~S in ~S" slot type)))

;;; Extract the value from the named slot from the record ALIEN. If
;;; ALIEN is actually a pointer, then DEREF it first.
(defun slot (alien slot)
  #!+sb-doc
  "Extract SLOT from the Alien STRUCT or UNION ALIEN. May be set with SETF."
  (declare (type alien-value alien)
	   (type symbol slot)
	   (optimize (inhibit-warnings 3)))
  (let ((type (alien-value-type alien)))
    (etypecase type
      (alien-pointer-type
       (slot (deref alien) slot))
      (alien-record-type
       (let ((field (slot-or-lose type slot)))
	 (extract-alien-value (alien-value-sap alien)
			      (alien-record-field-offset field)
			      (alien-record-field-type field)))))))

;;; Deposit the value in the specified slot of the record ALIEN. If
;;; the ALIEN is really a pointer, DEREF it first. The compiler uses
;;; this when it can't figure out anything better.
(defun %set-slot (alien slot value)
  (declare (type alien-value alien)
	   (type symbol slot)
	   (optimize (inhibit-warnings 3)))
  (let ((type (alien-value-type alien)))
    (etypecase type
      (alien-pointer-type
       (%set-slot (deref alien) slot value))
      (alien-record-type
       (let ((field (slot-or-lose type slot)))
	 (deposit-alien-value (alien-value-sap alien)
			      (alien-record-field-offset field)
			      (alien-record-field-type field)
			      value))))))

;;; Compute the address of the specified slot and return a pointer to it.
(defun %slot-addr (alien slot)
  (declare (type alien-value alien)
	   (type symbol slot)
	   (optimize (inhibit-warnings 3)))
  (let ((type (alien-value-type alien)))
    (etypecase type
      (alien-pointer-type
       (%slot-addr (deref alien) slot))
      (alien-record-type
       (let* ((field (slot-or-lose type slot))
	      (offset (alien-record-field-offset field))
	      (field-type (alien-record-field-type field)))
	 (%sap-alien (sap+ (alien-sap alien) (/ offset sb!vm:byte-bits))
		     (make-alien-pointer-type :to field-type)))))))

;;;; the DEREF operator

;;; Does most of the work of the different DEREF methods. Returns two values:
;;; the type and the offset (in bits) of the refered to alien.
(defun deref-guts (alien indices)
  (declare (type alien-value alien)
	   (type list indices)
	   (values alien-type integer))
  (let ((type (alien-value-type alien)))
    (etypecase type
      (alien-pointer-type
       (when (cdr indices)
	 (error "too many indices when derefing ~S: ~D"
		type
		(length indices)))
       (let ((element-type (alien-pointer-type-to type)))
	 (values element-type
		 (if indices
		     (* (align-offset (alien-type-bits element-type)
				      (alien-type-alignment element-type))
			(car indices))
		     0))))
      (alien-array-type
       (unless (= (length indices) (length (alien-array-type-dimensions type)))
	 (error "incorrect number of indices when derefing ~S: ~D"
		type (length indices)))
       (labels ((frob (dims indices offset)
		  (if (null dims)
		      offset
		      (frob (cdr dims) (cdr indices)
			(+ (if (zerop offset)
			       0
			       (* offset (car dims)))
			   (car indices))))))
	 (let ((element-type (alien-array-type-element-type type)))
	   (values element-type
		   (* (align-offset (alien-type-bits element-type)
				    (alien-type-alignment element-type))
		      (frob (alien-array-type-dimensions type)
			indices 0)))))))))

;;; Dereference the alien and return the results.
(defun deref (alien &rest indices)
  #!+sb-doc
  "De-reference an Alien pointer or array. If an array, the indices are used
   as the indices of the array element to access. If a pointer, one index can
   optionally be specified, giving the equivalent of C pointer arithmetic."
  (declare (type alien-value alien)
	   (type list indices)
	   (optimize (inhibit-warnings 3)))
  (multiple-value-bind (target-type offset) (deref-guts alien indices)
    (extract-alien-value (alien-value-sap alien)
			 offset
			 target-type)))

(defun %set-deref (alien value &rest indices)
  (declare (type alien-value alien)
	   (type list indices)
	   (optimize (inhibit-warnings 3)))
  (multiple-value-bind (target-type offset) (deref-guts alien indices)
    (deposit-alien-value (alien-value-sap alien)
			 offset
			 target-type
			 value)))

(defun %deref-addr (alien &rest indices)
  (declare (type alien-value alien)
	   (type list indices)
	   (optimize (inhibit-warnings 3)))
  (multiple-value-bind (target-type offset) (deref-guts alien indices)
    (%sap-alien (sap+ (alien-value-sap alien) (/ offset sb!vm:byte-bits))
		(make-alien-pointer-type :to target-type))))

;;;; accessing heap alien variables

(defun %heap-alien (info)
  (declare (type heap-alien-info info)
	   (optimize (inhibit-warnings 3)))
  (extract-alien-value (eval (heap-alien-info-sap-form info))
		       0
		       (heap-alien-info-type info)))

(defun %set-heap-alien (info value)
  (declare (type heap-alien-info info)
	   (optimize (inhibit-warnings 3)))
  (deposit-alien-value (eval (heap-alien-info-sap-form info))
		       0
		       (heap-alien-info-type info)
		       value))

(defun %heap-alien-addr (info)
  (declare (type heap-alien-info info)
	   (optimize (inhibit-warnings 3)))
  (%sap-alien (eval (heap-alien-info-sap-form info))
	      (make-alien-pointer-type :to (heap-alien-info-type info))))

;;;; accessing local aliens

(defun make-local-alien (info)
  (let* ((alien (eval `(make-alien ,(local-alien-info-type info))))
	 (alien-sap (alien-sap alien)))
    (finalize
     alien
     #'(lambda ()
	 (alien-funcall
	  (extern-alien "free" (function (values) system-area-pointer))
	  alien-sap)))
    alien))

(defun note-local-alien-type (info alien)
  (declare (ignore info alien))
  nil)

(defun local-alien (info alien)
  (declare (ignore info))
  (deref alien))

(defun %set-local-alien (info alien value)
  (declare (ignore info))
  (setf (deref alien) value))

(define-setf-expander local-alien (&whole whole info alien)
  (let ((value (gensym))
	(info (if (and (consp info)
		       (eq (car info) 'quote))
		  (second info)
		  (error "Something is wrong; local-alien-info not found: ~S"
			 whole))))
    (values nil
	    nil
	    (list value)
	    (if sb!c:*converting-for-interpreter*
		`(%set-local-alien ',info ,alien ,value)
		`(if (%local-alien-forced-to-memory-p ',info)
		     (%set-local-alien ',info ,alien ,value)
		     (setf ,alien
			   (deport ,value ',(local-alien-info-type info)))))
	    whole)))

(defun %local-alien-forced-to-memory-p (info)
  (local-alien-info-force-to-memory-p info))

(defun %local-alien-addr (info alien)
  (declare (type local-alien-info info))
  (unless (local-alien-info-force-to-memory-p info)
    (error "~S isn't forced to memory. Something went wrong." alien))
  alien)

(defun dispose-local-alien (info alien)
  (declare (ignore info))
  (cancel-finalization alien)
  (free-alien alien))

;;;; the CAST macro

(defmacro cast (alien type &environment env)
  #!+sb-doc
  "Convert ALIEN to an Alien of the specified TYPE (not evaluated.)  Both types
   must be Alien array, pointer or function types."
  `(%cast ,alien ',(parse-alien-type type env)))

(defun %cast (alien target-type)
  (declare (type alien-value alien)
	   (type alien-type target-type)
	   (optimize-interface (safety 2))
	   (optimize (inhibit-warnings 3)))
  (if (or (alien-pointer-type-p target-type)
	  (alien-array-type-p target-type)
	  (alien-function-type-p target-type))
      (let ((alien-type (alien-value-type alien)))
	(if (or (alien-pointer-type-p alien-type)
		(alien-array-type-p alien-type)
		(alien-function-type-p alien-type))
	    (naturalize (alien-value-sap alien) target-type)
	    (error "~S cannot be casted." alien)))
      (error "cannot cast to alien type ~S" (unparse-alien-type target-type))))

;;;; the ALIEN-SIZE macro

(defmacro alien-size (type &optional (units :bits) &environment env)
  #!+sb-doc
  "Return the size of the alien type TYPE. UNITS specifies the units to
   use and can be either :BITS, :BYTES, or :WORDS."
  (let* ((alien-type (parse-alien-type type env))
	 (bits (alien-type-bits alien-type)))
    (if bits
	(values (ceiling bits
			 (ecase units
			   (:bits 1)
			   (:bytes sb!vm:byte-bits)
			   (:words sb!vm:word-bits))))
	(error "unknown size for alien type ~S"
	       (unparse-alien-type alien-type)))))

;;;; NATURALIZE, DEPORT, EXTRACT-ALIEN-VALUE, DEPOSIT-ALIEN-VALUE

(defun naturalize (alien type)
  (declare (type alien-type type))
  (funcall (coerce (compute-naturalize-lambda type) 'function)
	   alien type))

(defun deport (value type)
  (declare (type alien-type type))
  (funcall (coerce (compute-deport-lambda type) 'function)
	   value type))

(defun extract-alien-value (sap offset type)
  (declare (type system-area-pointer sap)
	   (type unsigned-byte offset)
	   (type alien-type type))
  (funcall (coerce (compute-extract-lambda type) 'function)
	   sap offset type))

(defun deposit-alien-value (sap offset type value)
  (declare (type system-area-pointer sap)
	   (type unsigned-byte offset)
	   (type alien-type type))
  (funcall (coerce (compute-deposit-lambda type) 'function)
	   sap offset type value))

;;;; ALIEN-FUNCALL, DEF-ALIEN-ROUTINE

(defun alien-funcall (alien &rest args)
  #!+sb-doc
  "Call the foreign function ALIEN with the specified arguments. ALIEN's
   type specifies the argument and result types."
  (declare (type alien-value alien))
  (let ((type (alien-value-type alien)))
    (typecase type
      (alien-pointer-type
       (apply #'alien-funcall (deref alien) args))
      (alien-function-type
       (unless (= (length (alien-function-type-arg-types type))
		  (length args))
	 (error "wrong number of arguments for ~S~%expected ~D, got ~D"
		type
		(length (alien-function-type-arg-types type))
		(length args)))
       (let ((stub (alien-function-type-stub type)))
	 (unless stub
	   (setf stub
		 (let ((fun (gensym))
		       (parms (make-gensym-list (length args))))
		   (compile nil
			    `(lambda (,fun ,@parms)
			       (declare (type (alien ,type) ,fun))
			       (alien-funcall ,fun ,@parms)))))
	   (setf (alien-function-type-stub type) stub))
	 (apply stub alien args)))
      (t
       (error "~S is not an alien function." alien)))))

(defmacro def-alien-routine (name result-type &rest args &environment env)
  #!+sb-doc
  "Def-C-Routine Name Result-Type
		    {(Arg-Name Arg-Type [Style])}*

  Define a foreign interface function for the routine with the specified Name,
  which may be either a string, symbol or list of the form (string symbol).
  Return-Type is the Alien type for the function return value. VOID may be
  used to specify a function with no result.

  The remaining forms specifiy individual arguments that are passed to the
  routine. Arg-Name is a symbol that names the argument, primarily for
  documentation. Arg-Type is the C-Type of the argument. Style specifies the
  say that the argument is passed.

  :IN
	An :In argument is simply passed by value. The value to be passed is
	obtained from argument(s) to the interface function. No values are
	returned for :In arguments. This is the default mode.

  :OUT
	The specified argument type must be a pointer to a fixed sized object.
	A pointer to a preallocated object is passed to the routine, and the
	the object is accessed on return, with the value being returned from
	the interface function. :OUT and :IN-OUT cannot be used with pointers
	to arrays, records or functions.

  :COPY
	Similar to :IN, except that the argument values are stored in on
	the stack, and a pointer to the object is passed instead of
	the values themselves.

  :IN-OUT
	A combination of :OUT and :COPY. A pointer to the argument is passed,
	with the object being initialized from the supplied argument and
	the return value being determined by accessing the object on return."
  (multiple-value-bind (lisp-name alien-name)
      (pick-lisp-and-alien-names name)
    (collect ((docs) (lisp-args) (arg-types) (alien-vars)
	      (alien-args) (results))
      (dolist (arg args)
	(if (stringp arg)
	    (docs arg)
	    (destructuring-bind (name type &optional (style :in)) arg
	      (unless (member style '(:in :copy :out :in-out))
		(error "bogus argument style ~S in ~S" style arg))
	      (unless (eq style :out)
		(lisp-args name))
	      (when (and (member style '(:out :in-out))
			 (typep (parse-alien-type type env)
				'alien-pointer-type))
		(error "can't use :OUT or :IN-OUT on pointer-like type:~%  ~S"
		       type))
	      (cond ((eq style :in)
		     (arg-types type)
		     (alien-args name))
		    (t
		     (arg-types `(* ,type))
		     (if (eq style :out)
			 (alien-vars `(,name ,type))
			 (alien-vars `(,name ,type ,name)))
		     (alien-args `(addr ,name))))
	      (when (or (eq style :out) (eq style :in-out))
		(results name)))))
      `(defun ,lisp-name ,(lisp-args)
	 ,@(docs)
	 (with-alien
	     ((,lisp-name (function ,result-type ,@(arg-types))
			  :extern ,alien-name)
	      ,@(alien-vars))
	     ,(if (alien-values-type-p result-type)
		  (let ((temps (make-gensym-list
				(length
				 (alien-values-type-values result-type)))))
		    `(multiple-value-bind ,temps
			 (alien-funcall ,lisp-name ,@(alien-args))
		       (values ,@temps ,@(results))))
		  `(values (alien-funcall ,lisp-name ,@(alien-args))
			   ,@(results))))))))

(defun alien-typep (object type)
  #!+sb-doc
  "Return T iff OBJECT is an alien of type TYPE."
  (let ((lisp-rep-type (compute-lisp-rep-type type)))
    (if lisp-rep-type
	(typep object lisp-rep-type)
	(and (alien-value-p object)
	     (alien-subtype-p (alien-value-type object) type)))))
