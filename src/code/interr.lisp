;;;; functions and macros to define and deal with internal errors
;;;; (i.e. problems that can be signaled from assembler code)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;;; internal errors

(defvar *internal-errors*
  #.(map 'vector #'cdr sb!c:*backend-internal-errors*))

(eval-when (:compile-toplevel :execute)

(sb!xc:defmacro deferr (name args &rest body)
  (let* ((rest-pos (position '&rest args))
	 (required (if rest-pos (subseq args 0 rest-pos) args))
	 (fp (gensym))
	 (context (gensym))
	 (sc-offsets (gensym))
	 (fn-name (symbolicate name "-HANDLER")))
    `(progn
       ;; FIXME: Having a separate full DEFUN for each error doesn't
       ;; seem to add much value, and it takes a lot of space. Perhaps
       ;; we could do this dispatch with a big CASE statement instead?
       (defun ,fn-name (name ,fp ,context ,sc-offsets)
	 ;; FIXME: Perhaps put in OPTIMIZE declaration to make this
	 ;; byte coded.
	 ;;
	 ;; FIXME: It would probably be good to do *STACK-TOP-HINT*
	 ;; tricks to hide this internal error-handling logic from the
	 ;; poor high level user, so his debugger tells him about
	 ;; where his error was detected instead of telling him where
	 ;; he ended up inside the system error-handling logic.
	 (declare (ignorable name ,fp ,context ,sc-offsets))
	 (/show0 "about to do outer LETs in DEFERR macroexpanded DEFUN")
	 (let (,@(let ((offset -1))
		   (mapcar #'(lambda (var)
			       `(,var (sb!di::sub-access-debug-var-slot
				       ,fp
				       (nth ,(incf offset)
					    ,sc-offsets)
				       ,context)))
			   required))
	       ,@(when rest-pos
		   `((,(nth (1+ rest-pos) args)
		      (mapcar #'(lambda (sc-offset)
				  (sb!di::sub-access-debug-var-slot
				   ,fp
				   sc-offset
				   ,context))
			      (nthcdr ,rest-pos ,sc-offsets))))))
	   ,@body))
       (setf (svref *internal-errors* ,(error-number-or-lose name))
	     #',fn-name))))

) ; EVAL-WHEN

(deferr unknown-error (&rest args)
  (error "unknown error:~{ ~S~})" args))

(deferr object-not-function-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'function))

(deferr object-not-list-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'list))

(deferr object-not-bignum-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'bignum))

(deferr object-not-ratio-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'ratio))

(deferr object-not-single-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'single-float))

(deferr object-not-double-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'double-float))

#!+long-float
(deferr object-not-long-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'long-float))

(deferr object-not-simple-string-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'simple-string))

(deferr object-not-simple-bit-vector-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'simple-bit-vector))

(deferr object-not-simple-vector-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'simple-vector))

(deferr object-not-fixnum-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'fixnum))

(deferr object-not-function-or-symbol-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(or function symbol)))

(deferr object-not-vector-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'vector))

(deferr object-not-string-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'string))

(deferr object-not-bit-vector-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'bit-vector))

(deferr object-not-array-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'array))

(deferr object-not-number-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'number))

(deferr object-not-rational-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'rational))

(deferr object-not-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'float))

(deferr object-not-real-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'real))

(deferr object-not-integer-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'integer))

(deferr object-not-cons-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'cons))

(deferr object-not-symbol-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'symbol))

(deferr undefined-symbol-error (fdefn-or-symbol)
  (error 'undefined-function
	 :function-name name
	 :name (etypecase fdefn-or-symbol
		 (symbol fdefn-or-symbol)
		 (fdefn (fdefn-name fdefn-or-symbol)))))

(deferr object-not-coerceable-to-function-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'coerceable-to-function))

(deferr invalid-argument-count-error (nargs)
  (error 'simple-program-error
	 :function-name name
	 :format-control "invalid number of arguments: ~S"
	 :format-arguments (list nargs)))

(deferr bogus-argument-to-values-list-error (list)
  (error 'simple-type-error
	 :function-name name
	 :datum list
	 :expected-type 'list
	 :format-control
	 "~@<attempt to use VALUES-LIST on a dotted list: ~2I~_~S~:>"
	 :format-arguments (list list)))

(deferr unbound-symbol-error (symbol)
  (error 'unbound-variable :function-name name :name symbol))

(deferr object-not-base-char-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'base-char))

(deferr object-not-sap-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'system-area-pointer))

(deferr invalid-unwind-error ()
  (error 'simple-control-error
	 :function-name name
	 :format-control
	 "attempt to RETURN-FROM a block or GO to a tag that no longer exists"))

(deferr unseen-throw-tag-error (tag)
  (error 'simple-control-error
	 :function-name name
	 :format-control "attempt to THROW to a tag that does not exist: ~S"
	 :format-arguments (list tag)))

(deferr nil-function-returned-error (function)
  (error 'simple-control-error
	 :function-name name
	 :format-control
	 "A function with declared result type NIL returned:~%  ~S"
	 :format-arguments (list function)))

(deferr division-by-zero-error (this that)
  (error 'division-by-zero
	 :function-name name
	 :operation 'division
	 :operands (list this that)))

(deferr object-not-type-error (object type)
  (error (if (and (typep object 'instance)
		  (layout-invalid (%instance-layout object)))
	     'layout-invalid
	     'type-error)
	 :function-name name
	 :datum object
	 :expected-type type))

(deferr layout-invalid-error (object layout)
  (error 'layout-invalid
	 :function-name name
	 :datum object
	 :expected-type (layout-class layout)))

(deferr odd-keyword-arguments-error ()
  (error 'simple-program-error
	 :function-name name
	 :format-control "odd number of keyword arguments"))

(deferr unknown-keyword-argument-error (key)
  (error 'simple-program-error
	 :function-name name
	 :format-control "unknown keyword: ~S"
	 :format-arguments (list key)))

(deferr invalid-array-index-error (array bound index)
  (error 'simple-error
	 :function-name name
	 :format-control
	 "invalid array index ~D for ~S (should be nonnegative and <~D)"
	 :format-arguments (list index array bound)))

(deferr object-not-simple-array-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'simple-array))

(deferr object-not-signed-byte-32-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(signed-byte 32)))

(deferr object-not-unsigned-byte-32-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(unsigned-byte 32)))

(deferr object-not-simple-array-unsigned-byte-2-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (unsigned-byte 2) (*))))

(deferr object-not-simple-array-unsigned-byte-4-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (unsigned-byte 4) (*))))

(deferr object-not-simple-array-unsigned-byte-8-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (unsigned-byte 8) (*))))

(deferr object-not-simple-array-unsigned-byte-16-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (unsigned-byte 16) (*))))

(deferr object-not-simple-array-unsigned-byte-32-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (unsigned-byte 32) (*))))

(deferr object-not-simple-array-signed-byte-8-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (signed-byte 8) (*))))

(deferr object-not-simple-array-signed-byte-16-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (signed-byte 16) (*))))

(deferr object-not-simple-array-signed-byte-30-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (signed-byte 30) (*))))

(deferr object-not-simple-array-signed-byte-32-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (signed-byte 32) (*))))

(deferr object-not-simple-array-single-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array single-float (*))))

(deferr object-not-simple-array-double-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array double-float (*))))

(deferr object-not-simple-array-complex-single-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (complex single-float) (*))))

(deferr object-not-simple-array-complex-double-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (complex double-float) (*))))

#!+long-float
(deferr object-not-simple-array-complex-long-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (complex long-float) (*))))

(deferr object-not-complex-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'complex))

(deferr object-not-complex-rational-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(complex rational)))

(deferr object-not-complex-single-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(complex single-float)))

(deferr object-not-complex-double-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(complex double-float)))

#!+long-float
(deferr object-not-complex-long-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(complex long-float)))

(deferr object-not-weak-pointer-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'weak-pointer))

(deferr object-not-instance-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'instance))

(deferr object-not-complex-vector-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(and vector (not simple-array))))

;;;; fetching errorful function name

;;; This variable is used to prevent infinite recursive lossage when
;;; we can't find the caller for some reason.
(defvar *finding-name* nil)

(defun find-caller-name ()
  (if *finding-name*
      (values "<error finding caller name -- already finding name>" nil)
      (handler-case
	  (let* ((*finding-name* t)
		 (frame (sb!di:frame-down (sb!di:frame-down (sb!di:top-frame))))
		 (name (sb!di:debug-function-name
			(sb!di:frame-debug-function frame))))
	    (sb!di:flush-frames-above frame)
	    (values name frame))
	(error ()
	  (values "<error finding caller name -- trapped error>" nil))
	(sb!di:debug-condition ()
	  (values "<error finding caller name -- trapped debug-condition>"
		  nil)))))

(defun find-interrupted-name ()
  (if *finding-name*
      (values "<error finding interrupted name -- already finding name>" nil)
      (handler-case
	  (let ((*finding-name* t))
	    (do ((frame (sb!di:top-frame) (sb!di:frame-down frame)))
		((null frame)
		 (values "<error finding interrupted name -- null frame>" nil))
	      (when (and (sb!di::compiled-frame-p frame)
			 (sb!di::compiled-frame-escaped frame))
		(sb!di:flush-frames-above frame)
		(return (values (sb!di:debug-function-name
				 (sb!di:frame-debug-function frame))
				frame)))))
	(error ()
	  (values "<error finding interrupted name -- trapped error>" nil))
	(sb!di:debug-condition ()
	  (values "<error finding interrupted name -- trapped debug-condition>"
		  nil)))))

;;;; INTERNAL-ERROR signal handler

(defun internal-error (context continuable)
  (declare (type system-area-pointer context) (ignore continuable))
  (/show0 "entering INTERNAL-ERROR, CONTEXT=..")
  #!+sb-show (sb!sys:%primitive print (sb!impl::hexstr context))
  (infinite-error-protect
   (let ((context (locally
		    (declare (optimize (inhibit-warnings 3)))
		    (sb!alien:sap-alien context (* os-context-t)))))
     (multiple-value-bind (error-number arguments)
	 (sb!vm:internal-error-arguments context)
       (multiple-value-bind (name sb!debug:*stack-top-hint*)
	   (find-interrupted-name)
	 (let ((fp (int-sap (sb!vm:context-register context
						    sb!vm::cfp-offset)))
	       (handler (and (< -1 error-number (length *internal-errors*))
			     (svref *internal-errors* error-number))))
	   (cond ((null handler)
		  (error 'simple-error
			 :function-name name
			 :format-control
			 "unknown internal error, ~D? args=~S"
			 :format-arguments
			 (list error-number
			       (mapcar #'(lambda (sc-offset)
					   (sb!di::sub-access-debug-var-slot
					    fp sc-offset context))
				       arguments))))
		 ((not (functionp handler))
		  (error 'simple-error
			 :function-name name
			 :format-control "internal error ~D: ~A; args=~S"
			 :format-arguments
			 (list error-number
			       handler
			       (mapcar #'(lambda (sc-offset)
					   (sb!di::sub-access-debug-var-slot
					    fp sc-offset context))
				       arguments))))
		 (t
		  (funcall handler name fp context arguments)))))))))
