;;;; COERCE and related code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(macrolet ((def-frob (name result access src-type &optional typep)
	     `(defun ,name (object ,@(if typep '(type) ()))
		(do* ((index 0 (1+ index))
		      (length (length (the ,(ecase src-type
					      (:list 'list)
					      (:vector 'vector))
					   object)))
		      (result ,result)
		      (in-object object))
		     ((= index length) result)
		  (declare (fixnum length index))
		  (setf (,access result index)
			,(ecase src-type
			   (:list '(pop in-object))
			   (:vector '(aref in-object index))))))))

  (def-frob list-to-simple-string* (make-string length) schar :list)

  (def-frob list-to-bit-vector* (make-array length :element-type '(mod 2))
    sbit :list)

  (def-frob list-to-vector* (make-sequence-of-type type length)
    aref :list t)

  (def-frob vector-to-vector* (make-sequence-of-type type length)
    aref :vector t)

  (def-frob vector-to-simple-string* (make-string length) schar :vector)

  (def-frob vector-to-bit-vector* (make-array length :element-type '(mod 2))
    sbit :vector))

(defun vector-to-list* (object)
  (let ((result (list nil))
	(length (length object)))
    (declare (fixnum length))
    (do ((index 0 (1+ index))
	 (splice result (cdr splice)))
	((= index length) (cdr result))
      (declare (fixnum index))
      (rplacd splice (list (aref object index))))))

(defun string-to-simple-string* (object)
  (if (simple-string-p object)
      object
      (with-array-data ((data object)
			(start)
			(end (length object)))
	(declare (simple-string data))
	(subseq data start end))))

(defun bit-vector-to-simple-bit-vector* (object)
  (if (simple-bit-vector-p object)
      object
      (with-array-data ((data object)
			(start)
			(end (length object)))
	(declare (simple-bit-vector data))
	(subseq data start end))))

(defvar *offending-datum*); FIXME: Remove after debugging COERCE.

;;; These are used both by the full DEFUN function and by various
;;; optimization transforms in the constant-OUTPUT-TYPE-SPEC case.
;;;
;;; Most of them are INLINE so that they can be optimized when the
;;; argument type is known. It might be better to do this with
;;; DEFTRANSFORMs, though.
(declaim (inline coerce-to-list))
(declaim (inline coerce-to-simple-string coerce-to-bit-vector coerce-to-vector))
(defun coerce-to-function (object)
  ;; (Unlike the other COERCE-TO-FOOs, this one isn't inline, because
  ;; it's so big and because optimizing away the outer ETYPECASE
  ;; doesn't seem to buy us that much anyway.)
  (etypecase object
    (symbol
     ;; ANSI lets us return ordinary errors (non-TYPE-ERRORs) here.
     (cond ((macro-function object)
	    (error "~S names a macro." object))
	   ((special-operator-p object)
	    (error "~S is a special operator." object))
	   (t (fdefinition object))))
    (list
     (case (first object)
       ((setf)
	(fdefinition object))
       ((lambda instance-lambda)
	;; FIXME: If we go to a compiler-only implementation, this can
	;; become COMPILE instead of EVAL, which seems nicer to me.
	(eval `(function ,object)))
       (t
	(error 'simple-type-error
	       :datum object
	       :expected-type '(or symbol
				   ;; KLUDGE: ANSI wants us to
				   ;; return a TYPE-ERROR here, and
				   ;; a TYPE-ERROR is supposed to
				   ;; describe the expected type,
				   ;; but it's not obvious how to
				   ;; describe the coerceable cons
				   ;; types, so we punt and just say
				   ;; CONS. -- WHN 20000503
				   cons)
	       :format-control "~S can't be coerced to a function."
	       :format-arguments (list object)))))))
(defun coerce-to-list (object)
  (etypecase object
    (vector (vector-to-list* object))))
(defun coerce-to-simple-string (object)
  (etypecase object
    (list (list-to-simple-string* object))
    (string (string-to-simple-string* object))
    (vector (vector-to-simple-string* object))))
(defun coerce-to-bit-vector (object)
  (etypecase object
    (list (list-to-bit-vector* object))
    (vector (vector-to-bit-vector* object))))
(defun coerce-to-vector (object output-type-spec)
  (etypecase object
    (list (list-to-vector* object output-type-spec))
    (vector (vector-to-vector* object output-type-spec))))

;;; old working version
(defun coerce (object output-type-spec)
  #!+sb-doc
  "Coerces the Object to an object of type Output-Type-Spec."
  (flet ((coerce-error ()
	   (/show0 "entering COERCE-ERROR")
	   (error 'simple-type-error
		  :format-control "~S can't be converted to type ~S."
		  :format-arguments (list object output-type-spec)))
	 (check-result (result)
	   #!+high-security
	   (check-type-var result output-type-spec)
	   result))
    (let ((type (specifier-type output-type-spec)))
      (cond
	((%typep object output-type-spec)
	 object)
	((eq type *empty-type*)
	 (coerce-error))
	((csubtypep type (specifier-type 'character))
	 (character object))
	((csubtypep type (specifier-type 'function))
	 #!+high-security
	 (when (and (or (symbolp object)
			(and (listp object)
			     (= (length object) 2)
			     (eq (car object) 'setf)))
		    (not (fboundp object)))
	   (error 'simple-type-error
		  :datum object
		  :expected-type '(satisfies fboundp)
	       :format-control "~S isn't fbound."
	       :format-arguments (list object)))
	 #!+high-security
	 (when (and (symbolp object)
		    (sb!xc:macro-function object))
	   (error 'simple-type-error
		  :datum object
		  :expected-type '(not (satisfies sb!xc:macro-function))
		  :format-control "~S is a macro."
		  :format-arguments (list object)))
	 #!+high-security
	 (when (and (symbolp object)
		    (special-operator-p object))
	   (error 'simple-type-error
		  :datum object
		  :expected-type '(not (satisfies special-operator-p))
		  :format-control "~S is a special operator."
		  :format-arguments (list object)))
	 (eval `#',object))
	((numberp object)
	 (let ((res
		(cond
		  ((csubtypep type (specifier-type 'single-float))
		   (%single-float object))
		  ((csubtypep type (specifier-type 'double-float))
		   (%double-float object))
		  #!+long-float
		  ((csubtypep type (specifier-type 'long-float))
		   (%long-float object))
		  ((csubtypep type (specifier-type 'float))
		   (%single-float object))
		  ((csubtypep type (specifier-type '(complex single-float)))
		   (complex (%single-float (realpart object))
			    (%single-float (imagpart object))))
		  ((csubtypep type (specifier-type '(complex double-float)))
		   (complex (%double-float (realpart object))
			    (%double-float (imagpart object))))
		  #!+long-float
		  ((csubtypep type (specifier-type '(complex long-float)))
		   (complex (%long-float (realpart object))
			    (%long-float (imagpart object))))
		  ((csubtypep type (specifier-type 'complex))
		   (complex object))
		  (t
		   (coerce-error)))))
	   ;; If RES has the wrong type, that means that rule of canonical
	   ;; representation for complex rationals was invoked. According to
	   ;; the Hyperspec, (coerce 7/2 'complex) returns 7/2. Thus, if the
	   ;; object was a rational, there is no error here.
	   (unless (or (typep res output-type-spec) (rationalp object))
	     (coerce-error))
	   res))
	((csubtypep type (specifier-type 'list))
	 (if (vectorp object)
	     (vector-to-list* object)
	     (coerce-error)))
	((csubtypep type (specifier-type 'string))
	 (check-result
	  (typecase object
	    (list (list-to-simple-string* object))
	    (string (string-to-simple-string* object))
	    (vector (vector-to-simple-string* object))
	    (t
	     (coerce-error)))))
	((csubtypep type (specifier-type 'bit-vector))
	 (check-result
	  (typecase object
	    (list (list-to-bit-vector* object))
	    (vector (vector-to-bit-vector* object))
	    (t
	     (coerce-error)))))
	((csubtypep type (specifier-type 'vector))
	 (check-result
	  (typecase object
	    (list (list-to-vector* object output-type-spec))
	    (vector (vector-to-vector* object output-type-spec))
	    (t
	     (coerce-error)))))
	(t
	 (coerce-error))))))

;;; new version, which seems as though it should be better, but which
;;; does not yet work
#+nil
(defun coerce (object output-type-spec)
  #!+sb-doc
  "Coerces the Object to an object of type Output-Type-Spec."
  (flet ((coerce-error ()
           (error 'simple-type-error
		  :format-control "~S can't be converted to type ~S."
		  :format-arguments (list object output-type-spec)))
	 (check-result (result)
	   #!+high-security
	   (check-type-var result output-type-spec)
	   result))
    (let ((type (specifier-type output-type-spec)))
      (cond
	((%typep object output-type-spec)
	 object)
	((eq type *empty-type*)
	 (coerce-error))
	((csubtypep type (specifier-type 'character))
	 (character object))
	((csubtypep type (specifier-type 'function))
	 (coerce-to-function object))
	((numberp object)
	 (let ((res
		(cond
		  ((csubtypep type (specifier-type 'single-float))
		   (%single-float object))
		  ((csubtypep type (specifier-type 'double-float))
		   (%double-float object))
		  #!+long-float
		  ((csubtypep type (specifier-type 'long-float))
		   (%long-float object))
		  ((csubtypep type (specifier-type 'float))
		   (%single-float object))
		  ((csubtypep type (specifier-type '(complex single-float)))
		   (complex (%single-float (realpart object))
			    (%single-float (imagpart object))))
		  ((csubtypep type (specifier-type '(complex double-float)))
		   (complex (%double-float (realpart object))
			    (%double-float (imagpart object))))
		  #!+long-float
		  ((csubtypep type (specifier-type '(complex long-float)))
		   (complex (%long-float (realpart object))
			    (%long-float (imagpart object))))
		  ((csubtypep type (specifier-type 'complex))
		   (complex object))
		  (t
		   (coerce-error)))))
	   ;; If RES has the wrong type, that means that rule of
	   ;; canonical representation for complex rationals was
	   ;; invoked. According to the ANSI spec, (COERCE 7/2
	   ;; 'COMPLEX) returns 7/2. Thus, if the object was a
	   ;; rational, there is no error here.
	   (unless (or (typep res output-type-spec) (rationalp object))
	     (coerce-error))
	   res))
	((csubtypep type (specifier-type 'list))
	 (coerce-to-list object))
	((csubtypep type (specifier-type 'string))
	 (check-result (coerce-to-simple-string object)))
	((csubtypep type (specifier-type 'bit-vector))
	 (check-result (coerce-to-bit-vector object)))
	((csubtypep type (specifier-type 'vector))
	 (check-result (coerce-to-vector object output-type-spec)))
	(t
	 (coerce-error))))))
