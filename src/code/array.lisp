;;;; functions to implement arrays

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

#!-sb-fluid
(declaim (inline fill-pointer array-has-fill-pointer-p adjustable-array-p
		 array-displacement))

;;;; miscellaneous accessor functions

;;; These functions are only needed by the interpreter, 'cause the
;;; compiler inlines them.
(macrolet ((def (name)
	     `(progn
		(defun ,name (array)
		  (,name array))
		(defun (setf ,name) (value array)
		  (setf (,name array) value)))))
  (def %array-fill-pointer)
  (def %array-fill-pointer-p)
  (def %array-available-elements)
  (def %array-data-vector)
  (def %array-displacement)
  (def %array-displaced-p))

(defun %array-rank (array)
  (%array-rank array))

(defun %array-dimension (array axis)
  (%array-dimension array axis))

(defun %set-array-dimension (array axis value)
  (%set-array-dimension array axis value))

(defun %check-bound (array bound index)
  (declare (type index bound)
	   (fixnum index))
  (%check-bound array bound index))

(defun %with-array-data (array start end)
  (%with-array-data-macro array start end :fail-inline? t))

;;; It'd waste space to expand copies of error handling in every
;;; inline %WITH-ARRAY-DATA, so we have them call this function
;;; instead. This is just a wrapper which is known never to return.
(defun failed-%with-array-data (array start end)
  (declare (notinline %with-array-data))
  (%with-array-data array start end)
  (bug "called FAILED-%WITH-ARRAY-DATA with valid array parameters?"))

;;;; MAKE-ARRAY

(eval-when (:compile-toplevel :execute)
  (sb!xc:defmacro pick-vector-type (type &rest specs)
    `(cond ,@(mapcar (lambda (spec)
		       `(,(if (eq (car spec) t)
			      t
			      `(subtypep ,type ',(car spec)))
			 ,@(cdr spec)))
		     specs))))

;;; These functions are used in the implementation of MAKE-ARRAY for
;;; complex arrays. There are lots of transforms to simplify
;;; MAKE-ARRAY for various easy cases, but not for all reasonable
;;; cases, so e.g. as of sbcl-0.6.6 we still make full calls to
;;; MAKE-ARRAY for any non-simple array. Thus, there's some value to
;;; making this somewhat efficient, at least not doing full calls to
;;; SUBTYPEP in the easy cases.
(defun %vector-widetag-and-n-bits (type)
  (case type
    ;; Pick off some easy common cases.
    ;;
    ;; (Perhaps we should make a much more exhaustive table of easy
    ;; common cases here. Or perhaps the effort would be better spent
    ;; on smarter compiler transforms which do the calculation once
    ;; and for all in any reasonable user programs.)
    ((t)
     (values #.sb!vm:simple-vector-widetag #.sb!vm:n-word-bits))
    ((character base-char standard-char)
     (values #.sb!vm:simple-string-widetag #.sb!vm:n-byte-bits))
    ((bit)
     (values #.sb!vm:simple-bit-vector-widetag 1))
    ;; OK, we have to wade into SUBTYPEPing after all.
    (t
     ;; FIXME: The data here are redundant with
     ;; *SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES*.
     (pick-vector-type type
       (base-char (values #.sb!vm:simple-string-widetag #.sb!vm:n-byte-bits))
       (bit (values #.sb!vm:simple-bit-vector-widetag 1))
       ((unsigned-byte 2)
	(values #.sb!vm:simple-array-unsigned-byte-2-widetag 2))
       ((unsigned-byte 4)
	(values #.sb!vm:simple-array-unsigned-byte-4-widetag 4))
       ((unsigned-byte 8)
	(values #.sb!vm:simple-array-unsigned-byte-8-widetag 8))
       ((unsigned-byte 16)
	(values #.sb!vm:simple-array-unsigned-byte-16-widetag 16))
       ((unsigned-byte 32)
	(values #.sb!vm:simple-array-unsigned-byte-32-widetag 32))
       ((signed-byte 8)
	(values #.sb!vm:simple-array-signed-byte-8-widetag 8))
       ((signed-byte 16)
	(values #.sb!vm:simple-array-signed-byte-16-widetag 16))
       ((signed-byte 30)
	(values #.sb!vm:simple-array-signed-byte-30-widetag 32))
       ((signed-byte 32)
	(values #.sb!vm:simple-array-signed-byte-32-widetag 32))
       (single-float (values #.sb!vm:simple-array-single-float-widetag 32))
       (double-float (values #.sb!vm:simple-array-double-float-widetag 64))
       #!+long-float
       (long-float
	(values #.sb!vm:simple-array-long-float-widetag
		#!+x86 96 #!+sparc 128))
       ((complex single-float)
	(values #.sb!vm:simple-array-complex-single-float-widetag 64))
       ((complex double-float)
	(values #.sb!vm:simple-array-complex-double-float-widetag 128))
       #!+long-float
       ((complex long-float)
	(values #.sb!vm:simple-array-complex-long-float-widetag
		#!+x86 192
		#!+sparc 256))
       (t (values #.sb!vm:simple-vector-widetag #.sb!vm:n-word-bits))))))
(defun %complex-vector-widetag (type)
  (case type
    ;; Pick off some easy common cases.
    ((t)
     #.sb!vm:complex-vector-widetag)
    ((character base-char)
     #.sb!vm:complex-string-widetag) 
    ((bit)
     #.sb!vm:complex-bit-vector-widetag)
    ;; OK, we have to wade into SUBTYPEPing after all.
    (t
     (pick-vector-type type
       (base-char #.sb!vm:complex-string-widetag)
       (bit #.sb!vm:complex-bit-vector-widetag)
       (t #.sb!vm:complex-vector-widetag)))))

(defun make-array (dimensions &key
			      (element-type t)
			      (initial-element nil initial-element-p)
			      initial-contents adjustable fill-pointer
			      displaced-to displaced-index-offset)
  (let* ((dimensions (if (listp dimensions) dimensions (list dimensions)))
	 (array-rank (length (the list dimensions)))
	 (simple (and (null fill-pointer)
		      (not adjustable)
		      (null displaced-to))))
    (declare (fixnum array-rank))
    (when (and displaced-index-offset (null displaced-to))
      (error "can't specify :DISPLACED-INDEX-OFFSET without :DISPLACED-TO"))
    (if (and simple (= array-rank 1))
	;; Its a (simple-array * (*))
	(multiple-value-bind (type n-bits)
	    (%vector-widetag-and-n-bits element-type)
	  (declare (type (unsigned-byte 8) type)
		   (type (integer 1 256) n-bits))
	  (let* ((length (car dimensions))
		 (array (allocate-vector
			 type
			 length
			 (ceiling (* (if (= type sb!vm:simple-string-widetag)
					 (1+ length)
					 length)
				     n-bits)
				  sb!vm:n-word-bits))))
	    (declare (type index length))
	    (when initial-element-p
	      (fill array initial-element))
	    (when initial-contents
	      (when initial-element
		(error "can't specify both :INITIAL-ELEMENT and ~
		:INITIAL-CONTENTS"))
	      (unless (= length (length initial-contents))
		(error "There are ~W elements in the :INITIAL-CONTENTS, but ~
		       the vector length is ~W."
		       (length initial-contents)
		       length))
	      (replace array initial-contents))
	    array))
	;; It's either a complex array or a multidimensional array.
	(let* ((total-size (reduce #'* dimensions))
	       (data (or displaced-to
			 (data-vector-from-inits
			  dimensions total-size element-type
			  initial-contents initial-element initial-element-p)))
	       (array (make-array-header
		       (cond ((= array-rank 1)
			      (%complex-vector-widetag element-type))
			     (simple sb!vm:simple-array-widetag)
			     (t sb!vm:complex-array-widetag))
		       array-rank)))
	  (cond (fill-pointer
		 (unless (= array-rank 1)
		   (error "Only vectors can have fill pointers."))
		 (let ((length (car dimensions)))
		   (declare (fixnum length))
		   (setf (%array-fill-pointer array)
		     (cond ((eq fill-pointer t)
			    length)
			   (t
			    (unless (and (fixnump fill-pointer)
					 (>= fill-pointer 0)
					 (<= fill-pointer length))
			      ;; FIXME: should be TYPE-ERROR?
			      (error "invalid fill-pointer ~W"
				     fill-pointer))
			    fill-pointer))))
		 (setf (%array-fill-pointer-p array) t))
		(t
		 (setf (%array-fill-pointer array) total-size)
		 (setf (%array-fill-pointer-p array) nil)))
	  (setf (%array-available-elements array) total-size)
	  (setf (%array-data-vector array) data)
	  (cond (displaced-to
		 (when (or initial-element-p initial-contents)
		   (error "Neither :INITIAL-ELEMENT nor :INITIAL-CONTENTS ~
		   can be specified along with :DISPLACED-TO"))
		 (let ((offset (or displaced-index-offset 0)))
		   (when (> (+ offset total-size)
			    (array-total-size displaced-to))
		     (error "~S doesn't have enough elements." displaced-to))
		   (setf (%array-displacement array) offset)
		   (setf (%array-displaced-p array) t)))
		(t
		 (setf (%array-displaced-p array) nil)))
	  (let ((axis 0))
	    (dolist (dim dimensions)
	      (setf (%array-dimension array axis) dim)
	      (incf axis)))
	  array))))
	
;;; DATA-VECTOR-FROM-INITS returns a simple vector that has the
;;; specified array characteristics. Dimensions is only used to pass
;;; to FILL-DATA-VECTOR for error checking on the structure of
;;; initial-contents.
(defun data-vector-from-inits (dimensions total-size element-type
			       initial-contents initial-element
			       initial-element-p)
  (when (and initial-contents initial-element-p)
    (error "cannot supply both :INITIAL-CONTENTS and :INITIAL-ELEMENT to
	    either MAKE-ARRAY or ADJUST-ARRAY."))
  (let ((data (if initial-element-p
		  (make-array total-size
			      :element-type element-type
			      :initial-element initial-element)
		  (make-array total-size
			      :element-type element-type))))
    (cond (initial-element-p
	   (unless (simple-vector-p data)
	     (unless (typep initial-element element-type)
	       (error "~S cannot be used to initialize an array of type ~S."
		      initial-element element-type))
	     (fill (the vector data) initial-element)))
	  (initial-contents
	   (fill-data-vector data dimensions initial-contents)))
    data))

(defun fill-data-vector (vector dimensions initial-contents)
  (let ((index 0))
    (labels ((frob (axis dims contents)
	       (cond ((null dims)
		      (setf (aref vector index) contents)
		      (incf index))
		     (t
		      (unless (typep contents 'sequence)
			(error "malformed :INITIAL-CONTENTS: ~S is not a ~
				sequence, but ~W more layer~:P needed."
			       contents
			       (- (length dimensions) axis)))
		      (unless (= (length contents) (car dims))
			(error "malformed :INITIAL-CONTENTS: Dimension of ~
				axis ~W is ~W, but ~S is ~W long."
			       axis (car dims) contents (length contents)))
		      (if (listp contents)
			  (dolist (content contents)
			    (frob (1+ axis) (cdr dims) content))
			  (dotimes (i (length contents))
			    (frob (1+ axis) (cdr dims) (aref contents i))))))))
      (frob 0 dimensions initial-contents))))

(defun vector (&rest objects)
  #!+sb-doc
  "Construct a SIMPLE-VECTOR from the given objects."
  (coerce (the list objects) 'simple-vector))

;;;; accessor/setter functions

(eval-when (:compile-toplevel :execute)
  (defparameter *specialized-array-element-types*
    '(t
      character
      bit
      (unsigned-byte 2)
      (unsigned-byte 4)
      (unsigned-byte 8)
      (unsigned-byte 16)
      (unsigned-byte 32)
      (signed-byte 8)
      (signed-byte 16)
      (signed-byte 30)
      (signed-byte 32)
      single-float
      double-float
      #!+long-float long-float
      (complex single-float)
      (complex double-float)
      #!+long-float (complex long-float))))
    
(defun hairy-data-vector-ref (array index)
  (with-array-data ((vector array) (index index) (end))
    (declare (ignore end))
    (etypecase vector .
	       #.(mapcar (lambda (type)
			   (let ((atype `(simple-array ,type (*))))
			     `(,atype
			       (data-vector-ref (the ,atype vector)
						index))))
			 *specialized-array-element-types*))))

(defun hairy-data-vector-set (array index new-value)
  (with-array-data ((vector array) (index index) (end))
    (declare (ignore end) (optimize))
    (etypecase vector .
	       #.(mapcar (lambda (type)
			   (let ((atype `(simple-array ,type (*))))
			     `(,atype
			       (data-vector-set (the ,atype vector)
						index
						(the ,type
						  new-value)))))
			 *specialized-array-element-types*))))

(defun %array-row-major-index (array subscripts
				     &optional (invalid-index-error-p t))
  (declare (array array)
	   (list subscripts))
  (let ((rank (array-rank array)))
    (unless (= rank (length subscripts))
      (error "wrong number of subscripts, ~W, for array of rank ~W"
	     (length subscripts) rank))
    (if (array-header-p array)
	(do ((subs (nreverse subscripts) (cdr subs))
	     (axis (1- (array-rank array)) (1- axis))
	     (chunk-size 1)
	     (result 0))
	    ((null subs) result)
	  (declare (list subs) (fixnum axis chunk-size result))
	  (let ((index (car subs))
		(dim (%array-dimension array axis)))
	    (declare (fixnum index dim))
	    (unless (< -1 index dim)
	      (if invalid-index-error-p
		  (error "invalid index ~W~[~;~:; on axis ~:*~W~] in ~S"
			 index axis array)
		  (return-from %array-row-major-index nil)))
	    (incf result (* chunk-size index))
	    (setf chunk-size (* chunk-size dim))))
	(let ((index (first subscripts)))
	  (unless (< -1 index (length (the (simple-array * (*)) array)))
	    (if invalid-index-error-p
		(error "invalid index ~W in ~S" index array)
		(return-from %array-row-major-index nil)))
	  index))))

(defun array-in-bounds-p (array &rest subscripts)
  #!+sb-doc
  "Return T if the Subscipts are in bounds for the Array, Nil otherwise."
  (if (%array-row-major-index array subscripts nil)
      t))

(defun array-row-major-index (array &rest subscripts)
  (%array-row-major-index array subscripts))

(defun aref (array &rest subscripts)
  #!+sb-doc
  "Return the element of the Array specified by the Subscripts."
  (row-major-aref array (%array-row-major-index array subscripts)))

(defun %aset (array &rest stuff)
  (let ((subscripts (butlast stuff))
	(new-value (car (last stuff))))
    (setf (row-major-aref array (%array-row-major-index array subscripts))
	  new-value)))

;;; FIXME: What's supposed to happen with functions
;;; like AREF when we (DEFUN (SETF FOO) ..) when
;;; DEFSETF FOO is also defined? It seems as though the logical
;;; thing to do would be to nuke the macro definition for (SETF FOO)
;;; and replace it with the (SETF FOO) function, issuing a warning,
;;; just as for ordinary functions
;;;  * (LISP-IMPLEMENTATION-VERSION)
;;;  "18a+ release x86-linux 2.4.7 6 November 1998 cvs"
;;;  * (DEFMACRO ZOO (X) `(+ ,X ,X))
;;;  ZOO
;;;  * (DEFUN ZOO (X) (* 3 X))
;;;  Warning: ZOO previously defined as a macro.
;;;  ZOO
;;; But that doesn't seem to be what happens in CMU CL.
;;;
;;; Also, it would be nice to make DESCRIBE FOO tell whether a symbol
;;; has a setf expansion and/or a setf function defined.

#!-sb-fluid (declaim (inline (setf aref)))
(defun (setf aref) (new-value array &rest subscripts)
  (declare (type array array))
  (setf (row-major-aref array (%array-row-major-index array subscripts))
	new-value))

(defun row-major-aref (array index)
  #!+sb-doc
  "Return the element of array corressponding to the row-major index. This is
   SETF'able."
  (declare (optimize (safety 1)))
  (row-major-aref array index))

(defun %set-row-major-aref (array index new-value)
  (declare (optimize (safety 1)))
  (setf (row-major-aref array index) new-value))

(defun svref (simple-vector index)
  #!+sb-doc
  "Return the INDEX'th element of the given Simple-Vector."
  (declare (optimize (safety 1)))
  (aref simple-vector index))

(defun %svset (simple-vector index new)
  (declare (optimize (safety 1)))
  (setf (aref simple-vector index) new))

(defun bit (bit-array &rest subscripts)
  #!+sb-doc
  "Return the bit from the BIT-ARRAY at the specified SUBSCRIPTS."
  (declare (type (array bit) bit-array) (optimize (safety 1)))
  (row-major-aref bit-array (%array-row-major-index bit-array subscripts)))

(defun %bitset (bit-array &rest stuff)
  (declare (type (array bit) bit-array) (optimize (safety 1)))
  (let ((subscripts (butlast stuff))
	(new-value (car (last stuff))))
    (setf (row-major-aref bit-array
			  (%array-row-major-index bit-array subscripts))
	  new-value)))

#!-sb-fluid (declaim (inline (setf bit)))
(defun (setf bit) (new-value bit-array &rest subscripts)
  (declare (type (array bit) bit-array) (optimize (safety 1)))
  (setf (row-major-aref bit-array
			(%array-row-major-index bit-array subscripts))
	new-value))

(defun sbit (simple-bit-array &rest subscripts)
  #!+sb-doc
  "Return the bit from SIMPLE-BIT-ARRAY at the specified SUBSCRIPTS."
  (declare (type (simple-array bit) simple-bit-array) (optimize (safety 1)))
  (row-major-aref simple-bit-array
		  (%array-row-major-index simple-bit-array subscripts)))

;;; KLUDGE: Not all these things (%SET-ROW-MAJOR-AREF, %SET-FILL-POINTER,
;;; %SET-FDEFINITION, %SCHARSET, %SBITSET..) seem to deserve separate names.
;;; Could we just DEFUN (SETF SBIT) etc. and get rid of the non-ANSI names?
;;; -- WHN 19990911
(defun %sbitset (simple-bit-array &rest stuff)
  (declare (type (simple-array bit) simple-bit-array) (optimize (safety 1)))
  (let ((subscripts (butlast stuff))
	(new-value (car (last stuff))))
    (setf (row-major-aref simple-bit-array
			  (%array-row-major-index simple-bit-array subscripts))
	  new-value)))

#!-sb-fluid (declaim (inline (setf sbit)))
(defun (setf sbit) (new-value bit-array &rest subscripts)
  (declare (type (simple-array bit) bit-array) (optimize (safety 1)))
  (setf (row-major-aref bit-array
			(%array-row-major-index bit-array subscripts))
	new-value))

;;;; miscellaneous array properties

(defun array-element-type (array)
  #!+sb-doc
  "Return the type of the elements of the array"
  (let ((widetag (widetag-of array)))
    (macrolet ((pick-element-type (&rest stuff)
		 `(cond ,@(mapcar (lambda (stuff)
				    (cons
				     (let ((item (car stuff)))
				       (cond ((eq item t)
					      t)
					     ((listp item)
					      (cons 'or
						    (mapcar (lambda (x)
							      `(= widetag ,x))
							    item)))
					     (t
					      `(= widetag ,item))))
				     (cdr stuff)))
				  stuff))))
      ;; FIXME: The data here are redundant with
      ;; *SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES*.
      (pick-element-type
       ((sb!vm:simple-string-widetag sb!vm:complex-string-widetag) 'base-char)
       ((sb!vm:simple-bit-vector-widetag
	 sb!vm:complex-bit-vector-widetag) 'bit)
       (sb!vm:simple-vector-widetag t)
       (sb!vm:simple-array-unsigned-byte-2-widetag '(unsigned-byte 2))
       (sb!vm:simple-array-unsigned-byte-4-widetag '(unsigned-byte 4))
       (sb!vm:simple-array-unsigned-byte-8-widetag '(unsigned-byte 8))
       (sb!vm:simple-array-unsigned-byte-16-widetag '(unsigned-byte 16))
       (sb!vm:simple-array-unsigned-byte-32-widetag '(unsigned-byte 32))
       (sb!vm:simple-array-signed-byte-8-widetag '(signed-byte 8))
       (sb!vm:simple-array-signed-byte-16-widetag '(signed-byte 16))
       (sb!vm:simple-array-signed-byte-30-widetag '(signed-byte 30))
       (sb!vm:simple-array-signed-byte-32-widetag '(signed-byte 32))
       (sb!vm:simple-array-single-float-widetag 'single-float)
       (sb!vm:simple-array-double-float-widetag 'double-float)
       #!+long-float
       (sb!vm:simple-array-long-float-widetag 'long-float)
       (sb!vm:simple-array-complex-single-float-widetag
	'(complex single-float))
       (sb!vm:simple-array-complex-double-float-widetag
	'(complex double-float))
       #!+long-float
       (sb!vm:simple-array-complex-long-float-widetag '(complex long-float))
       ((sb!vm:simple-array-widetag
	 sb!vm:complex-vector-widetag
	 sb!vm:complex-array-widetag)
	(with-array-data ((array array) (start) (end))
	  (declare (ignore start end))
	  (array-element-type array)))
       (t
	(error 'type-error :datum array :expected-type 'array))))))

(defun array-rank (array)
  #!+sb-doc
  "Return the number of dimensions of ARRAY."
  (if (array-header-p array)
      (%array-rank array)
      1))

(defun array-dimension (array axis-number)
  #!+sb-doc
  "Return the length of dimension AXIS-NUMBER of ARRAY."
  (declare (array array) (type index axis-number))
  (cond ((not (array-header-p array))
	 (unless (= axis-number 0)
	   (error "Vector axis is not zero: ~S" axis-number))
	 (length (the (simple-array * (*)) array)))
	((>= axis-number (%array-rank array))
	 (error "Axis number ~W is too big; ~S only has ~D dimension~:P."
		axis-number array (%array-rank array)))
	(t
	 (%array-dimension array axis-number))))

(defun array-dimensions (array)
  #!+sb-doc
  "Return a list whose elements are the dimensions of the array"
  (declare (array array))
  (if (array-header-p array)
      (do ((results nil (cons (array-dimension array index) results))
	   (index (1- (array-rank array)) (1- index)))
	  ((minusp index) results))
      (list (array-dimension array 0))))

(defun array-total-size (array)
  #!+sb-doc
  "Return the total number of elements in the Array."
  (declare (array array))
  (if (array-header-p array)
      (%array-available-elements array)
      (length (the vector array))))

(defun array-displacement (array)
  #!+sb-doc
  "Return the values of :DISPLACED-TO and :DISPLACED-INDEX-offset
   options to MAKE-ARRAY, or NIL and 0 if not a displaced array."
  (declare (type array array))
  (if (and (array-header-p array) ; if unsimple and
	   (%array-displaced-p array)) ; displaced
      (values (%array-data-vector array) (%array-displacement array))
      (values nil 0)))

(defun adjustable-array-p (array)
  #!+sb-doc
  "Return T if (ADJUST-ARRAY ARRAY...) would return an array identical
   to the argument, this happens for complex arrays."
  (declare (array array))
  (not (typep array 'simple-array)))

;;;; fill pointer frobbing stuff

(defun array-has-fill-pointer-p (array)
  #!+sb-doc
  "Return T if the given ARRAY has a fill pointer, or NIL otherwise."
  (declare (array array))
  (and (array-header-p array) (%array-fill-pointer-p array)))

(defun fill-pointer (vector)
  #!+sb-doc
  "Return the FILL-POINTER of the given VECTOR."
  (declare (vector vector))
  (if (and (array-header-p vector) (%array-fill-pointer-p vector))
      (%array-fill-pointer vector)
      (error 'simple-type-error
	     :datum vector
	     :expected-type '(and vector (satisfies array-has-fill-pointer-p))
	     :format-control "~S is not an array with a fill pointer."
	     :format-arguments (list vector))))

(defun %set-fill-pointer (vector new)
  (declare (vector vector) (fixnum new))
  (if (and (array-header-p vector) (%array-fill-pointer-p vector))
      (if (> new (%array-available-elements vector))
	(error
	 "The new fill pointer, ~S, is larger than the length of the vector."
	 new)
	(setf (%array-fill-pointer vector) new))
      (error 'simple-type-error
	     :datum vector
	     :expected-type '(and vector (satisfies array-has-fill-pointer-p))
	     :format-control "~S is not an array with a fill pointer."
	     :format-arguments (list vector))))

;;; FIXME: It'd probably make sense to use a MACROLET to share the
;;; guts of VECTOR-PUSH between VECTOR-PUSH-EXTEND. Such a macro
;;; should probably be based on the VECTOR-PUSH-EXTEND code (which is
;;; new ca. sbcl-0.7.0) rather than the VECTOR-PUSH code (which dates
;;; back to CMU CL).
(defun vector-push (new-el array)
  #!+sb-doc
  "Attempt to set the element of ARRAY designated by its fill pointer
   to NEW-EL, and increment the fill pointer by one. If the fill pointer is
   too large, NIL is returned, otherwise the index of the pushed element is
   returned."
  (declare (vector array))
  (let ((fill-pointer (fill-pointer array)))
    (declare (fixnum fill-pointer))
    (cond ((= fill-pointer (%array-available-elements array))
	   nil)
	  (t
	   (setf (aref array fill-pointer) new-el)
	   (setf (%array-fill-pointer array) (1+ fill-pointer))
	   fill-pointer))))

(defun vector-push-extend (new-element
			   vector
			   &optional
			   (extension (1+ (length vector))))
  (declare (vector vector) (fixnum extension))
  (let ((fill-pointer (fill-pointer vector)))
    (declare (fixnum fill-pointer))
    (when (= fill-pointer (%array-available-elements vector))
      (adjust-array vector (+ fill-pointer extension)))
    (setf (aref vector fill-pointer) new-element)
    (setf (%array-fill-pointer vector) (1+ fill-pointer))
    fill-pointer))

(defun vector-pop (array)
  #!+sb-doc
  "Decrease the fill pointer by 1 and return the element pointed to by the
  new fill pointer."
  (declare (vector array))
  (let ((fill-pointer (fill-pointer array)))
    (declare (fixnum fill-pointer))
    (if (zerop fill-pointer)
	(error "There is nothing left to pop.")
	(aref array
	      (setf (%array-fill-pointer array)
		    (1- fill-pointer))))))

;;;; ADJUST-ARRAY

(defun adjust-array (array dimensions &key
			   (element-type (array-element-type array))
			   (initial-element nil initial-element-p)
			   initial-contents fill-pointer
			   displaced-to displaced-index-offset)
  #!+sb-doc
  "Adjust ARRAY's dimensions to the given DIMENSIONS and stuff."
  (let ((dimensions (if (listp dimensions) dimensions (list dimensions))))
    (cond ((/= (the fixnum (length (the list dimensions)))
	       (the fixnum (array-rank array)))
	   (error "The number of dimensions not equal to rank of array."))
	  ((not (subtypep element-type (array-element-type array)))
	   (error "The new element type, ~S, is incompatible with old type."
		  element-type)))
    (let ((array-rank (length (the list dimensions))))
      (declare (fixnum array-rank))
      (when (and fill-pointer (> array-rank 1))
	(error "Multidimensional arrays can't have fill pointers."))
      (cond (initial-contents
	     ;; array former contents replaced by INITIAL-CONTENTS
	     (if (or initial-element-p displaced-to)
		 (error "INITIAL-CONTENTS may not be specified with ~
		 the :INITIAL-ELEMENT or :DISPLACED-TO option."))
	     (let* ((array-size (apply #'* dimensions))
		    (array-data (data-vector-from-inits
				 dimensions array-size element-type
				 initial-contents initial-element
				 initial-element-p)))
	       (if (adjustable-array-p array)
		   (set-array-header array array-data array-size
				 (get-new-fill-pointer array array-size
						       fill-pointer)
				 0 dimensions nil)
		   (if (array-header-p array)
		       ;; simple multidimensional or single dimensional array
		       (make-array dimensions
				   :element-type element-type
				   :initial-contents initial-contents)
		       array-data))))
	    (displaced-to
	     ;; We already established that no INITIAL-CONTENTS was supplied.
	     (when initial-element
	       (error "The :INITIAL-ELEMENT option may not be specified ~
	              with :DISPLACED-TO."))
	     (unless (subtypep element-type (array-element-type displaced-to))
	       (error "can't displace an array of type ~S into another of ~
		       type ~S"
		      element-type (array-element-type displaced-to)))
	     (let ((displacement (or displaced-index-offset 0))
		   (array-size (apply #'* dimensions)))
	       (declare (fixnum displacement array-size))
	       (if (< (the fixnum (array-total-size displaced-to))
		      (the fixnum (+ displacement array-size)))
		   (error "The :DISPLACED-TO array is too small."))
	       (if (adjustable-array-p array)
		   ;; None of the original contents appear in adjusted array.
		   (set-array-header array displaced-to array-size
				     (get-new-fill-pointer array array-size
							   fill-pointer)
				     displacement dimensions t)
		   ;; simple multidimensional or single dimensional array
		   (make-array dimensions
			       :element-type element-type
			       :displaced-to displaced-to
			       :displaced-index-offset
			       displaced-index-offset))))
	    ((= array-rank 1)
	     (let ((old-length (array-total-size array))
		   (new-length (car dimensions))
		   new-data)
	       (declare (fixnum old-length new-length))
	       (with-array-data ((old-data array) (old-start)
				 (old-end old-length))
		 (cond ((or (%array-displaced-p array)
			    (< old-length new-length))
			(setf new-data
			      (data-vector-from-inits
			       dimensions new-length element-type
			       initial-contents initial-element
			       initial-element-p))
			(replace new-data old-data
				 :start2 old-start :end2 old-end))
		       (t (setf new-data
				(shrink-vector old-data new-length))))
		 (if (adjustable-array-p array)
		     (set-array-header array new-data new-length
				       (get-new-fill-pointer array new-length
							     fill-pointer)
				       0 dimensions nil)
		     new-data))))
	    (t
	     (let ((old-length (%array-available-elements array))
		   (new-length (apply #'* dimensions)))
	       (declare (fixnum old-length new-length))
	       (with-array-data ((old-data array) (old-start)
				 (old-end old-length))
		 (declare (ignore old-end))
		 (let ((new-data (if (or (%array-displaced-p array)
					 (> new-length old-length))
				     (data-vector-from-inits
				      dimensions new-length
				      element-type () initial-element
				      initial-element-p)
				     old-data)))
		   (if (or (zerop old-length) (zerop new-length))
		       (when initial-element-p (fill new-data initial-element))
		       (zap-array-data old-data (array-dimensions array)
				       old-start
				       new-data dimensions new-length
				       element-type initial-element
				       initial-element-p))
		   (set-array-header array new-data new-length
				     new-length 0 dimensions nil)))))))))

(defun get-new-fill-pointer (old-array new-array-size fill-pointer)
  (cond ((not fill-pointer)
	 (when (array-has-fill-pointer-p old-array)
	   (when (> (%array-fill-pointer old-array) new-array-size)
	     (error "cannot ADJUST-ARRAY an array (~S) to a size (~S) that is ~
		    smaller than its fill pointer (~S)"
		    old-array new-array-size (fill-pointer old-array)))
	   (%array-fill-pointer old-array)))
	((not (array-has-fill-pointer-p old-array))
	 (error "cannot supply a non-NIL value (~S) for :FILL-POINTER ~
		in ADJUST-ARRAY unless the array (~S) was originally ~
 		created with a fill pointer"
		fill-pointer
		old-array))
	((numberp fill-pointer)
	 (when (> fill-pointer new-array-size)
	   (error "can't supply a value for :FILL-POINTER (~S) that is larger ~
		  than the new length of the vector (~S)"
		  fill-pointer new-array-size))
	 fill-pointer)
	((eq fill-pointer t)
	 new-array-size)
	(t
	 (error "bogus value for :FILL-POINTER in ADJUST-ARRAY: ~S"
		fill-pointer))))

;;; Destructively alter VECTOR, changing its length to NEW-LENGTH,
;;; which must be less than or equal to its current length.
(defun shrink-vector (vector new-length)
  (declare (vector vector))
  (unless (array-header-p vector)
    (macrolet ((frob (name &rest things)
		 `(etypecase ,name
		    ,@(mapcar (lambda (thing)
				(destructuring-bind (type-spec fill-value)
				    thing
				  `(,type-spec
				    (fill (truly-the ,type-spec ,name)
					  ,fill-value
					  :start new-length))))
			      things))))
      ;; FIXME: The associations between vector types and initial
      ;; values here are redundant with
      ;; *SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES*.
      (frob vector
	(simple-vector 0)
	(simple-base-string #.*default-init-char-form*)
	(simple-bit-vector 0)
	((simple-array (unsigned-byte 2) (*)) 0)
	((simple-array (unsigned-byte 4) (*)) 0)
	((simple-array (unsigned-byte 8) (*)) 0)
	((simple-array (unsigned-byte 16) (*)) 0)
	((simple-array (unsigned-byte 32) (*)) 0)
	((simple-array (signed-byte 8) (*)) 0)
	((simple-array (signed-byte 16) (*)) 0)
	((simple-array (signed-byte 30) (*)) 0)
	((simple-array (signed-byte 32) (*)) 0)
	((simple-array single-float (*)) (coerce 0 'single-float))
	((simple-array double-float (*)) (coerce 0 'double-float))
	#!+long-float
	((simple-array long-float (*)) (coerce 0 'long-float))
	((simple-array (complex single-float) (*))
	 (coerce 0 '(complex single-float)))
	((simple-array (complex double-float) (*))
	 (coerce 0 '(complex double-float)))
	#!+long-float
	((simple-array (complex long-float) (*))
	 (coerce 0 '(complex long-float))))))
  ;; Only arrays have fill-pointers, but vectors have their length
  ;; parameter in the same place.
  (setf (%array-fill-pointer vector) new-length)
  vector)

;;; Fill in array header with the provided information, and return the array.
(defun set-array-header (array data length fill-pointer displacement dimensions
			 &optional displacedp)
  (setf (%array-data-vector array) data)
  (setf (%array-available-elements array) length)
  (cond (fill-pointer
	 (setf (%array-fill-pointer array) fill-pointer)
	 (setf (%array-fill-pointer-p array) t))
	(t
	 (setf (%array-fill-pointer array) length)
	 (setf (%array-fill-pointer-p array) nil)))
  (setf (%array-displacement array) displacement)
  (if (listp dimensions)
      (dotimes (axis (array-rank array))
	(declare (type index axis))
	(setf (%array-dimension array axis) (pop dimensions)))
      (setf (%array-dimension array 0) dimensions))
  (setf (%array-displaced-p array) displacedp)
  array)

;;;; ZAP-ARRAY-DATA for ADJUST-ARRAY

;;; a temporary to be used when OLD-DATA and NEW-DATA are EQ.
;;; KLUDGE: Boy, DYNAMIC-EXTENT would be nice.
(defvar *zap-array-data-temp* (make-array 1000 :initial-element t))

(defun zap-array-data-temp (length element-type initial-element
			    initial-element-p)
  (declare (fixnum length))
  (when (> length (the fixnum (length *zap-array-data-temp*)))
    (setf *zap-array-data-temp*
	  (make-array length :initial-element t)))
  (when initial-element-p
    (unless (typep initial-element element-type)
      (error "~S can't be used to initialize an array of type ~S."
	     initial-element element-type))
    (fill (the simple-vector *zap-array-data-temp*) initial-element
	  :end length))
  *zap-array-data-temp*)

;;; This does the grinding work for ADJUST-ARRAY. It zaps the data
;;; from the OLD-DATA in an arrangement specified by the OLD-DIMS to
;;; the NEW-DATA in an arrangement specified by the NEW-DIMS. OFFSET
;;; is a displaced offset to be added to computed indices of OLD-DATA.
;;; NEW-LENGTH, ELEMENT-TYPE, INITIAL-ELEMENT, and INITIAL-ELEMENT-P
;;; are used when OLD-DATA and NEW-DATA are EQ; in this case, a
;;; temporary must be used and filled appropriately. When OLD-DATA and
;;; NEW-DATA are not EQ, NEW-DATA has already been filled with any
;;; specified initial-element.
(defun zap-array-data (old-data old-dims offset new-data new-dims new-length
		       element-type initial-element initial-element-p)
  (declare (list old-dims new-dims))
  (setq old-dims (nreverse old-dims))
  (setq new-dims (reverse new-dims))
  (if (eq old-data new-data)
      (let ((temp (zap-array-data-temp new-length element-type
				       initial-element initial-element-p)))
	(zap-array-data-aux old-data old-dims offset temp new-dims)
	(dotimes (i new-length) (setf (aref new-data i) (aref temp i))))
      (zap-array-data-aux old-data old-dims offset new-data new-dims)))

(defun zap-array-data-aux (old-data old-dims offset new-data new-dims)
  (declare (fixnum offset))
  (let ((limits (mapcar (lambda (x y)
			  (declare (fixnum x y))
			  (1- (the fixnum (min x y))))
			old-dims new-dims)))
    (macrolet ((bump-index-list (index limits)
		 `(do ((subscripts ,index (cdr subscripts))
		       (limits ,limits (cdr limits)))
		      ((null subscripts) nil)
		    (cond ((< (the fixnum (car subscripts))
			      (the fixnum (car limits)))
			   (rplaca subscripts
				   (1+ (the fixnum (car subscripts))))
			   (return ,index))
			  (t (rplaca subscripts 0))))))
      (do ((index (make-list (length old-dims) :initial-element 0)
		  (bump-index-list index limits)))
	  ((null index))
	(setf (aref new-data (row-major-index-from-dims index new-dims))
	      (aref old-data
		    (+ (the fixnum (row-major-index-from-dims index old-dims))
		       offset)))))))

;;; Figure out the row-major-order index of an array reference from a
;;; list of subscripts and a list of dimensions. This is for internal
;;; calls only, and the subscripts and dim-list variables are assumed
;;; to be reversed from what the user supplied.
(defun row-major-index-from-dims (rev-subscripts rev-dim-list)
  (do ((rev-subscripts rev-subscripts (cdr rev-subscripts))
       (rev-dim-list rev-dim-list (cdr rev-dim-list))
       (chunk-size 1)
       (result 0))
      ((null rev-dim-list) result)
    (declare (fixnum chunk-size result))
    (setq result (+ result
		    (the fixnum (* (the fixnum (car rev-subscripts))
				   chunk-size))))
    (setq chunk-size (* chunk-size (the fixnum (car rev-dim-list))))))

;;;; some bit stuff

(defun bit-array-same-dimensions-p (array1 array2)
  (declare (type (array bit) array1 array2))
  (and (= (array-rank array1)
	  (array-rank array2))
       (dotimes (index (array-rank array1) t)
	 (when (/= (array-dimension array1 index)
		   (array-dimension array2 index))
	   (return nil)))))

(defun pick-result-array (result-bit-array bit-array-1)
  (case result-bit-array
    ((t) bit-array-1)
    ((nil) (make-array (array-dimensions bit-array-1)
		       :element-type 'bit
		       :initial-element 0))
    (t
     (unless (bit-array-same-dimensions-p bit-array-1
					  result-bit-array)
       (error "~S and ~S don't have the same dimensions."
	      bit-array-1 result-bit-array))
     result-bit-array)))

(defmacro def-bit-array-op (name function)
  `(defun ,name (bit-array-1 bit-array-2 &optional result-bit-array)
     ,(format nil
	      "Perform a bit-wise ~A on the elements of BIT-ARRAY-1 and ~
	      BIT-ARRAY-2,~%  putting the results in RESULT-BIT-ARRAY. ~
	      If RESULT-BIT-ARRAY is T,~%  BIT-ARRAY-1 is used. If ~
	      RESULT-BIT-ARRAY is NIL or omitted, a new array is~%  created. ~
	      All the arrays must have the same rank and dimensions."
	      (symbol-name function))
     (declare (type (array bit) bit-array-1 bit-array-2)
	      (type (or (array bit) (member t nil)) result-bit-array))
     (unless (bit-array-same-dimensions-p bit-array-1 bit-array-2)
       (error "~S and ~S don't have the same dimensions."
	      bit-array-1 bit-array-2))
     (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
       (if (and (simple-bit-vector-p bit-array-1)
		(simple-bit-vector-p bit-array-2)
		(simple-bit-vector-p result-bit-array))
	   (locally (declare (optimize (speed 3) (safety 0)))
	     (,name bit-array-1 bit-array-2 result-bit-array))
	   (with-array-data ((data1 bit-array-1) (start1) (end1))
	     (declare (ignore end1))
	     (with-array-data ((data2 bit-array-2) (start2) (end2))
	       (declare (ignore end2))
	       (with-array-data ((data3 result-bit-array) (start3) (end3))
		 (do ((index-1 start1 (1+ index-1))
		      (index-2 start2 (1+ index-2))
		      (index-3 start3 (1+ index-3)))
		     ((>= index-3 end3) result-bit-array)
		   (declare (type index index-1 index-2 index-3))
		   (setf (sbit data3 index-3)
			 (logand (,function (sbit data1 index-1)
					    (sbit data2 index-2))
				 1))))))))))

(def-bit-array-op bit-and logand)
(def-bit-array-op bit-ior logior)
(def-bit-array-op bit-xor logxor)
(def-bit-array-op bit-eqv logeqv)
(def-bit-array-op bit-nand lognand)
(def-bit-array-op bit-nor lognor)
(def-bit-array-op bit-andc1 logandc1)
(def-bit-array-op bit-andc2 logandc2)
(def-bit-array-op bit-orc1 logorc1)
(def-bit-array-op bit-orc2 logorc2)

(defun bit-not (bit-array &optional result-bit-array)
  #!+sb-doc
  "Performs a bit-wise logical NOT on the elements of BIT-ARRAY,
  putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
  BIT-ARRAY is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
  created. Both arrays must have the same rank and dimensions."
  (declare (type (array bit) bit-array)
	   (type (or (array bit) (member t nil)) result-bit-array))
  (let ((result-bit-array (pick-result-array result-bit-array bit-array)))
    (if (and (simple-bit-vector-p bit-array)
	     (simple-bit-vector-p result-bit-array))
	(locally (declare (optimize (speed 3) (safety 0)))
	  (bit-not bit-array result-bit-array))
	(with-array-data ((src bit-array) (src-start) (src-end))
	  (declare (ignore src-end))
	  (with-array-data ((dst result-bit-array) (dst-start) (dst-end))
	    (do ((src-index src-start (1+ src-index))
		 (dst-index dst-start (1+ dst-index)))
		((>= dst-index dst-end) result-bit-array)
	      (declare (type index src-index dst-index))
	      (setf (sbit dst dst-index)
		    (logxor (sbit src src-index) 1))))))))
