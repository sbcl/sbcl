;;;; array-specific optimizers and transforms

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; utilities for optimizing array operations

;;; Return UPGRADED-ARRAY-ELEMENT-TYPE for CONTINUATION, or do
;;; GIVE-UP-IR1-TRANSFORM if the upgraded element type can't be
;;; determined.
(defun upgraded-element-type-specifier-or-give-up (continuation)
  (let* ((element-ctype (extract-upgraded-element-type continuation))
	 (element-type-specifier (type-specifier element-ctype)))
    (if (eq element-type-specifier '*)
	(give-up-ir1-transform
	 "upgraded array element type not known at compile time")
	element-type-specifier)))

;;; Array access functions return an object from the array, hence its
;;; type is going to be the array upgraded element type.
(defun extract-upgraded-element-type (array)
  (let ((type (continuation-type array)))
    ;; Note that this IF mightn't be satisfied even if the runtime
    ;; value is known to be a subtype of some specialized ARRAY, because
    ;; we can have values declared e.g. (AND SIMPLE-VECTOR UNKNOWN-TYPE),
    ;; which are represented in the compiler as INTERSECTION-TYPE, not
    ;; array type.
    (if (array-type-p type)
	(array-type-specialized-element-type type)
	;; KLUDGE: there is no good answer here, but at least
	;; *wild-type* won't cause HAIRY-DATA-VECTOR-{REF,SET} to be
	;; erroneously optimized (see generic/vm-tran.lisp) -- CSR,
	;; 2002-08-21
	*wild-type*)))

;;; The ``new-value'' for array setters must fit in the array, and the
;;; return type is going to be the same as the new-value for SETF
;;; functions.
(defun assert-new-value-type (new-value array)
  (let ((type (continuation-type array)))
    (when (array-type-p type)
      (assert-continuation-type
       new-value
       (array-type-specialized-element-type type)
       (lexenv-policy (node-lexenv (continuation-dest new-value))))))
  (continuation-type new-value))

(defun assert-array-complex (array)
  (assert-continuation-type
   array
   (make-array-type :complexp t
                    :element-type *wild-type*)
   (lexenv-policy (node-lexenv (continuation-dest array)))))

;;; Return true if ARG is NIL, or is a constant-continuation whose
;;; value is NIL, false otherwise.
(defun unsupplied-or-nil (arg)
  (declare (type (or continuation null) arg))
  (or (not arg)
      (and (constant-continuation-p arg)
	   (not (continuation-value arg)))))

;;;; DERIVE-TYPE optimizers

;;; Array operations that use a specific number of indices implicitly
;;; assert that the array is of that rank.
(defun assert-array-rank (array rank)
  (assert-continuation-type
   array
   (specifier-type `(array * ,(make-list rank :initial-element '*)))
   (lexenv-policy (node-lexenv (continuation-dest array)))))

(defoptimizer (array-in-bounds-p derive-type) ((array &rest indices))
  (assert-array-rank array (length indices))
  *universal-type*)

(defoptimizer (aref derive-type) ((array &rest indices) node)
  (assert-array-rank array (length indices))
  ;; If the node continuation has a single use then assert its type.
  (let ((cont (node-cont node)))
    (when (= (length (find-uses cont)) 1)
      (assert-continuation-type cont (extract-upgraded-element-type array)
                                (lexenv-policy (node-lexenv node)))))
  (extract-upgraded-element-type array))

(defoptimizer (%aset derive-type) ((array &rest stuff))
  (assert-array-rank array (1- (length stuff)))
  (assert-new-value-type (car (last stuff)) array))

(defoptimizer (hairy-data-vector-ref derive-type) ((array index))
  (extract-upgraded-element-type array))
(defoptimizer (data-vector-ref derive-type) ((array index))
  (extract-upgraded-element-type array))

(defoptimizer (data-vector-set derive-type) ((array index new-value))
  (assert-new-value-type new-value array))
(defoptimizer (hairy-data-vector-set derive-type) ((array index new-value))
  (assert-new-value-type new-value array))

;;; Figure out the type of the data vector if we know the argument
;;; element type.
(defoptimizer (%with-array-data derive-type) ((array start end))
  (let ((atype (continuation-type array)))
    (when (array-type-p atype)
      (values-specifier-type
       `(values (simple-array ,(type-specifier
				(array-type-specialized-element-type atype))
			      (*))
		index index index)))))

(defoptimizer (array-row-major-index derive-type) ((array &rest indices))
  (assert-array-rank array (length indices))
  *universal-type*)

(defoptimizer (row-major-aref derive-type) ((array index))
  (extract-upgraded-element-type array))

(defoptimizer (%set-row-major-aref derive-type) ((array index new-value))
  (assert-new-value-type new-value array))

(defoptimizer (make-array derive-type)
	      ((dims &key initial-element element-type initial-contents
		adjustable fill-pointer displaced-index-offset displaced-to))
  (let ((simple (and (unsupplied-or-nil adjustable)
		     (unsupplied-or-nil displaced-to)
		     (unsupplied-or-nil fill-pointer))))
    (or (careful-specifier-type
         `(,(if simple 'simple-array 'array)
            ,(cond ((not element-type) t)
                   ((constant-continuation-p element-type)
                    (continuation-value element-type))
                   (t
                    '*))
            ,(cond ((not simple)
                    '*)
                   ((constant-continuation-p dims)
                    (let ((val (continuation-value dims)))
                      (if (listp val) val (list val))))
                   ((csubtypep (continuation-type dims)
                               (specifier-type 'integer))
                    '(*))
                   (t
                    '*))))
        (specifier-type 'array))))

;;; Complex array operations should assert that their array argument
;;; is complex.  In SBCL, vectors with fill-pointers are complex.
(defoptimizer (fill-pointer derive-type) ((vector))
  (assert-array-complex vector))
(defoptimizer (%set-fill-pointer derive-type) ((vector index))
  (declare (ignorable index))
  (assert-array-complex vector))

(defoptimizer (vector-push derive-type) ((object vector))
  (declare (ignorable object))
  (assert-array-complex vector))
(defoptimizer (vector-push-extend derive-type)
    ((object vector &optional index))
  (declare (ignorable object index))
  (assert-array-complex vector))
(defoptimizer (vector-pop derive-type) ((vector))
  (assert-array-complex vector))

;;;; constructors

;;; Convert VECTOR into a MAKE-ARRAY followed by SETFs of all the
;;; elements.
(define-source-transform vector (&rest elements)
  (let ((len (length elements))
	(n -1))
    (once-only ((n-vec `(make-array ,len)))
      `(progn
	 ,@(mapcar (lambda (el)
		     (once-only ((n-val el))
		       `(locally (declare (optimize (safety 0)))
				 (setf (svref ,n-vec ,(incf n))
				       ,n-val))))
		   elements)
	 ,n-vec))))

;;; Just convert it into a MAKE-ARRAY.
(deftransform make-string ((length &key
				   (element-type 'base-char)
				   (initial-element
				    #.*default-init-char-form*)))
  '(make-array (the index length)
               :element-type element-type
               :initial-element initial-element))

(defstruct (specialized-array-element-type-properties
	    (:conc-name saetp-)
	    (:constructor !make-saetp (ctype
				       initial-element-default
				       n-bits
				       typecode
				       &key
				       (n-pad-elements 0)))
	    (:copier nil))
  ;; the element type, e.g. #<BUILT-IN-CLASS BASE-CHAR (sealed)> or
  ;; #<SB-KERNEL:NUMERIC-TYPE (UNSIGNED-BYTE 4)>
  (ctype (missing-arg) :type ctype :read-only t)
  ;; what we get when the low-level vector-creation logic zeroes all
  ;; the bits (which also serves as the default value of MAKE-ARRAY's
  ;; :INITIAL-ELEMENT keyword)
  (initial-element-default (missing-arg) :read-only t)
  ;; how many bits per element
  (n-bits (missing-arg) :type index :read-only t)
  ;; the low-level type code
  (typecode (missing-arg) :type index :read-only t)
  ;; the number of extra elements we use at the end of the array for
  ;; low level hackery (e.g., one element for arrays of BASE-CHAR,
  ;; which is used for a fixed #\NULL so that when we call out to C
  ;; we don't need to cons a new copy)
  (n-pad-elements (missing-arg) :type index :read-only t))

(defparameter *specialized-array-element-type-properties*
  (map 'simple-vector
       (lambda (args)
	 (destructuring-bind (type-spec &rest rest) args
	   (let ((ctype (specifier-type type-spec)))
	     (apply #'!make-saetp ctype rest))))
       `(;; Erm.  Yeah.  There aren't a lot of things that make sense
	 ;; for an initial element for (ARRAY NIL). -- CSR, 2002-03-07
	 (nil '#:mu 0 ,sb!vm:simple-array-nil-widetag)
	 (base-char ,(code-char 0) 8 ,sb!vm:simple-string-widetag
		    ;; (SIMPLE-STRINGs are stored with an extra trailing
		    ;; #\NULL for convenience in calling out to C.)
		    :n-pad-elements 1)
	 (single-float 0.0f0 32 ,sb!vm:simple-array-single-float-widetag)
	 (double-float 0.0d0 64 ,sb!vm:simple-array-double-float-widetag)
	 #!+long-float (long-float 0.0L0 #!+x86 96 #!+sparc 128
				   ,sb!vm:simple-array-long-float-widetag)
	 (bit 0 1 ,sb!vm:simple-bit-vector-widetag)
	 ;; KLUDGE: The fact that these UNSIGNED-BYTE entries come
	 ;; before their SIGNED-BYTE partners is significant in the
	 ;; implementation of the compiler; some of the cross-compiler
	 ;; code (see e.g. COERCE-TO-SMALLEST-ELTYPE in
	 ;; src/compiler/debug-dump.lisp) attempts to create an array
	 ;; specialized on (UNSIGNED-BYTE FOO), where FOO could be 7;
	 ;; (UNSIGNED-BYTE 7) is SUBTYPEP (SIGNED-BYTE 8), so if we're
	 ;; not careful we could get the wrong specialized array when
	 ;; we try to FIND-IF, below. -- CSR, 2002-07-08
	 ((unsigned-byte 2) 0 2 ,sb!vm:simple-array-unsigned-byte-2-widetag)
	 ((unsigned-byte 4) 0 4 ,sb!vm:simple-array-unsigned-byte-4-widetag)
	 ((unsigned-byte 8) 0 8 ,sb!vm:simple-array-unsigned-byte-8-widetag)
	 ((unsigned-byte 16) 0 16 ,sb!vm:simple-array-unsigned-byte-16-widetag)
	 ((unsigned-byte 32) 0 32 ,sb!vm:simple-array-unsigned-byte-32-widetag)
	 ((signed-byte 8) 0 8 ,sb!vm:simple-array-signed-byte-8-widetag)
	 ((signed-byte 16) 0 16 ,sb!vm:simple-array-signed-byte-16-widetag)
	 ((signed-byte 30) 0 32 ,sb!vm:simple-array-signed-byte-30-widetag)
	 ((signed-byte 32) 0 32 ,sb!vm:simple-array-signed-byte-32-widetag)
	 ((complex single-float) #C(0.0f0 0.0f0) 64
	  ,sb!vm:simple-array-complex-single-float-widetag)
	 ((complex double-float) #C(0.0d0 0.0d0) 128
	  ,sb!vm:simple-array-complex-double-float-widetag)
	 #!+long-float ((complex long-float) #C(0.0L0 0.0L0)
			#!+x86 192 #!+sparc 256
			,sb!vm:simple-array-complex-long-float-widetag)
	 (t 0 32 ,sb!vm:simple-vector-widetag))))

(deftransform make-array ((dims &key initial-element element-type
				     adjustable fill-pointer)
			  (t &rest *))
  (when (null initial-element)
    (give-up-ir1-transform))
  (let* ((eltype (cond ((not element-type) t)
		       ((not (constant-continuation-p element-type))
			(give-up-ir1-transform
			 "ELEMENT-TYPE is not constant."))
		       (t
			(continuation-value element-type))))
	 (eltype-type (ir1-transform-specifier-type eltype))
	 (saetp (find-if (lambda (saetp)
			   (csubtypep eltype-type (saetp-ctype saetp)))
			 *specialized-array-element-type-properties*))
	 (creation-form `(make-array dims
			  :element-type ',(type-specifier (saetp-ctype saetp))
			  ,@(when fill-pointer
				  '(:fill-pointer fill-pointer))
			  ,@(when adjustable
				  '(:adjustable adjustable)))))

    (unless saetp
      (give-up-ir1-transform "ELEMENT-TYPE not found in *SAETP*: ~S" eltype))

    (cond ((and (constant-continuation-p initial-element)
		(eql (continuation-value initial-element)
		     (saetp-initial-element-default saetp)))
	   creation-form)
	  (t
	   ;; error checking for target, disabled on the host because
	   ;; (CTYPE-OF #\Null) is not possible.
	   #-sb-xc-host
	   (when (constant-continuation-p initial-element)
	     (let ((value (continuation-value initial-element)))
	       (cond
		 ((not (csubtypep (ctype-of value)
				  (saetp-ctype saetp)))
		  ;; this case will cause an error at runtime, so we'd
		  ;; better WARN about it now.
		  (compiler-warn "~@<~S is not a ~S (which is the ~
                                 UPGRADED-ARRAY-ELEMENT-TYPE of ~S).~@:>"
				 value
				 (type-specifier (saetp-ctype saetp))
				 eltype))
		 ((not (csubtypep (ctype-of value) eltype-type))
		  ;; this case will not cause an error at runtime, but
		  ;; it's still worth STYLE-WARNing about.
		  (compiler-style-warn "~S is not a ~S."
				       value eltype)))))
	   `(let ((array ,creation-form))
	     (multiple-value-bind (vector)
		 (%data-vector-and-index array 0)
	       (fill vector initial-element))
	     array)))))

;;; The integer type restriction on the length ensures that it will be
;;; a vector. The lack of :ADJUSTABLE, :FILL-POINTER, and
;;; :DISPLACED-TO keywords ensures that it will be simple; the lack of
;;; :INITIAL-ELEMENT relies on another transform to deal with that
;;; kind of initialization efficiently.
(deftransform make-array ((length &key element-type)
			  (integer &rest *))
  (let* ((eltype (cond ((not element-type) t)
		       ((not (constant-continuation-p element-type))
			(give-up-ir1-transform
			 "ELEMENT-TYPE is not constant."))
		       (t
			(continuation-value element-type))))
	 (len (if (constant-continuation-p length)
		  (continuation-value length)
		  '*))
	 (result-type-spec `(simple-array ,eltype (,len)))
	 (eltype-type (ir1-transform-specifier-type eltype))
	 (saetp (find-if (lambda (saetp)
			   (csubtypep eltype-type (saetp-ctype saetp)))
			 *specialized-array-element-type-properties*)))
    (unless saetp
      (give-up-ir1-transform
       "cannot open-code creation of ~S" result-type-spec))
    #-sb-xc-host
    (unless (csubtypep (ctype-of (saetp-initial-element-default saetp))
		       eltype-type)
      ;; This situation arises e.g. in (MAKE-ARRAY 4 :ELEMENT-TYPE
      ;; '(INTEGER 1 5)) ANSI's definition of MAKE-ARRAY says "If
      ;; INITIAL-ELEMENT is not supplied, the consequences of later
      ;; reading an uninitialized element of new-array are undefined,"
      ;; so this could be legal code as long as the user plans to
      ;; write before he reads, and if he doesn't we're free to do
      ;; anything we like. But in case the user doesn't know to write
      ;; elements before he reads elements (or to read manuals before
      ;; he writes code:-), we'll signal a STYLE-WARNING in case he
      ;; didn't realize this.
      (compiler-style-warn "The default initial element ~S is not a ~S."
			   (saetp-initial-element-default saetp)
			   eltype))
    (let* ((n-bits-per-element (saetp-n-bits saetp))
	   (typecode (saetp-typecode saetp))
	   (n-pad-elements (saetp-n-pad-elements saetp))
	   (padded-length-form (if (zerop n-pad-elements)
				   'length
				   `(+ length ,n-pad-elements)))
	   (n-words-form
	    (cond
	      ((= n-bits-per-element 0) 0)
	      ((>= n-bits-per-element sb!vm:n-word-bits)
	       `(* ,padded-length-form
		 (the fixnum ; i.e., not RATIO
		   ,(/ n-bits-per-element sb!vm:n-word-bits))))
	      (t
	       (let ((n-elements-per-word (/ sb!vm:n-word-bits
					     n-bits-per-element)))
		 (declare (type index n-elements-per-word)) ; i.e., not RATIO
		 `(ceiling ,padded-length-form ,n-elements-per-word))))))
      (values
       `(truly-the ,result-type-spec
	 (allocate-vector ,typecode length ,n-words-form))
       '((declare (type index length)))))))

;;; The list type restriction does not ensure that the result will be a
;;; multi-dimensional array. But the lack of adjustable, fill-pointer,
;;; and displaced-to keywords ensures that it will be simple.
;;;
;;; FIXME: should we generalize this transform to non-simple (though
;;; non-displaced-to) arrays, given that we have %WITH-ARRAY-DATA to
;;; deal with those? Maybe when the DEFTRANSFORM
;;; %DATA-VECTOR-AND-INDEX in the VECTOR case problem is solved? --
;;; CSR, 2002-07-01
(deftransform make-array ((dims &key element-type)
			  (list &rest *))
  (unless (or (null element-type) (constant-continuation-p element-type))
    (give-up-ir1-transform
     "The element-type is not constant; cannot open code array creation."))
  (unless (constant-continuation-p dims)
    (give-up-ir1-transform
     "The dimension list is not constant; cannot open code array creation."))
  (let ((dims (continuation-value dims)))
    (unless (every #'integerp dims)
      (give-up-ir1-transform
       "The dimension list contains something other than an integer: ~S"
       dims))
    (if (= (length dims) 1)
	`(make-array ',(car dims)
		     ,@(when element-type
			 '(:element-type element-type)))
	(let* ((total-size (reduce #'* dims))
	       (rank (length dims))
	       (spec `(simple-array
		       ,(cond ((null element-type) t)
			      ((constant-continuation-p element-type)
			       (continuation-value element-type))
			      (t '*))
			   ,(make-list rank :initial-element '*))))
	  `(let ((header (make-array-header sb!vm:simple-array-widetag ,rank)))
	     (setf (%array-fill-pointer header) ,total-size)
	     (setf (%array-fill-pointer-p header) nil)
	     (setf (%array-available-elements header) ,total-size)
	     (setf (%array-data-vector header)
		   (make-array ,total-size
			       ,@(when element-type
				   '(:element-type element-type))))
	     (setf (%array-displaced-p header) nil)
	     ,@(let ((axis -1))
		 (mapcar (lambda (dim)
			   `(setf (%array-dimension header ,(incf axis))
				  ,dim))
			 dims))
	     (truly-the ,spec header))))))

;;;; miscellaneous properties of arrays

;;; Transforms for various array properties. If the property is know
;;; at compile time because of a type spec, use that constant value.

;;; If we can tell the rank from the type info, use it instead.
(deftransform array-rank ((array))
  (let ((array-type (continuation-type array)))
    (unless (array-type-p array-type)
      (give-up-ir1-transform))
    (let ((dims (array-type-dimensions array-type)))
      (if (not (listp dims))
	  (give-up-ir1-transform
	   "The array rank is not known at compile time: ~S"
	   dims)
	  (length dims)))))

;;; If we know the dimensions at compile time, just use it. Otherwise,
;;; if we can tell that the axis is in bounds, convert to
;;; %ARRAY-DIMENSION (which just indirects the array header) or length
;;; (if it's simple and a vector).
(deftransform array-dimension ((array axis)
			       (array index))
  (unless (constant-continuation-p axis)
    (give-up-ir1-transform "The axis is not constant."))
  (let ((array-type (continuation-type array))
	(axis (continuation-value axis)))
    (unless (array-type-p array-type)
      (give-up-ir1-transform))
    (let ((dims (array-type-dimensions array-type)))
      (unless (listp dims)
	(give-up-ir1-transform
	 "The array dimensions are unknown; must call ARRAY-DIMENSION at runtime."))
      (unless (> (length dims) axis)
	(abort-ir1-transform "The array has dimensions ~S, ~W is too large."
			     dims
			     axis))
      (let ((dim (nth axis dims)))
	(cond ((integerp dim)
	       dim)
	      ((= (length dims) 1)
	       (ecase (array-type-complexp array-type)
		 ((t)
		  '(%array-dimension array 0))
		 ((nil)
		  '(length array))
		 ((:maybe)
		  (give-up-ir1-transform
		   "can't tell whether array is simple"))))
	      (t
	       '(%array-dimension array axis)))))))

;;; If the length has been declared and it's simple, just return it.
(deftransform length ((vector)
		      ((simple-array * (*))))
  (let ((type (continuation-type vector)))
    (unless (array-type-p type)
      (give-up-ir1-transform))
    (let ((dims (array-type-dimensions type)))
      (unless (and (listp dims) (integerp (car dims)))
	(give-up-ir1-transform
	 "Vector length is unknown, must call LENGTH at runtime."))
      (car dims))))

;;; All vectors can get their length by using VECTOR-LENGTH. If it's
;;; simple, it will extract the length slot from the vector. It it's
;;; complex, it will extract the fill pointer slot from the array
;;; header.
(deftransform length ((vector) (vector))
  '(vector-length vector))

;;; If a simple array with known dimensions, then VECTOR-LENGTH is a
;;; compile-time constant.
(deftransform vector-length ((vector) ((simple-array * (*))))
  (let ((vtype (continuation-type vector)))
    (if (array-type-p vtype)
	(let ((dim (first (array-type-dimensions vtype))))
	  (when (eq dim '*) (give-up-ir1-transform))
	  dim)
	(give-up-ir1-transform))))

;;; Again, if we can tell the results from the type, just use it.
;;; Otherwise, if we know the rank, convert into a computation based
;;; on array-dimension. We can wrap a TRULY-THE INDEX around the
;;; multiplications because we know that the total size must be an
;;; INDEX.
(deftransform array-total-size ((array)
				(array))
  (let ((array-type (continuation-type array)))
    (unless (array-type-p array-type)
      (give-up-ir1-transform))
    (let ((dims (array-type-dimensions array-type)))
      (unless (listp dims)
	(give-up-ir1-transform "can't tell the rank at compile time"))
      (if (member '* dims)
	  (do ((form 1 `(truly-the index
				   (* (array-dimension array ,i) ,form)))
	       (i 0 (1+ i)))
	      ((= i (length dims)) form))
	  (reduce #'* dims)))))

;;; Only complex vectors have fill pointers.
(deftransform array-has-fill-pointer-p ((array))
  (let ((array-type (continuation-type array)))
    (unless (array-type-p array-type)
      (give-up-ir1-transform))
    (let ((dims (array-type-dimensions array-type)))
      (if (and (listp dims) (not (= (length dims) 1)))
	  nil
	  (ecase (array-type-complexp array-type)
	    ((t)
	     t)
	    ((nil)
	     nil)
	    ((:maybe)
	     (give-up-ir1-transform
	      "The array type is ambiguous; must call ~
	      ARRAY-HAS-FILL-POINTER-P at runtime.")))))))

;;; Primitive used to verify indices into arrays. If we can tell at
;;; compile-time or we are generating unsafe code, don't bother with
;;; the VOP.
(deftransform %check-bound ((array dimension index))
  (unless (constant-continuation-p dimension)
    (give-up-ir1-transform))
  (let ((dim (continuation-value dimension)))
    `(the (integer 0 ,dim) index)))
(deftransform %check-bound ((array dimension index) * *
			    :policy (and (> speed safety) (= safety 0)))
  'index)

;;;; WITH-ARRAY-DATA

;;; This checks to see whether the array is simple and the start and
;;; end are in bounds. If so, it proceeds with those values.
;;; Otherwise, it calls %WITH-ARRAY-DATA. Note that %WITH-ARRAY-DATA
;;; may be further optimized.
;;;
;;; Given any ARRAY, bind DATA-VAR to the array's data vector and
;;; START-VAR and END-VAR to the start and end of the designated
;;; portion of the data vector. SVALUE and EVALUE are any start and
;;; end specified to the original operation, and are factored into the
;;; bindings of START-VAR and END-VAR. OFFSET-VAR is the cumulative
;;; offset of all displacements encountered, and does not include
;;; SVALUE.
;;;
;;; When FORCE-INLINE is set, the underlying %WITH-ARRAY-DATA form is
;;; forced to be inline, overriding the ordinary judgment of the
;;; %WITH-ARRAY-DATA DEFTRANSFORMs. Ordinarily the DEFTRANSFORMs are
;;; fairly picky about their arguments, figuring that if you haven't
;;; bothered to get all your ducks in a row, you probably don't care
;;; that much about speed anyway! But in some cases it makes sense to
;;; do type testing inside %WITH-ARRAY-DATA instead of outside, and
;;; the DEFTRANSFORM can't tell that that's going on, so it can make
;;; sense to use FORCE-INLINE option in that case.
(def!macro with-array-data (((data-var array &key offset-var)
			     (start-var &optional (svalue 0))
			     (end-var &optional (evalue nil))
			     &key force-inline)
			    &body forms)
  (once-only ((n-array array)
	      (n-svalue `(the index ,svalue))
	      (n-evalue `(the (or index null) ,evalue)))
    `(multiple-value-bind (,data-var
			   ,start-var
			   ,end-var
			   ,@(when offset-var `(,offset-var)))
	 (if (not (array-header-p ,n-array))
	     (let ((,n-array ,n-array))
	       (declare (type (simple-array * (*)) ,n-array))
	       ,(once-only ((n-len `(length ,n-array))
			    (n-end `(or ,n-evalue ,n-len)))
		  `(if (<= ,n-svalue ,n-end ,n-len)
		       ;; success
		       (values ,n-array ,n-svalue ,n-end 0)
		       (failed-%with-array-data ,n-array
						,n-svalue
						,n-evalue))))
	     (,(if force-inline '%with-array-data-macro '%with-array-data)
	      ,n-array ,n-svalue ,n-evalue))
       ,@forms)))

;;; This is the fundamental definition of %WITH-ARRAY-DATA, for use in
;;; DEFTRANSFORMs and DEFUNs.
(def!macro %with-array-data-macro (array
				   start
				   end
				   &key
				   (element-type '*)
				   unsafe?
				   fail-inline?)
  (with-unique-names (size defaulted-end data cumulative-offset)
    `(let* ((,size (array-total-size ,array))
	    (,defaulted-end
	      (cond (,end
		     (unless (or ,unsafe? (<= ,end ,size))
		       ,(if fail-inline?
			    `(error 'bounding-indices-bad-error
			      :datum (cons ,start ,end)
			      :expected-type `(cons (integer 0 ,',size)
					            (integer ,',start ,',size))
			      :object ,array)
			    `(failed-%with-array-data ,array ,start ,end)))
		     ,end)
		    (t ,size))))
       (unless (or ,unsafe? (<= ,start ,defaulted-end))
	 ,(if fail-inline?
	      `(error 'bounding-indices-bad-error
		:datum (cons ,start ,end)
		:expected-type `(cons (integer 0 ,',size)
				      (integer ,',start ,',size))
		:object ,array)
	      `(failed-%with-array-data ,array ,start ,end)))
       (do ((,data ,array (%array-data-vector ,data))
	    (,cumulative-offset 0
				(+ ,cumulative-offset
				   (%array-displacement ,data))))
	   ((not (array-header-p ,data))
	    (values (the (simple-array ,element-type 1) ,data)
		    (the index (+ ,cumulative-offset ,start))
		    (the index (+ ,cumulative-offset ,defaulted-end))
		    (the index ,cumulative-offset)))
	 (declare (type index ,cumulative-offset))))))

(deftransform %with-array-data ((array start end)
				;; It might very well be reasonable to
				;; allow general ARRAY here, I just
				;; haven't tried to understand the
				;; performance issues involved. --
				;; WHN, and also CSR 2002-05-26
				((or vector simple-array) index (or index null))
				*
				:important t
				:node node
				:policy (> speed space))
  "inline non-SIMPLE-vector-handling logic"
  (let ((element-type (upgraded-element-type-specifier-or-give-up array)))
    `(%with-array-data-macro array start end
			     :unsafe? ,(policy node (= safety 0))
			     :element-type ,element-type)))

;;;; array accessors

;;; We convert all typed array accessors into AREF and %ASET with type
;;; assertions on the array.
(macrolet ((define-frob (reffer setter type)
	     `(progn
		(define-source-transform ,reffer (a &rest i)
		  `(aref (the ,',type ,a) ,@i))
		(define-source-transform ,setter (a &rest i)
		  `(%aset (the ,',type ,a) ,@i)))))
  (define-frob sbit %sbitset (simple-array bit))
  (define-frob bit %bitset (array bit)))
(macrolet ((define-frob (reffer setter type)
	     `(progn
		(define-source-transform ,reffer (a i)
		  `(aref (the ,',type ,a) ,i))
	        (define-source-transform ,setter (a i v)
		  `(%aset (the ,',type ,a) ,i ,v)))))
  (define-frob svref %svset simple-vector)
  (define-frob schar %scharset simple-string)
  (define-frob char %charset string))

(macrolet (;; This is a handy macro for computing the row-major index
	   ;; given a set of indices. We wrap each index with a call
	   ;; to %CHECK-BOUND to ensure that everything works out
	   ;; correctly. We can wrap all the interior arithmetic with
	   ;; TRULY-THE INDEX because we know the the resultant
	   ;; row-major index must be an index.
	   (with-row-major-index ((array indices index &optional new-value)
				  &rest body)
	     `(let (n-indices dims)
		(dotimes (i (length ,indices))
		  (push (make-symbol (format nil "INDEX-~D" i)) n-indices)
		  (push (make-symbol (format nil "DIM-~D" i)) dims))
		(setf n-indices (nreverse n-indices))
		(setf dims (nreverse dims))
		`(lambda (,',array ,@n-indices
				   ,@',(when new-value (list new-value)))
		   (let* (,@(let ((,index -1))
			      (mapcar (lambda (name)
					`(,name (array-dimension
						 ,',array
						 ,(incf ,index))))
				      dims))
			    (,',index
			     ,(if (null dims)
				  0
				(do* ((dims dims (cdr dims))
				      (indices n-indices (cdr indices))
				      (last-dim nil (car dims))
				      (form `(%check-bound ,',array
							   ,(car dims)
							   ,(car indices))
					    `(truly-the
					      index
					      (+ (truly-the index
							    (* ,form
							       ,last-dim))
						 (%check-bound
						  ,',array
						  ,(car dims)
						  ,(car indices))))))
				    ((null (cdr dims)) form)))))
		     ,',@body)))))

  ;; Just return the index after computing it.
  (deftransform array-row-major-index ((array &rest indices))
    (with-row-major-index (array indices index)
      index))

  ;; Convert AREF and %ASET into a HAIRY-DATA-VECTOR-REF (or
  ;; HAIRY-DATA-VECTOR-SET) with the set of indices replaced with the an
  ;; expression for the row major index.
  (deftransform aref ((array &rest indices))
    (with-row-major-index (array indices index)
      (hairy-data-vector-ref array index)))
  (deftransform %aset ((array &rest stuff))
    (let ((indices (butlast stuff)))
      (with-row-major-index (array indices index new-value)
	(hairy-data-vector-set array index new-value)))))

;;; Just convert into a HAIRY-DATA-VECTOR-REF (or
;;; HAIRY-DATA-VECTOR-SET) after checking that the index is inside the
;;; array total size.
(deftransform row-major-aref ((array index))
  `(hairy-data-vector-ref array
			  (%check-bound array (array-total-size array) index)))
(deftransform %set-row-major-aref ((array index new-value))
  `(hairy-data-vector-set array
			  (%check-bound array (array-total-size array) index)
			  new-value))

;;;; bit-vector array operation canonicalization
;;;;
;;;; We convert all bit-vector operations to have the result array
;;;; specified. This allows any result allocation to be open-coded,
;;;; and eliminates the need for any VM-dependent transforms to handle
;;;; these cases.

(macrolet ((def (fun)
             `(progn
               (deftransform ,fun ((bit-array-1 bit-array-2
						&optional result-bit-array)
                                   (bit-vector bit-vector &optional null) *
                                   :policy (>= speed space))
                 `(,',fun bit-array-1 bit-array-2
                   (make-array (length bit-array-1) :element-type 'bit)))
               ;; If result is T, make it the first arg.
               (deftransform ,fun ((bit-array-1 bit-array-2 result-bit-array)
                                   (bit-vector bit-vector (member t)) *)
                 `(,',fun bit-array-1 bit-array-2 bit-array-1)))))
  (def bit-and)
  (def bit-ior)
  (def bit-xor)
  (def bit-eqv)
  (def bit-nand)
  (def bit-nor)
  (def bit-andc1)
  (def bit-andc2)
  (def bit-orc1)
  (def bit-orc2))

;;; Similar for BIT-NOT, but there is only one arg...
(deftransform bit-not ((bit-array-1 &optional result-bit-array)
		       (bit-vector &optional null) *
		       :policy (>= speed space))
  '(bit-not bit-array-1
	    (make-array (length bit-array-1) :element-type 'bit)))
(deftransform bit-not ((bit-array-1 result-bit-array)
		       (bit-vector (constant-arg t)))
  '(bit-not bit-array-1 bit-array-1))
;;; FIXME: What does (CONSTANT-ARG T) mean? Is it the same thing
;;; as (CONSTANT-ARG (MEMBER T)), or does it mean any constant
;;; value?

;;; Pick off some constant cases.
(deftransform array-header-p ((array) (array))
  (let ((type (continuation-type array)))
    (unless (array-type-p type)
      (give-up-ir1-transform))
    (let ((dims (array-type-dimensions type)))
      (cond ((csubtypep type (specifier-type '(simple-array * (*))))
	     ;; no array header
	     nil)
	    ((and (listp dims) (> (length dims) 1))
	     ;; multi-dimensional array, will have a header
	     t)
	    (t
	     (give-up-ir1-transform))))))
