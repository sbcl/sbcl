;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;;; structure frobbing primitives

(defun %make-instance (length)
  #!+sb-doc
  "Allocate a new instance with LENGTH data slots."
  (declare (type index length))
  (%make-instance length))

(defun %instance-length (instance)
  #!+sb-doc
  "Given an instance, return its length."
  (declare (type instance instance))
  (%instance-length instance))

(defun %instance-ref (instance index)
  #!+sb-doc
  "Return the value from the INDEXth slot of INSTANCE. This is SETFable."
  (%instance-ref instance index))

(defun %instance-set (instance index new-value)
  #!+sb-doc
  "Set the INDEXth slot of INSTANCE to NEW-VALUE."
  (setf (%instance-ref instance index) new-value))

(defun %raw-ref-single (vec index)
  (declare (type index index))
  (%raw-ref-single vec index))

(defun %raw-ref-double (vec index)
  (declare (type index index))
  (%raw-ref-double vec index))

#!+long-float
(defun %raw-ref-long (vec index)
  (declare (type index index))
  (%raw-ref-long vec index))

(defun %raw-set-single (vec index val)
  (declare (type index index))
  (%raw-set-single vec index val))

(defun %raw-set-double (vec index val)
  (declare (type index index))
  (%raw-set-double vec index val))

#!+long-float
(defun %raw-set-long (vec index val)
  (declare (type index index))
  (%raw-set-long vec index val))

(defun %raw-ref-complex-single (vec index)
  (declare (type index index))
  (%raw-ref-complex-single vec index))

(defun %raw-ref-complex-double (vec index)
  (declare (type index index))
  (%raw-ref-complex-double vec index))

#!+long-float
(defun %raw-ref-complex-long (vec index)
  (declare (type index index))
  (%raw-ref-complex-long vec index))

(defun %raw-set-complex-single (vec index val)
  (declare (type index index))
  (%raw-set-complex-single vec index val))

(defun %raw-set-complex-double (vec index val)
  (declare (type index index))
  (%raw-set-complex-double vec index val))

#!+long-float
(defun %raw-set-complex-long (vec index val)
  (declare (type index index))
  (%raw-set-complex-long vec index val))

(defun %instance-layout (instance)
  (%instance-layout instance))

(defun %set-instance-layout (instance new-value)
  (%set-instance-layout instance new-value))

(defun %make-funcallable-instance (len layout)
   (%make-funcallable-instance len layout))

(defun funcallable-instance-p (x) (funcallable-instance-p x))

(defun %funcallable-instance-info (fin i)
  (%funcallable-instance-info fin i))

(defun %set-funcallable-instance-info (fin i new-value)
  (%set-funcallable-instance-info fin i new-value))

(defun funcallable-instance-function (fin)
  (%funcallable-instance-lexenv fin))

;;; The heart of the magic of funcallable instances ("FINs"). The
;;; function for a FIN must be a magical INSTANCE-LAMBDA form. When
;;; called (as with any other function), we grab the code pointer, and
;;; call it, leaving the original function object in LEXENV (in case
;;; it was a closure). If it is actually a FIN, then we need to do an
;;; extra indirection with funcallable-instance-lexenv to get at any
;;; closure environment. This extra indirection is set up when
;;; accessing the closure environment of an INSTANCE-LAMBDA. Note that
;;; the original FIN pointer is lost, so if the called function wants
;;; to get at the original object to do some slot accesses, it must
;;; close over the FIN object.
;;;
;;; If we set the FIN function to be a FIN, we directly copy across
;;; both the code pointer and the lexenv, since that code pointer (for
;;; an instance-lambda) is expecting that lexenv to be accessed. This
;;; effectively pre-flattens what would otherwise be a chain of
;;; indirections. Lest this sound like an excessively obscure case,
;;; note that it happens when PCL dispatch functions are
;;; byte-compiled.
;;;
;;; The only loss is that if someone accesses the
;;; FUNCALLABLE-INSTANCE-FUNCTION, then won't get a FIN back. This
;;; probably doesn't matter, since PCL only sets the FIN function. And
;;; the only reason that interpreted functions are FINs instead of
;;; bare closures is for debuggability.
(defun (setf funcallable-instance-function) (new-value fin)
  (setf (%funcallable-instance-function fin)
	(%closure-function new-value))
  (setf (%funcallable-instance-lexenv fin)
	(if (funcallable-instance-p new-value)
	    (%funcallable-instance-lexenv new-value)
	    new-value)))

;;; Copy any old kind of structure.
(defun copy-structure (structure)
  #!+sb-doc
  "Return a copy of STRUCTURE with the same (EQL) slot values."
  (declare (type structure-object structure))
  (let* ((len (%instance-length structure))
	 (res (%make-instance len))
	 (layout (%instance-layout structure)))

    (declare (type index len))
    (when (layout-invalid layout)
      (error "attempt to copy an obsolete structure:~%  ~S" structure))

    ;; Copy ordinary slots.
    (dotimes (i len)
      (declare (type index i))
      (setf (%instance-ref res i)
	    (%instance-ref structure i)))

    ;; Copy raw slots.
    (let ((raw-index (dd-raw-index (layout-info layout))))
      (when raw-index
	(let* ((data (%instance-ref structure raw-index))
	       (raw-len (length data))
	       (new (make-array raw-len :element-type '(unsigned-byte 32))))
	  (declare (type (simple-array (unsigned-byte 32) (*)) data))
	  (setf (%instance-ref res raw-index) new)
	  (dotimes (i raw-len)
	    (setf (aref new i) (aref data i))))))

    res))

;;; default PRINT and MAKE-LOAD-FORM methods

(defun default-structure-print (structure stream depth)
  (declare (ignore depth))
  (if (funcallable-instance-p structure)
      (print-unreadable-object (structure stream :identity t :type t))
      (let* ((type (%instance-layout structure))
	     (name (sb!xc:class-name (layout-class type)))
	     (dd (layout-info type)))
	(if *print-pretty*
	    (pprint-logical-block (stream nil :prefix "#S(" :suffix ")")
	      (prin1 name stream)
	      (let ((slots (dd-slots dd)))
		(when slots
		  (write-char #\space stream)
		  ;; CMU CL had (PPRINT-INDENT :BLOCK 2 STREAM) here,
		  ;; but I can't see why. -- WHN 20000205
		  (pprint-newline :linear stream)
		  (loop
		    (pprint-pop)
		    (let ((slot (pop slots)))
		      (write-char #\: stream)
		      (output-symbol-name (dsd-%name slot) stream)
		      (write-char #\space stream)
		      (pprint-newline :miser stream)
		      (output-object (funcall (fdefinition (dsd-accessor slot))
					      structure)
				     stream)
		      (when (null slots)
			(return))
		      (write-char #\space stream)
		      (pprint-newline :linear stream))))))
	    (descend-into (stream)
	      (write-string "#S(" stream)
	      (prin1 name stream)
	      (do ((index 0 (1+ index))
		   (slots (dd-slots dd) (cdr slots)))
		  ((or (null slots)
		       (and (not *print-readably*)
			    (>= index *print-length*)))
		   (if (null slots)
		       (write-string ")" stream)
		       (write-string " ...)" stream)))
		(declare (type index index))
		(write-char #\space stream)
		(write-char #\: stream)
		(let ((slot (first slots)))
		  (output-symbol-name (dsd-%name slot) stream)
		  (write-char #\space stream)
		  (output-object (funcall (fdefinition (dsd-accessor slot))
					  structure)
				 stream))))))))
(def!method print-object ((x structure-object) stream)
  (default-structure-print x stream *current-level*))

(defun make-load-form-saving-slots (object &key slot-names environment)
  (declare (ignore object environment))
  (if slot-names
    (error "stub: MAKE-LOAD-FORM-SAVING-SLOTS :SLOT-NAMES not implemented") ; KLUDGE
    :just-dump-it-normally))

;;; Return true if OBJ is an object of the structure type
;;; corresponding to LAYOUT. This is called by the accessor closures,
;;; which have a handle on the type's layout.
;;;
;;; FIXME: This is fairly big, so it should probably become
;;; MAYBE-INLINE instead of INLINE. Or else we could fix things up so
;;; that the things which call it are all closures, so that it's
;;; expanded only in a small number of places.
#!-sb-fluid (declaim (inline typep-to-layout))
(defun typep-to-layout (obj layout)
  (declare (type layout layout) (optimize (speed 3) (safety 0)))
  (when (layout-invalid layout)
    (error "An obsolete structure accessor function was called."))
  ;; FIXME: CMU CL used (%INSTANCEP OBJ) here. Check that
  ;; (TYPEP OBJ 'INSTANCE) is optimized to equally efficient code.
  (and (typep obj 'instance)
       (let (;; FIXME: Mightn't there be a slight efficiency improvement
	     ;; by delaying the binding of DEPTHOID 'til it's needed?
	     (depthoid (layout-depthoid layout))
	     (obj-layout (%instance-layout obj)))
	 (cond ((eq obj-layout layout)
		t)
	       ;; FIXME: Does the test for LAYOUT-INVALID really belong
	       ;; after the test for EQ LAYOUT? Either explain why this
	       ;; is, or change the order.
	       ((layout-invalid obj-layout)
		(error 'layout-invalid
		       :expected-type (layout-class obj-layout)
		       :datum obj))
	       (t
		(and (> (layout-depthoid obj-layout) depthoid)
		     (eq (svref (layout-inherits obj-layout) depthoid)
			 layout)))))))

;;;; implementing structure slot accessors as closures

;;; In the normal case of structures that have a real type (i.e. no
;;; :TYPE option was specified), we want to optimize things for space
;;; as well as speed, since there can be thousands of defined slot
;;; accessors.
;;;
;;; What we do is define the accessors and copier as closures over
;;; general-case code. Since the compiler will normally open-code
;;; accessors, the (minor) extra speed penalty for full calls is not a
;;; concern.
;;;
;;; KLUDGE: This is a minor headache at cold init time, since genesis
;;; doesn't know how to create the closures in the cold image, so the
;;; function definitions aren't done until the appropriate top level
;;; forms are executed, so any forward references to structure slots
;;; (which are compiled into full calls) fail. The headache can be
;;; treated by using SB!XC:DEFSTRUCT on the relevant structure at
;;; build-the-cross-compiler time, so that the compiler is born
;;; knowing how to inline accesses to the relevant structure, so no
;;; full calls are made. This can be achieved by calling
;;; SB!XC:DEFSTRUCT directly, or by using DEF!STRUCT, which (among
;;; other things) calls SB!XC:DEFSTRUCT for you.

;;; Return closures to do slot access according to Layout and DSD. We check
;;; types, then do the access. This is only used for normal slots, not raw
;;; slots.
(defun structure-slot-getter (layout dsd)
  (let ((class (layout-class layout)))
    (if (typep class 'basic-structure-class)
	#'(lambda (structure)
	    (declare (optimize (speed 3) (safety 0)))
	    (flet ((structure-test (structure)
		     (typep-to-layout structure layout)))
	      (unless (structure-test structure)
		(error 'simple-type-error
		       :datum structure
		       ;; FIXME: :EXPECTED-TYPE should be something
		       ;; comprehensible to the user, not this. Perhaps we
		       ;; could work backwards from the LAYOUT-CLASS slot to
		       ;; find something. (Note that all four SIMPLE-TYPE-ERROR
		       ;; calls in this section have the same disease.)
		       :expected-type '(satisfies structure-test)
		       :format-control
		       "Structure for accessor ~S is not a ~S:~% ~S"
		       :format-arguments
		       (list (dsd-accessor dsd)
			     (sb!xc:class-name (layout-class layout))
			     structure))))
	    (%instance-ref structure (dsd-index dsd)))
	#'(lambda (structure)
	    (declare (optimize (speed 3) (safety 0)))
	    (unless (%typep structure class)
	      (error 'simple-type-error
		     :datum structure
		     :expected-type 'class
		     :format-control
		     "The structure for accessor ~S is not a ~S:~% ~S"
		     :format-arguments
		     (list (dsd-accessor dsd) class
			   structure)))
	    (%instance-ref structure (dsd-index dsd))))))
(defun structure-slot-setter (layout dsd)
  (let ((class (layout-class layout)))
    (if (typep class 'basic-structure-class)
	#'(lambda (new-value structure)
	    (declare (optimize (speed 3) (safety 0)))
	    (flet ((structure-test (structure)
		     (typep-to-layout structure layout))
		   (typep-test (new-value)
		     (%typep new-value (dsd-type dsd))))
	      (unless (structure-test structure)
		(error 'simple-type-error
		       :datum structure
		       :expected-type '(satisfies structure-test)
		       :format-control
		       "The structure for setter ~S is not a ~S:~% ~S"
		       :format-arguments
		       (list `(setf ,(dsd-accessor dsd))
			     (sb!xc:class-name (layout-class layout))
			     structure)))
	      (unless  (typep-test new-value)
		(error 'simple-type-error
		       :datum new-value
		       :expected-type '(satisfies typep-test)
		       :format-control
		       "The new value for setter ~S is not a ~S:~% ~S"
		       :format-arguments
		       (list `(setf ,(dsd-accessor dsd))
			      (dsd-type dsd)
			      new-value))))
	    (setf (%instance-ref structure (dsd-index dsd)) new-value))
	#'(lambda (new-value structure)
	    (declare (optimize (speed 3) (safety 0)))
	    (flet ((structure-test (structure)
		     (sb!xc:typep structure class))
		   (typep-test (new-value)
		     (%typep new-value (dsd-type dsd))))
	      (unless (structure-test structure)
		(error 'simple-type-error
		       :datum structure
		       :expected-type '(satisfies structure-test)
		       :format-control
		       "The structure for setter ~S is not a ~S:~% ~S"
		       :format-arguments
		       (list `(setf ,(dsd-accessor dsd))
			     (sb!xc:class-name class)
			     structure)))
	      (unless  (typep-test new-value)
		(error 'simple-type-error
		       :datum new-value
		       :expected-type '(satisfies typep-test)
		       :format-control
		       "The new value for setter ~S is not a ~S:~% ~S"
		       :format-arguments
		       (list `(setf ,(dsd-accessor dsd))
			     (dsd-type dsd)
			     new-value))))
	    (setf (%instance-ref structure (dsd-index dsd)) new-value)))))
