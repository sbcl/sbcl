;;;; Inspector for sb-aclrepl
;;;;
;;;; The documentation, which may or may not apply in its entirety at
;;;; any given time, for this functionality is on the ACL website:
;;;;   <http://www.franz.com/support/documentation/6.2/doc/inspector.htm>.
;;;;
;;;; A summary of inspector navigation is contained in the below *INSPECT-HELP*
;;;; variable.

(cl:in-package :sb-aclrepl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +default-inspect-length+ 10))

(defstruct inspect
  ;; stack of parents of inspected object
  object-stack 
  ;;  a stack of indices of parent object components
  select-stack)

;; FIXME - raw mode isn't currently used in object display
(defparameter *current-inspect* nil
  "current inspect") 
(defparameter *inspect-raw* nil
  "Raw mode for object display.")
(defparameter *inspect-length* +default-inspect-length+
  "maximum number of components to print") 
(defparameter *inspect-skip* 0
  "number of initial components to skip when displaying an object") 

(defvar *inspect-help*
  ":istep takes between 0 to 3 arguments.
The commands are:
:i             redisplay current object
:i =           redisplay current object
:i nil         redisplay current object
:i ?           display this help
:i *           inspect the current * value
:i + <form>    inspect the (eval form)
:i <index>     inspect the numbered component of object
:i <name>      inspect the named component of object
:i <form>      evaluation and inspect form
:i -           inspect parent
:i ^           inspect parent
:i <           inspect previous parent component
:i >           inspect next parent component
:i set <index> <form> set indexed component to evalated form
i set <name> <form>  set named component to evalated form
:i print <max> set the maximum number of components to print
:i skip <n>    skip a number of components when printing
:i tree        print inspect stack
")

;;; When *INSPECT-UNBOUND-OBJECT-MARKER* occurs in a parts list, it
;;; indicates that that a slot is unbound.
(defvar *inspect-unbound-object-marker* (gensym "INSPECT-UNBOUND-OBJECT-"))


;; Setup binding for multithreading
(let ((*current-inspect* nil)
      (*inspect-raw* nil)
      (*inspect-length* +default-inspect-length+)
      (*inspect-skip* 0))
  
  (defun inspector (object input-stream output-stream)
    (declare (ignore input-stream))
    (setq object (eval object))
    (setq *current-inspect* (make-inspect))
    (new-break :inspect *current-inspect*)
    (reset-stack)
    (setf (inspect-object-stack *current-inspect*) (list object))
    (setf (inspect-select-stack *current-inspect*)
	  (list (format nil "(inspect ~S)" object)))
    (%inspect output-stream))

 
  (defun set-current-inspect (inspect)
    (setq *current-inspect* inspect))

  (defun istep (arg-string output-stream)
    (%istep arg-string output-stream))

  (setq sb-impl::*inspect-fun* #'inspector)

  (defun reset-stack ()
    (setf (inspect-object-stack *current-inspect*) nil)
    (setf (inspect-select-stack *current-inspect*) nil))

  (defun %istep (arg-string output-stream)
    (unless *current-inspect*
      (setq *current-inspect* (make-inspect)))
    (let* ((args (when arg-string (string-to-list-skip-spaces arg-string)))
	   (option (car args))
	   (option-read (when arg-string
			  (read-from-string arg-string)))
	   (stack (inspect-object-stack *current-inspect*)))
      (cond
	;; Redisplay
	((or (string= "=" option)
	     (zerop (length args)))
	 (%inspect output-stream))
	;; Select parent
	((or (string= "-" option)
	     (string= "^" option))
	 (cond
	   ((> (length stack) 1)
	    (setf (inspect-object-stack *current-inspect*) (cdr stack))
	    (setf (inspect-select-stack *current-inspect*)
		  (cdr (inspect-select-stack *current-inspect*)))
	    (%inspect output-stream))
	   (stack
	    (format output-stream "Object has no parent.~%"))
	   (t
	    (%inspect output-stream))))
	;; Select * to inspect
	((string= "*" option)
	 (reset-stack) 
	 (setf (inspect-object-stack *current-inspect*) (list *))
	 (setf (inspect-select-stack *current-inspect*) (list "(inspect *)"))
	 (set-break-inspect *current-inspect*)
	 (%inspect output-stream))
	;; Start new inspect level for eval'd form
	((string= "+" option)
	 (inspector (eval (read-from-string (second args))) nil output-stream))
	;; Next or previous parent component
	((or (string= "<" option)
	     (string= ">" option))
	 (if stack
	     (if (eq (length stack) 1)
		 (format output-stream "Object does not have a parent")
		 (let ((parent (second stack))
		       (id (car (inspect-select-stack *current-inspect*))))
		   (multiple-value-bind (position parts)
		       (find-object-part-with-id parent id)
		     (let ((new-position (if (string= ">" option)
					     (1+ position)
					     (1- position))))
		       (if (< -1 new-position (parts-count parts))
			   (let* ((value (element-at parts new-position)))
			     (setf (car stack) value)
			     (setf (car (inspect-select-stack *current-inspect*))
				   (if (integerp id)
				       new-position
				       (let ((label (label-at parts new-position)))
					 (if (stringp label)
					     (read-from-string label)
					     label))))
			     (%inspect output-stream))
		       (format output-stream "Parent has no selectable component indexed by ~d"
				   new-position))))))
	     (%inspect output-stream)))
	;; Set component to eval'd form
	((string-equal "set" option)
	 (if stack
	     (let ((id (when (second args)
			 (read-from-string (second args)))))
	       (multiple-value-bind (position parts)
		   (find-object-part-with-id (car stack) id)
		 (if parts
		     (if position
			 (let ((value-stirng (third args)))
			   (when value-stirng
			     (let ((new-value (eval (read-from-string (third args)))))
			       (let ((result 
				      (set-component-value (car stack)
							   id
							   new-value
							   (element-at parts position))))
				 (typecase result
				   (string
				    (format output-stream result))
				   (t
				    (%inspect output-stream)))))))
			 (format output-stream
				 "Object has no selectable component named by ~A" id))
		     (format output-stream
			     "Object has no selectable components"))))
	     (%inspect output-stream)))
	;; Set/reset raw display mode for components
	((string-equal "raw" option)
	 (when stack
	   (when (and (second args)
		      (or (null (second args))
			  (eq (read-from-string (second args)) t)))
	     (setq *inspect-raw* t))
	   (%inspect output-stream)))
	;; Reset stack
	((string-equal "q" option)
	 (reset-stack)
	 (set-break-inspect *current-inspect*))
	;; Display help
	((string-equal "?" option)
	 (format output-stream *inspect-help*))
	;; Set number of components to skip
	((string-equal "skip" option)
	 (let ((len (read-from-string (second args))))
	   (if (and (integerp len) (>= len 0))
	       (let ((*inspect-skip* len)) 
		 (%inspect output-stream))
	       (format output-stream "Skip missing or invalid~%"))))
	;; Print stack tree
	((string-equal "tree" option)
	 (if stack
	     (progn
	       (format output-stream "The current object is:~%")
	       (dotimes (i (length stack))
		 (format output-stream "~A, ~A~%"
			 (inspected-description (nth i stack))
			 (let ((select (nth i (inspect-select-stack *current-inspect*))))
			   (typecase select
			     (integer
			      (format nil "which is componenent number ~d of" select))
			     (symbol
			      (format nil "which is the ~a component of" select))
			     (string
			      (format nil "which was selected by ~S" select))
			     (t
			      (write-to-string select)))))))
	     (%inspect output-stream)))
	;; Set maximum number of components to print 
	((string-equal "print" option)
	 (let ((len (read-from-string (second args))))
	   (if (and (integerp len) (plusp len))
	       (setq *inspect-length* len)
	       (format output-stream "Cannot set print limit to ~A~%" len))))
	;; Select numbered or named component
	((or (symbolp option-read)
	     (integerp option-read))
	 (if stack
	     (multiple-value-bind (position parts)
		 (find-object-part-with-id (car stack) option-read)
	       (cond
		 ((integerp position)
		  (let* ((value (element-at parts position)))
		    (cond ((eq value *inspect-unbound-object-marker*)
			   (format output-stream "That slot is unbound~%"))
			  (t
			   (push value (inspect-object-stack *current-inspect*))
			   (push option-read (inspect-select-stack *current-inspect*))
			   (%inspect output-stream)))))
		 ((null parts)
		  (format output-stream "Object does not contain any subobjects~%"))
		 (t
		  (typecase option-read
		    (symbol
		     (format output-stream
			     "Object has no selectable component named ~A"
			     option))
		    (integer
		     (format output-stream
			     "Object has no selectable component indexed by ~d~&Enter a valid index (~:[0-~W~;0~])~%"
			     option-read
			     (= (parts-count parts) 1)
			     (1- (parts-count parts))))))))
	     (%inspect output-stream)))
	;; Default is to select eval'd form
	(t
	 (reset-stack)
	 (let ((object (eval option-read)))
	   (setf (inspect-object-stack *current-inspect*) (list object))
	   (setf (inspect-select-stack *current-inspect*)
		 (list (format nil ":i ~S" object))))
	 (set-break-inspect *current-inspect*)
	 (%inspect output-stream))
	)))
  
  (defun %inspect (s)
    (if (inspect-object-stack *current-inspect*)
	(let ((inspected (car (inspect-object-stack *current-inspect*))))
	  (setq cl:* inspected)
	  (display-inspected-parts inspected s))
	(format s "No object is being inspected")))


  (defun display-inspected-parts (object stream)
    (multiple-value-bind (elements labels count)
	(inspected-elements object *inspect-length* *inspect-skip*)
      (format stream "~&~A" (inspected-description object))
      (unless (or (characterp object) (typep object 'fixnum))
	(format stream " at #x~X" (sb-kernel:get-lisp-obj-address object)))
      (princ #\newline stream)
      (dotimes (i count)
	(let ((label (elt labels i))
	      (element (elt elements i)))
	  (cond
	    ((eq label :ellipses)
	     (format stream "~&   ...~%"))
	    ((eq label :tail)
	     (format stream "tail-> ~A~%" (inspected-description element)))
	    ((consp label)
	     (format stream
		     (if (and (stringp (cdr label)) (char= (char (cdr label) 0) #\[))
			 ;; for arrays
			 "~4,' D ~A-> ~A~%"
			 ;; for named
			 "~4,' D ~16,1,1,'-A> ~A~%")
		     (car label)
		     (format nil "~A " (cdr label))
		     (if (eq element *inspect-unbound-object-marker*)
			 "..unbound.."
			 (inspected-description element))))
	    (t
	     (if (integerp label)
		 (format stream "~4,' D-> ~A~%" label (inspected-description element))
		 (format stream "~4A-> ~A~%" label (inspected-description element)))))))))
  
  ) ;; end binding for multithreading


;;; THE BEGINNINGS OF AN INSPECTOR API
;;; which can be used to retrieve object descriptions as component values/labels and also
;;; process component length and skip selectors
;;;
;;; FUNCTIONS TO CONSIDER FOR EXPORT
;;;   FIND-OBJECT-PART-WITH-ID
;;;   ELEMENT-AT
;;;   LABEL-AT
;;;   INSPECTED-ELEMENTS
;;;   INSPECTED-DESCRIPTION
;;;
;;; will also need hooks
;;;    *inspect-start-inspection* (maybe. Would setup a window for a GUI inspector)
;;;    *inspect-prompt-fun*
;;;    *inspect-read-cmd*
;;;
;;; and, either an *inspect-process-cmd*, or *inspect-display* hook
;;; That'll depend if choose to have standardized inspector commands such that
;;; (funcall *inspect-read-cmd*) will return a standard command that SBCL will
;;; process and then call the *inspect-display* hook, or if the *inspect-read-cmd*
;;; will return an impl-dependent cmd that sbcl will send to the contributed
;;; inspector for processing and display.

(defun find-object-part-with-id (object id)
  "COMPONENT-ID can be an integer or a name of a id.
Returns (VALUES POSITION PARTS).
POSITION is NIL if the id is invalid or not found."
  (if object
      (let* ((parts (inspected-parts object))
	     (seq-type (parts-seq-type parts))
	     (count (parts-count parts))
	     (components (parts-components parts)))
	(when (symbolp id)
	  (setq id (symbol-name id)))
	(let ((position
	       (cond ((and (eq seq-type :named)
			   (stringp id))
		      (position id (the list components) :key #'car
				:test #'string-equal))
		     ((and (eq seq-type :improper-list)
			   (stringp id)
			   (string-equal id "tail"))
		      (1- count))
		     ((numberp id)
		      (when (< -1 id count)
			id)))))
	  (values position parts)))
      (values nil nil)))


(defun element-at (parts position)
  (let ((count (parts-count parts))
	(components (parts-components parts)))
    (when (< -1 position count)
      (case (parts-seq-type parts)
	(:improper-list
	 (if (= position (1- count))
	     (cdr (last components))
	     (elt components position)))
	(:named
	 (cdr (elt components position)))
	(:array
	 (aref (the array components) position))
	(t
	 (elt components position))))))

(defun label-at (parts position)
  (let ((count (parts-count parts)))
    (when (< -1 position count)
      (case (parts-seq-type parts)
	(:improper-list
	 (if (= position (1- count))
	     :tail
	     position))
	(:array
	 (array-index-string position parts))
	(:named
	 (car (elt (parts-components parts) position)))
	(t
	 position)))))

(defun label-at-maybe-with-index (parts position)
  (let ((label (label-at parts position)))
    (if (stringp label)
	(cons position label)
	label)))

(defun array-index-string (index parts)
  (let ((rev-dimensions (parts-seq-hint parts)))
    (if (null rev-dimensions)
	"[]"
	(let ((list nil))
	  (dolist (dim rev-dimensions)
	    (multiple-value-bind (q r) (floor index dim)
	      (setq index q)
	      (push r list)))
	  (format nil "[~W~{,~W~}]" (car list) (cdr list))))))

(defun inspected-elements (object length skip)
  "Returns elements of an object that have been trimmed and labeled based on
length and skip. Returns (VALUES ELEMENTS LABELS COUNT) where ELEMENTS contains
COUNT ITERMS, LABELS is a SEQUENCES with COUNT items. LABELS may be a string, number,
:tail, or :ellipses. This function may return a COUNT of up to (+ 3 length) which would
include an :ellipses at the beginning, :ellipses at the end, and the last element."
  (let* ((parts (inspected-parts object))
	 (count (parts-count parts)))
    (unless skip (setq skip 0))
    (unless length (setq length count))
    (let* ((last (1- count))
	   (last-req (min last (+ skip length -1))) ;; last requested element
	   (total (min (- count skip) length)))
      (when (and (plusp total) (plusp skip)) ; starting ellipses
	(incf total))
      (when (< last-req last) ; last value
	(incf total) 
	(when (< last-req (1- last)) ; ending ellipses
	  (incf total)))
      (let ((index 0)
	    (elements nil)
	    (labels nil))
	(declare (type (or simple-vector null) elements labels))
	(when (plusp total) 
	  (setq elements (make-array total :adjustable nil :fill-pointer nil :initial-element nil))
	  (setq labels (make-array total :adjustable nil :fill-pointer nil))
	  (when (plusp skip)
	    (setf (aref labels 0) :ellipses)
	    (incf index))
	  (do ((i 0 (1+ i)))
	      ((> i (- last-req skip)))
	    (setf (aref elements (+ i index)) (element-at parts (+ i skip)))
	    (setf (aref labels (+ i index)) (label-at-maybe-with-index parts
					     (+ i skip))))
	  
	  (when (< last-req last) ; last value
	    (setf (aref elements (- total 1)) (element-at parts last))
	    (setf (aref labels (- total 1)) (label-at-maybe-with-index parts
								       last))
	    (when (< last-req (1- last)) ; ending ellipses or 2nd to last value
	      (if (= last-req (- last 2))
		  (progn
		    (setf (aref elements (- total 2)) (element-at parts (1- last)))
		    (setf (aref labels (- total 2)) (label-at-maybe-with-index
						      parts (1- last))))
		  (setf (aref labels (- total 2)) :ellipses)))))
	(values elements labels total)))))



;;; INSPECTED-DESCRIPTION
;;;
;;; Accepts an object and returns
;;;   DESCRIPTION is a summary description of the destructured object,
;;;   e.g. "the object is a CONS".

(defgeneric inspected-description (object))

(defmethod inspected-description ((object symbol))
  (format nil "the symbol ~A" object))

(defmethod inspected-description ((object structure-object))
  (format nil "~W" (find-class (type-of object))))

(defmethod inspected-description ((object package))
  (format nil "the ~A package" (package-name object)))

(defmethod inspected-description ((object standard-object))
  (format nil "~W" (class-of object)))

(defmethod inspected-description ((object sb-kernel:funcallable-instance))
  (format nil "a funcallable-instance of type ~S" (type-of object)))

(defmethod inspected-description ((object function))
  (format nil "~S" object) nil)

(defmethod inspected-description ((object vector))
  (declare (vector object))
  (format nil "a ~:[~;displaced ~]vector (~W)"
	  (and (sb-kernel:array-header-p object)
	       (sb-kernel:%array-displaced-p object))
	  (length object)))

(defmethod inspected-description ((object simple-vector))
  (declare (simple-vector object))
  (format nil "a simple ~A vector (~D)"
	  (array-element-type object)
	  (length object)))

(defmethod inspected-description ((object array))
  (declare (array object))
  (format nil "~:[A displaced~;An~] array of ~A with dimensions ~W"
	  (and (sb-kernel:array-header-p object)
	       (sb-kernel:%array-displaced-p object))
	  (array-element-type object)
	  (array-dimensions object)))

(defun simple-cons-pair-p (object)
  (atom (cdr object)))

(defmethod inspected-description ((object cons))
  (if (simple-cons-pair-p object)
      "a cons cell"
      (inspected-description-of-nontrivial-list object)))

(defun dotted-safe-length (object)
  "Returns (VALUES LENGTH PROPER-P) where length is the number of cons cells"
    (do ((length 0 (1+ length))
	 (lst object (cdr lst)))
	((not (consp lst))
	 (if (null lst)
	     (values length t)
	     (values length nil)))
      ;; nothing to do in body
      ))

(defun inspected-description-of-nontrivial-list (object)
  (multiple-value-bind (length proper-p) (dotted-safe-length object)
    (if proper-p
	(format nil "a proper list with ~D element~:*~P" length)
	(format nil "a dotted list with ~D element~:*~P + tail" length))))

(defmethod inspected-description ((object double-float))
  (format nil "double-float ~W" object))

(defmethod inspected-description ((object single-float))
  (format nil "single-float ~W" object))

(defmethod inspected-description ((object fixnum))
  (format nil "fixnum ~W" object))

(defmethod inspected-description ((object complex))
  (format nil "complex number ~W" object))

(defmethod inspected-description ((object simple-string))
  (format nil "a simple-string (~W) ~W" (length object) object))

(defmethod inspected-description ((object bignum))
  (format nil "bignum ~W" object))

(defmethod inspected-description ((object ratio))
  (format nil "ratio ~W" object))

(defmethod inspected-description ((object character))
  (format nil "character ~W char-code #x~X" object (char-code object)))

(defmethod inspected-description ((object t))
  (format nil "a generic object ~W" object))


;;; INSPECTED-PARTS
;;;
;;; Accepts the arguments OBJECT LENGTH SKIP and returns,
;;;   (LIST COMPONENTS SEQ-TYPE COUNT SEQ-HINT)
;;; where..
;;;
;;;   COMPONENTS are the component parts of OBJECT (whose
;;;   representation is determined by SEQ-TYPE). Except for the
;;;   SEQ-TYPE :named and :array, components is just the OBJECT itself
;;;
;;;   SEQ-TYPE determines what representation is used for components
;;;   of COMPONENTS.
;;;      If SEQ-TYPE is :named, then each element is (CONS NAME VALUE)
;;;      If SEQ-TYPE is :improper-list, then each element is just value,
;;;        but the last element must be retrieved by
;;;        (cdr (last components))
;;;      If SEQ-TYPE is :list, then each element is a value of an array
;;;      If SEQ-TYPE is :vector, then each element is a value of an vector
;;;      If SEQ-TYPE is :array, then each element is a value of an array
;;;        with rank >= 2
;;;
;;;   COUNT is the total number of components in the OBJECT
;;;
;;; SEQ-HINT Stores a seq-type dependent hint. Used by SEQ-TYPE :array
;;; to hold the reverse-dimensions of the orignal array.

(declaim (inline parts-components))
(defun parts-components (parts)
  (first parts))

(declaim (inline parts-count))
(defun parts-count (parts)
  (second parts))

(declaim (inline parts-seq-type))
(defun parts-seq-type (parts)
  (third parts))

(declaim (inline parts-seq-hint))
(defun parts-seq-hint (parts)
  (fourth parts))

(defgeneric inspected-parts (object)
  )

(defmethod inspected-parts ((object symbol))
  (let ((components
	 (list (cons "name" (symbol-name object))
	       (cons "package" (symbol-package object))
	       (cons "value" (if (boundp object)
				 (symbol-value object)
				 *inspect-unbound-object-marker*))
	       (cons "function" (if (fboundp object)
				    (symbol-function object)
				    *inspect-unbound-object-marker*))
	       (cons "plist" (symbol-plist object)))))
    (list components (length components) :named nil)))

(defun inspected-structure-parts (object)
  (let ((components-list '())
	(info (sb-kernel:layout-info (sb-kernel:layout-of object))))
    (when (sb-kernel::defstruct-description-p info)
      (dolist (dd-slot (sb-kernel:dd-slots info) (nreverse components-list))
	(push (cons (sb-kernel:dsd-%name dd-slot)
		    (funcall (sb-kernel:dsd-accessor-name dd-slot) object))
	      components-list)))))

(defmethod inspected-parts ((object structure-object))
  (let ((components (inspected-structure-parts object)))
    (list components (length components) :named nil)))

(defun inspected-standard-object-parts (object)
  (let ((reversed-components nil)
	(class-slots (sb-pcl::class-slots (class-of object))))
    (dolist (class-slot class-slots (nreverse reversed-components))
      (let* ((slot-name (slot-value class-slot 'sb-pcl::name))
	     (slot-value (if (slot-boundp object slot-name)
			       (slot-value object slot-name)
			       *inspect-unbound-object-marker*)))
	(push (cons slot-name slot-value) reversed-components)))))

(defmethod inspected-parts ((object standard-object))
  (let ((components (inspected-standard-object-parts object)))
    (list components (length components) :named nil)))

(defmethod inspected-parts ((object sb-kernel:funcallable-instance))
  (let ((components (inspected-structure-parts object)))
    (list components (length components) :named nil)))

(defmethod inspected-parts ((object function))
  (let* ((type (sb-kernel:widetag-of object))
	 (object (if (= type sb-vm:closure-header-widetag)
		     (sb-kernel:%closure-fun object)
		     object))
	 (components (list (cons "arglist"
			       (sb-kernel:%simple-fun-arglist object)))))
    (list components (length components) :named nil)))

(defmethod inspected-parts ((object vector))
  (list object (length object) :vector nil))

(defmethod inspected-parts ((object array))
  (let ((size (array-total-size object)))
    (list (make-array size :displaced-to object)
	    size
	    :array
	    (reverse (array-dimensions object)))))

(defmethod inspected-parts ((object cons))
  (if (simple-cons-pair-p object)
      (inspected-parts-of-simple-cons object)
      (inspected-parts-of-nontrivial-list object)))

(defun inspected-parts-of-simple-cons (object)
  (let ((components (list (cons "car" (car object))
			(cons "cdr" (cdr object)))))
    (list components 2 :named nil)))

(defun inspected-parts-of-nontrivial-list (object)
    (multiple-value-bind (count proper-p) (dotted-safe-length object)
      (if proper-p
	  (list object count :list nil)
	  ;; count tail element
	  (list object (1+ count) :improper-list nil))))

(defmethod inspected-parts ((object complex))
  (let ((components (list (cons "real" (realpart object))
			(cons "imag" (imagpart object)))))
    (list components (length components) :named nil)))

(defmethod inspected-parts ((object ratio))
  (let ((components (list (cons "numerator" (numerator object))
			(cons "denominator" (denominator object)))))
    (list components (length components) :named nil)))

(defmethod inspected-parts ((object t))
  (list nil 0 nil nil))


;; FIXME - implement setting of component values

(defgeneric set-component-value (object component-id value element))

(defmethod set-component-value ((object cons) id value element)
  (format nil "Cons object does not support setting of component ~A" id))

(defmethod set-component-value ((object array) id value element)
  (format nil "Array object does not support setting of component ~A" id))

(defmethod set-component-value ((object symbol) id value element)
  (format nil "Symbol object does not support setting of component ~A" id))

(defmethod set-component-value ((object structure-object) id value element)
  (format nil "Structure object does not support setting of component ~A" id))

(defmethod set-component-value ((object standard-object) id value element)
  (format nil "Standard object does not support setting of component ~A" id))

(defmethod set-component-value ((object sb-kernel:funcallable-instance) id value element)
  (format nil "Funcallable instance object does not support setting of component ~A" id))

(defmethod set-component-value ((object function) id value element)
  (format nil "Function object does not support setting of component ~A" id))

;; whn believes it is unsafe to change components of this object
(defmethod set-component-value ((object complex) id value element)
  (format nil "Object does not support setting of component ~A" id))

;; whn believes it is unsafe to change components of this object
(defmethod set-component-value ((object ratio) id value element)
  (format nil "Object does not support setting of component ~A" id))

(defmethod set-component-value ((object t) id value element)
  (format nil "Object does not support setting of component ~A" id))

