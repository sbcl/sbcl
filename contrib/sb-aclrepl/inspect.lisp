;;;; Inspector for sb-aclrepl
;;;;
;;;; The documentation, which may or may not apply in its entirety at
;;;; any given time, for this functionality is on the ACL website:
;;;;   <http://www.franz.com/support/documentation/6.2/doc/inspector.htm>.
;;;;
;;;; A summary of inspector navigation is contained in the below *INSPECT-HELP*
;;;; variable.

(cl:in-package #:sb-aclrepl)

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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *inspect-unbound-object-marker* (gensym "INSPECT-UNBOUND-OBJECT-")))


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
    (redisplay output-stream))

  (setq sb-impl::*inspect-fun* #'inspector)
  
  (defun istep (args stream)
    (unless *current-inspect*
      (setq *current-inspect* (make-inspect)))
    (istep-dispatch args
		     (first args)
		     (when (first args) (read-from-string (first args)))
		     stream))

  (defun istep-dispatch (args option-string option stream)
    (cond
      ((or (string= "=" option-string) (zerop (length args)))
       (istep-cmd-redisplay stream))
      ((or (string= "-" option-string) (string= "^" option-string))
       (istep-cmd-parent stream))
      ((string= "*" option-string)
       (istep-cmd-inspect-* stream))
      ((string= "+" option-string)
       (istep-cmd-inspect-new-form (read-from-string (second args)) stream))
      ((or (string= "<" option-string)
	   (string= ">" option-string))
       (istep-cmd-select-parent-component option-string stream))
      ((string-equal "set" option-string)
       (istep-cmd-set (second args) (third args) stream))
      ((string-equal "raw" option-string)
       (istep-cmd-set-raw (second args) stream))
      ((string-equal "q" option-string)
       (istep-cmd-reset))
      ((string-equal "?" option-string)
       (istep-cmd-help stream))
      ((string-equal "skip" option-string)
       (istep-cmd-skip (second args) stream))
      ((string-equal "tree" option-string)
       (istep-cmd-tree stream)) 
      ((string-equal "print" option-string)
       (istep-cmd-print (second args) stream))
      ((or (symbolp option)
	   (integerp option))
       (istep-cmd-select-component option stream))
      (t
       (istep-cmd-set-stack option stream))))

  (defun set-current-inspect (inspect)
    (setq *current-inspect* inspect))

  (defun reset-stack ()
    (setf (inspect-object-stack *current-inspect*) nil)
    (setf (inspect-select-stack *current-inspect*) nil))

  (defun output-inspect-note (stream note &rest args)
    (apply #'format stream note args)
    (princ #\Newline stream))

  (defun stack ()
     (inspect-object-stack *current-inspect*))

  (defun redisplay (stream)
    (display-current stream))

  ;;;
  ;;; istep command processing
  ;;;
  
  (defun istep-cmd-redisplay (stream)
    (redisplay stream))

  (defun istep-cmd-parent (stream)
    (cond
      ((> (length (inspect-object-stack *current-inspect*)) 1)
       (setf (inspect-object-stack *current-inspect*)
	     (cdr (inspect-object-stack *current-inspect*)))
       (setf (inspect-select-stack *current-inspect*)
	     (cdr (inspect-select-stack *current-inspect*)))
       (redisplay stream))
      ((stack)
       (output-inspect-note stream "Object has no parent"))
      (t
       (no-object-msg stream))))
  
  (defun istep-cmd-inspect-* (stream)
    (reset-stack) 
    (setf (inspect-object-stack *current-inspect*) (list *))
    (setf (inspect-select-stack *current-inspect*) (list "(inspect *)"))
    (set-break-inspect *current-inspect*)
    (redisplay stream))

  (defun istep-cmd-inspect-new-form (form stream)
    (inspector (eval form) nil stream))

  (defun istep-cmd-select-parent-component (option stream)
    (if (stack)
	(if (eql (length (stack)) 1)
	    (output-inspect-note stream "Object does not have a parent")
	    (let ((parent (second (stack)))
		  (id (car (inspect-select-stack *current-inspect*))))
	      (multiple-value-bind (position parts)
		  (find-part-id parent id)
		(let ((new-position (if (string= ">" option)
					(1+ position)
					(1- position))))
		  (if (< -1 new-position (parts-count parts))
		      (let* ((value (element-at parts new-position)))
			(setf (car (inspect-object-stack *current-inspect*))
			      value)
			(setf (car (inspect-select-stack *current-inspect*))
			      (id-at parts new-position))
			(redisplay stream))
		      (output-inspect-note stream
					   "Parent has no selectable component indexed by ~d"
					   new-position))))))
	(no-object-msg stream)))

  (defun istep-cmd-set-raw (option-string stream)
    (when (inspect-object-stack *current-inspect*)
      (cond
	((null option-string)
	 (setq *inspect-raw* t))
	((eq (read-from-string option-string) t)
	 (setq *inspect-raw* t))
	((eq (read-from-string option-string) nil)
	 (setq *inspect-raw* nil)))
      (redisplay stream)))

  (defun istep-cmd-reset ()
    (reset-stack)
    (set-break-inspect *current-inspect*))

  (defun istep-cmd-help (stream)
    (format stream *inspect-help*))

  (defun istep-cmd-skip (option-string stream)
    (if option-string
	(let ((len (read-from-string option-string)))
	  (if (and (integerp len) (>= len 0))
	      (let ((*inspect-skip* len)) 
		(redisplay stream))
	      (output-inspect-note stream "Skip length invalid")))
	(output-inspect-note stream "Skip length missing")))

  (defun istep-cmd-print (option-string stream)
    (if option-string
	(let ((len (read-from-string option-string)))
	  (if (and (integerp len) (plusp len))
	      (setq *inspect-length* len)
	      (output-inspect-note stream "Cannot set print limit to ~A~%" len)))
	(output-inspect-note stream "Print length missing")))

  (defun select-description (select)
    (typecase select
      (integer
       (format nil "which is componenent number ~d of" select))
      (symbol
       (format nil "which is the ~a component of" select))
      (string
       (format nil "which was selected by ~A" select))
      (t
       (write-to-string select))))
  
  (defun istep-cmd-tree (stream)
    (let ((stack (inspect-object-stack *current-inspect*)))
      (if stack
	  (progn
	    (output-inspect-note stream "The current object is:")
	    (dotimes (i (length stack))
	      (output-inspect-note
	       stream "~A, ~A"
	       (inspected-description (nth i stack))
	       (select-description
		(nth i (inspect-select-stack *current-inspect*))))))
	  (no-object-msg stream))))

  (defun istep-cmd-set (id-string value-string stream)
    (if (stack)
	(let ((id (when id-string (read-from-string id-string))))
	  (multiple-value-bind (position parts)
	      (find-part-id (car (stack)) id)
	    (if parts
		(if position
		    (when value-string
		      (let ((new-value (eval (read-from-string value-string))))
			(let ((result (set-component-value (car (stack))
							   id
							   new-value
							   (element-at
							    parts position))))
			  (typecase result
			    (string
			     (output-inspect-note stream result))
			    (t
			     (redisplay stream))))))
		    (output-inspect-note
		     stream
		     "Object has no selectable component named by ~A" id))
		(output-inspect-note stream
				     "Object has no selectable components"))))
	(no-object-msg stream)))

  (defun istep-cmd-select-component (id stream)
    (if (stack)
	(multiple-value-bind (position parts)
	    (find-part-id (car (stack)) id)
	  (cond
	    ((integerp position)
	     (let* ((value (element-at parts position)))
	       (cond ((eq value *inspect-unbound-object-marker*)
		      (output-inspect-note stream "That slot is unbound"))
		     (t
		      (push value (inspect-object-stack *current-inspect*))
		      (push id (inspect-select-stack *current-inspect*))
		      (redisplay stream)))))
	    ((null parts)
	     (output-inspect-note stream "Object does not contain any subobjects"))
	    (t
	     (typecase id
	       (symbol
		(output-inspect-note
		 stream "Object has no selectable component named ~A"
		 id))
	       (integer
		(output-inspect-note
		 stream "Object has no selectable component indexed by ~d"
		 id)
		(output-inspect-note
		 stream "Enter a valid index (~:[0-~W~;0~])"
		 (= (parts-count parts) 1)
		 (1- (parts-count parts))))))))
	(no-object-msg stream)))

  (defun istep-cmd-set-stack (form stream)
    (reset-stack)
    (let ((object (eval form)))
      (setf (inspect-object-stack *current-inspect*) (list object))
      (setf (inspect-select-stack *current-inspect*)
	    (list (format nil ":i ~S" object))))
    (set-break-inspect *current-inspect*)
    (redisplay stream))

  ;;;
  ;;; aclrepl-specific inspection display
  ;;;

  (defun no-object-msg (s)
    (output-inspect-note s "No object is being inspected"))
  
  (defun display-current (s)
    (if (stack)
	(let ((inspected (car (stack))))
	  (setq cl:* inspected)
	  (display-inspect inspected s *inspect-length* *inspect-skip*))
	(no-object-msg)))
  
  ) ;; end binding for multithreading


(defun display-inspect (object stream &optional length skip)
  (multiple-value-bind (elements labels count)
      (inspected-elements object length skip)
    (format stream "~&~A" (inspected-description object))
    (unless (or (characterp object) (typep object 'fixnum))
      (format stream " at #x~X" (sb-kernel:get-lisp-obj-address object)))
    (princ #\newline stream)
    (dotimes (i count)
      (fresh-line stream)
      (display-labeled-element (elt elements i) (elt labels i) stream))))
  
(defun array-label-p (label)
  (and (stringp (cdr label)) (char= (char (cdr label) 0) #\[)))

(defun named-or-array-label-p (label)
  (consp label))

(defun display-labeled-element (element label stream)
  (cond
    ((eq label :ellipses)
     (format stream "   ..."))
    ((eq label :tail)
     (format stream "tail-> ~A" (inspected-description element)))
    ((named-or-array-label-p label)
     (format stream
	     (if (array-label-p label)
		 "~4,' D ~A-> ~A"
		 "~4,' D ~16,1,1,'-A> ~A")
	     (car label)
	     (format nil "~A " (cdr label))
	     (inspected-description element)))
    (t
     (format stream "~4,' D-> ~A" label (inspected-description element)))))

;;; THE BEGINNINGS OF AN INSPECTOR API
;;; which can be used to retrieve object descriptions as component values/labels and also
;;; process print length and skip selectors
;;;
;;; FUNCTIONS TO CONSIDER FOR EXPORT
;;;   FIND-PART-ID
;;;   ELEMENT-AT
;;;   ID-AT
;;;   INSPECTED-ELEMENTS
;;;   INSPECTED-DESCRIPTION
;;;
;;; will also need hooks
;;;    *inspect-start-inspection*
;;;       (maybe. Would setup a window for a GUI inspector)
;;;    *inspect-prompt-fun*
;;;    *inspect-read-cmd*
;;;
;;; and, either an *inspect-process-cmd*, or *inspect-display* hook
;;; That'll depend if choose to have standardized inspector commands such that
;;; (funcall *inspect-read-cmd*) will return a standard command that SBCL will
;;; process and then call the *inspect-display* hook, or if the
;;; *inspect-read-cmd* will return an impl-dependent cmd that sbcl will
;;; send to the contributed inspector for processing and display.

(defun find-part-id (object id)
  "COMPONENT-ID can be an integer or a name of a id.
Returns (VALUES POSITION PARTS).
POSITION is NIL if the id is invalid or not found."
  (let* ((parts (inspected-parts object))
	 (name (when (symbolp id) (symbol-name id) id)))
    (values
     (if (numberp id)
	 (when (< -1 id (parts-count parts)) id)
	 (case (parts-seq-type parts)
	   (:named
	    (position name (the list (parts-components parts))
		      :key #'car :test #'string-equal))
	   (:improper-list
	    (when (string-equal name "tail")
	      (1- (parts-count parts))))))
     parts)))

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

(defun id-at (parts position)
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

(defun inspected-elements (object &optional length (skip 0))
  "Returns elements of an object that have been trimmed and labeled based on
length and skip. Returns (VALUES ELEMENTS LABELS ELEMENT-COUNT)
where ELEMENTS and LABELS are vectors containing ELEMENT-COUNT items.
LABELS may be a string, number, cons pair, :tail, or :ellipses.
This function may return an ELEMENT-COUNT of up to (+ 3 length) which would
include an :ellipses at the beginning, :ellipses at the end,
and the last element."
  (let* ((parts (inspected-parts object))
	 (print-length (if length length (parts-count parts)))
	 (last-part (last-part parts))
	 (last-requested (last-requested parts print-length skip))
	 (element-count (compute-elements-count parts print-length skip))
	 (first-to (if (first-element-ellipses-p parts skip) 1 0))
	 (elements (when (plusp element-count) (make-array element-count)))
	 (labels (when (plusp element-count) (make-array element-count))))
    (when (plusp element-count)
      ;; possible first ellipses
      (when (first-element-ellipses-p parts skip)
	(set-element-values elements labels 0 nil :ellipses))
      ;; main elements
      (do* ((i 0 (1+ i)))
	   ((> i (- last-requested skip)))
	(set-element elements labels parts (+ i first-to) (+ i skip)))
      ;; last parts value if needed
      (when (< last-requested last-part) 
	(set-element elements labels parts (- element-count 1) last-part))
      ;; ending ellipses or next to last parts value if needed
      (when (< last-requested (1- last-part)) 
	(if (= last-requested (- last-part 2))
	    (set-element elements labels parts (- element-count 2) (1- last-part)) 
	    (set-element-values elements labels (- element-count 2) nil :ellipses))))
    (values elements labels element-count)))

(defun last-requested (parts print skip)
  (min (1- (parts-count parts)) (+ skip print -1)))

(defun last-part (parts)
  (1- (parts-count parts)))

(defun compute-elements-count (parts length skip)
  "Compute the number of elements in parts given the print length and skip." 
  (let ((element-count (min (parts-count parts) length
			    (max 0 (- (parts-count parts) skip)))))
    (when (and (plusp (parts-count parts)) (plusp skip)) ; starting ellipses
      (incf element-count))
    (when (< (last-requested parts length skip)
	     (last-part parts)) ; last value
      (incf element-count) 
      (when (< (last-requested parts length skip)
	       (1- (last-part parts))) ; ending ellipses
	(incf element-count)))
    element-count))

(defun set-element (elements labels parts to-index from-index)
  (set-element-values elements labels to-index (element-at parts from-index)
		      (label-at parts from-index)))

(defun set-element-values (elements labels index element label)
  (setf (aref elements index) element)
  (setf (aref labels index) label))

(defun first-element-ellipses-p (parts skip)
  (and (parts-count parts) (plusp skip)))

(defun label-at (parts position)
  "Helper function for inspected-elements. Conses the
position with the label if the label is a string."
  (let ((id (id-at parts position)))
    (if (stringp id)
	(cons position id)
	id)))

(defun array-index-string (index parts)
  "Formats an array index in row major format."
  (let ((rev-dimensions (parts-seq-hint parts)))
    (if (null rev-dimensions)
	"[]"
	(let ((list nil))
	  (dolist (dim rev-dimensions)
	    (multiple-value-bind (q r) (floor index dim)
	      (setq index q)
	      (push r list)))
	  (format nil "[~W~{,~W~}]" (car list) (cdr list))))))


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

(defmethod inspected-description ((object (eql *inspect-unbound-object-marker*)))
  "..unbound..")


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
;;;        with rank >= 2. The 
;;;
;;;   COUNT is the total number of components in the OBJECT
;;;
;;; SEQ-HINT is a seq-type dependent hint. Used by SEQ-TYPE :array
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
	 (list (cons "NAME" (symbol-name object))
	       (cons "PACKAGE" (symbol-package object))
	       (cons "VALUE" (if (boundp object)
				 (symbol-value object)
				 *inspect-unbound-object-marker*))
	       (cons "FUNCTION" (if (fboundp object)
				    (symbol-function object)
				    *inspect-unbound-object-marker*))
	       (cons "PLIST" (symbol-plist object)))))
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
  (let ((components nil)
	(class-slots (sb-pcl::class-slots (class-of object))))
    (dolist (class-slot class-slots components)
      (let* ((slot-name (slot-value class-slot 'sb-pcl::name))
	     (slot-value (if (slot-boundp object slot-name)
			     (slot-value object slot-name)
			     *inspect-unbound-object-marker*)))
	(push (cons (symbol-name slot-name) slot-value) components)))))


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

(defmethod set-component-value ((object t) id value element)
  (format nil "Object does not support setting of component ~A" id))

