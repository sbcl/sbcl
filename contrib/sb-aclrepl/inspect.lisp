;;;; Inspector for sb-aclrepl
;;;;
;;;; The documentation, which may or may not apply in its entirety at
;;;; any given time, for this functionality is on the ACL website:
;;;;   <http://www.franz.com/support/documentation/6.2/doc/inspector.htm>.
;;;;
;;;; A summary of inspector navigation is contained in the below *INSPECT-HELP*
;;;; variable.

(cl:in-package :sb-aclrepl)

(defparameter *inspect-stack* nil
  "Stack of the hierarchy of an inspected object.")

(defparameter *parent-select-stack* nil
  "a stack of the indices of parent object components that brought us to the current object.")

(defparameter *inspect-length* 10
  "Number of components to display.")

;; FIXME - raw mode isn't currently used in object display
(defparameter *inspect-raw* nil
  "Raw mode for object display.")

(defvar *inspected*)
(setf (documentation '*inspected* 'variable)
      "the value currently being inspected by CL:INSPECT")

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
:i set <name> <form>  set named component to evalated form
:i print <max> set the maximum number of components to print
:i skip <n>    skip a number of components when printing
:i tree        print inspect stack
")

;;; When *INSPECT-UNBOUND-OBJECT-MARKER* occurs in a parts list, it
;;; indicates that that a slot is unbound.
(defvar *inspect-unbound-object-marker* (gensym "INSPECT-UNBOUND-OBJECT-"))


;; Setup binding for multithreading
(let ((*inspect-stack* nil)
      (*parent-select-stack* nil)
      (*inspect-length* 10)
      (*inspect-raw* nil)
      (*inspected* nil))
  
(defun inspector (object input-stream output-stream)
  (declare (ignore input-stream))
  (setq object (eval object))
  (reset-stack)
  (setq *inspect-stack* (list object))
  (setq *parent-select-stack* (list "(inspect ...)"))
  (%inspect output-stream))

 

(defun istep (arg-string output-stream)
  (%istep arg-string (string-to-list-skip-spaces arg-string) output-stream))

(setq sb-impl::*inspect-fun* #'inspector)

(defun reset-stack ()
  (setq *inspect-stack* nil)
  (setq *parent-select-stack* nil)
  (makunbound '*inspected*))

(defun %istep (arg-string args output-stream)
  (let* ((option (car args))
	 (option-read (when arg-string
			(read-from-string arg-string))))
    (cond
      ;; Redisplay
      ((or (string= "=" option)
	   (zerop (length args)))
       (%inspect output-stream))
      ;; Select parent
      ((or (string= "-" option)
	   (string= "^" option))
       (cond
	 ((> (length *inspect-stack*) 1)
	  (pop *inspect-stack*)
	  (%inspect output-stream))
	 (*inspect-stack*
	   (format output-stream "Object has no parent.~%"))
	 (t
	  (%inspect output-stream))))
      ;; Select * to inspect
      ((string= "*" option)
       (reset-stack) 
       (setq *inspect-stack* (list *))
       (setq *parent-select-stack* (list "(inspect ...)"))
       (%inspect output-stream))
      ;; Start new inspect level for eval'd form
      ((string= "+" option)
       (inspector (eval (second args)) nil output-stream))
      ;; Next or previous parent component
      ((or (string= "<" option)
	   (string= ">" option))
       (if *inspect-stack*
	   (if (eq (length *inspect-stack*) 1)
	       (format output-stream "Object does not have a parent")
	       (let ((parent (second *inspect-stack*))
		     (id (car *parent-select-stack*)))
		 (multiple-value-bind (position list-type elements)
		     (find-object-component parent id)
		   (declare (list elements)
			    (ignore list-type))
		   (let ((new-position (if (string= ">" option)
					   (1+ position)
					   (1- position))))
		     (if (< -1 new-position (length elements))
			 (let ((new-object (elt elements new-position)))
			   (setf (car *inspect-stack*) new-object)
			   (setf (car *parent-select-stack*)
				 (if (integerp id)
				     new-position
				     (read-from-string
				      (car (nth new-position elements)))))
			   (%inspect output-stream))
			 (format output-stream "Parent has no selectable component indexed by ~d"
				 new-position))))))
	   (%inspect output-stream)))
      ;; Set component to eval'd form
      ((string-equal "set" option)
       (if *inspect-stack*
	   (let ((id (when (second args)
			 (read-from-string (second args)))))
	     (multiple-value-bind (position list-type elements)
		 (find-object-component (car *inspect-stack*) id)
	       (declare (ignore list-type))
	       (if elements
		   (if position
		       (let ((value-stirng (third args)))
			 (when value-stirng
			   (let ((new-value (eval (read-from-string (third args)))))
			     (let ((result 
				    (set-component-value (car *inspect-stack*)
							 id
							 new-value
							 (nth position elements))))
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
       (when *inspect-stack*
	 (when (and (second args)
		    (or (null (second args))
			(eq (read-from-string (second args)) t)))
	   (setq *inspect-raw* t))
	 (%inspect output-stream)))
      ;; Reset stack
      ((string-equal "q" option)
       (reset-stack))
      ;; Display help
      ((string-equal "?" option)
       (format output-stream *inspect-help*))
      ;; Set number of components to skip
      ((string-equal "skip" option)
       (let ((len (read-from-string (second args))))
	 (if (and (integerp len) (>= len 0))
	     (%inspect output-stream len)
	     (format output-stream "Skip missing or invalid~%"))))
      ;; Print stack tree
      ((string-equal "tree" option)
       (if *inspect-stack*
	   (progn
	     (format output-stream "The current object is:~%")
	     (dotimes (i (length *inspect-stack*))
	       (format output-stream "~A, ~A~%"
		       (inspected-parts (nth i *inspect-stack*) :description t)
		       (let ((select (nth i *parent-select-stack*)))
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
       (if *inspect-stack*
	   (multiple-value-bind (position list-type elements)
	       (find-object-component (car *inspect-stack*) option-read)
	     (cond
	       ((integerp position)
		(let* ((element (elt elements position))
		       (value (if (eq list-type :named) (cdr element) element)))
		  (cond ((eq value *inspect-unbound-object-marker*)
			 (format output-stream "That slot is unbound~%"))
			(t
			 (push value *inspect-stack*)
			 (push option-read *parent-select-stack*)
			 (%inspect output-stream)))))
	       ((null elements)
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
			   (= (length elements) 1)
			   (1- (length elements))))))))
	   (%inspect output-stream)))
      ;; Default is to select eval'd form
      (t
       (reset-stack)
       (setq *inspect-stack* (list (eval option-read)))
       (setq *parent-select-stack* (list ":i <form>"))
       (%inspect output-stream))
      )))

(defun find-object-component (object id)
  "COMPONENT-ID can be an integer or a name of a id.
Returns POSITION LIST-TYPE ELEMENTS
POSITION is NIL if the id is invalid or not found."
  (if object
      (multiple-value-bind (description list-type elements)
	  (inspected-parts object)
	(declare (ignore description)
		 (list elements))
	(when (symbolp id)
	  (setq id (symbol-name id)))
	(let ((position
	       (cond ((and (eq list-type :named)
			   (stringp id))
		      (position id elements :key #'car :test #'string-equal))
		     ((numberp id)
		      (when (< -1 id (length elements))
			id)))))
	  (values position list-type elements)))
      (values nil nil nil)))


(defun %inspect (s &optional (skip 0))
  (if *inspect-stack*
      (progn
	(setq *inspected* (car *inspect-stack*))
	(setq cl:* *inspected*)
	(multiple-value-bind (description list-type elements) (inspected-parts *inspected*)
	  (display-inspected-parts *inspected* description list-type elements s skip)))
      (format s "No object is being inspected")))


(defun display-inspected-parts (object description list-type elements stream &optional (skip 0))
  (format stream "~&~A" description)
  (unless (or (characterp object) (typep object 'fixnum))
    (format stream " at #x~X" (sb-kernel:get-lisp-obj-address object)))
  (princ #\newline stream)
  (when elements
    (do* ((index skip (1+ index))
	  (nelem (length elements))
	  (max (min (1- nelem) (+ skip *inspect-length*)))
	  (suspension (when (plusp (- nelem max))
			(- nelem max)))
	  (count (if (typep elements 'sequence)
		     (length elements)
		     0))
	  (element))
	 ((> index max))
      (declare (ignore suspension)) ;; FIXME - not yet implemented
      (setq element (elt elements index))
      (cond
	((eq list-type :index-with-tail)
	 (if (eql index (- count 1))
	   (format stream "~4,' D ~A~%" "tail" (inspected-parts element :description t))
	   (format stream "~4,' D ~A~%" index (inspected-parts element :description t))))
	((eq list-type :named)
	 (destructuring-bind (name . value) element
	   (format stream "~4,' D ~16,1,1,'-A> ~A~%" index (format nil "~A "  name)
		 (if (eq value *inspect-unbound-object-marker*)
		     "..unbound.."
		     (inspected-parts value :description t)))))
	(t
	 (format stream "~4,' D ~A~%" index (inspected-parts element :description t)))))))

) ;; end binding for multithreading



;;; INSPECTED-PARTS
;;;
;;; Destructure an object for inspection, returning either
;;;   DESCRIPTION
;;; if description keyword is T, otherwise returns
;;;   (VALUES DESCRIPTION LIST-TYPE ELEMENTS),
;;; where..
;;;
;;;   DESCRIPTION is a summary description of the destructured object,
;;;   e.g. "the object is a CONS.~%".
;;;
;;;   LIST-TYPE determines what representation is used for elements
;;;   of ELEMENTS.
;;;      If LIST-TYPE is :named, then each element is (CONS NAME VALUE)
;;;      If LIST-TYPE is :index-with-tail, then each element is just value,
;;;        but the last element is label as "tail"
;;;      If LIST-TYPE is :long, then each element is just value,
;;;        and suspension points ('...) are shown before the last element.
;;;      Otherwise, each element is just VALUE.
;;;
;;;   ELEMENTS is a list of the component parts of OBJECT (whose
;;;   representation is determined by LIST-TYPE).
;;;
;;; (LIST-TYPE is useful because symbols and instances
;;; need to display both a slot name and a value, while lists and
;;; vectors need only display a value.)

(defgeneric inspected-parts (object &key description))

(defmethod inspected-parts ((object symbol) &key description)
  (let ((desc (format nil "the symbol ~A" object (sb-kernel:get-lisp-obj-address object))))
    (if description
	desc
	(values desc :named
		(list (cons "name" (symbol-name object))
		      (cons "package" (symbol-package object))
		      (cons "value" (if (boundp object)
					(symbol-value object)
					*inspect-unbound-object-marker*))
		      (cons "function" (if (fboundp object)
					   (symbol-function object)
					   *inspect-unbound-object-marker*))
		      (cons "plist" (symbol-plist object)))))))
    
(defun inspected-structure-elements (object)
  (let ((parts-list '())
        (info (sb-kernel:layout-info (sb-kernel:layout-of object))))
    (when (sb-kernel::defstruct-description-p info)
      (dolist (dd-slot (sb-kernel:dd-slots info) (nreverse parts-list))
        (push (cons (sb-kernel:dsd-%name dd-slot)
                    (funcall (sb-kernel:dsd-accessor-name dd-slot) object))
              parts-list)))))

(defmethod inspected-parts ((object structure-object) &key description)
  (let ((desc (format nil "~W" (find-class (type-of object)))))
    (if description
	desc
	(values desc :named (inspected-structure-elements object)))))

(defmethod inspected-parts ((object package) &key description)
  (let ((desc (format nil "the ~A package" (package-name object))))
    (if description
	desc
	(values desc :named (inspected-structure-elements object)))))

(defun inspected-standard-object-elements (object)
  (let ((reversed-elements nil)
	(class-slots (sb-pcl::class-slots (class-of object))))
    (dolist (class-slot class-slots (nreverse reversed-elements))
      (let* ((slot-name (slot-value class-slot 'sb-pcl::name))
	     (slot-value (if (slot-boundp object slot-name)
			     (slot-value object slot-name)
			     *inspect-unbound-object-marker*)))
	(push (cons slot-name slot-value) reversed-elements)))))

(defmethod inspected-parts ((object standard-object) &key description)
  (let ((desc (format nil "~W" (class-of object))))
    (if description
	desc
	(values desc :named
		(inspected-standard-object-elements object)))))

(defmethod inspected-parts ((object sb-kernel:funcallable-instance) &key description)
  (let ((desc (format nil "a funcallable-instance of type ~S"
		      (type-of object))))
    (if description
	desc
	(values desc :named
		(inspected-structure-elements object)))))

(defmethod inspected-parts ((object function) &key description)
  (let* ((type (sb-kernel:widetag-of object))
	 (object (if (= type sb-vm:closure-header-widetag)
		     (sb-kernel:%closure-fun object)
		     object))
	 (desc (format nil "~S" object)))
    (if description
	desc
	(values desc :named
		(list (cons "arglist" (sb-kernel:%simple-fun-arglist object)))))))

(defmethod inspected-parts ((object vector) &key description)
  (let ((desc  (format nil
		  "a ~:[~;displaced ~]vector (~W)"
		  (and (sb-kernel:array-header-p object)
		       (sb-kernel:%array-displaced-p object))
		  (length object)
		  (sb-kernel:get-lisp-obj-address object))))
    (if description
	desc
	(values desc nil object))))

(defun inspected-index-string (index rev-dimensions)
  (if (null rev-dimensions)
      "[]"
      (let ((list nil))
	(dolist (dim rev-dimensions)
	  (multiple-value-bind (q r) (floor index dim)
	    (setq index q)
 	    (push r list)))
	(format nil "[~W~{,~W~}]" (car list) (cdr list)))))

(defmethod inspected-parts ((object simple-vector) &key description)
  (let ((desc (format nil "a simple ~A vector (~D)"
		      (array-element-type object)
		      (length object))))
    (if description
	desc
	(values desc nil object))))

(defmethod inspected-parts ((object array) &key description)
  (declare (array object))
  (let* ((length (array-total-size object))
	 (reference-array (make-array length :displaced-to object))
	 (dimensions (array-dimensions object))
	 (reversed-elements nil)
	 (desc (format nil "~:[A displaced~;An~] array of ~A with dimensions ~W"
		       (and (sb-kernel:array-header-p object)
			    (sb-kernel:%array-displaced-p object))
		       (array-element-type object)
		       dimensions)))
    (if description
	desc
	(progn
	  (dotimes (i length)
	    (push (cons (format nil "~A "
				(inspected-index-string i (reverse dimensions)))
			(aref reference-array i))
		  reversed-elements))
	  (values desc :named (nreverse reversed-elements))))))

(defmethod inspected-parts ((object cons) &key description)
  (if (or (consp (cdr object)) (null (cdr object)))
      (inspected-parts-of-nontrivial-list object description)
      (inspected-parts-of-simple-cons object description)))

(defun inspected-parts-of-simple-cons (object description)
  (let ((desc (format nil "a cons pair")))
    (if description
	desc
	(values desc :named
		(list (cons "car" (car object))
		      (cons "cdr" (cdr object)))))))

(defun inspected-parts-of-nontrivial-list (object description)
  (let ((length 0)
	(in-list object)
	(reversed-elements nil))
    (flet ((done (description-format list-type)
	     (let ((desc (format nil description-format length length)))
	       (return-from inspected-parts-of-nontrivial-list
		 (if description
		     desc
		     (values desc list-type (nreverse reversed-elements)))))))
      (loop
       (cond ((null in-list)
	      (done "a proper list with ~D element~P" nil))
	     ((consp in-list)
	      (push (pop in-list) reversed-elements)
	      (incf length))
	     (t
	      (push in-list reversed-elements)
	      (done "a improper list with ~D element~P" :index-with-tail)))))))

(defmethod inspected-parts ((object simple-string) &key description)
  (let ((desc (format nil "a simple-string (~W) ~W" (length object) object)))
    (if description
	desc
	(values desc nil object))))

(defmethod inspected-parts ((object double-float) &key description)
  (let ((desc (format nil "double-float ~W" object)))
    (if description
	desc
	(values desc nil nil))))

(defmethod inspected-parts ((object single-float) &key description)
  (let ((desc (format nil "single-float ~W" object)))
    (if description
	desc
	(values desc nil nil))))

(defmethod inspected-parts ((object fixnum) &key description)
  (let ((desc (format nil "fixnum ~W" object)))
    (if description
	desc
	(values desc nil nil))))

(defmethod inspected-parts ((object complex) &key description)
  (let ((desc (format nil "complex number ~W" object)))
    (if description
	desc
	(values desc :named
		(list (cons "real" (realpart object))
		      (cons "imag" (imagpart object)))))))

(defmethod inspected-parts ((object bignum) &key description)
  (let ((desc (format nil "bignum ~W" object)))
    (if description
	desc
	(values desc nil nil))))

(defmethod inspected-parts ((object ratio) &key description)
  (let ((desc (format nil "ratio ~W" object)))
    (if description
	desc
	(values desc :named
		(list (cons "numerator" (numerator object))
		      (cons "denominator" (denominator object)))))))

(defmethod inspected-parts ((object character) &key description)
  (let ((desc (format nil "character ~W char-code #x~X" object (char-code object))))
    (if description
	desc
	(values desc nil nil))))

(defmethod inspected-parts ((object t) &key description)
  (let ((desc (format nil "a generic object ~W" object)))
    (if description
	desc
	(values desc nil nil))))

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

