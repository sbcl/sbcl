;;;; stuff originally from CMU CL's error.lisp which can or should
;;;; come late (mostly related to the CONDITION class itself)
;;;;

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;;; the CONDITION class

(/show0 "condition.lisp 20")

(eval-when (:compile-toplevel :load-toplevel :execute)

(/show0 "condition.lisp 24")

(def!struct (condition-class (:include slot-class)
			     (:constructor bare-make-condition-class))
  ;; list of CONDITION-SLOT structures for the direct slots of this
  ;; class
  (slots nil :type list)
  ;; list of CONDITION-SLOT structures for all of the effective class
  ;; slots of this class
  (class-slots nil :type list)
  ;; report function or NIL
  (report nil :type (or function null))
  ;; list of alternating initargs and initforms
  (default-initargs () :type list)
  ;; class precedence list as a list of CLASS objects, with all
  ;; non-CONDITION classes removed
  (cpl () :type list)
  ;; a list of all the effective instance allocation slots of this
  ;; class that have a non-constant initform or default-initarg.
  ;; Values for these slots must be computed in the dynamic
  ;; environment of MAKE-CONDITION.
  (hairy-slots nil :type list))

(/show0 "condition.lisp 49")

(defun make-condition-class (&rest rest)
  (apply #'bare-make-condition-class
	 (rename-key-args '((:name :%name)) rest)))

(/show0 "condition.lisp 53")

) ; EVAL-WHEN

(!defstruct-with-alternate-metaclass condition
  :slot-names (actual-initargs assigned-slots)
  :boa-constructor %make-condition-object
  :superclass-name instance
  :metaclass-name condition-class
  :metaclass-constructor make-condition-class
  :dd-type structure)

(defun make-condition-object (actual-initargs)
  (%make-condition-object actual-initargs nil))

(defstruct (condition-slot (:copier nil))
  (name (missing-arg) :type symbol)
  ;; list of all applicable initargs
  (initargs (missing-arg) :type list)
  ;; names of reader and writer functions
  (readers (missing-arg) :type list)
  (writers (missing-arg) :type list)
  ;; true if :INITFORM was specified
  (initform-p (missing-arg) :type (member t nil))
  ;; If this is a function, call it with no args. Otherwise, it's the
  ;; actual value.
  (initform (missing-arg) :type t)
  ;; allocation of this slot, or NIL until defaulted
  (allocation nil :type (member :instance :class nil))
  ;; If ALLOCATION is :CLASS, this is a cons whose car holds the value.
  (cell nil :type (or cons null)))

;;; KLUDGE: It's not clear to me why CONDITION-CLASS has itself listed
;;; in its CPL, while other classes derived from CONDITION-CLASS don't
;;; have themselves listed in their CPLs. This behavior is inherited
;;; from CMU CL, and didn't seem to be explained there, and I haven't
;;; figured out whether it's right. -- WHN 19990612
(eval-when (:compile-toplevel :load-toplevel :execute)
  (/show0 "condition.lisp 103")
  (let ((condition-class (locally
			   ;; KLUDGE: There's a DEFTRANSFORM FIND-CLASS for
			   ;; constant class names which creates fast but
			   ;; non-cold-loadable, non-compact code. In this
			   ;; context, we'd rather have compact, cold-loadable
			   ;; code. -- WHN 19990928
			   (declare (notinline sb!xc:find-class))
			   (sb!xc:find-class 'condition))))
    (setf (condition-class-cpl condition-class)
	  (list condition-class)))
  (/show0 "condition.lisp 103"))

(setf (condition-class-report (locally
				;; KLUDGE: There's a DEFTRANSFORM FIND-CLASS 
				;; for constant class names which creates fast
				;; but non-cold-loadable, non-compact code. In
				;; this context, we'd rather have compact,
				;; cold-loadable code. -- WHN 19990928
				(declare (notinline sb!xc:find-class))
				(find-class 'condition)))
      (lambda (cond stream)
	(format stream "Condition ~S was signalled." (type-of cond))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun find-condition-layout (name parent-types)
  (let* ((cpl (remove-duplicates
	       (reverse
		(reduce #'append
			(mapcar (lambda (x)
				  (condition-class-cpl
				   (sb!xc:find-class x)))
				parent-types)))))
	 (cond-layout (info :type :compiler-layout 'condition))
	 (olayout (info :type :compiler-layout name))
	 ;; FIXME: Does this do the right thing in case of multiple
	 ;; inheritance? A quick look at DEFINE-CONDITION didn't make
	 ;; it obvious what ANSI intends to be done in the case of
	 ;; multiple inheritance, so it's not actually clear what the
	 ;; right thing is..
	 (new-inherits
	  (order-layout-inherits (concatenate 'simple-vector
					      (layout-inherits cond-layout)
					      (mapcar #'class-layout cpl)))))
    (if (and olayout
	     (not (mismatch (layout-inherits olayout) new-inherits)))
	olayout
	(make-layout :class (make-undefined-class name)
		     :inherits new-inherits
		     :depthoid -1
		     :length (layout-length cond-layout)))))

) ; EVAL-WHEN

;;; FIXME: ANSI's definition of DEFINE-CONDITION says
;;;   Condition reporting is mediated through the PRINT-OBJECT method
;;;   for the condition type in question, with *PRINT-ESCAPE* always
;;;   being nil. Specifying (:REPORT REPORT-NAME) in the definition of
;;;   a condition type C is equivalent to:
;;;     (defmethod print-object ((x c) stream)
;;;       (if *print-escape* (call-next-method) (report-name x stream)))
;;; The current code doesn't seem to quite match that.
(def!method print-object ((x condition) stream)
  (if *print-escape*
      (print-unreadable-object (x stream :type t :identity t))
      ;; KLUDGE: A comment from CMU CL here said
      ;;   7/13/98 BUG? CPL is not sorted and results here depend on order of
      ;;   superclasses in define-condition call!
      (dolist (class (condition-class-cpl (sb!xc:class-of x))
		     (error "no REPORT? shouldn't happen!"))
	(let ((report (condition-class-report class)))
	  (when report
	    (return (funcall report x stream)))))))

;;;; slots of CONDITION objects

(defvar *empty-condition-slot* '(empty))

(defun find-slot-default (class slot)
  (let ((initargs (condition-slot-initargs slot))
	(cpl (condition-class-cpl class)))
    (dolist (class cpl)
      (let ((default-initargs (condition-class-default-initargs class)))
	(dolist (initarg initargs)
	  (let ((val (getf default-initargs initarg *empty-condition-slot*)))
	    (unless (eq val *empty-condition-slot*)
	      (return-from find-slot-default
			   (if (functionp val)
			       (funcall val)
			       val)))))))

    (if (condition-slot-initform-p slot)
	(let ((initform (condition-slot-initform slot)))
	  (if (functionp initform)
	      (funcall initform)
	      initform))
	(error "unbound condition slot: ~S" (condition-slot-name slot)))))

(defun find-condition-class-slot (condition-class slot-name)
  (dolist (sclass
	   (condition-class-cpl condition-class)
	   (error "There is no slot named ~S in ~S."
		  slot-name condition-class))
    (dolist (slot (condition-class-slots sclass))
      (when (eq (condition-slot-name slot) slot-name)
	(return-from find-condition-class-slot slot)))))

(defun condition-writer-function (condition new-value name)
  (dolist (cslot (condition-class-class-slots
		  (layout-class (%instance-layout condition)))
		 (setf (getf (condition-assigned-slots condition) name)
		       new-value))
    (when (eq (condition-slot-name cslot) name)
      (return (setf (car (condition-slot-cell cslot)) new-value)))))

(defun condition-reader-function (condition name)
  (let ((class (layout-class (%instance-layout condition))))
    (dolist (cslot (condition-class-class-slots class))
      (when (eq (condition-slot-name cslot) name)
	(return-from condition-reader-function
		     (car (condition-slot-cell cslot)))))

    (let ((val (getf (condition-assigned-slots condition) name
		     *empty-condition-slot*)))
      (if (eq val *empty-condition-slot*)
	  (let ((actual-initargs (condition-actual-initargs condition))
		(slot (find-condition-class-slot class name)))
            (unless slot
	      (error "missing slot ~S of ~S" name condition))
	    (dolist (initarg (condition-slot-initargs slot))
	      (let ((val (getf actual-initargs
			       initarg
			       *empty-condition-slot*)))
		(unless (eq val *empty-condition-slot*)
		  (return-from condition-reader-function
			       (setf (getf (condition-assigned-slots condition)
					   name)
				     val)))))
	    (setf (getf (condition-assigned-slots condition) name)
		  (find-slot-default class slot)))
	  val))))

;;;; MAKE-CONDITION

(defun make-condition (thing &rest args)
  #!+sb-doc
  "Make an instance of a condition object using the specified initargs."
  ;; Note: ANSI specifies no exceptional situations in this function.
  ;; signalling simple-type-error would not be wrong.
  (let* ((thing (if (symbolp thing)
		    (sb!xc:find-class thing)
		    thing))
	 (class (typecase thing
		  (condition-class thing)
		  (class
		   (error 'simple-type-error
			  :datum thing
			  :expected-type 'condition-class
			  :format-control "~S is not a condition class."
			  :format-arguments (list thing)))
		  (t
		   (error 'simple-type-error
			  :datum thing
			  :expected-type 'condition-class
			  :format-control "bad thing for class argument:~%  ~S"
			  :format-arguments (list thing)))))
	 (res (make-condition-object args)))
    (setf (%instance-layout res) (class-layout class))
    ;; Set any class slots with initargs present in this call.
    (dolist (cslot (condition-class-class-slots class))
      (dolist (initarg (condition-slot-initargs cslot))
	(let ((val (getf args initarg *empty-condition-slot*)))
	  (unless (eq val *empty-condition-slot*)
	    (setf (car (condition-slot-cell cslot)) val)))))
    ;; Default any slots with non-constant defaults now.
    (dolist (hslot (condition-class-hairy-slots class))
      (when (dolist (initarg (condition-slot-initargs hslot) t)
	      (unless (eq (getf args initarg *empty-condition-slot*)
			  *empty-condition-slot*)
		(return nil)))
	(setf (getf (condition-assigned-slots res) (condition-slot-name hslot))
	      (find-slot-default class hslot))))

    res))

;;;; DEFINE-CONDITION

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %compiler-define-condition (name direct-supers layout)
  (multiple-value-bind (class old-layout)
      (insured-find-class name #'condition-class-p #'make-condition-class)
    (setf (layout-class layout) class)
    (setf (class-direct-superclasses class)
	  (mapcar #'sb!xc:find-class direct-supers))
    (cond ((not old-layout)
	   (register-layout layout))
	  ((not *type-system-initialized*)
	   (setf (layout-class old-layout) class)
	   (setq layout old-layout)
	   (unless (eq (class-layout class) layout)
	     (register-layout layout)))
	  ((redefine-layout-warning "current"
				    old-layout
				    "new"
				    (layout-length layout)
				    (layout-inherits layout)
				    (layout-depthoid layout))
	   (register-layout layout :invalidate t))
	  ((not (class-layout class))
	   (register-layout layout)))

    (setf (layout-info layout)
	  (locally
	    ;; KLUDGE: There's a FIND-CLASS DEFTRANSFORM for constant class
	    ;; names which creates fast but non-cold-loadable, non-compact
	    ;; code. In this context, we'd rather have compact, cold-loadable
	    ;; code. -- WHN 19990928
	    (declare (notinline sb!xc:find-class))
	    (layout-info (class-layout (sb!xc:find-class 'condition)))))

    (setf (sb!xc:find-class name) class)

    ;; Initialize CPL slot.
    (setf (condition-class-cpl class)
	  (remove-if-not #'condition-class-p 
			 (std-compute-class-precedence-list class))))
  (values))

) ; EVAL-WHEN

;;; Compute the effective slots of CLASS, copying inherited slots and
;;; destructively modifying direct slots.
;;;
;;; FIXME: It'd be nice to explain why it's OK to destructively modify
;;; direct slots. Presumably it follows from the semantics of
;;; inheritance and redefinition of conditions, but finding the cite
;;; and documenting it here would be good. (Or, if this is not in fact
;;; ANSI-compliant, fixing it would also be good.:-)
(defun compute-effective-slots (class)
  (collect ((res (copy-list (condition-class-slots class))))
    (dolist (sclass (condition-class-cpl class))
      (dolist (sslot (condition-class-slots sclass))
	(let ((found (find (condition-slot-name sslot) (res))))
	  (cond (found
		 (setf (condition-slot-initargs found)
		       (union (condition-slot-initargs found)
			      (condition-slot-initargs sslot)))
		 (unless (condition-slot-initform-p found)
		   (setf (condition-slot-initform-p found)
			 (condition-slot-initform-p sslot))
		   (setf (condition-slot-initform found)
			 (condition-slot-initform sslot)))
		 (unless (condition-slot-allocation found)
		   (setf (condition-slot-allocation found)
			 (condition-slot-allocation sslot))))
		(t
		 (res (copy-structure sslot)))))))
    (res)))

(defun %define-condition (name slots documentation report default-initargs)
  (let ((class (sb!xc:find-class name)))
    (setf (condition-class-slots class) slots)
    (setf (condition-class-report class) report)
    (setf (condition-class-default-initargs class) default-initargs)
    (setf (fdocumentation name 'type) documentation)

    (dolist (slot slots)

      ;; Set up reader and writer functions.
      (let ((name (condition-slot-name slot)))
	(dolist (reader (condition-slot-readers slot))
	  (setf (fdefinition reader)
		(lambda (condition)
		  (condition-reader-function condition name))))
	(dolist (writer (condition-slot-writers slot))
	  (setf (fdefinition writer)
		(lambda (new-value condition)
		  (condition-writer-function condition new-value name))))))

    ;; Compute effective slots and set up the class and hairy slots
    ;; (subsets of the effective slots.)
    (let ((eslots (compute-effective-slots class))
	  (e-def-initargs
	   (reduce #'append
		   (mapcar #'condition-class-default-initargs
			   (condition-class-cpl class)))))
      (dolist (slot eslots)
	(ecase (condition-slot-allocation slot)
	  (:class
	   (unless (condition-slot-cell slot)
	     (setf (condition-slot-cell slot)
		   (list (if (condition-slot-initform-p slot)
			     (let ((initform (condition-slot-initform slot)))
			       (if (functionp initform)
				   (funcall initform)
				   initform))
			     *empty-condition-slot*))))
	   (push slot (condition-class-class-slots class)))
	  ((:instance nil)
	   (setf (condition-slot-allocation slot) :instance)
	   (when (or (functionp (condition-slot-initform slot))
		     (dolist (initarg (condition-slot-initargs slot) nil)
		       (when (functionp (getf e-def-initargs initarg))
			 (return t))))
	     (push slot (condition-class-hairy-slots class))))))))
  name)

(defmacro define-condition (name (&rest parent-types) (&rest slot-specs)
				 &body options)
  #!+sb-doc
  "DEFINE-CONDITION Name (Parent-Type*) (Slot-Spec*) Option*
   Define NAME as a condition type. This new type inherits slots and its
   report function from the specified PARENT-TYPEs. A slot spec is a list of:
     (slot-name :reader <rname> :initarg <iname> {Option Value}*

   The DEFINE-CLASS slot options :ALLOCATION, :INITFORM, [slot] :DOCUMENTATION
   and :TYPE and the overall options :DEFAULT-INITARGS and
   [type] :DOCUMENTATION are also allowed.

   The :REPORT option is peculiar to DEFINE-CONDITION. Its argument is either
   a string or a two-argument lambda or function name. If a function, the
   function is called with the condition and stream to report the condition.
   If a string, the string is printed.

   Condition types are classes, but (as allowed by ANSI and not as described in
   CLtL2) are neither STANDARD-OBJECTs nor STRUCTURE-OBJECTs. WITH-SLOTS and
   SLOT-VALUE may not be used on condition objects."
  (let* ((parent-types (or parent-types '(condition)))
	 (layout (find-condition-layout name parent-types))
	 (documentation nil)
	 (report nil)
	 (default-initargs ()))
    (collect ((slots)
	      (all-readers nil append)
	      (all-writers nil append))
      (dolist (spec slot-specs)
	(when (keywordp spec)
	  (warn "Keyword slot name indicates probable syntax error:~%  ~S"
		spec))
	(let* ((spec (if (consp spec) spec (list spec)))
	       (slot-name (first spec))
	       (allocation :instance)
	       (initform-p nil)
	       initform)
	  (collect ((initargs)
		    (readers)
		    (writers))
	    (do ((options (rest spec) (cddr options)))
		((null options))
	      (unless (and (consp options) (consp (cdr options)))
		(error "malformed condition slot spec:~%  ~S." spec))
	      (let ((arg (second options)))
		(case (first options)
		  (:reader (readers arg))
		  (:writer (writers arg))
		  (:accessor
		   (readers arg)
		   (writers `(setf ,arg)))
		  (:initform
		   (when initform-p
		     (error "more than one :INITFORM in ~S" spec))
		   (setq initform-p t)
		   (setq initform arg))
		  (:initarg (initargs arg))
		  (:allocation
		   (setq allocation arg))
		  (:type)
		  (t
		   (error "unknown slot option:~%  ~S" (first options))))))

	    (all-readers (readers))
	    (all-writers (writers))
	    (slots `(make-condition-slot
		     :name ',slot-name
		     :initargs ',(initargs)
		     :readers ',(readers)
		     :writers ',(writers)
		     :initform-p ',initform-p
		     :initform
		     ,(if (constantp initform)
			  `',(eval initform)
			  `#'(lambda () ,initform)))))))

      (dolist (option options)
	(unless (consp option)
	  (error "bad option:~%  ~S" option))
	(case (first option)
	  (:documentation (setq documentation (second option)))
	  (:report
	   (let ((arg (second option)))
	     (setq report
		   (if (stringp arg)
		       `#'(lambda (condition stream)
			  (declare (ignore condition))
			  (write-string ,arg stream))
		       `#'(lambda (condition stream)
			  (funcall #',arg condition stream))))))
	  (:default-initargs
	   (do ((initargs (rest option) (cddr initargs)))
	       ((endp initargs))
	     (let ((val (second initargs)))
	       (setq default-initargs
		     (list* `',(first initargs)
			    (if (constantp val)
				`',(eval val)
				`#'(lambda () ,val))
			    default-initargs)))))
	  (t
	   (error "unknown option: ~S" (first option)))))

      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (%compiler-define-condition ',name ',parent-types ',layout))

	 (declaim (ftype (function (t) t) ,@(all-readers)))
	 (declaim (ftype (function (t t) t) ,@(all-writers)))

	 (%define-condition ',name
			    (list ,@(slots))
			    ,documentation
			    ,report
			    (list ,@default-initargs))))))

;;;; DESCRIBE on CONDITIONs

;;; a function to be used as the guts of DESCRIBE-OBJECT (CONDITION T)
;;; eventually (once we get CLOS up and running so that we can define
;;; methods)
(defun describe-condition (condition stream)
  (format stream
	  "~@<~S ~_is a ~S. ~_Its slot values are ~_~S.~:>"
	  condition
	  (type-of condition)
	  (concatenate 'list
		       (condition-actual-initargs condition)
		       (condition-assigned-slots condition))))

;;;; various CONDITIONs specified by ANSI

(define-condition serious-condition (condition) ())

(define-condition error (serious-condition) ())

(define-condition warning (condition) ())
(define-condition style-warning (warning) ())

(defun simple-condition-printer (condition stream)
  (apply #'format
	 stream
	 (simple-condition-format-control condition)
	 (simple-condition-format-arguments condition)))

(define-condition simple-condition ()
  ((format-control :reader simple-condition-format-control
		   :initarg :format-control)
   (format-arguments :reader simple-condition-format-arguments
		     :initarg :format-arguments
		     :initform '()))
  (:report simple-condition-printer))

(define-condition simple-warning (simple-condition warning) ())

(define-condition simple-error (simple-condition error) ())

(define-condition storage-condition (serious-condition) ())

(define-condition type-error (error)
  ((datum :reader type-error-datum :initarg :datum)
   (expected-type :reader type-error-expected-type :initarg :expected-type))
  (:report
   (lambda (condition stream)
     (format stream
	     "~@<The value ~2I~:_~S ~I~_is not of type ~2I~_~S.~:>"
	     (type-error-datum condition)
	     (type-error-expected-type condition)))))

(define-condition simple-type-error (simple-condition type-error) ())

(define-condition program-error (error) ())
(define-condition parse-error   (error) ())
(define-condition control-error (error) ())
(define-condition stream-error  (error)
  ((stream :reader stream-error-stream :initarg :stream)))

(define-condition end-of-file (stream-error) ()
  (:report
   (lambda (condition stream)
     (format stream
	     "end of file on ~S"
	     (stream-error-stream condition)))))

(define-condition file-error (error)
  ((pathname :reader file-error-pathname :initarg :pathname))
  (:report
   (lambda (condition stream)
     (format stream "error on file ~S" (file-error-pathname condition)))))

(define-condition package-error (error)
  ((package :reader package-error-package :initarg :package)))

(define-condition cell-error (error)
  ((name :reader cell-error-name :initarg :name)))

(define-condition unbound-variable (cell-error) ()
  (:report
   (lambda (condition stream)
     (format stream
	     "The variable ~S is unbound."
	     (cell-error-name condition)))))

(define-condition undefined-function (cell-error) ()
  (:report
   (lambda (condition stream)
     (format stream
	     "The function ~S is undefined."
	     (cell-error-name condition)))))

(define-condition special-form-function (undefined-function) ()
  (:report
   (lambda (condition stream)
     (format stream
	     "Cannot FUNCALL the SYMBOL-FUNCTION of special operator ~S."
	     (cell-error-name condition)))))

(define-condition arithmetic-error (error)
  ((operation :reader arithmetic-error-operation
	      :initarg :operation
	      :initform nil)
   (operands :reader arithmetic-error-operands
	     :initarg :operands))
  (:report (lambda (condition stream)
	     (format stream
		     "arithmetic error ~S signalled"
		     (type-of condition))
	     (when (arithmetic-error-operation condition)
	       (format stream
		       "~%Operation was ~S, operands ~S."
		       (arithmetic-error-operation condition)
		       (arithmetic-error-operands condition))))))

(define-condition division-by-zero         (arithmetic-error) ())
(define-condition floating-point-overflow  (arithmetic-error) ())
(define-condition floating-point-underflow (arithmetic-error) ())
(define-condition floating-point-inexact   (arithmetic-error) ())
(define-condition floating-point-invalid-operation (arithmetic-error) ())

(define-condition print-not-readable (error)
  ((object :reader print-not-readable-object :initarg :object))
  (:report
   (lambda (condition stream)
     (let ((obj (print-not-readable-object condition))
	   (*print-array* nil))
       (format stream "~S cannot be printed readably." obj)))))

(define-condition reader-error (parse-error stream-error)
  ((format-control
    :reader reader-error-format-control
    :initarg :format-control)
   (format-arguments
    :reader reader-error-format-arguments
    :initarg :format-arguments
    :initform '()))
  (:report
   (lambda (condition stream)
     (let ((error-stream (stream-error-stream condition)))
       (format stream "READER-ERROR ~@[at ~W ~]on ~S:~%~?"
	       (file-position error-stream) error-stream
	       (reader-error-format-control condition)
	       (reader-error-format-arguments condition))))))

;;;; various other (not specified by ANSI) CONDITIONs
;;;;
;;;; These might logically belong in other files; they're here, after
;;;; setup of CONDITION machinery, only because that makes it easier to
;;;; get cold init to work.

;;; KLUDGE: a condition for floating point errors when we can't or
;;; won't figure out what type they are. (In FreeBSD and OpenBSD we
;;; don't know how, at least as of sbcl-0.6.7; in Linux we probably
;;; know how but the old code was broken by the conversion to POSIX
;;; signal handling and hasn't been fixed as of sbcl-0.6.7.)
;;;
;;; FIXME: Perhaps this should also be a base class for all
;;; floating point exceptions?
(define-condition floating-point-exception (arithmetic-error)
  ((flags :initarg :traps
          :initform nil
	  :reader floating-point-exception-traps))
  (:report (lambda (condition stream)
	     (format stream
		     "An arithmetic error ~S was signalled.~%"
		     (type-of condition))
	     (let ((traps (floating-point-exception-traps condition)))
	       (if traps
		   (format stream
			   "Trapping conditions are: ~%~{ ~S~^~}~%"
			   traps)
		   (write-line
		    "No traps are enabled? How can this be?"
		    stream))))))

(define-condition index-too-large-error (type-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream
	     "The index ~S is too large."
	     (type-error-datum condition)))))

(define-condition bounding-indices-bad-error (type-error)
  ((object :reader bounding-indices-bad-object :initarg :object))
  (:report
   (lambda (condition stream)
     (let* ((datum (type-error-datum condition))
	    (start (car datum))
	    (end (cdr datum))
	    (object (bounding-indices-bad-object condition)))
       (etypecase object
	 (sequence
	  (format stream
		  "The bounding indices ~S and ~S are bad for a sequence of length ~S."
		  start end (length object)))
	 (array
	  ;; from WITH-ARRAY-DATA
	  (format stream
		  "The START and END parameters ~S and ~S are bad for an array of total size ~S."
		  start end (array-total-size object))))))))

(define-condition nil-array-accessed-error (type-error)
  ()
  (:report (lambda (condition stream)
	     (format stream
		     "An attempt to access an array of element-type ~
                      NIL was made.  Congratulations!"))))

(define-condition io-timeout (stream-error)
  ((direction :reader io-timeout-direction :initarg :direction))
  (:report
   (lambda (condition stream)
     (declare (type stream stream))
     (format stream
	     "I/O timeout ~(~A~)ing ~S"
	     (io-timeout-direction condition)
	     (stream-error-stream condition)))))

(define-condition namestring-parse-error (parse-error)
  ((complaint :reader namestring-parse-error-complaint :initarg :complaint)
   (args :reader namestring-parse-error-args :initarg :args :initform nil)
   (namestring :reader namestring-parse-error-namestring :initarg :namestring)
   (offset :reader namestring-parse-error-offset :initarg :offset))
  (:report
   (lambda (condition stream)
     (format stream
	     "parse error in namestring: ~?~%  ~A~%  ~V@T^"
	     (namestring-parse-error-complaint condition)
	     (namestring-parse-error-args condition)
	     (namestring-parse-error-namestring condition)
	     (namestring-parse-error-offset condition)))))

(define-condition simple-package-error (simple-condition package-error) ())

(define-condition reader-package-error (reader-error) ())

(define-condition reader-eof-error (end-of-file)
  ((context :reader reader-eof-error-context :initarg :context))
  (:report
   (lambda (condition stream)
     (format stream
	     "unexpected end of file on ~S ~A"
	     (stream-error-stream condition)
	     (reader-eof-error-context condition)))))

(define-condition reader-impossible-number-error (reader-error)
  ((error :reader reader-impossible-number-error-error :initarg :error))
  (:report
   (lambda (condition stream)
     (let ((error-stream (stream-error-stream condition)))
       (format stream "READER-ERROR ~@[at ~W ~]on ~S:~%~?~%Original error: ~A"
	       (file-position error-stream) error-stream
	       (reader-error-format-control condition)
	       (reader-error-format-arguments condition)
	       (reader-impossible-number-error-error condition))))))

;;;; special SBCL extension conditions

;;; an error apparently caused by a bug in SBCL itself
;;;
;;; Note that we don't make any serious effort to use this condition
;;; for *all* errors in SBCL itself. E.g. type errors and array
;;; indexing errors can occur in functions called from SBCL code, and
;;; will just end up as ordinary TYPE-ERROR or invalid index error,
;;; because the signalling code has no good way to know that the
;;; underlying problem is a bug in SBCL. But in the fairly common case
;;; that the signalling code does know that it's found a bug in SBCL,
;;; this condition is appropriate, reusing boilerplate and helping
;;; users to recognize it as an SBCL bug.
(define-condition bug (simple-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream
	     "~@<  ~? ~:@_~?~:>"
	     (simple-condition-format-control condition)
	     (simple-condition-format-arguments condition)
	     "~@<This is probably a bug in SBCL itself. (Alternatively, ~
              SBCL might have been corrupted by bad user code, e.g. by an ~
              undefined Lisp operation like ~S, or by stray pointers from ~
              alien code or from unsafe Lisp code; or there might be a bug ~
              in the OS or hardware that SBCL is running on.) If it seems to ~
              be a bug in SBCL itself, the maintainers would like to know ~
              about it. Bug reports are welcome on the SBCL ~
              mailing lists, which you can find at ~
              <http://sbcl.sourceforge.net/>.~:@>"
	     '((fmakunbound 'compile))))))
(defun bug (format-control &rest format-arguments)
  (error 'bug
	 :format-control format-control
	 :format-arguments format-arguments))

;;; a condition for use in stubs for operations which aren't supported
;;; on some platforms
;;;
;;; E.g. in sbcl-0.7.0.5, it might be appropriate to do something like
;;;   #-(or freebsd linux)
;;;   (defun load-foreign (&rest rest)
;;;     (error 'unsupported-operator :name 'load-foreign))
;;;   #+(or freebsd linux)
;;;   (defun load-foreign ... actual definition ...)
;;; By signalling a standard condition in this case, we make it
;;; possible for test code to distinguish between (1) intentionally
;;; unimplemented and (2) unintentionally just screwed up somehow.
;;; (Before this condition was defined, test code tried to deal with 
;;; this by checking for FBOUNDP, but that didn't work reliably. In
;;; sbcl-0.7.0, a a package screwup left the definition of
;;; LOAD-FOREIGN in the wrong package, so it was unFBOUNDP even on
;;; architectures where it was supposed to be supported, and the
;;; regression tests cheerfully passed because they assumed that
;;; unFBOUNDPness meant they were running on an system which didn't
;;; support the extension.)
(define-condition unsupported-operator (cell-error) ()
  (:report
   (lambda (condition stream)
     (format stream
	     "unsupported on this platform (OS, CPU, whatever): ~S"
	     (cell-error-name condition)))))

;;;; restart definitions

(define-condition abort-failure (control-error) ()
  (:report
   "An ABORT restart was found that failed to transfer control dynamically."))

(defun abort (&optional condition)
  #!+sb-doc
  "Transfer control to a restart named ABORT, signalling a CONTROL-ERROR if
   none exists."
  (invoke-restart (find-restart 'abort condition))
  ;; ABORT signals an error in case there was a restart named ABORT
  ;; that did not transfer control dynamically. This could happen with
  ;; RESTART-BIND.
  (error 'abort-failure))

(defun muffle-warning (&optional condition)
  #!+sb-doc
  "Transfer control to a restart named MUFFLE-WARNING, signalling a
   CONTROL-ERROR if none exists."
  (invoke-restart (find-restart 'muffle-warning condition)))

(macrolet ((define-nil-returning-restart (name args doc)
	     #!-sb-doc (declare (ignore doc))
	     `(defun ,name (,@args &optional condition)
		#!+sb-doc ,doc
		;; FIXME: Perhaps this shared logic should be pulled out into
		;; FLET MAYBE-INVOKE-RESTART? See whether it shrinks code..
		(when (find-restart ',name condition)
		  (invoke-restart ',name ,@args)))))
  (define-nil-returning-restart continue ()
    "Transfer control to a restart named CONTINUE, or return NIL if none exists.")
  (define-nil-returning-restart store-value (value)
    "Transfer control and VALUE to a restart named STORE-VALUE, or return NIL if
   none exists.")
  (define-nil-returning-restart use-value (value)
    "Transfer control and VALUE to a restart named USE-VALUE, or return NIL if
   none exists."))

(/show0 "condition.lisp end of file")

