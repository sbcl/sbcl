;;;; that part of DEFSTRUCT implementation which is needed not just 
;;;; in the target Lisp but also in the cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;;; getting LAYOUTs

;;; Return the compiler layout for Name. (The class referred to by
;;; NAME must be a structure-like class.)
(defun compiler-layout-or-lose (name)
  (let ((res (info :type :compiler-layout name)))
    (cond ((not res)
	   (error "Class is not yet defined or was undefined: ~S" name))
	  ((not (typep (layout-info res) 'defstruct-description))
	   (error "Class is not a structure class: ~S" name))
	  (t res))))

;;; Delay looking for compiler-layout until the constructor is being
;;; compiled, since it doesn't exist until after the eval-when
;;; (compile) is compiled.
(sb!xc:defmacro %delayed-get-compiler-layout (name)
  `',(compiler-layout-or-lose name))

;;; Get layout right away.
(sb!xc:defmacro compile-time-find-layout (name)
  (find-layout name))

;;; re. %DELAYED-GET-COMPILER-LAYOUT and COMPILE-TIME-FIND-LAYOUT, above..
;;;
;;; FIXME: Perhaps both should be defined with DEFMACRO-MUNDANELY?
;;; FIXME: Do we really need both? If so, their names and implementations
;;; should probably be tweaked to be more parallel.

;;; The DEFSTRUCT-DESCRIPTION structure holds compile-time information about a
;;; structure type.
(def!struct (defstruct-description
	     (:conc-name dd-)
	     (:make-load-form-fun just-dump-it-normally)
	     #-sb-xc-host (:pure t)
	     (:constructor make-defstruct-description (name)))
  ;; name of the structure
  (name (required-argument) :type symbol)
  ;; documentation on the structure
  (doc nil :type (or string null))
  ;; prefix for slot names. If NIL, none.
  (conc-name (concat-pnames name '-) :type (or symbol null))
  ;; the name of the primary standard keyword constructor, or NIL if none
  (default-constructor nil :type (or symbol null))
  ;; all the explicit :CONSTRUCTOR specs, with name defaulted
  (constructors () :type list)
  ;; name of copying function
  (copier (concat-pnames 'copy- name) :type (or symbol null))
  ;; name of type predicate
  (predicate (concat-pnames name '-p) :type (or symbol null))
  ;; the arguments to the :INCLUDE option, or NIL if no included
  ;; structure
  (include nil :type list)
  ;; The arguments to the :ALTERNATE-METACLASS option (an extension
  ;; used to define structure-like objects with an arbitrary
  ;; superclass and that may not have STRUCTURE-CLASS as the
  ;; metaclass.) Syntax is:
  ;;    (superclass-name metaclass-name metaclass-constructor)
  (alternate-metaclass nil :type list)
  ;; a list of DEFSTRUCT-SLOT-DESCRIPTION objects for all slots
  ;; (including included ones)
  (slots () :type list)
  ;; number of elements we've allocated (See also RAW-LENGTH.)
  (length 0 :type index)
  ;; General kind of implementation.
  (type 'structure :type (member structure vector list
				 funcallable-structure))

  ;; The next three slots are for :TYPE'd structures (which aren't
  ;; classes, CLASS-STRUCTURE-P = NIL)
  ;;
  ;; vector element type
  (element-type 't)
  ;; T if :NAMED was explicitly specified, NIL otherwise
  (named nil :type boolean)
  ;; any INITIAL-OFFSET option on this direct type
  (offset nil :type (or index null))

  ;; the argument to the PRINT-FUNCTION option, or NIL if a
  ;; PRINT-FUNCTION option was given with no argument, or 0 if no
  ;; PRINT-FUNCTION option was given
  (print-function 0 :type (or cons symbol (member 0)))
  ;; the argument to the PRINT-OBJECT option, or NIL if a PRINT-OBJECT
  ;; option was given with no argument, or 0 if no PRINT-OBJECT option
  ;; was given
  (print-object 0 :type (or cons symbol (member 0)))
  ;; the index of the raw data vector and the number of words in it.
  ;; NIL and 0 if not allocated yet.
  (raw-index nil :type (or index null))
  (raw-length 0 :type index)
  ;; the value of the :PURE option, or :UNSPECIFIED. This is only
  ;; meaningful if CLASS-STRUCTURE-P = T.
  (pure :unspecified :type (member t nil :substructure :unspecified)))
(def!method print-object ((x defstruct-description) stream)
  (print-unreadable-object (x stream :type t)
    (prin1 (dd-name x) stream)))

;;; A DEFSTRUCT-SLOT-DESCRIPTION holds compile-time information about
;;; a structure slot.
(def!struct (defstruct-slot-description
	     (:make-load-form-fun just-dump-it-normally)
	     (:conc-name dsd-)
	     (:copier nil)
	     #-sb-xc-host (:pure t))
  ;; string name of slot
  %name	
  ;; its position in the implementation sequence
  (index (required-argument) :type fixnum)
  ;; Name of accessor, or NIL if this accessor has the same name as an
  ;; inherited accessor (which we don't want to shadow.)
  (accessor nil)
  default			; default value expression
  (type t)			; declared type specifier
  ;; If this object does not describe a raw slot, this value is T.
  ;;
  ;; If this object describes a raw slot, this value is the type of the
  ;; value that the raw slot holds. Mostly. (KLUDGE: If the raw slot has
  ;; type (UNSIGNED-BYTE 32), the value here is UNSIGNED-BYTE, not
  ;; (UNSIGNED-BYTE 32).)
  (raw-type t :type (member t single-float double-float
			    #!+long-float long-float
			    complex-single-float complex-double-float
			    #!+long-float complex-long-float
			    unsigned-byte))
  (read-only nil :type (member t nil)))
(def!method print-object ((x defstruct-slot-description) stream)
  (print-unreadable-object (x stream :type t)
    (prin1 (dsd-name x) stream)))

;;; Is DEFSTRUCT a structure with a class?
(defun class-structure-p (defstruct)
  (member (dd-type defstruct) '(structure funcallable-structure)))

;;; Return the name of a defstruct slot as a symbol. We store it as a
;;; string to avoid creating lots of worthless symbols at load time.
(defun dsd-name (dsd)
  (intern (string (dsd-%name dsd))
	  (if (dsd-accessor dsd)
	      (symbol-package (dsd-accessor dsd))
	      *package*)))

;;;; typed (non-class) structures

;;; Return a type specifier we can use for testing :TYPE'd structures.
(defun dd-lisp-type (defstruct)
  (ecase (dd-type defstruct)
    (list 'list)
    (vector `(simple-array ,(dd-element-type defstruct) (*)))))

;;;; the legendary DEFSTRUCT macro itself (both CL:DEFSTRUCT and its
;;;; close personal friend SB!XC:DEFSTRUCT)

;;; Return a list of forms to install print and make-load-form funs, mentioning
;;; them in the expansion so that they can be compiled.
(defun class-method-definitions (defstruct)
  (let ((name (dd-name defstruct)))
    `((locally
	;; KLUDGE: There's a FIND-CLASS DEFTRANSFORM for constant
	;; class names which creates fast but non-cold-loadable,
	;; non-compact code. In this context, we'd rather have
	;; compact, cold-loadable code. -- WHN 19990928
	(declare (notinline sb!xc:find-class))
	,@(let ((pf (dd-print-function defstruct))
		(po (dd-print-object defstruct))
		(x (gensym))
		(s (gensym)))
	    ;; Giving empty :PRINT-OBJECT or :PRINT-FUNCTION options
	    ;; leaves PO or PF equal to NIL. The user-level effect is
	    ;; to generate a PRINT-OBJECT method specialized for the type,
	    ;; implementing the default #S structure-printing behavior.
	    (when (or (eq pf nil) (eq po nil))
	      (setf pf '(default-structure-print)
		    po 0))
	    (flet (;; Given an arg from a :PRINT-OBJECT or :PRINT-FUNCTION
		   ;; option, return the value to pass as an arg to FUNCTION.
		   (farg (oarg)
		     (destructuring-bind (function-name) oarg
		       function-name)))
	      (cond ((not (eql pf 0))
		     `((def!method print-object ((,x ,name) ,s)
			 (funcall #',(farg pf) ,x ,s *current-level*))))
		    ((not (eql po 0))
		     `((def!method print-object ((,x ,name) ,s)
			 (funcall #',(farg po) ,x ,s))))
		    (t nil))))
	,@(let ((pure (dd-pure defstruct)))
	    (cond ((eq pure 't)
		   `((setf (layout-pure (class-layout
					 (sb!xc:find-class ',name)))
			   t)))
		  ((eq pure :substructure)
		   `((setf (layout-pure (class-layout
					 (sb!xc:find-class ',name)))
			   0)))))
	,@(let ((def-con (dd-default-constructor defstruct)))
	    (when (and def-con (not (dd-alternate-metaclass defstruct)))
	      `((setf (structure-class-constructor (sb!xc:find-class ',name))
		      #',def-con))))
	;; FIXME: MAKE-LOAD-FORM is supposed to be handled here, too.
	))))
;;; FIXME: I really would like to make structure accessors less special,
;;; just ordinary inline functions. (Or perhaps inline functions with special
;;; compact implementations of their expansions, to avoid bloating the system.)

;;; shared logic for CL:DEFSTRUCT and SB!XC:DEFSTRUCT
;;;
;;; FIXME: There should be some way to make this not be present in the
;;; target executable, with EVAL-WHEN (COMPILE EXECUTE) and all that good
;;; stuff, but for now I can't be bothered because of the messiness of
;;; using CL:DEFMACRO in one case and SB!XC:DEFMACRO in another case.
;;; Perhaps I could dodge this by defining it as an inline function instead?
;;; Or perhaps just use MACROLET? I tried MACROLET and got nowhere and thought
;;; I was tripping over either a compiler bug or ANSI weirdness, but this
;;; test case seems to work in Debian CMU CL 2.4.9:
;;;   (macrolet ((emit-printer () ''(print "********")))
;;;     (defmacro fizz () (emit-printer)))
;;; giving
;;;   * (fizz)
;;;   "********"
;;;   "********"
;;;   *
(defmacro expander-for-defstruct (name-and-options
				  slot-descriptions
				  expanding-into-code-for-xc-host-p)
  `(let ((name-and-options ,name-and-options)
	 (slot-descriptions ,slot-descriptions)
	 (expanding-into-code-for-xc-host-p
	  ,expanding-into-code-for-xc-host-p))
     (let* ((dd (parse-name-and-options-and-slot-descriptions
		 name-and-options
		 slot-descriptions))
	    (name (dd-name dd)))
       (if (class-structure-p dd)
	   (let ((inherits (inherits-for-structure dd)))
	     `(progn
		(eval-when (:compile-toplevel :load-toplevel :execute)
		  (%compiler-only-defstruct ',dd ',inherits))
		(%defstruct ',dd ',inherits)
		,@(when (eq (dd-type dd) 'structure)
		    `((%compiler-defstruct ',dd)))
		,@(unless expanding-into-code-for-xc-host-p
		    (append (raw-accessor-definitions dd)
			    (predicate-definitions dd)
			    ;; FIXME: We've inherited from CMU CL nonparallel
			    ;; code for creating copiers for typed and untyped
			    ;; structures. This should be fixed.
					;(copier-definition dd)
			    (constructor-definitions dd)
			    (class-method-definitions dd)))
		',name))
	   `(progn
	      (eval-when (:compile-toplevel :load-toplevel :execute)
		(setf (info :typed-structure :info ',name) ',dd))
	      ,@(unless expanding-into-code-for-xc-host-p
		  (append (typed-accessor-definitions dd)
			  (typed-predicate-definitions dd)
			  (typed-copier-definitions dd)
			  (constructor-definitions dd)))
	      ',name)))))

(sb!xc:defmacro defstruct (name-and-options &rest slot-descriptions)
  #!+sb-doc
  "DEFSTRUCT {Name | (Name Option*)} {Slot | (Slot [Default] {Key Value}*)}
   Define the structure type Name. Instances are created by MAKE-<name>, which
   takes keyword arguments allowing initial slot values to the specified.
   A SETF'able function <name>-<slot> is defined for each slot to read and
   write slot values. <name>-p is a type predicate.

   Popular DEFSTRUCT options (see manual for others):

   (:CONSTRUCTOR Name)
   (:PREDICATE Name)
       Specify the name for the constructor or predicate.

   (:CONSTRUCTOR Name Lambda-List)
       Specify the name and arguments for a BOA constructor
       (which is more efficient when keyword syntax isn't necessary.)

   (:INCLUDE Supertype Slot-Spec*)
       Make this type a subtype of the structure type Supertype. The optional
       Slot-Specs override inherited slot options.

   Slot options:

   :TYPE Type-Spec
       Asserts that the value of this slot is always of the specified type.

   :READ-ONLY {T | NIL}
       If true, no setter function is defined for this slot."
    (expander-for-defstruct name-and-options slot-descriptions nil))
#+sb-xc-host
(defmacro sb!xc:defstruct (name-and-options &rest slot-descriptions)
  #!+sb-doc
  "Cause information about a target structure to be built into the
  cross-compiler."
  (expander-for-defstruct name-and-options slot-descriptions t))

;;;; functions to create various parts of DEFSTRUCT definitions

;;; Catch requests to mess up definitions in COMMON-LISP.
#-sb-xc-host
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun protect-cl (symbol)
    (when (and *cold-init-complete-p*
	       (eq (symbol-package symbol) *cl-package*))
      (cerror "Go ahead and patch the system."
	      "attempting to modify a symbol in the COMMON-LISP package: ~S"
	      symbol))))

;;; Return forms to define readers and writers for raw slots as inline
;;; functions.
(defun raw-accessor-definitions (dd)
  (let* ((name (dd-name dd)))
    (collect ((res))
      (dolist (slot (dd-slots dd))
	(let ((stype (dsd-type slot))
	      (accname (dsd-accessor slot))
	      (argname (gensym "ARG"))
	      (nvname (gensym "NEW-VALUE-")))
	  (multiple-value-bind (accessor offset data)
	      (slot-accessor-form dd slot argname)
	    ;; When accessor exists and is raw
	    (when (and accname (not (eq accessor '%instance-ref)))
	      (res `(declaim (inline ,accname)))
	      (res `(declaim (ftype (function (,name) ,stype) ,accname)))
	      (res `(defun ,accname (,argname)
		      (truly-the ,stype (,accessor ,data ,offset))))
	      (unless (dsd-read-only slot)
		(res `(declaim (inline (setf ,accname))))
		(res `(declaim (ftype (function (,stype ,name) ,stype)
				      (setf ,accname))))
		;; FIXME: I rewrote this somewhat from the CMU CL definition.
		;; Do some basic tests to make sure that reading and writing
		;; raw slots still works correctly.
		(res `(defun (setf ,accname) (,nvname ,argname)
			(setf (,accessor ,data ,offset) ,nvname)
			,nvname)))))))
      (res))))

;;; Return a list of forms which create a predicate for an untyped DEFSTRUCT.
(defun predicate-definitions (dd)
  (let ((pred (dd-predicate dd))
	(argname (gensym)))
    (when pred
      (if (eq (dd-type dd) 'funcallable-structure)
	  ;; FIXME: Why does this need to be special-cased for
	  ;; FUNCALLABLE-STRUCTURE? CMU CL did it, but without explanation.
	  ;; Could we do without it? What breaks if we do? Or could we
	  ;; perhaps get by with no predicates for funcallable structures?
	  `((declaim (inline ,pred))
	    (defun ,pred (,argname) (typep ,argname ',(dd-name dd))))
	  `((protect-cl ',pred)
	    (declaim (inline ,pred))
	    (defun ,pred (,argname)
	      (declare (optimize (speed 3) (safety 0)))
	      (typep-to-layout ,argname
			       (compile-time-find-layout ,(dd-name dd)))))))))

;;; Return a list of forms which create a predicate function for a typed
;;; DEFSTRUCT.
(defun typed-predicate-definitions (defstruct)
  (let ((name (dd-name defstruct))
	(pred (dd-predicate defstruct))
	(argname (gensym)))
    (when (and pred (dd-named defstruct))
      (let ((ltype (dd-lisp-type defstruct)))
	`((defun ,pred (,argname)
	    (and (typep ,argname ',ltype)
		 (eq (elt (the ,ltype ,argname)
			  ,(cdr (car (last (find-name-indices defstruct)))))
		     ',name))))))))

;;; FIXME: We've inherited from CMU CL code to do typed structure copiers
;;; in a completely different way than untyped structure copiers. Fix this.
;;; (This function was my first attempt to fix this, but I stopped before
;;; figuring out how to install it completely and remove the parallel
;;; code which simply SETF's the FDEFINITION of the DD-COPIER name.
#|
;;; Return the copier definition for an untyped DEFSTRUCT.
(defun copier-definition (dd)
  (when (and (dd-copier dd)
	     ;; FUNCALLABLE-STRUCTUREs don't need copiers, and this
	     ;; implementation wouldn't work for them anyway, since
	     ;; COPY-STRUCTURE returns a STRUCTURE-OBJECT and they're not.
	     (not (eq (dd-type info) 'funcallable-structure)))
    (let ((argname (gensym)))
      `(progn
	 (protect-cl ',(dd-copier dd))
	 (defun ,(dd-copier dd) (,argname)
	   (declare (type ,(dd-name dd) ,argname))
	   (copy-structure ,argname))))))
|#

;;; Return a list of forms to create a copier function of a typed DEFSTRUCT.
(defun typed-copier-definitions (defstruct)
  (when (dd-copier defstruct)
    `((setf (fdefinition ',(dd-copier defstruct)) #'copy-seq)
      (declaim (ftype function ,(dd-copier defstruct))))))

;;; Return a list of function definitions for accessing and setting the
;;; slots of a typed DEFSTRUCT. The functions are proclaimed to be inline,
;;; and the types of their arguments and results are declared as well. We
;;; count on the compiler to do clever things with ELT.
(defun typed-accessor-definitions (defstruct)
  (collect ((stuff))
    (let ((ltype (dd-lisp-type defstruct)))
      (dolist (slot (dd-slots defstruct))
	(let ((name (dsd-accessor slot))
	      (index (dsd-index slot))
	      (slot-type `(and ,(dsd-type slot)
			       ,(dd-element-type defstruct))))
	  (stuff `(proclaim '(inline ,name (setf ,name))))
	  ;; FIXME: The arguments in the next two DEFUNs should be
	  ;; gensyms. (Otherwise e.g. if NEW-VALUE happened to be the
	  ;; name of a special variable, things could get weird.)
	  (stuff `(defun ,name (structure)
		    (declare (type ,ltype structure))
		    (the ,slot-type (elt structure ,index))))
	  (unless (dsd-read-only slot)
	    (stuff
	     `(defun (setf ,name) (new-value structure)
		(declare (type ,ltype structure) (type ,slot-type new-value))
		(setf (elt structure ,index) new-value)))))))
    (stuff)))

;;;; parsing

(defun require-no-print-options-so-far (defstruct)
  (unless (and (eql (dd-print-function defstruct) 0)
	       (eql (dd-print-object defstruct) 0))
    (error "no more than one of the following options may be specified:
  :PRINT-FUNCTION, :PRINT-OBJECT, :TYPE")))

;;; Parse a single defstruct option and store the results in DEFSTRUCT.
(defun parse-1-option (option defstruct)
  (let ((args (rest option))
	(name (dd-name defstruct)))
    (case (first option)
      (:conc-name
       (destructuring-bind (conc-name) args
	 (setf (dd-conc-name defstruct)
	       (if (symbolp conc-name)
		   conc-name
		   (make-symbol (string conc-name))))))
      (:constructor
       (destructuring-bind (&optional (cname (concat-pnames 'make- name))
				      &rest stuff)
	   args
	 (push (cons cname stuff) (dd-constructors defstruct))))
      (:copier
       (destructuring-bind (&optional (copier (concat-pnames 'copy- name)))
	   args
	 (setf (dd-copier defstruct) copier)))
      (:predicate
       (destructuring-bind (&optional (pred (concat-pnames name '-p))) args
	 (setf (dd-predicate defstruct) pred)))
      (:include
       (when (dd-include defstruct)
	 (error "more than one :INCLUDE option"))
       (setf (dd-include defstruct) args))
      (:alternate-metaclass
       (setf (dd-alternate-metaclass defstruct) args))
      (:print-function
       (require-no-print-options-so-far defstruct)
       (setf (dd-print-function defstruct)
	     (the (or symbol cons) args)))
      (:print-object
       (require-no-print-options-so-far defstruct)
       (setf (dd-print-object defstruct)
	     (the (or symbol cons) args)))
      (:type
       (destructuring-bind (type) args
	 (cond ((eq type 'funcallable-structure)
		(setf (dd-type defstruct) type))
	       ((member type '(list vector))
		(setf (dd-element-type defstruct) 't)
		(setf (dd-type defstruct) type))
	       ((and (consp type) (eq (first type) 'vector))
		(destructuring-bind (vector vtype) type
		  (declare (ignore vector))
		  (setf (dd-element-type defstruct) vtype)
		  (setf (dd-type defstruct) 'vector)))
	       (t
		(error "~S is a bad :TYPE for Defstruct." type)))))
      (:named
       (error "The DEFSTRUCT option :NAMED takes no arguments."))
      (:initial-offset
       (destructuring-bind (offset) args
	 (setf (dd-offset defstruct) offset)))
      (:pure
       (destructuring-bind (fun) args
	 (setf (dd-pure defstruct) fun)))
      (t (error "unknown DEFSTRUCT option:~%  ~S" option)))))

;;; Given name and options, return a DD holding that info.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun parse-name-and-options (name-and-options)
  (destructuring-bind (name &rest options) name-and-options
    (let ((defstruct (make-defstruct-description name)))
      (dolist (option options)
	(cond ((consp option)
	       (parse-1-option option defstruct))
	      ((eq option :named)
	       (setf (dd-named defstruct) t))
	      ((member option '(:constructor :copier :predicate :named))
	       (parse-1-option (list option) defstruct))
	      (t
	       (error "unrecognized DEFSTRUCT option: ~S" option))))

      (case (dd-type defstruct)
	(structure
	 (when (dd-offset defstruct)
	   (error ":OFFSET can't be specified unless :TYPE is specified."))
	 (unless (dd-include defstruct)
	   (incf (dd-length defstruct))))
	(funcallable-structure)
	(t
	 (require-no-print-options-so-far defstruct)
	 (when (dd-named defstruct)
	   (incf (dd-length defstruct)))
	 (let ((offset (dd-offset defstruct)))
	   (when offset (incf (dd-length defstruct) offset)))))

      (when (dd-include defstruct)
	(do-inclusion-stuff defstruct))

      defstruct)))

;;; Given name and options and slot descriptions (and possibly doc
;;; string at the head of slot descriptions) return a DD holding that
;;; info.
(defun parse-name-and-options-and-slot-descriptions (name-and-options
						     slot-descriptions)
  (/noshow "PARSE-NAME-AND-OPTIONS-AND-SLOT-DESCRIPTIONS" name-and-options)
  (let ((result (parse-name-and-options (if (atom name-and-options)
					  (list name-and-options)
					  name-and-options))))
    (when (stringp (car slot-descriptions))
      (setf (dd-doc result) (pop slot-descriptions)))
    (dolist (slot slot-descriptions)
      (allocate-1-slot result (parse-1-dsd result slot)))
    result))

) ; EVAL-WHEN

;;;; stuff to parse slot descriptions

;;; Parse a slot description for DEFSTRUCT, add it to the description
;;; and return it. If supplied, ISLOT is a pre-initialized DSD that we
;;; modify to get the new slot. This is supplied when handling
;;; included slots. If the new accessor name is already an accessor
;;; for same slot in some included structure, then set the
;;; DSD-ACCESSOR to NIL so that we don't clobber the more general
;;; accessor.
(defun parse-1-dsd (defstruct spec &optional
		     (islot (make-defstruct-slot-description :%name ""
							     :index 0
							     :type t)))
  (multiple-value-bind (name default default-p type type-p read-only ro-p)
      (cond
       ((listp spec)
	(destructuring-bind
	    (name
	     &optional (default nil default-p)
	     &key (type nil type-p) (read-only nil ro-p))
	    spec
	  (values name
		  default default-p
		  (uncross type) type-p
		  read-only ro-p)))
       (t
	(when (keywordp spec)
	  ;; FIXME: should be style warning
	  (warn "Keyword slot name indicates probable syntax ~
		 error in DEFSTRUCT -- ~S."
		spec))
	spec))

    (when (find name (dd-slots defstruct) :test #'string= :key #'dsd-%name)
      (error 'program-error
	     :format-control "duplicate slot name ~S"
	     :format-arguments (list name)))
    (setf (dsd-%name islot) (string name))
    (setf (dd-slots defstruct) (nconc (dd-slots defstruct) (list islot)))

    (let* ((accname (concat-pnames (dd-conc-name defstruct) name))
	   (existing (info :function :accessor-for accname)))
      (if (and (structure-class-p existing)
	       (not (eq (sb!xc:class-name existing) (dd-name defstruct)))
	       (string= (dsd-%name (find accname
					 (dd-slots
					  (layout-info
					   (class-layout existing)))
					 :key #'dsd-accessor))
			name))
	(setf (dsd-accessor islot) nil)
	(setf (dsd-accessor islot) accname)))

    (when default-p
      (setf (dsd-default islot) default))
    (when type-p
      (setf (dsd-type islot)
	    (if (eq (dsd-type islot) 't)
		type
		`(and ,(dsd-type islot) ,type))))
    (when ro-p
      (if read-only
	  (setf (dsd-read-only islot) t)
	  (when (dsd-read-only islot)
	    (error "Slot ~S is :READ-ONLY in parent and must be :READ-ONLY in subtype ~S."
		   name
		   (dsd-name islot)))))
    islot))

;;; When a value of type TYPE is stored in a structure, should it be
;;; stored in a raw slot? Return (VALUES RAW? RAW-TYPE WORDS), where
;;;   RAW? is true if TYPE should be stored in a raw slot.
;;;   RAW-TYPE is the raw slot type, or NIL if no raw slot.
;;;   WORDS is the number of words in the raw slot, or NIL if no raw slot.
(defun structure-raw-slot-type-and-size (type)
  (/noshow "in STRUCTURE-RAW-SLOT-TYPE-AND-SIZE" type (sb!xc:subtypep type 'fixnum))
  (cond #+nil
	(;; FIXME: For now we suppress raw slots, since there are various
	 ;; issues about the way that the cross-compiler handles them.
	 (not (boundp '*dummy-placeholder-to-stop-compiler-warnings*))
	 (values nil nil nil))
	((and (sb!xc:subtypep type '(unsigned-byte 32))
	      (multiple-value-bind (fixnum? fixnum-certain?)
		  (sb!xc:subtypep type 'fixnum)
		(/noshow fixnum? fixnum-certain?)
		;; (The extra test for FIXNUM-CERTAIN? here is
		;; intended for bootstrapping the system. In
		;; particular, in sbcl-0.6.2, we set up LAYOUT before
		;; FIXNUM is defined, and so could bogusly end up
		;; putting INDEX-typed values into raw slots if we
		;; didn't test FIXNUM-CERTAIN?.)
		(and (not fixnum?) fixnum-certain?)))
	 (values t 'unsigned-byte 1))
	((sb!xc:subtypep type 'single-float)
	 (values t 'single-float 1))
	((sb!xc:subtypep type 'double-float)
	 (values t 'double-float 2))
	#!+long-float
	((sb!xc:subtypep type 'long-float)
	 (values t 'long-float #!+x86 3 #!+sparc 4))
	((sb!xc:subtypep type '(complex single-float))
	 (values t 'complex-single-float 2))
	((sb!xc:subtypep type '(complex double-float))
	 (values t 'complex-double-float 4))
	#!+long-float
	((sb!xc:subtypep type '(complex long-float))
	 (values t 'complex-long-float #!+x86 6 #!+sparc 8))
	(t
	 (values nil nil nil))))

;;; Allocate storage for a DSD in DEFSTRUCT. This is where we decide
;;; whether a slot is raw or not. If raw, and we haven't allocated a
;;; raw-index yet for the raw data vector, then do it. Raw objects are
;;; aligned on the unit of their size.
(defun allocate-1-slot (defstruct dsd)
  (multiple-value-bind (raw? raw-type words)
      (if (eq (dd-type defstruct) 'structure)
	  (structure-raw-slot-type-and-size (dsd-type dsd))
	  (values nil nil nil))
    (/noshow "ALLOCATE-1-SLOT" dsd raw? raw-type words)
    (cond ((not raw?)
	   (setf (dsd-index dsd) (dd-length defstruct))
	   (incf (dd-length defstruct)))
	  (t
	   (unless (dd-raw-index defstruct)
	     (setf (dd-raw-index defstruct) (dd-length defstruct))
	     (incf (dd-length defstruct)))
	   (let ((off (rem (dd-raw-length defstruct) words)))
	     (unless (zerop off)
	       (incf (dd-raw-length defstruct) (- words off))))
	   (setf (dsd-raw-type dsd) raw-type)
	   (setf (dsd-index dsd) (dd-raw-length defstruct))
	   (incf (dd-raw-length defstruct) words))))
  (values))

(defun typed-structure-info-or-lose (name)
  (or (info :typed-structure :info name)
      (error ":TYPE'd DEFSTRUCT ~S not found for inclusion." name)))

;;; Process any included slots pretty much like they were specified.
;;; Also inherit various other attributes.
(defun do-inclusion-stuff (defstruct)
  (destructuring-bind
      (included-name &rest modified-slots)
      (dd-include defstruct)
    (let* ((type (dd-type defstruct))
	   (included-structure
	    (if (class-structure-p defstruct)
		(layout-info (compiler-layout-or-lose included-name))
		(typed-structure-info-or-lose included-name))))
      (unless (and (eq type (dd-type included-structure))
		   (type= (specifier-type (dd-element-type included-structure))
			  (specifier-type (dd-element-type defstruct))))
	(error ":TYPE option mismatch between structures ~S and ~S."
	       (dd-name defstruct) included-name))

      (incf (dd-length defstruct) (dd-length included-structure))
      (when (class-structure-p defstruct)
	(let ((mc (rest (dd-alternate-metaclass included-structure))))
	  (when (and mc (not (dd-alternate-metaclass defstruct)))
	    (setf (dd-alternate-metaclass defstruct)
		  (cons included-name mc))))
	(when (eq (dd-pure defstruct) :unspecified)
	  (setf (dd-pure defstruct) (dd-pure included-structure)))
	(setf (dd-raw-index defstruct) (dd-raw-index included-structure))
	(setf (dd-raw-length defstruct) (dd-raw-length included-structure)))

      (dolist (islot (dd-slots included-structure))
	(let* ((iname (dsd-name islot))
	       (modified (or (find iname modified-slots
				   :key #'(lambda (x) (if (atom x) x (car x)))
				   :test #'string=)
			     `(,iname))))
	  (parse-1-dsd defstruct modified (copy-structure islot)))))))

;;; This function is called at macroexpand time to compute the INHERITS
;;; vector for a structure type definition.
(defun inherits-for-structure (info)
  (declare (type defstruct-description info))
  (let* ((include (dd-include info))
	 (superclass-opt (dd-alternate-metaclass info))
	 (super
	  (if include
	      (compiler-layout-or-lose (first include))
	      (class-layout (sb!xc:find-class
			     (or (first superclass-opt)
				 'structure-object))))))
    (if (eq (dd-name info) 'lisp-stream)
	;; a hack to added the stream class as a mixin for LISP-STREAMs
	(concatenate 'simple-vector
		     (layout-inherits super)
		     (vector super
			     (class-layout (sb!xc:find-class 'stream))))
	(concatenate 'simple-vector
		     (layout-inherits super)
		     (vector super)))))

;;; Do miscellaneous (LOAD EVAL) time actions for the structure
;;; described by INFO. Create the class & layout, checking for
;;; incompatible redefinition. Define setters, accessors, copier,
;;; predicate, documentation, instantiate definition in load-time env.
;;; This is only called for default structures.
(defun %defstruct (info inherits)
  (declare (type defstruct-description info))
  (multiple-value-bind (class layout old-layout)
      (ensure-structure-class info inherits "current" "new")
    (cond ((not old-layout)
	   (unless (eq (class-layout class) layout)
	     (register-layout layout)))
	  (t
	   (let ((old-info (layout-info old-layout)))
	     (when (defstruct-description-p old-info)
	       (dolist (slot (dd-slots old-info))
		 (fmakunbound (dsd-accessor slot))
		 (unless (dsd-read-only slot)
		   (fmakunbound `(setf ,(dsd-accessor slot)))))))
	   (%redefine-defstruct class old-layout layout)
	   (setq layout (class-layout class))))

    (setf (sb!xc:find-class (dd-name info)) class)

    ;; Set FDEFINITIONs for structure accessors, setters, predicates,
    ;; and copiers.
    #-sb-xc-host
    (unless (eq (dd-type info) 'funcallable-structure)

      (dolist (slot (dd-slots info))
	(let ((dsd slot))
	  (when (and (dsd-accessor slot)
		     (eq (dsd-raw-type slot) t))
	    (protect-cl (dsd-accessor slot))
	    (setf (symbol-function (dsd-accessor slot))
		  (structure-slot-getter layout dsd))
	    (unless (dsd-read-only slot)
	      (setf (fdefinition `(setf ,(dsd-accessor slot)))
		    (structure-slot-setter layout dsd))))))

      ;; FIXME: See comment on corresponding code in %%COMPILER-DEFSTRUCT.
      #|
      (when (dd-predicate info)
	(protect-cl (dd-predicate info))
	(setf (symbol-function (dd-predicate info))
	      #'(lambda (object)
		  (declare (optimize (speed 3) (safety 0)))
		  (typep-to-layout object layout))))
      |#

      (when (dd-copier info)
	(protect-cl (dd-copier info))
	(setf (symbol-function (dd-copier info))
	      #'(lambda (structure)
		  (declare (optimize (speed 3) (safety 0)))
		  (flet ((layout-test (structure)
			   (typep-to-layout structure layout)))
		    (unless (layout-test structure)
		      (error 'simple-type-error
			     :datum structure
			     :expected-type '(satisfies layout-test)
			     :format-control
			     "Structure for copier is not a ~S:~% ~S"
			     :format-arguments
			     (list (sb!xc:class-name (layout-class layout))
				   structure))))
		  (copy-structure structure))))))

  (when (dd-doc info)
    (setf (fdocumentation (dd-name info) 'type) (dd-doc info)))

  (values))

;;; This function is called at compile-time to do the
;;; compile-time-only actions for defining a structure type. It
;;; installs the class in the type system in a similar way to
;;; %DEFSTRUCT, but is quieter and safer in the case of redefinition.
;;;
;;; The comments for the classic CMU CL version of this function said
;;; that EVAL-WHEN doesn't do the right thing when nested or
;;; non-top-level, and so CMU CL had the function magically called by
;;; the compiler. Unfortunately, this doesn't do the right thing
;;; either: compiling a function (DEFUN FOO () (DEFSTRUCT FOO X Y))
;;; causes the class FOO to become defined, even though FOO is never
;;; loaded or executed. Even more unfortunately, I've been unable to
;;; come up with any EVAL-WHEN tricks which work -- I finally gave up
;;; on this approach when trying to get the system to cross-compile
;;; error.lisp. (Just because I haven't found it doesn't mean that it
;;; doesn't exist, of course. Alas, I continue to have some trouble
;;; understanding compile/load semantics in Common Lisp.) So we
;;; continue to use the IR1 transformation approach, even though it's
;;; known to be buggy. -- WHN 19990507
;;;
;;; Basically, this function avoids trashing the compiler by only
;;; actually defining the class if there is no current definition.
;;; Instead, we just set the INFO TYPE COMPILER-LAYOUT. This behavior
;;; is left over from classic CMU CL and may not be necessary in the
;;; new build system. -- WHN 19990507
;;;
;;; FUNCTION-%COMPILER-ONLY-DEFSTRUCT is an ordinary function, called
;;; by both the IR1 transform version of %COMPILER-ONLY-DEFSTRUCT and
;;; by the ordinary function version of %COMPILER-ONLY-DEFSTRUCT. (The
;;; ordinary function version is there for the interpreter and for
;;; code walkers.)
(defun %compiler-only-defstruct (info inherits)
  (function-%compiler-only-defstruct info inherits))
(defun function-%compiler-only-defstruct (info inherits)
  (multiple-value-bind (class layout old-layout)
      (multiple-value-bind (clayout clayout-p)
	  (info :type :compiler-layout (dd-name info))
	(ensure-structure-class info
				inherits
				(if clayout-p "previously compiled" "current")
				"compiled"
				:compiler-layout clayout))
    (cond (old-layout
	   (undefine-structure (layout-class old-layout))
	   (when (and (class-subclasses class)
		      (not (eq layout old-layout)))
	     (collect ((subs))
		      (dohash (class layout (class-subclasses class))
			(declare (ignore layout))
			(undefine-structure class)
			(subs (class-proper-name class)))
		      (when (subs)
			(warn "Removing old subclasses of ~S:~%  ~S"
			      (sb!xc:class-name class)
			      (subs))))))
	  (t
	   (unless (eq (class-layout class) layout)
	     (register-layout layout :invalidate nil))
	   (setf (sb!xc:find-class (dd-name info)) class)))

    (setf (info :type :compiler-layout (dd-name info)) layout))
  (values))

;;; This function does the (COMPILE LOAD EVAL) time actions for updating the
;;; compiler's global meta-information to represent the definition of the
;;; structure described by Info. This primarily amounts to setting up info
;;; about the accessor and other implicitly defined functions. The constructors
;;; are explicitly defined by top-level code.
(defun %%compiler-defstruct (info)
  (declare (type defstruct-description info))
  (let* ((name (dd-name info))
	 (class (sb!xc:find-class name)))
    (let ((copier (dd-copier info)))
      (when copier
	(proclaim `(ftype (function (,name) ,name) ,copier))))

    ;; FIXME: This (and corresponding code in %DEFSTRUCT) are the way
    ;; that CMU CL defined the predicate, instead of using DEFUN.
    ;; Perhaps it would be better to go back to to the CMU CL way, or
    ;; something similar. I want to reduce the amount of magic in
    ;; defstruct functions, but making the predicate be a closure
    ;; looks like a good thing, and can even be done without magic.
    ;; (OTOH, there are some bootstrapping issues involved, since
    ;; GENESIS understands DEFUN but doesn't understand a
    ;; (SETF SYMBOL-FUNCTION) call inside %DEFSTRUCT.)
    #|
    (let ((pred (dd-predicate info)))
      (when pred
	(proclaim-as-defstruct-function-name pred)
	(setf (info :function :inlinep pred) :inline)
	(setf (info :function :inline-expansion pred)
	      `(lambda (x) (typep x ',name)))))
    |#

    (dolist (slot (dd-slots info))
      (let* ((fun (dsd-accessor slot))
	     (setf-fun `(setf ,fun)))
	(when (and fun (eq (dsd-raw-type slot) t))
	  (proclaim-as-defstruct-function-name fun)
	  (setf (info :function :accessor-for fun) class)
	  (unless (dsd-read-only slot)
	    (proclaim-as-defstruct-function-name setf-fun)
	    (setf (info :function :accessor-for setf-fun) class))))))

  (values))

;;; Ordinarily this is preempted by an IR1 transformation, but this
;;; definition is still useful for the interpreter and code walkers.
(defun %compiler-defstruct (info)
  (%%compiler-defstruct info))

;;;; redefinition stuff

;;; Compare the slots of OLD and NEW, returning 3 lists of slot names:
;;;   1. Slots which have moved,
;;;   2. Slots whose type has changed,
;;;   3. Deleted slots.
(defun compare-slots (old new)
  (let* ((oslots (dd-slots old))
	 (nslots (dd-slots new))
	 (onames (mapcar #'dsd-name oslots))
	 (nnames (mapcar #'dsd-name nslots)))
    (collect ((moved)
	      (retyped))
      (dolist (name (intersection onames nnames))
	(let ((os (find name oslots :key #'dsd-name))
	      (ns (find name nslots :key #'dsd-name)))
	  (unless (subtypep (dsd-type ns) (dsd-type os))
	    (/noshow "found retyped slots" ns os (dsd-type ns) (dsd-type os))
	    (retyped name))
	  (unless (and (= (dsd-index os) (dsd-index ns))
		       (eq (dsd-raw-type os) (dsd-raw-type ns)))
	    (moved name))))
      (values (moved)
	      (retyped)
	      (set-difference onames nnames)))))

;;; If we are redefining a structure with different slots than in the
;;; currently loaded version, give a warning and return true.
(defun redefine-structure-warning (class old new)
  (declare (type defstruct-description old new)
	   (type sb!xc:class class)
	   (ignore class))
  (let ((name (dd-name new)))
    (multiple-value-bind (moved retyped deleted) (compare-slots old new)
      (when (or moved retyped deleted)
	(warn
	 "incompatibly redefining slots of structure class ~S~@
	  Make sure any uses of affected accessors are recompiled:~@
	  ~@[  These slots were moved to new positions:~%    ~S~%~]~
	  ~@[  These slots have new incompatible types:~%    ~S~%~]~
	  ~@[  These slots were deleted:~%    ~S~%~]"
	 name moved retyped deleted)
	t))))

;;; This function is called when we are incompatibly redefining a
;;; structure Class to have the specified New-Layout. We signal an
;;; error with some proceed options and return the layout that should
;;; be used.
(defun %redefine-defstruct (class old-layout new-layout)
  (declare (type sb!xc:class class) (type layout old-layout new-layout))
  (let ((name (class-proper-name class)))
    (restart-case
	(error "redefining class ~S incompatibly with the current definition"
	       name)
      (continue ()
	:report "Invalidate current definition."
	(warn "Previously loaded ~S accessors will no longer work." name)
	(register-layout new-layout))
      (clobber-it ()
	:report "Smash current layout, preserving old code."
	(warn "Any old ~S instances will be in a bad way.~@
	       I hope you know what you're doing..."
	      name)
	(register-layout new-layout :invalidate nil
			 :destruct-layout old-layout))))
  (values))

;;; This is called when we are about to define a structure class. It
;;; returns a (possibly new) class object and the layout which should
;;; be used for the new definition (may be the current layout, and
;;; also might be an uninstalled forward referenced layout.) The third
;;; value is true if this is an incompatible redefinition, in which
;;; case it is the old layout.
(defun ensure-structure-class (info inherits old-context new-context
				    &key compiler-layout)
  (multiple-value-bind (class old-layout)
      (destructuring-bind
	  (&optional
	   name
	   (class 'sb!xc:structure-class)
	   (constructor 'make-structure-class))
	  (dd-alternate-metaclass info)
	(declare (ignore name))
	(insured-find-class (dd-name info)
			    (if (eq class 'sb!xc:structure-class)
			      (lambda (x)
				(typep x 'sb!xc:structure-class))
			      (lambda (x)
				(sb!xc:typep x (sb!xc:find-class class))))
			    (fdefinition constructor)))
    (setf (class-direct-superclasses class)
	  (if (eq (dd-name info) 'lisp-stream)
	      ;; a hack to add STREAM as a superclass mixin to LISP-STREAMs
	      (list (layout-class (svref inherits (1- (length inherits))))
		    (layout-class (svref inherits (- (length inherits) 2))))
	      (list (layout-class (svref inherits (1- (length inherits)))))))
    (let ((new-layout (make-layout :class class
				   :inherits inherits
				   :depthoid (length inherits)
				   :length (dd-length info)
				   :info info))
	  (old-layout (or compiler-layout old-layout)))
      (cond
       ((not old-layout)
	(values class new-layout nil))
       (;; This clause corresponds to an assertion in REDEFINE-LAYOUT-WARNING
	;; of classic CMU CL. I moved it out to here because it was only
	;; exercised in this code path anyway. -- WHN 19990510
	(not (eq (layout-class new-layout) (layout-class old-layout)))
	(error "shouldn't happen: weird state of OLD-LAYOUT?"))
       ((not *type-system-initialized*)
	(setf (layout-info old-layout) info)
	(values class old-layout nil))
       ((redefine-layout-warning old-context
				 old-layout
				 new-context
				 (layout-length new-layout)
				 (layout-inherits new-layout)
				 (layout-depthoid new-layout))
	(values class new-layout old-layout))
       (t
	(let ((old-info (layout-info old-layout)))
	  (typecase old-info
	    ((or defstruct-description)
	     (cond ((redefine-structure-warning class old-info info)
		    (values class new-layout old-layout))
		   (t
		    (setf (layout-info old-layout) info)
		    (values class old-layout nil))))
	    (null
	     (setf (layout-info old-layout) info)
	     (values class old-layout nil))
	    (t
	     (error "shouldn't happen! strange thing in LAYOUT-INFO:~%  ~S"
		    old-layout)
	     (values class new-layout old-layout)))))))))

;;; Blow away all the compiler info for the structure CLASS. Iterate
;;; over this type, clearing the compiler structure type info, and
;;; undefining all the associated functions.
(defun undefine-structure (class)
  (let ((info (layout-info (class-layout class))))
    (when (defstruct-description-p info)
      (let ((type (dd-name info)))
	(setf (info :type :compiler-layout type) nil)
	(undefine-function-name (dd-copier info))
	(undefine-function-name (dd-predicate info))
	(dolist (slot (dd-slots info))
	  (let ((fun (dsd-accessor slot)))
	    (undefine-function-name fun)
	    (unless (dsd-read-only slot)
	      (undefine-function-name `(setf ,fun))))))
      ;; Clear out the SPECIFIER-TYPE cache so that subsequent
      ;; references are unknown types.
      (values-specifier-type-cache-clear)))
  (values))

;;; Return a list of pairs (name . index). Used for :TYPE'd
;;; constructors to find all the names that we have to splice in &
;;; where. Note that these types don't have a layout, so we can't look
;;; at LAYOUT-INHERITS.
(defun find-name-indices (defstruct)
  (collect ((res))
    (let ((infos ()))
      (do ((info defstruct
		 (typed-structure-info-or-lose (first (dd-include info)))))
	  ((not (dd-include info))
	   (push info infos))
	(push info infos))

      (let ((i 0))
	(dolist (info infos)
	  (incf i (or (dd-offset info) 0))
	  (when (dd-named info)
	    (res (cons (dd-name info) i)))
	  (setq i (dd-length info)))))

    (res)))

;;;; slot accessors for raw slots

;;; Return info about how to read/write a slot in the value stored in
;;; OBJECT. This is also used by constructors (we can't use the
;;; accessor function, since some slots are read-only.) If supplied,
;;; DATA is a variable holding the raw-data vector.
;;;
;;; returned values:
;;; 1. accessor function name (SETFable)
;;; 2. index to pass to accessor.
;;; 3. object form to pass to accessor
(defun slot-accessor-form (defstruct slot object &optional data)
  (let ((rtype (dsd-raw-type slot)))
    (values
     (ecase rtype
       (single-float '%raw-ref-single)
       (double-float '%raw-ref-double)
       #!+long-float
       (long-float '%raw-ref-long)
       (complex-single-float '%raw-ref-complex-single)
       (complex-double-float '%raw-ref-complex-double)
       #!+long-float
       (complex-long-float '%raw-ref-complex-long)
       (unsigned-byte 'aref)
       ((t)
	(if (eq (dd-type defstruct) 'funcallable-structure)
	    '%funcallable-instance-info
	    '%instance-ref)))
     (case rtype
       #!+long-float
       (complex-long-float
	(truncate (dsd-index slot) #!+x86 6 #!+sparc 8))
       #!+long-float
       (long-float
	(truncate (dsd-index slot) #!+x86 3 #!+sparc 4))
       (double-float
	(ash (dsd-index slot) -1))
       (complex-double-float
	(ash (dsd-index slot) -2))
       (complex-single-float
	(ash (dsd-index slot) -1))
       (t
	(dsd-index slot)))
     (cond
      ((eq rtype 't) object)
      (data)
      (t
       `(truly-the (simple-array (unsigned-byte 32) (*))
		   (%instance-ref ,object ,(dd-raw-index defstruct))))))))

;;; These functions are called to actually make a constructor after we
;;; have processed the arglist. The correct variant (according to the
;;; DD-TYPE) should be called. The function is defined with the
;;; specified name and arglist. Vars and Types are used for argument
;;; type declarations. Values are the values for the slots (in order.)
;;;
;;; This is split four ways because:
;;; 1] list & vector structures need "name" symbols stuck in at
;;;    various weird places, whereas STRUCTURE structures have
;;;    a LAYOUT slot.
;;; 2] We really want to use LIST to make list structures, instead of
;;;    MAKE-LIST/(SETF ELT).
;;; 3] STRUCTURE structures can have raw slots that must also be
;;;    allocated and indirectly referenced. We use SLOT-ACCESSOR-FORM
;;;    to compute how to set the slots, which deals with raw slots.
;;; 4] Funcallable structures are weird.
(defun create-vector-constructor
       (defstruct cons-name arglist vars types values)
  (let ((temp (gensym))
	(etype (dd-element-type defstruct)))
    `(defun ,cons-name ,arglist
       (declare ,@(mapcar #'(lambda (var type) `(type (and ,type ,etype) ,var))
			  vars types))
       (let ((,temp (make-array ,(dd-length defstruct)
				:element-type ',(dd-element-type defstruct))))
	 ,@(mapcar #'(lambda (x)
		       `(setf (aref ,temp ,(cdr x))  ',(car x)))
		   (find-name-indices defstruct))
	 ,@(mapcar #'(lambda (dsd value)
		       `(setf (aref ,temp ,(dsd-index dsd)) ,value))
		   (dd-slots defstruct) values)
	 ,temp))))
(defun create-list-constructor
       (defstruct cons-name arglist vars types values)
  (let ((vals (make-list (dd-length defstruct) :initial-element nil)))
    (dolist (x (find-name-indices defstruct))
      (setf (elt vals (cdr x)) `',(car x)))
    (loop for dsd in (dd-slots defstruct) and val in values do
      (setf (elt vals (dsd-index dsd)) val))

    `(defun ,cons-name ,arglist
       (declare ,@(mapcar #'(lambda (var type) `(type ,type ,var))
			  vars types))
       (list ,@vals))))
(defun create-structure-constructor
       (defstruct cons-name arglist vars types values)
  (let* ((temp (gensym))
	 (raw-index (dd-raw-index defstruct))
	 (n-raw-data (when raw-index (gensym))))
    `(defun ,cons-name ,arglist
       (declare ,@(mapcar #'(lambda (var type) `(type ,type ,var))
			  vars types))
       (let ((,temp (truly-the ,(dd-name defstruct)
			       (%make-instance ,(dd-length defstruct))))
	     ,@(when n-raw-data
		 `((,n-raw-data
		    (make-array ,(dd-raw-length defstruct)
				:element-type '(unsigned-byte 32))))))
	 (setf (%instance-layout ,temp)
	       (%delayed-get-compiler-layout ,(dd-name defstruct)))
	 ,@(when n-raw-data
	     `((setf (%instance-ref ,temp ,raw-index) ,n-raw-data)))
	 ,@(mapcar (lambda (dsd value)
		     (multiple-value-bind (accessor index data)
			 (slot-accessor-form defstruct dsd temp n-raw-data)
		       `(setf (,accessor ,data ,index) ,value)))
		   (dd-slots defstruct)
		   values)
	 ,temp))))
(defun create-fin-constructor
       (defstruct cons-name arglist vars types values)
  (let ((temp (gensym)))
    `(defun ,cons-name ,arglist
       (declare ,@(mapcar #'(lambda (var type) `(type ,type ,var))
			  vars types))
       (let ((,temp (truly-the
		     ,(dd-name defstruct)
		     (%make-funcallable-instance
		      ,(dd-length defstruct)
		      (%delayed-get-compiler-layout ,(dd-name defstruct))))))
	 ,@(mapcar #'(lambda (dsd value)
		       `(setf (%funcallable-instance-info
			       ,temp ,(dsd-index dsd))
			      ,value))
		   (dd-slots defstruct) values)
	 ,temp))))

;;; Create a default (non-BOA) keyword constructor.
(defun create-keyword-constructor (defstruct creator)
  (collect ((arglist (list '&key))
	    (types)
	    (vals))
    (dolist (slot (dd-slots defstruct))
      (let ((dum (gensym))
	    (name (dsd-name slot)))
	(arglist `((,(intern (string name) "KEYWORD") ,dum)
		   ,(dsd-default slot)))
	(types (dsd-type slot))
	(vals dum)))
    (funcall creator
	     defstruct (dd-default-constructor defstruct)
	     (arglist) (vals) (types) (vals))))

;;; Given a structure and a BOA constructor spec, call CREATOR with
;;; the appropriate args to make a constructor.
(defun create-boa-constructor (defstruct boa creator)
  (multiple-value-bind (req opt restp rest keyp keys allowp aux)
      (sb!kernel:parse-lambda-list (second boa))
    (collect ((arglist)
	      (vars)
	      (types))
      (labels ((get-slot (name)
		 (let ((res (find name (dd-slots defstruct)
				  :test #'string=
				  :key #'dsd-name)))
		   (if res
		       (values (dsd-type res) (dsd-default res))
		       (values t nil))))
	       (do-default (arg)
		 (multiple-value-bind (type default) (get-slot arg)
		   (arglist `(,arg ,default))
		   (vars arg)
		   (types type))))
	(dolist (arg req)
	  (arglist arg)
	  (vars arg)
	  (types (get-slot arg)))
	
	(when opt
	  (arglist '&optional)
	  (dolist (arg opt)
	    (cond ((consp arg)
		   (destructuring-bind
		       (name &optional (def (nth-value 1 (get-slot name))))
		       arg
		     (arglist `(,name ,def))
		     (vars name)
		     (types (get-slot name))))
		  (t
		   (do-default arg)))))

	(when restp
	  (arglist '&rest rest)
	  (vars rest)
	  (types 'list))

	(when keyp
	  (arglist '&key)
	  (dolist (key keys)
	    (if (consp key)
		(destructuring-bind (wot &optional (def nil def-p)) key
		  (let ((name (if (consp wot)
				  (destructuring-bind (key var) wot
				    (declare (ignore key))
				    var)
				  wot)))
		    (multiple-value-bind (type slot-def) (get-slot name)
		      (arglist `(,wot ,(if def-p def slot-def)))
		      (vars name)
		      (types type))))
		(do-default key))))

	(when allowp (arglist '&allow-other-keys))

	(when aux
	  (arglist '&aux)
	  (dolist (arg aux)
	    (let* ((arg (if (consp arg) arg (list arg)))
		   (var (first arg)))
	      (arglist arg)
	      (vars var)
	      (types (get-slot var))))))

      (funcall creator defstruct (first boa)
	       (arglist) (vars) (types)
	       (mapcar #'(lambda (slot)
			   (or (find (dsd-name slot) (vars) :test #'string=)
			       (dsd-default slot)))
		       (dd-slots defstruct))))))

;;; Grovel the constructor options, and decide what constructors (if
;;; any) to create.
(defun constructor-definitions (defstruct)
  (let ((no-constructors nil)
	(boas ())
	(defaults ())
	(creator (ecase (dd-type defstruct)
		   (structure #'create-structure-constructor)
		   (funcallable-structure #'create-fin-constructor)
		   (vector #'create-vector-constructor)
		   (list #'create-list-constructor))))
    (dolist (constructor (dd-constructors defstruct))
      (destructuring-bind (name &optional (boa-ll nil boa-p)) constructor
	(declare (ignore boa-ll))
	(cond ((not name) (setq no-constructors t))
	      (boa-p (push constructor boas))
	      (t (push name defaults)))))

    (when no-constructors
      (when (or defaults boas)
	(error "(:CONSTRUCTOR NIL) combined with other :CONSTRUCTORs"))
      (return-from constructor-definitions ()))

    (unless (or defaults boas)
      (push (concat-pnames 'make- (dd-name defstruct)) defaults))

    (collect ((res))
      (when defaults
	(let ((cname (first defaults)))
	  (setf (dd-default-constructor defstruct) cname)
	  (res (create-keyword-constructor defstruct creator))
	  (dolist (other-name (rest defaults))
	    (res `(setf (fdefinition ',other-name) (fdefinition ',cname)))
	    (res `(declaim (ftype function ',other-name))))))

      (dolist (boa boas)
	(res (create-boa-constructor defstruct boa creator)))

      (res))))

;;;; compiler stuff

;;; Like PROCLAIM-AS-FUNCTION-NAME, but we also set the kind to
;;; :DECLARED and blow away any ASSUMED-TYPE. Also, if the thing is a
;;; slot accessor currently, quietly unaccessorize it. And if there
;;; are any undefined warnings, we nuke them.
(defun proclaim-as-defstruct-function-name (name)
  (when name
    (when (info :function :accessor-for name)
      (setf (info :function :accessor-for name) nil))
    (proclaim-as-function-name name)
    (note-name-defined name :function)
    (setf (info :function :where-from name) :declared)
    (when (info :function :assumed-type name)
      (setf (info :function :assumed-type name) nil)))
  (values))

;;;; finalizing bootstrapping

;;; early structure placeholder definitions: Set up layout and class
;;; data for structures which are needed early.
(dolist (args
	 '#.(sb-cold:read-from-file
	     "src/code/early-defstruct-args.lisp-expr"))
  (let* ((defstruct (parse-name-and-options-and-slot-descriptions
		     (first args)
		     (rest args)))
	 (inherits (inherits-for-structure defstruct)))
    (function-%compiler-only-defstruct defstruct inherits)))
