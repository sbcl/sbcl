;;;; This file contains structures and functions for the maintenance of
;;;; basic information about defined types. Different object systems
;;;; can be supported simultaneously. Some of the functions here are
;;;; nominally generic, and are overwritten when CLOS is loaded.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(!begin-collecting-cold-init-forms)

;;;; the CLASS structure

;;; The CLASS structure is a supertype of all class types. A CLASS is
;;; also a CTYPE structure as recognized by the type system.
(def!struct (;; FIXME: Yes, these #+SB-XC/#-SB-XC conditionals are
	     ;; pretty hairy. I'm considering cleaner ways to rewrite
	     ;; the whole build system to avoid these (and other hacks
	     ;; too, e.g. UNCROSS) but I'm not sure yet that I've got
	     ;; it figured out. -- WHN 19990729
	     #-sb-xc sb!xc:class
	     #+sb-xc cl:class
	     (:make-load-form-fun class-make-load-form-fun)
	     (:include ctype
		       (:class-info (type-class-or-lose #-sb-xc 'sb!xc:class
							#+sb-xc 'cl:class)))
	     (:constructor nil)
	     #-no-ansi-print-object
	     (:print-object
	      (lambda (class stream)
		(let ((name (sb!xc:class-name class)))
		  (print-unreadable-object (class stream
						  :type t
						  :identity (not name))
		    (format stream
			    ;; FIXME: Make sure that this prints
			    ;; reasonably for anonymous classes.
			    "~:[anonymous~;~:*~S~]~@[ (~(~A~))~]"
			    name
			    (class-state class))))))
	     #-sb-xc-host (:pure nil))
  ;; the value to be returned by CLASS-NAME. (CMU CL used the raw slot
  ;; accessor for this slot directly as the definition of
  ;; CL:CLASS-NAME, but that was slightly wrong, because ANSI says
  ;; that CL:CLASS-NAME is a generic function.)
  (%name nil :type symbol)
  ;; the current layout for this class, or NIL if none assigned yet
  (layout nil :type (or sb!kernel::layout null))
  ;; How sure are we that this class won't be redefined?
  ;;   :READ-ONLY = We are committed to not changing the effective 
  ;;                slots or superclasses.
  ;;   :SEALED    = We can't even add subclasses.
  ;;   NIL        = Anything could happen.
  (state nil :type (member nil :read-only :sealed))
  ;; direct superclasses of this class
  (direct-superclasses () :type list)
  ;; representation of all of the subclasses (direct or indirect) of
  ;; this class. This is NIL if no subclasses or not initalized yet;
  ;; otherwise, it's an EQ hash-table mapping CL:CLASS objects to the
  ;; subclass layout that was in effect at the time the subclass was
  ;; created.
  (subclasses nil :type (or null hash-table))
  ;; the PCL class object for this class, or NIL if none assigned yet
  (pcl-class nil))

;;; KLUDGE: ANSI says this is a generic function, but we need it for
;;; bootstrapping before CLOS exists, so we define it as an ordinary
;;; function and let CLOS code overwrite it later. -- WHN ca. 19990815
(defun sb!xc:class-name (class)
  (class-%name class))

(defun class-make-load-form-fun (class)
  (/show "entering CLASS-MAKE-LOAD-FORM-FUN" class)
  (let ((name (sb!xc:class-name class)))
    (unless (and name (eq (sb!xc:find-class name nil) class))
      (/show "anonymous/undefined class case")
      (error "can't use anonymous or undefined class as constant:~%  ~S"
	     class))
    `(locally
       ;; KLUDGE: There's a FIND-CLASS DEFTRANSFORM for constant class
       ;; names which creates fast but non-cold-loadable, non-compact
       ;; code. In this context, we'd rather have compact,
       ;; cold-loadable code. -- WHN 19990928
       (declare (notinline sb!xc:find-class))
       (sb!xc:find-class ',name))))

;;;; basic LAYOUT stuff

;;; Note: This bound is set somewhat less than MOST-POSITIVE-FIXNUM
;;; in order to guarantee that several hash values can be added without
;;; overflowing into a bignum.
(defconstant layout-clos-hash-max (ash most-positive-fixnum -3)
  #!+sb-doc
  "the inclusive upper bound on LAYOUT-CLOS-HASH values")

;;; a list of conses, initialized by genesis
;;;
;;; In each cons, the car is the symbol naming the layout, and the
;;; cdr is the layout itself.
(defvar *!initial-layouts*)

;;; a table mapping class names to layouts for classes we have
;;; referenced but not yet loaded. This is initialized from an alist
;;; created by genesis describing the layouts that genesis created at
;;; cold-load time.
(defvar *forward-referenced-layouts*)
(!cold-init-forms
  (setq *forward-referenced-layouts* (make-hash-table :test 'equal))
  #-sb-xc-host (progn
		 (/show0 "processing *!INITIAL-LAYOUTS*")
		 (dolist (x *!initial-layouts*)
		   (setf (gethash (car x) *forward-referenced-layouts*)
			 (cdr x)))
		 (/show0 "done processing *!INITIAL-LAYOUTS*")))

;;; The LAYOUT structure is pointed to by the first cell of instance
;;; (or structure) objects. It represents what we need to know for
;;; type checking and garbage collection. Whenever a class is
;;; incompatibly redefined, a new layout is allocated. If two object's
;;; layouts are EQ, then they are exactly the same type.
;;;
;;; KLUDGE: The genesis code has raw offsets of slots in this
;;; structure hardwired into it. It would be good to rewrite that code
;;; so that it looks up those offsets in the compiler's tables, but
;;; for now if you change this structure, lucky you, you get to grovel
;;; over the genesis code by hand.:-( -- WHN 19990820
(def!struct (layout
	     ;; KLUDGE: A special hack keeps this from being
	     ;; called when building code for the
	     ;; cross-compiler. See comments at the DEFUN for
	     ;; this. -- WHN 19990914
	     (:make-load-form-fun #-sb-xc-host ignore-it
				  ;; KLUDGE: DEF!STRUCT at #+SB-XC-HOST
				  ;; time controls both the
				  ;; build-the-cross-compiler behavior
				  ;; and the run-the-cross-compiler
				  ;; behavior. The value below only
				  ;; works for build-the-cross-compiler.
				  ;; There's a special hack in
				  ;; EMIT-MAKE-LOAD-FORM which gives
				  ;; effectively IGNORE-IT behavior for
				  ;; LAYOUT at run-the-cross-compiler
				  ;; time. It would be cleaner to
				  ;; actually have an IGNORE-IT value
				  ;; stored, but it's hard to see how to
				  ;; do that concisely with the current
				  ;; DEF!STRUCT setup. -- WHN 19990930
				  #+sb-xc-host
				  make-load-form-for-layout))
  ;; hash bits which should be set to constant pseudo-random values
  ;; for use by CLOS. Sleazily accessed via %INSTANCE-REF, see
  ;; LAYOUT-CLOS-HASH.
  ;;
  ;; FIXME: We should get our story straight on what the type of these
  ;; values is. (declared INDEX here, described as <=
  ;; LAYOUT-CLOS-HASH-MAX by the doc string of that constant,
  ;; generated as strictly positive in RANDOM-LAYOUT-CLOS-HASH..)
  ;;
  ;; KLUDGE: The fact that the slots here start at offset 1 is known
  ;; to the LAYOUT-CLOS-HASH function and to the LAYOUT-dumping code
  ;; in GENESIS.
  (clos-hash-0 (random-layout-clos-hash) :type index)
  (clos-hash-1 (random-layout-clos-hash) :type index)
  (clos-hash-2 (random-layout-clos-hash) :type index)
  (clos-hash-3 (random-layout-clos-hash) :type index)
  (clos-hash-4 (random-layout-clos-hash) :type index)
  (clos-hash-5 (random-layout-clos-hash) :type index)
  (clos-hash-6 (random-layout-clos-hash) :type index)
  (clos-hash-7 (random-layout-clos-hash) :type index)
  ;; the class that this is a layout for
  (class (required-argument)
	 ;; FIXME: Do we really know this is a CL:CLASS? Mightn't it
	 ;; be a SB-PCL:CLASS under some circumstances? What goes here
	 ;; when the LAYOUT is in fact a PCL::WRAPPER?
	 :type #-sb-xc sb!xc:class #+sb-xc cl:class)
  ;; The value of this slot can be:
  ;;   * :UNINITIALIZED if not initialized yet;
  ;;   * NIL if this is the up-to-date layout for a class; or
  ;;   * T if this layout has been invalidated (by being replaced by 
  ;;     a new, more-up-to-date LAYOUT).
  ;;   * something else (probably a list) if the class is a PCL wrapper
  ;;     and PCL has made it invalid and made a note to itself about it
  (invalid :uninitialized :type (or cons (member nil t :uninitialized)))
  ;; the layouts for all classes we inherit. If hierarchical, i.e. if
  ;; DEPTHOID >= 0, then these are ordered by ORDER-LAYOUT-INHERITS,
  ;; so that each inherited layout appears at its expected depth,
  ;; i.e. at its LAYOUT-DEPTHOID value.
  ;;
  ;; Remaining elements are filled by the non-hierarchical layouts or,
  ;; if they would otherwise be empty, by copies of succeeding layouts.
  (inherits #() :type simple-vector)
  ;; If inheritance is not hierarchical, this is -1. If inheritance is 
  ;; hierarchical, this is the inheritance depth, i.e. (LENGTH INHERITS).
  ;; Note:
  ;;  (1) This turns out to be a handy encoding for arithmetically
  ;;      comparing deepness; it is generally useful to do a bare numeric
  ;;      comparison of these depthoid values, and we hardly ever need to
  ;;      test whether the values are negative or not.
  ;;  (2) This was called INHERITANCE-DEPTH in classic CMU CL. It was
  ;;      renamed because some of us find it confusing to call something
  ;;      a depth when it isn't quite.
  (depthoid -1 :type layout-depthoid)
  ;; The number of top-level descriptor cells in each instance.
  (length 0 :type index)
  ;; If this layout has some kind of compiler meta-info, then this is
  ;; it. If a structure, then we store the DEFSTRUCT-DESCRIPTION here.
  (info nil)
  ;; This is true if objects of this class are never modified to
  ;; contain dynamic pointers in their slots or constant-like
  ;; substructure (and hence can be copied into read-only space by
  ;; PURIFY).
  ;;
  ;; KLUDGE: This slot is known to the C runtime support code.
  (pure nil :type (member t nil 0)))

(def!method print-object ((layout layout) stream)
  (print-unreadable-object (layout stream :type t :identity t)
    (format stream
	    "for ~S~@[, INVALID=~S~]"
	    (layout-proper-name layout)
	    (layout-invalid layout))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun layout-proper-name (layout)
    (class-proper-name (layout-class layout))))

;;;; support for the hash values used by CLOS when working with LAYOUTs

(defconstant layout-clos-hash-length 8)
#!-sb-fluid (declaim (inline layout-clos-hash))
(defun layout-clos-hash (layout i)
  ;; FIXME: Either this I should be declared to be `(MOD
  ;; ,LAYOUT-CLOS-HASH-LENGTH), or this is used in some inner loop
  ;; where we can't afford to check that kind of thing and therefore
  ;; should have some insane level of optimization. (This is true both
  ;; of this function and of the SETF function below.)
  (declare (type layout layout) (type index i))
  ;; FIXME: LAYOUT slots should have type `(MOD ,LAYOUT-CLOS-HASH-MAX),
  ;; not INDEX.
  (truly-the index (%instance-ref layout (1+ i))))
#!-sb-fluid (declaim (inline (setf layout-clos-hash)))
(defun (setf layout-clos-hash) (new-value layout i)
  (declare (type layout layout) (type index new-value i))
  (setf (%instance-ref layout (1+ i)) new-value))

;;; a generator for random values suitable for the CLOS-HASH slots of
;;; LAYOUTs. We use our own RANDOM-STATE here because we'd like
;;; pseudo-random values to come the same way in the target even when
;;; we make minor changes to the system, in order to reduce the
;;; mysteriousness of possible CLOS bugs.
(defvar *layout-clos-hash-random-state*)
(defun random-layout-clos-hash ()
  ;; FIXME: I'm not sure why this expression is (1+ (RANDOM FOO)),
  ;; returning a strictly positive value. I copied it verbatim from
  ;; CMU CL INITIALIZE-LAYOUT-HASH, so presumably it works, but I
  ;; dunno whether the hash values are really supposed to be 1-based.
  ;; They're declared as INDEX.. Or is this a hack to try to avoid
  ;; having to use bignum arithmetic? Or what? An explanation would be
  ;; nice.
  (1+ (random layout-clos-hash-max
	      (if (boundp '*layout-clos-hash-random-state*)
		  *layout-clos-hash-random-state*
		  (setf *layout-clos-hash-random-state*
			(make-random-state))))))

;;; If we can't find any existing layout, then we create a new one
;;; storing it in *FORWARD-REFERENCED-LAYOUTS*. In classic CMU CL, we
;;; used to immediately check for compatibility, but for
;;; cross-compilability reasons (i.e. convenience of using this
;;; function in a MAKE-LOAD-FORM expression) that functionality has
;;; been split off into INIT-OR-CHECK-LAYOUT.
(declaim (ftype (function (symbol) layout) find-layout))
(defun find-layout (name)
  (let ((class (sb!xc:find-class name nil)))
    (or (and class (class-layout class))
	(gethash name *forward-referenced-layouts*)
	(setf (gethash name *forward-referenced-layouts*)
	      (make-layout :class (or class (make-undefined-class name)))))))

;;; If LAYOUT is uninitialized, initialize it with CLASS, LENGTH,
;;; INHERITS, and DEPTHOID, otherwise require that it be consistent
;;; with CLASS, LENGTH, INHERITS, and DEPTHOID.
;;;
;;; UNDEFINED-CLASS values are interpreted specially as "we don't know
;;; anything about the class", so if LAYOUT is initialized, any
;;; preexisting class slot value is OK, and if it's not initialized,
;;; its class slot value is set to an UNDEFINED-CLASS. -- FIXME: This
;;; is no longer true, :UNINITIALIZED used instead.
(declaim (ftype (function (layout sb!xc:class index simple-vector layout-depthoid) layout)
		init-or-check-layout))
(defun init-or-check-layout (layout class length inherits depthoid)
  (cond ((eq (layout-invalid layout) :uninitialized)
	 ;; There was no layout before, we just created one which
	 ;; we'll now initialize with our information.
	 (setf (layout-length layout) length
	       (layout-inherits layout) inherits
	       (layout-depthoid layout) depthoid
	       (layout-class layout) class
	       (layout-invalid layout) nil))
	;; FIXME: Now that LAYOUTs are born :UNINITIALIZED, maybe this
	;; clause is not needed?
	((not *type-system-initialized*)
	 (setf (layout-class layout) class))
	(t
	 ;; There was an old layout already initialized with old
	 ;; information, and we'll now check that old information
	 ;; which was known with certainty is consistent with current
	 ;; information which is known with certainty.
	 (check-layout layout class length inherits depthoid)))
  layout)

;;; In code for the target Lisp, we don't use dump LAYOUTs using the
;;; standard load form mechanism, we use special fops instead, in
;;; order to make cold load come out right. But when we're building
;;; the cross-compiler, we can't do that because we don't have access
;;; to special non-ANSI low-level things like special fops, and we
;;; don't need to do that anyway because our code isn't going to be
;;; cold loaded, so we use the ordinary load form system.
;;;
;;; KLUDGE: A special hack causes this not to be called when we are
;;; building code for the target Lisp. It would be tidier to just not
;;; have it in place when we're building the target Lisp, but it
;;; wasn't clear how to do that without rethinking DEF!STRUCT quite a
;;; bit, so I punted. -- WHN 19990914
#+sb-xc-host
(defun make-load-form-for-layout (layout &optional env)
  (declare (type layout layout))
  (declare (ignore env))
  (when (layout-invalid layout)
    (compiler-error "can't dump reference to obsolete class: ~S"
		    (layout-class layout)))
  (let ((name (sb!xc:class-name (layout-class layout))))
    (unless name
      (compiler-error "can't dump anonymous LAYOUT: ~S" layout))
    ;; Since LAYOUT refers to a class which refers back to the LAYOUT,
    ;; we have to do this in two stages, like the TREE-WITH-PARENT
    ;; example in the MAKE-LOAD-FORM entry in the ANSI spec.
    (values
     ;; "creation" form (which actually doesn't create a new LAYOUT if
     ;; there's a preexisting one with this name)
     `(find-layout ',name)
     ;; "initialization" form (which actually doesn't initialize
     ;; preexisting LAYOUTs, just checks that they're consistent).
     `(init-or-check-layout ',layout
			    ',(layout-class layout)
			    ',(layout-length layout)
			    ',(layout-inherits layout)
			    ',(layout-depthoid layout)))))

;;; If LAYOUT's slot values differ from the specified slot values in
;;; any interesting way, then give a warning and return T.
(declaim (ftype (function (simple-string
			   layout
			   simple-string
			   index
			   simple-vector
			   layout-depthoid))
		redefine-layout-warning))
(defun redefine-layout-warning (old-context old-layout
				context length inherits depthoid)
  (declare (type layout old-layout) (type simple-string old-context context))
  (let ((name (layout-proper-name old-layout)))
    (or (let ((old-inherits (layout-inherits old-layout)))
	  (or (when (mismatch old-inherits
			      inherits
			      :key #'layout-proper-name)
		(warn "change in superclasses of class ~S:~%  ~
		       ~A superclasses: ~S~%  ~
		       ~A superclasses: ~S"
		      name
		      old-context
		      (map 'list #'layout-proper-name old-inherits)
		      context
		      (map 'list #'layout-proper-name inherits))
		t)
	      (let ((diff (mismatch old-inherits inherits)))
		(when diff
		  (warn
		   "in class ~S:~%  ~
		    ~:(~A~) definition of superclass ~S is incompatible with~%  ~
		    ~A definition."
		   name
		   old-context
		   (layout-proper-name (svref old-inherits diff))
		   context)
		  t))))
	(let ((old-length (layout-length old-layout)))
	  (unless (= old-length length)
	    (warn "change in instance length of class ~S:~%  ~
		   ~A length: ~D~%  ~
		   ~A length: ~D"
		  name
		  old-context old-length
		  context length)
	    t))
	(unless (= (layout-depthoid old-layout) depthoid)
	  (warn "change in the inheritance structure of class ~S~%  ~
		 between the ~A definition and the ~A definition"
		name old-context context)
	  t))))

;;; Require that LAYOUT data be consistent with CLASS, LENGTH,
;;; INHERITS, and DEPTHOID.
(declaim (ftype (function (layout sb!xc:class index simple-vector layout-depthoid))
		check-layout))
(defun check-layout (layout class length inherits depthoid)
  (aver (eq (layout-class layout) class))
  (when (redefine-layout-warning "current" layout
				 "compile time" length inherits depthoid)
    ;; Classic CMU CL had more options here. There are several reasons
    ;; why they might want more options which are less appropriate for
    ;; us: (1) It's hard to fit the classic CMU CL flexible approach
    ;; into the ANSI-style MAKE-LOAD-FORM system, and having a
    ;; non-MAKE-LOAD-FORM-style system is painful when we're trying to
    ;; make the cross-compiler run under vanilla ANSI Common Lisp. (2)
    ;; We have CLOS now, and if you want to be able to flexibly
    ;; redefine classes without restarting the system, it'd make sense
    ;; to use that, so supporting complexity in order to allow
    ;; modifying DEFSTRUCTs without restarting the system is a low
    ;; priority. (3) We now have the ability to rebuild the SBCL
    ;; system from scratch, so we no longer need this functionality in
    ;; order to maintain the SBCL system by modifying running images.
    (error "The class ~S was not changed, and there's no guarantee that~@
	    the loaded code (which expected another layout) will work."
	   (layout-proper-name layout)))
  (values))

;;; a common idiom (the same as CMU CL FIND-LAYOUT) rolled up into a
;;; single function call
;;;
;;; Used by the loader to forward-reference layouts for classes whose
;;; definitions may not have been loaded yet. This allows type tests
;;; to be loaded when the type definition hasn't been loaded yet.
(declaim (ftype (function (symbol index simple-vector layout-depthoid) layout)
		find-and-init-or-check-layout))
(defun find-and-init-or-check-layout (name length inherits depthoid)
  (/show0 "entering FIND-AND-INIT-OR-CHECK-LAYOUT")
  (let ((layout (find-layout name)))
    (init-or-check-layout layout
			  (or (sb!xc:find-class name nil)
			      (make-undefined-class name))
			  length
			  inherits
			  depthoid)))

;;; Record LAYOUT as the layout for its class, adding it as a subtype
;;; of all superclasses. This is the operation that "installs" a
;;; layout for a class in the type system, clobbering any old layout.
;;; However, this does not modify the class namespace; that is a
;;; separate operation (think anonymous classes.)
;;; -- If INVALIDATE, then all the layouts for any old definition
;;;    and subclasses are invalidated, and the SUBCLASSES slot is cleared.
;;; -- If DESTRUCT-LAYOUT, then this is some old layout, and is to be
;;;    destructively modified to hold the same type information.
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
(defun register-layout (layout &key (invalidate t) destruct-layout)
  (declare (type layout layout) (type (or layout null) destruct-layout))
  (let* ((class (layout-class layout))
	 (class-layout (class-layout class))
	 (subclasses (class-subclasses class)))

    ;; Attempting to register ourselves with a temporary undefined
    ;; class placeholder is almost certainly a programmer error. (I
    ;; should know, I did it.) -- WHN 19990927
    (aver (not (undefined-class-p class)))

    ;; This assertion dates from classic CMU CL. The rationale is
    ;; probably that calling REGISTER-LAYOUT more than once for the
    ;; same LAYOUT is almost certainly a programmer error.
    (aver (not (eq class-layout layout)))

    ;; Figure out what classes are affected by the change, and issue
    ;; appropriate warnings and invalidations.
    (when class-layout
      (modify-class class)
      (when subclasses
	(dohash (subclass subclass-layout subclasses)
	  (modify-class subclass)
	  (when invalidate
	    (invalidate-layout subclass-layout))))
      (when invalidate
	(invalidate-layout class-layout)
	(setf (class-subclasses class) nil)))

    (if destruct-layout
	(setf (layout-invalid destruct-layout) nil
	      (layout-inherits destruct-layout) (layout-inherits layout)
	      (layout-depthoid destruct-layout)(layout-depthoid layout)
	      (layout-length destruct-layout) (layout-length layout)
	      (layout-info destruct-layout) (layout-info layout)
	      (class-layout class) destruct-layout)
	(setf (layout-invalid layout) nil
	      (class-layout class) layout))

    (let ((inherits (layout-inherits layout)))
      (dotimes (i (length inherits)) ; FIXME: should be DOVECTOR
	(let* ((super (layout-class (svref inherits i)))
	       (subclasses (or (class-subclasses super)
			       (setf (class-subclasses super)
				     (make-hash-table :test 'eq)))))
	  (when (and (eq (class-state super) :sealed)
		     (not (gethash class subclasses)))
	    (warn "unsealing sealed class ~S in order to subclass it"
		  (sb!xc:class-name super))
	    (setf (class-state super) :read-only))
	  (setf (gethash class subclasses)
		(or destruct-layout layout))))))

  (values))
); EVAL-WHEN

;;; Arrange the inherited layouts to appear at their expected depth,
;;; ensuring that hierarchical type tests succeed. Layouts with 
;;; DEPTHOID >= 0 (i.e. hierarchical classes) are placed first,
;;; at exactly that index in the INHERITS vector. Then, non-hierarchical
;;; layouts are placed in remaining elements. Then, any still-empty
;;; elements are filled with their successors, ensuring that each
;;; element contains a valid layout.
;;;
;;; This reordering may destroy CPL ordering, so the inherits should
;;; not be read as being in CPL order.
(defun order-layout-inherits (layouts)
  (declare (simple-vector layouts))
  (let ((length (length layouts))
	(max-depth -1))
    (dotimes (i length)
      (let ((depth (layout-depthoid (svref layouts i))))
	(when (> depth max-depth)
	  (setf max-depth depth))))
    (let* ((new-length (max (1+ max-depth) length))
	   (inherits (make-array new-length)))
      (dotimes (i length)
	(let* ((layout (svref layouts i))
	       (depth (layout-depthoid layout)))
	  (unless (eql depth -1)
	    (let ((old-layout (svref inherits depth)))
	      (unless (or (eql old-layout 0) (eq old-layout layout))
		(error "layout depth conflict: ~S~%" layouts)))
	    (setf (svref inherits depth) layout))))
      (do ((i 0 (1+ i))
	   (j 0))
	  ((>= i length))
	(declare (type index i j))
	(let* ((layout (svref layouts i))
	       (depth (layout-depthoid layout)))
	  (when (eql depth -1)
	    (loop (when (eql (svref inherits j) 0)
		    (return))
		  (incf j))
	    (setf (svref inherits j) layout))))
      (do ((i (1- new-length) (1- i)))
	  ((< i 0))
	(declare (type fixnum i))
	(when (eql (svref inherits i) 0)
	  (setf (svref inherits i) (svref inherits (1+ i)))))
      inherits)))

;;;; class precedence lists

;;; Topologically sort the list of objects to meet a set of ordering
;;; constraints given by pairs (A . B) constraining A to precede B.
;;; When there are multiple objects to choose, the tie-breaker
;;; function is called with both the list of object to choose from and
;;; the reverse ordering built so far.
(defun topological-sort (objects constraints tie-breaker)
  (declare (list objects constraints)
	   (function tie-breaker))
  (let ((obj-info (make-hash-table :size (length objects)))
	(free-objs nil)
	(result nil))
    (dolist (constraint constraints)
      (let ((obj1 (car constraint))
	    (obj2 (cdr constraint)))
	(let ((info2 (gethash obj2 obj-info)))
	  (if info2
	      (incf (first info2))
	      (setf (gethash obj2 obj-info) (list 1))))
	(let ((info1 (gethash obj1 obj-info)))
	  (if info1
	      (push obj2 (rest info1))
	      (setf (gethash obj1 obj-info) (list 0 obj2))))))
    (dolist (obj objects)
      (let ((info (gethash obj obj-info)))
	(when (or (not info) (zerop (first info)))
	  (push obj free-objs))))
    (loop
     (flet ((next-result (obj)
	      (push obj result)
	      (dolist (successor (rest (gethash obj obj-info)))
		(let* ((successor-info (gethash successor obj-info))
		       (count (1- (first successor-info))))
		  (setf (first successor-info) count)
		  (when (zerop count)
		    (push successor free-objs))))))
       (cond ((endp free-objs)
	      (dohash (obj info obj-info)
		(unless (zerop (first info))
		  (error "Topological sort failed due to constraint on ~S."
			 obj)))
	      (return (nreverse result)))
	     ((endp (rest free-objs))
	      (next-result (pop free-objs)))
	     (t
	      (let ((obj (funcall tie-breaker free-objs result)))
		(setf free-objs (remove obj free-objs))
		(next-result obj))))))))


;;; standard class precedence list computation
(defun std-compute-class-precedence-list (class)
  (let ((classes nil)
	(constraints nil))
    (labels ((note-class (class)
	       (unless (member class classes)
		 (push class classes)
		 (let ((superclasses (class-direct-superclasses class)))
		   (do ((prev class)
			(rest superclasses (rest rest)))
		       ((endp rest))
		     (let ((next (first rest)))
		       (push (cons prev next) constraints)
		       (setf prev next)))
		   (dolist (class superclasses)
		     (note-class class)))))
	     (std-cpl-tie-breaker (free-classes rev-cpl)
	       (dolist (class rev-cpl (first free-classes))
		 (let* ((superclasses (class-direct-superclasses class))
			(intersection (intersection free-classes
						    superclasses)))
		   (when intersection
		     (return (first intersection)))))))
      (note-class class)
      (topological-sort classes constraints #'std-cpl-tie-breaker))))

;;;; object types to represent classes

;;; An UNDEFINED-CLASS is a cookie we make up to stick in forward
;;; referenced layouts. Users should never see them.
(def!struct (undefined-class (:include #-sb-xc sb!xc:class
				       #+sb-xc cl:class)
			     (:constructor make-undefined-class (%name))))

;;; BUILT-IN-CLASS is used to represent the standard classes that
;;; aren't defined with DEFSTRUCT and other specially implemented
;;; primitive types whose only attribute is their name.
;;;
;;; Some BUILT-IN-CLASSes have a TRANSLATION, which means that they
;;; are effectively DEFTYPE'd to some other type (usually a union of
;;; other classes or a "primitive" type such as NUMBER, ARRAY, etc.)
;;; This translation is done when type specifiers are parsed. Type
;;; system operations (union, subtypep, etc.) should never encounter
;;; translated classes, only their translation.
(def!struct (sb!xc:built-in-class (:include #-sb-xc sb!xc:class
					    #+sb-xc cl:class)
				  (:constructor bare-make-built-in-class))
  ;; the type we translate to on parsing. If NIL, then this class
  ;; stands on its own; or it can be set to :INITIALIZING for a period
  ;; during cold-load.
  (translation nil :type (or ctype (member nil :initializing))))
(defun make-built-in-class (&rest rest)
  (apply #'bare-make-built-in-class
	 (rename-key-args '((:name :%name)) rest)))

;;; FIXME: In CMU CL, this was a class with a print function, but not
;;; necessarily a structure class (e.g. CONDITIONs). In SBCL,
;;; we let CLOS handle our print functions, so that is no longer needed.
;;; Is there any need for this class any more?
(def!struct (slot-class (:include #-sb-xc sb!xc:class #+sb-xc cl:class)
			(:constructor nil)))

;;; STRUCTURE-CLASS represents what we need to know about structure
;;; classes. Non-structure "typed" defstructs are a special case, and
;;; don't have a corresponding class.
(def!struct (basic-structure-class (:include slot-class)
				   (:constructor nil)))

(def!struct (sb!xc:structure-class (:include basic-structure-class)
				   (:constructor bare-make-structure-class))
  ;; If true, a default keyword constructor for this structure.
  (constructor nil :type (or function null)))
(defun make-structure-class (&rest rest)
  (apply #'bare-make-structure-class
	 (rename-key-args '((:name :%name)) rest)))

;;; FUNCALLABLE-STRUCTURE-CLASS is used to represent funcallable
;;; structures, which are used to implement generic functions.
(def!struct (funcallable-structure-class (:include basic-structure-class)
					 (:constructor bare-make-funcallable-structure-class)))
(defun make-funcallable-structure-class (&rest rest)
  (apply #'bare-make-funcallable-structure-class
	 (rename-key-args '((:name :%name)) rest)))

;;;; class namespace

;;; We use an indirection to allow forward referencing of class
;;; definitions with load-time resolution.
(def!struct (class-cell
	     (:constructor make-class-cell (name &optional class))
	     (:make-load-form-fun (lambda (c)
				    `(find-class-cell ',(class-cell-name c))))
	     #-no-ansi-print-object
	     (:print-object (lambda (s stream)
			      (print-unreadable-object (s stream :type t)
				(prin1 (class-cell-name s) stream)))))
  ;; Name of class we expect to find.
  (name nil :type symbol :read-only t)
  ;; Class or NIL if not yet defined.
  (class nil :type (or #-sb-xc sb!xc:class #+sb-xc cl:class
		       null)))
(defun find-class-cell (name)
  (or (info :type :class name)
      (setf (info :type :class name)
	    (make-class-cell name))))

;;; FIXME: When the system is stable, this DECLAIM FTYPE should
;;; probably go away in favor of the DEFKNOWN for FIND-CLASS.
(declaim (ftype (function (symbol &optional t (or null sb!c::lexenv))) sb!xc:find-class))
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
(defun sb!xc:find-class (name &optional (errorp t) environment)
  #!+sb-doc
  "Return the class with the specified NAME. If ERRORP is false, then NIL is
   returned when no such class exists."
  (declare (type symbol name) (ignore environment))
  (let ((res (class-cell-class (find-class-cell name))))
    (if (or res (not errorp))
	res
	(error "class not yet defined:~%  ~S" name))))
(defun (setf sb!xc:find-class) (new-value name)
  #-sb-xc (declare (type sb!xc:class new-value))
  (ecase (info :type :kind name)
    ((nil))
    (:instance
     (let ((old (class-of (sb!xc:find-class name)))
	   (new (class-of new-value)))
       (unless (eq old new)
	 (warn "changing meta-class of ~S from ~S to ~S"
	       name
	       (class-name old)
	       (class-name new)))))
    (:primitive
     (error "illegal to redefine standard type ~S" name))
    (:defined
     (warn "redefining DEFTYPE type to be a class: ~S" name)
     (setf (info :type :expander name) nil)))

  (remhash name *forward-referenced-layouts*)
  (%note-type-defined name)
  (setf (info :type :kind name) :instance)
  (setf (class-cell-class (find-class-cell name)) new-value)
  (unless (eq (info :type :compiler-layout name)
	      (class-layout new-value))
    (setf (info :type :compiler-layout name) (class-layout new-value)))
  new-value)
) ; EVAL-WHEN

;;; Called when we are about to define NAME as a class meeting some
;;; predicate (such as a meta-class type test.) The first result is
;;; always of the desired class. The second result is any existing
;;; LAYOUT for this name.
(defun insured-find-class (name predicate constructor)
  (declare (type function predicate constructor))
  (let* ((old (sb!xc:find-class name nil))
	 (res (if (and old (funcall predicate old))
		  old
		  (funcall constructor :name name)))
	 (found (or (gethash name *forward-referenced-layouts*)
		    (when old (class-layout old)))))
    (when found
      (setf (layout-class found) res))
    (values res found)))

;;; If the class has a proper name, return the name, otherwise return
;;; the class.
(defun class-proper-name (class)
  #-sb-xc (declare (type sb!xc:class class))
  (let ((name (sb!xc:class-name class)))
    (if (and name (eq (sb!xc:find-class name nil) class))
	name
	class)))

;;;; CLASS type operations

(!define-type-class sb!xc:class)

;;; Simple methods for TYPE= and SUBTYPEP should never be called when
;;; the two classes are equal, since there are EQ checks in those
;;; operations.
(!define-type-method (sb!xc:class :simple-=) (type1 type2)
  (aver (not (eq type1 type2)))
  (values nil t))

(!define-type-method (sb!xc:class :simple-subtypep) (class1 class2)
  (aver (not (eq class1 class2)))
  (let ((subclasses (class-subclasses class2)))
    (if (and subclasses (gethash class1 subclasses))
	(values t t)
	(values nil t))))

;;; When finding the intersection of a sealed class and some other
;;; class (not hierarchically related) the intersection is the union
;;; of the currently shared subclasses.
(defun sealed-class-intersection2 (sealed other)
  (declare (type sb!xc:class sealed other))
  (let ((s-sub (class-subclasses sealed))
	(o-sub (class-subclasses other)))
    (if (and s-sub o-sub)
	(collect ((res *empty-type* type-union))
	  (dohash (subclass layout s-sub)
	    (declare (ignore layout))
	    (when (gethash subclass o-sub)
	      (res (specifier-type subclass))))
	  (res))
	*empty-type*)))

(!define-type-method (sb!xc:class :simple-intersection2) (class1 class2)
  (declare (type sb!xc:class class1 class2))
  (cond ((eq class1 class2)
	 class1)
	;; If one is a subclass of the other, then that is the
	;; intersection.
	((let ((subclasses (class-subclasses class2)))
	   (and subclasses (gethash class1 subclasses)))
	 class1)
	((let ((subclasses (class-subclasses class1)))
	   (and subclasses (gethash class2 subclasses)))
	 class2)
	;; Otherwise, we can't in general be sure that the
	;; intersection is empty, since a subclass of both might be
	;; defined. But we can eliminate it for some special cases.
	((or (basic-structure-class-p class1)
	     (basic-structure-class-p class2))
	 ;; No subclass of both can be defined.
	 *empty-type*)
	((eq (class-state class1) :sealed)
	 ;; checking whether a subclass of both can be defined:
	 (sealed-class-intersection2 class1 class2))
	((eq (class-state class2) :sealed)
	 ;; checking whether a subclass of both can be defined:
	 (sealed-class-intersection2 class2 class1))
	(t
	 ;; uncertain, since a subclass of both might be defined
	 nil)))

(!define-type-method (sb!xc:class :unparse) (type)
  (class-proper-name type))

;;;; PCL stuff

(def!struct (std-class (:include sb!xc:class)
		       (:constructor nil)))
(def!struct (sb!xc:standard-class (:include std-class)
				  (:constructor bare-make-standard-class)))
(def!struct (random-pcl-class (:include std-class)
			      (:constructor bare-make-random-pcl-class)))
(defun make-standard-class (&rest rest)
  (apply #'bare-make-standard-class
	 (rename-key-args '((:name :%name)) rest)))
(defun make-random-pcl-class (&rest rest)
  (apply #'bare-make-random-pcl-class
	 (rename-key-args '((:name :%name)) rest)))

;;;; built-in classes

;;; The BUILT-IN-CLASSES list is a data structure which configures the
;;; creation of all the built-in classes. It contains all the info
;;; that we need to maintain the mapping between classes, compile-time
;;; types and run-time type codes. These options are defined:
;;;
;;; :TRANSLATION (default none)
;;;     When this class is "parsed" as a type specifier, it is
;;;     translated into the specified internal type representation,
;;;     rather than being left as a class. This is used for types
;;;     which we want to canonicalize to some other kind of type
;;;     object because in general we want to be able to include more
;;;     information than just the class (e.g. for numeric types.)
;;;
;;; :ENUMERABLE (default NIL)
;;;     The value of the :ENUMERABLE slot in the created class.
;;;     Meaningless in translated classes.
;;;
;;; :STATE (default :SEALED)
;;;     The value of CLASS-STATE which we want on completion,
;;;     indicating whether subclasses can be created at run-time.
;;;
;;; :HIERARCHICAL-P (default T unless any of the inherits are non-hierarchical)
;;;     True if we can assign this class a unique inheritance depth.
;;;
;;; :CODES (default none)
;;;     Run-time type codes which should be translated back to this
;;;     class by CLASS-OF. Unspecified for abstract classes.
;;;
;;; :INHERITS (default this class and T)
;;;     The class-precedence list for this class, with this class and
;;;     T implicit.
;;;
;;; :DIRECT-SUPERCLASSES (default to head of CPL)
;;;     List of the direct superclasses of this class.
;;;
;;; FIXME: This doesn't seem to be needed after cold init (and so can
;;; probably be uninterned at the end of cold init).
(defvar *built-in-classes*)
(!cold-init-forms
  (/show0 "setting *BUILT-IN-CLASSES*")
  (setq
   *built-in-classes*
   '((t :state :read-only :translation t)
     (character :enumerable t :translation base-char)
     (base-char :enumerable t
		:inherits (character)
		:codes (#.sb!vm:base-char-type))
     (symbol :codes (#.sb!vm:symbol-header-type))

     (instance :state :read-only)

     (system-area-pointer :codes (#.sb!vm:sap-type))
     (weak-pointer :codes (#.sb!vm:weak-pointer-type))
     (code-component :codes (#.sb!vm:code-header-type))
     (lra :codes (#.sb!vm:return-pc-header-type))
     (fdefn :codes (#.sb!vm:fdefn-type))
     (random-class) ; used for unknown type codes

     (function
      :codes (#.sb!vm:closure-header-type
	      #.sb!vm:simple-fun-header-type)
      :state :read-only)
     (funcallable-instance
      :inherits (function)
      :state :read-only)

     ;; FIXME: Are COLLECTION and MUTABLE-COLLECTION used for anything
     ;; any more? COLLECTION is not defined in ANSI Common Lisp..
     (collection :hierarchical-p nil :state :read-only)
     (mutable-collection :state :read-only
			 :inherits (collection))
     (generic-sequence :state :read-only
		       :inherits (collection))
     (mutable-sequence :state :read-only
		       :direct-superclasses (mutable-collection
					     generic-sequence)
		       :inherits (mutable-collection
				  generic-sequence
				  collection))
     (generic-array :state :read-only
		    :inherits (mutable-sequence
			       mutable-collection
			       generic-sequence
			       collection))
     (generic-vector :state :read-only
		     :inherits (generic-array
				mutable-sequence mutable-collection
				generic-sequence collection))
     (array :translation array :codes (#.sb!vm:complex-array-type)
	    :inherits (generic-array mutable-sequence mutable-collection
				     generic-sequence collection))
     (simple-array
      :translation simple-array :codes (#.sb!vm:simple-array-type)
      :inherits (array generic-array mutable-sequence mutable-collection
		 generic-sequence collection))
     (sequence
      :translation (or cons (member nil) vector)
      :inherits (mutable-sequence mutable-collection generic-sequence
		 collection))
     (vector
      :translation vector :codes (#.sb!vm:complex-vector-type)
      :direct-superclasses (array sequence generic-vector)
      :inherits (array sequence generic-vector generic-array
		 mutable-sequence mutable-collection generic-sequence
		 collection))
     (simple-vector
      :translation simple-vector :codes (#.sb!vm:simple-vector-type)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array
		 sequence generic-vector generic-array
		 mutable-sequence mutable-collection
		 generic-sequence collection))
     (bit-vector
      :translation bit-vector :codes (#.sb!vm:complex-bit-vector-type)
      :inherits (vector array sequence
		 generic-vector generic-array mutable-sequence
		 mutable-collection generic-sequence collection))
     (simple-bit-vector
      :translation simple-bit-vector :codes (#.sb!vm:simple-bit-vector-type)
      :direct-superclasses (bit-vector simple-array)
      :inherits (bit-vector vector simple-array
		 array sequence
		 generic-vector generic-array mutable-sequence
		 mutable-collection generic-sequence collection))
     (simple-array-unsigned-byte-2
      :translation (simple-array (unsigned-byte 2) (*))
      :codes (#.sb!vm:simple-array-unsigned-byte-2-type)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence
		 generic-vector generic-array mutable-sequence
		 mutable-collection generic-sequence collection))
     (simple-array-unsigned-byte-4
      :translation (simple-array (unsigned-byte 4) (*))
      :codes (#.sb!vm:simple-array-unsigned-byte-4-type)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence
		 generic-vector generic-array mutable-sequence
		 mutable-collection generic-sequence collection))
     (simple-array-unsigned-byte-8
      :translation (simple-array (unsigned-byte 8) (*))
      :codes (#.sb!vm:simple-array-unsigned-byte-8-type)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence
		 generic-vector generic-array mutable-sequence
		 mutable-collection generic-sequence collection))
     (simple-array-unsigned-byte-16
     :translation (simple-array (unsigned-byte 16) (*))
     :codes (#.sb!vm:simple-array-unsigned-byte-16-type)
     :direct-superclasses (vector simple-array)
     :inherits (vector simple-array array sequence
		generic-vector generic-array mutable-sequence
		mutable-collection generic-sequence collection))
     (simple-array-unsigned-byte-32
     :translation (simple-array (unsigned-byte 32) (*))
     :codes (#.sb!vm:simple-array-unsigned-byte-32-type)
     :direct-superclasses (vector simple-array)
     :inherits (vector simple-array array sequence
		generic-vector generic-array mutable-sequence
		mutable-collection generic-sequence collection))
     (simple-array-signed-byte-8
     :translation (simple-array (signed-byte 8) (*))
     :codes (#.sb!vm:simple-array-signed-byte-8-type)
     :direct-superclasses (vector simple-array)
     :inherits (vector simple-array array sequence
		generic-vector generic-array mutable-sequence
		mutable-collection generic-sequence collection))
     (simple-array-signed-byte-16
     :translation (simple-array (signed-byte 16) (*))
     :codes (#.sb!vm:simple-array-signed-byte-16-type)
     :direct-superclasses (vector simple-array)
     :inherits (vector simple-array array sequence
		generic-vector generic-array mutable-sequence
		mutable-collection generic-sequence collection))
     (simple-array-signed-byte-30
     :translation (simple-array (signed-byte 30) (*))
     :codes (#.sb!vm:simple-array-signed-byte-30-type)
     :direct-superclasses (vector simple-array)
     :inherits (vector simple-array array sequence
		generic-vector generic-array mutable-sequence
		mutable-collection generic-sequence collection))
     (simple-array-signed-byte-32
     :translation (simple-array (signed-byte 32) (*))
     :codes (#.sb!vm:simple-array-signed-byte-32-type)
     :direct-superclasses (vector simple-array)
     :inherits (vector simple-array array sequence
		generic-vector generic-array mutable-sequence
		mutable-collection generic-sequence collection))
     (simple-array-single-float
     :translation (simple-array single-float (*))
     :codes (#.sb!vm:simple-array-single-float-type)
     :direct-superclasses (vector simple-array)
     :inherits (vector simple-array array sequence
		generic-vector generic-array mutable-sequence
		mutable-collection generic-sequence collection))
     (simple-array-double-float
     :translation (simple-array double-float (*))
     :codes (#.sb!vm:simple-array-double-float-type)
     :direct-superclasses (vector simple-array)
     :inherits (vector simple-array array sequence
		generic-vector generic-array mutable-sequence
		mutable-collection generic-sequence collection))
    #!+long-float
    (simple-array-long-float
     :translation (simple-array long-float (*))
     :codes (#.sb!vm:simple-array-long-float-type)
     :direct-superclasses (vector simple-array)
     :inherits (vector simple-array array sequence
		generic-vector generic-array mutable-sequence
		mutable-collection generic-sequence collection))
    (simple-array-complex-single-float
     :translation (simple-array (complex single-float) (*))
     :codes (#.sb!vm:simple-array-complex-single-float-type)
     :direct-superclasses (vector simple-array)
     :inherits (vector simple-array array sequence
		generic-vector generic-array mutable-sequence
		mutable-collection generic-sequence collection))
    (simple-array-complex-double-float
     :translation (simple-array (complex double-float) (*))
     :codes (#.sb!vm:simple-array-complex-double-float-type)
     :direct-superclasses (vector simple-array)
     :inherits (vector simple-array array sequence
		generic-vector generic-array mutable-sequence
		mutable-collection generic-sequence collection))
    #!+long-float
    (simple-array-complex-long-float
     :translation (simple-array (complex long-float) (*))
     :codes (#.sb!vm:simple-array-complex-long-float-type)
     :direct-superclasses (vector simple-array)
     :inherits (vector simple-array array sequence
		generic-vector generic-array mutable-sequence
		mutable-collection generic-sequence collection))
    (generic-string
     :state :read-only
     :inherits (mutable-sequence mutable-collection generic-sequence
		collection))
    (string
     :translation string
     :codes (#.sb!vm:complex-string-type)
     :direct-superclasses (vector generic-string)
     :inherits (vector array sequence
		generic-vector generic-array generic-string
		mutable-sequence mutable-collection
		generic-sequence collection))
    (simple-string
     :translation simple-string
     :codes (#.sb!vm:simple-string-type)
     :direct-superclasses (string simple-array)
     :inherits (string vector simple-array
		array sequence
		generic-string generic-vector generic-array mutable-sequence
		mutable-collection generic-sequence collection))
    (list
     :translation (or cons (member nil))
     :inherits (sequence mutable-sequence mutable-collection
		generic-sequence collection))
    (cons
     :codes (#.sb!vm:list-pointer-type)
     :translation cons
     :inherits (list sequence
		mutable-sequence mutable-collection
		generic-sequence collection))
    (null
     :translation (member nil)
     :inherits (list sequence
		mutable-sequence mutable-collection
		generic-sequence collection symbol)
     :direct-superclasses (list symbol))
    (generic-number :state :read-only)
    (number :translation number :inherits (generic-number))
    (complex
     :translation complex
     :inherits (number generic-number)
     :codes (#.sb!vm:complex-type))
    (complex-single-float
     :translation (complex single-float)
     :inherits (complex number generic-number)
     :codes (#.sb!vm:complex-single-float-type))
    (complex-double-float
     :translation (complex double-float)
     :inherits (complex number generic-number)
     :codes (#.sb!vm:complex-double-float-type))
    #!+long-float
    (complex-long-float
     :translation (complex long-float)
     :inherits (complex number generic-number)
     :codes (#.sb!vm:complex-long-float-type))
    (real :translation real :inherits (number generic-number))
    (float
     :translation float
     :inherits (real number generic-number))
    (single-float
     :translation single-float
     :inherits (float real number generic-number)
     :codes (#.sb!vm:single-float-type))
    (double-float
     :translation double-float
     :inherits (float real number generic-number)
     :codes (#.sb!vm:double-float-type))
    #!+long-float
    (long-float
     :translation long-float
     :inherits (float real number generic-number)
     :codes (#.sb!vm:long-float-type))
    (rational
     :translation rational
     :inherits (real number generic-number))
    (ratio
     :translation (and rational (not integer))
     :inherits (rational real number generic-number)
     :codes (#.sb!vm:ratio-type))
    (integer
     :translation integer
     :inherits (rational real number generic-number))
    (fixnum
     :translation (integer #.sb!vm:*target-most-negative-fixnum*
			   #.sb!vm:*target-most-positive-fixnum*)
     :inherits (integer rational real number
		generic-number)
     :codes (#.sb!vm:even-fixnum-type #.sb!vm:odd-fixnum-type))
    (bignum
     :translation (and integer (not fixnum))
     :inherits (integer rational real number
		generic-number)
     :codes (#.sb!vm:bignum-type))
    (stream
     :state :read-only
     :depth 3
     :inherits (instance)))))

;;; comment from CMU CL:
;;;   See also type-init.lisp where we finish setting up the
;;;   translations for built-in types.
(!cold-init-forms
  (dolist (x *built-in-classes*)
    #-sb-xc-host (/show0 "at head of loop over *BUILT-IN-CLASSES*")
    (destructuring-bind
	(name &key
	      (translation nil trans-p)
	      inherits
	      codes
	      enumerable
	      state
              depth
	      (hierarchical-p t) ; might be modified below
	      (direct-superclasses (if inherits
				     (list (car inherits))
				     '(t))))
	x
      (declare (ignore codes state translation))
      (let ((inherits-list (if (eq name t)
			       ()
			       (cons t (reverse inherits))))
	    (class (make-built-in-class
		    :enumerable enumerable
		    :name name
		    :translation (if trans-p :initializing nil)
		    :direct-superclasses
		    (if (eq name t)
		      nil
		      (mapcar #'sb!xc:find-class direct-superclasses)))))
	(setf (info :type :kind name) :primitive
	      (class-cell-class (find-class-cell name)) class)
	(unless trans-p
	  (setf (info :type :builtin name) class))
	(let* ((inherits-vector
		(map 'simple-vector
		     (lambda (x)
		       (let ((super-layout
			      (class-layout (sb!xc:find-class x))))
			 (when (minusp (layout-depthoid super-layout))
			   (setf hierarchical-p nil))
			 super-layout))
		     inherits-list))
	       (depthoid (if hierarchical-p
                           (or depth (length inherits-vector))
                           -1)))
	  (register-layout
	   (find-and-init-or-check-layout name
					  0
					  inherits-vector
					  depthoid)
	   :invalidate nil)))))
  (/show0 "done with loop over *BUILT-IN-CLASSES*"))

;;; Define temporary PCL STANDARD-CLASSes. These will be set up
;;; correctly and the Lisp layout replaced by a PCL wrapper after PCL
;;; is loaded and the class defined.
(!cold-init-forms
  (/show0 "about to define temporary STANDARD-CLASSes")
  (dolist (x '(;; Why is STREAM duplicated in this list? Because, when
               ;; the inherits-vector of FUNDAMENTAL-STREAM is set up,
               ;; a vector containing the elements of the list below,
               ;; i.e. '(T INSTANCE STREAM STREAM), is created, and
               ;; this is what the function ORDER-LAYOUT-INHERITS
               ;; would do, too.
               ;;
               ;; So, the purpose is to guarantee a valid layout for
               ;; the FUNDAMENTAL-STREAM class, matching what
               ;; ORDER-LAYOUT-INHERITS would do.
               ;; ORDER-LAYOUT-INHERITS would place STREAM at index 3
               ;; in the INHERITS(-VECTOR). Index 2 would not be
               ;; filled, so STREAM is duplicated there (as
               ;; ORDER-LAYOUTS-INHERITS would do). Maybe the
               ;; duplicate definition could be removed (removing a
               ;; STREAM element), because FUNDAMENTAL-STREAM is
               ;; redefined after PCL is set up, anyway. But to play
               ;; it safely, we define the class with a valid INHERITS
               ;; vector.
	       (fundamental-stream (t instance stream stream))))
    (/show0 "defining temporary STANDARD-CLASS")
    (let* ((name (first x))
	   (inherits-list (second x))
	   (class (make-standard-class :name name))
	   (class-cell (find-class-cell name)))
      (setf (class-cell-class class-cell) class
	    (info :type :class name) class-cell
	    (info :type :kind name) :instance)
      (let ((inherits (map 'simple-vector
			   (lambda (x)
			     (class-layout (sb!xc:find-class x)))
			   inherits-list)))
	#-sb-xc-host (/show0 "INHERITS=..") #-sb-xc-host (/hexstr inherits)
	(register-layout (find-and-init-or-check-layout name 0 inherits -1)
			 :invalidate nil))))
  (/show0 "done defining temporary STANDARD-CLASSes"))

;;; Now that we have set up the class heterarchy, seal the sealed
;;; classes. This must be done after the subclasses have been set up.
(!cold-init-forms
  (dolist (x *built-in-classes*)
    (destructuring-bind (name &key (state :sealed) &allow-other-keys) x
      (setf (class-state (sb!xc:find-class name)) state))))

;;;; class definition/redefinition

;;; This is to be called whenever we are altering a class.
(defun modify-class (class)
  (clear-type-caches)
  (when (member (class-state class) '(:read-only :frozen))
    ;; FIXME: This should probably be CERROR.
    (warn "making ~(~A~) class ~S writable"
	  (class-state class)
	  (sb!xc:class-name class))
    (setf (class-state class) nil)))

;;; Mark LAYOUT as invalid. Setting DEPTHOID -1 helps cause unsafe
;;; structure type tests to fail. Remove class from all superclasses
;;; too (might not be registered, so might not be in subclasses of the
;;; nominal superclasses.)
(defun invalidate-layout (layout)
  (declare (type layout layout))
  (setf (layout-invalid layout) t
	(layout-depthoid layout) -1)
  (let ((inherits (layout-inherits layout))
	(class (layout-class layout)))
    (modify-class class)
    (dotimes (i (length inherits)) ; FIXME: DOVECTOR
      (let* ((super (svref inherits i))
	     (subs (class-subclasses (layout-class super))))
	(when subs
	  (remhash class subs)))))
  (values))

;;;; cold loading initializations

;;; FIXME: It would be good to arrange for this to be called when the
;;; cross-compiler is being built, not just when the target Lisp is
;;; being cold loaded. Perhaps this could be moved to its own file
;;; late in the stems-and-flags.lisp-expr sequence, and be put in
;;; !COLD-INIT-FORMS there?
(defun !class-finalize ()
  (dohash (name layout *forward-referenced-layouts*)
    (let ((class (sb!xc:find-class name nil)))
      (cond ((not class)
	     (setf (layout-class layout) (make-undefined-class name)))
	    ((eq (class-layout class) layout)
	     (remhash name *forward-referenced-layouts*))
	    (t
	     ;; FIXME: ERROR?
	     (warn "something strange with forward layout for ~S:~%  ~S"
		   name
		   layout))))))

;;; a vector that maps type codes to layouts, used for quickly finding
;;; the layouts of built-in classes
(defvar *built-in-class-codes*) ; initialized in cold load
(declaim (type simple-vector *built-in-class-codes*))

(!cold-init-forms
  #-sb-xc-host (/show0 "about to set *BUILT-IN-CLASS-CODES*")
  (setq *built-in-class-codes*
	(let* ((initial-element
		(locally
		  ;; KLUDGE: There's a FIND-CLASS DEFTRANSFORM for
		  ;; constant class names which creates fast but
		  ;; non-cold-loadable, non-compact code. In this
		  ;; context, we'd rather have compact, cold-loadable
		  ;; code. -- WHN 19990928
		  (declare (notinline sb!xc:find-class))
		  (class-layout (sb!xc:find-class 'random-class))))
	       (res (make-array 256 :initial-element initial-element)))
	  (dolist (x *built-in-classes* res)
	    (destructuring-bind (name &key codes &allow-other-keys)
				x
	      (let ((layout (class-layout (sb!xc:find-class name))))
		(dolist (code codes)
		  (setf (svref res code) layout)))))))
  #-sb-xc-host (/show0 "done setting *BUILT-IN-CLASS-CODES*"))

(!defun-from-collected-cold-init-forms !classes-cold-init)
