;;;; This file contains portable versions of low-level functions and macros
;;;; which are ripe for implementation specific customization. None of the code
;;;; in this file *has* to be customized for a particular Common Lisp
;;;; implementation. Moreover, in some implementations it may not make any
;;;; sense to customize some of this code.
;;;;
;;;; The original version was intended to support portable customization to
;;;; lotso different Lisp implementations. This functionality is gone in the
;;;; current version, and it now runs only under SBCL. (Now that ANSI Common
;;;; Lisp has mixed CLOS into the insides of the system (e.g. error handling
;;;; and printing) so deeply that it's not very meaningful to bootstrap Common
;;;; Lisp without CLOS, the old functionality is of dubious use. -- WHN
;;;; 19981108)

;;;; This software is part of the SBCL system. See the README file for more
;;;; information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *optimize-speed*
  '(optimize (speed 3) (safety 0)))
) ; EVAL-WHEN

(defmacro dotimes-fixnum ((var count &optional (result nil)) &body body)
  `(dotimes (,var (the fixnum ,count) ,result)
     (declare (fixnum ,var))
     ,@body))

;;;; early definition of WRAPPER
;;;;
;;;; Most WRAPPER stuff is defined later, but the DEFSTRUCT itself
;;;; is here early so that things like (TYPEP .. 'WRAPPER) can be
;;;; compiled efficiently.

;;; Note that for SBCL, as for CMU CL, the WRAPPER of a built-in or
;;; structure class will be some other kind of SB-KERNEL:LAYOUT, but
;;; this shouldn't matter, since the only two slots that WRAPPER adds
;;; are meaningless in those cases.
(defstruct (wrapper
	    (:include layout
		      ;; KLUDGE: In CMU CL, the initialization default
		      ;; for LAYOUT-INVALID was NIL. In SBCL, that has
		      ;; changed to :UNINITIALIZED, but PCL code might
		      ;; still expect NIL for the initialization
		      ;; default of WRAPPER-INVALID. Instead of trying
		      ;; to find out, I just overrode the LAYOUT
		      ;; default here. -- WHN 19991204
		      (invalid nil))
	    (:conc-name %wrapper-)
	    (:constructor make-wrapper-internal)
	    (:copier nil))
  (instance-slots-layout nil :type list)
  (class-slots nil :type list))
#-sb-fluid (declaim (sb-ext:freeze-type wrapper))

;;;; PCL's view of funcallable instances

(!defstruct-with-alternate-metaclass pcl-funcallable-instance
  ;; KLUDGE: Note that neither of these slots is ever accessed by its
  ;; accessor name as of sbcl-0.pre7.63. Presumably everything works
  ;; by puns based on absolute locations. Fun fun fun.. -- WHN 2001-10-30
  :slot-names (clos-slots name hash-code)
  :boa-constructor %make-pcl-funcallable-instance
  :superclass-name funcallable-instance
  :metaclass-name random-pcl-classoid
  :metaclass-constructor make-random-pcl-classoid
  :dd-type funcallable-structure
  ;; Only internal implementation code will access these, and these
  ;; accesses (slot readers in particular) could easily be a
  ;; bottleneck, so it seems reasonable to suppress runtime type
  ;; checks.
  ;;
  ;; (Except note KLUDGE above that these accessors aren't used at all
  ;; (!) as of sbcl-0.pre7.63, so for now it's academic.)
  :runtime-type-checks-p nil)

(import 'sb-kernel:funcallable-instance-p)

(defun set-funcallable-instance-function (fin new-value)
  (declare (type function new-value))
  (aver (funcallable-instance-p fin))
  (setf (funcallable-instance-fun fin) new-value))
(defmacro fsc-instance-p (fin)
  `(funcallable-instance-p ,fin))
(defmacro fsc-instance-wrapper (fin)
  `(%funcallable-instance-layout ,fin))
;;; FIXME: This seems to bear no relation at all to the CLOS-SLOTS
;;; slot in the FUNCALLABLE-INSTANCE structure, above, which
;;; (bizarrely) seems to be set to the NAME of the
;;; FUNCALLABLE-INSTANCE. At least, the index 1 seems to return the
;;; NAME, and the index 2 NIL.  Weird.  -- CSR, 2002-11-07
(defmacro fsc-instance-slots (fin)
  `(%funcallable-instance-info ,fin 0))
(defmacro fsc-instance-hash (fin)
  `(%funcallable-instance-info ,fin 3))

(declaim (inline clos-slots-ref (setf clos-slots-ref)))
(declaim (ftype (function (simple-vector index) t) clos-slots-ref))
(defun clos-slots-ref (slots index)
  (svref slots index))
(declaim (ftype (function (t simple-vector index) t) (setf clos-slots-ref)))
(defun (setf clos-slots-ref) (new-value slots index)
  (setf (svref slots index) new-value))

;;; Note on implementation under CMU CL >=17 and SBCL: STD-INSTANCE-P
;;; is only used to discriminate between functions (including FINs)
;;; and normal instances, so we can return true on structures also. A
;;; few uses of (OR STD-INSTANCE-P FSC-INSTANCE-P) are changed to
;;; PCL-INSTANCE-P.
(defmacro std-instance-p (x)
  `(%instancep ,x))

;; a temporary definition used for debugging the bootstrap
#+sb-show
(defun print-std-instance (instance stream depth)
  (declare (ignore depth))	
  (print-unreadable-object (instance stream :type t :identity t)
    (let ((class (class-of instance)))
      (when (or (eq class (find-class 'standard-class nil))
		(eq class (find-class 'funcallable-standard-class nil))
		(eq class (find-class 'built-in-class nil)))
	(princ (early-class-name instance) stream)))))

;;; This is the value that we stick into a slot to tell us that it is
;;; unbound. It may seem gross, but for performance reasons, we make
;;; this an interned symbol. That means that the fast check to see
;;; whether a slot is unbound is to say (EQ <val> '..SLOT-UNBOUND..).
;;; That is considerably faster than looking at the value of a special
;;; variable. Be careful, there are places in the code which actually
;;; use ..SLOT-UNBOUND.. rather than this variable. So much for
;;; modularity..
;;;
;;; FIXME: Now that we're tightly integrated into SBCL, we could use
;;; the SBCL built-in unbound value token instead. Perhaps if we did
;;; so it would be a good idea to define collections of CLOS slots as
;;; a new type of heap object, instead of using bare SIMPLE-VECTOR, in
;;; order to avoid problems (in the debugger if nowhere else) with
;;; SIMPLE-VECTORs some of whose elements are unbound tokens.
(defconstant +slot-unbound+ '..slot-unbound..)

(defmacro %allocate-static-slot-storage--class (no-of-slots)
  `(make-array ,no-of-slots :initial-element +slot-unbound+))

(defmacro std-instance-class (instance)
  `(wrapper-class* (std-instance-wrapper ,instance)))

;;; When given a function should give this function the name
;;; NEW-NAME. Note that NEW-NAME is sometimes a list. Some lisps
;;; get the upset in the tummy when they start thinking about
;;; functions which have lists as names. To deal with that there is
;;; SET-FUN-NAME-INTERN which takes a list spec for a function
;;; name and turns it into a symbol if need be.
;;;
;;; When given a funcallable instance, SET-FUN-NAME *must* side-effect
;;; that FIN to give it the name. When given any other kind of
;;; function SET-FUN-NAME is allowed to return a new function which is
;;; "the same" except that it has the name.
;;;
;;; In all cases, SET-FUN-NAME must return the new (or same)
;;; function. (Unlike other functions to set stuff, it does not return
;;; the new value.)
(defun set-fun-name (fcn new-name)
  #+sb-doc
  "Set the name of a compiled function object. Return the function."
  (declare (special *boot-state* *the-class-standard-generic-function*))
  (cond ((symbolp fcn)
	 (set-fun-name (symbol-function fcn) new-name))
	((funcallable-instance-p fcn)
	 (if (if (eq *boot-state* 'complete)
		 (typep fcn 'generic-function)
		 (eq (class-of fcn) *the-class-standard-generic-function*))
	     (setf (%funcallable-instance-info fcn 1) new-name)
	     (bug "unanticipated function type"))
	 fcn)
	(t
	 ;; pw-- This seems wrong and causes trouble. Tests show
	 ;; that loading CL-HTTP resulted in ~5400 closures being
	 ;; passed through this code of which ~4000 of them pointed
	 ;; to but 16 closure-functions, including 1015 each of
	 ;; DEFUN MAKE-OPTIMIZED-STD-WRITER-METHOD-FUNCTION
	 ;; DEFUN MAKE-OPTIMIZED-STD-READER-METHOD-FUNCTION
	 ;; DEFUN MAKE-OPTIMIZED-STD-BOUNDP-METHOD-FUNCTION.
	 ;; Since the actual functions have been moved by PURIFY
	 ;; to memory not seen by GC, changing a pointer there
	 ;; not only clobbers the last change but leaves a dangling
	 ;; pointer invalid  after the next GC. Comments in low.lisp
	 ;; indicate this code need do nothing. Setting the
	 ;; function-name to NIL loses some info, and not changing
	 ;; it loses some info of potential hacking value. So,
	 ;; lets not do this...
	 #+nil
	 (let ((header (%closure-fun fcn)))
	   (setf (%simple-fun-name header) new-name))

	 ;; XXX Maybe add better scheme here someday.
	 fcn)))

(defun intern-fun-name (name)
  (cond ((symbolp name) name)
	((listp name)
	 (intern (let ((*package* *pcl-package*)
		       (*print-case* :upcase)
		       (*print-pretty* nil)
		       (*print-gensym* t))
		   (format nil "~S" name))
		 *pcl-package*))))

;;; FIXME: probably no longer needed after init
(defmacro precompile-random-code-segments (&optional system)
  `(progn
     (eval-when (:compile-toplevel)
       (update-dispatch-dfuns))
     (precompile-function-generators ,system)
     (precompile-dfun-constructors ,system)
     (precompile-ctors)))

;;; This definition is for interpreted code.
(defun pcl-instance-p (x)
  (typep (layout-of x) 'wrapper))

;;; CMU CL comment:
;;;   We define this as STANDARD-INSTANCE, since we're going to
;;;   clobber the layout with some standard-instance layout as soon as
;;;   we make it, and we want the accessor to still be type-correct.
#|
(defstruct (standard-instance
	    (:predicate nil)
	    (:constructor %%allocate-instance--class ())
	    (:copier nil)
	    (:alternate-metaclass instance
				  cl:standard-class
				  make-standard-class))
  (slots nil))
|#
(!defstruct-with-alternate-metaclass standard-instance
  :slot-names (slots hash-code)
  :boa-constructor %make-standard-instance
  :superclass-name instance
  :metaclass-name standard-classoid
  :metaclass-constructor make-standard-classoid
  :dd-type structure
  :runtime-type-checks-p nil)

;;; Both of these operations "work" on structures, which allows the above
;;; weakening of STD-INSTANCE-P.
(defmacro std-instance-slots (x) `(%instance-ref ,x 1))
(defmacro std-instance-wrapper (x) `(%instance-layout ,x))
;;; KLUDGE: This one doesn't "work" on structures.  However, we
;;; ensure, in SXHASH and friends, never to call it on structures.
(defmacro std-instance-hash (x) `(%instance-ref ,x 2))

;;; FIXME: These functions are called every place we do a
;;; CALL-NEXT-METHOD, and probably other places too. It's likely worth
;;; selectively optimizing them with DEFTRANSFORMs and stuff, rather
;;; than just indiscriminately expanding them inline everywhere.
(declaim (inline get-slots get-slots-or-nil))
(declaim (ftype (function (t) simple-vector) get-slots))
(declaim (ftype (function (t) (or simple-vector null)) get-slots-or-nil))
(defun get-slots (instance)
  (if (std-instance-p instance)
      (std-instance-slots instance)
      (fsc-instance-slots instance)))
(defun get-slots-or-nil (instance)
  ;; Suppress a code-deletion note.  FIXME: doing the FIXME above,
  ;; integrating PCL more with the compiler, would remove the need for
  ;; this icky stuff.
  (declare (optimize (inhibit-warnings 3)))
  (when (pcl-instance-p instance)
    (get-slots instance)))

(defmacro built-in-or-structure-wrapper (x) `(layout-of ,x))

(defmacro get-wrapper (inst)
  (once-only ((wrapper `(wrapper-of ,inst)))
    `(progn
       (aver (typep ,wrapper 'wrapper))
       ,wrapper)))

;;; FIXME: could be an inline function or ordinary function (like many
;;; other things around here)
(defmacro get-instance-wrapper-or-nil (inst)
  (once-only ((wrapper `(wrapper-of ,inst)))
    `(if (typep ,wrapper 'wrapper)
	 ,wrapper
	 nil)))

;;;; support for useful hashing of PCL instances
(let ((hash-code 0))
  (declare (fixnum hash-code))
  (defun get-instance-hash-code ()
    (if (< hash-code most-positive-fixnum)
	(incf hash-code)
	(setq hash-code 0))))

(defun sb-impl::sxhash-instance (x)
  (cond
    ((std-instance-p x) (std-instance-hash x))
    ((fsc-instance-p x) (fsc-instance-hash x))
    (t (bug "SXHASH-INSTANCE called on some weird thing: ~S" x))))

;;;; structure-instance stuff
;;;;
;;;; FIXME: Now that the code is SBCL-only, this extra layer of
;;;; abstraction around our native structure representation doesn't
;;;; seem to add anything useful, and could probably go away.

;;; The definition of STRUCTURE-TYPE-P was moved to early-low.lisp.

(defun get-structure-dd (type)
  (layout-info (classoid-layout (find-classoid type))))

(defun structure-type-included-type-name (type)
  (let ((include (dd-include (get-structure-dd type))))
    (if (consp include)
	(car include)
	include)))

(defun structure-type-slot-description-list (type)
  (nthcdr (length (let ((include (structure-type-included-type-name type)))
		    (and include
			 (dd-slots (get-structure-dd include)))))
	  (dd-slots (get-structure-dd type))))

(defun structure-slotd-name (slotd)
  (dsd-name slotd))

(defun structure-slotd-accessor-symbol (slotd)
  (dsd-accessor-name slotd))

(defun structure-slotd-reader-function (slotd)
  (fdefinition (dsd-accessor-name slotd)))

(defun structure-slotd-writer-function (type slotd)
  (if (dsd-read-only slotd)
      (let ((dd (get-structure-dd type)))
	(coerce (sb-kernel::slot-setter-lambda-form dd slotd) 'function))
      (fdefinition `(setf ,(dsd-accessor-name slotd)))))

(defun structure-slotd-type (slotd)
  (dsd-type slotd))

(defun structure-slotd-init-form (slotd)
  (dsd-default slotd))

;;; WITH-PCL-LOCK is used around some forms that were previously
;;; protected by WITHOUT-INTERRUPTS, but in a threaded SBCL we don't
;;; have a useful WITHOUT-INTERRUPTS.  In an unthreaded SBCL I'm not
;;; sure what the desired effect is anyway: should we be protecting
;;; against the possibility of recursive calls into these functions
;;; or are we using WITHOUT-INTERRUPTS as WITHOUT-SCHEDULING?
;;;
;;; Users: FORCE-CACHE-FLUSHES, MAKE-INSTANCES-OBSOLETE.  Note that
;;; it's not all certain this is sufficent for threadsafety: do we
;;; just have to protect against simultaneous calls to these mutators,
;;; or actually to stop normal slot access etc at the same time as one
;;; of them runs

#+sb-thread
(progn
(defstruct spinlock (value 0))
(defvar *pcl-lock* (make-spinlock))

(defmacro with-pcl-lock (&body body)
  `(progn
    (sb-thread::get-spinlock *pcl-lock* 1 (sb-thread::current-thread-id))
    (unwind-protect
       (progn ,@body)
      (setf (spinlock-value *pcl-lock*) 0))))
);progn

#-sb-thread
(defmacro with-pcl-lock (&body body)
  `(progn ,@body))
