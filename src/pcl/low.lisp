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
(defvar *optimize-speed* '(optimize (speed 3) (safety 0)))
) ; EVAL-WHEN

;;; FIXME: Do these definitions actually increase speed significantly?
;;; Could we just use SVREF instead, possibly with a few extra
;;; OPTIMIZE declarations added here and ther?
(defmacro %svref (vector index)
  `(locally (declare #.*optimize-speed*
		     (inline svref))
	    (svref (the simple-vector ,vector) (the fixnum ,index))))
(defsetf %svref %set-svref)
(defmacro %set-svref (vector index new-value)
  `(locally (declare #.*optimize-speed*
		     (inline svref))
     (setf (svref (the simple-vector ,vector) (the fixnum ,index))
	   ,new-value)))

;;; I want the body to be evaluated in such a way that no other code that is
;;; running PCL can be run during that evaluation. I agree that the body
;;; won't take *long* to evaluate. That is to say that I will only use
;;; WITHOUT-INTERRUPTS around relatively small computations.
;;;
;;; FIXME: We can get rid of this macro definitionand either USE package %SYS
;;; or add an explicit SB-SYS: prefix to each reference to WITHOUT-INTERRUPTS.
(defmacro without-interrupts (&rest stuff)
  `(sb-sys:without-interrupts ,@stuff))

(defmacro dotimes-fixnum ((var count &optional (result nil)) &body body)
  `(dotimes (,var (the fixnum ,count) ,result)
     (declare (fixnum ,var))
     ,@body))

;;;; very low-level representation of instances with meta-class
;;;; STANDARD-CLASS

;;; FIXME: more than one IN-PACKAGE in a source file, ick
(in-package "SB-C")

(defknown sb-pcl::pcl-instance-p (t) boolean
  (movable foldable flushable explicit-check))

(deftransform sb-pcl::pcl-instance-p ((object))
  (let* ((otype (continuation-type object))
	 (std-obj (specifier-type 'sb-pcl::std-object)))
    (cond
      ;; Flush tests whose result is known at compile time.
      ((csubtypep otype std-obj) 't)
      ((not (types-intersect otype std-obj)) 'nil)
      (t
       `(typep (sb-kernel:layout-of object) 'sb-pcl::wrapper)))))

(in-package "SB-PCL")

;;; FIXME: What do these do? Could we use SB-KERNEL:INSTANCE-REF instead?
(defmacro %instance-ref (slots index)
  `(%svref ,slots ,index))
(defmacro instance-ref (slots index)
  `(svref ,slots ,index))

;;; Note on implementation under CMU CL >=17 and SBCL: STD-INSTANCE-P is
;;; only used to discriminate between functions (including FINs) and
;;; normal instances, so we can return true on structures also. A few
;;; uses of (or std-instance-p fsc-instance-p) are changed to
;;; pcl-instance-p.
(defmacro std-instance-p (x)
  `(sb-kernel:%instancep ,x))

(defmacro get-slots (inst)
  `(cond ((std-instance-p ,inst) (std-instance-slots ,inst))
	 ((fsc-instance-p ,inst) (fsc-instance-slots ,inst))
	 (t (error "What kind of instance is this?"))))

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

;;; This is the value that we stick into a slot to tell us that it is unbound.
;;; It may seem gross, but for performance reasons, we make this an interned
;;; symbol. That means that the fast check to see whether a slot is unbound is
;;; to say (EQ <val> '..SLOT-UNBOUND..). That is considerably faster than
;;; looking at the value of a special variable. Be careful, there are places in
;;; the code which actually use ..slot-unbound.. rather than this variable. So
;;; much for modularity..
;;;
;;; FIXME: Now that we're tightly integrated into SBCL, we could use the
;;; SBCL built-in unbound value token instead.
(defconstant +slot-unbound+ '..slot-unbound..)

(defmacro %allocate-static-slot-storage--class (no-of-slots)
  `(make-array ,no-of-slots :initial-element +slot-unbound+))

(defmacro std-instance-class (instance)
  `(wrapper-class* (std-instance-wrapper ,instance)))

;;;; FUNCTION-ARGLIST

;;; FIXME: Does FUNCTION-PRETTY-ARGLIST need to be settable at all?
(defsetf function-pretty-arglist set-function-pretty-arglist)
(defun set-function-pretty-arglist (function new-value)
  (declare (ignore function))
  new-value)

;;; SET-FUNCTION-NAME
;;;
;;; When given a function should give this function the name <new-name>.
;;; Note that <new-name> is sometimes a list. Some lisps get the upset
;;; in the tummy when they start thinking about functions which have
;;; lists as names. To deal with that there is set-function-name-intern
;;; which takes a list spec for a function name and turns it into a symbol
;;; if need be.
;;;
;;; When given a funcallable instance, set-function-name MUST side-effect
;;; that FIN to give it the name. When given any other kind of function
;;; set-function-name is allowed to return new function which is the 'same'
;;; except that it has the name.
;;;
;;; In all cases, set-function-name must return the new (or same) function.
;;; (Unlike other functions to set stuff, it does not return the new value.)
(defun set-function-name (fcn new-name)
  #+sb-doc
  "Set the name of a compiled function object. Return the function."
  (declare (special *boot-state* *the-class-standard-generic-function*))
  (cond ((symbolp fcn)
	 (set-function-name (symbol-function fcn) new-name))
	((funcallable-instance-p fcn)
	 (if (if (eq *boot-state* 'complete)
		 (typep fcn 'generic-function)
		 (eq (class-of fcn) *the-class-standard-generic-function*))
	     (setf (sb-kernel:%funcallable-instance-info fcn 1) new-name)
	     (typecase fcn
	       (sb-kernel:byte-closure
		(set-function-name (sb-kernel:byte-closure-function fcn)
				   new-name))
	       (sb-kernel:byte-function
		(setf (sb-kernel:byte-function-name fcn) new-name))
	       (sb-eval:interpreted-function
		(setf (sb-eval:interpreted-function-name fcn) new-name))))
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
	 (let ((header (sb-kernel:%closure-function fcn)))
	   (setf (sb-c::%function-name header) new-name))

	 ;; Maybe add better scheme here someday.
	 fcn)))

(defun intern-function-name (name)
  (cond ((symbolp name) name)
	((listp name)
	 (intern (let ((*package* *pcl-package*)
		       (*print-case* :upcase)
		       (*print-pretty* nil)
		       (*print-gensym* 't))
		   (format nil "~S" name))
		 *pcl-package*))))

;;;; COMPILE-LAMBDA

;;; This is like the Common Lisp function COMPILE. In fact, that is what it
;;; ends up calling. The difference is that it deals with things like not
;;; calling the compiler in certain cases.
;;;
;;; FIXME: I suspect that in SBCL, we should always call the compiler. (PCL
;;; was originally designed to run even on systems with dog-slow call-out-to-C
;;; compilers, and I suspect that this code is needed only for that.)
(defun compile-lambda (lambda &optional (desirability :fast))
  (cond ((eq desirability :fast)
	 (compile nil lambda))
	(t
	 (compile-lambda-uncompiled lambda))))

(defun compile-lambda-uncompiled (uncompiled)
  #'(lambda (&rest args) (apply (coerce uncompiled 'function) args)))

(defun compile-lambda-deferred (uncompiled)
  (let ((function (coerce uncompiled 'function))
	(compiled nil))
    (declare (type (or function null) compiled))
    #'(lambda (&rest args)
	(if compiled
	    (apply compiled args)
	    (if (in-the-compiler-p)
		(apply function args)
		(progn (setq compiled (compile nil uncompiled))
		       (apply compiled args)))))))

;;; FIXME: probably no longer needed after init
(defmacro precompile-random-code-segments (&optional system)
  `(progn
     (eval-when (:compile-toplevel)
       (update-dispatch-dfuns)
       (compile-iis-functions nil))
     (precompile-function-generators ,system)
     (precompile-dfun-constructors ,system)
     (precompile-iis-functions ,system)
     (eval-when (:load-toplevel)
       (compile-iis-functions t))))

(defun record-definition (type spec &rest args)
  (declare (ignore type spec args))
  ())

(defun doctor-dfun-for-the-debugger (gf dfun) (declare (ignore gf)) dfun)

;;;; low level functions for structures I: functions on arbitrary objects

;;; FIXME: Maybe we don't need this given the SBCL-specific
;;; versions of the functions which would otherwise use it?
(defvar *structure-table* (make-hash-table :test 'eq))

(defun declare-structure (name included-name slot-description-list)
  (setf (gethash name *structure-table*)
	(cons included-name slot-description-list)))

(unless (fboundp 'structure-functions-exist-p)
  (setf (symbol-function 'structure-functions-exist-p)
	#'(lambda () nil)))

;;; FIXME: should probably be INLINE
;;; FIXME: should probably be moved to package SB-INT along with
;;; other nonstandard type predicates, or removed entirely
(defun structurep (x)
  (typep x 'cl:structure-object))

;;; This definition is for interpreted code.
(defun pcl-instance-p (x)
  (typep (sb-kernel:layout-of x) 'wrapper))

;;; We define this as STANDARD-INSTANCE, since we're going to clobber the
;;; layout with some standard-instance layout as soon as we make it, and we
;;; want the accessor to still be type-correct.
(defstruct (standard-instance
	    (:predicate nil)
	    (:constructor %%allocate-instance--class ())
	    (:copier nil)
	    (:alternate-metaclass sb-kernel:instance cl:standard-class
				  sb-kernel:make-standard-class))
  (slots nil))

;;; Both of these operations "work" on structures, which allows the above
;;; weakening of STD-INSTANCE-P.
(defmacro std-instance-slots (x) `(sb-kernel:%instance-ref ,x 1))
(defmacro std-instance-wrapper (x) `(sb-kernel:%instance-layout ,x))

(defmacro built-in-or-structure-wrapper (x) `(sb-kernel:layout-of ,x))

(defmacro get-wrapper (inst)
  (sb-int:once-only ((wrapper `(wrapper-of ,inst)))
    `(progn
       (assert (typep ,wrapper 'wrapper) () "What kind of instance is this?")
       ,wrapper)))

;;; FIXME: could be an inline function (like many other things around
;;; here)
(defmacro get-instance-wrapper-or-nil (inst)
  (sb-int:once-only ((wrapper `(wrapper-of ,inst)))
    `(if (typep ,wrapper 'wrapper)
	 ,wrapper
	 nil)))

(defmacro get-slots-or-nil (inst)
  (sb-int:once-only ((n-inst inst))
    `(when (pcl-instance-p ,n-inst)
       (if (std-instance-p ,n-inst)
	   (std-instance-slots ,n-inst)
	   (fsc-instance-slots ,n-inst)))))

;;;; structure-instance stuff

;;; FIXME: This can be removed by hardwiring uses of it to T.
(defun structure-functions-exist-p ()
  t)

;;; The definition of STRUCTURE-TYPE-P was moved to early-low.lisp.

(defun get-structure-dd (type)
  (sb-kernel:layout-info (sb-kernel:class-layout (cl:find-class type))))

(defun structure-type-included-type-name (type)
  (let ((include (sb-kernel::dd-include (get-structure-dd type))))
    (if (consp include)
	(car include)
	include)))

(defun structure-type-slot-description-list (type)
  (nthcdr (length (let ((include (structure-type-included-type-name type)))
		    (and include
			 (sb-kernel:dd-slots (get-structure-dd include)))))
	  (sb-kernel:dd-slots (get-structure-dd type))))

(defun structure-slotd-name (slotd)
  (sb-kernel:dsd-name slotd))

(defun structure-slotd-accessor-symbol (slotd)
  (sb-kernel:dsd-accessor slotd))

(defun structure-slotd-reader-function (slotd)
  (fdefinition (sb-kernel:dsd-accessor slotd)))

(defun structure-slotd-writer-function (slotd)
  (unless (sb-kernel:dsd-read-only slotd)
    (fdefinition `(setf ,(sb-kernel:dsd-accessor slotd)))))

(defun structure-slotd-type (slotd)
  (sb-kernel:dsd-type slotd))

(defun structure-slotd-init-form (slotd)
  (sb-kernel::dsd-default slotd))

;;; FIXME: more than one IN-PACKAGE in a source file, ick
(in-package "SB-C")

(def-source-context defmethod (name &rest stuff)
  (let ((arg-pos (position-if #'listp stuff)))
    (if arg-pos
	`(defmethod ,name ,@(subseq stuff 0 arg-pos)
	   ,(nth-value 2 (sb-pcl::parse-specialized-lambda-list
			  (elt stuff arg-pos))))
	`(defmethod ,name "<illegal syntax>"))))
