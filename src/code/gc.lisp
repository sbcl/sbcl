;;;; garbage collection and allocation-related code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;;; DYNAMIC-USAGE and friends

(declaim (special sb!vm:*read-only-space-free-pointer*
		  sb!vm:*static-space-free-pointer*))

(eval-when (:compile-toplevel :execute)
  (sb!xc:defmacro def-c-var-frob (lisp-fun c-var-name)
    `(progn
       #!-sb-fluid (declaim (inline ,lisp-fun))
       (defun ,lisp-fun ()
	 (sb!alien:extern-alien ,c-var-name (sb!alien:unsigned 32))))))

#!+(or cgc gencgc) (progn
#!-sb-fluid (declaim (inline dynamic-usage))
(def-c-var-frob dynamic-usage "bytes_allocated"))

#!-(or cgc gencgc)
(defun dynamic-usage ()
  (the (unsigned-byte 32)
       (- (sb!sys:sap-int (sb!c::dynamic-space-free-pointer))
          (current-dynamic-space-start))))

#!-gencgc (progn
#!-sb-fluid (declaim (inline current-dynamic-space-start))
(def-c-var-frob current-dynamic-space-start "current_dynamic_space"))

(defun static-space-usage ()
  (- (* sb!vm:*static-space-free-pointer* sb!vm:word-bytes)
     sb!vm:static-space-start))

(defun read-only-space-usage ()
  (- (* sb!vm::*read-only-space-free-pointer* sb!vm:word-bytes)
     sb!vm:read-only-space-start))

(defun control-stack-usage ()
  #!-x86 (- (sb!sys:sap-int (sb!c::control-stack-pointer-sap))
            sb!vm:control-stack-start)
  #!+x86 (- sb!vm:control-stack-end
	    (sb!sys:sap-int (sb!c::control-stack-pointer-sap))))

(defun binding-stack-usage ()
  (- (sb!sys:sap-int (sb!c::binding-stack-pointer-sap))
     sb!vm:binding-stack-start))

;;;; ROOM

(defun room-minimal-info ()
  (format t "Dynamic space usage is:   ~10:D bytes.~%" (dynamic-usage))
  (format t "Read-only space usage is: ~10:D bytes.~%" (read-only-space-usage))
  (format t "Static space usage is:    ~10:D bytes.~%" (static-space-usage))
  (format t "Control stack usage is:   ~10:D bytes.~%" (control-stack-usage))
  (format t "Binding stack usage is:   ~10:D bytes.~%" (binding-stack-usage))
  (format t "Garbage collection is currently ~:[enabled~;DISABLED~].~%"
	  *gc-inhibit*))

(defun room-intermediate-info ()
  (room-minimal-info)
  (sb!vm:memory-usage :count-spaces '(:dynamic)
		      :print-spaces t
		      :cutoff 0.05s0
		      :print-summary nil))

(defun room-maximal-info ()
  (room-minimal-info)
  (sb!vm:memory-usage :count-spaces '(:static :dynamic))
  (sb!vm:instance-usage :dynamic :top-n 10)
  (sb!vm:instance-usage :static :top-n 10))

(defun room (&optional (verbosity :default))
  #!+sb-doc
  "Prints to *STANDARD-OUTPUT* information about the state of internal
  storage and its management. The optional argument controls the
  verbosity of ROOM. If it is T, ROOM prints out a maximal amount of
  information. If it is NIL, ROOM prints out a minimal amount of
  information. If it is :DEFAULT or it is not supplied, ROOM prints out
  an intermediate amount of information. See also VM:MEMORY-USAGE and
  VM:INSTANCE-USAGE for finer report control."
  (fresh-line)
  (ecase verbosity
    ((t)
     (room-maximal-info))
    ((nil)
     (room-minimal-info))
    (:default
     (room-intermediate-info)))
  (values))

;;;; GET-BYTES-CONSED

;;; internal state
(defvar *last-bytes-in-use* nil)
(defvar *total-bytes-consed* 0)
(declaim (type (or index null) *last-bytes-in-use*))
(declaim (type unsigned-byte *total-bytes-consed*))

(declaim (ftype (function () unsigned-byte) get-bytes-consed))
(defun get-bytes-consed ()
  #!+sb-doc
  "Return the number of bytes consed since the first time this function
  was called. The first time it is called, it returns zero."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((null *last-bytes-in-use*)
	 (setq *last-bytes-in-use* (dynamic-usage))
	 (setq *total-bytes-consed* 0))
	(t
	 (let ((bytes (dynamic-usage)))
	   (incf *total-bytes-consed*
		 (the index (- bytes *last-bytes-in-use*)))
	   (setq *last-bytes-in-use* bytes))))
  ;; FIXME: We should really use something like PCOUNTER to make this
  ;; hold reliably.
  (aver (not (minusp *total-bytes-consed*)))
  *total-bytes-consed*)

;;;; variables and constants

;;; the default value of *BYTES-CONSED-BETWEEN-GCS* and *GC-TRIGGER*
(defconstant default-bytes-consed-between-gcs 2000000)

;;; the minimum amount of dynamic space which must be consed before a
;;; GC will be triggered
;;;
;;; Unlike CMU CL, we don't export this variable. (There's no need to,
;;; since the BYTES-CONSED-BETWEEN-GCS function is SETFable.)
(defvar *bytes-consed-between-gcs* default-bytes-consed-between-gcs)
(declaim (type index *bytes-consed-between-gcs*))

;;;; GC hooks

;;; These variables are a list of functions which are run before and
;;; after garbage collection occurs.
(defvar *before-gc-hooks* nil ; actually initialized in cold init
  #!+sb-doc
  "A list of functions that are called before garbage collection occurs.
  The functions should take no arguments.")
(defvar *after-gc-hooks* nil ; actually initialized in cold init
  #!+sb-doc
  "A list of functions that are called after garbage collection occurs.
  The functions should take no arguments.")

;;; This hook is invoked whenever SUB-GC intends to GC (unless the GC
;;; was explicitly forced by calling SB!EXT:GC). If the hook function
;;; returns NIL then the GC procedes; otherwise, the GC is inhibited and
;;; *GC-INHIBIT* and *NEED-TO-COLLECT-GARBAGE* are left bound to T.
;;; Presumably someone will call GC-ON later to collect the garbage.
(defvar *gc-inhibit-hook* nil
  #!+sb-doc
  "Should be bound to a function or NIL. If it is a function, this
  function should take one argument, the current amount of dynamic
  usage. The function should return NIL if garbage collection should
  continue and non-NIL if it should be inhibited. Use with caution.")

(defvar *gc-notify-stream* nil ; (actually initialized in cold init)
  #!+sb-doc
  "When non-NIL, this must be a STREAM; and the functions bound to
  *GC-NOTIFY-BEFORE* and *GC-NOTIFY-AFTER* are called with the
  STREAM value before and after a garbage collection occurs
  respectively.")

(defvar *gc-run-time* 0
  #!+sb-doc
  "The total CPU time spent doing garbage collection (as reported by
   GET-INTERNAL-RUN-TIME.)")
(declaim (type index *gc-run-time*))

;;; a limit to help catch programs which allocate too much memory,
;;; since a hard heap overflow is so hard to recover from. 
(declaim (type (or unsigned-byte null) *soft-heap-limit*))
(defvar *soft-heap-limit* nil)

;;; Internal trigger. When the dynamic usage increases beyond this
;;; amount, the system notes that a garbage collection needs to occur by
;;; setting *NEED-TO-COLLECT-GARBAGE* to T. It starts out as NIL meaning
;;; nobody has figured out what it should be yet.
(defvar *gc-trigger* nil)

(declaim (type (or index null) *gc-trigger*))

;;; On the X86, we store the GC trigger in a ``static'' symbol instead
;;; of letting magic C code handle it. It gets initialized by the
;;; startup code.
#!+x86
(defvar sb!vm::*internal-gc-trigger*)

;;;; The following specials are used to control when garbage collection
;;;; occurs.

;;; When non-NIL, inhibits garbage collection.
(defvar *gc-inhibit*) ; initialized in cold init

;;; This flag is used to prevent recursive entry into the garbage
;;; collector.
(defvar *already-maybe-gcing*) ; initialized in cold init

;;; When T, indicates that the dynamic usage has exceeded the value
;;; *GC-TRIGGER*.
(defvar *need-to-collect-garbage* nil) ; initialized in cold init

(defun default-gc-notify-before (notify-stream bytes-in-use)
  (declare (type stream notify-stream))
  (format notify-stream
	  "~&; GC is beginning with ~:D bytes in use.~%"
	  bytes-in-use)
  (finish-output notify-stream))
(defparameter *gc-notify-before* #'default-gc-notify-before
  #!+sb-doc
  "This function bound to this variable is invoked before GC'ing (unless
  *GC-NOTIFY-STREAM* is NIL) with the value of *GC-NOTIFY-STREAM* and
  current amount of dynamic usage (in bytes). It should notify the
  user that the system is going to GC.")

(defun default-gc-notify-after (notify-stream
				bytes-retained
				bytes-freed
				new-trigger)
  (declare (type stream notify-stream))
  (format notify-stream
	  "~&; GC has finished with ~:D bytes in use (~:D bytes freed).~%"
	  bytes-retained
	  bytes-freed)
  (format notify-stream
	  "~&; The new GC trigger is ~:D bytes.~%"
	  new-trigger)
  (finish-output notify-stream))
(defparameter *gc-notify-after* #'default-gc-notify-after
  #!+sb-doc
  "The function bound to this variable is invoked after GC'ing with
the value of *GC-NOTIFY-STREAM*, the amount of dynamic usage (in
bytes) now free, the number of bytes freed by the GC, and the new GC
trigger threshold. The function should notify the user that the system
has finished GC'ing.")

;;;; internal GC

(sb!alien:def-alien-routine collect-garbage sb!c-call:int
  #!+gencgc (last-gen sb!c-call:int))


(sb!alien:def-alien-routine set-auto-gc-trigger sb!c-call:void
  (dynamic-usage sb!c-call:unsigned-long))

(sb!alien:def-alien-routine clear-auto-gc-trigger sb!c-call:void)

;;; This variable contains the function that does the real GC. This is
;;; for low-level GC experimentation. Do not touch it if you do not
;;; know what you are doing.
(defvar *internal-gc* #'collect-garbage)

;;;; SUB-GC

;;; Used to carefully invoke hooks.
(eval-when (:compile-toplevel :execute)
  (sb!xc:defmacro carefully-funcall (function &rest args)
    `(handler-case (funcall ,function ,@args)
       (error (cond)
	      (warn "(FUNCALL ~S~{ ~S~}) lost:~%~A" ',function ',args cond)
	      nil))))

;;; SUB-GC decides when and if to do a garbage collection.
;;; The FORCE-P flags controls if a GC should occur even if
;;; the dynamic usage is not greater than *GC-TRIGGER*.
;;;
;;; For GENCGC all generations < GEN will be GC'ed.
(defun sub-gc (&key  force-p (gen 0))
  (/show0 "entering SUB-GC")
  (unless *already-maybe-gcing*
    (let* ((*already-maybe-gcing* t)
	   (start-time (get-internal-run-time))
	   (pre-gc-dyn-usage (dynamic-usage))
	   ;; Currently we only check *SOFT-HEAP-LIMIT* at GC time,
	   ;; not for every allocation. That makes it cheap to do,
	   ;; even if it is a little ugly.
	   (soft-heap-limit-exceeded? (and *soft-heap-limit*
					   (> pre-gc-dyn-usage
					      *soft-heap-limit*)))
	   (*soft-heap-limit* (if soft-heap-limit-exceeded?
				  (+ pre-gc-dyn-usage
				     *bytes-consed-between-gcs*)
				  *soft-heap-limit*)))
      (when soft-heap-limit-exceeded?
	(cerror "Continue with GC."
		"soft heap limit exceeded (temporary new limit=~D)"
		*soft-heap-limit*))
      (unless (integerp (symbol-value '*bytes-consed-between-gcs*))
	;; The noise w/ symbol-value above is to keep the compiler
	;; from optimizing the test away because of the type declaim
	;; for *bytes-consed-between-gcs*.
	;;
	;; FIXME: I'm inclined either to get rid of the DECLAIM or to
	;; trust it, instead of doing this weird hack. It's not
	;; particularly trustable, since (SETF
	;; *BYTES-CONSED-BETWEEN-GCS* 'SEVEN) works. But it's also not
	;; very nice to have the type of the variable specified in two
	;; places which can (and in CMU CL 2.4.8 did, INTEGER vs.
	;; INDEX) drift apart. So perhaps we should just add a note to
	;; the variable documentation for *BYTES-CONSED-BETWEEN-GCS*
	;; that it must be an INDEX, and remove the DECLAIM. Or we
	;; could make a SETFable (BYTES-CONSED-BETWEEN-GCS) function
	;; and enforce the typing that way. And in fact the SETFable
	;; function already exists, so all we need do is make the
	;; variable private, and then we can trust the DECLAIM.
	(warn "The value of *BYTES-CONSED-BETWEEN-GCS*, ~S, is not an ~
	       integer. Resetting it to ~D."
	      *bytes-consed-between-gcs*
	       default-bytes-consed-between-gcs)
	(setf *bytes-consed-between-gcs* default-bytes-consed-between-gcs))
      (when (and *gc-trigger* (> pre-gc-dyn-usage *gc-trigger*))
	(setf *need-to-collect-garbage* t))
      (when (or force-p
		(and *need-to-collect-garbage* (not *gc-inhibit*)))
	(when (and (not force-p)
		   *gc-inhibit-hook*
		   (carefully-funcall *gc-inhibit-hook* pre-gc-dyn-usage))
	  (setf *gc-inhibit* t)
	  (return-from sub-gc nil))
	;; KLUDGE: Wow, we really mask interrupts all the time we're
	;; collecting garbage? That seems like a long time.. -- WHN 19991129
	(without-interrupts
	 ;; FIXME: We probably shouldn't do this evil thing to
	 ;; *STANDARD-OUTPUT* in a binding which is wrapped around
	 ;; calls to user-settable GC hook functions.
	  (let ((*standard-output* *terminal-io*))
	    (when *gc-notify-stream*
	      (if (streamp *gc-notify-stream*)
		  (carefully-funcall *gc-notify-before*
				     *gc-notify-stream*
				     pre-gc-dyn-usage)
	          (warn
		   "*GC-NOTIFY-STREAM* is set, but not a STREAM -- ignored.")))
	    (dolist (hook *before-gc-hooks*)
	      (carefully-funcall hook))
	    (when *gc-trigger*
	      (clear-auto-gc-trigger))
	    #!-gencgc (funcall *internal-gc*)
	    ;; FIXME: This EQ test is pretty gross. Among its other
	    ;; nastinesses, it looks as though it could break if we
	    ;; recompile COLLECT-GARBAGE.
	    #!+gencgc (if (eq *internal-gc* #'collect-garbage)
			  (funcall *internal-gc* gen)
			  (funcall *internal-gc*))
	    (let* ((post-gc-dyn-usage (dynamic-usage))
		   (bytes-freed (- pre-gc-dyn-usage post-gc-dyn-usage)))
	      (/show0 "got (DYNAMIC-USAGE) and BYTES-FREED")
	      (when *last-bytes-in-use*
		(/show0 "doing *LAST-BYTES-IN-USE* thing")
		(incf *total-bytes-consed*
		      (- pre-gc-dyn-usage *last-bytes-in-use*))
		(/show0 "setting *LAST-BYTES-IN-USE*")
		(setq *last-bytes-in-use* post-gc-dyn-usage))
	      (/show0 "clearing *NEED-TO-COLLECT-GARBAGE*")
	      (setf *need-to-collect-garbage* nil)
	      (/show0 "calculating NEW-GC-TRIGGER")
	      (let ((new-gc-trigger (+ post-gc-dyn-usage
				       *bytes-consed-between-gcs*)))
		(/show0 "setting *GC-TRIGGER*")
		(setf *gc-trigger* new-gc-trigger))
	      (/show0 "calling SET-AUTO-GC-TRIGGER")
	      (set-auto-gc-trigger *gc-trigger*)
	      (dolist (hook *after-gc-hooks*)
		(/show0 "doing a hook from *AFTER-GC--HOOKS*")
		;; FIXME: This hook should be called with the same
		;; kind of information as *GC-NOTIFY-AFTER*. In
		;; particular, it would be nice for the hook function
		;; to be able to adjust *GC-TRIGGER* intelligently to
		;; e.g. 108% of total memory usage.
		(carefully-funcall hook))
	      (when *gc-notify-stream*
		(if (streamp *gc-notify-stream*)
		    (carefully-funcall *gc-notify-after*
				       *gc-notify-stream*
				       post-gc-dyn-usage
				       bytes-freed
				       *gc-trigger*)
		    (warn
		     "*GC-NOTIFY-STREAM* is set, but not a stream -- ignored.")))))
	  (scrub-control-stack)))       ;XXX again?  we did this from C ...
      (incf *gc-run-time* (- (get-internal-run-time)
			     start-time))))
  ;; FIXME: should probably return (VALUES), here and in RETURN-FROM
  nil)

;;; This routine is called by the allocation miscops to decide whether
;;; a GC should occur. The argument, OBJECT, is the newly allocated
;;; object which must be returned to the caller.
(defun maybe-gc (&optional object)
  (sub-gc)
  object)

;;; This is the user-advertised garbage collection function.

(defun gc (&key (gen 0) (full nil) &allow-other-keys)
  #!+(and sb-doc gencgc)
  "Initiates a garbage collection.  GEN controls the number of generations to garbage collect"
  #!+(and sb-doc (not gencgc))
  "Initiates a garbage collection.  GEN may be provided for compatibility, but is ignored"
  (sub-gc :force-p t :gen (if full 6 gen)))


;;;; auxiliary functions

(defun bytes-consed-between-gcs ()
  #!+sb-doc
  "Return the amount of memory that will be allocated before the next garbage
   collection is initiated. This can be set with SETF."
  *bytes-consed-between-gcs*)
(defun (setf bytes-consed-between-gcs) (val)
  ;; FIXME: Shouldn't this (and the DECLAIM for the underlying variable)
  ;; be for a strictly positive number type, e.g.
  ;; (AND (INTEGER 1) FIXNUM)?
  (declare (type index val))
  (let ((old *bytes-consed-between-gcs*))
    (setf *bytes-consed-between-gcs* val)
    (when *gc-trigger*
      (setf *gc-trigger* (+ *gc-trigger* (- val old)))
      (cond ((<= (dynamic-usage) *gc-trigger*)
	     (clear-auto-gc-trigger)
	     (set-auto-gc-trigger *gc-trigger*))
	    (t
	     (sb!sys:scrub-control-stack)
	     (sub-gc)))))
  val)

(defun gc-on ()
  #!+sb-doc
  "Enables the garbage collector."
  (setq *gc-inhibit* nil)
  (when *need-to-collect-garbage*
    (sub-gc))
  nil)

(defun gc-off ()
  #!+sb-doc
  "Disables the garbage collector."
  (setq *gc-inhibit* t)
  nil)

;;;; initialization stuff

(defun gc-cold-init-or-reinit ()
  (when *gc-trigger*
    (if (< *gc-trigger* (dynamic-usage))
	(sub-gc)
	(set-auto-gc-trigger *gc-trigger*))))
