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

#!-gencgc
(progn
  ;; This is called once per PROFILEd function call, so it's worth a
  ;; little possible space cost to reduce its time cost.
  #!-sb-fluid
  (declaim (inline current-dynamic-space-start))
  (def-c-var-frob current-dynamic-space-start "current_dynamic_space"))

#!-sb-fluid
(declaim (inline dynamic-usage)) ; to reduce PROFILEd call overhead
#!+(or cgc gencgc)
(def-c-var-frob dynamic-usage "bytes_allocated")
#!-(or cgc gencgc)
(defun dynamic-usage ()
  (the (unsigned-byte 32)
       (- (sb!sys:sap-int (sb!c::dynamic-space-free-pointer))
          (current-dynamic-space-start))))

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
  "Print to *STANDARD-OUTPUT* information about the state of internal
  storage and its management. The optional argument controls the
  verbosity of output. If it is T, ROOM prints out a maximal amount of
  information. If it is NIL, ROOM prints out a minimal amount of
  information. If it is :DEFAULT or it is not supplied, ROOM prints out
  an intermediate amount of information."
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

;;; the total number of bytes freed so far (including any freeing
;;; which goes on in PURIFY)
;;;
;;; (We save this so that we can calculate the total number of bytes
;;; ever allocated by adding this to the number of bytes currently
;;; allocated and never freed.)
(declaim (type unsigned-byte *n-bytes-freed-or-purified*))
(defvar *n-bytes-freed-or-purified* 0)
(push (lambda ()
	(setf *n-bytes-freed-or-purified* 0))
      ;; KLUDGE: It's probably not quite safely right either to do
      ;; this in *BEFORE-SAVE-INITIALIZATIONS* (since consing, or even
      ;; worse, something which depended on (GET-BYTES-CONSED), might
      ;; happen after that) or in *AFTER-SAVE-INITIALIZATIONS*. But
      ;; it's probably not a big problem, and there seems to be no
      ;; other obvious time to do it. -- WHN 2001-07-30
      *after-save-initializations*)

(declaim (ftype (function () unsigned-byte) get-bytes-consed))
(defun get-bytes-consed ()
  #!+sb-doc
  "Return the number of bytes consed since the program began. Typically
this result will be a consed bignum, so if you have an application (e.g.
profiling) which can't tolerate the overhead of consing bignums, you'll
probably want either to hack in at a lower level (as the code in the
SB-PROFILE package does), or to design a more microefficient interface
and submit it as a patch."
  (+ (dynamic-usage)
     *n-bytes-freed-or-purified*))

;;;; variables and constants

;;; the minimum amount of dynamic space which must be consed before a
;;; GC will be triggered
;;;
;;; Unlike CMU CL, we don't export this variable. (There's no need to,
;;; since our BYTES-CONSED-BETWEEN-GCS function is SETFable.)
(defvar *bytes-consed-between-gcs* (* 2 (expt 10 6)))
(declaim (type index *bytes-consed-between-gcs*))

;;;; GC hooks

(defvar *before-gc-hooks* nil ; actually initialized in cold init
  #!+sb-doc
  "A list of functions that are called before garbage collection occurs.
  The functions should take no arguments.")

(defvar *after-gc-hooks* nil ; actually initialized in cold init
  #!+sb-doc
  "A list of functions that are called after garbage collection occurs.
  The functions should take no arguments.")

(defvar *gc-notify-stream* nil ; (actually initialized in cold init)
  #!+sb-doc
  "When non-NIL, this must be a STREAM; and the functions bound to
  *GC-NOTIFY-BEFORE* and *GC-NOTIFY-AFTER* are called with the
  STREAM value before and after a garbage collection occurs
  respectively.")

(defvar *gc-run-time* 0
  #!+sb-doc
  "the total CPU time spent doing garbage collection (as reported by
   GET-INTERNAL-RUN-TIME)")
(declaim (type index *gc-run-time*))

;;; a limit to help catch programs which allocate too much memory,
;;; since a hard heap overflow is so hard to recover from
(declaim (type (or unsigned-byte null) *soft-heap-limit*))
(defvar *soft-heap-limit* nil)

;;; When the dynamic usage increases beyond this amount, the system
;;; notes that a garbage collection needs to occur by setting
;;; *NEED-TO-COLLECT-GARBAGE* to T. It starts out as NIL meaning
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
	  "~&; GC is beginning with ~:D bytes in use at internal runtime ~:D.~%"
	  bytes-in-use
	  (get-internal-run-time))
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
	  "~&; GC has finished with ~:D bytes in use (~:D bytes freed)~@
           ; at internal runtime ~:D. The new GC trigger is ~:D bytes.~%"
	  bytes-retained
	  bytes-freed
	  (get-internal-run-time)
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

;;; SUB-GC decides when and if to do a garbage collection. The FORCE-P
;;; flags controls whether a GC should occur even if the dynamic usage
;;; is not greater than *GC-TRIGGER*.
;;;
;;; For GENCGC all generations < GEN will be GC'ed.
(defun sub-gc (&key force-p (gen 0))
  (/show0 "entering SUB-GC")
  (unless *already-maybe-gcing*
    (let* ((*already-maybe-gcing* t)
	   (start-time (get-internal-run-time))
	   (pre-gc-dynamic-usage (dynamic-usage))
	   ;; Currently we only check *SOFT-HEAP-LIMIT* at GC time,
	   ;; not for every allocation. That makes it cheap to do,
	   ;; even if it is a little ugly.
	   (soft-heap-limit-exceeded? (and *soft-heap-limit*
					   (> pre-gc-dynamic-usage
					      *soft-heap-limit*)))
	   (*soft-heap-limit* (if soft-heap-limit-exceeded?
				  (+ pre-gc-dynamic-usage
				     *bytes-consed-between-gcs*)
				  *soft-heap-limit*)))
      (when soft-heap-limit-exceeded?
	(cerror "Continue with GC."
		"soft heap limit exceeded (temporary new limit=~D)"
		*soft-heap-limit*))
      (when (and *gc-trigger* (> pre-gc-dynamic-usage *gc-trigger*))
	(setf *need-to-collect-garbage* t))
      (when (or force-p
		(and *need-to-collect-garbage* (not *gc-inhibit*)))
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
				     pre-gc-dynamic-usage)
	          (warn
		   "*GC-NOTIFY-STREAM* is set, but not a STREAM -- ignored.")))
	    (dolist (hook *before-gc-hooks*)
	      (carefully-funcall hook))
	    (when *gc-trigger*
	      (clear-auto-gc-trigger))
	    (let* (;; We do DYNAMIC-USAGE once more here in order to
		   ;; get a more accurate measurement of the space
		   ;; actually freed, since the messing around, e.g.
		   ;; GC-notify stuff, since the DYNAMIC-USAGE which
		   ;; triggered GC could've done a fair amount of
		   ;; consing.)
		   (pre-internal-gc-dynamic-usage (dynamic-usage))
		   (ignore-me
		    #!-gencgc (funcall *internal-gc*)
		    ;; FIXME: This EQ test is pretty gross. Among its other
		    ;; nastinesses, it looks as though it could break if we
		    ;; recompile COLLECT-GARBAGE. We should probably just
		    ;; straighten out the interface so that all *INTERNAL-GC*
		    ;; functions accept a GEN argument (and then the
		    ;; non-generational ones just ignore it).
		    #!+gencgc (if (eq *internal-gc* #'collect-garbage)
				  (funcall *internal-gc* gen)
				  (funcall *internal-gc*)))
		   (post-gc-dynamic-usage (dynamic-usage))
		   (n-bytes-freed (- pre-internal-gc-dynamic-usage
				     post-gc-dynamic-usage))
		   ;; In sbcl-0.6.12.39, the raw N-BYTES-FREED from
		   ;; GENCGC could sometimes be substantially negative
		   ;; (e.g. -5872). I haven't looked into what causes
		   ;; that, but I suspect it has to do with
		   ;; fluctuating inefficiency in the way that the
		   ;; GENCGC packs things into page boundaries.
		   ;; Bumping the raw result up to 0 is a little ugly,
		   ;; but shouldn't be a problem, and it's even
		   ;; possible to sort of justify it: the packing
		   ;; inefficiency which has caused (DYNAMIC-USAGE) to
		   ;; grow is effectively consing, or at least
		   ;; overhead of consing, so it's sort of correct to
		   ;; add it to the running total of consing. ("Man
		   ;; isn't a rational animal, he's a rationalizing
		   ;; animal.":-) -- WHN 2001-06-23
		   (eff-n-bytes-freed (max 0 n-bytes-freed)))
	      (declare (ignore ignore-me))
	      (/show0 "got (DYNAMIC-USAGE) and EFF-N-BYTES-FREED")
	      (incf *n-bytes-freed-or-purified*
		    eff-n-bytes-freed)
	      (/show0 "clearing *NEED-TO-COLLECT-GARBAGE*")
	      (setf *need-to-collect-garbage* nil)
	      (/show0 "calculating NEW-GC-TRIGGER")
	      (let ((new-gc-trigger (+ post-gc-dynamic-usage
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
				       post-gc-dynamic-usage
				       eff-n-bytes-freed
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
  "Initiate a garbage collection. GEN controls the number of generations
  to garbage collect."
  #!+(and sb-doc (not gencgc))
  "Initiate a garbage collection. GEN may be provided for compatibility with
  generational garbage collectors, but is ignored in this implementation."
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
	     ;; FIXME: If SCRUB-CONTROL-STACK is required here, why
	     ;; isn't it built into SUB-GC? And *is* it required here?
	     (sb!sys:scrub-control-stack)
	     (sub-gc)))))
  val)

(defun gc-on ()
  #!+sb-doc
  "Enable the garbage collector."
  (setq *gc-inhibit* nil)
  (when *need-to-collect-garbage*
    (sub-gc))
  nil)

(defun gc-off ()
  #!+sb-doc
  "Disable the garbage collector."
  (setq *gc-inhibit* t)
  nil)

;;;; initialization stuff

(defun gc-reinit ()
  (when *gc-trigger*
    (if (< *gc-trigger* (dynamic-usage))
	(sub-gc)
	(set-auto-gc-trigger *gc-trigger*))))
