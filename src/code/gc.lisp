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
#!+gencgc
(def-c-var-frob dynamic-usage "bytes_allocated")
#!-gencgc
(defun dynamic-usage ()
  (the (unsigned-byte 32)
       (- (sb!sys:sap-int (sb!c::dynamic-space-free-pointer))
          (current-dynamic-space-start))))

(defun static-space-usage ()
  (- (* sb!vm:*static-space-free-pointer* sb!vm:n-word-bytes)
     sb!vm:static-space-start))

(defun read-only-space-usage ()
  (- (* sb!vm::*read-only-space-free-pointer* sb!vm:n-word-bytes)
     sb!vm:read-only-space-start))

(defun control-stack-usage ()
  #!-stack-grows-downward-not-upward
  (- (sb!sys:sap-int (sb!c::control-stack-pointer-sap))
     (sb!vm:fixnumize sb!vm::*control-stack-start*))
  #!+stack-grows-downward-not-upward
  (- (sb!vm:fixnumize sb!vm::*control-stack-end*)
     (sb!sys:sap-int (sb!c::control-stack-pointer-sap))))

(defun binding-stack-usage ()
  (- (sb!sys:sap-int (sb!c::binding-stack-pointer-sap))
     (sb!vm:fixnumize sb!vm::*binding-stack-start*)))

;;;; ROOM

(defun room-minimal-info ()
  (format t "Dynamic space usage is:   ~10:D bytes.~%" (dynamic-usage))
  (format t "Read-only space usage is: ~10:D bytes.~%" (read-only-space-usage))
  (format t "Static space usage is:    ~10:D bytes.~%" (static-space-usage))
  (format t "Control stack usage is:   ~10:D bytes.~%" (control-stack-usage))
  (format t "Binding stack usage is:   ~10:D bytes.~%" (binding-stack-usage))
  #!+sb-thread
  (format t 
	  "Control and binding stack usage is for the current thread only.~%")
  (format t "Garbage collection is currently ~:[enabled~;DISABLED~].~%"
	  (> *gc-inhibit* 0)))

(defun room-intermediate-info ()
  (room-minimal-info)
  (sb!vm:memory-usage :count-spaces '(:dynamic)
		      :print-spaces t
		      :cutoff 0.05f0
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
(defvar *bytes-consed-between-gcs*
  #!+gencgc (* 4 (expt 10 6))
  ;; Stop-and-copy GC is really really slow when used too often. CSR
  ;; reported that even on his old 64 Mb SPARC, 20 Mb is much faster
  ;; than 4 Mb when rebuilding SBCL ca. 0.7.1. For modern machines
  ;; with >> 128 Mb memory, the optimum could be significantly more
  ;; than this, but at least 20 Mb should be better than 4 Mb.
  #!-gencgc (* 20 (expt 10 6)))
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

;;;; The following specials are used to control when garbage
;;;; collection occurs.

;;; When the dynamic usage increases beyond this amount, the system
;;; notes that a garbage collection needs to occur by setting
;;; *NEED-TO-COLLECT-GARBAGE* to T. It starts out as NIL meaning
;;; nobody has figured out what it should be yet.
;;;
;;; FIXME: *GC-TRIGGER* seems to be denominated in bytes, not words.
;;; And limiting it to INDEX is fairly reasonable in order to avoid
;;; bignum arithmetic on every allocation, and to minimize the need
;;; for thought about weird gotchas of the GC-control mechanism itself
;;; consing as it operates. But as of sbcl-0.7.5, 512Mbytes of memory
;;; costs $54.95 at Fry's in Dallas but cheap consumer 64-bit machines
;;; are still over the horizon, so gratuitously limiting our heap size
;;; to FIXNUM bytes seems fairly stupid. It'd be reasonable to
;;; (1) allow arbitrary UNSIGNED-BYTE values of *GC-TRIGGER*, or
;;; (2) redenominate this variable in words instead of bytes, postponing
;;;     the problem to heaps which exceed 50% of the machine's address
;;;     space, or even
;;; (3) redemoninate this variable in CONS-sized two-word units,
;;;     allowing it to cover the entire memory space at the price of
;;;     possible loss of clarity.
;;; (And whatever is done, it'd also be good to rename the variable so
;;; that it's clear what unit it's denominated in.)
(declaim (type (or index null) *gc-trigger*))
(defvar *gc-trigger* nil)

;;; When >0, inhibits garbage collection.
(defvar *gc-inhibit*) ; initialized in cold init

;;; When T, indicates that a GC should have happened but did not due to 
;;; *GC-INHIBIT*. 
(defvar *need-to-collect-garbage* nil) ; initialized in cold init

;;;; internal GC

(sb!alien:define-alien-routine collect-garbage sb!alien:int
  (#!+gencgc last-gen #!-gencgc ignore sb!alien:int))

#!+sb-thread
(def-c-var-frob gc-thread-pid "gc_thread_pid")

	

;;;; SUB-GC

;;; This is used to carefully invoke hooks.
(eval-when (:compile-toplevel :execute)
  (sb!xc:defmacro carefully-funcall (function &rest args)
    `(handler-case (funcall ,function ,@args)
       (error (cond)
	      (warn "(FUNCALL ~S~{ ~S~}) lost:~%~A" ',function ',args cond)
	      nil))))

;;; SUB-GC does a garbage collection.  This is called from three places:
;;; (1) The C runtime will call here when it detects that we've consed 
;;;     enough to exceed the gc trigger threshold
;;; (2) The user may request a collection using GC, below
;;; (3) At the end of a WITHOUT-GCING section, we are called if
;;;     *NEED-TO-COLLECT-GARBAGE* is true
;;;
;;; This is different from the behaviour in 0.7 and earlier: it no
;;; longer decides whether to GC based on thresholds.  If you call
;;; SUB-GC you will definitely get a GC either now or when the
;;; WITHOUT-GCING is over

;;; For GENCGC all generations < GEN will be GC'ed.

#!+sb-thread
(defun sub-gc (&key (gen 0))
  (setf *need-to-collect-garbage* t)
  (when (zerop *gc-inhibit*)
    (setf (sb!alien:extern-alien "maybe_gc_pending" (sb!alien:unsigned 32))
	  (1+ gen))
    (if (zerop (sb!alien:extern-alien "stop_the_world" (sb!alien:unsigned 32)))
	(sb!unix:unix-kill (gc-thread-pid) :SIGALRM))
    (loop
     (when (zerop
	    (sb!alien:extern-alien "maybe_gc_pending" (sb!alien:unsigned 32)))
       (return nil)))
    (setf *need-to-collect-garbage* nil)
    (scrub-control-stack))
  (values))

#!-sb-thread
(defvar *already-in-gc* nil "System is running SUB-GC")
#!-sb-thread
(defun sub-gc (&key (gen 0))
  (when *already-in-gc* (return-from sub-gc nil))
  (setf *need-to-collect-garbage* t)
  (when (zerop *gc-inhibit*)
    (let ((*already-in-gc* t))
      (without-interrupts (collect-garbage gen))
      (setf *need-to-collect-garbage* nil))
    (scrub-control-stack))
  (values))
       


;;; This is the user-advertised garbage collection function.
(defun gc (&key (gen 0) (full nil) &allow-other-keys)
  #!+(and sb-doc gencgc)
  "Initiate a garbage collection. GEN controls the number of generations
  to garbage collect."
  #!+(and sb-doc (not gencgc))
  "Initiate a garbage collection. GEN may be provided for compatibility with
  generational garbage collectors, but is ignored in this implementation."
  (sub-gc  :gen (if full 6 gen)))


;;;; auxiliary functions

(defun bytes-consed-between-gcs ()
  #!+sb-doc
  "Return the amount of memory that will be allocated before the next garbage
   collection is initiated. This can be set with SETF."
  (sb!alien:extern-alien "bytes_consed_between_gcs"
			 (sb!alien:unsigned 32)))

(defun (setf bytes-consed-between-gcs) (val)
  (declare (type index val))
  (setf (sb!alien:extern-alien "bytes_consed_between_gcs"
			       (sb!alien:unsigned 32))
	val))

(defun gc-on ()
  #!+sb-doc
  "Enable the garbage collector."
  (setq *gc-inhibit* 0)
  (when *need-to-collect-garbage*
    (sub-gc))
  nil)

(defun gc-off ()
  #!+sb-doc
  "Disable the garbage collector."
  (setq *gc-inhibit* 1)
  nil)

