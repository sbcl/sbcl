;;;; cold initialization stuff, plus some other miscellaneous stuff
;;;; that we don't have any better place for

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; burning our ships behind us

;;; There's a fair amount of machinery which is needed only at cold
;;; init time, and should be discarded before freezing the final
;;; system. We discard it by uninterning the associated symbols.
;;; Rather than using a special table of symbols to be uninterned,
;;; which might be tedious to maintain, instead we use a hack:
;;; anything whose name matches a magic character pattern is
;;; uninterned.
(defun !unintern-init-only-stuff ()
  (do ((any-changes? nil nil))
      (nil)
    (dolist (package (list-all-packages))
      (do-symbols (symbol package)
	(let ((name (symbol-name symbol)))
	  (when (or (string= name "!" :end1 1 :end2 1)
		    (and (>= (length name) 2)
			 (string= name "*!" :end1 2 :end2 2)))
	    (/show0 "uninterning cold-init-only symbol..")
	    (/primitive-print name)
	    (unintern symbol package)
	    (setf any-changes? t)))))
    (unless any-changes?
      (return))))

;;;; !COLD-INIT

;;; a list of toplevel things set by GENESIS
(defvar *!reversed-cold-toplevels*)

;;; a SIMPLE-VECTOR set by GENESIS
(defvar *!load-time-values*)

(defun !cold-lose (msg)
  (%primitive print msg)
  (%primitive print "too early in cold init to recover from errors")
  (%halt))

#!+gengc
(defun !do-load-time-value-fixup (object offset index)
  (declare (type index offset))
  (let ((value (svref *!load-time-values* index)))
    (typecase object
      (list
       (case offset
	 (0 (setf (car object) value))
	 (1 (setf (cdr object) value))
	 (t (!cold-lose "bogus offset in cons cell"))))
      (instance
       (setf (%instance-ref object (- offset sb!vm:instance-slots-offset))
	     value))
      (code-component
       (setf (code-header-ref object offset) value))
      (simple-vector
       (setf (svref object (- offset sb!vm:vector-data-offset)) value))
      (t
       (!cold-lose "unknown kind of object for load-time-value fixup")))))

(eval-when (:compile-toplevel :execute)
  ;; FIXME: Perhaps we should make SHOW-AND-CALL-AND-FMAKUNBOUND, too,
  ;; and use it for most of the cold-init functions. (Just be careful
  ;; not to use it for the COLD-INIT-OR-REINIT functions.)
  (sb!xc:defmacro show-and-call (name)
    `(progn
       (/primitive-print ,(symbol-name name))
       (,name))))

;;; called when a cold system starts up
(defun !cold-init ()
  #!+sb-doc "Give the world a shove and hope it spins."

  (/show0 "entering !COLD-INIT")

  ;; FIXME: It'd probably be cleaner to have most of the stuff here
  ;; handled by calls like !GC-COLD-INIT, !ERROR-COLD-INIT, and
  ;; !UNIX-COLD-INIT. And *TYPE-SYSTEM-INITIALIZED* could be changed to
  ;; *TYPE-SYSTEM-INITIALIZED-WHEN-BOUND* so that it doesn't need to
  ;; be explicitly set in order to be meaningful.
  (setf *gc-verbose* nil)
  (setf *gc-notify-stream* nil)
  (setf *before-gc-hooks* nil)
  (setf *after-gc-hooks* nil)
  #!+gengc (setf *handler-clusters* nil)
  #!-gengc (setf *already-maybe-gcing* t
		 *gc-inhibit* t
		 *need-to-collect-garbage* nil
		 sb!unix::*interrupts-enabled* t
		 sb!unix::*interrupt-pending* nil)
  (setf *break-on-signals* nil)
  (setf *maximum-error-depth* 10)
  (setf *current-error-depth* 0)
  (setf *cold-init-complete-p* nil)
  (setf *type-system-initialized* nil)

  ;; Anyone might call RANDOM to initialize a hash value or something;
  ;; and there's nothing which needs to be initialized in order for
  ;; this to be initialized, so we initialize it right away.
  (show-and-call !random-cold-init)

  (show-and-call !package-cold-init)

  ;; All sorts of things need INFO and/or (SETF INFO).
  (/show0 "about to SHOW-AND-CALL !GLOBALDB-COLD-INIT")
  (show-and-call !globaldb-cold-init)

  ;; This needs to be done early, but needs to be after INFO is
  ;; initialized.
  (show-and-call !fdefn-cold-init)

  ;; Various toplevel forms call MAKE-ARRAY, which calls SUBTYPEP, so
  ;; the basic type machinery needs to be initialized before toplevel
  ;; forms run.
  (show-and-call !type-class-cold-init)
  (show-and-call !typedefs-cold-init)
  (show-and-call !classes-cold-init)
  (show-and-call !early-type-cold-init)
  (show-and-call !late-type-cold-init)
  (show-and-call !alien-type-cold-init)
  (show-and-call !target-type-cold-init)
  (show-and-call !vm-type-cold-init)
  ;; FIXME: It would be tidy to make sure that that these cold init
  ;; functions are called in the same relative order as the toplevel
  ;; forms of the corresponding source files.

  ;;(show-and-call !package-cold-init)
  (show-and-call !policy-cold-init-or-resanify)
  (/show0 "back from !POLICY-COLD-INIT-OR-RESANIFY")

  ;; KLUDGE: Why are fixups mixed up with toplevel forms? Couldn't
  ;; fixups be done separately? Wouldn't that be clearer and better?
  ;; -- WHN 19991204
  (/show0 "doing cold toplevel forms and fixups")
  (/show0 "(LISTP *!REVERSED-COLD-TOPLEVELS*)=..")
  (/hexstr (if (listp *!reversed-cold-toplevels*) "true" "NIL"))
  (/show0 "about to calculate (LENGTH *!REVERSED-COLD-TOPLEVELS*)")
  (/show0 "(LENGTH *!REVERSED-COLD-TOPLEVELS*)=..")
  #!+sb-show (let ((r-c-tl-length (length *!reversed-cold-toplevels*)))
	       (/show0 "(length calculated..)")
	       (let ((hexstr (hexstr r-c-tl-length)))
		 (/show0 "(hexstr calculated..)")
		 (/primitive-print hexstr)))
  (let (#!+sb-show (index-in-cold-toplevels 0))
    #!+sb-show (declare (type fixnum index-in-cold-toplevels))
    (dolist (toplevel-thing (prog1
				(nreverse *!reversed-cold-toplevels*)
			      ;; (Now that we've NREVERSEd it, it's
			      ;; somewhat scrambled, so keep anyone
			      ;; else from trying to get at it.)
			      (makunbound '*!reversed-cold-toplevels*)))
      #!+sb-show
      (when (zerop (mod index-in-cold-toplevels 1024))
	(/show0 "INDEX-IN-COLD-TOPLEVELS=..")
	(/hexstr index-in-cold-toplevels))
      #!+sb-show
      (setf index-in-cold-toplevels
	    (the fixnum (1+ index-in-cold-toplevels)))
      (typecase toplevel-thing
	(function
	 (funcall toplevel-thing))
	(cons
	 (case (first toplevel-thing)
	   (:load-time-value
	    (setf (svref *!load-time-values* (third toplevel-thing))
		  (funcall (second toplevel-thing))))
	   (:load-time-value-fixup
	    #!-gengc
	    (setf (sap-ref-32 (second toplevel-thing) 0)
		  (get-lisp-obj-address
		   (svref *!load-time-values* (third toplevel-thing))))
	    #!+gengc
	    (!do-load-time-value-fixup (second toplevel-thing)
				       (third  toplevel-thing)
				       (fourth toplevel-thing)))
	   #!+(and x86 gencgc)
	   (:load-time-code-fixup
	    (sb!vm::!do-load-time-code-fixup (second toplevel-thing)
					     (third  toplevel-thing)
					     (fourth toplevel-thing)
					     (fifth  toplevel-thing)))
	   (t
	    (!cold-lose "bogus fixup code in *!REVERSED-COLD-TOPLEVELS*"))))
	(t (!cold-lose "bogus function in *!REVERSED-COLD-TOPLEVELS*")))))
  (/show0 "done with loop over cold toplevel forms and fixups")

  ;; Set sane values again, so that the user sees sane values instead
  ;; of whatever is left over from the last DECLAIM/PROCLAIM.
  (show-and-call !policy-cold-init-or-resanify)

  ;; Only do this after toplevel forms have run, 'cause that's where
  ;; DEFTYPEs are.
  (setf *type-system-initialized* t)

  (show-and-call os-cold-init-or-reinit)

  (show-and-call stream-cold-init-or-reset)
  (show-and-call !loader-cold-init)
  (show-and-call signal-cold-init-or-reinit)
  (setf (sb!alien:extern-alien "internal_errors_enabled" boolean) t)

  ;; FIXME: This list of modes should be defined in one place and
  ;; explicitly shared between here and REINIT.
  (set-floating-point-modes :traps '(:overflow
				     #!-x86 :underflow
				     :invalid
				     :divide-by-zero))

  (show-and-call !class-finalize)

  ;; The reader and printer are initialized very late, so that they
  ;; can do hairy things like invoking the compiler as part of their
  ;; initialization.
  (show-and-call !reader-cold-init)
  (let ((*readtable* *standard-readtable*))
    (show-and-call !sharpm-cold-init)
    (show-and-call !backq-cold-init))
  (setf *readtable* (copy-readtable *standard-readtable*))
  (setf sb!debug:*debug-readtable* (copy-readtable *standard-readtable*))
  (sb!pretty:!pprint-cold-init)

  ;; the ANSI-specified initial value of *PACKAGE*
  (setf *package* (find-package "COMMON-LISP-USER"))
  ;; FIXME: I'm not sure where it should be done, but CL-USER really
  ;; ought to USE-PACKAGE publicly accessible packages like SB-DEBUG
  ;; (for ARG and VAR), SB-EXT, SB-EXT-C-CALL, and SB-EXT-ALIEN so
  ;; that the user has a hint about which symbols we consider public.
  ;; (Perhaps SB-DEBUG wouldn't need to be in the list if ARG and VAR
  ;; could be typed directly, with no parentheses, at the debug prompt
  ;; the way that e.g. F or BACKTRACE can be?)

  (/show0 "done initializing")
  (setf *cold-init-complete-p* t)

  ;; The system is finally ready for GC.
  #!-gengc (setf *already-maybe-gcing* nil)
  (/show0 "enabling GC")
  (gc-on)
  (/show0 "doing first GC")
  (gc :full t)
  (/show0 "back from first GC")

  ;; The show is on.
  (terpri)
  (/show0 "going into toplevel loop")
  (handling-end-of-the-world 
    (toplevel-init)))

(defun quit (&key recklessly-p
		  (unix-code 0 unix-code-p)
		  (unix-status unix-code))
  #!+sb-doc
  "Terminate the current Lisp. Things are cleaned up (with UNWIND-PROTECT
  and so forth) unless RECKLESSLY-P is non-NIL. On UNIX-like systems,
  UNIX-STATUS is used as the status code."
  (declare (type (signed-byte 32) unix-code))
  ;; FIXME: UNIX-CODE was deprecated in sbcl-0.6.8, after having been
  ;; around for less than a year. It should be safe to remove it after
  ;; a year.
  (when unix-code-p
    (warn "The UNIX-CODE argument is deprecated. Use the UNIX-STATUS argument
instead (which is another name for the same thing)."))
  (if recklessly-p
      (sb!unix:unix-exit unix-status)
      (throw '%end-of-the-world unix-status)))

;;;; initialization functions

(defun reinit ()
  (without-interrupts
    (without-gcing
      (os-cold-init-or-reinit)
      (stream-reinit)
      (signal-cold-init-or-reinit)
      (gc-cold-init-or-reinit)
      (setf (sb!alien:extern-alien "internal_errors_enabled" boolean) t)
      (set-floating-point-modes :traps
				'(:overflow
				  :invalid
				  :divide-by-zero
				  ;; PRINT seems not to like x86 NPX
				  ;; denormal floats like
				  ;; LEAST-NEGATIVE-SINGLE-FLOAT, so
				  ;; the :UNDERFLOW exceptions are
				  ;; disabled by default. Joe User can
				  ;; explicitly enable them if
				  ;; desired.
				  #!-x86 :underflow))
      ;; Clear pseudo atomic in case this core wasn't compiled with
      ;; support.
      ;;
      ;; FIXME: In SBCL our cores are always compiled with support. So
      ;; we don't need to do this, do we? At least not for this
      ;; reason.. (Perhaps we should do it anyway in case someone
      ;; manages to save an image from within a pseudo-atomic-atomic
      ;; operation?)
      #!+x86 (setf sb!impl::*pseudo-atomic-atomic* 0))
    (gc-on)))

;;;; some support for any hapless wretches who end up debugging cold
;;;; init code

;;; Decode THING into hexadecimal notation using only machinery
;;; available early in cold init.
#!+sb-show
(defun hexstr (thing)
  (/noshow0 "entering HEXSTR")
  (let ((addr (sb!kernel:get-lisp-obj-address thing))
	(str (make-string 10)))
    (/noshow0 "ADDR and STR calculated")
    (setf (char str 0) #\0
	  (char str 1) #\x)
    (/noshow0 "CHARs 0 and 1 set")
    (dotimes (i 8)
      (/noshow0 "at head of DOTIMES loop")
      (let* ((nibble (ldb (byte 4 0) addr))
	     (chr (char "0123456789abcdef" nibble)))
	(declare (type (unsigned-byte 4) nibble)
		 (base-char chr))
	(/noshow0 "NIBBLE and CHR calculated")
	(setf (char str (- 9 i)) chr
	      addr (ash addr -4))))
    str))

#!+sb-show
(defun cold-print (x)
  (typecase x
    (simple-string (sb!sys:%primitive print x))
    (symbol (sb!sys:%primitive print (symbol-name x)))
    (list (let ((count 0))
	    (sb!sys:%primitive print "list:")
	    (dolist (i x)
	      (when (>= (incf count) 4)
		(sb!sys:%primitive print "...")
		(return))
	      (cold-print i))))
    (t (sb!sys:%primitive print (hexstr x)))))
