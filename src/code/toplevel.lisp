;;;; stuff related to the toplevel read-eval-print loop, plus some
;;;; other miscellaneous functions that we don't have any better place
;;;; for

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defconstant most-positive-fixnum #.sb!vm:*target-most-positive-fixnum*
  #!+sb-doc
  "The fixnum closest in value to positive infinity.")

(defconstant most-negative-fixnum #.sb!vm:*target-most-negative-fixnum*
  #!+sb-doc
  "The fixnum closest in value to negative infinity.")

;;;; magic specials initialized by genesis

#!-gengc
(progn
  (defvar *current-catch-block*)
  (defvar *current-unwind-protect-block*)
  (defvar *free-interrupt-context-index*))

;;; specials initialized by !COLD-INIT

;;; FIXME: These could be converted to DEFVARs, and the stuff shared
;;; in both #!+GENGC and #!-GENGC (actually everything in #!+GENGC)
;;; could be made non-conditional.
(declaim
  #!-gengc
  (special *gc-inhibit* *already-maybe-gcing*
	   *need-to-collect-garbage* *gc-verbose*
	   *gc-notify-stream*
	   *before-gc-hooks* *after-gc-hooks*
	   #!+x86 *pseudo-atomic-atomic*
	   #!+x86 *pseudo-atomic-interrupted*
	   sb!unix::*interrupts-enabled*
	   sb!unix::*interrupt-pending*
	   *type-system-initialized*)
  #!+gengc
  (special *gc-verbose* *before-gc-hooks* *after-gc-hooks*
	   *gc-notify-stream*
	   *type-system-initialized*))

(defvar *cold-init-complete-p*)

;;; counts of nested errors (with internal errors double-counted)
(defvar *maximum-error-depth*)
(defvar *current-error-depth*)

;;;; miscellaneous utilities for working with with TOPLEVEL

;;; Execute BODY in a context where any %END-OF-THE-WORLD (thrown e.g.
;;; by QUIT) is caught and any final processing and return codes are
;;; handled appropriately.
(defmacro handling-end-of-the-world (&body body)
  (let ((caught (gensym "CAUGHT")))
    `(let ((,caught (catch '%end-of-the-world
		      (/show0 "inside CATCH '%END-OF-THE-WORLD")
		      (restart-case (progn ,@body)
			;; KLUDGE: I'd like to name this restart QUIT,
			;; but then people would hate me, since in CMU
			;; CL, even though they have essentially the
			;; same QUIT function as SBCL, the "QUIT"
			;; command in the debugger means to return to
			;; the toplevel, not to actually call QUIT. Oh
			;; well. -- WHN 2000-11-01
			(end-of-the-world ()
			  :report (lambda (s)
				    (format s
					    "Terminate the current Lisp, ~
                                            like #'~S."
					    'quit))
			  (quit))))))
       (/show0 "back from CATCH '%END-OF-THE-WORLD, flushing output")
       (flush-standard-output-streams)
       (/show0 "calling UNIX-EXIT")
       (sb!unix:unix-exit ,caught))))

;;;; working with *CURRENT-ERROR-DEPTH* and *MAXIMUM-ERROR-DEPTH*

;;; INFINITE-ERROR-PROTECT is used by ERROR and friends to keep us out of
;;; hyperspace.
(defmacro infinite-error-protect (&rest forms)
  `(unless (infinite-error-protector)
     (let ((*current-error-depth* (1+ *current-error-depth*)))
       ,@forms)))

;;; a helper function for INFINITE-ERROR-PROTECT
(defun infinite-error-protector ()
  (cond ((not *cold-init-complete-p*)
	 (%primitive print "Argh! error in cold init, halting")
	 (%primitive sb!c:halt))
	((or (not (boundp '*current-error-depth*))
	     (not (realp   *current-error-depth*))
	     (not (boundp '*maximum-error-depth*))
	     (not (realp   *maximum-error-depth*)))
	 (%primitive print "Argh! corrupted error depth, halting")
	 (%primitive sb!c:halt))
	((> *current-error-depth* *maximum-error-depth*)
	 (/show0 "in INFINITE-ERROR-PROTECTOR, calling ERROR-ERROR")
	 (error-error "Help! "
		      *current-error-depth*
		      " nested errors. "
		      "KERNEL:*MAXIMUM-ERROR-DEPTH* exceeded.")
	 t)
	(t
	 nil)))

;;; FIXME: I had a badly broken version of INFINITE-ERROR-PROTECTOR at
;;; one point (shown below), and SBCL cross-compiled it without
;;; warning about FORMS being undefined. Check whether that problem
;;; (missing warning) is repeatable in the final system and if so, fix
;;; it.
#|
(defun infinite-error-protector ()
  `(cond ((not *cold-init-complete-p*)
	  (%primitive print "Argh! error in cold init, halting")
	  (%primitive sb!c:halt))
	 ((or (not (boundp '*current-error-depth*))
	      (not (realp   *current-error-depth*))
	      (not (boundp '*maximum-error-depth*))
	      (not (realp   *maximum-error-depth*)))
	  (%primitive print "Argh! corrupted error depth, halting")
	  (%primitive sb!c:halt))
	 ((> *current-error-depth* *maximum-error-depth*)
	  (/show0 "in INFINITE-ERROR-PROTECTOR, calling ERROR-ERROR")
	  (error-error "Help! "
		       *current-error-depth*
		       " nested errors. "
		       "KERNEL:*MAXIMUM-ERROR-DEPTH* exceeded.")
	  (progn ,@forms)
	  t)
	 (t
	  (/show0 "in INFINITE-ERROR-PROTECTOR, returning normally")
	  nil)))
|#

;;;; miscellaneous external functions

#!-mp ; The multi-processing version is defined in multi-proc.lisp.
(defun sleep (n)
  #!+sb-doc
  "This function causes execution to be suspended for N seconds. N may
  be any non-negative, non-complex number."
  (when (or (not (realp n))
	    (minusp n))
    (error "Invalid argument to SLEEP: ~S.~%~
	    Must be a non-negative, non-complex number."
	   n))
  (multiple-value-bind (sec usec)
      (if (integerp n)
	  (values n 0)
	  (multiple-value-bind (sec frac)
	      (truncate n)
	    (values sec(truncate frac 1e-6))))
    (sb!unix:unix-select 0 0 0 0 sec usec))
  nil)

;;;; SCRUB-CONTROL-STACK

(defconstant bytes-per-scrub-unit 2048)

(defun scrub-control-stack ()
  #!+sb-doc
  "Zero the unused portion of the control stack so that old objects are not
   kept alive because of uninitialized stack variables."
  ;; FIXME: Why do we need to do this instead of just letting GC read
  ;; the stack pointer and avoid messing with the unused portion of
  ;; the control stack? (Is this a multithreading thing where there's
  ;; one control stack and stack pointer per thread, and it might not
  ;; be easy to tell what a thread's stack pointer value is when
  ;; looking in from another thread?)
  (declare (optimize (speed 3) (safety 0))
	   (values (unsigned-byte 20))) ; FIXME: DECLARE VALUES?

  #!-x86 ; machines where stack grows upwards (I guess) -- WHN 19990906
  (labels
      ((scrub (ptr offset count)
         (declare (type system-area-pointer ptr)
		  (type (unsigned-byte 16) offset)
		  (type (unsigned-byte 20) count)
		  (values (unsigned-byte 20)))
	 (cond ((= offset bytes-per-scrub-unit)
		(look (sap+ ptr bytes-per-scrub-unit) 0 count))
	       (t
		(setf (sap-ref-32 ptr offset) 0)
		(scrub ptr (+ offset sb!vm:word-bytes) count))))
       (look (ptr offset count)
	 (declare (type system-area-pointer ptr)
		  (type (unsigned-byte 16) offset)
		  (type (unsigned-byte 20) count)
		  (values (unsigned-byte 20)))
	 (cond ((= offset bytes-per-scrub-unit)
		count)
	       ((zerop (sap-ref-32 ptr offset))
		(look ptr (+ offset sb!vm:word-bytes) count))
	       (t
		(scrub ptr offset (+ count sb!vm:word-bytes))))))
    (let* ((csp (sap-int (sb!c::control-stack-pointer-sap)))
	   (initial-offset (logand csp (1- bytes-per-scrub-unit))))
      (declare (type (unsigned-byte 32) csp))
      (scrub (int-sap (- csp initial-offset))
	     (* (floor initial-offset sb!vm:word-bytes) sb!vm:word-bytes)
	     0)))

  #!+x86 ;; (Stack grows downwards.)
  (labels
      ((scrub (ptr offset count)
	 (declare (type system-area-pointer ptr)
		  (type (unsigned-byte 16) offset)
		  (type (unsigned-byte 20) count)
		  (values (unsigned-byte 20)))
	 (let ((loc (int-sap (- (sap-int ptr) (+ offset sb!vm:word-bytes)))))
	   (cond ((= offset bytes-per-scrub-unit)
		  (look (int-sap (- (sap-int ptr) bytes-per-scrub-unit))
			0 count))
		 (t ;; need to fix bug in %SET-STACK-REF
		  (setf (sap-ref-32 loc 0) 0)
		  (scrub ptr (+ offset sb!vm:word-bytes) count)))))
       (look (ptr offset count)
	 (declare (type system-area-pointer ptr)
		  (type (unsigned-byte 16) offset)
		  (type (unsigned-byte 20) count)
		  (values (unsigned-byte 20)))
	 (let ((loc (int-sap (- (sap-int ptr) offset))))
	   (cond ((= offset bytes-per-scrub-unit)
		  count)
		 ((zerop (sb!kernel::get-lisp-obj-address (stack-ref loc 0)))
		  (look ptr (+ offset sb!vm:word-bytes) count))
		 (t
		  (scrub ptr offset (+ count sb!vm:word-bytes)))))))
    (let* ((csp (sap-int (sb!c::control-stack-pointer-sap)))
	   (initial-offset (logand csp (1- bytes-per-scrub-unit))))
      (declare (type (unsigned-byte 32) csp))
      (scrub (int-sap (+ csp initial-offset))
	     (* (floor initial-offset sb!vm:word-bytes) sb!vm:word-bytes)
	     0))))

;;;; the default toplevel function

(defvar / nil
  #!+sb-doc
  "a list of all the values returned by the most recent top-level EVAL")
(defvar //  nil #!+sb-doc "the previous value of /")
(defvar /// nil #!+sb-doc "the previous value of //")
(defvar *   nil #!+sb-doc "the value of the most recent top-level EVAL")
(defvar **  nil #!+sb-doc "the previous value of *")
(defvar *** nil #!+sb-doc "the previous value of **")
(defvar +   nil #!+sb-doc "the value of the most recent top-level READ")
(defvar ++  nil #!+sb-doc "the previous value of +")
(defvar +++ nil #!+sb-doc "the previous value of ++")
(defvar -   nil #!+sb-doc "the form currently being evaluated")
(defvar *prompt* "* "
  #!+sb-doc
  "The top-level prompt string. This also may be a function of no arguments
   that returns a simple-string.")

(defun interactive-eval (form)
  "Evaluate FORM, returning whatever it returns and adjusting ***, **, *,
   +++, ++, +, ///, //, /, and -."
  (setf - form)
  (let ((results (multiple-value-list (eval form))))
    (setf /// //
	  // /
	  / results
	  *** **
	  ** *
	  * (car results)))
  (setf +++ ++
	++ +
	+ -)
  (unless (boundp '*)
    ;; The bogon returned an unbound marker.
    ;; FIXME: It would be safer to check every one of the values in RESULTS,
    ;; instead of just the first one.
    (setf * nil)
    (cerror "Go on with * set to NIL."
	    "EVAL returned an unbound marker."))
  (values-list /))

;;; Flush anything waiting on one of the ANSI Common Lisp standard
;;; output streams before proceeding.
(defun flush-standard-output-streams ()
  (dolist (name '(*debug-io*
		  *error-output*
		  *query-io*
		  *standard-output*
		  *trace-output*))
    (finish-output (symbol-value name)))
  (values))

;;; the default system top-level function
(defun toplevel-init ()

  (/show0 "entering TOPLEVEL-INIT")
  
  (let ((sysinit nil)      ; value of --sysinit option
	(userinit nil)     ; value of --userinit option
	(evals nil)	   ; values of --eval options (in reverse order)
	(noprint nil)      ; Has a --noprint option been seen?
	(noprogrammer nil) ; Has a --noprogammer option been seen?
	(options (rest *posix-argv*))) ; skipping program name

    (/show0 "done with outer LET in TOPLEVEL-INIT")
  
    ;; FIXME: There are lots of ways for errors to happen around here (e.g. bad
    ;; command line syntax, or READ-ERROR while trying to READ an --eval
    ;; string). Make sure that they're handled reasonably.

    ;; Parse command line options.
    (loop while options do
	  (/show0 "at head of LOOP WHILE OPTIONS DO in TOPLEVEL-INIT")
	  (let ((option (first options)))
	    (flet ((pop-option ()
		     (if options
			 (pop options)
			 (error "unexpected end of command line options"))))
	      (cond ((string= option "--sysinit")
		     (pop-option)
		     (if sysinit
			 (error "multiple --sysinit options")
			 (setf sysinit (pop-option))))
		    ((string= option "--userinit")
		     (pop-option)
		     (if userinit
			 (error "multiple --userinit options")
			 (setf userinit (pop-option))))
		    ((string= option "--eval")
		     (pop-option)
		     (let ((eval-as-string (pop-option)))
		       (with-input-from-string (eval-stream eval-as-string)
			 (let* ((eof-marker (cons :eof :eof))
				(eval (read eval-stream nil eof-marker))
				(eof (read eval-stream nil eof-marker)))
			   (cond ((eq eval eof-marker)
				  (error "unable to parse ~S"
					 eval-as-string))
				 ((not (eq eof eof-marker))
				  (error "more than one expression in ~S"
					 eval-as-string))
				 (t
				  (push eval evals)))))))
		    ((string= option "--noprint")
		     (pop-option)
		     (setf noprint t))
		    ((string= option "--noprogrammer")
		     (pop-option)
		     (setf noprogrammer t))
		    ((string= option "--end-toplevel-options")
		     (pop-option)
		     (return))
		    (t
		     ;; Anything we don't recognize as a toplevel
		     ;; option must be the start of user-level
		     ;; options.. except that if we encounter
		     ;; "--end-toplevel-options" after we gave up
		     ;; because we didn't recognize an option as a
		     ;; toplevel option, then the option we gave up on
		     ;; must have been an error. (E.g. in
		     ;;   sbcl --eval '(a)' --evl '(b)' --end-toplevel-options
		     ;; this test will let us detect that "--evl" is
		     ;; an error.)
		     (if (find "--end-toplevel-options" options
			       :test #'string=)
			 (error "bad toplevel option: ~S" (first options))
			 (return)))))))
    (/show0 "done with LOOP WHILE OPTIONS DO in TOPLEVEL-INIT")

    ;; Excise all the options that we processed, so that only user-level
    ;; options are left visible to user code.
    (setf (rest *posix-argv*) options)

    ;; FIXME: Verify that errors in init files and/or --eval operations
    ;; lead to reasonable behavior.

    ;; Handle initialization files.
    (/show0 "handling initialization files in TOPLEVEL-INIT")
    (flet (;; If any of POSSIBLE-INIT-FILE-NAMES names a real file,
	   ;; return its truename.
	   (probe-init-files (&rest possible-init-file-names)
	     (/show0 "entering PROBE-INIT-FILES")
	     (prog1
		 (find-if (lambda (x)
			    (and (stringp x) (probe-file x)))
			  possible-init-file-names)
	       (/show0 "leaving PROBE-INIT-FILES"))))
      (let* ((sbcl-home (posix-getenv "SBCL_HOME"))
	     (sysinit-truename (if sbcl-home
				   (probe-init-files sysinit
						     (concatenate
						      'string
						      sbcl-home
						      "/sbclrc"))
				   (probe-init-files sysinit
						     "/etc/sbclrc"
						     "/usr/local/etc/sbclrc")))
	     (user-home (or (posix-getenv "HOME")
			    (error "The HOME environment variable is unbound, ~
				    so user init file can't be found.")))
	     (userinit-truename (probe-init-files userinit
						  (concatenate
						   'string
						   user-home
						   "/.sbclrc"))))
	(/show0 "assigned SYSINIT-TRUENAME and USERINIT-TRUENAME")
	(when sysinit-truename
	  (unless (load sysinit-truename)
	    (error "~S was not successfully loaded." sysinit-truename))
	  (flush-standard-output-streams))
	(/show0 "loaded SYSINIT-TRUENAME")
	(when userinit-truename
	  (unless (load userinit-truename)
	    (error "~S was not successfully loaded." userinit-truename))
	  (flush-standard-output-streams))
	(/show0 "loaded USERINIT-TRUENAME"))

      ;; Handle --eval options.
      (/show0 "handling --eval options in TOPLEVEL-INIT")
      (dolist (eval (reverse evals))
	(/show0 "handling one --eval option in TOPLEVEL-INIT")
	(eval eval)
	(flush-standard-output-streams))

      ;; Handle stream binding controlled by --noprogrammer option.
      ;;
      ;; FIXME: When we do actually implement this, shouldn't it go
      ;; earlier in the sequence, so that its stream bindings will
      ;; affect the behavior of init files and --eval options?
      (/show0 "handling --noprogrammer option in TOPLEVEL-INIT")
      (when noprogrammer
	(warn "stub: --noprogrammer option unimplemented")) ; FIXME

      (/show0 "falling into TOPLEVEL-REPL from TOPLEVEL-INIT")
      (toplevel-repl noprint))))

;;; read-eval-print loop for the default system toplevel
(defun toplevel-repl (noprint)
  (/show0 "entering TOPLEVEL-REPL")
  (let ((* nil) (** nil) (*** nil)
	(- nil)
	(+ nil) (++ nil) (+++ nil)
	(/// nil) (// nil) (/ nil)
	(eof-marker (cons :eof nil)))
    (loop
      (/show0 "at head of outer LOOP in TOPLEVEL-REPL")
      ;; There should only be one TOPLEVEL restart, and it's here, so
      ;; restarting at TOPLEVEL always bounces you all the way out here.
      (with-simple-restart (toplevel
			    "Restart at toplevel READ/EVAL/PRINT loop.")
	;; We add a new ABORT restart for every debugger level, so 
	;; restarting at ABORT in a nested debugger gets you out to the
        ;; innermost enclosing debugger, and only when you're in the
        ;; outermost, unnested debugger level does restarting at ABORT 
	;; get you out to here.
        (with-simple-restart (abort
			      "Reduce debugger level (leaving debugger).")
	  (catch 'top-level-catcher
	  (sb!unix:unix-sigsetmask 0)	; FIXME: What is this for?
	  (/show0 "about to enter inner LOOP in TOPLEVEL-REPL")
	  (loop				; FIXME: Do we need this inner LOOP?
	   ;; FIXME: It seems bad to have GC behavior depend on scrubbing
	   ;; the control stack before each interactive command. Isn't
	   ;; there some way we can convince the GC to just ignore
	   ;; dead areas of the control stack, so that we don't need to
	   ;; rely on this half-measure?
	   (scrub-control-stack)
	   (unless noprint
	     (fresh-line)
	     (princ (if (functionp *prompt*)
			(funcall *prompt*)
			*prompt*))
	     (flush-standard-output-streams))
	   (let ((form (read *standard-input* nil eof-marker)))
	     (if (eq form eof-marker)
		 (quit)
		 (let ((results
			(multiple-value-list (interactive-eval form))))
		   (unless noprint
		     (dolist (result results)
		       (fresh-line)
		       (prin1 result)))))))))))))

;;; a convenient way to get into the assembly-level debugger
(defun %halt ()
  (%primitive sb!c:halt))
