;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-PROFILE")

;;;; reading internal run time with high resolution and low overhead

;;; FIXME: It might make sense to replace this with something
;;; with finer resolution, e.g. milliseconds or microseconds.
;;; For that matter, maybe we should boost the internal clock
;;; up to something faster, like milliseconds.

(defconstant +ticks-per-second+ internal-time-units-per-second)

(declaim (inline get-internal-ticks))
(defun get-internal-ticks () (get-internal-run-time))

;;;; PCOUNTER

;;; a PCOUNTER is used to represent an unsigned integer quantity which
;;; can grow bigger than a fixnum, but typically does so, if at all,
;;; in many small steps, where we don't want to cons on every step.
;;; (Total system consing, time spent in a profiled function, and
;;; bytes consed in a profiled function are all examples of such
;;; quantities.)
(defstruct (pcounter (:copier nil))
  (integer 0 :type unsigned-byte)
  (fixnum 0 :type (and fixnum unsigned-byte)))

(declaim (ftype (function (pcounter integer) pcounter) incf-pcounter))
;;;(declaim (inline incf-pcounter)) ; FIXME: maybe inline when more stable
(defun incf-pcounter (pcounter delta)
  (let ((sum (+ (pcounter-fixnum pcounter) delta)))
    (cond ((typep sum 'fixnum)
	   (setf (pcounter-fixnum pcounter) sum))
	  (t
	   (incf (pcounter-integer pcounter) sum)
	   (setf (pcounter-fixnum pcounter) 0))))
  pcounter)

(declaim (ftype (function (pcounter) integer) pcounter->integer))
;;;(declaim (inline pcounter->integer)) ; FIXME: maybe inline when more stable
(defun pcounter->integer (pcounter)
  (+ (pcounter-integer pcounter)
     (pcounter-fixnum pcounter)))

;;;; operations on (OR PCOUNTER FIXNUM)
;;;;
;;;; When we don't want to cons a PCOUNTER unless we're forced to, we
;;;; start with a FIXNUM counter and only create a PCOUNTER if the
;;;; FIXNUM overflows.

(declaim (ftype (function ((or pcounter fixnum) integer) (or pcounter fixnum)) %incf-pcounter-or-fixnum))
;;;(declaim (inline %incf-pcounter-or-fixnum)) ; FIXME: maybe inline when more stable
(defun %incf-pcounter-or-fixnum (x delta)
  (etypecase x
    (fixnum
     (let ((sum (+ x delta)))
       (if (typep sum 'fixnum)
	   sum
	   (make-pcounter :integer sum))))
    (pcounter
     (incf-pcounter x delta))))
  
(define-modify-macro incf-pcounter-or-fixnum (delta) %incf-pcounter-or-fixnum)

;;; Trade off space for execution time by handling the common fast
;;; (TYPEP DELTA 'FIXNUM) case inline and only calling generic
;;; arithmetic as a last resort.
(defmacro fastbig-incf-pcounter-or-fixnum (x delta)
  (once-only ((delta delta))
    `(etypecase ,delta
       (fixnum (incf-pcounter-or-fixnum ,x ,delta))
       (integer (incf-pcounter-or-fixnum ,x ,delta)))))

(declaim (ftype (function ((or pcounter fixnum)) integer) pcounter-or-fixnum->integer))
(declaim (maybe-inline pcounter-or-fixnum->integer))
(defun pcounter-or-fixnum->integer (x)
  (etypecase x
    (fixnum x)
    (pcounter (pcounter->integer x))))

;;;; implementation-dependent interfaces

#|
;;; To avoid unnecessary consing in the "encapsulation" code, we want
;;; find out the number of required arguments, and use &REST to
;;; capture only non-required arguments. This function returns (VALUES
;;; MIN-ARGS OPTIONALS-P), where MIN-ARGS is the number of required
;;; arguments and OPTIONALS-P is true iff there are any non-required
;;; arguments (such as &OPTIONAL, &REST, or &KEY).
(declaim (ftype (function ((or symbol cons)) (values fixnum t)) fun-signature))
(defun fun-signature (name)
  (let ((type (info :function :type name)))
    (cond ((not (function-type-p type))
	   (values 0 t))
	  (t
	   (values (length (function-type-required type))
		   (or (function-type-optional type)
		       (function-type-keyp type)
		       (function-type-rest type)))))))
|#

;;;; global data structures

;;; We associate a PROFILE-INFO structure with each profiled function
;;; name. This holds the functions that we call to manipulate the
;;; closure which implements the encapsulation.
(defvar *profiled-function-name->info* (make-hash-table))
(defstruct (profile-info (:copier nil))
  (name              (required-argument) :read-only t)
  (encapsulated-fun  (required-argument) :type function :read-only t)
  (encapsulation-fun (required-argument) :type function :read-only t)
  (read-stats-fun    (required-argument) :type function :read-only t)
  (clear-stats-fun   (required-argument) :type function :read-only t))

;;; These variables are used to subtract out the time and consing for
;;; recursive and other dynamically nested profiled calls. The total
;;; resource consumed for each nested call is added into the
;;; appropriate variable. When the outer function returns, these
;;; amounts are subtracted from the total.
(defvar *enclosed-ticks* 0)
(defvar *enclosed-consing* 0)
(declaim (type (or pcounter fixnum) *enclosed-ticks* *enclosed-consing*))

;;; This variable is also used to subtract out time for nested
;;; profiled calls. The time inside the profile wrapper call --
;;; between its two calls to GET-INTERNAL-TICKS -- is accounted
;;; for by the *ENCLOSED-TIME* variable. However, there's also extra
;;; overhead involved, before we get to the first call to
;;; GET-INTERNAL-TICKS, and after we get to the second call. By
;;; keeping track of the count of enclosed profiled calls, we can try
;;; to compensate for that.
(defvar *enclosed-profiles* 0)
(declaim (type (or pcounter fixnum) *enclosed-profiles*))

;;; the components of profiling overhead
(defstruct (overhead (:copier nil))
  ;; the number of ticks a bare function call takes. This is
  ;; factored into the other overheads, but not used for itself.
  (call (required-argument) :type single-float :read-only t)
  ;; the number of ticks that will be charged to a profiled
  ;; function due to the profiling code
  (internal (required-argument) :type single-float :read-only t)
  ;; the number of ticks of overhead for profiling that a single
  ;; profiled call adds to the total runtime for the program
  (total (required-argument) :type single-float :read-only t))
(defvar *overhead*)
(declaim (type overhead *overhead*))

;;;; profile encapsulations

;;; Trade off space for time by handling the usual all-FIXNUM cases
;;; inline.
(defmacro fastbig- (x y)
  (once-only ((x x) (y y))
    `(if (and (typep ,x 'fixnum)
	      (typep ,y 'fixnum))
	 (- ,x ,y)
	 (- ,x ,y))))
(defmacro fastbig-1+ (x)
  (once-only ((x x))
    `(if (typep ,x 'index)
	 (1+ ,x)
	 (1+ ,x))))

;;; Return a collection of closures over the same lexical context,
;;;   (VALUES ENCAPSULATION-FUN READ-STATS-FUN CLEAR-STATS-FUN).
;;;
;;; ENCAPSULATION-FUN is a plug-in replacement for ENCAPSULATED-FUN,
;;; which updates statistics whenver it's called.
;;;
;;; READ-STATS-FUN returns the statistics:
;;;   (VALUES COUNT TIME CONSING PROFILE).
;;; COUNT is the count of calls to ENCAPSULATION-FUN. TICKS is
;;; the total number of ticks spent in ENCAPSULATED-FUN.
;;; CONSING is the total consing of ENCAPSULATION-FUN. PROFILE is the
;;; number of calls to the profiled function, stored for the purposes
;;; of trying to estimate that part of profiling overhead which occurs
;;; outside the interval between the profile wrapper function's timer
;;; calls.
;;;
;;; CLEAR-STATS-FUN clears the statistics.
;;;
;;; (The reason for implementing this as coupled closures, with the
;;; counts built into the lexical environment, is that we hope this
;;; will minimize profiling overhead.)
(defun profile-encapsulation-lambdas (encapsulated-fun)
  (declare (type function encapsulated-fun))
  (declare (optimize speed safety))
  (let* ((count 0)
	 (ticks 0)
	 (consing 0)
	 (profiles 0))
    (declare (type (or pcounter fixnum) count ticks consing profiles))
    (values
     ;; ENCAPSULATION-FUN
     (lambda (sb-c:&more arg-context arg-count)
       #+nil (declare (optimize (speed 3) (safety 0))) ; FIXME: remove #+NIL?
       (fastbig-incf-pcounter-or-fixnum count 1)
       (let ((dticks 0)
	     (dconsing 0)
	     (inner-enclosed-profiles 0))
	 (declare (type unsigned-byte dticks dconsing))
	 (declare (type unsigned-byte inner-enclosed-profiles))
	 (multiple-value-prog1
	     (let ((start-ticks (get-internal-ticks))
		   ;; KLUDGE: We add (THE UNSIGNED-BYTE ..) wrappers
		   ;; around GET-BYTES-CONSED because as of
		   ;; sbcl-0.6.4, at the time that the FTYPE of
		   ;; GET-BYTES-CONSED is DECLAIMed, the
		   ;; cross-compiler's type system isn't mature enough
		   ;; to do anything about it. -- WHN 20000503
		   (start-consing (the unsigned-byte (get-bytes-consed)))
		   (*enclosed-ticks* 0)
		   (*enclosed-consing* 0)
		   (*enclosed-profiles* 0))
	       (declare (inline pcounter-or-fixnum->integer))
	       (multiple-value-prog1
		   (multiple-value-call encapsulated-fun
					(sb-c:%more-arg-values arg-context
							       0
							       arg-count))
		 (setf dticks (fastbig- (get-internal-ticks) start-ticks)
		       dconsing (fastbig- (the unsigned-byte
					       (get-bytes-consed))
					  start-consing))
		 (setf inner-enclosed-profiles
		       (pcounter-or-fixnum->integer *enclosed-profiles*))
		 (fastbig-incf-pcounter-or-fixnum ticks (fastbig-
							 dticks
							 *enclosed-ticks*))
		 (fastbig-incf-pcounter-or-fixnum consing
						  (fastbig-
						   dconsing
						   *enclosed-consing*))
		 (fastbig-incf-pcounter-or-fixnum profiles
						  inner-enclosed-profiles)))
	   (fastbig-incf-pcounter-or-fixnum *enclosed-ticks* dticks)
	   (fastbig-incf-pcounter-or-fixnum *enclosed-consing* dconsing)
	   (fastbig-incf-pcounter-or-fixnum *enclosed-profiles*
					    (fastbig-1+
					     inner-enclosed-profiles)))))
     ;; READ-STATS-FUN
     (lambda ()
       (values (pcounter-or-fixnum->integer count)
	       (pcounter-or-fixnum->integer ticks)
	       (pcounter-or-fixnum->integer consing)
	       (pcounter-or-fixnum->integer profiles)))
     ;; CLEAR-STATS-FUN
     (lambda ()
       (setf count 0
	     ticks 0
	     consing 0
	     profiles 0)))))

;;;; interfaces

;;; A symbol or (SETF FOO) list names a function, a string names all
;;; the functions named by symbols in the named package.
(defun mapc-on-named-functions (function names)
  (dolist (name names)
    (etypecase name
      (symbol (funcall function name))
      (list
       ;; We call this just for the side effect of checking that
       ;; NAME is a legal function name:
       (function-name-block-name name)
       ;; Then we map onto it.
       (funcall function name))
      (string (let ((package (find-undeleted-package-or-lose name)))
		(do-symbols (symbol package)
		  (when (eq (symbol-package symbol) package)
		    (when (fboundp symbol)
		      (funcall function symbol))
		    (let ((setf-name `(setf ,symbol)))
		      (when (fboundp setf-name)
			(funcall function setf-name)))))))))
  (values))

;;; Profile the named function, which should exist and not be profiled
;;; already.
(defun profile-1-unprofiled-function (name)
  (let ((encapsulated-fun (fdefinition name)))
    (multiple-value-bind (encapsulation-fun read-stats-fun clear-stats-fun)
	(profile-encapsulation-lambdas encapsulated-fun)
      (setf (fdefinition name)
	    encapsulation-fun)
      (setf (gethash name *profiled-function-name->info*)
	    (make-profile-info :name name
			       :encapsulated-fun encapsulated-fun
			       :encapsulation-fun encapsulation-fun
			       :read-stats-fun read-stats-fun
			       :clear-stats-fun clear-stats-fun))
      (values))))

;;; Profile the named function. If already profiled, unprofile first.
(defun profile-1-function (name)
  (cond ((fboundp name)
	 (when (gethash name *profiled-function-name->info*)
	   (warn "~S is already profiled, so unprofiling it first." name)
	   (unprofile-1-function name))
	 (profile-1-unprofiled-function name))
	(t
	 (warn "ignoring undefined function ~S" name)))
  (values))

;;; Unprofile the named function, if it is profiled.
(defun unprofile-1-function (name)
  (let ((pinfo (gethash name *profiled-function-name->info*)))
    (cond (pinfo
	   (remhash name *profiled-function-name->info*)
	   (if (eq (fdefinition name) (profile-info-encapsulation-fun pinfo))
	       (setf (fdefinition name) (profile-info-encapsulated-fun pinfo))
	       (warn "preserving current definition of redefined function ~S"
		     name)))
	  (t
	   (warn "~S is not a profiled function." name))))
  (values))

(defmacro profile (&rest names)
  #+sb-doc
  "PROFILE Name*

   If no names are supplied, return the list of profiled functions.

   If names are supplied, wrap profiling code around the named functions.
   As in TRACE, the names are not evaluated. A symbol names a function.
   A string names all the functions named by symbols in the named
   package. If a function is already profiled, then unprofile and
   reprofile (useful to notice function redefinition.)  If a name is
   undefined, then we give a warning and ignore it. See also
   UNPROFILE, REPORT and RESET."
  (if (null names)
      `(loop for k being each hash-key in *profiled-function-name->info*
	     collecting k)
      `(mapc-on-named-functions #'profile-1-function ',names)))

(defmacro unprofile (&rest names)
  #+sb-doc
  "Unwrap any profiling code around the named functions, or if no names
  are given, unprofile all profiled functions. A symbol names
  a function. A string names all the functions named by symbols in the
  named package. NAMES defaults to the list of names of all currently 
  profiled functions."
  (if names
      `(mapc-on-named-functions #'unprofile-1-function ',names)
      `(unprofile-all)))

(defun unprofile-all ()
  (dohash (name profile-info *profiled-function-name->info*)
    (declare (ignore profile-info))
    (unprofile-1-function name)))

(defun reset ()
  "Reset the counters for all profiled functions."
  (dohash (name profile-info *profiled-function-name->info*)
    (declare (ignore name))
    (funcall (profile-info-clear-stats-fun profile-info))))

;;;; reporting results

(defstruct (time-info (:copier nil))
  name
  calls
  seconds
  consing)

;;; Return our best guess for the run time in a function, subtracting
;;; out factors for profiling overhead. We subtract out the internal
;;; overhead for each call to this function, since the internal
;;; overhead is the part of the profiling overhead for a function that
;;; is charged to that function.
;;;
;;; We also subtract out a factor for each call to a profiled function
;;; within this profiled function. This factor is the total profiling
;;; overhead *minus the internal overhead*. We don't subtract out the
;;; internal overhead, since it was already subtracted when the nested
;;; profiled functions subtracted their running time from the time for
;;; the enclosing function.
(defun compensate-time (calls ticks profile)
  (let ((raw-compensated
	 (- (/ (float ticks) (float +ticks-per-second+))
	    (* (overhead-internal *overhead*) (float calls))
	    (* (- (overhead-total *overhead*)
		  (overhead-internal *overhead*))
	       (float profile)))))
    (max raw-compensated 0.0)))

(defun report ()
  "Report results from profiling. The results are
approximately adjusted for profiling overhead, but when RAW is true
the unadjusted results are reported. The compensation may be somewhat
inaccurate when bignums are involved in runtime calculation, as in
a very-long-running Lisp process."
  (declare (optimize (speed 0)))
  (unless (boundp '*overhead*)
    (setf *overhead*
	  (compute-overhead)))
  (let ((time-info-list ())
	(no-call-name-list ()))
    (dohash (name pinfo *profiled-function-name->info*)
      (unless (eq (fdefinition name)
		  (profile-info-encapsulation-fun pinfo))
	(warn "Function ~S has been redefined, so times may be inaccurate.~@
	       PROFILE it again to record calls to the new definition."
	      name))
      (multiple-value-bind (calls ticks consing profile)
	  (funcall (profile-info-read-stats-fun pinfo))
	(if (zerop calls)
	    (push name no-call-name-list)
	    (push (make-time-info :name name
				  :calls calls
				  :seconds (compensate-time calls
							    ticks
							    profile)
				  :consing consing)
		  time-info-list))))

    (setf time-info-list
	  (sort time-info-list
		#'>=
		:key #'time-info-seconds))

    (format *trace-output*
	    "~&  seconds  |  consed   |  calls  |  sec/call  |  name~@
	       ------------------------------------------------------~%")

    (let ((total-time 0.0)
	  (total-consed 0)
	  (total-calls 0))
      (dolist (time-info time-info-list)
	(incf total-time (time-info-seconds time-info))
	(incf total-calls (time-info-calls time-info))
	(incf total-consed (time-info-consing time-info))
	(format *trace-output*
		"~10,3F | ~9:D | ~7:D | ~10,6F | ~S~%"
		(time-info-seconds time-info)
		(time-info-consing time-info)
		(time-info-calls time-info)
		(/ (time-info-seconds time-info)
		   (float (time-info-calls time-info)))
		(time-info-name time-info)))
      (format *trace-output*
	      "------------------------------------------------------~@
	      ~10,3F | ~9:D | ~7:D |	    | Total~%"
	      total-time total-consed total-calls)
      (format *trace-output*
	      "~%estimated total profiling overhead: ~4,2F seconds~%"
	      (* (overhead-total *overhead*) (float total-calls)))
      (format *trace-output*
	      "~&overhead estimation parameters:~%  ~Ss/call, ~Ss total profiling, ~Ss internal profiling~%"
	      (overhead-call *overhead*)
	      (overhead-total *overhead*)
	      (overhead-internal *overhead*)))

    (when no-call-name-list
      (format *trace-output*
	      "~%These functions were not called:~%~{~<~%~:; ~S~>~}~%"
	      (sort no-call-name-list #'string<
		    :key (lambda (name)
			   (symbol-name (function-name-block-name name))))))

    (values)))

;;;; overhead estimation

;;; We average the timing overhead over this many iterations.
(defconstant +timer-overhead-iterations+ 50000)

;;; a dummy function that we profile to find profiling overhead
(declaim (notinline compute-overhead-aux))
(defun compute-overhead-aux (x)
  (declare (ignore x)))

;;; Return a newly computed OVERHEAD object.
(defun compute-overhead ()
  (flet ((frob ()
	   (let ((start (get-internal-ticks))
	         (fun (symbol-function 'compute-overhead-aux)))
	     (dotimes (i +timer-overhead-iterations+)
	       (funcall fun fun))
	     (/ (float (- (get-internal-ticks) start))
		(float +ticks-per-second+)
		(float +timer-overhead-iterations+)))))
    (let (;; Measure unprofiled calls to estimate call overhead.
	  (call-overhead (frob))
	  total-overhead
	  internal-overhead)
      ;; Measure profiled calls to estimate profiling overhead.
      (unwind-protect
	  (progn
	    (profile compute-overhead-aux)
	    (setf total-overhead
		  (- (frob) call-overhead)))
	(let* ((pinfo (gethash 'compute-overhead-aux
			       *profiled-function-name->info*))
	       (read-stats-fun (profile-info-read-stats-fun pinfo))
	       (time (nth-value 1 (funcall read-stats-fun))))
	  (setf internal-overhead
		(/ (float time)
		   (float +ticks-per-second+)
		   (float +timer-overhead-iterations+))))
	(unprofile compute-overhead-aux))
      (make-overhead :call call-overhead
		     :total total-overhead
		     :internal internal-overhead))))

;;; It would be bad to compute *OVERHEAD*, save it into a .core file,
;;; then load old *OVERHEAD* value from the .core file into a
;;; different machine running at a different speed. We avoid this by
;;; erasing *CALL-OVERHEAD* whenever we save a .core file.
(pushnew (lambda ()
	   (makunbound '*overhead*))
	 *before-save-initializations*)
