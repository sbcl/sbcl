;;;; low-level time functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defconstant sb!xc:internal-time-units-per-second 1000
  #!+sb-doc
  "The number of internal time units that fit into a second. See
  GET-INTERNAL-REAL-TIME and GET-INTERNAL-RUN-TIME.")

(defconstant micro-seconds-per-internal-time-unit
  (/ 1000000 sb!xc:internal-time-units-per-second))

;;; The base number of seconds for our internal "epoch". We initialize
;;; this to the time of the first call to GET-INTERNAL-REAL-TIME, and
;;; then subtract this out of the result.
(defvar *internal-real-time-base-seconds* nil)
(declaim (type (or (unsigned-byte 32) null) *internal-real-time-base-seconds*))

(defun get-internal-real-time ()
  #!+sb-doc
  "Return the real time in the internal time format. This is useful for
  finding elapsed time. See INTERNAL-TIME-UNITS-PER-SECOND."
  (multiple-value-bind (ignore seconds useconds) (sb!unix:unix-gettimeofday)
    (declare (ignore ignore) (type (unsigned-byte 32) seconds useconds))
    (let ((base *internal-real-time-base-seconds*)
	  (uint (truncate useconds
			  micro-seconds-per-internal-time-unit)))
      (declare (type (unsigned-byte 32) uint))
      (cond (base
	     (truly-the (unsigned-byte 32)
			(+ (the (unsigned-byte 32)
				(* (the (unsigned-byte 32) (- seconds base))
				   sb!xc:internal-time-units-per-second))
			   uint)))
	    (t
	     (setq *internal-real-time-base-seconds* seconds)
	     uint)))))

(defun get-internal-run-time ()
  #!+sb-doc
  "Return the run time in the internal time format. This is useful for
  finding CPU usage."
  (declare (values (unsigned-byte 32)))
  (multiple-value-bind (ignore utime-sec utime-usec stime-sec stime-usec)
      (sb!unix:unix-fast-getrusage sb!unix:rusage_self)
    (declare (ignore ignore)
	     (type (unsigned-byte 31) utime-sec stime-sec)
	     ;; (Classic CMU CL had these (MOD 1000000) instead, but
	     ;; at least in Linux 2.2.12, the type doesn't seem to be
	     ;; documented anywhere and the observed behavior is to
	     ;; sometimes return 1000000 exactly.)
	     (type (integer 0 1000000) utime-usec stime-usec))
    (let ((result (+ (the (unsigned-byte 32)
			  (* (the (unsigned-byte 32) (+ utime-sec stime-sec))
			     sb!xc:internal-time-units-per-second))
		     (floor (+ utime-usec
			       stime-usec
			       (floor micro-seconds-per-internal-time-unit 2))
			    micro-seconds-per-internal-time-unit))))
      result)))

;;;; Encode and decode universal times.

;;; Subtract from the returned Internal-Time to get the universal
;;; time. The offset between our time base and the Perq one is 2145
;;; weeks and five days.
(defconstant seconds-in-week (* 60 60 24 7))
(defconstant weeks-offset 2145)
(defconstant seconds-offset 432000)
(defconstant minutes-per-day (* 24 60))
(defconstant quarter-days-per-year (1+ (* 365 4)))
(defconstant quarter-days-per-century 146097)
(defconstant november-17-1858 678882)
(defconstant weekday-november-17-1858 2)
(defconstant unix-to-universal-time 2208988800)

(defun get-universal-time ()
  #!+sb-doc
  "Return a single integer for the current time of
   day in universal time format."
  (multiple-value-bind (res secs) (sb!unix:unix-gettimeofday)
    (declare (ignore res))
    (+ secs unix-to-universal-time)))

(defun get-decoded-time ()
  #!+sb-doc
  "Return nine values specifying the current time as follows:
   second, minute, hour, date, month, year, day of week (0 = Monday), T
   (daylight savings times) or NIL (standard time), and timezone."
  (decode-universal-time (get-universal-time)))

(defun decode-universal-time (universal-time &optional time-zone)
  #!+sb-doc
  "Converts a universal-time to decoded time format returning the following
   nine values: second, minute, hour, date, month, year, day of week (0 =
   Monday), T (daylight savings time) or NIL (standard time), and timezone.
   Completely ignores daylight-savings-time when time-zone is supplied."
  (multiple-value-bind (weeks secs)
      (truncate (+ universal-time seconds-offset)
		seconds-in-week)
    (let* ((weeks (+ weeks weeks-offset))
	   (second NIL)
	   (minute NIL)
	   (hour NIL)
	   (date NIL)
	   (month NIL)
	   (year NIL)
	   (day NIL)
	   (daylight NIL)
	   (timezone (cond
		       ((null time-zone)
			(multiple-value-bind
			      (ignore minwest dst)
			    (sb!unix::get-timezone (- universal-time
						      unix-to-universal-time))
			  (declare (ignore ignore))
			  (declare (fixnum minwest))
			  (setf daylight dst)
			  minwest))
		       (t (* time-zone 60)))))
      (declare (fixnum timezone))
      (multiple-value-bind (t1 seconds) (truncate secs 60)
	(setq second seconds)
	(setq t1 (- t1 timezone))
	(let* ((tday (if (< t1 0)
			 (1- (truncate (1+ t1) minutes-per-day))
			 (truncate t1 minutes-per-day))))
	  (multiple-value-setq (hour minute)
	    (truncate (- t1 (* tday minutes-per-day)) 60))
	  (let* ((t2 (1- (* (+ (* weeks 7) tday november-17-1858) 4)))
		 (tcent (truncate t2 quarter-days-per-century)))
	    (setq t2 (mod t2 quarter-days-per-century))
	    (setq t2 (+ (- t2 (mod t2 4)) 3))
	    (setq year (+ (* tcent 100) (truncate t2 quarter-days-per-year)))
	    (let ((days-since-mar0 (1+ (truncate (mod t2 quarter-days-per-year)
						 4))))
	      (setq day (mod (+ tday weekday-november-17-1858) 7))
	      (let ((t3 (+ (* days-since-mar0 5) 456)))
		(cond ((>= t3 1989)
		       (setq t3 (- t3 1836))
		       (setq year (1+ year))))
		(multiple-value-setq (month t3) (truncate t3 153))
		(setq date (1+ (truncate t3 5))))))))
      (values second minute hour date month year day
	      daylight
	      (if daylight
		  (1+ (/ timezone 60))
		  (/ timezone 60))))))

(defun pick-obvious-year (year)
  (declare (type (mod 100) year))
  (let* ((current-year (nth-value 5 (get-decoded-time)))
	 (guess (+ year (* (truncate (- current-year 50) 100) 100))))
    (declare (type (integer 1900 9999) current-year guess))
    (if (> (- current-year guess) 50)
	(+ guess 100)
	guess)))

(defun leap-years-before (year)
  (let ((years (- year 1901)))
    (+ (- (truncate years 4)
	  (truncate years 100))
       (truncate (+ years 300) 400))))

(defvar *days-before-month*
  #.(let ((reversed-result nil)
	  (sum 0))
      (push nil reversed-result)
      (dolist (days-in-month '(31 28 31 30 31 30 31 31 30 31 30 31))
	(push sum reversed-result)
	(incf sum days-in-month))
      (coerce (nreverse reversed-result) 'simple-vector)))

(defun encode-universal-time (second minute hour date month year
				     &optional time-zone)
  #!+sb-doc
  "The time values specified in decoded format are converted to
   universal time, which is returned."
  (declare (type (mod 60) second)
	   (type (mod 60) minute)
	   (type (mod 24) hour)
	   (type (integer 1 31) date)
	   (type (integer 1 12) month)
	   (type (or (integer 0 99) (integer 1900)) year)
	   (type (or null rational) time-zone))
  (let* ((year (if (< year 100)
		   (pick-obvious-year year)
		   year))
	 (days (+ (1- date)
		  (aref *days-before-month* month)
		  (if (> month 2)
		      (leap-years-before (1+ year))
		      (leap-years-before year))
		  (* (- year 1900) 365)))
	 (hours (+ hour (* days 24))))
    (if time-zone
	(+ second (* (+ minute (* (+ hours time-zone) 60)) 60))
	(let* ((minwest-guess
		(sb!unix::unix-get-minutes-west (- (* hours 60 60)
						  unix-to-universal-time)))
	       (guess (+ minute (* hours 60) minwest-guess))
	       (minwest
		(sb!unix::unix-get-minutes-west (- (* guess 60)
						  unix-to-universal-time))))
	  (+ second (* (+ guess (- minwest minwest-guess)) 60))))))

;;;; TIME

(defmacro time (form)
  #!+sb-doc
  "Execute FORM and print timing information on *TRACE-OUTPUT*."
  `(%time (lambda () ,form)))

;;; Return all the data that we want TIME to report.
(defun time-get-sys-info ()
  (multiple-value-bind (user sys faults) (sb!sys:get-system-info)
    (values user sys faults (get-bytes-consed))))

;;; The guts of the TIME macro. Compute overheads, run the (compiled)
;;; function, report the times.
(defun %time (fun)
  (let (old-run-utime
	new-run-utime
	old-run-stime
	new-run-stime
	old-real-time
	new-real-time
	old-page-faults
	new-page-faults
	real-time-overhead
	run-utime-overhead
	run-stime-overhead
	page-faults-overhead
	old-bytes-consed
	new-bytes-consed
	cons-overhead)
    ;; Calculate the overhead...
    (multiple-value-setq
	(old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (time-get-sys-info))
    ;; Do it a second time to make sure everything is faulted in.
    (multiple-value-setq
	(old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (time-get-sys-info))
    (multiple-value-setq
	(new-run-utime new-run-stime new-page-faults new-bytes-consed)
      (time-get-sys-info))
    (setq run-utime-overhead (- new-run-utime old-run-utime))
    (setq run-stime-overhead (- new-run-stime old-run-stime))
    (setq page-faults-overhead (- new-page-faults old-page-faults))
    (setq old-real-time (get-internal-real-time))
    (setq old-real-time (get-internal-real-time))
    (setq new-real-time (get-internal-real-time))
    (setq real-time-overhead (- new-real-time old-real-time))
    (setq cons-overhead (- new-bytes-consed old-bytes-consed))
    ;; Now get the initial times.
    (multiple-value-setq
	(old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (time-get-sys-info))
    (setq old-real-time (get-internal-real-time))
    (let ((start-gc-run-time *gc-run-time*))
    (multiple-value-prog1
	;; Execute the form and return its values.
	(funcall fun)
      (multiple-value-setq
	  (new-run-utime new-run-stime new-page-faults new-bytes-consed)
	(time-get-sys-info))
      (setq new-real-time (- (get-internal-real-time) real-time-overhead))
      (let ((gc-run-time (max (- *gc-run-time* start-gc-run-time) 0)))
	(format *trace-output*
		"~&Evaluation took:~%  ~
		 ~S second~:P of real time~%  ~
		 ~S second~:P of user run time~%  ~
		 ~S second~:P of system run time~%  ~
~@[                 [Run times include ~S second~:P GC run time.]~%  ~]~
		 ~S page fault~:P and~%  ~
		 ~S bytes consed.~%"
		(max (/ (- new-real-time old-real-time)
			(float sb!xc:internal-time-units-per-second))
		     0.0)
		(max (/ (- new-run-utime old-run-utime) 1000000.0) 0.0)
		(max (/ (- new-run-stime old-run-stime) 1000000.0) 0.0)
		(unless (zerop gc-run-time)
		  (/ (float gc-run-time)
		     (float sb!xc:internal-time-units-per-second)))
		(max (- new-page-faults old-page-faults) 0)
		(max (- new-bytes-consed old-bytes-consed) 0)))))))
