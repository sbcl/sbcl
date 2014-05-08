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

(defun time-reinit ()
  (reinit-internal-real-time))

;;; Implemented in unix.lisp and win32.lisp.
#!+sb-doc
(setf (fdocumentation 'get-internal-real-time 'function)
      "Return the real time (\"wallclock time\") since startup in the internal
time format. (See INTERNAL-TIME-UNITS-PER-SECOND.)")

(defun get-internal-run-time ()
  #!+sb-doc
  "Return the run time used by the process in the internal time format. (See
INTERNAL-TIME-UNITS-PER-SECOND.) This is useful for finding CPU usage.
Includes both \"system\" and \"user\" time."
  (system-internal-run-time))

;;;; Encode and decode universal times.

;;; In August 2003, work was done in this file for more plausible
;;; timezone handling after the unix timezone database runs out in
;;; 2038.  We assume that timezone rules are trending sane rather than
;;; insane, so for all years after the end of time_t we apply the
;;; rules for 2035/2036 instead of the actual date asked for.  Making
;;; the same assumption about the early 1900s would be less
;;; reasonable, however, so please note that we're still broken for
;;; local time between 1900-1-1 and 1901-12-13

;;; It should be noted that 64 bit machines don't actually fix this
;;; problem, at least as of 2003, because the Unix zonefiles are
;;; specified in terms of 32 bit fields even on, say, the Alpha.  So,
;;; references to the range of time_t elsewhere in this file should
;;; rightly be read as shorthand for the range of an signed 32 bit
;;; number of seconds since 1970-01-01

;;; I'm obliged to Erik Naggum's "Long, Painful History of Time" paper
;;; <http://naggum.no/lugm-time.html> for the choice of epoch here.
;;; By starting the year in March, we avoid having to test the month
;;; whenever deciding whether to account for a leap day.  2000 is
;;; especially special, because it's divisible by 400, hence the start
;;; of a 400 year leap year cycle

;;; If a universal-time is after time_t runs out, we find its offset
;;; from 1st March of whichever year it falls in, then add that to
;;; 2035-3-1.  This date has two relevant properties: (1) somewhere
;;; near the end of time_t, and (2) preceding a leap year.  Thus a
;;; date which is e.g. 365.5 days from March 1st in its year will be
;;; treated for timezone lookup as if it were Feb 29th 2036

;;; This epoch is used only for fixing the timezones-outside-time_t
;;; problem.  Someday it would be nice to come back to this code and
;;; see if the rest of the file and its references to Spice Lisp
;;; history (Perq time base?) could be cleaned up any on this basis.
;;; -- dan, 2003-08-08

;;; In order to accomodate universal times between January 1st 1900
;;; and sometime on December 13th 1901, I'm doing the same calculation
;;; as described above in order to handle dates in that interval, by
;;; normalizing them to March 1st 1903, which shares the same special
;;; properties described above (except for the 400-year property, but
;;; this isn't an issue for the limited range we need to handle).

;;; One open issue is whether to pass UNIX a 64-bit time_t value on
;;; 64-bit platforms. I don't know if time_t is always 64-bit on those
;;; platforms, and looking at this file reveals a scary amount of
;;; literal 31 and 32s.
;;; -- bem, 2005-08-09

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
  "Return a single integer for the current time of day in universal time
format."
  (+ (get-time-of-day) unix-to-universal-time))

(defun get-decoded-time ()
  #!+sb-doc
  "Return nine values specifying the current time as follows:
   second, minute, hour, date, month, year, day of week (0 = Monday), T
   (daylight savings times) or NIL (standard time), and timezone."
  (decode-universal-time (get-universal-time)))

(defconstant +mar-1-2000+ #.(encode-universal-time 0 0 0 1 3 2000 0))
(defconstant +mar-1-2035+ #.(encode-universal-time 0 0 0 1 3 2035 0))

(defconstant +mar-1-1903+ #.(encode-universal-time 0 0 0 1 3 1903 0))

(defun years-since-mar-2000 (utime)
  #!+sb-doc
  "Returns number of complete years since March 1st 2000, and remainder in seconds"
  (let* ((days-in-year (* 86400 365))
         (days-in-4year (+ (* 4 days-in-year) 86400))
         (days-in-100year (- (* 25 days-in-4year) 86400))
         (days-in-400year (+ (* 4 days-in-100year) 86400))
         (offset (- utime +mar-1-2000+))
         (year 0))
    (labels ((whole-num (x y inc max)
               (let ((w (truncate x y)))
                 (when (and max (> w max)) (setf w max))
                 (incf year (* w inc))
                 (* w y))))
      (decf offset (whole-num offset days-in-400year 400 nil))
      (decf offset (whole-num offset days-in-100year 100 3))
      (decf offset (whole-num offset days-in-4year 4 25))
      (decf offset (whole-num offset days-in-year 1 3))
      (values year offset))))

(defun truncate-to-unix-range (utime)
  (let ((unix-time (- utime unix-to-universal-time)))
    (cond
      ((< unix-time (- (ash 1 31)))
       (multiple-value-bind (year offset) (years-since-mar-2000 utime)
         (declare (ignore year))
         (+  +mar-1-1903+  (- unix-to-universal-time)  offset)))
      ((>= unix-time (ash 1 31))
       (multiple-value-bind (year offset) (years-since-mar-2000 utime)
         (declare (ignore year))
         (+  +mar-1-2035+  (- unix-to-universal-time)  offset)))
      (t unix-time))))

(defun decode-universal-time (universal-time &optional time-zone)
  #!+sb-doc
  "Converts a universal-time to decoded time format returning the following
   nine values: second, minute, hour, date, month, year, day of week (0 =
   Monday), T (daylight savings time) or NIL (standard time), and timezone.
   Completely ignores daylight-savings-time when time-zone is supplied."
  (multiple-value-bind (daylight seconds-west)
      (if time-zone
          (values nil (* time-zone 60 60))
          (multiple-value-bind (ignore seconds-west daylight)
              (sb!unix::get-timezone (truncate-to-unix-range universal-time))
            (declare (ignore ignore))
            (declare (fixnum seconds-west))
            (values daylight seconds-west)))
    (declare (fixnum seconds-west))
    (multiple-value-bind (weeks secs)
        (truncate (+ (- universal-time seconds-west) seconds-offset)
                  seconds-in-week)
      (let ((weeks (+ weeks weeks-offset)))
        (multiple-value-bind (t1 second)
            (truncate secs 60)
          (let ((tday (truncate t1 minutes-per-day)))
            (multiple-value-bind (hour minute)
                (truncate (- t1 (* tday minutes-per-day)) 60)
              (let* ((t2 (1- (* (+ (* weeks 7) tday november-17-1858) 4)))
                     (tcent (truncate t2 quarter-days-per-century)))
                (setq t2 (mod t2 quarter-days-per-century))
                (setq t2 (+ (- t2 (mod t2 4)) 3))
                (let* ((year (+ (* tcent 100)
                                (truncate t2 quarter-days-per-year)))
                       (days-since-mar0
                        (1+ (truncate (mod t2 quarter-days-per-year) 4)))
                       (day (mod (+ tday weekday-november-17-1858) 7))
                       (t3 (+ (* days-since-mar0 5) 456)))
                  (cond ((>= t3 1989)
                         (setq t3 (- t3 1836))
                         (setq year (1+ year))))
                  (multiple-value-bind (month t3)
                      (truncate t3 153)
                    (let ((date (1+ (truncate t3 5))))
                      (values second minute hour date month year day
                              daylight
                              (if daylight
                                  (1+ (/ seconds-west 60 60))
                                  (/ seconds-west 60 60))))))))))))))

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
           (type (or (integer 0 99) (integer 1899)) year)
           ;; that type used to say (integer 1900), but that's
           ;; incorrect when a time-zone is specified: we should be
           ;; able to encode to produce 0 when a non-zero timezone is
           ;; specified - bem, 2005-08-09
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
         (hours (+ hour (* days 24)))
         (encoded-time 0))
    (if time-zone
        (setf encoded-time (+ second (* (+ minute (* (+ hours time-zone) 60)) 60)))
        (let* ((secwest-guess
                (sb!unix::unix-get-seconds-west
                 (truncate-to-unix-range (* hours 60 60))))
               (guess (+ second (* 60 (+ minute (* hours 60)))
                         secwest-guess))
               (secwest
                (sb!unix::unix-get-seconds-west
                 (truncate-to-unix-range guess))))
          (setf encoded-time (+ guess (- secwest secwest-guess)))))
    (assert (typep encoded-time '(integer 0)))
    encoded-time))

;;;; TIME

(defvar *gc-run-time* 0
  #!+sb-doc
  "Total CPU time spent doing garbage collection (as reported by
GET-INTERNAL-RUN-TIME.) Initialized to zero on startup. It is safe to bind
this to zero in order to measure GC time inside a certain section of code, but
doing so may interfere with results reported by eg. TIME.")
(declaim (type index *gc-run-time*))

(defun print-time (&key real-time-ms user-run-time-us system-run-time-us
                   gc-run-time-ms processor-cycles eval-calls
                   lambdas-converted page-faults bytes-consed
                   aborted)
  (let ((total-run-time-us (+ user-run-time-us system-run-time-us)))
    (format *trace-output*
            "~&Evaluation took:~%~
                         ~@<  ~@;~/sb-impl::format-milliseconds/ of real time~%~
                                 ~/sb-impl::format-microseconds/ of total run time ~
                                  (~@/sb-impl::format-microseconds/ user, ~@/sb-impl::format-microseconds/ system)~%~
                                 ~[[ Run times consist of ~/sb-impl::format-milliseconds/ GC time, ~
                                                      and ~/sb-impl::format-milliseconds/ non-GC time. ]~%~;~2*~]~
                                 ~,2F% CPU~%~
                                 ~@[~:D form~:P interpreted~%~]~
                                 ~@[~:D lambda~:P converted~%~]~
                                 ~@[~:D processor cycles~%~]~
                                 ~@[~:D page fault~:P~%~]~
                                 ~:D bytes consed~%~
                                 ~@[~%before it was aborted by a non-local transfer of control.~%~]~:>~%"
            real-time-ms
            total-run-time-us
            user-run-time-us
            system-run-time-us
            (if (zerop gc-run-time-ms) 1 0)
            gc-run-time-ms
            ;; Round up so we don't mislead by saying 0.0 seconds of non-GC time...
            (- (ceiling total-run-time-us 1000) gc-run-time-ms)
            (if (zerop real-time-ms)
                100.0
                (float (* 100 (/ (round total-run-time-us 1000) real-time-ms))))
            eval-calls
            lambdas-converted
            processor-cycles
            page-faults
            bytes-consed
            aborted)))

(defmacro time (form)
  #!+sb-doc
  "Execute FORM and print timing information on *TRACE-OUTPUT*.

On some hardware platforms estimated processor cycle counts are
included in this output; this number is slightly inflated, since it
includes the pipeline involved in reading the cycle counter --
executing \(TIME NIL) a few times will give you an idea of the
overhead, and its variance. The cycle counters are also per processor,
not per thread: if multiple threads are running on the same processor,
the reported counts will include cycles taken up by all threads
running on the processor where TIME was executed. Furthermore, if the
operating system migrates the thread to another processor between
reads of the cycle counter, the results will be completely bogus.
Finally, the counter is cycle counter, incremented by the hardware
even when the process is halted -- which is to say that cycles pass
normally during operations like SLEEP."
  `(call-with-timing #'print-time (lambda () ,form)))

;;; Return all the data that we want TIME to report.
(defun time-get-sys-info ()
  (multiple-value-bind (user sys faults) (sb!sys:get-system-info)
    (values user sys faults (get-bytes-consed))))

(defun elapsed-cycles (h0 l0 h1 l1)
  (declare (ignorable h0 l0 h1 l1))
  #!+cycle-counter
  (+ (ash (- h1 h0) 32)
     (- l1 l0))
  #!-cycle-counter
  nil)
(declaim (inline read-cycle-counter))
(defun read-cycle-counter ()
  #!+cycle-counter
  (sb!vm::%read-cycle-counter)
  #!-cycle-counter
  (values 0 0))

;;; This is so that we don't have to worry about the vagaries of
;;; floating point printing, or about conversions to floats dropping
;;; or introducing decimals, which are liable to imply wrong precision.
(defun format-microseconds (stream usec &optional colonp atp)
  (declare (ignore colonp))
  (%format-decimal stream usec 6)
  (unless atp
    (write-string " seconds" stream)))

(defun format-milliseconds (stream usec &optional colonp atp)
  (declare (ignore colonp))
  (%format-decimal stream usec 3)
  (unless atp
    (write-string " seconds" stream)))

(defun %format-decimal (stream number power)
  (declare (stream stream)
           (integer number power))
  (when (minusp number)
    (write-char #\- stream)
    (setf number (- number)))
  (let ((scale (expt 10 power)))
    (labels ((%fraction (fraction)
               (if (zerop fraction)
                   (%zeroes)
                   (let ((scaled (* 10 fraction)))
                     (loop while (< scaled scale)
                           do (write-char #\0 stream)
                              (setf scaled (* scaled 10)))))
               (format stream "~D" fraction))
             (%zeroes ()
               (let ((scaled (/ scale 10)))
                 (write-char #\0 stream)
                 (loop while (> scaled 1)
                       do (write-char #\0 stream)
                          (setf scaled (/ scaled 10))))))
      (cond ((zerop number)
             (write-string "0." stream)
             (%zeroes))
            ((< number scale)
             (write-string "0." stream)
             (%fraction number))
            ((= number scale)
             (write-string "1." stream)
             (%zeroes))
            ((> number scale)
             (multiple-value-bind (whole fraction) (floor number scale)
               (format stream "~D." whole)
               (%fraction fraction))))))
  nil)

;;; The guts of the TIME macro. Compute overheads, run the (compiled)
;;; function, report the times.
(defun call-with-timing (timer function &rest arguments)
  #!+sb-doc
  "Calls FUNCTION with ARGUMENTS, and gathers timing information about it.
Then calls TIMER with keyword arguments describing the information collected.
Calls TIMER even if FUNCTION performs a non-local transfer of control. Finally
returns values returned by FUNCTION.

  :USER-RUN-TIME-US
      User run time in microseconds.

  :SYSTEM-RUN-TIME-US
      System run time in microseconds.

  :REAL-TIME-MS
      Real time in milliseconds.

  :GC-RUN-TIME-MS
      GC run time in milliseconds (included in user and system run time.)

  :PROCESSOR-CYCLES
      Approximate number of processor cycles used. (Omitted  if not supported on
      the platform -- currently available on x86 and x86-64 only.)

  :EVAL-CALLS
      Number of calls to EVAL. (Omitted if zero.)

  :LAMBDAS-CONVERTED
      Number of lambdas converted. (Omitted if zero.)

  :PAGE-FAULTS
      Number of page faults. (Omitted if zero.)

  :BYTES-CONSED
      Approximate number of bytes consed.

  :ABORTED
      True if FUNCTION caused a non-local transfer of control. (Omitted if
      NIL.)

EXPERIMENTAL: Interface subject to change."
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
        cons-overhead
        (fun (if (functionp function) function (fdefinition function))))
    (declare (function fun))
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
    (let ((start-gc-internal-run-time *gc-run-time*)
          (*eval-calls* 0)
          (sb!c::*lambda-conversions* 0)
          (aborted t))
      (declare (special *eval-calls* sb!c::*lambda-conversions*))
      (multiple-value-bind (h0 l0) (read-cycle-counter)
        (unwind-protect
             (multiple-value-prog1 (apply fun arguments)
               (setf aborted nil))
          (multiple-value-bind (h1 l1) (read-cycle-counter)
            (let ((stop-gc-internal-run-time *gc-run-time*))
              (multiple-value-setq
                  (new-run-utime new-run-stime new-page-faults new-bytes-consed)
                (time-get-sys-info))
              (setq new-real-time (- (get-internal-real-time) real-time-overhead))
              (let* ((gc-internal-run-time (max (- stop-gc-internal-run-time start-gc-internal-run-time) 0))
                     (real-time (max (- new-real-time old-real-time) 0))
                     (user-run-time (max (- new-run-utime old-run-utime) 0))
                     (system-run-time (max (- new-run-stime old-run-stime) 0))
                     (cycles (elapsed-cycles h0 l0 h1 l1))
                     (page-faults (max (- new-page-faults old-page-faults) 0)))
                (let (plist)
                  (flet ((note (name value &optional test)
                           (unless (and test (funcall test value))
                             (setf plist (list* name value plist)))))
                    (note :aborted aborted #'not)
                    (note :bytes-consed (max (- new-bytes-consed old-bytes-consed) 0))
                    (note :page-faults page-faults #'zerop)
                    ;; cycle counting isn't supported everywhere.
                    (when cycles
                      (note :processor-cycles cycles #'zerop)
                    (note :lambdas-converted sb!c::*lambda-conversions* #'zerop))
                    (note :eval-calls *eval-calls* #'zerop)
                    (note :gc-run-time-ms gc-internal-run-time)
                    (note :system-run-time-us system-run-time)
                    (note :user-run-time-us user-run-time)
                    (note :real-time-ms real-time))
                  (apply timer plist))))))))))
