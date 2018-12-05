;;;; time printing routines built upon the Common Lisp FORMAT function

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(defconstant +abbrev-weekday-table+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defconstant +long-weekday-table+
  #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defconstant +abbrev-month-table+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defconstant +long-month-table+
  #("January" "February" "March" "April" "May" "June" "July" "August"
   "September" "October" "November" "December"))

;;; The timezone table is incomplete but workable.
(defconstant +timezone-table+
  #("GMT" "" "" "" "" "EST" "CST" "MST" "PST"))

(defconstant +daylight-table+
  #(nil nil nil nil nil "EDT" "CDT" "MDT" "PDT"))

;;; VALID-DESTINATION-P ensures the destination stream is okay for the
;;; FORMAT function.
(defun valid-destination-p (destination)
  (or (not destination)
      (eq destination t)
      (streamp destination)
      (and (stringp destination)
           (array-has-fill-pointer-p destination))))

;;; CMU CL made the default style :SHORT here. I've changed that to :LONG, on
;;; the theory that since the 8/7/1999 style is hard to decode unambiguously,
;;; you should have to ask for it explicitly. (I prefer YYYYMMDD myself, since
;;; it sorts properly.:-) -- WHN 19990831
;;;
;;; FIXME: On the CMU CL mailing list 30 Jan 2000, Pierre Mai suggested
;;;   OTOH it probably wouldn't be a major problem to change compile-file to
;;;   use for example :long, so that the output would be Month DD, YYYY, or
;;;   even better to extend format-universal-time with a flag to output ISO
;;;   8601 formats (like e.g. :iso-8601 and :iso-8601-short) and migrate
;;;   slowly towards ISO dates in the user code...
;;; The :ISO-8601 and :ISO-8601-SHORT options sound sensible to me. Maybe
;;; someone will do them for CMU CL and we can steal them for SBCL.
(defun format-universal-time (destination universal-time
                                          &key
                                          (timezone nil)
                                          (style :long)
                                          (date-first t)
                                          (print-seconds t)
                                          (print-meridian t)
                                          (print-timezone t)
                                          (print-weekday t))
  "Format-Universal-Time formats a string containing the time and date
   given by universal-time in a common manner. The destination is any
   destination which can be accepted by the Format function. The
   timezone keyword is an integer specifying hours west of Greenwich.
   The style keyword can be :SHORT (numeric date), :LONG (months and
   weekdays expressed as words), :ABBREVIATED (like :LONG but words are
   abbreviated), or :GOVERNMENT (of the form \"XX Month XXXX XX:XX:XX\")
   The &KEY argument :DATE-FIRST, if NIL, will print the time first instead
   of the date (the default). The PRINT- keywords, if NIL, inhibit
   the printing of the obvious part of the time/date."
  (unless (valid-destination-p destination)
    (error "~A: Not a valid format destination." destination))
  (unless (integerp universal-time)
    (error "~A: Universal-Time should be an integer." universal-time))
  (when timezone
    (unless (and (rationalp timezone) (<= -24 timezone 24))
      (error "~A: Timezone should be a rational between -24 and 24." timezone))
    (unless (zerop (rem timezone 1/3600))
      (error "~A: Timezone is not a second (1/3600) multiple." timezone)))

  (multiple-value-bind (secs mins hours day month year dow dst tz)
      (if timezone
          (decode-universal-time universal-time timezone)
          (decode-universal-time universal-time))
    (declare (fixnum secs mins hours day month year dow))
    (let ((time-string "~2,'0D:~2,'0D")
          (date-string
           (case style
             (:short "~D/~D/~D")                ;;  MM/DD/Y
             ((:abbreviated :long) "~A ~D, ~D") ;;  Month DD, Y
             (:government "~2,'0D ~:@(~A~) ~D") ;;  DD MON Y
             (t
              (error "~A: Unrecognized :style keyword value." style))))
          (time-args
           (list mins (max (mod hours 12) (1+ (mod (1- hours) 12)))))
          (date-args (case style
                       (:short
                        (list month day year))
                       (:abbreviated
                        (list (svref +abbrev-month-table+ (1- month)) day year))
                       (:long
                        (list (svref +long-month-table+ (1- month)) day year))
                       (:government
                        (list day (svref +abbrev-month-table+ (1- month))
                              year)))))
      (declare (simple-string time-string date-string))
      (when print-weekday
        (push (case style
                ((:short :long) (svref +long-weekday-table+ dow))
                (:abbreviated (svref +abbrev-weekday-table+ dow))
                (:government (svref +abbrev-weekday-table+ dow)))
              date-args)
        (setq date-string
              (concatenate 'simple-string "~A, " date-string)))
      (when (or print-seconds (eq style :government))
        (push secs time-args)
        (setq time-string
              (concatenate 'simple-string time-string ":~2,'0D")))
      (when print-meridian
        (push (signum (floor hours 12)) time-args)
        (setq time-string
              (concatenate 'simple-string time-string " ~[AM~;PM~]")))
      (apply #'format destination
             (if date-first
                 (concatenate 'simple-string date-string " " time-string
                              (if print-timezone " ~A"))
                 (concatenate 'simple-string time-string " " date-string
                              (if print-timezone " ~A")))
             (if date-first
                 (nconc date-args (nreverse time-args)
                        (if print-timezone
                            (list (timezone-name dst tz))))
                 (nconc (nreverse time-args) date-args
                        (if print-timezone
                            (list (timezone-name dst tz)))))))))

(defun timezone-name (dst tz)
  (if (and (integerp tz)
           (or (and dst (= tz 0))
               (<= 5 tz 8)))
      (svref (if dst +daylight-table+ +timezone-table+) tz)
      (multiple-value-bind (rest seconds) (truncate (* tz 60 60) 60)
        (multiple-value-bind (hours minutes) (truncate rest 60)
          (format nil "[~C~D~@[~*:~2,'0D~@[~*:~2,'0D~]~]]"
                  (if (minusp tz) #\- #\+)
                  (abs hours)
                  (not (and (zerop minutes) (zerop seconds)))
                  (abs minutes)
                  (not (zerop seconds))
                  (abs seconds))))))

(defun format-decoded-time (destination seconds minutes hours
                                          day month year
                                          &key (timezone nil)
                                          (style :short)
                                          (date-first t)
                                          (print-seconds t)
                                          (print-meridian t)
                                          (print-timezone t)
                                          (print-weekday t))
  "FORMAT-DECODED-TIME formats a string containing decoded time
   expressed in a humanly-readable manner. The destination is any
   destination which can be accepted by the FORMAT function. The
   timezone keyword is an integer specifying hours west of Greenwich.
   The style keyword can be :SHORT (numeric date), :LONG (months and
   weekdays expressed as words), or :ABBREVIATED (like :LONG but words are
   abbreviated). The keyword DATE-FIRST, if NIL, will cause the time
   to be printed first instead of the date (the default). The PRINT-
   keywords, if nil, inhibit the printing of certain semi-obvious
   parts of the string."
  (unless (valid-destination-p destination)
    (error "~A: Not a valid format destination." destination))
  (unless (and (integerp seconds) (<= 0 seconds 59))
    (error "~A: Seconds should be an integer between 0 and 59." seconds))
  (unless (and (integerp minutes) (<= 0 minutes 59))
    (error "~A: Minutes should be an integer between 0 and 59." minutes))
  (unless (and (integerp hours) (<= 0 hours 23))
    (error "~A: Hours should be an integer between 0 and 23." hours))
  (unless (and (integerp day) (<= 1 day 31))
    (error "~A: Day should be an integer between 1 and 31." day))
  (unless (and (integerp month) (<= 1 month 12))
    (error "~A: Month should be an integer between 1 and 12." month))
  (unless (and (integerp year) (plusp year))
    (error "~A: Hours should be an non-negative integer." year))
  (when timezone
    (unless (and (integerp timezone) (<= 0 timezone 32))
      (error "~A: Timezone should be an integer between 0 and 32."
             timezone)))
  (format-universal-time destination
   (encode-universal-time seconds minutes hours day month year)
   :timezone timezone :style style :date-first date-first
   :print-seconds print-seconds :print-meridian print-meridian
   :print-timezone print-timezone :print-weekday print-weekday))
