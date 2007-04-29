;;;; global deadlines for blocking functions: a threadsafe alternative
;;;; to asynch timeouts

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; Current deadline as internal time units or NIL.
(defvar *deadline* nil)
(declaim (type (or unsigned-byte null) *deadline*))

;;; The relative number of seconds the current deadline corresponds
;;; to. Used for continuing from TIMEOUT conditions.
(defvar *deadline-seconds* nil)

(declaim (inline seconds-to-internal-time))
(defun seconds-to-internal-time (seconds)
  (truncate (* seconds sb!xc:internal-time-units-per-second)))

(defmacro with-deadline ((&key seconds override)
                         &body body)
  "Arranges for a TIMEOUT condition to be signalled if an operation respecting
deadlines occurs either after the deadline has passed, or would take longer
than the time left to complete.

Currently only blocking IO operations, GET-MUTEX, and CONDITION-WAIT respect
deadlines, but this includes their implicit uses inside SBCL itself.

Experimental."
  (with-unique-names (deadline-seconds deadline)
    ;; We're operating on a millisecond precision, so a single-float
    ;; is enough, and is an immediate on 64bit platforms.
    `(let* ((,deadline-seconds (coerce ,seconds 'single-float))
            (,deadline
             (+ (seconds-to-internal-time ,deadline-seconds)
                (get-internal-real-time))))
       (multiple-value-bind (*deadline* *deadline-seconds*)
           (if ,override
               (values ,deadline ,deadline-seconds)
               (let ((old *deadline*))
                 (if (and old (< old ,deadline))
                     (values old *deadline-seconds*)
                     (values ,deadline ,deadline-seconds))))
         ,@body))))

(declaim (inline decode-internal-time))
(defun decode-internal-time (time)
  #!+sb-doc
  "Returns internal time value TIME decoded into seconds and microseconds."
  (multiple-value-bind (sec frac)
      (truncate time sb!xc:internal-time-units-per-second)
    (values sec (* frac sb!unix::micro-seconds-per-internal-time-unit))))

(defun signal-timeout (datum &rest arguments)
  #!+sb-doc
  "Signals a timeout condition while inhibiting further timeouts due to
deadlines while the condition is being handled."
  (let ((*deadline* nil))
    (apply #'error datum arguments)))

(defun signal-deadline ()
  #!+sb-doc
  "Signal a DEADLINE-TIMEOUT condition. Implementors of blocking functions
are responsible for calling this when a deadline is reached."
  (signal-timeout 'deadline-timeout :seconds *deadline-seconds*))

;;; Returns TIMEOUT-SEC, TIMEOUT-USEC, DEADLINE-SEC, DEADLINE-USEC, SIGNALP
;;;
;;; Takes *DEADLINE* into account: if it occurs before given SECONDS,
;;; the values are based on it, and DEADLINEP is true -- and the
;;; receipent of the values should call SIGNAL-TIMEOUT if the decoded
;;; timeout is reached.
;;;
;;; If SECONDS is NIL and there is no *DEADLINE* all returned values
;;; are NIL.
(defun decode-timeout (seconds)
  #!+sb-doc
  "Decodes a relative timeout in SECONDS into five values, taking any
global deadlines into account: TO-SEC, TO-USEC, STOP-SEC, STOP-USEC,
DEADLINEP.

TO-SEC and TO-USEC indicate the relative timeout in seconds and microsconds.
STOP-SEC and STOP-USEC indicate the absolute timeout in seconds and
microseconds. DEADLINEP is true if the returned values reflect a global
deadline instead of the local timeout indicated by SECONDS.

If SECONDS is null and there is no global timeout all returned values will be
null. If a global deadline has already passed when DECODE-TIMEOUT is called,
it will signal a timeout condition."
  (let* ((timeout (when seconds (seconds-to-internal-time seconds)))
         (now (get-internal-real-time))
         (deadline *deadline*)
         (deadline-timeout
          (when deadline
            (let ((time-left (- deadline now)))
              (if (plusp time-left)
                  time-left
                  (signal-deadline))))))
    (multiple-value-bind (final-timeout final-deadline signalp)
        ;; Use either *DEADLINE* or TIMEOUT to produce both a timeout
        ;; and deadline in internal-time units
        (cond ((and deadline timeout)
               (if (< timeout deadline-timeout)
                   (values timeout (+ timeout now) nil)
                   (values deadline-timeout deadline t)))
              (deadline
               (values deadline-timeout deadline t))
              (timeout
               (values timeout (+ timeout now) nil))
              (t
               (values nil nil nil)))
      (if final-timeout
          (multiple-value-bind (to-sec to-usec)
              (decode-internal-time final-timeout)
            (multiple-value-bind (stop-sec stop-usec)
                (decode-internal-time final-deadline)
              (values to-sec to-usec stop-sec stop-usec signalp)))
          (values nil nil nil nil nil)))))
