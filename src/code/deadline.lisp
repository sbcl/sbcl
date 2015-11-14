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
(declaim (type (or unsigned-byte null) *deadline*))
(!defvar *deadline* nil)

;;; The relative number of seconds the current deadline corresponds
;;; to. Used for continuing from TIMEOUT conditions.
(!defvar *deadline-seconds* nil)

(declaim (inline seconds-to-internal-time))
(defun seconds-to-internal-time (seconds)
  (truncate (* seconds sb!xc:internal-time-units-per-second)))

(defmacro with-deadline ((&key seconds override)
                         &body body)
  #!+sb-doc
  "Arranges for a TIMEOUT condition to be signalled if an operation
respecting deadlines occurs either after the deadline has passed, or
would take longer than the time left to complete.

Currently only blocking IO operations, GET-MUTEX, and CONDITION-WAIT
respect deadlines, but this includes their implicit uses inside SBCL
itself.

Unless OVERRIDE is true, existing deadlines can only be restricted,
not extended. Deadlines are per thread: children are unaffected by
their parent's deadlines.

Experimental."
  (with-unique-names (tmp deadline-seconds deadline)
    ;; We're operating on a millisecond precision, so a single-float
    ;; is enough, and is an immediate on 64bit platforms.
    `(let* ((,tmp ,seconds)
            (,deadline-seconds
              (when ,tmp
                (coerce ,tmp 'single-float)))
            (,deadline
              (when ,deadline-seconds
                (+ (seconds-to-internal-time ,deadline-seconds)
                   (get-internal-real-time)))))
       (multiple-value-bind (*deadline* *deadline-seconds*)
           (if ,override
               (values ,deadline ,deadline-seconds)
               (let ((old *deadline*))
                 (if (and old (or (not ,deadline) (< old ,deadline)))
                     (values old *deadline-seconds*)
                     (values ,deadline ,deadline-seconds))))
         ,@body))))

(declaim (inline decode-internal-time))
(defun decode-internal-time (time)
  #!+sb-doc
  "Returns internal time value TIME decoded into seconds and microseconds."
  (declare (type sb!kernel:internal-time time))
  (multiple-value-bind (sec frac)
      (truncate time sb!xc:internal-time-units-per-second)
    (values sec (* frac sb!unix::micro-seconds-per-internal-time-unit))))

(defun signal-timeout (datum &rest arguments)
  #!+sb-doc
  "Signals a timeout condition while inhibiting further timeouts due to
deadlines while the condition is being handled."
  ;; FIXME: Maybe we should make ERROR do WITH-INTERRUPTS instead of
  ;; putting it all over the place (now that we have ALLOW-WITH-INTERRUPTS.)
  (with-interrupts
    ;; Don't signal a deadline while handling a non-deadline timeout.
    (let ((*deadline* nil))
      (apply #'error datum arguments))))

(defun signal-deadline ()
  #!+sb-doc
  "Signal a DEADLINE-TIMEOUT condition, and associate a DEFER-DEADLINE
restart with it. Implementors of blocking functions are responsible
for calling this when a deadline is reached."
  ;; Make sure we don't signal the same deadline twice. LET is not good
  ;; enough: we might catch the same deadline again while unwinding.
  (when *deadline*
    (setf *deadline* nil))
  (with-interrupts
    (restart-case
        (error 'deadline-timeout :seconds *deadline-seconds*)
      (defer-deadline (&optional (seconds *deadline-seconds*))
        :report "Defer the deadline for SECONDS more."
        :interactive (lambda ()
                       (sb!int:read-evaluated-form
                        "By how many seconds shall the deadline ~
                         be deferred?: "))
        (let* ((new-deadline-seconds (coerce seconds 'single-float))
               (new-deadline (+ (seconds-to-internal-time new-deadline-seconds)
                                (get-internal-real-time))))
          (setf *deadline* new-deadline
                *deadline-seconds* new-deadline-seconds)))
      (cancel-deadline ()
        :report "Cancel the deadline and continue."
        (setf *deadline* nil *deadline-seconds* nil))))
  nil)

(defun defer-deadline (seconds &optional condition)
  #!+sb-doc
  "Find the DEFER-DEADLINE restart associated with CONDITION, and
invoke it with SECONDS as argument (deferring the deadline by that many
seconds.) Otherwise return NIL if the restart is not found."
  (try-restart 'defer-deadline condition seconds))

(defun cancel-deadline (&optional condition)
  #!+sb-doc
  "Find and invoke the CANCEL-DEADLINE restart associated with
CONDITION, or return NIL if the restart is not found."
  (try-restart 'cancel-deadline condition))

(declaim (inline relative-decoded-times))
(defun relative-decoded-times (abs-sec abs-usec)
  #!+sb-doc
"Returns relative decoded time as two values: difference between
ABS-SEC and ABS-USEC and current real time.

If ABS-SEC and ABS-USEC are in the past, 0 0 is returned."
  (declare (type sb!kernel:internal-seconds abs-sec)
           (type (mod 1000000) abs-usec))
  (binding* (((now-sec now-usec)
              (decode-internal-time (get-internal-real-time)))
             (rel-sec (- abs-sec now-sec))
             (rel-usec (- abs-usec now-usec)))
    (when (minusp rel-usec)
      (decf rel-sec)
      (incf rel-usec 1000000))
    (if (minusp rel-sec)
        (values 0 0)
        (values rel-sec rel-usec))))

;;; Returns TIMEOUT-SEC, TIMEOUT-USEC, DEADLINE-SEC, DEADLINE-USEC, SIGNALP
;;;
;;; Takes *DEADLINE* into account: if it occurs before given SECONDS,
;;; the values are based on it, and DEADLINEP is true -- and the
;;; receipent of the values should call SIGNAL-TIMEOUT if the decoded
;;; timeout is reached.
;;;
;;; If SECONDS is NIL and there is no *DEADLINE* all returned values
;;; are NIL.
(declaim (ftype (function ((or null (real 0)))
                          (values (or null sb!kernel:internal-seconds)
                                  (or null (mod 1000000))
                                  (or null sb!kernel:internal-seconds)
                                  (or null (mod 1000000))
                                  t))
                decode-timeout))
(defun decode-timeout (seconds)
  #!+sb-doc
  "Decodes a relative timeout in SECONDS into five values, taking any
global deadlines into account: TO-SEC, TO-USEC, STOP-SEC, STOP-USEC,
DEADLINEP.

TO-SEC and TO-USEC indicate the relative timeout in seconds and microseconds.
STOP-SEC and STOP-USEC indicate the absolute timeout in seconds and
microseconds. DEADLINEP is true if the returned values reflect a global
deadline instead of the local timeout indicated by SECONDS.

If SECONDS is null and there is no global timeout all returned values will be
null. If a global deadline has already passed when DECODE-TIMEOUT is called,
it will signal a timeout condition."
  (tagbody
   :restart
     (let* ((timeout (when seconds (seconds-to-internal-time seconds)))
            (now (get-internal-real-time))
            (deadline *deadline*)
            (deadline-timeout
             (when deadline
               (let ((time-left (- deadline now)))
                 (if (plusp time-left)
                     time-left
                     (progn
                       (signal-deadline)
                       (go :restart)))))))
       (return-from decode-timeout
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
               (binding* (((to-sec to-usec)
                           (decode-internal-time final-timeout))
                          ((stop-sec stop-usec)
                           (decode-internal-time final-deadline)))
                 (values to-sec to-usec stop-sec stop-usec signalp))
               (values nil nil nil nil nil)))))))
