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

(in-package "SB-IMPL")

(declaim (inline make-deadline))
(defstruct (deadline
             (:constructor make-deadline (internal-time seconds))
             (:copier nil))
  ;; The absolute deadline in internal time.
  (internal-time nil :type internal-time :read-only t)
  ;; A relative representation of the deadline in seconds relative to
  ;; the time this deadline was established. This is used in error
  ;; message and when extended the deadline by the original amount of
  ;; time.
  (seconds       nil :type (real 0)      :read-only t))
(declaim (freeze-type deadline))

;;; Current DEADLINE or NIL.
(declaim (type (or deadline null) *deadline*))
(define-thread-local *deadline* nil)

(declaim (inline seconds-to-internal-time))
(defun seconds-to-internal-time (seconds)
  (the internal-time
       (values (truncate (* seconds internal-time-units-per-second)))))

(defconstant safe-internal-seconds-limit
  ;; Dropping one bit to ensure that
  ;;
  ;;  (let ((seconds (the (integer 0 #.safe-internal-seconds-limit) ...)))
  ;;    (truncate (* 1000 (float seconds 1.0f0))))
  ;;
  ;; doesn't go beyond the INTERNAL-TIME range due to rounding
  ;; errors.
  ;; #. is needed to make the value constant per se as opposed to
  ;; constant by decree, otherwise genesis runs into a problem.
  #.(floor (ash 1 (1- sb-kernel::internal-time-bits))
           internal-time-units-per-second))

(declaim (inline seconds-to-maybe-internal-time))
(defun seconds-to-maybe-internal-time (seconds)
  (typecase seconds
    ((integer 0 #.internal-seconds-limit)
     (locally ; FIXME compiler should learn to figure that out
         (declare (type (integer 0 #.internal-seconds-limit) seconds))
       (seconds-to-internal-time seconds)))
    ((single-float 0.0f0 #.(float safe-internal-seconds-limit 1.0f0))
     (seconds-to-internal-time seconds))
    ((and (not single-float) (real 0 #.safe-internal-seconds-limit))
     (seconds-to-internal-time seconds))))

(declaim (inline seconds-to-internal-time-deadline))
(defun seconds-to-internal-time-deadline (seconds)
  (let ((internal-time (when seconds
                         (seconds-to-maybe-internal-time seconds))))
    (when internal-time
      (+ internal-time (get-internal-real-time)))))

(defmacro with-deadline ((&key seconds override)
                         &body body)
  "Arranges for a TIMEOUT condition to be signalled if an operation
respecting deadlines occurs either after the deadline has passed, or
would take longer than the time left to complete.

Currently only SLEEP, blocking IO operations, GET-MUTEX, and
CONDITION-WAIT respect deadlines, but this includes their implicit
uses inside SBCL itself.

Unless OVERRIDE is true, existing deadlines can only be restricted,
not extended. Deadlines are per thread: children are unaffected by
their parent's deadlines.

Experimental."
  (once-only ((seconds seconds))
    (with-unique-names (deadline)
      `(labels ((with-deadline-thunk ()
                  ,@body)
                (bind-deadline-and-call (deadline)
                  (let ((*deadline* deadline))
                    (with-deadline-thunk)))
                (bind-new-deadline-and-call (deadline-internal-time seconds)
                  (dx-let ((deadline (make-deadline
                                      deadline-internal-time seconds)))
                    (bind-deadline-and-call deadline))))
         (let ((,deadline (when ,seconds
                            (seconds-to-internal-time-deadline ,seconds))))
           (cond
             ((and ,override ,deadline)
              (bind-new-deadline-and-call ,deadline ,seconds))
             (,override
              (bind-deadline-and-call nil))
             (,deadline
              (let ((old *deadline*))
                (if (and old (< (deadline-internal-time old)
                                ,deadline))
                    (bind-deadline-and-call old)
                    (bind-new-deadline-and-call ,deadline ,seconds))))
             (t
              (bind-deadline-and-call nil))))))))

(declaim (inline decode-internal-time))
(defun decode-internal-time (time)
  "Returns internal time value TIME decoded into seconds and microseconds."
  (declare (type internal-time time))
  (multiple-value-bind (sec frac)
      (truncate time internal-time-units-per-second)
    (values sec (* frac sb-unix::microseconds-per-internal-time-unit))))

(defun signal-timeout (datum &rest arguments)
  "Signals a timeout condition while inhibiting further timeouts due to
deadlines while the condition is being handled."
  ;; FIXME: Maybe we should make ERROR do WITH-INTERRUPTS instead of
  ;; putting it all over the place (now that we have ALLOW-WITH-INTERRUPTS.)
  (with-interrupts
    ;; Don't signal a deadline while handling a non-deadline timeout.
    (let ((*deadline* nil))
      (apply #'error datum arguments))))

(defun signal-deadline ()
  "Signal a DEADLINE-TIMEOUT condition, and associate a DEFER-DEADLINE
restart with it. Implementors of blocking functions are responsible
for calling this when a deadline is reached."
  ;; Make sure we don't signal the same deadline twice. LET is not good
  ;; enough: we might catch the same deadline again while unwinding.
  (let ((deadline *deadline*))
    (when deadline
      (setf *deadline* nil))
    (with-interrupts
      (let ((seconds (when deadline
                       (deadline-seconds deadline))))
        (restart-case
            (error 'deadline-timeout :seconds seconds)
          (defer-deadline (&optional (seconds seconds))
            :report "Defer the deadline for SECONDS more."
            :interactive (lambda ()
                           (read-evaluated-form
                            "By how many seconds shall the deadline ~
                             be deferred?: "))
            (setf *deadline*
                  (let ((deadline (when seconds
                                    (seconds-to-internal-time-deadline
                                     seconds))))
                    (when deadline
                      (make-deadline deadline seconds)))))
          (cancel-deadline ()
            :report "Cancel the deadline and continue."
            (setf *deadline* nil))))))
  nil)

(defun defer-deadline (seconds &optional condition)
  "Find the DEFER-DEADLINE restart associated with CONDITION, and
invoke it with SECONDS as argument (deferring the deadline by that many
seconds.) Otherwise return NIL if the restart is not found."
  (try-restart 'defer-deadline condition seconds))

(defun cancel-deadline (&optional condition)
  "Find and invoke the CANCEL-DEADLINE restart associated with
CONDITION, or return NIL if the restart is not found."
  (try-restart 'cancel-deadline condition))

(declaim (inline relative-decoded-times))
(defun relative-decoded-times (abs-sec abs-usec)
  "Returns relative decoded time as two values: difference between
ABS-SEC and ABS-USEC and current real time.

If ABS-SEC and ABS-USEC are in the past, 0 0 is returned."
  (declare (type internal-seconds abs-sec)
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
                          (values (or null internal-seconds)
                                  (or null (mod 1000000))
                                  (or null internal-seconds)
                                  (or null (mod 1000000))
                                  t))
                decode-timeout))
(defun decode-timeout (seconds)
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
  (declare (optimize speed)
           (explicit-check))
  (flet ((return-timeout (timeout deadline signalp)
           (binding* (((to-sec to-usec)
                       (decode-internal-time timeout))
                      ((stop-sec stop-usec)
                       (decode-internal-time deadline)))
             (values to-sec to-usec stop-sec stop-usec signalp)))
         (return-no-timeout ()
           (values nil nil nil nil nil)))
    (let ((deadline *deadline*))
      ;; Use either TIMEOUT or DEADLINE to produce both a timeout and
      ;; deadline in internal-time units.
      (if (or seconds deadline)
          (locally
              (declare (type (or null (real 0)) seconds))
            (let ((timeout (and seconds
                                (seconds-to-maybe-internal-time seconds))))
              (tagbody
               :restart
                 (let* ((now (get-internal-real-time))
                        (deadline-internal-time (when deadline
                                                  (deadline-internal-time deadline)))
                        (deadline-timeout
                          (when deadline-internal-time
                            (let ((time-left (- deadline-internal-time now)))
                              (when (plusp time-left) time-left)))))
                   (return-from decode-timeout
                     (cond
                       ;; We have a timeout and a non-expired deadline. Use the
                       ;; one that expires earlier.
                       ((and timeout deadline-timeout)
                        (if (< timeout deadline-timeout)
                            (return-timeout timeout (+ timeout now) nil)
                            (return-timeout deadline-timeout deadline-internal-time t)))
                       ;; Non-expired deadline.
                       (deadline-timeout
                        (return-timeout deadline-timeout deadline-internal-time t))
                       ;; Expired deadline. Signal the DEADLINE-TIMEOUT
                       ;; condition. In case we return here (i.e. the deadline
                       ;; has been deferred or canceled), pick up the new value
                       ;; of *DEADLINE*.
                       (deadline-internal-time
                        (signal-deadline)
                        (setf deadline *deadline*)
                        (go :restart))
                       ;; There is no deadline but a timeout.
                       (timeout
                        (return-timeout timeout (+ timeout now) nil))
                       (t
                        (return-no-timeout))))))))
          (return-no-timeout)))))
