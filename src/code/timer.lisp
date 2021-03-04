;;;; a timer facility based heavily on the timer package by Zach Beane

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; Heap (for the priority queue)

(declaim (inline heap-parent heap-left heap-right))

(defun heap-parent (i)
  (ash (1- i) -1))

(defun heap-left (i)
  (1+ (ash i 1)))

(defun heap-right (i)
  (+ 2 (ash i 1)))

(defun heapify (heap start &key (key #'identity) (test #'>=))
  (declare (function key test))
  (flet ((key (obj) (funcall key obj))
         (ge (i j) (funcall test i j)))
    (let ((l (heap-left start))
          (r (heap-right start))
          (size (length heap))
          largest)
      (setf largest (if (and (< l size)
                             (not (ge (key (aref heap start))
                                      (key (aref heap l)))))
                        l
                        start))
      (when (and (< r size)
                 (not (ge (key (aref heap largest))
                          (key (aref heap r)))))
        (setf largest r))
      (when (/= largest start)
        (rotatef (aref heap largest) (aref heap start))
        (heapify heap largest :key key :test test)))
    heap))

(defun heap-insert (heap new-item &key (key #'identity) (test #'>=))
  (declare (function key test))
  (flet ((key (obj) (funcall key obj))
         (ge (i j) (funcall test i j)))
    (vector-push-extend nil heap)
    (loop for i = (1- (length heap)) then parent-i
          for parent-i = (heap-parent i)
          while (and (> i 0)
                     (not (ge (key (aref heap parent-i))
                              (key new-item))))
          do (setf (aref heap i) (aref heap parent-i))
          finally (setf (aref heap i) new-item)
          (return-from heap-insert i))))

(defun heap-maximum (heap)
  (unless (zerop (length heap))
    (aref heap 0)))

(defun heap-extract (heap i &key (key #'identity) (test #'>=))
  (unless (> (length heap) i)
    (error "Heap underflow"))
  (prog1
      (aref heap i)
    (setf (aref heap i) (aref heap (1- (length heap))))
    (decf (fill-pointer heap))
    (heapify heap i :key key :test test)))

(defun heap-extract-maximum (heap &key (key #'identity) (test #'>=))
  (heap-extract heap 0 :key key :test test))

;;; Priority queue

(defstruct (priority-queue
             (:conc-name %pqueue-)
             (:constructor make-priority-queue
               (&key ((:key keyfun) #'identity) (element-type t)
                &aux (contents (make-array 100
                                           :adjustable t
                                           :fill-pointer 0
                                           :element-type element-type))))
             (:copier nil))
  (contents nil :type vector   :read-only t)
  (keyfun   nil :type function :read-only t))
(declaim (freeze-type priority-queue))

(defmethod print-object ((object priority-queue) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~[empty~:;~:*~D item~:P~]"
            (length (%pqueue-contents object)))))

(defun priority-queue-maximum (priority-queue)
  "Return the item in PRIORITY-QUEUE with the largest key."
  (symbol-macrolet ((contents (%pqueue-contents priority-queue)))
    (unless (zerop (length contents))
      (heap-maximum contents))))

(defun priority-queue-extract-maximum (priority-queue)
  "Remove and return the item in PRIORITY-QUEUE with the largest key."
  (symbol-macrolet ((contents (%pqueue-contents priority-queue))
                    (keyfun (%pqueue-keyfun priority-queue)))
    (unless (zerop (length contents))
      (heap-extract-maximum contents :key keyfun :test #'<=))))

(defun priority-queue-insert (priority-queue new-item)
  "Add NEW-ITEM to PRIORITY-QUEUE."
  (symbol-macrolet ((contents (%pqueue-contents priority-queue))
                    (keyfun (%pqueue-keyfun priority-queue)))
    (heap-insert contents new-item :key keyfun :test #'<=)))

(defun priority-queue-empty-p (priority-queue)
  (zerop (length (%pqueue-contents priority-queue))))

(defun priority-queue-remove (priority-queue item &key (test #'eq))
  "Remove and return ITEM from PRIORITY-QUEUE."
  (symbol-macrolet ((contents (%pqueue-contents priority-queue))
                    (keyfun (%pqueue-keyfun priority-queue)))
    (let ((i (position item contents :test test)))
      (when i
        (heap-extract contents i :key keyfun :test #'<=)
        i))))

;;; timers

(defstruct (timer
             (:conc-name %timer-)
             (:constructor
              make-timer
              (function &key name (thread sb-thread:*current-thread*)))
             (:copier nil))
  "Timer type. Do not rely on timers being structs as it may change in
future versions."
  (name               nil :read-only t)
  (function           nil :read-only t)
  (expire-time        1   :type (or null real))
  (repeat-interval    nil :type (or null (real 0)))
  (catch-up           nil :type boolean)
  (thread             nil :type (or sb-thread:thread boolean))
  (interrupt-function nil :type (or null function))
  (cancel-function    nil :type (or null function)))
(declaim (freeze-type timer))

(defmethod print-object ((timer timer) stream)
  (let ((name (%timer-name timer)))
    (if name
        (print-unreadable-object (timer stream :type t :identity t)
          (prin1 name stream))
        (print-unreadable-object (timer stream :type t :identity t)
          ;; body is empty => there is only one space between type and
          ;; identity
          ))))

(setf (documentation 'make-timer 'function)
      "Create a timer that runs FUNCTION when triggered.

If a THREAD is supplied, FUNCTION is run in that thread. If THREAD is
T, a new thread is created for FUNCTION each time the timer is
triggered. If THREAD is NIL, FUNCTION is run in an unspecified thread.

When THREAD is not T, INTERRUPT-THREAD is used to run FUNCTION and the
ordering guarantees of INTERRUPT-THREAD apply. In that case, FUNCTION
runs with interrupts disabled but WITH-INTERRUPTS is allowed.")

(defun timer-name (timer)
  "Return the name of TIMER."
  (%timer-name timer))

(defun timer-scheduled-p (timer &key (delta 0))
  "See if TIMER will still need to be triggered after DELTA seconds
from now. For timers with a repeat interval it returns true."
  (symbol-macrolet ((expire-time (%timer-expire-time timer))
                    (repeat-interval (%timer-repeat-interval timer)))
    (or (and repeat-interval (plusp repeat-interval))
        (and expire-time
             (<= (+ (get-internal-real-time) delta)
                 expire-time)))))

;;; The scheduler

(define-load-time-global *scheduler-lock* (sb-thread:make-mutex :name "Scheduler lock"))

(defmacro with-scheduler-lock ((&optional) &body body)
  ;; Don't let the SIGALRM handler mess things up.
  `(with-system-mutex (*scheduler-lock*)
     ,@body))

(defun under-scheduler-lock-p ()
  (sb-thread:holding-mutex-p *scheduler-lock*))

(define-load-time-global *schedule* (make-priority-queue :key #'%timer-expire-time))

(defun peek-schedule ()
  (priority-queue-maximum *schedule*))

(defun time-left (timer)
  (- (%timer-expire-time timer) (get-internal-real-time)))

;;; real time conversion

(defun delta->real (delta)
  (floor (* delta internal-time-units-per-second)))

;;; Public interface

(defun make-cancellable-interruptor (timer)
  ;; return a list of two functions: one that does the same as
  ;; FUNCTION until the other is called, from when it does nothing.
  (let ((mutex (sb-thread:make-mutex))
        (cancelledp nil)
        (function (if (%timer-repeat-interval timer)
                      (lambda ()
                        (unwind-protect
                             (funcall (%timer-function timer))
                          (reschedule-timer timer)))
                      (%timer-function timer))))
    (values
     (lambda ()
       ;; Use WITHOUT-INTERRUPTS for the acquiring lock to avoid
       ;; unblocking deferrables unless it's inevitable.
       (without-interrupts
         (sb-thread:with-recursive-lock (mutex)
           (unless cancelledp
             (allow-with-interrupts
               (funcall function))))))
     (lambda ()
       (sb-thread:with-recursive-lock (mutex)
         (setq cancelledp t))))))

(defun %schedule-timer (timer)
  (let ((changed-p nil)
        (old-position (priority-queue-remove *schedule* timer)))
    ;; Make sure interruptors are cancelled even if this timer was
    ;; scheduled again since our last attempt.
    (when old-position
      (funcall (%timer-cancel-function timer)))
    (when (eql 0 old-position)
      (setq changed-p t))
    (when (zerop (priority-queue-insert *schedule* timer))
      (setq changed-p t))
    (setf (values (%timer-interrupt-function timer)
                  (%timer-cancel-function timer))
          (make-cancellable-interruptor timer))
    (when changed-p
      (set-system-timer)))
  (values))

(defun schedule-timer (timer time
                       &key
                       repeat-interval
                       absolute-p
                       (catch-up nil catch-up-p))
  "Schedule TIMER to be triggered at TIME. If ABSOLUTE-P then TIME is
universal time, but non-integral values are also allowed, else TIME is
measured as the number of seconds from the current time.

If REPEAT-INTERVAL is given, TIMER is automatically rescheduled upon
expiry.

If REPEAT-INTERVAL is non-NIL, the Boolean CATCH-UP controls whether
TIMER will \"catch up\" by repeatedly calling its function without
delay in case calls are missed because of a clock discontinuity such
as a suspend and resume cycle of the computer. The default is NIL,
i.e. do not catch up."
  (when (and catch-up-p (not repeat-interval))
    (error "~@<~A does not make sense without ~A.~@:>"
           :catch-up :repeat-interval))
  ;; CANCEL-FUNCTION may block until all interruptors finish, let's
  ;; try to cancel without the scheduler lock first.
  (when (%timer-cancel-function timer)
    (funcall (%timer-cancel-function timer)))
  (with-scheduler-lock ()
    (let ((delta/real (delta->real
                       (if absolute-p
                           (- time (get-universal-time))
                           time))))
      (setf (%timer-expire-time timer) (+ (get-internal-real-time) delta/real)
            (%timer-repeat-interval timer) (when repeat-interval
                                             (delta->real repeat-interval))
            (%timer-catch-up timer) catch-up))
    (%schedule-timer timer)))

(defun unschedule-timer (timer)
  "Cancel TIMER. Once this function returns it is guaranteed that
TIMER shall not be triggered again and there are no unfinished
triggers."
  (let ((cancel-function (%timer-cancel-function timer)))
    (when cancel-function
      (funcall cancel-function)))
  (with-scheduler-lock ()
    (setf (%timer-expire-time timer) nil
          (%timer-repeat-interval timer) nil)
    (let ((old-position (priority-queue-remove *schedule* timer)))
      ;; Don't use cancel-function as the %timer-cancel-function
      ;; may have changed before we got the scheduler lock.
      (when old-position
        (funcall (%timer-cancel-function timer)))
      (when (eql 0 old-position)
        (set-system-timer))))
  (values))

(defun list-all-timers ()
  "Return a list of all timers in the system."
  (with-scheduler-lock ()
    (concatenate 'list (%pqueue-contents *schedule*))))

;;; Not public, but related

(defun reschedule-timer (timer)
  ;; unless unscheduled
  (symbol-macrolet ((expire-time (%timer-expire-time timer))
                    (repeat-interval (%timer-repeat-interval timer))
                    (catch-up (%timer-catch-up timer))
                    (thread (%timer-thread timer)))
    (when expire-time
      (if (and (sb-thread::thread-p thread)
               (not (sb-thread:thread-alive-p thread)))
          (unschedule-timer timer)
          (with-scheduler-lock ()
            ;; Schedule at regular intervals. If TIMER has not finished
            ;; in time then it may catch up later.
            (incf expire-time repeat-interval)
            ;; If the internal real time had a discontinuity
            ;; (e.g. computer suspended and resumed), maybe adjust the
            ;; expiration time accordingly unless the timer is
            ;; configured to "catch up" by performing the missed calls
            ;; immediately.
            (unless catch-up
              (let ((now (get-internal-real-time)))
                (when (< expire-time now)
                  (setf expire-time (+ now repeat-interval)))))
            (%schedule-timer timer))))))

;;; setitimer is unavailable for win32, but we can emulate it when
;;; threads are available -- using win32 waitable timers.
;;;
;;; Conversely, when we want to minimize signal use on POSIX, we emulate
;;; win32 waitable timers using a timerfd-like portability layer in
;;; the runtime.

#+win32
(define-alien-type wtimer system-area-pointer) ;HANDLE, but that's not defined yet

#+win32
(progn
  (define-alien-routine "os_create_wtimer" wtimer)
  (define-alien-routine "os_wait_for_wtimer" int (wt wtimer))
  (define-alien-routine "os_close_wtimer" void (wt wtimer))
  (define-alien-routine "os_cancel_wtimer" void (wt wtimer))
  (define-alien-routine "os_set_wtimer" void (wt wtimer) (sec int) (nsec int))

  ;; scheduler lock already protects us

  (defvar *waitable-timer-handle* nil)

  (defvar *timer-thread* nil)

  (defun get-waitable-timer ()
    (aver (under-scheduler-lock-p))
    (or *waitable-timer-handle*
        (prog1
            (setf *waitable-timer-handle* (os-create-wtimer))
          (setf *timer-thread*
                (sb-thread::make-system-thread
                 "System timer watchdog thread"
                 (lambda ()
                   (loop while
                         (or (zerop
                              (os-wait-for-wtimer *waitable-timer-handle*))
                             *waitable-timer-handle*)
                         doing (run-expired-timers)))
                 nil nil)))))

  (defun itimer-emulation-deinit ()
    (with-scheduler-lock ()
      (when *timer-thread*
        (sb-thread:terminate-thread *timer-thread*)
        (sb-thread:join-thread *timer-thread* :default nil))
      (when *waitable-timer-handle*
        (os-close-wtimer *waitable-timer-handle*)
        (setf *waitable-timer-handle* nil))))

  (defun %clear-system-timer ()
    (os-cancel-wtimer (get-waitable-timer)))

  (defun %set-system-timer (sec nsec)
    (os-set-wtimer (get-waitable-timer) sec nsec)))

;;; Expiring timers

(defun real-time->sec-and-nsec (time)
  ;; KLUDGE: Always leave 0.0001 second for other stuff in order to
  ;; avoid starvation.
  (let ((min-nsec 100000))
    (if (minusp time)
        (values 0 min-nsec)
        (multiple-value-bind (s u) (floor time internal-time-units-per-second)
          (setf u (floor (* (/ u internal-time-units-per-second)
                            #.(expt 10 9))))
          (if (and (= 0 s) (< u min-nsec))
              ;; 0 0 means "shut down the timer" for setitimer
              (values 0 min-nsec)
              (values s u))))))

#+unix
(progn
  (defun %set-system-timer (sec nsec)
    (sb-unix:unix-setitimer :real 0 0 sec (ceiling nsec 1000)))

  (defun %clear-system-timer ()
    (sb-unix:unix-setitimer :real 0 0 0 0)))

(defun set-system-timer ()
  (aver (under-scheduler-lock-p))
  (aver (not *interrupts-enabled*))
  (let ((next-timer (peek-schedule)))
    (if next-timer
        (let ((delta (- (%timer-expire-time next-timer)
                        (get-internal-real-time))))
          (multiple-value-call #'%set-system-timer
            (real-time->sec-and-nsec delta)))
        (%clear-system-timer))))

(defun run-timer (timer)
  (let ((function (%timer-interrupt-function timer))
        (thread (%timer-thread timer)))
    (if (eq t thread)
        (sb-thread:make-thread function :name (format nil "Timer ~A"
                                                      (%timer-name timer)))
        ;; Don't run the timer directly in the current thread.
        ;; The signal that interrupt-thread sends is blocked so it'll get queued
        ;; and processed after exiting from SIGALRM-HANDLER.
        ;; That way we process all pending signals and release the *SCHEDULER-LOCK*.
        (let ((thread (or thread sb-thread:*current-thread*)))
          (handler-case
              (sb-thread:interrupt-thread thread function)
            (sb-thread:interrupt-thread-error (c)
              (declare (ignore c))
              (warn "Timer ~S failed to interrupt thread ~S."
                    timer thread)))))))

;;; Called from the signal handler. We loop until all the expired timers
;;; have been run.
(defun run-expired-timers ()
  (loop
    (let ((now (get-internal-real-time))
          (timers nil))
      (flet ((run-timers ()
               (dolist (timer (nreverse timers))
                 (run-timer timer))))
        (with-scheduler-lock ()
          (loop for timer = (peek-schedule)
                when (or (null timer) (< now (%timer-expire-time timer)))
                ;; No more timers to run for now, reset the system timer.
                do (run-timers)
                   (set-system-timer)
                   (return-from run-expired-timers nil)
                else
                do (aver (eq timer (priority-queue-extract-maximum *schedule*)))
                   (push timer timers)))
        (run-timers)))))

(defun timeout-cerror (&optional seconds)
  (cerror "Continue" 'timeout :seconds seconds))

(defmacro with-timeout (expires &body body)
  "Execute the body, asynchronously interrupting it and signalling a TIMEOUT
condition after at least EXPIRES seconds have passed.

Note that it is never safe to unwind from an asynchronous condition. Consider:

  (defun call-with-foo (function)
    (let (foo)
      (unwind-protect
         (progn
           (setf foo (get-foo))
           (funcall function foo))
       (when foo
         (release-foo foo)))))

If TIMEOUT occurs after GET-FOO has executed, but before the assignment, then
RELEASE-FOO will be missed. While individual sites like this can be made proof
against asynchronous unwinds, this doesn't solve the fundamental issue, as all
the frames potentially unwound through need to be proofed, which includes both
system and application code -- and in essence proofing everything will make
the system uninterruptible."
  `(dx-flet ((timeout-body () ,@body))
     (let ((expires ,expires))
       ;; FIXME: a temporary compatibility workaround for CLX, if unsafe
       ;; unwinds are handled revisit it.
       (if (> expires 0)
           (let ((timer (make-timer (lambda () (timeout-cerror expires)))))
             (schedule-timer timer expires)
             (unwind-protect (timeout-body)
               (unschedule-timer timer)))
           (timeout-body)))))
