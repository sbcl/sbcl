;;;; a timer facility based heavily on the timer package by Zach Beane

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

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
             (:constructor %make-priority-queue))
  contents
  keyfun)

(defun make-priority-queue (&key (key #'identity) (element-type t))
  (let ((contents (make-array 100
                              :adjustable t
                              :fill-pointer 0
                              :element-type element-type)))
    (%make-priority-queue :keyfun key
                          :contents contents)))

(def!method print-object ((object priority-queue) stream)
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
  "Add NEW-ITEM to PRIOIRITY-QUEUE."
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
             (:constructor %make-timer))
  #!+sb-doc
  "Timer type. Do not rely on timers being structs as it may change in
future versions."
  name
  function
  expire-time
  repeat-interval
  (thread nil :type (or sb!thread:thread (member t nil)))
  interrupt-function
  cancel-function)

(def!method print-object ((timer timer) stream)
  (let ((name (%timer-name timer)))
    (if name
        (print-unreadable-object (timer stream :type t :identity t)
          (prin1 name stream))
        (print-unreadable-object (timer stream :type t :identity t)
          ;; body is empty => there is only one space between type and
          ;; identity
          ))))

(defun make-timer (function &key name (thread sb!thread:*current-thread*))
  #!+sb-doc
  "Create a timer object that's when scheduled runs FUNCTION. If
THREAD is a thread then that thread is to be interrupted with
FUNCTION. If THREAD is T then a new thread is created each timer
FUNCTION is run. If THREAD is NIL then FUNCTION can be run in any
thread. When THREAD is not T, INTERRUPT-THREAD is used to run FUNCTION
and the ordering guarantees of INTERRUPT-THREAD also apply here.
FUNCTION always runs with interrupts disabled but WITH-INTERRUPTS is
allowed."
  (%make-timer :name name :function function :thread thread))

(defun timer-name (timer)
  #!+sb-doc
  "Return the name of TIMER."
  (%timer-name timer))

(defun timer-scheduled-p (timer &key (delta 0))
  #!+sb-doc
  "See if TIMER will still need to be triggered after DELTA seconds
from now. For timers with a repeat interval it returns true."
  (symbol-macrolet ((expire-time (%timer-expire-time timer))
                    (repeat-interval (%timer-repeat-interval timer)))
      (or (and repeat-interval (plusp repeat-interval))
          (and expire-time
               (<= (+ (get-internal-real-time) delta)
                   expire-time)))))

;;; The scheduler

(defvar *scheduler-lock* (sb!thread:make-mutex :name "Scheduler lock"))

(defmacro with-scheduler-lock ((&optional) &body body)
  ;; Don't let the SIGALRM handler mess things up.
  `(sb!thread::with-system-mutex (*scheduler-lock*)
     ,@body))

(defun under-scheduler-lock-p ()
  (sb!thread:holding-mutex-p *scheduler-lock*))

(defparameter *schedule* (make-priority-queue :key #'%timer-expire-time))

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
  (let ((mutex (sb!thread:make-mutex))
        (cancelledp nil)
        (function (if (%timer-repeat-interval timer)
                      (lambda ()
                        (unwind-protect
                             (funcall (%timer-function timer))
                          (reschedule-timer timer)))
                      (%timer-function timer))))
    (list
     (lambda ()
       ;; Use WITHOUT-INTERRUPTS for the acquiring lock to avoid
       ;; unblocking deferrables unless it's inevitable.
       (without-interrupts
         (sb!thread:with-recursive-lock (mutex)
           (unless cancelledp
             (allow-with-interrupts
               (funcall function))))))
     (lambda ()
       (sb!thread:with-recursive-lock (mutex)
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
          (values-list (make-cancellable-interruptor timer)))
    (when changed-p
      (set-system-timer)))
  (values))

(defun schedule-timer (timer time &key repeat-interval absolute-p)
  #!+sb-doc
  "Schedule TIMER to be triggered at TIME. If ABSOLUTE-P then TIME is
universal time, but non-integral values are also allowed, else TIME is
measured as the number of seconds from the current time. If
REPEAT-INTERVAL is given, TIMER is automatically rescheduled upon
expiry."
  ;; CANCEL-FUNCTION may block until all interruptors finish, let's
  ;; try to cancel without the scheduler lock first.
  (when (%timer-cancel-function timer)
    (funcall (%timer-cancel-function timer)))
  (with-scheduler-lock ()
    (setf (%timer-expire-time timer) (+ (get-internal-real-time)
                                        (delta->real
                                         (if absolute-p
                                             (- time (get-universal-time))
                                             time)))
          (%timer-repeat-interval timer) (if repeat-interval
                                             (delta->real repeat-interval)
                                             nil))
    (%schedule-timer timer)))

(defun unschedule-timer (timer)
  #!+sb-doc
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
      (when old-position
        (funcall (%timer-cancel-function timer)))
      (when (eql 0 old-position)
        (set-system-timer))))
  (values))

(defun list-all-timers ()
  #!+sb-doc
  "Return a list of all timers in the system."
  (with-scheduler-lock ()
    (concatenate 'list (%pqueue-contents *schedule*))))

;;; Not public, but related

(defun reschedule-timer (timer)
  ;; unless unscheduled
  (when (%timer-expire-time timer)
    (let ((thread (%timer-thread timer)))
      (if (and (sb!thread::thread-p thread)
               (not (sb!thread:thread-alive-p thread)))
          (unschedule-timer timer)
          (with-scheduler-lock ()
            ;; Schedule at regular intervals. If TIMER has not finished
            ;; in time then it may catch up later.
            (incf (%timer-expire-time timer) (%timer-repeat-interval timer))
            (%schedule-timer timer))))))

;;; Expiring timers

(defun real-time->sec-and-usec (time)
  ;; KLUDGE: Always leave 0.0001 second for other stuff in order to
  ;; avoid starvation.
  (let ((min-usec 100))
    (if (minusp time)
        (list 0 min-usec)
        (multiple-value-bind (s u) (floor time internal-time-units-per-second)
          (setf u (floor (* (/ u internal-time-units-per-second) 1000000)))
          (if (and (= 0 s) (< u min-usec))
              ;; 0 0 means "shut down the timer" for setitimer
              (list 0 min-usec)
              (list s u))))))

(defun set-system-timer ()
  (assert (under-scheduler-lock-p))
  (assert (not *interrupts-enabled*))
  (let ((next-timer (peek-schedule)))
    (if next-timer
        (let ((delta (- (%timer-expire-time next-timer)
                        (get-internal-real-time))))
          (apply #'sb!unix:unix-setitimer
                 :real 0 0 (real-time->sec-and-usec delta)))
        (sb!unix:unix-setitimer :real 0 0 0 0))))

(defun run-timer (timer)
  (let ((function (%timer-interrupt-function timer))
        (thread (%timer-thread timer)))
    (if (eq t thread)
        (sb!thread:make-thread (without-interrupts
                                 (allow-with-interrupts
                                   function))
                               :name (format nil "Timer ~A"
                                             (%timer-name timer)))
        (let ((thread (or thread sb!thread:*current-thread*)))
          (handler-case
              (sb!thread:interrupt-thread thread function)
            (sb!thread:interrupt-thread-error (c)
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
                do (assert (eq timer (priority-queue-extract-maximum *schedule*)))
                   (push timer timers)))
        (run-timers)))))

(defun timeout-cerror ()
  (cerror "Continue" 'sb!ext::timeout))

(defmacro sb!ext:with-timeout (expires &body body)
  #!+sb-doc
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
           (let ((timer (make-timer #'timeout-cerror)))
             (schedule-timer timer expires)
             (unwind-protect (timeout-body)
               (unschedule-timer timer)))
           (timeout-body)))))
