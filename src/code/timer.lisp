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
  (ash i -1))

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
  (when (< (length heap) i)
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
  name
  function
  expire-time
  repeat-interval
  (thread nil :type (or sb!thread:thread (member t nil))))

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
  (%make-timer :name name :function function :thread thread))

(defun timer-name (timer)
  (%timer-name timer))

(defun timer-expired-p (timer &optional (delta 0))
  (symbol-macrolet ((expire-time (%timer-expire-time timer))
                    (repeat-interval (%timer-repeat-interval timer)))
    (and (not (and repeat-interval (plusp repeat-interval)))
         (or (null expire-time)
             (< expire-time
                (+ (get-internal-real-time) delta))))))

;;; The scheduler

(defvar *scheduler-lock* (sb!thread:make-mutex :name "Scheduler lock"))

(defmacro with-scheduler-lock ((&optional) &body body)
  ;; don't let the SIGALRM handler mess things up
  `(sb!sys:without-interrupts
    (sb!thread:with-mutex (*scheduler-lock*)
      ,@body)))

(defun under-scheduler-lock-p ()
  #!-sb-thread
  t
  #!+sb-thread
  (eq sb!thread:*current-thread* (sb!thread:mutex-value *scheduler-lock*)))

(defparameter *schedule* (make-priority-queue :key #'%timer-expire-time))

(defun peek-schedule ()
  (priority-queue-maximum *schedule*))

(defun time-left (timer)
  (- (%timer-expire-time timer) (get-internal-real-time)))

;;; real time conversion

(defun delta->real (delta)
  (floor (* delta internal-time-units-per-second)))

;;; Public interface

(defun %schedule-timer (timer)
  (let ((changed-p nil))
    (when (eql 0 (priority-queue-remove *schedule* timer))
      (setq changed-p t))
    (when (zerop (priority-queue-insert *schedule* timer))
      (setq changed-p t))
    (when changed-p
      (set-system-timer)))
  (values))

(defun schedule-timer (timer time &key repeat-interval absolute-p)
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
  (with-scheduler-lock ()
    (setf (%timer-expire-time timer) nil
          (%timer-repeat-interval timer) nil)
    (when (eql 0 (priority-queue-remove *schedule* timer))
      (set-system-timer)))
  (values))

(defun list-all-timers ()
  (with-scheduler-lock ()
    (concatenate 'list (%pqueue-contents *schedule*))))

;;; Not public, but related

(defun reschedule-timer (timer)
  (with-scheduler-lock ()
    (setf (%timer-expire-time timer) (+ (get-internal-real-time)
                                        (%timer-repeat-interval timer)))
    (%schedule-timer timer)))

;;; Expiring timers

(defun real-time->sec-and-usec(time)
  (if (minusp time)
      (list 0 1)
      (multiple-value-bind (s u) (floor time internal-time-units-per-second)
        (setf u (floor (* (/ u internal-time-units-per-second) 1000000)))
        (if (= 0 s u)
            ;; 0 0 means "shut down the timer" for setitimer
            (list 0 1)
            (list s u)))))

(defun set-system-timer ()
  (assert (under-scheduler-lock-p))
  (let ((next-timer (peek-schedule)))
    (if next-timer
        (let ((delta (- (%timer-expire-time next-timer)
                        (get-internal-real-time))))
          (apply #'sb!unix:unix-setitimer
                 :real 0 0 (real-time->sec-and-usec delta)))
        (sb!unix:unix-setitimer :real 0 0 0 0))))

(defun run-timer (timer)
  (symbol-macrolet ((function (%timer-function timer))
                    (repeat-interval (%timer-repeat-interval timer))
                    (thread (%timer-thread timer)))
    (when repeat-interval
      (reschedule-timer timer))
    (cond ((null thread)
           (funcall function))
          ((eq t thread)
           (sb!thread:make-thread function))
          (t
           (handler-case
               (sb!thread:interrupt-thread thread function)
             (sb!thread:interrupt-thread-error (c)
               (warn c)))))))

(defun run-expired-timers ()
  (unwind-protect
       (let (timer)
         (loop
          (with-scheduler-lock ()
            (setq timer (peek-schedule))
            (unless (and timer
                         (> (get-internal-real-time)
                            (%timer-expire-time timer)))
              (return-from run-expired-timers nil))
            (assert (eq timer (priority-queue-extract-maximum *schedule*))))
          ;; run the timer without the lock
          (run-timer timer)))
    (with-scheduler-lock ()
      (set-system-timer))))

(defmacro sb!ext:with-timeout (expires &body body)
  "Execute the body, asynchronously interrupting it and signalling a
TIMEOUT condition after at least EXPIRES seconds have passed."
  (with-unique-names (timer)
    `(let ((,timer (make-timer (lambda ()
                                 (cerror "Continue" 'sb!ext::timeout)))))
       (schedule-timer ,timer ,expires)
       (unwind-protect
            (progn ,@body)
         (unschedule-timer ,timer)))))
