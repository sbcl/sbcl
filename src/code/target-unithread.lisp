;;;; unithread stub support for threads in the target machine

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!THREAD")

;;; used bu debug-int.lisp to access interrupt contexts
#!-sb-fluid (declaim (inline sb!vm::current-thread-offset-sap))
(defun sb!vm::current-thread-offset-sap (n) 
  (declare (type (unsigned-byte 27) n))
  (sb!sys:sap-ref-sap (alien-sap (extern-alien "all_threads" (* t))) 
	       (* n 4)))

(defun current-thread-id ()
  (sb!sys:sap-ref-32 (alien-sap (extern-alien "all_threads" (* t))) 
	       (* sb!vm::thread-pid-slot 4)))

;;;; queues, locks 

;; spinlocks use 0 as "free" value: higher-level locks use NIL
(defun get-spinlock (lock offset new-value)
  (declare (ignore lock offset new-value)))

(defmacro with-spinlock ((queue) &body body)
  (declare (ignore queue))
  `(progn ,@body))

;;;; the higher-level locking operations are based on waitqueues

(defstruct waitqueue
  (name nil :type (or null simple-base-string))
  (lock 0)
  (data nil))

(defstruct (mutex (:include waitqueue))
  (value nil))

#+nil
(defun wait-on-queue (queue &optional lock)
  (let ((pid (current-thread-id)))
    ;; FIXME what should happen if we get interrupted when we've blocked
    ;; the sigcont?  For that matter, can we get interrupted?
    (block-sigcont)
    (when lock (release-mutex lock))
    (get-spinlock queue 2 pid)
    (pushnew pid (waitqueue-data queue))
    (setf (waitqueue-lock queue) 0)
    (unblock-sigcont-and-sleep)))

#+nil
(defun dequeue (queue)
  (let ((pid (current-thread-id)))
    (get-spinlock queue 2 pid)
    (setf (waitqueue-data queue)
	  (delete pid (waitqueue-data queue)))
    (setf (waitqueue-lock queue) 0)))

#+nil
(defun signal-queue-head (queue)
  (let ((pid (current-thread-id)))
    (get-spinlock queue 2 pid)
    (let ((h (car (waitqueue-data queue))))
      (setf (waitqueue-lock queue) 0)
      (when h
	(sb!unix:unix-kill h sb!unix:sigcont)))))

;;;; mutex

#+nil
(defun get-mutex (lock &optional new-value (wait-p t))
  (declare (type mutex lock))
  (let ((pid (current-thread-id)))
    (unless new-value (setf new-value pid))
    (assert (not (eql new-value (mutex-value lock))))
    (loop
     (unless
	 ;; args are object slot-num old-value new-value
	 (sb!vm::%instance-set-conditional lock 4 nil new-value)
       (dequeue lock)
       (return t))
     (unless wait-p (return nil))
     (wait-on-queue lock nil))))

#+nil
(defun release-mutex (lock &optional (new-value nil))
  (declare (type mutex lock))
  (let ((old-value (mutex-value lock))
	(t1 nil))
    (loop
     (unless
	 ;; args are object slot-num old-value new-value
	 (eql old-value
	      (setf t1
		    (sb!vm::%instance-set-conditional lock 4 old-value new-value)))       
       (signal-queue-head lock)
       (return t))
     (setf old-value t1))))

(defun get-mutex (lock &optional new-value (wait-p t))
  (declare (type mutex lock))
  (let ((old-value (mutex-value lock)))
    (when (and old-value wait-p)
      (error "In unithread mode, mutex ~S was requested with WAIT-P ~S and ~
              new-value ~S, but has already been acquired (with value ~S)."
	     lock wait-p new-value old-value))
    (setf (mutex-value lock) new-value)
    t))

(defun release-mutex (lock)
  (declare (type mutex lock))
  (setf (mutex-value lock) nil))

;;; what's the best thing to do with these on unithread?  commented
;;; functions are the thread versions, just to remind me what they do
;;; there
#+nil
(defun condition-wait (queue lock)
  "Atomically release LOCK and enqueue ourselves on QUEUE.  Another
thread may subsequently notify us using CONDITION-NOTIFY, at which
time we reacquire LOCK and return to the caller."
  (unwind-protect
       (wait-on-queue queue lock)
    ;; If we are interrupted while waiting, we should do these things
    ;; before returning.  Ideally, in the case of an unhandled signal,
    ;; we should do them before entering the debugger, but this is
    ;; better than nothing.
    (dequeue queue)
    (get-mutex lock)))

#+nil
(defun condition-notify (queue)
  "Notify one of the processes waiting on QUEUE"
  (signal-queue-head queue))

(defun maybe-install-futex-functions () nil)

;;;; job control

(defun init-job-control () t)
(defun debugger-wait-until-foreground-thread (stream)
  (declare (ignore stream))
  t)
(defun get-foreground () t)
(defun release-foreground (&optional next)
  (declare (ignore next))
  t)
(defun terminate-session ())
