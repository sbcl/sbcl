(in-package "SB!THREAD")

;;; FIXME it would be good to define what a thread id is or isn't (our
;;; current assumption is that it's a fixnum).  It so happens that on
;;; Linux it's a pid, but it might not be on posix thread implementations

(sb!alien::define-alien-routine ("create_thread" %create-thread)
     sb!alien:unsigned-long
  (lisp-fun-address sb!alien:unsigned-long))

(sb!alien::define-alien-routine "signal_thread_to_dequeue"
    sb!alien:unsigned-int
  (thread-pid sb!alien:unsigned-long))


(defun make-thread (function)
  (let ((real-function (coerce function 'function)))
    (%create-thread
     (sb!kernel:get-lisp-obj-address
      (lambda ()
	;; in time we'll move some of the binding presently done in C
	;; here too
	(let ((sb!kernel::*restart-clusters* nil)
	      (sb!impl::*descriptor-handlers* nil); serve-event
	      (sb!impl::*available-buffers* nil)) ;for fd-stream
	  ;; can't use handling-end-of-the-world, because that flushes
	  ;; output streams, and we don't necessarily have any (or we
	  ;; could be sharing them)
	  (sb!sys:enable-interrupt sb!unix:sigint :ignore)
	  (sb!unix:unix-exit
	   (catch 'sb!impl::%end-of-the-world 
	     (with-simple-restart 
		 (destroy-thread
		  (format nil "~~@<Destroy this thread (~A)~~@:>"
			  (current-thread-id)))
	       (funcall real-function))
	     0))))))))

;;; Really, you don't want to use these: they'll get into trouble with
;;; garbage collection.  Use a lock or a waitqueue instead
(defun suspend-thread (thread-id)
  (sb!unix:unix-kill thread-id sb!unix:sigstop))
(defun resume-thread (thread-id)
  (sb!unix:unix-kill thread-id sb!unix:sigcont))
;;; Note warning about cleanup forms
(defun destroy-thread (thread-id)
  "Destroy the thread identified by THREAD-ID abruptly, without running cleanup forms"
  (sb!unix:unix-kill thread-id sb!unix:sigterm)
  ;; may have been stopped for some reason, so now wake it up to
  ;; deliver the TERM
  (sb!unix:unix-kill thread-id sb!unix:sigcont))


;;; a moderate degree of care is expected for use of interrupt-thread,
;;; due to its nature: if you interrupt a thread that was holding
;;; important locks then do something that turns out to need those
;;; locks, you probably won't like the effect.  Used with thought
;;; though, it's a good deal gentler than the last-resort functions above

(defun interrupt-thread (thread function)
  "Interrupt THREAD and make it run FUNCTION.  "
  (sb!unix::syscall* ("interrupt_thread"
		      sb!alien:unsigned-long  sb!alien:unsigned-long)
		     thread
		     thread (sb!kernel:get-lisp-obj-address
			     (coerce function 'function))))
(defun terminate-thread (thread-id)
  "Terminate the thread identified by THREAD-ID, by causing it to run
SB-EXT:QUIT - the usual cleanup forms will be evaluated"
  (interrupt-thread thread-id 'sb!ext:quit))

(declaim (inline current-thread-id))
(defun current-thread-id ()
  (logand 
   (sb!sys:sap-int
    (sb!vm::current-thread-offset-sap sb!vm::thread-pid-slot))
   ;; KLUDGE pids are 16 bit really.  Avoid boxing the return value
   (1- (ash 1 16))))

;;;; iterate over the in-memory threads

(defun mapcar-threads (function)
  "Call FUNCTION once for each known thread, giving it the thread structure as argument"
  (let ((function (coerce function 'function)))
    (loop for thread = (alien-sap (extern-alien "all_threads" (* t)))
	  then  (sb!sys:sap-ref-sap thread (* 4 sb!vm::thread-next-slot))
	  until (sb!sys:sap= thread (sb!sys:int-sap 0))
	  collect (funcall function thread))))

;;;; queues, locks 

;; spinlocks use 0 as "free" value: higher-level locks use NIL
(declaim (inline get-spinlock release-spinlock))

(defun get-spinlock (lock offset new-value)
  (declare (optimize (speed 3) (safety 0)))
  (loop until
	(eql (sb!vm::%instance-set-conditional lock offset 0 new-value) 0)))

;; this should do nothing if we didn't own the lock, so safe to use in
;; unwind-protect cleanups when lock acquisition failed for some reason
(defun release-spinlock (lock offset our-value)
  (declare (optimize (speed 3) (safety 0)))
  (sb!vm::%instance-set-conditional lock offset our-value 0))

(defmacro with-spinlock ((queue) &body body)
  (with-unique-names (pid)
    `(let ((,pid (current-thread-id)))
       (unwind-protect
	    (progn
	      (get-spinlock ,queue 2 ,pid)
	      ,@body)
	 (release-spinlock ,queue 2 ,pid)))))


;;;; the higher-level locking operations are based on waitqueues

(declaim (inline waitqueue-data-address mutex-value-address))

(defstruct waitqueue
  (name nil :type (or null simple-base-string))
  (lock 0)
  (data nil))

;;; The bare 4 here and 5 below are offsets of the slots in the struct.
;;; There ought to be some better way to get these numbers
(defun waitqueue-data-address (lock)
  (declare (optimize (speed 3)))
  (sb!ext:truly-the
   (unsigned-byte 32)
   (+ (sb!kernel:get-lisp-obj-address lock)
      (- (* 4 sb!vm:n-word-bytes) sb!vm:instance-pointer-lowtag))))

(defstruct (mutex (:include waitqueue))
  (value nil))

(defun mutex-value-address (lock)
  (declare (optimize (speed 3)))
  (sb!ext:truly-the
   (unsigned-byte 32)
   (+ (sb!kernel:get-lisp-obj-address lock)
      (- (* 5 sb!vm:n-word-bytes) sb!vm:instance-pointer-lowtag))))

(sb!alien:define-alien-routine "block_sigcont"  void)
(sb!alien:define-alien-routine "unblock_sigcont_and_sleep"  void)

#!+sb-futex
(declaim (inline futex-wait futex-wake))
#!+sb-futex
(sb!alien:define-alien-routine
    "futex_wait" int (word unsigned-long) (old-value unsigned-long))
#!+sb-futex
(sb!alien:define-alien-routine
    "futex_wake" int (word unsigned-long) (n unsigned-long))

;;; this should only be called while holding the queue spinlock.
;;; it releases the spinlock before sleeping
(defun wait-on-queue (queue &optional lock)
  (let ((pid (current-thread-id)))
    (block-sigcont)
    (when lock (release-mutex lock))
    (sb!sys:without-interrupts
     (pushnew pid (waitqueue-data queue)))
    (setf (waitqueue-lock queue) 0)
    (unblock-sigcont-and-sleep)))

;;; this should only be called while holding the queue spinlock.  It doesn't
;;; release it
(defun dequeue (queue)
  (let ((pid (current-thread-id)))
    (sb!sys:without-interrupts     
     (setf (waitqueue-data queue)
	   (delete pid (waitqueue-data queue))))))

;;; this should only be called while holding the queue spinlock.
(defun signal-queue-head (queue)
  (let ((p (car (waitqueue-data queue))))
    (when p (signal-thread-to-dequeue p))))

;;;; mutex

;;; i suspect there may be a race still in this: the futex version requires
;;; the old mutex value before sleeping, so how do we get away without it
(defun get-mutex (lock &optional new-value (wait-p t))
  (declare (type mutex lock) (optimize (speed 3)))
  (let ((pid (current-thread-id)))
    (unless new-value (setf new-value pid))
    (assert (not (eql new-value (mutex-value lock))))
    (get-spinlock lock 2 pid)
    (loop
     (unless
	 ;; args are object slot-num old-value new-value
	 (sb!vm::%instance-set-conditional lock 4 nil new-value)
       (dequeue lock)
       (setf (waitqueue-lock lock) 0)
       (return t))
     (unless wait-p
       (setf (waitqueue-lock lock) 0)
       (return nil))
     (wait-on-queue lock nil))))

#!+sb-futex
(defun get-mutex/futex (lock &optional new-value (wait-p t))
  (declare (type mutex lock)  (optimize (speed 3)))
  (let ((pid (current-thread-id))
	old)
    (unless new-value (setf new-value pid))
    (assert (not (eql new-value (mutex-value lock))))
    (loop
     (unless
	 (setf old (sb!vm::%instance-set-conditional lock 4 nil new-value))
       (return t))
     (unless wait-p (return nil))
     (futex-wait (mutex-value-address lock)
		 (sb!kernel:get-lisp-obj-address old)))))

(defun release-mutex (lock &optional (new-value nil))
  (declare (type mutex lock))
  ;; we assume the lock is ours to release
  (with-spinlock (lock)
    (setf (mutex-value lock) new-value)
    (signal-queue-head lock)))

#!+sb-futex
(defun release-mutex/futex (lock)
  (declare (type mutex lock))
  (setf (mutex-value lock) nil)
  (futex-wake (mutex-value-address lock) 1))


(defmacro with-mutex ((mutex &key value (wait-p t))  &body body)
  (with-unique-names (got)
    `(let ((,got (get-mutex ,mutex ,value ,wait-p)))
      (when ,got
	(unwind-protect
	     (progn ,@body)
	  (release-mutex ,mutex))))))


;;;; condition variables

(defun condition-wait (queue lock)
  "Atomically release LOCK and enqueue ourselves on QUEUE.  Another
thread may subsequently notify us using CONDITION-NOTIFY, at which
time we reacquire LOCK and return to the caller."
  (assert lock)
  (let ((value (mutex-value lock)))
    (unwind-protect
	 (progn
	   (get-spinlock queue 2 (current-thread-id))
	   (wait-on-queue queue lock))
      ;; If we are interrupted while waiting, we should do these things
      ;; before returning.  Ideally, in the case of an unhandled signal,
      ;; we should do them before entering the debugger, but this is
      ;; better than nothing.
      (with-spinlock (queue)
	(dequeue queue))
      (get-mutex lock value))))

#!+sb-futex
(defun condition-wait/futex (queue lock)
  (assert lock)
  (let ((value (mutex-value lock)))
    (unwind-protect
	 (let ((me (current-thread-id)))
	   ;; XXX we should do something to ensure that the result of this setf
	   ;; is visible to all CPUs
	   (setf (waitqueue-data queue) me)
	   (release-mutex lock)
	   ;; Now we go to sleep using futex-wait.  If anyone else
	   ;; manages to grab LOCK and call CONDITION-NOTIFY during
	   ;; this comment, it will change queue->data, and so
	   ;; futex-wait returns immediately instead of sleeping.
	   ;; Ergo, no lost wakeup
	   (futex-wait (waitqueue-data-address queue)
		       (sb!kernel:get-lisp-obj-address me)))
      ;; If we are interrupted while waiting, we should do these things
      ;; before returning.  Ideally, in the case of an unhandled signal,
      ;; we should do them before entering the debugger, but this is
      ;; better than nothing.
      (get-mutex lock value))))


(defun condition-notify (queue)
  "Notify one of the processes waiting on QUEUE"
  (with-spinlock (queue) (signal-queue-head queue)))

#!+sb-futex
(defun condition-notify/futex (queue)
  "Notify one of the processes waiting on QUEUE."
  (let ((me (current-thread-id)))
    ;; no problem if >1 thread notifies during the comment in
    ;; condition-wait: as long as the value in queue-data isn't the
    ;; waiting thread's id, it matters not what it is
    ;; XXX we should do something to ensure that the result of this setf
    ;; is visible to all CPUs
    (setf (waitqueue-data queue) me)
    (futex-wake (waitqueue-data-address queue) 1)))

;; FIXME need non-futex variant of this too
#!+sb-futex
(defun condition-broadcast (queue)
  "Notify all of the processes waiting on QUEUE."
  (let ((me (current-thread-id)))
    (setf (waitqueue-data queue) me)
    (futex-wake (waitqueue-data-address queue) (ash 1 30))))

;;; Futexes may be available at compile time but not runtime, so we
;;; default to not using them unless os_init says they're available
(defun maybe-install-futex-functions ()
  #!+sb-futex
  (unless (zerop (extern-alien "linux_supports_futex" int))
    (setf (fdefinition 'get-mutex) #'get-mutex/futex
	  (fdefinition 'release-mutex) #'release-mutex/futex
	  (fdefinition 'condition-wait) #'condition-wait/futex
	  (fdefinition 'condition-notify) #'condition-notify/futex)
    t))

;;;; multiple independent listeners

(defvar *session-lock* nil)

(defun make-listener-thread (tty-name)  
  (assert (probe-file tty-name))
  ;; FIXME probably still need to do some tty stuff to get signals
  ;; delivered correctly.
  ;; FIXME 
  (let* ((in (sb!unix:unix-open (namestring tty-name) sb!unix:o_rdwr #o666))
	 (out (sb!unix:unix-dup in))
	 (err (sb!unix:unix-dup in)))
    (labels ((thread-repl () 
	       (sb!unix::unix-setsid)
	       (let* ((*session-lock*
		       (make-mutex :name (format nil "lock for ~A" tty-name)))
		      (sb!impl::*stdin* 
		       (sb!sys:make-fd-stream in :input t :buffering :line))
		      (sb!impl::*stdout* 
		       (sb!sys:make-fd-stream out :output t :buffering :line))
		      (sb!impl::*stderr* 
		       (sb!sys:make-fd-stream err :output t :buffering :line))
		      (sb!impl::*tty* 
		       (sb!sys:make-fd-stream err :input t :output t :buffering :line))
		      (sb!impl::*descriptor-handlers* nil))
		 (get-mutex *session-lock*)
		 (sb!sys:enable-interrupt sb!unix:sigint #'sb!unix::sigint-handler)
		 (unwind-protect
		      (sb!impl::toplevel-repl nil)
		   (sb!int:flush-standard-output-streams)))))
      (make-thread #'thread-repl))))
  
;;;; job control

;;; called from top of invoke-debugger
(defun debugger-wait-until-foreground-thread (stream)
  "Returns T if thread had been running in background, NIL if it was
already the foreground thread."
  (let ((lock *session-lock*))
    (when (not (eql (mutex-value lock)   (CURRENT-THREAD-ID)))
      (get-foreground))))

;;; note that this is broken in a futex world as no way to find out
;;; which threads are on a kernel queue
(defun thread-repl-prompt-fun (out-stream)
  (let ((lock *session-lock*))
    (get-foreground)
    (let ((stopped-threads (waitqueue-data lock)))
      (when stopped-threads
	(format out-stream "~{~&Thread ~A suspended~}~%" stopped-threads))
      (sb!impl::repl-prompt-fun out-stream))))


