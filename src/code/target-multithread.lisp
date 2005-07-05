;;;; support for threads in the target machine

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!THREAD")

(define-alien-routine ("create_thread" %create-thread)
    system-area-pointer
  (lisp-fun-address unsigned-long))

(define-alien-routine reap-dead-thread void
  (thread-sap system-area-pointer))

(defvar *session* nil)

;;;; queues, locks

;; spinlocks use 0 as "free" value: higher-level locks use NIL
(declaim (inline get-spinlock release-spinlock))

(defun get-spinlock (lock offset new-value)
  (declare (optimize (speed 3) (safety 0)))
  ;; %instance-set-conditional can test for 0 (which is a fixnum) and
  ;; store any value
  (loop until
       (eql (sb!vm::%instance-set-conditional lock offset 0 new-value) 0)))

(defun release-spinlock (lock offset)
  (declare (optimize (speed 3) (safety 0)))
  ;; %instance-set-conditional cannot compare arbitrary objects
  ;; meaningfully, so
  ;; (sb!vm::%instance-set-conditional lock offset our-value 0)
  ;; does not work for bignum thread ids.
  (sb!vm::%instance-set lock offset 0))

(defmacro with-spinlock ((queue) &body body)
  `(unwind-protect
    (progn
      (get-spinlock ,queue 2 *current-thread*)
      ,@body)
    (release-spinlock ,queue 2)))


;;;; the higher-level locking operations are based on waitqueues

(declaim (inline waitqueue-data-address mutex-value-address))

(defstruct waitqueue
  (name nil :type (or null simple-string))
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

(declaim (inline futex-wait futex-wake))
(sb!alien:define-alien-routine
    "futex_wait" int (word unsigned-long) (old-value unsigned-long))
(sb!alien:define-alien-routine
    "futex_wake" int (word unsigned-long) (n unsigned-long))


;;;; mutex

(defun get-mutex (lock &optional new-value (wait-p t))
  "Acquire LOCK, setting it to NEW-VALUE or some suitable default value
if NIL.  If WAIT-P is non-NIL and the lock is in use, sleep until it
is available"
  (declare (type mutex lock) (optimize (speed 3)))
  (let (old)
    (unless new-value (setf new-value *current-thread*))
    (when (eql new-value (mutex-value lock))
      (warn "recursive lock attempt ~S~%" lock)
      (format *debug-io* "Thread: ~A~%" *current-thread*)
      (sb!debug:backtrace most-positive-fixnum *debug-io*)
      (force-output *debug-io*))
    (loop
     (unless
         (setf old (sb!vm::%instance-set-conditional lock 4 nil new-value))
       (return t))
     (unless wait-p (return nil))
     (futex-wait (mutex-value-address lock)
                 (sb!kernel:get-lisp-obj-address old)))))

(defun release-mutex (lock)
  (declare (type mutex lock))
  (setf (mutex-value lock) nil)
  (futex-wake (mutex-value-address lock) 1))

;;;; condition variables

(defun condition-wait (queue lock)
  "Atomically release LOCK and enqueue ourselves on QUEUE.  Another
thread may subsequently notify us using CONDITION-NOTIFY, at which
time we reacquire LOCK and return to the caller."
  (assert lock)
  (let ((value (mutex-value lock)))
    (unwind-protect
         (let ((me *current-thread*))
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
  (let ((me *current-thread*))
    ;; no problem if >1 thread notifies during the comment in
    ;; condition-wait: as long as the value in queue-data isn't the
    ;; waiting thread's id, it matters not what it is
    ;; XXX we should do something to ensure that the result of this setf
    ;; is visible to all CPUs
    (setf (waitqueue-data queue) me)
    (futex-wake (waitqueue-data-address queue) 1)))

(defun condition-broadcast (queue)
  (let ((me *current-thread*))
    (setf (waitqueue-data queue) me)
    (futex-wake (waitqueue-data-address queue) (ash 1 30))))

(defun make-thread (function &key name)
  ;;   ;; don't let them interrupt us because the child is waiting for setup-p
  ;;   (sb!sys:without-interrupts
  (let* ((thread (%make-thread :name name))
         (setup-p nil)
         (real-function (coerce function 'function))
         (thread-sap
          (%create-thread
           (sb!kernel:get-lisp-obj-address
            (lambda ()
              ;; FIXME: use semaphores?
              (loop until setup-p)
              ;; in time we'll move some of the binding presently done in C
              ;; here too
              (let ((*current-thread* thread)
                    (sb!kernel::*restart-clusters* nil)
                    (sb!kernel::*handler-clusters* nil)
                    (sb!kernel::*condition-restarts* nil)
                    (sb!impl::*descriptor-handlers* nil) ; serve-event
                    (sb!impl::*available-buffers* nil)) ;for fd-stream
                ;; can't use handling-end-of-the-world, because that flushes
                ;; output streams, and we don't necessarily have any (or we
                ;; could be sharing them)
                (unwind-protect
                     (catch 'sb!impl::toplevel-catcher
                       (catch 'sb!impl::%end-of-the-world
                         (with-simple-restart
                             (terminate-thread
                              (format nil "~~@<Terminate this thread (~A)~~@:>"
                                      *current-thread*))
                           ;; now that most things have a chance to work
                           ;; properly without messing up other threads, it's
                           ;; time to enable signals
                           (sb!unix::reset-signal-mask)
                           (unwind-protect
                                (funcall real-function)
                             ;; we're going down, can't handle
                             ;; interrupts sanely anymore
                             (sb!unix::block-blockable-signals)))))
                  ;; mark the thread dead, so that the gc does not
                  ;; wait for it to handle sig-stop-for-gc
                  (%set-thread-state thread :dead)
                  ;; and remove what can be the last reference to
                  ;; the thread object
                  (handle-thread-exit thread)
                  0))
              (values))))))
    (when (sb!sys:sap= thread-sap (sb!sys:int-sap 0))
      (error "Can't create a new thread"))
    (setf (thread-%sap thread) thread-sap)
    (with-mutex (*all-threads-lock*)
      (push thread *all-threads*))
    (with-mutex ((session-lock *session*))
      (push thread (session-threads *session*)))
    (setq setup-p t)
    (sb!impl::finalize thread (lambda () (reap-dead-thread thread-sap)))
    thread))

(defun destroy-thread (thread)
  "Deprecated. Soon to be removed or reimplemented using pthread_cancel."
  (terminate-thread thread))

;;; a moderate degree of care is expected for use of interrupt-thread,
;;; due to its nature: if you interrupt a thread that was holding
;;; important locks then do something that turns out to need those
;;; locks, you probably won't like the effect.

(define-condition interrupt-thread-error (error)
  ((thread :reader interrupt-thread-error-thread :initarg :thread)
   (errno :reader interrupt-thread-error-errno :initarg :errno))
  (:report (lambda (c s)
             (format s "interrupt thread ~A failed (~A: ~A)"
                     (interrupt-thread-error-thread c)
                     (interrupt-thread-error-errno c)
                     (strerror (interrupt-thread-error-errno c))))))

(defun interrupt-thread (thread function)
  "Interrupt THREAD and make it run FUNCTION."
  (let ((function (coerce function 'function)))
    (multiple-value-bind (res err)
        (sb!unix::syscall ("interrupt_thread"
                           system-area-pointer  sb!alien:unsigned-long)
                          thread
                          (thread-%sap thread)
                          (sb!kernel:get-lisp-obj-address function))
      (unless res
        (error 'interrupt-thread-error :thread thread :errno err)))))

(defun terminate-thread (thread)
  "Terminate the thread identified by THREAD, by causing it to run
SB-EXT:QUIT - the usual cleanup forms will be evaluated"
  (interrupt-thread thread 'sb!ext:quit))

;;; internal use only.  If you think you need to use this, either you
;;; are an SBCL developer, are doing something that you should discuss
;;; with an SBCL developer first, or are doing something that you
;;; should probably discuss with a professional psychiatrist first
(defun symbol-value-in-thread (symbol thread)
  (let ((thread-sap (thread-%sap thread)))
    (let* ((index (sb!vm::symbol-tls-index symbol))
           (tl-val (sb!sys:sap-ref-word thread-sap
                                        (* sb!vm:n-word-bytes index))))
      (if (eql tl-val sb!vm::unbound-marker-widetag)
          (sb!vm::symbol-global-value symbol)
          (sb!kernel:make-lisp-obj tl-val)))))

;;;; job control, independent listeners

(defstruct session
  (lock (make-mutex :name "session lock"))
  (threads nil)
  (interactive-threads nil)
  (interactive-threads-queue (make-waitqueue)))

(defun new-session ()
  (make-session :threads (list *current-thread*)
                :interactive-threads (list *current-thread*)))

(defun init-job-control ()
  (setf *session* (new-session)))

(defun %delete-thread-from-session (thread session)
  (with-mutex ((session-lock session))
    (setf (session-threads session)
          (delete thread (session-threads session))
          (session-interactive-threads session)
          (delete thread (session-interactive-threads session)))))

(defun call-with-new-session (fn)
  (%delete-thread-from-session *current-thread* *session*)
  (let ((*session* (new-session)))
    (funcall fn)))

(defmacro with-new-session (args &body forms)
  (declare (ignore args))               ;for extensibility
  (sb!int:with-unique-names (fb-name)
    `(labels ((,fb-name () ,@forms))
      (call-with-new-session (function ,fb-name)))))

;;; Remove thread from its session, if it has one.
(defun handle-thread-exit (thread)
  (with-mutex (*all-threads-lock*)
    (setq *all-threads* (delete thread *all-threads*)))
  (when *session*
    (%delete-thread-from-session thread *session*)))

(defun terminate-session ()
  "Kill all threads in session except for this one.  Does nothing if current
thread is not the foreground thread"
  ;; FIXME: threads created in other threads may escape termination
  (let ((to-kill
         (with-mutex ((session-lock *session*))
           (and (eq *current-thread*
                    (car (session-interactive-threads *session*)))
                (session-threads *session*)))))
    ;; do the kill after dropping the mutex; unwind forms in dying
    ;; threads may want to do session things
    (dolist (thread to-kill)
      (unless (eq thread *current-thread*)
        ;; terminate the thread but don't be surprised if it has
        ;; exited in the meantime
        (handler-case (terminate-thread thread)
          (interrupt-thread-error ()))))))

;;; called from top of invoke-debugger
(defun debugger-wait-until-foreground-thread (stream)
  "Returns T if thread had been running in background, NIL if it was
interactive."
  (declare (ignore stream))
  (prog1
      (with-mutex ((session-lock *session*))
        (not (member *current-thread*
                     (session-interactive-threads *session*))))
    (get-foreground)))

(defun get-foreground ()
  (let ((was-foreground t))
    (loop
     (with-mutex ((session-lock *session*))
       (let ((int-t (session-interactive-threads *session*)))
         (when (eq (car int-t) *current-thread*)
           (unless was-foreground
             (format *query-io* "Resuming thread ~A~%" *current-thread*))
           (return-from get-foreground t))
         (setf was-foreground nil)
         (unless (member *current-thread* int-t)
           (setf (cdr (last int-t))
                 (list *current-thread*)))
         (condition-wait
          (session-interactive-threads-queue *session*)
          (session-lock *session*)))))))

(defun release-foreground (&optional next)
  "Background this thread.  If NEXT is supplied, arrange for it to
have the foreground next"
  (with-mutex ((session-lock *session*))
    (setf (session-interactive-threads *session*)
          (delete *current-thread* (session-interactive-threads *session*)))
    (when next
      (setf (session-interactive-threads *session*)
            (list* next
                   (delete next (session-interactive-threads *session*)))))
    (condition-broadcast (session-interactive-threads-queue *session*))))

(defun foreground-thread ()
  (car (session-interactive-threads *session*)))

(defun make-listener-thread (tty-name)
  (assert (probe-file tty-name))
  (let* ((in (sb!unix:unix-open (namestring tty-name) sb!unix:o_rdwr #o666))
         (out (sb!unix:unix-dup in))
         (err (sb!unix:unix-dup in)))
    (labels ((thread-repl ()
               (sb!unix::unix-setsid)
               (let* ((sb!impl::*stdin*
                       (sb!sys:make-fd-stream in :input t :buffering :line
                                              :dual-channel-p t))
                      (sb!impl::*stdout*
                       (sb!sys:make-fd-stream out :output t :buffering :line
                                              :dual-channel-p t))
                      (sb!impl::*stderr*
                       (sb!sys:make-fd-stream err :output t :buffering :line
                                              :dual-channel-p t))
                      (sb!impl::*tty*
                       (sb!sys:make-fd-stream err :input t :output t
                                              :buffering :line
                                              :dual-channel-p t))
                      (sb!impl::*descriptor-handlers* nil))
                 (with-new-session ()
                   (unwind-protect
                        (sb!impl::toplevel-repl nil)
                     (sb!int:flush-standard-output-streams))))))
      (make-thread #'thread-repl))))
