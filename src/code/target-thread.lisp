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

;;; Of the WITH-PINNED-OBJECTS in this file, not every single one is
;;; necessary because threads are only supported with the conservative
;;; gencgc and numbers on the stack (returned by GET-LISP-OBJ-ADDRESS)
;;; are treated as references.

;;; set the doc here because in early-thread FDOCUMENTATION is not
;;; available, yet
#!+sb-doc
(setf (sb!kernel:fdocumentation '*current-thread* 'variable)
      "Bound in each thread to the thread itself.")

(defstruct (thread (:constructor %make-thread))
  #!+sb-doc
  "Thread type. Do not rely on threads being structs as it may change
in future versions."
  name
  %alive-p
  os-thread
  interruptions
  (interruptions-lock (make-mutex :name "thread interruptions lock")))

#!+sb-doc
(setf (sb!kernel:fdocumentation 'thread-name 'function)
      "The name of the thread. Setfable.")

(def!method print-object ((thread thread) stream)
  (if (thread-name thread)
      (print-unreadable-object (thread stream :type t :identity t)
        (prin1 (thread-name thread) stream))
      (print-unreadable-object (thread stream :type t :identity t)
        ;; body is empty => there is only one space between type and
        ;; identity
        ))
  thread)

(defun thread-alive-p (thread)
  #!+sb-doc
  "Check if THREAD is running."
  (thread-%alive-p thread))

;; A thread is eligible for gc iff it has finished and there are no
;; more references to it. This list is supposed to keep a reference to
;; all running threads.
(defvar *all-threads* ())
(defvar *all-threads-lock* (make-mutex :name "all threads lock"))

(defun list-all-threads ()
  #!+sb-doc
  "Return a list of the live threads."
  (with-mutex (*all-threads-lock*)
    (copy-list *all-threads*)))

(declaim (inline current-thread-sap))
(defun current-thread-sap ()
  (sb!vm::current-thread-offset-sap sb!vm::thread-this-slot))

(declaim (inline current-thread-sap-id))
(defun current-thread-sap-id ()
  (sap-int
   (sb!vm::current-thread-offset-sap sb!vm::thread-os-thread-slot)))

(defun init-initial-thread ()
  (/show0 "Entering INIT-INTIAL-THREAD")
  (let ((initial-thread (%make-thread :name "initial thread"
                                      :%alive-p t
                                      :os-thread (current-thread-sap-id))))
    (setq *current-thread* initial-thread)
    ;; Either *all-threads* is empty or it contains exactly one thread
    ;; in case we are in reinit since saving core with multiple
    ;; threads doesn't work.
    (setq *all-threads* (list initial-thread))))

;;;;

#!+sb-thread
(progn
  ;; FIXME it would be good to define what a thread id is or isn't
  ;; (our current assumption is that it's a fixnum).  It so happens
  ;; that on Linux it's a pid, but it might not be on posix thread
  ;; implementations.
  (define-alien-routine ("create_thread" %create-thread)
      unsigned-long (lisp-fun-address unsigned-long))

  (define-alien-routine "signal_interrupt_thread"
      integer (os-thread unsigned-long))

  (define-alien-routine "block_blockable_signals"
      void)

  #!+sb-lutex
  (progn
    (declaim (inline %lutex-init %lutex-wait %lutex-wake
                     %lutex-lock %lutex-unlock))

    (sb!alien:define-alien-routine ("lutex_init" %lutex-init)
        int (lutex unsigned-long))

    (sb!alien:define-alien-routine ("lutex_wait" %lutex-wait)
        int (lutex unsigned-long))

    (sb!alien:define-alien-routine ("lutex_wake" %lutex-wake)
        int (lutex unsigned-long) (n int))

    (sb!alien:define-alien-routine ("lutex_lock" %lutex-lock)
        int (lutex unsigned-long))

    (sb!alien:define-alien-routine ("lutex_unlock" %lutex-unlock)
        int (lutex unsigned-long))

    ;; FIXME: still need to figure out how finalization works...
    (sb!alien:define-alien-routine ("lutex_destroy" %lutex-destroy)
        int (lutex unsigned-long))

    ;; FIXME: Defining a whole bunch of alien-type machinery just for
    ;; passing primitive lutex objects directly to foreign functions
    ;; doesn't seem like fun right now. So instead we just manually
    ;; pin the lutex, get its address, and let the callee untag it.
    (defmacro with-lutex-address ((name lutex) &body body)
      `(let ((,name ,lutex))
         (with-pinned-objects (,name)
           (let ((,name (sb!kernel:get-lisp-obj-address ,name)))
             ,@body))))

    (defun make-lutex ()
      (/show0 "Entering MAKE-LUTEX")
      ;; Suppress GC until the lutex has been properly registered with
      ;; the GC.
      (without-gcing
        (let ((lutex (sb!vm::%make-lutex)))
          (/show0 "LUTEX=..")
          (/hexstr lutex)
          (with-lutex-address (lutex lutex)
            (%lutex-init lutex))
          lutex))))

  #!-sb-lutex
  (progn
    (declaim (inline futex-wait futex-wake))

    (sb!alien:define-alien-routine "futex_wait"
        int (word unsigned-long) (old-value unsigned-long))

    (sb!alien:define-alien-routine "futex_wake"
        int (word unsigned-long) (n unsigned-long))))

;;; used by debug-int.lisp to access interrupt contexts
#!-(and sb-fluid sb-thread) (declaim (inline sb!vm::current-thread-offset-sap))
#!-sb-thread
(defun sb!vm::current-thread-offset-sap (n)
  (declare (type (unsigned-byte 27) n))
  (sap-ref-sap (alien-sap (extern-alien "all_threads" (* t)))
               (* n sb!vm:n-word-bytes)))

;;;; spinlocks

(declaim (inline get-spinlock release-spinlock))

;;; The bare 2 here and below are offsets of the slots in the struct.
;;; There ought to be some better way to get these numbers
(defun get-spinlock (spinlock)
  (declare (optimize (speed 3) (safety 0))
           #!-sb-thread
           (ignore spinlock new-value))
  ;; %instance-set-conditional can test for 0 (which is a fixnum) and
  ;; store any value
  #!+sb-thread
  (loop until
        (eql (sb!vm::%instance-set-conditional spinlock 2 0 1) 0)))

(defun release-spinlock (spinlock)
  (declare (optimize (speed 3) (safety 0))
           #!-sb-thread (ignore spinlock))
  ;; %instance-set-conditional cannot compare arbitrary objects
  ;; meaningfully, so
  ;; (sb!vm::%instance-set-conditional spinlock 2 our-value 0)
  ;; does not work for bignum thread ids.
  #!+sb-thread
  (sb!vm::%instance-set spinlock 2 0))

(defmacro with-spinlock ((spinlock) &body body)
  (sb!int:with-unique-names (lock)
    `(let ((,lock ,spinlock))
      (get-spinlock ,lock)
      (unwind-protect
           (progn ,@body)
        (release-spinlock ,lock)))))

;;;; mutexes

#!+sb-doc
(setf (sb!kernel:fdocumentation 'make-mutex 'function)
      "Create a mutex."
      (sb!kernel:fdocumentation 'mutex-name 'function)
      "The name of the mutex. Setfable."
      (sb!kernel:fdocumentation 'mutex-value 'function)
      "The value of the mutex. NIL if the mutex is free. Setfable.")

#!-sb-lutex
(progn
#!+sb-thread
(declaim (inline mutex-value-address))
#!+sb-thread
(defun mutex-value-address (mutex)
  (declare (optimize (speed 3)))
  (sb!ext:truly-the
   sb!vm:word
   (+ (sb!kernel:get-lisp-obj-address mutex)
      (- (* 3 sb!vm:n-word-bytes) sb!vm:instance-pointer-lowtag))))
)

(defun get-mutex (mutex &optional (new-value *current-thread*) (wait-p t))
  #!+sb-doc
  "Acquire MUTEX, setting it to NEW-VALUE or some suitable default
value if NIL.  If WAIT-P is non-NIL and the mutex is in use, sleep
until it is available"
  (declare (type mutex mutex) (optimize (speed 3)))
  (/show0 "Entering GET-MUTEX")
  (unless new-value
    (setq new-value *current-thread*))
  #!-sb-thread
  (let ((old-value (mutex-value mutex)))
    (when (and old-value wait-p)
      (error "In unithread mode, mutex ~S was requested with WAIT-P ~S and ~
              new-value ~S, but has already been acquired (with value ~S)."
             mutex wait-p new-value old-value))
    (setf (mutex-value mutex) new-value)
    t)
  #!+sb-thread
  (let (old)
    (when (eql new-value (mutex-value mutex))
      (warn "recursive lock attempt ~S~%" mutex)
      (format *debug-io* "Thread: ~A~%" *current-thread*)
      (sb!debug:backtrace most-positive-fixnum *debug-io*)
      (force-output *debug-io*))
    (loop
     (unless
         (setf old (sb!vm::%instance-set-conditional mutex 2 nil new-value))
       (return t))
     (unless wait-p (return nil))
     #!+sb-lutex
     (with-lutex-address (lutex (mutex-lutex mutex))
       (%lutex-lock lutex))
     #!-sb-lutex
     (with-pinned-objects (mutex old)
       (futex-wait (mutex-value-address mutex)
                   (sb!kernel:get-lisp-obj-address old))))))

(defun release-mutex (mutex)
  #!+sb-doc
  "Release MUTEX by setting it to NIL. Wake up threads waiting for
this mutex."
  (declare (type mutex mutex))
  (/show0 "Entering RELEASE-MUTEX")
  (setf (mutex-value mutex) nil)
  #!+sb-thread
  (progn
    #!+sb-lutex
    (with-lutex-address (lutex (mutex-lutex mutex))
      (%lutex-unlock lutex))
    #!-sb-lutex
    (futex-wake (mutex-value-address mutex) 1)))

;;;; waitqueues/condition variables

(defstruct (waitqueue (:constructor %make-waitqueue))
  #!+sb-doc
  "Waitqueue type."
  (name nil :type (or null simple-string))
  #!+sb-lutex
  (lutex (make-lutex))
  #!-sb-lutex
  (data nil))

(defun make-waitqueue (&key name)
  #!+sb-doc
  "Create a waitqueue."
  (%make-waitqueue :name name))

#!+sb-doc
(setf (sb!kernel:fdocumentation 'waitqueue-name 'function)
      "The name of the waitqueue. Setfable.")

#!-sb-lutex
(progn
#!+sb-thread
(declaim (inline waitqueue-data-address))
#!+sb-thread
(defun waitqueue-data-address (waitqueue)
  (declare (optimize (speed 3)))
  (sb!ext:truly-the
   sb!vm:word
   (+ (sb!kernel:get-lisp-obj-address waitqueue)
      (- (* 3 sb!vm:n-word-bytes) sb!vm:instance-pointer-lowtag))))
)

(defun condition-wait (queue mutex)
  #!+sb-doc
  "Atomically release MUTEX and enqueue ourselves on QUEUE.  Another
thread may subsequently notify us using CONDITION-NOTIFY, at which
time we reacquire MUTEX and return to the caller."
  #!-sb-thread (declare (ignore queue))
  (assert mutex)
  #!-sb-thread (error "Not supported in unithread builds.")
  #!+sb-thread
  (let ((value (mutex-value mutex)))
    (unwind-protect
         (let ((me *current-thread*))
           #!+sb-lutex (declare (ignore me))
           (/show0 "CONDITION-WAITing")
           ;; XXX we should do something to ensure that the result of this setf
           ;; is visible to all CPUs
           ;;
           ;; XXX do lutexes handle this?
           #!-sb-lutex
           (setf (waitqueue-data queue) me)
           (release-mutex mutex)
           ;; Now we go to sleep using futex-wait.  If anyone else
           ;; manages to grab MUTEX and call CONDITION-NOTIFY during
           ;; this comment, it will change queue->data, and so
           ;; futex-wait returns immediately instead of sleeping.
           ;; Ergo, no lost wakeup
           #!+sb-lutex
           (with-lutex-address (lutex (waitqueue-lutex queue))
             (%lutex-wait lutex))
           #!-sb-lutex
           (with-pinned-objects (queue me)
             (futex-wait (waitqueue-data-address queue)
                         (sb!kernel:get-lisp-obj-address me))))
      ;; If we are interrupted while waiting, we should do these things
      ;; before returning.  Ideally, in the case of an unhandled signal,
      ;; we should do them before entering the debugger, but this is
      ;; better than nothing.
      (get-mutex mutex value))))

(defun condition-notify (queue &optional (n 1))
  #!+sb-doc
  "Notify N threads waiting on QUEUE."
  #!-sb-thread (declare (ignore queue n))
  #!-sb-thread (error "Not supported in unithread builds.")
  #!+sb-thread
  (declare (type (and fixnum (integer 1)) n))
  (/show0 "Entering CONDITION-NOTIFY")
  #!+sb-thread
  (progn
    #!+sb-lutex
    (with-lutex-address (lutex (waitqueue-lutex queue))
      (%lutex-wake lutex n))
    ;; no problem if >1 thread notifies during the comment in
    ;; condition-wait: as long as the value in queue-data isn't the
    ;; waiting thread's id, it matters not what it is
    ;; XXX we should do something to ensure that the result of this setf
    ;; is visible to all CPUs
    #!-sb-lutex
    (let ((me *current-thread*))
      (progn
        (setf (waitqueue-data queue) me)
        (with-pinned-objects (queue)
          (futex-wake (waitqueue-data-address queue) n))))))

(defun condition-broadcast (queue)
  #!+sb-doc
  "Notify all threads waiting on QUEUE."
  (condition-notify queue
                    ;; On a 64-bit platform truncating M-P-F to an int results
                    ;; in -1, which wakes up only one thread.
                    (ldb (byte 29 0)
                         most-positive-fixnum)))

;;;; semaphores

(defstruct (semaphore (:constructor %make-semaphore))
  #!+sb-doc
  "Semaphore type."
  (name nil :type (or null simple-string))
  (count 0 :type (integer 0))
  (mutex (make-mutex))
  (queue (make-waitqueue)))

(defun make-semaphore (&key name (count 0))
  #!+sb-doc
  "Create a semaphore with the supplied COUNT."
  (%make-semaphore :name name :count count))

(setf (sb!kernel:fdocumentation 'semaphore-name 'function)
      "The name of the semaphore. Setfable.")

(defun wait-on-semaphore (sem)
  #!+sb-doc
  "Decrement the count of SEM if the count would not be negative. Else
block until the semaphore can be decremented."
  ;; a more direct implementation based directly on futexes should be
  ;; possible
  (with-mutex ((semaphore-mutex sem))
    (loop until (> (semaphore-count sem) 0)
          do (condition-wait (semaphore-queue sem) (semaphore-mutex sem))
          finally (decf (semaphore-count sem)))))

(defun signal-semaphore (sem &optional (n 1))
  #!+sb-doc
  "Increment the count of SEM by N. If there are threads waiting on
this semaphore, then N of them is woken up."
  (declare (type (and fixnum (integer 1)) n))
  (with-mutex ((semaphore-mutex sem))
    (when (= n (incf (semaphore-count sem) n))
      (condition-notify (semaphore-queue sem) n))))

;;;; job control, independent listeners

(defstruct session
  (lock (make-mutex :name "session lock"))
  (threads nil)
  (interactive-threads nil)
  (interactive-threads-queue (make-waitqueue)))

(defvar *session* nil)

;;; the debugger itself tries to acquire the session lock, don't let
;;; funny situations (like getting a sigint while holding the session
;;; lock) occur
(defmacro with-session-lock ((session) &body body)
  #!-sb-thread (declare (ignore session))
  #!-sb-thread
  `(locally ,@body)
  #!+sb-thread
  `(without-interrupts
     (with-mutex ((session-lock ,session))
       ,@body)))

(defun new-session ()
  (make-session :threads (list *current-thread*)
                :interactive-threads (list *current-thread*)))

(defun init-job-control ()
  (/show0 "Entering INIT-JOB-CONTROL")
  (setf *session* (new-session))
  (/show0 "Exiting INIT-JOB-CONTROL"))

(defun %delete-thread-from-session (thread session)
  (with-session-lock (session)
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
#!+sb-thread
(defun handle-thread-exit (thread)
  (with-mutex (*all-threads-lock*)
    (/show0 "HANDLING THREAD EXIT")
    #!+sb-lutex
    (when (thread-interruptions-lock thread)
      (/show0 "FREEING MUTEX LUTEX")
      (with-lutex-address (lutex (mutex-lutex (thread-interruptions-lock thread)))
        (%lutex-destroy lutex)))
    (setq *all-threads* (delete thread *all-threads*)))
  (when *session*
    (%delete-thread-from-session thread *session*)))

(defun terminate-session ()
  #!+sb-doc
  "Kill all threads in session except for this one.  Does nothing if current
thread is not the foreground thread."
  ;; FIXME: threads created in other threads may escape termination
  (let ((to-kill
         (with-session-lock (*session*)
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
  #!-sb-thread nil
  #!+sb-thread
  (prog1
      (with-session-lock (*session*)
        (not (member *current-thread*
                     (session-interactive-threads *session*))))
    (get-foreground)))

(defun get-foreground ()
  #!-sb-thread t
  #!+sb-thread
  (let ((was-foreground t))
    (loop
     (/show0 "Looping in GET-FOREGROUND")
     (with-session-lock (*session*)
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
  #!+sb-doc
  "Background this thread.  If NEXT is supplied, arrange for it to
have the foreground next."
  #!-sb-thread (declare (ignore next))
  #!-sb-thread nil
  #!+sb-thread
  (with-session-lock (*session*)
    (when (rest (session-interactive-threads *session*))
      (setf (session-interactive-threads *session*)
            (delete *current-thread* (session-interactive-threads *session*))))
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
                       (make-fd-stream in :input t :buffering :line
                                       :dual-channel-p t))
                      (sb!impl::*stdout*
                       (make-fd-stream out :output t :buffering :line
                                              :dual-channel-p t))
                      (sb!impl::*stderr*
                       (make-fd-stream err :output t :buffering :line
                                              :dual-channel-p t))
                      (sb!impl::*tty*
                       (make-fd-stream err :input t :output t
                                              :buffering :line
                                              :dual-channel-p t))
                      (sb!impl::*descriptor-handlers* nil))
                 (with-new-session ()
                   (unwind-protect
                        (sb!impl::toplevel-repl nil)
                     (sb!int:flush-standard-output-streams))))))
      (make-thread #'thread-repl))))

;;;; the beef

(defun make-thread (function &key name)
  #!+sb-doc
  "Create a new thread of NAME that runs FUNCTION. When the function
returns the thread exits."
  #!-sb-thread (declare (ignore function name))
  #!-sb-thread (error "Not supported in unithread builds.")
  #!+sb-thread
  (let* ((thread (%make-thread :name name))
         (setup-sem (make-semaphore :name "Thread setup semaphore"))
         (real-function (coerce function 'function))
         (initial-function
          (lambda ()
            ;; in time we'll move some of the binding presently done in C
            ;; here too
            (let ((*current-thread* thread)
                  (sb!kernel::*restart-clusters* nil)
                  (sb!kernel::*handler-clusters* nil)
                  (sb!kernel::*condition-restarts* nil)
                  (sb!impl::*descriptor-handlers* nil)) ; serve-event
              (setf (thread-os-thread thread) (current-thread-sap-id))
              (with-mutex (*all-threads-lock*)
                (push thread *all-threads*))
              (with-session-lock (*session*)
                (push thread (session-threads *session*)))
              (setf (thread-%alive-p thread) t)
              (signal-semaphore setup-sem)
              ;; can't use handling-end-of-the-world, because that flushes
              ;; output streams, and we don't necessarily have any (or we
              ;; could be sharing them)
              (catch 'sb!impl::toplevel-catcher
                (catch 'sb!impl::%end-of-the-world
                  (with-simple-restart
                      (terminate-thread
                       (format nil
                               "~~@<Terminate this thread (~A)~~@:>"
                               *current-thread*))
                    (unwind-protect
                         (progn
                           ;; now that most things have a chance to
                           ;; work properly without messing up other
                           ;; threads, it's time to enable signals
                           (sb!unix::reset-signal-mask)
                           (funcall real-function))
                      ;; we're going down, can't handle
                      ;; interrupts sanely anymore
                      (let ((sb!impl::*gc-inhibit* t))
                        (block-blockable-signals)
                        (setf (thread-%alive-p thread) nil)
                        (setf (thread-os-thread thread) nil)
                        ;; and remove what can be the last
                        ;; reference to this thread
                        (handle-thread-exit thread)))))))
            (values))))
    ;; Keep INITIAL-FUNCTION pinned until the child thread is
    ;; initialized properly.
    (with-pinned-objects (initial-function)
      (let ((os-thread
             (%create-thread
              (sb!kernel:get-lisp-obj-address initial-function))))
        (when (zerop os-thread)
          (error "Can't create a new thread"))
        (wait-on-semaphore setup-sem)
        thread))))

(defun destroy-thread (thread)
  #!+sb-doc
  "Deprecated. Same as TERMINATE-THREAD."
  (terminate-thread thread))

(define-condition interrupt-thread-error (error)
  ((thread :reader interrupt-thread-error-thread :initarg :thread))
  #!+sb-doc
  (:documentation "Interrupting thread failed.")
  (:report (lambda (c s)
             (format s "Interrupt thread failed: thread ~A has exited."
                     (interrupt-thread-error-thread c)))))

#!+sb-doc
(setf (sb!kernel:fdocumentation 'interrupt-thread-error-thread 'function)
      "The thread that was not interrupted.")

(defmacro with-interruptions-lock ((thread) &body body)
  `(without-interrupts
     (with-mutex ((thread-interruptions-lock ,thread))
       ,@body)))

;; Called from the signal handler.
(defun run-interruption ()
  (in-interruption ()
    (loop
       (let ((interruption (with-interruptions-lock (*current-thread*)
                             (pop (thread-interruptions *current-thread*)))))
         (if interruption
             (with-interrupts
               (funcall interruption))
             (return))))))

;; The order of interrupt execution is peculiar. If thread A
;; interrupts thread B with I1, I2 and B for some reason receives I1
;; when FUN2 is already on the list, then it is FUN2 that gets to run
;; first. But when FUN2 is run SIG_INTERRUPT_THREAD is enabled again
;; and I2 hits pretty soon in FUN2 and run FUN1. This is of course
;; just one scenario, and the order of thread interrupt execution is
;; undefined.
(defun interrupt-thread (thread function)
  #!+sb-doc
  "Interrupt the live THREAD and make it run FUNCTION. A moderate
degree of care is expected for use of INTERRUPT-THREAD, due to its
nature: if you interrupt a thread that was holding important locks
then do something that turns out to need those locks, you probably
won't like the effect."
  #!-sb-thread (declare (ignore thread))
  ;; not quite perfect, because it does not take WITHOUT-INTERRUPTS
  ;; into account
  #!-sb-thread
  (funcall function)
  #!+sb-thread
  (if (eq thread *current-thread*)
      (funcall function)
      (let ((os-thread (thread-os-thread thread)))
        (cond ((not os-thread)
               (error 'interrupt-thread-error :thread thread))
              (t
               (with-interruptions-lock (thread)
                 (push function (thread-interruptions thread)))
               (when (minusp (signal-interrupt-thread os-thread))
                 (error 'interrupt-thread-error :thread thread)))))))

(defun terminate-thread (thread)
  #!+sb-doc
  "Terminate the thread identified by THREAD, by causing it to run
SB-EXT:QUIT - the usual cleanup forms will be evaluated"
  (interrupt-thread thread 'sb!ext:quit))

;;; internal use only.  If you think you need to use this, either you
;;; are an SBCL developer, are doing something that you should discuss
;;; with an SBCL developer first, or are doing something that you
;;; should probably discuss with a professional psychiatrist first
#!+sb-thread
(defun thread-sap-for-id (id)
  (let ((thread-sap (alien-sap (extern-alien "all_threads" (* t)))))
    (loop
     (when (sap= thread-sap (int-sap 0)) (return nil))
     (let ((os-thread (sap-ref-word thread-sap
                                    (* sb!vm:n-word-bytes
                                       sb!vm::thread-os-thread-slot))))
       (when (= os-thread id) (return thread-sap))
       (setf thread-sap
             (sap-ref-sap thread-sap (* sb!vm:n-word-bytes
                                        sb!vm::thread-next-slot)))))))

#!+sb-thread
(defun symbol-value-in-thread (symbol thread-sap)
  (let* ((index (sb!vm::symbol-tls-index symbol))
         (tl-val (sap-ref-word thread-sap
                               (* sb!vm:n-word-bytes index))))
    (if (eql tl-val sb!vm::no-tls-value-marker-widetag)
        (sb!vm::symbol-global-value symbol)
        (sb!kernel:make-lisp-obj tl-val))))
