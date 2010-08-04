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

;;; Conditions

(define-condition thread-error (error)
  ((thread :reader thread-error-thread :initarg :thread))
  #!+sb-doc
  (:documentation
   "Conditions of type THREAD-ERROR are signalled when thread operations fail.
The offending thread is initialized by the :THREAD initialization argument and
read by the function THREAD-ERROR-THREAD."))

#!+sb-doc
(setf
 (fdocumentation 'thread-error-thread 'function)
 "Return the offending thread that the THREAD-ERROR pertains to.")

(define-condition symbol-value-in-thread-error (cell-error thread-error)
  ((info :reader symbol-value-in-thread-error-info :initarg :info))
  (:report
   (lambda (condition stream)
     (destructuring-bind (op problem)
         (symbol-value-in-thread-error-info condition)
       (format stream "Cannot ~(~A~) value of ~S in ~S: ~S"
               op
               (cell-error-name condition)
               (thread-error-thread condition)
               (ecase problem
                 (:unbound-in-thread "the symbol is unbound in thread.")
                 (:no-tls-value "the symbol has no thread-local value.")
                 (:thread-dead "the thread has exited.")
                 (:invalid-tls-value "the thread-local value is not valid."))))))
  #!+sb-doc
  (:documentation
   "Signalled when SYMBOL-VALUE-IN-THREAD or its SETF version fails due to eg.
the symbol not having a thread-local value, or the target thread having
exited. The offending symbol can be accessed using CELL-ERROR-NAME, and the
offending thread using THREAD-ERROR-THREAD."))

(define-condition join-thread-error (thread-error) ()
  (:report (lambda (c s)
             (format s "Joining thread failed: thread ~A ~
                        did not return normally."
                     (thread-error-thread c))))
  #!+sb-doc
  (:documentation
   "Signalled when joining a thread fails due to abnormal exit of the thread
to be joined. The offending thread can be accessed using
THREAD-ERROR-THREAD."))

(defun join-thread-error-thread (condition)
  (thread-error-thread condition))
(define-compiler-macro join-thread-error-thread (condition)
  (deprecation-warning 'join-thread-error-thread 'thread-error-thread)
  `(thread-error-thread ,condition))

#!+sb-doc
(setf
 (fdocumentation 'join-thread-error-thread 'function)
 "The thread that we failed to join. Deprecated, use THREAD-ERROR-THREAD
instead.")

(define-condition interrupt-thread-error (thread-error) ()
  (:report (lambda (c s)
             (format s "Interrupt thread failed: thread ~A has exited."
                     (thread-error-thread c))))
  #!+sb-doc
  (:documentation
   "Signalled when interrupting a thread fails because the thread has already
exited. The offending thread can be accessed using THREAD-ERROR-THREAD."))

(defun interrupt-thread-error-thread (condition)
  (thread-error-thread condition))
(define-compiler-macro interrupt-thread-error-thread (condition)
  (deprecation-warning 'join-thread-error-thread 'thread-error-thread)
  `(thread-error-thread ,condition))

#!+sb-doc
(setf
 (fdocumentation 'interrupt-thread-error-thread 'function)
 "The thread that was not interrupted. Deprecated, use THREAD-ERROR-THREAD
instead.")

;;; Of the WITH-PINNED-OBJECTS in this file, not every single one is
;;; necessary because threads are only supported with the conservative
;;; gencgc and numbers on the stack (returned by GET-LISP-OBJ-ADDRESS)
;;; are treated as references.

;;; set the doc here because in early-thread FDOCUMENTATION is not
;;; available, yet
#!+sb-doc
(setf (fdocumentation '*current-thread* 'variable)
      "Bound in each thread to the thread itself.")

#!+sb-doc
(setf
 (fdocumentation 'thread-name 'function)
 "Name of the thread. Can be assigned to using SETF. Thread names can be
arbitrary printable objects, and need not be unique.")

(def!method print-object ((thread thread) stream)
  (print-unreadable-object (thread stream :type t :identity t)
    (let* ((cookie (list thread))
           (info (if (thread-alive-p thread)
                     :running
                     (multiple-value-list
                      (join-thread thread :default cookie))))
           (state (if (eq :running info)
                      info
                      (if (eq cookie (car info))
                          :aborted
                          :finished)))
           (values (when (eq :finished state) info)))
      (format stream
              "~@[~S ~]~:[~A~;~A~:[ no values~; values: ~:*~{~S~^, ~}~]~]"
              (thread-name thread)
              (eq :finished state)
              state
              values))))

(defun thread-alive-p (thread)
  #!+sb-doc
  "Return T if THREAD is still alive. Note that the return value is
potentially stale even before the function returns, as the thread may exit at
any time."
  (thread-%alive-p thread))

;; A thread is eligible for gc iff it has finished and there are no
;; more references to it. This list is supposed to keep a reference to
;; all running threads.
(defvar *all-threads* ())
(defvar *all-threads-lock* (make-mutex :name "all threads lock"))

(defvar *default-alloc-signal* nil)

(defmacro with-all-threads-lock (&body body)
  `(with-system-mutex (*all-threads-lock*)
     ,@body))

(defun list-all-threads ()
  #!+sb-doc
  "Return a list of the live threads. Note that the return value is
potentially stale even before the function returns, as new threads may be
created and old ones may exit at any time."
  (with-all-threads-lock
    (copy-list *all-threads*)))

(declaim (inline current-thread-sap))
(defun current-thread-sap ()
  (sb!vm::current-thread-offset-sap sb!vm::thread-this-slot))

(declaim (inline current-thread-os-thread))
(defun current-thread-os-thread ()
  #!+sb-thread
  (sap-int (sb!vm::current-thread-offset-sap sb!vm::thread-os-thread-slot))
  #!-sb-thread
  0)

(defun init-initial-thread ()
  (/show0 "Entering INIT-INITIAL-THREAD")
  (let ((initial-thread (%make-thread :name "initial thread"
                                      :%alive-p t
                                      :os-thread (current-thread-os-thread))))
    (setq *current-thread* initial-thread)
    ;; Either *all-threads* is empty or it contains exactly one thread
    ;; in case we are in reinit since saving core with multiple
    ;; threads doesn't work.
    (setq *all-threads* (list initial-thread))))


;;;; Aliens, low level stuff

(define-alien-routine "kill_safely"
    integer
  (os-thread #!-alpha unsigned-long #!+alpha unsigned-int)
  (signal int))

#!+sb-thread
(progn
  ;; FIXME it would be good to define what a thread id is or isn't
  ;; (our current assumption is that it's a fixnum).  It so happens
  ;; that on Linux it's a pid, but it might not be on posix thread
  ;; implementations.
  (define-alien-routine ("create_thread" %create-thread)
      unsigned-long (lisp-fun-address unsigned-long))

  (declaim (inline %block-deferrable-signals))
  (define-alien-routine ("block_deferrable_signals" %block-deferrable-signals)
      void
    (where sb!alien:unsigned-long)
    (old sb!alien:unsigned-long))

  (defun block-deferrable-signals ()
    (%block-deferrable-signals 0 0))

  #!+sb-lutex
  (progn
    (declaim (inline %lutex-init %lutex-wait %lutex-wake
                     %lutex-lock %lutex-unlock))

    (define-alien-routine ("lutex_init" %lutex-init)
        int (lutex unsigned-long))

    (define-alien-routine ("lutex_wait" %lutex-wait)
        int (queue-lutex unsigned-long) (mutex-lutex unsigned-long))

    (define-alien-routine ("lutex_wake" %lutex-wake)
        int (lutex unsigned-long) (n int))

    (define-alien-routine ("lutex_lock" %lutex-lock)
        int (lutex unsigned-long))

    (define-alien-routine ("lutex_trylock" %lutex-trylock)
        int (lutex unsigned-long))

    (define-alien-routine ("lutex_unlock" %lutex-unlock)
        int (lutex unsigned-long))

    (define-alien-routine ("lutex_destroy" %lutex-destroy)
        int (lutex unsigned-long))

    ;; FIXME: Defining a whole bunch of alien-type machinery just for
    ;; passing primitive lutex objects directly to foreign functions
    ;; doesn't seem like fun right now. So instead we just manually
    ;; pin the lutex, get its address, and let the callee untag it.
    (defmacro with-lutex-address ((name lutex) &body body)
      `(let ((,name ,lutex))
         (with-pinned-objects (,name)
           (let ((,name (get-lisp-obj-address ,name)))
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
    (declaim (inline futex-wait %futex-wait futex-wake))

    (define-alien-routine ("futex_wait" %futex-wait)
        int (word unsigned-long) (old-value unsigned-long)
        (to-sec long) (to-usec unsigned-long))

    (defun futex-wait (word old to-sec to-usec)
      (with-interrupts
        (%futex-wait word old to-sec to-usec)))

    (define-alien-routine "futex_wake"
        int (word unsigned-long) (n unsigned-long))))

;;; used by debug-int.lisp to access interrupt contexts
#!-(or sb-fluid sb-thread) (declaim (inline sb!vm::current-thread-offset-sap))
#!-sb-thread
(defun sb!vm::current-thread-offset-sap (n)
  (declare (type (unsigned-byte 27) n))
  (sap-ref-sap (alien-sap (extern-alien "all_threads" (* t)))
               (* n sb!vm:n-word-bytes)))

#!+sb-thread
(defun sb!vm::current-thread-offset-sap (n)
  (declare (type (unsigned-byte 27) n))
  (sb!vm::current-thread-offset-sap n))


;;;; Spinlocks

(declaim (inline get-spinlock release-spinlock))

;;; Should always be called with interrupts disabled.
(defun get-spinlock (spinlock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((new *current-thread*)
         (old (sb!ext:compare-and-swap (spinlock-value spinlock) nil new)))
    (when old
      (when (eq old new)
        (error "Recursive lock attempt on ~S." spinlock))
      #!+sb-thread
      (flet ((cas ()
               (if (sb!ext:compare-and-swap (spinlock-value spinlock) nil new)
                   (thread-yield)
                   (return-from get-spinlock t))))
        (if (and (not *interrupts-enabled*) *allow-with-interrupts*)
            ;; If interrupts are disabled, but we are allowed to
            ;; enabled them, check for pending interrupts every once
            ;; in a while. %CHECK-INTERRUPTS is taking shortcuts, make
            ;; sure that deferrables are unblocked by doing an empty
            ;; WITH-INTERRUPTS once.
            (progn
              (with-interrupts)
              (loop
               (loop repeat 128 do (cas)) ; 128 is arbitrary here
               (sb!unix::%check-interrupts)))
            (loop (cas)))))
    t))

(defun release-spinlock (spinlock)
  (declare (optimize (speed 3) (safety 0)))
  ;; On x86 and x86-64 we can get away with no memory barriers, (see
  ;; Linux kernel mailing list "spin_unlock optimization(i386)"
  ;; thread, summary at
  ;; http://kt.iserv.nl/kernel-traffic/kt19991220_47.html#1.
  ;;
  ;; If the compiler may reorder this with other instructions, insert
  ;; compiler barrier here.
  ;;
  ;; FIXME: this does not work on SMP Pentium Pro and OOSTORE systems,
  ;; neither on most non-x86 architectures (but we don't have threads
  ;; on those).
  (setf (spinlock-value spinlock) nil)

  ;; FIXME: Is a :memory barrier too strong here?  Can we use a :write
  ;; barrier instead?
  #!+(not (or x86 x86-64))
  (barrier (:memory)))


;;;; Mutexes

#!+sb-doc
(setf (fdocumentation 'make-mutex 'function)
      "Create a mutex."
      (fdocumentation 'mutex-name 'function)
      "The name of the mutex. Setfable.")

#!+(and sb-thread (not sb-lutex))
(progn
  (define-structure-slot-addressor mutex-state-address
      :structure mutex
      :slot state)
  ;; Important: current code assumes these are fixnums or other
  ;; lisp objects that don't need pinning.
  (defconstant +lock-free+ 0)
  (defconstant +lock-taken+ 1)
  (defconstant +lock-contested+ 2))

(defun mutex-owner (mutex)
  "Current owner of the mutex, NIL if the mutex is free. Naturally,
this is racy by design (another thread may acquire the mutex after
this function returns), it is intended for informative purposes. For
testing whether the current thread is holding a mutex see
HOLDING-MUTEX-P."
  ;; Make sure to get the current value.
  (sb!ext:compare-and-swap (mutex-%owner mutex) nil nil))

(defun get-mutex (mutex &optional (new-owner *current-thread*)
                                  (waitp t) (timeout nil))
  #!+sb-doc
  "Deprecated in favor of GRAB-MUTEX."
  (declare (type mutex mutex) (optimize (speed 3))
           #!-sb-thread (ignore waitp timeout))
  (unless new-owner
    (setq new-owner *current-thread*))
  (barrier (:read))
  (let ((old (mutex-%owner mutex)))
    (when (eq new-owner old)
      (error "Recursive lock attempt ~S." mutex))
    #!-sb-thread
    (when old
      (error "Strange deadlock on ~S in an unithreaded build?" mutex)))
  #!-sb-thread
  (setf (mutex-%owner mutex) new-owner)
  #!+sb-thread
  (progn
    ;; FIXME: Lutexes do not currently support deadlines, as at least
    ;; on Darwin pthread_foo_timedbar functions are not supported:
    ;; this means that we probably need to use the Carbon multiprocessing
    ;; functions on Darwin.
    ;;
    ;; FIXME: This is definitely not interrupt safe: what happens if
    ;; we get hit (1) during the lutex calls (ok, they may be safe,
    ;; but has that been checked?) (2) after the lutex call, but
    ;; before setting the mutex owner.
    #!+sb-lutex
    (progn
      (when timeout
        (error "Mutex timeouts not supported on this platform."))
      (when (zerop (with-lutex-address (lutex (mutex-lutex mutex))
                    (if waitp
                        (with-interrupts (%lutex-lock lutex))
                        (%lutex-trylock lutex))))
       (setf (mutex-%owner mutex) new-owner)
       (barrier (:write))
       t))
    #!-sb-lutex
    ;; This is a direct translation of the Mutex 2 algorithm from
    ;; "Futexes are Tricky" by Ulrich Drepper.
    (let ((old (sb!ext:compare-and-swap (mutex-state mutex)
                                        +lock-free+
                                        +lock-taken+)))
      (unless (or (eql +lock-free+ old) (not waitp))
        (tagbody
         :retry
           (when (or (eql +lock-contested+ old)
                     (not (eql +lock-free+
                               (sb!ext:compare-and-swap (mutex-state mutex)
                                                        +lock-taken+
                                                        +lock-contested+))))
             ;; Wait on the contested lock.
             (loop
              (multiple-value-bind (to-sec to-usec stop-sec stop-usec deadlinep)
                  (decode-timeout timeout)
                (declare (ignore stop-sec stop-usec))
                (case (with-pinned-objects (mutex)
                        (futex-wait (mutex-state-address mutex)
                                    (get-lisp-obj-address +lock-contested+)
                                    (or to-sec -1)
                                    (or to-usec 0)))
                  ((1) (if deadlinep
                           (signal-deadline)
                           (return-from get-mutex nil)))
                  ((2))
                  (otherwise (return))))))
           (setf old (sb!ext:compare-and-swap (mutex-state mutex)
                                              +lock-free+
                                              +lock-contested+))
           ;; Did we get it?
           (unless (eql +lock-free+ old)
             (go :retry))))
      (cond ((eql +lock-free+ old)
             (let ((prev (sb!ext:compare-and-swap (mutex-%owner mutex)
                                                  nil new-owner)))
               (when prev
                 (bug "Old owner in free mutex: ~S" prev))
               t))
            (waitp
             (bug "Failed to acquire lock with WAITP."))))))

(defun grab-mutex (mutex &key (waitp t) (timeout nil))
  #!+sb-doc
  "Acquire MUTEX for the current thread. If WAITP is true (the default) and
the mutex is not immediately available, sleep until it is available.

If TIMEOUT is given, it specifies a relative timeout, in seconds, on
how long GRAB-MUTEX should try to acquire the lock in the contested
case. Unsupported on :SB-LUTEX platforms (eg. Darwin), where a non-NIL
TIMEOUT signals an error.

If GRAB-MUTEX returns T, the lock acquisition was successful. In case
of WAITP being NIL, or an expired TIMEOUT, GRAB-MUTEX may also return
NIL which denotes that GRAB-MUTEX did -not- acquire the lock.

Notes:

  - GRAB-MUTEX is not interrupt safe. The correct way to call it is:

      (WITHOUT-INTERRUPTS
        ...
        (ALLOW-WITH-INTERRUPTS (GRAB-MUTEX ...))
        ...)

    WITHOUT-INTERRUPTS is necessary to avoid an interrupt unwinding
    the call while the mutex is in an inconsistent state while
    ALLOW-WITH-INTERRUPTS allows the call to be interrupted from
    sleep.

  - (GRAB-MUTEX <mutex> :timeout 0.0) differs from
    (GRAB-MUTEX <mutex> :waitp nil) in that the former may signal a
    DEADLINE-TIMEOUT if the global deadline was due already on
    entering GRAB-MUTEX.

    The exact interplay of GRAB-MUTEX and deadlines are reserved to
    change in future versions.

  - It is recommended that you use WITH-MUTEX instead of calling
    GRAB-MUTEX directly.
"
  (get-mutex mutex nil waitp timeout))

(defun release-mutex (mutex &key (if-not-owner :punt))
  #!+sb-doc
  "Release MUTEX by setting it to NIL. Wake up threads waiting for
this mutex.

RELEASE-MUTEX is not interrupt safe: interrupts should be disabled
around calls to it.

If the current thread is not the owner of the mutex then it silently
returns without doing anything (if IF-NOT-OWNER is :PUNT), signals a
WARNING (if IF-NOT-OWNER is :WARN), or releases the mutex anyway (if
IF-NOT-OWNER is :FORCE)."
  (declare (type mutex mutex))
  ;; Order matters: set owner to NIL before releasing state.
  (let* ((self *current-thread*)
         (old-owner (sb!ext:compare-and-swap (mutex-%owner mutex) self nil)))
    (unless (eql self old-owner)
      (ecase if-not-owner
        ((:punt) (return-from release-mutex nil))
        ((:warn)
         (warn "Releasing ~S, owned by another thread: ~S" mutex old-owner))
        ((:force))))
    #!+sb-thread
    (when old-owner
      (setf (mutex-%owner mutex) nil)
      #!+sb-lutex
      (with-lutex-address (lutex (mutex-lutex mutex))
        (%lutex-unlock lutex))
      #!-sb-lutex
      ;; FIXME: once ATOMIC-INCF supports struct slots with word sized
      ;; unsigned-byte type this can be used:
      ;;
      ;;     (let ((old (sb!ext:atomic-incf (mutex-state mutex) -1)))
      ;;       (unless (eql old +lock-free+)
      ;;         (setf (mutex-state mutex) +lock-free+)
      ;;         (with-pinned-objects (mutex)
      ;;           (futex-wake (mutex-state-address mutex) 1))))
      (let ((old (sb!ext:compare-and-swap (mutex-state mutex)
                                          +lock-taken+ +lock-free+)))
        (when (eql old +lock-contested+)
          (sb!ext:compare-and-swap (mutex-state mutex)
                                   +lock-contested+ +lock-free+)
          (with-pinned-objects (mutex)
            (futex-wake (mutex-state-address mutex) 1))))
      nil)))


;;;; Waitqueues/condition variables

(defstruct (waitqueue (:constructor %make-waitqueue))
  #!+sb-doc
  "Waitqueue type."
  (name nil :type (or null thread-name))
  #!+(and sb-lutex sb-thread)
  (lutex (make-lutex))
  #!-sb-lutex
  (token nil))

(defun make-waitqueue (&key name)
  #!+sb-doc
  "Create a waitqueue."
  (%make-waitqueue :name name))

#!+sb-doc
(setf (fdocumentation 'waitqueue-name 'function)
      "The name of the waitqueue. Setfable.")

#!+(and sb-thread (not sb-lutex))
(define-structure-slot-addressor waitqueue-token-address
    :structure waitqueue
    :slot token)

(defun condition-wait (queue mutex)
  #!+sb-doc
  "Atomically release MUTEX and enqueue ourselves on QUEUE.  Another
thread may subsequently notify us using CONDITION-NOTIFY, at which
time we reacquire MUTEX and return to the caller.

Note that if CONDITION-WAIT unwinds (due to eg. a timeout) instead of
returning normally, it may do so without holding the mutex."
  #!-sb-thread (declare (ignore queue))
  (assert mutex)
  #!-sb-thread (error "Not supported in unithread builds.")
  #!+sb-thread
  (let ((me *current-thread*))
    (barrier (:read))
    (assert (eq me (mutex-%owner mutex)))
    (/show0 "CONDITION-WAITing")
    #!+sb-lutex
    ;; Need to disable interrupts so that we don't miss setting the
    ;; owner on our way out. (pthread_cond_wait handles the actual
    ;; re-acquisition.)
    (without-interrupts
      (unwind-protect
           (progn
             (setf (mutex-%owner mutex) nil)
             (with-lutex-address (queue-lutex-address (waitqueue-lutex queue))
               (with-lutex-address (mutex-lutex-address (mutex-lutex mutex))
                 (with-local-interrupts
                   (%lutex-wait queue-lutex-address mutex-lutex-address)))))
        (barrier (:write)
          (setf (mutex-%owner mutex) me))))
    #!-sb-lutex
    ;; Need to disable interrupts so that we don't miss grabbing the
    ;; mutex on our way out.
    (without-interrupts
      ;; This setf becomes visible to other CPUS due to the usual
      ;; memory barrier semantics of lock acquire/release. This must
      ;; not be moved into the loop else wakeups may be lost upon
      ;; continuing after a deadline or EINTR.
      (setf (waitqueue-token queue) me)
      (loop
        (multiple-value-bind (to-sec to-usec)
            (allow-with-interrupts (decode-timeout nil))
          (case (unwind-protect
                     (with-pinned-objects (queue me)
                       ;; RELEASE-MUTEX is purposefully as close to
                       ;; FUTEX-WAIT as possible to reduce the size of
                       ;; the window where the token may be set by a
                       ;; notifier.
                       (release-mutex mutex)
                       ;; Now we go to sleep using futex-wait. If
                       ;; anyone else manages to grab MUTEX and call
                       ;; CONDITION-NOTIFY during this comment, it
                       ;; will change the token, and so futex-wait
                       ;; returns immediately instead of sleeping.
                       ;; Ergo, no lost wakeup. We may get spurious
                       ;; wakeups, but that's ok.
                       (allow-with-interrupts
                         (futex-wait (waitqueue-token-address queue)
                                     (get-lisp-obj-address me)
                                     ;; our way of saying "no
                                     ;; timeout":
                                     (or to-sec -1)
                                     (or to-usec 0))))
                  ;; If we are interrupted while waiting, we should
                  ;; do these things before returning. Ideally, in
                  ;; the case of an unhandled signal, we should do
                  ;; them before entering the debugger, but this is
                  ;; better than nothing.
                  (allow-with-interrupts (get-mutex mutex)))
            ;; ETIMEDOUT; we know it was a timeout, yet we cannot
            ;; signal a deadline unconditionally here because the
            ;; call to GET-MUTEX may already have signaled it.
            ((1))
            ;; EINTR; we do not need to return to the caller because
            ;; an interleaved wakeup would change the token causing an
            ;; EWOULDBLOCK in the next iteration.
            ((2))
            ;; EWOULDBLOCK, -1 here, is the possible spurious wakeup
            ;; case. 0 is the normal wakeup.
            (otherwise (return))))))))

(defun condition-notify (queue &optional (n 1))
  #!+sb-doc
  "Notify N threads waiting on QUEUE. The same mutex that is used in
the corresponding CONDITION-WAIT must be held by this thread during
this call."
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
    ;; No problem if >1 thread notifies during the comment in condition-wait:
    ;; as long as the value in queue-data isn't the waiting thread's id, it
    ;; matters not what it is -- using the queue object itself is handy.
    ;;
    ;; XXX we should do something to ensure that the result of this setf
    ;; is visible to all CPUs.
    ;;
    ;; ^-- surely futex_wake() involves a memory barrier?
    #!-sb-lutex
    (progn
      (setf (waitqueue-token queue) queue)
      (with-pinned-objects (queue)
        (futex-wake (waitqueue-token-address queue) n)))))

(defun condition-broadcast (queue)
  #!+sb-doc
  "Notify all threads waiting on QUEUE."
  (condition-notify queue
                    ;; On a 64-bit platform truncating M-P-F to an int
                    ;; results in -1, which wakes up only one thread.
                    (ldb (byte 29 0)
                         most-positive-fixnum)))


;;;; Semaphores

(defstruct (semaphore (:constructor %make-semaphore (name %count)))
  #!+sb-doc
  "Semaphore type. The fact that a SEMAPHORE is a STRUCTURE-OBJECT
should be considered an implementation detail, and may change in the
future."
  (name    nil :type (or null thread-name))
  (%count    0 :type (integer 0))
  (waitcount 0 :type sb!vm:word)
  (mutex (make-mutex))
  (queue (make-waitqueue)))

(setf (fdocumentation 'semaphore-name 'function)
      "The name of the semaphore INSTANCE. Setfable.")

(declaim (inline semaphore-count))
(defun semaphore-count (instance)
  "Returns the current count of the semaphore INSTANCE."
  (semaphore-%count instance))

(defun make-semaphore (&key name (count 0))
  #!+sb-doc
  "Create a semaphore with the supplied COUNT and NAME."
  (%make-semaphore name count))

(defun wait-on-semaphore (semaphore)
  #!+sb-doc
  "Decrement the count of SEMAPHORE if the count would not be
negative. Else blocks until the semaphore can be decremented."
  ;; A more direct implementation based directly on futexes should be
  ;; possible.
  ;;
  ;; We need to disable interrupts so that we don't forget to
  ;; decrement the waitcount (which would happen if an asynch
  ;; interrupt should catch us on our way out from the loop.)
  (with-system-mutex ((semaphore-mutex semaphore) :allow-with-interrupts t)
    ;; Quick check: is it positive? If not, enter the wait loop.
    (let ((count (semaphore-%count semaphore)))
      (if (plusp count)
          (setf (semaphore-%count semaphore) (1- count))
          (unwind-protect
               (progn
                 ;; Need to use ATOMIC-INCF despite the lock, because on our
                 ;; way out from here we might not be locked anymore -- so
                 ;; another thread might be tweaking this in parallel using
                 ;; ATOMIC-DECF. No danger over overflow, since there it
                 ;; at most one increment per thread waiting on the semaphore.
                 (sb!ext:atomic-incf (semaphore-waitcount semaphore))
                 (loop until (plusp (setf count (semaphore-%count semaphore)))
                       do (condition-wait (semaphore-queue semaphore)
                                          (semaphore-mutex semaphore)))
                 (setf (semaphore-%count semaphore) (1- count)))
            ;; Need to use ATOMIC-DECF instead of DECF, as CONDITION-WAIT
            ;; may unwind without the lock being held due to timeouts.
            (sb!ext:atomic-decf (semaphore-waitcount semaphore)))))))

(defun try-semaphore (semaphore &optional (n 1))
  #!+sb-doc
  "Try to decrement the count of SEMAPHORE by N. If the count were to
become negative, punt and return NIL, otherwise return true."
  (declare (type (integer 1) n))
  (with-mutex ((semaphore-mutex semaphore))
    (let ((new-count (- (semaphore-%count semaphore) n)))
      (when (not (minusp new-count))
        (setf (semaphore-%count semaphore) new-count)))))

(defun signal-semaphore (semaphore &optional (n 1))
  #!+sb-doc
  "Increment the count of SEMAPHORE by N. If there are threads waiting
on this semaphore, then N of them is woken up."
  (declare (type (integer 1) n))
  ;; Need to disable interrupts so that we don't lose a wakeup after
  ;; we have incremented the count.
  (with-system-mutex ((semaphore-mutex semaphore) :allow-with-interrupts t)
    (let ((waitcount (semaphore-waitcount semaphore))
          (count (incf (semaphore-%count semaphore) n)))
      (when (plusp waitcount)
        (condition-notify (semaphore-queue semaphore) (min waitcount count))))))


;;;; Job control, independent listeners

(defstruct session
  (lock (make-mutex :name "session lock"))
  (threads nil)
  (interactive-threads nil)
  (interactive-threads-queue (make-waitqueue)))

(defvar *session* nil)

;;; The debugger itself tries to acquire the session lock, don't let
;;; funny situations (like getting a sigint while holding the session
;;; lock) occur. At the same time we need to allow interrupts while
;;; *waiting* for the session lock for things like GET-FOREGROUND to
;;; be interruptible.
;;;
;;; Take care: we sometimes need to obtain the session lock while
;;; holding on to *ALL-THREADS-LOCK*, so we must _never_ obtain it
;;; _after_ getting a session lock! (Deadlock risk.)
;;;
;;; FIXME: It would be good to have ordered locks to ensure invariants
;;; like the above.
(defmacro with-session-lock ((session) &body body)
  `(with-system-mutex ((session-lock ,session) :allow-with-interrupts t)
     ,@body))

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
  (/show0 "HANDLING THREAD EXIT")
  ;; Lisp-side cleanup
  (with-all-threads-lock
    (setf (thread-%alive-p thread) nil)
    (setf (thread-os-thread thread) nil)
    (setq *all-threads* (delete thread *all-threads*))
    (when *session*
      (%delete-thread-from-session thread *session*)))
  #!+sb-lutex
  (without-gcing
    (/show0 "FREEING MUTEX LUTEX")
    (with-lutex-address (lutex (mutex-lutex (thread-interruptions-lock thread)))
      (%lutex-destroy lutex))))

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


;;;; The beef

(defun make-thread (function &key name)
  #!+sb-doc
  "Create a new thread of NAME that runs FUNCTION. When the function
returns the thread exits. The return values of FUNCTION are kept
around and can be retrieved by JOIN-THREAD."
  #!-sb-thread (declare (ignore function name))
  #!-sb-thread (error "Not supported in unithread builds.")
  #!+sb-thread
  (let* ((thread (%make-thread :name name))
         (setup-sem (make-semaphore :name "Thread setup semaphore"))
         (real-function (coerce function 'function))
         (initial-function
          (named-lambda initial-thread-function ()
            ;; In time we'll move some of the binding presently done in C
            ;; here too.
            ;;
            ;; KLUDGE: Here we have a magic list of variables that are
            ;; not thread-safe for one reason or another.  As people
            ;; report problems with the thread safety of certain
            ;; variables, (e.g. "*print-case* in multiple threads
            ;; broken", sbcl-devel 2006-07-14), we add a few more
            ;; bindings here.  The Right Thing is probably some variant
            ;; of Allegro's *cl-default-special-bindings*, as that is at
            ;; least accessible to users to secure their own libraries.
            ;;   --njf, 2006-07-15
            ;;
            ;; As it is, this lambda must not cons until we are ready
            ;; to run GC. Be very careful.
            (let* ((*current-thread* thread)
                   (*restart-clusters* nil)
                   (*handler-clusters* (sb!kernel::initial-handler-clusters))
                   (*condition-restarts* nil)
                   (sb!impl::*deadline* nil)
                   (sb!impl::*deadline-seconds* nil)
                   (sb!impl::*step-out* nil)
                   ;; internal printer variables
                   (sb!impl::*previous-case* nil)
                   (sb!impl::*previous-readtable-case* nil)
                   (sb!impl::*internal-symbol-output-fun* nil)
                   (sb!impl::*descriptor-handlers* nil)) ; serve-event
              ;; Binding from C
              (setf sb!vm:*alloc-signal* *default-alloc-signal*)
              (setf (thread-os-thread thread) (current-thread-os-thread))
              (with-mutex ((thread-result-lock thread))
                (with-all-threads-lock
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
                      (without-interrupts
                        (unwind-protect
                             (with-local-interrupts
                               ;; Now that most things have a chance
                               ;; to work properly without messing up
                               ;; other threads, it's time to enable
                               ;; signals.
                               (sb!unix::unblock-deferrable-signals)
                               (setf (thread-result thread)
                                     (cons t
                                           (multiple-value-list
                                            (funcall real-function))))
                               ;; Try to block deferrables. An
                               ;; interrupt may unwind it, but for a
                               ;; normal exit it prevents interrupt
                               ;; loss.
                               (block-deferrable-signals))
                          ;; We're going down, can't handle interrupts
                          ;; sanely anymore. GC remains enabled.
                          (block-deferrable-signals)
                          ;; We don't want to run interrupts in a dead
                          ;; thread when we leave WITHOUT-INTERRUPTS.
                          ;; This potentially causes important
                          ;; interupts to be lost: SIGINT comes to
                          ;; mind.
                          (setq *interrupt-pending* nil)
                          (handle-thread-exit thread))))))))
            (values))))
    ;; If the starting thread is stopped for gc before it signals the
    ;; semaphore then we'd be stuck.
    (assert (not *gc-inhibit*))
    ;; Keep INITIAL-FUNCTION pinned until the child thread is
    ;; initialized properly. Wrap the whole thing in
    ;; WITHOUT-INTERRUPTS because we pass INITIAL-FUNCTION to another
    ;; thread.
    (without-interrupts
      (with-pinned-objects (initial-function)
        (let ((os-thread
               (%create-thread
                (get-lisp-obj-address initial-function))))
          (when (zerop os-thread)
            (error "Can't create a new thread"))
          (wait-on-semaphore setup-sem)
          thread)))))

(defun join-thread (thread &key (default nil defaultp))
  #!+sb-doc
  "Suspend current thread until THREAD exits. Returns the result
values of the thread function. If the thread does not exit normally,
return DEFAULT if given or else signal JOIN-THREAD-ERROR."
  (with-system-mutex ((thread-result-lock thread) :allow-with-interrupts t)
    (cond ((car (thread-result thread))
           (return-from join-thread
             (values-list (cdr (thread-result thread)))))
          (defaultp
           (return-from join-thread default))))
  (error 'join-thread-error :thread thread))

(defun destroy-thread (thread)
  #!+sb-doc
  "Deprecated. Same as TERMINATE-THREAD."
  (terminate-thread thread))

(defmacro with-interruptions-lock ((thread) &body body)
  `(with-system-mutex ((thread-interruptions-lock ,thread))
     ,@body))

;;; Called from the signal handler.
#!-win32
(defun run-interruption ()
  (let ((interruption (with-interruptions-lock (*current-thread*)
                        (pop (thread-interruptions *current-thread*)))))
    ;; If there is more to do, then resignal and let the normal
    ;; interrupt deferral mechanism take care of the rest. From the
    ;; OS's point of view the signal we are in the handler for is no
    ;; longer pending, so the signal will not be lost.
    (when (thread-interruptions *current-thread*)
      (kill-safely (thread-os-thread *current-thread*) sb!unix:sigpipe))
    (when interruption
      (funcall interruption))))

(defun interrupt-thread (thread function)
  #!+sb-doc
  "Interrupt the live THREAD and make it run FUNCTION. A moderate
degree of care is expected for use of INTERRUPT-THREAD, due to its
nature: if you interrupt a thread that was holding important locks
then do something that turns out to need those locks, you probably
won't like the effect. FUNCTION runs with interrupts disabled, but
WITH-INTERRUPTS is allowed in it. Keep in mind that many things may
enable interrupts (GET-MUTEX when contended, for instance) so the
first thing to do is usually a WITH-INTERRUPTS or a
WITHOUT-INTERRUPTS. Within a thread interrupts are queued, they are
run in same the order they were sent."
  #!+win32
  (declare (ignore thread))
  #!+win32
  (with-interrupt-bindings
    (with-interrupts (funcall function)))
  #!-win32
  (let ((os-thread (thread-os-thread thread)))
    (cond ((not os-thread)
           (error 'interrupt-thread-error :thread thread))
          (t
           (with-interruptions-lock (thread)
             ;; Append to the end of the interruptions queue. It's
             ;; O(N), but it does not hurt to slow interruptors down a
             ;; bit when the queue gets long.
             (setf (thread-interruptions thread)
                   (append (thread-interruptions thread)
                           (list (lambda ()
                                   (without-interrupts
                                     (allow-with-interrupts
                                       (funcall function))))))))
           (when (minusp (kill-safely os-thread sb!unix:sigpipe))
             (error 'interrupt-thread-error :thread thread))))))

(defun terminate-thread (thread)
  #!+sb-doc
  "Terminate the thread identified by THREAD, by causing it to run
SB-EXT:QUIT - the usual cleanup forms will be evaluated"
  (interrupt-thread thread 'sb!ext:quit))

(define-alien-routine "thread_yield" int)

#!+sb-doc
(setf (fdocumentation 'thread-yield 'function)
      "Yield the processor to other threads.")

;;; internal use only.  If you think you need to use these, either you
;;; are an SBCL developer, are doing something that you should discuss
;;; with an SBCL developer first, or are doing something that you
;;; should probably discuss with a professional psychiatrist first
#!+sb-thread
(progn
  (defun %thread-sap (thread)
    (let ((thread-sap (alien-sap (extern-alien "all_threads" (* t))))
          (target (thread-os-thread thread)))
      (loop
        (when (sap= thread-sap (int-sap 0)) (return nil))
        (let ((os-thread (sap-ref-word thread-sap
                                       (* sb!vm:n-word-bytes
                                          sb!vm::thread-os-thread-slot))))
          (when (= os-thread target) (return thread-sap))
          (setf thread-sap
                (sap-ref-sap thread-sap (* sb!vm:n-word-bytes
                                           sb!vm::thread-next-slot)))))))

  (defun %symbol-value-in-thread (symbol thread)
    ;; Prevent the thread from dying completely while we look for the TLS
    ;; area...
    (with-all-threads-lock
      (loop
        (if (thread-alive-p thread)
            (let* ((epoch sb!kernel::*gc-epoch*)
                   (offset (* sb!vm:n-word-bytes
                              (sb!vm::symbol-tls-index symbol)))
                   (tl-val (sap-ref-word (%thread-sap thread) offset)))
              (cond ((zerop offset)
                     (return (values nil :no-tls-value)))
                    ((or (eql tl-val sb!vm:no-tls-value-marker-widetag)
                         (eql tl-val sb!vm:unbound-marker-widetag))
                     (return (values nil :unbound-in-thread)))
                    (t
                     (multiple-value-bind (obj ok) (make-lisp-obj tl-val nil)
                       ;; The value we constructed may be invalid if a GC has
                       ;; occurred. That is harmless, though, since OBJ is
                       ;; either in a register or on stack, and we are
                       ;; conservative on both on GENCGC -- so a bogus object
                       ;; is safe here as long as we don't return it. If we
                       ;; ever port threads to a non-conservative GC we must
                       ;; pin the TL-VAL address before constructing OBJ, or
                       ;; make WITH-ALL-THREADS-LOCK imply WITHOUT-GCING.
                       ;;
                       ;; The reason we don't just rely on TL-VAL pinning the
                       ;; object is that the call to MAKE-LISP-OBJ may cause
                       ;; bignum allocation, at which point TL-VAL might not
                       ;; be alive anymore -- hence the epoch check.
                       (when (eq epoch sb!kernel::*gc-epoch*)
                         (if ok
                             (return (values obj :ok))
                             (return (values obj :invalid-tls-value))))))))
            (return (values nil :thread-dead))))))

  (defun %set-symbol-value-in-thread (symbol thread value)
    (with-pinned-objects (value)
      ;; Prevent the thread from dying completely while we look for the TLS
      ;; area...
      (with-all-threads-lock
        (if (thread-alive-p thread)
            (let ((offset (* sb!vm:n-word-bytes
                             (sb!vm::symbol-tls-index symbol))))
              (cond ((zerop offset)
                     (values nil :no-tls-value))
                    (t
                     (setf (sap-ref-word (%thread-sap thread) offset)
                           (get-lisp-obj-address value))
                     (values value :ok))))
            (values nil :thread-dead))))))

(defun symbol-value-in-thread (symbol thread &optional (errorp t))
  "Return the local value of SYMBOL in THREAD, and a secondary value of T
on success.

If the value cannot be retrieved (because the thread has exited or because it
has no local binding for NAME) and ERRORP is true signals an error of type
SYMBOL-VALUE-IN-THREAD-ERROR; if ERRORP is false returns a primary value of
NIL, and a secondary value of NIL.

Can also be used with SETF to change the thread-local value of SYMBOL.

SYMBOL-VALUE-IN-THREAD is primarily intended as a debugging tool, and not as a
mechanism for inter-thread communication."
  (declare (symbol symbol) (thread thread))
  #!+sb-thread
  (multiple-value-bind (res status) (%symbol-value-in-thread symbol thread)
    (if (eq :ok status)
        (values res t)
        (if errorp
            (error 'symbol-value-in-thread-error
                   :name symbol
                   :thread thread
                   :info (list :read status))
            (values nil nil))))
  #!-sb-thread
  (if (boundp symbol)
      (values (symbol-value symbol) t)
      (if errorp
          (error 'symbol-value-in-thread-error
                 :name symbol
                 :thread thread
                 :info (list :read :unbound-in-thread))
          (values nil nil))))

(defun (setf symbol-value-in-thread) (value symbol thread &optional (errorp t))
  (declare (symbol symbol) (thread thread))
  #!+sb-thread
  (multiple-value-bind (res status) (%set-symbol-value-in-thread symbol thread value)
    (if (eq :ok status)
        (values res t)
        (if errorp
            (error 'symbol-value-in-thread-error
                   :name symbol
                   :thread thread
                   :info (list :write status))
            (values nil nil))))
  #!-sb-thread
  (if (boundp symbol)
      (values (setf (symbol-value symbol) value) t)
      (if errorp
          (error 'symbol-value-in-thread-error
                 :name symbol
                 :thread thread
                 :info (list :write :unbound-in-thread))
          (values nil nil))))

(defun sb!vm::locked-symbol-global-value-add (symbol-name delta)
  (sb!vm::locked-symbol-global-value-add symbol-name delta))


;;;; Stepping

(defun thread-stepping ()
  (make-lisp-obj
   (sap-ref-word (current-thread-sap)
                 (* sb!vm::thread-stepping-slot sb!vm:n-word-bytes))))

(defun (setf thread-stepping) (value)
  (setf (sap-ref-word (current-thread-sap)
                      (* sb!vm::thread-stepping-slot sb!vm:n-word-bytes))
        (get-lisp-obj-address value)))
