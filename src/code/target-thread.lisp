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
(setf (fdocumentation '*current-thread* 'variable)
      "Bound in each thread to the thread itself.")

(defstruct (thread (:constructor %make-thread))
  #!+sb-doc
  "Thread type. Do not rely on threads being structs as it may change
in future versions."
  name
  %alive-p
  os-thread
  interruptions
  (interruptions-lock (make-mutex :name "thread interruptions lock"))
  result
  (result-lock (make-mutex :name "thread result lock")))

#!+sb-doc
(setf (fdocumentation 'thread-name 'function)
      "The name of the thread. Setfable.")

(def!method print-object ((thread thread) stream)
  (print-unreadable-object (thread stream :type t :identity t)
    (let* ((cookie (list thread))
           (info (if (thread-alive-p thread)
                     :running
                     (multiple-value-list (join-thread thread :default cookie))))
           (state (if (eq :running info)
                      info
                      (if (eq cookie (car info))
                          :aborted
                          :finished)))
           (values (when (eq :finished state) info)))
      (format stream "~@[~S ~]~:[~A~;~A~:[ no values~; values: ~:*~{~S~^, ~}~]~]"
              (thread-name thread)
              (eq :finished state)
              state
              values))))

(defun thread-alive-p (thread)
  #!+sb-doc
  "Check if THREAD is running."
  (thread-%alive-p thread))

;; A thread is eligible for gc iff it has finished and there are no
;; more references to it. This list is supposed to keep a reference to
;; all running threads.
(defvar *all-threads* ())
(defvar *all-threads-lock* (make-mutex :name "all threads lock"))

(defmacro with-all-threads-lock (&body body)
  `(with-system-mutex (*all-threads-lock*)
     ,@body))

(defun list-all-threads ()
  #!+sb-doc
  "Return a list of the live threads."
  (with-all-threads-lock
    (copy-list *all-threads*)))

(declaim (inline current-thread-sap))
(defun current-thread-sap ()
  (sb!vm::current-thread-offset-sap sb!vm::thread-this-slot))

(declaim (inline current-thread-os-thread))
(defun current-thread-os-thread ()
  (sap-int
   (sb!vm::current-thread-offset-sap sb!vm::thread-os-thread-slot)))

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

  (define-alien-routine "block_deferrable_signals"
      void)

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

(declaim (inline get-spinlock release-spinlock))

;; Should always be called with interrupts disabled.
(defun get-spinlock (spinlock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((new *current-thread*)
         (old (sb!ext:compare-and-swap (spinlock-value spinlock) nil new)))
    (when old
      (when (eq old new)
        (error "Recursive lock attempt on ~S." spinlock))
      #!+sb-thread
      (flet ((cas ()
               (unless (sb!ext:compare-and-swap (spinlock-value spinlock) nil new)
                 (return-from get-spinlock t))))
        (if (and (not *interrupts-enabled*) *allow-with-interrupts*)
            ;; If interrupts are enabled, but we are allowed to enabled them,
            ;; check for pending interrupts every once in a while.
            (loop
              (loop repeat 128 do (cas)) ; 128 is arbitrary here
              (sb!unix::%check-interrupts))
            (loop (cas)))))
    t))

(defun release-spinlock (spinlock)
  (declare (optimize (speed 3) (safety 0)))
  (setf (spinlock-value spinlock) nil)
  nil)

;;;; mutexes

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

(defun get-mutex (mutex &optional (new-owner *current-thread*) (waitp t))
  #!+sb-doc
  "Acquire MUTEX for NEW-OWNER, which must be a thread or NIL. If
NEW-OWNER is NIL, it defaults to the current thread. If WAITP is
non-NIL and the mutex is in use, sleep until it is available.

Note: using GET-MUTEX to assign a MUTEX to another thread then the
current one is not recommended, and liable to be deprecated.

GET-MUTEX is not interrupt safe. The correct way to call it is:

 (WITHOUT-INTERRUPTS
   ...
   (ALLOW-WITH-INTERRUPTS (GET-MUTEX ...))
   ...)

WITHOUT-INTERRUPTS is necessary to avoid an interrupt unwinding the
call while the mutex is in an inconsistent state while
ALLOW-WITH-INTERRUPTS allows the call to be interrupted from sleep.

It is recommended that you use WITH-MUTEX instead of calling GET-MUTEX
directly."
  (declare (type mutex mutex) (optimize (speed 3))
           #!-sb-thread (ignore waitp))
  (unless new-owner
    (setq new-owner *current-thread*))
  (when (eql new-owner (mutex-%owner mutex))
    (error "Recursive lock attempt ~S." mutex))
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
    (when (zerop (with-lutex-address (lutex (mutex-lutex mutex))
                   (if waitp
                       (with-interrupts (%lutex-lock lutex))
                       (%lutex-trylock lutex))))
      (setf (mutex-%owner mutex) new-owner)
      t)
    #!-sb-lutex
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
             (multiple-value-bind (to-sec to-usec) (decode-timeout nil)
               (when (= 1 (with-pinned-objects (mutex)
                            (futex-wait (mutex-state-address mutex)
                                        (get-lisp-obj-address +lock-contested+)
                                        (or to-sec -1)
                                        (or to-usec 0))))
                 (signal-deadline))))
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

(defun release-mutex (mutex)
  #!+sb-doc
  "Release MUTEX by setting it to NIL. Wake up threads waiting for
this mutex.

RELEASE-MUTEX is not interrupt safe: interrupts should be disabled
around calls to it.

Signals a WARNING is current thread is not the current owner of the
mutex."
  (declare (type mutex mutex))
  ;; Order matters: set owner to NIL before releasing state.
  (let* ((self *current-thread*)
         (old-owner (sb!ext:compare-and-swap (mutex-%owner mutex) self nil)))
    (unless  (eql self old-owner)
      (warn "Releasing ~S, owned by another thread: ~S" mutex old-owner)
      (setf (mutex-%owner mutex) nil)))
  #!+sb-thread
  (progn
    #!+sb-lutex
    (with-lutex-address (lutex (mutex-lutex mutex))
      (%lutex-unlock lutex))
    #!-sb-lutex
    (let ((old (sb!ext:compare-and-swap (mutex-state mutex)
                                        +lock-taken+ +lock-free+)))
      (when (eql old +lock-contested+)
        (sb!ext:compare-and-swap (mutex-state mutex)
                                 +lock-contested+ +lock-free+)
        (with-pinned-objects (mutex)
          (futex-wake (mutex-state-address mutex) 1))))
    nil))

;;;; waitqueues/condition variables

(defstruct (waitqueue (:constructor %make-waitqueue))
  #!+sb-doc
  "Waitqueue type."
  (name nil :type (or null simple-string))
  #!+(and sb-lutex sb-thread)
  (lutex (make-lutex))
  #!-sb-lutex
  (data nil))

(defun make-waitqueue (&key name)
  #!+sb-doc
  "Create a waitqueue."
  (%make-waitqueue :name name))

#!+sb-doc
(setf (fdocumentation 'waitqueue-name 'function)
      "The name of the waitqueue. Setfable.")

#!+(and sb-thread (not sb-lutex))
(define-structure-slot-addressor waitqueue-data-address
    :structure waitqueue
    :slot data)

(defun condition-wait (queue mutex)
  #!+sb-doc
  "Atomically release MUTEX and enqueue ourselves on QUEUE.  Another
thread may subsequently notify us using CONDITION-NOTIFY, at which
time we reacquire MUTEX and return to the caller."
  #!-sb-thread (declare (ignore queue))
  (assert mutex)
  #!-sb-thread (error "Not supported in unithread builds.")
  #!+sb-thread
  (let ((owner (mutex-%owner mutex)))
    (/show0 "CONDITION-WAITing")
    #!+sb-lutex
    (progn
      ;; FIXME: This doesn't look interrupt safe!
      (setf (mutex-%owner mutex) nil)
      (with-lutex-address (queue-lutex-address (waitqueue-lutex queue))
        (with-lutex-address (mutex-lutex-address (mutex-lutex mutex))
          (%lutex-wait queue-lutex-address mutex-lutex-address)))
      (setf (mutex-%owner mutex) owner))
    #!-sb-lutex
    (unwind-protect
         (let ((me *current-thread*))
           ;; FIXME: should we do something to ensure that the result
           ;; of this setf is visible to all CPUs?
           (setf (waitqueue-data queue) me)
           (release-mutex mutex)
           ;; Now we go to sleep using futex-wait.  If anyone else
           ;; manages to grab MUTEX and call CONDITION-NOTIFY during
           ;; this comment, it will change queue->data, and so
           ;; futex-wait returns immediately instead of sleeping.
           ;; Ergo, no lost wakeup. We may get spurious wakeups,
           ;; but that's ok.
           (multiple-value-bind (to-sec to-usec) (decode-timeout nil)
             (when (= 1 (with-pinned-objects (queue me)
                          (futex-wait (waitqueue-data-address queue)
                                      (get-lisp-obj-address me)
                                      (or to-sec -1) ;; our way if saying "no timeout"
                                      (or to-usec 0))))
               (signal-deadline))))
      ;; If we are interrupted while waiting, we should do these things
      ;; before returning.  Ideally, in the case of an unhandled signal,
      ;; we should do them before entering the debugger, but this is
      ;; better than nothing.
      (get-mutex mutex owner))))

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

(defstruct (semaphore (:constructor %make-semaphore (name %count)))
  #!+sb-doc
  "Semaphore type. The fact that a SEMAPHORE is a STRUCTURE-OBJECT
should be considered an implementation detail, and may change in the
future."
  (name nil :type (or null simple-string))
  (%count 0 :type (integer 0))
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
  ;; a more direct implementation based directly on futexes should be
  ;; possible
  (with-mutex ((semaphore-mutex semaphore))
    (loop until (> (semaphore-%count semaphore) 0)
          do (condition-wait (semaphore-queue semaphore) (semaphore-mutex semaphore))
          finally (decf (semaphore-%count semaphore)))))

(defun signal-semaphore (semaphore &optional (n 1))
  #!+sb-doc
  "Increment the count of SEMAPHORE by N. If there are threads waiting
on this semaphore, then N of them is woken up."
  (declare (type (integer 1) n))
  (with-mutex ((semaphore-mutex semaphore))
    (when (= n (incf (semaphore-%count semaphore) n))
      (condition-notify (semaphore-queue semaphore) n))))

;;;; job control, independent listeners

(defstruct session
  (lock (make-mutex :name "session lock"))
  (threads nil)
  (interactive-threads nil)
  (interactive-threads-queue (make-waitqueue)))

(defvar *session* nil)

;;; The debugger itself tries to acquire the session lock, don't let
;;; funny situations (like getting a sigint while holding the session
;;; lock) occur. At the same time we need to allow interrupts while
;;; *waiting* for the session lock for things like GET-FOREGROUND
;;; to be interruptible.
;;;
;;; Take care: we sometimes need to obtain the session lock while holding
;;; on to *ALL-THREADS-LOCK*, so we must _never_ obtain it _after_ getting
;;; a session lock! (Deadlock risk.)
;;;
;;; FIXME: It would be good to have ordered locks to ensure invariants like
;;; the above.
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
  ;; We're going down, can't handle interrupts sanely anymore.
  ;; GC remains enabled.
  (block-deferrable-signals)
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

;;;; the beef

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
          (lambda ()
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
            (let* ((*current-thread* thread)
                   (*restart-clusters* nil)
                   (*handler-clusters* nil)
                   (*condition-restarts* nil)
                   (sb!impl::*deadline* nil)
                   (sb!impl::*step-out* nil)
                   ;; internal printer variables
                   (sb!impl::*previous-case* nil)
                   (sb!impl::*previous-readtable-case* nil)
                   (empty (vector))
                   (sb!impl::*merge-sort-temp-vector* empty)
                   (sb!impl::*zap-array-data-temp* empty)
                   (sb!impl::*internal-symbol-output-fun* nil)
                   (sb!impl::*descriptor-handlers* nil)) ; serve-event
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
                      (unwind-protect
                           (progn
                             ;; now that most things have a chance to
                             ;; work properly without messing up other
                             ;; threads, it's time to enable signals
                             (sb!unix::reset-signal-mask)
                             (setf (thread-result thread)
                                   (cons t
                                         (multiple-value-list
                                          (funcall real-function)))))
                        (handle-thread-exit thread)))))))
            (values))))
    ;; Keep INITIAL-FUNCTION pinned until the child thread is
    ;; initialized properly.
    (with-pinned-objects (initial-function)
      (let ((os-thread
             (%create-thread
              (get-lisp-obj-address initial-function))))
        (when (zerop os-thread)
          (error "Can't create a new thread"))
        (wait-on-semaphore setup-sem)
        thread))))

(define-condition join-thread-error (error)
  ((thread :reader join-thread-error-thread :initarg :thread))
  #!+sb-doc
  (:documentation "Joining thread failed.")
  (:report (lambda (c s)
             (format s "Joining thread failed: thread ~A ~
                        has not returned normally."
                     (join-thread-error-thread c)))))

#!+sb-doc
(setf (fdocumentation 'join-thread-error-thread 'function)
      "The thread that we failed to join.")

(defun join-thread (thread &key (default nil defaultp))
  #!+sb-doc
  "Suspend current thread until THREAD exits. Returns the result
values of the thread function. If the thread does not exit normally,
return DEFAULT if given or else signal JOIN-THREAD-ERROR."
  (with-mutex ((thread-result-lock thread))
    (cond ((car (thread-result thread))
           (values-list (cdr (thread-result thread))))
          (defaultp
           default)
          (t
           (error 'join-thread-error :thread thread)))))

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
(setf (fdocumentation 'interrupt-thread-error-thread 'function)
      "The thread that was not interrupted.")

(defmacro with-interruptions-lock ((thread) &body body)
  `(with-system-mutex ((thread-interruptions-lock ,thread))
     ,@body))

;; Called from the signal handler in C.
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
  #!-sb-thread
  (with-interrupt-bindings
    (with-interrupts (funcall function)))
  #!+sb-thread
  (if (eq thread *current-thread*)
      (with-interrupt-bindings
        (with-interrupts (funcall function)))
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
    (tagbody
       ;; Prevent the dead from dying completely while we look for the TLS area...
       (with-all-threads-lock
         (if (thread-alive-p thread)
             (let* ((offset (* sb!vm:n-word-bytes (sb!vm::symbol-tls-index symbol)))
                    (tl-val (sap-ref-word (%thread-sap thread) offset)))
               (if (eql tl-val sb!vm::no-tls-value-marker-widetag)
                   (go :unbound)
                   (return-from %symbol-value-in-thread (values (make-lisp-obj tl-val) t))))
             (return-from %symbol-value-in-thread (values nil nil))))
     :unbound
       (error "Cannot read thread-local symbol value: ~S unbound in ~S" symbol thread)))

  (defun %set-symbol-value-in-thread (symbol thread value)
    (tagbody
       (with-pinned-objects (value)
         ;; Prevent the dead from dying completely while we look for the TLS area...
         (with-all-threads-lock
           (if (thread-alive-p thread)
               (let* ((offset (* sb!vm:n-word-bytes (sb!vm::symbol-tls-index symbol)))
                      (sap (%thread-sap thread))
                      (tl-val (sap-ref-word sap offset)))
                 (if (eql tl-val sb!vm::no-tls-value-marker-widetag)
                     (go :unbound)
                     (setf (sap-ref-word sap offset) (get-lisp-obj-address value)))
                 (return-from %set-symbol-value-in-thread (values value t)))
               (return-from %set-symbol-value-in-thread (values nil nil)))))
     :unbound
       (error "Cannot set thread-local symbol value: ~S unbound in ~S" symbol thread))))

(defun sb!vm::locked-symbol-global-value-add (symbol-name delta)
  (sb!vm::locked-symbol-global-value-add symbol-name delta))

;;; Stepping

(defun thread-stepping ()
  (make-lisp-obj
   (sap-ref-word (current-thread-sap)
                 (* sb!vm::thread-stepping-slot sb!vm:n-word-bytes))))

(defun (setf thread-stepping) (value)
  (setf (sap-ref-word (current-thread-sap)
                      (* sb!vm::thread-stepping-slot sb!vm:n-word-bytes))
        (get-lisp-obj-address value)))
